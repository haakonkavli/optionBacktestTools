simulateOptionVectors <- function(vol_data,
                                  price_data,
                                  in_underlying,
                                  in_type,
                                  in_holding_period = 28) {




# Check inputs ------------------------------------------------------------
    if (max(c(length(in_underlying),
              length(in_type),
              length(in_holding_period))) > 1) {
        stop('Only pass one underlying, type and holding period at the time.')
    }


    if (!(in_type %in% c('put','call'))) {
        stop('Type must be put or call')
    }

    if (!(in_underlying %in% vol_data$underlying)) {
        stop('Underlying not in vol_data')
    }

    if (!(in_underlying %in% price_data$underlying)) {
        stop('Underlying not in price_data')
    }

    if (in_holding_period < 1) {
        stop('in_holding_period must be greater than 1')
    }



# Prepare data and variables ----------------------------------------------


    # Select subset after start date:
    setkey(vol_data,'underlying')

    # Extract relevant rows
    v <- vol_data[underlying %in% in_underlying]
    p <- price_data[underlying %in% in_underlying]

    # All dates:
    dates <- unique(v$datestamp)

    if (sum(!(dates %in% p$datestamp)) > 0) {
        warning('Price data missing for same dates.')
    }

    # All moneyness:
    sample_money <- unique(v$moneyness)

    # All maturities:
    sample_maturities <- unique(v$maturity_days)

    # All directions:
    sample_directions <- c('entry','exit')

    # All variables:
    sample_variables <- c( "value",
                           "delta",
                           "gamma",
                           "vega",
                           "theta",
                           "rho",
                           "divRho")

    # Initialize output:
    nrows = length(in_type) *
        length(in_underlying) *
        length(dates) *
        length(sample_money)  *
        length(sample_maturities) *
        length(sample_directions) *
        (length(sample_variables) + 2)

    option_sims <- data.table(datestamp = character(nrows),
                              underlying = character(nrows),
                              type = character(nrows),
                              field = character(nrows),
                              position_id = integer(nrows),
                              maturity = integer(nrows),
                              moneyness = numeric(nrows),
                              value = numeric(nrows)
    )

    cnames <- names(option_sims)

    # Initialize row id:
    row.id <- 1


    # Start from position id ppid:
    ppid <- 0

    # Start timer:
    tic <- proc.time()


    # Short names:
    t <- in_type
    u <- in_underlying
    h <- in_holding_period

    setkey(v,'datestamp')
    setkey(p,'datestamp')

    # Extract unique dates, maturities and strikes for u:
    m_days <- unique(v$maturity_days)
    m_days <- m_days[!is.na(m_days)]

    strikes <- unique(v$moneyness)
    strikes <- strikes[!is.na(strikes)]

# Start simulations -------------------------------------------------------

    for (d_i in 1:length(dates)) {

        d <- dates[d_i]

        toc <- as.numeric(format(proc.time()[3] - tic[3]))
        toc.hat <- round(toc * ((nrows - row.id)/row.id)/60)
        writeLines(paste('Underlying: ',u, ', Type: ',
                         t, ', Date: ', d,
                         '\n', row.id, ' estimated, ',
                         nrows - row.id, ' to go...',
                         '\nExpected time left: ',
                         toc.hat,' minutes... ',
                         '\n \n ... \n \n',
                         sep = ''))

        # Exit date:
        d.exit <- as.Date(d) + h

        # Price of underlying at entry and exit:
        u.price <- p[datestamp == d]$value
        u.price.exit <- p[datestamp == d.exit]$value

        # Check we have price data for the selected date:
        if ((length(u.price.exit) == 0) | (length(u.price) == 0)) {
            next
        }

        # Extract data set for relevant dates:
        vd <- v[datestamp == d,
                .(datestamp,
                  underlying,
                  item,
                  field,
                  moneyness,
                  maturity_days,
                  value)]

        vd.exit <- v[datestamp == d.exit,
                     .(datestamp,
                       underlying,
                       item,
                       field,
                       raw_moneyness = moneyness,
                       raw_maturity_days = maturity_days,
                       moneyness = moneyness * u.price / u.price.exit,
                       maturity_days = maturity_days - h,
                       raw_value = value)]

        # Check there is data for the exit date:
        if ((dim(vd)[1] == 0 ) | (dim(vd.exit)[1] == 0)) {
            warning('Some exit dates missing price data')
            next

        }

        vd.exit <- vd.exit[
            ,.(datestamp,
               underlying,
               item,
               field,
               raw_moneyness,
               raw_maturity_days,
               moneyness,
               raw_value,
               value_90MNY = raw_value[raw_moneyness == min(raw_moneyness)],
               value_110MNY = raw_value[raw_moneyness == max(raw_moneyness)]
            ),by = maturity_days
            ]

        interpol.points <- vd.exit[,.(moneyness,maturity_days)]

        # Linear interpolation of implied vol:
        interpol.vol <- interpp(x = vd.exit$raw_moneyness,
                                y = vd.exit$raw_maturity_days,
                                z = vd.exit$raw_value,
                                xo = interpol.points$moneyness,
                                yo = interpol.points$maturity_days,
                                linear = TRUE,
                                extrap = FALSE)


        vd.exit$value <- interpol.vol$z

        # Extrapolation = extreme points
        vd.exit[moneyness <= 0.9]$value <-
            vd.exit[moneyness <= 0.9]$value_90MNY
        vd.exit[moneyness >= 1.1]$value <-
            vd.exit[moneyness >= 1.1]$value_110MNY

        # Assume maturity = 0 if only 2 days left:
        vd.exit[maturity_days < 3]$maturity_days <- 0
        vd.exit[maturity_days == 0]$value <- 0

        # Create vector of strike prices
        s.price <- strikes * u.price

        # Start loop: maturity_days
        for (maturity.entry in m_days) {

            # Generate position ids:
            ppids <- (max(ppid) + 1):(max(ppid) + length(s.price))
            ppid <- c(ppid,ppids)


            if (maturity.entry == 30) {
                m.entry <- 28
            } else {
                m.entry <- maturity.entry
            }
            # Exit maturity:
            m.exit <- m.entry - h

            # Extract data for maturity
            vdm <- vd[maturity_days == maturity.entry]
            vdm.exit <- vd.exit[maturity_days == m.exit]

            # Estimate entry price of option at range of moneyness:
            # Entry price:
            vol.entry <- vdm$value/100

            tmp.i <- 0
            while (sum(duplicated(vol.entry)) > 0) {
                vol.entry[duplicated(vol.entry)] <- vol.entry[duplicated(vol.entry)] + 0.00001
                tmp.i <- tmp.i + 1
                if (tmp.i >100) {
                    stop('Too many duplicates...')
                }
            }

            p.option <- EuropeanOptionArrays(t,u.price,s.price,0,0,m.entry/360,vol.entry)
            if (is.null(p.option)) {
                next
            }

            # Extract results:
            p.entry.list <- lapply(
                lapply(
                    lapply(p.option[1:7],diag),
                    cbind,
                    moneyness = vdm$moneyness),
                data.table)
            p.entry.list[['vol']] <- data.table(V1 = vdm$value,
                                                moneyness = vdm$moneyness)

            p.entry.list[['option_price']] <- data.table(V1 = p.entry.list[['value']]$V1 / u.price,
                                                         moneyness = vdm$moneyness)

            p.entry <- rbindlist(p.entry.list)
            p.entry$variable <- rep(names(p.entry.list),each = length(diag(p[1]$value)))
            setnames(p.entry,c('V1'),c('value'))

            p.entry$maturity <- as.integer(m.entry)
            p.entry$position_id <- ppids



            p.entry$datestamp <- as.character(d)

            # Exit price:
            if (m.exit > 0) {

                # Estimate derivative price:
                vol.exit <- vdm.exit$value/100
                tmp.i <- 0
                while (sum(duplicated(vol.exit)) > 0) {
                    vol.exit[duplicated(vol.exit)] <- vol.exit[duplicated(vol.exit)] + 0.00001
                    tmp.i <- tmp.i + 1
                    if (tmp.i >100) {
                        stop('Too many duplicates...')
                    }
                }

                p.e <- EuropeanOptionArrays(t,
                                            u.price.exit,
                                            s.price,
                                            0,
                                            0,
                                            m.exit/360,
                                            vol.exit)
                if (is.null(p.e)) {
                    next
                }

                # Extract results:
                p.exit.list <- lapply(
                    lapply(
                        lapply(p.e[1:7],diag),
                        cbind,
                        moneyness = vdm.exit$moneyness),
                    data.table)

                p.exit.list[['vol']] <- data.table(V1 = vdm.exit$value,
                                                   moneyness = vdm.exit$moneyness)

                p.exit.list[['option_price']] <- data.table(V1 = p.exit.list[['value']]$V1 / u.price.exit,
                                                            moneyness = vdm.exit$moneyness)
                p.exit <- rbindlist(p.exit.list)

                p.exit$variable <- rep(names(p.exit.list),
                                       each = length(diag(p.e[1]$value)))
                setnames(p.exit,'V1','value')

            } else {
                p.exit <- p.entry[
                    ,.(value = 0,
                       moneyness = vdm.exit$moneyness,
                       variable)
                    ]
                if (t == 'put') {
                    p.exit[variable == 'value']$value <-
                        s.price - u.price.exit
                } else if (t == 'call') {
                    p.exit[variable == 'value']$value <-
                        u.price.exit - s.price
                } else {
                    stop('Type must be put or call')
                }
                p.exit[variable == 'value' & value < 0]$value <- 0

            }

            p.exit$maturity <- as.integer(m.exit)
            p.exit$position_id <- ppids

            p.exit$datestamp <- as.character(d.exit)


            ## Rbind entry and exit results:
            p.output <- rbind(p.entry,p.exit)

            p.output$maturity <- as.integer(p.output$maturity)
            p.output$underlying <- u
            p.output$type <- t
            setnames(p.output,'variable','field')

            p.output <- p.output[,cnames,with = F]

            dd <- dim(p.output)
            set(option_sims,(row.id):(row.id + dd[1] - 1),1:dd[2], p.output)
            row.id <- row.id + dim(p.output)[1]
        }
    }




        option_sims <- option_sims[position_id > 0]

        if (dim(option_sims)[1] == 0) {
            return(output = NULL)
        }

        return(option_sims)
}
