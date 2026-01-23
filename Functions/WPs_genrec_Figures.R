

### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------
###
###     DESCRIPTION
###
###     ...wherein this script contains functions needed to construct the various FIGURES of the GenRec working paper...
###
###    ------------------
###       CALIBRATIONS
###    ------------------
###    *** FIG.cbt.cal( )
###         ...which compares charterboat catch estimates ( in CHTS vs. FHS units )
###             as a faceted ggplot ( AB1 vs. B2 ) of timeseries with error bars...           -- Figure 1 --
###
###    *** FIG.mrip.cal( )
###         ...which compares MRIP catch estimates ( in FCAL vs. ACAL vs. BASE units )
###             as a faceted ggplot ( AB1 vs. B2 ) of timeseries with error bars...           -- Figure 2 --
###
###    ------------------------
###       TIMESERIES (LINES)
###    ------------------------
###    *** FIG.catch.errorbars( )
###         ...which produces a simple faceted ggplot ( AB1 vs. B2 ) of timeseries with error bars,
###             and isn't currently in-use in this script but has been used in Caribbean SEDARs...
###
###    *** FIG.catch.sedarcomp( )
###         ...which produces a simple faceted ggplot ( AB1 vs. B2 ) of catch estimates
###             provided for different SEDARs (i.e., current vs. previous )...                -- Figure 3 --
###
###    *** FIG.landings.timeseries( )
###         ...which summarizes landings estimates as three separate timeseries:
###             AB1 in numbers, SEFSC avgwgts, and LBS in pounds...                           -- Figure 6 --
###
###    *** FIG.discards.timeseries( )
###         ...which summarizes discard estimates into three separate facet plots:
###             total discards (with AB1 for comparison), dead discards (with AB1 for comparison),
###             and the associated discard rate (B2/AB1)...                                   -- Figure 7 --
###
###
###    ----------------------------
###       BAR PLOTS / PIE CHARTS
###    ----------------------------
###    *** FIG.catnum.yr.strata( )
###         ...which summarizes catch-in-number ( AB1 & B2 ) as three separate plots:
###             (1) absolute catch by year and strata ( state or mode ),
###             (2) relative catch by year and strata, and (3) relative catch by strata       -- Figures 4 & 5 --
###
###    *** FIG.catwgt.hier( )
###         ...which summarizes the amount of catch ( landings-in-weight ) from SEFSC avgwgts
###             estimated at various levels of the hierarchy (i.e., s/sr/sry/srys/etc. )...   -- Figure 8 --
###
###    *** FIG.landings.unid( )
###         ...which summarizes landings estimates (AB1) across any 'identified' species groups
###             that could be contributing to an 'unidentified' catch estimate...             -- Figure 9 --
###
###
###    ------------------------
###       DISTRIBUTION PLOTS
###    ------------------------
###    *** FIG.catnum.covid( )
###         ...which compares raw (observed) catch records from MRIP based on whether they were
###             actually observed (RAW) or imputed to fill COVID data gaps (IMP)...           -- Figure 10 --
###
###
###    ------------------
###       SPATIAL MAPS
###    ------------------
###    *** FIG.catch.maps( )
###         ...which produces spatial maps of catches (averaged over all years)...
###
###
### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------

FIG.cbt.cal = function( my.data, params ) {
  ###       ...where "my.data" is the data table with CBT catch & CV estimates from various calibrations...
  ###       and 'params' the R object that (amongst other things) identifies the strata associated with this
  ###               particular data product (e.g., region-of-interest for this SEDAR )...
  
  
  ### MEAN Table ###
  data.mean <- reshape2::melt( my.data, id.vars="Year", measure.vars=c( "AB1","B2","CHTS_AB1","CHTS_B2" ) )
  ###       ...to which a column for 'survey' is added (i.e., "FHS" or "CHTS" )...
  data.mean$survey <- rep( "",length=dim(data.mean)[1] )
  data.mean$survey[ which( gsub( "_.*","", data.mean$variable ) == "CHTS" ) ] <- "CHTS"
  data.mean$survey[ which( gsub( "_.*","", data.mean$variable ) %in% c("AB1","B2") ) ] <- "FHS"
  ###       ...and the 'variable' header is redefined (i.e., "AB1" or "B2" )...
  data.mean$variable <- gsub( ".*_","", data.mean$variable )
  ### As a last step, I had some issues with tables that read in numbers formatted so that comma's separted
  ###       the thousands place. Therefore, I format the 'value' column as numeric and remove any commas...
  data.mean$value <- as.numeric( gsub( ",", "", data.mean$value ) )
  
  ### ERROR Table ###
  data.error <- reshape2::melt( my.data, id.vars="Year", measure.vars=c( "CV_AB1","CV_B2","CHTS_CV_AB1","CHTS_CV_B2" ) )
  ###       ...wherein I define a 'survey' column: FHS/CHTS
  data.error$survey <- rep( "",length=dim(data.error)[1] )
  data.error$survey[ which( gsub( "_.*","", data.error$variable ) == "CHTS" ) ] <- "CHTS"
  data.error$survey[ which( gsub( "_.*","", data.error$variable ) == "CV" ) ] <- "FHS"
  ###       ...a 'variable' column: AB1/B2
  data.error$variable <- gsub( ".*_","", data.error$variable )
  ### I also convert the estimated CVs ( in the 'value' column ) to PSEs...
  data.error$value <- data.mean$value * data.error$value
  
  
  AB1.long <- full_join( data.mean[ which( data.mean$variable == "AB1" ), ],
                         data.error[ which( data.error$variable == "AB1" ), ],
                         by=c("Year","variable","survey"), suffix=c("",".PSE") )
  B2.long <- full_join( data.mean[ which( data.mean$variable == "B2" ), ],
                        data.error[ which( data.error$variable == "B2" ), ],
                        by=c("Year","variable","survey"), suffix=c("",".PSE") )
  rm( data.mean, data.error )
  
  
  ### I now combine "AB1.long" and "B2.long" into a single data table...
  dat.long <- rbind( AB1.long, B2.long )
  ###       ...which will allow me to generate multiple plots simultaneously (in ggplot) that can all
  ###       be scaled against the same x- & y-axes. For some SEDARs, there may be a need to generate figures
  ###       for multiple areas and so I create an 'Area' column ( where 'Area' figures go from left-to-right
  ###       in the plot space ), but note that the code works fine with just one 'Area" defined
  ###       ( plot space will have just two figures, AB1 stacked on top of B2, both from the same area )...
  dat.long$Area <- rep( params$region, times=dim(dat.long)[1] )
  
  ###       ...and convert the "value" and "value.PSE" columns into thousands, so that I can plot
  ###         everything in units of thousands of fish ( there's probably some 'thousands' transformation
  ###         I can apply within the ggplot() call, but can't seem to find it... )
  dat.long$value <- dat.long$value / 1000
  dat.long$value.PSE <- dat.long$value.PSE / 1000
  
  ### Lastly, I subset "dat.long" to only include years where CBT data was collected as part of CHTS...
  if( params$region %in% c("Gulf of America","Gulf of America and South Atlantic","Southeast") ) {
    dat.long = dat.long[ dat.long$Year < 2000, ]
  } else {
    dat.long = dat.long[ dat.long$Year < 2004, ]
  }
  ###       ...and then define the resultant number of years, as needed to specify the breaks in my x-axis...
  n.years <- length( unique(dat.long$Year) )
  
  ### In creating my plots, note that I borrow much of the formatting from Kyle's (and Rich) cv plots...
  Fig1.comb <- ggplot( data=dat.long, aes( x=Year, colour=survey ) ) +
    geom_line( aes( y=value ), size=1.2 ) +
    geom_errorbar( aes( ymin=pmax(value-value.PSE,0), ymax=value+value.PSE ),
                   width=0.8, size=1, position=position_dodge(width=0.2) ) +
    ###       ...where position_dodge() preserves the vertical position of a geom (in ggplot) while adjusting
    ###           the horizontal position to avoid overlapping objects...
    facet_grid( variable ~ Area ) +
    labs( title = "",   # title = paste0( "Charterboat Catch Estimates - ", params$species.name ),
          x="Year", y="Thousands of Fish" ) +
    ###       ...where I don't bother with a title as figures are described in the caption (see below)...
    scale_x_continuous( breaks = scales::pretty_breaks( n = n.years ) ) +
    ###     ...where n=(n.years/2) could be used to plot every other year...
    expand_limits(y = 0) +
    # scale_y_continuous(breaks = scales::pretty_breaks(n = 10), labels = scales::comma) +
    theme_bw() +
    scale_color_manual( values=c('darkblue','springgreen4') ) +
    theme( text = element_text(size = 11),
           axis.text.x = element_text(angle = 90, vjust=0.5),
           # plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
           legend.position = "bottom",
           # panel.background = element_blank(),
           panel.grid.major = element_line(colour = "grey", size = 0.5),
           panel.grid.minor = element_line(colour = "grey", size = 0.2),
           panel.border = element_rect(colour = "black", fill = NA) )
  
  rm( dat.long, AB1.long, B2.long, n.years )
  
  
  return.object = Fig1.comb
  rm(Fig1.comb)
  
  return( return.object )
  
}
###
###
### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------

FIG.mrip.cal = function( my.data, params ) {
  ###       ...where "my.data" is the list of data tables ( 2 total ) with AB1 & B2 estimates ( catch & PSEs )
  ###               from various MRIP calibrations (i.e., BASE vs. ACAL vs. FCAL )...
  ###       and 'params' the R object that (amongst other things) identifies the strata associated with this
  ###               particular data product (e.g., region-of-interest for this SEDAR )...
  
  
  ### Converting the above data tables (AB1 & B2) into a LONG format for my ggplot...
  AB1.long.value <- reshape2::melt( my.data$AB1, id.vars="Year", measure.vars=c("BASE","ACAL","FCAL") )
  AB1.long.error <- reshape2::melt( my.data$AB1, id.vars="Year", measure.vars=c("BASE.PSE","ACAL.PSE","FCAL.PSE") )
  ###       ...I then remove the ".PSE" suffix from the column names of my two "error" data tables so that
  ###         the colnames are equivalent to that in my two "value" data tables, as needed for my join...
  levels( AB1.long.error$variable ) <- c( "BASE","ACAL","FCAL" )
  
  B2.long.value <- reshape2::melt( my.data$B2, id.vars="Year", measure.vars=c("BASE","ACAL","FCAL") )
  B2.long.error <- reshape2::melt( my.data$B2, id.vars="Year", measure.vars=c("BASE.PSE","ACAL.PSE","FCAL.PSE") )
  levels( B2.long.error$variable ) <- c( "BASE","ACAL","FCAL" )
  
  ### Joining my catch and error data tables...
  AB1.long <- full_join( AB1.long.value, AB1.long.error, by=c("Year","variable"), suffix=c("",".PSE") )
  B2.long <- full_join( B2.long.value, B2.long.error, by=c("Year","variable"), suffix=c("",".PSE") )
  
  rm( AB1.long.value,AB1.long.error, B2.long.value,B2.long.error )
  
  
  colnames(AB1.long)[ which( colnames(AB1.long) == "variable" ) ] <- "survey"
  AB1.long$variable <- rep( "AB1", times=dim(AB1.long)[1] )
  colnames(B2.long)[ which( colnames(B2.long) == "variable" ) ] <- "survey"
  B2.long$variable <- rep( "B2", times=dim(B2.long)[1] )
  
  dat.long <- rbind( AB1.long, B2.long )
  rm( AB1.long, B2.long )
  
  dat.long$Area <- rep( params$region, times=dim(dat.long)[1] )
  dat.long$value <- dat.long$value / 1000
  dat.long$value.PSE <- dat.long$value.PSE / 1000
  
  n.years <- length( unique(dat.long$Year) )
  
  ### Lastly, I make sure that all years are included in the above figure so that years with catch=0 go down to zero...
  term.year <- max(dat.long$Year)
  inc.years <- 1981:term.year
  for( i in 1:length(inc.years) ) {
    ### AB1 Comparison ###
    if( inc.years[i] %notin% dat.long$Year[ which( dat.long$variable == "AB1" ) ] ) {
      dat.long <- rbind( data.frame( dat.long ),
                         data.frame( Year=inc.years[i], survey="BASE", value=0, value.PSE=0, variable="AB1",Area=params$region ),
                         data.frame( Year=inc.years[i], survey="ACAL", value=0, value.PSE=0, variable="AB1",Area=params$region ),
                         data.frame( Year=inc.years[i], survey="FCAL", value=0, value.PSE=0, variable="AB1",Area=params$region ) )
    }
    ### B2 Comparison ###
    if( inc.years[i] %notin% dat.long$Year[ which( dat.long$variable == "B2" ) ] ) {
      dat.long <- rbind( data.frame( dat.long ),
                         data.frame( Year=inc.years[i], survey="BASE", value=0, value.PSE=0, variable="B2",Area=params$region ),
                         data.frame( Year=inc.years[i], survey="ACAL", value=0, value.PSE=0, variable="B2",Area=params$region ),
                         data.frame( Year=inc.years[i], survey="FCAL", value=0, value.PSE=0, variable="B2",Area=params$region ) )
    }
  }
  rm( term.year, inc.years )
  
  
  Fig2.comb <- ggplot( data=dat.long, aes( x=Year, colour=survey ) ) +
    geom_line( aes( y=value ), size=1.2 ) +
    geom_errorbar( aes( ymin=pmax(value-value.PSE,0), ymax=value+value.PSE ),
                   width=0.8, size=1, position=position_dodge(width=0.2) ) +
    facet_grid( variable ~ Area ) +
    labs( title="", x="Year", y="Thousands of Fish" ) +
    scale_x_continuous( breaks = scales::pretty_breaks( n = (n.years/2) ) ) +
    expand_limits(y = 0) +
    theme_bw() +
    scale_color_manual( values=c('deeppink','springgreen4','darkblue') ) +
    theme( text = element_text(size = 11),
           axis.text.x = element_text(angle = 90, vjust=0.5),
           legend.position = "bottom",
           panel.grid.major = element_line(colour = "grey", size = 0.5),
           panel.grid.minor = element_line(colour = "grey", size = 0.2),
           panel.border = element_rect(colour = "black", fill = NA) )
  
  rm( dat.long, n.years )
  
  
  return.object = Fig2.comb
  rm(Fig2.comb)
  
  return( return.object )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------

FIG.catch.errorbars = function( my.data, params ) {
  ###       ...where "my.data" is the data table with catch & CV estimates ( for AB1 & B2 )...
  ###       and 'params' the R object that (amongst other things) identifies the strata associated with this
  ###               particular data product (e.g., region-of-interest for this SEDAR )...
  
  
  # data.mean  <- reshape2::melt( my.data[,c("Year","AB1","B2")], id.vars="Year", measure.vars=c( "AB1","B2" ) )
  # 
  # data.error <- reshape2::melt( my.data[,c("Year","CV_AB1","CV_B2")], id.vars="Year", measure.vars=c( "CV_AB1","CV_B2" ) )
  # data.error$variable <- gsub( "CV_","", data.error$variable )
  # data.error$value <- data.mean$value * data.error$value
  # 
  # data.long <- full_join( data.mean, data.error, by=c("Year","variable"), suffix=c("",".PSE") )
  # rm( data.mean, data.error )
  
  data.long = my.data
  
  strat = colnames(data.long)[ colnames(data.long)  %notin% c('variable','value','value.PSE') ]
  
  if( 'SID' %in% strat ) {
    data.long$Area <- data.long$SID
  } else {
    data.long$Area <- rep( params$region, times=dim(data.long)[1] )
  }
  data.long$value <- data.long$value / 1000
  data.long$value.PSE <- data.long$value.PSE / 1000
  
  if( length( strat[ strat %notin% c('Year','SID') ] ) > 0 ) {
    dummy.strat = strat[ strat %notin% c('Year','SID') ]
    command.line = paste0( "data.long = data.long %>% mutate( GROUPING = paste( ",
                           paste( dummy.strat, collapse=', '),", sep='_' ) )" )
    eval( parse( text = command.line ) )
    rm( dummy.strat, command.line )
  } else {
    data.long = data.long %>% mutate( GROUPING = Area )
  }
  
  n.years <- length( unique(data.long$Year) )
  
  ### In creating my plots, note that I borrow much of the formatting from Kyle's (and Rich) cv plots...
  Fig11.comb <- ggplot( data=data.long, aes( x=Year, colour=variable ) ) +
    geom_line( aes( y=value ), linewidth=1.2 ) +
    geom_errorbar( aes( ymin=pmax(value-value.PSE,0), ymax=value+value.PSE ),
                   width=0.25, size=1, position=position_dodge(width=0.2) ) +
    ###       ...where position_dodge() preserves the vertical position of a geom (in ggplot) while adjusting
    ###           the horizontal position to avoid overlapping objects...
    
    labs( title = "",   # title = paste0( "Uncertainty in Catch Estimates - ", params$species.name ),
          x="Year", y="Thousands of Fish" ) +
    ###       ...where I don't bother with a title as figures are described in the caption (see below)...
    expand_limits(y = 0) +
    # scale_y_continuous(breaks = scales::pretty_breaks(n = 10), labels = scales::comma) +
    theme_bw() +
    scale_color_manual( values=c('darkblue','springgreen4') ) +
    theme( text = element_text(size = 11),
           axis.text.x = element_text(angle = 90, vjust=0.5),
           # plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
           legend.position = "bottom",
           # panel.background = element_blank(),
           panel.grid.major = element_line(colour = "grey", linewidth = 0.5),
           panel.grid.minor = element_line(colour = "grey", linewidth = 0.2),
           panel.border = element_rect(colour = "black", fill = NA) )
  
  if( 'SID' %in% strat ) {
    Fig11.comb = Fig11.comb + scale_x_continuous( breaks = scales::pretty_breaks( n = (n.years/5) ) )
  } else {
    Fig11.comb = Fig11.comb + scale_x_continuous( breaks = scales::pretty_breaks( n = (n.years/2) ) )
  }
  
  if( length( strat[ strat %notin% c('Year','SID') ] ) > 0 ) {
    Fig11.comb <- Fig11.comb + facet_grid( GROUPING ~ Area, scales = "free" )
  } else {
    Fig11.comb <- Fig11.comb + facet_grid( variable ~ Area, scales = "free" )
  }
  
  rm( data.long, n.years )
  
  
  return.object = Fig11.comb
  rm(Fig11.comb)
  
  return( return.object )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------

FIG.catch.sedarcomp = function( my.data, params ) {
  ###       ...where "my.data" is the data table with catch estimates from different SEDARs...
  ###       and 'params' the R object that (amongst other things) identifies the strata associated with this
  ###               particular data product (e.g., region-of-interest for this SEDAR )...
  
  
  dat.long = sedar.comp %>%
    pivot_longer( cols = colnames(sedar.comp)[ grepl( "AB1",colnames(sedar.comp) ) | grepl( "B2",colnames(sedar.comp) ) ],
                  names_to="SEDAR" ) %>%
    separate( col="SEDAR", into=c("SEDAR","Metric"), sep="_" ) %>%
    mutate_at( vars( Year, value ), list( ~ as.numeric(.) ) ) %>%
    mutate( Area = params$region,
            value = value / 1000 )
  
  n.years <- length( unique(dat.long$Year) )
  
  dummy = dat.long[ !is.na(dat.long$value), ]
  term.year = lapply( unique(dummy$SEDAR), function(x) dummy[ dummy$SEDAR %in% x , ] )
  term.year = unlist( lapply( term.year, function(x) max(x$Year) ) )
  rm( dummy )
  
  Fig3.comb <- ggplot( data=dat.long, aes( x=Year, colour=SEDAR, linetype=SEDAR ) ) +
    geom_line( aes( y=value ), linewidth=1.2 ) +
    facet_grid( Metric ~ Area, scales='free' ) +
    labs( title="", x="Year", y="Thousands of Fish" ) +
    scale_x_continuous( breaks = scales::pretty_breaks( n = (n.years/2) ) ) +
    expand_limits(y = 0) +
    theme_bw() +
    scale_color_manual( values=c('darkblue','springgreen4') ) +
    theme( text = element_text(size = 11),
           axis.text.x = element_text(angle = 90, vjust=0.5),
           legend.position = "bottom",
           panel.grid.major = element_line(colour = "grey", linewidth = 0.5),
           panel.grid.minor = element_line(colour = "grey", linewidth = 0.2),
           panel.border = element_rect(colour = "black", fill = NA) )
  rm( n.years )
  
  
  return.object = list( Fig3.comb, dat.long, term.year )
  names(return.object) = c( "plot", "data.table", "term.year" )
  rm( Fig3.comb, dat.long, term.year )
  
  return( return.object )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------

FIG.catnum.yr.strata = function( my.data, strata = c('state','mode'), fig.colors, params ) {
  ###     ...where 'my.data' is the data table with (AB1 & B2) catch estimates from different strata,
  ###           'strata' identifies the specific strata ( states or mode ) being summarized,
  ###           'fig.colors' is the chosen color palette for this particular plot,
  ###       and 'params' the R object that (amongst other things) identifies the strata associated with this
  ###                     particular data product (e.g., region-of-interest for this SEDAR )...
  
  
  strat = colnames(my.data)[ !grepl("AB1",colnames(my.data) ) & !grepl("B2",colnames(my.data) ) ]
  
  
  AB1.strata <- my.data[ , c( which( colnames(my.data) %in% strat ), grep("AB1",colnames(my.data)) ) ]
  colnames(AB1.strata) <- gsub( "_.*","",colnames(AB1.strata) )     ### ...removing "AB1"...
  B2.strata  <- my.data[ , c( which( colnames(my.data) %in% strat ), grep("B2",colnames(my.data)) ) ]
  colnames(B2.strata)  <- gsub( "_.*","",colnames(B2.strata)  )     ### ...removing "B2"...
  
  AB1.strata <- AB1.strata[ ,which( colnames(AB1.strata) != "Total" ) ]
   B2.strata <-  B2.strata[ ,which( colnames( B2.strata) != "Total" ) ]
  
  AB1.long = AB1.strata %>%
    pivot_longer( cols = !contains(strat), names_to = toupper(strata), values_to = 'value' ) %>%
    mutate( variable = "AB1" )
  B2.long =  B2.strata %>%
    pivot_longer( cols = !contains(strat), names_to = toupper(strata), values_to = 'value' ) %>%
    mutate( variable = "B2" )
  rm( AB1.strata, B2.strata )
  
  dat.long <- bind_rows( AB1.long, B2.long )
  rm( AB1.long, B2.long )
  
  
  if( 'SID' %in% strat ) {
    dat.long$Area <- dat.long$SID
  } else {
    dat.long$Area <- rep( params$region, times=dim(dat.long)[1] )
  }
  
  dat.long$value <- round( dat.long$value, digits=0 )
  dat.long$value[ is.na(dat.long$value) ] <- 0
  dat.long$value <- dat.long$value / 1000
  
  
  ###   Before we transitioned to manually-defined colors, I was applying the "Blues" & "Reds" palettes
  ###   ( from RColorBrewer ) to the 'state' and 'mode' figures respectively, which worked well in most situations
  ###   but did lead to occasional issues with having "TOO MANY" strata to plot. In particular, for those SEDARs
  ###   spanning multiple sub-regions, the number of states needing to be summarized would sometimes exceed
  ###   that allowed by the palette (the "Blues" palette has a cut-off of n=9) or lead to only slight differences
  ###   in colors/hues between strata, making it difficult to distinguish states in the final figures.
  ###   For these situations, the codes was updated to :
  ### 
  ###     (1) Evaluate if any states could be combined into SUB_REG categories, for which I would combine
  ###         all states from any regions where the relative catch (of AB1 & B2) was less than 10% of TOTAL catch.
  ###         Note that this adjustment is only applied when the number of strata exceeds 9, which should only
  ###         apply when NATL & MATL areas are considered (i.e., GOM has 5 states & SATL has 4 states, so even
  ###         GOM+SATL assessments shouldn't exceed n.states > 9 ).
  ###     (2) Impute additional levels into the "Blues" & "Reds" palettes (for my plot), which was obviously not
  ###         the preferred option as it was still difficult to distinguish states via their assigned colors/hues.
  
  
  ###   Adjustment (1) - Combine States into SUB_REG
  if( strata == 'state' ) {
    if( length( unique(dat.long$STATE) ) > 9 ) {
      dat.long$SUB_REG = as.character( dat.long$STATE )
      dat.long$SUB_REG[ dat.long$SUB_REG %in% c("TX","LA","MS","AL","FLW") ] = "GOA"
      dat.long$SUB_REG[ dat.long$SUB_REG %in% c("FLE","GA","SC","NC","SNC","NNC") ] = "SATL"
      dat.long$SUB_REG[ dat.long$SUB_REG %in% c("VA","MD","DE","PA","NJ","NY") ] = "MATL"
      dat.long$SUB_REG[ dat.long$SUB_REG %in% c("CT","RI","MA","NH","ME") ] = "NATL"
      dat.long$SUB_REG[ dat.long$SUB_REG %in% c("PR","VI") ] = "CAR"
      
      dat.summary = dat.long %>%
        group_by( SUB_REG, variable ) %>%
        summarise( catch = sum( value, na.rm=TRUE ) ) %>%
        pivot_wider( values_from=catch, names_from=variable )
      dat.summary$p.AB1 = dat.summary$AB1 / sum(dat.summary$AB1)
      dat.summary$p.B2  = dat.summary$B2  / sum(dat.summary$B2 )
      
      if( any( dat.summary$p.AB1 < 0.1 & dat.summary$p.B2 < 0.1 ) ) {
        combine.region = dat.summary$SUB_REG[ dat.summary$p.AB1 < 0.1 & dat.summary$p.B2 < 0.1 ]
        
        dat.long$STATE = as.character(dat.long$STATE)
        dat.long$STATE[ dat.long$SUB_REG %in% combine.region ] = dat.long$SUB_REG[ dat.long$SUB_REG %in% combine.region ]
        factor.levels = c(strata,combine.region)[ match( unique(dat.long$STATE), c(strata,combine.region) ) ]
        dat.long$STATE = factor( dat.long$STATE, levels=factor.levels )
      }
      rm( dat.summary, combine.region, factor.levels )
    }
  }
  
  ###   Adjustment (2) - Impute Additional Colors into "Blues" color palette (in RColorBrewer)
  ###
  ###     ...which only allows for 9 factors and so any levels of STATE beyond n=9 would not be assigned a color
  ###     (i.e., blank color ). The fix here was to expand the palette, interpolating a distinct blue value for any
  ###     additional levels of STATE ( using the colorRampPalette() function -- see 'else' statement below ),
  ###     but we have since moved away from this palette in favor of a manually-defined color wheel. However,
  ###     the script for this adjustment is commented out (below) for future reference...
  ### 
  ### # if( length(unique(dat.long$STATE)) > 9 ) {
  ### #   colorCount = length(unique(dat.long$STATE))
  ### #   getPalette = colorRampPalette( brewer.pal(9,"Blues") )
  ### #   ###   ...where to expand the 'Blues' color palette, I followed the approach at:
  ### #   ###       https://www.r-bloggers.com/2013/09/how-to-expand-color-palette-with-ggplot-and-rcolorbrewer/
  ### # }
  
  if( strata == 'state' ) {
    col.levels = c( 'TX','LA','MS','AL','FLW','FLKeys','GOA',
                    'FLE','GA','SC','NC','SNC','NNC','SATL',
                    'VA','MD','DE','PA','NJ','NY','MATL',
                    'CT','RI','MA','NH','ME','NATL',  'PR','CAR' )
    col.levels = col.levels[ col.levels %in% unique(dat.long$STATE) ]
    dat.long = dat.long %>% mutate( STATE = factor( STATE, levels = col.levels ) )
    rm( col.levels )
  }
  if( strata == 'mode' ) {
    col.levels = c( 'Cbt','CbtHbt','Hbt','Priv','PrivShore','Shore' )
    col.levels = col.levels[ col.levels %in% unique(dat.long$MODE) ]
    dat.long = dat.long %>% mutate( MODE = factor( MODE, levels = col.levels ) )
    rm( col.levels )
  }
  
  
  ### ABSOLUTE BARPLOT ###
  ### --------------------
  
  n.years <- length( unique(dat.long$Year) )
  
  if( strata == "state" ) {
    Fig4.comb <- ggplot( data=dat.long, aes( x=Year, y=value, fill=STATE) )
  } else if( strata == "mode" ) {
    Fig4.comb <- ggplot( data=dat.long, aes( x=Year, y=value, fill=MODE) )
  }
  Fig4.comb <- Fig4.comb +
    geom_col( position = "stack", colour="black" ) +     ### ...where colour="black" draws black boxes around each area...
    # geom_col( position = "stack" ) +                   ### ...with no boxes...
    facet_grid( variable ~ Area, scales = "free" ) +
    labs( title="", x="Year", y="Thousands of Fish" ) +
    
    # scale_fill_brewer( palette="Blues") +
    # scale_fill_manual( values = getPalette(colorCount) ) +
    #  ### ...more palettes at http://www.sthda.com/english/wiki/ggplot2-colors-how-to-change-colors-automatically-and-manually
    # scale_fill_manual( values = c("deeppink","yellow","firebrick1","darkorchid1","turquoise","green4","brown4",
    #                               "deepskyblue4","orange","lightcoral","mediumorchid","olivedrab2","gray70") ) +
    scale_fill_manual( values = fig.colors ) +
    theme_bw() +
    theme( text = element_text(size = 11),
           axis.text.x = element_text(angle = 90, vjust=0.5),
           legend.position = "bottom",
           panel.grid.major = element_line(colour = "grey", size = 0.5),
           panel.grid.minor = element_line(colour = "grey", size = 0.2),
           panel.border = element_rect(colour = "black", fill = NA) )
  
  if( 'SID' %in% strat ) {
    Fig4.comb <- Fig4.comb + scale_x_continuous( breaks = scales::pretty_breaks( n = n.years/5 ) )
  } else {
    Fig4.comb <- Fig4.comb + scale_x_continuous( breaks = scales::pretty_breaks( n = n.years/2 ) )
  }
  
  ### RELATIVE BARPLOT ###
  ### --------------------
  
  if( strata == "state" ) {
    Fig4a.bars <- ggplot( data=dat.long, aes( x=Year, y=value, fill=STATE) )
  } else if( strata == "mode" ) {
    Fig4a.bars <- ggplot( data=dat.long, aes( x=Year, y=value, fill=MODE) )
  }
  Fig4a.bars <- Fig4a.bars +
    geom_bar( position = "fill", stat="identity", colour="black" ) +
    facet_grid( variable ~ Area ) +
    labs( title="", x="Year", y="Proportion of Catch (Number of Fish)" ) +
    scale_fill_manual( values = fig.colors ) +
    theme_bw() +
    theme( text = element_text(size = 11),
           axis.text.x = element_text(angle = 90, vjust=0.5),
           # legend.position = "none",
           panel.grid.major = element_line(colour = "grey", size = 0.5),
           panel.grid.minor = element_line(colour = "grey", size = 0.2),
           panel.border = element_rect(colour = "black", fill = NA) )
  
  if( 'SID' %in% strat ) {
    Fig4a.bars <- Fig4a.bars +
      scale_x_continuous( breaks = scales::pretty_breaks( n = n.years/5 ) ) +
      theme( legend.position = "bottom" )
  } else {
    Fig4a.bars <- Fig4a.bars +
      scale_x_continuous( breaks = scales::pretty_breaks( n = n.years/2 ) ) +
      theme( legend.position = "none" )
  }
  
  
  ### RELATIVE PIE CHARTS ###
  ### -----------------------
  
  if( strata == "state" ) {
    df <- dat.long %>%
      group_by(STATE,variable) %>%
      summarize( prop = sum( as.numeric(value), na.rm=TRUE ) )
    Fig4a.pie.AB1 <- ggplot( data=df[ which(df$variable == "AB1"), ], aes( x="", y=prop, fill=STATE) )
    Fig4a.pie.B2  <- ggplot( data=df[ which(df$variable ==  "B2"), ], aes( x="", y=prop, fill=STATE) )
    
  } else if( strata == "mode" ) {
    df <- dat.long %>%
      group_by(MODE,variable) %>%
      summarize( prop = sum( as.numeric(value), na.rm=TRUE ) )
    Fig4a.pie.AB1 <- ggplot( data=df[ which(df$variable == "AB1"), ], aes( x="", y=prop, fill=MODE) )
    Fig4a.pie.B2  <- ggplot( data=df[ which(df$variable ==  "B2"), ], aes( x="", y=prop, fill=MODE) )
  }
  
  ### AB1 Pie ###
  Fig4a.pie.AB1 <- Fig4a.pie.AB1 +
    geom_bar( width=1, stat="identity" ) + coord_polar( "y", start=0 ) + theme_void() +
    scale_fill_manual( values = fig.colors ) +
    geom_label_repel( aes( label=paste0( round( prop/sum(prop)*100, 1 ),"%" ) ),
                      position=position_stack(vjust=0.5), show.legend = FALSE, force=5 )
  
  if( 'SID' %in% strat ) {
    Fig4a.pie.AB1 <- Fig4a.pie.AB1 + theme( legend.position = "none" )
    ###   ...for which the legend has been moved to under the relative bar chart (i.e., 'Fig4a.bars' )...
  }
  
  ###   Originally, I tried to include STATE as part of the label (e.g., FLE = 25% ), but the resultant
  ###   labels were too large for the working paper and so I ultimately just stuck with the percentages.
  ###   However, for future reference, I retained the code...
  ###
  ### #  geom_label_repel( aes( label=paste0( STATE," = ",round( prop/sum(prop)*100, 1 ),"%" ) ),
  ### #                    position=position_stack(vjust=0.5), show.legend = FALSE )
  
  ### B2 Pie ###
  Fig4a.pie.B2 <- Fig4a.pie.B2 +
    geom_bar( width=1, stat="identity" ) + coord_polar( "y", start=0 ) + theme_void() +
    scale_fill_manual( values = fig.colors ) +
    geom_label_repel( aes( label=paste0( round( prop/sum(prop)*100, 1 ),"%" ) ),
                      position=position_stack(vjust=0.5), show.legend = FALSE, force=5 )
  
  if( 'SID' %in% strat ) {
    Fig4a.pie.B2 <- Fig4a.pie.B2 + theme( legend.position = "none" )
    ###   ...for which the legend has been moved to under the relative bar chart (i.e., 'Fig4a.bars' )...
  }
  
  ### ### ...for which I validate the labels by calculating the percentages manually...
  ### # AB1.labels <- vector( mode="double", length=length( unique(dat.long$STATE) ) )
  ### # names(AB1.labels) <- unique(dat.long$STATE)
  ### # B2.labels <- AB1.labels
  ### # for( i in 1:length(AB1.labels) ) {
  ### #   AB1.labels[i] <- sum( dat.long$value[ which( dat.long$variable=="AB1" & dat.long$STATE == names(AB1.labels)[i] ) ] )
  ### #    B2.labels[i] <- sum( dat.long$value[ which( dat.long$variable== "B2" & dat.long$STATE == names( B2.labels)[i] ) ] )
  ### # }
  ### # AB1.labels <- AB1.labels / sum(AB1.labels)
  ### #  B2.labels <-  B2.labels / sum( B2.labels)
  
  rm( n.years )
  
  
  return.object = list( Fig4.comb, Fig4a.bars, Fig4a.pie.AB1, Fig4a.pie.B2 )
  names(return.object) = c( "abs.bars", "rel.bars", "rel.pie.AB1", "rel.pie.B2" )
  rm( Fig4.comb, Fig4a.bars, Fig4a.pie.AB1, Fig4a.pie.B2 )
  
  return( return.object )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------

FIG.landings.timeseries = function( my.data, params ) {
  ###       ...where "my.data" is the full data table of GenRec catch estimates at the finest resolution...
  ###       and 'params' the R object that (amongst other things) identifies the weight metric requested
  ###               for this particular assessment (i.e., whole vs. gutted weight )...
  
  
  ###   This function requires time series for (1) total Landings (num), (2) total Landings (wgt), and
  ###     (3) average fish weight, each of which are constructed below from our 'my.data' table...
  
  
  strata.vec = colnames(my.data)[ colnames(my.data) %in% c('Year','SID') ]
  
  ### Landings in Number ###
  ### ----------------------
  LandNum <- my.data %>%
    # group_by( Year ) %>%
    group_by( across( any_of( strata.vec ) ) ) %>%
    summarize( AB1 = sum( as.numeric(AB1), na.rm=TRUE ) )
  
  
  ### Landings in Weight ###
  ### ----------------------
  if( params$wgt.metric == "whole weight" ) {
    if( "lbsest_SECwwt" %in% colnames(my.data) ) {
      LandWgt <- my.data %>%
        # group_by( Year ) %>%
        group_by( across( any_of( strata.vec ) ) ) %>%
        summarize( lbs = sum( as.numeric(lbsest_SECwwt), na.rm=TRUE ) )
    } else {
      LandWgt <- my.data %>%
        # group_by( Year ) %>%
        group_by( across( any_of( strata.vec ) ) ) %>%
        summarize( lbs = sum( as.numeric(lbsest_SEC), na.rm=TRUE ) )
    }
  } else if( params$wgt.metric == "gutted weight" ) {
    if( "lbsest_SECgwt" %in% colnames(my.data) ) {
      LandWgt <- my.data %>%
        # group_by( Year ) %>%
        group_by( across( any_of( strata.vec ) ) ) %>%
        summarize( lbs = sum( as.numeric(lbsest_SECgwt), na.rm=TRUE ) )
    } else {
      LandWgt <- my.data %>%
        # group_by( Year ) %>%
        group_by( across( any_of( strata.vec ) ) ) %>%
        summarize( lbs = sum( as.numeric(lbsest_SEC), na.rm=TRUE ) )
    }
  }
  
  
  ### SEFSC Average (Fish) Weights ###
  ### --------------------------------
  comb.data = LandNum %>%
    full_join( LandWgt, by = strata.vec ) %>%
    mutate( Mean = ifelse( AB1>0, lbs/AB1, NA ) )
    ###   ...where setting AvgWgt = <NA> (i.e., not plotted ) to prevent forcing the AvgWgt trendline to
    ###     zero in years/strata where nothing was caught (i.e., AvgWgt isn't zero, just no data for it )...
  rm( LandNum, LandWgt )
  
  
  
  ### PLOTTING ###
  ### ------------
  
  comb.data = comb.data %>%
    mutate( AB1 = AB1 / 1000,
            lbs = lbs / 1000 ) %>%
    pivot_longer( cols = !contains(strata.vec), names_to = 'Param', values_to = 'Value' ) %>%
    mutate( Order = ifelse( Param == "AB1", "A",
                    ifelse( Param == "lbs", "B",
                    ifelse( Param == "Mean","C", NA ))) ) %>%
    mutate( Param = ifelse( Param == "AB1", "AB1 (1000s Fish)",
                    ifelse( Param == "lbs", "AB1 (1000s lbs)",
                    ifelse( Param == "Mean","AvgWgt (lbs)", NA ))) )
  
  ###   ...where 'Param' contains the title for each of my lines and 'Order' the code that I apply
  ###       to ensure plots are ordered appropriately (i.e., I want AB1.num on top, than AB1.wgt, then AvgWgt );
  ###       which is needed b/c facet_wrap sorts plots using alphabetical order ( of 'Params' in this case ).
  ### For this approach, I also need to define a character vector to convert the numeric codes ( in 'Order' )
  ###       to their appropriate text ( in 'Param' ), for which I identify the corresponding values...
  plot.names <- unique( comb.data[ ,c("Order","Param") ] )
  ###       ...and use the eval(parse(text= ) ) trick to define my vector from "plot.names"...
  vec.names <- vector( "character" )
  for( i in 1:dim(plot.names)[1] ) {
    eval( parse( text=paste0( "vec.names <- c( vec.names, ",plot.names[i,1]," = '",plot.names[i,2],"' )" ) ) )
  }
  rm( i )
  
  
  n.years <- length( unique(comb.data$Year) )
  
  Fig6.AB1 <- ggplot( comb.data, aes( x=Year, y=Value, colour=Param ) ) +
    geom_line( linewidth=1.2 ) +
    labs( title="", x="Year", y="" ) +
    expand_limits(y=0) +
    theme_bw() +
    scale_color_manual( values = c('deeppink','springgreen4','darkblue') ) +
    theme( text = element_text(size = 11),
           axis.text.x = element_text(angle = 90, vjust=0.5),
           legend.position = "bottom",
           panel.grid.major = element_line(colour = "grey", linewidth = 0.5),
           panel.grid.minor = element_line(colour = "grey", linewidth = 0.2),
           panel.border = element_rect(colour = "black", fill = NA) )
  
  if( "SID" %in% strata.vec ) {
    Fig6.AB1 = Fig6.AB1 +
      scale_x_continuous( breaks = scales::pretty_breaks( n = (n.years/5) ) ) +
      facet_grid( Order ~ SID, scales="free", labeller = labeller( Order = vec.names ) )
      ###     ...where I plot by 'Order' to make sure figures are sorted the way I want,
      ###       but then label these facets based on the 'Order-Param' relationship defined in "vec.names"...
  } else {
    Fig6.AB1 = Fig6.AB1 +
      scale_x_continuous( breaks = scales::pretty_breaks( n = (n.years/2) ) ) +
      facet_grid( Order ~., scales="free", labeller = labeller( Order = vec.names ) )
  }
  rm( comb.data, n.years, plot.names, vec.names )
  
  
  return.object = Fig6.AB1
  rm(Fig6.AB1)
  
  return( return.object )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------

FIG.discards.timeseries = function( my.data, allow.rescale.DISCpanel = TRUE, params ) {
  ###       ...where "my.data" is the full data table of GenRec catch estimates at the finest resolution...
  ###           'allow.rescale.DISCpanel' is a flag meant to identify whether the 'DISCARD RATE' panel
  ###               in the final figure should include a check for "outliers"...
  ###       and 'params' the R object that (amongst other things) identifies the weight metric requested
  ###               for this particular assessment (i.e., whole vs. gutted weight )...
  
  
  ###   This function requires time series for (1) total Landings (num), (2) total discards (num),
  ###     (3) dead discards (num), calculated as total discards * params$discard.mortality, and
  ###     (4) discard rate, calculated as total discards / landings. Each of these timeseries are
  ###   constructed below ( from our 'my.data' table object )...
  
  
  ###   As a first step in this function, I identify the strata at which the plot is to be printed.
  ###   In particular, if discard mortality rates are being imported as a table, which are joined
  ###   with our 'DiscNum' table, the strata in the DiscMort table will define the strata at which
  ###   results are printed. In other cases (e.g., wherein a single DiscMort rate assumed ),
  ###   we only check to see if 'Year' & 'SID' strata exist in the table, and summarize as such if so...
  
  if( params$discard.mortality == 'Table' ) {
    mort.table = read.csv( paste0( params$discard.mortality.dir,"/",params$discard.mortality.table ) )
    colnames(mort.table) = toupper(colnames(mort.table))
    
    if( 'Year' %in% colnames(my.data) ){
      mort.table = mort.table %>% rename( any_of( c( Year = "YEAR" ) ) )
    }
    
    strata.vec = colnames(mort.table)[ colnames(mort.table) %notin% c('DISCMORT') ]
    ###   ...and as a check that all fields in 'strata.vec' are also in our catch table (i.e., in 'my.data' ),
    ###     which is meant to safeguard against cases where additional strata are included in 'mort.table'
    ###     but not needed for the subsequent JOIN in this function (e.g., if 'mort.table' has
    ###     multiple 'YEAR' values are grouped into 'BLOCKS', but for which only 'YEAR' is needed in the
    ###     join between 'mort.table' and 'my.data' )...
    strata.vec = strata.vec[ strata.vec %in% colnames(my.data) ]
    
  } else {
    strata.vec = colnames(my.data)[ colnames(my.data) %in% c('Year','SID') ]
  }
  
  ###   ...changing any LACR estimates for the combined 'Priv/Shore' mode into private estimates...
  if( 'NEW_MODEN' %in% strata.vec ) {
    format.table = my.data %>% mutate( NEW_MODEN = ifelse( NEW_MODEN == "Priv/Shore", "Priv", NEW_MODEN ) )
  }
  if( 'NEW_MODE' %in% strata.vec ) {
    format.table = my.data %>% mutate( NEW_MODE = ifelse( NEW_MODE == "6", "4", NEW_MODE  ) )
  }
  
  if( 'Year' %notin% strata.vec ) {
    col.vec = c( 'Year',strata.vec )
  } else {
    col.vec = strata.vec
  }
  
  
  ### Landings in Number ###
  ### ----------------------
  
  LandNum <- format.table %>%
    group_by( across( any_of( col.vec ) ) ) %>%
    summarize( AB1 = sum( as.numeric(AB1), na.rm=TRUE ) ) %>%
    mutate( AB1 = AB1 / 1000 )
  
  
  ### Discards in Number ###
  ### ----------------------
  
  DiscNum <- format.table %>%
    group_by( across( any_of( col.vec ) ) ) %>%
    summarize( B2 = sum( as.numeric(B2), na.rm=TRUE ) ) %>%
    mutate( B2 = B2 / 1000 )
  
  
  ### Dead Discards ###
  ### -----------------
  
  if( params$discard.mortality != 'None' ) {
    
    ### (Discard) Mortality Rates imported from CSV Table ###
    if( params$discard.mortality == 'Table' ) {
      DiscDead <- DiscNum %>% left_join( mort.table, by = strata.vec )
      # summary( as.numeric(DiscDead$DISCMORT) )
      
    ### Single (Discard) Mortality Rate from 'params' object ###
    } else {
      DiscDead <- DiscNum %>% mutate( DISCMORT = as.numeric(params$discard.mortality) )
    }
    
    ###   ...quick check that 'DISCMORT' isn't input as a percentage (e.g., 35% changed to 0.35 )...
    if( max(DiscDead$DISCMORT) > 1 ) {
      DiscDead <- DiscDead %>% mutate( DISCMORT = DISCMORT / 100 )
    }
    DiscDead = DiscDead %>%
      mutate( B2.dead = B2 * DISCMORT ) %>%
      select( -c(B2,DISCMORT) ) %>%
      rename( B2 = B2.dead )
    
    ###   ...and adding any factor levels back into 'DiscDead', which may have been lost on join with 'mort.table'...
    if( 'SID' %in% colnames(DiscDead) ) {
      DiscDead = DiscDead %>% mutate( SID = factor( SID, levels = levels( DiscNum$SID ) ) )
    }
    if( 'FED_CLOSED' %in% colnames(DiscDead) ) {
      DiscDead = DiscDead %>% mutate( FED_CLOSED = factor( FED_CLOSED, levels = levels( DiscNum$FED_CLOSED ) ) )
    }
  }
  
  # ### Discard Rate ###
  # ### ----------------
  # 
  # DiscRate <- full_join( LandNum, DiscNum, by=col.vec ) %>%
  #   mutate( Rate = ifelse( AB1==0, NA, B2/AB1 ) ) %>%
  #   select( -c(AB1,B2) )
  
  
  
  ### Combining Estimates into a single table ###
  ### -------------------------------------------
  
  ### Panel 1 -- Total Discards ###
  blah1 = LandNum %>%
    mutate( CAT_VAR = 'LANDINGS (AB1)',
            METHOD  = 'CATCH (1000s Fish)' ) %>%
    rename( value = AB1 )
  blah2 = DiscNum %>%
    mutate( CAT_VAR = 'TOTAL DISCARDS (B2)',
            METHOD  = 'CATCH (1000s Fish)' ) %>%
    rename( value = B2 )
  data1 = bind_rows( blah1, blah2 )
  rm( blah1, blah2 )
  ### ...and for cases where multiple strata are retained (i.e., in 'col.vec' ),
  ###   a final grouped summary is done to sum across all strata except 'Year' & 'SID'...
  data1 = data1 %>%
    group_by( across( any_of( c('Year','SID','CAT_VAR','METHOD') ) ) ) %>%
    summarize( value = sum( value, na.rm=TRUE ) ) %>%
    arrange( across( any_of( c( 'SID','Year','METHOD','CAT_VAR' ) ) ) )
  
  
  ### Panel 2 -- Discard Rate ###
  data2 = data1 %>%
    pivot_wider( names_from = 'CAT_VAR', values_from = 'value' ) %>%
    mutate( Rate = ifelse( `LANDINGS (AB1)` > 0, `TOTAL DISCARDS (B2)` / `LANDINGS (AB1)`, NA ) ) %>%
    select( !contains( c('LANDINGS (AB1)','DISCARDS (B2)') ) ) %>%
    
  # data2 = DiscRate %>%
    mutate( CAT_VAR = 'DISCARD RATE',
            METHOD  = 'DISCARD RATE' ) %>%
    rename( value = Rate )
  
  
  ### Panel 3 -- Dead Discards ###
  if( params$discard.mortality != 'None' ) {
    
    blah1 = LandNum %>%
      mutate( CAT_VAR = 'LANDINGS (AB1)',
              METHOD  = 'REMOVALS (1000s Fish)' ) %>%
      rename( value = AB1 )
    blah2 = DiscDead %>%
      mutate( CAT_VAR = 'DEAD DISCARDS',
              METHOD  = 'REMOVALS (1000s Fish)' ) %>%
      rename( value = B2 )
    data3 = bind_rows( blah1, blah2 )
    rm( blah1, blah2 )
    
    data3 = data3 %>%
      group_by( across( any_of( c('Year','SID','CAT_VAR','METHOD') ) ) ) %>%
      summarize( value = sum( value, na.rm=TRUE ) ) %>%
      arrange( across( any_of( c( 'SID','Year','METHOD','CAT_VAR' ) ) ) )
    
    
  ### JOINING ALL THREE PANELS INTO A SINGLE 'DUMMY.TABLE' ###
    dummy.table = bind_rows( data1, data2, data3 )
    rm( data1, data2, data3 )
    rm( LandNum, DiscNum, DiscDead )
    
    dummy.table = dummy.table %>%
      mutate( CAT_VAR = factor( CAT_VAR, levels=c('LANDINGS (AB1)','TOTAL DISCARDS (B2)',
                                                  'DEAD DISCARDS','DISCARD RATE') ),
              METHOD  = factor( METHOD,  levels=c('CATCH (1000s Fish)',
                                                  'REMOVALS (1000s Fish)','DISCARD RATE') ) )
    
  } else {
    dummy.table = bind_rows( data1, data2 )
    rm( data1, data2 )
    rm( LandNum, DiscNum )
    
    dummy.table = dummy.table %>%
      mutate( CAT_VAR = factor( CAT_VAR, levels=c('LANDINGS (AB1)',
                                                  'TOTAL DISCARDS (B2)','DISCARD RATE') ),
              METHOD  = factor( METHOD,  levels=c('CATCH (1000s Fish)','DISCARD RATE') ) )
  }
  
  
  
  ### Plotting ###
  ### ------------
  
  n.years <- length( unique(dummy.table$Year) )
  
  ###   ...which includes a check that there isn't a year(s) with "extreme" Discard Rates
  ###     ( defined as larger than twice the 95% quantile ), in which case the y-axis of the
  ###     Discard Rate facet can be adjusted to allow for evaluation of the non-extreme values..
  DR.dummy = dummy.table %>% filter( METHOD == 'DISCARD RATE' )
  DR.dummy = DR.dummy$value
  
  # DR.quant = quantile( DR.dummy, probs = 0.99, na.rm=TRUE )
  DR.quant = quantile( DR.dummy, probs = 0.95, na.rm=TRUE )
  
  
  ### ...with 'EXTREME' DISCARD RATES ###
  if( allow.rescale.DISCpanel & max( DR.dummy, na.rm=TRUE ) > 2*DR.quant ) {
    ###   ...for which the easiest solution (in adjusting the y-axis for a single facet)
    ###     is to separate that data from the others, plot them, & recombine with gridArrange()...

    Fig7.flag = TRUE


    ### Plot for Total Catch (and Removals) ###
    blah1 = dummy.table %>% filter( METHOD != 'DISCARD RATE' )

    dummy.plot1 = ggplot( data=blah1, aes( x=Year, colour=CAT_VAR ) ) +
      geom_line( aes( y=value ), linewidth=1.2 ) +
      labs( title="", x="Year", y="" ) +
      # labs( title="", x="Year", y="Thousands of Fish" ) +
      scale_color_manual( values = c('darkblue','springgreen4','deeppink') ) +

      expand_limits(y = 0) +
      theme_bw() +
      theme( text = element_text(size = 11),
             # axis.text.x = element_text(angle = 90, vjust=0.5),
             axis.title.x = element_blank(),
             axis.text.x = element_blank(),
             
             legend.position = "none",
             plot.margin = unit( c(1,1,0,1), "mm" ),
             ###    ...no whitespace printed at bottom of plot ( unit( c(top,right,bottom,left) ) )...
             panel.grid.major = element_line(colour = "grey", linewidth = 0.5),
             panel.grid.minor = element_line(colour = "grey", linewidth = 0.2),
             panel.border = element_rect(colour = "black", fill = NA) )
    
    if( 'SID' %in% colnames(dummy.table) ) {
      dummy.plot1 = dummy.plot1 +
        scale_x_continuous( breaks = scales::pretty_breaks( n = (n.years/5) ) ) +
        facet_grid( METHOD ~ SID, scales='free' )
    } else {
      dummy.plot1 = dummy.plot1 +
        scale_x_continuous( breaks = scales::pretty_breaks( n = (n.years/2) ) ) +
        facet_grid( METHOD ~ ., scales='free' )
        # facet_wrap( vars(METHOD), scales='free', ncol=1 )
    }
    rm( blah1 )


    ### Plot for Discard Rate ###
    blah2 = dummy.table %>% filter( METHOD == 'DISCARD RATE' )

    # ###   ...using BREAK POINTS, for which we identify appropriate (y-axis) break points by:
    # ###         (1) plotting DISCARD RATES without the 'outliers', for the 'minimum' break point
    # ###         (2) identifying the smallest 'outlier' in DISCARD RATES
    # min.break = ggplot( data=blah2 %>% filter( value < DR.quant),
    #                     aes( x=Year, colour=CAT_VAR ) ) +
    #   geom_line( aes( y=value ), linewidth=1.2 ) +
    #   scale_x_continuous( breaks = scales::pretty_breaks( n = (n.years/2) ) )
    # min.break = layer_scales( min.break )$y$range$range[2]
    # max.break = min( blah2$value[ blah2$value > DR.quant ], na.rm=TRUE )
    # new.ybreaks = c( ceiling(min.break), floor(max.break)*0.95 )
    # rm( min.break, max.break )
    #
    # dummy.plot2 = ggplot( data=blah2, aes( x=Year, colour=CAT_VAR ) ) +
    #   geom_line( aes( y=value ), linewidth=1.2 ) +
    #   facet_grid( METHOD ~ ., scales='free' ) +
    #   labs( title="", x="Year", y="" ) +
    #   scale_x_continuous( breaks = scales::pretty_breaks( n = (n.years/2) ) ) +
    #   scale_y_break( new.ybreaks ) +
    #   # scale_y_break( new.ybreaks, scales = 'free' ) +
    #   scale_color_manual( values = c('gray50') ) +
    #
    #   # expand_limits(y = 0) +
    #   theme_bw() +
    #   theme( text = element_text(size = 11),
    #          axis.text.x = element_text(angle = 90, vjust=0.5),
    #          legend.position = "none",
    #          panel.grid.major = element_line(colour = "grey", linewidth = 0.5),
    #          panel.grid.minor = element_line(colour = "grey", linewidth = 0.2),
    #          panel.border = element_rect(colour = "black", fill = NA) )
    # rm( blah2, new.ybreaks )

    
    ###   ...by ZOOMING the plot dimensions into the data (i.e., to ignore the 'outliers' ),
    ###     whereby the new y-axis range is identified by R from a plot without the 'outliers'...
    # quant.99 = quantile( DR.dummy, probs = 0.99, na.rm=TRUE )
    # new.ylims = ggplot( data=blah2 %>% filter( value < quant.99),
    new.ylims = ggplot( data=blah2 %>% filter( value < DR.quant),
                        aes( x=Year, colour=CAT_VAR ) ) +
      geom_line( aes( y=value ), linewidth=1.2 ) +
      scale_x_continuous( breaks = scales::pretty_breaks( n = (n.years/2) ) )
    new.ylims = layer_scales( new.ylims )$y$range$range

    dummy.plot2 = ggplot( data=blah2, aes( x=Year, colour=CAT_VAR ) ) +
      geom_line( aes( y=value ), linewidth=1.2 ) +
      labs( title="", x="Year", y="" ) +
      coord_cartesian( ylim = new.ylims ) +
      scale_color_manual( values = c('gray50') ) +

      expand_limits(y = 0) +
      theme_bw() +
      theme( text = element_text(size = 11),
             strip.text.x = element_blank(),
             ###    ...which removes the x-axis labels from FACET_GRID()...
             axis.text.x = element_text(angle = 90, vjust=0.5),
             legend.position = "none",
             plot.margin = unit( c(0,1,1,1), "mm" ),
             ###    ...no whitespace printed at top of plot ( unit( c(top,right,bottom,left) ) )...
             panel.grid.major = element_line(colour = "grey", linewidth = 0.5),
             panel.grid.minor = element_line(colour = "grey", linewidth = 0.2),
             panel.border = element_rect(colour = "black", fill = NA) )
    
    if( 'SID' %in% colnames(dummy.table) ) {
      dummy.plot2 = dummy.plot2 +
        scale_x_continuous( breaks = scales::pretty_breaks( n = (n.years/5) ) ) +
        facet_grid( METHOD ~ SID, scales='free' )
    } else {
      dummy.plot2 = dummy.plot2 +
        scale_x_continuous( breaks = scales::pretty_breaks( n = (n.years/2) ) ) +
        facet_grid( METHOD ~ ., scales='free' )
    }
    rm( blah2, new.ylims )

    
    
    ### 'Plot' for Legend ###
    dummy.legend = ggplot( data=dummy.table, aes( x=Year, colour=CAT_VAR ) ) +
      geom_line( aes( y=value ), linewidth=1.2 ) +
      scale_x_continuous( breaks = scales::pretty_breaks( n = (n.years/2) ) ) +
      scale_color_manual( values = c('darkblue','springgreen4','deeppink','gray50') ) +
      theme( legend.position = "bottom" )
    dummy.legend = ggpubr::get_legend( dummy.legend )
    dummy.legend = ggpubr::as_ggplot( dummy.legend )


    return.object = list( Fig7.flag, dummy.plot1, dummy.plot2, dummy.legend )
    names(return.object) = c( "flag", "TotalCatch", "DiscRate", "Legend" )
    rm( Fig7.flag, dummy.plot1, dummy.plot2, dummy.legend )
    
    
  ### ...without 'EXTREME' DISCARD RATES ###
  } else {
    
    Fig7.flag = FALSE
    
    Fig7.B2 <- ggplot( data=dummy.table, aes( x=Year, colour=CAT_VAR ) ) +
      geom_line( aes( y=value ), linewidth=1.2 ) +
      # labs( title="", x="Year", y="Thousands of Fish" ) +
      labs( title="", x="Year", y="" ) +
      scale_color_manual( values = c('darkblue','springgreen4','deeppink','gray50') ) +
      
      expand_limits(y = 0) +
      theme_bw() +
      theme( text = element_text(size = 11),
             axis.text.x = element_text(angle = 90, vjust=0.5),
             legend.position = "bottom",
             panel.grid.major = element_line(colour = "grey", linewidth = 0.5),
             panel.grid.minor = element_line(colour = "grey", linewidth = 0.2),
             panel.border = element_rect(colour = "black", fill = NA) )
    
    if( 'SID' %in% colnames(dummy.table) ) {
      Fig7.B2 = Fig7.B2 +
        scale_x_continuous( breaks = scales::pretty_breaks( n = (n.years/5) ) ) +
        facet_grid( METHOD ~ SID, scales='free' )
    } else {
      Fig7.B2 = Fig7.B2 +
        scale_x_continuous( breaks = scales::pretty_breaks( n = (n.years/2) ) ) +
        # facet_wrap( vars(METHOD), scales='free', ncol=1 )
        facet_grid( METHOD ~ ., scales='free' )
    }
    
    return.object = list( Fig7.flag, Fig7.B2 )
    names(return.object) = c( "flag", "TotalCatch" )
    rm( Fig7.flag, Fig7.B2 )
    
  }
  
  rm( dummy.table, n.years, DR.dummy, DR.quant )
  
  
  return( return.object )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------


FIG.catwgt.hier = function( my.data ) {
  ###       ...where "my.data" is the data table with SEFSC landings-in-weight estimates broken down by
  ###               the particular strata at which avgwgts were estimated (i.e., s, sr, sry, srys... )...
  
  
  avgwgt.table = my.data
  avgwgt.table$AB1.lbs = avgwgt.table$AB1.lbs / 1000
  
  colnames(avgwgt.table)[ which( colnames(avgwgt.table) == "lbsest_SECsource" ) ] = "Strata"
  
  ### Before making my summary figures, I remove any records where DS == "MRIPsub" or "TPWDsub",
  ###     which are used to identify catch estimates that were imputed to fill data gaps
  ###     (e.g., MRIP 1981 wave1, TPWD 1981-May1983, TPWD & LACreel discards ) and to which the
  ###     SEFSC avg.wgt estimation approach wasn't applied...
  avgwgt.table = avgwgt.table[ avgwgt.table$Strata %notin% c('MRIPsub','TPWDsub') , ]
  
  ### I also redefine my "Strata" field as a factor to ensure color-coding is consistent across the plots below...
  avgwgt.table$Strata = factor( avgwgt.table$Strata,
                                levels = c( "s","sr","sry","srys","srysm","srysmw","srysmwa" ) )
  
  
  ### (Absolute) Stacked Barplot ###
  ### ------------------------------
  
  n.years <- length( unique(avgwgt.table$Year) )
  
  Fig8.a = ggplot( avgwgt.table, aes( fill=Strata, y=AB1.lbs, x=Year ) ) +
    geom_col( position="stack", colour="black" ) +
    ylab( "Thousands of Pounds" ) + xlab( "Year" ) +
    # scale_fill_brewer( palette = "Blues" ) +
    scale_fill_manual( values = c("paleturquoise1","lightsteelblue1","lightblue3",
                                  "steelblue2","royalblue2","mediumblue","midnightblue") ) +
    scale_x_continuous( breaks = scales::pretty_breaks( n = (n.years/2) ) ) +
    theme_bw() +
    theme( text = element_text(size = 11),
           axis.text.x = element_blank(), axis.title.x = element_blank(),
           # legend.position = "top",
           # legend.position = "none",
           panel.grid.major = element_line(colour = "grey", size = 0.5),
           panel.grid.minor = element_line(colour = "grey", size = 0.2),
           panel.border = element_rect(colour = "black", fill = NA),
           plot.margin = unit( c(0,1,0,0.5),"cm" ) )
  ###     ...where I remove the x-axis labels, grid marks, and title ( in the theme() argument ) as
  ###         the x-axis of this plot will "share" that in "Fig1b.mode" ( constructed in code below ).
  ###         I also manipulate the margins to reduce the white-space between these two figures
  ###         ( the arguments for which define margins for the top/right/bottom/left respectively )...
  
  Fig8.legend = ggpubr::get_legend( Fig8.a )
  Fig8.legend = ggpubr::as_ggplot( Fig8.legend )
  
  Fig8.a = Fig8.a + theme( legend.position = "none" )
  
  
  # ### SAVING FILL COLORS ###
  # ### ----------------------
  # 
  # color.code <- list()
  # 
  # g <- ggplot_build( Fig8.a )
  # color.code[["SECsource"]] <- data.frame( GROUP = unique( g$data[[1]]$group ) )
  # color.code$SECsource$HEX <- g$data[[1]]$fill[ match( color.code$SECsource$GROUP,g$data[[1]]$group ) ]
  # color.code$SECsource$COLOR <- sapply( color.code$SECsource$HEX, plotrix::color.id )
  # color.code$SECsource <- color.code$SECsource[ order(color.code$SECsource$GROUP), ]
  # color.code$SECsource$NAME  <- levels(avgwgt.table$Strata)[ levels(avgwgt.table$Strata) %in% unique(avgwgt.table$Strata) ]
  # rm( g )
  
  
  ### (Relative) Stacked Barplot ###
  ### ------------------------------
  
  Fig8.b = ggplot( avgwgt.table, aes( fill=Strata, y=AB1.lbs, x=Year ) ) +
    geom_bar( position="fill", stat="identity", colour="black" ) +
    ylab( "Proportion of Catch (Pounds)" ) + xlab( "Year" ) +
    # scale_fill_manual( values = setNames( color.code$SECsource$COLOR, color.code$SECsource$NAME ) ) +
    scale_fill_manual( values = c("paleturquoise1","lightsteelblue1","lightblue3",
                                  "steelblue2","royalblue2","mediumblue","midnightblue") ) +
    scale_x_continuous( breaks = scales::pretty_breaks( n = (n.years/2) ) ) +
    theme_bw() +
    theme( text = element_text(size = 11),
           axis.text.x = element_text(angle = 90, vjust=0.5),
           legend.position = "none",
           panel.grid.major = element_line(colour = "grey", size = 0.5),
           panel.grid.minor = element_line(colour = "grey", size = 0.2),
           panel.border = element_rect(colour = "black", fill = NA),
           # plot.margin = unit( c(-0.5,1,0,0.5),"cm" ) )
           plot.margin = unit( c(0,1,0,0.25),"cm" ) )
  
  
  ### Pie Chart ###
  ### -------------
  
  df <- avgwgt.table %>%
    group_by( Strata ) %>%
    summarize( prop = sum( as.numeric(AB1.lbs), na.rm=TRUE ) )
  
  Fig8.c <- ggplot( data=df, aes( x="", y=prop, fill=Strata ) ) +
    geom_bar( width=1, stat="identity" ) + coord_polar( "y", start=0 ) + theme_void() +
    # scale_fill_manual( values = setNames( color.code$SECsource$COLOR, color.code$SECsource$NAME ) ) +
    scale_fill_manual( values = c("paleturquoise1","lightsteelblue1","lightblue3",
                                  "steelblue2","royalblue2","mediumblue","midnightblue") ) +
    geom_label_repel( aes( label=paste0( round( prop/sum(prop)*100, 1 ),"%" ) ),
                      position=position_stack(vjust=0.5), show.legend = FALSE ) +
    theme( legend.position = "none",
           plot.margin = unit( c(-1,0,-1,-1),"cm" ) )
  
  rm( avgwgt.table, df, n.years )
  # rm( color.code )
  
  
  ###   ...and to make sure that the x-axes of the two barplots are aligned properly...
  Fig8.ab = cowplot::plot_grid( Fig8.a, Fig8.b, ncol=1, align="v" )
  rm( Fig8.a, Fig8.b )
  
  
  return.object = list( Fig8.ab, Fig8.c, Fig8.legend )
  names(return.object) = c( "abs.bars", "rel.pie", "legend" )
  rm( Fig8.ab, Fig8.c, Fig8.legend )
  
  return( return.object )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------

FIG.landings.unid = function( my.data, params ) {
  ###       ...where "my.data" is the table of catch estimates for various 'identified' species groups
  ###               that could be contributing to an 'unidentified' catch estimate...
  ###       and 'params' the R object that (amongst other things) identifies the 'unidentified' taxa,
  ###               the catch estimates of which are removed from this comparison of 'identified' catch
  ###               ( across species )...
  
  
  ### (Identified) Catch by Year and Species...
  unid.table = my.data[ which( my.data$NEW_COM %notin% tolower(params$species.unid) ), ] %>%
    group_by( Year, NEW_COM ) %>%
    summarize( AB1 = sum( as.numeric(AB1), na.rm=TRUE ),
                B2 = sum( as.numeric(B2), na.rm=TRUE ) ) %>%
               # AB1.wgt = sum( as.numeric(lbsest_SECwwt), na.rm=TRUE ) ) %>%
    select( Year, NEW_COM, AB1, B2 ) %>%
    arrange( Year )
  
  ###     ...and converting NEW_COM to a factor (and renaming as "SPECIES") so the species-of-interest are plotted first...
  spp.order = c( tolower(params$species.name), tolower(params$species.add),
                 sort( unique(unid.table$NEW_COM)[ unique(unid.table$NEW_COM) %notin%
                                                   c( tolower(params$species.name), tolower(params$species.add) ) ] ) )
  spp.order = spp.order[ which( spp.order != 'none' ) ]
  
  unid.table$NEW_COM = factor( unid.table$NEW_COM, levels=spp.order )
  rm( spp.order )
  
  colnames(unid.table)[ which( colnames(unid.table) == "NEW_COM" ) ] = "SPECIES"
  
  
  ###   ...combining the AB1 & B2 fields into a single variable (i.e., convert to long-format )...
  unid.table = unid.table %>% pivot_longer( cols = c('AB1','B2'), names_to = "Catch.Var" )

  ###   ...and converting catch estimates into units of thousands (of fish)...
  unid.table$value = unid.table$value / 1000
  
  
  n.years <- length( unique(unid.table$Year) )
  
  ### (ABSOLUTE) Stacked Barplot ###
  ### ------------------------------
  
  Fig9.a = ggplot( unid.table, aes( fill=SPECIES, y=value, x=Year ) ) +
    geom_col( position="stack", colour="black" ) +
    facet_grid( . ~ Catch.Var, scales='free' ) +
    ylab( "Thousands of Fish" ) + xlab( "Year" ) +
    scale_fill_brewer( palette = "Blues" ) +
    scale_x_continuous( breaks = scales::pretty_breaks( n = (n.years/2) ) ) +
    theme_bw() +
    theme( text = element_text(size = 11),
           axis.text.x = element_blank(), axis.title.x = element_blank(),
           legend.position = "top",
           # legend.position = "none",
           panel.grid.major = element_line(colour = "grey", size = 0.5),
           panel.grid.minor = element_line(colour = "grey", size = 0.2),
           panel.border = element_rect(colour = "black", fill = NA),
           plot.margin = unit( c(0,1,0,0.5),"cm" ) )
  
  
  ### SAVING FILL COLORS ###
  ### ----------------------
  
  color.code <- list()
  
  g <- ggplot_build( Fig9.a )
  color.code[["SECsource"]] <- data.frame( GROUP = unique( g$data[[1]]$group ) )
  color.code$SECsource$HEX <- g$data[[1]]$fill[ match( color.code$SECsource$GROUP,g$data[[1]]$group ) ]
  color.code$SECsource$COLOR <- sapply( color.code$SECsource$HEX, plotrix::color.id )
  color.code$SECsource <- color.code$SECsource[ order(color.code$SECsource$GROUP), ]
  color.code$SECsource$NAME  <- sort( unique( unid.table$SPECIES ) )
  rm( g )
  
  
  ### (RELATIVE) Stacked Barplot ###
  ### ------------------------------
  
  Fig9.b = ggplot( unid.table, aes( fill=SPECIES, y=value, x=Year ) ) +
    geom_bar( position="fill", stat="identity", colour="black" ) +
    facet_grid( . ~ Catch.Var, scales='free' ) +
    ylab( "Proportion of Catch (in Numbers)" ) + xlab( "Year" ) +
    scale_fill_manual( values = setNames( color.code$SECsource$COLOR, color.code$SECsource$NAME ) ) +
    # scale_fill_brewer( palette = "Blues" ) +
    scale_x_continuous( breaks = scales::pretty_breaks( n = (n.years/2) ) ) +
    theme_bw() +
    theme( text = element_text(size = 11),
           axis.text.x = element_text(angle = 90, vjust=0.5),
           legend.position = "none",
           panel.grid.major = element_line(colour = "grey", size = 0.5),
           panel.grid.minor = element_line(colour = "grey", size = 0.2),
           panel.border = element_rect(colour = "black", fill = NA),
           plot.margin = unit( c(0,1,0,0.25),"cm" ) )
  
  
  ### Timeseries Comparison ###
  ### -------------------------
  ###     ...of the amount of 'unidentified' catch relative to that identified to the species-of-interest...
  
  unid.table = my.data %>%
    filter( NEW_COM %in% tolower( c(params$species.name,params$species.add,params$species.unid) ) ) %>%
    ###   ...wherein any catch from the 'additional' species group (i.e., from params$species.add )
    ###       is to be combined with that from the species-of-interest (i.e., params$species.name )...
    mutate( NEW_COM = ifelse( NEW_COM == tolower(params$species.add), tolower(params$species.name), NEW_COM ) ) %>%
    group_by( Year, NEW_COM ) %>%
    summarize( AB1 = sum( as.numeric(AB1), na.rm=TRUE ),
               B2 = sum( as.numeric(B2), na.rm=TRUE ) ) %>%
               # AB1.wgt = sum( as.numeric(lbsest_SECwwt), na.rm=TRUE ) ) %>%
    select( Year, NEW_COM, AB1, B2 ) %>%
    arrange( Year )
  
  spp.order = c( tolower(params$species.name), tolower(params$species.unid) )
  unid.table$NEW_COM = factor( unid.table$NEW_COM, levels=spp.order )
  rm( spp.order )
  
  colnames(unid.table)[ which( colnames(unid.table) == "NEW_COM" ) ] = "SPECIES"
  
  unid.table = unid.table %>% pivot_longer( cols = c('AB1','B2'), names_to = "Catch.Var" )
  unid.table$value = unid.table$value / 1000
  
  
  Fig9.c <- ggplot( data=unid.table, aes( x=Year, colour=SPECIES ) ) +
    geom_line( aes( y=value ), size=1.2 ) +
    facet_grid( . ~ Catch.Var, scales='free' ) +
    labs( title="", x="Year", y="Thousands of Fish" ) +
    scale_x_continuous( breaks = scales::pretty_breaks( n = (n.years/2) ) ) +
    expand_limits(y = 0) +
    theme_bw() +
    theme( text = element_text(size = 11),
           axis.text.x = element_text(angle = 90, vjust=0.5),
           legend.position = "bottom",
           panel.grid.major = element_line(colour = "grey", size = 0.5),
           panel.grid.minor = element_line(colour = "grey", size = 0.2),
           panel.border = element_rect(colour = "black", fill = NA) )
  
  rm( unid.table, n.years, color.code )
  
  
  return.object = list( Fig9.a, Fig9.b, Fig9.c )
  names(return.object) = c( "abs.bars", "rel.bars", "ID.vs.UNID" )
  rm( Fig9.a, Fig9.b, Fig9.c )
  
  return( return.object )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------

FIG.catch.maps = function( my.data,
                           data.type = c( "catch","effort" ),
                           plot.region = c( "Gulf of America",  "South Atlantic",
                                            "Gulf of America and South Atlantic",
                                            "Atlantic", "Southeast", "Mid Atlantic", "North Atlantic" ),
                           FL.sub = c( "WholeState","FLW_FLE","FL_REG" ),
                           NC.sub = c( "WholeState","NC_REG" ),
                           mode.filter = c( "None", "Cbt","Priv","Hbt","Shore","CbtHbt" ),
                           params ) {
  ###       ...where "my.data" is the full table of GenRec estimates at the finest resolution...
  ###           "data.type" identifies the type of table being summarized: catch (AB1 & B2) or effort...
  ###           "plot.region" identifies those states to include in the map/plot space, which is currently
  ###                 configured to allow the following inputs ( but additional LOVs could easily be added ):
  ###                   'Gulf of America' = TX-FL,
  ###                   'South Atlantic'  = FL-NC,
  ###                   'Gulf of America and South Atlantic' = TX-NC,
  ###                   'Atlantic'     = FL-ME,   'Southeast'      = TX-ME
  ###                   'Mid Atlantic' = VA-NY,   'North Atlantic' = CT-ME
  ###           "FL.sub" identifies the sub-state domains to be used when plotting Florida:
  ###                   'WholeState' identifies no sub-state domains to be used for FL
  ###                   'FLW_FLE' breaks-up FL into west vs. east components (mirrors NEW_ST)
  ###                   'FL_REG'  breaks-up FL into five domains (mirrors FL_REG)
  ###           "NC.sub" identifies the sub-state domains to be used when plotting North Carolina:
  ###                   'WholeState' identifies no sub-state domains to be used for NC
  ###                   'NC_REG'  breaks-up NC into north vs. south components (mirrors NC_REG)
  ###       and "params" the R object that (amongst other things) identifies the region being assessed...
  
  
  
  ### STATES TO PLOT ###
  ### ------------------
  
  if( plot.region == "Gulf of America" ) {
    map.states <- c( "texas", "louisiana", "mississippi", "alabama", "florida" )
  } else if( plot.region == "South Atlantic" ) {
    map.states <- c( "florida", "georgia", "south carolina", "north carolina" )
  } else if( plot.region == "Gulf of America and South Atlantic" ) {
    map.states <- c( "texas", "louisiana", "mississippi", "alabama", "florida",
                     "georgia", "south carolina", "north carolina" )
  } else if( plot.region == "Mid Atlantic" ) {
    map.states <- c( "virginia", "maryland", "delaware", "new jersey", "pennsylvania", "new york" )
  } else if( plot.region == "North Atlantic" ) {
    map.states <- c( "connecticut", "rhode island", "massachusetts", "new hampshire", "maine" )
  } else if( plot.region == "Atlantic" ) {
    map.states <- c( "florida", "georgia", "south carolina", "north carolina",
                     "virginia", "maryland", "delaware", "new jersey", "pennsylvania", "new york",
                     "connecticut", "rhode island", "massachusetts", "new hampshire", "maine" )
  } else if( plot.region == "Southeast" ) {
    map.states <- c( "texas", "louisiana", "mississippi", "alabama", "florida",
                     "georgia", "south carolina", "north carolina",
                     "virginia", "maryland", "delaware", "new jersey", "pennsylvania", "new york",
                     "connecticut", "rhode island", "massachusetts", "new hampshire", "maine" )
  }
  
  
  ### SUB-STATE DOMAINS ###
  ### ---------------------
  
  states.to.sub <- list()
  ###     ...which is left empty if no partitioning is needed (i.e., all mapping by state )...
  
  if( FL.sub != 'WholeState' ) {    states.to.sub$florida <- FL.sub   }
  if( NC.sub != 'WholeState' ) {    states.to.sub$northcarolina <- NC.sub  }
  
  ###   Generally speaking, maps will be constructed by summarizing estimates by state, but
  ###   county-specific mapping is needed in any states where sub-state domains (e.g., FL_REG ) are identified.
  ###   In particular, county is the next spatial resolution available ( below state ) in the R "maps" packages,
  ###   and so any maps requiring sub-state plotting will need to manipulate the county-specific designations (ugh).
  ###   Therefore, the code below identifies which counties 'belong' to the strata identified in FL.sub or NC.sub...
  
  subrg.id <- list()
  
  if( length( states.to.sub ) == 0 ) {
    ### DO NOTHING - no states with sub-state domains to plot...
    
  } else {
    
    ### FLORIDA ###
    ###     ...where I found the borders of the five FL subregions at:
    ###       https://www.researchgate.net/figure/The-For-Hire-Telephone-Survey-FHS-stratifies-Florida-into-five-subregions-the-Florida_fig1_293815839
    if( length( states.to.sub$florida ) > 0 ) {
      subrg.id$florida <- data.frame(
        county = c( "escambia","santa rosa","okaloosa","walton","holmes","washington","bay","jackson",
                    "calhoun","gulf","liberty","franklin","gadsden","leon","wakulla","jefferson",
                    "madison","taylor","dixie",
                    
                    "hamilton","suwannee","lafayette","columbia","baker","union","bradford","alachua","gilchrist",
                    "levy","marion","citrus","sumter","hernando","pasco","pinellas","hillsborough","polk",
                    "manatee","hardee","highlands","de soto","sarasota","charlotte","glades",
                    "lee","hendry","collier",
                    
                    "monroe",
                    
                    "miami-dade","broward","palm beach","martin","st lucie","okeechobee","indian river",
                    
                    "brevard","osceola","orange","seminole","lake","volusia","flagler","putnam","st johns",
                    "clay","duval","nassau" ),
        FL_REG = c( rep("1",times=19),rep("2",times=28),
                  rep("3",times=1),rep("4",times=7),rep("5",times=12) ),
        FLW_FLE = c( rep("1",times=48),rep("2",times=19) )
      )
    }
    
    ### NORTH CAROLINA ###
    if( length( states.to.sub$northcarolina ) > 0 ) {
      subrg.id$north.carolina <- data.frame(
        county = c( "bertie","camden","chowan","currituck","dare","pasquotank","perquimans","tyrrell","washington",
                    # 15 29 41 53 55 139 143 177 187
                    "beaufort","brunswick","carteret","craven","hyde","new hanover","onslow","pamlico","pender","pitt"
                    # 13 19 31 49 95 129 133 137 141 147
                   ),
        NC_REG = c( rep("N",times=9),rep("S",times=10) ) )
    }
  }
  
  
  ### COMPILE DATA ###
  ### ----------------
  
  if( mode.filter == 'None' ) {
    dummy.table = my.data %>% group_by( Year, NEW_ST,FL_REG,NC_REG )
  } else {
    dummy.table = my.data %>% filter( NEW_MODEN == mode.filter ) %>% group_by( Year, NEW_ST,FL_REG,NC_REG )
  }
  
  if( data.type == "catch" ) {
    dummy.table = dummy.table %>%
      summarize( AB1 = sum( as.numeric(AB1) ),
                  B2 = sum( as.numeric( B2) ),
                 LBS = sum( as.numeric(LBSEST_SECWWT) ) ) %>%
      select( Year, NEW_ST,FL_REG,NC_REG, AB1,B2,LBS ) %>%
      pivot_longer( cols = c('AB1','B2','LBS'), names_to = "variable" )
    
  } else if( data.type == "effort" ) {
    dummy.table = dummy.table %>%
      summarize( value = sum( as.numeric(ESTRIPS) ) ) %>%
      select( Year, NEW_ST,FL_REG,NC_REG, value )
  }
  
  dummy.table$Year   <- as.numeric( dummy.table$Year )
  # dummy.table$FL_REG <- as.numeric( dummy.table$FL_REG )
  
  ###   I then start constructing the columns needed for my maps...
  
  ###   For my 'state' column (i.e., NEW_STA ), I round any "fractional" entries that may have come-in
  ###   with SRHS data, but save as a new column in case I need the original NEW_ST values at some point...
  dummy.table$MY_ST <- round( as.numeric(dummy.table$NEW_ST), digits=0 )
  dummy.table$MY_ST[ which( dummy.table$MY_ST==1 ) ] <- "texas"
  dummy.table$MY_ST[ which( dummy.table$MY_ST==2 ) ] <- "louisiana"
  dummy.table$MY_ST[ which( dummy.table$MY_ST==3 ) ] <- "mississippi"
  dummy.table$MY_ST[ which( dummy.table$MY_ST==4 ) ] <- "alabama"
  dummy.table$MY_ST[ which( dummy.table$MY_ST==5 ) ] <- "florida"
  dummy.table$MY_ST[ which( dummy.table$MY_ST==6 ) ] <- "florida"
  dummy.table$MY_ST[ which( dummy.table$MY_ST==7 ) ] <- "georgia"
  dummy.table$MY_ST[ which( dummy.table$MY_ST==8 ) ] <- "south carolina"
  dummy.table$MY_ST[ which( dummy.table$MY_ST==9 ) ] <- "north carolina"
  dummy.table$MY_ST[ which( dummy.table$MY_ST==10 ) ] <- "virginia"
  dummy.table$MY_ST[ which( dummy.table$MY_ST==11 ) ] <- "maryland"
  dummy.table$MY_ST[ which( dummy.table$MY_ST==12 ) ] <- "delaware"
  dummy.table$MY_ST[ which( dummy.table$MY_ST==13 ) ] <- "new jersey"
  dummy.table$MY_ST[ which( dummy.table$MY_ST==14 ) ] <- "new york"
  dummy.table$MY_ST[ which( dummy.table$MY_ST==15 ) ] <- "connecticut"
  dummy.table$MY_ST[ which( dummy.table$MY_ST==16 ) ] <- "rhode island"
  dummy.table$MY_ST[ which( dummy.table$MY_ST==17 ) ] <- "massachusetts"
  dummy.table$MY_ST[ which( dummy.table$MY_ST==18 ) ] <- "new hampshire"
  dummy.table$MY_ST[ which( dummy.table$MY_ST==19 ) ] <- "maine"
  
  ###   For my column of 'sub-state' domains, I first check whether any subregions are required...
  if( length(states.to.sub) > 0 ) {
    
    ###     ...and, if so, populate a "MY_DOM" column with appropriate identifiers...
    dummy.table$MY_DOM <- NA
    
    if( "florida" %in% names(states.to.sub) ) {
      rows <- which( dummy.table$MY_ST == "florida" )
      dummy.table$MY_DOM[rows] <- dummy.table$FL_REG[rows]
      ###     ...where I populate the "MY_DOM" column directly from "FL_REG" as these subregion designations
      ###       represent the finest spatial resolution with which areas could be defined in "dummy.table"
      ###       and so all of my spatial breakdowns make use of the information in "FL_REG"...
      
      if( states.to.sub$florida == "FL_REG" ) {
        ### DO NOTHING - already done...
      } else if( states.to.sub$florida == "FLW_FLE" ){
        ###   ...where I create a summary table ( from subrg.id$florida ) to relate the FL sub-regions
        ###     (i.e., from FL_REG ) to my east/west designations...
        ref.table <- unique( subrg.id$florida[ ,c("FL_REG","FLW_FLE") ] )
        for( i in 1:dim(ref.table)[1] ) {
          dummy.table$MY_DOM[rows] <- ref.table$FLW_FLE[ match( dummy.table$MY_DOM[rows], ref.table$FL_REG ) ]
        }
        rm(ref.table)
      }
      rm(rows)
    }
    
    if( "northcarolina" %in% names(states.to.sub) ) {
      rows <- which( dummy.table$MY_ST == "north carolina" )
      dummy.table$MY_DOM[rows] <- dummy.table$NC_REG[rows]
      
      # if( states.to.sub$northcarolina == "NC_REG" ) {
      #   ### DO NOTHING - already done...
      # } else if( states.to.sub$northcarolina == "___" ){
      #   
      # }
      rm(rows)
    }
    
    ###       ...and then calculate a GRAND TOTAL across all years (from which my maps will be constructed)...
    
    if( 'variable' %in% colnames(dummy.table) ) {
      dummy.table <- dummy.table %>%
        group_by( MY_ST, MY_DOM, variable ) %>%
        summarize( value = sum( as.numeric(value) ) ) %>%
        select( MY_ST, MY_DOM, variable, value )
      summary.data = dummy.table %>% rename( state = MY_ST, subrg = MY_DOM )
      
    } else {
      dummy.table <- dummy.table %>%
        group_by( MY_ST, MY_DOM ) %>%
        summarize( value = sum( as.numeric(value) ) ) %>%
        select( MY_ST, MY_DOM, value )
      summary.data = dummy.table %>% rename( state = MY_ST, subrg = MY_DOM )
    }
    
    
  } else {
    ###     ...where maps are constructed by state (i.e., no subregions )...
    
    if( 'variable' %in% colnames(dummy.table) ) {
      dummy.table <- dummy.table %>%
        group_by( MY_ST, variable ) %>%
        summarize( value = sum( as.numeric(value) ) ) %>%
        select( MY_ST, variable, value )
      summary.data = dummy.table %>% rename( state = MY_ST )
      
    } else {
      dummy.table <- dummy.table %>%
        group_by( MY_ST ) %>%
        summarize( value = sum( as.numeric(value) ) ) %>%
        select( MY_ST, value )
      summary.data = dummy.table %>% rename( state = MY_ST )
    }
  }
  
  rm(dummy.table)
  
  
  
  ####################################################################################################
  ####################################################################################################
  ####################################################################################################
  
  
  
  ### I start by defining some desired settings for my plots...
  ditch_the_axes <- theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank()
  )
  
  
  ### For the actual plotting, the map_data() function contains quite a few options for
  ###       how boundaries are to be drawn in maps:
  ### ### mapdata <- map_data( "usa" )
  ### ### mapdata <- map_data( "state" )
  ### ### mapdata <- map_data( "county" )
  ###       ...other options include world, world2, nz, italy, france...
  
  
  ### IMPORTING MAP DATA ###
  ### ----------------------
  ###   ...for which I first check to see if any sub-state domains are being plotted...
  
  mapdata.list = list()
  
  if( data.type == "catch" ) {
    
    blah = c('AB1','B2','LBS')
    for( i in 1:length(blah) ) {
      dummy.table = summary.data %>% filter( variable == blah[i] )
      
      if( "subrg" %notin% colnames(summary.data) ) {
        ###     ...for which only state (map) data is needed...
        mapdata <- map_data( "state", region=map.states )
        mapdata$data <- dummy.table$value[ match( mapdata$region, dummy.table$state ) ]
        mapdata$data <- mapdata$data / 1000000      ###  ...converting catch into millions of fish...
        
      } else {
        mapdata <- map_data( "county", region=map.states )
        mapdata$data <- dummy.table$value[ match( mapdata$region, dummy.table$state ) ]
        ###     ...but for those states with subregions, there are multiple by-state values and so I go
        ###       back to these states and refill with subregion(county)-specific values...
        replace.states <- unique( as.vector( dummy.table$state[ which(!is.na(dummy.table$subrg)) ] ) )
        
        if( "florida" %in% replace.states ) {
          rows <- which( mapdata$region == "florida" )
          ref.table <- subrg.id$florida
          if( states.to.sub$florida == "FL_REG" ) {
            ref.table <- ref.table[ ,c("county","FL_REG") ]
          } else if( states.to.sub$florida == "FLW_FLE" ) {
            ref.table <- ref.table[ ,c("county","FLW_FLE") ]
          }
          colnames(ref.table)[2] <- "code"
          mapdata$data[rows] <- dummy.table$value[
            match( ref.table$code[ match( mapdata$subregion[rows], ref.table$county ) ], dummy.table$subrg ) ]
          rm( rows, ref.table )
        }
        
        if( "north carolina" %in% replace.states ) {
          rows <- which( mapdata$region == "north carolina" )
          ref.table <- subrg.id$north.carolina
          if( states.to.sub$northcarolina == "NC_REG" ) {
            ref.table <- ref.table[ ,c("county","NC_REG") ]
          }
          colnames(ref.table)[2] <- "code"
          mapdata$data[rows] <- dummy.table$value[
            match( ref.table$code[ match( mapdata$subregion[rows], ref.table$county ) ], dummy.table$subrg ) ]
          rm( rows, ref.table )
        }
        
        mapdata$data <- mapdata$data / 1000000
        mapdata$data[ is.na(mapdata$data) ] = 0
      }
      
      mapdata.list[[i]] = mapdata
    }
    rm( dummy.table, mapdata, i, blah )
  }
  
  if( data.type == "effort" ) {
    
    dummy.table = summary.data
    
    if( "subrg" %notin% colnames(summary.data) ) {
      ###     ...for which only state (map) data is needed...
      mapdata <- map_data( "state", region=map.states )
      mapdata$data <- dummy.table$value[ match( mapdata$region, dummy.table$state ) ]
      mapdata$data <- mapdata$data / 1000000      ###  ...converting effort into millions of trips...
      
    } else {
      mapdata <- map_data( "county", region=map.states )
      mapdata$data <- dummy.table$value[ match( mapdata$region, dummy.table$state ) ]
      ###     ...but for those states with subregions, there are multiple by-state values and so I go
      ###       back to these states and refill with subregion(county)-specific values...
      replace.states <- unique( as.vector( dummy.table$state[ which(!is.na(dummy.table$subrg)) ] ) )
      
      if( "florida" %in% replace.states ) {
        rows <- which( mapdata$region == "florida" )
        ref.table <- subrg.id$florida
        if( states.to.sub$florida == "FL_REG" ) {
          ref.table <- ref.table[ ,c("county","FL_REG") ]
        } else if( states.to.sub$florida == "FLW_FLE" ) {
          ref.table <- ref.table[ ,c("county","FLW_FLE") ]
        }
        colnames(ref.table)[2] <- "code"
        mapdata$data[rows] <- dummy.table$value[
          match( ref.table$code[ match( mapdata$subregion[rows], ref.table$county ) ], dummy.table$subrg ) ]
        rm( rows, ref.table )
      }
      
      if( "north carolina" %in% replace.states ) {
        rows <- which( mapdata$region == "north carolina" )
        ref.table <- subrg.id$north.carolina
        if( states.to.sub$northcarolina == "NC_REG" ) {
          ref.table <- ref.table[ ,c("county","NC_REG") ]
        }
        colnames(ref.table)[2] <- "code"
        mapdata$data[rows] <- dummy.table$value[
          match( ref.table$code[ match( mapdata$subregion[rows], ref.table$county ) ], dummy.table$subrg ) ]
        rm( rows, ref.table )
      }
      
      mapdata$data <- mapdata$data / 1000000
      mapdata$data[ is.na(mapdata$data) ] = 0
    }
    
    mapdata.list[[1]] = mapdata
      
      
    rm( dummy.table, mapdata )
  }
  
  
  ### MAPPING ###
  ### -----------
  
  options( scipen=999 )     ### ...apply a 'large' penalty to using scientific notation...
  
  return.object = list()
  
  if( data.type == "catch" ) {
    blah = c('AB1','B2','LBS')
    for( i in 1:length(blah) ) {
      return.object[[i]] = ggplot( data=mapdata.list[[i]] ) + coord_fixed(1.3) + theme_bw() + ditch_the_axes +
        geom_polygon( aes( x=long,y=lat,group=group, fill=data ) ) +
        borders("state", colour = 1, regions = map.states) +
        scale_fill_gradient(low = "white", high = "grey25", na.value = "black") +
        # scale_fill_gradient(low = "white", high = "dodgerblue", na.value = "black") +
        # scale_fill_gradient2(low = "red", mid = "white", high = "dodgerblue", midpoint = 10, na.value = "black") +
        # scale_fill_gradientn(colours = rev(rainbow(7)), na.value = "white" ) +
        
        theme( legend.title.align = 0.5 )
      
      if( blah[i] %in% c('AB1','B2') ) {
        return.object[[i]] = return.object[[i]] + labs( fill="Total Catch\n(Millions Fish)" )
      } else {
        return.object[[i]] = return.object[[i]] + labs( fill="Total Catch\n(Millions Pounds)" )
      }
      
      if( mode.filter == 'None' ) {
        return.object[[i]] = return.object[[i]] +
          labs( title = paste0( "Sum Catch (",blah[i],") for ",params$sedar.name," - ",toupper(params$species.name) ) )
        
      } else {
        if( mode.filter == "Cbt" ) {
          return.object[[i]] = return.object[[i]] +
            labs( title = paste0( "Sum Charter Catch (",blah[i],") for ",
                                  params$sedar.name," - ",toupper(params$species.name) ) )
        } else if( mode.filter == "Priv" ) {
          return.object[[i]] = return.object[[i]] +
            labs( title = paste0( "Sum Private Catch (",blah[i],") for ",
                                  params$sedar.name," - ",toupper(params$species.name) ) )
        } else if( mode.filter == "Hbt" ) {
          return.object[[i]] = return.object[[i]] +
            labs( title = paste0( "Sum Headboat Catch (",blah[i],") for ",
                                  params$sedar.name," - ",toupper(params$species.name) ) )
        } else if( mode.filter == "Shore" ) {
          return.object[[i]] = return.object[[i]] +
            labs( title = paste0( "Sum Shore Catch (",blah[i],") for ",
                                  params$sedar.name," - ",toupper(params$species.name) ) )
        } else if( mode.filter == "CbtHbt" ) {
          return.object[[i]] = return.object[[i]] +
            labs( title = paste0( "Sum For-Hire Catch (",blah[i],") for ",
                                  params$sedar.name," - ",toupper(params$species.name) ) )
        }
      }
      
      names(return.object)[i] = blah[i]
    }
    rm( blah, i )
  }
  
  
  if( data.type == "effort" ) {
    
    return.object[[1]] = ggplot( data=mapdata.list[[1]] ) + coord_fixed(1.3) + theme_bw() + ditch_the_axes +
      geom_polygon( aes( x=long,y=lat,group=group, fill=data ) ) +
      borders("state", colour = 1, regions = map.states) +
      scale_fill_gradient(low = "white", high = "grey25", na.value = "black") +
      # scale_fill_gradient(low = "white", high = "dodgerblue", na.value = "black") +
      # scale_fill_gradient2(low = "red", mid = "white", high = "dodgerblue", midpoint = 10, na.value = "black") +
      # scale_fill_gradientn(colours = rev(rainbow(7)), na.value = "white" ) +
      theme( legend.title.align = 0.5 ) +
      labs( fill="Total Effort\n(Millions Trips)" )
    
    
    blah = colnames(my.data)
    blah = blah[ blah %in% c('ESTRIPS','ESTDAYS') ]
    
    if( mode.filter == 'None' ) {
      return.object[[1]] = return.object[[1]] +
        labs( title = paste0( "Sum Effort (",blah,") for ",params$sedar.name," - ",toupper(params$species.name) ) )
      
    } else {
      if( mode.filter == "Cbt" ) {
        return.object[[1]] = return.object[[1]] +
          labs( title = paste0( "Sum Charter Effort (",blah,") for ",
                                params$sedar.name," - ",toupper(params$species.name) ) )
      } else if( mode.filter == "Priv" ) {
        return.object[[1]] = return.object[[1]] +
          labs( title = paste0( "Sum Private Effort (",blah,") for ",
                                params$sedar.name," - ",toupper(params$species.name) ) )
      } else if( mode.filter == "Hbt" ) {
        return.object[[1]] = return.object[[1]] +
          labs( title = paste0( "Sum Headboat Effort (",blah,") for ",
                                params$sedar.name," - ",toupper(params$species.name) ) )
      } else if( mode.filter == "Shore" ) {
        return.object[[1]] = return.object[[1]] +
          labs( title = paste0( "Sum Shore Effort (",blah,") for ",
                                params$sedar.name," - ",toupper(params$species.name) ) )
      } else if( mode.filter == "CbtHbt" ) {
        return.object[[1]] = return.object[[1]] +
          labs( title = paste0( "Sum For-Hire Effort (",blah,") for ",
                                params$sedar.name," - ",toupper(params$species.name) ) )
      }
    }
    
    names(return.object)[1] = blah
  }
  
  rm( map.states )
  
  
  
  return( return.object )
  
}
### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------

FIG.catnum.covid = function( params, plot.alpha, plot.bw = "nrd0", plot.log = c(TRUE,FALSE) ) {
  ###       ...where 'params' is the R object that (amongst other things) identifies the species and
  ###             strata associated with this data product (e.g., region-of-interest for this SEDAR ),
  ###         'plot.alpha' controls the transparency of the constructed distribution plot,
  ###         'plot.bw' controls the bindwidths used in the constructed distribution plot, where the
  ###             default is set at that of geom_density ("nrd0" defines a rule to calculate bw),
  ###     and 'plot.log' is a binary indicator used to identify if catch or log-catch is to be plotted...
  
  
  ###   Note that this function only pulls raw MRIP data as TPWD & LACreel haven't (yet?)
  ###   imputed any 2020 data to fill COVID data gaps...
  
  
  ### FUNCTIONS ###
  ### ---------------------------------------------------------------------------------------------
  ### ---------------------------------------------------------------------------------------------
  ### ---------------------------------------------------------------------------------------------
  
  nodc.code.info <- function( taxa, spp.table ) {
    ###   ...which identifies the NODC code associated with the scientific name stored in 'taxa'...
    info <- spp.table$NODC_CODE[ grep( paste0("^",taxa,"$"), spp.table$SCIENTIFIC ) ]
    return( info )
  }
  
  ### ---------------------------------------------------------------------------------------------
  
  raw.dat.filter = function( spp.filter, yr.filter, mode.filter,
                             reg.filter, sta.filter, fl.filter, nc.filter ) {
    ###   ...which pulls raw catch (and effort) data for a given set of filters...
    
    con = dbConnect(dbDriver("Oracle"), username = keyring::key_list("SECPR")[1,2],
                    password = keyring::key_get("SECPR", keyring::key_list("SECPR")[1,2]), dbname = "SECPR")
    
    #############################
    ######     FILTERS     ######
    #############################
    
    ### State FIPS Codes ###
    state.codes = sta.filter
    state.codes = gsub( "TX", 48, state.codes )
    state.codes = gsub( "LA", 22, state.codes )
    state.codes = gsub( "MS", 28, state.codes )
    state.codes = gsub( "AL",  1, state.codes )
    state.codes = gsub( "FLW",12, state.codes )
    state.codes = gsub( "FLE",12, state.codes )
    state.codes = gsub( "GA", 13, state.codes )
    state.codes = gsub( "SC", 45, state.codes )
    state.codes = gsub( "NC", 37, state.codes )
    state.codes = gsub( "VA", 51, state.codes )
    state.codes = gsub( "MD", 24, state.codes )
    state.codes = gsub( "DE", 10, state.codes )
    state.codes = gsub( "PA", 42, state.codes )
    state.codes = gsub( "NJ", 34, state.codes )
    state.codes = gsub( "NY", 36, state.codes )
    state.codes = gsub( "CT",  9, state.codes )
    state.codes = gsub( "RI", 44, state.codes )
    state.codes = gsub( "MA", 25, state.codes )
    state.codes = gsub( "NH", 33, state.codes )
    state.codes = gsub( "ME", 23, state.codes )
    state.codes = gsub( "PR", 72, state.codes )
    state.codes = gsub( "VI", 78, state.codes )
    
    ### MRIP MODE_FX Codes ###
    mode.codes = mode.filter
    mode.codes = gsub( "Priv",  7, mode.codes )
    mode.codes = gsub( "Hbt",   4, mode.codes )
    mode.codes = gsub( "Cbt",   5, mode.codes )
    mode.codes = gsub( "Shore", 3, mode.codes )
    ###   ...and where the only "Hbt" records I retain in this summary are those from the Mid/North-Atlantic
    ###         ( SRHS covers all years for HBT in the SATL and all years except 1981-85 in the GOM, none of which
    ###           are needed in this 2015+ summary )...
    if( ( "Hbt" %in% mode.filter ) &
        !any( c("VA","MD","DE","PA","NJ","NY","CT","RI","MA","NH","ME") %in% sta.filter ) ) {
      mode.codes = mode.codes[ mode.codes != 4 ]
    }
    
    
    ##############################
    ######     RAW TRIP     ######
    ##############################
    
    effort.sub = dbGetQuery(con,
                            paste0("select t.PRIM2_COMMON, t.PRIM1_COMMON, t.STRAT_ID, t.PSU_ID, t.ID_CODE,
                                           t.MODE_FX,   t.MODE_F, t.AREA_X, t.AREA, t.ST, t.CNTY, t.INTSITE,
                                           t.HRSF, t.FFDAYS12, t.FFDAYS2, t.CNTY_RES, t.ST_RES,
                                           t.TELEFON, t.CNTRBTRS, t.NUM_TYP2, t.NUM_TYP3, t.NUM_TYP4,
                                           t.SUB_REG, t.REG_RES, t.WAVE, t.ADD_HRS, t.COASTAL, t.CATCH, t.YEAR,
                                           t.ASG_CODE, t.MONTH, t.KOD, t.MODE_ASG, t.NEW_LIST, t.ON_LIST,
                                           t.PRT_CODE, t.CELLTYPE, t.FSHINSP_A, t.NUM_FISH_A,
                                           t.FL_REG, t.COUNTY, t.ADD_PH, t.DATE1, t.DIST, t.TIME, t.GEAR,
                                           t.PVT_RES, t.SEP_FISH, t.AGE, t.F_BY_P,
                                           t.WP_INT, t.VAR_ID, t.ARX_METHOD, t.ALT_FLAG, t.LEADER,
                                           t.WP_INT_PRECAL, t.BOAT_HRS, t.FIRST, t.NUM_TYP6, t.PARTY,
                                           t.AREA_NC, t.MODE2001, t.MUNI_RES, t.MUNI_TRP, t.RIG, t.TOURN,
                                           t.TURTLE, t.NUM_TYP9, t.DISTKEYS,
                                           t.TSN1, t.TSN2, t.COMPFLAG, t.LICENSE, t.MONITOR, t.ART_REEF,
                                           t.REEFCODE, t.GENDER, t.HRS_DTD, t.REGION, t.STRAT_INTERVAL, t.IMP_REC
           from rdi.mrip_st_pub_trip_cal@secapxdv_dblk.sfsc.noaa.gov t
                        where t.YEAR IN (",     sprintf("'%s'", paste(  yr.filter, collapse = "','")),")",
                                   " AND t.ST IN (",      sprintf("'%s'", paste(state.codes, collapse = "','")),")",
                                   " AND t.MODE_FX IN (", sprintf("'%s'", paste( mode.codes, collapse = "','")),")"
                            ))
    ###     ...where the raw MRIP trip table still has issues importing the PRIM1 and PRIM2 fields
    ###         ( which are therefore excluded in above select statement )...
    
    effort.sub = effort.sub %>%
      mutate( FL_REG = ifelse( ST==12 & CNTY %in% c(33,113,91,131,5,133,59,45,13,63,37,77,39,129,73,65,123,79,29), 1,
                       ifelse( ST==12 & CNTY %in% c(67,121,47,75,41,1,125,23,83,17,119,53,69,101,105,
                                                    103,57,81,49,55,115,27,15,43,71,51,21),                        2,
                       ifelse( ST==12 & CNTY==87,                                                                  3,
                       ifelse( ST==12 & CNTY %in% c(025,011,099,085,111,093,061,25,11,99,85,93,61,086,86),         4,
                       ifelse( ST==12 & CNTY %in% c(097,009,095,117,127,035,107,109,019,007,031,003,
                                                    089,97,9,95,35,19,7,31,3),                                     5, NA ))))),
              NC_REG = ifelse( ST==37 & CNTY %in% c(15,29,41,53,55,139,143,177,187),                               "N",
                       ifelse( ST==37 & CNTY %in% c(13,19,31,49,95,129,133,137,141,147),                           "S", NA )),
              NEW_STA = ifelse( ST==48,"TX", ifelse( ST==22,"LA", ifelse( ST==28,"MS", ifelse( ST==1,"AL",
                        ifelse( ST==12 & FL_REG %in% c(1,2,3), "FLW", ifelse( ST==12 & FL_REG %in% c(4,5), "FLE",
                        ifelse( ST==13,"GA", ifelse( ST==45,"SC", ifelse( ST==37,"NC", ifelse( ST==51,"VA",
                        ifelse( ST==24,"MD", ifelse( ST==10,"DE", ifelse( ST==42,"PA", ifelse( ST==34,"NJ",
                        ifelse( ST==36,"NY", ifelse( ST==9, "CT", ifelse( ST==44,"RI", ifelse( ST==25,"MA",
                        ifelse( ST==33,"NH", ifelse( ST==23,"ME", ifelse( ST==72,"PR", ifelse( ST==78,"VI", NA )))))))))))))))))))))),
              NEW_MODEN = ifelse( MODE_FX == 3, "Shore", ifelse( MODE_FX == 4, "Hbt",
                          ifelse( MODE_FX == 5, "Cbt",   ifelse( MODE_FX == 7, "Priv", NA )))) )
    colnames(effort.sub) = toupper( colnames(effort.sub) )
    
    if( "FL" %in% sta.filter | "FLW" %in% sta.filter | "FLE" %in% sta.filter ) {
      effort.sub <- effort.sub[ !( effort.sub$ST == 12 & effort.sub$FL_REG %notin% fl.filter ), ]
    }
    # if( "NC" %in% sta.filter ) {
    #   effort.sub <- effort.sub[ !( effort.sub$ST == 37 & effort.sub$NC_REG %notin% nc.filter ), ]
    # }
    
    effort.sub <- effort.sub[ !( effort.sub$MODE_FX == 4 & effort.sub$SUB_REG %in% c(6,7) ), ]
    ###     ...only looking at MRIP records for years 2016+ so I can drop the GOM & SATL entirely (for headboat)...
    effort.sub <- effort.sub[ !( effort.sub$MODE_FX == 4 & effort.sub$ST == 12 & effort.sub$FL_REG == 3 ), ]
    
    
    
    ###############################
    ######     RAW CATCH     ######
    ###############################
    
    ### IMPORT ###
    ###
    ###     ...for which I have to reference the raw (calibrated) MRIP catch files
    ###         ( not our final SEDAR2 view -- RDI.V_A_DOM_FES_APAIS_CH_SDR2 )
    
    if( length(spp.filter)==1 ) {
      catch.sub = dbGetQuery(con,
                             paste0("select *
                              from rdi.mrip_st_pub_catch_cal@secapxdv_dblk.sfsc.noaa.gov t
                                  where t.SP_CODE = ", sprintf("'%s'", paste(spp.filter, collapse = "','")),
                                    " AND t.YEAR IN (",    sprintf("'%s'", paste(  yr.filter, collapse = "','")),")",
                                    " AND t.ST IN (",      sprintf("'%s'", paste(state.codes, collapse = "','")),")",
                                    " AND t.MODE_FX IN (", sprintf("'%s'", paste( mode.codes, collapse = "','")),")"
                             ))
    } else {
      catch.sub = dbGetQuery(con,
                             paste0("select *
                              from rdi.mrip_st_pub_catch_cal@secapxdv_dblk.sfsc.noaa.gov t
                                      where ", paste0( "t.SP_CODE IN (",sprintf("'%s'", paste(spp.filter, collapse = "','")),")" ),
                                    " AND t.YEAR IN (",    sprintf("'%s'", paste(  yr.filter, collapse = "','")),")",
                                    " AND t.ST IN (",      sprintf("'%s'", paste(state.codes, collapse = "','")),")",
                                    " AND t.MODE_FX IN (", sprintf("'%s'", paste( mode.codes, collapse = "','")),")"
                             ))
    }
    
    colnames(catch.sub) = toupper( colnames(catch.sub) )
    
    catch.sub = left_join( catch.sub[ ,!( colnames(catch.sub)=="FL_REG" ) ],
                           effort.sub[ ,c("ID_CODE","YEAR","NEW_STA","NEW_MODEN","FL_REG","NC_REG") ],
                           by=c("ID_CODE","YEAR"), suffix=c(".cat",".eff") )
    ###     ...and adding the same value-added fields to "catch.sub" that were just added to "effort.sub"...
    
    if( "FL" %in% sta.filter | "FLW" %in% sta.filter | "FLE" %in% sta.filter ) {
      catch.sub <- catch.sub[ !( catch.sub$ST == 12 & catch.sub$FL_REG %notin% fl.filter ), ]
    }
    # if( "NC" %in% sta.filter ) {
    #   catch.sub <- catch.sub[ !( catch.sub$ST == 37 & catch.sub$FL_REG %notin% nc.filter ), ]
    # }
    
    catch.sub <- catch.sub[ !( catch.sub$MODE_FX == 4 & catch.sub$SUB_REG %in% c(6,7) ), ]
    catch.sub <- catch.sub[ !( catch.sub$MODE_FX == 4 & catch.sub$ST == 12 & catch.sub$FL_REG == 3 ), ]
    
    
    
    
    ############################
    ######     OUTPUT     ######
    ############################
    
    # end.list = list( catch.sub, effort.sub )
    # names(end.list) = c( "catch","effort" )
    # return( end.list )
    # ### 
    # ###   ...where Lucas (OST/MRIP) informed me that no (2020 COVID) imputations were needed for the
    # ###       FES or FHS so I decided to drop the effort comparison (Oct 21 2021 email); the imputed intercepts
    # ###       effect (1) any proportions used to partition effort to areas (e.g., domain estimation) and
    # ###       (2) proportions used for the out-of-state adjustment, but the raw effort data was all observed.
    # ###       I therefore only return the catch table...
    
    return( catch.sub )
    
  }
  ### ---------------------------------------------------------------------------------------------
  ### ---------------------------------------------------------------------------------------------
  ### ---------------------------------------------------------------------------------------------
  
  
  
  con = dbConnect(dbDriver("Oracle"), username = keyring::key_list("SECPR")[1,2],
                  password = keyring::key_get("SECPR", keyring::key_list("SECPR")[1,2]), dbname = "SECPR")
  spp.info = dbGetQuery(con, "SELECT * FROM RDI.v_species_xref@secapxdv_dblk.sfsc.noaa.gov")
  
  nodc.code <- sapply( params$species.sci, FUN=nodc.code.info, spp.table=spp.info )
  ###     ...where the appropriate SCIENTIFIC name is defined in my YAML,
  ###                         but identified by looking at the spp_xref table...
  ### # View( spp.info[ grep( "SNAPPER", spp.info$COMMON ), ] )
  ### # View( spp.info[ grep( "SCAMP", spp.info$COMMON ), ] )
  
  if( params$species.add != 'None' ) {
    nodc.code <- c( nodc.code, sapply( params$species.sci.add, FUN=nodc.code.info, spp.table=spp.info ) )
  }
  
  sta.filter = params$subset.states[ params$subset.states %notin% c('SNC','NNC') ]
  nc.filter = gsub( "6","S", gsub( "7","N", params$subset.NC ) )
  covid.dat = raw.dat.filter( spp.filter = nodc.code,
                              yr.filter = 2015:params$term.year,  mode.filter = params$subset.modes,
                              reg.filter = params$region,          sta.filter = sta.filter,
                              fl.filter = params$subset.FL,         nc.filter = nc.filter )
  rm( nodc.code, nc.filter )
  
  # ###     ...and to check that the MRIP data in my new (raw) tables are the same as those in my estimate tables...
  # ###
  # ### Catch ###
  # mrip.new = covid.dat %>%
  #   group_by( YEAR, NEW_MODEN ) %>%
  #   summarise( AB1 = sum( LANDING * WP_CATCH, na.rm=TRUE ),
  #               B2 = sum( RELEASE * WP_CATCH, na.rm=TRUE ) ) %>%
  #   select( YEAR, NEW_MODEN, AB1, B2 ) %>%
  #   pivot_wider( names_from=NEW_MODEN, values_from=c("AB1","B2") )
  # mrip.old = raw.catch %>%
  #   rename( YEAR = Year ) %>%
  #   filter( DS == "MRIP",
  #           YEAR %in% 2015:params$term.year ) %>%
  #   group_by( YEAR, NEW_MODEN ) %>%
  #   summarise( AB1 = sum( AB1, na.rm=TRUE ),
  #               B2 = sum(  B2, na.rm=TRUE ) ) %>%
  #   select( YEAR, NEW_MODEN, AB1, B2 ) %>%
  #   pivot_wider( names_from=NEW_MODEN, values_from=c("AB1","B2") )
  # mrip.summary = full_join( mrip.new, mrip.old, by="YEAR", suffix=c(".new",".old") )
  # dummy.modes = unique(covid.dat$NEW_MODEN)
  # for( i in 1:length(dummy.modes) ) {
  #   eval( parse( text=paste0( "mrip.summary$AB1_",dummy.modes[i],".diff = mrip.summary$AB1_",dummy.modes[i],".new",
  #                             " - mrip.summary$AB1_",dummy.modes[i],".old " ) ) )
  # }
  # mrip.summary = mrip.summary[ , c(1,grep(".diff",colnames(mrip.summary))) ]
  # rm( mrip.summary, mrip.new,mrip.old, i,dummy.modes )
  # 
  # 
  # ### Effort ###
  # mrip.new = covid.dat$effort
  # mrip.new$ESTRIPS = rep( 1, times=dim(mrip.new)[1] )
  # mrip.new = mrip.new %>%
  #   group_by( YEAR, NEW_MODEN ) %>%
  #   summarise( ESTRIPS = sum( ESTRIPS * WP_INT, na.rm=TRUE ) ) %>%
  #   select( YEAR, NEW_MODEN, ESTRIPS ) %>%
  #   pivot_wider( names_from=NEW_MODEN, values_from=ESTRIPS )
  # mrip.old = raw.mrip.eff %>%
  #   rename( YEAR = Year ) %>%
  #   filter( DS == "MRIP",
  #           YEAR %in% 2015:params$term.year ) %>%
  #   group_by( YEAR, NEW_MODEN ) %>%
  #   summarise( ESTRIPS = sum( as.numeric(ESTRIPS), na.rm=TRUE ) ) %>%
  #   select( YEAR, NEW_MODEN, ESTRIPS ) %>%
  #   pivot_wider( names_from=NEW_MODEN, values_from=ESTRIPS )
  # mrip.old$YEAR = as.numeric( mrip.old$YEAR )
  # mrip.summary = full_join( mrip.new, mrip.old, by="YEAR", suffix=c(".new",".old") )
  # dummy.modes = unique(covid.dat$catch$NEW_MODEN)
  # for( i in 1:length(dummy.modes) ) {
  #   eval( parse( text=paste0( "mrip.summary$",dummy.modes[i],".diff = mrip.summary$",dummy.modes[i],".new",
  #                             " - mrip.summary$",dummy.modes[i],".old " ) ) )
  # }
  # mrip.summary = mrip.summary[ , c(1,grep(".diff",colnames(mrip.summary))) ]
  # rm( mrip.summary, mrip.new,mrip.old, i,dummy.modes )
  
  
  
  if( sum( covid.dat[ !is.na(covid.dat$IMP_REC), c('LANDING','RELEASE') ], na.rm=TRUE ) == 0 ) {
    
    Fig10.flag = FALSE
    Fig10.cap = ""
    ###   ...no 2020 records were imputed for this assessment...
    
    return.object = list( Fig10.flag, Fig10.cap )
    names(return.object) = c( "flag", "caption" )
    
  } else {
    
    
    Fig10.flag = TRUE
    
    return.object = list()
    
    
    ######################################
    ######     TRIP-LEVEL CATCH     ######
    ######################################
    
    
    ### Flextable ###
    ### _____________
    ###
    ###     ...raw vs. imputed for 2020 MRIP by-mode & by-wave
    
    imp.years = sort( unique( covid.dat$YEAR[ !is.na(covid.dat$IMP_REC) ] ) )
    
    dummy.table = covid.dat[ covid.dat$YEAR %in% imp.years, ] %>%
      mutate( IMP_REC = as.character(IMP_REC) ) %>%
      mutate( IMP_REC = ifelse( is.na(IMP_REC), "RAW","IMP" ) ) %>%
      rename( Wave = WAVE ) %>%
      group_by( NEW_MODEN, Wave, IMP_REC ) %>%
      summarise( N = length(ID_CODE) ) %>%
      select( NEW_MODEN, Wave, IMP_REC, N ) %>%
      pivot_wider( names_from=c(NEW_MODEN,IMP_REC), values_from=N )
    
    col.order <- colnames(dummy.table)
    
    upper.hdr <- list()
    lower.hdr  <- list()
    
    lower.hdr$Wave <- "Wave"
    upper.hdr$Wave <- ""
    for( i in 2:length(col.order) ) {
      eval( parse( text=paste0(
        "upper.hdr$",col.order[i]," = '",gsub("_.*","",col.order[i]),"'" ) ) )
      eval( parse( text=paste0(
        "lower.hdr$",col.order[i]," = '",gsub(".*_","",col.order[i]),"'" ) ) )
    }
    
    return.object = list( Fig10.flag, dummy.table, col.order, upper.hdr, lower.hdr )
    names(return.object) = c( "flag","catch.table","col.order","upper.hdr","lower.hdr" )
    
    rm( dummy.table, col.order,i, upper.hdr, lower.hdr )
    
    
    
    ### Density Plot ###
    ### ________________
    ###
    ###     ...for which I will need three LABELS for assessments that include years 'after COVID'...
    ###           (1) pre-COVID imputations (e.g., 2015-2019),
    ###           (2) during COVID (e.g., 2020), and
    ###           (3) post-COVID imputations (e.g., 2021+)
    ###     ...but only two LABELS if catch estimates from post-COVID years aren't considered...
    
    imp.yr.label = data.frame( YEAR  = sort( unique( covid.dat$YEAR ) ),
                               LABEL = "X" )
    
    ### (1) PRE-COVID LABELS ###
    blah = sort( unique( covid.dat$YEAR[ covid.dat$YEAR %notin% imp.years ] ) )
    blah = blah[ blah < min(imp.years) ]
    imp.yr.label = imp.yr.label %>%
      mutate( LABEL = ifelse( YEAR %in% blah,
                              paste0( c(blah[1],blah[length(blah)]), collapse="-" ), LABEL ) )
    rm( blah )
    
    ### (2) COVID-IMPUTATION LABELS ###
    if( length(imp.years) > 1 ) {
      imp.yr.label = imp.yr.label %>%
        mutate( LABEL = ifelse( YEAR %in% imp.years,
                                paste0( c(imp.years[1],imp.years[length(imp.years)]), collapse="-" ), LABEL ) )
    } else {
      imp.yr.label = imp.yr.label %>%
        mutate( LABEL = ifelse( YEAR %in% imp.years, as.character(imp.years), LABEL ) )
    }
    
    ### (3) POST-COVID LABELS ###
    ###     ...which is essentially a 'catch-all' label (i.e., any years that weren't labeled as (1) or (2) )
    blah = imp.yr.label$YEAR[ imp.yr.label$LABEL == "X" ]
    if( length(blah) > 1 ) {
      imp.yr.label = imp.yr.label %>%
        mutate( LABEL = ifelse( YEAR %in% blah,
                                paste0( c(blah[1],blah[length(blah)]), collapse="-" ), LABEL ) )
    }  else {
      imp.yr.label = imp.yr.label %>%
        mutate( LABEL = ifelse( YEAR %in% blah, as.character(blah), LABEL ) )
    }
    rm( blah )
    
    
    dummy.table = covid.dat %>%
      mutate( IMP_REC = as.character(IMP_REC) ) %>%
      mutate( IMP_REC = ifelse( is.na(IMP_REC), "RAW","IMP" ),
              # YR.GRP = ifelse( YEAR == 2020, "2020", "2015-2019" ) ) %>%
              YR.GRP = imp.yr.label$LABEL[ match( YEAR, imp.yr.label$YEAR ) ] ) %>%
      # mutate( LANDING = ifelse( is.infinite( log10(LANDING) ), NA, log10(LANDING) ),
      #         RELEASE = ifelse( is.infinite( log10(RELEASE) ), NA, log10(RELEASE) ) ) %>%
      pivot_longer( cols=c("LANDING","RELEASE"), names_to = "CATCH" )
    
    
    Fig10.a = ggplot( dummy.table, aes(x=value) ) +
      geom_density( aes(fill=IMP_REC), alpha=plot.alpha, bw=plot.bw )
      ###     ...where 'bw' controls the binwidth used in smoothing (e.g., if density plots look too chaotic,
      ###          might need a larger binwidth to increase the degree of smoothing between 'peaks' )...
    
    ### LOG-TRANSFORMED CATCH vs. ORIGINAL SCALE ###
    if( plot.log ) {
      Fig10.a = Fig10.a + scale_x_log10( ) + labs( title="Frequency Distribution for log10(catch)" )
    } else {
      Fig10.a = Fig10.a + labs( title="Frequency Distribution for Catch Observations" )
    }
    
    Fig10.a = Fig10.a +
      facet_grid( YR.GRP ~ CATCH , scales="free" ) +
      theme_bw() +
      scale_fill_manual( values = c('deeppink','darkblue') ) +
      theme( text = element_text(size = 11),
             axis.text.x = element_text(angle = 90, vjust=0.5),
             axis.title.x = element_blank(),
             axis.title.y = element_blank(),
             legend.position = "bottom",
             panel.grid.major = element_line(colour = "grey", size = 0.5),
             panel.grid.minor = element_line(colour = "grey", size = 0.2),
             panel.border = element_rect(colour = "black", fill = NA) )
    ###   ...where the "Transformation introduced infinite values" warning is associated with
    ###       log-transforming a zero-catch value (i.e., #rows removed = #rows with no observed catch ).
    ###       Therefore, this warning message is fine -- code is working as intended...
    
    
    return.object = c( return.object, list(Fig10.a) )
    names(return.object)[length(return.object)] = "density.plot"
    
    rm( dummy.table, imp.yr.label )
    
    
    # ###   ...and as a quick check that the above figure looks to be accurately plotting the data...
    # 
    # blah = dummy.table %>%
    #   group_by( YR.GRP, CATCH ) %>%
    #   summarise( value = sum(value,na.rm=TRUE) ) %>%
    #   pivot_wider( names_from = YR.GRP, values_from = value )
    # 
    # summary( as.factor( log10( dummy.table$value[ dummy.table$CATCH == 'LANDING' & dummy.table$YR.GRP == '2020' ] ) ) )
    
    
    
    # ############################
    # ######     EFFORT     ######
    # ############################
    # ###
    # ###   ...which, as noted in my MRIP raw.dat.filter() function, the effort comparison was dropped from this figure
    # ###       no FES or FHS data were imputed to fill 2020 COVID data gaps (10/21/2021 email from Lucas)...
    # 
    # 
    # ### Stacked Bar Plots ###
    # ### _____________________
    # 
    # dummy.table = covid.dat$effort %>%
    #   mutate( ESTRIPS = 1,
    #          # YR.GRP = ifelse( YEAR == 2020, "2020", "2015-2019" ),
    #           IMP_REC = as.character(IMP_REC) ) %>%
    #   mutate( IMP_REC = ifelse( is.na(IMP_REC), "RAW","IMP" ) ) %>%
    #   group_by( NEW_MODEN, YEAR, IMP_REC ) %>%
    #   summarise( ESTRIPS = sum( ESTRIPS * WP_INT, na.rm=TRUE ) ) %>%
    #   select( NEW_MODEN, YEAR, IMP_REC, ESTRIPS )
    # 
    # n.years = length( unique(dummy.table$YEAR) )
    # Fig10.b <- ggplot( data=dummy.table, aes( x=YEAR, y=ESTRIPS, fill=IMP_REC) ) +
    #   geom_col( position = "stack", colour="black" ) +
    #   # facet_grid( NEW_MODEN ~ . , scales = "free" ) +
    #   facet_wrap( ~ NEW_MODEN, ncol=2, scales="free" ) +
    #   labs( title="Estimated Number of Angler Trips" ) +
    #   scale_x_continuous( breaks = scales::pretty_breaks( n = n.years ) ) +
    #   theme_bw() +
    #   theme( text = element_text(size = 11),
    #     axis.text.x = element_text(angle = 90, vjust=0.5),
    #     axis.title.x = element_blank(),
    #     axis.title.y = element_blank(),
    #     legend.position = "bottom",
    #     panel.grid.major = element_line(colour = "grey", size = 0.5),
    #     panel.grid.minor = element_line(colour = "grey", size = 0.2),
    #     panel.border = element_rect(colour = "black", fill = NA) )
    # 
    # rm( dummy.table, n.years )
    
    
    
    ### CAPTION ###
    ### -----------
    
    Fig10.cap <- paste0( "**Figure 11.** COVID data gaps in the MRIP APAIS and associated imputations",
                         " for (positive) fishing trips that intercepted ",params$region," ",full.common,".",
                         " No 2020 data were imputed for the FES or FHS. (A) Number of positive intercepts in ",
                         paste0(imp.years,collapse="-"), " from the APAIS (RAW) vs. those imputed from intercepts",
                         " in adjacent years (IMP). (B) Distribution of APAIS catch observations",
                         " in years with no imputed catch data (in ",
                         paste0( unique( Fig10.a$data$YR.GRP )[ unique( Fig10.a$data$YR.GRP ) %notin% imp.years ],
                                 collapse =' and ' ),"), in raw ",paste0(imp.years,collapse="-")," APAIS data,",
                         " and in ",paste0(imp.years,collapse="-")," imputations.",
                         " Refer to Cody (2021) for more information on COVID data gaps in MRIP." )
    
    if( plot.log ) {
      Fig10.cap <- gsub( "(B) Distribution of (log-transformed) APAIS catch observations",
                         "(B) Distribution of APAIS catch observations", Fig10.cap )
    }
    
    if( params$species.unid == "None" & !SEDAR.comp.flag ) {
      Fig10.cap = gsub( "Figure 11","Figure 9", Fig10.cap )
    } else if( ( params$species.unid != "None" & !SEDAR.comp.flag ) |
               ( params$species.unid == "None" &  SEDAR.comp.flag ) ) {
      Fig10.cap = gsub( "Figure 11","Figure 10", Fig10.cap )
    }
    
    rm( imp.years )
    
    return.object = c( return.object, list(Fig10.cap) )
    names(return.object)[length(return.object)] = "caption"
    
  }
  
  
  return( return.object )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------



