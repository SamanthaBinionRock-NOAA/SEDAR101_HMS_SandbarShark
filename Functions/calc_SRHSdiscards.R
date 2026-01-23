

### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------
###
###     DESCRIPTION
###
###   ...where the functions below are used to generate proxies of SRHS headboat discards for those years where SRHS estimates
###       are unavailable (1972-2003) or considered unreliable (e.g., 2004-2007 )...
###
###     The first function in this script ( = srhsB2.calculate.proxies() ) is meant to 'manage' all of the method-specific functions,
###     in that this function will calculate proxy discard estimates for all methods identified in it's 'approaches' argument.
###
###     These proxy estimates (from the first function) are then fed into the second function ( = srhs.table.method.comparison() ),
###     which collapses all sets of proxy estimates into a single table...
###     ...which is then fed into the third function ( = srhsB2.comparison.pdf() ) that creates two pdf's of relevant summaries
###     of the various methods to inform decisions on which method is 'most appropriate'.
###
###     All other functions in this script are specific to the individual method, in that each function provides proxies generated
###     by a different method (lots of approaches have been applied/considered in previous SEDARs). Note that the decision as to
###     which method is to be applied in a given SEDAR is made in the main script (i.e., "SRHS_proxyDiscards.R" ), and so the purpose
###     of these functions is to simply make proxy estimates (from each approach) available to data providers for evaluation...
###
###   Note that this function is set-up to handle stocks with multiple StockID boundaries. Briefly, the main script is setup to
###   create a 'SID' field if the assessment is for a 'SID species'. Therefore, each function checks if 'SID' exists in the
###   provided MRIP & SRHS tables and, if so, adds this strata to the various group_by() and select() statements to carry it
###   through the estimation...
###
###
### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------

srhsB2.calculate.proxies = function( genrec.table, srhs.table,
                                     approaches = c( 'superratio',     'mrip-cbt',  'mrip-cbt-smooth',
                                                     'superratio-priv','mrip-priv', 'mrip-priv-smooth',
                                                     'srhs-mean', 'srhs-biosamp' ),
                                     years.impute, years.ratio, bp.ratios, yr.avg ) {
  
  ###     ...where 'genrec.table' and 'srhs.table' are the tables of ( GenRec & SRHS ) catch estimates (AB1 & B2)
  ###                   provided at the resolution at which proxy discards are to be calculated. In particular,
  ###                   the functions below include code to identify strata/vars from table headers and
  ###                   then call these vars in all subsequent group_by(), join(), and select() statements...
  ###         'approaches' identifies those potential proxy methods that the analyst wishes to evaluate for its suitability
  ###                   ( as a SRHS discard proxy ) for this particular SEDAR...
  ###         'years.impute' identifies those years for which the discard proxies will be used as an estimate of SRHS B2,
  ### 
  ###     ...and, for the function-specific arguments, which aren't required for each SRHS discard proxy method...
  ###         'years.ratio' identifies the years over which any required ratios are calculated (e.g., the 'super-ratio' ),
  ###         'bp.ratios' is a table of ratios calculated from the SRHS bioprofile (size) data to support the
  ###                   SRHS-BIOSAMP approach. These ratios are of size ranges of landed fish meant to reflect the amount
  ###                   of (regulatory) discards, relative to landings, expected from the implementation of a size limit...
  ###         'yr.avg' identifies the number of years over which smoothing will occur (e.g. 3-yr vs. 5-year averages ),
  ###                   which is obviously only applicable in any approaches that 'smooth' the estimated discard rates...
  
  
  list.proxies = list()
  
  blah1 = colnames(genrec.table)[ colnames(genrec.table) %notin% c('AB1','B2','VAR_AB1','VAR_B2') ]
  blah2 = colnames(srhs.table)[   colnames(srhs.table  ) %notin% c('AB1','B2','VAR_AB1','VAR_B2') ]
  var.vec = unique( c(blah1,blah2) )
  rm( blah1,blah2 )
  
  
  ### MRIP-CBT METHODS ###
  ### ____________________
  
  if( 'superratio' %in% approaches ) {
    proxy.table = srhsB2.superratio( genrec.table = genrec.table, srhs.table = srhs.table, var.vec = var.vec,
                                     years.impute = years.impute, years.ratio = years.ratio, mode.subset = 'CBT' )
    i = length(list.proxies)+1
    list.proxies[[i]] = proxy.table
    names(list.proxies)[i] = 'SUPERRATIO'
    rm( i, proxy.table )
  }
  
  if( 'mrip-cbt' %in% approaches ) {
    proxy.table = srhsB2.MRIP( genrec.table = genrec.table, srhs.table = srhs.table, var.vec = var.vec,
                               years.impute = years.impute, mode.subset = 'CBT' )
    i = length(list.proxies)+1
    list.proxies[[i]] = proxy.table
    names(list.proxies)[i] = 'MRIP-CBT'
    rm( i, proxy.table )
  }
  
  if( 'mrip-cbt-smooth' %in% approaches ) {
    proxy.table = srhsB2.MRIP.moving.avg( genrec.table = genrec.table, srhs.table = srhs.table, var.vec = var.vec,
                                          years.impute = years.impute, mode.subset = 'CBT', yr.avg = yr.avg )
    i = length(list.proxies)+1
    list.proxies[[i]] = proxy.table
    names(list.proxies)[i] = 'MRIP-CBT-SMOOTH'
    rm( i, proxy.table )
  }
  
  
  ### MRIP-PRIV METHODS ###
  ### _____________________
  
  if( 'superratio-priv' %in% approaches ) {
    proxy.table = srhsB2.superratio( genrec.table = genrec.table, srhs.table = srhs.table, var.vec = var.vec,
                                     years.impute = years.impute, years.ratio = years.ratio, mode.subset = 'PRIV' )
    i = length(list.proxies)+1
    list.proxies[[i]] = proxy.table
    names(list.proxies)[i] = 'SUPERRATIO-PRIV'
    rm( i, proxy.table )
  }
  
  if( 'mrip-priv' %in% approaches ) {
    proxy.table = srhsB2.MRIP( genrec.table = genrec.table, srhs.table = srhs.table, var.vec = var.vec,
                               years.impute = years.impute, mode.subset = 'PRIV' )
    i = length(list.proxies)+1
    list.proxies[[i]] = proxy.table
    names(list.proxies)[i] = 'MRIP-PRIV'
    rm( i, proxy.table )
  }
  
  if( 'mrip-priv-smooth' %in% approaches ) {
    proxy.table = srhsB2.MRIP.moving.avg( genrec.table = genrec.table, srhs.table = srhs.table, var.vec = var.vec,
                                          years.impute = years.impute, mode.subset = 'PRIV', yr.avg = yr.avg )
    i = length(list.proxies)+1
    list.proxies[[i]] = proxy.table
    names(list.proxies)[i] = 'MRIP-PRIV-SMOOTH'
    rm( i, proxy.table )
  }
  
  
  ### SRHS METHODS ###
  ### ________________
  
  if( 'srhs-mean' %in% approaches ) {
    proxy.table = srhsB2.meanSRHS( srhs.table = srhs.table, var.vec = var.vec,
                                   years.impute = years.impute, years.ratio = years.ratio )
    
    i = length(list.proxies)+1
    list.proxies[[i]] = proxy.table
    names(list.proxies)[i] = 'SRHS-MEAN'
    rm( i, proxy.table )
  }
  
  if( 'srhs-biosamp' %in% approaches ) {
    proxy.table = srhsB2.bpSRHS( srhs.table = srhs.table, var.vec = var.vec,
                                 years.impute = years.impute, bp.ratios = bp.ratios )
    
    i = length(list.proxies)+1
    list.proxies[[i]] = proxy.table
    names(list.proxies)[i] = 'SRHS-BIO'
    rm( i, proxy.table )
  }
  
  return( list.proxies )
  
}

### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------

srhs.table.method.comparison = function( list.proxies ) {
  ###     ...where the 'list.proxies' object contains the outputs from the different proxy methods,
  ###         which is collapsed into a single table from which various summaries (i.e., pdf's ) can be generated...
  
  
  var.vec = colnames( list.proxies[[1]]$Catch.Table )
  ###   ...with the understanding that the same 'var.vec' object is used to specify strata for the
  ###     'list.proxies' objects of all proxy methods ( see srhsB2.calculate.proxies() function above ),
  ###     so we can pull this vector from any 'Catch.Table' in our 'list.proxies' object...
  var.vec = var.vec[ var.vec %notin% c('AB1','B2','VAR_AB1','VAR_B2') ]
  
  
  ### (COLLAPSED) TABLE OF PROXY ESTIMATES ###
  
  for( i in 1:length(list.proxies) ) {
    
    dummy.table = list.proxies[[i]]$Proxy.Table %>%
      filter( type == 'DISCARDS' ) %>%
      select( -type )
    
    if( i == 1 ) {
      ###   ...where the actual estimates of SRHS discards are retained from only the first table to allow for comparison
      ###     (of the actual SRHS estimates) to each set of proxy estimates. Note that these values should be identical
      ###     across all the proxy methods and so only need to be retained once (they're filtered out of all subsequent tables)...
      proxy.table = dummy.table %>%
        mutate( METHOD = ifelse( Metric == 'B2', 'SRHS', names(list.proxies)[i] ) ) %>%
        select( -Metric )
      
    } else {
      dummy.table = dummy.table %>%
        filter( Metric == 'proxy' ) %>%
        select( -Metric ) %>%
        mutate( METHOD = names(list.proxies)[i] )
      
      proxy.table = bind_rows( proxy.table, dummy.table )
    }
    rm( dummy.table )
    
  }
  
  proxy.table = proxy.table %>%
    mutate( METHOD = factor( METHOD, levels = c( 'SRHS',names(list.proxies)[1:length(list.proxies)] ) ) ) %>%
    mutate( value = value / 1000 )
  
  proxy.table = proxy.table %>% arrange( METHOD, YEAR, SID )
  
  
  
  ### (COLLAPSED) TABLE OF DISCARD RATES ###
  
  for( i in 1:length(list.proxies) ) {
    
    if( grepl( 'SUPERRATIO',names(list.proxies)[i] ) ) {
      dummy.table = list.proxies[[i]]$MRIP_disc.rate %>%
        select( any_of( c( var.vec[ var.vec %notin% c('NEW_MODEN') ], 'disc.rate' ) ) ) %>%
        mutate( METHOD = names(list.proxies)[i] )
      
      dummy.table = dummy.table %>%
        left_join( list.proxies[[i]]$super.ratio %>%
                     select( any_of( c( var.vec[ var.vec %notin% c('YEAR','NEW_MODEN') ], 'scalar' ) ) ),
                   by = var.vec[ var.vec %notin% c('YEAR','NEW_MODEN') ] ) %>%
        mutate( disc.rate = disc.rate * scalar ) %>%
        select( -c('scalar') )
    }
    if( grepl( 'MRIP',names(list.proxies)[i] ) ) {
      dummy.table = list.proxies[[i]]$MRIP_disc.rate
      if( grepl('SMOOTH',names(list.proxies)[i] ) ) {
        dummy.table = dummy.table %>% rename( disc.rate = smooth.rate )
      }
      dummy.table = dummy.table %>%
        select( any_of( c( var.vec[ var.vec %notin% c('NEW_MODEN') ], 'disc.rate' ) ) ) %>%
        mutate( METHOD = names(list.proxies)[i] )
    }
    if( grepl( 'SRHS-MEAN',names(list.proxies)[i] ) ) {
      ###   ...where the actual SRHS discard rates (2004+) are retained from a single table ('SRHS-MEAN') to allow for comparison
      ###     of the actual SRHS discard rates to those being applied in each set of proxy estimates...
      dummy.table = list.proxies[[i]]$SRHS_disc.rate %>%
        select( any_of( c( var.vec[ var.vec %notin% c('NEW_MODEN') ], 'disc.rate' ) ) ) %>%
        mutate( disc.rate = ifelse( YEAR < 2004, NA, disc.rate ) ) %>%
        mutate( METHOD = 'SRHS' )
      
      ###   ...to which the applied (SHRS-mean) discard ratios also need to be added...
      blah = left_join( dummy.table, list.proxies[[i]]$SRHS_mean.rate,
                        by = var.vec[ var.vec %notin% c('YEAR','NEW_MODEN') ] ) %>%
        mutate( disc.rate = as.numeric(avg.rate) ) %>%
        select( any_of( c( var.vec[ var.vec %notin% c('NEW_MODEN') ], 'disc.rate' ) ) ) %>%
        mutate( METHOD = 'SRHS-MEAN' )
      
      dummy.table = bind_rows( dummy.table, blah )
      rm( blah )
    }
    if( grepl( 'SRHS-BIO',names(list.proxies)[i] ) ) {
      dummy.table = list.proxies[[i]]$SRHS_disc.rate %>%
        mutate( disc.rate = as.numeric(avg.rate) ) %>%
        select( any_of( c( var.vec[ var.vec %notin% c('NEW_MODEN') ], 'disc.rate' ) ) ) %>%
        mutate( METHOD = 'SRHS-BIO' )
    }
    
    if( i == 1 ) {
      rate.table = dummy.table
    } else {
      rate.table = bind_rows( rate.table, dummy.table )
    }
    rm( dummy.table )
    
  }
  
  
  dummy.list = list( proxy.table, rate.table )
  names(dummy.list) = c( 'proxy.table','rate.table' )
  
  return( dummy.list )
  
}

### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------

srhsB2.comparison.pdf = function( proxy.table, years.impute, years.ratio ) {
  ###     ...where the primary object required for this function is the collapsed table of proxy methods,
  ###             as is stored in the 'proxy.table' object returned by the second function (i.e., srhs.table.method.comparison() ),
  ###         'years.impute' identifies those years for which the discard proxies will be used as an estimate of SRHS B2, and
  ###         'years.ratio' identifies the years over which any required ratios are calculated (e.g., the 'super-ratio' ),
  
  
  
  #########################################
  ###   COMPARISON OF PROXY ESTIMATES   ###
  ###         ACROSS METHODS            ###
  #########################################
  
  pdf( paste0( dir,"/Summary Plots_SRHS Proxy Discards by Method.pdf" ), width = 14, height = 8.5 )
  
  ### ALL METHODS ON A SINGLE PLOT ###
  ### ________________________________
  ###
  ###     ...where, at this point in the script, I will leave 'proxy.table' (as is) so as to retain an independent object
  ###       from which a correlation analysis can be conducted ( see code immediately following dev.off() below ).
  ###       In particular, I want to evaluate correlations between the different discard proxies across the entire timeseries
  ###       (e.g., 1981+ ) and not just in those years for which SRHS discard estimates are available (i.e., 2004+ ),
  ###       which is the focus of the comparison plots being produced by this section of code. Therefore, I apply the
  ###       YEAR filter below for these plots ( and save this object as 'proxy.comp' ), but also retain 'proxy.table'
  ###       ( to which no YEAR filter has been applied ) for the subsequent correlation analysis...
  
  proxy.comp = proxy.table %>% filter( YEAR >= 2004 )
  n.years = length(unique(proxy.comp$YEAR)) / 2
  
  mycolors = c( "SRHS" = "black",
                "SUPERRATIO" = "turquoise", "MRIP-CBT" = "blue", "MRIP-CBT-SMOOTH" = "darkorchid1",
                "SUPERRATIO-PRIV" = "green","MRIP-PRIV" = "red", "MRIP-PRIV-SMOOTH" = "orange",
                "SRHS-MEAN" = "gray70", "SRHS-BIO" = "gray30" )
  # scale_color_manual( values = c("deeppink","yellow","firebrick1","darkorchid1","turquoise","green4","brown4",
  #                               "deepskyblue4","orange","lightcoral","mediumorchid","olivedrab2","gray70") ) +
  
  mytype = c( "SRHS" = "solid",
              "SUPERRATIO" = "solid", "MRIP-CBT" = "solid", "MRIP-CBT-SMOOTH" = "dotted",
              "SUPERRATIO-PRIV" = "solid","MRIP-PRIV" = "solid", "MRIP-PRIV-SMOOTH" = "dotted",
              "SRHS-MEAN" = "solid", "SRHS-BIO" = "solid" )
  # scale_linetype_manual( values = c("solid","dashed","dotted","dotdash","longdash","twodash") ) +
  
  
  for( i in 1:length(unique(proxy.comp$SID)) ) {
    
    dummy.table = proxy.comp %>% filter( SID == unique(proxy.comp$SID)[i] )
    
    proxy.plot = ggplot( data=dummy.table, aes( x=YEAR, colour=METHOD, linetype=METHOD ) ) +
      geom_line( aes( y=value ), linewidth=1.2 ) +
      scale_x_continuous( breaks = scales::pretty_breaks( n = n.years ) ) +
      labs( title = paste0( "SRHS Discard Proxy Estimates by Method for SID = ",unique(proxy.comp$SID)[i] ),
            subtitle = paste0( "Shaded Rectangle = Recent Years wherein catch rates were used to adjust Historic Discard Rates",
                               " (i.e., in the 'SUPERRATIO' & 'SRHS-MEAN' Approaches)" ),
            x="Year", y="Thousands of Fish" ) +
      scale_color_manual( values = mycolors ) +
      scale_linetype_manual( values = mytype ) +
      expand_limits(y = 0) +
      # scale_y_continuous(breaks = scales::pretty_breaks(n = 10), labels = scales::comma) +
      theme_bw() +
      theme( text = element_text(size = 11),
             axis.text.x = element_text(angle = 90, vjust=0.5),
             # plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
             legend.position = "bottom",
             # panel.background = element_blank(),
             panel.grid.major = element_line(colour = "grey", linewidth = 0.5),
             panel.grid.minor = element_line(colour = "grey", linewidth = 0.2),
             panel.border = element_rect(colour = "black", fill = NA) )
    
    ###   ...where, in these plots, the 'years.ratio' range is identified with a shaded rectangle to highlight (recent) years
    ###     where catch is influencing historic discard rates ( relevant in SUPERRATIO and SRHS-MEAN approaches )...
    
    ymax.discards = ceiling( max( dummy.table$value ) / 10 ) * 10
    # ymax.discards = ceiling( max( dummy.table$value[ dummy.table$YEAR %in% years.ratio ] ) / 10 ) * 10
    
    proxy.plot = proxy.plot + annotate( 'rect',
                                        xmin = min(years.ratio), xmax = max(years.ratio),
                                        ymin = 0, ymax = ymax.discards,
                                        fill = 'blue', alpha = 0.1 )
    rm( ymax.discards )
    
    grid.arrange( grobs=list( proxy.plot ) )
    
  }
  
  rm( dummy.table, proxy.plot )
  rm( proxy.comp, n.years )
  
  
  
  ### METHOD-SPECIFIC PLOTS ###
  ###   ...which are summaries of the SRHS landings and discard estimates provided by each method individually
  ###     ( by Year and SID ), allowing for an evaluation of the ability of individual methods to produce 'reasonable' estimates,
  ###     which may be impeded in the 'combined' plot produced above if one/couple methods produce highly variable estimates
  ###     that increase the bounds (of the y-axis) to a point where fine-scale evaluations of other methods can no longer be made...
  
  for( i in 1:length(list.proxies) ) {
    
    grid.arrange( grobs=list( list.proxies[[i]]$Plot ) )
    # if( i < length(list.proxies) ) {     grid::grid.newpage()     }
    
  }
  
  dev.off()
  
  
  
  
  ########################
  ###   CORRELATIONS   ###
  ########################
  ###
  ###     ...which are provided as both tables and plots (i.e., heatmaps ) to evaluate the relative agreement
  ###       in trends between:
  ###
  ###           -- each of the SRHS discard proxy methods to identify methods that seem to 'agree' with one another.
  ###             As an example, the super-ratio, MRIP, and MRIP-smooth approaches are all based on annual estimates
  ###             of MRIP discard rates (e.g., from the MRIP-CBT mode ), and so correlations between these methods
  ###             tend to be fairly high...
  ###
  ###           -- individual SRHS discard proxy methods with the actual SRHS discard estimates, to identify methods
  ###             that seem to provide a reasonable set of discard proxies. Note that for these comparisons, I remove
  ###             any (actual) discard estimates in years for which estimates were considered unreliable (e.g., 2004-2007 )
  ###             as the decision has already been made to substitute these estimates (in the assessment) with imputed estimates,
  ###             and so these years are excluded from this evaluation too (of the relative suitability of individual approaches )...
  
  proxy.table = proxy.table %>%
    mutate( value = ifelse( YEAR %in% years.impute & METHOD == 'SRHS', NA, value  ) )
    ###   ...for which any unreliable SRHS discard estimates are being replaced by imputed estimates, typically b/w 2004-2007...
  
  
  pdf( paste0( dir,"/Correlations_SRHS Proxy Discards.pdf" ), width = 14, height = 8.5 )
  ###   ...for which I set the height & width of the output pdf to that which is considered 'landscape'. Note that
  ###       standard 'portrait' dimensions are the default by this function, but could be defined with:
  ###                   pdf( file = paste0('...'), width = 8.5, height = 11.5 )
  
  
  cor.table = list()
  
  for( i in 1:length(unique(proxy.table$SID)) ) {
    
    dummy.table = proxy.table %>%
      filter( SID == unique(proxy.table$SID)[i] ) %>%
      pivot_wider( names_from = 'METHOD', values_from = 'value' ) %>%
      select( -c('YEAR','NEW_MODEN','SID') )
    
    cor.table[[i]] = cor( dummy.table, use = 'pairwise.complete.obs' )
    names(cor.table)[i] = unique(proxy.table$SID)[i]
    
    dummy.table = round( cor.table[[i]], 4 )
    
    ###   ...to which I will redefine the aesthetic parameters of my tableGrob object
    ###     ( to fill cells with colors based on the strength of the correlation coefficients )
    ###     using the approach outlined at:
    ###         https://stackoverflow.com/questions/23819209/change-text-color-for-cells-using-tablegrob
    
    cols.matrix = as.character(dummy.table)
    cols.matrix[                           abs(dummy.table) == 1.0 ] = 'black'
    cols.matrix[ abs(dummy.table) <  1.0 & abs(dummy.table) >= 0.8 ] = 'green'
    cols.matrix[ abs(dummy.table) <  0.8 & abs(dummy.table) >= 0.5 ] = 'yellow'
    cols.matrix[ abs(dummy.table) <  0.5                           ] = 'white'
    cols.matrix = matrix( cols.matrix, nrow=dim(dummy.table)[1], ncol=dim(dummy.table)[2] )
    
    tt <- ttheme_default( core = list( bg_params = list( fill=cols.matrix ) ) )     ### FILL COLOR
    # tt <- ttheme_default( core = list( fg_params = list(  col=cols.matrix ) ) )   ### TEXT COLOR
    rm( cols.matrix )
    
    dummy.text.main = paste0( 'CORRELATIONS between Discard Estimates for SID = ',unique(proxy.table$SID)[i] )
    dummy.text.sub1 = paste0( '(1) Between Proxy Methods (ALL Years - ',
                              min(proxy.table$YEAR),':',max(proxy.table$YEAR),')',  '\n',
                              '(2) Actual SRHS Estimates vs. Proxies (',
                              min( proxy.table$YEAR[ !is.na(proxy.table$value[proxy.table$METHOD=='SRHS']) ] ),':',
                              max( proxy.table$YEAR[ !is.na(proxy.table$value[proxy.table$METHOD=='SRHS']) ] ),')' )
    dummy.text.sub2 = paste0(  'BLACK  :  |R|= 1.0', '\n',
                               'GREEN  :  0.8 <=|R|< 1.0', '\n',
                              'YELLOW  :  0.5 <=|R|< 0.8', '\n',
                               'WHITE  :  |R|< 0.5' )
    
    dummy.plot = ggpubr::ggarrange( textGrob( dummy.text.main, gp = gpar( fontsize=24, fontface='bold') ),
                                    textGrob( dummy.text.sub1, gp = gpar( fontsize=13, fontface='bold') ),
                                    textGrob( dummy.text.sub2, gp = gpar( fontsize=13, fontface='bold') ),
                                    tableGrob( dummy.table, theme = tt ),
                                    nrow=4, ncol=1, heights=c( 0.20, 0.15, 0.15, 0.50 ) )
    grid.arrange( grobs=list( dummy.plot ) )
    rm( dummy.plot, dummy.text.main, dummy.text.sub1, dummy.text.sub2, dummy.table, tt )
    
    
    ### The last summary plot provided is a heatmap...
    dummy.plot = corrplot( cor.table[[i]] )
    cor.plot = ggpubr::ggarrange( dummy.plot, nrow=1, ncol=1 )
    grid.arrange( grobs=list( cor.plot ) )
    rm( cor.plot, dummy.plot )
  }
  rm( i, cor.table )
  
  dev.off()
  
  
}

### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------

srhsB2.superratio = function( genrec.table, srhs.table, var.vec, years.ratio, years.impute, mode.subset ) {
  ###     ...where 'genrec.table' is the table of GenRec catch estimates (AB1 & B2) by YEAR, MODE, and SID (if applicable),
  ###         'srhs.table' is the table of SRHS catch estimates (AB1 & B2) by YEAR, MODE, and SID (if applicable),
  ###         'var.vec' identifies those variables/strata over which proxies are to be calculated,
  ###         'years.ratio' identifies the years over which any required ratios (i.e., the 'super-ratio' ) is calculated,
  ###         'years.impute' identifies those years for which the discard proxy will be used as an estimate of SRHS B2, and
  ###         'mode.subset' is the mode from which discard rates will be calculated. Note that,
  ###                 according to 'best practices', the super-ratio should be calculated from MRIP-CBT estimates...
  
  
  ###   This approach is considered 'best practices' for SRHS discard proxies (SEDAR-PW-07), and is described as:
  ###         "Calculate the ratio of the mean ratio of SRHS discard:landings (2004-xxxx) and MRIP CH discard:landings
  ###           (2004-xxxx). Apply this ratio to the yearly MRIP charter boat discard:landings ratio (xxxx-2003) to
  ###           estimate the yearly SRHS discard:landings ratio (xxxx-2003). This ratio is then applied to the SRHS
  ###           landings (xxxx-2003) to estimate headboat discards (xxxx-2003)" ( Option4 in the S68 AW report ).
  ###   The logic behind this approach is that the super-ratio(s) represent the relative difference in discarding rate
  ###   between the MRIP and SRHS programs, and so using it to scale MRIP-CBT discard rates back-in-time (e.g., 1981-2003 )
  ###   is a way of converting MRIP discarding rates into a type of 'SRHS currency'. Additionally, this approach uses
  ###   discard rates calculated from the same year as to which discards are to be imputed, and so any changes in management
  ###   or year class effects for that year are incorporated into the final discard proxy estimates.
  
  
  genrec.table = genrec.table %>% ungroup()
    srhs.table =   srhs.table %>% ungroup()
  
  
  ### Calculate MRIP discard rates (B2:AB1) ###
  ### _________________________________________
  
  meanratio.MRIP = genrec.table %>%
    filter( NEW_MODEN == mode.subset ) %>%
    select( -c(NEW_MODEN) ) %>%
    
    group_by( across( any_of( var.vec[ var.vec %notin% c('NEW_MODEN') ] ) ) ) %>%
    mutate( MRIP.rate = ifelse( AB1==0, NA, B2/AB1 ) ) %>%
    ###     ...where years that have no landings are excluded as they are assumed uninformative.
    ###       I considered setting the discard rate = 0 for these strata, but it is entirely possible
    ###       to have AB1=0 but B2>0 (e.g., regulation action that heavily impeded harvest ) and so
    ###       these strata are simply dropped from the calculations...
    mutate( VAR.rate = ifelse( AB1==0, NA,
                               ( VAR_B2/(AB1^2) ) + ( ((B2^2)*VAR_AB1)/(AB1^4) ) ) )
    ###     ...which is the variance of a ratio for two independent random variables,
    ###       as approximated using a Taylor Series expansion ( S74-DW-10, Equation 2 )...
  
  ###   ...where I save a copy of this table as an output of this function...
  discard.rate.MRIP = meanratio.MRIP %>%
      rename(  var.ab1  = VAR_AB1,
               var.b2   = VAR_B2,
              disc.rate = MRIP.rate,
               var.rate =  VAR.rate )
      # pivot_wider( names_from = 'SID',
      #              values_from = c('AB1','var.ab1','B2','var.b2','disc.rate','var.rate'),
      #              names_glue = "{SID}_{.value}" )
  
  meanratio.MRIP = meanratio.MRIP %>%
    filter( YEAR %in% years.ratio ) %>%
    select( -c(AB1,B2,VAR_AB1,VAR_B2) )
  
  
  ### Average MRIP discard rate over 'years.ratio' ###
  meanratio.MRIP = meanratio.MRIP %>%
    group_by( across( any_of( var.vec[ var.vec %notin% c('YEAR','NEW_MODEN') ] ) ) ) %>%
    summarise( avg.rate = mean( MRIP.rate, na.rm=TRUE ),
               var.rate = sum( meanratio.MRIP$VAR.rate, na.rm=TRUE ) /
                          ( length( meanratio.MRIP$VAR.rate[ !is.na(meanratio.MRIP$VAR.rate) ] )^2 ) )
    # mutate( avg.rate = ifelse( is.nan(avg.rate), NA, avg.rate ) )
    # ###   ...where 'avg.rate' = <NaN> for any strata where the only records returned have 'MRIP.rate' = <NA> ,
    # ###     which were identified above as any strata with no observed landings (i.e., AB1 = 0, B2 > 0 )...
  
  
  
  ### Calculate SRHS discard rates (B2:AB1) ###
  ### _________________________________________
  
  meanratio.SRHS = srhs.table %>%
    
    group_by( across( any_of( var.vec[ var.vec %notin% c('NEW_MODEN') ] ) ) ) %>%
    mutate( SRHS.rate = ifelse( AB1==0, NA, B2/AB1 ) ) %>%
    mutate(  VAR.rate = ifelse( AB1==0, NA,
                                ( VAR_B2/(AB1^2) ) + ( ((B2^2)*VAR_AB1)/(AB1^4) ) ) )
    ###     ...where (like MRIP) years that have no landings are excluded as they are assumed uninformative...
  
  ###   ...where I save a copy of this table as an output of this function...
  discard.rate.SRHS = meanratio.SRHS %>%
      rename(   var.ab1  = VAR_AB1,
                var.b2   = VAR_B2,
               disc.rate = SRHS.rate,
                var.rate =  VAR.rate )
      # pivot_wider( names_from = 'SID',
      #              values_from = c('AB1','var.ab1','B2','var.b2','disc.rate','var.rate'),
      #              names_glue = "{SID}_{.value}" )
  
  meanratio.SRHS = meanratio.SRHS %>%
    filter( YEAR %in% years.ratio ) %>%
    select( -c(AB1,B2,VAR_AB1,VAR_B2) )
  
  
  ### Average SRHS discard rate over 'years.ratio' ###
  meanratio.SRHS = meanratio.SRHS %>%
    group_by( across( any_of( var.vec[ var.vec %notin% c('YEAR','NEW_MODEN') ] ) ) ) %>%
    summarise( avg.rate = mean( SRHS.rate, na.rm=TRUE ),
               var.rate = sum( meanratio.SRHS$VAR.rate, na.rm=TRUE ) /
                         ( length( meanratio.SRHS$VAR.rate[ !is.na(meanratio.SRHS$VAR.rate) ] )^2 ) )
    # mutate( avg.rate = ifelse( is.nan(avg.rate), NA, avg.rate ) )
    # ###     ...where (like MRIP) strata that have no landings are excluded as they are assumed uninformative...
  
  
  ### Calculate Super-Ratio ###
  ### _________________________
  ###
  ###     ...where superratio = meanSRHS / meanMRIP ( by SID if applicable )
  
  superratio.table = full_join( meanratio.MRIP, meanratio.SRHS,
                                by = var.vec[ var.vec %notin% c('YEAR','NEW_MODEN') ],
                                suffix = c('.MRIP','.SRHS') ) %>%
    mutate( scalar = ifelse( avg.rate.MRIP==0 | avg.rate.SRHS==0, NA, avg.rate.SRHS/avg.rate.MRIP ),
               var = ifelse( avg.rate.MRIP==0 | avg.rate.SRHS==0, NA,
                           ( var.rate.SRHS/(avg.rate.MRIP^2) ) + ( ( (avg.rate.SRHS^2)*var.rate.MRIP )/(avg.rate.MRIP^4) ) ) )
    ###   ...where I set 'scalar' = <NA> for those strata where data is insufficient to estimate a 'superratio'
    ###     (i.e., where the (avg) discard rates from either survey are 0 or <NA> ). In this way, the associated
    ###     proxy discard estimates for these strata will also not calculated ( proxy = <NA> ) to highlight
    ###     the inadequacy of the data to support estimation at the current stratification level...
  rm( meanratio.MRIP, meanratio.SRHS )
  
  ###   ...where a separate 'superratio' table is created below, allowing 'superratio.table' to be retained
  ###       (as is) and provided as an output from this function...
  
  superratio = superratio.table %>%
    select( -c('avg.rate.MRIP','avg.rate.SRHS','var.rate.MRIP','var.rate.SRHS') )
  
  
  ### Apply 'superratio' to the associated MRIP discard rates ###
  ### ___________________________________________________________
  ###     ...wherein the super-ratio(s) calculated above represent the (average) relative difference in discarding rate
  ###       between the MRIP and SRHS programs, and so the super-ratio is used to scale MRIP discard rates back-in-time
  ###       (e.g., 2000-2003 ) and to convert these (MRIP) discarding rates into a type of 'SRHS currency'...
  
  discard.rate = genrec.table %>%
    
    ###   ...within which I recalculate the MRIP discard rates & associated variances for the appropriate mode...
    ###       (i.e., charter or private )...
    filter( NEW_MODEN == mode.subset ) %>%
    mutate( MRIP.rate = ifelse( AB1==0, NA, B2/AB1 ) ) %>%
    mutate(  VAR.rate = ifelse( AB1==0, NA,
                              ( VAR_B2/(AB1^2) ) + ( ((B2^2)*VAR_AB1)/(AB1^4) ) ) ) %>%
    
    ###   ...which are joined with and scaled by the associated super-ratios...
    full_join( superratio, by = var.vec[ var.vec %notin% c('YEAR','NEW_MODEN') ] ) %>%
    
    mutate( rescaled.rate = ifelse( AB1>0, MRIP.rate*scalar, NA ) ) %>%
    ###   ...Note that 'rescaled.rate' can still be <NA> even if AB1>0 (i.e., when 'scalar' = <NA> )
    mutate( rescaled.var  = ifelse( is.na(rescaled.rate), NA,
                                    ( (MRIP.rate^2)*var ) + ( (scalar^2)*VAR.rate ) - ( var*VAR.rate ) ) ) %>%
    ###     ...which is the variance of the product of two independent random variables,
    ###       as is being approximated using Goodman's Formula ( S74-DW-10, Equation 5 )...
    
    select( -c('NEW_MODEN','AB1','B2','VAR_AB1','VAR_B2','MRIP.rate','VAR.rate','scalar','var') )
  rm( superratio )
  
  
  
  ### Calculate the SRHS Discard Proxies ###
  ### ______________________________________
  ###     ...which, for this method, is done by multiplying the re-scaled annual discard rates of MRIP-CBT (see above)
  ###       by the corresponding (annual) SRHS landings estimates as a proxy for SRHS discard estimates...
  
  srhsB2.proxy = srhs.table %>% ungroup() %>%
    left_join( discard.rate, by = var.vec[ var.vec %notin% c('NEW_MODEN') ] )
  rm( discard.rate )
  
  srhsB2.proxy = srhsB2.proxy %>%
    # mutate( rescaled.rate = ifelse( is.na( rescaled.rate ), 0, rescaled.rate ) ) %>%
    # ###   ...where I considered adjusting any 'rescaled.rate' = <NA> values to equal zero, but decided against it
    # ###     so as that the resultant proxy estimates would also be equal to <NA> , which would serve to highlight
    # ###     that the data is inadequate to support estimation at the current level of (spatial) stratification
    # ###     (e.g., no discard rate being estimated from the MRIP survey -- AB1 = B2 = 0 )...
    
    mutate( proxy     = AB1 * rescaled.rate ) %>%
    mutate( proxy.var = ( (AB1^2)*rescaled.var ) + ( (rescaled.rate^2)*VAR_AB1 ) - ( rescaled.var*VAR_AB1 ) )
    ###     ...which is the variance of the product of two independent random variables,
    ###       as is being approximated using Goodman's Formula ( S74-DW-10, Equation 5 )...
  
  
  ### Summary Plot ###
  ### ________________
  ###
  ###   Before substituting 'proxy' discard estimates into 'B2' for the selected years (i.e., in 'years.ratio' ),
  ###   I first construct a summary figure to compare trends in 'actual' SRHS discard estimates vs. those imputed...
  
  dummy.table = srhsB2.proxy %>%
    select( -c('VAR_AB1','VAR_B2','rescaled.rate','rescaled.var','proxy.var') ) %>%
    
    mutate( SID = factor( SID, levels = levels(genrec.table$SID) ) ) %>%
    ###     ...wherein I format 'SID' geographically (i.e., as defined in 'genrec.table' ) to ensure
    ###       the proper min/max values are applied when adding shaded rectangles to these plots below
    ###       (i.e., to identify what years are considered when calculating/applying discard rates )...
    
    mutate( B2 = ifelse( YEAR < 2004 & B2 == 0, NA, B2 ) ) %>%
    ###   ...where SRHS didn't start collecting discard info until 2004 and so I don't want these years graphed
    ###     on the summary figure (i.e., set B2 = <NA> )...
    pivot_longer( cols = c('AB1','B2','proxy'), names_to = 'Metric' ) %>%
    mutate( value = value / 1000,
             type = ifelse( Metric == 'AB1', 'LANDINGS', 'DISCARDS' ) ) %>%
    mutate(  type = factor( type, levels = c('LANDINGS','DISCARDS') ) )
  
  n.years = length(unique(dummy.table$YEAR)) / 2
  
  
  proxy.plot = ggplot( data=dummy.table ) +
    geom_line( aes( x=YEAR, y=value, colour=Metric ), linewidth=1.2 ) +
    
    facet_grid( SID ~ type , scales = 'free' ) +
    labs( title = paste0( "Super-Ratio Approach (",mode.subset,"): Headboat Discard Proxies by Area" ),
          subtitle = paste0( "Discard Proxies (",min(years.impute),":",max(years.impute),", Green)",
                             " from corresponding MRIP-",mode.subset," Discard Rates scaled by",
                             " SRHS:MRIP Super-Ratios (",min(years.ratio),":",max(years.ratio),", Blue)" ),
          x="Year", y="Thousands of Fish" ) +
    
    scale_x_continuous( breaks = scales::pretty_breaks( n = n.years ) ) +
    scale_fill_manual( values = c("red","yellow","blue") ) +
    expand_limits(y = 0) +
    # scale_y_continuous(breaks = scales::pretty_breaks(n = 10), labels = scales::comma) +
    
    theme_bw() +
    theme( text = element_text(size = 11),
           axis.text.x = element_text(angle = 90, vjust=0.5),
           # plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
           legend.position = "bottom",
           # panel.background = element_blank(),
           panel.grid.major = element_line(colour = "grey", linewidth = 0.5),
           panel.grid.minor = element_line(colour = "grey", linewidth = 0.2),
           panel.border = element_rect(colour = "black", fill = NA) )
  
  rm( n.years )
  
  
  ###   To these plots, I then add shaded rectangles to highlight years wherein:
  ###     (1) proxies of SRHS discards (i.e., those imputed by the selected method ) are to be used within the assessment,
  ###           which are highlighted in GREEN
  ###     (2) catch rates in recent years are used to inform (historic) SRHS discard proxies, which are highlighted in BLUE
  ###           and only applicable to a couple proxy methods (i.e., estimates of super-ratios and mean(SRHS) catch rates )
  ###   Note that these rectangles are only drawn on the 'DISCARDS' facet of these plots.
  
  ###   The approach used to construct these rectangles differs for a 'SID' assessment, in that the max(discard) value is
  ###   likely to be different across different SID domains ( vs. region-wide assessments where all estimates are summarized
  ###   for a single spatial domain ). For SID assessments, I therefore start by extracting the y-axis ranges selected in the
  ###   plot above ( which has scales='free' and so R has already identified the appropriate y-scale for each SID domain ),
  ###   which are then used to define the (y-axis) borders of my shaded rectangles...
  
  loc.disc.col  = which( levels(dummy.table$type) == 'DISCARDS' )
  
  y.discards = data.frame( MIN = 0, MAX = 0 )
  for( i in 1:length( levels(dummy.table$SID) ) ) {
    y.discards[i,] = layer_scales( proxy.plot, i=i, j=loc.disc.col )$y$range$range
  }
  rm( i, loc.disc.col )
  
  y.discards$type = factor('DISCARDS')
  y.discards$SID  = levels(dummy.table$SID)
  ###     ...to which I identify the catch type (i.e., 'DISCARDS' ) and 'SID' domain to which each MIN/MAX range pertains...
  
  proxy.plot = proxy.plot +
    geom_rect( data = y.discards,
               aes( xmin = min(years.impute), xmax = max(years.impute), ymin = MIN, ymax = MAX ),
               fill = 'green', alpha = 0.1 )
  
  proxy.plot = proxy.plot +
    geom_rect( data = y.discards,
               aes( xmin = min(years.ratio), xmax = max(years.ratio), ymin = MIN, ymax = MAX ),
               fill = 'blue', alpha = 0.1 )
  
  rm( y.discards )
  
  
  ###   ...where the final step in this function is to produce the 'final' catch tables, with a single (B2) discard timeseries
  ###       being populated from either the discard proxies or actual SRHS discard estimates. However, before doing this,
  ###       I save a copy of this table ( as an output of the function ) to allow for comparisons of proxy discard estimates
  ###       across methods, in particular for those years where SRHS discard estimates exist (e.g., 2004+ ).
  ###   These comparisons are made in my srhsB2.comparison.pdf() function, which call these 'proxy.table' objects...
  proxy.table = dummy.table %>% mutate( value = value * 1000 )
  rm( dummy.table )
  
  
  
  ### Final SRHS discard proxy table ###
  ### __________________________________
  ###
  ###     ...for which the last step is just to substitute the discard proxy estimates into the SRHS catch table
  ###       for those years (1) when SRHS didn't estimate discards and (2) discards are assumed non-negligible...
  
  srhsB2.proxy = srhsB2.proxy %>%
    
    mutate( B2     = ifelse( YEAR %in% years.impute, as.integer(proxy), B2 ),
            VAR_B2 = ifelse( YEAR %in% years.impute,         proxy.var, VAR_B2 ) ) %>%
    
    select( -c('rescaled.rate','rescaled.var','proxy','proxy.var') )
  
  
  
  output.list = list( srhsB2.proxy, proxy.table, proxy.plot, discard.rate.MRIP, discard.rate.SRHS, superratio.table )
  names( output.list ) = c( 'Catch.Table', 'Proxy.Table', 'Plot', 'MRIP_disc.rate', 'SRHS_disc.rate', 'super.ratio' )
  
  return( output.list )
  
}

### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------

srhsB2.MRIP = function( genrec.table, srhs.table, var.vec, years.impute, mode.subset ) {
  ###     ...where 'genrec.table' is the table of GenRec catch estimates (AB1 & B2) by YEAR, MODE, and SID (if applicable),
  ###         'srhs.table' is the table of SRHS catch estimates (AB1 & B2) by YEAR, MODE, and SID (if applicable),
  ###         'var.vec' identifies those variables/strata over which proxies are to be calculated,
  ###         'years.impute' identifies those years for which the discard proxy will be used as an estimate of SRHS B2, and
  ###         'mode.subset' is the (MRIP) mode from which discard rates will be calculated and used to inform SRHS discards
  
  
  ###   This function was written to estimate discard proxies for two approaches that have been used in past SEDARs:
  ###         "Apply the MRIP private boat discard:landings ratio to estimated headboat landings to estimate headboat discards
  ###             from xxxx-2003" ( Option1 in the S68 AW report )
  ###         "Apply the MRIP charter boat discard:landings ratio to estimated headboat landings to estimate headboat discards
  ###             from xxxx-2003" ( Option2 in the S68 AW report )
  ###   ...for which the 'mode.subset' argument is used to distinguish the two approaches...
  ###   Like the 'Super Ratio' approach, this method uses discard rates calculated from the same year as which discards
  ###   are to be imputed, and so any changes in management or year class effects for that year are incorporated into
  ###   the final discard proxy estimates. However, these discard rates are provided in a 'MRIP currency', and so any
  ###   differences in discarding rate between the SRHS and MRIP programs are not being accounted for...
  
  
  genrec.table = genrec.table %>% ungroup()
  srhs.table =   srhs.table %>% ungroup()
  
  
  ### Calculate MRIP discard rates (B2:AB1) ###
  ### _________________________________________
  
  discard.rate = genrec.table %>%
    filter( NEW_MODEN %in% mode.subset ) %>%
    select( -c('NEW_MODEN') ) %>%
    
    mutate( MRIP.rate = ifelse( AB1==0, NA, B2/AB1 ) ) %>%
    ###     ...where years that have no landings are excluded as they are assumed uninformative. I considered setting
    ###       the discard rate = 0 for these strata, but it is entirely possible to have AB1=0 but B2>0 (e.g., regulation
    ###       action that heavily impeded harvest ) and so these strata are simply dropped from the calculations...
    mutate( VAR.rate = ifelse( AB1==0, NA,
                             ( VAR_B2/(AB1^2) ) + ( ((B2^2)*VAR_AB1)/(AB1^4) ) ) )
    ###     ...which is the variance of a ratio for two independent random variables,
    ###       as approximated using a Taylor Series expansion ( S74-DW-10, Equation 2 )...
  
  
  ###   ...where I save a copy of this table as an output of this function...
  discard.rate.MRIP = discard.rate %>%
      rename(  var.ab1  = VAR_AB1,
               var.b2   = VAR_B2,
              disc.rate = MRIP.rate,
               var.rate =  VAR.rate )
      # pivot_wider( names_from = 'SID',
      #              values_from = c('AB1','var.ab1','B2','var.b2','disc.rate','var.rate'),
      #              names_glue = "{SID}_{.value}" )
  
  discard.rate = discard.rate %>% select( -c(AB1,B2,VAR_AB1,VAR_B2) )
  
  
  ### Calculate the SRHS Discard Proxies ###
  ### ______________________________________
  ###
  ###     ...which, for this method, is done by multiplying the (mode-specific) MRIP discard rate by the
  ###       annual SRHS landings estimates as a proxy for SRHS discard estimates...
  
  srhsB2.proxy = srhs.table %>% left_join( discard.rate, by = var.vec[ var.vec %notin% c('NEW_MODEN') ] )
  rm( discard.rate )
  
  
  srhsB2.proxy = srhsB2.proxy %>%
    # mutate( MRIP.rate = ifelse( is.na( MRIP.rate ), 0, MRIP.rate ) ) %>%
    # ###   ...where I considered adjusting any 'MRIP.rate' = <NA> values to equal zero, but decided against it
    # ###     so as that the resultant proxy estimates would also be equal to <NA> , which serves to highlight that
    # ###     the data is inadequate to support estimation at the current level of (spatial) stratification
    # ###     (e.g., no discard rate being estimated from the MRIP survey -- AB1 = B2 = 0 )...
    
    mutate( proxy     = AB1 * MRIP.rate ) %>%
    mutate( proxy.var = ( (AB1^2)*VAR.rate ) + ( (MRIP.rate^2)*VAR_AB1 ) - ( VAR.rate*VAR_AB1 ) )
    ###     ...which is the variance of the product of two independent random variables,
    ###       as is being approximated using Goodman's Formula ( S74-DW-10, Equation 5 )...
  
  
  
  ### Summary Plot ###
  ### ________________
  ###
  ###   Before substituting 'proxy' discard estimates into 'B2' for the selected years (i.e., in 'years.ratio' ),
  ###   I first construct a summary figure to compare trends in 'actual' SRHS discard estimates vs. those imputed...
  
  dummy.table = srhsB2.proxy %>%
    select( -c('VAR_AB1','VAR_B2','MRIP.rate','VAR.rate','proxy.var') ) %>%
    
    mutate( SID = factor( SID, levels = levels(genrec.table$SID) ) ) %>%
    ###     ...wherein I format 'SID' geographically (i.e., as defined in 'genrec.table' ) to ensure
    ###       the proper min/max values are applied when adding shaded rectangles to these plots below
    ###       (i.e., to identify what years are considered when calculating/applying discard rates )...
    
    mutate( B2 = ifelse( YEAR < 2004 & B2 == 0, NA, B2 ) ) %>%
    ###   ...where SRHS didn't start collecting discard info until 2004 and so I don't want these years graphed
    ###     on the summary figure (i.e., set B2 = <NA> )...
    pivot_longer( cols = c('AB1','B2','proxy'), names_to = 'Metric' ) %>%
    mutate( value = value / 1000,
            type = ifelse( Metric == 'AB1', 'LANDINGS', 'DISCARDS' ) ) %>%
    mutate(  type = factor( type, levels = c('LANDINGS','DISCARDS') ) )
  
  n.years = length(unique(dummy.table$YEAR)) / 2
  
  
  proxy.plot = ggplot( data=dummy.table ) +
    geom_line( aes( x=YEAR, y=value, colour=Metric ), linewidth=1.2 ) +
    
    facet_grid( SID ~ type , scales = 'free' ) +
    labs( title = paste0( "MRIP Approach (",mode.subset,"): Headboat Discard Proxies by Area" ),
          subtitle = paste0( "Discard Proxies (",min(years.impute),":",max(years.impute),", Green)",
                             " from corresponding MRIP-",mode.subset," Discard Rates" ),
          x="Year", y="Thousands of Fish" ) +
    
    scale_x_continuous( breaks = scales::pretty_breaks( n = n.years ) ) +
    scale_fill_manual( values = c("red","yellow","blue") ) +
    expand_limits(y = 0) +
    # scale_y_continuous(breaks = scales::pretty_breaks(n = 10), labels = scales::comma) +
    
    theme_bw() +
    theme( text = element_text(size = 11),
           axis.text.x = element_text(angle = 90, vjust=0.5),
           # plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
           legend.position = "bottom",
           # panel.background = element_blank(),
           panel.grid.major = element_line(colour = "grey", linewidth = 0.5),
           panel.grid.minor = element_line(colour = "grey", linewidth = 0.2),
           panel.border = element_rect(colour = "black", fill = NA) )
  
  rm( n.years )
  
  
  ###   To these plots, I then add shaded rectangles ( to the 'DISCARDS' facet ) to highlight years wherein:
  ###     (1) proxies of SRHS discards (i.e., those imputed by the selected method ) are to be used within the assessment,
  ###           which are highlighted in GREEN
  ###     (2) catch rates in recent years are used to inform (historic) SRHS discard proxies, which are highlighted in BLUE
  ###           and only applicable to a couple proxy methods (i.e., estimates of super-ratios and mean(SRHS) catch rates )
  ###   Note that these rectangles are only drawn on the 'DISCARDS' facet of these plots.
  
  loc.disc.col  = which( levels(dummy.table$type) == 'DISCARDS' )
  
  y.discards = data.frame( MIN = 0, MAX = 0 )
  for( i in 1:length( levels(dummy.table$SID) ) ) {
    y.discards[i,] = layer_scales( proxy.plot, i=i, j=loc.disc.col )$y$range$range
  }
  rm( i, loc.disc.col )
  
  y.discards$type = factor('DISCARDS')
  y.discards$SID  = levels(dummy.table$SID)
  
  proxy.plot = proxy.plot +
    geom_rect( data = y.discards,
               aes( xmin = min(years.impute), xmax = max(years.impute), ymin = MIN, ymax = MAX ),
               fill = 'green', alpha = 0.1 )
  
  # proxy.plot = proxy.plot +
  #   geom_rect( data = y.discards,
  #              aes( xmin = min(years.ratio), xmax = max(years.ratio), ymin = MIN, ymax = MAX ),
  #              fill = 'blue', alpha = 0.1 )
  
  rm( y.discards )
  
  
  ###   ...where the final step in this function is to produce the 'final' catch tables, with a single (B2) discard timeseries
  ###       being populated from either the discard proxies or actual SRHS discard estimates. However, before doing this,
  ###       I save a copy of this table ( as an output of the function ) to allow for comparisons of proxy discard estimates
  ###       across methods, in particular for those years where SRHS discard estimates exist (e.g., 2004+ ).
  ###   These comparisons are made in my srhsB2.comparison.pdf() function, which call these 'proxy.table' objects...
  proxy.table = dummy.table %>% mutate( value = value * 1000 )
  rm( dummy.table )
  
  
  ### Final SRHS discard proxy table ###
  ### __________________________________
  ###
  ###     ...for which the last step is just to substitute the discard proxy estimates into the SRHS catch table
  ###       for those years (1) when SRHS didn't estimate discards and (2) discards are assumed non-negligible...
  
  srhsB2.proxy = srhsB2.proxy %>%
    
    mutate( B2     = ifelse( YEAR %in% years.impute, as.integer(proxy), B2 ),
            VAR_B2 = ifelse( YEAR %in% years.impute,         proxy.var, VAR_B2 ) ) %>%
    
    select( -c('MRIP.rate','VAR.rate','proxy','proxy.var') )
  
  
  
  output.list = list( srhsB2.proxy, proxy.table, proxy.plot, discard.rate.MRIP )
  names( output.list ) = c( 'Catch.Table', 'Proxy.Table', 'Plot', 'MRIP_disc.rate' )
  
  return( output.list )
  
}

### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------

srhsB2.MRIP.moving.avg = function( genrec.table, srhs.table, var.vec, years.impute, mode.subset, yr.avg = c(3,5) ) {
  ###     ...where 'genrec.table' is the table of GenRec catch estimates (AB1 & B2) by YEAR, MODE, and SID (if applicable),
  ###         'srhs.table' is the table of SRHS catch estimates (AB1 & B2) by YEAR, MODE, and SID (if applicable),
  ###         'var.vec' identifies those variables/strata over which proxies are to be calculated,
  ###         'years.impute' identifies those years for which the discard proxy will be used as an estimate of SRHS B2,
  ###         'mode.subset' is the (MRIP) mode from which discard rates will be calculated and used to inform SRHS discards,
  ###         'yr.avg' identifies the number of years over which smoothing will occur (e.g. 3-yr vs. 5-year averages )
  
  
  ###   This approach has been applied in past SEDARs and can be described as:
  ###         "Apply a three year rolling average MRIP charter boat discard:landings ratio to estimated headboat landings
  ###             to estimate headboat discards (xxxx-2003)" ( Option3 in the S68 AW report )
  ###   Like the 'Super Ratio' approach, this method uses discard rates calculated from the same year as which discards
  ###   are to be imputed, and so any changes in management or year class effects for that year are **PARTIALLY** incorporated
  ###   into the final discard proxy estimates. Note that the reason I highlight **PARTIALLY** is that this approach is
  ###   designed to smooth over any strong year-specific effects, which is beneficial for highly variable/uncertain estimates
  ###   but more problematic when these year-specific signals are "real" (e.g., management changes, recruitment pulses).
  ###   Additionally, these discard rates are provided in a 'MRIP currency', and so any differences in discarding rate between
  ###   the SRHS and MRIP programs are not being accounted for...
  
  
  genrec.table = genrec.table %>% ungroup()
  srhs.table =   srhs.table %>% ungroup()
  
  
  ### Calculate SMOOTHED discard rates (B2:AB1) from MRIP Estimates ###
  ### _________________________________________________________________
  
  discard.rate = genrec.table %>%
    filter( NEW_MODEN %in% mode.subset ) %>%
    select( -c('NEW_MODEN') ) %>%
    
    mutate( MRIP.rate = ifelse( AB1==0, NA, B2/AB1 ) ) %>%
    ###     ...where years that have no landings are excluded as they are assumed uninformative. I considered setting
    ###       the discard rate = 0 for these strata, but it is entirely possible to have AB1=0 but B2>0 (e.g., regulation
    ###       action that heavily impeded harvest ) and so these strata are simply dropped from the calculations...
    
    mutate( VAR.rate = ifelse( AB1==0, NA,
                             ( VAR_B2/(AB1^2) ) + ( ((B2^2)*VAR_AB1)/(AB1^4) ) ) )
    ###     ...which is the variance of a ratio for two independent random variables,
    ###       as approximated using a Taylor Series expansion ( S74-DW-10, Equation 2 )...
  
  
  ### SMOOTHING ###
  ###     ...where the code below separates the calculations of smoothed discard rates and associated variances
  ###       into distinct tables, 'smooth.table' and 'var.table' respectively...
  smooth.table = discard.rate %>% select( -c('AB1','VAR_AB1','B2','VAR_B2', 'VAR.rate') )
     var.table = discard.rate %>% select( -c('AB1','VAR_AB1','B2','VAR_B2','MRIP.rate') )
  
  
  ###   ...where, before converting this table into 'wide' format, I save the factor levels of 'SID'
  ###     so that they can be reset after the (smoothing) calculations have been conducted...
  area.order = levels( discard.rate$SID )
  smooth.table = smooth.table %>%
    pivot_wider( names_from = 'SID', values_from = 'MRIP.rate' )
  smooth.table[ is.na(smooth.table) ] = 0
  var.table = var.table %>%
    pivot_wider( names_from = 'SID', values_from = 'VAR.rate' )
  var.table[ is.na(var.table) ] = 0
  
  
  for( j in 2:dim(smooth.table)[2] ) {
    ###   ...which skips the first column (i.e., YEAR )...
    
    smooth.table$working = 0
       var.table$working = 0
    
    for( i in 1:dim(smooth.table)[1] ) {
      
      ### Three-Year Moving Average ###
      if( yr.avg == 3 ) {
        
        ### First Year -- use average from first year & two subsequent years
        if( i==1 ) {
          smooth.table$working[i] = sum( smooth.table[ 1:3 , j ] ) / 3
             var.table$working[i] = sum(    var.table[ 1:3 , j ] ) / 9
          
        ### Last Year -- use average from last year & two preceeding years
        } else if( i==dim(smooth.table)[1] ) {
          smooth.table$working[i] = sum( smooth.table[ (i-2):i , j ] ) / 3
             var.table$working[i] = sum(    var.table[ (i-2):i , j ] ) / 9
          
        ### All Other Years -- use average from that year & two adjacent years (before & after)
        } else {
          smooth.table$working[i] = sum( smooth.table[ (i-1):(i+1) , j ] ) / 3
             var.table$working[i] = sum(    var.table[ (i-1):(i+1) , j ] ) / 9
        }
      }
      
      ### Five-Year Moving Average ###
      if( yr.avg == 5 ) {
        
        ### First or Second Year -- use average from first year & two subsequent years
        if( i %in% 1:2 ) {
          smooth.table$working[i] = sum( smooth.table[ 1:5 , j ] ) / 5
             var.table$working[i] = sum(    var.table[ 1:5 , j ] ) / 25
          
          ### Last Year -- use average from last year & two preceeding years
        } else if( i %in% (dim(smooth.table)[1]-1):(dim(smooth.table)[1]) ) {
          smooth.table$working[i] = sum( smooth.table[ (dim(smooth.table)[1]-4):dim(smooth.table)[1] , j ] ) / 5
             var.table$working[i] = sum(    var.table[ (dim(   var.table)[1]-4):dim(   var.table)[1] , j ] ) / 25
          
          ### All Other Years -- use average from that year & two adjacent years (before & after)
        } else {
          smooth.table$working[i] = sum( smooth.table[ (i-2):(i+2) , j ] ) / 5
             var.table$working[i] = sum(    var.table[ (i-2):(i+2) , j ] ) / 25
        }
      }
      
    }
    # colnames(smooth.table)[ which( colnames(smooth.table)=='working' ) ] = paste0( colnames(smooth.table)[j],"_smooth" )
    smooth.table[j] = smooth.table$working
    smooth.table = smooth.table %>% select( -c('working') )
    
    var.table[j] = var.table$working
    var.table = var.table %>% select( -c('working') )
  }
  
  
  smooth.table = smooth.table %>%
    pivot_longer( cols = -c('YEAR'), names_to = 'SID', values_to = 'MRIP.smooth' ) %>%
    mutate( SID = factor( SID, levels = area.order ) )
  var.table = var.table %>%
    pivot_longer( cols = -c('YEAR'), names_to = 'SID', values_to = 'VAR.smooth' ) %>%
    mutate( SID = factor( SID, levels = area.order ) )
  rm( area.order )
  
  discard.rate = discard.rate %>%
    full_join( smooth.table, by = var.vec[ var.vec %notin% c('NEW_MODEN') ] ) %>%
    full_join(    var.table, by = var.vec[ var.vec %notin% c('NEW_MODEN') ] )
  rm( smooth.table, var.table )
  
  
  ###   ...where I save a copy of this table as an output of this function...
  discard.rate.MRIP = discard.rate %>%
    rename(    var.ab1  = VAR_AB1,
               var.b2   = VAR_B2,
               raw.rate = MRIP.rate,
            smooth.rate = MRIP.smooth )
    # pivot_wider( names_from = 'SID',
    #              values_from = c('AB1','var.ab1','B2','var.b2','raw.rate','VAR.rate','smooth.rate','VAR.smooth'),
    #              names_glue = "{SID}_{.value}" )
  
  
  discard.rate = discard.rate %>% select( -c('AB1','VAR_AB1','B2','VAR_B2','MRIP.rate','VAR.rate') )
  
  
  
  ### Calculate the SRHS Discard Proxies ###
  ### ______________________________________
  ###
  ###     ...which, for this method, is done by multiplying the (mode-specific) MRIP discard rate by the
  ###       annual SRHS landings estimates as a proxy for SRHS discard estimates...
  
  srhsB2.proxy = srhs.table %>% left_join( discard.rate, by = var.vec[ var.vec %notin% c('NEW_MODEN') ] )
  rm( discard.rate )
  
  
  srhsB2.proxy = srhsB2.proxy %>%
    
    # mutate( MRIP.smooth = ifelse( is.na( MRIP.smooth ), 0, MRIP.smooth ) ) %>%
    # ###   ...where I considered adjusting any 'MRIP.smooth' = <NA> values to equal zero, but decided against it
    # ###     so as that the resultant proxy estimates would also be equal to <NA> , which serves to highlight that
    # ###     the data is inadequate to support estimation at the current level of (spatial) stratification
    # ###     (e.g., no discard rate being estimated from the MRIP survey -- AB1 = B2 = 0 )...
    # ###   Note that the chances of <NA> values are relatively small in this approach (compared to others)
    # ###   as we're smoothing over multiple years, but its still possible...
    
    mutate( proxy     = AB1 * MRIP.smooth ) %>%
    mutate( proxy.var = ( (AB1^2)*VAR.smooth ) + ( (MRIP.smooth^2)*VAR_AB1 ) - ( VAR.smooth*VAR_AB1 ) )
    ###     ...which is the variance of the product of two independent random variables,
    ###       as is being approximated using Goodman's Formula ( S74-DW-10, Equation 5 )...
  
  
  
  ### Summary Plot ###
  ### ________________
  ###
  ###   Before substituting 'proxy' discard estimates into 'B2' for the selected years (i.e., in 'years.ratio' ),
  ###   I first construct a summary figure to compare trends in 'actual' SRHS discard estimates vs. those imputed...
  
  dummy.table = srhsB2.proxy %>%
    select( -c('VAR_AB1','VAR_B2','MRIP.smooth','VAR.smooth','proxy.var') ) %>%
    
    mutate( SID = factor( SID, levels = levels(genrec.table$SID) ) ) %>%
    ###     ...wherein I format 'SID' geographically (i.e., as defined in 'genrec.table' ) to ensure
    ###       the proper min/max values are applied when adding shaded rectangles to these plots below
    ###       (i.e., to identify what years are considered when calculating/applying discard rates )...
    
    mutate( B2 = ifelse( YEAR < 2004 & B2 == 0, NA, B2 ) ) %>%
    ###   ...where SRHS didn't start collecting discard info until 2004 and so I don't want these years graphed
    ###     on the summary figure (i.e., set B2 = <NA> )...
    pivot_longer( cols = c('AB1','B2','proxy'), names_to = 'Metric' ) %>%
    mutate( value = value / 1000,
            type = ifelse( Metric == 'AB1', 'LANDINGS', 'DISCARDS' ) ) %>%
    mutate(  type = factor( type, levels = c('LANDINGS','DISCARDS') ) )
  
  n.years = length(unique(dummy.table$YEAR)) / 2
  
  
  proxy.plot = ggplot( data=dummy.table ) +
    geom_line( aes( x=YEAR, y=value, colour=Metric ), linewidth=1.2 ) +
    
    facet_grid( SID ~ type , scales = 'free' ) +
    labs( title = paste0( "MRIP ",yr.avg,"-Year Moving-Average Approach (",mode.subset,"): Headboat Discard Proxies by Area" ),
          subtitle = paste0( "Discard Proxies (",min(years.impute),":",max(years.impute),", Green)",
                             " from smoothed MRIP-",mode.subset," Discard Rates" ),
          x="Year", y="Thousands of Fish" ) +
    
    scale_x_continuous( breaks = scales::pretty_breaks( n = n.years ) ) +
    scale_fill_manual( values = c("red","yellow","blue") ) +
    expand_limits(y = 0) +
    # scale_y_continuous(breaks = scales::pretty_breaks(n = 10), labels = scales::comma) +
    
    theme_bw() +
    theme( text = element_text(size = 11),
           axis.text.x = element_text(angle = 90, vjust=0.5),
           # plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
           legend.position = "bottom",
           # panel.background = element_blank(),
           panel.grid.major = element_line(colour = "grey", linewidth = 0.5),
           panel.grid.minor = element_line(colour = "grey", linewidth = 0.2),
           panel.border = element_rect(colour = "black", fill = NA) )
  
  rm( n.years )
  
  
  ###   To these plots, I then add shaded rectangles ( to the 'DISCARDS' facet ) to highlight years wherein:
  ###     (1) proxies of SRHS discards (i.e., those imputed by the selected method ) are to be used within the assessment,
  ###           which are highlighted in GREEN
  ###     (2) catch rates in recent years are used to inform (historic) SRHS discard proxies, which are highlighted in BLUE
  ###           and only applicable to a couple proxy methods (i.e., estimates of super-ratios and mean(SRHS) catch rates )
  ###   Note that these rectangles are only drawn on the 'DISCARDS' facet of these plots.
  
  loc.disc.col  = which( levels(dummy.table$type) == 'DISCARDS' )
  
  y.discards = data.frame( MIN = 0, MAX = 0 )
  for( i in 1:length( levels(dummy.table$SID) ) ) {
    y.discards[i,] = layer_scales( proxy.plot, i=i, j=loc.disc.col )$y$range$range
  }
  rm( i, loc.disc.col )
  
  y.discards$type = factor('DISCARDS')
  y.discards$SID  = levels(dummy.table$SID)
  
  proxy.plot = proxy.plot +
    geom_rect( data = y.discards,
               aes( xmin = min(years.impute), xmax = max(years.impute), ymin = MIN, ymax = MAX ),
               fill = 'green', alpha = 0.1 )
  
  # proxy.plot = proxy.plot +
  #   geom_rect( data = y.discards,
  #              aes( xmin = min(years.ratio), xmax = max(years.ratio), ymin = MIN, ymax = MAX ),
  #              fill = 'blue', alpha = 0.1 )
  
  rm( y.discards )
  
  
  
  ###   ...where the final step in this function is to produce the 'final' catch tables, with a single (B2) discard timeseries
  ###       being populated from either the discard proxies or actual SRHS discard estimates. However, before doing this,
  ###       I save a copy of this table ( as an output of the function ) to allow for comparisons of proxy discard estimates
  ###       across methods, in particular for those years where SRHS discard estimates exist (e.g., 2004+ ).
  ###   These comparisons are made in my srhsB2.comparison.pdf() function, which call these 'proxy.table' objects...
  proxy.table = dummy.table %>% mutate( value = value * 1000 )
  
  rm( dummy.table )
  
  
  
  ### Final SRHS discard proxy table ###
  ### __________________________________
  ###
  ###     ...for which the last step is just to substitute the discard proxy estimates into the SRHS catch table
  ###       for those years (1) when SRHS didn't estimate discards and (2) discards are assumed non-negligible...
  
  srhsB2.proxy = srhsB2.proxy %>%
    mutate( B2     = ifelse( YEAR %in% years.impute, as.integer(proxy), B2 ),
            VAR_B2 = ifelse( YEAR %in% years.impute,         proxy.var, VAR_B2 ) ) %>%
    
    select( -c('MRIP.smooth','VAR.smooth','proxy','proxy.var') )
  
  
  
  output.list = list( srhsB2.proxy, proxy.table, proxy.plot, discard.rate.MRIP )
  names( output.list ) = c( 'Catch.Table', 'Proxy.Table', 'Plot', 'MRIP_disc.rate' )
  
  return( output.list )
  
}

### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------

srhsB2.meanSRHS = function( srhs.table, var.vec, years.ratio, years.impute ) {
  ###     ...where 'srhs.table' is the table of SRHS catch estimates (AB1 & B2) by YEAR, MODE, and SID (if applicable),
  ###         'var.vec' identifies those variables/strata over which proxies are to be calculated,
  ###         'years.ratio' identifies the years over which any required ratios (i.e., mean SRHS discard rate ) is calculated,
  ###         'years.impute' identifies those years for which the discard proxy will be used as an estimate of SRHS B2, and
  
  
  ###   This approach has been applied in past SEDARs and can be described as:
  ###         "Apply a mean SRHS discard:landings ratio (2004-xxxx) to estimated headboat landings to estimate
  ###               headboat discards (xxxx-2003)"
  ###   ...which represented Options 6 & 7 in the S68 AW report with different year ranges tested in each option
  ###       ( #6: 2004-2008 vs. #7: 2004-2018 ), but both used the same basic method...
  ###   The benefit of this approach is that discard rates are being calculated from the same survey for which
  ###   discards are being imputed (i.e., both in the same 'currencies' ). However, this approach requires discard rates
  ###   to be calculated from years that differ from those for which we're imputing discards (i.e., discards are missing,
  ###   hence the need to impute... ), and so this approach (unlike previous methods) does not incorporate any potential
  ###   effects of changes in management or year class effects into the final discard proxy estimates.
  
  
  srhs.table = srhs.table %>% ungroup()
  
  
  ### Calculate SRHS discard rates (B2:AB1) ###
  ### _________________________________________
  
  discard.rate = srhs.table %>%
    
    mutate( SRHS.rate = ifelse( AB1==0, NA, B2/AB1 ) ) %>%
    ###     ...where years that have no landings are excluded as they are assumed uninformative. I considered setting
    ###       the discard rate = 0 for these strata, but it is entirely possible to have AB1=0 but B2>0 (e.g., regulation
    ###       action that heavily impeded harvest ) and so these strata are simply dropped from the calculations...
    
    mutate( VAR.rate = ifelse( AB1==0, NA,
                             ( VAR_B2/(AB1^2) ) + ( ((B2^2)*VAR_AB1)/(AB1^4) ) ) )
    ###     ...which is the variance of a ratio for two independent random variables,
    ###       as approximated using a Taylor Series expansion ( S74-DW-10, Equation 2 )...
  
  
  ###   ...where I save a copy of this table as an output of this function...
  discard.rate.SRHS = discard.rate %>%
      rename(  var.ab1  = VAR_AB1,
               var.b2   = VAR_B2,
              disc.rate = SRHS.rate,
               var.rate =  VAR.rate )
      # pivot_wider( names_from = 'SID',
      #              values_from = c('AB1','var.ab1','B2','var.b2','disc.rate','var.rate'),
      #              names_glue = "{SID}_{.value}" )
  
  discard.rate = discard.rate %>%
    filter( YEAR %in% years.ratio ) %>%
    select( -c('AB1','B2','VAR_AB1','VAR_B2') )
  
  
  
  ### Calculate Mean SRHS Discard Rate ###
  ### ____________________________________
  ###
  ###     ...where meanratio = mean( B2.SRHS / AB1.SRHS )
  
  meanratio = discard.rate %>%
    group_by( across( any_of( var.vec[ var.vec %notin% c('YEAR','NEW_MODEN') ] ) ) ) %>%
    summarise( avg.rate = mean( SRHS.rate, na.rm=TRUE ),
               var.rate =  sum(  VAR.rate, na.rm=TRUE ) / ( length( VAR.rate[ !is.na(VAR.rate) ] )^2 ) )
  rm( discard.rate )
  
  
  ###   ...and saving a copy of this table as an output of this function...
  meanratio.table = meanratio
  
  
  
  ### Calculate the SRHS Discard Proxies ###
  ### ______________________________________
  ###
  ###     ...which, for this method, is done by multiplying the (mode-specific) MRIP discard rate by the
  ###       annual SRHS landings estimates as a proxy for SRHS discard estimates...
  
  srhsB2.proxy = srhs.table %>% left_join( meanratio, by = var.vec[ var.vec %notin% c('YEAR','NEW_MODEN') ] )
  rm(meanratio)
  
  
  srhsB2.proxy = srhsB2.proxy %>%
    # mutate( SRHS.rate = ifelse( is.na( SRHS.rate ), 0, SRHS.rate ) ) %>%
    # ###   ...where I considered adjusting any 'SRHS.rate' = <NA> values to equal zero, but decided against it
    # ###     so as that the resultant proxy estimates would also be equal to <NA> , which serves to highlight that
    # ###     the data is inadequate to support estimation at the current level of (spatial) stratification
    # ###     (e.g., no discard rate being estimated from the MRIP survey -- AB1 = B2 = 0 )...
    
    mutate( proxy     = AB1 * avg.rate ) %>%
    mutate( proxy.var = ( (AB1^2)*var.rate ) + ( (avg.rate^2)*VAR_AB1 ) - ( var.rate*VAR_AB1 ) )
    ###     ...which is the variance of the product of two independent random variables,
    ###       as is being approximated using Goodman's Formula ( S74-DW-10, Equation 5 )...
  
  
  
  ### Summary Plot ###
  ### ________________
  ###
  ###   Before substituting 'proxy' discard estimates into 'B2' for the selected years (i.e., in 'years.ratio' ),
  ###   I first construct a summary figure to compare trends in 'actual' SRHS discard estimates vs. those imputed...
  
  dummy.table = srhsB2.proxy %>%
    select( -c('VAR_AB1','VAR_B2','avg.rate','var.rate','proxy.var') ) %>%
    
    mutate( SID = factor( SID, levels = levels(srhs.table$SID) ) ) %>%
    ###     ...wherein I format 'SID' geographically (i.e., as defined in 'genrec.table' ) to ensure
    ###       the proper min/max values are applied when adding shaded rectangles to these plots below
    ###       (i.e., to identify what years are considered when calculating/applying discard rates )...
    
    mutate( B2 = ifelse( YEAR < 2004 & B2 == 0, NA, B2 ) ) %>%
    ###   ...where SRHS didn't start collecting discard info until 2004 and so I don't want these years graphed
    ###     on the summary figure (i.e., set B2 = <NA> )...
    pivot_longer( cols = c('AB1','B2','proxy'), names_to = 'Metric' ) %>%
    mutate( value = value / 1000,
            type = ifelse( Metric == 'AB1', 'LANDINGS', 'DISCARDS' ) ) %>%
    mutate(  type = factor( type, levels = c('LANDINGS','DISCARDS') ) )
  
  n.years = length(unique(dummy.table$YEAR)) / 2
  
  
  proxy.plot = ggplot( data=dummy.table ) +
    geom_line( aes( x=YEAR, y=value, colour=Metric ), linewidth=1.2 ) +
    
    facet_grid( SID ~ type , scales = 'free' ) +
    labs( title = paste0( "Mean-SRHS Approach: Headboat Discard Proxies by Area" ),
          subtitle = paste0( "Discard Proxies (",min(years.impute),":",max(years.impute),", Green)",
                             " from SRHS Discard Rates (",min(years.ratio),":",max(years.ratio),", Blue)" ),
          x="Year", y="Thousands of Fish" ) +
    
    scale_x_continuous( breaks = scales::pretty_breaks( n = n.years ) ) +
    scale_fill_manual( values = c("red","yellow","blue") ) +
    expand_limits(y = 0) +
    # scale_y_continuous(breaks = scales::pretty_breaks(n = 10), labels = scales::comma) +
    
    theme_bw() +
    theme( text = element_text(size = 11),
           axis.text.x = element_text(angle = 90, vjust=0.5),
           # plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
           legend.position = "bottom",
           # panel.background = element_blank(),
           panel.grid.major = element_line(colour = "grey", linewidth = 0.5),
           panel.grid.minor = element_line(colour = "grey", linewidth = 0.2),
           panel.border = element_rect(colour = "black", fill = NA) )
  
  rm( n.years )
  
  
  ###   To these plots, I then add shaded rectangles ( to the 'DISCARDS' facet ) to highlight years wherein:
  ###     (1) proxies of SRHS discards (i.e., those imputed by the selected method ) are to be used within the assessment,
  ###           which are highlighted in GREEN
  ###     (2) catch rates in recent years are used to inform (historic) SRHS discard proxies, which are highlighted in BLUE
  ###           and only applicable to a couple proxy methods (i.e., estimates of super-ratios and mean(SRHS) catch rates )
  ###   Note that these rectangles are only drawn on the 'DISCARDS' facet of these plots.
  
  loc.disc.col  = which( levels(dummy.table$type) == 'DISCARDS' )
  
  y.discards = data.frame( MIN = 0, MAX = 0 )
  for( i in 1:length( levels(dummy.table$SID) ) ) {
    y.discards[i,] = layer_scales( proxy.plot, i=i, j=loc.disc.col )$y$range$range
  }
  rm( i, loc.disc.col )
  
  y.discards$type = factor('DISCARDS')
  y.discards$SID  = levels(dummy.table$SID)
  
  proxy.plot = proxy.plot +
    geom_rect( data = y.discards,
               aes( xmin = min(years.impute), xmax = max(years.impute), ymin = MIN, ymax = MAX ),
               fill = 'green', alpha = 0.1 )
  
  proxy.plot = proxy.plot +
    geom_rect( data = y.discards,
               aes( xmin = min(years.ratio), xmax = max(years.ratio), ymin = MIN, ymax = MAX ),
               fill = 'blue', alpha = 0.1 )
  
  rm( y.discards )
  
  
  ###   ...where the final step in this function is to produce the 'final' catch tables, with a single (B2) discard timeseries
  ###       being populated from either the discard proxies or actual SRHS discard estimates. However, before doing this,
  ###       I save a copy of this table ( as an output of the function ) to allow for comparisons of proxy discard estimates
  ###       across methods, in particular for those years where SRHS discard estimates exist (e.g., 2004+ ).
  ###   These comparisons are made in my srhsB2.comparison.pdf() function, which call these 'proxy.table' objects...
  proxy.table = dummy.table %>% mutate( value = value * 1000 )
  
  rm( dummy.table )
  
  
  
  ### Final SRHS discard proxy table ###
  ### __________________________________
  ###
  ###     ...for which the last step is just to substitute the discard proxy estimates into the SRHS catch table
  ###       for those years (1) when SRHS didn't estimate discards and (2) discards are assumed non-negligible...
  
  srhsB2.proxy = srhsB2.proxy %>%
    
    mutate( B2     = ifelse( YEAR %in% years.impute, as.integer(proxy), B2 ),
            VAR_B2 = ifelse( YEAR %in% years.impute,         proxy.var, VAR_B2 ) ) %>%
    
    select( -c('avg.rate','var.rate','proxy','proxy.var') )
  
  
  
  output.list = list( srhsB2.proxy, proxy.table, proxy.plot, discard.rate.SRHS, meanratio.table )
  names( output.list ) = c( 'Catch.Table', 'Proxy.Table', 'Plot', 'SRHS_disc.rate', 'SRHS_mean.rate' )
  
  return( output.list )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------

srhsB2.bpSRHS = function( srhs.table, var.vec, bp.ratios, years.impute ) {
  ###     ...where 'srhs.table' is the table of SRHS catch estimates (AB1 & B2) by YEAR, MODE, and SID (if applicable),
  ###         'var.vec' identifies those variables/strata over which proxies are to be calculated,
  ###         'bp.ratios' is a table of ratios calculated from the SRHS bioprofile (size) data that are
  ###                   from size ranges of landed fish meant to reflect the amount of (regulatory) discards,
  ###                   relative to landings, expected from the implementation of a size limit, and
  ###         'years.impute' identifies those years for which the discard proxy will be used as an estimate of SRHS B2,
  
  
  srhs.table = srhs.table %>% ungroup()
  
  var.vec = colnames(bp.ratios)[ colnames(bp.ratios) %in% colnames(srhs.table) ]
  
  
  ### SRHS discard rates (from SRHS bioprofile data) ###
  ### __________________________________________________
  ###
  ###   ...for which, unlike other proxy methods, the discard rates being applied in the 'SHRS-BIOPROFILE'
  ###     method are calculated in the main body of the script and then imported as the 'bp.ratios' table.
  ###     Therefore, the calculation of these discard rates can be skipped in this function...
  
  
  
  ### Calculate the SRHS Discard Proxies ###
  ### ______________________________________
  ###
  ###     ...which, for this method, is done by multiplying the (mode-specific) MRIP discard rate by the
  ###       annual SRHS landings estimates as a proxy for SRHS discard estimates...
  
  srhsB2.proxy = srhs.table %>% left_join( bp.ratios, by = var.vec )
  
  discard.rate.SRHS = srhsB2.proxy
  
  srhsB2.proxy = srhsB2.proxy %>%
    # mutate( avg.rate = ifelse( is.na( avg.rate ), 0, avg.rate ) ) %>%
    # ###   ...where I considered adjusting any 'SRHS.rate' = <NA> values to equal zero, but decided against it
    # ###     so as that the resultant proxy estimates would also be equal to <NA> , which serves to highlight that
    # ###     the data is inadequate to support estimation (e.g., no sampled fish in the SRHS bioprofile data )...
    
    mutate( proxy     = AB1 * avg.rate ) %>%
    mutate( proxy.var = ( (AB1^2)*var.rate ) + ( (avg.rate^2)*VAR_AB1 ) - ( var.rate*VAR_AB1 ) )
    ###     ...which is the variance of the product of two independent random variables,
    ###       as is being approximated using Goodman's Formula ( S74-DW-10, Equation 5 )...
  
  
  
  ### Summary Plot ###
  ### ________________
  ###
  ###   Before substituting 'proxy' discard estimates into 'B2' for the selected years (i.e., in 'years.ratio' ),
  ###   I first construct a summary figure to compare trends in 'actual' SRHS discard estimates vs. those imputed...
  
  dummy.table = srhsB2.proxy %>%
    select( -c('VAR_AB1','VAR_B2','avg.rate','var.rate','proxy.var') ) %>%
    
    mutate( SID = factor( SID, levels = levels(srhs.table$SID) ) ) %>%
    ###     ...wherein I format 'SID' geographically (i.e., as defined in 'genrec.table' ) to ensure
    ###       the proper min/max values are applied when adding shaded rectangles to these plots below
    ###       (i.e., to identify what years are considered when calculating/applying discard rates )...
    
    mutate( B2 = ifelse( YEAR < 2004 & B2 == 0, NA, B2 ) ) %>%
    ###   ...where SRHS didn't start collecting discard info until 2004 and so I don't want these years graphed
    ###     on the summary figure (i.e., set B2 = <NA> )...
    pivot_longer( cols = c('AB1','B2','proxy'), names_to = 'Metric' ) %>%
    mutate( value = value / 1000,
            type = ifelse( Metric == 'AB1', 'LANDINGS', 'DISCARDS' ) ) %>%
    mutate(  type = factor( type, levels = c('LANDINGS','DISCARDS') ) )
  
  n.years = length(unique(dummy.table$YEAR)) / 2
  
  
  proxy.plot = ggplot( data=dummy.table ) +
    geom_line( aes( x=YEAR, y=value, colour=Metric ), linewidth=1.2 ) +
    
    facet_grid( SID ~ type , scales = 'free' ) +
    labs( title = paste0( "SRHS-BIO Approach: Headboat Discard Proxies by Area" ),
          subtitle = paste0( "Discard Proxies (",min(years.impute),":",max(years.impute),", Green)",
                             " from corresponding Discard Rates from SRHS Bioprofile Data" ),
          x="Year", y="Thousands of Fish" ) +
    
    scale_x_continuous( breaks = scales::pretty_breaks( n = n.years ) ) +
    scale_fill_manual( values = c("red","yellow","blue") ) +
    expand_limits(y = 0) +
    # scale_y_continuous(breaks = scales::pretty_breaks(n = 10), labels = scales::comma) +
    
    theme_bw() +
    theme( text = element_text(size = 11),
           axis.text.x = element_text(angle = 90, vjust=0.5),
           # plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
           legend.position = "bottom",
           # panel.background = element_blank(),
           panel.grid.major = element_line(colour = "grey", linewidth = 0.5),
           panel.grid.minor = element_line(colour = "grey", linewidth = 0.2),
           panel.border = element_rect(colour = "black", fill = NA) )
  
  rm( n.years )
  
  
  ###   To these plots, I then add shaded rectangles ( to the 'DISCARDS' facet ) to highlight years wherein:
  ###     (1) proxies of SRHS discards (i.e., those imputed by the selected method ) are to be used within the assessment,
  ###           which are highlighted in GREEN
  ###   Note that these rectangles are only drawn on the 'DISCARDS' facet of these plots.
  ###
  ###   Additionally, to these plots, I also usually add shaded (blue) rectangles for:
  ###     (2) catch rates in recent years are used to inform (historic) SRHS discard proxies, which are highlighted in BLUE
  ###           and only applicable to a couple proxy methods (i.e., estimates of super-ratios and mean(SRHS) catch rates )
  ###   ...but these aren't added to the "SRHS-BIO" plots as such discard rates are calculated separately from bioprofile
  ###   data ( the years over which are not readily identifiable from the 'bp.ratios' table ) and largely from the same years
  ###   as which proxy discards are being calculated ( which are already being shaded green ). Therefore, this facet is
  ###   commented out in the code below...
  
  loc.disc.col  = which( levels(dummy.table$type) == 'DISCARDS' )
  
  y.discards = data.frame( MIN = 0, MAX = 0 )
  for( i in 1:length( levels(dummy.table$SID) ) ) {
    y.discards[i,] = layer_scales( proxy.plot, i=i, j=loc.disc.col )$y$range$range
  }
  rm( i, loc.disc.col )
  
  y.discards$type = factor('DISCARDS')
  y.discards$SID  = levels(dummy.table$SID)
  
  proxy.plot = proxy.plot +
    geom_rect( data = y.discards,
               aes( xmin = min(years.impute), xmax = max(years.impute), ymin = MIN, ymax = MAX ),
               fill = 'green', alpha = 0.1 )
  
  # proxy.plot = proxy.plot +
  #   geom_rect( data = y.discards,
  #              aes( xmin = min(years.ratio), xmax = max(years.ratio), ymin = MIN, ymax = MAX ),
  #              fill = 'blue', alpha = 0.1 )
  
  rm( y.discards )
  
  
  ###   ...where the final step in this function is to produce the 'final' catch tables, with a single (B2) discard timeseries
  ###       being populated from either the discard proxies or actual SRHS discard estimates. However, before doing this,
  ###       I save a copy of this table ( as an output of the function ) to allow for comparisons of proxy discard estimates
  ###       across methods, in particular for those years where SRHS discard estimates exist (e.g., 2004+ ).
  ###   These comparisons are made in my srhsB2.comparison.pdf() function, which call these 'proxy.table' objects...
  proxy.table = dummy.table %>% mutate( value = value * 1000 )
  
  rm( dummy.table )
  
  
  
  ### Final SRHS discard proxy table ###
  ### __________________________________
  ###
  ###     ...for which the last step is just to substitute the discard proxy estimates into the SRHS catch table
  ###       for those years (1) when SRHS didn't estimate discards and (2) discards are assumed non-negligible...
  
  srhsB2.proxy = srhsB2.proxy %>%
    
    mutate( B2     = ifelse( YEAR %in% years.impute, as.integer(proxy), B2 ),
            VAR_B2 = ifelse( YEAR %in% years.impute,         proxy.var, VAR_B2 ) ) %>%
    
    select( -c('avg.rate','var.rate','proxy','proxy.var') )
  
  
  
  output.list = list( srhsB2.proxy, proxy.table, proxy.plot, discard.rate.SRHS )
  names( output.list ) = c( 'Catch.Table', 'Proxy.Table', 'Plot', 'SRHS_disc.rate' )
  
  return( output.list )
  
}

### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------

