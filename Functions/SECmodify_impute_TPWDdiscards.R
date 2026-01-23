

### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------
###
###     DESCRIPTION
###
###   There are a number of data gaps in the recreational (fishery) surveys in the southeast region.
###     To provide assessment analysts with a consistent timeseries covering the entire period of interest
###     (e.g., 1981+ ), we therefore apply a number of imputation approaches to fill these data gaps,
###     the details of which are described below...
###
###
### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------
###
###
###   -------------
###   TPWD discards
###   -------------
###
###
###   The TPWD survey has never collected information on released (alive) fish and so a proxy is needed to estimate
###   Texas discards. To fill-in this data gap (across all years), B2:AB1 catch ratios are calculated (by year & mode)
###   and applied to TPWD landings estimates. In accordance with SEDAR best practices ( SEDAR PW7 -- Rec Issue #10 ),
###   these ratios are calculated from either:
###
###       (1) catch estimates from just Louisiana or
###       (2) catch estimates across the entire Gulf of Mexico (i.e., Gulf-wide ratios)
###
###   The preferred approach is option #1, using just LA data, but the reliability of these estimates is a function of
###   the reliability (and availability) of LA data. In particular, LACreel replaced MRIP operations in 2014 (in LA),
###   and only collects discard info for a subset of species since (discard) data collection began (in 2016).
###
###      -- If the availability of LA catch data is sufficient, in that the (B2:AB1) catch ratios b/w zero & one
###             and relatively stable over time (years), year-specific ratios will be calculated from LA catch estimates
###      -- If LA catch data is insufficient (e.g., ratios not well estimated or variable ), Gulf-wide ratios are used
###
###   Note that, in both of these approaches, year-specific (B2:AB1) catch ratios are calculated and applied to
###   year-specific TPWD landings estimates (e.g., 2014 ratios applied to 2014 TPWD AB1 as a proxy for 2014 TPWD B2 ).
###
###
###    *** summary.TPWD.B2( )
###           ...which evaluates the amount and relative stability of catch estimates from the various Gulf states
###             and that of total (Gulf-wide) catch. These summaries inform (respectively) whether
###             TPWD discards need to be imputed and how this imputation is to be done (i.e., LA vs. GOM ratios ).
###
###    *** impute.TPWD.B2( )
###           ...which applies the chosen method ( from the summary.TPWD.B2() function) to impute TPWD discards...
###
###
### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------


summary.TPWD.B2 = function( genrec.table ) {
  
  
  summary.table <- list()
  
  
  ### Table & Figure - Total Catch by State ###
  ### -----------------------------------------
  ###     ...which I apply to evaluate the relative catch coming from TX (i.e., is catch negligible? )...
  
  summary.table$StateCatch = genrec.table %>%
    filter( SUB_REG == 7 ) %>%
    mutate( NEW_MODEN = ifelse( NEW_MODEN == "Priv/Shore", "Priv", NEW_MODEN ) ) %>%
    filter( NEW_MODEN %in% c("Priv","Cbt") ) %>%
    
    group_by( NEW_ST,NEW_STA ) %>%
    summarise( AB1 = sum( AB1, na.rm=TRUE ),
                B2 = sum(  B2, na.rm=TRUE ) )
  AB1.total = sum( summary.table$StateCatch$AB1 )
   B2.total = sum( summary.table$StateCatch$B2  )
  
  summary.table$StateCatch = summary.table$StateCatch %>%
    mutate( AB1.p = AB1 / AB1.total,
             B2.p =  B2 /  B2.total )
  
  
  dummy = genrec.table %>%
    filter( SUB_REG == 7 ) %>%
    mutate( NEW_MODEN = ifelse( NEW_MODEN == "Priv/Shore", "Priv", NEW_MODEN ) ) %>%
    filter( NEW_MODEN %in% c("Priv","Cbt") ) %>%
    
    group_by( YEAR, NEW_ST, NEW_STA, NEW_MODEN ) %>%
    mutate( NEW_STA = factor(NEW_STA) ) %>%
    summarise( AB1 = sum( AB1, na.rm=TRUE ),
                B2 = sum(  B2, na.rm=TRUE ) ) %>%
    pivot_longer( cols = c("AB1","B2"), names_to = "Metric", values_to = "Catch" )
  
  summary.table$State.fig = ggplot( dummy, aes( x=YEAR , y=Catch , colour=NEW_STA ) ) +
    geom_point() + geom_line() + facet_wrap( Metric ~ NEW_MODEN, scales="free" )

  rm( dummy )
  
  
  ### Table & Figure - Stability of Ratios ###
  ### ----------------------------------------
  ###     ...to evaluate stability across time (years) and space (states/regionally)...
  
  ### State Ratios ###
  st.fracs = genrec.table %>%
    filter( NEW_STA %in% c("LA","MS","AL","FLW") ) %>%
    mutate( NEW_MODEN = ifelse( NEW_MODEN == "Priv/Shore", "Priv", NEW_MODEN ) ) %>%
    filter( NEW_MODEN %in% c("Priv","Cbt") ) %>%
    
    group_by( YEAR, NEW_STA, NEW_MODEN ) %>%
    summarise( AB1 = sum( AB1, na.rm=TRUE ),
                B2 = sum(  B2, na.rm=TRUE ) ) %>%
    mutate( ratio = ifelse( AB1==0, 0, B2/AB1 ) ) %>%
    select( YEAR, NEW_STA, NEW_MODEN, ratio ) %>%
    arrange( NEW_MODEN, YEAR, NEW_STA )
    # pivot_wider( names_from = c(NEW_MODEN,NEW_STA), values_from = ratio )
  
  ### Gulf Ratios ###
  gu.fracs = genrec.table %>%
    filter( NEW_STA %in% c("LA","MS","AL","FLW") ) %>%
    mutate( NEW_MODEN = ifelse( NEW_MODEN == "Priv/Shore", "Priv", NEW_MODEN ) ) %>%
    filter( NEW_MODEN %in% c("Priv","Cbt") ) %>%
    
    group_by( YEAR, NEW_MODEN ) %>%
    summarise( AB1 = sum( AB1, na.rm=TRUE ),
                B2 = sum(  B2, na.rm=TRUE ) ) %>%
    mutate( ratio = ifelse( AB1==0, 0, B2/AB1 ) ) %>%
    select( YEAR, NEW_MODEN, ratio ) %>%
    mutate( NEW_STA = "GULF" )
    # pivot_wider( names_from = c(NEW_MODEN,NEW_STA), values_from = ratio )
  
  
  ### Comparisons ###
  
  fracs = st.fracs %>% bind_rows( gu.fracs )
  rm( st.fracs,gu.fracs )
  
  summary.table$fracs = fracs %>%
    arrange( YEAR, NEW_MODEN, NEW_STA ) %>%
    pivot_wider( names_from = c(NEW_MODEN,NEW_STA), values_from = ratio )
  
  summary.table$fracs.fig = ggplot( fracs, aes( x=YEAR , y=ratio , colour=NEW_STA ) ) +
    geom_point() + geom_line() +
    facet_grid( NEW_MODEN ~ . , scales = "free" ) +
    # scale_x_continuous( breaks=seq(1981,term.year,2) ) +
    
    geom_line( data = fracs %>% filter( NEW_STA == "GULF" ), colour = 'black', size=1, linetype = 1 ) +
    geom_line( data = fracs %>% filter( NEW_STA == "LA"   ), colour = 'black', size=1, linetype = 2 ) +
    labs( subtitle = 'LA = dashed black line ; GULF = solid black line' ) +
    ###   ...where 'GULF' is summarized with a solid line (linetype=1) and 'LA' with a dashed line (linetype=2)

  rm( fracs )
  
  
  return( summary.table )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------


impute.TPWD.B2 = function( genrec.table, method = c('None','la_ratio','gu_ratio') ) {
  
  
  fracs = genrec.table %>%
    mutate( NEW_MODEN = ifelse( NEW_MODEN == "Priv/Shore", "Priv", NEW_MODEN ) ) %>%
    filter( NEW_MODEN %in% c("Priv","Cbt") )
  
  if( method == 'la_ratio' ) {
    fracs = fracs %>% filter( NEW_STA == 'LA' )
    
  } else if( method == 'gu_ratio' ) {
    fracs = fracs %>% filter( NEW_STA %in% c('LA','MS','AL','FLW') )
  }
  
  fracs = fracs %>%
    group_by( YEAR, NEW_MODEN ) %>%
    summarise( AB1 = sum( AB1, na.rm=TRUE ),
                B2 = sum(  B2, na.rm=TRUE ) ) %>%
    mutate( TX_B2ratio = ifelse( AB1==0, 0, B2/AB1 ) ) %>%
    select( YEAR, NEW_MODEN, TX_B2ratio )
  
  
  
  ### Applying Ratios to TPWD catch records (all years) ###
  ### _____________________________________________________

  dummy = genrec.table %>%
    filter( DS == "TPWD" ) %>%
    left_join( fracs, by = c("YEAR","NEW_MODEN") ) %>%
    mutate( B2 = AB1 * TX_B2ratio )
  
  genrec.table = genrec.table %>%
    filter( DS != "TPWD" ) %>%
    bind_rows( dummy )
  
  rm( dummy, fracs )
  
  
  return( genrec.table )
  
}



impute.TPWD.B2v2 = function( genrec.table, method = c('None','la_ratio','gu_ratio') ) {
  
  
  fracs = genrec.table %>%
    mutate( NEW_MODEN = ifelse( NEW_MODEN == "Priv/Shore", "Priv", NEW_MODEN ) ) %>%
    filter( NEW_MODEN %in% c("Priv","Cbt") )
  
  if( method == 'la_ratio' ) {
    fracs = fracs %>% filter( NEW_STA == 'LA' )
    
  } else if( method == 'gu_ratio' ) {
    fracs = fracs %>% filter( NEW_STA %in% c('LA','MS','AL','FLW') )
  }
  
  fracs = fracs %>%
    group_by( YEAR ) %>%
    summarise( AB1 = sum( AB1, na.rm=TRUE ),
               B2 = sum(  B2, na.rm=TRUE ) ) %>%
    mutate( TX_B2ratio = ifelse( AB1==0, 0, B2/AB1 ) ) %>%
    select( YEAR, TX_B2ratio )
  
  
  
  ### Applying Ratios to TPWD catch records (all years) ###
  ### _____________________________________________________
  
  dummy = genrec.table %>%
    filter( DS == "TPWD" ) %>%
    left_join( fracs, by = c("YEAR") ) %>%
    mutate( B2 = AB1 * TX_B2ratio )
  
  genrec.table = genrec.table %>%
    filter( DS != "TPWD" ) %>%
    bind_rows( dummy )
  
  rm( dummy, fracs )
  
  
  return( genrec.table )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------



