

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
###   LACR discards
###   -------------
###
###
###     ...wherein the LACreel survey replaced MRIP sampling (in LA) starting in 2014. Discard information
###       wasn't collected as part of this survey until 2016, and only for a subset of species
###       ( ~20 spp, most of which are state managed ). Therefore, there is an inherent data gap in this survey,
###       with LA discard estimates either:
###
###           (1) not available for years 2014+ (i.e., discards for species never collected by LACreel ) or
###           (2) not available for years 2014-2015 (i.e., LACreel began collecting discard info in 2016 )
###
###     Unlike some of the other SEDAR-specific modifications we make to GenRec catch estimates, there are
###     no best practices for imputing LACreel discard estimates (assuming we decide to impute).
###     However, there is a best practices approach for imputing TPWD discards, which I modify here...
###     As a proxy for LACR discards, LACR AB1 estimates (by year and mode) are multiplied by mode-specific
###     B2:AB1 ratios (combined across years). The question just comes down to which years/states to use in
###     the calculation of these (B2:AB1) catch ratios.
###     
###     For species for which LACR collects discard information ( data gap = 2014-2015 )...
###         -- If the availability of LA catch data is sufficient, in that the (B2:AB1) catch ratios b/w zero & one
###               and relatively stable over time (years), these ratios will be calculated from LA catch estimates
###               in adjacent years (e.g., mode-specific ratios from MRIP 2011-2013 and/or LACR 2016-2018 ).
###         -- If LA catch data is insufficient (e.g., ratios not well estimated or variable ), Gulf-wide ratios
###               will be applied. However, in this case, ratios are calculated for the same years over which the
###               LACR discards are imputed (i.e., 2014 & 2015 Gulf-wide ratios to impute 2014 & 2015 discards ).
###
###     For species for which LACR does not collect discard information ( data gap = 2014+ )...
###         -- LA catch estimates will be used to calculate (B2:AB1) catch ratios when these ratios are
###               b/w zero & one and relatively stable over time (years). Because LA catch estimates are only
###               available from 1981-2013 in this case, ratios will be calculated from a chosen subset of
###               adjacent years of MRIP data (including 2013) over which B2:AB1 ratios appear stable
###               (e.g., 2011-2013, 2000-2013 ).
###            Note that when evaluating the stability of catch ratios, we look at both the 1981-2013 timeseries
###               of LA catch ( to determine if LA ratios are well estimated & stable ) and those from other Gulf states
###               ( to determine if the assumption of a static LA catch ratio is appropriate for years 2014+ ).
###         -- If LA catch data is insufficient (e.g., not well estimated or variable ) or ratios from other Gulf states
###               suggest catch ratios are not static over the 'imputation' years (2014+), Gulf-wide ratios
###               will be applied. Again, Gulf-wide ratios are calculated for the same years over which the
###               LACR discards are imputed (i.e., year-specific Gulf-wide ratios to impute year-specific LACR discards ).
###
###
###    *** summary.LACR.B2( )
###           ...which evaluates the amount and relative stability of catch estimates from the various Gulf states
###             and that of total (Gulf-wide) catch. These summaries inform (respectively) whether
###             LACR discards need to be imputed and how this imputation is to be done (i.e., what years & states ).
###
###    *** impute.LACR.B2( )
###           ...which applies the chosen method ( from the summary.LACR.B2() function) to impute LACR discards...
###
###
###   *********************************
###   -- OTHER APPROACHES CONSIDERED --
###
###   Note the approaches (above) that apply B2:AB1 catch ratios calculated from LA data are largely based on
###   static ratios calculated over a pre-determined set of years (e.g., average of MRIP 2011-2013 & LACR 2016-2018 ).
###   This choice of a static ratio is because there is no LA data (to calculate catch ratios) in the years for which
###   LACR discards need to be imputed. However, as an alternative, I did also consider applying a (linear) regression
###   to estimate year-specific ratios for those years requiring imputations of 2014-2015 discards
###   (e.g., fit mode-specific regressions to ratios from 2010-2013 MRIP data & 2016+ LACR data and
###          apply these regression to estimate separate LA catch ratios for 2014 & 2015 ).
###   While potentially valid, this approach could get us down a rabbit hole of what type of regression is
###   most appropriate (for LACR data or for specific species) and so wasn't used -- I ultimately decided to stay with
###   the simpler (static) average ratios. However, I did retain some of my (regression) code for future reference...
### #      priv.reg = lm( Priv_LA ~ YEAR , data=fracs )
### #      plot( fracs$Priv_LA ~ fracs$YEAR )
### #      lines( priv.reg$fitted.values ~ fracs$YEAR )
### #      cbt.reg = lm( Cbt_LA ~ YEAR , data=fracs )
### #      plot( fracs$Cbt_LA ~ fracs$YEAR )
### #      lines( cbt.reg$fitted.values ~ fracs$YEAR )
###
###   Similarly, I also considered applying catch ratios from neighboring states which ( in the case of LA )
###   would be B2:AB1 ratios calculated from either TX or MS catch estimates. Unfortunately, such an approach
###   was deemed inappropriate because (1) TPWD has not estimated TX discards at any point in the history of the survey
###   (in fact, we might need these imputed LA discards to compute LA B2:AB1 ratios, and impute TX discards ) and
###   (2) MRIP estimates for MS tend to be quite variable and likely too imprecise to be appropriate for most species.
###   Therefore, the choice was made to calculate ratios from either LA estimates or Gulf-wide estimates...
###
###
### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------


summary.LACR.B2 = function( genrec.table ) {
  
  
  summary.table <- list()
  
  
  ### Table - Total Catch by State ###
  ### --------------------------------
  ###     ...to evaluate the relative catch coming from LA (i.e., is catch negligible? )
  
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
  
  rm( AB1.total, B2.total )
  
  
  ### Table - Catch Timeseries in LA ###
  ### ----------------------------------
  ###     ...to evaluate whether LACreel discards should be imputed:
  ###           -- Does B2=0 look reasonable for LA 2014+ , relative to other years and/or other states?
  ###           -- Has this species been consistently discarded in LA (over time), or are discards a sporadic phenomenon?
  
  summary.table$LACatch = genrec.table %>%
    filter( NEW_STA == "LA" ) %>%
    mutate( NEW_MODEN = ifelse( NEW_MODEN == "Priv/Shore", "Priv", NEW_MODEN ) ) %>%
    filter( NEW_MODEN %in% c("Priv","Cbt") ) %>%
    
    group_by( YEAR ) %>%
    summarise( AB1 = sum( AB1, na.rm=TRUE ),
                B2 = sum(  B2, na.rm=TRUE ) )
  AB1.total = sum( summary.table$LACatch$AB1 )
   B2.total = sum( summary.table$LACatch$B2  )
  
  summary.table$LACatch = summary.table$LACatch %>%
     mutate( AB1.p = AB1 / sum(AB1),
              B2.p =  B2 / sum( B2) )
  
  rm( AB1.total, B2.total )
  
  
  ### Table & Figure - Stability of Ratios ###
  ### ----------------------------------------
  ###     ...to evaluate stability across time (years) and space (states/regionally)...
  
  ### State Ratios ###
  st.fracs = genrec.table %>%
    filter( NEW_STA %in% c("LA","MS","AL","FLW") & YEAR %in% 2000:term.year ) %>%
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
    filter( NEW_STA %in% c("LA","MS","AL","FLW") & YEAR %in% 2000:term.year ) %>%
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
    scale_x_continuous( breaks=seq(2000,term.year,2) ) +
    
    geom_line( data = fracs %>% filter( NEW_STA == "GULF" ), colour = 'black', linewidth=1, linetype = 1 ) +
    geom_line( data = fracs %>% filter( NEW_STA == "LA"   ), colour = 'black', linewidth=1, linetype = 2 ) +
    labs( subtitle = 'LA = dashed black line ; GULF = solid black line' ) +
    ###   ...where 'GULF' is summarized with a solid line (linetype=1) and 'LA' with a dashed line (linetype=2)
    
    geom_vline( xintercept = 2014, linetype = 'solid' , color = 'gray30' )
    ###   ...and where I add a vertical line at 2014 to mark the beginning of the LACR survey...
  
  rm( fracs )
  
  
  return( summary.table )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------


impute.LACR.B2 = function( genrec.table, method = c('None','la_ratio','gu_ratio'), ratio.years ) {
  
  ###   ...where 'ratio.years' identifies the years over which the (B2:AB1) ratios are to be calculated.
  ###          For example, if ratios are estimated from (MRIP) 2011-2013 catch estimates and
  ###          (LACR) 2016-2018 catch estimates (to estimate LACR discards for 2014-2015), then
  ###          'imp.years.LACR' = c(2011:2013,2016:2018).
  ###       Note that these years only need to be identified when method == 'la_ratio' because,
  ###          for the 'gu_ratio method, these years are identified (automatically) as those years
  ###          for which LACR discards = 0 ( in 'genrec.table' )...
  
  
  if( method != 'None' ) {
    
    
    fracs = genrec.table %>%
      mutate( NEW_MODEN = ifelse( NEW_MODEN == "Priv/Shore", "Priv", NEW_MODEN ) ) %>%
      filter( NEW_MODEN %in% c("Priv","Cbt") )
    
    
    ### ***********************************************************************************************
    if( method == 'la_ratio' ) {
      
      
      fracs = fracs %>%
        filter( YEAR %in% ratio.years ) %>%
        filter( NEW_STA == 'LA' )
      
      
      ### Estimated (B2:AB1) Catch Ratios ###
      ### ___________________________________
      ###
      ###     ...for which I considered two approaches:
      ###         (1) ratios estimated as a ratio of sums  --        sum(B2)/sum(AB1)
      ###         (2) ratios estimated as a mean of ratios -- mean( B2[year]/AB1[year] )
      ###   I ended up going with option #1 as this approach appears less susceptible to the variability
      ###   in catch ratios in any given year(s), but approach #2 may also be valid given we might
      ###   be combining estimates from different surveys (e.g., MRIP ratios 2011-2013 & LACR ratios 2016-2018 ),
      ###   wherein the magnitude of these catches may not be directly comparable. Something to revisit in the future (?)...
      
      fracs = fracs %>%
        
        ### RATIO = sum(B2) / sum(AB1)
        ###
        group_by( NEW_MODEN ) %>%
        summarise( AB1 = sum( AB1, na.rm=TRUE ),
                    B2 = sum(  B2, na.rm=TRUE ) ) %>%
        mutate( LA_B2ratio = ifelse( AB1==0, 0, B2/AB1 ) ) %>%
        select( NEW_MODEN, LA_B2ratio ) %>%
        mutate( NEW_MODEN = ifelse( NEW_MODEN == "Priv", "Priv/Shore", NEW_MODEN ) )
      
      # fracs = fracs %>%
      #   
      #   ### RATIO = mean( B2[year]/AB1[year] )
      #   ###
      #   group_by( YEAR, NEW_MODEN ) %>%
      #   summarise( AB1 = sum( AB1, na.rm=TRUE ),
      #               B2 = sum(  B2, na.rm=TRUE ) ) %>%
      #   mutate( LA_B2ratio = ifelse( AB1==0, 0, B2/AB1 ) ) %>%
      #   select( YEAR, NEW_MODEN, LA_B2ratio ) %>%
      #   ungroup() %>%
      #   group_by( NEW_MODEN ) %>%
      #   summarise( LA_B2ratio = mean( LA_B2ratio, na.rm=TRUE ) ) %>%
      #   mutate( NEW_MODEN = ifelse( NEW_MODEN == "Priv", "Priv/Shore", NEW_MODEN ) )
      
      
    ### ***********************************************************************************************
    } else if( method == 'gu_ratio' ) {
      
      
      ###   Note that 'ratio.years' is not applied in the (Gulf) script below. In particular, the GULF approach
      ###   calculates (and applies) year-specific B2:AB1 ratios -- as an example, a "2015 ratio" is calculated
      ###   to impute "2015 discards". This is in contrast to the LA approach, wherein the data doesn't exist
      ###   to calculate year-specific ratios (e.g., the whole reason we're calculating 2015 ratios is because
      ###   we're missing 2015 discards ). In this, we don't need to identify the years over which to calculate
      ###   our B2:AB1 ratios in the GULF approach as it's simply the same years over which we want to impute discards.
      ###   The YEAR filter below is therefore based on those years that are currently missing discard estimates...
      
      
      blah = genrec.table %>% filter( NEW_STA == 'LA' & YEAR >= 2016 )
      
      if( sum( blah$B2, na.rm=TRUE ) == 0 ) {
        fracs = fracs %>% filter( YEAR >= 2014 )     ### ...no LACR discard estimates available for this species
      } else {
        fracs = fracs %>% filter( YEAR %in% 2014:2015 )
      }
      rm(blah)
      
      fracs = fracs %>% filter( NEW_STA %in% c("MS","AL","FLW") )
      ###     ...excluding LA & TX which (currently) have zero discards...
      ###
      ###     Note that TPWD estimates are not included in this calculation because TPWD does not collect
      ###     discard info and I didn't want any imputed values included in the estimation of my B2:AB1 ratios
      ###     (e.g., B2[TPWD] = B2:AB1[ LA or GULF ] * AB1[TPWD] )...
      ###     Similarly, LACR estimates are also no longer included in this calculation. LACR estimates were included
      ###     in the previous approach, wherein I was calculating a single (mode-specific) Gulf-wide ratio across
      ###     all years of data. However, upon reflection, this approach was dropped in favor of appplying year-specific ratios,
      ###     which are possible when using all GULF data (e.g., estimate 2015 LACR discards from the 2015 catch rates
      ###     of other Gulf states ). LACR data is obviously not needed in such an approach as the whole reason we're calculating
      ###     B2:AB1 catch rates is because we're missing LA discards, and so B2=0 for those years requiring ratios.
      ###     The script below was therefore updated to calculate ratios from only MS-FLW...
      
      
      ### RATIO = sum(B2) / sum(AB1)
      ###
      fracs = fracs %>%
        group_by( YEAR, NEW_MODEN ) %>%
        summarise( AB1 = sum( AB1, na.rm=TRUE ),
                    B2 = sum(  B2, na.rm=TRUE ) ) %>%
        mutate( GU_B2ratio = ifelse( AB1==0, 0, B2/AB1 ) ) %>%
        select( YEAR, NEW_MODEN, GU_B2ratio ) %>%
        mutate( NEW_MODEN = ifelse( NEW_MODEN == "Priv", "Priv/Shore", NEW_MODEN ) )
      
    }
    
    
    
    ### Applying Ratios to LACR catch records ###
    ### _________________________________________
    ###
    ###     Instead of searching for this (assessed) species in a list of LACR 'target' species,
    ###     which may change from year-to-year, I choose to identify if there are any B2 estimates
    ###     from LA 2016+ (which is when LACR began collecting discard info). If sum(B2)>0 2016+,
    ###     then I assume this species is considered a 'target' spp and we only need to impute discards
    ###     for 2014-2015. Conversely, if sum(B2)=0 2016+, then imputations are needed for all years (2014+).
    ###     If we ever decide to change this approach, I retain the list of LACR 'target' spp I compiled
    ###     (as of Apr2023), but it is not currently being used in the script below...
    # lacr.target.spp <- c( 'striped mullet','southern flounder','spotted seatrout','black drum','red drum','sheepshead',
    #                       'gray triggerfish','greater amberjack','largemouth bass','dolphin',
    #                       'red snapper','gray snapper','lane snapper','vermilion snapper',
    #                       'sand seatrout','silver seatrout','atlantic croaker','wahoo',
    #                       'king mackerel','spanish mackerel','yellowfin tuna','blackfin tuna' )
    
    blah = genrec.table %>% filter( NEW_STA == 'LA' & YEAR >= 2016 )
    
    if( sum( blah$B2, na.rm=TRUE ) > 0 ) {
      ###   ...LACR provides some discard estimates for this species...
      
      dummy = genrec.table %>%
        filter( DS == "LA Creel" & YEAR %in% 2014:2015 )
      
      if( method == 'la_ratio' ) {
        dummy = dummy %>%
          left_join( fracs, by = "NEW_MODEN" ) %>%
          mutate( B2 = AB1 * LA_B2ratio )
      } else if( method == 'gu_ratio' ) {
        dummy = dummy %>%
          left_join( fracs, by = c("YEAR","NEW_MODEN") ) %>%
          mutate( B2 = AB1 * GU_B2ratio )
      }
      
      ###   ...which are then added back to our original 'genrec.table'...
      genrec.table = genrec.table %>%
        filter( !( DS == "LA Creel" & YEAR %in% 2014:2015 ) ) %>%
        bind_rows( dummy )
      
    } else {
      
      dummy = genrec.table %>%
        filter( DS == "LA Creel" & YEAR >= 2014 )
      
      if( method == 'la_ratio' ) {
        dummy = dummy %>%
          left_join( fracs, by = "NEW_MODEN" ) %>%
          mutate( B2 = AB1 * LA_B2ratio )
      } else if( method == 'gu_ratio' ) {
        dummy = dummy %>%
          left_join( fracs, by = c("YEAR","NEW_MODEN") ) %>%
          mutate( B2 = AB1 * GU_B2ratio )
      }
      
      genrec.table = genrec.table %>%
        filter( !( DS == "LA Creel" & YEAR >= 2014 ) ) %>%
        bind_rows( dummy )
      
    }
    
    rm( blah, dummy, fracs )
    
  }
  
  
  return ( genrec.table )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------



