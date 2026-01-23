

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
###   ---------------------
###   TPWD 1981 - 1983wave2
###   ---------------------
###
###
###     The (standard) TPWD survey didn't begin until the start of the 1983 high-use season (May15) and so
###     there is an inherent data gap in this survey. In accordance with best practices ( SEDAR PW7 -- Rec Issue #3 ),
###     and as applied in past SEDARs (e.g., S28 spanish mackerel & cobia, S31 red snapper, S33 gag & greater amberjack),
###     average TPWD catch estimates from 1983-1985 (by mode and wave) can be used to estimate:
###
###         -- Texas catch in waves 1-2 for years 1981-1983 (imputed from TPWD estimates in 1984-1985 -- two-year avgs )
###         -- Texas catch in waves 3-6 for years 1981-1982 (imputed from TPWD estimates in 1983-1985 -- three-year avgs )
###
###
###    *** summary.TPWD.1981.1983( )
###           ...which evaluates the relative landings coming from TX (i.e., is catch negligible? )
###             for those years over which average catches would be calculated and applied to impute those
###             TPWD catch estimates that are missing (i.e., 1983:1985 used to impute 1981:1983 catch )...
###                 -- Does AB1=0 look reasonable for TX 1981-1983 , relative to adjacent years ?
###                 -- Has this species been consistently landed (in TX) over time? Are landings a sporadic phenomenon?
###
###
###    *** impute.TPWD.1981.1983( )
###           ...which applies the chosen method ( from the summary.TPWD.1981.1983() function )
###                 to impute TPWD 1981-1983w2 catch...
###
###    *** impute.TPWD.1981.1983.effort( )
###           ...which applies the chosen method ( from the summary.TPWD.1981.1983() function )
###                 to impute TPWD 1981-1983w2 effort...
###
###
### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------


summary.TPWD.1981.1983 = function( genrec.table ) {
  
  
  summary.table <- list()
  
  
  ### Table - Total Landings by State ###
  ### -----------------------------------
  ###     ...to evaluate the relative catch coming from TX from those years over which average catches
  ###           would be calculated (1983:1985). Are landings negligible compared to other Gulf states?
  
  summary.table$StateCatch = genrec.table %>%
    filter( YEAR %in% 1983:1985 ) %>%
    group_by( NEW_STA ) %>%
    summarise( AB1 = sum( AB1, na.rm=TRUE ) ) %>%
    mutate( AB1.p = AB1 / sum(AB1) )
  
  
  ### Table - Landings Timeseries in TX ###
  ### -------------------------------------
  ###     ...to evaluate whether TPWD landings should be imputed:
  ###           -- Does AB1=0 look reasonable for TX 1981-1983 , relative to other years?
  ###           -- Has this species been consistently landed in TX (over time), or are landings a sporadic phenomenon?
  
  summary.table$TXCatch = genrec.table %>%
    filter( NEW_STA == "TX" ) %>%
    group_by( YEAR ) %>%
    summarise( AB1 = sum( AB1, na.rm=TRUE ) )
  
  
  ### Figure - Landings Timeseries by (Gulf) State ###
  ### ------------------------------------------------
  
  dummy = genrec.table %>%
    filter( YEAR %in% 1981:1999 & NEW_MODEN %in% c('Priv','Cbt') ) %>%
    filter( SUB_REG == 7 ) %>%
    group_by( NEW_STA, YEAR, NEW_MODEN ) %>%
    summarise( AB1 = sum( AB1, na.rm=TRUE ) )
  
  summary.table$State.fig = ggplot( dummy, aes( x=YEAR , y=AB1 , colour=NEW_STA ) ) +
    geom_point() + geom_line() +
    facet_grid( NEW_MODEN ~ . , scales = "free" ) +
    scale_x_continuous( breaks=seq(1981,1999,2) ) +
    
    geom_line( data = dummy %>% filter( NEW_STA == "TX" ), colour = 'black', size=1, linetype = 2 ) +
    labs( subtitle = 'TX = dashed black line' ) +
    ###   ...where 'TX' is summarized with a dashed line (linetype=2)
    
    geom_vline( xintercept = 1983, linetype = 'solid' , color = 'gray30' )
    ###   ...and where I add a vertical line at 1983 to mark the beginning of the TPWD survey...
  
  rm( dummy )
  
  
  return( summary.table )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------


impute.TPWD.1981.1983 = function( genrec.table, method = c('None','avg_83_85') ) {
  
  
  if( method == 'avg_83_85' ) {
    
    
    ### Estimated (1983:1985) Avg Catches ###
    ### _____________________________________
    ###
    avg.cat = genrec.table %>%
      filter( DS == "TPWD" & YEAR %in% 1983:1985 ) %>%
      ###    ...where 'genrec.table' does not (currently) have any estimates for 1983 waves 1-2 (which is why we're
      ###       imputing them in this script). Therefore, we don't need to apply a WAVE filter in the above statement...
      group_by( WAVE, NEW_MODE,NEW_MODEN ) %>%
      summarise( AB1 = sum( AB1, na.rm=TRUE ),
                 wwt = sum( lbsest_SECwwt, na.rm=TRUE ),
                 gwt = sum( lbsest_SECgwt, na.rm=TRUE ) ) %>%
      mutate( AB1 = ifelse( WAVE %in% 1:2, AB1/2, AB1/3 ),
              wwt = ifelse( WAVE %in% 1:2, wwt/2, wwt/3 ),
              gwt = ifelse( WAVE %in% 1:2, gwt/2, gwt/3 ) ) %>%
      ###     ...where 'avg.cat' is calculated from TPWD data for 1983-1985 waves 3-6 and 1984-1985 waves 1-6,
      ###       wherein we have two years of data for waves 1-2 ( divide by 2 ) and three for waves 3-6 (divide by 3 )...
      ungroup()
    
    
    ###       ...I then duplicate the above (by-wave) table appropriately, such that there is a separate row
    ###             for each year that is to be imputed:
    ###                   -- need 3 rows for the 3 years needing imputed estimates for waves 1-2 (1981-1983)
    ###                   -- need 2 rows for the 2 years needing imputed estimates for waves 3-6 (1981-1982)
    ###             I also update the variables for my imputed TPWD landings (to represent previous years/waves)...
    
    dummy.12 = avg.cat %>%
      arrange( WAVE, NEW_MODE ) %>%
      filter(  WAVE %in% 1:2 ) %>%
      uncount( 3 )
    dummy.12 = dummy.12 %>%
      mutate( YEAR = rep( 1981:1983, times = (dim(dummy.12)[1])/3 ) )
    
    dummy.36 = avg.cat %>%
      arrange( WAVE, NEW_MODE ) %>%
      filter(  WAVE %in% 3:6 ) %>%
      uncount( 2 )
    dummy.36 = dummy.36 %>%
      mutate( YEAR = rep( 1981:1982, times = (dim(dummy.36)[1])/2 ) )
    
    avg.cat = rbind( dummy.12, dummy.36 )
    rm( dummy.12, dummy.36 )
    
    
    avg.cat = avg.cat %>%
      mutate( DS = "TPWD",
              NEW_ST = 1,
              NEW_STA = "TX",
              SUB_REG = 7,
              
              NEW_COM = ifelse( length(new.com) == 1, new.com, NA ),
              NEW_SCI = ifelse( length(new.sci) == 1, new.sci, NA ),
              SP_CODE = ifelse( length(nodc.code) == 1, nodc.code, NA ),
              ITIS_CODE = ifelse( length(itis.code) == 1, itis.code, NA ),
              SA_LABEL  = ifelse( length(new.com) == 1, spp.info$SA_LABEL[ spp.info$NEW_COM == new.com ], NA ),
              GOM_LABEL = ifelse( length(new.com) == 1, spp.info$GOM_LABEL[ spp.info$NEW_COM == new.com ], NA ),
              
              LBSEST_SECSOURCE = "TPWDsub" ) %>%
      rename( lbsest_SECwwt = wwt,
              lbsest_SECgwt = gwt ) %>%
      mutate_all( list( ~ as.character(.) ) ) %>%
      mutate_at( vars( YEAR,WAVE, SUB_REG, NEW_ST, NEW_MODE,
                       AB1,lbsest_SECwwt,lbsest_SECgwt ), list( ~ as.numeric(.) ) )
    
    
    # ### Comparing imputed estimates (1981-May1983) with those provided by TPWD (May1983-1990) ###
    #
    # dummy1 = expand.grid( YEAR=1981:1983, WAVE=1:6, NEW_MODEN=c("Priv","Cbt") )
    # dummy1 = dummy1[ !( dummy1$YEAR == 1983 & dummy1$WAVE %in% 3:6 ), ]
    # dummy1 = dummy1 %>%
    #   left_join( avg.cat, by = c("WAVE","NEW_MODEN") ) %>%
    #   arrange( YEAR, WAVE, NEW_MODE )
    #
    # dummy1 = avg.cat %>%
    #   group_by( YEAR, WAVE, NEW_MODE,NEW_MODEN ) %>%
    #   summarise(           AB1 = sum( AB1, na.rm=TRUE ),
    #              lbsest_SECwwt = sum( lbsest_SECwwt, na.rm=TRUE ),
    #              lbsest_SECgwt = sum( lbsest_SECgwt, na.rm=TRUE ) )
    #
    # dummy2 = catch.table %>%
    #   filter( DS == "TPWD" & YEAR %in% 1983:1990 ) %>%
    #   group_by( YEAR, WAVE, NEW_MODE,NEW_MODEN ) %>%
    #   summarise(           AB1 = sum( AB1, na.rm=TRUE ),
    #              lbsest_SECwwt = sum( lbsest_SECwwt, na.rm=TRUE ),
    #              lbsest_SECgwt = sum( lbsest_SECgwt, na.rm=TRUE ) )
    #
    # dummy = dummy1 %>%
    #   bind_rows( dummy2 )
    # rm( dummy1, dummy2 )
    #
    # dummy = dummy %>%
    #   group_by( YEAR, NEW_MODE,NEW_MODEN ) %>%
    #   summarise(           AB1 = mean( AB1, na.rm=TRUE ),
    #              lbsest_SECwwt = mean( lbsest_SECwwt, na.rm=TRUE ),
    #              lbsest_SECgwt = mean( lbsest_SECgwt, na.rm=TRUE ) )
    #
    # dummy.plot = ggplot( dummy, aes( x=YEAR , y=AB1 , colour=NEW_MODEN ) ) +
    #   geom_point() + geom_line() +
    #   scale_x_continuous( breaks=seq(1981,1990,2) )
    # dummy.plot
    # rm( dummy, dummy.plot )
    
    
    genrec.table = genrec.table %>% bind_rows( avg.cat )
    rm( avg.cat )
    
  }
  
  return ( genrec.table )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------


impute.TPWD.1981.1983.effort = function( genrec.table, method = c('None','avg_83_85') ) {
  
  
  if( method == 'avg_83_85' ) {
    
    
    ### Estimated (1983:1985) Avg Effort ###
    ### ____________________________________
    
    avg.eff = genrec.table %>%
      filter( DS == "TPWD" & CYEAR %in% 1983:1985 ) %>%
      ###    ...where 'genrec.table' does not (currently) have any estimates for 1983 waves 1-2 (which is why we're
      ###       imputing them in this script). Therefore, we don't need to apply a WAVE filter in the above statement...
      group_by( WAVE, NEW_MODE,NEW_MODEN ) %>%
      
      ###   ...where NTRP is a TPWD effort estimate as the total number of angler-trips,
      ###          ESTHRS is a TPWD effort estimate as the total number of angler-hours ( in trip hours, not fishing hours ),
      ###        and NPAR is the total number of fishing parties...
      summarise(   NTRP = sum(   NTRP, na.rm=TRUE ),
                   NPAR = sum(   NPAR, na.rm=TRUE ),
                 ESTHRS = sum( ESTHRS, na.rm=TRUE ) ) %>%
      mutate(   NTRP = ifelse( WAVE %in% 1:2,   NTRP/2,   NTRP/3 ),
                NPAR = ifelse( WAVE %in% 1:2,   NPAR/2,   NPAR/3 ),
              ESTHRS = ifelse( WAVE %in% 1:2, ESTHRS/2, ESTHRS/3 ) ) %>%
      ###     ...where 'avg.cat' is calculated from TPWD data for 1983-1985 waves 3-6 and 1984-1985 waves 1-6,
      ###       wherein we have two years of data for waves 1-2 ( divide by 2 ) and three for waves 3-6 (divide by 3 )...
      
      ###   ...where PARSIZE is the average party size
      ###        and TRIPLEN is the average trip length
      mutate( PARSIZE =   NTRP / NPAR,
              TRIPLEN = ESTHRS / NTRP ) %>%
      
      ungroup()
    
    
    ###       ...I then duplicate the above (by-wave) table appropriately, such that there is a separate row
    ###             for each year that is to be imputed:
    ###                   -- need 3 rows for the 3 years needing imputed estimates for waves 1-2 (1981-1983)
    ###                   -- need 2 rows for the 2 years needing imputed estimates for waves 3-6 (1981-1982)
    ###             I also update the variables for my imputed TPWD landings (to represent previous years/waves)...
    
    dummy.12 = avg.eff %>%
      arrange( WAVE, NEW_MODE ) %>%
      filter(  WAVE %in% 1:2 ) %>%
      uncount( 3 )
    dummy.12 = dummy.12 %>%
      mutate( CYEAR = rep( 1981:1983, times = (dim(dummy.12)[1])/3 ) )
    
    dummy.36 = avg.eff %>%
      arrange( WAVE, NEW_MODE ) %>%
      filter(  WAVE %in% 3:6 ) %>%
      uncount( 2 )
    dummy.36 = dummy.36 %>%
      mutate( CYEAR = rep( 1981:1982, times = (dim(dummy.36)[1])/2 ) )
    
    avg.eff = rbind( dummy.12, dummy.36 )
    rm( dummy.12, dummy.36 )
    
    
    avg.eff = avg.eff %>%
      mutate( DS = "TPWD",
              NEW_ST = 1,
              NEW_STA = "TX",
              SUB_REG = 7,
              IMP_NTRP = method )
      # mutate_all( list( ~ as.character(.) ) ) %>%
      # mutate_at( vars( CYEAR,SYEAR,WAVE, SUB_REG, NEW_ST, ACTIVITY,NEW_MODE, AREA,NEW_AREA,
      #                  PARSIZE,TRIPLEN,TTLANGLR,ESTHRS,ESTHRS_SE,NTRP,NPAR ), list( ~ as.numeric(.) ) )
    
    
    genrec.table = genrec.table %>% bind_rows( avg.eff )
    rm( avg.eff )
    
  }
  
  return ( genrec.table )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------



