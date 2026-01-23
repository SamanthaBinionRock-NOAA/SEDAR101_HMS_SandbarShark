

### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------
###
###     DESCRIPTION
###
###     Unlike size and catch data, the SEFSC does not produce an ACL file for recreational effort estimates.
###     Instead, effort estimates are pulled from our Oracle views, for which we have created unique views
###     for each survey. As each (survey-specific) view requires different modifications, and unique coding,
###     the script required to construct the initial effort table can get a bit long and so has been moved
###     to its own function (coded below)...
###
### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------

pull.GenRec.effort = function( survey = c('MRIP','TPWD','LACR'),
                               yr.filter, mode.filter,
                               reg.filter, sta.filter, fl.filter, nc.filter ) {
  ###     ...where 'survey' represents the particular dataset being imported (and formatted) and
  ###         all other objects are associated with the filters to be applied in this pull...
  
  
  
  ##########################
  ######     MRIP     ######
  ##########################
  
  if( survey == 'MRIP' ) {
    
    mrip.raw = dbGetQuery(con,
                          paste0("select *
                        from RDI.v_mrip_domain_cal_eff@secapxdv_dblk.sfsc.noaa.gov" ))
    
    ### TEMPORAL ###
    effort.table <- mrip.raw[ which( mrip.raw$INT_YEAR %in% yr.filter ), ]
    # effort.table <- mrip.raw[ which( mrip.raw$YEAR %in% yr.filter ), ]
    
    ### SPATIAL ###
    effort.table <- effort.table[ which( effort.table$NEW_STA %in% sta.filter ), ]
    
    if( "FL" %in% sta.filter | "FLW" %in% sta.filter | "FLE" %in% sta.filter ) {
      effort.table <- effort.table[ which(
        is.na(effort.table$FL_REG) | effort.table$FL_REG == "" | effort.table$FL_REG %in% fl.filter ), ]
    }
    if( "NC" %in% sta.filter ) {
      effort.table <- effort.table[ which(
        is.na(effort.table$NC_REG) | effort.table$NC_REG == "" | effort.table$NC_REG %in% nc.filter ), ]
    }
    
    ### MODE ###
    effort.modes = mode.filter
    # if( "Priv" %in% effort.modes & "LA" %in% sta.filter ) {    effort.modes = c( effort.modes,"Priv/Shore" )   }
    if( ( ( "Cbt" %in% effort.modes ) | ( "Hbt" %in% effort.modes ) ) &
        any( c("VA","MD","DE","PA","NJ","NY","CT","RI","MA","NH","ME") %in% sta.filter ) ) {
      effort.modes = c( effort.modes,"Cbt/Hbt" )
    }
    effort.table <- effort.table[ which( effort.table$NEW_MODEN %in% effort.modes ), ]
    rm( effort.modes )
    
    effort.table <- effort.table[ !( effort.table$NEW_STA == "LA" & effort.table$INT_YEAR >= 2014 ), ]
    effort.table <- effort.table[ !( effort.table$NEW_MODEN == "Hbt" & effort.table$SUB_REG == 6 ), ]
    effort.table <- effort.table[ !( effort.table$NEW_MODEN == "Hbt" & effort.table$SUB_REG == 7 & effort.table$INT_YEAR >= 1986 ), ]
    effort.table <- effort.table[ !( effort.table$NEW_MODEN == "Hbt" & effort.table$NEW_STA == "FLW" & effort.table$FL_REG == 3 ), ]
    
    
    # effort.summary = effort.table %>%
    #   group_by( YEAR, NEW_ST, NEW_STA ) %>%
    #   summarise( EFF = sum( ESTRIPS, na.rm=TRUE ) ) %>%
    #   select( YEAR, NEW_ST, NEW_STA, EFF ) %>%
    #   pivot_wider( names_from=c("NEW_ST","NEW_STA"), values_from=EFF )
    # 
    # effort.summary = effort.table %>%
    #   group_by( YEAR, NEW_MODE, NEW_MODEN ) %>%
    #   summarise( EFF = sum( ESTRIPS, na.rm=TRUE ) ) %>%
    #   select( YEAR, NEW_MODE, NEW_MODEN, EFF ) %>%
    #   pivot_wider( names_from=c("NEW_MODE","NEW_MODEN"), values_from=EFF )
    
  }
  
  
  
  ##########################
  ######     TPWD     ######
  ##########################
  
  if( survey == 'TPWD' ) {
    
    
    # tpwd.raw = dbGetQuery(con,
    #                          paste0("select *
    #                       from rdi.v_tpwd_trip_est_wave@secapxdv_dblk.sfsc.noaa.gov" ))
    # tpwd.effort <- tpwd.raw[ which( tpwd.raw$CYEAR %in% (first.year:term.year) ), ]
    # tpwd.effort <- tpwd.effort[ which( tpwd.effort$NEW_MODEN %in% mode_sub ), ]
    
    
    ###   Alternatively, "tpwd.raw" can also be reconstructed from the raw TPWD effort estimates table,
    ###   where TPWD now provides wave estimates and so we no longer need FRAC ( %catch by wave for each strata )
    ###   to partition TPWD seasonal estimates across waves, which was the old SEFSC approach. Instead,
    ###   we simply add our value-added fields to the raw TPWD effort estimates (as provided)...
    
    tpwd.raw = dbGetQuery(con,
                          paste0("select *
                        from rdi.tpwd_estimates_effort@secapxdv_dblk.sfsc.noaa.gov" ))
    
    ###   As documented in our 'Texas Effort Data' email string, the SYEAR field in this table is actually CYEAR.
    ###     Drew is going to work with Yanet to get this corrected but, in the meantime, I'm simply going to rename
    ###     the field myself ( and recreate the SYEAR field in case it's needed later - shouldn't be... )
    
    tpwd.raw = tpwd.raw %>%
      mutate_at( vars( SYEAR ), list( ~ as.integer(.) ) ) %>%
      
      rename( CYEAR = SYEAR ) %>%
      
      # filter( SYEAR > 1983 | ( SYEAR == 1983 & SEASON == 1 ) ) %>%
      # ###     ...where the raw TPWD effort table looks to have a six estimates from the 1983 low-use season
      # ###           ( for ACTIVITY = c(1,2) from AREANAME = c("BAY","TTS","EEZ") ), which I remove...
      
      mutate( 
        SYEAR = ifelse( SEASON == 2 & WAVE == 6, CYEAR+1, CYEAR ),
        # CYEAR = ifelse( SEASON == 2 & WAVE == 6, SYEAR-1, SYEAR ),
        NEW_SEASN = ifelse( SEASON == 1, "High", "Low" ),
        
        SUB_REG = 7, NEW_ST = 1,  NEW_STA = "TX", DS = "TPWD",
        NEW_AREA  = ifelse( AREANAME == "BAY", 5, ifelse( AREANAME == "TTS", 3, 4 ) ),
        NEW_AREAN = ifelse( AREANAME == "BAY", "Inshore", ifelse( AREANAME == "TTS", "Ocean<=10mi", "Ocean>10mi" ) ),
        
        NEW_MODE  = ifelse( ACTIVITY == 1, 4, 3 ),
        NEW_MODEN = ifelse( ACTIVITY == 1, "Priv", "Cbt" ),
        
        NTRP = ESTHRS/TRIPLEN ) %>%
      
      mutate( NPAR = NTRP/PARSIZE ) %>%
      ###     ...where NTRP = number of angler trips ( total hours fished / avg trip length in hours )
      ###          and NPAR = number of fishing parties ( number of angler trips / avg party size )
      
      select( CYEAR, SYEAR, SEASON, NEW_SEASN, WAVE, SUB_REG, NEW_ST, NEW_STA, ACTIVITY, NEW_MODE, NEW_MODEN,
              AREA, AREANAME, NEW_AREA, NEW_AREAN, DS,
              PARSIZE, TRIPLEN, TTLANGLR, ESTHRS, ESTHRS_SE, NTRP, NPAR )
    
    effort.table <- tpwd.raw[ which( tpwd.raw$CYEAR %in% yr.filter ), ]
    effort.table <- effort.table[ which( effort.table$NEW_MODEN %in% mode.filter ), ]
    
    
    # effort.summary = effort.table %>%
    #   group_by( YEAR, NEW_MODE, NEW_MODEN ) %>%
    #   summarise( EFF = sum( NTRP, na.rm=TRUE ) ) %>%
    #   select( YEAR, NEW_MODE, NEW_MODEN, EFF ) %>%
    #   pivot_wider( names_from=c("NEW_MODE","NEW_MODEN"), values_from=EFF )
    
  }
  
  
  
  ##########################
  ######     LACR     ######
  ##########################
  
  if( survey == 'LACR' ) {
    
    
    lacr.raw = dbGetQuery(con,
                          paste0("select *
                        from rdi.la_creel_effort@secapxdv_dblk.sfsc.noaa.gov" ))
    
    lacr.raw = lacr.raw %>%
      mutate( SUB_REG = 7, NEW_ST = 2,  NEW_STA = "LA", DS = "LACR",
              NEW_AREA  = ifelse( AREA_X_F ==                        "Ocean > 3 mi" ,  2,
                          ifelse( AREA_X_F == "State Waters (Inland + Ocean < 3 mi)",  6, NA ) ),
              NEW_AREAN = ifelse( AREA_X_F ==                        "Ocean > 3 mi" , "Ocean>3 mi",
                          ifelse( AREA_X_F == "State Waters (Inland + Ocean < 3 mi)", "Inland+Ocean<=3mi", NA ) ),
              
              NEW_MODE  = ifelse( MODE_FX_F ==  'Charter Boat',  3,
                          ifelse( MODE_FX_F == 'Private/Shore',  6, NA ) ),
              NEW_MODEN = ifelse( MODE_FX_F ==  'Charter Boat', 'Cbt',
                          ifelse( MODE_FX_F == 'Private/Shore', 'Priv/Shore', NA ) ) )
    
    
    effort.table <- lacr.raw[ which( lacr.raw$INT_YEAR %in% yr.filter ), ]
    if( !( "Priv" %in% mode.filter ) ) {    effort.table <- effort.table[ -which( effort.table$MODES == "Private" ), ]     }
    if( !( "Cbt"  %in% mode.filter ) ) {    effort.table <- effort.table[ -which( effort.table$MODES == "Charter" ), ]     }
    
    
    # effort.summary = effort.table %>%
    #   group_by( YEAR, NEW_MODE, NEW_MODEN ) %>%
    #   summarise( EFF = sum( EXPANDED_EFFORT, na.rm=TRUE ) ) %>%
    #   select( YEAR, NEW_MODE, NEW_MODEN, EFF ) %>%
    #   pivot_wider( names_from=c("NEW_MODE","NEW_MODEN"), values_from=EFF )
    
  }
  
  
  return( effort.table )
  
}
