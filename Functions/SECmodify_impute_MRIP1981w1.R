

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
###   ---------------
###   MRIP 1981-wave1
###   ---------------
###
###
###     ...wherein the MRFSS survey didn't begin until March 1981 (i.e., wave 2 ) and so there is an inherent data gap
###       in MRIP survey data/estimates. In accordance with SEDAR best practices ( SEDAR PW7 -- Recreational Issue #2 ),
###       and as has been applied in past SEDARs (e.g., S10 gag, S12 red grouper, S28 spanish mackerel & cobia, S31 red snapper),
###       MRIP catch for 1981-wave1 can be imputed for the Gulf of Mexico and east coast of Florida using either:
###
###         (1) the proportion of wave1 catch to that from other waves (2-6) in years 1982-1984 by fishing mode and area.
###             These proportions can then be applied to the total catch from waves 2-6 in 1981 to estimate 1981 wave 1 catch...
###         (2) the average (wave1) catch across years 1982-1984...
###
###       The ratio method (#1) is the preferred method and applied when ratios are reasonably stable from year to year.
###       However, when ratios are highly variable (from year to year), the average catch approach (#2) is to be applied...
###
###
###    *** summary.MRIP.1981w1( )
###           ...which evaluates the amount and relative stability of catch estimates from wave1
###             ( relative to other waves ) over years 1982-1984, to determine (respectively)
###             if 1981-wave1 MRIP needs to be imputed and if the preferred ratio approach (#1) is reasonable...
###
###
###    *** impute.MRIP.1981w1( )
###           ...which applies the chosen method ( from the summary.MRIP.1981w1() function) to impute MRIP 1981-wave1 catch...
###
###    *** impute.MRIP.1981w1.effort( )
###           ...which applies the chosen method ( from the summary.MRIP.1981w1() function) to impute MRIP 1981-wave1 effort...
###
###
### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------


summary.MRIP.1981w1 = function( genrec.table ) {
  
  
  summary.table <- list()
  
  
  ### -------------------------------
  ### Table - Total Catch by Wave ###
  ### -------------------------------
  ###     (i.e., to evaluate 'how much catch' is likely to be attributed to wave1 )
  
  summary.table$WaveCatch = genrec.table %>%
    
    # filter( DS == "MRIP" ) %>%                                         ### ...wave1 RATIOs over all years
    filter( DS == "MRIP" & YEAR %in% 1982:1984 ) %>%                     ### ...wave1 RATIOs for 1982:1984
    
    filter( NEW_STA %in% c("TX","LA","MS","AL","FLW","FLE") ) %>%
    ###   ...where 1981 wave1 catch is only estimated for FLE & GOM states -- MRIP doesn't sample in states north of FLE
    ###     for wave1, for which effort is considered negligible. Therefore, 'missing' 1981-wave1 estimates in these states
    ###     (north of FLE) isn't considered a data gap, just an assumption (of zero catch)...
    
    group_by( WAVE ) %>%
    summarise( AB1 = sum( AB1, na.rm=TRUE ),
                B2 = sum(  B2, na.rm=TRUE ),
               wwt = sum( lbsest_SECwwt, na.rm=TRUE ),
               gwt = sum( lbsest_SECgwt, na.rm=TRUE ) ) %>%
    mutate( AB1.p = AB1 / sum(AB1),
             B2.p =  B2 / sum( B2),
            wwt.p = wwt / sum(wwt),
            gwt.p = gwt / sum(gwt) )
  
  
  
  ### -------------------------------------------
  ### Plots - Stability of Wave1 Catch Ratios ###
  ### -------------------------------------------
  ###     (i.e., to determine whether approach #1 or #2 is more appropriate for this stock )
  
  
  ### Table of %Wave1 Catch ###
  ### -------------------------
  
  fracs.table = genrec.table %>%
    
    # filter( DS == "MRIP" ) %>%                                         ### ...wave1 RATIOs over all years
    filter( DS == "MRIP" & YEAR %in% 1982:1984 ) %>%                     ### ...wave1 RATIOs for 1982:1984
    filter( NEW_STA %in% c("TX","LA","MS","AL","FLW","FLE") ) %>%
    
    mutate( wave_collapse = ifelse( WAVE==1, "w1","other" ) ) %>%
    
    group_by( wave_collapse, YEAR, NEW_MODEN, NEW_AREAN ) %>%                   ### MODE/AREA -- ***Best Practices***
    # group_by( wave_collapse, YEAR, NEW_STA, NEW_MODEN, NEW_AREAN ) %>%        ### STATE/MODE/AREA -- ...just to look...
    
    summarise( AB1 = sum( AB1, na.rm=TRUE ),
                B2 = sum(  B2, na.rm=TRUE ),
               wwt = sum( lbsest_SECwwt, na.rm=TRUE ),
               gwt = sum( lbsest_SECgwt, na.rm=TRUE ) ) %>%
    pivot_wider( names_from = wave_collapse, values_from = c(AB1,B2,wwt,gwt) )
  
  ###   ...and as a check that wave1 is included in 'fracs.table', which is required in the script below...
  if( 'AB1_w1' %notin% names(fracs.table) ) {  fracs.table$AB1_w1 = 0  }
  if(  'B2_w1' %notin% names(fracs.table) ) {  fracs.table$B2_w1  = 0  }
  if( 'wwt_w1' %notin% names(fracs.table) ) {  fracs.table$wwt_w1 = 0  }
  if( 'gwt_w1' %notin% names(fracs.table) ) {  fracs.table$gwt_w1 = 0  }
  
  fracs.table = fracs.table %>%
    
    replace_na( list( AB1_w1 = 0, AB1_other = 0,  B2_w1 = 0,  B2_other = 0,
                      wwt_w1 = 0, wwt_other = 0, gwt_w1 = 0, gwt_other = 0 ) ) %>%
    
    mutate( AB1_ratio = ifelse( AB1_other == 0, 0, AB1_w1 / AB1_other ),
             B2_ratio = ifelse(  B2_other == 0, 0,  B2_w1 /  B2_other ),
            wwt_ratio = ifelse( wwt_other == 0, 0, wwt_w1 / wwt_other ),
            gwt_ratio = ifelse( gwt_other == 0, 0, gwt_w1 / gwt_other ) ) %>%
    
    select( -c( AB1_w1,AB1_other, B2_w1,B2_other, wwt_w1,wwt_other, gwt_w1,gwt_other )  ) %>%
    pivot_longer( cols = c("AB1_ratio","B2_ratio","wwt_ratio","gwt_ratio"), names_to = "Metric", values_to = "Ratio" )
  
  
  
  ### Figure of %Wave1 Catch across Years (1982-84) ###
  ### -------------------------------------------------
  summary.table$fracs.byYEAR = ggplot(
    
    fracs.table,
    
    # ### STATE/MODE/AREA  --  < Data Exploration >
    # ###
    # ###     Note that this plot can also be used to explore the data. In particular,
    # ###     state is not used in the imputation of 1981 wave1 estimates (under any circumstances),
    # ###     but may be included ( in the created 'fracs' table above ) to investigate whether the
    # ###     %wave1 RATIOs are abnormally high/low for certain states. Note that for such analyses,
    # ###     NEW_STA needs to be included in 'fracs' ( which is currently commented out above )...
    # 
    # fracs.table[ fracs.table$NEW_STA == "LA" , ],
    # fracs.table[ fracs.table$NEW_STA == "MS" , ],
    # fracs.table[ fracs.table$NEW_STA == "AL" , ],
    # fracs.table[ fracs.table$NEW_STA == "FLW" , ],
    # fracs.table[ fracs.table$NEW_STA == "FLE" , ],
    
    aes( x=YEAR , y=Ratio , colour=Metric ) ) +
    
    geom_point() + geom_line() +
    facet_grid( NEW_AREAN ~ NEW_MODEN, scales="free" ) +
    scale_x_continuous( breaks=seq(1982,1984,1) )
  
  
  
  ### Figure of %Wave1 Catch across Catch Type (AB1,B2,LBS) ###
  ### ---------------------------------------------------------
  
  dummy = fracs.table
  
  dummy$Metric = gsub( "_ratio","", dummy$Metric )      ### ...to simplify the x-axis (e.g., print 'AB1' vs 'AB1_ratio' )
  dummy = dummy[ dummy$Metric != 'gwt', ]               ### ...b/c WWT & GWT have the same ratio (related by a scaling factor),
  dummy$Metric[ dummy$Metric == 'wwt' ] = 'WGT'         ###      will only include one in my summary plot below...
  
  dummy = dummy %>% rename( Year = YEAR )
  
  summary.table$fracs.byMETRIC = ggplot( dummy, aes( x=Metric , y=Ratio , fill=as.factor(Year) ) ) +
    geom_col( position='dodge' ) +
    facet_grid( NEW_MODEN ~ NEW_AREAN, scales="free" ) +
    ylab( "Catch Ratio ( Wave1 : Waves 2-6 )" ) +
    guides( fill = guide_legend( title="Year") ) +
    scale_fill_brewer( palette="Blues") +
    theme_bw() +
    theme( text = element_text(size = 11),
           # axis.text.x = element_text(angle = 90, vjust=0.5),
           axis.title.x = element_blank(),
           # axis.title.y = element_blank(),
           legend.position = "bottom",
           panel.grid.major = element_line(colour = "grey", linewidth = 0.5),
           panel.grid.minor = element_line(colour = "grey", linewidth = 0.2),
           panel.border = element_rect(colour = "black", fill = NA) )
  
  
  
  ### --------------------------------------------------------------
  ### Plots - Comparison of Imputed 1981-wave1 Catch btw Methods ###
  ### --------------------------------------------------------------
  ###     (i.e., to evaluate if the decision of which approach to use even 'matters' for this stock )
  
  
  ### Table of Imputations ###
  ### ------------------------
  
  ### Approach 1 -- Proportion Wave1-Catch ( "Best Practice" )
  
  fracs = genrec.table %>%
    filter( DS == "MRIP" & YEAR %in% 1982:1984 ) %>%
    filter( NEW_STA %in% c("TX","LA","MS","AL","FLW","FLE") ) %>%
    mutate( wave_collapse = ifelse( WAVE==1, "w1","other" ) ) %>%
    group_by( wave_collapse, NEW_MODEN, NEW_AREAN ) %>%
    summarise( AB1 = sum( AB1, na.rm=TRUE ),
                B2 = sum(  B2, na.rm=TRUE ),
               wwt = sum( lbsest_SECwwt, na.rm=TRUE ),
               gwt = sum( lbsest_SECgwt, na.rm=TRUE ) ) %>%
    pivot_wider( names_from = wave_collapse, values_from = c(AB1,B2,wwt,gwt) )
  
  if( 'AB1_w1' %notin% names(fracs) ) {  fracs$AB1_w1 = 0  }
  if(  'B2_w1' %notin% names(fracs) ) {  fracs$B2_w1  = 0  }
  if( 'wwt_w1' %notin% names(fracs) ) {  fracs$wwt_w1 = 0  }
  if( 'gwt_w1' %notin% names(fracs) ) {  fracs$gwt_w1 = 0  }
  
  fracs = fracs %>%
    replace_na( list( AB1_w1 = 0, AB1_other = 0,  B2_w1 = 0,  B2_other = 0,
                      wwt_w1 = 0, wwt_other = 0, gwt_w1 = 0, gwt_other = 0 ) ) %>%
    mutate( AB1_ratio = ifelse( AB1_other == 0, 0, AB1_w1 / AB1_other ),
             B2_ratio = ifelse(  B2_other == 0, 0,  B2_w1 /  B2_other ),
            wwt_ratio = ifelse( wwt_other == 0, 0, wwt_w1 / wwt_other ),
            gwt_ratio = ifelse( gwt_other == 0, 0, gwt_w1 / gwt_other ) ) %>%
    select( -c( AB1_w1,AB1_other, B2_w1,B2_other, wwt_w1,wwt_other, gwt_w1,gwt_other ) )
  
  dummy = genrec.table %>%
    filter( DS == "MRIP" & YEAR == 1981 & WAVE %in% 2:6 ) %>%
    group_by( NEW_ST,NEW_STA, FL_REG, MODE_FX,NEW_MODE,NEW_MODEN, AREA_X,NEW_AREAN ) %>%
    summarise( AB1_w26 = sum( AB1, na.rm=TRUE ),
                B2_w26 = sum(  B2, na.rm=TRUE ),
               wwt_w26 = sum( lbsest_SECwwt, na.rm=TRUE ),
               gwt_w26 = sum( lbsest_SECgwt, na.rm=TRUE ) ) %>%
    left_join( fracs, by = c("NEW_MODEN","NEW_AREAN") ) %>%
    replace_na( list( AB1_ratio = 0,  B2_ratio = 0, wwt_ratio = 0, gwt_ratio = 0 ) ) %>%
    mutate( DS = "MRIP.imp", YEAR = 1981, WAVE = 1,
            AB1 = AB1_w26 * AB1_ratio,  lbsest_SECwwt = wwt_w26 * wwt_ratio,
             B2 =  B2_w26 *  B2_ratio,  lbsest_SECgwt = gwt_w26 * gwt_ratio ) %>%
    select( -c( AB1_w26,B2_w26,wwt_w26,gwt_w26, AB1_ratio,B2_ratio,wwt_ratio,gwt_ratio ) )
  
  blah1 = genrec.table %>% bind_rows( dummy )
  rm( fracs, dummy )
  
  blah1 = blah1 %>%
    filter( YEAR %in% 1981:1990 ) %>%
    group_by( YEAR,DS ) %>%
    summarise( AB1 = sum( AB1, na.rm=TRUE ),
                B2 = sum(  B2, na.rm=TRUE ) )
  
  
  ### Approach 2 -- Average Catch (1982-84) ###
  
  avg.cat = genrec.table %>%
    filter( DS == "MRIP" & YEAR %in% 1982:1984 & WAVE == 1 ) %>%
    filter( NEW_STA %in% c("TX","LA","MS","AL","FLW","FLE") ) %>%
    group_by( SUB_REG, NEW_ST,NEW_STA, FL_REG, NEW_MODE,NEW_MODEN, NEW_AREAN ) %>%
    summarise( AB1 = sum( AB1, na.rm=TRUE ) / 3 ,
                B2 = sum(  B2, na.rm=TRUE ) / 3 ,
               wwt = sum( lbsest_SECwwt, na.rm=TRUE ) / 3 ,
               gwt = sum( lbsest_SECgwt, na.rm=TRUE ) / 3   )
  
  avg.cat = avg.cat %>%
    ungroup() %>%
    mutate( DS = "MRIP.imp", YEAR = 1981, WAVE = 1 ) %>%
    rename( lbsest_SECwwt = wwt,
            lbsest_SECgwt = gwt ) %>%
    mutate_all( list( ~ as.character(.) ) ) %>%
    mutate_at( vars( YEAR,WAVE, SUB_REG, NEW_ST, NEW_MODE,
                     AB1,B2,lbsest_SECwwt,lbsest_SECgwt ), list( ~ as.numeric(.) ) )
  
  blah2 = genrec.table %>% bind_rows( avg.cat )
  rm( avg.cat )
  
  blah2 = blah2 %>%
    filter( YEAR %in% 1981:1990 ) %>%
    group_by( YEAR,DS ) %>%
    summarise( AB1 = sum( AB1, na.rm=TRUE ),
                B2 = sum(  B2, na.rm=TRUE ) )
  
  ### JOIN ###
  
  dummy = full_join( blah1, blah2, by=c('YEAR','DS'), suffix=c('_prop','_avg') ) %>%
    pivot_longer( !c('YEAR','DS'), names_to = "VARIABLE", values_to = "VALUE" ) %>%
    mutate( METHOD   = gsub( ".*_","", VARIABLE ),
            VARIABLE = gsub( "_.*","", VARIABLE ),
            VALUE    = VALUE / 1000 ) %>%
    mutate( DS = factor( DS, levels = c("MRIP.imp","MRIP") ) )
  rm( blah1, blah2 )
  
  
  
  ### Stacked Barplot - Timeseries of RAW (+) IMP Catch ###
  ### -----------------------------------------------------
  
  n.years = length(unique(dummy$YEAR))
  
  summary.table$methods.RAWvIMP <- ggplot( data = dummy, aes( x=YEAR, y=VALUE, fill=DS ) ) +
    facet_grid( VARIABLE ~ METHOD, scales = 'free' ) +
    geom_col( position = "stack", color="black" ) +
    scale_fill_manual( values = c("blue","red") ) +
    labs( title="", x="Year", y="Thousands of Fish" ) +
    scale_x_continuous( breaks = scales::pretty_breaks( n = (n.years/2) ) ) +
    expand_limits(y = 0) +
    theme_bw() +
    theme( text = element_text(size = 11),
           axis.text.x = element_text(angle = 90, vjust=0.5),
           legend.position = "bottom",
           panel.grid.major = element_line(colour = "grey", linewidth = 0.5),
           panel.grid.minor = element_line(colour = "grey", linewidth = 0.2),
           panel.border = element_rect(colour = "black", fill = NA) )
  
  
  
  ### Linear Plot - Timeseries of TOTAL Catch ###
  ### -------------------------------------------
  
  dummy = dummy %>%
    group_by( YEAR, VARIABLE, METHOD ) %>%
    summarize( VALUE = sum( VALUE, na.rm=TRUE ) )
  
  summary.table$methods.TOTAL <- ggplot( data = dummy, aes( x=YEAR ) ) +
    facet_grid( VARIABLE ~ . , scales = 'free' ) +
    geom_line( aes( y=VALUE, color=METHOD, linetype=METHOD ), linewidth=1.2 ) +
    labs( title="", x="Year", y="Thousands of Fish" ) +
    scale_x_continuous( breaks = scales::pretty_breaks( n = (n.years/2) ) ) +
    expand_limits(y = 0) +
    theme_bw() +
    theme( text = element_text(size = 11),
           axis.text.x = element_text(angle = 90, vjust=0.5),
           legend.position = "bottom",
           panel.grid.major = element_line(colour = "grey", linewidth = 0.5),
           panel.grid.minor = element_line(colour = "grey", linewidth = 0.2),
           panel.border = element_rect(colour = "black", fill = NA) )
  rm( dummy )
  
  
  
  return( summary.table )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------


impute.MRIP.1981w1 = function( genrec.table, method = c('None','prop_w1_w26','avg_82_84') ) {
  
  
  if( method == 'prop_w1_w26' ) {
    
    ### Approach (1) ###
    ###
    ###       CATCH_81_wave1  =  CATCH_81_waves2-6 * ( CATCH_8284_wave1 / CATCH_8284_waves2-6 )
    
    fracs = genrec.table %>%
      filter( DS == "MRIP" & YEAR %in% 1982:1984 ) %>%
      filter( NEW_STA %in% c("TX","LA","MS","AL","FLW","FLE") ) %>%
      mutate( wave_collapse = ifelse( WAVE==1, "w1","other" ) ) %>%
      group_by( wave_collapse, NEW_MODEN, NEW_AREAN ) %>%
      summarise( AB1 = sum( AB1, na.rm=TRUE ),
                 B2 = sum(  B2, na.rm=TRUE ),
                 wwt = sum( lbsest_SECwwt, na.rm=TRUE ),
                 gwt = sum( lbsest_SECgwt, na.rm=TRUE ) ) %>%
      pivot_wider( names_from = wave_collapse, values_from = c(AB1,B2,wwt,gwt) ) %>%
      replace_na( list( AB1_w1 = 0, AB1_other = 0,  B2_w1 = 0,  B2_other = 0,
                        wwt_w1 = 0, wwt_other = 0, gwt_w1 = 0, gwt_other = 0 ) ) %>%
      mutate( AB1_ratio = ifelse( AB1_other == 0, 0, AB1_w1 / AB1_other ),
              B2_ratio = ifelse(  B2_other == 0, 0,  B2_w1 /  B2_other ),
              wwt_ratio = ifelse( wwt_other == 0, 0, wwt_w1 / wwt_other ),
              gwt_ratio = ifelse( gwt_other == 0, 0, gwt_w1 / gwt_other ) ) %>%
      select( -c( AB1_w1,AB1_other, B2_w1,B2_other, wwt_w1,wwt_other, gwt_w1,gwt_other ) )
    
    dummy = genrec.table %>%
      filter( DS == "MRIP" & YEAR == 1981 & WAVE %in% 2:6 ) %>%
      filter( NEW_STA %in% c("TX","LA","MS","AL","FLW","FLE") ) %>%
      group_by( NEW_ST,NEW_STA, FL_REG, MODE_FX,NEW_MODE,NEW_MODEN, AREA_X,NEW_AREAN ) %>%
      ###     ...where, although ratios may not be calculated at the state/mode/area level, they are applied at that resolution...
      summarise( AB1_w26 = sum( AB1, na.rm=TRUE ),
                 B2_w26 = sum(  B2, na.rm=TRUE ),
                 wwt_w26 = sum( lbsest_SECwwt, na.rm=TRUE ),
                 gwt_w26 = sum( lbsest_SECgwt, na.rm=TRUE ) ) %>%
      left_join( fracs, by = c("NEW_MODEN","NEW_AREAN") ) %>%
      replace_na( list( AB1_ratio = 0,  B2_ratio = 0, wwt_ratio = 0, gwt_ratio = 0 ) ) %>%
      mutate( DS = "MRIP",
              YEAR = 1981,
              WAVE = 1,
              
              SUB_REG   = ifelse( NEW_STA == 'FLE', 6, 7 ),
              NEW_COM   = ifelse( length(new.com) == 1, new.com, NA ),
              NEW_SCI   = ifelse( length(new.sci) == 1, new.sci, NA ),
              SP_CODE   = ifelse( length(nodc.code) == 1, nodc.code, NA ),
              ITIS_CODE = ifelse( length(itis.code) == 1, itis.code, NA ),
              SA_LABEL  = ifelse( length(new.com) == 1, spp.info$SA_LABEL[ spp.info$NEW_COM == new.com ], NA ),
              GOM_LABEL = ifelse( length(new.com) == 1, spp.info$GOM_LABEL[ spp.info$NEW_COM == new.com ], NA ),
              
              AB1 = AB1_w26 * AB1_ratio,
              B2 =  B2_w26 *  B2_ratio,
              lbsest_SECwwt = wwt_w26 * wwt_ratio,
              lbsest_SECgwt = gwt_w26 * gwt_ratio,
              LBSEST_SECSOURCE = "MRIPsub" ) %>%
      select( -c( AB1_w26,B2_w26,wwt_w26,gwt_w26, AB1_ratio,B2_ratio,wwt_ratio,gwt_ratio ) )
    
    genrec.table = genrec.table %>% bind_rows( dummy )
    rm( fracs, dummy )
    
    
    
  } else if( method == 'avg_82_84' ) {
    
    ### Approach (2) ###
    ###
    ###       CATCH_81_wave1  =  mean( CATCH_8284_wave1 )
    
    
    ### Average MRIP wave1 catch from 1982-1984 ###
    ###
    ###       ...where 'genrec.table' already contains all the strata for which 1982-1984 (wave1) catch estimates
    ###             exist for this species, and so all I do is sum this catch (across those strata used in the imputation)
    ###             and divide by three (for an average across the three years)...
    avg.cat = genrec.table %>%
      filter( DS == "MRIP" & YEAR %in% 1982:1984 & WAVE == 1 ) %>%
      filter( NEW_STA %in% c("TX","LA","MS","AL","FLW","FLE") ) %>%
      
      # group_by( YEAR, SUB_REG, NEW_ST,NEW_STA, FL_REG, NEW_MODE,NEW_MODEN, NEW_AREAN ) %>%
      group_by(         SUB_REG, NEW_ST,NEW_STA, FL_REG, NEW_MODE,NEW_MODEN, NEW_AREAN ) %>%
      summarise( AB1 = sum( AB1, na.rm=TRUE ) / 3 ,
                 B2 = sum(  B2, na.rm=TRUE ) / 3 ,
                 wwt = sum( lbsest_SECwwt, na.rm=TRUE ) / 3 ,
                 gwt = sum( lbsest_SECgwt, na.rm=TRUE ) / 3   )
    
    ###       ...I then add the 'missing' fields for my 1981 wave1 imputations...
    
    avg.cat = avg.cat %>%
      ungroup() %>%
      mutate( DS = "MRIP",
              YEAR = 1981,
              WAVE = 1,
              
              SUB_REG   = ifelse( NEW_STA == 'FLE', 6, 7 ),
              NEW_COM   = ifelse( length(new.com) == 1, new.com, NA ),
              NEW_SCI   = ifelse( length(new.sci) == 1, new.sci, NA ),
              SP_CODE   = ifelse( length(nodc.code) == 1, nodc.code, NA ),
              ITIS_CODE = ifelse( length(itis.code) == 1, itis.code, NA ),
              SA_LABEL  = ifelse( length(new.com) == 1, spp.info$SA_LABEL[ spp.info$NEW_COM == new.com ], NA ),
              GOM_LABEL = ifelse( length(new.com) == 1, spp.info$GOM_LABEL[ spp.info$NEW_COM == new.com ], NA ),
              
              LBSEST_SECSOURCE = ifelse( NEW_STA == "TX", "TPWDsub", "MRIPsub" ) ) %>%
      rename( lbsest_SECwwt = wwt,
              lbsest_SECgwt = gwt ) %>%
      mutate_all( list( ~ as.character(.) ) ) %>%
      mutate_at( vars( YEAR,WAVE, SUB_REG, NEW_ST, NEW_MODE,
                       AB1,B2,lbsest_SECwwt,lbsest_SECgwt ), list( ~ as.numeric(.) ) )
    
    genrec.table = genrec.table %>% bind_rows( avg.cat )
    rm( avg.cat )
    
  }
  
  return ( genrec.table )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------


impute.MRIP.1981w1.effort = function( genrec.table, method = c('None','prop_w1_w26','avg_82_84') ) {
  
  
  if( method == 'prop_w1_w26' ) {
    
    ### Approach (1) ###
    ###
    ###       CATCH_81_wave1  =  CATCH_81_waves2-6 * ( CATCH_8284_wave1 / CATCH_8284_waves2-6 )
    
    fracs = genrec.table %>%
      filter( DS == "MRIP" & INT_YEAR %in% 1982:1984 ) %>%
      filter( NEW_STA %in% c("TX","LA","MS","AL","FLW","FLE") ) %>%
      mutate( wave_collapse = ifelse( WAVE==1, "w1","other" ) ) %>%
      group_by( wave_collapse, NEW_MODEN, NEW_AREAN ) %>%
      summarise(  FES = sum(    ESTRIPS, na.rm=TRUE ),
                 CHTS = sum( CHTS_TRIPS, na.rm=TRUE ) ) %>%
      pivot_wider( names_from = wave_collapse, values_from = c(FES,CHTS) ) %>%
      replace_na( list( FES_w1 = 0, FES_other = 0,  CHTS_w1 = 0,  CHTS_other = 0 ) ) %>%
      mutate(  FES_ratio = ifelse(  FES_other == 0, 0,  FES_w1 /  FES_other ),
              CHTS_ratio = ifelse( CHTS_other == 0, 0, CHTS_w1 / CHTS_other ) ) %>%
      select( -c( FES_w1, FES_other, CHTS_w1, CHTS_other ) )
    
    dummy = genrec.table %>%
      filter( DS == "MRIP" & INT_YEAR == 1981 & WAVE %in% 2:6 ) %>%
      filter( NEW_STA %in% c("TX","LA","MS","AL","FLW","FLE") ) %>%
      group_by( NEW_ST,NEW_STA, FL_REG, MODE_FX,NEW_MODE,NEW_MODEN, AREA_X,NEW_AREAN ) %>%
      ###     ...where, although ratios may not be calculated at the state/mode/area level, they are applied at that resolution...
      summarise(  FES_w26 = sum(    ESTRIPS, na.rm=TRUE ),
                 CHTS_w26 = sum( CHTS_TRIPS, na.rm=TRUE ) ) %>%
      left_join( fracs, by = c("NEW_MODEN","NEW_AREAN") ) %>%
      replace_na( list( FES_ratio = 0, CHTS_ratio = 0 ) ) %>%
      mutate( DS = "MRIP",
              INT_YEAR = 1981,
              WAVE = 1,
              SUB_REG   = ifelse( NEW_STA == 'FLE', '6', '7' ),
              
              ESTRIPS = FES_w26 * FES_ratio,
              CHTS_TRIPS = CHTS_w26 * CHTS_ratio,
              IMP_NTRP = method ) %>%
      select( -c( FES_w26, CHTS_w26, FES_ratio, CHTS_ratio ) )
    
    genrec.table = genrec.table %>% bind_rows( dummy )
    rm( fracs, dummy )
    
    
    
  } else if( method == 'avg_82_84' ) {
    
    ### Approach (2) ###
    ###
    ###       CATCH_81_wave1  =  mean( CATCH_8284_wave1 )
    
    
    ### Average MRIP wave1 catch from 1982-1984 ###
    ###
    ###       ...where 'genrec.table' already contains all the strata for which 1982-1984 (wave1) catch estimates
    ###             exist for this species, and so all I do is sum this catch (across those strata used in the imputation)
    ###             and divide by three (for an average across the three years)...
    
    avg.cat = genrec.table %>%
      filter( DS == "MRIP" & INT_YEAR %in% 1982:1984 & WAVE == 1 ) %>%
      filter( NEW_STA %in% c("TX","LA","MS","AL","FLW","FLE") ) %>%
      
      # group_by( INT_YEAR, SUB_REG, NEW_ST,NEW_STA, FL_REG, NEW_MODE,NEW_MODEN, NEW_AREAN ) %>%
      group_by(             SUB_REG, NEW_ST,NEW_STA, FL_REG, NEW_MODE,NEW_MODEN, NEW_AREAN ) %>%
      summarise(    ESTRIPS = sum(    ESTRIPS, na.rm=TRUE ) / 3 ,
                 CHTS_TRIPS = sum( CHTS_TRIPS, na.rm=TRUE ) / 3   )
    
    ###       ...I then add the 'missing' fields for my 1981 wave1 imputations...
    
    avg.cat = avg.cat %>%
      ungroup() %>%
      mutate( DS = "MRIP",
              INT_YEAR = 1981,
              WAVE = 1,
              IMP_NTRP = method )
      # mutate_all( list( ~ as.character(.) ) ) %>%
      # mutate_at( vars( INT_YEAR,WAVE, NEW_ST, NEW_MODE,
      #                  ESTRIPS,NUM_VAR, CHTS_TRIPS,CHTS_VAR_TRIPS ), list( ~ as.numeric(.) ) )
    
    genrec.table = genrec.table %>% bind_rows( avg.cat )
    rm( avg.cat )
    
  }
  
  return ( genrec.table )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------



