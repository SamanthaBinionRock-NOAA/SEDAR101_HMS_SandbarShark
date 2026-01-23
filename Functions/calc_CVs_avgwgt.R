

### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------
###
###     DESCRIPTION
###
###   ...where the functions below estimate uncertainties for the catch-in-number estimates (AB1 & B2) of
###       our various general recreational surveys...
###
###
###    *** CVs.avgwgt( )
###           ...which is the primary function contained in this script and calculates the variability in raw size data,
###             which are used as a proxy for uncertainty in SEFSC average weight estimates ( as described in Approach #2 in S74-DW-12 ).
###
###         As an overview of the general approach, we calculate a mean and standard error from the raw size data,
###         the calculation of which can be done in a few ways. When samples sizes are adequate, the preferred approach is to:
###
###         -- summarize the original (fish-level) 'size.table' at the trip/vessel level to account for any
###           non-independence in fish sizes (e.g., similar fish sizes when landed by the same vessel ).
###           Once calculated, these trip-level summaries (by vessel) are then treated as "raw size data", from which
###           we calculate mean and standard errors at the finest possible resolution (region-year-state-mode-wave-area)
###           to match the stratification used in the calculation of GenRec catch & effort estimates.
###
###         However, when samples sizes are not adequate to support this (trip-level) approach, wherein multiple strata have
###         fish sizes being informed from a single observation (i.e., n.trip=1, var=0 ), two additional modifications may be needed:
###
###         -- First, we change the resolution at which the mean and standard error summaries are calculated to the year-mode level
###           ( vs. RYSMWA ), which is the coarsest possible resolution that still meets the demands of our assessment analysts.
###         -- If the year-mode adjustment still results in a number of strata having a SE = 0, the trip-level summaries
###           will not be calculated, instead having mean and standard errors calculated from the (observed) fish level.
###           This adjustment may lead to biased estimates if fish sizes from the same intercept are correlated but,
###           given this adjustment is only being made when sample sizes are limited (i.e., not many fish/intercepts to consider),
###           it is believed to be minor. Additionally, this bias is probably 'worth it' if it leads to non-zero avgwgt CVs,
###           which are needed by assessment analysts ( CV=0 cannot be used ).
###
###     *** convert.long.table.avgwt()
###           ...which converts a GenRec size-CV table from wide-format, with fields that (can) include
###                       YEAR, PRIV_WGT, CBT_WGT,... PRIV_SE, CBT_SE,... PRIV_FISH,... PRIV_TRP,... etc
###             into long-format, with four fields = YEAR, NEW_MODEN, METRIC (WGT,SE,FISH,TRP), and value
###
###
###       Note that these functions are set-up to handle stocks with multiple StockID boundaries. Briefly, if a 'SID' field exists,
###       it is added to the various group_by() and select() statements to carry it through (as a strata) in the estimation.
###       This field is then used to separate the avgwgt summaries when the final excel file is written (in the main 'size' script)...
###
### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------

CVs.avgwgt = function( data = c( 'trip','fish' ), estimation = c( 'RYSMWA','YM' ), add.strata, genrec.table ) {
  ###     ...where 'data' identifies the original 'data units' from which mean & SEs are to be calculated
  ###                 ( 'trip' = trip-level summaries  ;  'fish' = raw observations of fish size ),
  ###         'estimation' identifies the resolution at which mean & SE calculations are to be conducted
  ###                 ( 'RYSMWA' = region-year-state-mode-wave-area   ;   'YM' = year-mode ),
  ###         'add.strata' identifies any strata that are to be added to the estimation (e.g., 'SID' ), and
  ###         'genrec.table' is the table of raw size data (fish-level observations of size)
  ###                 from which all size summaries in this function are to be calculated...
  
  
  ###   Note that the code below requires different fields in its group_by() arguments, depending on what is being summarized
  ###     and (more importantly) what additional strata are to be considered (as identified in 'add.strata'). Instead of making
  ###     these distinctions below, which would require separate if() statements at each group_by() argument, I define all
  ###     required group_by() arguments here...
  
  group_by_arguments_1 <- c( 'MY_ID_CODE','SUB_REG','YEAR','NEW_STA','FL_REG','NC_REG','NEW_MODE','NEW_MODEN','WAVE','NEW_AREAN' )
  group_by_arguments_2 <- c(              'SUB_REG','YEAR','NEW_STA','FL_REG','NC_REG','NEW_MODE','NEW_MODEN','WAVE','NEW_AREAN' )
  group_by_arguments_3 <- c(                        'YEAR','NEW_MODE','NEW_MODEN'                          )
  group_by_arguments_4 <- c(                        'YEAR','NEW_MODE'                                      )
  group_by_arguments_5 <- c(                        'YEAR',           'NEW_MODEN', 'Wgt','SE','Trp','Fish' )
  group_by_arguments_6 <- c(                        'YEAR'                                                 )
  group_by_arguments_7 <- c(                        'YEAR',                        'Wgt','SE','Trp','Fish' )
  
  
  if( any( add.strata != 'none' | exists('add.strata') ) ) {
    
    if( 'SID' %in% add.strata ) {
      ###     ...wherein I want to place the 'SID' strata above NEW_STA (which is right after 'YEAR'). All other strata
      ###         ( in 'add.strata' ) are simply concatenated to the end of each 'group_by_arguments' object...
      
      group_by_arguments_1 = append( group_by_arguments_1, 'SID', after = which( 'YEAR' == group_by_arguments_1 ) )
      group_by_arguments_2 = append( group_by_arguments_2, 'SID', after = which( 'YEAR' == group_by_arguments_2 ) )
      group_by_arguments_3 = append( group_by_arguments_3, 'SID', after = which( 'YEAR' == group_by_arguments_3 ) )
      group_by_arguments_4 = append( group_by_arguments_4, 'SID', after = which( 'YEAR' == group_by_arguments_4 ) )
      group_by_arguments_5 = append( group_by_arguments_5, 'SID', after = which( 'YEAR' == group_by_arguments_5 ) )
      group_by_arguments_6 = append( group_by_arguments_6, 'SID', after = which( 'YEAR' == group_by_arguments_6 ) )
      group_by_arguments_7 = append( group_by_arguments_7, 'SID', after = which( 'YEAR' == group_by_arguments_7 ) )
      
      if( length(add.strata) > 1 ) {
        
        blah = add.strata[ add.strata != 'SID' ]
        group_by_arguments_1 = c( group_by_arguments_1, blah )
        group_by_arguments_2 = c( group_by_arguments_2, blah )
        group_by_arguments_3 = c( group_by_arguments_3, blah )
        group_by_arguments_4 = c( group_by_arguments_4, blah )
        group_by_arguments_5 = append( group_by_arguments_5, blah, after = which( 'Wgt' == group_by_arguments_5 )-1 )
        group_by_arguments_6 = c( group_by_arguments_6, blah )
        group_by_arguments_7 = append( group_by_arguments_7, blah, after = which( 'Wgt' == group_by_arguments_7 )-1 )
        rm(blah)
      }
    }
  }
  
  
  
  ### Raw Data Table ###
  ### ------------------
  ###
  ###     ...for which the preference is to convert the original (fish size) table into a trip-level summary,
  ###       from which mean and standard errors will be calculated. Estimates were originally calculated at the
  ###       FISH level (i.e., no trip-level summary), but the resultant CVs were too precise and didn't account
  ###       for correlation in fish size within a given trip. In particular, uncertainty estimates (for average weights)
  ###       are being calculated from standard errors ( SE = stdev / sqrt(N) ), which decreases with increasing sample size
  ###       wherein trip-level summaries ( N = #trips ) tended to produce larger standard errors than those calculated from
  ###       the raw data ( N = #fish ). This was important from an assessment perspective as the uncertainties being calculated
  ###       from fish-level summaries tended to be too precise to be useful in the assessment model fitting process ( SE ~ 0.02 ).
  ###       Note that when sample sizes are inadequate, calculations will revert back to the original approach
  ###       wherein mean and standard erros are calculated at the observation level (i.e., N = #fish ).
  ###
  ###   As such, there are two possible ways in which the raw data table may be constructed in this code (below)...
  
  
  ### Trip-Level Summary of Raw Size Data ###
  ###
  if( data == 'trip' ) {
    
    avgwgt.table = genrec.table %>%
      group_by_at( group_by_arguments_1 ) %>%
      ###   ...wherein this 'group_by_arguments' object includes the unique trip identifier (MY_ID_CODE) and all survey strata:
      ###           -- c( 'MY_ID_CODE','SUB_REG','YEAR','NEW_STA','FL_REG','NC_REG','NEW_MODE','NEW_MODEN','WAVE','NEW_AREAN' )
      ###       the latter of which is needed in subsequent parts of the code. Note that the only strata not retained is species
      ###       (e.g., NEW_COM ), which is excluded because (size) data from different species should not be separated
      ###       (e.g., scamp & YMG were treated as the 'same' species in SEDAR 68 )...
      summarize( N   = length( all_lbs[ !is.na(all_lbs) ] ),
                 Avg = mean( as.numeric(all_lbs), na.rm=TRUE ),
                 SD  =   sd( as.numeric(all_lbs), na.rm=TRUE ) ) %>%
      ### Note that, although calculated here, the standard deviation of fish sizes (within a trip) are not actually used
      ### in this approach. Its simply included here in case this summary is something the GenRec analyst wants to look at...
      
      filter( N > 0 & !is.na(Avg) ) %>%
      ungroup()
    
    
    # ###   ...where, to aid in decisions on whether the (size) data is adequate to inform variance/CV calculations at the
    # ###     trip-level, we can investigate the number of strata from which a single size measurement was collected...
    # strata.n1 = subset( avgwgt.table, N==1 )
    # prop.n1   = nrow(strata.n1) / nrow(avgwgt.table)
    
  }
  
  ### Formatting the Original (Fish-Level) Size Data ###
  ###
  if( data == 'fish' ) {
    
    avgwgt.table = genrec.table %>%
      mutate( N   = 1,
              Avg = all_lbs ) %>%
      select( all_of( c(group_by_arguments_1,'N','Avg') ) ) %>%
      ###   ...which uses the same 'group_by_arguments' statement as that needed for the trip-level summary above
      ###           -- c( 'MY_ID_CODE','SUB_REG','YEAR','NEW_STA','FL_REG','NC_REG','NEW_MODE','NEW_MODEN','WAVE','NEW_AREAN' )
      ###     However, there are no summaries being calculated for the fish-level data, we're simply formatting the table
      ###     to include the fields needed in subsequent parts of the code. Therefore, I simply define the numeric fields
      ###     that we need below ( N=1 as each record represents one fish and 'Avg' set equal to the raw pounds value ),
      ###     and make sure these numbers are retained (i.e., added to the select statement).
      
      filter( N > 0 & !is.na(Avg) )

  }
  
  rm( group_by_arguments_1 )
  
  
  
  ### Estimation of Mean and SEs ###
  ### ------------------------------
  ###
  if( estimation == 'RYSMWA' ) {
    
    
    ### FINEST POSSIBLE RESOLUTION ###
    ###
    ###       ...which is the preferred approach (i.e., use when sample sizes are adequate )...
    
    dummy = avgwgt.table %>%
      group_by_at( group_by_arguments_2 ) %>%
      ###     ...wherein this 'group_by_arguments' object includes all survey strata (rysmwa):
      ###           -- c( 'SUB_REG','YEAR','NEW_STA','FL_REG','NC_REG','NEW_MODE','NEW_MODEN','WAVE','NEW_AREAN' )
      summarize( Fish = sum( N, na.rm=TRUE ),
                 Trip = length( unique( MY_ID_CODE[ !is.na(MY_ID_CODE) ] ) ),
                 Wgt  = mean( as.numeric(Avg), na.rm=TRUE ),
                 Var  =  var( as.numeric(Avg), na.rm=TRUE ) ) %>%
      ungroup()
    
    
    avgwgt.mode = dummy %>%
      group_by_at( group_by_arguments_3 ) %>%
      ###   ...which are then combined (summed) to provide summaries at the required year-mode level:
      ###           -- c( 'YEAR','NEW_MODE','NEW_MODEN' )
      summarize( Fish = sum( Fish, na.rm=TRUE ),
                 Trp  = sum( Trip, na.rm=TRUE ),
                 Wgt  = weighted.mean( Wgt, Trip ),            ### Weighted Mean ###
                 # Wgt_check = sum(Wgt*Trip)/sum(Trip),        ###    ...check that weighted.mean() is properly calculated
                 
                 ### Unweighted Variance ###
                 Var  = sum( Var , na.rm=TRUE )
                 
                 # ### Weighted Variance ###
                 # Var = sum( Trip * ( ( Wgt - ( sum(Wgt*Trip)/sum(Trip) ) )^2 ) ) / ( sum(Trip)-1 )
                 # ###    ...which is the variance of a weighted sample when weights are equal to number of occurrences
                 # ###        (i.e., 'frequency weights' ) ( https://en.wikipedia.org/wiki/Weighted_arithmetic_mean )...
                 
                 ###  Although the weighted variance is probably the more statistically defensible approach,
                 ###  it also tends to equal zero in most strata and so the unweighted variance is being used...
              ) %>%
      
      mutate( Var = ifelse( Var==0 & Wgt>0 & Trp==1, NA, Var ) )
    
    
    avgwgt.year <- dummy %>%
      group_by_at( group_by_arguments_6 ) %>%
      ###     ...wherein this 'group_by_arguments' object includes only year:
      ###             -- c( 'YEAR' )
      summarize( Fish = sum( Fish, na.rm=TRUE ),
                 Trp  = sum( Trip, na.rm=TRUE ),
                 Wgt  = weighted.mean( Wgt, Trip ),
                 Var  = sum( Var , na.rm=TRUE ) ) %>%
      mutate( Var = ifelse( Var==0 & Wgt>0 & Trp==1, NA, Var ) )
    
    
    rm( dummy )
    
    
  } else if( estimation == 'YM' ) {
    
    
    ### YEAR-MODE RESOLUTION ###
    
    avgwgt.mode = avgwgt.table %>%
      
      group_by_at( group_by_arguments_3 ) %>%
      ###     ...wherein this 'group_by_arguments' object includes only year and mode...
      ###           -- c( 'YEAR','NEW_MODE','NEW_MODEN' )
      
      summarize( Fish = sum( N, na.rm=TRUE ),
                 Trip = length( unique( MY_ID_CODE[ !is.na(MY_ID_CODE) ] ) ),
                 Wgt  = mean( as.numeric(Avg), na.rm=TRUE ),
                 Var  =  var( as.numeric(Avg), na.rm=TRUE ) ) %>%
      ###   Note that the weighted.mean() function is not required here as summaries statistics are calculated
      ###   directly from the 'raw' size data (whehter that be fish-level observations or trip-level summaries),
      ###   and not from a previous summary of fish sizes (e.g., at the RYSMWA level )...
      
      rename( Trp = Trip )
    
    
    avgwgt.year = avgwgt.table %>%
      
      group_by_at( group_by_arguments_6 ) %>%
      ###     ...wherein this 'group_by_arguments' object includes only year:
      ###             -- c( 'YEAR' )
      
      summarize( Fish = sum( N, na.rm=TRUE ),
                 Trip = length( unique( MY_ID_CODE[ !is.na(MY_ID_CODE) ] ) ),
                 Wgt  = mean( as.numeric(Avg), na.rm=TRUE ),
                 Var  =  var( as.numeric(Avg), na.rm=TRUE ) ) %>%
      rename( Trp = Trip )
    
    
  } else {
    
    stop( 'Desired Stratification not defined in Function' )
    
  }
  
  rm( group_by_arguments_2, group_by_arguments_3 )
  
  
  ###   ...and identifying those modes in the avgwgt (CV) table, as will be needed in final sorting/formatting...
  modes = c( unique( gsub( '.*_','', avgwgt.mode$NEW_MODEN ) ), 'Total' )
  
  
  ### CONVERTING VARIANCES TO STD.ERRORS AND CVs ###
  
  if( data == 'trip' ) {
    
    avgwgt.mode = avgwgt.mode %>% mutate( SE = sqrt(Var) / sqrt(Trp) )
    avgwgt.year = avgwgt.year %>% mutate( SE = sqrt(Var) / sqrt(Trp) )
  }
  if( data == 'fish' ) {
    
    avgwgt.mode = avgwgt.mode %>% mutate( SE = sqrt(Var) / sqrt(Fish) )
    avgwgt.year = avgwgt.year %>% mutate( SE = sqrt(Var) / sqrt(Fish) )
  }
  
  
  avgwgt.mode = avgwgt.mode %>%
    
    mutate( CV = SE / Wgt ) %>%
    ungroup() %>%
    
    arrange_at( group_by_arguments_4 ) %>%
    ###     ...wherein this 'group_by_arguments' object includes only year and the numerical mode field ('NEW_MODE'):
    ###           -- c( 'YEAR','NEW_MODE' )
    select_at(  group_by_arguments_5 ) %>%
    ###     ...and this 'group_by_arguments' object includes year and the factor mode field ('NEW_MODEN'),
    ###            as well as the size summaries estimated in this function (i.e., Wgt,SE,Trp,Fish ):
    ###           -- c( 'YEAR', 'NEW_MODEN', 'Wgt','SE','Trp','Fish' )
    pivot_wider( names_from=NEW_MODEN, values_from=c( Wgt,SE,Trp,Fish ) )
  
  rm( group_by_arguments_4, group_by_arguments_5 )
  
  
  avgwgt.year <- avgwgt.year %>%
    
    mutate( CV = SE / Wgt ) %>%
    ungroup() %>%
    
    arrange_at( group_by_arguments_6 ) %>%
    ###     ...where this 'group_by_arguments' object includes only year:
    ###           -- c( 'YEAR' )
    select_at(  group_by_arguments_7 ) %>%
    ###     ...and this 'group_by_arguments' object includes year and the calculated size summaries (i.e., Wgt,SE,Trp,Fish ):
    ###           -- c( 'YEAR','Wgt','SE','Trp','Fish' )
    rename(   Wgt_Total = Wgt,
               SE_Total = SE,
              Trp_Total = Trp,
             Fish_Total = Fish )
  
  rm( group_by_arguments_6, group_by_arguments_7 )
  
  
  
  ### JOINING THE YEAR-MODE SUMMARY WITH THE TOTAL (ANNUAL) SUMMARY ###
  
  if( all( add.strata != 'none' ) ) {
    avgwgt.table <- full_join( avgwgt.mode, avgwgt.year, by=c("YEAR",add.strata) )
  } else {
    avgwgt.table <- full_join( avgwgt.mode, avgwgt.year, by="YEAR" )
  }
  
  rm( avgwgt.mode, avgwgt.year )
  
  
  
  
  ### ---------------------------------------------------------------------------------------------------------
  ### FINAL FORMATTING ###
  
  
  dummy = avgwgt.table %>%
    mutate_at( vars(contains(c('Fish','Trp'))), list( ~ as.character( format( round(.,0), big.mark=',' ) ) ) ) %>%
    mutate_at( vars(contains(c('Fish','Trp'))), list( ~ ifelse( grepl('NA',.), NA, . ) ) )
  
  
  # ### COMBINING the 'Fish' and 'Trp' fields into a SINGLE COLUMN (i.e., = Trp (Fish) )
  #
  # for( i in 1:length(modes) ) {
  #   dummy <- unite( dummy, newcol,
  #                          c( paste0("Trp_",modes[i]),paste0("Fish_",modes[i]) ), sep=" (", remove=TRUE )
  #   dummy$newcol <- paste0( dummy$newcol,")" )
  #   colnames(dummy)[ which( colnames(dummy) == "newcol" ) ] <- paste0("N_",modes[i])
  #   
  #   dummy[ dummy[ colnames(dummy)==paste0("N_",modes[i]) ] == "NA (NA)" , paste0("N_",modes[i]) ] = NA
  # }
  # 
  # avgwgt.table = dummy
  # rm(dummy)
  
  
  
  ### RENAME & REORDER COLUMNS ###
  
  avgwgt.table = avgwgt.table %>% rename_all( toupper )
  
  loc = !( colnames(avgwgt.table) %in% c('YEAR',toupper(add.strata)) )
  colnames(avgwgt.table)[loc] = paste0( gsub( ".*_","", colnames(avgwgt.table)[loc] ),
                                    "_",gsub( "_.*","", colnames(avgwgt.table)[loc] ) )
  ###     ...renaming the columns in 'avgwgt.table', which follow a variable-mode format, into a mode-variable format...
  rm(loc)
  
  
  avgwgt.cols <- vector()
  
  col.IDs <- toupper(modes)
  col.IDs = col.IDs[ order( match(col.IDs,c("CBT","CBTHBT","HBT","PRIV","PRIVSHORE","SHORE","TOTAL")) ) ]
  
  for( i in 1:length(col.IDs) ) {
    # avgwgt.cols <- c( avgwgt.cols, paste0( col.IDs[i], c("_WGT","_SE","_N") ) )
    avgwgt.cols <- c( avgwgt.cols, paste0( col.IDs[i], c("_WGT","_SE","_FISH","_TRP") ) )
  }
  
  if( all( add.strata != 'none' ) ) {
    group_by_arguments <- c( 'YEAR',toupper(add.strata), avgwgt.cols )
  } else {
    group_by_arguments <- c( 'YEAR',avgwgt.cols )
  }
  
  avgwgt.table <- avgwgt.table %>% select_at( group_by_arguments )
  
  rm( col.IDs, avgwgt.cols, modes, group_by_arguments )
  
  
  
  ### ENSURE TABLE INCLUDES ALL YEARS ###
  ###     ...and all combinations of year with any of the additional strata fields (i.e., in 'add.strata' )...
  
  if( all( add.strata != 'none' ) ) {
    
    ###   ...creating vectors for all the unique values of each variable...
    Yr <- seq( first.year, term.year, by=1 )
    for( i in 1:length(add.strata) ) {
      command.line = paste0( add.strata[i],' = unique( genrec.table$',add.strata[i],' )' )
      eval( parse( text = command.line ) )
      rm( command.line )
    }
    
    ###   ...identifying all permutations of these variables, and saving in the 'dummy' dataframe...
    command.line = paste0( 'dummy = list( Yr, ',paste( add.strata, collapse=', ' ),' )' )
    eval( parse( text = command.line ) )
    rm( command.line )
    dummy = do.call( expand.grid, dummy )
    colnames(dummy) = c( 'YEAR', toupper(add.strata) )
    
    command.line = paste0( 'rm( Yr, ',paste( add.strata, collapse=', ' ),' )' )
    eval( parse( text = command.line ) )
    rm( command.line )
    
    avgwgt.table = avgwgt.table %>%
      full_join( dummy, by=c( toupper(add.strata),"YEAR" ) ) %>%
      arrange(!!! rlang::syms( c(toupper(add.strata),"YEAR") ) )
    
    
  } else {
    dummy = data.frame( YEAR = seq( first.year, term.year, by=1 ) )
    avgwgt.table = dummy %>%
      full_join( avgwgt.table, by="YEAR" ) %>%
      arrange( YEAR )
  }
  rm(dummy)
  
  
  
  return( avgwgt.table )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------


convert.long.table.avgwt = function( avgwgt.table, possible.strata = 'none' ) {
  
  ###     ...where 'avgwgt.table' is a table of recreational estimates ( avgwgt, CV, & sample sizes )
  ###         currently in wide-format but to be converted into long-format by this function.
  ###         The format of this long table will (possibly) include columns:
  ### #             SID, <other 'possible.strata'> , YEAR, NEW_MODE, NEW_MODEN,   WGT, SE, FISH, TRP
  
  dummy = avgwgt.table %>%
    pivot_longer( -any_of( c('YEAR',toupper(possible.strata)) ), names_to = 'VARIABLE', values_to = 'value' ) %>%
    
    mutate( NEW_MODEN = gsub( "_.*","", VARIABLE ) ) %>%
    mutate( VARIABLE  = str_remove( VARIABLE, NEW_MODEN ) ) %>%
    
    mutate( METRIC = ifelse( grepl(  'WGT', VARIABLE ),  'WGT',
                     ifelse( grepl(   'SE', VARIABLE ),   'SE',
                     ifelse( grepl( 'FISH', VARIABLE ), 'FISH',
                     ifelse( grepl(  'TRP', VARIABLE ),  'TRP', NA )))) ) %>%
    select( -VARIABLE ) %>%
    ###   ...where 'METRIC' is formatted as a factor to control the order with which values are to be displayed...
    mutate( METRIC = factor( METRIC, levels = c('WGT','SE','FISH','TRP') ) ) %>%
    
    ###   ...and, lastly, to which I add a numeric NEW_MODE field so that estimates are properly sorted in
    ###       the pivot table ( of the final GenRec size file )...
    mutate( NEW_MODE = ifelse( NEW_MODEN ==     'SHORE', 1,
                       ifelse( NEW_MODEN ==       'HBT', 2,
                       ifelse( NEW_MODEN ==       'CBT', 3,
                       ifelse( NEW_MODEN ==      'PRIV', 4,
                       ifelse( NEW_MODEN ==    'CBTHBT', 5,
                       ifelse( NEW_MODEN == 'PRIVSHORE', 6,
                       ifelse( NEW_MODEN ==     'TOTAL', 99, NA ))))))) )
  
  if( all( possible.strata != 'none' ) ) {
    dummy = dummy %>%
      select( any_of( c( toupper(possible.strata),"YEAR","NEW_MODE","NEW_MODEN","METRIC","value" ) ) ) %>%
      arrange(!!! rlang::syms( c(toupper(possible.strata),"YEAR","NEW_MODE") ) )
    
  } else {
    dummy = dummy %>%
      select(  YEAR, NEW_MODE, NEW_MODEN, METRIC, value ) %>%
      arrange( YEAR, NEW_MODE )
  }
  
  
  avgwgt.table = dummy
  rm(dummy)
  
  
  return( avgwgt.table )
  
}




