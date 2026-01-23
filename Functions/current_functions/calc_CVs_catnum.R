

### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------
###
###     DESCRIPTION
###
###   ...where the functions below estimate uncertainties for the catch-in-number estimates (AB1 & B2) of
###       our various general recreational surveys...
###
###
###    *** CVs.catnum.MRIP( )
###           ...which pulls MRIP catch, CV estimates, and associated sample size info from pre-constructed RDI reports.
###             As described in S68-DW-10 and S68-DW-31, these uncertainties are calculated using standard survey methodology
###             and directly from the raw data to ensure estimates are appropriate for custom (assessment-specific)
###             data aggregations. Note that additional adjustments may be needed to the RDI reports depending on whether
###             modifications were made to the original 'catch.table' (e.g., partitioning the combined for-hire,
###             imputing 'missing' estimates ), which are carried out in the function below using flags...
###         Note that CVs for Caribbean assessments are calculated differently, using the "old" method by which the
###         the MRIP-provided "var_ab1" and "var_b2" fields are summed up (at the strata level), square-rooted, and
###         divided by sum(AB1) and sum(B2) respectively. These steps are also automated in this function...
###
###    *** CVs.catnum.TPWD( )
###           ...which TPWD catch, CV estimates, and associated sample size info from pre-constructed RDI views.
###             These uncertainties are calculated from the native 'variance' estimates provided by the TPWD survey
###             (e.g., summed over strata ), some of which may be modified if additional adjustments were made to the
###             original 'catch.table' (e.g., imputing 1981-1983 estimates, imputing TPWD discards )...
###
###    *** CVs.catnum.LACR( )
###           ...which pulls LACR catch, CV estimates, and associated sample size info from pre-constructed RDI views.
###             These uncertainties are calculated from the native 'variance' estimates provided by the LACR survey
###             (e.g., summed over strata ), some of which may be modified if additional adjustments were made to the
###             original 'catch.table' (e.g., imputing LACR discards )...
###
###     *** convert.long.table.cat()
###           ...which converts a GenRec catch-CV table from wide-format, with fields that (can) include
###                       YEAR, PRIV_AB1, CBT_AB1,... PRIV_CV_AB1, CBT_CV_AB1,... PRIV_PSU_AB1,... PRIV_AT_AB1,... etc
###             into long-format, with five fields = YEAR, NEW_MODEN, CATCH.VAR (AB1,B2,TOTAL), METRIC (CAT,CV,PSU,AT), and value
###
###
###   Note that these functions are set-up to handle stocks with multiple StockID boundaries. For MRIP tables,
###   multiple tables are imported (from the RDI CV report tool) for stocks with multiple SID boundaries.
###   For TPWD and LACR tables, all that is needed to to combine these 'state' tables with the appropriate MRIP table,
###   which is done outside of these functions (in the main 'catch' code)...
###
### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------

CVs.catnum.MRIP = function( rdi.report, report.type = c("annual","detailed"), inc.modes, genrec.table = NA,
                            imp.1981w1 = 'None',
                            flag.unid    = FALSE,  catch.table.unid    = NA,
                            flag.forhire = FALSE,  catch.table.forhire = NA,   loc.cv.forhire = NA,
                            loc.FH.ratios = 'C://Users/matthew.nuttall/Desktop/Functions/import_datasets/ForHire Partitioning Ratios.csv',
                            Carib.SEDAR = FALSE, total.trips = NA, pos.trips = NA ) {
  
  ###     ...where 'rdi.report' identifies the report to which the catch & CV estimates have been saved.
  ###             Note that for assessments with multiple SID boundaries, 'rdi.report' will be composed of multiple elements,
  ###             one report for each SID domain ( that should be ordered geographically, as arranged using NEW_ST ),
  ###       'report.type' identifies the type of report built by RDI (i.e., "annual" provides CVs by year, year-mode, or year-state
  ###             while "detailed" provides CVs at a much finer resolution, namely by year-mode-state-wave ),
  ###       'inc.modes' the modes for which estimates are needed (for this particular assessment),
  ###
  ###       'genrec.table' -- table of catch estimates being constructed by the main script, which is used to calculate CVs for SEDARs:
  ###               (1) ...in the Caribbean ( 'Carib_SEDAR' == TRUE ), for which uncertainties are estimated from the
  ###                   'var_ab1' and 'var_b2' fields native to the MRIP survey. Note that the CVs for Caribbean SEDARs
  ###                   may be calculated from a different 'catch.table' than that providing the primary estimates
  ###                   for the assessment (i.e., if a different subset of species is considered ),
  ###               (2) ...where 1981-wave1 MRIP estimates ( for GOM+FLE ) are imputed
  ###                   ( 'imp.1981w1' != 'None' ), for which we extract the total 1981 MRIP estimate ( in 'genrec.table' )
  ###                   that includes any wave1 estimates imputed by the impute.MRIP.1981w1() function,
  ###
  ###       'imp.1981w1' acts as a flag for SEDARs where 1981-wave1 (MRIP) catch estimates were imputed,
  ###
  ###       'flag.unid' is a flag for SEDARs where we assign some percent of unidentified MRIP catch to the assessed species,
  ###       'catch.table.unid' is a previous iteration of our GenRec catch estimates table, constructed immediately after
  ###           some fraction of catch ( from an unidentified taxa ) was allocated to the species-of-interest, but before
  ###           any other adjustments were made (i.e., before breaking CBTHBT, imputing MRIP 1981-wave1, etc. ).
  ###           This table is needed to calculate CVs for SEDARs where 'flag.unid' = TRUE. In particular, catch estimates
  ###           originally recorded as the unidentified taxa (i.e., 'UNID_FLAG' = Y ) need to be added to those for the
  ###           the species-of-interest (i.e., RDI CV estimates are calculated from data specific to the species-of-interest ),
  ###
  ###       'flag.forhire' is as flag for SEDARs where we partitioned any MRIP estimates for the combined for-hire mode.
  ###             In these SEDARs, calculating the associated CVs for these partitioned (Cbt & Hbt) estimates requires...
  ###       'catch.table.forhire' is a previous iteration of our GenRec catch estimates table, constructed immediately before
  ###             MRIP estimates for the combined forhire mode (MATL/NATL, 1981-2003) were partitioned between CBT & HBT.
  ###             Similar to 'catch.table.unid' above, this table is needed to calculate CVs where 'flag.forhire' = TRUE,
  ###             in that the (original) catch estimates for the combined for-hire mode need to be used...
  ###       'loc.cv.forhire' -- identifies the position in 'rdi.report' (e.g., = 1 or 2 or 3... ) of the table containing
  ###             estimates for the combined forhire mode, which is only applicable to assessments with multiple SID domains,
  ###       'loc.FH.ratios' identifies the location of the spreadsheet of forhire partitioning ratios and their associated variances...
  ###
  ###       Lastly, for Caribbean SEDARs, for which CVs are calculated from the variances provided by OST...
  ###           -- 'Carib.SEDAR' is a flag to identify CVs being estimated for a Caribbean SEDAR, which start from the
  ###                   'catch.table' object that is imported (into the function) as 'genrec.table'...
  ###           -- 'total.trips' is a table providing the number of TOTAL trips, and
  ###           -- 'pos.trips' is a table providing the number of POSITIVE trips
  ###       Note that these calculations shouldn't be needed anymore as the Caribbean branch now does their own data pulls,
  ###       but the method is retained for historical tracking (or if its needed for another application in the future).
  ###       Therefore, the 'Carib.SEDAR' object is set to FALSE as a default, so that these argument can largely be ignored when
  ###       calling the function. Similarly, the two (number of trip) tables default to <NA> and are only called when needed...
  
  
  
  ### GOM & ATL SEDARs ###
  ### --------------------
  ###
  if( !Carib.SEDAR ) {
    
    ### Defining the columns (from this query) that I want to extract...
    if( report.type == "annual" ) {
      
      cols.extract <- c( "t.YEAR" )
      cols.extract <- c( cols.extract, paste0( "t.",toupper(inc.modes),"_AB1" ) )
      cols.extract <- c( cols.extract, paste0( "t.",toupper(inc.modes),"_CV_AB1" ) )
      
      cols.extract <- c( cols.extract, paste0( "t.",toupper(inc.modes),"_B2" ) )
      cols.extract <- c( cols.extract, paste0( "t.",toupper(inc.modes),"_CV_B2" ) )
      
      ### PSU columns ###
      cols.extract <- c( cols.extract, paste0( "t.",toupper(inc.modes),"_N" ) )
      cols.extract <- c( cols.extract, paste0( "t.",toupper(inc.modes),"_N_B2" ) )
      cols.extract <- c( cols.extract, paste0( "t.",toupper(inc.modes),"_SZ" ) )
      
      ### AT columns ###
      cols.extract <- c( cols.extract, paste0( "t.",toupper(inc.modes),"_SZ_AT" ) )
      cols.extract <- c( cols.extract, paste0( "t.",toupper(inc.modes),"_SZ_B2_AT" ) )
      cols.extract <- c( cols.extract, paste0( "t.T",toupper(inc.modes),"_SZ_AT" ) )
      
    } else if( report.type == "detailed" ) {
      cols.extract <- c( "t.YEAR", "t.WAVE", "t.NEW_ST", "NEW_MODEN" )
      cols.extract <- c( cols.extract, "AB1", "CV_AB1", "SZ", "B2", "CV_B2", "SZ_MODE",
                                       "SZ_AB1", "SZ_B2", "SZ_AB1_AT", "SZ_B2_AT", "SZ_AT" )
    }
    
    
    ### Assessments with no SID domains ###
    ### -----------------------------------
    ###     ...wherein the entire population is considered one stock and so I only import one (region-wide) summary table...
    ###
    if( length(rdi.report) == 1 ) {
      
      
      con = dbConnect(dbDriver("Oracle"), username = keyring::key_list("SECPR")[1,2],
                      password = keyring::key_get("SECPR", keyring::key_list("SECPR")[1,2]), dbname = "SECPR")
      
      ### RDI Pull of MRIP CVs (by-mode)...
      ### ---------------------------------
      
      # cols.extract <- paste0( cols.extract, collapse=", " )
      # mrip.mode = dbGetQuery(con,
      #                        paste0("select ", cols.extract, " ",
      #                               "from rdi.apex_cv_data_yr_m@secapxdv_dblk.sfsc.noaa.gov t
      #                           where t.APP_USER = ", sprintf("'%s'", paste( rdi.report, collapse = "','" ))
      #                        ))
      
      if( report.type == "annual" ) {
        
        mrip.mode = dbGetQuery(con,
                               paste0("select * ",
                                      "from rdi.apex_cv_data_yr_m@secapxdv_dblk.sfsc.noaa.gov t ",
                                      "where t.APP_USER = ", sprintf("'%s'", paste( rdi.report, collapse = "','" ))
                               ))
        mrip.mode = mrip.mode[ , which( colnames(mrip.mode) %in% gsub( "t.","", cols.extract ) ) ]
        
        ###     I then ensure rows are sorted by year...
        # mrip.mode = mrip.mode[ order(mrip.mode$YEAR), ]
        mrip.mode = mrip.mode %>% arrange( across( any_of( c("YEAR") ) ) )
        
        ###         ...and rename my sample size columns to better reflect their meaning. Starting with PSU...
        replace.string = paste0( toupper(inc.modes),"_N" )
        colnames(mrip.mode)[ colnames(mrip.mode) %in% replace.string ] = paste0( toupper(inc.modes),"_PSU_AB1" )
        replace.string = paste0( toupper(inc.modes),"_N_B2" )
        colnames(mrip.mode)[ colnames(mrip.mode) %in% replace.string ] = paste0( toupper(inc.modes),"_PSU_B2" )
        replace.string = paste0( toupper(inc.modes),"_SZ" )
        colnames(mrip.mode)[ colnames(mrip.mode) %in% replace.string ] = paste0( toupper(inc.modes),"_PSU_Tot" )
        
        ###         ...and finishing with angler trips...
        replace.string = paste0( toupper(inc.modes),"_SZ_AT" )
        colnames(mrip.mode)[ colnames(mrip.mode) %in% replace.string ] = paste0( toupper(inc.modes),"_AT_AB1" )
        replace.string = paste0( toupper(inc.modes),"_SZ_B2_AT" )
        colnames(mrip.mode)[ colnames(mrip.mode) %in% replace.string ] = paste0( toupper(inc.modes),"_AT_B2" )
        replace.string = paste0( "T",toupper(inc.modes),"_SZ_AT" )
        colnames(mrip.mode)[ colnames(mrip.mode) %in% replace.string ] = paste0( toupper(inc.modes),"_AT_Tot" )
        
        
        ### RDI Pull of MRIP CVs (TOTAL = by-year)
        ###   ...and, for those data pulls that are not looking at CVs at the finest possible resolution
        ###       (i.e., detailed CV reports provided by year/wave/state/mode ), we also add a 'TOTAL' column
        ###       that gives CVs at an annual level (i.e., that collapses across mode )...
        
        mrip.total = dbGetQuery(con,
                                paste0("select
                                  YEAR, AB1, CV_AB1, B2, CV_B2,     AB1_SZ, B2_SZ, AB1_N,   AB1_ATN, B2_ATN, ATSZ
                                from rdi.apex_cv_data_yr@secapxdv_dblk.sfsc.noaa.gov t
                                where t.APP_USER = ", sprintf("'%s'", paste( rdi.report, collapse = "','" ))
                                ))
        mrip.total = mrip.total[ order(mrip.total$YEAR), ]
        colnames( mrip.total ) <- c( "YEAR", "TOTAL_AB1", "TOTAL_CV_AB1", "TOTAL_B2", "TOTAL_CV_B2",
                                     "TOTAL_PSU_AB1", "TOTAL_PSU_B2", "TOTAL_PSU_Tot",
                                     "TOTAL_AT_AB1", "TOTAL_AT_B2", "TOTAL_AT_Tot" )
        ###       Note that I invert the 'meaning' of the c( "SZ","N" ) cols because they are defined differently
        ###       in the "mrip.mode" & "mrip.total" tables ( as created by RDI )...
        
        
        ### MERGE Tables (MODE+TOTAL)
        cv.table <- full_join( mrip.mode, mrip.total, by="YEAR", suffix=c("","_removeDuplicate") )
        ###     ...where there shouldn't be any duplicate columns in "mrip.mode" and "mrip.total"
        ###                 ( and so no cols with suffix = "_removeDuplicate" )...
        rm( mrip.total )
        
        
      } else if( report.type == "detailed" ) {
        
        mrip.mode = dbGetQuery(con,
                               paste0("select * ",
                                      "from rdi.apex_cv_data_ymsw@secapxdv_dblk.sfsc.noaa.gov t ",
                                      "where t.APP_USER = ", sprintf("'%s'", paste( rdi.report, collapse = "','" ))
                               ))
        mrip.mode = mrip.mode[ , which( colnames(mrip.mode) %in% gsub( "t.","", cols.extract ) ) ]
        
        mrip.mode = mrip.mode %>% arrange( across( any_of( c("YEAR","NEW_MODEN","NEW_ST","WAVE") ) ) )
        
        ###         ...and rename my sample size columns to better reflect their meaning. Starting with PSU...
        colnames(mrip.mode)[ colnames(mrip.mode) == "SZ_AB1"  ] = "PSU_AB1"
        colnames(mrip.mode)[ colnames(mrip.mode) == "SZ_B2"   ] = "PSU_B2"
        colnames(mrip.mode)[ colnames(mrip.mode) == "SZ_MODE" ] = "PSU_Tot"
        
        ###         ...and finishing with angler trips...
        colnames(mrip.mode)[ colnames(mrip.mode) == "SZ_AB1_AT" ] = "AT_AB1"
        colnames(mrip.mode)[ colnames(mrip.mode) == "SZ_B2_AT"  ] = "AT_B2"
        colnames(mrip.mode)[ colnames(mrip.mode) == "SZ_AT"     ] = "AT_Tot"
        
        mrip.mode = mrip.mode %>% select( -c("SZ") )
        
        ### Lastly, I pivot the "NEW_MODEN" field to be included in the column headers...
        mrip.mode = mrip.mode %>%
          mutate( NEW_MODEN = toupper(NEW_MODEN) ) %>%
          pivot_wider( names_from = NEW_MODEN,
                       values_from = -all_of( c("YEAR","NEW_MODEN","NEW_ST","WAVE") ),
                       names_glue = "{NEW_MODEN}_{.value}" )
        
        ###   ...and add the NEW_STA field to 'mrip.mode'...
        st_tab = dbGetQuery( con, "SELECT * FROM RDI.MRIP_STATE_CODES@secapxdv_dblk.sfsc.noaa.gov" )
        mrip.mode = mrip.mode %>% left_join( st_tab %>% select(-ST), by = "NEW_ST" )
        
        mrip.mode = mrip.mode %>% select( -NEW_ST )
        
        cv.table = mrip.mode

      }
      rm( mrip.mode )
      
      
      ###       ...and a little formatting...
      cv.table[ is.na(cv.table) ] = 0
      
      
      
    ### Assessments with SID domains ###
    ### --------------------------------
    ###     ...wherein multiple (CV) summary tables are imported from RDI, one for each SID domain...
    ###
    } else if( length(rdi.report) > 1 ) {
      
      
      cv.table = vector( mode="list", length=length(rdi.report) )
      
      
      for( j in 1:length(rdi.report) ) {
        
        
        con = dbConnect(dbDriver("Oracle"), username = keyring::key_list("SECPR")[1,2],
                        password = keyring::key_get("SECPR", keyring::key_list("SECPR")[1,2]), dbname = "SECPR")
        
        
        # cols.extract <- paste0( cols.extract, collapse=", " )
        # mrip.mode = dbGetQuery(con,
        #                        paste0("select ", cols.extract, " ",
        #                               "from rdi.apex_cv_data_yr_m@secapxdv_dblk.sfsc.noaa.gov t
        #                       where t.APP_USER = ", sprintf("'%s'", paste( rdi.report[j], collapse = "','" ))
        #                        ))
        
        if( report.type == "annual" ) {
          
          mrip.mode = dbGetQuery(con,
                                 paste0("select * ",
                                        "from rdi.apex_cv_data_yr_m@secapxdv_dblk.sfsc.noaa.gov t ",
                                        "where t.APP_USER = ", sprintf("'%s'", paste( rdi.report[j], collapse = "','" ))
                                 ))
          mrip.mode = mrip.mode[ , which( colnames(mrip.mode) %in% gsub( "t.","", cols.extract ) ) ]
          
          ###     I then ensure rows are sorted by year...
          # mrip.mode = mrip.mode[ order(mrip.mode$YEAR), ]
          mrip.mode = mrip.mode %>% arrange( across( any_of( c("YEAR") ) ) )
          
          ###         ...and rename my sample size columns to better reflect their meaning. Starting with PSU...
          replace.string = paste0( toupper(inc.modes),"_N" )
          colnames(mrip.mode)[ colnames(mrip.mode) %in% replace.string ] = paste0( toupper(inc.modes),"_PSU_AB1" )
          replace.string = paste0( toupper(inc.modes),"_N_B2" )
          colnames(mrip.mode)[ colnames(mrip.mode) %in% replace.string ] = paste0( toupper(inc.modes),"_PSU_B2" )
          replace.string = paste0( toupper(inc.modes),"_SZ" )
          colnames(mrip.mode)[ colnames(mrip.mode) %in% replace.string ] = paste0( toupper(inc.modes),"_PSU_Tot" )
          
          ###         ...and finishing with angler trips...
          replace.string = paste0( toupper(inc.modes),"_SZ_AT" )
          colnames(mrip.mode)[ colnames(mrip.mode) %in% replace.string ] = paste0( toupper(inc.modes),"_AT_AB1" )
          replace.string = paste0( toupper(inc.modes),"_SZ_B2_AT" )
          colnames(mrip.mode)[ colnames(mrip.mode) %in% replace.string ] = paste0( toupper(inc.modes),"_AT_B2" )
          replace.string = paste0( "T",toupper(inc.modes),"_SZ_AT" )
          colnames(mrip.mode)[ colnames(mrip.mode) %in% replace.string ] = paste0( toupper(inc.modes),"_AT_Tot" )
          
          
          mrip.total = dbGetQuery(con,
                                  paste0("select 
                                  YEAR, AB1, CV_AB1, B2, CV_B2,     AB1_SZ, B2_SZ, AB1_N,   AB1_ATN, B2_ATN, ATSZ
                                from rdi.apex_cv_data_yr@secapxdv_dblk.sfsc.noaa.gov t
                                where t.APP_USER = ", sprintf("'%s'", paste( rdi.report[j], collapse = "','" ))
                                  ))
          mrip.total = mrip.total[ order(mrip.total$YEAR), ]
          colnames( mrip.total ) <- c( "YEAR", "TOTAL_AB1", "TOTAL_CV_AB1", "TOTAL_B2", "TOTAL_CV_B2",
                                       "TOTAL_PSU_AB1", "TOTAL_PSU_B2", "TOTAL_PSU_Tot",
                                       "TOTAL_AT_AB1", "TOTAL_AT_B2", "TOTAL_AT_Tot" )
          
          cv.dummy <- full_join( mrip.mode, mrip.total, by="YEAR", suffix=c("","_removeDuplicate") )
          rm( mrip.total )
          
          
        } else if( report.type == "detailed" ) {
          mrip.mode = dbGetQuery(con,
                                 paste0("select * ",
                                        "from rdi.apex_cv_data_ymsw@secapxdv_dblk.sfsc.noaa.gov t ",
                                        "where t.APP_USER = ", sprintf("'%s'", paste( rdi.report[j], collapse = "','" ))
                                 ))
          mrip.mode = mrip.mode[ , which( colnames(mrip.mode) %in% gsub( "t.","", cols.extract ) ) ]
          
          mrip.mode = mrip.mode %>% arrange( across( any_of( c("YEAR","NEW_MODEN","NEW_ST","WAVE") ) ) )
          
          ###         ...and rename my sample size columns to better reflect their meaning. Starting with PSU...
          colnames(mrip.mode)[ colnames(mrip.mode) == "SZ_AB1"  ] = "PSU_AB1"
          colnames(mrip.mode)[ colnames(mrip.mode) == "SZ_B2"   ] = "PSU_B2"
          colnames(mrip.mode)[ colnames(mrip.mode) == "SZ_MODE" ] = "PSU_Tot"
          
          ###         ...and finishing with angler trips...
          colnames(mrip.mode)[ colnames(mrip.mode) == "SZ_AB1_AT" ] = "AT_AB1"
          colnames(mrip.mode)[ colnames(mrip.mode) == "SZ_B2_AT"  ] = "AT_B2"
          colnames(mrip.mode)[ colnames(mrip.mode) == "SZ_AT"     ] = "AT_Tot"
          
          st_tab = dbGetQuery( con, "SELECT * FROM RDI.MRIP_STATE_CODES@secapxdv_dblk.sfsc.noaa.gov" )
          mrip.mode = mrip.mode %>% left_join( st_tab %>% select(-ST), by = "NEW_ST" )
          
          cv.dummy = mrip.mode
          
        }
        rm( mrip.mode )
        
        
        cv.dummy[ is.na(cv.dummy) ] = 0
        
        cv.table[[j]] = cv.dummy
        rm( cv.dummy )
        
      }
      rm( j )
      
    }
    
    rm( cols.extract )
    
    
    
    
    
    ### Estimates of UNID Taxa ###
    ### --------------------------
    ### 
    ###     ...where the above CV report(s) do not include any catch that has been allocated (to the species-of-interest)
    ###       from an unidentified taxa, which is by design as there is no way to identify which intercepts of the
    ###       unidentified taxa should be included in those for the species-of-interest (and so CV calculations from the
    ###       raw intercept data need to ignore catch records from unidentified groups). Therefore, to match the estimates
    ###       provided in our final 'catch.table' and to ensure all subsequent adjustments (e.g., imputations of MRIP 1981-wave1 )
    ###       include both identified catch and the portion of unidentified catch assumed composed of the assessed species,
    ###       any relevant UNID catch is added back into 'cv.table' at this step...
    ### 
    ###   Note that 'cv.table' (in its current state) has uncertainty estimates expressed as CVs, and so the assumption
    ###   is that the %errors being estimated from intercepts solely for the species-of-interest (in 'cv.table') are also
    ###   representative of those CVs that would be estimated from the combined intercepts of those that observed the
    ###   species-of-interest (i.e., identified ) and those recorded as unidentified but are assumed to belong to the
    ###   assessed species (i.e., unidentified records )...
    
    if( flag.unid ) {
      
      unid.dummy = catch.table.unid %>%
        filter( UNID_FLAG == 'Y' ) %>%
        
        ###     ...where we create a filtered copy of 'catch.table.unid' to only include those catch estimates of the
        ###       unidentified taxa that have been allocated to the species-of-interest. These 'UNID' estimates are
        ###       currently missing from 'cv.table' as CVs are only calculated from 'identified' records, and so we add
        ###       these (unidentified) catches back into 'cv.table' to ensure they match those in the main 'catch.table'...
        
        mutate( NEW_MODEN = toupper(NEW_MODEN) ) %>%
        mutate( NEW_MODEN = ifelse( NEW_MODEN ==    'CBT/HBT',    'CBT_HBT', NEW_MODEN ) ) %>%
        mutate( NEW_MODEN = ifelse( NEW_MODEN == 'PRIV/SHORE', 'PRIV_SHORE', NEW_MODEN ) )
      
      
      if( 'SID' %in% colnames(genrec.table) ) {
        ###   ...wherein to identify which SID domain corresponds to which element in 'cv.table', I reference
        ###     'genrec.table' and sort the SID field according to the NEW_ST, FL_REG, & NC_REG fields;
        ###     ( CV reports, for each SID domain, are sorted geographically in our 'rdi.report' object )...
        
        dummy = genrec.table %>%
          mutate( FL_REG = factor( FL_REG, levels = c(1,2,3,4,5,NA) ) ) %>%
          mutate( NC_REG = factor( NC_REG, levels = c('S','N',NA) ) ) %>%
          arrange( NEW_ST, FL_REG, NC_REG )
        SID.levels = unique( dummy$SID )
        rm(dummy)
      }
      
      
      ### JOIN -- UNID + ID CATCH ###
      
      if( report.type == "annual" ) {
        
        dummy.table = unid.dummy %>%
          group_by( across( any_of( c("SID","YEAR","NEW_MODEN") ) ) ) %>%
          summarize( AB1 = sum( AB1, na.rm=TRUE ),
                     B2  = sum(  B2, na.rm=TRUE ) )
        
        blah = unid.dummy %>%
          group_by( across( any_of( c("SID","YEAR") ) ) ) %>%
          summarize( AB1 = sum( AB1, na.rm=TRUE ),
                     B2  = sum(  B2, na.rm=TRUE ) ) %>%
          mutate( NEW_MODEN = 'TOTAL' )
        
        dummy.table = dummy.table %>%
          bind_rows( blah ) %>%
          arrange( across( any_of( c("SID","YEAR","NEW_MODEN") ) ) )
        rm( blah )
        
      } else if( report.type == "detailed" ) {
        
        dummy.table = unid.dummy %>%
          group_by( across( any_of( c("SID","YEAR","WAVE","NEW_STA","NEW_MODEN") ) ) ) %>%
          summarize( AB1 = sum( AB1, na.rm=TRUE ),
                     B2  = sum(  B2, na.rm=TRUE ) )
        
        dummy.table = dummy.table %>%
          arrange( across( any_of( c("SID","YEAR","NEW_MODEN","NEW_STA","WAVE") ) ) )
      }
      
      dummy.table = dummy.table %>%
        pivot_wider( names_from = NEW_MODEN, values_from = c(AB1,B2), names_glue = "{NEW_MODEN}_{.value}" ) %>%
        ungroup()
      
      
      
      if( 'SID' %in% colnames(genrec.table) ) {
        
        for( i in 1:length(cv.table) ) {
          
          cv.dummy = cv.table[[i]] %>%
            bind_rows( dummy.table %>% filter( SID == SID.levels[i] ) %>% select( -SID ) )
          
          if( report.type == "annual" ) {
            cv.dummy = cv.dummy %>%
              group_by( across( any_of( c("YEAR") ) ) ) %>%
              summarise_all( ~ sum( ., na.rm=TRUE ) )
          } else if( report.type == "detailed" ) {
            cv.dummy = cv.dummy %>%
              group_by( across( any_of( c("YEAR","NEW_STA","WAVE") ) ) ) %>%
              summarise_all( ~ sum( ., na.rm=TRUE ) )
          }
          
          cv.table[[i]] = cv.dummy
          rm( cv.dummy )
        }
        rm( i, SID.levels )
        
      } else {
        
        cv.dummy = cv.table %>% bind_rows( dummy.table )
        
        if( report.type == "annual" ) {
          cv.dummy = cv.dummy %>%
            group_by( across( any_of( c("YEAR") ) ) ) %>%
            summarise_all( ~ sum( ., na.rm=TRUE ) )
        } else if( report.type == "detailed" ) {
          cv.dummy = cv.dummy %>%
            group_by( across( any_of( c("YEAR","NEW_STA","WAVE") ) ) ) %>%
            summarise_all( ~ sum( ., na.rm=TRUE ) )
        }
        
        cv.table = cv.dummy
        rm( cv.dummy )
      }
      
      rm( unid.dummy, dummy.table )
      
    }
    
    
    
    
    
    ### MRIP -- Split of CbtHbt in MATL/NATL ###
    ### ----------------------------------------
    ###
    ###     ...for which the script below estimates uncertainty in those (individual) Cbt & Hbt catch estimates partitioned
    ###         from MRIP estimates of the combined for-hire mode (i.e., all CbtHbt catch comes from the MATL and/or NATL ).
    ###         This CV calculation is based on a modified version of Equation (5) in S74-DW-10:
    ###
    ###             var( C[yr,mode] ) = ( C[yr,FH]^2 )*var( ratio ) + ( ratio^2 )*var( C[yr,FH] ) - var(ratio)*var(C[yr,FH])
    
    if( flag.forhire ) {
      
      
      ### ALLOCATION 'RATIOS' & VARIANCES ###
      ###
      ###     For the two 'ratio' components in the above equation (i.e., catch & variance estimates for CBT:HBT ratios ),
      ###     these are saved in an excel spreadsheet constructed for SEDAR 82 (SATL GTF) that was based on calculations
      ###     of the number of angler-trips from raw intercept data ( TRP = CNTRBTRS * WP_INT ) for
      ###     MATL/NATL CBT & HBT vessels btw 1981-2003...
      
      ratios = read.csv( loc.FH.ratios ) %>%
      # ratios = read.csv( 'C://Users/matthew.nuttall/Desktop/Functions/import_datasets/ForHire Partitioning Ratios.csv' ) %>%
        
        rename( Hbt_ratio_est = Hbt,
                Hbt_ratio_var = Hbt_var,
                Cbt_ratio_est = Cbt,
                Cbt_ratio_var = Cbt_var )
      
      ### FORHIRE CATCH & UNCERTAINTY ###
      ###
      ### -- CATCH --
      ###     For the 'forhire' catch estimates in the above equation, these are imported from the 'catch.table.forhire',
      ###     which is a filtered copy of the 'catch.table' object to only include MATL/NATL forhire catch estimates
      ###     and so doesn't require an additional filter...
      ###         # dummy = genrec.table %>%
      ###         #   filter( DS == 'MRIP' & SUB_REG %in% 4:5 & NEW_MODEN %in% c('Cbt','Hbt') & YEAR %in% 1981:2003 ) %>%
      ###         # # filter( DS == 'MRIP' & SUB_REG %in% 4:5 & NEW_MODEN %in% c('Cbt','Hbt') & YEAR %in% 1981:2003 & is.na(UNID_FLAG) ) %
      ###     The (raw) catch estimates in 'catch.table.forhire' are then aggregated (i.e., summed ) to match the
      ###     resolution of (CV) estimates provided in 'cv.table', which differs between "annual" & "detailed" reports
      ###     and so the script below is separated into two components...
      ### 
      ### -- UNCERTAINTY --
      ###     Adding the last component to 'dummy' from the above equation (i.e., variance/uncertainty estimates of
      ###     catch for the combined forhire mode ) is a bit trickier... In particular, for the "annual" reports,
      ###     RDI provides CVs at either the year-mode or year-state level, not at the year-mode-state level required
      ###     for this calculation (i.e., to match the resolution at which CBTHBT allocation ratios were estimated ).
      ###     For these cases (i.e., report.type == "annual" ), we assume the (year-mode) forhire CVs in the
      ###     current 'cv.table' are representative of the associated year-mode-state CVs, in that the CVs for all states
      ###     ( within a given year-mode combo ) will be assigned the same uncertainty estimate...
      ###     Note that this assumption is not required for the "detailed" CV reports from RDI, which provide catch &
      ###     uncertainty estimates by year-mode-state-wave. However, the assumption here is that the allocation ratios
      ###     and variances, which are not wave-specific, are representative of the catches & CVs across all waves...
      
      if( report.type == "annual" ) {
        
        dummy = catch.table.forhire %>%
          
          ###     ...where state-specific estimates of the combined forhire mode are back-calculated from
          ###       'catch.table.forhire', summed to provide total (CBTHBT) estimates by year and state
          ###       which is the resolution at which CBTHBT allocation ratios were estimated and are applied...
          group_by( YEAR, NEW_STA ) %>%
          summarise( AB1 = sum( AB1, na.rm=TRUE ),
                     B2  = sum(  B2, na.rm=TRUE ) ) %>%
          ungroup() %>%
          
          ###     ...join the combined (forhire) estimates with the associated allocation ratios & their variances...
          left_join( ratios, by = c('YEAR','NEW_STA') )
        
        
        if( length(rdi.report) == 1 ) {
          dummy = dummy %>% left_join( cv.table %>%
                                         select( YEAR, CBT_HBT_CV_AB1, CBT_HBT_CV_B2 ), by = 'YEAR' )
        } else {
          dummy = dummy %>% left_join( cv.table[[loc.cv.forhire]] %>%
                                         select( YEAR, CBT_HBT_CV_AB1, CBT_HBT_CV_B2 ), by = 'YEAR' )
        }
        
      } else if( report.type == "detailed" ) {
        
        dummy = catch.table.forhire %>%
          
          ###     ...where state-specific estimates of the combined forhire mode are back-calculated from
          ###       'catch.table.forhire', summed to provide total (CBTHBT) estimates by year-state-wave
          ###       which is the resolution at which uncertainties have been estimated by RDI...
          group_by( YEAR, NEW_STA, WAVE ) %>%
          summarise( AB1 = sum( AB1, na.rm=TRUE ),
                     B2  = sum(  B2, na.rm=TRUE ) ) %>%
          ungroup() %>%
          
          ###     ...join the combined (forhire) estimates with the associated allocation ratios & their variances...
          left_join( ratios, by = c('YEAR','NEW_STA') )
        
        if( length(rdi.report) == 1 ) {
          dummy = dummy %>% left_join( cv.table %>%
                                         select( YEAR,WAVE,NEW_STA, CBT_HBT_CV_AB1, CBT_HBT_CV_B2 ),
                                       by = c('YEAR','NEW_STA','WAVE') )
        } else {
          dummy = dummy %>% left_join( cv.table[[loc.cv.forhire]] %>%
                                         select( YEAR,WAVE,NEW_STA, CBT_HBT_CV_AB1, CBT_HBT_CV_B2 ),
                                       by = c('YEAR','NEW_STA','WAVE') )
        }
      }
      
      
      ### CALCULATIONS ###

      dummy = dummy %>%
        mutate( AB1_var = ( CBT_HBT_CV_AB1 * AB1 )^2,
                B2_var  = ( CBT_HBT_CV_B2  * B2  )^2 ) %>%
        
        select( any_of( c( 'YEAR', 'NEW_STA', 'WAVE', 'AB1', 'AB1_var', 'B2', 'B2_var',
                           'Hbt_ratio_est', 'Hbt_ratio_var', 'Cbt_ratio_est', 'Cbt_ratio_var' ) ) ) %>%
        ###     ...the result of which is a table of all the elements required in our modified version of Equation (5),
        ###       applied here to calculate the catch & associated variances (i.e., uncertainty ) of our re-partitioned
        ###       Cbt & Hbt estimates...
        
        mutate( CBT_AB1 = AB1 * Cbt_ratio_est,
                HBT_AB1 = AB1 * Hbt_ratio_est,
                CBT_B2  =  B2 * Cbt_ratio_est,
                HBT_B2  =  B2 * Hbt_ratio_est ) %>%
        
        mutate( CBT_VAR_AB1 = ( (AB1^2)*Cbt_ratio_var ) +
                  ( (Cbt_ratio_est^2)*AB1_var ) - (Cbt_ratio_var*AB1_var),
                HBT_VAR_AB1 = ( (AB1^2)*Hbt_ratio_var ) +
                  ( (Hbt_ratio_est^2)*AB1_var ) - (Hbt_ratio_var*AB1_var),
                CBT_VAR_B2  = ( ( B2^2)*Cbt_ratio_var ) +
                  ( (Cbt_ratio_est^2)*B2_var  ) - (Cbt_ratio_var*B2_var ),
                HBT_VAR_B2  = ( ( B2^2)*Hbt_ratio_var ) +
                  ( (Hbt_ratio_est^2)*B2_var  ) - (Hbt_ratio_var*B2_var ) ) %>%
        
        select( any_of( c( 'YEAR', 'NEW_STA', 'WAVE',
                           'CBT_AB1', 'CBT_VAR_AB1', 'HBT_AB1', 'HBT_VAR_AB1',
                           'CBT_B2', 'CBT_VAR_B2', 'HBT_B2', 'HBT_VAR_B2' ) ) )
      
      ###   Lastly, I aggregate these estimates (e.g., summed across states ) into the final resolution
      ###   at which CV estimates are being provided (i.e., in 'cv.table' )...
      
      if( report.type == "annual" ) {
        
        dummy = dummy %>%
          group_by( YEAR ) %>%
          summarise( CBT_AB1     = sum( CBT_AB1    , na.rm=TRUE ),
                     CBT_VAR_AB1 = sum( CBT_VAR_AB1, na.rm=TRUE ),
                     HBT_AB1     = sum( HBT_AB1    , na.rm=TRUE ),
                     HBT_VAR_AB1 = sum( HBT_VAR_AB1, na.rm=TRUE ),
                     CBT_B2      = sum( CBT_B2     , na.rm=TRUE ),
                     CBT_VAR_B2  = sum( CBT_VAR_B2 , na.rm=TRUE ),
                     HBT_B2      = sum( HBT_B2     , na.rm=TRUE ),
                     HBT_VAR_B2  = sum( HBT_VAR_B2 , na.rm=TRUE ) )
          
          # mutate( CBT_CV_AB1 = sqrt(CBT_VAR_AB1) / CBT_AB1,
          #         HBT_CV_AB1 = sqrt(HBT_VAR_AB1) / HBT_AB1,
          #         CBT_CV_B2  = sqrt(CBT_VAR_B2 ) / CBT_B2,
          #         HBT_CV_B2  = sqrt(HBT_VAR_B2 ) / HBT_B2  ) %>%
          # select( any_of( c( 'YEAR', 'NEW_STA', 'WAVE',
          #                    'CBT_AB1', 'CBT_CV_AB1', 'HBT_AB1', 'HBT_CV_AB1',
          #                    'CBT_B2', 'CBT_CV_B2', 'HBT_B2', 'HBT_CV_B2' ) ) )
          ###   Note that the (commented-out) script above is retained to allow the user to evaluate the resultant CVs
          ###   from the above calculations. However, it isn't actually applied because the resultant CBT & HBT components
          ###   (as calculated from the combined forhire estimates) will need to be combined/summed with any other
          ###   CBT & HBT estimates provided in our final 'cv.table' (i.e., any estimates for these separate modes )...
        
      } else if( report.type == "detailed" ) {
        
        dummy = dummy %>%
          group_by( YEAR, NEW_STA, WAVE ) %>%
          summarise( CBT_AB1     = sum( CBT_AB1    , na.rm=TRUE ),
                     CBT_VAR_AB1 = sum( CBT_VAR_AB1, na.rm=TRUE ),
                     HBT_AB1     = sum( HBT_AB1    , na.rm=TRUE ),
                     HBT_VAR_AB1 = sum( HBT_VAR_AB1, na.rm=TRUE ),
                     CBT_B2      = sum( CBT_B2     , na.rm=TRUE ),
                     CBT_VAR_B2  = sum( CBT_VAR_B2 , na.rm=TRUE ),
                     HBT_B2      = sum( HBT_B2     , na.rm=TRUE ),
                     HBT_VAR_B2  = sum( HBT_VAR_B2 , na.rm=TRUE ) )
      }
      
      
      dummy[ is.na(dummy) ] = 0
      
      
      ###   To "complete" this table (before the merge with our original 'cv.table'), I then add the associated sample sizes
      ###   of the combined for-hire mode to 'dummy' -- again, these are imported from 'cv.table'. In terms of estimation,
      ###   all Cbt & Hbt trips were used to produce a single combined for-hire estimate, and so each of the
      ###   mode-specific estimates generated above will use the same sample size (i.e., N(CbtHbt) = N(Cbt) = N(Hbt) )...
      
      if( report.type == "annual" ) {
        
        if( length(rdi.report) == 1 ) {
          dummy = dummy %>% left_join( cv.table %>%
                                         select( contains( c('YEAR','CBT_HBT_PSU','CBT_HBT_AT_') ) ), by = 'YEAR' )
        } else {
          dummy = dummy %>% left_join( cv.table[[loc.cv.forhire]] %>%
                                         select( contains( c('YEAR','CBT_HBT_PSU','CBT_HBT_AT_') ) ), by = 'YEAR' )
        }
        
      } else if( report.type == "detailed" ) {
        if( length(rdi.report) == 1 ) {
          dummy = dummy %>% left_join( cv.table %>%
                                         select( contains( c('YEAR','NEW_STA','WAVE','CBT_HBT_PSU','CBT_HBT_AT_') ) ),
                                       by = c('YEAR','NEW_STA','WAVE') )
        } else {
          dummy = dummy %>% left_join( cv.table[[loc.cv.forhire]] %>%
                                         select( contains( c('YEAR','NEW_STA','WAVE','CBT_HBT_PSU','CBT_HBT_AT_') ) ),
                                       by = c('YEAR','NEW_STA','WAVE') )
        }
      }
      
      dummy = dummy %>%
        mutate( HBT_PSU_AB1 = CBT_HBT_PSU_AB1,
                HBT_PSU_B2  = CBT_HBT_PSU_B2,
                HBT_PSU_Tot = CBT_HBT_PSU_Tot,
                HBT_AT_AB1  = CBT_HBT_AT_AB1,
                HBT_AT_B2   = CBT_HBT_AT_B2,
                HBT_AT_Tot  = CBT_HBT_AT_Tot ) %>%
        rename( CBT_PSU_AB1 = CBT_HBT_PSU_AB1,
                CBT_PSU_B2  = CBT_HBT_PSU_B2,
                CBT_PSU_Tot = CBT_HBT_PSU_Tot,
                CBT_AT_AB1  = CBT_HBT_AT_AB1,
                CBT_AT_B2   = CBT_HBT_AT_B2,
                CBT_AT_Tot  = CBT_HBT_AT_Tot )
      
      
      ### JOIN WITH 'CV.TABLE' ###
      ###
      ###   With the combined for-hire estimates now partitioned ( in 'dummy' ), I then join them with my
      ###   original 'cv.table', remove the combined CbtHbt estimates, and add the newly separated
      ###   Cbt and Hbt estimates with their respective column. Note that, unlike some of the other imputation
      ###   approaches run in this script, no associated adjustment is needed to the TOTAL catch columns
      ###   (i.e., 'TOTAL_AB1' or 'TOTAL_B2' ) as we aren't 'creating' any catch, we're simply re-allocating
      ###   it from the combined 'CBT_HBT' mode into the separate 'CBT' & 'HBT' modes...
      
      if( length(rdi.report) == 1 ) {
        blah = cv.table
      } else {
        blah = cv.table[[loc.cv.forhire]]
      }
      
      blah = blah %>%
        select( -contains( 'CBT_HBT_' ) ) %>%
        mutate( CBT_VAR_AB1 = ( CBT_AB1 * CBT_CV_AB1 )^2,
                HBT_VAR_AB1 = ( HBT_AB1 * HBT_CV_AB1 )^2,
                CBT_VAR_B2  = ( CBT_B2  * CBT_CV_B2  )^2,
                HBT_VAR_B2  = ( HBT_B2  * HBT_CV_B2  )^2 ) %>%
        
        bind_rows( dummy )
      
      if( report.type == "annual" ) {
        blah = blah %>% group_by( YEAR ) %>% summarise_all( list( sum ), na.rm=TRUE )
        ###     ...where the only fields that have been updated are those for 'Cbt' and 'Hbt'
        ###         ( the estimates for all other modes had a zero value added to them, and so are unchanged )
        
      } else if( report.type == "detailed" ) {
        blah = blah %>% group_by( YEAR, NEW_STA, WAVE ) %>% summarise_all( list( sum ), na.rm=TRUE )
      }
      
      ###   Finally, the CV columns are recalculated for Cbt & Hbt from the newly summed catch and variance fields...
      blah = blah %>%
        mutate( CBT_CV_AB1 = ifelse( CBT_AB1 == 0, 0, sqrt(CBT_VAR_AB1) / CBT_AB1 ),
                CBT_CV_B2  = ifelse( CBT_B2  == 0, 0, sqrt(CBT_VAR_B2 ) / CBT_B2  ),
                HBT_CV_AB1 = ifelse( HBT_AB1 == 0, 0, sqrt(HBT_VAR_AB1) / HBT_AB1 ),
                HBT_CV_B2  = ifelse( HBT_B2  == 0, 0, sqrt(HBT_VAR_B2 ) / HBT_B2  ) ) %>%
        ###   ...after which, the variance fields are no longer needed...
        select( -contains( '_VAR_' ) )
      
      
      if( length(rdi.report) == 1 ) {
        cv.table = blah
      } else {
        cv.table[[loc.cv.forhire]] = blah
      }
      
      rm( blah, dummy )
      
    }
    
    
    
    
    
    ### MRIP Imputation -- 1981 wave1 ###
    ### ---------------------------------
    ###
    ###     ...for which 1981 is a 'partial' imputation requiring wave-level calculations to recreate
    ###         ( MRIP estimates already exist for 1981 waves 2-6, so only need to impute estimates for one wave ).
    ###         Instead of recreating this calculation, and estimating the associated wave-level variances,
    ###         I simply import the 1981 estimates from 'catch.table' ( which already includes the imputed estimates )
    ###         and substitute these into my MRIP 'cv.table'...
    ###     Note that I am only substituting the 1981 (MRIP) catch estimates with this script, wherein the
    ###         assumption is that the CVs estimated from waves 2-6 ( in 1981 ) are also representative of wave 1...
    
    if( ( imp.1981w1 != 'None' ) & ( 1981 %in% cv.table$YEAR ) ) {
      ###   ...where the check for 1981 in 'cv.table' is to account for cases where we might be looking at CVs
      ###     for a subset of the full GenRec time series...
      
      
      dummy.table = genrec.table %>% filter( DS == 'MRIP' & YEAR == 1981 )
      
      ###   In addition to the MRIP-1981 filter above, we also include a filter to make sure only those modes
      ###   being considered in this pull (i.e., in "inc.modes" ) are being retained in 'dummy.table', which
      ###   may not be true if we're only looking at a subset of the full, region-wide catch table...
      dummy.table = dummy.table %>% filter( NEW_MODEN %in% inc.modes )
      
      
      ###   Additionally, there are two other filters that we (might) need to apply before we can pull the appropriate
      ###     (1981-MRIP) catch estimates from 'genrec.table':
      ###
      ###     (1) Unidentified Catch Filter
      ###         ...where for assessments where some percent of unidentified catch is assigned to the species-of-interest
      ###         ( 'flag.unid' == TRUE ), we will typically ignore this unidentified catch when calculating our uncertainties
      ###         ( unless 100% of UnIDd catch is assigned to the species-of-interest, we cannot distinguish which intercepts
      ###           'belong' to the assessed species vs. other species ). Therefore, to match that data used to calculate the
      ###         uncertainties in 'cv.table', we must exclude any estimates that were originally assigned as 'unidentified'...
      ###           # if( flag.unid ) {     dummy.table = dummy.table %>% filter( is.na(UNID_FLAG) )      }
      ###           # # if( flag.unid ) {     dummy.table = dummy.table %>% filter( UNID_FLAG != 'Y' )      }
      ###
      ###     (2) Spatial (SID) Filter
      ###         ...where for assessments that have multiple SID boundaries ( length(rdi.report) > 1 ), we also need to
      ###         apply a spatial filter to 'genrec.table' to ensure those catch estimates pulled from 'genrec.table'
      ###         correspond to the same states considered when calculating the associated CV estimates ( in 'cv.table' ).
      ###         For 'SID' assessments, I therefore iteratively call each of the elements in 'rdi.report' to determine
      ###         which states were considered in calculating each set of CV estimates, for which I reference the 'state' summary
      ###         in RDI instead of the 'mode' summary (i.e., view = 'rdi.apex_cv_data_yr_s' )...
      ###
      if( length(rdi.report) == 1 ) {
        
        
        ### Assessments with no SID domains ###
        ### -----------------------------------
        ###     ...for which no additional spatial filter is needed to subset 'genrec.table' to a particular SID...
        
        
        ### Calculation of (total) 1981 MRIP catch
        
        if( report.type == "annual" ) {
          
          dummy = dummy.table %>%
            group_by( NEW_MODEN ) %>%
            summarise( AB1 = sum( AB1, na.rm=TRUE ),
                       B2  = sum(  B2, na.rm=TRUE ) ) %>%
            mutate( NEW_MODEN = toupper(NEW_MODEN) )
          
          blah = dummy %>% bind_rows( data.frame( NEW_MODEN = 'TOTAL', AB1=sum(dummy$AB1), B2=sum(dummy$B2) ) )
          rm( dummy )
          
        } else if( report.type == "detailed" ) {
          
          blah = dummy.table %>%
            group_by( NEW_MODEN, NEW_STA, WAVE ) %>%
            summarise( AB1 = sum( AB1, na.rm=TRUE ),
                       B2  = sum(  B2, na.rm=TRUE ) ) %>%
            mutate( NEW_MODEN = toupper(NEW_MODEN) )
        }
        
        
        ### Substitution of (total) 1981 MRIP catch into 'cv.table'
        
        # col.IDs = apply( expand.grid(unique(blah$NEW_MODEN),c("AB1","B2")), 1,paste,collapse="_" )
        col.IDs = unique( blah$NEW_MODEN )
        
        ###   ...where, instead of replacing the "TOTAL" column with that in the current 'dummy' table, which
        ###     may be a subset of the full catch table (e.g., if we're looking at CVs for a specific state/mode,
        ###     instead of region-wide catch for the stock ), the adjustments of "TOTAL" will be done iteratively
        ###     by calculating the change in "TOTAL" from each mode (i.e., from each element in 'col.IDs' ) and
        ###     then applying that difference to the "TOTAL" column...
        col.IDs = col.IDs[ col.IDs %notin% c("TOTAL") ]
        
        dummy = cv.table
        
        for( i in 1:length(col.IDs) ) {
          
          if( report.type == "annual" ) {
            
            ### AB1 ###
            eval( parse( text = paste0( "dummy.val.TOTAL = ",
                                        " blah$AB1[ blah$NEW_MODEN == '",col.IDs[i],"' ] - ",
                                        " dummy$",col.IDs[i],"_AB1[ dummy$YEAR == 1981 ]" ) ) )
            eval( parse( text = paste0( "dummy$",col.IDs[i],"_AB1[ ",
                                        " dummy$YEAR == 1981 ] = ",
                                        " blah$AB1[ blah$NEW_MODEN == '",col.IDs[i],"' ]" ) ) )
            eval( parse( text = paste0( "dummy$TOTAL_AB1[ dummy$YEAR == 1981 ] = ",
                                        " dummy$TOTAL_AB1[ dummy$YEAR == 1981 ] + dummy.val.TOTAL" ) ) )
            
            ### B2 ###
            eval( parse( text = paste0( "dummy.val.TOTAL = ",
                                        " blah$B2[ blah$NEW_MODEN == '",col.IDs[i],"' ] - ",
                                        " dummy$",col.IDs[i],"_B2[ dummy$YEAR == 1981 ]" ) ) )
            eval( parse( text = paste0( "dummy$",col.IDs[i],"_B2[ ",
                                        " dummy$YEAR == 1981 ] = ",
                                        " blah$B2[ blah$NEW_MODEN == '",col.IDs[i],"' ]" ) ) )
            eval( parse( text = paste0( "dummy$TOTAL_B2[ dummy$YEAR == 1981 ] = ",
                                        " dummy$TOTAL_B2[ dummy$YEAR == 1981 ] + dummy.val.TOTAL" ) ) )
            rm( dummy.val.TOTAL )
            
            
          } else if( report.type == "detailed" ) {
            
            wave.IDs  = unique( blah$WAVE[ blah$NEW_MODEN == col.IDs[i] ] )
            state.IDs = unique( blah$NEW_STA[ blah$NEW_MODEN == col.IDs[i] ] )
            
            for( s in 1:length(state.IDs) ) {
              for( w in 1:length(wave.IDs) ) {
                
                ### AB1 ###
                eval( parse( text = paste0( "dummy.val.TOTAL = ",
                                            " blah$AB1[ blah$NEW_MODEN == '",col.IDs[i],"' & ",
                                                      " blah$NEW_STA == '",state.IDs[s],"' & ",
                                                      " blah$WAVE == '",wave.IDs[w],"' ] - ",
                                            " dummy$",col.IDs[i],"_AB1[ dummy$YEAR == 1981 & ",
                                                      " dummy$NEW_STA == '",state.IDs[s],"' & ",
                                                      " dummy$WAVE == '",wave.IDs[w],"' ]" ) ) )
                eval( parse( text = paste0( "dummy$",col.IDs[i],"_AB1[ ",
                                                      " dummy$YEAR == 1981 & ",
                                                      " dummy$NEW_STA == '",state.IDs[s],"' & ",
                                                      " dummy$WAVE == '",wave.IDs[w],"' ] = ",
                                            " blah$AB1[ blah$NEW_MODEN == '",col.IDs[i],"' & ",
                                                      " blah$NEW_STA == '",state.IDs[s],"' & ",
                                                      " blah$WAVE == '",wave.IDs[w],"' ]" ) ) )
                eval( parse( text = paste0( "dummy$TOTAL_AB1[ dummy$YEAR == 1981 & ",
                                                      " dummy$NEW_STA == '",state.IDs[s],"' & ",
                                                      " dummy$WAVE == '",wave.IDs[w],"' ] = ",
                                            " dummy$TOTAL_AB1[ dummy$YEAR == 1981 & ",
                                                      " dummy$NEW_STA == '",state.IDs[s],"' & ",
                                                      " dummy$WAVE == '",wave.IDs[w],"' ] + dummy.val.TOTAL" ) ) )
                
                ### B2 ###
                eval( parse( text = paste0( "dummy.val.TOTAL = ",
                                            " blah$B2[ blah$NEW_MODEN == '",col.IDs[i],"' & ",
                                                      " blah$NEW_STA == '",state.IDs[s],"' & ",
                                                      " blah$WAVE == '",wave.IDs[w],"' ] - ",
                                            " dummy$",col.IDs[i],"_B2[ dummy$YEAR == 1981 & ",
                                                      " dummy$NEW_STA == '",state.IDs[s],"' & ",
                                                      " dummy$WAVE == '",wave.IDs[w],"' ]" ) ) )
                eval( parse( text = paste0( "dummy$",col.IDs[i],"_B2[ ",
                                                      " dummy$YEAR == 1981 & ",
                                                      " dummy$NEW_STA == '",state.IDs[s],"' & ",
                                                      " dummy$WAVE == '",wave.IDs[w],"' ] = ",
                                            " blah$B2[ blah$NEW_MODEN == '",col.IDs[i],"' & ",
                                                      " blah$NEW_STA == '",state.IDs[s],"' & ",
                                                      " blah$WAVE == '",wave.IDs[w],"' ]" ) ) )
                eval( parse( text = paste0( "dummy$TOTAL_B2[ dummy$YEAR == 1981 & ",
                                                      " dummy$NEW_STA == '",state.IDs[s],"' & ",
                                                      " dummy$WAVE == '",wave.IDs[w],"' ] = ",
                                            " dummy$TOTAL_B2[ dummy$YEAR == 1981 & ",
                                                      " dummy$NEW_STA == '",state.IDs[s],"' & ",
                                                      " dummy$WAVE == '",wave.IDs[w],"' ] + dummy.val.TOTAL" ) ) )
                rm( dummy.val.TOTAL )
              }
            }
            
            rm( s,w ,wave.IDs, state.IDs )
            
          }
          
        }
        
        cv.table = dummy
        
        rm( blah, dummy, col.IDs )
        
        
      } else {
        
        
        ### Assessments with SID domains ###
        ### --------------------------------
        ###
        ###     ...for which a unique spatial filter is required for each element of 'rdi.report'. Therefore,
        ###       the script below iteratively cycles through each SID domain, making sure the same subset of states
        ###       are pulled from 'genrec.table' as those used in calculating the associated CVs ( in 'cv.table' )...

        for( j in 1:length(rdi.report) ) {
          
          
          ### Identification of those states considered in each element of 'cv.table'
          
          con = dbConnect(dbDriver("Oracle"), username = keyring::key_list("SECPR")[1,2],
                          password = keyring::key_get("SECPR", keyring::key_list("SECPR")[1,2]), dbname = "SECPR")
          
          mrip.state = dbGetQuery(con,
                                  paste0("select * ",
                                         "from rdi.apex_cv_data_yr_s@secapxdv_dblk.sfsc.noaa.gov t
                              where t.APP_USER = ", sprintf("'%s'", paste( report.name[j], collapse = "','" ))
                                  ))
          mrip.state = mrip.state %>% select( -c('APP_USER','YEAR') )
          mrip.state = unique( gsub( '_.*','', colnames(mrip.state)[ colSums(mrip.state,na.rm=TRUE) > 0 ] ) )
          
          
          dummy = dummy.table %>% filter( NEW_STA %in% mrip.state )
          rm( mrip.state )
          
          
          ### Calculation of (total) 1981 MRIP catch
          
          if( report.type == "annual" ) {
            
            dummy = dummy %>%
              group_by( NEW_MODEN ) %>%
              summarise( AB1 = sum( AB1, na.rm=TRUE ),
                         B2  = sum(  B2, na.rm=TRUE ) ) %>%
              mutate( NEW_MODEN = toupper(NEW_MODEN) )
            
            blah = dummy %>% bind_rows( data.frame( NEW_MODEN = 'TOTAL', AB1=sum(dummy$AB1), B2=sum(dummy$B2) ) )
            rm( dummy )
            
          } else if( report.type == "detailed" ) {
            
            dummy = dummy %>%
              group_by( NEW_MODEN, NEW_STA, WAVE ) %>%
              summarise( AB1 = sum( AB1, na.rm=TRUE ),
                         B2  = sum(  B2, na.rm=TRUE ) ) %>%
              mutate( NEW_MODEN = toupper(NEW_MODEN) )
          }
          
          
          ### Substitution of (total) 1981 MRIP catch into 'cv.table'
          
          col.IDs = unique( blah$NEW_MODEN )
          col.IDs = col.IDs[ col.IDs %notin% c("TOTAL") ]
          
          dummy = cv.table[[j]]
          
          for( i in 1:length(col.IDs) ) {
            
            if( report.type == "annual" ) {
              
              ### AB1 ###
              eval( parse( text = paste0( "dummy.val.TOTAL = ",
                                          " blah$AB1[ blah$NEW_MODEN == '",col.IDs[i],"' ] - ",
                                          " dummy$",col.IDs[i],"_AB1[ dummy$YEAR == 1981 ]" ) ) )
              eval( parse( text = paste0( "dummy$",col.IDs[i],"_AB1[ ",
                                          " dummy$YEAR == 1981 ] = ",
                                          " blah$AB1[ blah$NEW_MODEN == '",col.IDs[i],"' ]" ) ) )
              eval( parse( text = paste0( "dummy$TOTAL_AB1[ dummy$YEAR == 1981 ] = ",
                                          " dummy$TOTAL_AB1[ dummy$YEAR == 1981 ] + dummy.val.TOTAL" ) ) )
              
              ### B2 ###
              eval( parse( text = paste0( "dummy.val.TOTAL = ",
                                          " blah$B2[ blah$NEW_MODEN == '",col.IDs[i],"' ] - ",
                                          " dummy$",col.IDs[i],"_B2[ dummy$YEAR == 1981 ]" ) ) )
              eval( parse( text = paste0( "dummy$",col.IDs[i],"_B2[ ",
                                          " dummy$YEAR == 1981 ] = ",
                                          " blah$B2[ blah$NEW_MODEN == '",col.IDs[i],"' ]" ) ) )
              eval( parse( text = paste0( "dummy$TOTAL_B2[ dummy$YEAR == 1981 ] = ",
                                          " dummy$TOTAL_B2[ dummy$YEAR == 1981 ] + dummy.val.TOTAL" ) ) )
              rm( dummy.val.TOTAL )
              
              
            } else if( report.type == "detailed" ) {
              
              wave.IDs  = unique( blah$WAVE[ blah$NEW_MODEN == col.IDs[i] ] )
              state.IDs = unique( blah$NEW_STA[ blah$NEW_MODEN == col.IDs[i] ] )
              
              for( s in 1:length(state.IDs) ) {
                for( w in 1:length(wave.IDs) ) {
                  
                  ### AB1 ###
                  eval( parse( text = paste0( "dummy.val.TOTAL = ",
                                              " blah$AB1[ blah$NEW_MODEN == '",col.IDs[i],"' & ",
                                                        " blah$NEW_STA == '",state.IDs[s],"' & ",
                                                        " blah$WAVE == '",wave.IDs[w],"' ] - ",
                                              " dummy$",col.IDs[i],"_AB1[ dummy$YEAR == 1981 & ",
                                                        " dummy$NEW_STA == '",state.IDs[s],"' & ",
                                                        " dummy$WAVE == '",wave.IDs[w],"' ]" ) ) )
                  eval( parse( text = paste0( "dummy$",col.IDs[i],"_AB1[ ",
                                                        " dummy$YEAR == 1981 & ",
                                                        " dummy$NEW_STA == '",state.IDs[s],"' & ",
                                                        " dummy$WAVE == '",wave.IDs[w],"' ] = ",
                                              " blah$AB1[ blah$NEW_MODEN == '",col.IDs[i],"' & ",
                                                        " blah$NEW_STA == '",state.IDs[s],"' & ",
                                                        " blah$WAVE == '",wave.IDs[w],"' ]" ) ) )
                  eval( parse( text = paste0( "dummy$TOTAL_AB1[ dummy$YEAR == 1981 & ",
                                                        " dummy$NEW_STA == '",state.IDs[s],"' & ",
                                                        " dummy$WAVE == '",wave.IDs[w],"' ] = ",
                                              " dummy$TOTAL_AB1[ dummy$YEAR == 1981 & ",
                                                        " dummy$NEW_STA == '",state.IDs[s],"' & ",
                                                        " dummy$WAVE == '",wave.IDs[w],"' ] + dummy.val.TOTAL" ) ) )
                  
                  ### B2 ###
                  eval( parse( text = paste0( "dummy.val.TOTAL = ",
                                              " blah$B2[ blah$NEW_MODEN == '",col.IDs[i],"' & ",
                                                        " blah$NEW_STA == '",state.IDs[s],"' & ",
                                                        " blah$WAVE == '",wave.IDs[w],"' ] - ",
                                              " dummy$",col.IDs[i],"_B2[ dummy$YEAR == 1981 & ",
                                                        " dummy$NEW_STA == '",state.IDs[s],"' & ",
                                                        " dummy$WAVE == '",wave.IDs[w],"' ]" ) ) )
                  eval( parse( text = paste0( "dummy$",col.IDs[i],"_B2[ ",
                                                        " dummy$YEAR == 1981 & ",
                                                        " dummy$NEW_STA == '",state.IDs[s],"' & ",
                                                        " dummy$WAVE == '",wave.IDs[w],"' ] = ",
                                              " blah$B2[ blah$NEW_MODEN == '",col.IDs[i],"' & ",
                                                        " blah$NEW_STA == '",state.IDs[s],"' & ",
                                                        " blah$WAVE == '",wave.IDs[w],"' ]" ) ) )
                  eval( parse( text = paste0( "dummy$TOTAL_B2[ dummy$YEAR == 1981 & ",
                                                        " dummy$NEW_STA == '",state.IDs[s],"' & ",
                                                        " dummy$WAVE == '",wave.IDs[w],"' ] = ",
                                              " dummy$TOTAL_B2[ dummy$YEAR == 1981 & ",
                                                        " dummy$NEW_STA == '",state.IDs[s],"' & ",
                                                        " dummy$WAVE == '",wave.IDs[w],"' ] + dummy.val.TOTAL" ) ) )
                  rm( dummy.val.TOTAL )
                }
              }
              
              rm( s,w ,wave.IDs, state.IDs )
              
            }
            
          }
          
          cv.table[[j]] = dummy
          
          rm( blah, dummy, col.IDs )
          
        }
        
        rm( j )
      }
      rm( dummy.table )
    }
    
    
    
    
    ### Substituting Catch Values from 'catch.table' ###
    ### ------------------------------------------------
    ###
    ###   As a final step, we substitute the (actual) catch estimates from 'catch.table' into our constructed 'cv.table'.
    ###   While these estimates should be similar ( the adjustments made to 'catch.table' in the main script have largely
    ###   been recreated in the 'cv.table' script ), they may not match exactly for a couple reasons:
    ###         (1) the original (MRIP) catch estimates in 'catch.table' are pulled from the MRIP SEDAR2 view in Oracle,
    ###             which contain estimates as provided by OST (i.e., no rounding ). Conversely, the original (MRIP) estimates
    ###             in 'cv.table' are pulled from the MRIP CV reports in RDI, which are rounded to integers. Although
    ###             insignificant initially, these 'rounding errors' may be amplified by any subsequent adjustments made
    ###             to the original estimates (e.g., allocation of UNID, imputations of 1981-wave1 ).
    ###         (2) the generation of 'cv.table' is considered (fully) automated at this point, but only for those adjustments
    ###             that are a routine step in estimating MRIP catch & the associated CVs. However, manual corrections are
    ###             sometimes required for a specific strata, which would obviously not be included in the standard CV script.
    ###   To ensure catch estimates in our final 'cv.table' match those in other tables (i.e., to avoid confusion for analysts ),
    ###   the code below replaces the AB1 estimates in 'cv.table' with those from 'catch.table'...
    
    
    if( length(rdi.report) == 1 ) {
      
      
      ### Assessments with no SID domains ###
      ### -----------------------------------
      
      dummy.cv = cv.table
      
      dummy.catch <- genrec.table %>%
        filter( DS == 'MRIP' ) %>%
        
        ###   ...in addition to a "MRIP" filter, we also make sure to only include those modes being considered
        ###   in this pull (i.e., in "inc.modes" ), which may be less than those in the full ( region-wide )
        ###   "genrec.table" table if we're only looking at a subset of stock's range in 'cv.table'...
        filter( NEW_MODEN %in% inc.modes ) %>%
        
        mutate( NEW_MODEN = ifelse( NEW_MODEN == "Priv/Shore", "Priv",
                            ifelse( NEW_MODEN == "Cbt/Hbt", "Cbt_Hbt", NEW_MODEN )) ) %>%
        mutate( NEW_MODEN = toupper(NEW_MODEN) )
      
      if( report.type == "annual" ) {
        
        dummy.catch = dummy.catch %>%
          group_by( YEAR, NEW_MODEN ) %>%
          summarise( AB1 = sum( AB1, na.rm=TRUE ),
                     B2  = sum(  B2, na.rm=TRUE ) )
        blah = dummy.catch %>%
          group_by( YEAR ) %>%
          summarise( AB1 = sum( AB1, na.rm=TRUE ),
                     B2 = sum(  B2, na.rm=TRUE ) ) %>%
          mutate( NEW_MODEN = "TOTAL" )
        dummy.catch = bind_rows( dummy.catch, blah ) %>%
          pivot_wider( names_from = NEW_MODEN, values_from = c(AB1,B2), names_glue = "{NEW_MODEN}_{.value}" )
        rm( blah )
        
      } else if( report.type == "detailed" ) {
        
        dummy.catch = dummy.catch %>%
          group_by( YEAR, NEW_MODEN, NEW_STA, WAVE ) %>%
          summarise( AB1 = sum( AB1, na.rm=TRUE ),
                     B2  = sum(  B2, na.rm=TRUE ) ) %>%
          pivot_wider( names_from = NEW_MODEN, values_from = c(AB1,B2), names_glue = "{NEW_MODEN}_{.value}" )
      }
      
      
      if( dim(dummy.cv)[1] > dim(dummy.catch)[1] ) {
        ###   ...where the 'genrec.table' (i.e., 'dummy.catch' ) is likely missing a number of years associated
        ###     with zero catch estimates, which would be dropped in the above summaries...
        blah = unique(dummy.cv$YEAR)[ unique(dummy.cv$YEAR) %notin% unique(dummy.catch$YEAR) ]
        dummy.catch = bind_rows( dummy.catch, data.frame( YEAR = blah ) ) %>%
          arrange( across( any_of( c("YEAR","NEW_STA","WAVE") ) ) )
        rm( blah )
        
      } else {
        ###   ...where the 'dummy.cv' table likely represents a subset of the full time series
        ###     (i.e., RDI CV report generated for only a few years of the MRIP survey )...
        blah = unique(dummy.cv$YEAR)[ unique(dummy.cv$YEAR) %in% unique(dummy.catch$YEAR) ]
        dummy.catch = dummy.catch %>% filter( YEAR %in% blah )
        rm( blah )
      }
      
      dummy.catch[ is.na(dummy.catch) ] = 0
      
      
      ### I then replace the catch estimates in 'dummy.cv' with those from 'dummy.catch'...
      
      # dummy.cv    = dummy.cv    %>% arrange( across( any_of( c("YEAR","NEW_STA","WAVE") ) ) )
      # dummy.catch = dummy.catch %>% arrange( across( any_of( c("YEAR","NEW_STA","WAVE") ) ) )
      
      col.IDs = colnames(dummy.catch)[ grepl("_AB1",colnames(dummy.catch)) | grepl("_B2",colnames(dummy.catch)) ]
      
      for( i in 1:length(col.IDs) ) {
        
        # eval( parse( text = paste0( "dummy.cv$",col.IDs[i]," = dummy.catch$",col.IDs[i] ) ) )
        
        # if( report.type == "annual" ) {
        #   locs = match( dummy.catch$YEAR, dummy.cv$YEAR )
        #   locs = locs[ !is.na(locs) ]
        #   eval( parse( text = paste0( "dummy.cv$",col.IDs[i],"[ locs ] = dummy.catch$",col.IDs[i] ) ) )
        # } else if( report.type == "detailed" ) {
        #   
        # }
        # rm( locs )
        
        for( r in dim(dummy.catch)[1] ) {
          dummy.cv[ dummy.cv$YEAR == dummy.catch$YEAR[r] &
                      dummy.cv$NEW_STA == dummy.catch$NEW_STA[r] &
                      dummy.cv$WAVE == dummy.catch$WAVE[r], col.IDs[i] ] = as.numeric( dummy.catch[ r , col.IDs[i] ] )
        }
        rm( r )
      }
      
      cv.table <- dummy.cv
      rm( col.IDs, i, dummy.catch, dummy.cv )
      
      
      
    } else {
      
      
      ### Assessments with SID domains ###
      ### --------------------------------
      ###
      ###     ...for which I start by identifying which states/domains are included in each SID region,
      ###         which is informed from my 'catch.table' object. In particular, the CV reports in 'rdi.report'
      ###         are sorted geographically, and so sorting 'catch.table' geographically will both tell
      ###             (1) which RDI CV report corresponds to which SID region and
      ###             (2) which states/domains are included in which SID region
      ###       Both sources of information are used in the script below...
      
      SID.table = genrec.table %>%
        ungroup() %>%
        arrange( NEW_ST, FL_REG, NC_REG ) %>%
        distinct( SID, NEW_ST, NEW_STA, FL_REG, NC_REG )
      
      SID.values = unique( SID.table$SID )
      ###   ...for which I remove any 'SID' domains corresponding to the state of Texas, which has never
      ###     been sampled by MRIP and so not applicable to this pull ( of MRIP-CVs )...
      SID.values = SID.values[ !grepl('TX',SID.values) ]
      
      
      for( j in 1:length(rdi.report) ) {
        
        dummy.cv <- cv.table[[j]]
        
        dummy.states = SID.table[ SID.table$SID == SID.values[j] , ]
        
        dummy.catch <- genrec.table %>%
          filter( DS == 'MRIP' ) %>%
          filter( NEW_STA %in% dummy.states$NEW_STA &
                   FL_REG %in% dummy.states$FL_REG &
                   NC_REG %in% dummy.states$NC_REG ) %>%
          mutate( NEW_MODEN = ifelse( NEW_MODEN == "Priv/Shore", "Priv",
                              ifelse( NEW_MODEN == "Cbt/Hbt", "Cbt_Hbt", NEW_MODEN )) ) %>%
          mutate( NEW_MODEN = toupper(NEW_MODEN) )
        rm(dummy.states)
        
        if( report.type == "annual" ) {
          
          dummy.catch = dummy.catch %>%
            group_by( YEAR, NEW_MODEN ) %>%
            summarise( AB1 = sum( AB1, na.rm=TRUE ),
                       B2  = sum(  B2, na.rm=TRUE ) )
          blah = dummy.catch %>%
            group_by( YEAR ) %>%
            summarise( AB1 = sum( AB1, na.rm=TRUE ),
                       B2 = sum(  B2, na.rm=TRUE ) ) %>%
            mutate( NEW_MODEN = "TOTAL" )
          dummy.catch = bind_rows( dummy.catch, blah ) %>%
            pivot_wider( names_from = NEW_MODEN, values_from = c(AB1,B2), names_glue = "{NEW_MODEN}_{.value}" )
          rm( blah )
          
        } else if( report.type == "detailed" ) {
          
          dummy.catch = dummy.catch %>%
            group_by( YEAR, NEW_MODEN, NEW_STA, WAVE ) %>%
            summarise( AB1 = sum( AB1, na.rm=TRUE ),
                       B2  = sum(  B2, na.rm=TRUE ) ) %>%
            pivot_wider( names_from = NEW_MODEN, values_from = c(AB1,B2), names_glue = "{NEW_MODEN}_{.value}" )
        }
        
        
        if( dim(dummy.cv)[1] > dim(dummy.catch)[1] ) {
          blah = unique(dummy.cv$YEAR)[ unique(dummy.cv$YEAR) %notin% unique(dummy.catch$YEAR) ]
          dummy.catch = bind_rows( dummy.catch, data.frame( YEAR = blah ) ) %>%
            arrange( across( any_of( c("YEAR","NEW_STA","WAVE") ) ) )
          rm( blah )
          
        } else {
          blah = unique(dummy.cv$YEAR)[ unique(dummy.cv$YEAR) %in% unique(dummy.catch$YEAR) ]
          dummy.catch = dummy.catch %>% filter( YEAR %in% blah )
          rm( blah )
        }
        
        dummy.catch[ is.na(dummy.catch) ] = 0
        
        
        ### I then replace the catch estimates in 'dummy.cv' with those from 'dummy.catch'...
        
        # dummy.cv    = dummy.cv    %>% arrange( across( any_of( c("YEAR","NEW_STA","WAVE") ) ) )
        # dummy.catch = dummy.catch %>% arrange( across( any_of( c("YEAR","NEW_STA","WAVE") ) ) )
        
        
        ##* *This section isn't working for S100 Gray Trig*
        
        #col.IDs = colnames(dummy.catch)[ grepl("_AB1",colnames(dummy.catch)) | grepl("_B2",colnames(dummy.catch)) ]
        
        #for( i in 1:length(col.IDs) ) {
        #  for( r in dim(dummy.catch)[1] ) {
        #    dummy.cv[ dummy.cv$YEAR == dummy.catch$YEAR[r] &
        #                dummy.cv$NEW_STA == dummy.catch$NEW_STA[r] &
        #                dummy.cv$WAVE == dummy.catch$WAVE[r], col.IDs[i] ] = as.numeric( dummy.catch[ r , col.IDs[i] ] )
        #  }
        #  rm( r )
        #}
        
        
        ##* *Replace AB1 and B2 values in dummy.cv with dummy.catch values*
        ##* *Drop AB1 and B2 from dummy.cv*
        dummy.cv <- dummy.cv %>%
          select(-CBT_AB1, -CBT_B2,
                 -HBT_AB1, -HBT_B2,
                 -PRIV_AB1, -PRIV_B2,
                 -TOTAL_AB1, -TOTAL_B2)
        
        ##* *Merge dummy.catch and dummy.cv*
        dummy.cv <- merge(dummy.cv, dummy.catch, by = "YEAR")
        
        cv.table[[j]] <- dummy.cv
        rm( col.IDs, i, dummy.catch, dummy.cv )
        
      }
      rm( SID.table, SID.values, j )
      
    }
    
    
    
    
    
  
  ### CARIB SEDARs ###
  ### ----------------
  ###
  } else if( Carib.SEDAR ) {
    
    
    ### CATCH CVs ###
    ### _____________
    ###
    ###     S&T has yet to include Caribbean data in the new file format and, therefore, it is not included in any of our
    ###     tables in RDI. CVs for Caribbean assessments are therefore generated using the "old" approach, wherein we
    ###     sum-up the MRIP-provided "var_ab1" and "var_b2" fields (at the strata level), square-root them, and divide them
    ###     by sum(AB1) and sum(B2) respectively.
    
    
    cv.table <- genrec.table %>%
      group_by( YEAR, NEW_MODEN ) %>%
      summarize( AB1 = sum( as.numeric(AB1), na.rm=TRUE ),
                 B2 = sum( as.numeric(B2), na.rm=TRUE ),
                 VAR_AB1 = sum( as.numeric(VAR_AB1), na.rm=TRUE ),
                 VAR_B2 = sum( as.numeric(VAR_B2), na.rm=TRUE ) ) %>%
      mutate( CV_AB1 = ( sqrt(VAR_AB1) / AB1 ),
              CV_B2 = ( sqrt(VAR_B2) / B2 ) ) %>%
      select( YEAR,NEW_MODEN, AB1,B2, CV_AB1,CV_B2 )
    
    cv.table[ is.na(cv.table) ] = 0
    
    
    cv.total <- genrec.table %>%
      group_by( YEAR ) %>%
      summarize( AB1 = sum( as.numeric(AB1), na.rm=TRUE ),
                 B2 = sum( as.numeric(B2), na.rm=TRUE ),
                 VAR_AB1 = sum( as.numeric(VAR_AB1), na.rm=TRUE ),
                 VAR_B2 = sum( as.numeric(VAR_B2), na.rm=TRUE ) ) %>%
      mutate( CV_AB1 = ( sqrt(VAR_AB1) / AB1 ),
              CV_B2 = ( sqrt(VAR_B2) / B2 ) ) %>%
      select( YEAR,AB1,B2, CV_AB1,CV_B2 )
    
    cv.total[ is.na(cv.total) ] = 0
    

    ### SAMPLE SIZES ###
    ### ________________
    ###
    ###   ...for which I need total number of angler trips and (in parentheses) num.trips that intercepted assessed fish
    ###       For Caribbean assessments, I will have to go back into the i-files to identify the appropriate
    ###       fishing trips ( which is why there is script that separates if( region=="Caribbean" ) )...
    
    st_carib = states
    # st_carib = gsub( "PR", 72, st_carib )     ### MRIP FIPS codes
    # st_carib = gsub( "VI", 78, st_carib )     ### MRIP FIPS codes
    st_carib = gsub( "PR", 20, st_carib )     ### SEFSC (NEW_ST) codes
    st_carib = gsub( "VI", 21, st_carib )     ### SEFSC (NEW_ST) codes
    
    mode_carib = mode_sub
    mode_carib = gsub( "Shore", "3", mode_carib )
    mode_carib = gsub(   "Hbt", "4", mode_carib )
    mode_carib = gsub(   "Cbt", "5", mode_carib )
    mode_carib = gsub(  "Priv", "7", mode_carib )
    
    
    ### Total Num.Trips ###

    total.trips = total.trips[ which( total.trips$sub_reg == "11" ), ]
    total.trips = total.trips[ which( total.trips$state %in% st_carib ), ]
    total.trips = total.trips[ which( total.trips$year %in% first.year:term.year ), ]
    total.trips = total.trips[ which( total.trips$new_moden %in% mode_sub ), ]
    
    total.trips = total.trips %>%
      group_by( year, new_moden ) %>%
      summarize( N = length( unique(id_code) ) ) %>%
      select( year, new_moden, N ) %>%
      rename( YEAR = year, NEW_MODEN = new_moden )
    
    
    ### Number of Positive Trips ###

    ### Although this file is species-specific, I still apply my filters as a double-check...
    pos.trips = pos.trips[ which( pos.trips$sp_code %in% nodc.code ), ]
    ###     ...where 'pos.trips' is already species-specific, but I (re)apply the species filter as a double check...
    pos.trips = pos.trips[ which( pos.trips$year %in% first.year:term.year ), ]
    pos.trips = pos.trips[ which( pos.trips$sub_reg == 11 ), ]
    pos.trips = pos.trips[ which( pos.trips$new_sta %in% states ), ]
    pos.trips = pos.trips[ which( pos.trips$new_moden %in% mode_sub ), ]
    
    pos.ab1.trips = pos.trips[ which( pos.trips$A > 0 | pos.trips$B1 > 0 ), ] %>%
      group_by( year, new_moden ) %>%
      summarize( SS = length( unique(id_code) ) ) %>%
      select( year, new_moden, SS ) %>%
      rename( YEAR = year, NEW_MODEN = new_moden )
    pos.b2.trips = pos.trips[ which( pos.trips$B2 > 0 ), ] %>%
      group_by( year, new_moden ) %>%
      summarize( SS = length( unique(id_code) ) ) %>%
      select( year, new_moden, SS ) %>%
      rename( YEAR = year, NEW_MODEN = new_moden )
    
    
    ### JOINING TABLES ###
    ### __________________
    ###
    cv.table = full_join( cv.table, total.trips, by=c("YEAR","NEW_MODEN"), suffix=c("","_N") )
    cv.table = full_join( cv.table, pos.ab1.trips, by=c("YEAR","NEW_MODEN"), suffix=c("","_ab1") )
    cv.table = full_join( cv.table, pos.b2.trips, by=c("YEAR","NEW_MODEN"), suffix=c("","_b2") )
    ###     ...and where I explicitly add "_ab1" to the end of my "SS" column so as to avoid confusion
    ###         with the "SS_b2" column, which was just added to the table...
    colnames(cv.table)[ which( colnames(cv.table)=="SS" ) ] = "SS_ab1"
    
    ### PIVOT ###
    cv.table = cv.table %>%
      pivot_wider( names_from="NEW_MODEN", values_from=c("AB1","B2","CV_AB1","CV_B2","N","SS_ab1","SS_b2") )
    
    
    ### Adding Columns for TOTAL ###
    cv.table$TOTAL_AB1 = rowSums( cv.table[ grep( "^AB1_",colnames(cv.table) ) ], na.rm=TRUE )
    cv.table$TOTAL_B2 = rowSums( cv.table[ grep( "^B2_",colnames(cv.table) ) ], na.rm=TRUE )
    
    cv.table$TOTAL_CV_AB1 = cv.total$CV_AB1[ cv.total$YEAR %in% cv.table$YEAR ]
    cv.table$TOTAL_CV_B2 = cv.total$CV_B2[ cv.total$YEAR %in% cv.table$YEAR ]
    
    cv.table$TOTAL_N = rowSums( cv.table[ grep( "^N_",colnames(cv.table) ) ], na.rm=TRUE )
    cv.table$TOTAL_SS_ab1 = rowSums( cv.table[ grep( "^SS_ab1",colnames(cv.table) ) ], na.rm=TRUE )
    cv.table$TOTAL_SS_b2 = rowSums( cv.table[ grep( "^SS_b2",colnames(cv.table) ) ], na.rm=TRUE )
    
    
    ### RENAMING COLUMNS ###
    ###     ...for which I'm essentially changing the structure from STAT_MODE to MODE_STAT...
    colnames(cv.table)[ grep( "_Priv$", colnames(cv.table) ) ] = paste0(
      "PRIV_", gsub( "_Priv$","", colnames(cv.table)[grep("_Priv$",colnames(cv.table))] ) )
    colnames(cv.table)[ grep( "_Hbt$", colnames(cv.table) ) ] = paste0(
      "HBT_", gsub( "_Hbt$","", colnames(cv.table)[grep("_Hbt$",colnames(cv.table))] ) )
    colnames(cv.table)[ grep( "_Cbt$", colnames(cv.table) ) ] = paste0(
      "CBT_", gsub( "_Cbt$","", colnames(cv.table)[grep("_Cbt$",colnames(cv.table))] ) )
    colnames(cv.table)[ grep( "_Shore$", colnames(cv.table) ) ] = paste0(
      "SHORE_", gsub( "_Shore$","", colnames(cv.table)[grep("_Shore$",colnames(cv.table))] ) )
    ###     Note that my TOTAL columns are already appropriately defined...
    
    
    cv.table[ is.na(cv.table) ] = 0
    
    
    ### Combining N+SS Columns ###
    cv.table = as.data.frame( cv.table )
    
    cv.table[ ,grep( "_SS", colnames(cv.table) ) ] <- format(
      round( cv.table[ ,grep( "_SS", colnames(cv.table) ) ], 0 ), big.mark="," )
    cv.table[ ,grep( "_N", colnames(cv.table) ) ] <- format(
      round( cv.table[ ,grep( "_N", colnames(cv.table) ) ], 0 ), big.mark="," )
    
    col.IDs <- c( toupper(mode_sub),"TOTAL" )
    for( i in 1:length(col.IDs) ) {
      
      ### AB1 Sample Size Column ###
      cv.table <- unite( cv.table, newcol,
                         c( paste0(col.IDs[i],"_N"),paste0(col.IDs[i],"_SS_ab1") ), sep=" (", remove=FALSE )
      cv.table$newcol <- paste0( cv.table$newcol,")" )
      colnames(cv.table)[ which( colnames(cv.table) == "newcol" ) ] <- paste0(col.IDs[i],"_SS_AB1")
      
      ### B2 Sample Size Column ###
      cv.table <- unite( cv.table, newcol,
                         c( paste0(col.IDs[i],"_N"),paste0(col.IDs[i],"_SS_b2") ), sep=" (", remove=FALSE )
      cv.table$newcol <- paste0( cv.table$newcol,")" )
      colnames(cv.table)[ which( colnames(cv.table) == "newcol" ) ] <- paste0(col.IDs[i],"_SS_B2")
      
      ###   ...and removing the original sample size columns...
      cv.table <- cv.table[ ,!( colnames(cv.table) %in% c( 
        paste0(col.IDs[i],"_SS_ab1"),paste0(col.IDs[i],"_SS_b2"),paste0(col.IDs[i],"_N") ) ) ]
      
    }
    
    ### SEPARATE AND ORDER AB1 & B2 Columns ###
    AB1.cols <- vector()
    B2.cols <- vector()
    for( i in 1:length(col.IDs) ) {
      AB1.cols <- c( AB1.cols, paste0( col.IDs[i], c("_AB1","_CV_AB1","_SS_AB1") ) )
      B2.cols <- c( B2.cols, paste0( col.IDs[i], c("_B2","_CV_B2","_SS_B2") ) )
    }
    cv.table <- cv.table[ ,c("YEAR",AB1.cols,B2.cols) ]
    
    rm( col.IDs, AB1.cols,B2.cols )
    rm( cv.total, total.trips, pos.ab1.trips, pos.b2.trips )
    
    
  }
  
  
  message( paste( "\n", "   ***  !!! --- MRIP Table Constructed --- !!!   ***   " ) )
  
  return( cv.table )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------


CVs.catnum.LACR = function( itis.code, inc.modes, inc.years, cal.ratios = NA, mode.filter = NA,
                            imp.LACR.B2 = 'None', calc.ratios.from = NA, mrip.cv.table = NA,
                            attach.samplesize = TRUE ) {
  
  ###     ...where the first three arguments in this function call represent filters on the CV pull (from RDI):
  ###             'itis.code' for species, 'inc.modes' for modes, and 'inc.years' for years,
  ###       'cal.ratios' contains appropriate LACR:MRIP calibration factors for this particular stock
  ###             and for the mode(s) identified in 'mode.filter'...
  ###
  ###       'imp.LACR.B2' acts as a flag for how LACR discards were treated in this assessment. In particular,
  ###             = 'None' if no imputation was conducted
  ###             = 'la_ratio' if discard rates were calculated from LA catch estimates
  ###             = 'gu_ratio' if discard rates were calculated from GOM-wide estimates
  ###
  ###       In cases where 'imp.LACR.B2' != 'None', the remaining two arguments provide details on how imputations were calculated:
  ###           -- 'calc.ratios.from' identifies the years over which (LA-specific) B2:AB1 catch ratios were calculated
  ###             (e.g., B2:AB1 ratios from 2000:2013 MRIP data, 2016 LACR data, or 2011:2013 MRIP & 2016:2018 LACR data ).
  ###             Note that this argument is only needed when imp.LACR.B2 == 'la_ratio' as, in the Gulf-wide approach,
  ###             discard rates are calculated from the same years as which LA-discards are being imputed...
  ###           -- 'mrip.cv.table' is the table of MRIP catch & associated CV estimates from which the B2:AB1 catch ratios
  ###             were calculated. Note that the GOM states considered in these CV tables will differ based on the
  ###             imputation method being applied ( 'la_ratio' uses LA data , 'gu_ratio' uses MS/AL/FLW data ),
  ###             and both of these may differ from the spatial domain of the assessment (e.g., if it includes SATL states ).
  ###             We therefore construct a separate CV table (from RDI) for this calculation...
  ###
  ###     ...and, lastly, 'attach.samplesize' is a simple flag to indicate whether the raw intercept data should
  ###           be queried and sample.size summaries attached to the final CV table. While sample sizes are included
  ###           in the final table, it takes awhile for this data to be imported given we're producing summaries of
  ###           both positive and total trips (i.e., import the entire intercept table ) and so this flag is a way
  ###           to turn this step-off when debugging other parts of the code...
  
  
  
  ### Raw LACR Estimate Table ###
  ### ---------------------------
  
  
  message( paste( "\n", "   *** Importing -- LACR Catch & Var Estimates   ***   " ) )
  
  con = dbConnect(dbDriver("Oracle"), username = keyring::key_list("SECPR")[1,2],
                  password = keyring::key_get("SECPR", keyring::key_list("SECPR")[1,2]), dbname = "SECPR")
  
  lacr.cv = dbGetQuery( con, paste0("select *
                              from RDI.la_creel_harvest@secapxdv_dblk.sfsc.noaa.gov t
                                       where t.SP_CODE IN (", sprintf("'%s'", paste(itis.code, collapse = "','")),")" ) )
  
  lacr.cv = lacr.cv %>%
    mutate( NEW_MODEN = ifelse( MODE_FX_F == "Private/Shore", "PRIV",
                        ifelse( MODE_FX_F == "Charter Boat" ,  "CBT", NA )) ) %>%
    
    filter( INT_YEAR %in% inc.years & NEW_MODEN %in% toupper(inc.modes) ) %>%
    group_by( INT_YEAR, NEW_MODEN ) %>%
    summarise( AB1     = sum( LA_LANDING, na.rm=TRUE ),
               AB1_VAR = sum( LA_LANVAR , na.rm=TRUE ),
               B2      = sum( LA_DISCARD, na.rm=TRUE ),
               B2_VAR  = sum( LA_DISVAR , na.rm=TRUE ) ) %>%
    
    rename( YEAR = INT_YEAR )
  
  lacr.cv[ is.na(lacr.cv) ] = 0
  
  
  
  
  ### *****************************************************************************************************
  ###
  ### LACR Imputation -- Discards ###
  ### -------------------------------
  ###
  ###     ...for which the script below estimates uncertainties for any LACR discard estimates imputed from
  ###         B2:AB1 catch ratios ( LA vs. Gulf-wide ratios ), as may be needed for 2014-2015 or 2014+
  ###         ( depending on if LDWF has ever collected discard info for this particular species ).
  ###         This CV calculation is based on modified versions of Equations (2) and (5) in S74-DW-10:
  ###
  ###             ratio     = B2 / AB1
  ###             ratio.VAR = ( var(B2)/(AB1^2) ) + ( (B2^2)*var(AB1)/(AB1^4) )
  ###
  ###             var( B2.LA[yr,mode] ) = ( AB1.LA[yr,mode]^2 )*var(ratio) +
  ###                                     ( ratio^2 )*var(AB1.LA[yr,mode]) - var(ratio)*var(AB1.LA[yr,mode])
  ###
  ###     In this script, LACR discards were either:
  ###           (1) considered negligible and not imputed
  ###           (2) imputed from (MRIP) B2:AB1 ratios calculated from LA data
  ###           (3) imputed from (MRIP) B2:AB1 ratios calculated from Gulf-wide data
  ###
  ###     ...where if #1 is true (imp.LACR.B2 == 'None'), none of this section of code needs to be run...
  
  
  if( imp.LACR.B2 != 'None' ) {
    
    
    ###   To start, I identify those years 'missing' a LACR discard estimate (i.e., those requiring imputation ).
    ###   This information is needed in both the LA approach (#2) and the GULF approach (#3) in applying the
    ###   estimated (B2:AB1) catch rates to LACR landings estimates ( as per the imputation of LACR discards ).
    ###   This information is also used by the GULF approach when calculating the catch ratios
    ###   ( catch ratios calculated from the same year as those 'missing' a discard estimate )...
    
    blah = lacr.cv %>%
      filter( YEAR >= 2016 ) %>%
      group_by( YEAR ) %>%
      summarise( B2 = sum( B2, na.rm=TRUE ) )
    
    if( sum( blah$B2, na.rm=TRUE ) == 0 ) {
      impute.years = inc.years                ### ...no LACR discard estimates available for this species
    } else {
      impute.years = 2014:2015
    }
    rm(blah)
    
    
    
    ###   Both of these approaches also require a table of MRIP catch & CV estimates in calculating B2/AB1 catch ratios:
    ###       Approach #2 -- in case catch ratios are to include any LA data between 1981-2013
    ###       Approach #3 -- as catch ratios are calculated entirely from MRIP data (MS-FLW)
    ###   The data included in both of these 'MRIP' tables differs between the approaches, but this distinction is made
    ###   in the main script (i.e., 'mrip.cv.table' is different if method = 'la_ratio' vs. method = 'gu_ratio' ).
    ###   Therefore, all that's needed in this function is to import & format the appropriate CV table from RDI...
    
    con = dbConnect(dbDriver("Oracle"), username = keyring::key_list("SECPR")[1,2],
                    password = keyring::key_get("SECPR", keyring::key_list("SECPR")[1,2]), dbname = "SECPR")
    
    mrip.dummy = dbGetQuery(con,
                            paste0("select * from rdi.apex_cv_data_yr_m@secapxdv_dblk.sfsc.noaa.gov t
                                where t.APP_USER = ", sprintf("'%s'", paste( mrip.cv.table, collapse = "','" ))
                            ))
    
    mrip.dummy = mrip.dummy %>%
      
      select( contains( c("YEAR","CBT",'PRIV') ) & !contains( c("CBT_HBT_","_CHTS", "_N","_SZ") ) ) %>%
      ###     ...to only include those modes for which LACR provides estimates, excluding any 'CHTS' estimates,
      ###       and excluding any MRIP sample size fields ( the LACR sample sizes are used in the final 'lacr.cv' table )...
      
      arrange( YEAR ) %>%
      
      ###     ...reformatting 'mrip.dummy' to match the structure of 'lacr.cv'...
      pivot_longer( !YEAR, names_to = "VARIABLE", values_to = "VALUE" ) %>%
      mutate( NEW_MODEN = gsub( "_.*","", VARIABLE ) ) %>%
      mutate( CATCH_VAR = ifelse( NEW_MODEN == "PRIV", gsub( "PRIV_","", VARIABLE ),
                          ifelse( NEW_MODEN ==  "CBT", gsub(  "CBT_","", VARIABLE ), NA ) ) ) %>%
      select( !VARIABLE ) %>%
      
      pivot_wider( names_from = CATCH_VAR, values_from = VALUE ) %>%
      
      ###     ...converting the CV estimates in 'mrip.dummy' to variances...
      mutate( AB1_VAR = ifelse( is.na(AB1), NA, ( CV_AB1 * AB1 )^2 ),
               B2_VAR = ifelse( is.na(B2 ), NA, ( CV_B2  * B2  )^2 ) ) %>%
      select( YEAR, NEW_MODEN, AB1, AB1_VAR, B2, B2_VAR )
    
  }
  
  
  if( imp.LACR.B2 == 'la_ratio' ) {
    
    
    ###   Approach #2
    ###
    ###         ...wherein LACR discards are imputed from just LA data and so my 'lacr.cv' table
    ###             (by itself) includes all the relevant information needed to calculate catch CVs 2014+ .
    ###             For those years prior to 2014, we need to reference the (MRIP) catch CVs in 'mrip.cv.table'...
    
    
    ### Calculation of Ratios ###
    ### -------------------------
    
    ratios = bind_rows( mrip.dummy, lacr.cv ) %>%
      
      ###   ...which provides the full timeseries of LA catch ( MRIP 1981-2013, LACR 2014+ ), and so we filter
      ###       those years excluded from the calculation of LA catch ratios ( as identified in 'calc.ratios.from' )...
      filter( YEAR %in% calc.ratios.from ) %>%
      
      ###   ...from which we calculate our catch ratios as a ratio-of-sums (summed across years)  :  sum(B2) / sum(AB1)
      group_by( NEW_MODEN ) %>%
      summarise( AB1 = sum( AB1, na.rm=TRUE ),
                  B2 = sum(  B2, na.rm=TRUE ),
                 AB1_var = sum( AB1_VAR, na.rm=TRUE ),
                  B2_var = sum(  B2_VAR, na.rm=TRUE ) ) %>%
      
      mutate( ratio.B2  = B2 / AB1,
              ratio.VAR = ( B2_var/(AB1^2) ) + ( (B2^2)*AB1_var/(AB1^4) ) ) %>%
      ungroup() %>%
      select( NEW_MODEN, ratio.B2, ratio.VAR )
    
    rm( mrip.dummy )
    
    
    
    ### Application of Ratios ###
    ### -------------------------
    
    dummy = lacr.cv %>%
      
      left_join( ratios, by = 'NEW_MODEN' ) %>%
      
      mutate( ratio.B2  = ifelse( YEAR %in% impute.years, ratio.B2 , NA ),
              ratio.VAR = ifelse( YEAR %in% impute.years, ratio.VAR, NA ) ) %>%
      mutate( B2     = ifelse( is.na(ratio.B2),   B2    , AB1*ratio.B2 ),
              B2_VAR = ifelse( is.na(ratio.B2),   B2_VAR,
                               ( (AB1^2)*ratio.VAR ) + ( (ratio.B2^2)*AB1_VAR ) - ( ratio.VAR*AB1_VAR ) ) ) %>%
      select( !c( ratio.B2, ratio.VAR ) )
      # mutate( CV_AB1 = sqrt( AB1_VAR ) / AB1,
      #         CV_B2  = sqrt(  B2_VAR ) /  B2 )
    
    lacr.cv = dummy
    rm( dummy, ratios )
    
    
    
  } else if( imp.LACR.B2 == 'gu_ratio' ) {
    
    
    ###   Approach #3
    ###
    ###         ...wherein LACR discards are imputed from Gulf-wide ratios ( excluding LA ) and
    ###             so I only need the (MRIP) catch & variance estimates in 'mrip.cv.table'...
    
    
    ### Calculation of Ratios ###
    ### -------------------------
    
    ratios = mrip.dummy %>%
      
      filter( YEAR %in% impute.years ) %>%
      
      group_by( YEAR, NEW_MODEN ) %>%
      summarise( AB1 = sum( AB1,na.rm=TRUE ),
                  B2 = sum(  B2,na.rm=TRUE ),
                 AB1_var = sum( AB1_var,na.rm=TRUE ),
                  B2_var = sum(  B2_var,na.rm=TRUE ) ) %>%
      
      mutate( ratio.B2  = B2 / AB1,
              ratio.VAR = ( B2_var/(AB1^2) ) + ( (B2^2)*AB1_var/(AB1^4) ) ) %>%
      ungroup() %>%
      select( NEW_MODEN, ratio.B2, ratio.VAR )
    
    rm( mrip.dummy )
    
    
    
    ### Application of Ratios ###
    ### -------------------------
    
    dummy = lacr.cv %>%
      
      left_join( ratios, by = c('YEAR','NEW_MODEN') ) %>%
      
      mutate( ratio.B2  = ifelse( YEAR %in% impute.years, ratio.B2 , NA ),
              ratio.VAR = ifelse( YEAR %in% impute.years, ratio.VAR, NA ) ) %>%
      mutate( B2     = ifelse( is.na(ratio.B2),   B2    , AB1*ratio.B2 ),
              B2_VAR = ifelse( is.na(ratio.B2),   B2_VAR,
                               ( (AB1^2)*ratio.VAR ) + ( (ratio.B2^2)*AB1_VAR ) - ( ratio.VAR*AB1_VAR ) ) ) %>%
      select( !c( ratio.B2, ratio.VAR ) )
      # mutate( CV_AB1 = sqrt( AB1_VAR ) / AB1,
      #         CV_B2  = sqrt(  B2_VAR ) /  B2 )
    
    lacr.cv = dummy
    rm( dummy, ratios )
    
    
  }
  ### *****************************************************************************************************
  
  
  
  lacr.cv.tpwdB2 = lacr.cv
  ###     ...wherein I save a copy of 'lacr.cv' here as these raw estimates
  ###       ( before applying any MRIP:state calibration factors ) are what are used to impute TPWD discards
  ###       when LA-specific (B2:AB1) discard ratios are applied...
  ###    Note that this object is included in the final return() statement of this function...
  
  
  
  ### Reapplying the MRIP-FES calibration ###
  ### ---------------------------------------
  
  if( !all( is.na(cal.ratios) ) ) {
    
    dummy = lacr.cv %>%
      mutate( AB1_VAR = ifelse( NEW_MODEN %in% mode.filter,
                                ( (AB1^2)*cal.ratios[ grepl('var.AB1',names(cal.ratios)) ] ) +
                                ( (cal.ratios[ grepl('ratio.AB1',names(cal.ratios)) ]^2)*AB1_VAR ) -
                                ( cal.ratios[ grepl('var.AB1',names(cal.ratios)) ]*AB1_VAR ), AB1_VAR ),
              AB1     = ifelse( NEW_MODEN %in% mode.filter,
                                AB1 * cal.ratios[ grepl('ratio.AB1',names(cal.ratios)) ], AB1 ),
              B2_VAR  = ifelse( NEW_MODEN %in% mode.filter,
                                ( ( B2^2)*cal.ratios[ grepl('var.B2',names(cal.ratios)) ] ) +
                                ( (cal.ratios[ grepl('ratio.B2',names(cal.ratios)) ]^2 )* B2_VAR ) -
                                ( cal.ratios[ grepl('var.B2',names(cal.ratios)) ] * B2_VAR ),  B2_VAR ),
              B2      = ifelse( NEW_MODEN %in% mode.filter,
                                B2 * cal.ratios[ grepl('ratio.B2',names(cal.ratios)) ] ,  B2 ) ) %>%
      # mutate( AB1_CV = sqrt( AB1_VAR ) / AB1,
      #          B2_CV = sqrt(  B2_VAR ) /  B2 ) %>%
      # select( YEAR, NEW_MODEN, AB1, AB1_VAR, AB1_CV, B2, B2_VAR, B2_CV )
      select( YEAR, NEW_MODEN, AB1, AB1_VAR, B2, B2_VAR )
    
    
    lacr.cv = dummy
    rm( dummy )
    
  }
  
  
  ### TOTAL (annual) catch & uncertainty ###
  ### --------------------------------------
  
  dummy = lacr.cv %>%
    group_by( YEAR ) %>%
    summarise( AB1     = sum( AB1    , na.rm=TRUE ),
               AB1_VAR = sum( AB1_VAR, na.rm=TRUE ),
               B2      = sum(  B2    , na.rm=TRUE ),
               B2_VAR  = sum(  B2_VAR, na.rm=TRUE ) ) %>%
    mutate( NEW_MODEN = "Total" )
    # mutate( AB1_CV = sqrt(AB1_VAR) / AB1,
    #          B2_CV = sqrt( B2_VAR) / B2 )
  
  lacr.cv = lacr.cv %>% bind_rows( dummy ) %>% arrange( YEAR, NEW_MODEN )
  rm( dummy )
  
  
  ###     ...and applying some formatting to these fields...
  
  lacr.cv = lacr.cv %>% mutate( NEW_MODEN = toupper(NEW_MODEN) )
  
  
  
  
  ### SAMPLE SIZE -- number of TRIPS & PSU's ###
  ### ------------------------------------------
  
  if( attach.samplesize ) {
    
    message( paste( "\n", "   *** Importing -- LACR Sample Sizes (numTrips) ***   " ) )
    
    con = dbConnect(dbDriver("Oracle"), username = keyring::key_list("SECPR")[1,2],
                    password = keyring::key_get("SECPR", keyring::key_list("SECPR")[1,2]), dbname = "SECPR")
    
    # modes.lacr = mode_sub
    # modes.lacr = modes.lacr[ modes.lacr %in% c("Priv","Cbt") ]
    # if( "Priv" %in% modes.lacr ) {
    #   modes.lacr =  gsub( "Priv", "3: Private Offshore", modes.lacr )
    #   modes.lacr = c( "2: Shore", modes.lacr )
    #   modes.lacr = c( "1: Private Inshore", modes.lacr )
    # }
    # modes.lacr = gsub( "Cbt",  "4: Charter", modes.lacr )
    
    lacr.ss = dbGetQuery(con,
                         paste0("select *
                          from RDI.la_creel_raw@secapxdv_dblk.sfsc.noaa.gov t"
                                # from RDI.v_la_creel_raw@secapxdv_dblk.sfsc.noaa.gov t"
                                # where t.REF_DESC_4 IN (", sprintf("'%s'", paste(modes.lacr, collapse = "','")),")",
                                # " AND t.ITIS_CODE IN (", sprintf("'%s'", paste(itis.code, collapse = "','")),")"
                         ))
    ###     ...where the entire table needs to be pulled as we provide sample size information on the total number of trips/PSUs
    ###         ( negative + positive ), not just the number of positive trips/PSUs -- and so no ITIS_CODE filter applied
    ###     ...and where the mode filter is not applied on the initial import (above) but in the formatting below...
    
    
    # ###   I also created a vector of federal holidays, with the assumption it would be needed to construct
    # ###     my DAYTYPE field (i.e., weekdays (Mon-Thur) vs. weekends (Fri-Sun, including holidays) ).
    # ###     However, LDWF includes a DAY_OF_WEEK_TYPE field in their intercept data and so I don't actually
    # ###     need this step. I kept the script I wrote for reference, but comment it out...
    # lacr.holidays = as.Date( c( timeDate::USNewYearsDay(     1981:max(inc.years) ),
    #                             timeDate::USMLKingsBirthday( 1981:max(inc.years) ),
    #                             timeDate::USPresidentsDay(   1981:max(inc.years) ),
    #                             timeDate::USMemorialDay(     1981:max(inc.years) ),
    #                             timeDate::USIndependenceDay( 1981:max(inc.years) ),
    #                             timeDate::USLaborDay(        1981:max(inc.years) ),
    #                             timeDate::USColumbusDay(     1981:max(inc.years) ),
    #                             timeDate::USVeteransDay(     1981:max(inc.years) ),
    #                             timeDate::USThanksgivingDay( 1981:max(inc.years) ),
    #                             timeDate::USChristmasDay(    1981:max(inc.years) ) ) )
    
    dummy = lacr.ss %>%
      
      ###     ...modify ASSIGN_DATE field to 'agree' with SAMPLE_YEAR
      mutate( ASSIGN_DATE = ifelse( as.numeric(SAMPLE_YEAR) > as.numeric(substr(ASSIGN_DATE,1,4)),
                                    paste0( as.character(SAMPLE_YEAR),"-01-01" ),
                            ifelse( as.numeric(SAMPLE_YEAR) < as.numeric(substr(ASSIGN_DATE,1,4)),
                                    paste0( as.character(SAMPLE_YEAR),"-12-31" ),
                                    substr( as.character(ASSIGN_DATE),1,10 ) ) ) ) %>%
      
      mutate( YEAR  = as.integer( substr( ASSIGN_DATE, 1,4 ) ),
              MONTH = as.integer( substr( ASSIGN_DATE, 6,7 ) ),
              DAY   = as.integer( substr( ASSIGN_DATE, 9,10 ) ) ) %>%
      mutate( WAVE  = ifelse( MONTH %in%  1:2 , 1,
                      ifelse( MONTH %in%  3:4 , 2,
                      ifelse( MONTH %in%  5:6 , 3,
                      ifelse( MONTH %in%  7:8 , 4,
                      ifelse( MONTH %in%  9:10, 5,
                      ifelse( MONTH %in% 11:12, 6, NA )))))) ) %>%
      # mutate( DAYTYPE = ifelse( weekdays(as.Date(ASSIGN_DATE)) %in% c('Friday','Saturday',"Sunday"), 'we',
      #                   ifelse(          as.Date(ASSIGN_DATE)  %in%       lacr.holidays,       'we', 'wd' )) ) %>%
      mutate( DAYTYPE = ifelse( DAY_OF_WEEK_TYPE == "Week"   , "wd",
                        ifelse( DAY_OF_WEEK_TYPE == "Weekend", "we", NA ) ) ) %>%
      
      mutate( DS_MODE  = as.character( substr( REF_DESC_4, 1,1 ) ),
              DS_MODEN = as.character( substr( REF_DESC_4, 4,100000 ) ) ) %>%
      mutate( NEW_MODEN = ifelse( DS_MODEN == "Charter", "CBT",
                          ifelse( DS_MODEN %in% c("Private Inshore","Private Offshore"), "PRIV",
                          # ifelse( DS_MODEN == "Shore", "SHORE", NA ) ) ),
                          ifelse( DS_MODEN == "Shore", "PRIV", NA ) ) ) ) %>%
      ###       ...where I set all 'SHORE' intercepts to NEW_MODEN == 'PRIV' because LDWF combines these modes in estimation...
      
      # mutate( DS_DISP  = as.character( substr( REF_DESC_5, 1,1 ) ),
      #         DS_DISPN = as.character( substr( REF_DESC_5, 4,100000 ) ) ) %>%
      # ###       ...where DS_DISPN (i.e., catch disposition) is needed to distinguish catch types -- type-A vs. B1 vs. B2...
      mutate( DS_DISP  = as.character( substr( OBSERVATIONTYPE, 1,1 ) ),
              DS_DISPN = as.character( substr( OBSERVATIONTYPE, 4,100000 ) ) ) %>%
      
      mutate( CATCH_TYPE = ifelse( DS_DISPN %in% c("Counted"),             "A",
                           ifelse( DS_DISPN %in% c("Reported","Bait"),     "B1",
                           ifelse( DS_DISPN %in% c("Under Size","Other"),  "B2", NA ))) ) %>%
      
      mutate( TRIP_KEY = paste0( YEAR, "LA", sprintf( "%07s", INTERCEPT_DETAILS_ID ),
                                 "L" , sprintf( "%04s", CONTROL_NBR ), sprintf( "%03s", TRIP_NBR ) ),
              PSU_ID = paste0( BASIN_ID, SITE_CODE, YEAR, MONTH, DAY, SAMPLE_PERIOD, DAYTYPE ) ) %>%
      ###     ...where TRIP_KEY is a unique identifier for individual fishing trips (i.e., interview/vessel level)
      ###          and   PSU_ID is a unique identifier for individual survey assignments (i.e., survey-day ).
      ###       Note the inclusion of SAMPLE_PERIOD and DAYTYPE in PSU_ID as LACR site assignments are drawn
      ###       separately for each calendar week and weekday/weekend (respectively)...
      
      filter( YEAR %in% inc.years,
              NEW_MODEN %in% toupper(mode_sub) )
    
    # rm( lacr.holidays )
    
    dummy1 = dummy %>%
      mutate( NEW_MODEN = toupper(NEW_MODEN) ) %>%
      group_by( YEAR, NEW_MODEN ) %>%
      summarise( AT_Tot  = length( unique( TRIP_KEY ) ),
                 AT_AB1  = length( unique( TRIP_KEY[ ITIS_CODE %in% itis.code &
                                                       CATCH_TYPE %in% c('A','B1') & !is.na(CATCH_TYPE) ] ) ),
                 AT_B2   = length( unique( TRIP_KEY[ ITIS_CODE %in% itis.code &
                                                       CATCH_TYPE == 'B2' & !is.na(CATCH_TYPE) ] ) ),
                 PSU_Tot = length( unique( PSU_ID ) ),
                 PSU_AB1 = length( unique( PSU_ID[ ITIS_CODE %in% itis.code &
                                                     CATCH_TYPE %in% c('A','B1') & !is.na(CATCH_TYPE) ] ) ),
                 PSU_B2  = length( unique( PSU_ID[ ITIS_CODE %in% itis.code &
                                                     CATCH_TYPE == 'B2' & !is.na(CATCH_TYPE) ] ) ) )
    # pivot_wider( names_from = "NEW_MODEN", values_from = c("AT_AB1","AT_B2,"AT_Tot","PSU_AB1","PSU_B2,"PSU_Tot"),
    #              names_glue = "{NEW_MODEN}_{.value}" )
    
    dummy2 = dummy %>%
      group_by( YEAR ) %>%
      summarise( AT_Tot  = length( unique( TRIP_KEY ) ),
                 AT_AB1  = length( unique( TRIP_KEY[ ITIS_CODE %in% itis.code &
                                                       CATCH_TYPE %in% c('A','B1') & !is.na(CATCH_TYPE) ] ) ),
                 AT_B2   = length( unique( TRIP_KEY[ ITIS_CODE %in% itis.code &
                                                       CATCH_TYPE == 'B2' & !is.na(CATCH_TYPE) ] ) ),
                 PSU_Tot = length( unique( PSU_ID ) ),
                 PSU_AB1 = length( unique( PSU_ID[ ITIS_CODE %in% itis.code &
                                                     CATCH_TYPE %in% c('A','B1') & !is.na(CATCH_TYPE) ] ) ),
                 PSU_B2  = length( unique( PSU_ID[ ITIS_CODE %in% itis.code &
                                                     CATCH_TYPE == 'B2' & !is.na(CATCH_TYPE) ] ) ) ) %>%
      mutate( NEW_MODEN = "TOTAL" )
    # pivot_wider( names_from = "NEW_MODEN", values_from = c("AT_AB1","AT_B2,"AT_Tot","PSU_AB1","PSU_B2,"PSU_Tot"),
    #              names_glue = "{NEW_MODEN}_{.value}" )
    
    dummy = bind_rows( dummy1, dummy2 ) %>%
      arrange( YEAR, NEW_MODEN )
    
    
    blah = lacr.cv %>% full_join( dummy, by=c("YEAR","NEW_MODEN") ) %>% arrange( YEAR, NEW_MODEN )
    ###     ...where we use a full_join() to make sure all information is included in 'lacr.cv', including...
    ###         -- years where sampling occurred ( N > 0 ) but the species-of-interest just wasn't intercepted ( catch = 0 )
    ###             (i.e., all trips in a given fishing year were a negative intercept for this spp )
    ###         -- years where LACR catch & CV estimates were imputed from other data sources, and so not actually
    ###             generated from observed data (e.g., for LACR in 2014-2015, PSU_B2 = 0 and AT_B2 = 0 regardless of
    ###             whether LACR discards were imputed or not )...
    
    blah[ is.na(blah) ] = 0
    ###     Additionally, as these fields (in 'lacr.cv') are to be combined with the associated MRIP catch/CV estimates
    ###     (in 'cv.table'), we also change any <NA> values to zero's. Note that this step occurs outside of the
    ###     CVs.catnum.LACR() function, as similar aggregations will be needed if we impute any TPWD catch estimates...
    
    lacr.cv = blah
    rm( blah, dummy, dummy1, dummy2 )
    
    
    message( paste( "\n", "   ***  !!! --- Full LACR Table Constructed --- !!!   ***   " ) )
    
  }
  
  
  return( list( lacr.cv, lacr.cv.tpwdB2 ) )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------


CVs.catnum.TPWD = function( tpwd.code, inc.modes, inc.years, genrec.cat.table,
                            cal.ratios = NA, mode.filter = NA,
                            imp.TPWD.8183 = 'None',
                            imp.TPWD.B2 = 'None', genrec.cv.table = NA,
                            attach.samplesize = TRUE ) {
  
  ###     ...where the first three arguments in this function call represent filters on the CV pull (from RDI):
  ###             'tpwd.code' for species, 'inc.modes' for modes, and 'inc.years' for years,
  ###       'cal.ratios' contains appropriate LACR:MRIP calibration factors for this particular stock
  ###             and for the mode(s) identified in 'mode.filter'...
  ###
  ###       'genrec.cat.table' is the table of GenRec catch estimates, which is needed if...
  ###           -- CVs are being calculated for a non-target species group, in that CVs from the 'OTHER FINFISH' group
  ###             ( assumed representative of all non-target spp ) are to be combined with species-specific catch estimates
  ###             ( the latter being extracted from this 'genrec.cat.table' )...
  ###
  ###       'imp.TPWD.8183' acts as a flag for whether 1981 - (May)1983 TPWD estimates are to be imputed. In particular,
  ###             = 'None' if no imputation was conducted
  ###             = 'avg_83_85' if historic estimates were set equal to averages over subsequent years (1983-85)
  ###
  ###       'imp.TPWD.B2' acts as a flag for how TPWD discards were treated in this assessment. In particular,
  ###             = 'None' if no imputation was conducted
  ###             = 'la_ratio' if discard rates were calculated from LA catch estimates
  ###             = 'gu_ratio' if discard rates were calculated from GOM-wide estimates
  ###       In cases where 'imp.TPWD.B2' != 'None'...
  ###           -- 'genrec.cv.table' is the table of catch & associated CV estimates from which the B2:AB1 catch ratios were calculated.
  ###             When defined, this object is composed of two tables:
  ###                   (1) one of LACR estimates (2014+), which is a table of raw & imputed estimates exported from the
  ###                       CV.catnum.LACR() function and is the first element in the 'genrec.cv.table' list...
  ###                   (2) one of MRIP estimates, which is a table of MRIP estimates constructed from RDI CV reports, the
  ###                       name of which is stored (as text) as the second element in the 'genrec.cv.table' list...
  ###                       This MRIP table will contain estimates from the entire GOM (LA 2013+, MS-FLW 1981+) for assessments
  ###                       where GOM-wide discard rates are being applied, but only LA estimates (1981-2013) for assessments
  ###                       where LA-specific discard rates are being used...
  ###
  ###     ...and, lastly, 'attach.samplesize' is a simple flag to indicate whether the raw intercept data should
  ###           be queried and sample.size summaries attached to the final CV table. While sample sizes are included
  ###           in the final table, it takes awhile for this data to be imported given we're producing summaries of
  ###           both positive and total trips (i.e., import the entire intercept table ) and so this flag is a way
  ###           to turn this step-off when debugging other parts of the code...
  
  
  
  ### Raw TPWD Estimate Table ###
  ### ---------------------------
  
  
  message( paste( "\n", "   *** Importing -- TPWD Catch & Var Estimates   ***   " ) )
  
  con = dbConnect(dbDriver("Oracle"), username = keyring::key_list("SECPR")[1,2],
                  password = keyring::key_get("SECPR", keyring::key_list("SECPR")[1,2]), dbname = "SECPR")
  
  
  ###   CVs for the TPWD survey are calculated from the native 'LANDINGS_SE' field of the raw estimate files
  ###   ( as provided by TPWD ). However, because we're using TPWD's standard errors, we're also limited to
  ###   those estimates provided by TPWD, wherein TPWD has a list of TARGET SPECIES for which estimates are
  ###   provided at the species levels -- all other species/taxa are lumped into a single 'OTHER FINFISH' category.
  ###   This is problematic in assessments for NON-TARGET SPECIES, for which TPWD doesn't provide estimates at the
  ###   species-level. As part of our regular workflow, the SEFSC already partitions landings estimates of the
  ###   TPWD 'OTHER FINISH' group amongst species, but this workflow does not (currently) carry the associated
  ###   SEs through this calculation. As an immediate solution, the script below assumes the standard errors of the
  ###   combined 'OTHER FINFISH' group are representative of all (non-target) species, but this may be revisited
  ###   in the future...
  ###
  ###   As such, the script below includes a check of target vs. non-target species, wherein the species-specific
  ###   landings & SE estimates are used for target species (as provided by TPWD), but the CVs of non-target species
  ###   are calculated from the estimates provided for the OTHER FINFISH group...
  
  
  if( tpwd.code %in% c(507,544,594,597,602,611,613,614,616,621,625,629,634,656,671,681,763,772,787,818,996) ) {
    
    
    ###   -- TPWD TARGET SPECIES --
    ###
    ###   ...which includes...
    ###       507 = gray triggerfish , 544 = little tunny ,        594 = vermilion snapper, 597 = dolphin ,
    ###       602 = atl. croaker ,     611 = gafftopsail catfish , 613 = sand seatrout ,    614 = spotted seatrout ,
    ###       616 = southern flounder, 621 = sheepshead ,          625 = black drum ,       629 = red drum ,
    ###       634 = atl. spadefish ,   656 = cobia ,               671 = lane snapper ,     681 = spanish mackerel
    ###       763 = atl. sharpnose shark , 772 = king mackerel ,   787 = blacktip shark ,
    ###       818 = red snapper ,          996 = greater amberjack
    
    tpwd.cv = dbGetQuery( con, paste0("select *
                              from RDI.tpwd_estimates@secapxdv_dblk.sfsc.noaa.gov t
                                where t.SPECIES_CODE IN (", sprintf("'%s'", paste(tpwd.code, collapse = "','")),")" ) )
    
  } else {
    
    
    ###   -- TPWD NONTARGET SPECIES --
    
    tpwd.cv = dbGetQuery( con, paste0("select *
                              from RDI.tpwd_estimates@secapxdv_dblk.sfsc.noaa.gov t
                                where t.SPECIES = 'OTHER FINFISH'" ) )
  }
  
  tpwd.cv = tpwd.cv %>%
    
    mutate( NEW_MODE  = ifelse( ACTIVITY %in% c(1,3), 4,          ifelse( ACTIVITY == 2, 3, NA )),
            NEW_MODEN = ifelse( ACTIVITY %in% c(1,3), "PRIV",     ifelse( ACTIVITY == 2, "CBT", NA )) ) %>%
    
    filter( CYEAR %in% inc.years & NEW_MODEN %in% toupper(inc.modes) )
  
  
  ###   These (raw) TPWD estimates are then summarized, where WAVE specific estimates are needed if TPWD estimates
  ###   are being imputed for 1981-1983...
  if( imp.TPWD.8183 != 'None' ) {
    tpwd.cv = tpwd.cv %>% group_by( CYEAR, WAVE, NEW_MODEN )
  } else {
    tpwd.cv = tpwd.cv %>% group_by( CYEAR, NEW_MODEN )
  }
  
  tpwd.cv = tpwd.cv %>%
    summarise( AB1     = sum( LANDINGS,       na.rm=TRUE ),
               AB1_VAR = sum( LANDINGS_SE^2 , na.rm=TRUE ) ) %>%
    
    rename( YEAR = CYEAR )
  
  tpwd.cv[ is.na(tpwd.cv) ] = 0
  
  
  ###   ...where, if the assessment is for a (TPWD) 'target' species, we're ready to move on to the next (imputation) step,
  ###       in that no additional modifications are needed to 'tpwd.cv'...
  ###
  ###     Conversely, for assessments of 'non-target' taxa, the AB1 & VAR estimates in the above 'tpwd.cv' table
  ###       (which are specific to 'OTHER FINFISH') still need to be converted to those for the species-of-interest.
  ###       In these cases, we will assume the CVs estimated for the overall 'OTHER FINFISH' group is representative of
  ###       that for all of non-target species, and so we calculate a CV from the 'tpwd.cv' table. We then join these
  ###       'OTHER FINFISH' CVs with the actual catch estimate for the species-of-interest, the latter of which is
  ###       extracted from our 'genrec.cat.table' table...
  
  if( tpwd.code %notin% c(507,544,594,597,602,611,613,614,616,621,625,629,634,656,671,681,763,772,787,818,996) ) {
    
    
    if( imp.TPWD.8183 != 'None' ) {
      
      tpwd.cv = tpwd.cv %>%
        mutate( CV = sqrt(AB1_VAR) / AB1 ) %>%
        select( YEAR, WAVE, NEW_MODEN, CV )
      
      dummy = genrec.cat.table %>%
        filter( DS == 'TPWD' ) %>%
        mutate( NEW_MODEN = toupper(NEW_MODEN) ) %>%
        group_by( YEAR, WAVE, NEW_MODEN ) %>%
        summarise( AB1 = sum( AB1, na.rm=TRUE ) )
      
      blah = left_join( dummy, tpwd.cv, by = c('YEAR','WAVE','NEW_MODEN') ) %>%
        mutate( VAR = (CV*AB1)^2 ) %>%
        select( -CV )
      
    } else {
      
      tpwd.cv = tpwd.cv %>%
        mutate( CV = sqrt(AB1_VAR) / AB1 ) %>%
        select( YEAR, NEW_MODEN, CV )
      
      dummy = genrec.cat.table %>%
        filter( DS == 'TPWD' ) %>%
        mutate( NEW_MODEN = toupper(NEW_MODEN) ) %>%
        group_by( YEAR, NEW_MODEN ) %>%
        summarise( AB1 = sum( AB1, na.rm=TRUE ) )
      
      blah = left_join( dummy, tpwd.cv, by = c('YEAR','NEW_MODEN') ) %>%
        mutate( VAR = (CV*AB1)^2 ) %>%
        select( -CV )
    }
    
    tpwd.cv = blah
    rm( dummy, blah )
    
  } else {
    
    tpwd.cv = tpwd.cv %>% rename( VAR = AB1_VAR )
    
  }
  
  
  
  ### *****************************************************************************************************
  ###
  ### TPWD Imputation -- 1981-(May) 1983 ###
  ### --------------------------------------
  ###
  ###     ...for which the script below estimates uncertainties for any TPWD landings estimates being imputed
  ###         for those (historical) years over which the 'current' TPWD survey didn't operate (1981-1983).
  ###         These imputations, when conducted, are calculated as the average of TPWD estimates from
  ###         subsequent years (1983-1985), which are based on standard statistical equations:
  ###
  ###             AB1 = sum(AB1) / N
  ###             VAR = sum(VAR) / N^2
  ###
  ###     In this script, historical TPWD landings (1981-1983) were either:
  ###           (1) considered negligible and not imputed
  ###           (2) imputed from the average landings estimated in subsequent years (1983-1985)
  ###
  ###     ...where if #1 is true (imp.TPWD.8183 == 'None'), none of this section of code needs to be run...
  
  
  if( imp.TPWD.8183 != 'None' ) {
    
    dummy = tpwd.cv %>% filter( YEAR %in% 1983:1985 ) %>% ungroup()
    
    if( dim(dummy)[1] > 0 ) {
      ###   ...where, even if the imputation flag is set to something other than 'None', historical estimates will only
      ###     be calculated ( as non-zero estimates ) if some landings were observed between 1983-1985...
      
      dummy = dummy %>%
        group_by( WAVE, NEW_MODEN ) %>%
        summarise( AB1 = sum( AB1, na.rm=TRUE ),
                   VAR = sum( VAR, na.rm=TRUE ) ) %>%
        mutate( AB1 = ifelse( WAVE %in% 1:2, AB1/2, AB1/3 ),
                VAR = ifelse( WAVE %in% 1:2, VAR/4, VAR/9 ) ) %>%
        ###     ...where we have two years of estimates (1984-1985) available for waves 1-2 ( N=2 )
        ###       and three years of estimates (1983-1985) available for waves 3-6 ( N=3 )...
        ungroup()
      
      
      ###       ...I then duplicate the above (by-wave) table appropriately, such that there is a separate row
      ###             for each year-wave combination for which TPWD landings are being imputed:
      ###                   -- need 3 rows for the 3 years missing waves 1-2 estimates (1981-1983)
      ###                   -- need 2 rows for the 2 years missing waves 3-6 estimates (1981-1982)
      ###             I also update the variables for my imputed TPWD landings (to represent previous years/waves)...
      
      dummy.12 = dummy %>%
        arrange( WAVE, NEW_MODEN ) %>%
        filter(  WAVE %in% 1:2 ) %>%
        uncount( 3 )
      dummy.12 = dummy.12 %>%
        mutate( YEAR = rep( 1981:1983, times = (dim(dummy.12)[1])/3 ) )
      
      dummy.36 = dummy %>%
        arrange( WAVE, NEW_MODEN ) %>%
        filter(  WAVE %in% 3:6 ) %>%
        uncount( 2 )
      dummy.36 = dummy.36 %>%
        mutate( YEAR = rep( 1981:1982, times = (dim(dummy.36)[1])/2 ) )
      
      avg.cat = rbind( dummy.12, dummy.36 )
      rm( dummy, dummy.12, dummy.36 )
      
      avg.cat = avg.cat %>% select( YEAR, WAVE, NEW_MODEN, AB1, VAR )
      
      
      ###   I then bind the (1981-1983) imputations with the raw 'tpwd.cv' estimates...
      tpwd.cv = tpwd.cv %>%
        bind_rows( avg.cat ) %>%
        arrange( YEAR, WAVE, NEW_MODEN ) %>%
        
        ###   ...and resummarize the table (i.e., sum ) to remove the WAVE field, which is no longer needed...
        group_by( YEAR, NEW_MODEN ) %>%
        summarize( AB1 = sum( AB1, na.rm=TRUE ),
                   VAR = sum( VAR, na.rm=TRUE ) )
      
      rm( avg.cat )
      
      
    }
    
  }
  
  
  
  ### *****************************************************************************************************
  ###
  ### TPWD Imputation -- Discards ###
  ### -------------------------------
  ###
  ###     ...for which the script below estimates uncertainties for any TPWD discard estimates imputed from
  ###         B2:AB1 catch ratios ( LA vs. Gulf-wide ratios ), which have never been estimated as part of the
  ###         TPWD survey. This CV calculation is based on modified versions of Equations (2) and (5) in S74-DW-10:
  ###
  ###             ratio     = B2 / AB1
  ###             ratio.VAR = ( var(B2)/(AB1^2) ) + ( (B2^2)*var(AB1)/(AB1^4) )
  ###
  ###             B2.TX[yr,mode] = AB1.TX[yr,mode] * ratio[yr,mode]
  ###             var( B2.TX[yr,mode] ) = ( AB1.TX[yr,mode]^2 )*var(ratio) +
  ###                                     ( ratio^2 )*var(AB1.TX[yr,mode]) - var(ratio)*var(AB1.TX[yr,mode])
  ###
  ###     In this script, TPWD discards were either:
  ###           (1) considered negligible and not imputed
  ###           (2) imputed from (MRIP+LACR) B2:AB1 ratios calculated from LA data
  ###           (3) imputed from (MRIP) B2:AB1 ratios calculated from Gulf-wide data
  ###
  ###     ...where if #1 is true (imp.TPWD.B2 == 'None'), none of this section of code needs to be run...
  
  
  if( imp.TPWD.B2 != 'None' ) {
    
    ###   Both of the imputation approaches require a table of catch & CV estimates to calculate B2/AB1 discard rates:
    ###       Approach #2 --  LA discard rates -- table of both MRIP (LA 1981-2013) and LACR (2014+) catch estimates,
    ###                       with the latter including any LACR discard estimates that were imputed for this assessment...
    ###       Approach #3 -- GOM discard rates -- table of MRIP (LA 1981-2013, MS-FLW 1981+) and LACR estimates (2014+)...
    ###   The data included in both of these 'MRIP' tables differs between the approaches, but this distinction is made
    ###   in the main script (i.e., first element of 'genrec.cv.table' is different if method = 'la_ratio' vs. method = 'gu_ratio' )...
    
    
    ### TABLE OF GENREC DISCARD RATES ###
    ### ---------------------------------
    
    ###   In both methods, some table of MRIP estimates needs to be imported (from RDI) and so we start there...
    
    mrip.cv.table = genrec.cv.table[[1]]
    ###     ...where 'mrip.cv.table' identifies the report name (in RDI) of MRIP-CV estimates either specific to
    ###       Louisiana (i.e., 'la_ratio' approach ) or for the entire Gulf of Mexico (i.e., 'gu_ratio' approach).
    ###       In either case, this MRIP table is imported, formatted, and joined with the LACR table
    ###       ( which is the second element in 'genrec.cv.table' )...
    
    con = dbConnect(dbDriver("Oracle"), username = keyring::key_list("SECPR")[1,2],
                    password = keyring::key_get("SECPR", keyring::key_list("SECPR")[1,2]), dbname = "SECPR")
    
    mrip.dummy = dbGetQuery(con,
                            paste0("select * from rdi.apex_cv_data_yr_m@secapxdv_dblk.sfsc.noaa.gov t
                                where t.APP_USER = ", sprintf("'%s'", paste( mrip.cv.table, collapse = "','" ))
                            ))
    rm( mrip.cv.table )
    
    mrip.dummy = mrip.dummy %>%
      
      select( contains( c("YEAR","CBT",'PRIV') ) & !contains( c("CBT_HBT_","_CHTS", "_N","_SZ") ) ) %>%
      ###     ...to only include those modes for which LACR provides estimates, excluding any 'CHTS' estimates,
      ###       and excluding any MRIP sample size fields ( the LACR sample sizes are used in the final 'lacr.cv' table )...
      
      arrange( YEAR ) %>%
      
      ###     ...reformatting 'mrip.dummy' to match the structure of 'tpwd.cv'...
      pivot_longer( !YEAR, names_to = "VARIABLE", values_to = "VALUE" ) %>%
      mutate( NEW_MODEN = gsub( "_.*","", VARIABLE ) ) %>%
      mutate( CATCH_VAR = ifelse( NEW_MODEN == "PRIV", gsub( "PRIV_","", VARIABLE ),
                          ifelse( NEW_MODEN ==  "CBT", gsub(  "CBT_","", VARIABLE ), NA ) ) ) %>%
      select( !VARIABLE ) %>%
      
      pivot_wider( names_from = CATCH_VAR, values_from = VALUE ) %>%
      
      ###     ...converting the CV estimates in 'mrip.dummy' to variances...
      mutate( AB1_VAR = ifelse( is.na(AB1), NA, ( CV_AB1 * AB1 )^2 ),
               B2_VAR = ifelse( is.na(B2 ), NA, ( CV_B2  * B2  )^2 ) ) %>%
      select( YEAR, NEW_MODEN, AB1, AB1_VAR, B2, B2_VAR )
    
    
    ###   I then join the MRIP (CV) table imported above with the LACR (CV) table, which is an output of
    ###       the CVs.catnum.LACR() function run in the main script and imported into this function...
    
    discard.rate.table = mrip.dummy %>%
      bind_rows( genrec.cv.table[[2]] ) %>%
      
      ###   ...which, for GOM discard rates, we need to resummarize (i.e., sum ) to combine year-mode estimates
      ###       across states (i.e., combine LA estimates with those from MS-FLW ). Note that for LA discard rates,
      ###       there should be no overlap in estimates (across year-mode) and so these calculations won't have
      ###       any effect on the resultant 'discard.rate.table'...
      group_by( YEAR, NEW_MODEN ) %>%
      summarize( AB1     = sum( AB1,     na.rm=TRUE ),
                 AB1_VAR = sum( AB1_VAR, na.rm=TRUE ),
                  B2     = sum(  B2,     na.rm=TRUE ),
                  B2_VAR = sum(  B2_VAR, na.rm=TRUE ) )
    rm( mrip.dummy )
    
    
    ###   With the required catch & variance estimates calculated, we then estimate the RATIO (and associated variance)
    ###   that will be applied to TPWD landings estimates as a proxy for TPWD discards...
    
    discard.rate.table = discard.rate.table %>%
      
      mutate( RATIO     = ifelse( AB1==0, 0, B2/AB1 ),
              RATIO_VAR = ifelse( AB1==0, 0, ( B2_VAR/(AB1^2) ) + ( (B2^2)*AB1_VAR/(AB1^4) ) ) ) %>%
      
      select( YEAR, NEW_MODEN, RATIO, RATIO_VAR )
    
    
    
    ### PROXY FOR TPWD DISCARDS ###
    ### ---------------------------
    ###
    ###     ...which is calculated as the product of TPWD landings estimates ( in 'tpwd.cv' ) and the
    ###       chosen B2:AB1 discard rate ( in 'discard.rate.table' )...
    
    dummy = tpwd.cv %>%
      left_join( discard.rate.table, by = c('YEAR','NEW_MODEN') ) %>%
      
      mutate( B2     = ifelse( is.na(RATIO),   0, AB1*RATIO ),
              B2_VAR = ifelse( is.na(RATIO),   0,
                               ( (AB1^2)*RATIO_VAR ) + ( (RATIO^2)*VAR ) - ( RATIO_VAR*VAR ) ) ) %>%
      select( !c( RATIO, RATIO_VAR ) )
      # mutate( CV_AB1 = sqrt( AB1_VAR ) / AB1,
      #         CV_B2  = sqrt(  B2_VAR ) /  B2 )
    
    tpwd.cv = dummy
    rm( dummy, discard.rate.table )
    
  }
  
  
  ### FINAL FORMATTING ###
  ###
  ###     ...which largely is just getting the table ready for its subsequent join with the MRIP & LACR CV tables
  ###       (i.e., renaming the native 'VAR' field to 'AB1_VAR', adding 'B2' & 'B2_VAR' is they haven't been imputed )...
  
  tpwd.cv = tpwd.cv %>% rename( AB1_VAR = VAR )
  
  if( 'B2' %notin% colnames(tpwd.cv) ) {
    tpwd.cv = tpwd.cv %>% mutate( B2 = 0, B2_VAR = 0 )
  }
  
  
  
  ### Reapplying the MRIP-FES calibration ###
  ### ---------------------------------------
  
  if( !all( is.na(cal.ratios) ) ) {
    
    dummy = tpwd.cv %>%
      mutate( AB1_VAR = ifelse( NEW_MODEN %in% mode.filter,
                                ( (AB1^2)*cal.ratios[ grepl('var',names(cal.ratios)) ] ) +
                                ( (cal.ratios[ grepl('ratio',names(cal.ratios)) ]^2)*AB1_VAR ) -
                                ( cal.ratios[ grepl('var',names(cal.ratios)) ]*AB1_VAR ), AB1_VAR ),
              AB1     = ifelse( NEW_MODEN %in% mode.filter,
                                AB1 * cal.ratios[ grepl('ratio',names(cal.ratios)) ], AB1 ),
              B2_VAR  = ifelse( NEW_MODEN %in% mode.filter,
                                ( ( B2^2)*cal.ratios[ grepl('var',names(cal.ratios)) ] ) +
                                ( (cal.ratios[ grepl('ratio',names(cal.ratios)) ]^2 )* B2_VAR ) -
                                ( cal.ratios[ grepl('var',names(cal.ratios)) ] * B2_VAR ),  B2_VAR ),
              B2      = ifelse( NEW_MODEN %in% mode.filter,
                                B2 * cal.ratios[ grepl('ratio',names(cal.ratios)) ] ,  B2 ) ) %>%
      # mutate( AB1_CV = sqrt( AB1_VAR ) / AB1,
      #         B2_CV = sqrt(  B2_VAR ) /  B2 ) %>%
      # select( YEAR, NEW_MODEN, AB1, AB1_VAR, AB1_CV, B2, B2_VAR, B2_CV )
      select( YEAR, NEW_MODEN, AB1, AB1_VAR, B2, B2_VAR )
    
    
    tpwd.cv = dummy
    rm( dummy )
    
  }
  
  
  ### TOTAL (annual) catch & uncertainty ###
  ### --------------------------------------
  
  dummy = tpwd.cv %>%
    group_by( YEAR ) %>%
    summarise( AB1     = sum( AB1    , na.rm=TRUE ),
               AB1_VAR = sum( AB1_VAR, na.rm=TRUE ),
                B2     = sum(  B2    , na.rm=TRUE ),
                B2_VAR = sum(  B2_VAR, na.rm=TRUE ) ) %>%
    mutate( NEW_MODEN = "TOTAL" )
    # mutate( AB1_CV = sqrt(AB1_VAR) / AB1,
    #          B2_CV = sqrt( B2_VAR) / B2 )
  
  tpwd.cv = tpwd.cv %>% bind_rows( dummy ) %>% arrange( YEAR, NEW_MODEN )
  rm( dummy )
  
  
  ###     ...and applying some formatting to these fields...
  
  tpwd.cv = tpwd.cv %>% mutate( NEW_MODEN = toupper(NEW_MODEN) )
  
  
  
  
  ### SAMPLE SIZE -- number of TRIPS & PSU's ###
  ### ------------------------------------------
  
  if( attach.samplesize ) {
    
    message( paste( "\n", "   *** Importing -- TPWD Sample Sizes (numTrips) ***   " ) )
    
    con = dbConnect(dbDriver("Oracle"), username = keyring::key_list("SECPR")[1,2],
                    password = keyring::key_get("SECPR", keyring::key_list("SECPR")[1,2]), dbname = "SECPR")
    
    
    tpwd.ss = dbGetQuery( con, paste0("select *
                                from RDI.TX_ANGLER_CATCH@secapxdv_dblk.sfsc.noaa.gov t" ) )
                                # where t.SPECIES_CODE IN (", sprintf("'%s'", paste(tpwd.code, collapse = "','")),")" ) )
    ###     ...where the entire table needs to be pulled as our sample size fields include info on positive trips/PSUs
    ###         and the total number of trips/PSUs ( negative + positive )...
    
    
    ###  I also define the days considered 'holidays' by TPWD, as needed to construct my DAYTYPE variable
    ###       ( which is used when randomly drawing survey assignments and so part of the PSU_ID field ).
    ###       According to the TPWD Operations Manual (Green 2017), "only Memorial Day, July 4th, and Labor Day
    ###       are considered holidays for survey scheduling purposes". I also add Thanksgiving, Christmas, and Easter,
    ###       as these were identified as holidays in this manual as well (when discussing survey design changes over time).
    ###       To see the list of holidays that can be added to this vector -->       timeDate::listHolidays()
    tpwd.holidays = as.Date( c( timeDate::USMemorialDay(1981:term.year),
                                timeDate::USIndependenceDay(1981:term.year),
                                timeDate::USLaborDay(1981:term.year),
                                timeDate::Easter(1981:term.year),
                                timeDate::USThanksgivingDay(1981:term.year),
                                timeDate::USChristmasDay(1981:term.year) ) )
    
    dummy = tpwd.ss %>%
      
      filter( SPECIES_CODE < 1000 ) %>%
      ###   ...where, according to Mark's email (6/30/21), finfish are assigned codes b/w 1-999 and
      ###         so anything larger than 1000 is removed from the table (i.e., doesn't represent finfish catch)...
      
      mutate( CYEAR  = as.numeric( substr( COMPLETION_DTTM, 6,9 ) ),
              MONTH  = substr( COMPLETION_DTTM, 3,5 ),
              DAY    = as.numeric( substr( COMPLETION_DTTM, 1,2 ) ) ) %>%
      mutate( MONTH.N = ifelse( MONTH == "JAN", 1,      ifelse( MONTH == "FEB", 2,
                        ifelse( MONTH == "MAR", 3,      ifelse( MONTH == "APR", 4,
                        ifelse( MONTH == "MAY", 5,      ifelse( MONTH == "JUN", 6,
                        ifelse( MONTH == "JUL", 7,      ifelse( MONTH == "AUG", 8,
                        ifelse( MONTH == "SEP", 9,      ifelse( MONTH == "OCT", 10,
                        ifelse( MONTH == "NOV", 11,     ifelse( MONTH == "DEC", 12, NA )))))))))))),
              SEASON = ifelse( MONTH == "MAY" & DAY %in% c(15:31),            1,
                       ifelse( MONTH %in% c( "JUN","JUL","AUG","SEP","OCT" ), 1,
                       ifelse( MONTH == "NOV" & DAY %in% c( 1:20),            1,
                       ifelse( MONTH == "NOV" & DAY %in% c(21:31),            2,
                       ifelse( MONTH %in% c( "DEC","JAN","FEB","MAR","APR" ), 2,
                       ifelse( MONTH == "MAY" & DAY %in% c( 1:14),            2, NA )))))),
              WAVE   = ifelse( MONTH %in% c("JAN","FEB"), 1,
                       ifelse( MONTH %in% c("MAR","APR"), 2,
                       ifelse( MONTH %in% c("MAY","JUN"), 3,
                       ifelse( MONTH %in% c("JUL","AUG"), 4,
                       ifelse( MONTH %in% c("SEP","OCT"), 5,
                       ifelse( MONTH %in% c("NOV","DEC"), 6, NA )))))) ) %>%
      mutate( DATE    = as.Date( paste0(CYEAR,"-",MONTH.N,"-",DAY ) ) ) %>%
      mutate( DAYTYPE = ifelse( weekdays(DATE) %in% c('Saturday',"Sunday"), 'we',
                        ifelse(          DATE  %in%    tpwd.holidays,       'we', 'wd' )) ) %>%
      mutate( NEW_MODEN = ifelse( ACTIVITY_CODE == 1, "Priv",
                          ifelse( ACTIVITY_CODE == 2, "Cbt",   NA )) ) %>%
      mutate( AREA   = ifelse( MINOR_BAY_OF_CATCH_CODE %in% c(990,992,994,996,998), 'TTS',
                       ifelse( MINOR_BAY_OF_CATCH_CODE %in% c(991,993,995,997,999), 'EEZ',
                       ifelse( is.na(MINOR_BAY_OF_CATCH_CODE), NA,                  'BAY' ))) ) %>%
      mutate( NEW_AREA  = ifelse( AREA == "BAY", 5,
                          ifelse( AREA == "TTS", 3, 4 ) ),
              NEW_AREAN = ifelse( AREA == "BAY", "Inshore",
                          ifelse( AREA == "TTS", "Ocean<=10mi", "Ocean>10mi" ) ) ) %>%
      
      mutate( TRIP_KEY = paste0( MAJOR_AREA_CODE, STATION_CODE, COMPLETION_DTTM, INTERVIEW_ID_TXT, INTERVIEW_TIME_NUM ),
               PSU_ID  = paste0( MAJOR_AREA_CODE, STATION_CODE, CYEAR, MONTH, DAY, SEASON, DAYTYPE ) ) %>%
      ###     ...where TRIP_KEY is a unique identifier for individual fishing trips (i.e., interview/vessel level)
      ###          and   PSU_ID is a unique identifier for individual survey assignments (i.e., survey-day ).
      ###         Note the inclusion of DAYTYPE in PSU_ID as separate TPWD estimates are generated for weekdays vs. weekends...
      
      rename( YEAR = CYEAR ) %>%
      
      filter( YEAR %in% inc.years,
              NEW_MODEN %in% inc.modes )
    
    rm( tpwd.holidays )
    
    
    dummy1 = dummy %>%
      mutate( NEW_MODEN = toupper(NEW_MODEN) ) %>%
      group_by( YEAR, NEW_MODEN ) %>%
      summarise( AT_Tot  = length( unique( TRIP_KEY ) ),
                 AT_AB1  = length( unique( TRIP_KEY[ SPECIES_CODE %in% tpwd.code ] ) ),
                 # AT_B2   = length( unique( TRIP_KEY[ SPECIES_CODE %in% tpwd.code &
                 #                                       CATCH_TYPE == 'B2' & !is.na(CATCH_TYPE) ] ) ),
                 PSU_Tot = length( unique( PSU_ID ) ),
                 PSU_AB1 = length( unique( PSU_ID[ SPECIES_CODE %in% tpwd.code ] ) )
                 # PSU_B2  = length( unique( PSU_ID[ SPECIES_CODE %in% tpwd.code &
                 #                                     CATCH_TYPE == 'B2' & !is.na(CATCH_TYPE) ] ) )
                ) %>%
      mutate( AT_B2 = 0, PSU_B2 = 0 )
      # pivot_wider( names_from = "NEW_MODEN", values_from = c("AT_AB1","AT_B2,"AT_Tot","PSU_AB1","PSU_B2,"PSU_Tot"),
      #              names_glue = "{NEW_MODEN}_{.value}" )
    
    dummy2 = dummy %>%
      group_by( YEAR ) %>%
      summarise( AT_Tot  = length( unique( TRIP_KEY ) ),
                 AT_AB1  = length( unique( TRIP_KEY[ SPECIES_CODE %in% tpwd.code ] ) ),
                 # AT_B2   = length( unique( TRIP_KEY[ SPECIES_CODE %in% tpwd.code &
                 #                                       CATCH_TYPE == 'B2' & !is.na(CATCH_TYPE) ] ) ),
                 PSU_Tot = length( unique( PSU_ID ) ),
                 PSU_AB1 = length( unique( PSU_ID[ SPECIES_CODE %in% tpwd.code ] ) )
                 # PSU_B2  = length( unique( PSU_ID[ SPECIES_CODE %in% tpwd.code &
                 #                                     CATCH_TYPE == 'B2' & !is.na(CATCH_TYPE) ] ) )
      ) %>%
      mutate( AT_B2 = 0, PSU_B2 = 0 ) %>%
      mutate( NEW_MODEN = "TOTAL" )
      # pivot_wider( names_from = "NEW_MODEN", values_from = c("AT_AB1","AT_B2,"AT_Tot","PSU_AB1","PSU_B2,"PSU_Tot"),
      #              names_glue = "{NEW_MODEN}_{.value}" )
    
    rm( dummy )
    
    dummy = bind_rows( dummy1, dummy2 ) %>%
      arrange( YEAR, NEW_MODEN )
    
    
    blah = tpwd.cv %>% full_join( dummy, by=c("YEAR","NEW_MODEN") ) %>% arrange( YEAR, NEW_MODEN )
    ###     ...where we use a full_join() to make sure all information is included in 'tpwd.cv', including...
    ###         -- years where sampling occurred ( N > 0 ) but the species-of-interest just wasn't intercepted ( catch = 0 )
    ###             (i.e., all trips in a given fishing year were a negative intercept for this spp )
    ###         -- years where TPWD catch & CV estimates were imputed from other data sources, and so not actually
    ###             generated from observed data...
    
    blah[ is.na(blah) ] = 0
    ###     Additionally, as these fields (in 'tpwd.cv') are to be combined with the associated MRIP catch/CV estimates
    ###     (in 'cv.table'), we also change any <NA> values to zero's...
    
    tpwd.cv = blah
    rm( blah, dummy, dummy1, dummy2 )
    
    
    message( paste( "\n", "   ***  !!! --- Full TPWD Table Constructed --- !!!   ***   " ) )
    
  }
  
  
  return( tpwd.cv )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------


convert.long.table.cat = function( cv.table, report.type = c("annual","detailed"), survey = c('MRIP','LACR','TPWD') ) {
  
  ###     ...where 'cv.table' is a table of recreational estimates ( catch, CV, & sample size ) which is
  ###               currently in wide-format and to be converted to long-format,
  ###       'report.type' identifies the type of report built by RDI (i.e., "annual" provides CVs by year, year-mode, or year-state
  ###             while "detailed" provides CVs at a much finer resolution, namely by year-mode-state-wave ),
  ###       'survey' identifies the survey from which the associated estimates originate ( the fields of which may differ )...
  
  
  if( survey == 'MRIP' ) {
    
    #if( report.type == "annual" ) {
      dummy = cv.table %>% pivot_longer( !c(YEAR, SID), names_to = 'VARIABLE', values_to = 'value' )
        
  # } else if( report.type == "detailed") {
     # dummy = cv.table %>% pivot_longer( !c(YEAR,NEW_STA,WAVE), names_to = 'VARIABLE', values_to = 'value' )
    #}
    
    dummy = dummy %>%
      mutate( VARIABLE = gsub( 'CBT_HBT_','CBTHBT_', VARIABLE ) ) %>%
      
      mutate( NEW_MODEN = gsub( "_.*","", VARIABLE ) ) %>%
      mutate( VARIABLE  = str_remove( VARIABLE, NEW_MODEN ) ) %>%
      
      mutate( CATCH_VAR = ifelse( grepl( 'AB1', VARIABLE ), 'AB1',
                          ifelse( grepl(  'B2', VARIABLE ),  'B2', 'TOTAL' )) ) %>%
      mutate( METRIC = ifelse( grepl(  'CV', VARIABLE ),  'CV',
                       ifelse( grepl( 'PSU', VARIABLE ), 'PSU',
                       ifelse( grepl(  'AT', VARIABLE ),  'AT', 'CAT' ))) ) %>%
      
      select( any_of( c( 'YEAR', 'NEW_MODEN', 'NEW_STA', 'WAVE', 'CATCH_VAR', 'METRIC', 'value', 'SID' ) ) ) %>%
      arrange( across( any_of( c( 'YEAR', 'SID', 'NEW_MODEN', 'NEW_STA', 'WAVE', 'CATCH_VAR', 'METRIC' ) ) ) )
    
    
    ###   ...where, as a last step for MRIP tables, I replace the 'CV' field with a 'VAR' field
    ###     as variances are what are needed when combining catch tables (e.g., combining MRIP+TPWD+LACR
    ###     and in calculating CVs for landings-in-weight )...
    
    dummy = dummy %>%
      pivot_wider( names_from = METRIC, values_from = value ) %>%
      mutate( VAR = ( CV * CAT )^2 )
    
    #if( report.type == "annual" ) {
      dummy = dummy %>%
        pivot_longer( !c(YEAR,SID,NEW_MODEN,CATCH_VAR), names_to = 'METRIC', values_to = 'value' )
      
   # } else if( report.type == "detailed" ) {
    #  dummy = dummy %>%
    #    pivot_longer( !c(YEAR,NEW_MODEN,NEW_STA,WAVE,CATCH_VAR), names_to = 'METRIC', values_to = 'value' )
    #}
    
    dummy = dummy %>%
      filter( !is.na(value) ) %>%
      filter( METRIC != 'CV' )
    
    
    
  } else if( survey %in% c('LACR','TPWD') ) {
    
    
    #if( report.type == "annual" ) {
      dummy = cv.table %>% pivot_longer( !c(YEAR,NEW_MODEN), names_to = 'VARIABLE', values_to = 'value' )
     #dummy = cv.table %>% pivot_longer( !c(YEAR,NEW_MODEN), names_to = 'VARIABLE', values_to = 'value' )
      
   # } else if( report.type == "detailed") {
     # dummy = cv.table %>% pivot_longer( !c(YEAR,NEW_MODEN,NEW_STA,WAVE), names_to = 'VARIABLE', values_to = 'value' )
   # }
    
    
    dummy = dummy %>%
      
      # mutate( CATCH_VAR = gsub( "_.*","", VARIABLE ) ) %>%
      # mutate( VARIABLE  = str_remove( VARIABLE, NEW_MODEN ) ) %>%
      
      mutate( CATCH_VAR = ifelse( grepl( 'AB1', VARIABLE ), 'AB1',
                          ifelse( grepl(  'B2', VARIABLE ),  'B2', 'TOTAL' )) ) %>%
      mutate( METRIC = ifelse( grepl( 'VAR', VARIABLE ), 'VAR',
                       ifelse( grepl( 'PSU', VARIABLE ), 'PSU',
                       ifelse( grepl(  'AT', VARIABLE ),  'AT',
                       ifelse( grepl(  'CV', VARIABLE ),  'CV', 'CAT' )))) ) %>%
      
      select( any_of( c( 'YEAR', 'NEW_MODEN', 'NEW_STA', 'WAVE', 'CATCH_VAR', 'METRIC', 'value' ) ) ) %>%
      arrange( across( any_of( c( 'YEAR', 'NEW_MODEN', 'NEW_STA', 'WAVE', 'CATCH_VAR', 'METRIC' ) ) ) )
    
  }
  
  
  cv.table = dummy
  rm(dummy)
  
  
  return( cv.table )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------


# join.CV.tables.wide = function( mrip.table, state.table ) {
#   
#   ###     ...where  'mrip.table' is the table of MRIP catch, CV, & sample size estimates that are to be joined
#   ###         with the associated estimates in 'state.table'. Note that for SEDARs with multiple SID domains,
#   ###         we have already (outside of this function) identified the appropriate 'mrip.table' object that
#   ###         contains TPWD/LACR estimates, and so only one (MRIP) table should ever be imported into this function...
#   
#   
#   
#   ### JOIN -- LACR CVs + MRIP CVs ###
#   ### -------------------------------
#   ###
#   ###     ...for which the join is based on summing catch and variance estimates between the two tables (by strata)
#   ###       and so the first step is to make sure the field names match between my MRIP-CV and LACR-CV tables...
#   ###
#   ###       For this, I need to change some field names in 'mrip.table' to remove any second underscores. In particular,
#   ###       I'm using underscores to distinguish NEW_MODEN & the catch VARIABLE fields (see below), but these assignments
#   ###       "break" when there's an additional underscore in the field name (e.g., 'CBT_HBT' , 'CV_AB1' , 'PSU_AB1' ).
#   ###       Note that the original naming conventions will be added back-in later ( in the subsequent summarise() statement ),
#   ###       but these modifications are needed before I can make the NEW_MODEN vs. VARIABLE assignments...
#   
#   dummy = mrip.table
#   
#   colnames(dummy) = gsub( "CBT_HBT","CBTHBT", colnames(dummy) )
#   colnames(dummy) = gsub(  "CV_AB1", "CVab1", colnames(dummy) )
#   colnames(dummy) = gsub( "PSU_AB1","PSUab1", colnames(dummy) )
#   colnames(dummy) = gsub(  "AT_AB1", "ATab1", colnames(dummy) )
#   colnames(dummy) = gsub(  "CV_B2" , "CVb2" , colnames(dummy) )
#   colnames(dummy) = gsub( "PSU_B2" ,"PSUb2" , colnames(dummy) )
#   colnames(dummy) = gsub(  "AT_B2" , "ATb2" , colnames(dummy) )
#   colnames(dummy) = gsub( "PSU_Tot","PSUtot", colnames(dummy) )
#   colnames(dummy) = gsub(  "AT_Tot", "ATtot", colnames(dummy) )
#   
#   dummy = dummy %>%
#     pivot_longer( !YEAR, names_to = "VARIABLE", values_to = "VALUE" ) %>%
#     mutate( NEW_MODEN = gsub( "_.*","", VARIABLE ),
#             VARIABLE  = gsub( ".*_","", VARIABLE ) ) %>%
#     pivot_wider( names_from = VARIABLE, values_from = VALUE ) %>%
#     rename( PSU_AB1 = PSUab1,    PSU_B2 = PSUb2,       PSU_Tot = PSUtot,
#             AT_AB1 =  ATab1,     AT_B2 =  ATb2,        AT_Tot =  ATtot ) %>%
#     
#     mutate( AB1_VAR = ( CVab1*AB1 )^2,
#              B2_VAR = ( CVb2 *B2  )^2 ) %>%
#     # select( -c(CVab1,CVb2) )
#     select( YEAR, NEW_MODEN, AB1, AB1_VAR, B2, B2_VAR, AT_Tot, AT_AB1, AT_B2, PSU_Tot, PSU_AB1, PSU_B2 )
#   
#   dummy = bind_rows( dummy, lacr.dummy %>% select( !contains("CV") ) ) %>%
#     group_by( YEAR, NEW_MODEN ) %>%
#     summarise( AB1     = sum( AB1    , na.rm=TRUE ),
#                AB1_VAR = sum( AB1_VAR, na.rm=TRUE ),
#                B2     = sum(  B2    , na.rm=TRUE ),
#                B2_VAR = sum(  B2_VAR, na.rm=TRUE ),
#                AT_AB1 = sum( AT_AB1, na.rm=TRUE ),
#                AT_B2  = sum( AT_B2 , na.rm=TRUE ),
#                AT_Tot = sum( AT_Tot, na.rm=TRUE ),
#                PSU_AB1 = sum( PSU_AB1, na.rm=TRUE ),
#                PSU_B2  = sum( PSU_B2 , na.rm=TRUE ),
#                PSU_Tot = sum( PSU_Tot, na.rm=TRUE ) )
#   
#   
#   # dummy.plot <- ggplot( data = dummy %>%
#   #                         filter( NEW_MODEN == "TOTAL" ) %>%
#   #                         mutate( AB1_SE = sqrt(AB1_VAR),
#   #                                  B2_SE = sqrt(B2_VAR) ),
#   #                       aes( x=YEAR ) ) +
#   #   geom_line( aes( y=AB1, colour='red' ), size=1.2 ) +
#   #   geom_errorbar( aes( ymin=pmax(AB1-AB1_SE,0), ymax=AB1+AB1_SE ),
#   #                  width=0.8, size=1, position=position_dodge(width=0.2) ) +
#   #   geom_line( aes( y=B2 , colour='blue'), size=1.2 ) +
#   #   geom_errorbar( aes( ymin=pmax( B2-B2_SE ,0), ymax= B2+B2_SE  ),
#   #                  width=0.8, size=1, position=position_dodge(width=0.2) ) +
#   #   labs( title="", x="Year", y="Thousands of Fish" ) +
#   #   # scale_x_continuous( breaks = scales::pretty_breaks( n = (n.years/2) ) ) +
#   #   expand_limits(y = 0) +
#   #   theme_bw() +
#   #   theme( text = element_text(size = 11),
#   #          axis.text.x = element_text(angle = 90, vjust=0.5),
#   #          legend.position = "bottom",
#   #          panel.grid.major = element_line(colour = "grey", linewidth = 0.5),
#   #          panel.grid.minor = element_line(colour = "grey", linewidth = 0.2),
#   #          panel.border = element_rect(colour = "black", fill = NA) )
#   # rm( dummy.plot )
#   
#   
#   
#   ### FORMAT -- FINAL JOIN TABLE ###
#   ### ------------------------------
#   ###
#   ###     ...where, as a last step, I then format the new (MRIP+LACR) 'dummy' table to match that used
#   ###            for the AB1, CV, and sample size summaries in our final 'cv.table'...
#   
#   dummy = dummy %>%
#     mutate( NEW_MODEN = ifelse( NEW_MODEN == 'CBTHBT', 'CBT_HBT', NEW_MODEN ) ) %>%
#     
#     mutate( CV_AB1 = ifelse( AB1==0, 0, sqrt(AB1_VAR) / AB1 ),
#             CV_B2  = ifelse(  B2==0, 0, sqrt( B2_VAR) / B2  ) ) %>%
#     select( -c(AB1_VAR,B2_VAR) ) %>%
#     pivot_wider( names_from = c(NEW_MODEN),
#                  values_from = c( AB1, CV_AB1, B2, CV_B2, AT_AB1, AT_B2, AT_Tot, PSU_AB1, PSU_B2, PSU_Tot ),
#                  names_glue = "{NEW_MODEN}_{.value}" )
#   
#   
#   
#   return( dummy )
#   
# }


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------


# format.cv.table = function( cv.table ) {
#   
#   
#   ### ************************************************************************************************************
#   ###
#   ###     FINAL FORMATTING
#   ###
#   ### ************************** ###
# 
# 
#   ###  I start by substituting any errors in my CV tables with zero's...
# 
#   if( paste0( stockID, collapse = ' ' ) == 'None' ) {
# 
#     cv.table[ is.na(cv.table) ] = 0
# 
#   } else {
# 
#     for( i in 1:length(cv.table) ) {
# 
#       cv.table[[i]][ is.na(cv.table[[i]]) ] = 0
# 
#     }
#   }
# 
# 
#   ### Substituting Catch Values from 'catch.table' ###
#   ### ------------------------------------------------
#   ###
#   ###     ...which ensures that the final CV tables contain the correct catch estimates. In particular,
#   ###         manual corrections are sometimes made to estimates for specific strata, which would obviously not
#   ###         be included in the standard CV script above. This substitution serves to avoid any confusion by ensuring
#   ###         such adjustments are included in the CV tables (i.e., catch in CV tables match that in other tables)...
# 
#   if( paste0( stockID, collapse = ' ' ) == 'None' ) {
# 
# 
#     dummy.cv = cv.table
# 
# 
#     ###     ...which are applied as a filter to 'catch.table'...
#     dummy.catch <- catch.table %>%
#       mutate( NEW_MODEN = ifelse( NEW_MODEN == "Priv/Shore", "Priv", NEW_MODEN ) ) %>%
#       mutate( NEW_MODEN = toupper(NEW_MODEN) ) %>%
#       group_by( YEAR, NEW_MODEN ) %>%
#       summarise( AB1 = sum( AB1, na.rm=TRUE ),
#                  B2 = sum(  B2, na.rm=TRUE ) )
# 
#     blah = dummy.catch %>%
#       group_by( YEAR ) %>%
#       summarise( AB1 = sum( AB1, na.rm=TRUE ),
#                  B2 = sum(  B2, na.rm=TRUE ) ) %>%
#       mutate( NEW_MODEN = "TOTAL" )
# 
#     dummy.catch = bind_rows( dummy.catch, blah ) %>%
#       pivot_wider( names_from = NEW_MODEN, values_from = c(AB1,B2), names_glue = "{NEW_MODEN}_{.value}" )
#     rm( blah )
# 
#     dummy.catch[ is.na(dummy.catch) ] = 0
# 
#     colnames(dummy.catch) = gsub( "\\/","_", colnames(dummy.catch) )    ### ...to change any 'CBT/HBT' records to 'CBT_HBT'...
# 
# 
#     ### I then replace the catch estimates in 'dummy.cv' with those from 'dummy.catch'...
# 
#     col.IDs = colnames(dummy.catch)[-1]
# 
#     for( i in 1:length(col.IDs) ) {
# 
#       eval( parse( text = paste0( "dummy.cv$",col.IDs[i]," = dummy.catch$",col.IDs[i] ) ) )
#     }
# 
#     cv.table <- dummy.cv
#     rm( col.IDs, i, dummy.catch, dummy.cv )
# 
# 
#   } else {
# 
# 
#     SID = unique( stockID )
# 
#     for( j in 1:length(SID) ) {
# 
# 
#       dummy.cv <- cv.table[[ SID[j] ]]
# 
# 
#       ### I then identify which states/domains were included in this SID region...
#       dummy.states = SID.states[ which( stockID %in% SID[j] ) ]
# 
#       ###     ...which are applied as a filter to 'catch.table'...
#       dummy.catch <- catch.table %>%
#         filter( NEW_STA %in% dummy.states | FL_REG %in% dummy.states ) %>%
#         mutate( NEW_MODEN = ifelse( NEW_MODEN == "Priv/Shore", "Priv", NEW_MODEN ) ) %>%
#         mutate( NEW_MODEN = toupper(NEW_MODEN) ) %>%
#         group_by( YEAR, NEW_MODEN ) %>%
#         summarise( AB1 = sum( AB1, na.rm=TRUE ),
#                    B2 = sum(  B2, na.rm=TRUE ) )
# 
#       blah = dummy.catch %>%
#         group_by( YEAR ) %>%
#         summarise( AB1 = sum( AB1, na.rm=TRUE ),
#                    B2 = sum(  B2, na.rm=TRUE ) ) %>%
#         mutate( NEW_MODEN = "TOTAL" )
# 
#       dummy.catch = bind_rows( dummy.catch, blah ) %>%
#         pivot_wider( names_from = NEW_MODEN, values_from = c(AB1,B2), names_glue = "{NEW_MODEN}_{.value}" )
#       rm( blah )
# 
#       dummy.catch[ is.na(dummy.catch) ] = 0
# 
# 
#       ### I then replace the catch estimates in 'dummy.cv' with those from 'dummy.catch'...
# 
#       col.IDs = colnames(dummy.catch)[-1]
# 
#       for( i in 1:length(col.IDs) ) {
# 
#         eval( parse( text = paste0( "dummy.cv$",col.IDs[i]," = dummy.catch$",col.IDs[i] ) ) )
#       }
# 
#       cv.table[[ SID[j] ]] <- dummy.cv
#       rm( col.IDs, i, dummy.catch, dummy.cv, dummy.states )
# 
#     }
#     rm( SID, j )
# 
#   }
# 
# 
# 
#   ### Combining the Sample Size Columns ###
#   ### -------------------------------------
#   ###
#   ###     ...wherein the new fields will use parentheses to separate the positive and total sample size metrics
#   ###       ( for both AT & PSU ), which naturally requires a format of     as.character()     . I therefore start
#   ###       by making sure the c( "PSU","AT" ) columns are properly formatted (e.g., number of sigfigs ), and then
#   ###       combine the positive & total fields with the 'separator' defined using parentheses...
# 
# 
#   # if( ( ( "Cbt" %in% mode_sub ) | ( "Hbt" %in% mode_sub ) ) &
#   #     any( c("VA","MD","DE","PA","NJ","NY","CT","RI","MA","NH","ME") %in% states ) ) {
#   if( any( grepl( 'CBT_HBT', names(cv.table) ) ) ) {
#     if( "Cbt" %in% mode_sub ) {
#       cv.cols = append( mode_sub,"Cbt_Hbt", after=match("Cbt",mode_sub) )
#     } else {
#       cv.cols = append( mode_sub,"Cbt_Hbt", after=match("Hbt",mode_sub)-1 )
#     }
#   } else {    cv.cols = mode_sub    }
# 
# 
#   if( paste0( stockID, collapse = ' ' ) == 'None' ) {
# 
#     cv.table <- as.data.frame( cv.table )
#     cv.table[ , grep( "_PSU_", colnames(cv.table) ) ] <- format(
#       round( cv.table[ , grep( "_PSU_", colnames(cv.table) ) ], 0 ), big.mark="," )
#     cv.table[ , grep(  "_AT_", colnames(cv.table) ) ] <- format(
#       round( cv.table[ , grep(  "_AT_", colnames(cv.table) ) ], 0 ), big.mark="," )
# 
#     col.IDs <- c( toupper(cv.cols),"TOTAL" )
#     for( i in 1:length(col.IDs) ) {
# 
#       ### PSU ###
#       cv.table <- unite( cv.table, newcol,
#                          c( paste0(col.IDs[i],"_PSU_Tot"),paste0(col.IDs[i],"_PSU_B2") ), sep=" (", remove=FALSE )
#       cv.table$newcol <- paste0( cv.table$newcol,")" )
#       ###   ...where I set remove=FALSE because I need to retain the "PSU_Tot" field for the AB1 sample size column (below).
#       ###         I do drop the (original) "PSU_B2" column here though...
#       cv.table <- cv.table[ ,!( colnames(cv.table) == paste0(col.IDs[i],"_PSU_B2") ) ]
#       colnames(cv.table)[ which( colnames(cv.table) == "newcol" ) ] <- paste0(col.IDs[i],"_PSU_B2")
# 
#       cv.table <- unite( cv.table, newcol,
#                          c( paste0(col.IDs[i],"_PSU_Tot"),paste0(col.IDs[i],"_PSU_AB1") ), sep=" (", remove=TRUE )
#       cv.table$newcol <- paste0( cv.table$newcol,")" )
#       colnames(cv.table)[ which( colnames(cv.table) == "newcol" ) ] <- paste0(col.IDs[i],"_PSU_AB1")
# 
# 
#       ### Angler Trips ###
#       cv.table <- unite( cv.table, newcol,
#                          c( paste0(col.IDs[i],"_AT_Tot"),paste0(col.IDs[i],"_AT_B2") ), sep=" (", remove=FALSE )
#       cv.table$newcol <- paste0( cv.table$newcol,")" )
#       ###   ...where I set remove=FALSE because I need to retain the "PSU_Tot" field for the AB1 sample size column (below).
#       ###         I do drop the (original) "PSU_B2" column here though...
#       cv.table <- cv.table[ ,!( colnames(cv.table) == paste0(col.IDs[i],"_AT_B2") ) ]
#       colnames(cv.table)[ which( colnames(cv.table) == "newcol" ) ] <- paste0(col.IDs[i],"_TRP_B2")
# 
#       cv.table <- unite( cv.table, newcol,
#                          c( paste0(col.IDs[i],"_AT_Tot"),paste0(col.IDs[i],"_AT_AB1") ), sep=" (", remove=TRUE )
#       cv.table$newcol <- paste0( cv.table$newcol,")" )
#       colnames(cv.table)[ which( colnames(cv.table) == "newcol" ) ] <- paste0(col.IDs[i],"_TRP_AB1")
# 
#     }
# 
# 
#     ### ORGANIZE THE AB1 & B2 columns...
#     AB1.cols <- vector()
#     B2.cols <- vector()
# 
#     col.IDs = col.IDs[ order( match(col.IDs,c("CBT","CBT_HBT","HBT","PRIV","PRIV_SHORE","SHORE","TOTAL")) ) ]
# 
#     for( i in 1:length(col.IDs) ) {
#       AB1.cols <- c( AB1.cols, paste0( col.IDs[i], c("_AB1","_CV_AB1","_PSU_AB1","_TRP_AB1") ) )
#       B2.cols <- c( B2.cols, paste0( col.IDs[i], c("_B2","_CV_B2","_PSU_B2","_TRP_B2") ) )
#     }
#     cv.table <- cv.table[ ,c("YEAR",AB1.cols,B2.cols) ]
# 
#     rm( AB1.cols,B2.cols )
# 
# 
#   } else {
# 
# 
#     SID = unique( stockID )
# 
#     for( j in 1:length(SID) ) {
# 
#       cv.dummy <- cv.table[[SID[j]]]
# 
#       cv.dummy <- as.data.frame( cv.dummy )
#       cv.dummy[ , grep( "_PSU_", colnames(cv.dummy) ) ] <- format(
#         round( cv.dummy[ , grep( "_PSU_", colnames(cv.dummy) ) ], 0 ), big.mark="," )
#       cv.dummy[ , grep(  "_AT_", colnames(cv.dummy) ) ] <- format(
#         round( cv.dummy[ , grep(  "_AT_", colnames(cv.dummy) ) ], 0 ), big.mark="," )
# 
#       col.IDs <- c( toupper(cv.cols),"TOTAL" )
#       for( i in 1:length(col.IDs) ) {
# 
#         ### PSU ###
#         cv.dummy <- unite( cv.dummy, newcol,
#                            c( paste0(col.IDs[i],"_PSU_Tot"),paste0(col.IDs[i],"_PSU_B2") ), sep=" (", remove=FALSE )
#         cv.dummy$newcol <- paste0( cv.dummy$newcol,")" )
#         ###   ...where I set remove=FALSE because I need to retain the "PSU_Tot" field for the AB1 sample size column (below).
#         ###         I do drop the (original) "PSU_B2" column here though...
#         cv.dummy <- cv.dummy[ ,!( colnames(cv.dummy) == paste0(col.IDs[i],"_PSU_B2") ) ]
#         colnames(cv.dummy)[ which( colnames(cv.dummy) == "newcol" ) ] <- paste0(col.IDs[i],"_PSU_B2")
# 
#         cv.dummy <- unite( cv.dummy, newcol,
#                            c( paste0(col.IDs[i],"_PSU_Tot"),paste0(col.IDs[i],"_PSU_AB1") ), sep=" (", remove=TRUE )
#         cv.dummy$newcol <- paste0( cv.dummy$newcol,")" )
#         colnames(cv.dummy)[ which( colnames(cv.dummy) == "newcol" ) ] <- paste0(col.IDs[i],"_PSU_AB1")
# 
# 
#         ### Angler Trips ###
#         cv.dummy <- unite( cv.dummy, newcol,
#                            c( paste0(col.IDs[i],"_AT_Tot"),paste0(col.IDs[i],"_AT_B2") ), sep=" (", remove=FALSE )
#         cv.dummy$newcol <- paste0( cv.dummy$newcol,")" )
#         ###   ...where I set remove=FALSE because I need to retain the "PSU_Tot" field for the AB1 sample size column (below).
#         ###         I do drop the (original) "PSU_B2" column here though...
#         cv.dummy <- cv.dummy[ ,!( colnames(cv.dummy) == paste0(col.IDs[i],"_AT_B2") ) ]
#         colnames(cv.dummy)[ which( colnames(cv.dummy) == "newcol" ) ] <- paste0(col.IDs[i],"_TRP_B2")
# 
#         cv.dummy <- unite( cv.dummy, newcol,
#                            c( paste0(col.IDs[i],"_AT_Tot"),paste0(col.IDs[i],"_AT_AB1") ), sep=" (", remove=TRUE )
#         cv.dummy$newcol <- paste0( cv.dummy$newcol,")" )
#         colnames(cv.dummy)[ which( colnames(cv.dummy) == "newcol" ) ] <- paste0(col.IDs[i],"_TRP_AB1")
# 
#       }
# 
# 
#       ### ORGANIZE THE AB1 & B2 columns...
#       AB1.cols <- vector()
#       B2.cols <- vector()
# 
#       col.IDs = col.IDs[ order( match(col.IDs,c("CBT","CBT_HBT","HBT","PRIV","PRIV_SHORE","SHORE","TOTAL")) ) ]
# 
#       for( i in 1:length(col.IDs) ) {
#         AB1.cols <- c( AB1.cols, paste0( col.IDs[i], c("_AB1","_CV_AB1","_PSU_AB1","_TRP_AB1") ) )
#         B2.cols <- c( B2.cols, paste0( col.IDs[i], c("_B2","_CV_B2","_PSU_B2","_TRP_B2") ) )
#       }
#       cv.dummy <- cv.dummy[ ,c("YEAR",AB1.cols,B2.cols) ]
# 
#       cv.table[[SID[j]]] = cv.dummy
# 
#       rm( cv.dummy, AB1.cols,B2.cols )
# 
#     }
#     rm( SID )
# 
#   }
# 
#   rm( cv.cols )
#   
#   
# }

