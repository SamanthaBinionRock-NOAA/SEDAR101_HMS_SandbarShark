

### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------
###
###     DESCRIPTION
###
###     ...wherein this script contains the functions needed to import (and format) all relevant data files
###       for SEDAR working papers summarizing the general recreational datasets...
###
###
###    *** wp.import.catch( )           ...which imports the 'raw catch' file...
###    *** wp.import.catch.unid( )      ...which imports the 'unidentified catch' file...
###
###    *** wp.import.catch.numcvs( )    ...which imports the 'catch CV' estimates for catch-in-number...
###    *** wp.import.catch.wgtcvs( )    ...which imports the 'catch CV' estimates for average weights...
###    *** wp.import.catch.lbscvs( )    ...which imports the 'catch CV' estimates for landings-in-weight...
###    *** wp.import.catch.cbtcvs( )    ...which imports the 'catch CV' estimates for the CBT calibration figure...
###
###    *** wp.import.comp.sedar( )      ...which imports the 'SEDAR catch comparison' file...
###    *** wp.import.comp.calib( )      ...which imports the (Fig2) data file to compare calibrated MRIP estimates...
###
###    *** wp.import.size( )            ...which imports the 'raw size' file...
###    *** wp.import.size.nmrip( )      ...which imports the sample size info for 'raw MRIP size' data...
###    *** wp.import.size.ntpwd( )      ...which imports the sample size info for 'raw TPWD size' data...
###    *** wp.import.size.nlacr( )      ...which imports the sample size info for 'raw LACR size' data...
###
###    *** wp.import.eff.mrip( )        ...which imports the 'MRIP effort' file...
###    *** wp.import.eff.tpwd( )        ...which imports the 'TPWD effort' file...
###    *** wp.import.eff.lacr( )        ...which imports the 'LACR effort' file...
###
### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------

wp.import.catch = function( file.dir, file.name, tab.name, mode.filter ) {
  ###     ...where 'file.dir' is the file directory containing the catch file to be imported,
  ###           'file.name' is the name of that file, 'tab.name' the associated (raw catch) tab,
  ###       and 'mode.filter' to identify those modes (potentially) included in this assessment...
  
  
  raw.catch <- read_excel( path  = paste0( file.dir,"/",file.name ),
                           sheet = tab.name, trim_ws=FALSE, col_types="text" )
  ###     ...where I format everything as 'text' because there are columns in the raw catch file that
  ###       R assumes will be 'logical' based on the name (e.g., "itis_code" seems to be read as "it is..." ? )
  
  colnames(raw.catch) = toupper( colnames(raw.catch) )
  if( "YEAR" %notin% colnames(raw.catch) & "INT_YEAR" %in% colnames(raw.catch) ) {
    colnames(raw.catch)[ colnames(raw.catch) %in% c("INT_YEAR") ] = "YEAR"
  }
  
  ### SUBSETTING ###
  ###     ...while the excel files we provide to analysts are already subsetted for the assessment of interest,
  ###         there are times where Vivian and I may need to go back and apply some additional subsetting
  ###         (e.g., after choices made during the data workshop). While a new excel file is presumably produced
  ###         after such decisions, I add a few lines of code below to ensure the 'proper' subsetting is applied
  ###         even if pulling from the old (out-of-date) estimates...
  ###
  ###     ...producing GenRec catch estimates, so need to drop SRHS...
  raw.catch <- raw.catch[ which( raw.catch$DS != "SRHS" ), ]
  ###     ...subsetting by year...
  raw.catch <- raw.catch[ which( raw.catch$YEAR %in% (1981:params$term.year) ), ]
  ###     ...subsetting by area...
  raw.catch <- raw.catch[ which( raw.catch$NEW_STA %in% params$subset.states ), ]
  
  if( "FL" %in% params$subset.states | "FLW" %in% params$subset.states | "FLE" %in% params$subset.states ) {
    raw.catch <- raw.catch[ which(
      is.na(raw.catch$FL_REG) | raw.catch$FL_REG == "" | raw.catch$FL_REG %in% params$subset.FL ), ]
  }
  # if( "NC" %in% states ) {
  #   raw.catch <- raw.catch[ which(
  #     is.na(raw.catch$NC_REG) | raw.catch$NC_REG == "" | raw.catch$NC_REG %in% params$subset.NC ), ]
  # }
  ###     ...subsetting by mode...
  raw.catch <- raw.catch[ which( raw.catch$NEW_MODEN %in% mode.filter ), ]
  
  ### To avoid duplicating catch estimates from the SRHS survey...
  raw.catch <- raw.catch[ !( raw.catch$NEW_MODEN == "Hbt" & raw.catch$SUB_REG == 6 ), ]
  raw.catch <- raw.catch[ !( raw.catch$NEW_MODEN == "Hbt" & raw.catch$SUB_REG == 7 & raw.catch$YEAR >= 1986 ), ]
  ### Additionally, as per an email from Kelly Fitzpatrick for SEDAR 71 (July 22 2020), it was decided that all MRIP Hbt fishing
  ###       from Monroe County (FL_REG==3) would be excluded from future SEDAR assessments. The thinking is that "most of the
  ###       MRIP HB Monroe County landings come from the Keys [Atlantic side], which is included in the [SRHS] area 12 and 17 estimates."
  ###       Therefore, including them in Atlantic assessments equates to 'double counting' HBT landings from Monroe County
  ###       ( but from two different sources; MRIP & SRHS ) whereas including them in Gulf assessments is equivalent to including
  ###       SATL Hbt landings in a Gulf assessment. Neither of these situations is desirable, so we exclude MRIP Hbt from FL_REG==3...
  if( "FL_REG" %in% colnames(raw.catch) ) {       ### ...where I first check that FL_REG is a field (not seen in some CAR assessments)...
    if( dim( raw.catch[ which( raw.catch$NEW_MODEN == "Hbt" & raw.catch$FL_REG == 3 ), ] )[1] > 0 ) {
      raw.catch <- raw.catch[ -which( raw.catch$NEW_MODEN == "Hbt" & raw.catch$FL_REG == 3 ), ]
    }
  }
  
  ### ADDITIONAL FORMATTING ###
  
  colnames(raw.catch)[ which( colnames(raw.catch) == "YEAR" ) ] <- "Year"
  ###     ...I also remove any rows with missing years (as a precaution)...
  raw.catch <- raw.catch[ which( !is.na(raw.catch$Year) ), ]
  ###     ...and make sure the columns I need (for my flextables) are properly formatted...
  raw.catch$Year <- as.numeric( raw.catch$Year )
  raw.catch$AB1  <- as.numeric( raw.catch$AB1 )
  raw.catch$B2   <- as.numeric( raw.catch$B2 )
  if( "LBSEST_SECWWT" %in% colnames(raw.catch) ) {
    colnames(raw.catch)[ which( colnames(raw.catch) == "LBSEST_SECWWT" ) ] <- "lbsest_SECwwt"
    raw.catch$lbsest_SECwwt <- as.numeric( raw.catch$lbsest_SECwwt )
  }
  if( "LBSEST_SECGWT" %in% colnames(raw.catch) ) {
    colnames(raw.catch)[ which( colnames(raw.catch) == "LBSEST_SECGWT" ) ] <- "lbsest_SECgwt"
    raw.catch$lbsest_SECgwt <- as.numeric( raw.catch$lbsest_SECgwt )
  }
  if( "LBSEST_SEC" %in% colnames(raw.catch) ) {
    colnames(raw.catch)[ which( colnames(raw.catch) == "LBSEST_SEC" ) ] <- "lbsest_SEC"
    raw.catch$lbsest_SEC <- as.numeric( raw.catch$lbsest_SEC )
  }
  if( "LBSEST_SECSOURCE" %in% colnames(raw.catch) ) {
    colnames(raw.catch)[ which( colnames(raw.catch) == "LBSEST_SECSOURCE" ) ] <- "lbsest_SECsource"
  }
  
  
  return( raw.catch )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------


wp.import.catch.unid = function( file.dir, file.name, tab.name, mode.filter, params ) {
  ###     ...where 'file.dir' is the file directory containing the catch file to be imported,
  ###           'file.name' is the name of that file, 'tab.name' the associated (unidentified catch) tab,
  ###           'mode.filter' to identify those modes (potentially) included in this assessment,
  ###       and 'params' the R object that (amongst other things) identifies the filters applied in this SEDAR...
  
  
  unid.catch <- read_excel( path  = paste0( file.dir,"/",file.name ),
                            sheet = tab.name, trim_ws=FALSE, col_types="text" )
  
  colnames(unid.catch) = toupper( colnames(unid.catch) )
  if( "YEAR" %notin% colnames(unid.catch) & "INT_YEAR" %in% colnames(unid.catch) ) {
    colnames(unid.catch)[ colnames(unid.catch) %in% c("INT_YEAR") ] = "YEAR"
  }
  
  ### SUBSETTING ###
  ###     ...while the excel files we provide to analysts are already subsetted for the assessment of interest,
  ###         there are times where Vivian and I may need to go back and apply some additional subsetting
  ###         (e.g., after choices made during the data workshop). While a new excel file is presumably produced
  ###         after such decisions, I add a few lines of code below to ensure the 'proper' subsetting is applied
  ###         even if pulling from the old (out-of-date) estimates...
  ###
  ###     ...producing GenRec catch estimates, so need to drop SRHS...
  unid.catch <- unid.catch[ which( unid.catch$DS != "SRHS" ), ]
  ###     ...subsetting by year...
  unid.catch <- unid.catch[ which( unid.catch$YEAR %in% (1981:params$term.year) ), ]
  ###     ...subsetting by area...
  unid.catch <- unid.catch[ which( unid.catch$NEW_STA %in% params$subset.states ), ]
  
  if( "FL" %in% params$subset.states | "FLW" %in% params$subset.states | "FLE" %in% params$subset.states ) {
    unid.catch <- unid.catch[ which(
      is.na(unid.catch$FL_REG) | unid.catch$FL_REG == "" | unid.catch$FL_REG %in% params$subset.FL ), ]
  }
  # if( "NC" %in% states ) {
  #   unid.catch <- unid.catch[ which(
  #     is.na(unid.catch$NC_REG) | unid.catch$NC_REG == "" | unid.catch$NC_REG %in% params$subset.NC ), ]
  # }
  ###     ...subsetting by mode...
  unid.catch <- unid.catch[ which( unid.catch$NEW_MODEN %in% mode.filter ), ]
  
  ### To avoid duplicating catch estimates from the SRHS survey...
  unid.catch <- unid.catch[ !( unid.catch$NEW_MODEN == "Hbt" & unid.catch$SUB_REG == 6 ), ]
  unid.catch <- unid.catch[ !( unid.catch$NEW_MODEN == "Hbt" & unid.catch$SUB_REG == 7 & unid.catch$YEAR >= 1986 ), ]
  ### Additionally, as per an email from Kelly Fitzpatrick for SEDAR 71 (July 22 2020), it was decided that all MRIP Hbt fishing
  ###       from Monroe County (FL_REG==3) would be excluded from future SEDAR assessments. The thinking is that "most of the
  ###       MRIP HB Monroe County landings come from the Keys [Atlantic side], which is included in the [SRHS] area 12 and 17 estimates."
  ###       Therefore, including them in Atlantic assessments equates to 'double counting' HBT landings from Monroe County
  ###       ( but from two different sources; MRIP & SRHS ) whereas including them in Gulf assessments is equivalent to including
  ###       SATL Hbt landings in a Gulf assessment. Neither of these situations is desirable, so we exclude MRIP Hbt from FL_REG==3...
  if( "FL_REG" %in% colnames(unid.catch) ) {       ### ...where I first check that FL_REG is a field (not seen in some CAR assessments)...
    if( dim( unid.catch[ which( unid.catch$NEW_MODEN == "Hbt" & unid.catch$FL_REG == 3 ), ] )[1] > 0 ) {
      unid.catch <- unid.catch[ -which( unid.catch$NEW_MODEN == "Hbt" & unid.catch$FL_REG == 3 ), ]
    }
  }
  
  ### ADDITIONAL FORMATTING ###
  
  colnames(unid.catch)[ which( colnames(unid.catch) == "YEAR" ) ] <- "Year"
  ###     ...I also remove any rows with missing years (as a precaution)...
  unid.catch <- unid.catch[ which( !is.na(unid.catch$Year) ), ]
  ###     ...and make sure the columns I need (for my flextables) are properly formatted...
  unid.catch$Year <- as.numeric( unid.catch$Year )
  unid.catch$AB1  <- as.numeric( unid.catch$AB1 )
  unid.catch$B2   <- as.numeric( unid.catch$B2 )
  if( "LBSEST_SECWWT" %in% colnames(unid.catch) ) {
    colnames(unid.catch)[ which( colnames(unid.catch) == "LBSEST_SECWWT" ) ] <- "lbsest_SECwwt"
    unid.catch$lbsest_SECwwt <- as.numeric( unid.catch$lbsest_SECwwt )
  }
  if( "LBSEST_SECGWT" %in% colnames(unid.catch) ) {
    colnames(unid.catch)[ which( colnames(unid.catch) == "LBSEST_SECGWT" ) ] <- "lbsest_SECgwt"
    unid.catch$lbsest_SECgwt <- as.numeric( unid.catch$lbsest_SECgwt )
  }
  if( "LBSEST_SEC" %in% colnames(unid.catch) ) {
    colnames(unid.catch)[ which( colnames(unid.catch) == "LBSEST_SEC" ) ] <- "lbsest_SEC"
    unid.catch$lbsest_SEC <- as.numeric( unid.catch$lbsest_SEC )
  }
  if( "LBSEST_SECSOURCE" %in% colnames(unid.catch) ) {
    colnames(unid.catch)[ which( colnames(unid.catch) == "LBSEST_SECSOURCE" ) ] <- "lbsest_SECsource"
  }
  
  
  return( unid.catch )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------


wp.import.catch.numcvs = function( file.type = c('RDI','Catch.File'),
                                   file.dir, file.name, tab.name, mode.filter, params ) {
  ###     ...where 'file.type' identifies the type of import being conducted (i.e., RDI CV report or GenRec catch file )
  ###
  ###       For file.type == 'Catch.File'...
  ###           'file.dir' is the file directory containing the catch file to be imported,
  ###           'file.name' is the name of that file, 'tab.name' the associated (catch CV) tab,
  ###
  ###       For file.type == 'RDI'...
  ###           'mode.filter' identifies those modes (potentially) included in this assessment,
  ###           'params' the R object that (amongst other things) identifies the appropriate CV reports in RDI...
  
  
  ### In deciding which 'file.type' to import...
  ###   ...CV estimates (for catch in numbers) were originally imported from pre-constructed RDI queries
  ###       (of the SEFSC Oracle server), which are (largely) already properly filtered and so the code/function
  ###        was designed to identify the appropriate fields ( for Tables 3, 4, & 5 ) and apply a bit of formatting...
  ###   ...However, CV scripts have since gotten a bit more complicated, requiring uncertainty estimates from other surveys
  ###       (e.g., TPWD & LACR) that account for any applied calibrations (e.g., state:MRIP calibration factors ) or
  ###       imputations of 'missing' catch estimates (e.g., MRIP 1981-wave1, TPWD 1981-(May)1983, TPWD/LACR discards ),
  ###       all of which require different equations to properly carry through the associated uncertainties.
  ###       These calculations are done in the GenRec catch file and generate the resultant "MRIP catCV" tabs, and so
  ###       there really is no need to 'recreate' these steps here. Instead, the code/function below simply imports the
  ###       catch/CV/sample size fields from the appropriate "catCV" tab (in the GenRec file).
  ### As usual, I retain the scripts of both approaches for future reference...
  
  
  if( file.type == 'Catch.File' ) {
    
    cv.table <- read_excel( path  = paste0( file.dir,"/",file.name ),
                            sheet = tab.name, trim_ws=FALSE, col_types="text" )
    ###   ...the formatting of which requires an if() statement to differentiate Caribbean assessments because S&T does not
    ###   provide Caribbean data/estimates in the new file format. Caribbean species are therefore not included
    ###   in the S&T Oracle data tables, which means they aren't passed into RDI and cannot be queried in our reports.
    ###   Instead, CVs for Caribbean assessments are calculated using the old approach (i.e., MRIP variance fields )...
    
    
    if( params$region != "Caribbean" ) {
      
      cv.table = as.data.frame( cv.table ) %>%
        select( -NEW_MODE ) %>%
        
        mutate(  AT = format( round(as.numeric( AT),0), big.mark="," ),
                PSU = format( round(as.numeric(PSU),0), big.mark="," ),
                 ATtotal = format( round(as.numeric( ATtotal),0), big.mark="," ),
                PSUtotal = format( round(as.numeric(PSUtotal),0), big.mark="," ) ) %>%
        mutate( Trp = paste0( str_trim( ATtotal)," (",str_trim( AT),")" ),
                PSU = paste0( str_trim(PSUtotal)," (",str_trim(PSU),")" ) ) %>%
        select( -c( AT,ATtotal, PSUtotal ) ) %>%
        
        pivot_wider( names_from="NEW_MODEN", values_from=c("CAT","CV","PSU","Trp"), names_glue = "{NEW_MODEN}_{.value}" )
      
      
      colnames(cv.table) = toupper( colnames(cv.table) )
      colnames(cv.table)[ colnames(cv.table) == "YEAR" ] = "Year"
      
      colnames(cv.table) = gsub( "CBT_HBT_","CbtHbt_", colnames(cv.table) )
      colnames(cv.table) = gsub(  "CBTHBT_","CbtHbt_", colnames(cv.table) )
      colnames(cv.table) = gsub( "CBT_","Cbt_", colnames(cv.table) )
      colnames(cv.table) = gsub( "HBT_","Hbt_", colnames(cv.table) )
      colnames(cv.table) = gsub( "PRIV_","Priv_", colnames(cv.table) )
      colnames(cv.table) = gsub( "SHORE_","Shore_", colnames(cv.table) )
      colnames(cv.table) = gsub( "TOTAL_","Total_", colnames(cv.table) )
      colnames(cv.table) = gsub( "_TRP","_Trp", colnames(cv.table) )
      
      # cv.AB1.table = cv.table[ ,c( 1, which( grepl( "AB1",colnames(cv.table) ) ) ) ]
      # cv.B2.table  = cv.table[ ,c( 1, which( grepl(  "B2",colnames(cv.table) ) ) ) ]
      
      cv.AB1.table = cv.table %>% filter( CATCH_VAR == 'AB1' ) %>% select( -CATCH_VAR )
      colnames(cv.AB1.table) = gsub( '_CAT','_AB1', colnames(cv.AB1.table) )
      
      cv.B2.table  = cv.table %>% filter( CATCH_VAR == 'B2'  ) %>% select( -CATCH_VAR )
      colnames(cv.B2.table ) = gsub( '_CAT','_B2' , colnames(cv.B2.table ) )
      
      rm( cv.table )
      
      
    } else {
      
      ### CARIBBEAN ASSESSMENTS ###
      
      ###     ...apply a bit of formatting to the column names...
      colnames(cv.table) = toupper( colnames(cv.table) )
      colnames(cv.table)[ which( colnames(cv.table) == "YEAR" ) ] = "Year"
      colnames(cv.table) = unlist( lapply( colnames(cv.table), function(y) gsub("TOTAL_","Total_",y) ) )
      colnames(cv.table) = unlist( lapply( colnames(cv.table), function(y) gsub("CBT_","Cbt_",y) ) )
      colnames(cv.table) = unlist( lapply( colnames(cv.table), function(y) gsub("HBT_","Hbt_",y) ) )
      colnames(cv.table) = unlist( lapply( colnames(cv.table), function(y) gsub("PRIV_","Priv_",y) ) )
      colnames(cv.table) = unlist( lapply( colnames(cv.table), function(y) gsub("SHORE_","Shore_",y) ) )
      
      ###     ...separate into AB1 & B2 tables...
      cv.table <- as.data.frame( cv.table )
      cv.AB1.table <- cv.table[ ,c( grep("Year",colnames(cv.table)), grep("_AB1",colnames(cv.table)) ) ]
      cv.B2.table  <- cv.table[ ,c( grep("Year",colnames(cv.table)), grep("_B2", colnames(cv.table)) ) ]
      rm( cv.table )
      
      ###     ...and shorten some of the field names...
      colnames(cv.AB1.table)[ grep("_CV_AB1",colnames(cv.AB1.table)) ] <- gsub( "_AB1","",
                                              colnames(cv.AB1.table)[ grep("_CV_AB1",colnames(cv.AB1.table)) ] )
      colnames(cv.AB1.table)[ grep("_SS_AB1",colnames(cv.AB1.table)) ] <- gsub( "_AB1","",
                                              colnames(cv.AB1.table)[ grep("_SS_AB1",colnames(cv.AB1.table)) ] )
      colnames(cv.B2.table)[ grep("_CV_B2",colnames(cv.B2.table)) ] <- gsub( "_B2","",
                                              colnames(cv.B2.table)[ grep("_CV_B2",colnames(cv.B2.table)) ] )
      colnames(cv.B2.table)[ grep("_SS_B2",colnames(cv.B2.table)) ] <- gsub( "_B2","",
                                              colnames(cv.B2.table)[ grep("_SS_B2",colnames(cv.B2.table)) ] )
    }
  }
  
  
  # ### OLD SCRIPT BASED ON IMPORT FROM RDI ###
  # if( file.type == 'RDI' ) {
  #   
  #   con = dbConnect(dbDriver("Oracle"), username = keyring::key_list("SECPR")[1,2],
  #                   password = keyring::key_get("SECPR", keyring::key_list("SECPR")[1,2]), dbname = "SECPR")
  #   
  #   
  #   ### CV TABLES (3,4,5) ###
  #   ### ---------------------
  #   ###
  #   if( params$CV.tables.by == "Mode" ) {
  #     
  #     mrip.catch = dbGetQuery(con,
  #                          paste0("select *
  #                                 from rdi.apex_cv_data_yr_m@secapxdv_dblk.sfsc.noaa.gov t
  #                                 where t.APP_USER = ", sprintf("'%s'", paste( params$RDI.ALL, collapse = "','" ))
  #                   ))
  #     
  #     ### I then construct my table, subsetting/formatting 'mrip.catch' to ensure the proper columns
  #     ###     are being imported and columns are being labeled with the proper column name. Because
  #     ###     I will be doing this column-by-column, and will dynamically identify columns
  #     ###     (in 'mrip.catch' ), I apply the eval(parse(text= )) trick...
  #     mrip.cv.mode <- list( )
  #     mrip.cv.mode$Year = mrip.catch$YEAR
  #     eval( parse( text =
  #       paste0( "mrip.cv.mode$",as.character(gsub("/","",mode.filter)), '_AB1 <- mrip.catch$',
  #               as.character(toupper(gsub("/","_",mode.filter))), '_AB1' ) ) )
  #     ###     ...where the different gsub() functions applied to 'mode.filter' are needed for any "Cbt/Hbt" estimates
  #     ###         ( these rows are identified with "CBT_HBT" in RDI and I remove the backslash in my tables )...
  #     eval( parse( text =
  #       paste0( "mrip.cv.mode$",as.character(gsub("/","",mode.filter)), '_CV_AB1 <- mrip.catch$',
  #               as.character(toupper(gsub("/","_",mode.filter))), '_CV_AB1' ) ) )
  #     eval( parse( text =
  #       paste0( "mrip.cv.mode$",as.character(gsub("/","",mode.filter)), '_B2 <- mrip.catch$',
  #               as.character(toupper(gsub("/","_",mode.filter))), '_B2' ) ) )
  #     eval( parse( text =
  #       paste0( "mrip.cv.mode$",as.character(gsub("/","",mode.filter)), '_CV_B2 <- mrip.catch$',
  #               as.character(toupper(gsub("/","_",mode.filter))), '_CV_B2' ) ) )
  #     
  #     ### SAMPLE SIZE ###
  #     
  #     ### PSU
  #     eval( parse( text =
  #       paste0( "mrip.cv.mode$",as.character(gsub("/","",mode.filter)), '_PSU_AB1 <- mrip.catch$',
  #               as.character(toupper(gsub("/","_",mode.filter))), '_N' ) ) )
  #     ###     ...where "PSU_AB1" = number of PSUs that intercepted this spp as an AB1...
  #     eval( parse( text =
  #       paste0( "mrip.cv.mode$",as.character(gsub("/","",mode.filter)), '_PSU_B2 <- mrip.catch$',
  #               as.character(toupper(gsub("/","_",mode.filter))), '_N_B2' ) ) )
  #     eval( parse( text =
  #       paste0( "mrip.cv.mode$",as.character(gsub("/","",mode.filter)), '_PSU_Tot <- mrip.catch$',
  #               as.character(toupper(gsub("/","_",mode.filter))), '_SZ' ) ) )
  #     ###     ...where "PSU_Tot" = total number of PSUs...
  #     
  #     ### Angler Trips
  #     eval( parse( text =
  #       paste0( "mrip.cv.mode$",as.character(gsub("/","",mode.filter)), '_AT_AB1 <- mrip.catch$',
  #               as.character(toupper(gsub("/","_",mode.filter))), '_SZ_AT' ) ) )
  #     eval( parse( text =
  #       paste0( "mrip.cv.mode$",as.character(gsub("/","",mode.filter)), '_AT_B2 <- mrip.catch$',
  #               as.character(toupper(gsub("/","_",mode.filter))), '_SZ_B2_AT' ) ) )
  #     eval( parse( text =
  #       paste0( "mrip.cv.mode$",as.character(gsub("/","",mode.filter)), '_AT_Tot <- mrip.catch$T',
  #               as.character(toupper(gsub("/","_",mode.filter))), '_SZ_AT' ) ) )
  #     
  #     
  #     
  #     ### ...where "mrip.cv.mode" has all mode-specific estimates, but not TOTALs. Therefore, I do one more pull...
  #     mrip.catch = dbGetQuery(con,
  #                          paste0("select
  #                                 t.YEAR,
  #                                 t.AB1, t.CV_AB1, t.B2,  t.CV_B2 , t.B2_N , t.AB1_SZ , t.B2_SZ , t.ATSZ , t.AB1_ATN , t.B2_ATN
  #                                 from rdi.apex_cv_data_yr@secapxdv_dblk.sfsc.noaa.gov t
  #                                 where t.APP_USER = ", sprintf("'%s'", paste( params$RDI.ALL, collapse = "','" ))
  #                   ))
  #     colnames(mrip.catch)[1] <- "Year"
  #     colnames(mrip.catch)[(-1)] <- paste0( "Total_",colnames(mrip.catch)[-1] )   ### ...add "Total_" to colnames...
  #     ###   ...and changing sample size field names to match the format used in "mrip.cv.mode"...
  #     colnames(mrip.catch)[ which( colnames(mrip.catch)=="Total_B2_N" ) ] = "Total_PSU_Tot"
  #     colnames(mrip.catch)[ which( colnames(mrip.catch)=="Total_AB1_SZ" ) ] = "Total_PSU_AB1"
  #     colnames(mrip.catch)[ which( colnames(mrip.catch)=="Total_B2_SZ" ) ] = "Total_PSU_B2"
  #     colnames(mrip.catch)[ which( colnames(mrip.catch)=="Total_ATSZ" ) ] = "Total_AT_Tot"
  #     colnames(mrip.catch)[ which( colnames(mrip.catch)=="Total_AB1_ATN" ) ] = "Total_AT_AB1"
  #     colnames(mrip.catch)[ which( colnames(mrip.catch)=="Total_B2_ATN" ) ] = "Total_AT_B2"
  #     ###     ...and then add the TOTALs to "mrip.cv.mode"...
  #     mrip.cv.mode <- full_join( as.data.frame(mrip.cv.mode), as.data.frame(mrip.catch), by="Year" )
  #     
  #     
  #     
  #     ### I then separate the AB1 & B2 statistics into separate tables...
  #     mrip.cv.mode <- as.data.frame( mrip.cv.mode )
  #     cv.AB1.table <- mrip.cv.mode[ ,c( grep("Year",colnames(mrip.cv.mode)), grep("_AB1",colnames(mrip.cv.mode)),
  #                                         grep("_PSU_Tot",colnames(mrip.cv.mode)), grep("_AT_Tot",colnames(mrip.cv.mode)) ) ]
  #     cv.B2.table <- mrip.cv.mode[ ,c( grep("Year",colnames(mrip.cv.mode)), grep( "_B2",colnames(mrip.cv.mode)),
  #                                        grep("_PSU_Tot",colnames(mrip.cv.mode)), grep("_AT_Tot",colnames(mrip.cv.mode)) ) ]
  #     rm( mrip.catch, mrip.cv.mode )
  #     
  #     ###   ...and, lastly, because AB1 & B2 are now separated, I shortened the "CV" colnames...
  #     colnames(cv.AB1.table)[ grep("_CV_AB1",colnames(cv.AB1.table)) ] <- gsub( "_AB1","",
  #                                                   colnames(cv.AB1.table)[ grep("_CV_AB1",colnames(cv.AB1.table)) ] )
  #     colnames(cv.B2.table)[ grep("_CV_B2",colnames(cv.B2.table)) ] <- gsub( "_B2","",
  #                                                   colnames(cv.B2.table)[ grep("_CV_B2",colnames(cv.B2.table)) ] )
  #     
  #   } else if( params$CV.tables.by == "Area" ) {
  #     
  #     # ###     ...which needs to be updated to include BOTH sample size fields (PSU and AT)...
  #     
  #     
  #     # mrip.catch = dbGetQuery(con,
  #     #                      paste0("select *
  #     #                             from rdi.apex_cv_data_yr_s@secapxdv_dblk.sfsc.noaa.gov t
  #     #                             where t.APP_USER = ", sprintf("'%s'", paste( params$RDI.ALL, collapse = "','" ))
  #     #               ))
  #     # mrip.cv.area <- list( )
  #     # mrip.cv.area$Year = mrip.catch$YEAR
  #     # eval( parse( text =
  #     #   paste0( "mrip.cv.area$",as.character(params$subset.states), '_AB1 <- mrip.catch$',
  #     #           as.character(toupper(params$subset.states)), '_AB1' ) ) )
  #     # eval( parse( text =
  #     #   paste0( "mrip.cv.area$",as.character(params$subset.states), '_CV_AB1 <- mrip.catch$',
  #     #           as.character(toupper(params$subset.states)), '_CV_AB1' ) ) )
  #     # eval( parse( text =
  #     #   paste0( "mrip.cv.area$",as.character(params$subset.states), '_B2 <- mrip.catch$',
  #     #           as.character(toupper(params$subset.states)), '_B2' ) ) )
  #     # eval( parse( text =
  #     #   paste0( "mrip.cv.area$",as.character(params$subset.states), '_CV_B2 <- mrip.catch$',
  #     #           as.character(toupper(params$subset.states)), '_CV_B2' ) ) )
  #     # eval( parse( text =
  #     #   paste0( "mrip.cv.area$",as.character(params$subset.states), '_N <- mrip.catch$',
  #     #           as.character(toupper(params$subset.states)), '_N' ) ) )
  #     # ###     ...where "N" = number of angler trips that intercepted this spp (either AB1 or B2)...
  #     # eval( parse( text =
  #     #   paste0( "mrip.cv.area$",as.character(params$subset.states), '_Trp <- mrip.catch$',
  #     #           as.character(toupper(params$subset.states)), '_SZ' ) ) )
  #     #
  #     #
  #     # mrip.catch = dbGetQuery(con,
  #     #                      paste0("select
  #     #                             t.YEAR,
  #     #                             t.AB1, t.CV_AB1, t.B2,  t.CV_B2 , t.B2_N , t.B2_SZ
  #     #                             from rdi.apex_cv_data_yr@secapxdv_dblk.sfsc.noaa.gov t
  #     #                             where t.APP_USER = ", sprintf("'%s'", paste( params$RDI.ALL, collapse = "','" ))
  #     #               ))
  #     # colnames(mrip.catch)[1] <- "Year"
  #     # colnames(mrip.catch)[(-1)] <- paste0( "Total_",colnames(mrip.catch)[-1] )   ### ...add "Total_" to colnames...
  #     # ###     ...and, because "N" & "SZ" have the opposite meanings in the by-year table...
  #     # colnames(mrip.catch)[ grep( "_N",colnames(mrip.catch)) ] <- "Total_Trp"
  #     # colnames(mrip.catch)[ grep("_SZ",colnames(mrip.catch)) ] <- "Total_N"
  #     # ###     ...and then add the TOTALs to "mrip.cv.area"...
  #     # mrip.cv.area <- full_join( as.data.frame(mrip.cv.area), as.data.frame(mrip.catch), by="Year" )
  #     #
  #     #
  #     # mrip.cv.area <- as.data.frame( mrip.cv.area )
  #     # cv.AB1.table <- mrip.cv.area[ ,c( grep("Year",colnames(mrip.cv.area)), grep("_AB1",colnames(mrip.cv.area)),
  #     #                                     grep("_N",colnames(mrip.cv.area)), grep("_Trp",colnames(mrip.cv.area)) ) ]
  #     # cv.B2.table <- mrip.cv.area[ ,c( grep("Year",colnames(mrip.cv.area)), grep( "_B2",colnames(mrip.cv.area)),
  #     #                                    grep("_N",colnames(mrip.cv.area)), grep("_Trp",colnames(mrip.cv.area)) ) ]
  #     # rm( mrip.catch, mrip.cv.area )
  #     #
  #     #
  #     # colnames(cv.AB1.table)[ grep("_CV_AB1",colnames(cv.AB1.table)) ] <- gsub( "_AB1","",
  #     #                                               colnames(cv.AB1.table)[ grep("_CV_AB1",colnames(cv.AB1.table)) ] )
  #     # colnames(cv.B2.table)[ grep("_CV_B2",colnames(cv.B2.table)) ] <- gsub( "_B2","",
  #     #                                               colnames(cv.B2.table)[ grep("_CV_B2",colnames(cv.B2.table)) ] )
  #     
  #   }
  #   
  # }
  
  return.object <- list( cv.AB1.table, cv.B2.table )
  names( return.object ) <- c( "cv.AB1.table","cv.B2.table" )
  rm( cv.AB1.table, cv.B2.table )
  
  
  return( return.object )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------


wp.import.catch.wgtcvs = function( file.dir, file.name, tab.name, catch.table ) {
  ###     ...where 'file.dir' is the file directory containing the catch file to be imported,
  ###           'file.name' is the name of that file, 'tab.name' the associated (avgwgt CV) tab,
  ###       and 'catch.table' is the table of AB1 & LBS estimates from which SEFSC avgwgts are calculated
  ###               and used to replace the averages (from raw size data) currently in the avgwgt-CV table...
  
  
  cv.AvgWt.table <- read_excel( path  = paste0( file.dir,"/",file.name ),
                                sheet = tab.name, trim_ws=FALSE, col_types="text" )
  
  colnames(cv.AvgWt.table) = toupper( colnames(cv.AvgWt.table) )
  colnames(cv.AvgWt.table)[ colnames(cv.AvgWt.table) == "YEAR" ] = "Year"
  
  cv.AvgWt.table = as.data.frame( cv.AvgWt.table ) %>%
    select( -any_of( c('NEW_MODE') ) ) %>%
    mutate( NEW_MODEN = ifelse( NEW_MODEN ==     'CBT',    'Cbt',
                        ifelse( NEW_MODEN ==     'HBT',    'Hbt',
                        ifelse( NEW_MODEN ==    'PRIV',   'Priv',
                        ifelse( NEW_MODEN ==   'SHORE',  'Shore',
                        ifelse( NEW_MODEN == 'CBT_HBT', 'CbtHbt',
                        ifelse( NEW_MODEN ==   'TOTAL',  'Total', NA )))))) )
  
  if( 'FED_CLOSED' %in% colnames(cv.AvgWt.table) ) {
    if( '0' %in% unique(cv.AvgWt.table$FED_CLOSED) ) {
      cv.AvgWt.table = cv.AvgWt.table %>%
        mutate( FED_CLOSED = ifelse( FED_CLOSED == "0", 'open',
                             ifelse( FED_CLOSED == "1", 'closed',
                             ifelse( FED_CLOSED == "2", 'partial', NA ))) )
    }
  }
  
  cv.AvgWt.table = as.data.frame( cv.AvgWt.table ) %>%
    ###   ...combining sample size variables ( TRP & FISH ) into a single field...
    mutate(  TRP = ifelse( is.na( TRP), NA, format( round(as.numeric( TRP),0), big.mark="," ) ),
            FISH = ifelse( is.na(FISH), NA, format( round(as.numeric(FISH),0), big.mark="," ) ) ) %>%
    mutate(    N = ifelse( is.na(FISH), NA, paste0( str_trim(TRP)," (",str_trim(FISH),")" ) ) ) %>%
    select( -c( TRP,FISH ) ) %>%
    
    ###   ...convert SE's into CVs...
    mutate_at( vars( Year,WGT,SE ), list( ~ as.numeric(.) ) ) %>%
    mutate( CV = SE / WGT ) %>%
    select( -SE )
    
    # pivot_wider( names_from="NEW_MODEN", values_from=c("WGT","CV","N"), names_glue = "{NEW_MODEN}_{.value}" )
  
  
  ###     ...replace avgwgts calculated at the data-level with those representing that applied to
  ###         the corresponding catch estimates (i.e., those from 'catch.table' )...
  dummy1 = catch.table %>%
    mutate( NEW_MODEN = ifelse( NEW_MODEN == "Priv/Shore","Priv", NEW_MODEN ) ) %>%
    group_by( across( any_of( c('SID','Year','NEW_MODEN','FED_CLOSED') ) ) ) %>%
    summarise( AB1 = sum( AB1, na.rm=TRUE ),
               LBS = sum( lbsest_SECwwt, na.rm=TRUE ) ) %>%
    mutate( WGT = ifelse( AB1==0, NA, LBS / AB1 ) ) %>%
    ungroup()
  dummy2 = catch.table %>%
    group_by( across( any_of( c('SID','Year','FED_CLOSED') ) ) ) %>%
    summarise( AB1 = sum( AB1, na.rm=TRUE ),
               LBS = sum( lbsest_SECwwt, na.rm=TRUE ) ) %>%
    mutate( NEW_MODEN = "Total",
            WGT = ifelse( AB1==0, NA, LBS / AB1 ) ) %>%
    ungroup()
  
  blah = dummy1 %>% bind_rows(dummy2) %>%
    arrange( across( any_of( c('SID','Year','NEW_MODEN','FED_CLOSED') ) ) ) %>%
    select(  any_of( c('SID','Year','NEW_MODEN','FED_CLOSED','WGT') ) )
  rm( dummy1, dummy2 )
  
  
  col.vec = c('Year','NEW_MODEN')
  if( 'FED_CLOSED' %in% colnames(cv.AvgWt.table) ) { col.vec = c('FED_CLOSED',col.vec) }
  if( 'SID' %in% colnames(cv.AvgWt.table) ) { col.vec = c('SID',col.vec) }
  
  dummy = cv.AvgWt.table %>%
    full_join( blah, by=col.vec, suffix=c("_data","_SEC") ) %>%
    rename( WGT = WGT_SEC ) %>%
    select( any_of( c('SID','Year','NEW_MODEN','FED_CLOSED', 'WGT','CV','N' ) ) )
  rm( col.vec )
  
  
  cv.AvgWt.table = dummy %>%
    pivot_wider( names_from = "NEW_MODEN", values_from = c("WGT","CV","N"), names_glue = "{NEW_MODEN}_{.value}" )
  rm( dummy, blah )
  
  
  return( cv.AvgWt.table )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------


wp.import.catch.lbscvs = function( file.dir, file.name, tab.name, catch.table ) {
  ###     ...where 'file.dir' is the file directory containing the catch file to be imported,
  ###           'file.name' is the name of that file, 'tab.name' the associated (avgwgt CV) tab,
  ###       and 'catch.table' is the table of AB1 & LBS estimates from which SEFSC avgwgts are calculated
  ###               and used to replace the averages (from raw size data) currently in the avgwgt-CV table...
  
  
  cv.WWT.table <- read_excel( path  = paste0( file.dir,"/",file.name ),
                              sheet = tab.name, trim_ws=FALSE, col_types="text" )
  
  
  colnames(cv.WWT.table) = toupper( colnames(cv.WWT.table) )
  colnames(cv.WWT.table)[ colnames(cv.WWT.table) == "YEAR" ] = "Year"
  
  cv.WWT.table = as.data.frame( cv.WWT.table ) %>%
    select( -any_of( c('NEW_MODE') ) ) %>%
    mutate( NEW_MODEN = ifelse( NEW_MODEN ==     'CBT',    'Cbt',
                        ifelse( NEW_MODEN ==     'HBT',    'Hbt',
                        ifelse( NEW_MODEN ==    'PRIV',   'Priv',
                        ifelse( NEW_MODEN ==   'SHORE',  'Shore',
                        ifelse( NEW_MODEN == 'CBT_HBT', 'CbtHbt',
                        ifelse( NEW_MODEN ==   'TOTAL',  'Total', NA )))))) ) %>%
    
    rename( LBS = LBS_CAT,
             CV = LBS_CV  ) %>%
    mutate_at( vars( Year,LBS,CV ), list( ~ as.numeric(.) ) )
    
    # pivot_wider( names_from="NEW_MODEN", values_from=c("LBS","CV"), names_glue = "{NEW_MODEN}_{.value}" )
  
  
  ###     ...replace landings-in-weights in the CV table with those in 'catch.table'...
  dummy1 = catch.table %>%
    mutate( NEW_MODEN = ifelse( NEW_MODEN == "Priv/Shore","Priv", NEW_MODEN ) ) %>%
    group_by( across( any_of( c('SID','Year','NEW_MODEN','FED_CLOSED') ) ) ) %>%
    summarise( LBS = sum( lbsest_SECwwt, na.rm=TRUE ) ) %>%
    ungroup()
  dummy2 = catch.table %>%
    group_by( across( any_of( c('SID','Year','FED_CLOSED') ) ) ) %>%
    summarise( LBS = sum( lbsest_SECwwt, na.rm=TRUE ) ) %>%
    mutate( NEW_MODEN = "Total" ) %>%
    ungroup()
  
  blah = dummy1 %>% bind_rows(dummy2) %>%
    arrange( across( any_of( c('SID','Year','NEW_MODEN','FED_CLOSED') ) ) ) %>%
    select(  any_of( c('SID','Year','NEW_MODEN','FED_CLOSED','LBS') ) )
  rm( dummy1, dummy2 )
  
  
  col.vec = c('Year','NEW_MODEN')
  if( 'FED_CLOSED' %in% colnames(cv.WWT.table) ) { col.vec = c('FED_CLOSED',col.vec) }
  if( 'SID' %in% colnames(cv.WWT.table) ) { col.vec = c('SID',col.vec) }
  
  # dummy = cv.WWT.table %>%
  #   pivot_longer( !Year, names_to = "VARIABLE", values_to = "VALUE" ) %>%
  #   mutate( NEW_MODEN = gsub( "_.*","", VARIABLE ),
  #           VARIABLE  = gsub( ".*_","", VARIABLE ) ) %>%
  #   pivot_wider( names_from = VARIABLE, values_from = VALUE ) %>%
  #   mutate_at( vars( Year,LBS,CV ), list( ~ as.numeric(.) ) ) %>%
  
  dummy = cv.WWT.table %>%
    full_join( blah, by=col.vec, suffix=c("_est","_SEC") ) %>%
    rename( LBS = LBS_SEC ) %>%
    mutate( LBS = ifelse( LBS==0, NA, LBS ) ) %>%
    select( any_of( c('SID','Year','NEW_MODEN','FED_CLOSED', 'LBS','CV' ) ) )
  rm( col.vec )
  
  cv.WWT.table = dummy %>%
    pivot_wider( names_from = "NEW_MODEN", values_from = c("LBS","CV"), names_glue = "{NEW_MODEN}_{.value}" )
  rm( dummy, blah )
  
  
  return( cv.WWT.table )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------


wp.import.catch.cbtcvs = function( params ) {
  ###     ...where 'params' is the R object that (amongst other things) identifies the appropriate CV report in RDI...
  
  
  con = dbConnect(dbDriver("Oracle"), username = keyring::key_list("SECPR")[1,2],
                  password = keyring::key_get("SECPR", keyring::key_list("SECPR")[1,2]), dbname = "SECPR")
  
  
  mrip.catch = dbGetQuery(con,
                          paste0("select *
                            from rdi.apex_cv_data_yr@secapxdv_dblk.sfsc.noaa.gov t
                            where t.APP_USER = ", sprintf("'%s'", paste( params$RDI.CBT, collapse = "','" ))
                          ))
  cv.Cbt.fig = data.frame( Year = mrip.catch$YEAR,
                           AB1 = mrip.catch$AB1, CV_AB1 = mrip.catch$CV_AB1,
                           B2 = mrip.catch$B2, CV_B2 = mrip.catch$CV_B2,
                           CHTS_AB1 = mrip.catch$CHTS_AB1, CHTS_CV_AB1 = mrip.catch$CHTS_CV_AB1,
                           CHTS_B2 = mrip.catch$CHTS_B2, CHTS_CV_B2 = mrip.catch$CHTS_CV_B2 )
  rm( mrip.catch )
  
  
  return( cv.Cbt.fig )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------


wp.import.comp.sedar = function( file.dir, file.name, tab.name ) {
  ###     ...where 'file.dir' is the file directory containing the catch file to be imported,
  ###           'file.name' is the name of that file, and 'tab.name' the associated (SEDAR catch comparison) tab...
  
  
  sedar.comp <- read_excel( path  = paste0( file.dir,"/",file.name ),
                            sheet = tab.name, trim_ws=FALSE, col_types="text" )
  
  
  colnames(sedar.comp) = gsub( "\\...*","", colnames(sedar.comp) )
  
  ### Adding the respective SEDAR number to each column of "AB1" and "B2" estimates...
  colnames(sedar.comp)[ which(grepl("AB1",colnames(sedar.comp))) ] = paste0(
    gsub( "EDAR ","", colnames(sedar.comp) )[ which(grepl("AB1",colnames(sedar.comp))) - 1 ], "_AB1" )
  colnames(sedar.comp)[ which(grepl("B2",colnames(sedar.comp))) ] = paste0(
    gsub( "EDAR ","", colnames(sedar.comp) )[ which(grepl("B2",colnames(sedar.comp))) - 2 ], "_B2" )
  
  ### Removing the 'YEAR' column of all subsequent sets of SEDAR catch estimates
  ###       (i.e., only keeping the first column named 'SEDAR xx' )...
  sedar.comp = sedar.comp[ ,c( colnames(sedar.comp)[ grepl("SEDAR ",colnames(sedar.comp)) ][1],
                               colnames(sedar.comp)[ grepl("AB1",colnames(sedar.comp)) | grepl("B2",colnames(sedar.comp)) ] ) ]
  colnames(sedar.comp)[1] = "Year"
  
  
  return( sedar.comp )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------


wp.import.comp.calib = function( file.dir, region, species, skip.lines, cat.metric = c('AB1','B2') ) {
  ###     ...where 'file.dir' is the file directory containing the calibrated catch estimates to be imported,
  ###           'region' and 'species' identify the stock being assessed in this SEDAR,
  ###           'skip.lines' is the number of lines to skip when importing the file,
  ###       and 'cat.metric' identifies the catch metric being compiled (e.g., AB1 vs. B2 )...
  
  
  Fig2.data <- read.csv( paste0( file.dir,"/Fig2_",cat.metric,"_",region,"_",species,".csv" ), skip=skip.lines )
  ###   ...where the data file compiled for this summary (i.e., FIGURE 2 ) follows a standard naming convention:
  ###                 "Fig2_AB1_<REGION>_<SPECIES>"   or    "Fig2_B2_<REGION>_<SPECIES>
  ###   ...with region is included to accommodate assessments that span multiple regions (e.g., S68 - GOM+SATL scamp )...
  
  
  colnames(Fig2.data)[ which( tolower(colnames(Fig2.data)) == "year" ) ] <- "Year"
  
  ### AB1 & B2 Estimates ###
  ###   ...for which I shorten the names of these columns...
  if( cat.metric == 'AB1' ) {
    colnames( Fig2.data )[ which( colnames(Fig2.data) == "BASE.Total.Harvest..A.B1." ) ] = "BASE"
    colnames( Fig2.data )[ which( colnames(Fig2.data) == "ACAL.Total.Harvest..A.B1." ) ] = "ACAL"
    colnames( Fig2.data )[ which( colnames(Fig2.data) == "FCAL.Total.Harvest..A.B1." ) ] = "FCAL"
  } else if( cat.metric == 'B2' ) {
    colnames( Fig2.data )[ which( colnames(Fig2.data) == "BASE.Released.Alive..B2." ) ] = "BASE"
    colnames( Fig2.data )[ which( colnames(Fig2.data) == "ACAL.Released.Alive..B2." ) ] = "ACAL"
    colnames( Fig2.data )[ which( colnames(Fig2.data) == "FCAL.Released.Alive..B2." ) ] = "FCAL"
  }
  ###   ...and the associated error estimates, which are already appropriately named but need to be converted
  ###       into absolute error units ( from PSE )...
  Fig2.data$BASE.PSE <- Fig2.data$BASE*(Fig2.data$BASE.PSE/100)
  Fig2.data$ACAL.PSE <- Fig2.data$ACAL*(Fig2.data$ACAL.PSE/100)
  Fig2.data$FCAL.PSE <- Fig2.data$FCAL*(Fig2.data$FCAL.PSE/100)
  
  
  return( Fig2.data )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------


wp.import.size = function( file.dir, file.name, tab.name, mode.filter, params, new_maxwt ) {
  ###     ...where 'file.dir' is the file directory containing the size file to be imported,
  ###           'file.name' is the name of that file, 'tab.name' the associated (raw size) tab,
  ###           'mode.filter' to identify those modes (potentially) included in this assessment,
  ###           'params' the R object that (amongst other things) identifies the filters applied in this SEDAR,
  ###       and 'new_maxwt' the maximum weight believed realistic for this species...
  
  
  raw.size <- read_excel( path  = paste0( file.dir,"/",file.name ),
                          sheet = tab.name, trim_ws=FALSE, col_types="text" )
  
  
  colnames(raw.size) = toupper( colnames(raw.size) )
  
  # if( "YEAR" %notin% colnames(raw.size) & "INT_YEAR" %in% colnames(raw.size) ) {
  #   colnames(raw.size)[ colnames(raw.size) %in% c("INT_YEAR") ] = "YEAR"
  # }
  
  ### RENAMING ###
  raw.size = raw.size %>%
    rename( DS = SAMPLING_PROGRAM,
            NEW_STA = STATE_LANDED,
            FL_mm = OBSERVED_FL_MM,
            TL_mm = OBSERVED_MAXIMUM_TL_MM ) %>%
    mutate( NEW_MODEN = ifelse( FISHING_MODE == 'PR', 'Priv',
                        ifelse( FISHING_MODE == 'CB', 'Cbt', 
                        ifelse( FISHING_MODE == 'SH', 'Shore',
                        ifelse( FISHING_MODE == 'HB', 'Hbt', NA )))),
            ID_CODE  = ifelse( DS == 'MRIP', SAMPLING_UNIT_ID, NA ),
            TRIP_KEY = ifelse( DS == 'TPWD', SAMPLING_UNIT_ID, NA ),
            SUPPLIER_SAMPLE_ID = ifelse( DS %in% c('LA BIO','LA Creel'), SAMPLING_UNIT_ID, NA ),
            LNGTH    = ifelse( DS == 'MRIP',                   FL_mm, NA ),
            LENGTH   = ifelse( DS == 'TPWD',                   TL_mm, NA ),
            LENGTH_1 = ifelse( DS %in% c('LA BIO','LA Creel'), FL_mm, NA ),
            LENGTH_3 = ifelse( DS %in% c('LA BIO','LA Creel'), TL_mm, NA ),
            WGT               = ifelse( DS == 'MRIP',                   as.numeric(ALL_LBS) * 0.453592, NA ),
            WHOLE_WEIGHT_KILO = ifelse( DS %in% c('LA BIO','LA Creel'), as.numeric(ALL_LBS) * 0.453592, NA ) )
  
  ### I also apply some as.numeric() formatting to ensure my size variables are numeric and that
  ###       my flextables are sorted properly ( e.g., in geographical order -- TX=1, LA=2, etc )...
  raw.size[ colnames(raw.size) %in% c("YEAR","NEW_ST","NEW_MODE","FL_mm","TL_mm","ALL_LBS") ] =
    sapply( raw.size[ colnames(raw.size) %in% c("YEAR","NEW_ST","NEW_MODE","FL_mm","TL_mm","ALL_LBS") ], as.numeric )
  
  
  ### SUBSETTING ###
  raw.size <- raw.size[ which( raw.size$DS != "SRHS" ), ]
  raw.size <- raw.size[ which( raw.size$YEAR %in% (1981:params$term.year) ), ]
  raw.size <- raw.size[ which( raw.size$NEW_STA %in% params$subset.states ), ]
  if( "FL" %in% params$subset.states | "FLW" %in% params$subset.states | "FLE" %in% params$subset.states ) {
    raw.size <- raw.size[ which(
      is.na(raw.size$FL_REG) | raw.size$FL_REG == "" | raw.size$FL_REG == 8 | raw.size$FL_REG %in% params$subset.FL ), ]
  }
  
  raw.size <- raw.size[ which( raw.size$NEW_MODEN %in% mode.filter ), ]
  
  raw.size <- raw.size[ !( raw.size$NEW_MODEN == "Hbt" & raw.size$SUB_REG == 6 ), ]
  raw.size <- raw.size[ !( raw.size$NEW_MODEN == "Hbt" & raw.size$SUB_REG == 7 & raw.size$YEAR >= 1986 ), ]
  if( dim( raw.size[ which( raw.size$NEW_MODEN == "Hbt" & raw.size$FL_REG == 3 ), ] )[1] > 0 ) {
    raw.size <- raw.size[ -which( raw.size$NEW_MODEN == "Hbt" & raw.size$FL_REG == 3 ), ]
  }
  
  
  ### ADDITIONAL FORMATTING ###
  colnames(raw.size)[ which( colnames(raw.size) == "YEAR"    ) ] <- "Year"
  colnames(raw.size)[ which( colnames(raw.size) == "FL_MM"   ) ] <- "FL_mm"
  colnames(raw.size)[ which( colnames(raw.size) == "TL_MM"   ) ] <- "TL_mm"
  # colnames(raw.size)[ which( colnames(raw.size) == "LBS" ) ] <- "lbs"
  # colnames(raw.size)[ which( colnames(raw.size) == "WGT" ) ] <- "WGT"
  
  raw.size <- raw.size[ which( !is.na(raw.size$Year) ), ]    ### ...remove any rows with missing years
  
  
  ### MAX SIZE FILTER ###
  ###       ...as a last step, I filter 'raw.size' to remove any records of weight observations that are unrealistic
  ###           for the stock ( ALL_LBS > 'new_maxwt' ), but retain those records that lack an ALL_LBS estimate
  ###           ( is.na(ALL_LBS) ) -- of which the latter likely corresponds to records with length data but not wgt...
  
  if( params$species.add == 'None' ) {
    raw.size = raw.size %>%
      filter( is.na(raw.size$ALL_LBS) | as.numeric(raw.size$ALL_LBS) <= new_maxwt )
  } else {
    raw.size = raw.size %>%
      filter( is.na(raw.size$ALL_LBS) | 
                ( as.numeric(raw.size$ALL_LBS) <= new_maxwt     & raw.size$NEW_COM == tolower(params$species.name) ) |
                ( as.numeric(raw.size$ALL_LBS) <= new_maxwt_add & raw.size$NEW_COM == tolower(params$species.add)  ) )
  }
  
  
  return( raw.size )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------


wp.import.size.nmrip = function( file.dir, file.name, tab.name, mode.filter, params ) {
  ###     ...where 'file.dir' is the file directory containing the size file to be imported,
  ###           'file.name' is the name of that file, 'tab.name' the associated (sample size) tab,
  ###           'mode.filter' to identify those modes (potentially) included in this assessment,
  ###       and 'params' the R object that (amongst other things) identifies the filters applied in this SEDAR...
  
  
  n.mrip.size <- read_excel( path  = paste0( file.dir,"/",file.name ),
                             sheet = tab.name, trim_ws=FALSE, col_types="text" )
  
  
  colnames(n.mrip.size) = toupper( colnames(n.mrip.size) )
  if( "YEAR" %notin% colnames(n.mrip.size) & "INT_YEAR" %in% colnames(n.mrip.size) ) {
    colnames(n.mrip.size)[ colnames(n.mrip.size) %in% c("INT_YEAR") ] = "YEAR"
  }
  
  ### SUBSETTING ###
  n.mrip.size <- n.mrip.size[ which( n.mrip.size$DS %in% c("MRIP","MRFSS") ), ]
  n.mrip.size <- n.mrip.size[ which( n.mrip.size$YEAR %in% (1981:params$term.year) ), ]
  n.mrip.size <- n.mrip.size[ which( n.mrip.size$NEW_STA %in% params$subset.states ), ]
  if( "FL" %in% params$subset.states | "FLW" %in% params$subset.states | "FLE" %in% params$subset.states ) {
    n.mrip.size <- n.mrip.size[ which(
      is.na(n.mrip.size$FL_REG) | n.mrip.size$FL_REG == "" | n.mrip.size$FL_REG == 8 | n.mrip.size$FL_REG %in% params$subset.FL ), ]
  }
  n.mrip.size <- n.mrip.size[ which( n.mrip.size$NEW_MODEN %in% mode.filter ), ]
  
  n.mrip.size <- n.mrip.size[ !( n.mrip.size$NEW_MODEN == "Hbt" & n.mrip.size$SUB_REG == 6 ), ]
  n.mrip.size <- n.mrip.size[ !( n.mrip.size$NEW_MODEN == "Hbt" & n.mrip.size$SUB_REG == 7 & n.mrip.size$YEAR >= 1986 ), ]
  if( "FL_REG" %in% colnames(n.mrip.size) ) {
    if( dim( n.mrip.size[ which( n.mrip.size$NEW_MODEN == "Hbt" & n.mrip.size$FL_REG == 3 ), ] )[1] > 0 ) {
      n.mrip.size <- n.mrip.size[ -which( n.mrip.size$NEW_MODEN == "Hbt" & n.mrip.size$FL_REG == 3 ), ]
    }
  }
  
  ### FORMATTING ###
  colnames(n.mrip.size)[ which( colnames(n.mrip.size) == "YEAR" ) ] <- "Year"
  colnames(n.mrip.size)[ which( colnames(n.mrip.size) == "ID_CODE" ) ] <- "id_code"
  n.mrip.size <- n.mrip.size[ which( !is.na(n.mrip.size$Year) ), ]
  n.mrip.size$Year <- as.numeric( n.mrip.size$Year )
  n.mrip.size$NEW_ST <- as.numeric( n.mrip.size$NEW_ST )
  n.mrip.size$NEW_MODE <- as.numeric( n.mrip.size$NEW_MODE )
  n.mrip.size$id_code <- as.character( n.mrip.size$id_code )
  
  
  return( n.mrip.size )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------


wp.import.size.ntpwd = function( file.dir, file.name, tab.name, mode.filter, params ) {
  ###     ...where 'file.dir' is the file directory containing the size file to be imported,
  ###           'file.name' is the name of that file, 'tab.name' the associated (sample size) tab,
  ###           'mode.filter' to identify those modes (potentially) included in this assessment,
  ###       and 'params' the R object that (amongst other things) identifies the filters applied in this SEDAR...
  
  
  n.tpwd.size <- read_excel( path  = paste0( file.dir,"/",file.name ),
                             sheet = tab.name, trim_ws=FALSE, col_types="text" )
  
  
  colnames(n.tpwd.size) = toupper( colnames(n.tpwd.size) )
  if( "YEAR" %notin% colnames(n.tpwd.size) & "INT_YEAR" %in% colnames(n.tpwd.size) ) {
    colnames(n.tpwd.size)[ colnames(n.tpwd.size) %in% c("INT_YEAR") ] = "YEAR"
  }
  
  ### SUBSETTING ###
  n.tpwd.size <- n.tpwd.size[ which( n.tpwd.size$DS == "TPWD" ), ]
  n.tpwd.size <- n.tpwd.size[ which( n.tpwd.size$YEAR %in% (1981:params$term.year) ), ]
  n.tpwd.size <- n.tpwd.size[ which( n.tpwd.size$NEW_STA %in% params$subset.states ), ]
  n.tpwd.size <- n.tpwd.size[ which( n.tpwd.size$NEW_MODEN %in% mode.filter ), ]
  
  ### FORMATTING ###
  colnames(n.tpwd.size)[ which( colnames(n.tpwd.size) == "YEAR" ) ] <- "Year"
  n.tpwd.size <- n.tpwd.size[ which( !is.na(n.tpwd.size$Year) ), ]
  n.tpwd.size$Year <- as.numeric( n.tpwd.size$Year )
  n.tpwd.size$TRIP_KEY <- as.character( n.tpwd.size$TRIP_KEY )
  
  
  return( n.tpwd.size )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------


wp.import.size.nlacr = function( file.dir, file.name, tab.name, mode.filter, params ) {
  ###     ...where 'file.dir' is the file directory containing the size file to be imported,
  ###           'file.name' is the name of that file, 'tab.name' the associated (sample size) tab,
  ###           'mode.filter' to identify those modes (potentially) included in this assessment,
  ###       and 'params' the R object that (amongst other things) identifies the filters applied in this SEDAR...
  
  
  n.lacr.size <- read_excel( path  = paste0( file.dir,"/",file.name ),
                             sheet = tab.name, trim_ws=FALSE, col_types="text" )
  
  
  colnames(n.lacr.size) = toupper( colnames(n.lacr.size) )
  if( "YEAR" %notin% colnames(n.lacr.size) & "INT_YEAR" %in% colnames(n.lacr.size) ) {
    colnames(n.lacr.size)[ colnames(n.lacr.size) %in% c("INT_YEAR") ] = "YEAR"
  }
  
  ### SUBSETTING ###
  n.lacr.size <- n.lacr.size[ which( n.lacr.size$DS %in% c("LA Creel","LA BIO") ), ]
  n.lacr.size <- n.lacr.size[ which( n.lacr.size$YEAR %in% (1981:params$term.year) ), ]
  n.lacr.size <- n.lacr.size[ which( n.lacr.size$NEW_STA %in% params$subset.states ), ]
  n.lacr.size <- n.lacr.size[ which( n.lacr.size$NEW_MODEN %in% mode.filter ), ]
  
  ### FORMATTING ###
  colnames(n.lacr.size)[ which( colnames(n.lacr.size) == "YEAR" ) ] <- "Year"
  n.lacr.size <- n.lacr.size[ which( !is.na(n.lacr.size$Year) ), ]
  n.lacr.size$Year <- as.numeric( n.lacr.size$Year )
  
  if( 'TRIP_KEY' %in% colnames(n.lacr.size) ) {
    n.lacr.size$TRIP_KEY <- as.character( n.lacr.size$TRIP_KEY )
  }
  
  
  return( n.lacr.size )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------


wp.import.eff.mrip = function( file.dir, file.name, tab.name, mode.filter, params ) {
  ###     ...where 'file.dir' is the file directory containing the size file to be imported,
  ###           'file.name' is the name of that file, 'tab.name' the associated (effort) tab,
  ###           'mode.filter' to identify those modes (potentially) included in this assessment,
  ###       and 'params' the R object that (amongst other things) identifies the filters applied in this SEDAR...
  
  
  raw.mrip.eff <- read_excel( path  = paste0( file.dir,"/",file.name ),
                              sheet = tab.name, trim_ws=FALSE, col_types="text" )
  
  
  colnames(raw.mrip.eff) = toupper( colnames(raw.mrip.eff) )
  if( "YEAR" %notin% colnames(raw.mrip.eff) & "INT_YEAR" %in% colnames(raw.mrip.eff) ) {
    colnames(raw.mrip.eff)[ colnames(raw.mrip.eff) %in% c("INT_YEAR") ] = "YEAR"
  }
  
  ### Unlike the raw catch and size files (i.e., "raw.catch", "raw.size" ), the effort datasets are constructed
  ###       from an excel worksheet that may not have been filtered to the appropriate resolution. Therefore,
  ###       the subsetting below must be conducted if recreational fishing effort is to be properly summarized
  ###       (vs. catch & size where the subsetting is applied more as a 'double-check' )...
  
  ### SUBSETTING ###
  raw.mrip.eff <- raw.mrip.eff[ which( raw.mrip.eff$DS %in% c("MRIP","MRFSS") ), ]
  raw.mrip.eff <- raw.mrip.eff[ which( raw.mrip.eff$NEW_STA %in% params$subset.states ), ]
  ###     ...and because TX & LA(2014+) effort estimates are provided by the TPWD and the LA Creel surveys,
  ###         I remove the associated MRIP estimates from "raw.mrip.eff". However, these values will be added to
  ###         the 'final' effort table if TPWD & LACreel are included in this assessment (see if() statement below)...
  raw.mrip.eff <- raw.mrip.eff[ which( raw.mrip.eff$NEW_STA != "TX" ), ]
  raw.mrip.eff <- raw.mrip.eff[ !( raw.mrip.eff$NEW_STA == "LA" & as.numeric(raw.mrip.eff$YEAR) > 2014 ), ]
  if( "FL" %in% params$subset.states | "FLW" %in% params$subset.states | "FLE" %in% params$subset.states ) {
    raw.mrip.eff <- raw.mrip.eff[ which(
      is.na(raw.mrip.eff$FL_REG) | raw.mrip.eff$FL_REG == "" | raw.mrip.eff$FL_REG %in% params$subset.FL ), ]
  }
  
  raw.mrip.eff <- raw.mrip.eff[ which( raw.mrip.eff$NEW_MODEN %in% mode.filter ), ]
  
  raw.mrip.eff <- raw.mrip.eff[ !( raw.mrip.eff$NEW_MODEN == "Hbt" & raw.mrip.eff$SUB_REG == 6 ), ]
  raw.mrip.eff <- raw.mrip.eff[ !( raw.mrip.eff$NEW_MODEN == "Hbt" & raw.mrip.eff$SUB_REG == 7 & raw.mrip.eff$YEAR >= 1986 ), ]
  if( "FL_REG" %in% colnames(raw.mrip.eff) ) {
    raw.mrip.eff <- raw.mrip.eff[ !( raw.mrip.eff$NEW_MODEN == "Hbt" & raw.mrip.eff$NEW_STA == "FLW" & raw.mrip.eff$FL_REG == 3 ), ]
  }
  
  ### FORMATTING ###
  ###     ...where, as done in my previous tables, the year columns will be 'standardized' for each survey...
  colnames(raw.mrip.eff)[ which( colnames(raw.mrip.eff)== "YEAR" ) ] <- "Year"
  raw.mrip.eff <- raw.mrip.eff[ which( !is.na(raw.mrip.eff$Year) ), ]
  
  
  return( raw.mrip.eff )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------


wp.import.eff.tpwd = function( file.dir, file.name, tab.name, mode.filter, params ) {
  ###     ...where 'file.dir' is the file directory containing the size file to be imported,
  ###           'file.name' is the name of that file, 'tab.name' the associated (effort) tab,
  ###           'mode.filter' to identify those modes (potentially) included in this assessment,
  ###       and 'params' the R object that (amongst other things) identifies the filters applied in this SEDAR...
  
  
  raw.tpwd.eff <- read_excel( path  = paste0( file.dir,"/",file.name ),
                              sheet = tab.name, trim_ws=FALSE, col_types="text" )
  
  
  colnames(raw.tpwd.eff) = toupper( colnames(raw.tpwd.eff) )
  if( "YEAR" %notin% colnames(raw.tpwd.eff) & "INT_YEAR" %in% colnames(raw.tpwd.eff) ) {
    colnames(raw.tpwd.eff)[ colnames(raw.tpwd.eff) %in% c("INT_YEAR") ] = "YEAR"
  }
  
  raw.tpwd.eff <- raw.tpwd.eff[ which( raw.tpwd.eff$CYEAR %in% (1981:params$term.year) ), ]
  raw.tpwd.eff <- raw.tpwd.eff[ which( raw.tpwd.eff$NEW_MODEN %in% params$subset.modes ), ]
  
  colnames(raw.tpwd.eff)[ which( colnames(raw.tpwd.eff)=="CYEAR" ) ] <- "Year"
  # colnames(raw.tpwd.eff)[ which( colnames(raw.tpwd.eff)=="NTRP" ) ] <- "EFFORT"
  raw.tpwd.eff <- raw.tpwd.eff[ which( !is.na(raw.tpwd.eff$Year) ), ]
  
  
  return( raw.tpwd.eff )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------


wp.import.eff.lacr = function( file.dir, file.name, tab.name, mode.filter, params ) {
  ###     ...where 'file.dir' is the file directory containing the size file to be imported,
  ###           'file.name' is the name of that file, 'tab.name' the associated (effort) tab,
  ###           'mode.filter' to identify those modes (potentially) included in this assessment,
  ###       and 'params' the R object that (amongst other things) identifies the filters applied in this SEDAR...
  
  
  raw.lacr.eff <- read_excel( path  = paste0( file.dir,"/",file.name ),
                              sheet = tab.name, trim_ws=FALSE, col_types="text" )
  
  
  colnames(raw.lacr.eff) = toupper( colnames(raw.lacr.eff) )
  if( "YEAR" %notin% colnames(raw.lacr.eff) & "INT_YEAR" %in% colnames(raw.lacr.eff) ) {
    colnames(raw.lacr.eff)[ colnames(raw.lacr.eff) %in% c("INT_YEAR") ] = "YEAR"
  }
  
  raw.lacr.eff <- raw.lacr.eff[ which( raw.lacr.eff$YEAR %in% (2014:params$term.year) ), ]
  if( "Priv" %in% params$subset.modes & "Cbt" %in% params$subset.modes ) {
    raw.lacr.eff <- raw.lacr.eff[ which( raw.lacr.eff$MODES %in% c( "Private","Charter" ) ), ]
  } else if( "Priv" %in% params$subset.modes & "Cbt" %notin% params$subset.modes ) {
    raw.lacr.eff <- raw.lacr.eff[ which( raw.lacr.eff$MODES == "Private" ), ]
  } else if( "Priv" %notin% params$subset.modes & "Cbt" %in% params$subset.modes ) {
    raw.lacr.eff <- raw.lacr.eff[ which( raw.lacr.eff$MODES == "Charter" ), ]
  }
  
  colnames(raw.lacr.eff)[ which( colnames(raw.lacr.eff)== "YEAR" ) ] <- "Year"
  # colnames(raw.lacr.eff)[ which( colnames(raw.lacr.eff)== "EXPANDED_EFFORT" ) ] <- "EFFORT"
  raw.lacr.eff <- raw.lacr.eff[ which( !is.na(raw.lacr.eff$Year) ), ]
  
  
  return( raw.lacr.eff )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------


wp.import.eff.srhs = function( file.dir, file.name, tab.name, mode.filter, params ) {
  ###     ...where 'file.dir' is the file directory containing the size file to be imported,
  ###           'file.name' is the name of that file, 'tab.name' the associated (effort) tab,
  ###           'mode.filter' to identify those modes (potentially) included in this assessment,
  ###       and 'params' the R object that (amongst other things) identifies the filters applied in this SEDAR...
  
  
  raw.srhs.eff <- read_excel( path  = paste0( file.dir,"/",file.name ),
                              sheet = tab.name, trim_ws=FALSE, col_types="text" )
  
  
  colnames(raw.srhs.eff)[ which( colnames(raw.srhs.eff)=="year" ) ] <- "Year"
  raw.srhs.eff <- raw.srhs.eff[ which( !is.na(raw.srhs.eff$Year) ), ]
  
  
  return( raw.srhs.eff )
  
}





