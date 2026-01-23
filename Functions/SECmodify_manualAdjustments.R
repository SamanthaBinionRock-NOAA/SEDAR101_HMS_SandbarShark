

### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------
###
###     DESCRIPTION
###
###   ...where, for some SEDARs, data/estimates from other (e.g., state) surveys might be used as
###     a substitute for those (e.g., MRIP) estimates in the current catch file. In such cases,
###     the function below can be applied to make these adjustments...
###
###
###   substitute.MRIPstate( )
###       ...which substitutes the ***CATCH*** estimates in the current catch file with those
###           provided by a dataframe ( input as an argument into the function )...
###
### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------


substitute.MRIPstate = function( dummy.table, genrec.table, avgwgt.dir ) {
  ###     ...where 'dummy.table' is the table of catch estimates that will be substituted into the current table,
  ###           'genrec.table' the current table of catch estimates, some of which are to be replaced...
  ###       and 'avgwgt.dir' identifies the directory within which SEFSC avgwgt estimates from the most recent ACL files
  ###           are stored ( as needed when updating the lbsest_SEC fields in our new 'catch.table' object )
  
  
  ### 'Completing' the Table of Replacement Estimates ###
  ### ---------------------------------------------------
  
  ###   ...by adding any strata that may be missing from 'dummy.table'...
  if( "SP_CODE" %notin% names(dummy.table) ) {
    dummy.table = dummy.table %>% add_column( SP_CODE = as.character( nodc.code[1] ) ) }
  if( "NEW_COM" %notin% names(dummy.table) ) {
    dummy.table = dummy.table %>% add_column( NEW_COM = as.character( new.com[1]   ) ) }
  if( "NEW_STA"   %notin% names(dummy.table) ) { dummy.table = dummy.table %>% add_column( NEW_STA   = NA ) }
  if( "NEW_MODEN" %notin% names(dummy.table) ) { dummy.table = dummy.table %>% add_column( NEW_MODEN = NA ) }
  if( "WAVE"      %notin% names(dummy.table) ) { dummy.table = dummy.table %>% add_column( WAVE      = NA ) }
  if( "NEW_AREAN" %notin% names(dummy.table) ) { dummy.table = dummy.table %>% add_column( NEW_AREAN = NA ) }
  if( "FL_REG" %notin% names(dummy.table) ) { dummy.table = dummy.table %>% add_column( FL_REG = NA ) }
  if( "NC_REG" %notin% names(dummy.table) ) { dummy.table = dummy.table %>% add_column( NC_REG = NA ) }
  
  ###   ...and any catch variables that may be missing from 'dummy.table', in that:
  ###         -- 'dummy.table' may only contain landings estimates if discards aren't being replaced
  ###         -- landings-in-weight estimates may not be provided in 'dummy.table' if they are being
  ###             re-calculated using SEFSC avgwgt estimates ( from an updated AB1 estimate )...
  if( "AB1" %notin% names(dummy.table) ) { dummy.table = dummy.table %>% add_column( AB1 = NA ) }
  if(  "B2" %notin% names(dummy.table) ) { dummy.table = dummy.table %>% add_column(  B2 = NA ) }
  if( "lbsest_SECwwt" %notin% names(dummy.table) ) { dummy.table = dummy.table %>% add_column( lbsest_SECwwt = NA ) }
  if( "lbsest_SECgwt" %notin% names(dummy.table) ) { dummy.table = dummy.table %>% add_column( lbsest_SECgwt = NA ) }
  
  
  ### LANDINGS IN (WHOLE) WEIGHT ###
  ###     ...for which we extract strata-level (SEFSC) avgwgt estimates from the original catch.table...
  
  if( all( c('NEW_COM') %in% names(dummy.table) ) ) {
    avgwgt.s.file = read_sas( data_file = paste0( avgwgt.dir,"/avgwgt_s.sas7bdat" ) )
    avgwgt.s.file$dummy_label = paste0( avgwgt.s.file$NEW_COM )
    dummy.table$dummy_label = paste0(   dummy.table$NEW_COM )
    dummy.table$AVGWGT_S = avgwgt.s.file$avgwgt_s[ match( dummy.table$dummy_label, avgwgt.s.file$dummy_label ) ]
    dummy.table$NUMWGT_S = avgwgt.s.file$numwgt_s[ match( dummy.table$dummy_label, avgwgt.s.file$dummy_label ) ]
    dummy.table = dummy.table %>% select( !dummy_label )
    rm( avgwgt.s.file )
  }
  
  if( all( c('NEW_COM','SUB_REG') %in% names(dummy.table) ) ) {
    avgwgt.sr.file = read_sas( data_file = paste0( avgwgt.dir,"/avgwgt_sr.sas7bdat" ) )
    avgwgt.sr.file$dummy_label = paste0( avgwgt.sr.file$NEW_COM,"_",avgwgt.sr.file$SUB_REG )
       dummy.table$dummy_label = paste0(    dummy.table$NEW_COM,"_",   dummy.table$SUB_REG )
    dummy.table$AVGWGT_SR = avgwgt.sr.file$avgwgt_sr[ match( dummy.table$dummy_label, avgwgt.sr.file$dummy_label ) ]
    dummy.table$NUMWGT_SR = avgwgt.sr.file$numwgt_sr[ match( dummy.table$dummy_label, avgwgt.sr.file$dummy_label ) ]
    dummy.table = dummy.table %>% select( !dummy_label )
    rm( avgwgt.sr.file )
  }
  
  if( all( c('NEW_COM','SUB_REG','YEAR') %in% names(dummy.table) ) ) {
    avgwgt.sry.file = read_sas( data_file = paste0( avgwgt.dir,"/avgwgt_sry.sas7bdat" ) )
    avgwgt.sry.file$dummy_label = paste0( avgwgt.sry.file$NEW_COM,"_",avgwgt.sry.file$SUB_REG,"_",avgwgt.sry.file$year )
        dummy.table$dummy_label = paste0(     dummy.table$NEW_COM,"_",    dummy.table$SUB_REG,"_",    dummy.table$YEAR )
    dummy.table$AVGWGT_SRY = avgwgt.sry.file$avgwgt_sry[ match( dummy.table$dummy_label, avgwgt.sry.file$dummy_label ) ]
    dummy.table$NUMWGT_SRY = avgwgt.sry.file$numwgt_sry[ match( dummy.table$dummy_label, avgwgt.sry.file$dummy_label ) ]
    dummy.table = dummy.table %>% select( !dummy_label )
    rm( avgwgt.sry.file )
  }
  
  if( all( c('NEW_COM','SUB_REG','YEAR','NEW_STA') %in% names(dummy.table) ) ) {
    avgwgt.srys.file = read_sas( data_file = paste0( avgwgt.dir,"/avgwgt_srys.sas7bdat" ) )
    avgwgt.srys.file$dummy_label = paste0( avgwgt.srys.file$NEW_COM,"_",avgwgt.srys.file$SUB_REG,"_",avgwgt.srys.file$year,"_",
                                           avgwgt.srys.file$NEW_STA )
         dummy.table$dummy_label = paste0(      dummy.table$NEW_COM,"_",     dummy.table$SUB_REG,"_",     dummy.table$YEAR,"_",
                                                dummy.table$NEW_STA )
    dummy.table$AVGWGT_SRYS = avgwgt.srys.file$avgwgt_srys[ match( dummy.table$dummy_label, avgwgt.srys.file$dummy_label ) ]
    dummy.table$NUMWGT_SRYS = avgwgt.srys.file$numwgt_srys[ match( dummy.table$dummy_label, avgwgt.srys.file$dummy_label ) ]
    dummy.table = dummy.table %>% select( !dummy_label )
    rm( avgwgt.srys.file )
  }
  
  if( all( c('NEW_COM','SUB_REG','YEAR','NEW_STA','NEW_MODEN') %in% names(dummy.table) ) ) {
    avgwgt.srysm.file = read_sas( data_file = paste0( avgwgt.dir,"/avgwgt_srysm.sas7bdat" ) )
    avgwgt.srysm.file$dummy_label = paste0( avgwgt.srysm.file$NEW_COM,"_",avgwgt.srysm.file$SUB_REG,"_",avgwgt.srysm.file$year,"_",
                                            avgwgt.srysm.file$NEW_STA,"_",avgwgt.srysm.file$NEW_MODEN )
          dummy.table$dummy_label = paste0(       dummy.table$NEW_COM,"_",      dummy.table$SUB_REG,"_",      dummy.table$YEAR,"_",
                                                  dummy.table$NEW_STA,"_",      dummy.table$NEW_MODEN )
    dummy.table$AVGWGT_SRYSM = avgwgt.srysm.file$avgwgt_srysm[ match( dummy.table$dummy_label, avgwgt.srysm.file$dummy_label ) ]
    dummy.table$NUMWGT_SRYSM = avgwgt.srysm.file$numwgt_srysm[ match( dummy.table$dummy_label, avgwgt.srysm.file$dummy_label ) ]
    dummy.table = dummy.table %>% select( !dummy_label )
    rm( avgwgt.srysm.file )
  }
  
  if( all( c('NEW_COM','SUB_REG','YEAR','NEW_STA','NEW_MODEN','WAVE') %in% names(dummy.table) ) ) {
    avgwgt.srysmw.file = read_sas( data_file = paste0( avgwgt.dir,"/avgwgt_srysmw.sas7bdat" ) )
    avgwgt.srysmw.file$dummy_label = paste0( avgwgt.srysmw.file$NEW_COM,"_",avgwgt.srysmw.file$SUB_REG,  "_",avgwgt.srysmw.file$year,"_",
                                             avgwgt.srysmw.file$NEW_STA,"_",avgwgt.srysmw.file$NEW_MODEN,"_",avgwgt.srysmw.file$WAVE )
           dummy.table$dummy_label = paste0(        dummy.table$NEW_COM,"_",       dummy.table$SUB_REG,  "_",       dummy.table$YEAR,"_",
                                                    dummy.table$NEW_STA,"_",       dummy.table$NEW_MODEN,"_",       dummy.table$WAVE )
    dummy.table$AVGWGT_SRYSMW = avgwgt.srysmw.file$avgwgt_srysmw[ match( dummy.table$dummy_label, avgwgt.srysmw.file$dummy_label ) ]
    dummy.table$NUMWGT_SRYSMW = avgwgt.srysmw.file$numwgt_srysmw[ match( dummy.table$dummy_label, avgwgt.srysmw.file$dummy_label ) ]
    dummy.table = dummy.table %>% select( !dummy_label )
    rm( avgwgt.srysmw.file )
  }
  
  if( all( c('NEW_COM','SUB_REG','YEAR','NEW_STA','NEW_MODEN','WAVE','NEW_AREAN') %in% names(dummy.table) ) ) {
    avgwgt.srysmwa.file = read_sas( data_file = paste0( avgwgt.dir,"/avgwgt_srysmwa.sas7bdat" ) )
    avgwgt.srysmwa.file$dummy_label = paste0( avgwgt.srysmwa.file$NEW_COM,"_",avgwgt.srysmwa.file$SUB_REG,  "_",avgwgt.srysmwa.file$year,"_",
                                              avgwgt.srysmwa.file$NEW_STA,"_",avgwgt.srysmwa.file$NEW_MODEN,"_",avgwgt.srysmwa.file$WAVE,"_",
                                              avgwgt.srysmwa.file$NEW_AREAN )
            dummy.table$dummy_label = paste0(         dummy.table$NEW_COM,"_",        dummy.table$SUB_REG,  "_",        dummy.table$YEAR,"_",
                                                      dummy.table$NEW_STA,"_",        dummy.table$NEW_MODEN,"_",        dummy.table$WAVE,"_",
                                                      dummy.table$NEW_AREAN )
    dummy.table$AVGWGT_SRYSMWA = avgwgt.srysmwa.file$avgwgt_srysmwa[ match( dummy.table$dummy_label, avgwgt.srysmwa.file$dummy_label ) ]
    dummy.table$NUMWGT_SRYSMWA = avgwgt.srysmwa.file$numwgt_srysmwa[ match( dummy.table$dummy_label, avgwgt.srysmwa.file$dummy_label ) ]
    dummy.table = dummy.table %>% select( !dummy_label )
    rm( avgwgt.srysmwa.file )
  }
  
  
  ###   We then go row-by-row, identifying the SEFSC avgwgt estimates most appropriate for a given strata
  ###   ( as recorded in 'lbsest_SECsource' ) and saving it to a new 'AVGWGT_SEC' field such that it can then
  ###   be multiplied by the associated AB1 estimate ( to update our 'lbest_SECwwt' estimates )...
  
  n.size.threshold = 15
  
  dummy.table$AVGWGT_SEC = 0
  for( i in 1:dim(dummy.table)[1] ) {
    
    if( !is.na(dummy.table$NUMWGT_SRYSMWA[i]) & dummy.table$NUMWGT_SRYSMWA[i] >= n.size.threshold ) {
      dummy.table$LBSEST_SECSOURCE[i] = "srysmwa"
      dummy.table$AVGWGT_SEC[i] = dummy.table$AVGWGT_SRYSMWA[i]
    } else if( !is.na(dummy.table$NUMWGT_SRYSMW[i]) & dummy.table$NUMWGT_SRYSMW[i] >= n.size.threshold ) {
      dummy.table$LBSEST_SECSOURCE[i] = "srysmw"
      dummy.table$AVGWGT_SEC[i] = dummy.table$AVGWGT_SRYSMW[i]
    } else if( !is.na(dummy.table$NUMWGT_SRYSM[i]) & dummy.table$NUMWGT_SRYSM[i] >= n.size.threshold ) {
      dummy.table$LBSEST_SECSOURCE[i] = "srysm"
      dummy.table$AVGWGT_SEC[i] = dummy.table$AVGWGT_SRYSM[i]
    } else if( !is.na(dummy.table$NUMWGT_SRYS[i]) & dummy.table$NUMWGT_SRYS[i] >= n.size.threshold ) {
      dummy.table$LBSEST_SECSOURCE[i] = "srys"
      dummy.table$AVGWGT_SEC[i] = dummy.table$AVGWGT_SRYS[i]
    } else if( !is.na(dummy.table$NUMWGT_SRY[i]) & dummy.table$NUMWGT_SRY[i] >= n.size.threshold ) {
      dummy.table$LBSEST_SECSOURCE[i] = "sry"
      dummy.table$AVGWGT_SEC[i] = dummy.table$AVGWGT_SRY[i]
    } else if( !is.na(dummy.table$NUMWGT_SR[i]) & dummy.table$NUMWGT_SR[i] >= n.size.threshold ) {
      dummy.table$LBSEST_SECSOURCE[i] = "sr"
      dummy.table$AVGWGT_SEC[i] = dummy.table$AVGWGT_SR[i]
    } else {
      dummy.table$LBSEST_SECSOURCE[i] = "s"
      dummy.table$AVGWGT_SEC[i] = dummy.table$AVGWGT_S[i]
    }
  }
  rm( n.size.threshold )
  
  dummy.table = dummy.table %>%
    mutate( lbsest_SECwwt = ifelse( is.na(lbsest_SECwwt) & !is.na(AB1), AB1*AVGWGT_SEC, lbsest_SECwwt ) ) %>%
    ###   ...which only substitutes LBSEST values if the associated AB1 estimate is being replaced
    ###       ( !is.na(AB1) ) and an appropriate LBSEST value isn't being provided ( is.na(lbsest_SECwwt) )...
    select( !AVGWGT_SEC )
  
  
  ### LANDINGS IN (GUTTED) WEIGHT ###
  ###     ...for which we need to identify the whole:gutted weight conversion factor...
  
  blah = genrec.table %>%
    filter( !is.na(lbsest_SECgwt) & !is.na(lbsest_SECwwt) & lbsest_SECgwt > 0 & lbsest_SECwwt > 0 )
  
  if( dim(blah)[1] > 0 ) {
    blah = blah %>%
      mutate( conv.factor = round( lbsest_SECgwt / lbsest_SECwwt, 10 ) ) %>%
      distinct( conv.factor ) %>%
      select( conv.factor )
    conv.factor = as.numeric( blah$conv.factor )
    
    dummy.table = dummy.table %>%
      mutate( lbsest_SECgwt = ifelse( is.na(lbsest_SECgwt) & !is.na(AB1),
                                      lbsest_SECwwt*conv.factor, lbsest_SECgwt ) )
    rm(conv.factor)
  }
  rm(blah)
  
  
  
  ### Substituting Catch Values ###
  ### -----------------------------
  
  ###   ...for which I start by identifying the variables being used to distinguish those strata for which
  ###       a substitute catch estimate has been provided ( in 'dummy.table' )...
  dummy.strata = colnames(dummy.table)
  dummy.strata = dummy.strata[ which( dummy.strata %notin% c("AB1","B2","lbsest_SECwwt","lbsest_SECgwt") ) ]
  dummy.strata = dummy.strata[ which( dummy.strata %notin% c("DS","SP_CODE") ) ]
  dummy.strata = dummy.strata[ !grepl( "NUMWGT", dummy.strata ) & !grepl( "AVGWGT", dummy.strata ) ]
  dummy.strata = dummy.strata[ !grepl( "lbsest", tolower(dummy.strata) ) ]
  
  ###   We then go row-by-row, replacing the original catch estimates ( in 'genrec.table' )
  ###   and replacing them with the corresponding substitute ( in 'dummy.table' )...
  for( i in 1:dim(dummy.table)[1] ) {
    
    ###   ...identifying the strata to which the substitute estimate corresponds ( as in dummy.table[ i, ] )...
    blah = dummy.table[ i , dummy.strata ]
    ###   ...for which I ensure any character elements are enclosed in single quotation marks (i.e., 'value' )
    ###     as that format is needed in the filter statement below ( using eval(parse(text='xxx')) )...
    if( "NEW_COM" %in% dummy.strata ) {
      blah[ which( dummy.strata == "NEW_COM" ) ]   = paste0( "'",blah[ which( dummy.strata == "NEW_COM" ) ],"'" ) }
    if( "NEW_STA" %in% dummy.strata ) {
      blah[ which( dummy.strata == "NEW_STA" ) ]   = paste0( "'",blah[ which( dummy.strata == "NEW_STA" ) ],"'" ) }
    if( "NEW_MODEN" %in% dummy.strata ) {
      blah[ which( dummy.strata == "NEW_MODEN" ) ] = paste0( "'",blah[ which( dummy.strata == "NEW_MODEN" ) ],"'" ) }
    if( "NEW_AREAN" %in% dummy.strata ) {
      blah[ which( dummy.strata == "NEW_AREAN" ) ] = paste0( "'",blah[ which( dummy.strata == "NEW_AREAN" ) ],"'" ) }
    dummy.filter = paste( dummy.strata, blah, sep=" == " )
    dummy.filter = dummy.filter[ which( !is.na(blah) & blah != "'NA'" ) ]
    rm(blah)
    
    ###   ...removing the original (e.g., MRIP ) estimate from 'genrec.table', which requires consideration
    ###       of what is being replaced (i.e., just AB1, just B2, or both )...
    ### 
    ###     If the replacement record has a new (e.g., state) estimate for both landings & discards,
    ###       we remove the original record entirely (i.e., filtered out of 'genrec.table' )...
    if( !is.na(dummy.table$AB1[i]) & !is.na(dummy.table$B2[i]) ) {
      command.line = paste0( "genrec.table = genrec.table %>% ",
                                             " filter( !( ", paste( dummy.filter, collapse=" & " )," ) )" )
      eval( parse( text = command.line ) )
      
    ###     Conversely, if only AB1 or B2 are being substituted, the original (e.g., MRIP ) record is
    ###       retained but with the corresponding catch estimate reset to a <NA> value...
    } else if( !is.na(dummy.table$AB1[i]) ) {
      command.line = paste0( "genrec.table = genrec.table %>% ",
          " mutate( AB1 = ifelse( ", paste( dummy.filter, collapse=" & " ),", NA, AB1 ),",
                  " lbsest_SECwwt = ifelse( ", paste( dummy.filter, collapse=" & " ),", NA, lbsest_SECwwt ),",
                  " lbsest_SECgwt = ifelse( ", paste( dummy.filter, collapse=" & " ),", NA, lbsest_SECgwt ) )" )
      eval( parse( text = command.line ) )
    } else if( !is.na(dummy.table$B2[i]) ) {
      command.line = paste0( "genrec.table = genrec.table %>% ",
          " mutate( B2 = ifelse( ", paste( dummy.filter, collapse=" & " ),", NA, B2 ) )" )
      eval( parse( text = command.line ) )
    }
    
    ###   ...adding the substitute estimate ( from 'dummy.table' ) to 'genrec.table'...
    genrec.table = bind_rows( genrec.table, dummy.table[ i, ] )
  }
  
  
  return( genrec.table )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------





