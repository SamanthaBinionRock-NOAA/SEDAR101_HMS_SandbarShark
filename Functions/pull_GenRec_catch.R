

### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------
###
###     DESCRIPTION
###
###     For any given assessment, multiple pulls of catch data may be needed (i.e., multiple "catch.table" elements),
###     and so a function was created to minimize the necessary coding for each of these pulls...
###
###     As examples...
###       (1) ...assessments may require additional analyses beyond a simple pull of catch data for the species-of-interest.
###         The most common example of this is when analysts request some percentage of 'unidentified' catch to be allocated
###         to the assessed species, wherein we typically estimate this percentage (i.e., ratio ) from a summary of the
###         relative catch of 'identified' species/taxa groups (which requires a separate pull of catch estimates that include
###         a larger number of taxa -- i.e., all those that could be contributing to the 'unidentified' catch records).
###       (2) Additionally, recreational inputs for Caribbean assessments are largely based on MRIP data, which has yet
###         to be converted into the new (catch/size/trip) file format. Therefore, Caribbean SEDARs require CV's to be calculated
###         outside of RDI (i.e., using the MRIP variance fields; "var_ab1" & "var_b2"). However, these fields are specific to an
###         individual taxa and so if multiple taxa are considered in an assessment (e.g., species-specific catch plus some percent
###         of UnIDd catch ), the final "catch.table" may require a different set of taxa than the constructed "cv.table"
###         (summing catch across taxa is fine, but variances cannot be summed across taxa)...
###
### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------

pull.GenRec.catch = function( raw.table,       pull.type = c('ACL','RDI'),
                              spp.filter, yr.filter, mode.filter,
                              reg.filter, sta.filter, fl.filter, nc.filter ) {
  ###     ...where 'raw.table' is either:
  ###             (1) a table of raw catch estimates that have been imported from the FES ACL file or
  ###             (2) undefined ( = <NA> ), coinciding with catch estimates that are to be imported from RDI
  ###     ...the details of which are identified in the 'pull.type' argument...
  ###     ...all other objects in this function are associated with the filters to be applied in this pull...
  
  
  if( pull.type == 'ACL' ) {  catch.sub = raw.table
    
  } else if( pull.type == 'RDI' ) {
    
    con = dbConnect(dbDriver("Oracle"), username = keyring::key_list("SECPR")[1,2],
                    password = keyring::key_get("SECPR", keyring::key_list("SECPR")[1,2]), dbname = "SECPR")
    raw.table = dbGetQuery(con,
                           paste0("select *
                                from RDI.V_REC_ACL_MRIP_FES@secapxdv_dblk.sfsc.noaa.gov t
                                where t.SP_CODE IN (", sprintf("'%s'", paste(spp.filter, collapse = "','")),")" ))
    raw.table$DS <- gsub( "^\\s+|\\s+$", "", raw.table$DS )
    
    catch.sub = raw.table
  }
  
  
  
  #################################
  ######     STANDARDIZE     ######
  #################################
  
  colnames(catch.sub) = toupper( colnames(catch.sub) )
  
  if( "YEAR" %notin% colnames(catch.sub) ) {
    colnames(catch.sub)[ which( colnames(catch.sub) %in% c("INT_YEAR") ) ] = "YEAR"
  }
  colnames(catch.sub)[ which( colnames(catch.sub) %in% c("LBSEST_SEC","LBSEST_SECWWT","LBSEST_SEC_WWT") ) ] = "lbsest_SECwwt"
  colnames(catch.sub)[ which( colnames(catch.sub) %in% c(             "LBSEST_SECGWT","LBSEST_SEC_GWT") ) ] = "lbsest_SECgwt"
  
  
  
  ###############################
  ######     FILTERING     ######
  ###############################
  
  ### SPECIES ###
  catch.sub <- catch.sub[ which( catch.sub$SP_CODE %in% spp.filter ), ]
  
  ### DATA SOURCE ###
  ###     ...producing GenRec catch estimates, so need to drop SRHS...
  catch.sub <- catch.sub[ which( catch.sub$DS != "SRHS" ), ]
  
  ### TEMPORAL ###
  catch.sub <- catch.sub[ which( catch.sub$YEAR %in% yr.filter ), ]
  
  if( reg.filter == "Caribbean" ) {
    
    ### SPATIAL ###
    catch.sub <- catch.sub[ which( catch.sub$SUB_REG == 11 ), ]             ### ...where can look at the entire Caribbean...
    catch.sub <- catch.sub[ which( catch.sub$NEW_STA %in% sta.filter ), ]   ### ...or some part of it (usually just PR)...
    
    ### MODE ###
    catch.sub <- catch.sub[ which( catch.sub$NEW_MODEN %in% mode.filter ), ]
    
  } else {      ### SUB_REG %in% c( 4,5,6,7 )
    
    ### SPATIAL ###
    catch.sub <- catch.sub[ which( catch.sub$NEW_STA %in% sta.filter ), ]
    if( "FL" %in% sta.filter | "FLW" %in% sta.filter | "FLE" %in% sta.filter ) {
      catch.sub <- catch.sub[ which(
        is.na(catch.sub$FL_REG) | catch.sub$FL_REG == "" | catch.sub$FL_REG %in% fl.filter ), ]
    }
    if( "NC" %in% sta.filter ) {
      catch.sub <- catch.sub[ which(
        is.na(catch.sub$NC_REG) | catch.sub$NC_REG == "" | catch.sub$NC_REG %in% nc.filter ), ]
    }
    
    ### MODE ###
    ###
    ###   ...so as to retain any "Priv" catch estimates from the LA_Creel survey...
    if( "Priv" %in% mode.filter & any( grepl( "LA", sta.filter ) ) ) {
      mode.filter = c( mode.filter,"Priv/Shore" )
    }
    ###   ...so as to retain (1981-2003) for-hire fishing in the Mid & North-Atlantic...
    if( ( ( "Cbt" %in% mode.filter ) | ( "Hbt" %in% mode.filter ) ) &
        any( c("VA","MD","DE","PA","NJ","NY","CT","RI","MA","NH","ME") %in% sta.filter ) ) {
      mode.filter = c( mode.filter,"Cbt/Hbt" )
    }
    
    catch.sub <- catch.sub[ which( catch.sub$NEW_MODEN %in% mode.filter ), ]
    
    ### To avoid duplicating catch estimates from the SRHS survey, I remove all "Hbt" fishing from SUB_REG==6...
    catch.sub <- catch.sub[ !( catch.sub$NEW_MODEN == "Hbt" & catch.sub$SUB_REG == 6 ), ]
    ###     ...and "Hbt" fishing from 1986+ in SUB_REG==7...
    catch.sub <- catch.sub[ !( catch.sub$NEW_MODEN == "Hbt" & catch.sub$SUB_REG == 7 & catch.sub$YEAR >= 1986 ), ]
    ### Additionally, as per an email from Kelly Fitzpatrick for SEDAR 71 (July 22 2020), it was decided that all MRIP Hbt fishing
    ###       from Monroe County (FL_REG==3) would be excluded from future SEDAR assessments. The thinking is that:
    ###             "...most of the MRIP HB Monroe County landings come from the Keys [Atlantic side],
    ###                                                     which is included in the [SRHS] area 12 and 17 estimates."
    ###       Therefore, including them in Atlantic assessments equates to 'double counting' HBT landings from Monroe County
    ###       ( but from two different sources; MRIP & SRHS ) whereas including them in Gulf assessments is equivalent to including
    ###       SATL Hbt landings in a Gulf assessment. Neither of these situations is desirable, so we exclude all MRIP Hbt from FL_REG==3...
    catch.sub <- catch.sub[ !( catch.sub$NEW_MODEN == "Hbt" & catch.sub$NEW_STA == "FLW" & catch.sub$FL_REG == 3 ), ]
    
    ### Similarly, I exclude any MRIP sampling from LA during the years of the LA_Creel survey (2014+)...
    catch.sub <- catch.sub[ !( catch.sub$DS == "MRIP" & catch.sub$NEW_STA == "LA" & catch.sub$YEAR >= 2014 ), ]
    ###     ...and any LDWF sampling during those years within which MRIP operated in LA (1981-2013)...
    catch.sub <- catch.sub[ !( catch.sub$DS %in% c("LA BIO","LA Creel") & catch.sub$NEW_STA == "LA" & catch.sub$YEAR <= 2013 ), ]
    
  }
  
  
  # catch.summary = catch.sub %>%
  #   group_by( YEAR, NEW_ST, NEW_STA ) %>%
  #   summarise( AB1 = sum(AB1,na.rm=TRUE) ) %>%
  #   select( YEAR, NEW_ST, NEW_STA, AB1 ) %>%
  #   pivot_wider( names_from=c("NEW_ST","NEW_STA"), values_from=AB1 )
  
  # catch.summary = catch.sub %>%
  #   group_by( YEAR, NEW_MODE, NEW_MODEN ) %>%
  #   summarise( AB1 = sum(AB1,na.rm=TRUE) ) %>%
  #   select( YEAR, NEW_MODE, NEW_MODEN, AB1 ) %>%
  #   pivot_wider( names_from=c("NEW_MODE","NEW_MODEN"), values_from=AB1 )
  
  
  return( catch.sub )
  
}
