

### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------
###
###     DESCRIPTION
###
###   Most SEDAR stock assessments are based on a single unit stock. However, as part of the Stock ID process,
###   some SEDAR stock assessments separate data/estimates into distinct spatial areas, which are then treated
###   separately in the final stock assessment model. This function keeps track of these StockID decisions and
###   adds a 'SID' field to the relevant GenRec table when necessary...
###
###
###    *** assign.stockID( )
###           ...which populates a 'SID' field from the spatial fields in GenRec data ( SUB_REG,NEW_STA,FL_REG,NC_REG )...
###
###    *** assign.stockID.srhs( )
###           ...which populates a 'SID' field from the spatial fields in SRHS data ( AREA )...
###
### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------

assign.stockID = function( new.com, region, genrec.table ) {
  ###     ...where 'new.com' and 'region' identify the stock that is being assessed
  ###          and 'genrec.table' is the table (of data/estimates) to which a 'SID' field may be added...
  
  
  ### ---------------------------------------------------------------------------------------------------------
  ### SEDAR 74 and SEDAR 98 -- Gulf of America RED SNAPPER ###
  ###
  ###       ...for which there are three stock boundaries:
  ###           (1) West    -- TX & LA
  ###           (2) Central -- MS, AL, FL1
  ###           (3) East    -- FL2 & FL3
  ###
  if( region == 'Gulf of America' & 'red snapper' %in% new.com ) {
    
    genrec.table = genrec.table %>%
      mutate( SID = ifelse( NEW_STA %in% c("TX","LA"),               'West',
                    ifelse( NEW_STA %in% c("MS","AL") | FL_REG == 1, 'Central',
                    ifelse( FL_REG %in% 2:3,                         'East',    NA ))) )
    
    # ###   ...as applied to provide catch & CV estimates by state & FL_REG, which the individual states
    # ###       need to calibrate their survey estimates to MRIP currency...
    # genrec.table = genrec.table %>%
    #   mutate( SID = ifelse( ( is.na(FL_REG) | FL_REG == '' ) & ( is.na(NC_REG) | NC_REG == '' ), NEW_STA,
    #                 ifelse( FL_REG == 1, 'FLpan',   ifelse( FL_REG %in% 2:3, 'FLwest', NA ))) )
    # # genrec.table = genrec.table %>%
    # #   mutate( SID = ifelse( ( is.na(FL_REG) | FL_REG == '' ) & ( is.na(NC_REG) | NC_REG == '' ), NEW_STA,
    # #                 ifelse( (!is.na(FL_REG) & FL_REG != '' ), paste0( "FL",FL_REG ),
    # #                 ifelse( (!is.na(NC_REG) & NC_REG != '' ), paste0( "NC",NC_REG ), NA ))) )
  }
  
  ### ---------------------------------------------------------------------------------------------------------
  ### SEDAR 79 -- Southeastern MUTTON SNAPPER ###
  ###
  ###       ...for which there are two stock boundaries:
  ###           (1) West -- TX:FLW (including FLKeys)
  ###           (2) East -- FLE:ME
  ###
  if( region == 'Gulf of America and Atlantic' & 'mutton snapper' %in% new.com ) {
    
    genrec.table = genrec.table %>%
      mutate( SID = ifelse( NEW_STA %in% c("TX","LA","MS","AL","FLW"),  'West',
                    ifelse( NEW_STA %in% c("FLE","GA","SC","NC") | SUB_REG %in% 4:5, 'East', NA )) )
  }
  
  ### ---------------------------------------------------------------------------------------------------------
  ### SEDAR 85 -- Gulf of America YELLOWEDGE GROUPER ###
  ###
  ###       ...for which there are two stock boundaries:
  ###           (1) West -- TX & LA
  ###           (2) East -- MS, AL, FL1, FL2
  ###
  if( region == 'Gulf of America' & 'yellowedge grouper' %in% new.com ) {
    
    genrec.table = genrec.table %>%
      mutate( SID = ifelse( NEW_STA %in% c("TX","LA"),                   'West',
                    ifelse( NEW_STA %in% c("MS","AL") | FL_REG %in% 1:2, 'East', NA )) )
  }
  
  ### ---------------------------------------------------------------------------------------------------------
  ### SEDAR 86 -- South Atlantic RED GROUPER ###
  ###
  ###       ...for which there are two stock boundaries:
  ###           (1) NCSC -- NC & SC
  ###           (2) GAFL -- GA, FLE, FL3
  ###
  if( region == 'South Atlantic' & 'red grouper' %in% new.com ) {
    
    genrec.table = genrec.table %>%
      mutate( SID = ifelse( NEW_STA %in% c("NC","SC"),                  'NCSC',
                    ifelse( NEW_STA %in% c("GA","FLE") | FL_REG %in% 3, 'GAFL', NA )) )
  }
  
  ### ---------------------------------------------------------------------------------------------------------
  ### SEDAR 92 -- Atlantic BLUELINE TILEFISH ###
  ###
  ###       ...for which there were two 'stock boundaries' considered as part of a LANDINGS TWG, with a focus on
  ###         evaluating the need to consider areas north of Cape Hatteras (which were excluded in SEDAR 50):
  ###           (1) s.Hatteras -- FLKeys:SNC
  ###           (2) n.Hatteras -- NNC:NY
  ###
  if( region == 'Atlantic' & 'blueline tilefish' %in% new.com ) {
    
    # genrec.table = genrec.table %>%
    #   mutate( SID = ifelse(  FL_REG %in% 3:5 | NEW_STA %in% c("GA","SC") | NC_REG == "S",  's.Hatteras',
    #                 ifelse( SUB_REG %in% 4:5 | NC_REG == "N",                              'n.Hatteras',    NA )) )
    
    genrec.table = genrec.table %>%
      mutate( SID = ifelse( FL_REG %in% 3:5 | NEW_STA %in% c("GA","SC"),   's.Hatteras',
                    ifelse( NEW_STA == "NC" & NC_REG == "S",               's.Hatteras',
                    ifelse( NEW_STA == "NC" & NC_REG == "N",               'n.Hatteras',
                    ifelse( SUB_REG %in% 4:5,                              'n.Hatteras',    NA )))) )
  }
  
 
  
  
  
  ### ---------------------------------------------------------------------------------------------------------
  ### SEDAR 100 -- Gulf Gray Triggerfish ###
  ###
  ###       ... two stock boundaries:
  ###           (1) East -- FLW:MS
  ###           (2) West -- LA:TX
  ###
  if( region == 'Gulf of America' & 'gray triggerfish' %in% new.com ) {
    
    # genrec.table = genrec.table %>%
    #   mutate( SID = ifelse(  FL_REG %in% 3:5 | NEW_STA %in% c("GA","SC") | NC_REG == "S",  's.Hatteras',
    #                 ifelse( SUB_REG %in% 4:5 | NC_REG == "N",                              'n.Hatteras',    NA )) )
    
    genrec.table = genrec.table %>%
      mutate( SID = ifelse( FL_REG %in% 1:2 | NEW_STA %in% c('AL', 'MS'), 'East',
                    ifelse( NEW_STA %in% c('TX', 'LA')                  , 'West', NA )) )
  }
  
  
   ### ---------------------------------------------------------------------------------------------------------
  
  
  return( genrec.table )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------

assign.stockID.srhs = function( new.com, region, srhs.table ) {
  ###     ...where 'new.com' and 'region' identify the stock that is being assessed
  ###          and 'srhs.table' is the table (of data/estimates) to which a 'SID' field may be added...
  
  
  ### ---------------------------------------------------------------------------------------------------------
  ### SEDAR 90 -- South Atlantic RED SNAPPER ###
  ###
  ###       ...for which there are three stock boundaries:
  ###           (1) West    -- TX & LA
  ###           (2) Central -- MS, AL, FL1
  ###           (3) East    -- FL2 & FL3
  ###
  if( region == 'Gulf of America' & 'red snapper' %in% new.com ) {
    
    srhs.table = srhs.table %>%
      mutate( SID = ifelse( AREA %in% c("NC","SC"),    'NORTH',
                    ifelse( AREA %in% c("GNFL","SFL"), 'SOUTH', NA )) )
  }
  
  ### ---------------------------------------------------------------------------------------------------------
  
  
  return( srhs.table )
  
}

