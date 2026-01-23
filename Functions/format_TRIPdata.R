

### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------
###
###     DESCRIPTION
###
###   The GenRec size files that we provide to SEDAR (comps) analysts include tables (as separate, survey-specific tabs)
###   that identify which intercepts/trips collected size/biological information for the species-of-interest.
###   To avoid confusion in which fields should be used in these tables (e.g., tables only retain distinct trips
###   and so the size/catch information in these tables are 'incomplete'), the functions below:
###       (1) filter any fields that pertain to a different survey (e.g., remove 'TPWD' fields from the 'MRIP' trip table )
###       (2) filter any fields providing information on size/catch data, for which analysts should refer to the
###           associated 'MRIP_size' or 'GenRec_size' tabs ( these 'trip' summaries remove size/catch data )...
###
### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------

filter.MRIPtrip = function( genrec.table ) {
  ###     ...where 'genrec.table' is the table of trip-level MRIP intercepts that is to be filtered of extraneous fields...
  
  genrec.table = genrec.table %>%
    
    select( any_of( c(
      
      "DS",      "ID_CODE", "PRT_CODE", "LEADER", "PSU_ID", "STRAT_ID", "VAR_ID", "ASG_CODE", "STRAT_INTERVAL",
      
      "YEAR", "WAVE",     "month", "day", "DATE1", "WEEKDAY", "KOD", "INT_TIME",
      "SUB_REG", "NEW_ST", "NEW_STA", "FL_REG", "NC_REG", "SID", "NEW_CNTY", "NEW_CNTYN",    "ST", "INTSITE", "CNTY", "MUNI_TRP",
      "NEW_MODE", "NEW_MODEN", "NEW_MODE_FX", "NEW_MODEN_FX",   "MODE_FX", "MODE_F", "ON_LIST",
      "NEW_AREA", "NEW_AREAN",   "AREA_X", "AREA_NC", "area_mrip", "INT_DIST", "DISTKEYS",
      "NEW_GEARN", "GEAR_MRIP",
      
      "NEW_COM", "NEW_SCI", "SP_CODE",
      "PRIM1", "PRIM2", "TSN1", "TSN2", "PRIM1_COMMON", "PRIM2_COMMON",
      "TOURN", "REEFCODE", "ART_REEF", "RIG", "TURTLE",
      
      "AGE", "GENDER",
      "REG_RES", "ST_RES", "CNTY_RES", "COUNTY", "MUNI_RES", "PVT_RES", "COASTAL", "FFDAYS2", "FFDAYS12",
      "CNTRBTRS", "HRSF", "ADD_HRS", "BOAT_HRS", "HRS_DTD"
      
    ) ) )
  
  return( genrec.table )
  
}

### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------

filter.TPWDtrip = function( genrec.table ) {
  ###     ...where 'genrec.table' is the table of trip-level TPWD intercepts that is to be filtered of extraneous fields...
  
  genrec.table = genrec.table %>%
    
    select( any_of( c(
      
      "DS",      "TRIP_KEY", "ID_CODE",
      
      "YEAR", "WAVE",     "month", "day", "INTTIME",
      "SUB_REG", "NEW_ST", "NEW_STA", "FL_REG", "NC_REG", "SID", "NEW_CNTY", "NEW_CNTYN", "CNTY_TYPE",    "ST", "MAJOR", "STRATA", "site_tx", "CNTY",
      "NEW_MODE", "NEW_MODEN",    "ACTIVE",
      "NEW_AREA", "NEW_AREAN",
      "NEW_GEARN",
      
      "NEW_COM", "NEW_SCI", "SP_CODE", "SPECCODE",
      
      "BAIT_CODE",
      "TTLANGLR", "TRIPLEN"
      
    ) ) )
  
  return( genrec.table )
  
}

### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------

filter.LACRtrip = function( genrec.table ) {
  ###     ...where 'genrec.table' is the table of trip-level LACR intercepts that is to be filtered of extraneous fields...
  
  genrec.table = genrec.table %>%
    
    select( any_of( c(
      
      "DS",     "SUPPLIER_SAMPLE_ID", "DATA_SOURCE", "DATA_SOURCE_NM", 'ID_CODE',
      
      "YEAR", "WAVE",     "PERIOD_ID", "INT_MONTH", "INT_DAY",
      "SUB_REG", "NEW_ST", "NEW_STA", "FL_REG", "NC_REG", "SID", "NEW_CNTY", "NEW_CNTYN",
          "STATE_LANDED", "COUNTY_LANDED", "STATE_SAMPLED", "COUNTY_SAMPLED", "SITE_TYPE", "ACCESS_SITE_ID", "site_la",
      "NEW_MODE", "NEW_MODEN", "FISHING_MODE", "FISHING_MODE_NM",
      "NEW_AREA", "NEW_AREAN",   "AREA_CODE", "SUB_AREA_CODE", "area_lacr", "DISTANCE_CODE", "DISTANCE_NM", "DEPTH",
      "NEW_GEARN", "GEAR_FINS", "GEAR_NAME",
      
      "NEW_COM", "NEW_SCI", "SP_CODE", "SPECIES_ITIS"
      
    ) ) )
  
  return( genrec.table )
  
}


