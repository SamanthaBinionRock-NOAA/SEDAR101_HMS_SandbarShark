

### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------
###
###     DESCRIPTION
###
###   SEDAR stock assessments are (by their nature) specific to certain species and/or taxa. Therefore,
###   when pulling data/estimates for individual SEDARs, species-specific filters need to be applied.
###   The following functions export the relevant 'species ID' information from the most recent
###   translation key (e.g., 'spp.table' = v_species_xref ), which are then used (by the GenRec standard scripts)
###   to apply these filters. Note that the appropriate speciesID field changes between surveys
###   (e.g., LACR uses ITIS, TPWD uses TXcodes, etc.) and so a variety of functions have been constructed
###   (to return a variety of speciesID fields)...
###
### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------

new.com.info <- function( taxa, spp.field, spp.table ) {
  ###     ...where 'taxa' identifies the species that are to be looked up,
  ###              'spp.field' the type of spp info being provided (e.g., is 'taxa' a common/scientific name ),
  ###          and 'spp.table' is the species translation key (i.e., excel file) containing all our spp ID info...

  info <- spp.table$NEW_COM[ grep( paste0("^",taxa,"$"), spp.table[,spp.field] ) ]
  # info <- spp.table$NEW_COM[ grep( paste0("^",taxa,"$"), spp.table$COMMON ) ]
  # info <- spp.table$NEW_COM[ grep( paste0("^",taxa,"$"), spp.table$SCIENTIFIC ) ]
  
  info <- trimws( info, "both" )
  return( info )
}

### -----------------------------------------------------------------------------------------------------------

new.sci.info <- function( taxa, spp.field, spp.table ) {
  info <- spp.table$NEW_SCI[ grep( paste0("^",taxa,"$"), spp.table[,spp.field] ) ]
  return( info )
}

### -----------------------------------------------------------------------------------------------------------

nodc.code.info <- function( taxa, spp.field, spp.table ) {
  info <- spp.table$NODC_CODE[ grep( paste0("^",taxa,"$"), spp.table[,spp.field] ) ]
  return( info )
}

### -----------------------------------------------------------------------------------------------------------

itis.code.info <- function( taxa, spp.field, spp.table ) {
  info <- spp.table$SPECIES_ITIS[ grep( paste0("^",taxa,"$"), spp.table[,spp.field] ) ]
  return( info )
}

### -----------------------------------------------------------------------------------------------------------

tpwd.code.info <- function( taxa, spp.field, spp.table ) {
  info <- spp.table$TX_CODE[ grep( paste0("^",taxa,"$"), spp.table[,spp.field] ) ]
  return( info )
}

### -----------------------------------------------------------------------------------------------------------

srhs.code.info <- function( taxa, spp.field, spp.table ) {
  info <- spp.table$HB_CODE[ grep( paste0("^",taxa,"$"), spp.table[,spp.field] ) ]
  return( info )
}

### -----------------------------------------------------------------------------------------------------------

