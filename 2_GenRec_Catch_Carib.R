


####################################################################################################################
####################################################################################################################
####################################################################################################################
#############################                                                          #############################
#############################                      -CARIBBEAN SEDAR-                   #############################
#############################                      GENREC CATCH FILES                  #############################
#############################                                                          #############################
####################################################################################################################
####################################################################################################################
####################################################################################################################


### Loading the required libraries...
library(tidyverse)
library(reshape2)
library(dplyr)
library(openxlsx)

library(ROracle)
library(haven)
library(readxl)

con = dbConnect(dbDriver("Oracle"), username = keyring::key_list("SECPR")[1,2],
                password = keyring::key_get("SECPR", keyring::key_list("SECPR")[1,2]), dbname = "SECPR")
spp.info = dbGetQuery(con, "SELECT * 
                     FROM RDI.v_species_xref@secapxdv_dblk.sfsc.noaa.gov")


`%notin%` <- Negate(`%in%`)




################################
######     ACL IMPORT     ######
################################


dir <- "C:/Users/matthew.nuttall/Desktop"

# ACL_catch  <- read_sas( data_file = paste0( "R:/ACL/Carib_ACL/2021_Apr27_SEDAR","/mrcat_all00_17_27apr21.sas7bdat" ) )
# ACL_size   <- read_sas( data_file = paste0( "R:/ACL/Carib_ACL/2021_Apr27_SEDAR","/all_c_size00_17final.sas7bdat" ) )
# 
# avgwgt_s       <- read_sas( data_file = paste0( "R:/ACL/Carib_ACL/2021_Apr27_SEDAR",'/avgwgt_s.sas7bdat' ) )
# avgwgt_sr      <- read_sas( data_file = paste0( "R:/ACL/Carib_ACL/2021_Apr27_SEDAR",'/avgwgt_sr.sas7bdat' ) )
# avgwgt_sry     <- read_sas( data_file = paste0( "R:/ACL/Carib_ACL/2021_Apr27_SEDAR",'/avgwgt_sry.sas7bdat' ) )
# avgwgt_srys    <- read_sas( data_file = paste0( "R:/ACL/Carib_ACL/2021_Apr27_SEDAR",'/avgwgt_srys.sas7bdat' ) )
# avgwgt_srysm   <- read_sas( data_file = paste0( "R:/ACL/Carib_ACL/2021_Apr27_SEDAR",'/avgwgt_srysm.sas7bdat' ) )
# avgwgt_srysmw  <- read_sas( data_file = paste0( "R:/ACL/Carib_ACL/2021_Apr27_SEDAR",'/avgwgt_srysmw.sas7bdat' ) )
# avgwgt_srysmwa <- read_sas( data_file = paste0( "R:/ACL/Carib_ACL/2021_Apr27_SEDAR",'/avgwgt_srysmwa.sas7bdat' ) )
# 
# cpue_trips <- read_sas( data_file = paste0( "R:/RecrSurveys/MRFSS/catch-eff/detailed/Atl&Gulf-byState",
#                                             "/mrcpue_all00_17.sas7bdat" ) )
# 
# effort_old <- read_sas( data_file = paste0( "R:/RecrSurveys/MRFSS/estimates/oldcbt_effort",'/oldmrfsseff2000_2013.sas7bdat' ) )
# effort_new <- read_sas( data_file = paste0( "R:/RecrSurveys/MRFSS/MRIP/Pre2018_update/Effort",'/mripeff_2014_2017.sas7bdat' ) )
# 
# save.image( file = paste0( dir,"/ACL_Carib.RData" ) )

load( paste0( dir,"/ACL_carib.RData" ) )








####################################################################################################################
####################################################################################################################
####################################################################################################################
#############################                                                          #############################
#############################                       SEDAR 84                           #############################
#############################               Caribbean Yellowtail Snapper               #############################
#############################                                                          #############################
####################################################################################################################
####################################################################################################################
####################################################################################################################


###       ...for this assessment, data is pulled for...
###
###           CAR Yellowtail Snapper - Ocyurus chrysurus
###               - Temporal - include 2000-2017
###                               ...as MRIP data collection in the Caribbean started in 2000, not 1981...
###               - Modes    - as done in SEDAR 08 (SEDAR 08-DW-12), assessment includes three fishing modes ( charter, private, shore )
###               - Spatial  - MRIP sampling in the Caribbean is largely limited to just Puerto Rico
###
###           Unidentified snappers:
###                 At the request of the analysts, we also took a look at any catch estimates for
###                 unidentified snappers ( new_com = "snapper family" ), wherein we'd only
###                 partition into identified groups (e.g., "yellowtail snapper" ) if:
###                     (1) there is a substantial amount of unidentified catch and
###                     (2) "yellowtail snapper" are going to receive a "large" proportion of these catches




#############################
######     FILTERS     ######
#############################



### SEDAR HISTORY ###
###
###   ...where the two SEDAR objects are used to define my columns in the "Compare Previous SEDARs" tab...
###
current.sedar <- "SEDAR 84"

# prev.sedar <- "None"
prev.sedar <- "SEDAR 08"



### SPATIOTEMPORAL & MODE ###
###
# first.year <- 1981
first.year <- 2000
# first.year <- 2018
###       ...where "first.year" = 2000 for all Caribbean requests except projections (which only need a couple years)...
term.year <- 2017


region <- "Caribbean"
states <- c( "PR" )
###     ...which has options c( "TX","LA","MS","AL","FLW","FLE","GA","SC","NC","VA","MD","DE","PA","NJ","NY","CT","RI","MA","NH","ME" )
###     ...or c( "PR","VI" ) for Caribbean assessments...
# if( "FL" %in% states | "FLW" %in% states | "FLE" %in% states ) {  FL_sub <- c( 1,2,3 )  }
# if( "NC" %in% states ) {                                          NC_sub <- c( "N","S" )  }


mode_sub <- c( "Priv","Cbt","Shore" )
#       ...which has options c( "Priv","Cbt","Hbt","Shore" )
###           Note that the code below removes all HBT fishing from SUB_REG = 6 (SATL), FL_REG = 3 (FL Keys),
###           and SUB_REG = 7 from 1986+, all of which is designed to avoid overlap with SRHS...



### Moving onto the species-specific filter, I need to pull data for the species of interest...
###     Therefore, I start by searching for the appropriate identifiers...
# View( spp.info[grep( "SNAPPER", spp.info$COMMON ),] )
# View( spp.info[grep( "Centropristis striata", spp.info$SCIENTIFIC ),] )

# taxa <- c( "SNAPPER,YELLOWTAIL" )
taxa <- c( "Ocyurus chrysurus" )


### FUNCTIONS ###
### ---------------------------------------------------------------------------------------------
new.com.info <- function( taxa, spp.table ) {
  # info <- spp.table$NEW_COM[ grep( paste0("^",taxa,"$"), spp.table$COMMON ) ]
  info <- spp.table$NEW_COM[ grep( paste0("^",taxa,"$"), spp.table$SCIENTIFIC ) ]
  info <- trimws( info, "both" )
  return( info )
}
new.sci.info <- function( taxa, spp.table ) {
  # info <- spp.table$NEW_SCI[ grep( paste0("^",taxa,"$"), spp.table$COMMON ) ]
  info <- spp.table$NEW_SCI[ grep( paste0("^",taxa,"$"), spp.table$SCIENTIFIC ) ]
  return( info )
}
nodc.code.info <- function( taxa, spp.table ) {
  # info <- spp.table$NODC_CODE[ grep( paste0("^",taxa,"$"), spp.table$COMMON ) ]
  info <- spp.table$NODC_CODE[ grep( paste0("^",taxa,"$"), spp.table$SCIENTIFIC ) ]
  return( info )
}
itis.code.info <- function( taxa, spp.table ) {
  # info <- spp.table$SPECIES_ITIS[ grep( paste0("^",taxa,"$"), spp.table$COMMON ) ]
  info <- spp.table$SPECIES_ITIS[ grep( paste0("^",taxa,"$"), spp.table$SCIENTIFIC ) ]
  return( info )
}
tpwd.code.info <- function( taxa, spp.table ) {
  # info <- spp.table$TX_CODE[ grep( paste0("^",taxa,"$"), spp.table$COMMON ) ]
  info <- spp.table$TX_CODE[ grep( paste0("^",taxa,"$"), spp.table$SCIENTIFIC ) ]
  return( info )
}
### ---------------------------------------------------------------------------------------------

new.com   <- sapply( taxa, FUN=new.com.info, spp.table=spp.info )
new.sci   <- sapply( taxa, FUN=new.sci.info, spp.table=spp.info )
nodc.code <- sapply( taxa, FUN=nodc.code.info, spp.table=spp.info )
itis.code <- sapply( taxa, FUN=itis.code.info, spp.table=spp.info )
tpwd.code <- sapply( taxa, FUN=tpwd.code.info, spp.table=spp.info )





### CV Tables ###
### _____________


### MRIP CVs ###

report.name = 'None'
###     ...where "report.name" isn't included in Caribbean assessments because S&T has yet to include Caribbean data
###         in the new file format. Instead, CVs for Caribbean assessments are generated from the "old" method
###         (i.e., sum(variance) ) and not from a pre-existing RDI report...

###     The appropriate sample size info (for the MRIP-CV estimates) is then obtained from the MRIP CPUE file,
###     generated from Vivian's legacy scripts (i.e., that reads directly from the i-files ), which was imported
###     as 'cpue_trips' at the top of the script. From this file, we then create a table that provides a count of
###     all intercepted trips ( 'n.table' = one record for every trip ) and one a count of all positive trips
###     ( 'pos.table' = one record for each trip landing the spp of interest ):
  n.table = cpue_trips %>% distinct( id_code, .keep_all = TRUE )
pos.table = cpue_trips %>% filter( sp_code %in% nodc.code ) %>% distinct( id_code, .keep_all = TRUE )



### SEDAR Size File ###
###
###     ...which identifies the spreadsheet (uploaded to the desktop) containing CV estimates for SEFSC avgwgts,
###         as needed in calculating uncertainties for landings-in-weight estimates...

sedar.size.file = "YTL_rec_sizeGEN_0017_20240319.xlsx"






### FLAGS & ASSOCIATED OBJECTS ###
### ______________________________


###     (1) Allocation of Unidentified Catch Estimates
###
###     ...wherein a number of SEDARs assume some percentage of unidentified catch is comprised of the
###     assessed species (e.g., unidentified triggerfish in S82, for gray triggerfish ). In such cases,
###     the objects below are used to:
###
###         -- identify the unidentified taxa, (some of) the catch estimates of which will be assigned
###             to the species-of-interest for this SEDAR
###         -- identify those taxa to include in a summary of relative catch, which will evaluate the
###             potential allocation of unidentified catch estimates across different (identified) species groups.
###             In such cases, an additional tab is included (in the final GenRec catch file) that summarizes the
###             relative breakdown of catch identified at the species level

flag.unid = TRUE

if( flag.unid ) {
  
  taxa.unid.catch = c( "Lutjanidae" )
  nodc.unid.catch = sapply( taxa.unid.catch, FUN=nodc.code.info, spp.table=spp.info )
  ###     ...which is the (unidentified) taxa for which some fraction of its catch is assumed to be the species-of-interest
  
  taxa.unid = c( "Ocyurus chrysurus", "Lutjanidae",
                 "Lutjanus synagris","Rhomboplites aurorubens","Lutjanus griseus" )
  nodc.unid = sapply( taxa.unid, FUN=nodc.code.info, spp.table=spp.info )
  ###     ...which represents the taxa to include in the relative catch summary (i.e., that used to determine
  ###         the fraction of UnIDd catch to allocate to the assessed species )...
  
}



###     (2) Flag for when the final "cv.table" includes a different set of species than that in "catch.table"
###
###     Specifically, a number of SEDARs assume some percentage of unidentified catch is comprised of the
###     assessed species (e.g., unidentified triggerfish in S82, for gray triggerfish ). In such cases,
###     the provided catch estimates ( in 'catch.table' ) include these unidentified fish, but I do not include
###     these in the CV calculation as its unclear which records are from the species-of-interest (vs. related taxa).
###     The objects below were developed to apply a different (species) filter for the CV calculation, when appropriate...

flag.cv = TRUE

if( flag.cv ) {
  
  taxa.cv <- c( "Ocyurus chrysurus" )
  
  nodc.cv <- sapply( taxa.cv, FUN=nodc.code.info, spp.table=spp.info )
  
} else {
  
  nodc.cv <- nodc.code
  
}







### * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
### * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
### * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
### * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

### For any given assessment, multiple "catch.table" elements may be needed and so I construct a function...
###
###     For example...
###       (1) Caribbean assessments are based on MRIP data that has not yet been converted into the
###         new file format, requiring CV's to be calculated outside of RDI (i.e., using the MRIP variance fields;
###         "var_ab1" & "var_b2"). However, these fields are specific to an individual taxa and so if multiple taxa
###         are considered in an assessment (e.g., species-specific catch + some percent of UnIDd catch ),
###         the final "catch.table" may include a different set of taxa than the constructed "cv.table"
###         (summing catch across taxa is fine, but variances cannot be summed across taxa)...
###       (2) Similarly, assessments may require additional analyses beyond a simple pull of catch data.
###         For example, analysts may request a comparison of the relative catch of different species,
###         as to be applied in estimating appropriate ratios to allocate unidentified catch across different
###         species groups. Such analyses also require a separate pull of catch estimates...
###
### _______________________________________________________________________________________________________________________

dat.filter = function( acl.table,  spp.filter, yr.filter, mode.filter,
                       reg.filter, sta.filter, fl.filter, nc.filter ) {
  
  catch.sub = acl.table
  
  
  #################################
  ######     STANDARDIZE     ######
  #################################
  
  colnames(catch.sub) = toupper( colnames(catch.sub) )
  
  colnames(catch.sub)[ which( colnames(catch.sub) %in% c("INT_YEAR") ) ] = "YEAR"
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
    
    # ### SPATIAL ###
    # catch.sub <- catch.sub[ which( catch.sub$NEW_STA %in% sta.filter ), ]
    # if( "FL" %in% sta.filter | "FLW" %in% sta.filter | "FLE" %in% sta.filter ) {
    #   catch.sub <- catch.sub[ which(
    #     is.na(catch.sub$FL_REG) | catch.sub$FL_REG == "" | catch.sub$FL_REG %in% fl.filter ), ]
    # }
    # if( "NC" %in% sta.filter ) {
    #   catch.sub <- catch.sub[ which(
    #     is.na(catch.sub$NC_REG) | catch.sub$NC_REG == "" | catch.sub$NC_REG %in% nc.filter ), ]
    # }
    # 
    # ### MODE ###
    # ###
    # ###   ...so as to retain any "Priv" catch estimates from the LA_Creel survey...
    # if( "Priv" %in% mode.filter & grepl( "Gulf of Mexico",reg.filter ) ) {
    #   mode.filter = c( mode.filter,"Priv/Shore" )
    # }
    # ###   ...so as to retain (1981-2003) for-hire fishing in the Mid & North-Atlantic...
    # if( ( ( "Cbt" %in% mode.filter ) | ( "Hbt" %in% mode.filter ) ) &
    #         any( c("VA","MD","DE","PA","NJ","NY","CT","RI","MA","NH","ME") %in% sta.filter ) ) {
    #   mode.filter = c( mode.filter,"Cbt/Hbt" )
    # }
    # 
    # catch.sub <- catch.sub[ which( catch.sub$NEW_MODEN %in% mode.filter ), ]
    # 
    # ### To avoid duplicating catch estimates from the SRHS survey, I remove all "Hbt" fishing from SUB_REG==6...
    # catch.sub <- catch.sub[ !( catch.sub$NEW_MODEN == "Hbt" & catch.sub$SUB_REG == 6 ), ]
    # ###     ...and "Hbt" fishing from 1986+ in SUB_REG==7...
    # catch.sub <- catch.sub[ !( catch.sub$NEW_MODEN == "Hbt" & catch.sub$SUB_REG == 7 & catch.sub$YEAR >= 1986 ), ]
    # ### Additionally, as per an email from Kelly Fitzpatrick for SEDAR 71 (July 22 2020), it was decided that all MRIP Hbt fishing
    # ###       from Monroe County (FL_REG==3) would be excluded from future SEDAR assessments. The thinking is that:
    # ###             "...most of the MRIP HB Monroe County landings come from the Keys [Atlantic side],
    # ###                                                     which is included in the [SRHS] area 12 and 17 estimates."
    # ###       Therefore, including them in Atlantic assessments equates to 'double counting' HBT landings from Monroe County
    # ###       ( but from two different sources; MRIP & SRHS ) whereas including them in Gulf assessments is equivalent to including
    # ###       SATL Hbt landings in a Gulf assessment. Neither of these situations is desirable, so we exclude all MRIP Hbt from FL_REG==3...
    # catch.sub <- catch.sub[ !( catch.sub$NEW_MODEN == "Hbt" & catch.sub$NEW_STA == "FLW" & catch.sub$FL_REG == 3 ), ]
    # 
    # ### Similarly, I exclude any MRIP sampling from LA during the years of the LA_Creel survey (2014+)...
    # catch.sub <- catch.sub[ !( catch.sub$DS == "MRIP" & catch.sub$NEW_STA == "LA" & catch.sub$YEAR >= 2014 ), ]
    # ###     ...and any LDWF sampling during those years within which MRIP operated in LA (1981-2013)...
    # catch.sub <- catch.sub[ !( catch.sub$DS %in% c("LA BIO","LA Creel") & catch.sub$NEW_STA == "LA" & catch.sub$YEAR <= 2013 ), ]
    
  }
  
  
  return( catch.sub )
}
### ____________________________________________________________________________________________


### * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
### * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
### * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
### * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 






####################################################################################################################
####################################################################################################################
####################################################################################################################
#############################                                                          #############################
#############################                   GENERATE CATCH TABLE                   #############################
#############################                                                          #############################
####################################################################################################################
####################################################################################################################
####################################################################################################################



catch.table = dat.filter(  acl.table = ACL_catch,             spp.filter = nodc.code,
                           yr.filter = first.year:term.year,  mode.filter = mode_sub,
                          reg.filter = region, sta.filter = states, fl.filter = FL_sub, nc.filter = NC_sub )

###     ...where "catch.table" is (currently) not the final table as there may be a number of SEDAR-specific
###         modifications that need to be applied. To determine the appropriate modifications, I check previous SEDARs
###         (and the associated SAS scripts Vivian used in these SEDARs) and apply some 'standard' modifications,
###         some of which are noted in best practices ( SEDAR 2015 -- Procedural Workshop #7 -- http://sedarweb.org/pw-07 )...


# catch.summary <- catch.table %>%
#   group_by( NEW_COM, YEAR, NEW_STA ) %>%
#   summarize( AB1 = sum( as.numeric(AB1), na.rm=TRUE ),
#              B2 = sum( as.numeric(B2), na.rm=TRUE ) ) %>%
#   select( NEW_COM, YEAR, NEW_STA, AB1, B2 ) %>%
#   pivot_wider( names_from=NEW_STA, values_from=c(AB1,B2) )
# 
# catch.summary <- catch.table %>%
#   group_by( NEW_COM, YEAR, NEW_MODEN ) %>%
#   summarize( AB1 = sum( as.numeric(AB1), na.rm=TRUE ),
#              B2 = sum( as.numeric(B2), na.rm=TRUE ) ) %>%
#   select( NEW_COM, YEAR, NEW_MODEN, AB1, B2 ) %>%
#   pivot_wider( names_from=NEW_MODEN, values_from=c(AB1,B2) )






####################################################################################################################
####################################################################################################################
####################################################################################################################
#############################                                                          #############################
#############################             ASSESSMENT-SPECIFIC MODIFICATIONS            #############################
#############################                                                          #############################
####################################################################################################################
####################################################################################################################
####################################################################################################################


###     ...wherein 'special' modifications (e.g., assignment of StockID boundaries ) are unlikely to be needed
###       in every SEDAR, but this section is reserved when such requests are made...



### ---------------------------------------------------------------------------------------------------------------- ###
### ---------------------------------------------------------------------------------------------------------------- ###



##############################################
######     Allocation of UnIDd Catch    ######
######            to Species            ######
##############################################


if( flag.unid ) {
  
  
  
  ### Catch of UnIDd Taxa and all Species that could contribute to these estimates
  ### ----------------------------------------------------------------------------
  
  unid.dat = dat.filter(  acl.table = ACL_catch,             spp.filter = nodc.unid,
                          yr.filter = first.year:term.year,  mode.filter = mode_sub,
                          reg.filter = region, sta.filter = states, fl.filter = FL_sub, nc.filter = NC_sub )
  
  
  
  ### Evaluation of %UnIDd to allocate to Assessed Species
  ### ----------------------------------------------------
  
  ###   The summaries constructed in (and provided by) the code below are composed of three parts:
  ###       (1) the total catch of the 'unidentified' group (by year), namely that which is to be partitioned amongst species,
  ###       (2) the relative catch of the 'identified' species/taxa ( including the assessed species ) by species, and
  ###       (3) the relative catch of the 'identified' species/taxa ( including the assessed species ) by species and year.
  ###   Note that AB1.wgt is not included in this analysis because allocations (when needed) are applied to catch-in-numbers,
  ###       after which AB1.wgt estimates are repulled (wgt estimation may be rerun to include avg.wgt of unidentified catch)...
  
  
  ###   Starting with (1) -- 'unidentified' catch-by-year (AB1 & B2)
  unid.catch = unid.dat[ which( unid.dat$SP_CODE %in% nodc.unid.catch ), ] %>%
    group_by( YEAR ) %>%
    summarize( AB1 = sum( as.numeric(AB1), na.rm=TRUE ),
               B2  = sum( as.numeric( B2), na.rm=TRUE ) ) %>%
    # AB1.wgt = sum( as.numeric(lbsest_SECwwt), na.rm=TRUE ) ) %>%
    select( YEAR, AB1, B2 ) %>%
    arrange( YEAR )
  unid.catch = as.data.frame( unid.catch )
  
  
  ###   Moving on to (2) -- 'identified' catch-by-species (collapsed across year)
  ratio.spp = unid.dat[ which( unid.dat$SP_CODE %notin% nodc.unid.catch ), ] %>%
    group_by( NEW_COM ) %>%
    summarise( AB1 = sum( AB1, na.rm=TRUE ),
               B2  = sum(  B2, na.rm=TRUE ) )
    # AB1.wgt = sum( as.numeric(lbsest_SECwwt), na.rm=TRUE ) )

  dummy = colSums( ratio.spp[ ,c('AB1','B2')] )

  ratio.spp = ratio.spp %>%
    mutate( AB1.total = dummy['AB1'],
             B2.total = dummy['B2']  ) %>%
    mutate( p.AB1 = ifelse( AB1.total==0, NA, AB1 / AB1.total ),
            p.B2  = ifelse(  B2.total==0, NA,  B2 /  B2.total ) ) %>%
    # p.AB1.wgt = AB1.wgt / LBS.total ) ) %>%
    select( !c(AB1.total,B2.total) )
  rm(dummy)
  
  
  ###     ...and to (3) -- 'identified' catch by-species and by-year
  ratio.year = unid.dat[ which( unid.dat$SP_CODE %notin% nodc.unid.catch ), ] %>%
    group_by( NEW_COM, YEAR ) %>%
    summarise( AB1 = sum( AB1, na.rm=TRUE ),
               B2  = sum(  B2, na.rm=TRUE ) )
  # AB1.wgt = sum( as.numeric(lbsest_SECwwt), na.rm=TRUE ) )
  
  dummy = ratio.year %>%
    group_by( YEAR ) %>%
    summarise( AB1.total = sum( AB1, na.rm=TRUE ),
               B2.total  = sum(  B2, na.rm=TRUE ) )
  # LBS.total = sum( as.numeric(lbsest_SECwwt), na.rm=TRUE ) )
  
  ratio.year = ratio.year %>%
    full_join( dummy, by = 'YEAR' ) %>%
    mutate( p.AB1 = ifelse( AB1.total==0, NA, AB1 / AB1.total ),
            p.B2  = ifelse(  B2.total==0, NA,  B2 /  B2.total ) ) %>%
    # p.AB1.wgt = AB1.wgt / LBS.total ) ) %>%
    select( !c(AB1.total,B2.total) )
  rm(dummy)
  
  
  
  ###   Lastly, I need to format the three tables (above) before joining them into the (final) 'unid.table' summary
  ###     (i.e., that printed to our final excel spreadsheet ), which will be of the form:
  ###
  ###              UNID CATCH      ID CATCH #1      ID CATCH #2     ID CATCH #3
  ###             -- NEWCOM --    -- NEWCOM --     -- NEWCOM --    -- NEWCOM --
  ###
  ###      YEAR     AB1   B2        AB1   B2        AB1   B2        AB1   B2
  ###
  ###     TOTAL     ...   ...       ...   ...       ...   ...       ...   ...
  ###
  ###      1981     ...   ...       ...   ...       ...   ...       ...   ...
  ###      1982     ...   ...       ...   ...       ...   ...       ...   ...
  ###      1983     ...   ...       ...   ...       ...   ...       ...   ...
  ###     < and so on... >          < and so on... >          < and so on... >
  
  
  ###     Starting with ( 1 = 'unid.catch' ), which is comprised of the 'unidentified' catch estimates
  ###     that are to be partitioned ( and represented by the first two columns in our summary 'unid.table' ),
  ###     the associated 'unid.catch' table is already set-up in the desired format (i.e., rows = YEAR, columns = AB1 & B2 )
  ###     and so no additional adjustments to formatting need to be applied. However, the current 'unid.catch' table
  ###     is missing the TOTAL summary (i.e., sum(catch) across years ), which I add now...
  
  unid.catch = unid.catch %>% mutate( YEAR = as.character(YEAR) )
  dummy = data.frame( YEAR = 'TOTAL',
                      AB1 = sum( unid.catch$AB1,na.rm=TRUE ),
                      B2  = sum( unid.catch$B2 ,na.rm=TRUE ) )
  unid.catch = rbind( dummy, unid.catch )
  rm(dummy)
  
  
  ###     Moving onto (2) and (3), which are to be printed into the "TOTAL" and YEAR-specific columns of 'unid.table' respectively,
  ###     we need to convert these tables from a long format to a wide format, using the NEW_COM field to perform the pivot.
  ###     Note that I also make sure that the species-of-interest is printed first in these summaries, as that is likely
  ###     to be of the most interest to assessment analysts...
  
  spp.order = ratio.spp$NEW_COM
  spp.order = c( as.character( new.com ),
                 as.character( spp.order[spp.order %notin% new.com] ) )
  
  col.order = spp.order
  for( i in 1:length(spp.order) ) {
    col.order[ ((i-1)*4) + 1 ] = paste0( spp.order[i],"_AB1" )
    col.order[ ((i-1)*4) + 2 ] = paste0( spp.order[i],"_B2"  )
    col.order[ ((i-1)*4) + 3 ] = paste0( spp.order[i],"_p.AB1" )
    col.order[ ((i-1)*4) + 4 ] = paste0( spp.order[i],"_p.B2"  )
  }
  
  ratio.spp = ratio.spp %>%
    # arrange( factor( NEW_COM, levels = spp.order ) ) %>%
    # ###     ...which doesn't do anything as pivot_wider() only sorts columns based on what elements are seen first
    # ###         (factor level doesn't impact sorting). Instead, I apply a select() statement to get the desired sorting...
    pivot_wider( names_from = NEW_COM , values_from = c( AB1, B2, p.AB1, p.B2 ), names_glue = "{NEW_COM}_{.value}" ) %>%
    select( contains( col.order ) )
  
  ratio.year = ratio.year %>%
    pivot_wider( names_from = NEW_COM , values_from = c( AB1, B2, p.AB1, p.B2 ), names_glue = "{NEW_COM}_{.value}" ) %>%
    select( YEAR, contains( col.order ) )
  
  rm( col.order, spp.order )
  
  
  ### FINAL MERGE/JOIN ###
  ###
  ###   ...in preparation of joining the three summary tables above, we make sure all fields are formatted the same way...
  
  ###   The 'ratio.spp' table (which is a summary across years) is missing a YEAR field, which we set = 'TOTAL'...
  dummy = data.frame( YEAR = 'TOTAL' )
  ratio.spp = cbind( dummy, ratio.spp )
  rm( dummy )
  
  ###   Similarly, we also change the format of YEAR in 'ratio.year' to character, which is how it is be defined in 'ratio.spp'...
  ratio.year = ratio.year %>% mutate( YEAR = as.character(YEAR) ) %>% arrange( YEAR )
  
  ###   With the two 'ratio' tables appropriately defined, we then join them...
  ratio.table = rbind( ratio.spp, ratio.year )
  rm( ratio.spp, ratio.year )
  
  ###   ...and join this combined 'ratio' table with 'unid.catch' to produce our final summary table...
  unid.table = full_join( unid.catch, ratio.table, by='YEAR' )
  unid.table = unid.table %>% arrange( YEAR )
  
  rm( unid.catch, ratio.table )
  
  
  
  
  # ### Assignment of UnIDd Catch to Assessed Species
  # ### ---------------------------------------------
  # 
  # 
  # ###   For which the analysis above ( in 'unid.table' ) is used to determine whether (and how much)
  # ###   unidentified catch to allocate to the species-of-interest...
  # ###
  # ###     -- Is there enough unidentified catch to warrant its consideration?
  # 
  # colSums(  unid.table[ ,c('AB1','B2')                 ], na.rm=TRUE )
  # colSums( catch.table[ ,c('AB1','B2','lbsest_SECwwt') ], na.rm=TRUE )
  # 
  # ###     -- What fraction of unidentified catch to assign to the species-of-interest?
  # 
  # sumAB1.bySPP <- colSums( unid.table[ -1 ,grepl('_AB1',colnames(unid.table)) ], na.rm=TRUE )
  # ###           ...with the first row excluded to remove the 'TOTAL' row...
  # sumAB1.bySPP[1] / sum( sumAB1.bySPP,na.rm=TRUE )
  # 
  # sumB2.bySPP <- colSums( unid.table[ -1 ,grepl('_B2',colnames(unid.table)) ], na.rm=TRUE )
  # sumB2.bySPP[1] / sum( sumB2.bySPP,na.rm=TRUE )
  # 
  # 
  # ### **********************************************************************************************
  # ###
  # ###     ...where, for S84, the decision was to assign some percentage of UnID snappers to YTL,
  # ###       the rationale being that unidentified catch was fairly consistently observed throughout the
  # ###       first ~10 years of the timeseries and that a non-significant percentage of this catch looks
  # ###       to be associated with YTL (~35-40%). Note that the (total) catch from UnID snappers was only
  # ###       ~5-10% of that estimated for YTL and so probably has limited effect in the assessment either way...
  # ###
  # ### **********************************************************************************************
  # 
  # 
  # unid.ratio = 0.40
  # 
  # 
  # dummy = unid.dat[ unid.dat$SP_CODE %in% nodc.unid.catch, ]
  # 
  # dummy$UNID_FLAG = "Y"       ### ...which acts as a flag for catch records that were originally 'unidentified'...
  # 
  # dummy$SP_CODE      = as.character( nodc.code[1] )
  # dummy$NEW_COM      = as.character( new.com[1]   )
  # dummy$NEW_SCI      = as.character( new.sci[1]   )
  # dummy$ITIS_CODE    = as.character( itis.code[1] )
  # dummy$SPECIES_CODE = as.character( tpwd.code[1] )
  # 
  # 
  # ### Catch-in-Numbers ###
  # 
  # dummy$AB1 = dummy$AB1 * unid.ratio
  # dummy$B2  = dummy$B2  * unid.ratio
  # # dummy$A   = dummy$A   * unid.ratio
  # # dummy$B1  = dummy$B1  * unid.ratio
  # # dummy$CHTS_CL = dummy$CHTS_CL * unid.ratio
  # # dummy$CHTS_H  = dummy$CHTS_H  * unid.ratio
  # # dummy$CHTS_RL = dummy$CHTS_RL * unid.ratio
  # 
  # dummy$VAR_AB1 = dummy$VAR_AB1 * (unid.ratio^2)
  # dummy$VAR_B2  = dummy$VAR_B2  * (unid.ratio^2)
  # # dummy$CHTS_VAR_CL = dummy$CHTS_VAR_CL * (unid.ratio^2)
  # # dummy$CHTS_VAR_H  = dummy$CHTS_VAR_H  * (unid.ratio^2)
  # # dummy$CHTS_VAR_RL = dummy$CHTS_VAR_RL * (unid.ratio^2)
  # 
  # 
  # ### Catch-in-Weight ###
  # ###
  # ###     ...for which I first need to add the SEFSC avgwgt estimates to 'dummy', which are loaded from the
  # ###         avgwgt files imported at the top of the script -- one file (one set of estimates) per hierarchy level...
  # 
  # avgwgt_s$dummy_label = paste0( avgwgt_s$new_com )
  #    dummy$dummy_label = paste0(    dummy$NEW_COM )
  # dummy$AVGWGT_S = avgwgt_s$avgwgt_s[ match( dummy$dummy_label, avgwgt_s$dummy_label ) ]
  # dummy$NUMWGT_S = avgwgt_s$numwgt_s[ match( dummy$dummy_label, avgwgt_s$dummy_label ) ]
  # dummy = dummy %>% select( !dummy_label )
  # 
  # avgwgt_sr$dummy_label = paste0( avgwgt_sr$new_com,"_",avgwgt_sr$SUB_REG )
  #     dummy$dummy_label = paste0(     dummy$NEW_COM,"_",    dummy$SUB_REG )
  # dummy$AVGWGT_SR = avgwgt_sr$avgwgt_sr[ match( dummy$dummy_label, avgwgt_sr$dummy_label ) ]
  # dummy$NUMWGT_SR = avgwgt_sr$numwgt_sr[ match( dummy$dummy_label, avgwgt_sr$dummy_label ) ]
  # dummy = dummy %>% select( !dummy_label )
  # 
  # avgwgt_sry$dummy_label = paste0( avgwgt_sry$new_com,"_",avgwgt_sry$SUB_REG,"_",avgwgt_sry$YEAR )
  #      dummy$dummy_label = paste0(      dummy$NEW_COM,"_",     dummy$SUB_REG,"_",     dummy$YEAR )
  # dummy$AVGWGT_SRY = avgwgt_sry$avgwgt_sry[ match( dummy$dummy_label, avgwgt_sry$dummy_label ) ]
  # dummy$NUMWGT_SRY = avgwgt_sry$numwgt_sry[ match( dummy$dummy_label, avgwgt_sry$dummy_label ) ]
  # dummy = dummy %>% select( !dummy_label )
  # 
  # avgwgt_srys$dummy_label = paste0( avgwgt_srys$new_com,"_",avgwgt_srys$SUB_REG,"_",avgwgt_srys$YEAR,"_",
  #                                   avgwgt_srys$new_sta )
  #       dummy$dummy_label = paste0(       dummy$NEW_COM,"_",      dummy$SUB_REG,"_",      dummy$YEAR,"_",
  #                                         dummy$NEW_STA )
  # dummy$AVGWGT_SRYS = avgwgt_srys$avgwgt_srys[ match( dummy$dummy_label, avgwgt_srys$dummy_label ) ]
  # dummy$NUMWGT_SRYS = avgwgt_srys$numwgt_srys[ match( dummy$dummy_label, avgwgt_srys$dummy_label ) ]
  # dummy = dummy %>% select( !dummy_label )
  # 
  # avgwgt_srysm$dummy_label = paste0( avgwgt_srysm$new_com,"_",avgwgt_srysm$SUB_REG,"_",avgwgt_srysm$YEAR,"_",
  #                                    avgwgt_srysm$new_sta,"_",avgwgt_srysm$new_moden )
  #        dummy$dummy_label = paste0(        dummy$NEW_COM,"_",       dummy$SUB_REG,"_",       dummy$YEAR,"_",
  #                                           dummy$NEW_STA,"_",       dummy$NEW_MODEN )
  # dummy$AVGWGT_SRYSM = avgwgt_srysm$avgwgt_srysm[ match( dummy$dummy_label, avgwgt_srysm$dummy_label ) ]
  # dummy$NUMWGT_SRYSM = avgwgt_srysm$numwgt_srysm[ match( dummy$dummy_label, avgwgt_srysm$dummy_label ) ]
  # dummy = dummy %>% select( !dummy_label )
  # 
  # avgwgt_srysmw$dummy_label = paste0( avgwgt_srysmw$new_com,"_",avgwgt_srysmw$SUB_REG,"_",avgwgt_srysmw$YEAR,"_",
  #                                     avgwgt_srysmw$new_sta,"_",avgwgt_srysmw$new_moden,"_",avgwgt_srysmw$WAVE )
  #         dummy$dummy_label = paste0(         dummy$NEW_COM,"_",        dummy$SUB_REG,"_",        dummy$YEAR,"_",
  #                                             dummy$NEW_STA,"_",        dummy$NEW_MODEN,"_",      dummy$WAVE )
  # dummy$AVGWGT_SRYSMW = avgwgt_srysmw$avgwgt_srysmw[ match( dummy$dummy_label, avgwgt_srysmw$dummy_label ) ]
  # dummy$NUMWGT_SRYSMW = avgwgt_srysmw$numwgt_srysmw[ match( dummy$dummy_label, avgwgt_srysmw$dummy_label ) ]
  # dummy = dummy %>% select( !dummy_label )
  # 
  # avgwgt_srysmwa$dummy_label = paste0( avgwgt_srysmwa$new_com,"_",avgwgt_srysmwa$SUB_REG,"_",avgwgt_srysmwa$YEAR,"_",
  #                                      avgwgt_srysmwa$new_sta,"_",avgwgt_srysmwa$new_moden,"_",avgwgt_srysmwa$WAVE,"_",
  #                                      avgwgt_srysmwa$new_arean )
  #          dummy$dummy_label = paste0(          dummy$NEW_COM,"_",         dummy$SUB_REG,"_",         dummy$YEAR,"_",
  #                                               dummy$NEW_STA,"_",         dummy$NEW_MODEN,"_",       dummy$WAVE,"_",
  #                                               dummy$NEW_AREAN )
  # dummy$AVGWGT_SRYSMWA = avgwgt_srysmwa$avgwgt_srysmwa[ match( dummy$dummy_label, avgwgt_srysmwa$dummy_label ) ]
  # dummy$NUMWGT_SRYSMWA = avgwgt_srysmwa$numwgt_srysmwa[ match( dummy$dummy_label, avgwgt_srysmwa$dummy_label ) ]
  # dummy = dummy %>% select( !dummy_label )
  # 
  # 
  # ###   ...and then apply the appropriate SEFSC avgwgt estimate to the associated catch (in-number) estimate...
  # 
  # n.size.threshold = 15
  # 
  # dummy$AVGWGT_SEC = 0
  # 
  # for( i in 1:dim(dummy)[1] ) {
  # 
  #   if( !is.na(dummy$NUMWGT_SRYSMWA[i]) & dummy$NUMWGT_SRYSMWA[i] >= n.size.threshold ) {
  #     dummy$LBSEST_SECSOURCE[i] = "srysmwa"
  #     dummy$AVGWGT_SEC[i] = dummy$AVGWGT_SRYSMWA[i]
  #   } else if( !is.na(dummy$NUMWGT_SRYSMW[i]) & dummy$NUMWGT_SRYSMW[i] >= n.size.threshold ) {
  #     dummy$LBSEST_SECSOURCE[i] = "srysmw"
  #     dummy$AVGWGT_SEC[i] = dummy$AVGWGT_SRYSMW[i]
  #   } else if( !is.na(dummy$NUMWGT_SRYSM[i]) & dummy$NUMWGT_SRYSM[i] >= n.size.threshold ) {
  #     dummy$LBSEST_SECSOURCE[i] = "srysm"
  #     dummy$AVGWGT_SEC[i] = dummy$AVGWGT_SRYSM[i]
  #   } else if( !is.na(dummy$NUMWGT_SRYS[i]) & dummy$NUMWGT_SRYS[i] >= n.size.threshold ) {
  #     dummy$LBSEST_SECSOURCE[i] = "srys"
  #     dummy$AVGWGT_SEC[i] = dummy$AVGWGT_SRYS[i]
  #   } else if( !is.na(dummy$NUMWGT_SRY[i]) & dummy$NUMWGT_SRY[i] >= n.size.threshold ) {
  #     dummy$LBSEST_SECSOURCE[i] = "sry"
  #     dummy$AVGWGT_SEC[i] = dummy$AVGWGT_SRY[i]
  #   } else if( !is.na(dummy$NUMWGT_SR[i]) & dummy$NUMWGT_SR[i] >= n.size.threshold ) {
  #     dummy$LBSEST_SECSOURCE[i] = "sr"
  #     dummy$AVGWGT_SEC[i] = dummy$AVGWGT_SR[i]
  #   } else {
  #     dummy$LBSEST_SECSOURCE[i] = "s"
  #     dummy$AVGWGT_SEC[i] = dummy$AVGWGT_S[i]
  #   }
  # 
  # }
  # 
  # dummy$lbsest_SECwwt = dummy$AB1 * dummy$AVGWGT_SEC
  # dummy = dummy %>% select( !AVGWGT_SEC )
  # 
  # 
  # blah = catch.table[ catch.table$SP_CODE %notin% nodc.unid.catch, ]
  # catch.table = bind_rows( blah, dummy )
  # 
  # rm( unid.ratio, n.size.threshold, blah, dummy )

}




### ---------------------------------------------------------------------------------------------------------------- ###
### ---------------------------------------------------------------------------------------------------------------- ###



# catch.summary <- catch.table %>%
#   filter( NEW_STA == "PR" ) %>%
#   
#   group_by( NEW_COM, YEAR ) %>%
#   # group_by( NEW_COM, YEAR, NEW_STA ) %>%
#   # group_by( NEW_COM, YEAR, NEW_MODEN ) %>%
#   
#   summarize( AB1 = sum( as.numeric(AB1), na.rm=TRUE ),
#              B2  = sum( as.numeric(B2), na.rm=TRUE ),
#              LBS = sum( as.numeric(lbsest_SECwwt), na.rm=TRUE ) ) %>%
#   
#   select( NEW_COM, YEAR, AB1, B2, LBS )
#   # select( NEW_COM, YEAR, NEW_STA, AB1, B2, LBS ) %>%
#   # pivot_wider( names_from=NEW_STA, values_from=c(AB1,B2,LBS) )
#   # select( NEW_COM, YEAR, NEW_MODEN, AB1, B2, LBS ) %>%
#   # pivot_wider( names_from=NEW_MODEN, values_from=c(AB1,B2,LBS) )





####################################################################################################################
####################################################################################################################
####################################################################################################################
#############################                                                          #############################
#############################               Comparison to Previous SEDARs              #############################
#############################                                                          #############################
####################################################################################################################
####################################################################################################################
####################################################################################################################


if( prev.sedar != "None" ) {
  
  sedar.comparison <- catch.table %>%
    group_by( YEAR ) %>%
    summarize( AB1 = sum( as.numeric(AB1), na.rm=TRUE ),
               B2  = sum( as.numeric(B2), na.rm=TRUE ) ) %>%
    select( YEAR, AB1, B2 )
  
  ### I also make sure all the years (between "first.year" and "term.year") are accounted for in this table...
  assess.years <- first.year:term.year
  
  for( i in 1:length(assess.years) ) {
    if( assess.years[i] %notin% sedar.comparison$YEAR ) {
      sedar.comparison <- rbind( data.frame( sedar.comparison ), data.frame( YEAR=assess.years[i], AB1=0, B2=0 ) )
    } }
  rm( assess.years )
  
  ### I then...
  sedar.comparison <- sedar.comparison %>%
    arrange( YEAR ) %>%                                             ### ...sort by year...
    mutate_at( c("AB1","B2"), round, digits=0 )                     ### ...round the "AB1" & "B2" columns to zero decimal places...
  sedar.comparison <- sedar.comparison %>%                          ### ...and add a new column to identify the old assessment
    add_column( old_sedar=sedar.comparison$YEAR, .after=3 )         ###   ( which has values=YEAR and colname='prev.sedar' )
  colnames(sedar.comparison)[ which( colnames(sedar.comparison)=="old_sedar" ) ] <- prev.sedar
  colnames(sedar.comparison)[ which( colnames(sedar.comparison)=="YEAR" ) ] <- current.sedar
  
  ### I also considered adding a comma-separator to the "AB1" and "B2" columns...
  # sedar.comparison <- sedar.comparison %>%
  #   mutate_at( c("AB1","B2"), format, nsmall=0, big.mark="," )
  # ###     ...but this changes the format to 'character' so I don't bother with this step
  # ###       ( instead, I just apply the appropriate formatting in the excel template document )...
  
}






####################################################################################################################
####################################################################################################################
####################################################################################################################
#############################                                                          #############################
#############################                 MRIP CVs -- Catch-in-Number              #############################
#############################                                                          #############################
####################################################################################################################
####################################################################################################################
####################################################################################################################



###################################
###     Caribbean Assessment    ###
###     -- use old approach     ###
###################################
###
###     S&T has yet to include Caribbean data in the new file format and, therefore, it is not included in any of our
###     tables in RDI. CVs for Caribbean assessments are therefore generated using the "old" approach, wherein we
###     sum-up the MRIP-provided "var_ab1" and "var_b2" fields (at the strata level), square-root them, and divide them
###     by sum(AB1) and sum(B2) respectively. This is relatively straight-forward to do in R (see code below), but it also
###     requires knowledge of the strata at which CV estimates need to be generated b/c the output is a static table.
###     To provide analysts flexibility in exploring different stratification options (for catch CVs):
###
###               ****    "CV_AB1" and "CV_B2" fields will be manually added to the pivot table   ****
###
### However, analysts are also frequently interested in the associated sample sizes for these estimates. Therefore,
###     in addition to adding the "CV_AB1" and "CV_B2" fields to the provided pivot table, I'll also retain the
###     typical "MRIP_CV" tab (at the year-mode level), which I construct from the MRIP "var" fields instead of a RDI report...


### CATCH CVs ###
### _____________
###
###     ...where if multiple species are included in "catch.table", a separate pull of catch estimates is needed
###       ( cannot sum variances across species groups )...

if( flag.cv ) {
  cv.dat  = dat.filter(  acl.table = ACL_catch,             spp.filter = nodc.cv,
                         yr.filter = first.year:term.year,  mode.filter = mode_sub,
                        reg.filter = region, sta.filter = states, fl.filter = FL_sub, nc.filter = NC_sub )
} else {
  cv.dat = catch.table
}


cv.table <- cv.dat %>%
  group_by( YEAR, NEW_MODEN ) %>%
  ###   ...where these group_by() statements control the strata at which CVs are calculated...
  summarize( AB1 = sum( as.numeric(AB1), na.rm=TRUE ),
             B2  = sum( as.numeric( B2), na.rm=TRUE ),
             VAR_AB1 = sum( as.numeric(VAR_AB1), na.rm=TRUE ),
             VAR_B2  = sum( as.numeric(VAR_B2 ), na.rm=TRUE ) ) %>%
  mutate( CV_AB1 = ( sqrt(VAR_AB1) / AB1 ),
          CV_B2  = ( sqrt(VAR_B2 ) / B2  ) ) %>%
  select( YEAR,NEW_MODEN, AB1,B2, CV_AB1,CV_B2 )

cv.table[ is.na(cv.table) ] = 0


cv.total <- cv.dat %>%
  group_by( YEAR ) %>%
  summarize( AB1 = sum( as.numeric(AB1), na.rm=TRUE ),
             B2  = sum( as.numeric( B2), na.rm=TRUE ),
             VAR_AB1 = sum( as.numeric(VAR_AB1), na.rm=TRUE ),
             VAR_B2  = sum( as.numeric(VAR_B2 ), na.rm=TRUE ) ) %>%
  mutate( CV_AB1 = ( sqrt(VAR_AB1) / AB1 ),
          CV_B2  = ( sqrt(VAR_B2 ) / B2  ) ) %>%
  select( YEAR,AB1,B2, CV_AB1,CV_B2 )

cv.total[ is.na(cv.total) ] = 0


rm( cv.dat )


### SAMPLE SIZES ###
### ________________
###
###   ...for which I need total number of angler trips and (in parentheses) num.trips that intercepted assessed fish
###       For Caribbean assessments, I will have to go back into the i-files to identify the appropriate
###       fishing trips ( which is why there is script that separates if( region=="Caribbean" ) )...

con = dbConnect(dbDriver("Oracle"), username = keyring::key_list("SECPR")[1,2],
                password = keyring::key_get("SECPR", keyring::key_list("SECPR")[1,2]), dbname = "SECPR")

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

n.table = n.table[ which( n.table$sub_reg == "11" ), ]
n.table = n.table[ which( n.table$state %in% st_carib ), ]
n.table = n.table[ which( n.table$year %in% first.year:term.year ), ]
n.table = n.table[ which( n.table$new_moden %in% mode_sub ), ]

n.table = n.table %>%
  group_by( year, new_moden ) %>%
  summarize( N = length( unique(id_code) ) ) %>%
  select( year, new_moden, N ) %>%
  rename( YEAR = year, NEW_MODEN = new_moden )



### Number of Positive Trips ###

pos.table = pos.table[ which( pos.table$sp_code %in% nodc.code ), ]
###     ...where 'pos.table' is already species-specific, but I (re)apply the species filter as a double check...
pos.table = pos.table[ which( pos.table$year %in% first.year:term.year ), ]
pos.table = pos.table[ which( pos.table$sub_reg == 11 ), ]
pos.table = pos.table[ which( pos.table$new_sta %in% states ), ]
pos.table = pos.table[ which( pos.table$new_moden %in% mode_sub ), ]

pos.ab1.trips = pos.table[ which( pos.table$A > 0 | pos.table$B1 > 0 ), ] %>%
  group_by( year, new_moden ) %>%
  summarize( SS = length( unique(id_code) ) ) %>%
  select( year, new_moden, SS ) %>%
  rename( YEAR = year, NEW_MODEN = new_moden )
pos.b2.trips = pos.table[ which( pos.table$B2 > 0 ), ] %>%
  group_by( year, new_moden ) %>%
  summarize( SS = length( unique(id_code) ) ) %>%
  select( year, new_moden, SS ) %>%
  rename( YEAR = year, NEW_MODEN = new_moden )



### JOINING TABLES ###
### __________________
###
cv.table = full_join( cv.table, n.table, by=c("YEAR","NEW_MODEN"), suffix=c("","_N") )
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
rm( cv.total, n.table, pos.ab1.trips, pos.b2.trips )







####################################################################################################################
####################################################################################################################
####################################################################################################################
#############################                                                          #############################
#############################                 MRIP CVs -- Catch-in-Weight              #############################
#############################                 APPROACH (2) -- S74-DW-12                #############################
#############################                                                          #############################
####################################################################################################################
####################################################################################################################
####################################################################################################################




###########################
### Landings-in-Numbers ###
###########################

AB1.cv.table = cv.table

AB1.cv.table = AB1.cv.table[ , !( grepl("_SS",colnames(AB1.cv.table)) | grepl("_B2",colnames(AB1.cv.table)) ) ]
AB1.cv.table = AB1.cv.table %>%
  mutate_all( as.numeric ) %>%
  rename_all( toupper )

colnames(AB1.cv.table) = gsub( "_CV_AB1","_CV", colnames(AB1.cv.table) )


### Convert CV column to VAR ###

modes = unique( gsub( "_.*","", colnames(AB1.cv.table)[-1] ) )
# if( ( ( "Cbt" %in% mode_sub ) | ( "Hbt" %in% mode_sub ) ) &
#     any( c("VA","MD","DE","PA","NJ","NY","CT","RI","MA","NH","ME") %in% states ) ) {
#   modes = append( modes, "CBT_HBT", after = match("CBT",modes) )
# }


blah = AB1.cv.table %>% select( YEAR )
for( i in 1:length(modes) ) {
  
  eval( parse( text = paste0( "dummy = AB1.cv.table %>% select( which( grepl( 'YEAR',colnames(AB1.cv.table) ) ) |
                                                                which( grepl( '",modes[i],"',colnames(AB1.cv.table) ) ) ) %>% ",
                              "mutate( ",modes[i],"_VAR = (",modes[i],"_CV * ",modes[i],"_AB1)^2 )" ) ) )
  if( modes[i] == "CBT" ) { dummy = dummy[ !grepl( "HBT", colnames(dummy) ) ] }     ### ...to remove any "CBT_HBT" entries in the Mid/North ATL...
  if( modes[i] == "HBT" ) { dummy = dummy[ !grepl( "CBT", colnames(dummy) ) ] }     ### ...to remove any "CBT_HBT" entries in the Mid/North ATL...
  blah = blah %>%
    full_join( dummy %>% select( which( !grepl( 'CV',colnames(dummy) ) ) ), by = "YEAR" )
  
}

AB1.cv.table = blah
rm( modes, dummy, blah, i )

AB1.cv.table[ is.na(AB1.cv.table) ] = 0



######################
### Average Weight ###
######################

avgwt.cv.table <- read_excel( path=paste0( dir,"/",sedar.size.file ),
                              sheet="Weight Summary by Mode", trim_ws=FALSE, col_types="text" )

avgwt.cv.table = avgwt.cv.table[ , !grepl("_N",colnames(avgwt.cv.table)) ]

avgwt.cv.table = avgwt.cv.table %>%
  mutate_all( as.numeric ) %>%
  rename_all( toupper )


### Convert SE column to CV ###
###
###       ...where although I ultimately need variances for avgwgts (to combine with variances for catch-in-number),
###           I first convert these SEs to CVs to quantify the %variability in the original WGT 'currencies'.
###           In particular, the WGT estimates in the current table are calculated from the raw data and are not
###           the same as the actual actual SEFSC avgwgt estimates, which may be calculated across multiple strata.
###           Therefore, I calculate the CVs of the raw (size) summaries so that I may then apply them to the
###           actual SEFSC avgwgts, so that variances are in the 'correct' units...

modes = unique( gsub( "_.*","", colnames(avgwt.cv.table)[ colnames(avgwt.cv.table) %notin% c("YEAR") ] ) )

blah = avgwt.cv.table %>% select( YEAR )
for( i in 1:length(modes) ) {
  
  eval( parse( text = paste0( "dummy = avgwt.cv.table %>% select( which( grepl( 'YEAR',colnames(avgwt.cv.table) ) ) |
                                                                which( grepl( '",modes[i],"',colnames(avgwt.cv.table) ) ) ) %>% ",
                              "mutate( ",modes[i],"_CV = ",modes[i],"_SE / ",modes[i],"_WGT )" ) ) )
  blah = blah %>%
    full_join( dummy %>% select( which( !grepl( 'SE',colnames(dummy) ) ) ), by = "YEAR" )
  
}

avgwt.cv.table = blah
rm( modes, dummy, blah, i )



### Substitute SEFSC AvgWgts into Table ###
###
###       ...which are calculated from the final AB1 & LBSEST estimates in 'catch.table'...

dummy = catch.table %>%
  group_by( YEAR, NEW_MODEN ) %>%
  summarise( LBS = sum( lbsest_SECwwt, na.rm=TRUE ),
             AB1 = sum( AB1, na.rm=TRUE ) ) %>%
  mutate( WGT = ifelse( AB1==0, 0, LBS / AB1 ) )

blah = dummy %>%
  group_by( YEAR ) %>%
  summarise( LBS = sum( LBS, na.rm=TRUE ),
             AB1 = sum( AB1, na.rm=TRUE ) ) %>%
  mutate( NEW_MODEN = "Total",
          WGT = ifelse( AB1==0, 0, LBS / AB1 ) )

dummy = bind_rows( dummy, blah ) %>%
  pivot_wider( names_from = NEW_MODEN, values_from = c( LBS,AB1,WGT ), names_glue = "{NEW_MODEN}_{.value}" ) %>%
  rename_all( toupper ) %>%
  select( which( colnames(.) == "YEAR" | grepl( 'WGT',colnames(.) ) ) )
rm( blah )


blah = avgwt.cv.table %>% select( which( !grepl( 'WGT',colnames(.) ) ) )

dummy = dummy %>% full_join( blah, by="YEAR" )
rm( blah )


colnames(dummy) = gsub( "\\/","_",colnames(dummy) )       ### ...to account for any records where NEW_MODEN = 'CBT/HBT'...


modes = unique( gsub( "_.*","", colnames(dummy)[ colnames(dummy) %notin% c("YEAR") ] ) )
# if( ( ( "Cbt" %in% mode_sub ) | ( "Hbt" %in% mode_sub ) ) &
#     any( c("VA","MD","DE","PA","NJ","NY","CT","RI","MA","NH","ME") %in% states ) ) {
#   modes = append( modes, "CBT_HBT", after = match("CBT",modes) )
# }
# 
# if( "CBT_HBT" %in% modes ) {
#   dummy$CBT_HBT_CV = NA
#   dummy$CBT_HBT_CV[ !is.na( dummy$CBT_HBT_WGT) ] = dummy$HBT_CV[ !is.na( dummy$CBT_HBT_WGT) ]
# }

###     ...to which I convert the associated CV estimates to variances...

command.line = "dummy = dummy %>% mutate( "
for( i in 1:length(modes) ) {
  
  if( i != length(modes) ) {
    command.line = paste0( command.line, modes[i],"_VAR = ( ", modes[i],"_CV * ", modes[i],"_WGT )^2 , " )
  } else {
    command.line = paste0( command.line, modes[i],"_VAR = ( ", modes[i],"_CV * ", modes[i],"_WGT )^2 ) " )
  }
  
}

###     ...and sort the table...

command.line = paste0( command.line, " %>% select( YEAR , " )
for( i in 1:length(modes) ) {
  
  if( i != length(modes) ) {
    command.line = paste0( command.line, modes[i],"_WGT , ", modes[i],"_VAR , " )
  } else {
    command.line = paste0( command.line, modes[i],"_WGT , ", modes[i],"_VAR ) " )
  }
  
}

eval( parse( text = paste0( command.line ) ) )


avgwt.cv.table = dummy
rm( modes, dummy, command.line, i )

avgwt.cv.table[ is.na(avgwt.cv.table) ] = 0



######################
###      JOIN      ###
######################

lbs.cv.table.2 = AB1.cv.table %>%
  full_join( avgwt.cv.table, by = "YEAR", suffix=c("_AB1","_WGT") )
rm( AB1.cv.table, avgwt.cv.table )

###     ...and to start, I do a quick check and remove any columns for which estimates weren't provided
###         (i.e., colSum(catch) = 0 ), which can happen (for example) in SATL assessment where the HBT mode was
###         'included' in the CV pull -- even though no MRIP HBT estimates exist ( HBT from SRHS in the SATL )...
# 
# lbs.cv.table.2 = lbs.cv.table.2[ , which( colSums(lbs.cv.table.2) != 0 ) ]
# 
drop.modes = colnames(lbs.cv.table.2)[ which( colSums(lbs.cv.table.2) == 0 ) ]
drop.modes = drop.modes[ !( grepl( 'YEAR', drop.modes ) | grepl( '_VAR_', drop.modes ) ) ]
###     ...where I want to identify modes/columns to drop via the CATCH estimates, not the variance estimates;
###         variances may not be estimable from the data for some modes (i.e., var=0 ), but that doesn't mean I
###         want to drop the mode altogether (i.e., if catch estimates still exist )...
drop.modes = unique( gsub( '_.*','', drop.modes ) )

if( length(drop.modes) > 0 ) {
  for( i in 1:length(drop.modes) ) {
    lbs.cv.table.2 = lbs.cv.table.2[ , !grepl( drop.modes[i], names(lbs.cv.table.2) ) ]
  }
}
rm( drop.modes )


modes = unique( gsub( "_.*","", colnames(lbs.cv.table.2)[-1] ) )
# if( ( ( "Cbt" %in% mode_sub ) | ( "Hbt" %in% mode_sub ) ) &
#     any( c("VA","MD","DE","PA","NJ","NY","CT","RI","MA","NH","ME") %in% states ) ) {
#   modes = append( modes, "CBT_HBT", after = match("CBT",modes) )
# }

command.line = "dummy = lbs.cv.table.2 %>% "
for( i in 1:length(modes) ) {
  
  if( i != length(modes) ) {
    command.line = paste0( command.line,
                           "mutate( ", modes[i],"_LBS     = ", modes[i],"_AB1 * ", modes[i],"_WGT , ",
                           modes[i],"_VAR_LBS = ( (",modes[i],"_WGT)^2 * ",modes[i],"_VAR_AB1 ) + ",
                           "( (",modes[i],"_AB1)^2 * ",modes[i],"_VAR_WGT ) - ",
                           "( ",modes[i],"_VAR_WGT * ",modes[i],"_VAR_AB1 ) ) %>% ",
                           "mutate( ", modes[i],"_CV_LBS  = sqrt(",modes[i],"_VAR_LBS) / ",modes[i],"_LBS ) %>% " )
  } else {
    command.line = paste0( command.line,
                           "mutate( ", modes[i],"_LBS     = ", modes[i],"_AB1 * ", modes[i],"_WGT , ",
                           modes[i],"_VAR_LBS = ( (",modes[i],"_WGT)^2 * ",modes[i],"_VAR_AB1 ) + ",
                           "( (",modes[i],"_AB1)^2 * ",modes[i],"_VAR_WGT ) - ",
                           "( ",modes[i],"_VAR_WGT * ",modes[i],"_VAR_AB1 ) ) %>% ",
                           "mutate( ",modes[i],"_CV_LBS  = sqrt(",modes[i],"_VAR_LBS) / ",modes[i],"_LBS ) " )
  }
}

eval( parse( text = paste0( command.line ) ) )

dummy = dummy %>%
  # select( which(colnames(.)=="YEAR") | which( grepl( 'AB1',colnames(.) ) ) | which( grepl( 'LBS',colnames(.) ) ) ) %>%
  select( which(colnames(.)=="YEAR") | which( grepl( 'LBS',colnames(.) ) ) ) %>%
  select( which( !grepl( 'VAR',colnames(.) ) ) )

colnames(dummy) = gsub( "CV_LBS","CV", colnames(dummy) )

lbs.cv.table.2 = dummy
rm( modes, dummy, command.line, i )

lbs.cv.table.2[ is.na(lbs.cv.table.2) | lbs.cv.table.2 == 0 ] = NA





####################################################################################################################
####################################################################################################################
####################################################################################################################
#############################                                                          #############################
#############################                   FINAL EXCEL WORKBOOK                   #############################
#############################                                                          #############################
####################################################################################################################
####################################################################################################################
####################################################################################################################


table.ID <- paste0( "YTL_rec_catGEN_",
                    substr( first.year, nchar(first.year)-1, nchar(first.year) ),
                    substr( term.year, nchar(term.year)-1, nchar(term.year) ),
                    "_", gsub("-","", Sys.Date() ) )


if( flag.unid ) {     tab.unid.dat <- paste( "LUTJANIDAE", "_rec_catGEN" )    }


### _______________________________________________________________________________________


### Import template excel file with settings already saved for my pivots...
wb <- loadWorkbook( file=paste0( dir,"/Template_SEDAR_GenCatch_Carib.xlsx" ) )


### General Catch Estimates ###
###
removeWorksheet( wb, sheet="SNWY_REC_CATGEN_8118_20200210" )
###     ...which corresponds to the GenCatch estimates provided for SEDAR 36U snowy, from which the GenCatch.xlsx template was constructed...
addWorksheet( wb, sheet=table.ID )
writeData( wb, sheet=table.ID, x=catch.table, colNames=TRUE )


### Comparisons to Previous SEDAR ###
###
if( prev.sedar != "None" ) {
  writeData( wb, sheet="Compare Previous SEDARs", x=sedar.comparison, colNames=TRUE )
  ### #  renameWorksheet( wb, sheet="Compare Previous SEDARs", newName=paste0("Compare with ",gsub( "EDAR ", "", prev.sedar )) )
  ###     ...where I no longer rename this tab (to reference the specific comparison) because this was screwing up the automatic
  ###         update to the excel figures on this tab, which are looking for the name of the old tab (i.e., "Compare Previous SEDARs" ).
  ###         Therefore, I keep a generic name for this tab and, if choose to later, can update the name to something more
  ###         assessment-specific in the final output...
} else {
  removeWorksheet( wb, sheet="Compare Previous SEDARs" )
}


### MRIP CVs -- Number ###
###
writeData( wb, sheet="MRIP catCV numbers", x=cv.table, colNames=TRUE )


### MRIP CVs -- Weight ###
###
writeData( wb, sheet="MRIP landCV weight", x=lbs.cv.table.2, colNames=TRUE )


### UNIDENTIFIED CATCH ###
###
if( flag.unid ) {
  
  ### (Raw) Table of 'Unidentified' Catch Estimates ###
  removeWorksheet( wb, sheet="UNID_rec_catGEN" )
  addWorksheet( wb, sheet=tab.unid.dat )
  writeData( wb, sheet=tab.unid.dat, x=unid.dat, colNames=TRUE )
  
  
  ### Summary of Annual Catch ( by Year & Species ) --
  writeData( wb, sheet="UNID_ratios",
             x = unid.table %>% filter( YEAR != 'TOTAL' ),
             colNames=FALSE, startRow=9 )
  ###     ...where colNames=FALSE prevents the column names from being written
  
  
  ### Summary of Species Catch ( collapse Year )
  writeData( wb, sheet="UNID_ratios",
             x = unid.table %>% filter( YEAR == 'TOTAL' ),
             colNames=FALSE, startRow=7 )
  
  
  ### Identification of Taxa Groups ( 'header' row )
  ###       ...most of which are identified from the column names in 'unid.table'. ..
  unid.taxa = unique( gsub( '_.*','', colnames(unid.table) ) )
  ###     The only exception to this is the unidentified taxa, the catch of which is saved in columns 2:3 that
  ###     are named 'AB1' and 'B2' (with no reference to the UNID taxa). I therefore remove these 'AB1' & 'B2'
  ###     columns from my 'unid.taxa' object (along with the YEAR column)...
  unid.taxa = unid.taxa[ !( unid.taxa %in% c('YEAR','AB1','B2') ) ]
  ###     ...and add the actual 'unidentified' taxa to my vector (at position 1)
  unid.taxa = c( spp.info$NEW_COM[ spp.info$NODC_CODE %in% nodc.unid.catch[1] ], unid.taxa )
  ###     Lastly, I add the appropriate spacing between each element (so that text isn't written to each column).
  ###     For the 'identified' groups ( each of which needs three <NA>'s inserted ), this is done using a for() statement
  ###     that works backwards, such that previous iterations (of the for() statement) don't require an update to the
  ###     associated 'i' object (in subsequent iterations)...
  for( i in (length(unid.taxa)-1):2 ) {     unid.taxa = append( unid.taxa, c(NA,NA,NA), after = i )   }
  ###     For the 'unidentified' group ( which requires two <NA>'s ), this is just done manually...
  unid.taxa = append( unid.taxa, c(NA,NA), after = 1 )
  
  writeData( wb, sheet="UNID_ratios", x = data.frame( t(unid.taxa) ), colNames=FALSE, startRow=3 )
  rm( i, unid.taxa )
  
  # ### (Raw) Catch Estimates ###
  # removeWorksheet( wb, sheet="UNID_rec_catGEN" )
  # addWorksheet( wb, sheet=tab.unid.dat )
  # writeData( wb, sheet=tab.unid.dat, x=unid.dat, colNames=TRUE )
  # 
  # ### Summary of Catch Estimates ###
  # ###
  # ###     ...catch by year and species...
  # writeData( wb, sheet="UNID_ratios", x=unid.table, colNames=FALSE, startRow=9 )
  # ###
  # ###     ...catch by species (combined over years)...
  # ###
  # ###     ...catch by species (combined over years),
  # ###           and identifying the taxa to which the above catch estimates correspond...
  # writeData( wb, sheet="UNID_ratios", x=new.com.info( taxa.unid.catch, spp.info ), startRow = 3, startCol = 1 )
  # writeData( wb, sheet="UNID_ratios",
  #            x=matrix( as.numeric( colSums(unid.catch[,-1]) ),nrow=1 ), colNames=FALSE, startRow = 7, startCol = 2 )
  # for( i in 1:length(spp.list) ) {
  #   writeData( wb, sheet="UNID_ratios", x=as.character( names(spp.list)[i] ), startRow = 3, startCol = 4*i )
  #   writeData( wb, sheet="UNID_ratios",
  #              x=matrix( as.numeric(
  #                ratio.spp[ ratio.spp$NEW_COM==names(spp.list)[i], ( colnames(ratio.spp) != "NEW_COM" ) ] ), nrow=1 ),
  #              colNames=FALSE, startRow=7, startCol= 4*i )
  # }
} else {
  removeWorksheet( wb, sheet="UNID_rec_catGEN" )
  removeWorksheet( wb, sheet="UNID_ratios" )
}


saveWorkbook( wb, file=paste0( dir,"/",table.ID,".xlsx" ), overwrite=TRUE )




