


####################################################################################################################
####################################################################################################################
####################################################################################################################
#############################                                                          #############################
#############################                      CATCH FILES                         #############################
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


### The first time I imported an ACL (SAS) data file into R, I was converting a SAS size file into a .csv so that
###       Yanet could import it into RDI (to validate some weight estimation code) because RDI cannot import SAS files.
###       The import into R took about 4 hours and writing it to a .csv took another 14 hours. While the ACL files are large,
###       it really shouldn't take this long...
###       The issue with my original code was that I was reading from (and writing to) the NMFS servers, which tends to
###       slow things down immensely. In this code, I therefore import any SAS data file (from these servers) onto my desktop
###       and side-step the NMFS servers completely. Any files that need to be uploaded to the NMFS servers are added manually...

dir <- getwd()

### # dat <- read_sas( data_file = paste0( dir,"/mrip_fes_rec81_25wv4_3dec25.sas7bdat" ) )
### # save.image( file=paste0( dir,"/ACL_catch.RData" ) )

#load( paste0( dir,"/ACL_catch.RData" ) )
dat <- read_sas( data_file = paste0( dir,"/Catch/hmsspec_rec81_25wv1_16may25.sas7bdat" ) )


### ### Caribbean Files ###
### ### -------------------
### # ACL_catch  <- read_sas( data_file = paste0( "R:/ACL/Carib_ACL/2021_Apr27_SEDAR","/mrcat_all00_17_27apr21.sas7bdat" ) )
### # ACL_size   <- read_sas( data_file = paste0( "R:/ACL/Carib_ACL/2021_Apr27_SEDAR","/all_c_size00_17final.sas7bdat" ) )
### # 
### # avgwgt_s       <- read_sas( data_file = paste0( "R:/ACL/Carib_ACL/2021_Apr27_SEDAR",'/avgwgt_s.sas7bdat' ) )
### # avgwgt_sr      <- read_sas( data_file = paste0( "R:/ACL/Carib_ACL/2021_Apr27_SEDAR",'/avgwgt_sr.sas7bdat' ) )
### # avgwgt_sry     <- read_sas( data_file = paste0( "R:/ACL/Carib_ACL/2021_Apr27_SEDAR",'/avgwgt_sry.sas7bdat' ) )
### # avgwgt_srys    <- read_sas( data_file = paste0( "R:/ACL/Carib_ACL/2021_Apr27_SEDAR",'/avgwgt_srys.sas7bdat' ) )
### # avgwgt_srysm   <- read_sas( data_file = paste0( "R:/ACL/Carib_ACL/2021_Apr27_SEDAR",'/avgwgt_srysm.sas7bdat' ) )
### # avgwgt_srysmw  <- read_sas( data_file = paste0( "R:/ACL/Carib_ACL/2021_Apr27_SEDAR",'/avgwgt_srysmw.sas7bdat' ) )
### # avgwgt_srysmwa <- read_sas( data_file = paste0( "R:/ACL/Carib_ACL/2021_Apr27_SEDAR",'/avgwgt_srysmwa.sas7bdat' ) )
### # 
### # cpue_trips <- read_sas( data_file = paste0( "R:/RecrSurveys/MRFSS/catch-eff/detailed/Atl&Gulf-byState",
### #                                             "/mrcpue_all00_17.sas7bdat" ) )
### # effort_old <- read_sas( data_file = paste0( "R:/RecrSurveys/MRFSS/estimates/oldcbt_effort",'/oldmrfsseff2000_2013.sas7bdat' ) )
### # effort_new <- read_sas( data_file = paste0( "R:/RecrSurveys/MRFSS/MRIP/Pre2018_update/Effort",'/mripeff_2014_2017.sas7bdat' ) )
### # 
### # save.image( file = paste0( dir,"/ACL_Carib.RData" ) )
### 
### load( paste0( dir,"/ACL_Carib.RData" ) )
### dat = ACL_catch





####################################################################################################################
####################################################################################################################
####################################################################################################################
#############################                                                          #############################
#############################                         SEDAR 101                         #############################
#############################               Southeast Sandbar Shark                    #############################
#############################                                                          #############################
####################################################################################################################
####################################################################################################################
####################################################################################################################


###       ...for this assessment, data is pulled for...
###
###          Sandbar Shark - Carcharhinus plumbeus
###               - Temporal - include 1981-2024
###               - Modes    - includes charter, private, shore and headboat
###               - Spatial  - ME to TX
###
###
###





#############################
######     FILTERS     ######
#############################



current.sedar <- "SEDAR 101"

 prev.sedar <- "None"
###     ...where the previous SEDAR stock assessment, if one has been conducted,
###         also needs to be identified ( for the "Compare Previous SEDARs" tab )...


### TEMPORAL ###
first.year <- 1981
# first.year <- 2000
# first.year <- 2018
###       ...where "first.year" = 1981 in all requests except projections (which only need a couple years)
###           and Caribbean assessments (wherein MRIP started in 2000)...
term.year <- 2024


### SPATIAL ###
region <- "Southeast"
###   ...which has options:
###         'Gulf of America' = c( "TX","LA","MS","AL","FLW" )
###         'South Atlantic'  = c( "FLE","GA","SC","NC" )
###         'Mid Atlantic'    = c( "VA","MD","DE","PA","NJ","NY" )
###         'North Atlantic'  = c( "CT","RI","MA","NH","ME" )
###         'Caribbean'       = c( "PR","VI" )
###         'Gulf of America and South Atlantic' = *GOA* + *SATL*
###         'Atlantic'                           =         *SATL* + *MATL* + *NATL*
###         'Southeast'                          = *GOA* + *SATL* + *MATL* + *NATL*


states <- c( "TX","LA","MS","AL","FLW","FLE","GA","SC","NC","VA","MD","DE","PA","NJ","NY","CT","RI","MA","NH","ME" )

###     ...which has options c( "TX","LA","MS","AL","FLW","FLE","GA","SC","NC","VA","MD","DE","PA","NJ","NY","CT","RI","MA","NH","ME" )
###     ...or c( "PR","VI" ) for Caribbean assessments...
if( "FL" %in% states | "FLW" %in% states | "FLE" %in% states ) {  FL_sub <- c( 1,2,3, 4, 5 )  }
if( "NC" %in% states ) {                                          NC_sub <- c( "N","S" )  }


### MODE ###
mode_sub <- c( "Priv","Cbt","Hbt","Shore" )
#       ...which has options c( "Priv","Cbt","Hbt","Shore" )
###           Note that the code below removes all HBT fishing from SUB_REG = 6 (SATL), FL_REG = 3 (FL Keys),
###           and SUB_REG = 7 from 1986+, all of which is designed to avoid overlap with SRHS...


### SPECIES ###
###
### Moving onto the species-specific filter, I need to pull data for the species of interest...
###     Therefore, I start by searching for the appropriate identifiers...
# View( spp.info[grep( "GROUPER", spp.info$COMMON ),] )
# View( spp.info[grep( "Centropristis striata", spp.info$SCIENTIFIC ),] )

#sppID.type = 'COMMON'
#taxa <- c( "MACKEREL,KING" )

sppID.type = 'SCIENTIFIC'
taxa <- c( "Carcharhinus plumbeus"  )


###   ...and then import (and apply) the relevant functions to identify the associated spp ID info for 'taxa'...
source( paste0(dir,'/Functions/lookup_sppID.R') )

new.com   <- sapply( taxa, FUN=  new.com.info, spp.field=sppID.type, spp.table=spp.info )
new.sci   <- sapply( taxa, FUN=  new.sci.info, spp.field=sppID.type, spp.table=spp.info )
nodc.code <- sapply( taxa, FUN=nodc.code.info, spp.field=sppID.type, spp.table=spp.info )
itis.code <- sapply( taxa, FUN=itis.code.info, spp.field=sppID.type, spp.table=spp.info )
tpwd.code <- sapply( taxa, FUN=tpwd.code.info, spp.field=sppID.type, spp.table=spp.info )
srhs.code <- sapply( taxa, FUN=srhs.code.info, spp.field=sppID.type, spp.table=spp.info )

rm( sppID.type )




### ADDITIONAL OBJECTS ###
###
### -------------------------------------------------------------------------------------------
###
###   -- MRIP CV TABLES --
###
###     ...which are generated in RDI and, therefore, need to be identified (and imported) into R.
###       Note that the 'report.name' object contains as many elements as there are stockID areas
###       being considered in this assessment, wherein...
###
###       ...length(report.name) == 1   when we're assessing a single stock (i.e., no 'SID' field in catch.table )
###
###       ...but multiple entries when individual 'SID' boundaries are defined for this stock
###         (i.e., one element for each SID domain ). Note that the order in which CV-reports are
###         defined in 'report.name' should align with their geographic order (i.e., as per NEW_ST )...

report.name = c( '1730_GAITLYN.MALONE@NOAA.GOV_2_170955' )


# report.name = "1683_MATTHEW.NUTTALL@NOAA.GOV_160119"
# report.name = c( '1642_MATTHEW.NUTTALL@NOAA.GOV_2',
#                  '1643_MATTHEW.NUTTALL@NOAA.GOV_2',
#                  '1644_MATTHEW.NUTTALL@NOAA.GOV_2' )

report.type = "annual"
# report.type = "detailed"



###     Additionally, separate MRIP CV reports may be needed if we are imputing LACR/TPWD discards using
###     catch rates calculated from either LA or Gulf-wide (B2:AB1) discard ratios, for which we might need:

# LA.report.name = '1648_MATTHEW.NUTTALL@NOAA.GOV_2'
###     ...which identifies a MRIP CV report that includes only Louisiana data

GOM.report.name = '1736_GAITLYN.MALONE@NOAA.GOV_2_130958'
###     ...which identifies a MRIP CV report that includes (at most) Gulf of Mexico states for MS-FLW.
###       Note that for SID species, this table only includes those (non-LA) states in the Gulf of Mexico
###       belonging to the same SID domain as Louisiana (e.g., if SID 'West' includes TX-MS, then
###       'GOM.report.name' only includes MRIP data from MS - excludes the 'East' states of AL & FLW )...

###     Note that these reports may already be defined in 'report.name' (e.g., if SID = 'West' includes TX/LA ,
###       'LA.report.name' can just point to one of the same tables in 'report.name' ), but this isn't
###       necessarily the case and so I define them as separate objects...

### -------------------------------------------------------------------------------------------

# ### Objects for Carib SEDARs ###
# ### ----------------------------
# 
# report.name = 'None'
# ###     ...where "report.name" isn't included in Caribbean assessments because S&T has yet to include Caribbean data
# ###         in the new file format. Instead, CVs for Caribbean assessments are generated from the "old" method
# ###         (i.e., sum(variance) ) and not from a pre-existing RDI report...
# 
# ###     The associated sample size info (for the MRIP-CV estimates) is obtained from the MRIP CPUE file,
# ###     generated from Vivian's legacy scripts (i.e., that reads directly from the i-files ), which was imported
# ###     as 'cpue_trips' at the top of the script. From this file, we then create a table that provides a count of
# ###     all intercepted trips ( 'n.table' = one record for every trip ) and one a count of all positive trips
# ###     ( 'pos.table' = one record for each trip landing the spp of interest ):
# n.table = cpue_trips %>% distinct( id_code, .keep_all = TRUE )
# pos.table = cpue_trips %>% filter( sp_code %in% nodc.code ) %>% distinct( id_code, .keep_all = TRUE )
# 
# 
# ###     Additionally, in some SEDARs, we may also want to consider a different set of species in our general catch table
# ###     ( 'catch.table' ) than that used in CV estimation ( 'cv.table' ). In Gulf & SATL assessments, this might be
# ###     needed when some percentage of unidentified/misidentified catch is to be allocated to the species-of-interest;
# ###     for which we only know some fraction of the raw intercepts were (assumed to be) misidentified to species, but
# ###     we cannot identify the specific records that were misidentified. In Carib assessments, wherein CVs are calculated
# ###     using the 'old' approach, the MRIP-provided variance fields are not additive across species groups. In this,
# ###     only one species should be included in the CV calculation for Carib assessments, even if involving multiple species.
# ###     These species 'adjustments' are already incorporated into RDI, which lets the user choose the appropriate taxa,
# ###     and so no additional steps are needed for Gulf & SATL assessments ( beyond identifying the appropriate 'report.name' ).
# ###     However, for Caribbean assessments, the data for which is not currently available in RDI, I define (below):
# ###            (1) whether the final "cv.table" (which contains CV estimates for catch-in-number) is to include a
# ###                   different set of species than that in "catch.table" (i.e., the official estimates) and...
# ###            (2) if so, which species/taxa are to be included in "cv.table"...
# 
# flag.cv = FALSE
# 
# if( flag.cv ) {
#   
#   sppID.type = 'COMMON'
#   # sppID.type = 'SCIENTIFIC'
#   
#   taxa.cv <- c( "TRIGGERFISH,QUEEN" )
#   
#   nodc.cv <- sapply( taxa.cv, FUN=nodc.code.info, spp.field=sppID.type, spp.table=spp.info )
#   rm( sppID.type )
#   
# } else {
#   
#   nodc.cv <- nodc.code
#   
# }
# 
### -------------------------------------------------------------------------------------------
###
###   -- SEDAR Size File --
###
###     ...which identifies the spreadsheet ( saved to the desktop ) containing CV estimates for SEFSC avgwgts,
###       as needed when calculating CVs for SEFSC landings-in-weight estimates ( AB1 * avgwgt = lbsest_SEC )...

sedar.size.file = "Size/SBS_rec_sizeGEN_8124_20260127_SBR.xlsx"



### -------------------------------------------------------------------------------------------
###
###   -- Directory of SEFSC AvgWgt Estimates --
###
###     ...which identifies the folder/directory containing the latest SEFSC avgwgt estimates.
###       This object will probably not be needed for many SEDARs and so could be commented out (below)
###       in most cases, but it's currently needed if:
###
###           (1) some percentage of 'unidentified catch' is to be allocated to the assessed species,
###               in that (as per a chat with Vivian) the avgwgt estimates for the UNID group need to be
###               substituted with that for the species-of-interest (i.e., we do not want avgwgts calculated
###               from size data that was recorded as an 'unidentified' taxa )
###           (2) some manual adjustment(s) to GenRec catch estimates are being made, in that any
###               landings-in-number estimates (e.g., from MRIP) being substituted (e.g., by state estimates )
###               may need an associated (SEFSC) avgwgt to update the landings-in-weight estimates
###           (3) uncertainties for SEFSC landings-in-weight estimates are requested under Approach #1
###               (see S74-DW-12) (e.g., if additional comparisons b/w Approaches #1 and #2 are requested ).

avgwgt.dir = paste0(dir, '/Catch')
# avgwgt.dir = "U:/_Data/_ACL Files/Size Files/ACL_Sep22"

###     Note that this directory assignment can probably be replaced with a view name once weight estimation
###     has been validated in RDI, after which we can start creating avgwgt-CV reports...

### -------------------------------------------------------------------------------------------

# ### -------------------------------------------------------------------------------------------
# ###
# ###   -- Directory of SEFSC AvgWgt Estimates --
# ###
# ###     ...which identifies the folder/directory containing the latest SEFSC avgwgt estimates.
# ###       This object will probably not be needed for many SEDARs and so could be commented out (below)
# ###       in most cases, but it's currently needed if:
# ###
# ###           (1) some percentage of 'unidentified catch' is to be allocated to the assessed species,
# ###               in that (as per a chat with Vivian) the avgwgt estimates for the UNID group need to be
# ###               substituted with that for the species-of-interest (i.e., we do not want avgwgts calculated
# ###               from size data that was recorded as an 'unidentified' taxa )
# ###           (2) some manual adjustment(s) to GenRec catch estimates are being made, in that any
# ###               landings-in-number estimates (e.g., from MRIP) being substituted (e.g., by state estimates )
# ###               may need an associated (SEFSC) avgwgt to update the landings-in-weight estimates
# ###           (3) uncertainties for SEFSC landings-in-weight estimates are requested under Approach #1
# ###               (see S74-DW-12) (e.g., if additional comparisons b/w Approaches #1 and #2 are requested ).
# 
# avgwgt.dir = "C:/Users/matthew.nuttall/Desktop/ACL AvgWgt Files/2025_02_Feb"
# # avgwgt.dir = "U:/_Data/_ACL Files/Size Files/ACL_Sep22"
# 
# ###     Note that this directory assignment can probably be replaced with a view name once weight estimation
# ###     has been validated in RDI, after which we can start creating avgwgt-CV reports...
# 
# ### -------------------------------------------------------------------------------------------




### ADDITIONAL FLAGS ###
### (old)
### -------------------------------------------------------------------------------------------
###
###   -- TAB for %UNID FISH --
###
###   ...to indicate if, for this particular SEDAR, some fraction of 'unidentified' catch is to be
###       allocated to the species-of-interest. In addition to modifying the catch file
###       (i.e., reducing 'unidentified' catch to the level thought to represent the assessed species),
###       we also provide an additional summary (tab) of the relative breakdown of 'identified' catch
###       (identified at the species level), both of which require the objects below...

#flag.unid = TRUE

#if( flag.unid ) {
  
 # sppID.type = 'COMMON'
  # sppID.type = 'SCIENTIFIC'
  
 # taxa.unid = c("SHARK,SANDBAR", "SHARK,DUSKY", "SHARK,BULL", "SHARK,BLACKNOSE",
  #              "SHARK,BIGNOSE", "SHARK,SILKY", "SHARK,BLACKTIP", "SHARK,OCEANIC WHITETIP")
  ###     ...I also specify the taxa ( in "taxa.unid" ) that represents unidentified catch,
  ###         which will be dropped when estimating potential allocation ratios...
 # taxa.unid.catch = c("SHARK, REQUIEM", "REQUIEM SHARK GENUS", "UNIDENTIFIED SHARKS")
  
 # nodc.unid       = sapply( taxa.unid,       FUN=nodc.code.info, spp.field=sppID.type, spp.table=spp.info )
#  nodc.unid.catch = sapply( taxa.unid.catch, FUN=nodc.code.info, spp.field=sppID.type, spp.table=spp.info )
 # rm( sppID.type )
  
#}

###################
### New (GM)

#props_genus <- annual_props(catch.table.unid.work, pool_requiem_genus,  target_newcom)
#props_family <- annual_props(catch.table.unid.work, pool_requiem_family, target_newcom)




norm_com <- function(x) tolower(trimws(as.character(x)))

annual_props <- function(df, pool, target) {
  
  tmp <- df %>%
    mutate(NEW_COM = norm_com(NEW_COM)) %>%
    filter(NEW_COM %in% pool) %>%
    group_by(YEAR, NEW_COM) %>%
    summarise(AB1 = sum(as.numeric(AB1), na.rm = TRUE),
              B2 = sum(as.numeric(B2), na.rm = TRUE),
              .groups = "drop")
  
  pool_year <- tmp %>%
    group_by(YEAR) %>%
    summarise(pool_AB1 = sum(AB1, na.rm = TRUE),
              pool_B2 = sum(B2, na.rm = TRUE),
              .groups = "drop")
  
  targ_year <- tmp %>%
    filter(NEW_COM == target) %>%
    group_by(YEAR) %>%
    summarise(targ_AB1 = sum(AB1, na.rm = TRUE),
              targ_B2 = sum(B2, na.rm = TRUE),
              .groups = "drop")
  
  full_join(pool_year, targ_year, by = "YEAR") %>%
    mutate(targ_AB1 = coalesce(targ_AB1, 0),
           targ_B2 = coalesce(targ_B2, 0),
           pAB1 = if_else(pool_AB1 > 0, targ_AB1/pool_AB1, 0),
           pB2 = if_else(pool_B2 > 0, targ_B2/pool_B2, 0)) %>%
    select(YEAR, pAB1, pB2, pool_AB1, pool_B2, targ_AB1, targ_B2)
}

allocate_bucket_strata <- function(df, bucket_label, props, target_label) {
  
  has_avgwgt <- "AVGWGT_SEC" %in% names(df)
  has_lbs <- "lbsest_SECwwt" %in% names(df)
  
  df <- df %>% mutate(NEW_COM = norm_com(NEW_COM))
  bucket_label <- norm_com(bucket_label)
  
  if(!"UNID_FLAG" %in% names(df)) {
    df$UNID_FLAG <- NA_character_
  }
  
  bucket_rows <- df %>% filter(NEW_COM == bucket_label)
  if (nrow(bucket_rows) == 0) {
    warning(paste0("No rows found for bucket: ", bucket_label, " (skipping)"))
    return(list(catch = df, added_rows = 0))
  }
  
  bucket_alloc <- bucket_rows %>%
    left_join(props %>% select(YEAR, pAB1, pB2), by = "YEAR") %>%
    mutate(pAB1 = coalesce(pAB1, 0),
           pB2 = coalesce(pB2, 0),
           alloc_AB1 = as.numeric(AB1) * pAB1,
           alloc_B2 = as.numeric(B2) * pB2)
  
  bucket_reduced <- bucket_alloc %>%
    mutate(AB1 = as.numeric(AB1) - alloc_AB1,
           B2 = as.numeric(B2) - alloc_B2) %>%
    select(-pAB1, -pB2)
  
  target_added <- bucket_alloc %>%
    mutate(NEW_COM = target_label,
           AB1 = alloc_AB1,
           B2 = alloc_B2) %>%
    select(-pAB1, -pB2)
  
  if (has_avgwgt) {
    if (!has_lbs) {
      df$lbsest_SECwwt <- NA_real_
      has_lbs <- TRUE
    }
    
    bucket_reduced <- bucket_reduced %>%
      mutate(lbsest_SECwwt = if_else(!is.na(AVGWGT_SEC),
                                     as.numeric(AB1) * as.numeric(AVGWGT_SEC),
                                     as.numeric(lbsest_SECwwt)))
    
    target_added <- target_added %>%
      mutate(lbsest_SECwwt = if_else(!is.na(AVGWGT_SEC),
                                     as.numeric(AB1) * as.numeric(AVGWGT_SEC),
                                     as.numeric(lbsest_SECwwt)))
  }
  
  out <- df %>%
    filter(NEW_COM != bucket_label) %>%
    bind_rows(bucket_reduced) %>%
    bind_rows(target_added)
  
  list(catch = out, added_rows = nrow(target_added))
}

make_unid_table <- function(unid_dat, bucket_name, pool_vec, target_label) {
  
  bucket_name <- norm_com(bucket_name)
  
  unid_catch <- unid_dat %>%
    filter(NEW_COM == bucket_name) %>%
    group_by(YEAR) %>%
    summarise(AB1 = sum(as.numeric(AB1), na.rm = TRUE),
              B2 = sum(as.numeric(B2), na.rm = TRUE),
              .groups = "drop") %>%
    mutate(YEAR = as.character(YEAR))
  
  unid_catch <- bind_rows(data.frame(YEAR = "TOTAL",
                                     AB1 = sum(unid_catch$AB1, na.rm = TRUE),
                                     B2 = sum(unid_catch$B2, na.rm = TRUE)),
                          unid_catch)
  
  id_long <- unid_dat %>%
    filter(NEW_COM %in% pool_vec) %>%
    group_by(NEW_COM, YEAR) %>%
    summarise(AB1 = sum(as.numeric(AB1), na.rm = TRUE),
              B2 = sum(as.numeric(B2), na.rm = TRUE),
              .groups = "drop")
  
  spp_order <- c(target_label, setdiff(unique(id_long$NEW_COM), target_label))
  
  id_tot <- id_long %>%
    group_by(NEW_COM) %>%
    summarise(AB1 = sum(AB1, na.rm = TRUE),
              B2 = sum(B2, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(AB1_total = sum(AB1, na.rm = TRUE),
           B2_total = sum(B2, na.rm = TRUE),
           p.AB1 = if_else(AB1_total > 0, AB1/AB1_total, NA_real_),
           p.B2 = if_else(B2_total > 0, B2/B2_total, NA_real_)) %>%
    select(NEW_COM, AB1, B2, p.AB1, p.B2)
  
  id_tot_w <- id_tot %>%
    mutate(NEW_COM = factor(NEW_COM, levels = spp_order)) %>%
    arrange(NEW_COM) %>%
    pivot_wider(names_from = NEW_COM,
                values_from = c(AB1, B2, p.AB1, p.B2),
                names_glue = "{NEW_COM}_{.value}") %>%
    bind_cols(data.frame(YEAR = "TOTAL"), .)
  
  id_year_w <- id_long %>%
    group_by(YEAR) %>%
    mutate(AB1_total = sum(AB1, na.rm = TRUE),
           B2_total = sum(B2,  na.rm = TRUE),
           p.AB1 = if_else(AB1_total > 0, AB1/AB1_total, NA_real_),
           p.B2 = if_else(B2_total > 0, B2/B2_total, NA_real_)) %>%
    ungroup() %>%
    mutate(NEW_COM = factor(NEW_COM, levels = spp_order)) %>%
    arrange(NEW_COM, YEAR) %>%
    select(NEW_COM, YEAR, AB1, B2, p.AB1, p.B2) %>%
    pivot_wider(names_from = NEW_COM,
                values_from = c(AB1, B2, p.AB1, p.B2),
                names_glue = "{NEW_COM}_{.value}") %>%
    mutate(YEAR = as.character(YEAR)) %>%
    select(YEAR, everything())
  
  ratio_table <- bind_rows(id_tot_w, id_year_w)
  
  full_join(unid_catch, ratio_table, by = "YEAR") %>%
    arrange(YEAR)
}

source(paste0(dir, "/Functions/pull_GenRec_catch.R"))

catch.table <- pull.GenRec.catch(raw.table = dat,
                                 pull.type = "ACL",
                                 spp.filter = nodc.code,
                                 yr.filter = first.year:term.year,
                                 mode.filter = mode_sub,
                                 reg.filter = region,
                                 sta.filter = states,
                                 fl.filter = FL_sub,
                                 nc.filter = NC_sub)

# Unidentified allocation
flag.unid <- TRUE

if (flag.unid) {
  
  target_newcom <- norm_com(new.com[1])
  
  bucket_genus <- "requiem shark genus"
  bucket_family <- "requiem shark family"
  
  pool_requiem_genus <- norm_com(c("sandbar shark","dusky shark","bull shark","blacknose shark",
                                   "bignose shark","silky shark","blacktip shark","ocean whitetip shark",
                                   "reef shark","smalltail shark","spinner shark","finetooth shark","night shark"))
  
  pool_requiem_family <- unique(norm_com(c(pool_requiem_genus,"blue shark","lemon shark","tiger shark","atlantic sharpnose shark")))
  
  spp.info$NEW_COM <- norm_com(spp.info$NEW_COM)
  
  taxa.unid.pool <- unique(c(pool_requiem_family,
                             norm_com(bucket_genus),
                             norm_com(bucket_family)))
  
  nodc.unid.pool <- spp.info %>%
    filter(NEW_COM %in% taxa.unid.pool) %>%
    pull(NODC_CODE) %>%
    as.character() %>%
    unique()
  
  catch.table.unid.work <- pull.GenRec.catch(raw.table = dat,
                                             pull.type = "ACL",
                                             spp.filter = nodc.unid.pool,
                                             yr.filter = first.year:term.year,
                                             mode.filter = mode_sub,
                                             reg.filter = region,
                                             sta.filter = states,
                                             fl.filter = FL_sub,
                                             nc.filter = NC_sub) %>%
    mutate(NEW_COM = norm_com(NEW_COM))
  
  #preallocation of unidentified check
  unid_qc_before <- catch.table.unid.work %>%
    filter(NEW_COM %in% c(target_newcom, bucket_genus, bucket_family)) %>%
    group_by(NEW_COM) %>%
    summarise(AB1 = sum(as.numeric(AB1), na.rm = TRUE),
              B2 = sum(as.numeric(B2), na.rm = TRUE),
              .groups = "drop")
  
  print(unid_qc_before)
  
  #annual proportions based on identified pools
  props_genus <- annual_props(catch.table.unid.work, pool_requiem_genus,  target_newcom)
  props_family <- annual_props(catch.table.unid.work, pool_requiem_family, target_newcom)
  
  #allocate genus
  out1 <- allocate_bucket_strata(catch.table.unid.work, bucket_genus, props_genus, target_newcom)
  catch.table.unid.work <- out1$catch
  
  #allocate family
  out2 <- allocate_bucket_strata(catch.table.unid.work, bucket_family, props_family, target_newcom)
  catch.table.unid.work <- out2$catch
  
  #post allocation check
  unid_qc_after <- catch.table.unid.work %>%
    filter(NEW_COM %in% c(target_newcom, bucket_genus, bucket_family)) %>%
    group_by(NEW_COM) %>%
    summarise(AB1 = sum(as.numeric(AB1), na.rm = TRUE),
              B2 = sum(as.numeric(B2), na.rm = TRUE),
              .groups = "drop")
  
  print(unid_qc_after)
  
  unid_qc_compare <- unid_qc_before %>%
    rename(AB1_before = AB1, B2_before = B2) %>%
    full_join(unid_qc_after %>% rename(AB1_after = AB1, B2_after = B2),
              by = "NEW_COM") %>%
    mutate(AB1_change = AB1_after - AB1_before,
           B2_change = B2_after - B2_before)
  
  print(unid_qc_compare)
  
  write.csv(unid_qc_compare, file = paste0(dir, "/unid_qc_compare.csv", row.names = FALSE))
  
  #after unidentified allocation check
  catch.table.unid <- catch.table.unid.work
  
  catch.table <- catch.table.unid.work %>%
    filter(NEW_COM == target_newcom)
  
  unid.dat <- catch.table.unid.work %>%
    filter(NEW_COM %in% unique(c(pool_requiem_family, norm_com(bucket_genus), norm_com(bucket_family))))
  
  #summary tables
  unid.table.genus <- make_unid_table(unid.dat, bucket_genus,  pool_requiem_genus,  target_newcom)
  unid.table.family <- make_unid_table(unid.dat, bucket_family, pool_requiem_family, target_newcom)
  
  unid.table <- unid.table.family
}

catch.table.unid %>% 
  filter(NEW_COM %in% c("sandbar shark", "requiem shark genus", "requiem shark family")) %>%
  group_by(NEW_COM) %>%
  summarise(AB1 = sum(as.numeric(AB1), na.rm = TRUE),
            B2 = sum(as.numeric(B2), na.rm = TRUE),
            .groups = "drop")

#### End of New (GM)



##* *Evaluate how much unidentified shark catch there is*

unid.shark <- catch.table.unid.work %>%
  filter(NEW_COM == 'requiem shark family')


##* *Summarise AB1 and B2*
unid.shark2 <- unid.shark %>%
  group_by(YEAR) %>%
  summarise(annual.AB1 = sum(AB1, na.rm=TRUE),
            annual.B2  = sum(B2, na.rm=TRUE))



##* *Export and compare with sandbar*


##______________________________________________________________________________
##* *Exporting the unidentified shark catch*

table.ID <- paste0("unid_shark_catch_",
                   #substr(first.year, nchar(first.year)-1, nchar(first.year)),
                   #substr(last.year, nchar(last.year)-1, nchar(last.year)),
                   "_", gsub("-","", Sys.Date()))


wb = createWorkbook()
addWorksheet(wb, sheet='Strata_level')
writeData(wb, sheet='Strata_level', x=unid.shark, colNames=TRUE)
addWorksheet(wb, sheet='Annual')
writeData(wb, sheet='Annual', x=unid.shark2, colNames=TRUE)
addWorksheet(wb, sheet='All_Shark')
writeData(wb, sheet='All_Shark', x=tmp, colNames=TRUE)

saveWorkbook(wb, file=paste0(dir, "/Catch/", table.ID, ".xlsx"), overwrite=TRUE)





### -------------------------------------------------------------------------------------------
###
###   -- CBT/HBT PARTITIONING FOR MATL/NATL --
###
###   ...to indicate if, for this particular SEDAR, any catch & effort estimates for the combined for-hire fleet
###       in the Mid- and North-Atlantic (1981-2003) are to be partitioned between 'Cbt' and 'Hbt' modes.
###
###       Although combined (MRIP) for-hire estimates also exist for the Gulf of Mexico (1981-1985),
###       the SEFSC already (routinely) applies its own method to separate these estimates into separate
###       'Cbt' and 'Hbt' components and so no additional steps are needed (in this script) to modify
###       combined for-hire estimates for the Gulf of Mexico (they're already partitioned).
###
###       Conversely, the only approach available by which (MRIP) for-hire estimates may be partitioned
###       for the  MATL/NATL (amongst modes) was developed as part of SEDAR 82. This approach is effort-based
###       (i.e., not species-specific ), wherein it's applicability to other SEDARs may also be appropriate,
###       although this applicability has yet to be evaluated for any other species (e.g., as a SEDAR 'best practice' ).
###       Regardless, it may be requested and so has been incorporated into the standard GenRec scripts...

flag.forhire = TRUE
###   ...where TRUE represents SEDARs for which combined 'CbtHbt' estimates for the MATL/NATL are to be
###       separated into 'Cbt' and 'Hbt' components (i.e., no combined for-hire estimates provided in this file )...


### -------------------------------------------------------------------------------------------
###
###   -- PARTITIONING OPEN vs. CLOSED FISHING --
###
###   ...to indicate if, for this particular SEDAR, the strata-level catch & effort estimates are to be separated
###     into that originating from an "open" fishing season and that from when fishing was supposed "closed"...

flag.open.closed = FALSE


### -------------------------------------------------------------------------------------------

###   -- MRIP:STATE CALIBRATION FACTORS --
###
###     ...where we may be asked to calibrate between catch estimates in state units vs. those from MRIP
###     (i.e., in MRIP-FES units), for which we have to apply calibration factors (e.g., STATE * CAL = MRIP )
###     that may have been developed as part of the SEDAR process...
###
###   *** NOTE ~ calibrations between MRIP:state survey units are currently only available for Gulf red snapper
###   ***     (developed as part of the MRIP Transition process and first applied in SEDAR 74), but similar
###   ***     calibrations may be developed (in the future) for other species/stocks. Therefore, we created
###   ***     a function to keep track of relevant MRIP:state calibrations, and apply them when applicable...

flag.cal.MRIPstate = FALSE


### -------------------------------------------------------------------------------------------





####################################################################################################################
####################################################################################################################
####################################################################################################################
#############################                                                          #############################
#############################                   GENERATE CATCH TABLE                   #############################
#############################                                                          #############################
####################################################################################################################
####################################################################################################################
####################################################################################################################

source( paste0(dir,'/Functions/pull_GenRec_catch.R') )

con = dbConnect(dbDriver("Oracle"), username = keyring::key_list("SECPR")[1,2],
                password = keyring::key_get("SECPR", keyring::key_list("SECPR")[1,2]), dbname = "SECPR")

catch.table <- pull.GenRec.catch(  raw.table = dat,
                                   pull.type = 'ACL',
                                   # pull.type = 'RDI',
                                   spp.filter = nodc.code, yr.filter = first.year:term.year,  mode.filter = mode_sub,
                                   reg.filter = region,   sta.filter = states,  fl.filter = FL_sub, nc.filter = NC_sub  )

# catch.summary <- catch.table %>%
#   # filter( NEW_STA == 'FLE' & FL_REG == 4 ) %>%
#   # filter( NEW_MODEN == 'Priv' ) %>%
#   group_by( NEW_COM, YEAR, NEW_STA ) %>%
#   summarize( AB1 = sum( as.numeric(AB1), na.rm=TRUE ),
#               B2 = sum( as.numeric( B2), na.rm=TRUE ) ) %>%
#   select( NEW_COM, YEAR, NEW_STA, AB1, B2 ) %>%
#   pivot_wider( names_from=NEW_STA, values_from=c(AB1,B2) )
# View( catch.summary )

# catch.summary <- catch.table %>%
#   # filter( NEW_STA == 'AL', YEAR == 1981, WAVE == 2, NEW_AREAN == 'Ocean>3mi' ) %>%
#   group_by( NEW_COM, YEAR, NEW_MODEN ) %>%
#   summarize( AB1 = sum( as.numeric(AB1), na.rm=TRUE ),
#               B2 = sum( as.numeric( B2), na.rm=TRUE ) ) %>%
#   select( NEW_COM, YEAR, NEW_MODEN, AB1, B2 ) %>%
#   pivot_wider( names_from=NEW_MODEN, values_from=c(AB1,B2) )
# View( catch.summary )



###   Note that, generally speaking, "catch.table" is not the final table as a number of SEDAR-specific modifications
###   are often needed (e.g., imputations of MRIP-wave1, partitioning of UNID catch, manual adjustments). The method
###   by which these modifications are made may be based on previous SEDARs (refer to past SAS scripts) and/or
###   SEDAR best practices ( SEDAR 2015 -- Procedural Workshop #7 -- http://sedarweb.org/pw-07 ), but all of these
###   modifications (and its associated application to our 'catch.table' object) are discussed below...





####################################################################################################################
####################################################################################################################
####################################################################################################################
#############################                                                          #############################
#############################             ASSESSMENT-SPECIFIC MODIFICATIONS            #############################
#############################                                                          #############################
####################################################################################################################
####################################################################################################################
####################################################################################################################



### ---------------------------------------------------------------------------------------------------------------- ###
### ---------------------------------------------------------------------------------------------------------------- ###
# 
# 
# ##########################################
# ######     <MANUAL ADJUSTMENTS>     ######
# ######      to estimates from       ######
# ######      a specific strata       ######
# ##########################################
# ### 
# ###   ...which (hopefully) won't be needed in too many SEDARs, the preference being to integrate adjustments
# ###     into the standard workflow, but manual adjustments ( to 'catch.table' ) may be needed until such
# ###     improvements can be made...
# 
# 
# source( paste0(dir,'/Functions/SECmodify_manualAdjustments.R') )
# 
# 
# ### ---------------------------------------------------
# ### Manual Construction of (Substitute) Catch Table ###
# ### ---------------------------------------------------
# 
# ### SEDAR 74 ###
# ###
# ###     ...which included an investigation into the 1981 for-hire estimates for Alabama, wave2, ocean>3miles:
# ###             -- Charterboat -- (original) catch = 1,374,578 fish
# ###             -- Headboat    -- (original) catch =   859,711 fish
# ###       As noted in the working paper (S74-DW-01), the FHS calibration model was inflating the effort estimate
# ###       for this strata to account for a zero effort estimate in the associated Florida-west cell. As a substitute,
# ###       the FHS-calibration ratios calculated for AL-wave2 in subsequent years (1982-1985) were averaged and
# ###       applied to the 1981 (CHTS) estimates to convert them into FHS units. As new FHS-estimates for this strata:
# ###             -- Charterboat -- catch = 37,936 fish and effort = 1,897 angler trips
# ###             -- Headboat    -- catch = 24,546 fish and effort = 1,186 angler trips
# 
# ### 1981 Alabama For-Hire ###
# dummy.table = data.frame( DS = rep( "MRIP", times=2 ),
#                           NEW_COM = rep( as.character(new.com[1]), times=2 ),
#                           YEAR = c(1981, 1981 ),
#                           WAVE = c(   2,    2 ),
#                           SUB_REG = rep( 7, times=2 ),
#                           NEW_STA = rep( "AL", times=2 ),
#                           NEW_MODEN = c("Cbt","Hbt"),
#                           NEW_AREAN = rep( "Ocean>3mi", times=2 ),
#                           AB1 = c( 37936, 24546 ) )
# 
# ###   ...value-added fields...
# con = dbConnect(dbDriver("Oracle"), username = keyring::key_list("SECPR")[1,2],
#                 password = keyring::key_get("SECPR", keyring::key_list("SECPR")[1,2]), dbname = "SECPR")
# st_tab   = dbGetQuery( con, "SELECT * FROM RDI.MRIP_STATE_CODES@secapxdv_dblk.sfsc.noaa.gov" )
# dummy.table = dummy.table %>% left_join( st_tab %>% select( NEW_ST,NEW_STA ), by='NEW_STA' )
# mode_tab = dbGetQuery( con, "SELECT * FROM RDI.MRIP_MODE_CODES@secapxdv_dblk.sfsc.noaa.gov" )
# dummy.table = dummy.table %>% left_join( mode_tab %>% select( NEW_MODE,NEW_MODEN ), by='NEW_MODEN' )
# rm( st_tab, mode_tab )
# 
# 
# # write.csv( x = dummy.table,
# #            file = paste0( "C:/Users/matthew.nuttall/Desktop/",gsub(' ','',current.sedar),"_manualAdj.csv" ),
# #            na = "", row.names=FALSE )
# # 
# # 
# # ### --------------------------------------
# # ### Import of (Substitute) Catch Table ###
# # ### --------------------------------------
# # 
# # dummy.table = read.csv( file = paste0( "C:/Users/matthew.nuttall/Desktop/SEDAR73U_manualAdj.csv" ), na.strings = NA )
# 
# 
# 
# ### ---------------------------------------------
# ### Apply Function to Substitute Catch Values ###
# ### ---------------------------------------------
# 
# catch.table = substitute.MRIPstate( dummy.table = dummy.table, genrec.table = catch.table, avgwgt.dir = avgwgt.dir )
# rm( dummy.table )
# 
# 
# 
### ---------------------------------------------------------------------------------------------------------------- ###
### ---------------------------------------------------------------------------------------------------------------- ###
########New (GM)
#Partition Combined CBT/HBT catch
source(paste0(dir, "/Functions/SECmodify_allocate_forhire.R"))

if (flag.forhire) {
  
  if (!"WGT_AB1C" %in% names(catch.table)) catch.table$WGT_AB1C <- catch.table$lbsest_SECwwt
  if (!"WGT_AB1H" %in% names(catch.table)) catch.table$WGT_AB1H <- catch.table$lbsest_SECwwt
  if (!"WGT_B2C" %in% names(catch.table)) catch.table$WGT_B2C <- NA_real_
  if (!"WGT_B2H" %in% names(catch.table)) catch.table$WGT_B2H <- NA_real_
  
  if (!"A" %in% names(catch.table)) catch.table$A <- NA_real_
  if (!"B1" %in% names(catch.table)) catch.table$B1 <- NA_real_
  if (!"CHTS_CL" %in% names(catch.table)) catch.table$CHTS_CL <- NA_real_
  if (!"CHTS_H" %in% names(catch.table)) catch.table$CHTS_H <- NA_real_
  if (!"CHTS_RL" %in% names(catch.table)) catch.table$CHTS_RL <- NA_real_
  
  if (!"VAR_AB1" %in% names(catch.table)) catch.table$VAR_AB1 <- NA_real_
  if (!"VAR_B2" %in% names(catch.table)) catch.table$VAR_B2 <- NA_real_
  if (!"CHTS_VAR_CL" %in% names(catch.table)) catch.table$CHTS_VAR_CL <- NA_real_
  if (!"CHTS_VAR_H" %in% names(catch.table)) catch.table$CHTS_VAR_H <- NA_real_
  if (!"CHTS_VAR_RL" %in% names(catch.table)) catch.table$CHTS_VAR_RL <- NA_real_
  
  catch.table.forhire <- catch.table %>%
    filter(DS == "MRIP",
           SUB_REG %in% 4:5,
           NEW_MODEN %in% c("Cbt/Hbt"),
           YEAR %in% 1981:2003)
  
  catch.table <- partition.forhire(genrec.table = catch.table)
  
}
  
##  catch.table.forhire = catch.table %>%
  #  filter(DS == "MRIP" & SUB_REG %in% 4:5 & NEW_MODEN %in% c("Cbt/Hbt") & YEAR %in% 1981:2003)
  
  #catch.table = partition.forhire(genrec.table = catch.table)

# MRIP 1981 Wave 1 imputation

#Check if object exists
if (!exists("method.MRIP.1981w1")) {
  method.MRIP.1981 <- "None"
}

if (any(c("TX","LA","MS","AL","FLW","FLE") %in% states) & 1981 %in% first.year:term.year) {
  
  source(paste0(dir, "/Functions/SECmodify_impute_MRIP1981w1.R"))
  
  dummy <- summary.MRIP.1981w1(genrec.table = catch.table)
  
  dummy$WaveCatch
  dummy$fracs.byMETRIC
  dummy$methods.RAWvIMP
  dummy$methods.TOTAL
  
  wave_check <- dummy$WaveCatch
  
  names(wave_check) <- toupper(names(wave_check))
  
  print(wave_check)
  
  if (all(c("YEAR", "WAVE", "AB1", "B2") %in% names(wave_check))) {
    wave_check2 <- wave_check %>%
      mutate(YEAR = as.integer(YEAR),
             WAVE = as.character(WAVE))
    
    w1_share <- wave_check2 %>%
      filter(YEAR %in% 1982:1984) %>%
      mutate(wave_group = ifelse(WAVE %in% c("1", "W1", "wave1", "WAVE1"), "wave1", "other")) %>%
      group_by(wave_group) %>%
      summarise(AB1 = sum(as.numeric(AB1), na.rm = TRUE),
                B2 = sum(as.numeric(B2), na.rm = TRUE),
                .groups = "drop")
    
    total_AB1 <- sum(w1_share$AB1, na.rm = TRUE)
    total_B2 <- sum(w1_share$B2, na.rm = TRUE)
    
    wave1_AB1_pct <- ifelse(total_AB1 > 0, 100 * w1_shares$AB1[w1_share$wave_group == "wave1"]/total_AB1, NA)
    wave1_B2_pct <- ifelse(total_B2 > 0, 100 * w1_share$B2[w1_share$wave_group == "wave1"]/total_B2, NA)
    
    cat("\nWave 1 contribution during 1982-1984:\n")
    cat("AB1 % from wave 1 =", round(wave1_AB1_pct, 2), "\n")
    cat("B2 % from wave 1 =", round(wave1_B2_pct, 2), "\n")
    
  } else {
    cat("\nCould not compute WaveCatch percentages automatically becase expected columns were not found.\n")
    cat("Run names (dummy$WaveCatch) and inspect manually.\n")
  }
  
  method.MRIP.1981w1 <- "prop_w1_w26"
  cat("\nInitial recommended method for review:", method.MRIP.1981w1, "\n")
  cat("Change to 'avg_82_84' if the 1982-1984 rations look unstable. \n")
  cat("Change to 'Nonne' if wave1 is negligible and imputation has little effect.\n")
  
  if (method.MRIP.1981w1 != "None") {
    catch.table <- impute.MRIP.1981w1(genrec.table = catch.table,
                                      method = method.MRIP.1981w1)
  }
  
  rm(dummy)
}

#LACR discard imputation


if ("LA" %in% states) {
  
  source(paste0(dir, "/Functions/SECmodify_impute_LACRdiscards.R"))
  
  dummy <- summary.LACR.B2(genrec.table = catch.table)
  
  dummy$StateCatch
  
  View(dummy$LACatch)
  blah = dummy$LACatch %>% select(YEAR, AB1, B2) %>% pivot_longer(!YEAR)
  dummy.plot = ggplot(data = blah, aes(x = YEAR, y = value)) + geom_point() + geom_line() +
    geom_vline(xintercept = 2014) + ylim(0, NA) + facet_grid(name ~ ., scales = "free")
  dummy.plot
  rm(blah, dummy.plot)
  
  View(dummy$fracs)
  dummy$fracs.fig
  blah = dummy$fracs %>% select(YEAR, Cbt_LA, Cbt_GULF, Priv_LA, Priv_GULF) %>%
    pivot_longer(!YEAR) %>% mutate(MODE = gsub("_.*","",name), DATA = gsub(".*_","",name))
  dummy.plot = ggplot(data = blah, aes(x = YEAR, y = value, colour = MODE)) + geom_point() + geom_line() +
    geom_vline(xintercept = 2014) + ylim(0, NA) + facet_grid(DATA ~ ., scales = "free")
  dummy.plot
  rm(blah, dummy.plot)
  
  method.LACR.B2 = "None"
  
  if (method.LACR.B2 != "None") {
    catch.table = impute.LACR.B2(genrec.table = catch.table,
                                 method = method.LACR.B2,
                                 ratio.years = ratio.yrs.LACR.B2)
  }
  
  rm(dummy)
}

#TPWD 1981-MAY 1983 imputation

if (!exists("method.TPWD.1981.83")) {
  method.TPWD.1981.83 <- "None"
}

method.TPWD.1981.83 <- "None"
method.tpwd.1981.83 <- method.TPWD.1981.83

if ("TX" %in% states) {
  
  source(paste0(dir, "/Functions/SECmodify_impute_TPWD1981_83w2.R"))
  
  dummy <- summary.TPWD.1981.1983(genrec.table = catch.table)
  
  dummy$StateCatch
  View(dummy$TXCatch)
  dummy.plot = ggplot(data = dummy$TXCatch, aes(x = YEAR, y = AB1)) + geom_point() + geom_line()
  dummy.plot
  rm(dummy.plot)
  
  dummy$State.fig
  
  method.TPWD.1981.93 = 'None'
  
  if (method.TPWD.1981.83 != "None") {
    catch.table = impute.TPWD.1981.1983(genrec.table = catch.table, method = method.TPWD.1981.83)
  }
  
  rm(dummy)
}

#TPWD discard imputation

#dummy$fracs %>%
 # summarise(Priv_GULF_mean = mean(Priv_GULF, na.rm = TRUE),
  #          Priv_GULF_sd = sd(Priv_GULF, na.rm = TRUE),
   #         Priv_Gulf_cv = sd(Priv_GULF, na.rm = TRUE)/mean(Priv_GULF, na.rm = TRUE),
    #        Cbt_GULF_mean = mean(Cbt_GULF, na.rm = TRUE),
     #       Cbt_GULF_sd = sd(Cbt_GULF, na.rm = TRUE),
      #      Cbt_GULF_cv = sd(Cbt_GULF, na.rm = TRUE)/mean(Cbt_GULF, na.rm = TRUE))

#dummy$fracs %>%
 # summarise(n_Priv_Gulf = sum(!is.na(Priv_GULF)),
  #          n_Cbt_Gulf = sum(!is.na(Cbt_GULF)))

#catch.table %>% filter(DS == "TPWD") %>%
 # summarise(AB1 = sum(AB1, na.rm = TRUE),
  #          B2 = sum(B2, na.rm = TRUE))

if ("TX" %in% states) {
  
  source(paste0(dir, "/Functions/SECmodify_impute_TPWDdiscards.R"))
  
  dummy <- summary.TPWD.B2(genrec.table = catch.table)
  
  dummy$StateCatch
  dummy$State.fig
  
  View(dummy$fracs)
  dummy$fracs.fig
  blah = dummy$fracs %>% select(YEAR, Cbt_LA, Cbt_GULF, Priv_LA, Priv_GULF) %>%
    pivot_longer(!YEAR) %>% mutate(MODE = gsub("_.*","",name), DATA = gsub(".*_","",name))
  dummy.plot = ggplot(data = blah, aes(x = YEAR, y = value, colour = DATA)) + geom_point() + geom_line() +
    ylim(0, NA) + facet_grid(MODE ~ ., scales = "free")
  dummy.plot
  rm(blah, dummy.plot)
  
  method.TPWD.B2 <- "gu_ratio"
  
  if (method.TPWD.B2 != "None") {
    catch.table = impute.TPWD.B2(genrec.table = catch.table, method = method.TPWD.B2)
  }
  
  rm(dummy)
}

#Assign stock ID
source(paste0(dir, "/Functions/assign_stockID.R"))

catch.table <- assign.stockID(new.com = new.com, region = region, genrec.table = catch.table)

if (flag.unid) {
  catch.table.unid <- assign.stockID(new.com = new.com, region = region, genrec.table = catch.table.unid)
}
if (flag.forhire) {
  catch.table.forhire <- assign.stockID(new.com = new.com, region = region, genrec.table = catch.table.forhire)
}

if (new.com == "king mackerel") {
  if (region == "Gulf of America") {
    catch.table = catch.table %>% filter(SID == "GULF")
    if (flag.unid) catch.table.unid <- catch.table.unid %>% filter(SID == "GULF")
    if (flag.forhire) catch.table.forhire <- catch.table.forhire %>% filter(SID == "GULF")
  }
  if (region == "South Atlantic") {
    catch.table = catch.table %>% filter(SID == "ATL")
    if (flag.unid) catch.table.unid <- catch.table.unid %>% filter(SID == "ATL")
    if (flag.forhire) catch.table.forhire <- catch.table.forhire %>% filter(SID == "ATL")
  }
}

#Assign fishing season
source(paste0(dir, "/Functions/assign_FishingSeason.R"))

if (flag.open.closed) {
  
  dummy <- partition.fishing.season(new.com = new.com, region = region, genrec.table = catch.table,
                                    method.LACR.B2 = method.LACR.B2, ratio.yrs.LACR.B2 = ratio.yrs.LACR.B2,
                                    method.TPWD.B2 = method.TPWD.B2)
  
  catch.table = dummy$catch.table
  open.closed_ratios = dummy$part.factors
  rm(dummy)
}

#Assign fishing year
source(paste0(dir, "/Functions/assign_FishingYear.R"))
catch.table <- assign.fyear(new.com = new.com, region = region, genrec.table = catch.table)

#Calibrating state and MRIP estimates
source(paste0(dir, "/Functions/SECmodify_cal_MRIPstate.R"))

if (flag.cal.MRIPstate) {
  
  cal.factors = extract.cal.ratio(new.com = new.com, region = region)
  
  for (i in 1:length(cal.factors)) {
    
    dummy.ratio = cal.factors[[i]][which(grepl("lacr", names(cal.factors[[i]])) &
                                           !grepl(".EFF", names(cal.factors[[i]])))]
    if (length(dummy.ratio) > 0) {
      if (toupper(names(cal.factors)[i]) == "PRIV") {
        dummy.mode = c(str_to_title(names(cal.factors)[i]), "Priv/Shore")
      } else {
        dummy.mode = str_to_title(names(cal.factors)[i])
      }
      catch.table = calibrate.MRIPstate(DS.filter = "LA Creel", mode.filter = dummy.mode,
                                        cal.ratios = dummy.ratio, genrec.table = catch.table)
      rm(dummy.mode)
    }
    rm(dummy.ratio)
    
    dummy.ratio = cal.factors[[i]][which(grepl("tpwd", names(cal.factors[[i]])) &
                                           !grepl(".EFF", names(cal.factors[[i]])))]
    dummy.mode = str_to_title(names(cal.factors)[i])
    catch.table = calibrate.MRIPstate(DS.filter = "TPWD", mode.filter = dummy.mode,
                                      cal.ratios = dummy.ratio, genrec.table = catch.table)
    rm(dummy.ratio, dummy.mode)
  }
}

#Compare to previous SEDARs
if (prev.sedar != "None") {
  
  sedar.comparison <- catch.table %>%
    group_by(YEAR) %>%
    summarize(AB1 = sum(as.numeric(AB1), na.rm = TRUE),
              B2 = sum(as.numeric(B2), na.rm = TRUE)) %>%
    select(YEAR, AB1, B2)
  
  assess.years <- first.year:term.year
  
  for (i in 1:length(assess.years)) {
    if (assess.years[i] %notin% sedar.comparison$YEAR) {
      sedar.comparison <- rbind(data.frame(sedar.comparison),
                                data.frame(YEAR = assess.years[i], AB1 = 0, B2 = 0))
    }
  }
  rm(assess.years)
  
  sedar.comparison <- sedar.comparison %>%
    arrange(YEAR) %>%
    mutate_at(c("AB1","B2"), round, digits = 0)
  
  sedar.comparison <- sedar.comparison %>%
    add_column(old_sedar = sedar.comparison$YEAR, .after = 3)
  
  colnames(sedar.comparison)[which(colnames(sedar.comparison) == "old_sedar")] <- prev.sedar
  colnames(sedar.comparison)[which(colnames(sedar.comparison) == "YEAR")] <- current.sedar
}

#MRIP CVS (catch in number)
source(paste0(dir, "/Functions/calc_CVs_catnum.R"))

#MRIP state calibration flag
if(!exists("flag.cal.MRIPstate")) {
  flag.cal.MRIPstate <- FALSE
}

#Make sure cal.factors always exists
if(!exists("cal.factors")) {
  cal.factors <- list()
}

if(flag.cal.MRIPstate) {
  cal.factors <- extract.cat.ratio(new.com = new.com, region = region)
}

if (region == "Caribbean") {
  
  if (flag.cv) {
    cv.dat  = dat.filter(acl.table = dat, spp.filter = nodc.cv,
                         yr.filter = first.year:term.year, mode.filter = mode_sub,
                         reg.filter = region, sta.filter = states, fl.filter = FL_sub, nc.filter = NC_sub)
  } else {
    cv.dat = catch.table
  }
  
  cv.table = CVs.catnum.MRIP(Carib.SEDAR = TRUE, genrec.table = cv.dat, total.trips = n.table, pos.trips = pos.table)
  rm(cv.dat)
  
} else {
  
  if ((("Cbt" %in% mode_sub) | ("Hbt" %in% mode_sub)) &
      any(c("VA","MD","DE","PA","NJ","NY","CT","RI","MA","NH","ME") %in% states)) {
    if ("Cbt" %in% mode_sub) {
      cv.modes = append(mode_sub, "Cbt_Hbt", after = match("Cbt", mode_sub))
    } else {
      cv.modes = append(mode_sub, "Cbt_Hbt", after = match("Hbt", mode_sub) - 1)
    }
  } else {
    cv.modes = mode_sub
  }
  
  if (!exists("method.MRIP.1981w1")) method.MRIP.1981w1 = "None"
  
  mrip.dummy = CVs.catnum.MRIP(rdi.report = report.name,
                               report.type = report.type,
                               inc.modes = cv.modes, genrec.table = catch.table,
                               imp.1981w1 = method.MRIP.1981w1,
                               flag.unid = flag.unid, catch.table.unid = catch.table.unid,
                               flag.forhire = flag.forhire, catch.table.forhire = catch.table.forhire,
                               loc.cv.forhire = NA,
                               loc.FH.ratios = "C://Users/gaitlyn.malone/Documents/SEDAR/SEDAR101/Functions/import_datasets/ForHire Partitioning Ratios.csv")
  
  if (length(report.name) > 1) {
    mrip.dummy = lapply(cv.table = mrip.dummy, FUN = convert.long.table.cat, survey = "MRIP", report.type = report.type)
  } else {
    mrip.dummy = convert.long.table.cat(cv.table = mrip.dummy, survey = "MRIP", report.type = report.type)
  }
  
  if (length(report.name) > 1) {
    
    dummy = catch.table %>%
      mutate(FL_REG = factor(FL_REG, levels = c(1,2,3,4,5,NA))) %>%
      mutate(NC_REG = factor(NC_REG, levels = c("S","N",NA))) %>%
      arrange(NEW_ST, FL_REG, NC_REG)
    
    SID.levels = unique(dummy$SID)
    rm(dummy)
    
    names(mrip.dummy) = SID.levels
    
    for (i in 1:length(mrip.dummy)) {
      mrip.dummy[[i]] = mrip.dummy[[i]] %>%
        mutate(SID = names(mrip.dummy)[i]) %>%
        mutate(SID = factor(SID, levels = SID.levels)) %>%
        select(SID, YEAR, NEW_MODEN, CATCH_VAR, METRIC, value)
    }
    rm(i)
  }
  
  if (flag.open.closed) {
    source(paste0(dir, "/Functions/assign_FishingSeason.R"))
    mrip.dummy = cv.fishing.season(cv.table = mrip.dummy, genrec.table = catch.table, DS.filter = "MRIP")
  }
  
  if ("LA" %in% states) {
    
    if (method.LACR.B2 == "la_ratio") mrip.cv.table = LA.report.name
    if (method.LACR.B2 == "gu_ratio") mrip.cv.table = GOM.report.name
    
    if (flag.cal.MRIPstate &
        length(which(grepl("lacr", as.vector(unlist(lapply(cal.factors, names)))))) > 0) {
      
      for (i in 1:length(cal.factors)) {
        
        dummy.ratio = cal.factors[[i]][which(grepl("lacr", names(cal.factors[[i]])) &
                                               !grepl(".EFF", names(cal.factors[[i]])))]
        dummy.mode = names(cal.factors)[i]
        dummy.mode = c(dummy.mode, str_to_title(dummy.mode))
        if ("Priv" %in% dummy.mode) {
          dummy.mode = c(dummy.mode, "PRIV/SHORE", "Priv/Shore")
        }
        
        lacr.dummy = CVs.catnum.LACR(report.type = report.type,
                                     itis.code = itis.code, inc.modes = cv.modes, inc.years = 2014:term.year,
                                     cal.ratios = dummy.ratio, mode.filter = dummy.mode,
                                     imp.LACR.B2 = method.LACR.B2, calc.ratios.from = ratio.yrs.LACR.B2,
                                     mrip.cv.table = mrip.cv.table,
                                     attach.samplesize = TRUE)
        rm(dummy.ratio, dummy.mode)
      }
      rm(i)
      
    } else {
      lacr.dummy = CVs.catnum.LACR(report.type = report.type,
                                   itis.code = itis.code, inc.modes = cv.modes, inc.years = 2014:term.year,
                                   imp.LACR.B2 = method.LACR.B2, calc.ratios.from = ratio.yrs.LACR.B2,
                                   mrip.cv.table = mrip.cv.table)
    }
    
    if (method.LACR.B2 != "None") rm(mrip.cv.table)
    
    lacr.cv.tpwdB2 = lacr.dummy[[2]]
    lacr.dummy = lacr.dummy[[1]]
    
    if ("NEW_STA" %notin% colnames(lacr.dummy)) {
      lacr.dummy = lacr.dummy %>% mutate(NEW_STA = "LA")
    }
    
    lacr.dummy = convert.long.table.cat(lacr.dummy, report.type = report.type, survey = "LACR")
    
    if (length(report.name) > 1) {
      
      for (j in 1:length(report.name)) {
        
        con = dbConnect(dbDriver("Oracle"),
                        username = keyring::key_list("SECPR")[1,2],
                        password = keyring::key_get("SECPR", keyring::key_list("SECPR")[1,2]),
                        dbname = "SECPR")
        
        mrip.state = dbGetQuery(con,
                                paste0("select * ",
                                       "from rdi.apex_cv_data_yr_s@secapxdv_dblk.sfsc.noaa.gov t
                                        where t.APP_USER = ", sprintf("'%s'", paste(report.name[j], collapse = "','"))))
        mrip.state = mrip.state %>% select(-c("APP_USER","YEAR"))
        mrip.state = unique(gsub("_.*","", colnames(mrip.state)[colSums(mrip.state, na.rm = TRUE) > 0]))
        
        if ("LA" %in% mrip.state) {
          loc.cv.LACR = j
          break
        }
        rm(mrip.state)
      }
      rm(j)
      
      lacr.dummy = lacr.dummy %>%
        mutate(SID = names(mrip.dummy)[loc.cv.LACR]) %>%
        mutate(SID = factor(SID, levels = SID.levels)) %>%
        select(any_of(c("SID","YEAR","NEW_MODEN","CATCH_VAR","METRIC","value")))
      rm(loc.cv.LACR)
    }
    
    if (flag.open.closed) {
      source(paste0(dir, "/Functions/assign_FishingSeason.R"))
      lacr.dummy = cv.fishing.season(cv.table = lacr.dummy, genrec.table = catch.table, DS.filter = "LA Creel")
    }
  }
  
  if ("TX" %in% states) {
    
    if (method.TPWD.B2 == "la_ratio") genrec.cv.table = list(LA.report.name, lacr.cv.tpwdB2)
    if (method.TPWD.B2 == "gu_ratio") genrec.cv.table = list(GOM.report.name, lacr.cv.tpwdB2)
    
    if (flag.cal.MRIPstate &
        length(which(grepl("tpwd", as.vector(unlist(lapply(cal.factors, names)))))) > 0) {
      
      for (i in 1:length(cal.factors)) {
        dummy.ratio = cal.factors[[i]][which(grepl("tpwd", names(cal.factors[[i]])) &
                                               !grepl(".EFF", names(cal.factors[[i]])))]
        dummy.mode = names(cal.factors)[i]
        dummy.mode = c(dummy.mode, str_to_title(dummy.mode))
        tpwd.dummy = CVs.catnum.TPWD(report.type = report.type,
                                     tpwd.code = tpwd.code, inc.modes = cv.modes, inc.years = 1983:term.year,
                                     cal.ratios = dummy.ratio, mode.filter = dummy.mode,
                                     genrec.cat.table = catch.table,
                                     imp.TPWD.8183 = method.TPWD.1981.83,
                                     imp.TPWD.B2 = method.TPWD.B2, genrec.cv.table = genrec.cv.table,
                                     attach.samplesize = TRUE)
        rm(dummy.ratio, dummy.mode)
      }
      rm(i)
      
    } else {
      tpwd.dummy = CVs.catnum.TPWD(report.type = report.type,
                                   tpwd.code = tpwd.code, inc.modes = cv.modes, inc.years = 1983:term.year,
                                   genrec.cat.table = catch.table,
                                   imp.TPWD.8183 = method.TPWD.1981.83,
                                   imp.TPWD.B2 = method.TPWD.B2, genrec.cv.table = genrec.cv.table)
    }
    
    rm(lacr.cv.tpwdB2)
    if (method.TPWD.B2 != "None") rm(genrec.cv.table)
    
    if ("NEW_STA" %notin% colnames(tpwd.dummy)) {
      tpwd.dummy = tpwd.dummy %>% mutate(NEW_STA = "TX")
    }
    
    tpwd.dummy = convert.long.table.cat(tpwd.dummy, report.type = report.type, survey = "TPWD")
    
    if (length(report.name) > 1) {
      
      SID.TPWD = unique(catch.table$SID[catch.table$NEW_STA == "TX"])
      loc.cv.TPWD = which(names(mrip.dummy) == SID.TPWD)
      rm(SID.TPWD)
      
      tpwd.dummy = tpwd.dummy %>%
        mutate(SID = names(mrip.dummy)[loc.cv.TPWD]) %>%
        mutate(SID = factor(SID, levels = SID.levels)) %>%
        select(SID, YEAR, NEW_MODEN, CATCH_VAR, METRIC, value)
      rm(loc.cv.TPWD)
    }
    
    if (flag.open.closed) {
      source(paste0(dir, "/Functions/assign_FishingSeason.R"))
      tpwd.dummy = cv.fishing.season(cv.table = tpwd.dummy, genrec.table = catch.table, DS.filter = "TPWD")
    }
  }
  
  cv.table = mrip.dummy
  
  if ("LA" %in% states) {
    
    if ("SID" %in% colnames(catch.table)) {
      loc.cv.LACR = which(names(mrip.dummy) == unique(lacr.dummy$SID))
      blah = cv.table[[loc.cv.LACR]]
    } else {
      blah = cv.table
    }
    
    dummy = bind_rows(blah, lacr.dummy) %>%
      group_by(across(any_of(c("SID","YEAR","NEW_MODEN","fed_closed","CATCH_VAR","METRIC")))) %>%
      summarise(value = sum(value, na.rm = TRUE))
    rm(blah)
    
    if ("SID" %in% colnames(catch.table)) {
      cv.table[[loc.cv.LACR]] = dummy
      rm(dummy, loc.cv.LACR)
    } else {
      cv.table = dummy
      rm(dummy)
    }
  }
  
  if ("TX" %in% states) {
    
    if ("SID" %in% colnames(catch.table)) {
      loc.cv.TPWD = which(names(mrip.dummy) == unique(tpwd.dummy$SID))
      blah = cv.table[[loc.cv.TPWD]]
    } else {
      blah = cv.table
    }
    
    dummy = bind_rows(blah, tpwd.dummy) %>%
      group_by(across(any_of(c("SID","YEAR","NEW_MODEN","fed_closed","CATCH_VAR","METRIC")))) %>%
      summarise(value = sum(value, na.rm = TRUE))
    rm(blah)
    
    if ("SID" %in% colnames(catch.table)) {
      cv.table[[loc.cv.TPWD]] = dummy
      rm(dummy, loc.cv.TPWD)
    } else {
      cv.table = dummy
      rm(dummy)
    }
  }
  
  if ("SID" %in% colnames(catch.table)) {
    for (i in 1:length(cv.table)) {
      
      blah = cv.table[[i]] %>%
        group_by(NEW_MODEN) %>%
        summarize(PSU = sum(value[CATCH_VAR == "TOTAL" & METRIC == "PSU"], na.rm = TRUE),
                  AT = sum(value[CATCH_VAR == "TOTAL" & METRIC == "AT"],  na.rm = TRUE))
      null.modes = blah$NEW_MODEN[rowSums(blah[, c("PSU","AT")]) == 0]
      rm(blah)
      
      cv.table[[i]] = cv.table[[i]] %>% filter(NEW_MODEN %notin% null.modes)
      rm(null.modes)
    }
    rm(i)
  } else {
    
    blah = cv.table %>%
      group_by(NEW_MODEN) %>%
      summarize(PSU = sum(value[CATCH_VAR == "TOTAL" & METRIC == "PSU"], na.rm = TRUE),
                AT = sum(value[CATCH_VAR == "TOTAL" & METRIC == "AT"], na.rm = TRUE))
    null.modes = blah$NEW_MODEN[rowSums(blah[, c("PSU","AT")]) == 0]
    rm(blah)
    
    cv.table = cv.table %>% filter(NEW_MODEN %notin% null.modes)
    rm(null.modes)
  }
  
  if (length(report.name) > 1) cv.table = do.call(rbind, cv.table)
  
  cv.table = cv.table %>%
    mutate(METRIC = factor(METRIC, levels = c("CAT","VAR","AT","PSU"))) %>%
    mutate(NEW_MODE = ifelse(NEW_MODEN == "SHORE", 1,
                             ifelse(NEW_MODEN == "HBT", 2,
                                    ifelse(NEW_MODEN == "CBT", 3,
                                           ifelse(NEW_MODEN == "PRIV", 4,
                                                  ifelse(NEW_MODEN == "CBTHBT", 5,
                                                         ifelse(NEW_MODEN == "PRIVSHORE", 6,
                                                                ifelse(NEW_MODEN == "TOTAL", 99, NA))))))))
  
  dummy = list(YEAR = seq(first.year, term.year, by = 1))
  if ("SID" %in% colnames(catch.table)) dummy$SID = unique(catch.table$SID)
  if (flag.open.closed) dummy$fed_closed = unique(catch.table$fed_closed)
  dummy = expand.grid(dummy)
  
  cv.table = cv.table %>%
    full_join(dummy, by = colnames(dummy)) %>%
    arrange(across(colnames(dummy)))
  rm(dummy)
  
  cv.table = cv.table %>%
    select(any_of(c("SID","YEAR","fed_closed","NEW_MODE","NEW_MODEN","NEW_STA","WAVE",
                    "CATCH_VAR","METRIC","value")))
  
  groupby.cols = colnames(cv.table)[which(colnames(cv.table) %in%
                                            c("SID","YEAR","fed_closed","NEW_MODE","NEW_MODEN","NEW_STA","WAVE"))]
  
  blah1 = catch.table %>%
    mutate(NEW_MODEN = toupper(NEW_MODEN)) %>%
    mutate(NEW_MODE = ifelse(NEW_MODE == 6, 4, NEW_MODE),
           NEW_MODEN = ifelse(NEW_MODEN == "PRIV/SHORE", "PRIV", NEW_MODEN)) %>%
    group_by(across(all_of(groupby.cols))) %>%
    summarize(AB1 = sum(AB1, na.rm = TRUE),
              B2 = sum(B2, na.rm = TRUE)) %>%
    ungroup()
  
  blah2 = catch.table %>%
    group_by(across(all_of(groupby.cols[groupby.cols %notin% c("NEW_MODE","NEW_MODEN")]))) %>%
    summarize(AB1 = sum(AB1, na.rm = TRUE),
              B2 = sum(B2, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(NEW_MODE = 99,
           NEW_MODEN = "TOTAL")
  
  blah = bind_rows(blah1, blah2)
  rm(blah1, blah2)
  
  blah = blah %>%
    pivot_longer(cols = c("AB1","B2"), names_to = "CATCH_VAR", values_to = "value") %>%
    mutate(METRIC = "CAT")
  
  groupby.cols = c(groupby.cols, "CATCH_VAR", "METRIC")
  dummy = cv.table %>% left_join(blah, by = groupby.cols)
  rm(groupby.cols, blah)
  
  cv.table = dummy %>%
    mutate(value.x = ifelse(is.na(value.y), value.x, value.y)) %>%
    rename(value = value.x) %>%
    select(-value.y)
  rm(dummy)
}

#Landings in weight
source(paste0(dir, "/Functions/calc_CVs_catwgt.R"))

num.table = cv.table
wgt.table = read_excel(path=paste0("C:/Users/gaitlyn.malone/Documents/SEDAR/SEDAR101/",sedar.size.file),
  sheet = "Weight Summary by Mode", trim_ws = FALSE, col_types = "text")

wgt.table = wgt.table %>% filter(NEW_MODEN %in% unique(num.table$NEW_MODEN))

lbs.cv.table.2 = CVs.landwgt(approach = 2, catch.table = catch.table, num.table = num.table, wgt.table = wgt.table)
rm(num.table, wgt.table)

groupby.cols = colnames(cv.table)[which(colnames(cv.table) %in%
                                          c("SID","YEAR","fed_closed","NEW_MODE","NEW_MODEN","NEW_STA","WAVE"))]

blah1 = catch.table %>%
  mutate(NEW_MODEN = toupper(NEW_MODEN)) %>%
  mutate(NEW_MODE = ifelse(NEW_MODE == 6, 4, NEW_MODE),
         NEW_MODEN = ifelse(NEW_MODEN == "PRIV/SHORE", "PRIV", NEW_MODEN)) %>%
  group_by(across(all_of(groupby.cols))) %>%
  summarize(LBS = sum(lbsest_SECwwt, na.rm = TRUE)) %>%
  ungroup() %>%
  rename_all(~toupper(.))

blah2 = catch.table %>%
  group_by(across(all_of(groupby.cols[groupby.cols %notin% c("NEW_MODE","NEW_MODEN")]))) %>%
  summarize(LBS = sum(lbsest_SECwwt, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(NEW_MODE = 99,
         NEW_MODEN = "TOTAL") %>%
  rename_all(~toupper(.))

blah = bind_rows(blah1, blah2)
rm(blah1, blah2)

dummy = lbs.cv.table.2 %>% left_join(blah, by = groupby.cols)
rm(groupby.cols, blah)

lbs.cv.table.2 = dummy %>%
  mutate(LBS_CAT = ifelse(is.na(LBS), LBS_CAT, LBS)) %>%
  select(-LBS)
rm(dummy)

#Create fina Excel workbook
table.ID <- paste0("SBS_rec_catGEN_",
                   substr(first.year, nchar(first.year)-1, nchar(first.year)),
                   substr(term.year,  nchar(term.year)-1,  nchar(term.year)),
                   "_", gsub("-", "", Sys.Date()))

if (flag.unid) tab.unid.dat <- paste("CARCHARHINIDAE", "_rec_catGEN")

wb <- loadWorkbook(file = paste0(dir, "/Template_SEDAR_GenCatch_fromACL_v2.xlsx"))

removeWorksheet(wb, sheet = "SNWY_REC_CATGEN_8118_20200210")
addWorksheet(wb, sheet = table.ID)
writeData(wb, sheet = table.ID, x = catch.table, colNames = TRUE)

if (prev.sedar != "None") {
  writeData(wb, sheet = "Compare Previous SEDARs", x = sedar.comparison, colNames = TRUE)
} else {
  removeWorksheet(wb, sheet = "Compare Previous SEDARs")
}

cv.dummy = cv.table %>%
  ungroup() %>%
  mutate(METRIC = as.character(METRIC)) %>%
  mutate(VARIABLE = paste0(CATCH_VAR, "_", METRIC)) %>%
  mutate(VARIABLE = ifelse(VARIABLE == "TOTAL_AT",  "ATtotal",
                           ifelse(VARIABLE == "TOTAL_PSU", "PSUtotal", VARIABLE)))

blah.AB1 = cv.dummy %>%
  filter(VARIABLE %in% c("ATtotal","PSUtotal")) %>%
  mutate(CATCH_VAR = "AB1") %>%
  mutate(METRIC = VARIABLE)

blah.B2 = blah.AB1 %>% mutate(CATCH_VAR = "B2")

cv.dummy = cv.dummy %>%
  filter(!grepl("ATtotal", VARIABLE) & !grepl("PSUtotal", VARIABLE)) %>%
  bind_rows(blah.AB1) %>%
  bind_rows(blah.B2) %>%
  select(-VARIABLE) %>%
  pivot_wider(names_from = METRIC, values_from = value) %>%
  mutate(CV = ifelse(CAT == 0, 0, sqrt(VAR) / CAT))

rm(blah.AB1, blah.B2)

cv.dummy = cv.dummy %>%
  select(any_of(c("SID","YEAR","fed_closed","NEW_MODE","NEW_MODEN","NEW_STA","WAVE","CATCH_VAR",
                  "CAT","CV","AT","ATtotal","PSU","PSUtotal"))) %>%
  arrange(across(any_of(c("SID","YEAR","fed_closed","NEW_MODE","CATCH_VAR"))))

dummy.offset = 3
if ("SID" %in% colnames(cv.dummy)) dummy.offset = dummy.offset - 1
if ("fed_closed" %in% tolower(colnames(cv.dummy))) dummy.offset = dummy.offset - 1

writeData(wb, sheet = "MRIP catCV numbers", startCol = dummy.offset, x = cv.dummy, colNames = TRUE)
rm(dummy.offset, cv.dummy)

removeWorksheet(wb, sheet = "MRIP landCV weight #1")

dummy.offset = 3
if ("SID" %in% colnames(lbs.cv.table.2)) dummy.offset = dummy.offset - 1
if ("fed_closed" %in% tolower(colnames(lbs.cv.table.2))) dummy.offset = dummy.offset - 1

cloneWorksheet(wb, sheetName = "MRIP landCV weight", clonedSheet = "MRIP landCV weight #2")
removeWorksheet(wb, sheet = "MRIP landCV weight #2")
writeData(wb, sheet = "MRIP landCV weight", startCol = dummy.offset, x = lbs.cv.table.2, colNames = TRUE)
rm(dummy.offset)

if (flag.unid) {
  
  removeWorksheet(wb, sheet = "UNID_rec_catGEN")
  addWorksheet(wb, sheet = tab.unid.dat)
  writeData(wb, sheet = tab.unid.dat, x = unid.dat, colNames = TRUE)
  
  writeData(wb, sheet = "UNID_ratios",
            x = unid.table %>% filter(YEAR != "TOTAL"),
            colNames = FALSE, startRow = 9)
  
  writeData(wb, sheet = "UNID_ratios",
            x = unid.table %>% filter(YEAR == "TOTAL"),
            colNames = FALSE, startRow = 7)
  
  unid.taxa = unique(gsub("_.*", "", colnames(unid.table)))
  unid.taxa = unid.taxa[!(unid.taxa %in% c("YEAR","AB1","B2"))]
  
  # updated header label logic: use family bucket label directly
  unid.taxa = c(bucket_family, unid.taxa)
  
  for (i in (length(unid.taxa)-1):2) {
    unid.taxa = append(unid.taxa, c(NA, NA, NA), after = i)
  }
  unid.taxa = append(unid.taxa, c(NA, NA), after = 1)
  
  writeData(wb, sheet = "UNID_ratios", x = data.frame(t(unid.taxa)), colNames = FALSE, startRow = 3)
  rm(i, unid.taxa)
  
} else {
  removeWorksheet(wb, sheet = "UNID_rec_catGEN")
  removeWorksheet(wb, sheet = "UNID_ratios")
}

saveWorkbook(wb, file = paste0(dir, "/", table.ID, "_ACL.xlsx"), overwrite = TRUE)


#Checks
catch.table %>% summarise(totalAB1 = sum(AB1, na.rm = TRUE),
                          total_B2 = sum(B2, na.rm = TRUE))

catch.table %>%
  group_by(NEW_COM) %>%
  summarise(AB1 = sum(AB1, na.rm = TRUE),
            B2 = sum(B2, na.rm = TRUE)) %>%
  filter(grepl("sandbar|requiem", NEW_COM, ignore.case = TRUE))

#plot time series
catch.table %>% group_by(YEAR) %>%
  summarise(AB1 = sum(AB1, na.rm = TRUE),
            B2 = sum(B2, na.rm = TRUE)) %>%
  pivot_longer(-YEAR) %>%
  ggplot(aes(YEAR, value, color = name)) + 
  geom_line() +
  theme_bw()

#check discards relative to landings
catch.table %>%
  group_by(YEAR) %>%
  summarise(AB1 = sum(AB1, na.rm = TRUE),
            B2 = sum(B2, na.rm = TRUE),
            ratio = B2/AB1)

#mode totals
catch.table %>%
  group_by(NEW_MODEN) %>%
  summarise(AB1 = sum(AB1, na.rm = TRUE),
            B2 = sum(B2, na.rm = TRUE))

#state distribution levels
catch.table %>%
  group_by(NEW_ST) %>%
  summarise(AB1 = sum(AB1, na.rm = TRUE),
            B2 = sum(B2, na.rm = TRUE))

#check MRIP 1981 Wave 1
catch.table %>%
  filter(YEAR == 1981) %>%
  group_by(WAVE) %>%
  summarise(AB1 = sum(AB1, na.rm = TRUE))

dat %>% filter(YEAR == 1981,
               DS == "MRIP",
               NEW_SCI == "Carcharhinus plumbeus") %>%
  count(WAVE, NEW_MODEN)

catch.table %>%
  filter(DS == "MRIP", YEAR == 1981) %>%
  group_by(WAVE, NEW_MODEN) %>%
  summarise(AB1 = sum(AB1, na.rm = TRUE),
            B2 = sum(B2, na.rm = TRUE),
            .groups = "drop") %>%
  arrange(WAVE, NEW_MODEN)

catch.table %>% filter(DS == "MRIP",
                       YEAR == 1981,
                       WAVE == 6) %>%
  summarise(AB1 = sum(AB1, na.rm = TRUE),
            B2 = sum(B2, na.rm = TRUE))

#check discard ratio
catch.table %>% filter(YEAR == 1981) %>%
  summarise(AB1 = sum(AB1, na.rm = TRUE),
            B2 = sum(B2, na.rm = TRUE),
            ratio = B2/AB1)

#TPWD landings
catch.table %>%
  filter(DS == "TPWD") %>%
  group_by(YEAR) %>%
  summarise(AB1 = sum(AB1, na.rm = TRUE))

#check for negative values
catch.table %>% filter(AB1 <0 | B2 <0)

#check for missing years
catch.table %>%
  group_by(YEAR) %>%
  summarise(AB1 = sum(AB1, na.rm = TRUE)) %>%
  arrange(YEAR) %>%
  print(n = 44)

#check avg weights
catch.table %>%
  summarise(min_weight = min(AVG_WGT, na.rm = TRUE),
            max_weight = max(AVG_WGT, na.rm = TRUE))

#total catch check
sum(catch.table$AB1)
sum(catch.table$B2)

###########End test

##############################################
######     Allocation of UnIDd Catch    ######
######            to Species            ######
##############################################
###
###     ...which is the first ADJUSTMENT made to 'catch.table' to ensure that all catch records corresponding to
###       the species-of-interest ( whether reported as such or allocated from another UNID group ) are included
###       in any additional modifications/imputations conducted in this script. As an example, in imputing 1981-wave1 MRIP catch,
###       if any unidentified catch was recorded in wave1 for 1982-1984, and some percentage of this catch is assumed to belong
###       to the assessed species, then this (unidentified) catch should also be included in any fractions being used to
###       impute the missing MRIP catch estimates (e.g., wave1 / waves2-6 )...
###
###   The script below, which conducts this allocation ( from UNID to ID catch ), is composed of three parts:
###
###         (1) new pull of catch data that includes all taxa that could be contributing to the 'unidentified' catch record(s),
###         (2) summary of the relative catch of the 'identified' taxa (pulled in step1), from which an appropriate ratio
###             is to be identified/estimated (i.e., to partition 'unidentified' catch amongst species, in step3 ),
###         (3) application of this ratio (from step2) to our 'catch.table' object

source( paste0(dir,'/Functions/pull_GenRec_catch.R') )
###   ...which contains the pull.GenRec.catch() function...
source( paste0(dir,'/Functions/SECmodify_allocate_unid.R') )
###   ...which contains the summary.unid() and allocate.unid() functions...


# summary( as.factor(catch.table$NEW_COM) )

if( flag.unid ) {
  
  
  ###   -- STEP 1 --
  ###
  ###     The first step is to pull another GenRec catch file. In particular, the summary table constructed in step2
  ###     ( relative catch of species/taxa ) is likely to consider a different subset of species than that in 'catch.table'
  ###     (i.e., species that 'fall under' the unidentified taxa vs. the species-of-interest for this assessment ).
  ###     Therefore, we start by constructing a separate 'unid.dat' (catch) data table...
  
  unid.dat <- pull.GenRec.catch(  raw.table = dat,
                                  pull.type = 'ACL',
                                  # pull.type = 'RDI',
                                  spp.filter = nodc.unid, yr.filter = first.year:term.year,  mode.filter = mode_sub,
                                  reg.filter = region,   sta.filter = states,  fl.filter = FL_sub, nc.filter = NC_sub  )
  
  
  ###   -- STEP 1.5 --
  ###
  ###   Additionally, for some species, the standard ACL scripts already apply correction factors to allocate some percent
  ###   of unidentified catch to the species-of-interest (e.g., gag from black grouper, blueline from UNID tilefish ).
  ###   In these cases, I (re-)evaluate the appropriateness of the current proportions by "backing-out" the allocations
  ###   that have already been applied (in the ACL files), reverting the original table of catch records to those that
  ###   explicitly identified for the species-of-interest (i.e., removing all 'unidentified' records )...
  
  if( exists("unid.ratio.prev") ) {
    
    dummy.table <- revert.unid( genrec.table = catch.table,
                                unid.dat     = unid.dat,
                                unid.ratio.prev = unid.ratio.prev,
                                nodc.unid.catch = nodc.unid.catch,
                                new.com.unid = new.com.unid,
                                new.sci.unid = new.sci.unid )
    
    unid.dat    = dummy.table$unid.dat
    catch.table = dummy.table$catch.table
    rm( dummy.table )
  }
  
  # ###   ...and to validate that the "back-calculation" in our revert.unid( ) function is working properly...
  # sum( catch.table$AB1[ catch.table$NEW_COM %in% new.com ], na.rm=TRUE ) +
  #   ( sum( catch.table$AB1[ catch.table$NEW_COM %in% new.com.unid ], na.rm=TRUE ) * unid.ratio.prev )
  # sum( catch.table$B2[ catch.table$NEW_COM %in% new.com ], na.rm=TRUE ) +
  #   ( sum( catch.table$B2[ catch.table$NEW_COM %in% new.com.unid ], na.rm=TRUE ) * unid.ratio.prev )
  
  
  
  ###   -- STEP 2 --
  ###
  ###     We then determine what fraction of unidentified catch should be retained in our 'catch.table' object
  ###     (i.e., that assumed to be comprised of the species-of-interest). To inform this determination,
  ###     we summarize the relative catch of all ('identified') species/taxa that may be contributing to the
  ###     'unidentified' catch record(s). Note that the step2 function that constructs this summary (see below)
  ###     does not choose the ratio for the GenRec data provider, it simply constructs the summary table
  ###     to inform the decision of an appropriate ratio (which is then manually input into the step3 function)...
  
  unid.table <- summary.unid( genrec.table = unid.dat,
                              nodc.unid.catch = nodc.unid.catch,  nodc.unid = nodc.unid )
  ###       -- 'nodc.unid.catch' -- NODC code for the UNID group (i.e., catch to partition )
  ###       -- 'nodc.unid'       -- NODC codes for all taxa that 'fall under' the unidentified taxa
  
  
  # ###   ...and a plot to evaluate the percentage of 'identified' catch composed of the target species...
  # command.line = paste0( "dummy.table = unid.table %>% group_by( YEAR ) %>%
  #                               summarise( pAB1 = sum( `",new.com,"_p.AB1` ),
  #                                          pB2  = sum( `",new.com,"_p.B2`  ) ) %>%
  #                               mutate( pAB1 = ifelse( is.nan(pAB1), NA, pAB1 ),
  #                                       pB2  = ifelse( is.nan(pB2 ), NA, pB2  ) ) %>%
  #                               pivot_longer( cols = -c(YEAR), names_to='CAT_VAR', values_to='prop' )" )
  # eval( parse( text = command.line ) )
  # rm(command.line)
  # 
  # # # poly.degf = 5
  # # poly.degf = round( ( length( unique(dummy.table$YEAR) ) - 1 ) / 3, 0 )
  # # ###     ...(-1) to not count the YEAR='TOTAL' row, and divided by three as a subjective attempt prevent 'overfitting'...
  # 
  # dummy.plot = ggplot( data = dummy.table %>% filter( YEAR != 'TOTAL' & !is.na(prop) ) %>% mutate( YEAR = as.numeric(YEAR) ) ) +
  #   geom_point( aes( x=YEAR, y=prop ) ) +
  #   # stat_smooth( aes( x=YEAR, y=prop ), method = lm, formula = y ~ poly( x,poly.degf ), se=FALSE ) +
  #   geom_hline( data = dummy.table %>% filter( YEAR == 'TOTAL' ),
  #               aes( yintercept = prop ), linewidth=1.2 ) +
  #   facet_grid( CAT_VAR ~ . , scales = 'free' ) +
  # 
  #   labs( title="", x="Year", y=paste0( "Percent Catch (",new.com,")" ) ) +
  #   expand_limits(y = 0) +
  #   theme_bw() +
  #   theme( text = element_text(size = 11),
  #          axis.text.x = element_text(angle = 90, vjust=0.5),
  #          legend.position = "bottom",
  #          panel.grid.major = element_line(colour = "grey", linewidth = 0.5),
  #          panel.grid.minor = element_line(colour = "grey", linewidth = 0.2),
  #          panel.border = element_rect(colour = "black", fill = NA) )
  # dummy.plot
  # 
  # dummy.plot = dummy.plot +
  #   geom_abline( intercept=0.23, slope=0, color= 'red', linetype='dashed' ) +    ### SEDAR 32 Ratio
  #   geom_abline( intercept=0.45, slope=0, color='blue', linetype='dashed' )      ### SEDAR 50 Ratio
  # dummy.plot
  # 
  # rm( dummy.table, poly.degf, dummy.plot )
  
  
  
  # eval( parse( text = paste0(
  #       "sum( unid.table$`",new.com,"_AB1`[ unid.table$YEAR %in% 2005:2023 ], na.rm=TRUE ) /
  #        sum( unid.table[ unid.table$YEAR %in% 2005:2023, grepl('_AB1',colnames(unid.table)) ], na.rm=TRUE )" ) ) )
  # eval( parse( text = paste0(
  #   "sum( unid.table$`",new.com,"_B2`[ unid.table$YEAR %in% 2005:2023 ], na.rm=TRUE ) /
  #    sum( unid.table[ unid.table$YEAR %in% 2005:2023, grepl('_B2',colnames(unid.table)) ], na.rm=TRUE )" ) ) )
  # 
  # eval( parse( text = paste0( "mean( unid.table$`",new.com,"_p.AB1`[ unid.table$YEAR %in% 2005:2023 ] )" ) ) )
  # eval( parse( text = paste0( "mean( unid.table$`",new.com,"_p.B2`[  unid.table$YEAR %in% 2005:2023 ] )" ) ) )
  
  
  
  ### ***************************************************
  
  ### -- NOTES ON DECISION --
  ###
  ###     In SEDAR 50 (p92 of S50 DWR), 45% of unidentified tilefish landings were assumed to be composed of BLT,
  ###     which is roughly the same percentage being calculated with the updated SEDAR 92 landings data
  ###     (i.e., 44.41% ; over all years with data updated through 2023 ). However:
  ###         -- there appears to be a trend in these percentages, in that the relative contribution of BLT to
  ###           total tilefish catch looks to have been growing over the last two decades (2005+)
  ###         -- BLT also appear to comprise significantly less of the total (UNID) tilefish discards than they
  ###           do the landings, with sand tilefish dominating tilefish discards (~92%) and leaving ~7% assumed
  ###           to be BLT ( over all years with data through 2023 )
  ###     Taken together, for SEDAR 92, we will apply unique (unidentified) allocation ratios to AB1 & B2,
  ###     both calculated from data between 2005-2023, which yielded ratios of 0.5901 and 0.1447 respectively.
  ###     This proposal was emailed to the lead analyst (Nikolai), who agreed via email (on May 6, 2024).
  
  # unid.ratio = 0.45
  unid.ratio = data.frame( AB1=0.59, B2=0.14 )
  
  ### ***************************************************
  
  
  ###   -- STEP 3 --
  ###
  ###     Informed by the above analysis (i.e., relative catch of species that 'belong' to the unidentified taxa ),
  ###     we then apply the chosen fraction to our 'catch.table' object. Note that the allocate.unid() function
  ###     imports the SEFSC avgwgt estimates at each level of the hierarchy ( saved in 'avgwgt.dir' ), as needed
  ###     to update the lbsest_SEC fields with an avgwgt representative of the species-of-interest (not the UNID taxa),
  ###     and so the function below may take a little time to run...
  
  catch.table <- allocate.unid( genrec.table = catch.table,
                                unid.ratio = unid.ratio,
                                nodc.unid.catch = nodc.unid.catch, avgwgt.dir = avgwgt.dir )
  ###   ...where 'unid.ratio' is the chosen fraction of catch ( *NOT* identified to species ) that is believed to be
  ###         composed of the species-of-interest. Note that this ratio is manually defined (i.e., = numeric )
  ###         as no 'BEST PRACTICE' approach currently exists in how to set this ratio, but such automation could be
  ###         incorporated into this script if guidance is ever developed in the future...
  ###   ...'nodc.unid.catch' identifies the taxonomic group(s) for which 'unidentified' catch estimates exist
  ###         (in 'catch.table') and need to be reduced ( by the fraction defined in 'unid.ratio' ), and
  ###   ...'avgwgt.dir' identifies the directory within which SEFSC avgwgt estimates from the most recent ACL files are stored
  
  
  ###   -- STEP 4 --
  ###
  ###     As a last step, I save a copy of the above 'catch.table' object before any additional adjustments are made
  ###     (i.e., before breaking CBTHBT, imputing MRIP 1981-wave1, etc. ), which is needed in estimating uncertainties for
  ###     catch-in-number for which CV calculations only include records from 'identified' records ( and so any catch
  ###     allocated from the 'unidentified' taxa needs to be added into our 'cv.table' object )...
  
  catch.table.unid = catch.table
  
}

# summary( as.factor(catch.table$NEW_COM) )



### ---------------------------------------------------------------------------------------------------------------- ###
### ---------------------------------------------------------------------------------------------------------------- ###


#######################################################
######     Partitioning combined CbtHbt Catch    ######
######     from MATL/NATL into separate Modes    ######
#######################################################


source( paste0(dir,'/Functions/SECmodify_allocate_forhire.R') )


# summary( as.factor(catch.table$NEW_MODEN) )

if( flag.forhire ) {
  
  ###     ...where I save a filtered copy of 'catch.table' to only include those MATL/NATL for-hire estimates
  ###       that are to be partitioned into separate CBT & HBT catch estimates ( OST only provides estimates
  ###       of a combined forhire mode for 1981-2003 from the MATL & NATL, and so that's the filter applied below ).
  ###       The need for this table is tied to the calculation of our CV estimates, in that we need the original
  ###       combined CBTHBT estimates (e.g., before imputing any MRIP 1981-wave1 estimates ) to ensure estimates
  ###       in 'cv.table' match those being calculated in 'catch.table'...
  
  catch.table.forhire = catch.table %>%
    filter( DS == 'MRIP' & SUB_REG %in% 4:5 & NEW_MODEN %in% c('Cbt/Hbt') & YEAR %in% 1981:2003 )
  
  
  catch.table = partition.forhire( genrec.table = catch.table )
  
}

# summary( as.factor(catch.table$NEW_MODEN) )


### ---------------------------------------------------------------------------------------------------------------- ###
### ---------------------------------------------------------------------------------------------------------------- ###


##########################################################
######         Imputations to Fill Data Gaps        ######
######            in Regional Rec Surveys           ######
######             -- MRIP 1981-wave1 --            ######
##########################################################

###   Based on SEDAR best practices ( SEDAR PW7 -- Recreational Issue #2 ), MRIP catch for 1981-wave1 can be
###   imputed for the Gulf of Mexico and east coast of Florida using either:
###
###         (1) the proportion of wave1 catch to that from other waves (2-6) in years 1982-1984 by fishing mode and area.
###             These proportions can then be applied to the total catch from waves 2-6 in 1981 to estimate 1981 wave 1 catch...
###         (2) the average (wave1) catch across years 1982-1984...
###
###   The ratio method (#1) is the preferred method and applied when ratios are reasonably stable from year to year.
###   However, when ratios are highly variable (from year to year), the average catch approach (#2) is to be applied...



###   Note that 1981-wave1 (MRIP) catch is only estimated for FLE & GOM states:
###         MRIP doesn't sample in states north of FLE for wave1, for which effort is considered negligible.
###         Therefore, 'missing' 1981-wave1 estimates in these states (north of FLE) isn't considered a data gap,
###         just an assumption of the survey (of zero catch)...
###   Therefore, this code is only needed when one/more of these states are included in the assessment...

if( any( c('TX','LA','MS','AL','FLW','FLE') %in% states ) & 1981 %in% first.year:term.year ) {
  
  
  source( paste0(dir,'/Functions/SECmodify_impute_MRIP1981w1.R') )
  
  
  ###   ...for which we first apply the summary.MRIP.1981w1() function to determine:
  ###         (1) if these imputations are necessary and
  ###         (2) if so, how to conduct these imputations
  dummy <- summary.MRIP.1981w1( genrec.table = catch.table )
  
  
  dummy$WaveCatch
  ###     ...where 'WaveCatch' is the total catch (by wave) from years 1982-1984 and states TX-FLE, which are the
  ###     spatiotemporal domains over which the imputation factors would be estimated ( avg.ratio vs mean.catch ).
  ###     This table is used to evaluate whether MRIP 1981-wave1 catch is even necessary to impute
  ###     (i.e., if wave1 catch is insignificant (~0), it's not going to effect the assessment anyway )...
  
  dummy$fracs.byMETRIC
  # dummy$fracs.byYEAR
  ###     ...where these plots are used to evaluate the relative stability of wave1 catch estimates to other waves
  ###       ( over years 1982-1984 ), which is the primary criteria in determining if approach #1 or #2 is the
  ###       most appropriate for this assessment. Note that while these summaries are provided by YEAR,
  ###       the applied ratios are not year-specific ( ratios calculated from the sum(catch) across years by mode & area )...
  
  dummy$methods.RAWvIMP
  dummy$methods.TOTAL
  ###     ...where these plots provide comparisons of the imputed 1981-wave1 estimates between the two approaches,
  ###       to evaluate whether the choice of approach (#1 vs. #2) has much of a difference on the final estimates...
  
  
  ### ***************************************************
  
  ### -- NOTES ON DECISION --
  ###   ...where it doesn't appear as though MRIP estimates for 1981-wave1 were imputed in S38U:
  ###               S38U -- N:\FMB\SEDAR\SEDAR38U\Recreational\program.sas
  ###   ...which Vivian explained as having "no time with all the SEDARs on deck". Digging into this for S99...
  ###         -- wave1 catch was relatively negligible historically ( ~10% of AB1 and 0% of B2 ) and so whatever choice
  ###                 is made (e.g., to impute or not ), it should have little effect on the resultant time series.
  ###         -- there seems to be little difference in the total (annual) catch estimates between the two imputation
  ###                 methods, so the choice of method also has little effect on the resultant time series.
  ###   Taken together, the decision to not impute is probably justified in SEDAR 99 as well, especially considering that
  ###   Gulf king mackerel assessments are based on a FISHING YEAR variable (i.e., Jul01-Jun30 ) and so 1981-wave1 is
  ###   considered an incomplete year and will be dropped, but it is imputed here "for completion" and using the
  ###   "best practice" approach given similar estimates between the two approaches...
  
  # ###   ...where, from the above analysis, we identify what (if any) imputation to apply...
  # method.MRIP.1981w1 = 'None'
  method.MRIP.1981w1 = 'prop_w1_w26'
  # method.MRIP.1981w1 = 'avg_82_84'
  
  ### ***************************************************
  
  
  if( method.MRIP.1981w1 != 'None' ) {
    
    catch.table = impute.MRIP.1981w1( genrec.table = catch.table, method = method.MRIP.1981w1 )
    
  }
  
  rm( dummy )
  
}


### ---------------------------------------------------------------------------------------------------------------- ###
### ---------------------------------------------------------------------------------------------------------------- ###


##########################################################
######         Imputations to Fill Data Gaps        ######
######            in Regional Rec Surveys           ######
######              -- LACR discards --             ######
##########################################################

###   Unlike some of the other SEDAR-specific modifications we make to GenRec catch estimates, there are
###   no best practices for imputing LACreel discard estimates (assuming the decision is to impute).
###   However, there is a best practices approach for imputing TPWD discards, the code for which is a bit
###   further down in the script. I will therefore apply a similar approach for LACR discards...
###   As a proxy for LACR discards, LACR AB1 estimates (by year and mode) are multiplied by mode-specific
###   B2:AB1 ratios (combined across years). The question just comes down to which years/states to use in
###   the calculation of these (B2:AB1) catch ratios.
###
###   For species for which LACR collects discard information ( data gap = 2014-2015 )...
###      -- If the availability of LA catch data is sufficient, in that the (B2:AB1) catch ratios b/w zero & one
###             and relatively stable over time (years), these ratios will be calculated from LA catch estimates
###             in adjacent years (e.g., mode-specific ratios from MRIP 2011-2013 and/or LACR 2016-2018 ).
###      -- If LA catch data is insufficient (e.g., ratios not well estimated or variable ), Gulf-wide ratios
###             will be applied. However, in this case, ratios are calculated for the same years over which the
###             LACR discards are imputed (i.e., 2014 & 2015 Gulf-wide ratios to impute 2014 & 2015 discards ).
###
###   For species for which LACR does not collect discard information ( data gap = 2014+ )...
###       -- LA catch estimates will be used to calculate (B2:AB1) catch ratios when these ratios are
###             b/w zero & one and relatively stable over time (years). Because LA catch estimates are only
###             available from 1981-2013 in this case, ratios will be calculated from a chosen subset of
###             adjacent years of MRIP data (including 2013) over which B2:AB1 ratios appear stable
###             (e.g., 2011-2013, 2000-2013 ).
###          Note that when evaluating the stability of catch ratios, we look at both the 1981-2013 timeseries
###             of LA catch ( to determine if LA ratios are well estimated & stable ) and those from other Gulf states
###             ( to determine if the assumption of a static LA catch ratio is appropriate for years 2014+ ).
###       -- If LA catch data is insufficient (e.g., not well estimated or variable ) or ratios from other Gulf states
###             suggest catch ratios are not static over the 'imputation' years (2014+), Gulf-wide ratios
###             will be applied. Again, Gulf-wide ratios are calculated for the same years over which the
###             LACR discards are imputed (i.e., year-specific Gulf-wide ratios to impute year-specific LACR discards ).


if( 'LA' %in% states ) {
  
  
  source( paste0(dir,'/Functions/SECmodify_impute_LACRdiscards.R') )
  
  
  ###   ...for which we first apply the summary.LACR.B2() function to determine:
  ###         (1) if these imputations are necessary and
  ###         (2) if so, how to conduct these imputations
  dummy <- summary.LACR.B2( genrec.table = catch.table )
  
  
  ###   In addressing (1), as to whether LACreel discards should be imputed, we evaluate...
  ###
  ###       -- the relative catch coming from LA (i.e., is catch negligible? )
  dummy$StateCatch
  
  ###       -- Does B2=0 look reasonable for LA 2014+ relative to other years and/or other states?
  ###       -- Has this species been consistently discarded in LA (over time), or are discards a sporadic phenomenon?
  View(dummy$LACatch)
  blah = dummy$LACatch %>% select( YEAR, AB1, B2 ) %>% pivot_longer( !YEAR )
  dummy.plot = ggplot( data = blah, aes( x=YEAR, y=value ) ) + geom_point() + geom_line() +
    geom_vline( xintercept = 2014 ) + ylim( 0, NA ) + facet_grid( name~.,scales='free' )
  dummy.plot
  rm( blah, dummy.plot )
  
  
  ###   Assuming we decide to impute, we then explore (2) and how to do this imputation...
  ###       -- Are the catch ratios (B2:AB1) relatively stable across time (years) and space (states/regionally),
  ###           in which they would be defensible to apply in producing proxy discard estimates for LACR?
  View(dummy$fracs)
  dummy$fracs.fig
  blah = dummy$fracs %>% select( YEAR, Cbt_LA,Cbt_GULF, Priv_LA,Priv_GULF ) %>%
    pivot_longer( !YEAR ) %>% mutate( MODE = gsub( '_.*','',name ), DATA = gsub( '.*_','',name ) )
  # dummy.plot = ggplot( data = blah, aes( x=YEAR, y=value, colour=DATA ) ) + geom_point() + geom_line() +
  #   geom_vline( xintercept = 2014 ) + ylim( 0, NA ) + facet_grid( MODE ~ . , scales='free' )
  dummy.plot = ggplot( data = blah, aes( x=YEAR, y=value, colour=MODE ) ) + geom_point() + geom_line() +
    geom_vline( xintercept = 2014 ) + ylim( 0, NA ) + facet_grid( DATA ~ . , scales='free' )
  dummy.plot
  rm( blah, dummy.plot )
  
  
  ### ***************************************************
  
  ### -- NOTES ON DECISION --
  ###   Discard information is collected as part of the LACreel survey for this particular species (2016+) and
  ###   so the only data gap for LA discards is 2014-2015. Exploring the data a bit...
  ###         -- Spatially, LA catch is relatively small across the Gulf ( ~2.4% of AB1 and ~3.3% of B2 ) and so the
  ###                 decision as to whether to impute (or not) should have little effect on the resultant time series
  ###         -- Temporally, LA catch is relatively low over those years for which LACR provides LA estimates (2014+),
  ###                 and so any imputations (of LACR discards) are expected to be similarly small
  ###         -- LA catch ratios (B2:AB1) have quite a bit more variability than the GULF ratios, with LA ratios showing
  ###                 some apparent "spikes" in discard rates for charter (~3.2 btw 2017-2018) and private (~8.2 btw 2004/2006).
  ###                 Conversely, GULF ratios were relatively static over time, being almost consistently estimated between
  ###                 0 and 1 and with no apparent indication of hitting any bounds (i.e., not 'excessively' large).
  ###   Looking back at S38U, LA discards (2014-2015) were not imputed for Gulf king mackerel.
  ###   From Vivian's script for the S38U assessment:
  ###               -- N:\FMB\SEDAR\SEDAR38U\Recreational\program.sas
  ###           Vivian "left 2014 and 2015 discards as missing, [which was] similar to what [she] did for gtrig
  ###           (Shannon 3/6/19) [and notes that they] do not have a whole lot of data available to estimate those years."
  ###   However, for completion, I will impute LA discards in SEDAR 99, relying on the stability in the GULF ratios to
  ###   adequately represent the discard rates of LA king mackerels...
  
  ###   ...where, from the above analysis, we identify what (if any) imputation to apply...
  # method.LACR.B2 = 'None'
  # method.LACR.B2 = 'la_ratio'
  method.LACR.B2 = 'gu_ratio'
  
  ###   ...and, if the choice is to impute, we also identify the years over which the (B2:AB1) ratios are calculated
  ###       (e.g., ratios calculated over 'ratio.yrs.LACR.B2', but applied to estimate LACR discards for 2014+ ).
  ###       Note that this input is only relevant for the 'la_ratio' method as the 'gu_ratio' method applies
  ###       B2:AB1 ratios calculated from the same year as that requiring an imputation (e.g., use 2014 catch rates
  ###       to impute 2014 LACR discards ). Therefore, 'ratio.yrs.LACR.B2' is not called ( within the function )
  ###       when method='gu_ratio' ( its set to either 2014:2015 or 2014:term.year in the function )...
  # ratio.yrs.LACR.B2 = 2016
  # ratio.yrs.LACR.B2 = 2000:2013
  # ratio.yrs.LACR.B2 = c(2011:2013,2016:2018)
  ratio.yrs.LACR.B2 = 2016:2018
  
  ### ***************************************************
  
  
  if( method.LACR.B2 != 'None' ) {
    
    catch.table = impute.LACR.B2( genrec.table = catch.table,
                                  method = method.LACR.B2,
                                  ratio.years = ratio.yrs.LACR.B2 )
    ###     ...where the 'ratio.years' argument is defined regardless of the chosen 'method',
    ###         but only applied in the function for method == 'la_ratio'...
    
  }
  
  rm( dummy )
  
}


### ---------------------------------------------------------------------------------------------------------------- ###
### ---------------------------------------------------------------------------------------------------------------- ###


##########################################################
######         Imputations to Fill Data Gaps        ######
######            in Regional Rec Surveys           ######
######            -- TPWD 1981-May1983 --           ######
##########################################################

###   The (standard) TPWD survey didn't begin until the start of the 1983 high-use season (May15) and so
###   there is an inherent data gap in this survey. In accordance with best practices ( SEDAR PW7 -- Rec Issue #3 ),
###   and as applied in past SEDARs (e.g., S28 spanish mackerel & cobia, S31 red snapper, S33 gag & greater amberjack),
###   average TPWD catch estimates from 1983-1985 (by mode and wave) were used to estimate:
###
###         -- Texas catch in waves 1-2 for years 1981-1983 (imputed from TPWD estimates in 1984-1985 -- two-year avgs )
###         -- Texas catch in waves 3-6 for years 1981-1982 (imputed from TPWD estimates in 1983-1985 -- three-year avgs )


if( 'TX' %in% states ) {
  
  
  source( paste0(dir,'/Functions/SECmodify_impute_TPWD1981_83w2.R') )
  
  
  ###   ...for which we first apply the summary.TPWD.1981.1983() function to determine if imputations are necessary
  dummy <- summary.TPWD.1981.1983( genrec.table = catch.table )
  
  ###       -- the relative catch coming from TX over those years for which average catches would be calculated.
  ###           In particular, are TX landings negligible in years 1983-1985 compared to other Gulf states?
  dummy$StateCatch
  
  ###       -- Does AB1=0 look reasonable for TX 1981-1983, relative to other years?
  ###       -- Has this species been consistently landed in TX (over time), or are landings a sporadic phenomenon?
  View(dummy$TXCatch)
  dummy.plot = ggplot( data = dummy$TXCatch, aes( x=YEAR, y=AB1 ) ) + geom_point() + geom_line()
  dummy.plot
  rm( dummy.plot )
  
  dummy$State.fig
  
  
  ### ***************************************************
  
  ### -- NOTES ON DECISION --
  ###   ...for which the only real decision to be made here is whether to impute, as only a single method
  ###   exists by which the SEFSC has ever imputed TPWD discards (i.e., average catches from adjacent years ).
  ###         -- Spatially, TX landings were relatively small (historically) across the Gulf ( ~8.1% of AB1 btw 1983-1985 )
  ###               and so the decision to impute (or not) should have little effect on the resultant time series
  ###         -- Temporally, TX catch was relatively high over the first few years of the TPWD survey (1983-1985),
  ###               and so its seems unlikely that landings were zero in the years before (1981-1983)
  ###   Given non-zero (TPWD) landings estimates exist over those years from which historic landings would be imputed
  ###   ( May1983-1985 ), the decision for S99 was to impute historic TPWD landings. This was the same approach applied
  ###   in S38 and S38U, as noted in Vivian's S38U SAS script:
  ###               -- N:\FMB\SEDAR\SEDAR38U\Recreational\program.sas
  
  ###   ...where, from the above analysis, we identify what (if any) imputation to apply...
  # method.TPWD.1981.83 = 'None'
  method.TPWD.1981.83 = 'avg_83_85'
  
  ### ***************************************************
  
  
  if( method.TPWD.1981.83 != 'None' ) {
    
    catch.table = impute.TPWD.1981.1983( genrec.table = catch.table, method = method.TPWD.1981.83 )
    
  }
  
  rm( dummy )
  
}


### ---------------------------------------------------------------------------------------------------------------- ###
### ---------------------------------------------------------------------------------------------------------------- ###


##########################################################
######         Imputations to Fill Data Gaps        ######
######            in Regional Rec Surveys           ######
######              -- TPWD discards --             ######
##########################################################

###   The TPWD survey has never collected information on released (alive) fish and so a proxy is needed to estimate
###   Texas discards. To fill-in this data gap (across all years), B2:AB1 catch ratios are calculated (by year & mode)
###   and applied to TPWD landings estimates. In accordance with SEDAR best practices ( SEDAR PW7 -- Rec Issue #10 ),
###   these ratios are calculated from either:
###
###       (1) catch estimates from just Louisiana or
###       (2) catch estimates across the entire Gulf of Mexico (i.e., Gulf-wide ratios)
###
###   The preferred approach is option #1, using just LA data, but the reliability of these estimates is a function of
###   the reliability (and availability) of LA data. In particular, LACreel replaced MRIP operations in 2014 (in LA),
###   and only collects discard info for a subset of species since (discard) data collection began (in 2016).
###
###      -- If the availability of LA catch data is sufficient, in that the (B2:AB1) catch ratios b/w zero & one
###             and relatively stable over time (years), year-specific ratios will be calculated from LA catch estimates
###      -- If LA catch data is insufficient (e.g., ratios not well estimated or variable ), Gulf-wide ratios are used
###
###   Note that, in both of these approaches, year-specific (B2:AB1) catch ratios are calculated and applied to
###   year-specific TPWD landings estimates (e.g., 2014 ratios applied to 2014 TPWD AB1 as a proxy for 2014 TPWD B2 ).


if( 'TX' %in% states ) {
  
  
  source( paste0(dir,'/Functions/SECmodify_impute_TPWDdiscards.R') )
  
  
  ###   ...for which we first apply the summary.TPWD.B2() function to determine:
  ###         (1) if these imputations are necessary and
  ###         (2) if so, how to conduct these imputations
  dummy <- summary.TPWD.B2( genrec.table = catch.table )
  
  
  ###   In addressing (1), as to whether TPWD discards should be imputed, we evaluate...
  ###
  ###       -- the relative catch coming from TX (i.e., is catch negligible? )
  dummy$StateCatch
  dummy$State.fig
  
  
  ###   Assuming we decide to impute, we then explore (2) and how to do this imputation...
  ###       -- Are the catch ratios (B2:AB1) relatively stable across time (years) and space (states/regionally),
  ###           in which they would be defensible to apply in producing proxy discard estimates for TPWD?
  View(dummy$fracs)
  dummy$fracs.fig
  blah = dummy$fracs %>% select( YEAR, Cbt_LA,Cbt_GULF, Priv_LA,Priv_GULF ) %>%
    pivot_longer( !YEAR ) %>% mutate( MODE = gsub( '_.*','',name ), DATA = gsub( '.*_','',name ) )
  dummy.plot = ggplot( data = blah, aes( x=YEAR, y=value, colour=DATA ) ) + geom_point() + geom_line() +
    ylim( 0, NA ) + facet_grid( MODE ~ . , scales='free' )
  # dummy.plot = ggplot( data = blah, aes( x=YEAR, y=value, colour=MODE ) ) + geom_point() + geom_line() +
  #   ylim( 0, NA ) + facet_grid( DATA ~ . , scales='free' )
  dummy.plot
  rm( blah, dummy.plot )
  
  
  
  ### ***************************************************
  
  ### -- NOTES ON DECISION --
  ###   Exploring the data a bit...
  ###         -- Spatially, TX catch is relatively small across the Gulf ( ~3.8% of AB1 ) and so the decision as
  ###                 to whether to impute (or not) should have little effect on the resultant time series
  ###         -- LA catch ratios (B2:AB1) have quite a bit more variability than the GULF ratios, with LA ratios showing
  ###                 some apparent "spikes" in discard rates for charter (~3.2 btw 2017-2018) and private (~8.2 btw 2004/2006).
  ###                 Conversely, GULF ratios were relatively static over time, being almost consistently estimated between
  ###                 0 and 1 and with no apparent indication of hitting any bounds (i.e., not 'excessively' large).
  ###                 The only exception to this is the Cbt-GULF ratio in 1988, which was ~4.8 ...
  ###   Looking back at Vivian's script for the S38U assessment
  ###               -- N:\FMB\SEDAR\SEDAR38U\Recreational\program.sas
  ###   ...TPWD discards were also imputed for Gulf king mackerel in S38 and S38U using the GULF ratios:
  ###           "estimate TPWD discards using MRFSS ratios by year, wave, and mode (Gulf-wide LA-FLW not including the Keys)"
  ###   ...which agrees with my assessment of the data and so the same approach will be applied in S99 as well...
  
  ###   ...where, from the above analysis, we identify what (if any) imputation to apply...
  # method.TPWD.B2 = 'None'
  # method.TPWD.B2 = 'la_ratio'
  method.TPWD.B2 = 'gu_ratio'
  
  ### ***************************************************
  
  
  if( method.TPWD.B2 != 'None' ) {
    
    catch.table = impute.TPWD.B2( genrec.table = catch.table, method = method.TPWD.B2 )
    
  }
  
  rm( dummy )
  
}


### ---------------------------------------------------------------------------------------------------------------- ###
### ---------------------------------------------------------------------------------------------------------------- ###



####################################
######     Assign StockID     ######
######       Boundaries       ######
####################################


source( paste0(dir,'/Functions/assign_stockID.R') )

catch.table <- assign.stockID( new.com = new.com, region = region, genrec.table = catch.table )


###   Similarly, I also add the SID field to any of the other catch tables that might have been produced above...
if( flag.unid ) {
  catch.table.unid <- assign.stockID( new.com = new.com, region = region, genrec.table = catch.table.unid )
}
if( flag.forhire ) {
  catch.table.forhire <- assign.stockID( new.com = new.com, region = region, genrec.table = catch.table.forhire )
}


### -----------------------------------------------------------------------------
###   ...where some stocks may require an additional filter to be applied here
###     (e.g., King Mackerel where the "mixing zone" boundaries change seasonally )...

if( new.com == "king mackerel" ) {
  if( region == "Gulf of America" ) {
    catch.table = catch.table %>% filter( SID == 'GULF' )
    if( flag.unid ) { catch.table.unid = catch.table.unid %>% filter( SID == 'GULF' ) }
    if( flag.forhire ) { catch.table.forhire = catch.table.forhire %>% filter( SID == 'GULF' ) }
  }
  if( region == "South Atlantic"  ) {
    catch.table = catch.table %>% filter( SID == 'ATL' )
    if( flag.unid ) { catch.table.unid = catch.table.unid %>% filter( SID == 'ATL' ) }
    if( flag.forhire ) { catch.table.forhire = catch.table.forhire %>% filter( SID == 'ATL' ) }
  }
}
### -----------------------------------------------------------------------------



### ---------------------------------------------------------------------------------------------------------------- ###
### ---------------------------------------------------------------------------------------------------------------- ###



########################################
######     Assign Open/Closed     ######
######       Federal Seasons      ######
########################################
###
###   ...for which assign.fishing.season( ) can be used if 'catch.table' already includes the fields needed to distinguish
###     open vs. closed fishing (e.g., based on WAVE ). Conversely, if the trip-level microdata is needed for these
###     assignments (e.g., differentiate open/closed at a daily level ), then the partition.fishing.season( ) function
###     will be needed. Note that both of these functions are saved in the same (R) workspace...

source( paste0(dir,'/Functions/assign_FishingSeason.R') )

if( flag.open.closed ) {
  
  # catch.table <- assign.fishing.season( new.com = new.com, region = region, genrec.table = catch.table )
  dummy <- partition.fishing.season( new.com = new.com, region = region, genrec.table = catch.table,
                                     method.LACR.B2 = method.LACR.B2, ratio.yrs.LACR.B2 = ratio.yrs.LACR.B2,
                                     method.TPWD.B2 = method.TPWD.B2 )
  # summary( as.factor( dummy$catch.table$fed_closed ) )
  # sum( catch.table$AB1, na.rm=TRUE ) - sum( dummy$catch.table$AB1, na.rm=TRUE )
  # sum( catch.table$B2 , na.rm=TRUE ) - sum( dummy$catch.table$B2 , na.rm=TRUE )
  
  catch.table = dummy$catch.table
  open.closed_ratios = dummy$part.factors
  rm( dummy )
  
  
  # if( flag.unid ) {
  #   catch.table.unid <- assign.fishing.season( new.com = new.com, region = region, genrec.table = catch.table.unid )
  #   # catch.table.unid <- partition.fishing.season( new.com = new.com, region = region, genrec.table = catch.table.unid )
  # }
  # 
  # if( flag.forhire ) {
  #   catch.table.forhire <- assign.fishing.season( new.com = new.com, region = region, genrec.table = catch.table.forhire )
  #   # catch.table.forhire <- partition.fishing.season( new.com = new.com, region = region, genrec.table = catch.table.forhire )
  # }
  
}


### ---------------------------------------------------------------------------------------------------------------- ###
### ---------------------------------------------------------------------------------------------------------------- ###



####################################
######     Assign Fishing     ######
######          Year          ######
####################################

source( paste0(dir,'/Functions/assign_FishingYear.R') )

catch.table <- assign.fyear( new.com = new.com, region = region, genrec.table = catch.table )


### ---------------------------------------------------------------------------------------------------------------- ###
### ---------------------------------------------------------------------------------------------------------------- ###


if(!exists("flag.cal.MRIPstate")) {
  flag.cal.MRIPstate <- FALSE
}

if (flag.cal.MRIPstate) {
  cal.factors <- extract.cal.ratio(new.com = new.com, region = region)
} else {
  cal.factors <- list()
}


###########################################
######     Calibrating State and     ######
######     MRIP FES/FHS estimates    ######
###########################################

source( paste0(dir,'/Functions/SECmodify_cal_MRIPstate.R') )

# blah = catch.table %>% group_by( DS ) %>%
#   summarize( AB1 = sum( AB1, na.rm=TRUE ),
#              B2  = sum(  B2, na.rm=TRUE ) )

if( flag.cal.MRIPstate ) {
  
 # cal.factors = extract.cal.ratio( new.com = new.com, region = region )
  
  for( i in 1:length(cal.factors) ) {
    
    ### LACR ###
    ### --------
    dummy.ratio = cal.factors[[i]][ which( grepl('lacr',names(cal.factors[[i]])) &
                                             !grepl('.EFF',names(cal.factors[[i]])) ) ]
    if( length(dummy.ratio) > 0 ) {
      if( toupper( names(cal.factors)[i] ) == 'PRIV' ) {
        dummy.mode = c( str_to_title(names(cal.factors)[i]), "Priv/Shore" )
      } else {
        dummy.mode = str_to_title(names(cal.factors)[i])
      }
      catch.table = calibrate.MRIPstate( DS.filter = 'LA Creel', mode.filter = dummy.mode,
                                         cal.ratios = dummy.ratio, genrec.table = catch.table )
      rm( dummy.mode )
    }
    rm( dummy.ratio )
    
    
    ### TPWD ###
    ### --------
    dummy.ratio = cal.factors[[i]][ which( grepl('tpwd',names(cal.factors[[i]])) &
                                             !grepl('.EFF',names(cal.factors[[i]])) ) ]
    dummy.mode = str_to_title(names(cal.factors)[i])
    catch.table = calibrate.MRIPstate( DS.filter = 'TPWD', mode.filter = dummy.mode,
                                       cal.ratios = dummy.ratio, genrec.table = catch.table )
    rm( dummy.ratio, dummy.mode )
    
  }
}

# blah = catch.table %>% group_by( DS ) %>%
#   summarize( AB1 = sum( AB1, na.rm=TRUE ),
#              B2  = sum(  B2, na.rm=TRUE ) )


### ---------------------------------------------------------------------------------------------------------------- ###
### ---------------------------------------------------------------------------------------------------------------- ###




# catch.summary <- catch.table %>%
#   group_by( YEAR ) %>%
#   summarize( AB1 = sum( as.numeric(AB1), na.rm=TRUE ),
#               B2 = sum( as.numeric( B2), na.rm=TRUE ) ) %>%
#   select( YEAR, AB1, B2 )

# catch.summary <- catch.table %>%
#   group_by( YEAR, NEW_STA ) %>%
#   summarize( AB1 = sum( as.numeric(AB1), na.rm=TRUE ),
#               B2 = sum( as.numeric( B2), na.rm=TRUE ) ) %>%
#   select( YEAR, NEW_STA, AB1, B2 ) %>%
#   pivot_wider( names_from=NEW_STA, values_from=c(AB1,B2) )

# catch.summary <- catch.table %>%
#   group_by( YEAR, NEW_MODEN ) %>%
#   summarize( AB1 = sum( as.numeric(AB1), na.rm=TRUE ),
#               B2 = sum( as.numeric( B2), na.rm=TRUE ) ) %>%
#   select( YEAR, NEW_MODEN, AB1, B2 ) %>%
#   pivot_wider( names_from=NEW_MODEN, values_from=c(AB1,B2) )

# catch.summary <- catch.table %>%
#   # filter( NEW_STA == "TX" ) %>%
#   filter( NEW_STA == "LA" ) %>%
#   group_by( NEW_COM, YEAR, NEW_MODEN ) %>%
#   summarize( AB1 = sum( as.numeric(AB1), na.rm=TRUE ),
#               B2 = sum( as.numeric( B2), na.rm=TRUE ) ) %>%
#   select( NEW_COM, YEAR, NEW_MODEN, AB1, B2 ) %>%
#   pivot_wider( names_from=NEW_MODEN, values_from=c(AB1,B2) )

# catch.summary <- catch.table %>%
#   # filter( NEW_MODEN %in% c('Priv','Priv/Shore') ) %>%
#   # group_by( YEAR, NEW_STA, fed_closed ) %>%
#   group_by( YEAR, SID, fed_closed ) %>%
#   summarize( AB1 = sum( as.numeric(AB1), na.rm=TRUE ),
#               B2 = sum( as.numeric( B2), na.rm=TRUE ) ) %>%
#   pivot_longer( cols = c('AB1','B2'), names_to = 'CAT_VAR' )
# dummy.plot = ggplot( data = catch.summary, aes( x=YEAR, y=value, fill=fed_closed ) ) +
#   geom_col( position = "stack", colour="black" ) +
#   # geom_bar( position = "fill", stat="identity", colour="black" ) +
#   # facet_grid( NEW_STA ~ CAT_VAR, scales='free' )
#   facet_grid( SID ~ CAT_VAR, scales='free' )
# dummy.plot






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
               B2 = sum( as.numeric( B2), na.rm=TRUE ) ) %>%
    # summarize( AB1 = sum( as.numeric(ab1), na.rm=TRUE ),
    #             B2 = sum( as.numeric( b2), na.rm=TRUE ) ) %>%
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


source( paste0(dir,'/Functions/calc_CVs_catnum.R') )


if( region == 'Caribbean' ) {
  # if( report.name == 'None' ) {
  
  
  ###################################
  ###     Caribbean Assessment    ###
  ###     -- use old approach     ###
  ###################################
  ###
  ###     ...in that for Caribbean assessments, for which the raw MRIP data files are still in the old i-file format,
  ###       there is no RDI report available from which to extract catch uncertainties (i.e., not available in RDI ).
  ###
  ###   For Caribbean SEDARs, we calculate catch CVs using the 'old' method, which assumes the MRIP-provided
  ###     'var_ab1' and 'var_b2' fields are additive across strata. These fields are available in the Carib_ACL catch file
  ###     and so, for this calculation, we either start with our 'catch.table' object or pull a new 'catch.table'
  ###     ( depending on whether CVs are to be calculated using a different subset of species, as identified by
  ###       the 'flag.cv' object -- see below ). Note that analysts are also frequently interested in the associated
  ###     sample sizes for these estimates, and so the functions below also include some metrics for sample size
  ###     ( as calculated from 'n.table' & 'pos.table' )...
  
  
  ###   If multiple species are included in "catch.table", a separate pull of catch estimates is needed
  ###     ( we cannot sum variances across species groups and so a new pull, for just one species, is needed )...
  ###
  if( flag.cv ) {
    cv.dat  = dat.filter(  acl.table = dat,                   spp.filter = nodc.cv,
                           yr.filter = first.year:term.year,  mode.filter = mode_sub,
                           reg.filter = region, sta.filter = states, fl.filter = FL_sub, nc.filter = NC_sub )
  } else {
    cv.dat = catch.table
  }
  
  cv.table = CVs.catnum.MRIP( Carib.SEDAR = TRUE, genrec.table = cv.dat, total.trips = n.table, pos.trips = pos.table )
  ###   ...where 'Carib.SEDAR' is a flag to identify CVs being estimated for a Caribbean SEDAR, the steps of which differ
  ###           from other SEDARs. These calculations shouldn't be needed anymore, as the Caribbean branch now does their
  ###           own data pulls, but it's left in the function for historical tracking...
  ###     The calculations for Carib SEDARs start with a new catch table, which is a imported (into the function)
  ###           as 'genrec.table'. Similar imports are needed for the tables of total and positive number of trips...
  
  rm( cv.dat )
  
  
  
} else {
  
  
  
  ##################################
  ###   GOM & ATL Assessments    ###
  ###     -- pull from RDI       ###
  ##################################
  ###
  ###     ...wherein I start by defining the modes-of-interest in this assesment, which I do to make sure
  ###           the appropriate information is being pulled into R (from RDI). This is done by looking at my
  ###           'mode_sub' object, to which I add the combined Cbt/Hbt mode for MATL/NATL assessments...
  
  if( ( ( "Cbt" %in% mode_sub ) | ( "Hbt" %in% mode_sub ) ) &
      any( c("VA","MD","DE","PA","NJ","NY","CT","RI","MA","NH","ME") %in% states ) ) {
    if( "Cbt" %in% mode_sub ) {
      cv.modes = append( mode_sub,"Cbt_Hbt", after=match("Cbt",mode_sub) )
    } else {
      cv.modes = append( mode_sub,"Cbt_Hbt", after=match("Hbt",mode_sub)-1 )
    }
  } else {    cv.modes = mode_sub    }
  
  
  
  ### MRIP CVs ###
  ### ------------
  
  if( !exists('method.MRIP.1981w1') ) {   method.MRIP.1981w1 = 'None'   }
  
  mrip.dummy = CVs.catnum.MRIP( rdi.report = report.name,
                                report.type = report.type,
                                inc.modes = cv.modes, genrec.table = catch.table,
                                imp.1981w1 = method.MRIP.1981w1,
                                flag.unid    = flag.unid,     catch.table.unid    = catch.table.unid,
                                flag.forhire = flag.forhire,  catch.table.forhire = catch.table.forhire,
                                loc.cv.forhire = NA,
                                loc.FH.ratios = 'C://Users/matthew.nuttall/Desktop/Functions/import_datasets/ForHire Partitioning Ratios.csv' )
  
  ###     ...where 'rdi.report' identifies the report to which the catch & CV estimates have been saved.
  ###             Note that for assessments with multiple SID boundaries, 'rdi.report' will be composed of multiple elements,
  ###             one report for each SID domain ( that should be ordered geographically, as arranged using NEW_ST ).
  ###             In this cases, 'mrip.dummy' will be a list with as many elements as there are unique SID domains...
  ###       'report.type' identifies the type of report built by RDI (i.e., "annual" provides CVs by year, year-mode, or year-state
  ###             while "detailed" provides CVs at a much finer resolution, namely by year-mode-state-wave ),
  ###
  ###       'genrec.table' is the (final) table of catch estimates constructed by this script (i.e., 'catch.table' ),
  ###             as needed to ensure the proper catch estimates are being used in particular steps of the
  ###             CV calculation process (e.g., when imputing MRIP estimates for 1981-wave1 ; imp.1981w1 != 'None' ),
  ###
  ###       'imp.1981w1' acts as a flag for SEDARs where 1981-wave1 (MRIP) catch estimates were imputed
  ###
  ###       'flag.unid' is a flag for SEDARs where we assign some percent of unidentified MRIP catch to the assessed species,
  ###       'catch.table.unid' is a previous iteration of our GenRec catch estimates table, constructed immediately after
  ###             some fraction of catch ( from an unidentified taxa ) was allocated to the species-of-interest, but before
  ###             any other adjustments were made (i.e., before breaking CBTHBT, imputing MRIP 1981-wave1, etc. )...
  ###
  ###       'flag.forhire' is a flag for SEDARs where we partitioned any MRIP estimates for the combined for-hire mode.
  ###             In these SEDARs, calculating the associated CVs for these partitioned (Cbt & Hbt) estimates requires...
  ###       'catch.table.forhire' is a previous iteration of our GenRec catch estimates table, constructed immediately before
  ###             MRIP estimates for the combined forhire mode (MATL/NATL, 1981-2003) were partitioned between CBT & HBT...
  ###       'loc.cv.forhire' -- identifies the position (in 'rdi.report') of the CV table containing estimates for the
  ###             combined forhire mode, which is only applicable to assessments with multiple SID domains,
  ###       'loc.FH.ratios' identifies the location of the spreadsheet of forhire partitioning ratios and their associated variances...
  
  
  ###   ...where the MRIP CV table(s) are converted into a long-format...
  if( length(report.name) > 1 ) {
    # if( 'SID' %in% colnames(catch.table) ) {
    mrip.dummy = lapply( cv.table = mrip.dummy, FUN = convert.long.table.cat, survey = 'MRIP', report.type = report.type )
    
  } else {
    mrip.dummy = convert.long.table.cat( cv.table = mrip.dummy, survey = 'MRIP', report.type = report.type )
  }
  
  
  ###   For SID assessments, we then assign names to 'mrip.dummy' using the unique values of SID in
  ###   'catch.table'. In particular, each of the CV-reports in 'report.name' are provided in geographic order
  ###   and so if we sort the SID domains in 'catch.table' (i.e., using the NEW_ST field ), we can apply this
  ###   sorting to our 'mrip.dummy' table to identify which CV report corresponds to which SID domain...
  
  if( length(report.name) > 1 ) {
    # if( 'SID' %in% colnames(catch.table) ) {
    
    dummy = catch.table %>%
      mutate( FL_REG = factor( FL_REG, levels = c(1,2,3,4,5,NA) ) ) %>%
      mutate( NC_REG = factor( NC_REG, levels = c('S','N',NA) ) ) %>%
      arrange( NEW_ST, FL_REG, NC_REG )
    SID.levels = unique( dummy$SID )
    rm(dummy)
    
    names(mrip.dummy) = SID.levels
    
    ###     Note that, in addition to naming each CV report, I also add a SID factor that will be needed to distinguish
    ###     catch & CV estimates (regionally) once all estimates have been joined into a single table...
    for( i in 1:length(mrip.dummy) ) {
      mrip.dummy[[i]] = mrip.dummy[[i]] %>%
        mutate( SID = names(mrip.dummy)[i] ) %>%
        mutate( SID = factor( SID, levels = SID.levels ) ) %>%
        select( SID, YEAR, NEW_MODEN, CATCH_VAR, METRIC, value )
    }
    rm( i )
  }
  
  
  ###   ...and, as a last step for assessments that differentiate catch between open vs. closed fishing seasons,
  ###     I apply the same CVs estimated above ( for combined catch = open + closed ) to catches specific to
  ###     each fishing season, using %catches extracted from 'catch.table' to split 'CAT' b/w open vs. closed
  ###     ( the catches in 'catch.table' have already been partitioned into open vs. closed season fishing )...
  
  if( flag.open.closed ) {
    source( paste0(dir,'/Functions/assign_FishingSeason.R') )
    mrip.dummy = cv.fishing.season( cv.table = mrip.dummy, genrec.table = catch.table, DS.filter = 'MRIP' )
  }
  
  
  
  ### LACR CVs ###
  ### ------------
  
  if( "LA" %in% states ) {
    
    if( method.LACR.B2 == 'la_ratio' ) {      mrip.cv.table =  LA.report.name    }
    if( method.LACR.B2 == 'gu_ratio' ) {      mrip.cv.table = GOM.report.name    }
    
    if( flag.cal.MRIPstate &
        length(which(grepl( 'lacr', as.vector( unlist( lapply( cal.factors, names ) ) ) ))) > 0 ){
      
      for( i in 1:length(cal.factors) ) {
        ###   ...which cycles through all of the 'cal.factors' provided for each of the (i) modes...
        
        dummy.ratio = cal.factors[[i]][ which( grepl('lacr',names(cal.factors[[i]])) &
                                                 !grepl('.EFF',names(cal.factors[[i]])) ) ]
        dummy.mode = names(cal.factors)[i]
        dummy.mode = c( dummy.mode, str_to_title(dummy.mode) )
        if( 'Priv' %in% dummy.mode ) {
          dummy.mode = c( dummy.mode, "PRIV/SHORE","Priv/Shore" )
        }
        lacr.dummy = CVs.catnum.LACR( report.type = report.type,
                                      itis.code = itis.code, inc.modes = cv.modes, inc.years = 2014:term.year,
                                      cal.ratios = dummy.ratio, mode.filter = dummy.mode,
                                      imp.LACR.B2 = method.LACR.B2, calc.ratios.from = ratio.yrs.LACR.B2,
                                      mrip.cv.table =  mrip.cv.table,
                                      attach.samplesize = TRUE )
        rm( dummy.ratio, dummy.mode )
        ###   Note that, as in the script above, the state:MRIP calibration factors in 'dummy.ratio' are
        ###   applied as the last step in reproducing the CAT & CV estimates in 'lacr.dummy'...
        
      }
      rm( i )
      
    } else {
      lacr.dummy = CVs.catnum.LACR( report.type = report.type,
                                    itis.code = itis.code, inc.modes = cv.modes, inc.years = 2014:term.year,
                                    imp.LACR.B2 = method.LACR.B2, calc.ratios.from = ratio.yrs.LACR.B2,
                                    mrip.cv.table =  mrip.cv.table )
    }
    
    if( method.LACR.B2 != 'None' ) {    rm( mrip.cv.table )    }
    
    
    lacr.cv.tpwdB2 = lacr.dummy[[2]]
    ###     ...where raw estimates of LACR may be needed to impute TPWD discards ( before any MRIP:state calibration factors
    ###       are applied ) and so I made the raw estimates a separate output of the above function in case they're needed
    ###       (i.e., LA-specific B2:AB1 discard ratios used to impute TPWD discards )...
    
    lacr.dummy = lacr.dummy[[1]]
    
    if( "NEW_STA" %notin% colnames(lacr.dummy) ){
      lacr.dummy = lacr.dummy %>% mutate( NEW_STA = "LA" )
    }
    
    ###     ...where the LACR cv table is converted into a long-format...
    lacr.dummy = convert.long.table.cat( lacr.dummy, report.type = report.type, survey = 'LACR' )
    
    
    ###   For SID assessments, I then add a field to identify the SID domain to which LACR belongs...
    if( length(report.name) > 1 ) {
      # if( 'SID' %in% colnames(catch.table) ) {
      
      ###   ...which is done by identifying which element in 'report.name' contains estimates from Louisiana...
      for( j in 1:length(report.name) ) {
        
        con = dbConnect(dbDriver("Oracle"), username = keyring::key_list("SECPR")[1,2],
                        password = keyring::key_get("SECPR", keyring::key_list("SECPR")[1,2]), dbname = "SECPR")
        
        mrip.state = dbGetQuery(con,
                                paste0("select * ",
                                       "from rdi.apex_cv_data_yr_s@secapxdv_dblk.sfsc.noaa.gov t
                                        where t.APP_USER = ", sprintf("'%s'", paste( report.name[j], collapse = "','" ))
                                ))
        mrip.state = mrip.state %>% select( -c('APP_USER','YEAR') )
        mrip.state = unique( gsub( '_.*','', colnames(mrip.state)[ colSums(mrip.state,na.rm=TRUE) > 0 ] ) )
        
        if( 'LA' %in% mrip.state ) {
          loc.cv.LACR = j
          break
        }
        rm( mrip.state )
      }
      rm( j )
      
      lacr.dummy = lacr.dummy %>%
        mutate( SID = names(mrip.dummy)[loc.cv.LACR] ) %>%
        mutate( SID = factor( SID, levels = SID.levels ) ) %>%
        ###   ...where 'SID.levels' was defined (above) for the MRIP CV report...
        select( any_of( c('SID','YEAR','NEW_MODEN','CATCH_VAR','METRIC','value') ) )
      rm( loc.cv.LACR )
    }
    
    
    ###   ...and as a last step, I differentiate any catch between open vs. closed fishing seasons
    ###     for those assessments that consider such regulations...
    if( flag.open.closed ) {
      source( paste0(dir,'/Functions/assign_FishingSeason.R') )
      lacr.dummy = cv.fishing.season( cv.table = lacr.dummy, genrec.table = catch.table, DS.filter = 'LA Creel' )
    }
    
  }
  
  
  
  ### TPWD CVs ###
  ### ------------
  
  if( "TX" %in% states ) {
    
    if( method.TPWD.B2 == 'la_ratio' ) {      genrec.cv.table = list(  LA.report.name, lacr.cv.tpwdB2 )    }
    if( method.TPWD.B2 == 'gu_ratio' ) {      genrec.cv.table = list( GOM.report.name, lacr.cv.tpwdB2 )    }
    
    if( flag.cal.MRIPstate &
        length(which(grepl( 'tpwd', as.vector( unlist( lapply( cal.factors, names ) ) ) ))) > 0  ){
      
      for( i in 1:length(cal.factors) ) {
        dummy.ratio = cal.factors[[i]][ which( grepl('tpwd',names(cal.factors[[i]])) &
                                                 !grepl('.EFF',names(cal.factors[[i]])) ) ]
        dummy.mode = names(cal.factors)[i]
        dummy.mode = c( dummy.mode, str_to_title(dummy.mode) )
        tpwd.dummy = CVs.catnum.TPWD( report.type = report.type,
                                      tpwd.code = tpwd.code, inc.modes = cv.modes, inc.years = 1983:term.year,
                                      cal.ratios = dummy.ratio, mode.filter = dummy.mode,
                                      genrec.cat.table = catch.table,
                                      imp.TPWD.8183 = method.TPWD.1981.83,
                                      imp.TPWD.B2 = method.TPWD.B2, genrec.cv.table = genrec.cv.table,
                                      attach.samplesize = TRUE )
        rm( dummy.ratio, dummy.mode )
        ###   Note that, as in the script above, the state:MRIP calibration factors in 'dummy.ratio' are
        ###   applied as the last step in reproducing the CAT & CV estimates in 'tpwd.dummy'...
      }
      rm( i )
      
      
    } else {
      tpwd.dummy = CVs.catnum.TPWD( report.type = report.type,
                                    tpwd.code = tpwd.code, inc.modes = cv.modes, inc.years = 1983:term.year,
                                    genrec.cat.table = catch.table,
                                    imp.TPWD.8183 = method.TPWD.1981.83,
                                    imp.TPWD.B2 = method.TPWD.B2, genrec.cv.table = genrec.cv.table )
    }
    
    rm( lacr.cv.tpwdB2 )
    if( method.TPWD.B2 != 'None' ) {    rm( genrec.cv.table )    }
    
    if( "NEW_STA" %notin% colnames(tpwd.dummy) ){
      tpwd.dummy = tpwd.dummy %>% mutate( NEW_STA = "TX" )
    }
    
    ###     ...where the TPWD cv table is converted into a long-format...
    tpwd.dummy = convert.long.table.cat( tpwd.dummy, report.type = report.type, survey = 'TPWD' )
    
    
    ###   For SID assessments, I then add a field to identify the SID domain to which TPWD belongs...
    if( length(report.name) > 1 ) {
      # if( 'SID' %in% colnames(catch.table) ) {
      
      ###     Note that this can't be done using the same approach as was applied for LACR estimates, which referenced
      ###     the RDI CV reports (see above), because MRIP has never provided data/estimates for Texas
      ###     ( and so TX isn't included in any RDI CV report ). Instead, we identify the approriate SID domain
      ###     ( for TX ) by looking at 'catch.table' and identifying the 'SID' domain to which TX has been assigned...
      
      SID.TPWD = unique( catch.table$SID[ catch.table$NEW_STA == 'TX' ] )
      loc.cv.TPWD = which( names(mrip.dummy) == SID.TPWD )
      rm( SID.TPWD )
      
      tpwd.dummy = tpwd.dummy %>%
        mutate( SID = names(mrip.dummy)[loc.cv.TPWD] ) %>%
        mutate( SID = factor( SID, levels = SID.levels ) ) %>%
        ###   ...where 'SID.levels' was defined (above) for the MRIP CV report...
        select( SID, YEAR, NEW_MODEN, CATCH_VAR, METRIC, value )
      rm( loc.cv.TPWD )
    }
    
    
    ###   ...and as a last step, I differentiate any catch between open vs. closed fishing seasons
    ###     for those assessments that consider such regulations...
    if( flag.open.closed ) {
      source( paste0(dir,'/Functions/assign_FishingSeason.R') )
      tpwd.dummy = cv.fishing.season( cv.table = tpwd.dummy, genrec.table = catch.table, DS.filter = 'TPWD' )
    }
    
  }
  
  
  # if( 'SID' %in% colnames(catch.table) ) {     rm( SID.levels )    }
  
  
  
  
  ###   JOIN   ###
  ### ------------
  
  
  ### MRIP ###
  
  cv.table = mrip.dummy
  # rm( mrip.dummy )
  
  
  
  ### LACR + (MRIP) ###
  
  if( 'LA' %in% states ) {
    
    if( 'SID' %in% colnames(catch.table) ) {
      loc.cv.LACR = which( names(mrip.dummy) == unique(lacr.dummy$SID) )
      blah = cv.table[[loc.cv.LACR]]
    } else {
      blah = cv.table
    }
    
    dummy = bind_rows( blah, lacr.dummy ) %>%
      group_by( across( any_of( c('SID','YEAR','NEW_MODEN','fed_closed','CATCH_VAR','METRIC') ) ) ) %>%
      summarise( value = sum( value, na.rm=TRUE ) )
    rm( blah )
    
    if( 'SID' %in% colnames(catch.table) ) {
      cv.table[[loc.cv.LACR]] = dummy
      rm( dummy, loc.cv.LACR )
    } else {
      cv.table = dummy
      rm( dummy )
    }
  }
  
  
  
  ### TPWD + (MRIP+LACR) ###
  
  if( 'TX' %in% states ) {
    
    if( 'SID' %in% colnames(catch.table) ) {
      loc.cv.TPWD = which( names(mrip.dummy) == unique(tpwd.dummy$SID) )
      blah = cv.table[[loc.cv.TPWD]]
    } else {
      blah = cv.table
    }
    
    dummy = bind_rows( blah, tpwd.dummy ) %>%
      group_by( across( any_of( c('SID','YEAR','NEW_MODEN','fed_closed','CATCH_VAR','METRIC') ) ) ) %>%
      summarise( value = sum( value, na.rm=TRUE ) )
    rm( blah )
    
    if( 'SID' %in% colnames(catch.table) ) {
      cv.table[[loc.cv.TPWD]] = dummy
      rm( dummy, loc.cv.TPWD )
    } else {
      cv.table = dummy
      rm( dummy )
    }
  }
  
  
  
  ### FINAL FORMATTING ###
  ### --------------------
  
  ###   ...wherein I remove any modes from which no sampling was conducted, which can occur (for example) in SEDAR assessments
  ###       that include both SID domains and the combined for-hire mode (e.g., there are no 'CBTHBT' estimates associated with
  ###       with the GOM or SATL regions and so 'CBTHBT' should be dropped from any CV tables specific to these regions )...
  if( 'SID' %in% colnames(catch.table) ) {
    for( i in 1:length(cv.table) ) {
      
      blah = cv.table[[i]] %>%
        group_by( NEW_MODEN ) %>%
        summarize( PSU = sum( value[ CATCH_VAR == 'TOTAL' & METRIC == 'PSU' ], na.rm=TRUE ),
                   AT = sum( value[ CATCH_VAR == 'TOTAL' & METRIC ==  'AT' ], na.rm=TRUE ) )
      null.modes = blah$NEW_MODEN[ rowSums( blah[,c('PSU','AT') ] ) == 0 ]
      rm( blah )
      
      cv.table[[i]] = cv.table[[i]] %>% filter( NEW_MODEN %notin% null.modes )
      rm( null.modes )
    }
    rm( i )
    
  } else {
    ###   Similarly, this can also happen when 'cv.table' is looking at a subset of the region-wide stock boundaries...
    
    blah = cv.table %>%
      group_by( NEW_MODEN ) %>%
      summarize( PSU = sum( value[ CATCH_VAR == 'TOTAL' & METRIC == 'PSU' ], na.rm=TRUE ),
                 AT = sum( value[ CATCH_VAR == 'TOTAL' & METRIC ==  'AT' ], na.rm=TRUE ) )
    null.modes = blah$NEW_MODEN[ rowSums( blah[,c('PSU','AT') ] ) == 0 ]
    rm( blah )
    
    cv.table = cv.table %>% filter( NEW_MODEN %notin% null.modes )
    rm( null.modes )
  }
  
  
  ###   For SID assessments, I then collapse the multiple elements of 'cv.table' into a single table...
  if( length(report.name) > 1 ) {    cv.table = do.call( rbind, cv.table )    }
  
  
  cv.table = cv.table %>%
    
    ###   ...format 'METRIC' as a factor to control the order with which values are to be displayed...
    mutate( METRIC = factor( METRIC, levels = c('CAT','VAR','AT','PSU') ) ) %>%
    
    ###   ...and add a numeric NEW_MODE field so that estimates can be properly sorted in the
    ###       final pivot table ( in the final GenRec size file )...
    mutate( NEW_MODE = ifelse( NEW_MODEN ==     'SHORE', 1,
                               ifelse( NEW_MODEN ==       'HBT', 2,
                                       ifelse( NEW_MODEN ==       'CBT', 3,
                                               ifelse( NEW_MODEN ==      'PRIV', 4,
                                                       ifelse( NEW_MODEN ==    'CBTHBT', 5,
                                                               ifelse( NEW_MODEN == 'PRIVSHORE', 6,
                                                                       ifelse( NEW_MODEN ==     'TOTAL', 99, NA ))))))) )
  
  
  ###   ...making sure the final 'cv.table' includes ALL years, whether estimates exist or not...
  
  dummy = list( YEAR = seq( first.year, term.year, by=1 ) )
  if( 'SID' %in% colnames(catch.table) ) {   dummy$SID = unique(catch.table$SID)   }
  if( flag.open.closed ) {   dummy$fed_closed = unique(catch.table$fed_closed)   }
  dummy = expand.grid( dummy )
  
  cv.table = cv.table %>%
    full_join( dummy, by=colnames(dummy) ) %>%
    arrange( across( colnames(dummy) ) )
  rm(dummy)
  
  cv.table = cv.table %>%
    select( any_of( c('SID','YEAR','fed_closed','NEW_MODE','NEW_MODEN','NEW_STA','WAVE',
                      'CATCH_VAR','METRIC','value') ) )
  
  
  
  ### Replace catch values ( in 'cv.table' ) with those from 'catch.table'...
  groupby.cols = colnames(cv.table)[ which( colnames(cv.table) %in%
                                              c('SID','YEAR','fed_closed','NEW_MODE','NEW_MODEN','NEW_STA','WAVE') ) ]
  
  ###     ...mode-specific estimates...
  blah1 = catch.table %>%
    mutate( NEW_MODEN = toupper(NEW_MODEN) ) %>%
    mutate( NEW_MODE  = ifelse( NEW_MODE  == 6, 4, NEW_MODE ),
            NEW_MODEN = ifelse( NEW_MODEN == "PRIV/SHORE", "PRIV", NEW_MODEN ) ) %>%
    # group_by( across( any_of( c('SID','YEAR','fed_closed','NEW_MODE','NEW_MODEN','NEW_STA','WAVE',
    #                             'CATCH_VAR','METRIC','value') ) ) ) %>%
    group_by( across( all_of( groupby.cols ) ) ) %>%
    summarize( AB1 = sum( AB1, na.rm=TRUE ),
               B2  = sum(  B2, na.rm=TRUE ) ) %>%
    ungroup()
  
  ###     ...total (annual) estimates...
  blah2 = catch.table %>%
    # group_by( across( any_of( c('SID','YEAR','fed_closed','NEW_STA','WAVE',
    #                             'CATCH_VAR','METRIC','value') ) ) ) %>%
    group_by( across( all_of( groupby.cols[ groupby.cols %notin% c('NEW_MODE','NEW_MODEN') ] ) ) ) %>%
    summarize( AB1 = sum( AB1, na.rm=TRUE ),
               B2  = sum(  B2, na.rm=TRUE ) ) %>%
    ungroup() %>%
    mutate( NEW_MODE  = 99,
            NEW_MODEN = 'TOTAL' )
  blah = bind_rows( blah1, blah2 )
  rm( blah1, blah2 )
  
  
  blah = blah %>%
    pivot_longer( cols=c('AB1','B2'), names_to = 'CATCH_VAR', values_to = 'value' ) %>%
    mutate( METRIC = 'CAT' )
  
  groupby.cols = c( groupby.cols,'CATCH_VAR','METRIC' )
  # join.vec = colnames(cv.table)[ colnames(cv.table) %in% c('SID','YEAR','fed_closed','NEW_MODE','NEW_MODEN',
  #                                                          'NEW_STA','WAVE','CATCH_VAR','METRIC') ]
  dummy = cv.table %>% left_join( blah, by=groupby.cols )
  rm( groupby.cols, blah )
  
  # sum( dummy$value.x, na.rm=TRUE ) - sum( dummy$value.y, na.rm=TRUE )
  # 
  # blah = dummy %>% mutate( DIFF = value.x - value.y )
  # summary(as.numeric(blah$DIFF))
  # rm( blah )
  
  cv.table = dummy %>%
    mutate( value.x = ifelse( is.na(value.y), value.x, value.y ) ) %>%
    rename( value = value.x ) %>%
    select( -value.y )
  rm( dummy )
  
}







####################################################################################################################
####################################################################################################################
####################################################################################################################
#############################                                                          #############################
#############################               MRIP CVs -- Landings-in-Weight             #############################
#############################                                                          #############################
####################################################################################################################
####################################################################################################################
####################################################################################################################


source( paste0(dir,'/Functions/calc_CVs_catwgt.R') )


### APPROACH #1 ###
# 
# dummy = CVs.landwgt( approach = 1, avgwgt.dir = avgwgt.dir )



### APPROACH #2 ###

num.table = cv.table
wgt.table = read_excel( path=paste0( "C:/Users/matthew.nuttall/Desktop/",sedar.size.file ),
                        sheet="Weight Summary by Mode", trim_ws=FALSE, col_types="text" )

###   ...and a quick check that we're including the same modes ( in 'wgt.table' ) as in 'num.table', which
###     may not be the case if the catch file being constructed is a subset of the regional stock. For example,
###     a catch file specific to FLORIDA to allow FWC to (back)calculate a SRFS timeseries from historic MRIP...
wgt.table = wgt.table %>% filter( NEW_MODEN %in% unique(num.table$NEW_MODEN) )

lbs.cv.table.2 = CVs.landwgt( approach = 2, catch.table = catch.table, num.table = num.table, wgt.table = wgt.table )
rm( num.table, wgt.table )



### Replace catch values ( in 'lbs.cv.table' ) with those from 'catch.table'...
groupby.cols = colnames(cv.table)[ which( colnames(cv.table) %in%
                                            c('SID','YEAR','fed_closed','NEW_MODE','NEW_MODEN','NEW_STA','WAVE') ) ]

blah1 = catch.table %>%
  mutate( NEW_MODEN = toupper(NEW_MODEN) ) %>%
  mutate( NEW_MODE  = ifelse( NEW_MODE  == 6, 4, NEW_MODE ),
          NEW_MODEN = ifelse( NEW_MODEN == "PRIV/SHORE", "PRIV", NEW_MODEN ) ) %>%
  # group_by( across( any_of( c('SID','YEAR','fed_closed','NEW_MODE','NEW_MODEN','NEW_STA','WAVE',
  #                             'CATCH_VAR','METRIC','value') ) ) ) %>%
  group_by( across( all_of( groupby.cols ) ) ) %>%
  summarize( LBS = sum( lbsest_SECwwt, na.rm=TRUE ) ) %>%
  ungroup() %>%
  rename_all( ~toupper(.) )

blah2 = catch.table %>%
  # group_by( across( any_of( c('SID','YEAR','fed_closed','NEW_STA','WAVE',
  #                             'CATCH_VAR','METRIC','value') ) ) ) %>%
  group_by( across( all_of( groupby.cols[ groupby.cols %notin% c('NEW_MODE','NEW_MODEN') ] ) ) ) %>%
  summarize( LBS = sum( lbsest_SECwwt, na.rm=TRUE ) ) %>%
  ungroup() %>%
  mutate( NEW_MODE  = 99,
          NEW_MODEN = 'TOTAL' ) %>%
  rename_all( ~toupper(.) )
blah = bind_rows( blah1, blah2 )
rm( blah1, blah2 )


# join.vec = colnames(lbs.cv.table.2)[
#   colnames(lbs.cv.table.2) %in% c('SID','YEAR','FED_CLOSED','NEW_MODE','NEW_MODEN','NEW_STA','WAVE') ]
dummy = lbs.cv.table.2 %>% left_join( blah, by=groupby.cols )
rm( groupby.cols, blah )

lbs.cv.table.2 = dummy %>%
  mutate( LBS_CAT = ifelse( is.na(LBS), LBS_CAT, LBS ) ) %>%
  select( -LBS )
rm( dummy )










####################################################################################################################
####################################################################################################################
####################################################################################################################
#############################                                                          #############################
#############################                   FINAL EXCEL WORKBOOK                   #############################
#############################                                                          #############################
####################################################################################################################
####################################################################################################################
####################################################################################################################


table.ID <- paste0( "KM_rec_catGEN_",
                    substr( first.year, nchar(first.year)-1, nchar(first.year) ),
                    substr( term.year, nchar(term.year)-1, nchar(term.year) ),
                    "_", gsub("-","", Sys.Date() ) )

if( flag.unid ) {   tab.unid.dat <- paste( "BALISTIDAE", "_rec_catGEN" )   }


### _______________________________________________________________________________________


### IMPORT EXCEL TEMPLATE ###
### -------------------------
wb <- loadWorkbook( file=paste0( dir,"/Template_SEDAR_GenCatch_fromACL.xlsx" ) )


### General Catch Estimates ###
### ---------------------------
removeWorksheet( wb, sheet="SNWY_REC_CATGEN_8118_20200210" )
###     ...which corresponds to the GenCatch estimates provided for SEDAR 36U snowy, from which the GenCatch.xlsx template was constructed...
addWorksheet( wb, sheet=table.ID )
writeData( wb, sheet=table.ID, x=catch.table, colNames=TRUE )


### Comparisons to Previous SEDAR ###
### ---------------------------------
if( prev.sedar != "None" ) {
  writeData( wb, sheet="Compare Previous SEDARs", x=sedar.comparison, colNames=TRUE )
  
  ###   Note that I also considered renaming the 'Compare Previous SEDAR' tab to something specific to the individual assessment
  ###   (e.g., 'Compare with SEDAR xx' ), which was done using the renameWorksheet() function below...
  ### #       renameWorksheet( wb, sheet="Compare Previous SEDARs", newName=paste0("Compare with ",gsub( "EDAR ", "", prev.sedar )) )
  ###   However, this (dynamic) renaming was screwing up the automatic updates to my excel figures (on the comparison tab) because
  ###   these figures are looking to plot values found on the old tab name (i.e., 'Compare Previous SEDARs' ). I retain the above
  ###   renameWorksheet() statement in case this type of naming is something that's requested in the future but (for automation purposes)
  ###   the generic tab name is being used in the current script...
} else {
  removeWorksheet( wb, sheet="Compare Previous SEDARs" )
}


### MRIP CVs -- Number ###
### ----------------------
###     ...which, for SID assessments, was originally provided as tables on separate excel tabs
###       (i.e., one tab for each unique SID domain )...
### 
### # if( paste0( stockID, collapse = ' ' ) == 'None' ) {
### #   
### #   writeData( wb, sheet="MRIP catCV numbers", x=cv.table, colNames=TRUE )
### #   
### # } else {
### #   
### #   SID.dummy = unique(stockID)
### #   ###     ...where, by design, the 'stockID' variable identifies each SID boundary in geographic order.
### #   ###         Therefore, these tabs will also be geographically ordered in the constructed GenRec size xsheet...
### #   
### #   for( i in 1:length(SID.dummy) ) {
### #     
### #     dummy.table = cv.table[[ SID.dummy[i] ]]
### #     
### #     cloneWorksheet( wb,
### #                     sheetName = paste0( toupper(SID.dummy[i]),"_catCV num" ),
### #                     clonedSheet = "MRIP catCV numbers" )
### #     writeData( wb, sheet=paste0( toupper(SID.dummy[i]),"_catCV num" ), x=dummy.table, colNames=TRUE )
### #     
### #   }
### #   removeWorksheet( wb, sheet="MRIP catCV numbers" )
### #   
### #   rm( SID.dummy, i, dummy.table )
### #   
### # }
### 
###   However, for automation purposes, these estimates are now being provided in a single (long-format) table,
###   with a separate SID field added to distinguish estimates from different regions. In preparing this table
###   for output (i.e., writing values to the CV tab in the final GenRec catch file ), I:
###         -- modify the total sample size records ( CATCH_VAR == 'TOTAL' ) so that they will be displayed
###               alongside both the AB1 and B2 estimates in the final pivot table below (i.e., so that the
###               positive and total number of trips can be compared within the same row/record )...
###         -- convert the 'VAR' field into its associated 'CV' estimates...
###         -- pivot the 'METRIC' field into distinct columns ( keeping YEAR/SID/MODE/CATCH_VAR values as unique rows )

cv.dummy = cv.table %>%
  ungroup() %>%
  mutate( METRIC = as.character(METRIC) ) %>%
  mutate( VARIABLE = paste0( CATCH_VAR,"_",METRIC ) ) %>%
  mutate( VARIABLE = ifelse( VARIABLE == 'TOTAL_AT' , 'ATtotal',
                             ifelse( VARIABLE == 'TOTAL_PSU', 'PSUtotal', VARIABLE )) )

blah.AB1 = cv.dummy %>%
  filter( VARIABLE %in% c('ATtotal','PSUtotal') ) %>%
  mutate( CATCH_VAR = 'AB1' ) %>%
  mutate(    METRIC = VARIABLE )
blah.B2  = blah.AB1 %>% mutate( CATCH_VAR = 'B2' )

cv.dummy = cv.dummy %>%
  filter( !grepl( 'ATtotal',VARIABLE ) & !grepl( 'PSUtotal',VARIABLE ) ) %>%
  bind_rows( blah.AB1 ) %>%
  bind_rows( blah.B2  ) %>%
  select( -VARIABLE ) %>%
  
  # mutate( METRIC = factor( METRIC, levels = c('CAT','VAR','AT','PSU') ) ) %>%
  pivot_wider( names_from=METRIC, values_from=value ) %>%
  mutate( CV = ifelse( CAT == 0 , 0, sqrt(VAR) / CAT ) )
rm( blah.AB1, blah.B2 )


cv.dummy = cv.dummy %>%
  select( any_of( c('SID','YEAR','fed_closed','NEW_MODE','NEW_MODEN','NEW_STA','WAVE','CATCH_VAR',
                    'CAT','CV','AT','ATtotal','PSU','PSUtotal') ) ) %>%
  arrange( across( any_of( c('SID','YEAR','fed_closed','NEW_MODE','CATCH_VAR') ) ) )

dummy.offset = 3
if( 'SID' %in% colnames(cv.dummy) ) { dummy.offset = dummy.offset - 1 }
if( 'fed_closed' %in% tolower(colnames(cv.dummy)) ) { dummy.offset = dummy.offset - 1 }
###     ...where an offset is added to print the 'cv.dummy' table based on whether the 'SID' and
###       'fed_closed' fields are included in 'cv.dummy', both of which are identified in the template...

writeData( wb, sheet="MRIP catCV numbers", startCol = dummy.offset, x=cv.dummy, colNames=TRUE )
rm( dummy.offset )

rm(cv.dummy)



### MRIP landWGT CVs #1 ###
### -----------------------
###       ...where the landings-in-weight estimates from Approach #1 are no longer being used
###           ( in favor of those from Approach #2 ). However, in case comparisons are requested
###           between the two approaches (i.e., in future SEDARs ), these can be written using...
###
### #       writeData( wb, sheet="MRIP landCV weight #1", x=cv.wgt.table.1, colNames=TRUE )
###
###       Until that time, however, I simply remove the 'approach #1' tab from the final catch table...

removeWorksheet( wb, sheet="MRIP landCV weight #1" )



### MRIP landWGT CVs #2 ###
### -----------------------
### 
### # if( paste0( stockID, collapse = ' ' ) == 'None' ) {
### #   
### #   cloneWorksheet( wb, sheetName = "MRIP landCV weight", clonedSheet = "MRIP landCV weight #2" )
### #   removeWorksheet( wb, sheet="MRIP landCV weight #2" )
### #   writeData( wb, sheet="MRIP landCV weight", x=lbs.cv.table.2, colNames=TRUE )
### #   
### # } else {
### #   
### #   SID.dummy = unique(stockID)
### #   ###     ...where, by design, the 'stockID' variable identifies each SID boundary in geographic order.
### #   ###         Therefore, these tabs will also be geographically ordered in the constructed GenRec size xsheet...
### #   
### #   for( i in 1:length(SID.dummy) ) {
### #     
### #     # dummy.table = lbs.cv.table.2 %>% filter( SID == SID.dummy[i] ) %>% select( -SID )
### #     dummy.table = lbs.cv.table.2[[ SID.dummy[i] ]]
### #     
### #     cloneWorksheet( wb,
### #                     sheetName = paste0( toupper(SID.dummy[i]),"_landCV weight" ),
### #                     clonedSheet = "MRIP landCV weight #2" )
### #     writeData( wb, sheet=paste0( toupper(SID.dummy[i]),"_landCV weight" ), x=dummy.table, colNames=TRUE )
### #     
### #   }
### #   removeWorksheet( wb, sheet="MRIP landCV weight #2" )
### #   
### #   rm( SID.dummy, i, dummy.table )
### #   
### # }

dummy.offset = 3
if( 'SID' %in% colnames(lbs.cv.table.2) ) { dummy.offset = dummy.offset - 1 }
if( 'fed_closed' %in% tolower(colnames(lbs.cv.table.2)) ) { dummy.offset = dummy.offset - 1 }

cloneWorksheet( wb, sheetName = "MRIP landCV weight", clonedSheet = "MRIP landCV weight #2" )
removeWorksheet( wb, sheet="MRIP landCV weight #2" )
writeData( wb, sheet="MRIP landCV weight", startCol = dummy.offset, x=lbs.cv.table.2, colNames=TRUE )
rm( dummy.offset )



### UNIDENTIFIED CATCH ###
### ----------------------
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
  
  
} else {
  removeWorksheet( wb, sheet="UNID_rec_catGEN" )
  removeWorksheet( wb, sheet="UNID_ratios" )
}


saveWorkbook( wb, file=paste0( dir,"/",table.ID,"_ACL.xlsx" ), overwrite=TRUE )



