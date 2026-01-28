


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

dat <- read_sas(data_file = paste0( dir,"/Catch/hmsspec_rec81_25wv1_16may25.sas7bdat"))
### # save.image( file=paste0( dir,"/ACL_catch.RData" ) )

#load( paste0( dir,"/ACL_catch.RData" ) )


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
#############################                       SEDAR 101                           #############################
#############################                      HMS Sandbar Shark                #############################
#############################                                                          #############################
####################################################################################################################
####################################################################################################################
####################################################################################################################


###       ...for this assessment, data is pulled for...
###
###           Sandbar Shark - Carcharhinus plumbeus
###               - Temporal - include 1981-2024
###               - Modes    - includes charter, private, shore, and headboat 
###               - Spatial  - ME to TX





#############################
######     FILTERS     ######
#############################



current.sedar <- "SEDAR 101"

prev.sedar <- "None"
#prev.sedar <- "SEDAR 62"
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

states <- c("TX","LA","MS","AL","FLW","FLE","GA","SC","NC","VA","MD","DE","PA","NJ","NY","CT","RI","MA","NH","ME")
###     ...which has options c( "TX","LA","MS","AL","FLW","FLE","GA","SC","NC","VA","MD","DE","PA","NJ","NY","CT","RI","MA","NH","ME" )
###     ...or c( "PR","VI" ) for Caribbean assessments...
if( "FL" %in% states | "FLW" %in% states | "FLE" %in% states ) {  FL_sub <- c( 1,2,3,4,5 )  }
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
View( spp.info[grep( "SHARK,SANDBAR", spp.info$COMMON ),] )
#View( spp.info[grep( "Carcharhinus", spp.info$SCIENTIFIC ),] )

sppID.type = 'COMMON'
taxa <- c( "SHARK,SANDBAR" )

# sppID.type = 'SCIENTIFIC'
# taxa <- c( "Lutjanus griseus"  )

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
 report.name = '1728_SAMANTHA.BINION-ROCK@NOAA.GOV_2_280817'
###
###       ...but multiple entries when individual 'SID' boundaries are defined for this stock
###         (i.e., one element for each SID domain ). Note that the order in which CV-reports are
###         defined in 'report.name' should align with their geographic order (i.e., as per NEW_ST )...
#report.name = c( '1702_SAMANTHA.BINION-ROCK@NOAA.GOV_2_90633',
#                 '1704_SAMANTHA.BINION-ROCK@NOAA.GOV_2_90625')


# ###     Additionally, separate MRIP CV reports may be needed if we are imputing LACR/TPWD discards using
# ###     catch rates calculated from either LA or Gulf-wide (B2:AB1) discard ratios, for which we might need:
# ###
# LA.report.name = '1702_SAMANTHA.BINION-ROCK@NOAA.GOV_2_90633'
# ###     ...which identifies a MRIP CV report that includes only Louisiana data
# 
# GOM.report.name = '1457_MATTHEW.NUTTALL@NOAA.GOV'
# ###     ...which identifies a MRIP CV report that includes (at most) Gulf of Mexico states for MS-FLW.
# ###       Note that for SID species, this table only includes those (non-LA) states in the Gulf of Mexico
# ###       belonging to the same SID domain as Louisiana (e.g., if SID 'West' includes TX-MS, then
# ###       'GOM.report.name' only includes MRIP data from MS - excludes the 'East' states of AL & FLW )...
# 
# ###     Note that these reports may already be defined in 'report.name' (e.g., if SID = 'West' includes TX/LA ,
# ###       'LA.report.name' can just point to one of the same tables in 'report.name' ), but this isn't
# ###       necessarily the case and so I define them as separate objects...

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

sedar.size.file = "/Size/SBS_rec_sizeGEN_8124_20260127_SBR.xlsx"


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

dir <- getwd()

avgwgt.dir = paste0(dir, '/Catch')
#"C:/Users/matthew.nuttall/Desktop/ACL AvgWgt Files/2025_02_Feb"
# avgwgt.dir = "U:/_Data/_ACL Files/Size Files/ACL_Sep22"

###     Note that this directory assignment can probably be replaced with a view name once weight estimation
###     has been validated in RDI, after which we can start creating avgwgt-CV reports...

### -------------------------------------------------------------------------------------------




### ADDITIONAL FLAGS ###
###
### -------------------------------------------------------------------------------------------
###
###   -- TAB for %UNID FISH --
###
###   ...to indicate if, for this particular SEDAR, some fraction of 'unidentified' catch is to be
###       allocated to the species-of-interest. In addition to modifying the catch file
###       (i.e., reducing 'unidentified' catch to the level thought to represent the assessed species),
###       we also provide an additional summary (tab) of the relative breakdown of 'identified' catch
###       (identified at the species level), both of which require the objects below...

flag.unid = TRUE

if( flag.unid ) {
  
  sppID.type = 'COMMON'
  # sppID.type = 'SCIENTIFIC'
  
  taxa.unid = c( "REQUIEM SHARK GENUS", "SHARK,DUSKY", "SHARK,BULL", "SHARK,SANDBAR", 
                 "SHARK,BLACKNOSE", "SHARK,BIGNOSE", "SHARK,SILKY", "SHARKBLACKTIP",
                 "SHARK,OCEANIC WHITETIP", "REEF SHARK", "SHARK,SMALLTAIL", "SPINNER SHARK",
                 "FINETOOTH SHARK", "NIGHT SHARK")
  ###     ...I also specify the taxa ( in "taxa.unid" ) that represents unidentified catch,
  ###         which will be dropped when estimating potential allocation ratios...
  taxa.unid.catch = c( "REQUIEM SHARK GENUS" )
  
  nodc.unid       = sapply( taxa.unid,       FUN=nodc.code.info, spp.field=sppID.type, spp.table=spp.info )
  nodc.unid.catch = sapply( taxa.unid.catch, FUN=nodc.code.info, spp.field=sppID.type, spp.table=spp.info )
  rm( sppID.type )
  
}

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

catch.table <- pull.GenRec.catch(  raw.table = dat,
                                   pull.type = 'ACL',
                                   # pull.type = 'RDI',
                                   spp.filter = nodc.code, yr.filter = first.year:term.year,  mode.filter = mode_sub,
                                   reg.filter = region,   sta.filter = states,  fl.filter = FL_sub, nc.filter = NC_sub  )

# catch.summary <- catch.table %>%
#   filter( NEW_MODEN == 'Priv' ) %>%
#   group_by( NEW_COM, YEAR, NEW_STA ) %>%
#   summarize( AB1 = sum( as.numeric(AB1), na.rm=TRUE ),
#               B2 = sum( as.numeric( B2), na.rm=TRUE ) ) %>%
#   select( NEW_COM, YEAR, NEW_STA, AB1, B2 ) %>%
#   pivot_wider( names_from=NEW_STA, values_from=c(AB1,B2) )
# View( catch.summary )
# 
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
source( paste0(dir,'/Functions/SECmodify_allocate_unid_HMS.R') )
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
   command.line = paste0( "dummy.table = unid.table %>% group_by( YEAR ) %>%
                                 summarise( pAB1 = sum( `",new.com,"_p.AB1` ),
                                            pB2  = sum( `",new.com,"_p.B2`  ) ) %>%
                                 mutate( pAB1 = ifelse( is.nan(pAB1), NA, pAB1 ),
                                         pB2  = ifelse( is.nan(pB2 ), NA, pB2  ) ) %>%
                                 pivot_longer( cols = -c(YEAR), names_to='CAT_VAR', values_to='prop' )" )
   eval( parse( text = command.line ) )
   rm(command.line)
   
   # # poly.degf = 5
    poly.degf = round( ( length( unique(dummy.table$YEAR) ) - 1 ) / 3, 0 )
  # # ###     ...(-1) to not count the YEAR='TOTAL' row, and divided by three as a subjective attempt prevent 'overfitting'...
  # 
   dummy.plot = ggplot( data = dummy.table %>% filter( YEAR != 'TOTAL' & !is.na(prop) ) %>% mutate( YEAR = as.numeric(YEAR) ) ) +
     geom_point( aes( x=YEAR, y=prop ) ) +
     # stat_smooth( aes( x=YEAR, y=prop ), method = lm, formula = y ~ poly( x,poly.degf ), se=FALSE ) +
     geom_hline( data = dummy.table %>% filter( YEAR == 'TOTAL' ),
                 aes( yintercept = prop ), linewidth=1.2 ) +
     facet_grid( CAT_VAR ~ . , scales = 'free' ) +
   
     labs( title="", x="Year", y=paste0( "Percent Catch (",new.com,")" ) ) +
     expand_limits(y = 0) +
     theme_bw() +
     theme( text = element_text(size = 11),
            axis.text.x = element_text(angle = 90, vjust=0.5),
            legend.position = "bottom",
            panel.grid.major = element_line(colour = "grey", linewidth = 0.5),
            panel.grid.minor = element_line(colour = "grey", linewidth = 0.2),
            panel.border = element_rect(colour = "black", fill = NA) )
   dummy.plot
 
   dummy.plot = dummy.plot +
     geom_abline( intercept=0.23, slope=0, color= 'red', linetype='dashed' ) +    ### SEDAR 32 Ratio
     geom_abline( intercept=0.45, slope=0, color='blue', linetype='dashed' )      ### SEDAR 50 Ratio
   dummy.plot
#  # 
#  # rm( dummy.table, poly.degf, dummy.plot )
  
  
  
#   eval( parse( text = paste0(
#         "sum( unid.table$`",new.com,"_AB1`[ unid.table$YEAR %in% 2005:2023 ], na.rm=TRUE ) /
#          sum( unid.table[ unid.table$YEAR %in% 2005:2023, grepl('_AB1',colnames(unid.table)) ], na.rm=TRUE )" ) ) )
#   eval( parse( text = paste0(
#     "sum( unid.table$`",new.com,"_B2`[ unid.table$YEAR %in% 2005:2023 ], na.rm=TRUE ) /
#      sum( unid.table[ unid.table$YEAR %in% 2005:2023, grepl('_B2',colnames(unid.table)) ], na.rm=TRUE )" ) ) )
#   
#   eval( parse( text = paste0( "mean( unid.table$`",new.com,"_p.AB1`[ unid.table$YEAR %in% 2005:2023 ] )" ) ) )
#   eval( parse( text = paste0( "mean( unid.table$`",new.com,"_p.B2`[  unid.table$YEAR %in% 2005:2023 ] )" ) ) )
  
  
  
  ### ***************************************************
  
  ### -- NOTES ON DECISION --
  ##* *Consider different ratios for AB1 for 1981-2000 and 2001-2024*
  ##* *Just dummy ratios to make sure the function is working*    
  
  # unid.ratio = 0.45
  unid.ratio = data.frame( AB1=0.4, B2=0.6 )
  
  ### ***************************************************
  
  
  ###   -- STEP 3 --
  ###
  ###     Informed by the above analysis (i.e., relative catch of species that 'belong' to the unidentified taxa ),
  ###     we then apply the chosen fraction to our 'catch.table' object. Note that the allocate.unid() function
  ###     imports the SEFSC avgwgt estimates at each level of the hierarchy ( saved in 'avgwgt.dir' ), as needed
  ###     to update the lbsest_SEC fields with an avgwgt representative of the species-of-interest (not the UNID taxa),
  ###     and so the function below may take a little time to run...
  
  
  
  ##* *Need to merge catch table and the unidentified catch table to create the*
  ##* *Gen Rec table which contains gray trig and unid Balistidae*
  
  
  unique(unid.dat$NEW_SCI)
  
  unid.dat2 <- unid.dat %>%
    filter(NEW_SCI == 'Carcharhinus spp.')
  
  
  catch.table <- rbind(catch.table, unid.dat2)
  
  
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

unique(catch.table$NEW_SCI)

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

if( any( c('TX','LA','MS','AL','FLW','FLE') %in% states ) ) {
  
  
  source( paste0(dir,'/Functions/SECmodify_impute_MRIP1981w1.R') )
  
  
  summary <- catch.table %>%
    group_by(YEAR, WAVE, FL_REG) %>%
    summarise(total.ab1 = sum(AB1, na.rm=TRUE),
              total.b2  = sum(B2, na.rm=TRUE))
  
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
  ##* *Wave 1 landings are negligible (<1% for 1982:1984) but make up 17% of discards*    
  ##* *Don't think it's reasonable to say that removals were negligible*
  ##* *The ratios are stable in 1982 and 1984 but are > 1 in 1993*
  ##* *Discards spiked in 1983*
  ##* *Usin the proportion of wave 1:waves2-6 because the proportion method is heavily influenced by 1983*
  ###
  # ###   ...where, from the above analysis, we identify what (if any) imputation to apply...
   #method.MRIP.1981w1 = 'None'
   method.MRIP.1981w1 = 'prop_w1_w26'
   #method.MRIP.1981w1 = 'avg_82_84'
  
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
  
  dummy$StateCatch
  ###     ...to evaluate the relative catch coming from LA (i.e., is catch negligible? )
  
  dummy$LACatch
  ###     ...to evaluate whether LACreel discards should be imputed:
  ###           -- Does B2=0 look reasonable for LA 2014+ , relative to other years and/or other states?
  ###           -- Has this species been consistently discarded in LA (over time), or are discards a sporadic phenomenon?
  
  dummy$fracs
  dummy$fracs.fig
  ###     ...to evaluate stability of (B2:AB1) catch ratios across time (years) and space (states/regionally)
  ###         and to inform decisions on whether LA-specific or Gulf-wide ratios are more appropriate for this stock...
  
  
  ### ***************************************************
  
  ### -- NOTES ON DECISION --
  ###         Discard information is collected as part of the LACreel survey for this particular species (2016+),
  ###         so the only data gap that needs filling is for 2014-2015. 
  ##* *LA discards contribute less than 1% from 2011-2024*
  ##* *Discards were not imputed for SEDAR 62*
  ##* *Will not impute discards for 2014 and 2016*
  ###   ...where, from the above analysis, we identify what (if any) imputation to apply...
   method.LACR.B2 = 'None'
  #method.LACR.B2 = 'la_ratio'
  # method.LACR.B2 = 'gu_ratio'
  
  ###   ...and, if the choice is to impute, we also identify the years over which the (B2:AB1) ratios are calculated
  ###       (e.g., ratios calculated over 'ratio.yrs.LACR.B2', but applied to estimate LACR discards for 2014+ ).
  ###       Note that this input is only relevant for the 'la_ratio' method as the 'gu_ratio' method applies
  ###       B2:AB1 ratios calculated from the same year as that requiring an imputation (e.g., use 2014 catch rates
  ###       to impute 2014 LACR discards ). Therefore, 'ratio.yrs.LACR.B2' is not called ( within the function )
  ###       when method='gu_ratio' ( its set to either 2014:2015 or 2014:term.year in the function )...
  ratio.yrs.LACR.B2 = 2016
  # ratio.yrs.LACR.B2 = 2000:2013
  # ratio.yrs.LACR.B2 = c(2011:2013,2016:2018)
  # ratio.yrs.LACR.B2 = 2014:2015
  
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
  
  
  ###   ...for which we first apply the summary.TPWD.1981.1983() function to determine:
  ###         (1) if these imputations are necessary and
  ###         (2) if so, how to conduct these imputations
  
  dummy <- summary.TPWD.1981.1983( genrec.table = catch.table )
  
  dummy$StateCatch
  ###     ...to evaluate the relative catch coming from TX from those years over which average catches
  ###           would be calculated (1983:1985). Are landings negligible compared to other Gulf states?
  
  dummy$TXCatch
  ###     ...to evaluate whether TPWD landings should be imputed:
  ###           -- Does AB1=0 look reasonable for TX 1981-1983 , relative to other years?
  ###           -- Has this species been consistently landed in TX (over time), or are landings a sporadic phenomenon?
  
  dummy$State.fig
  
  
  ### ***************************************************
  
  ### -- NOTES ON DECISION --
  ##* *TX catches are low, but there are no non-zero years*
  ##* *Decide to impute since there are no non-zero years*
  ###   ...where, from the above analysis, we identify what (if any) imputation to apply...
   #method.TPWD.1981.83 = 'None'
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
  
  dummy$StateCatch
  dummy$State.fig
  ###     ...to evaluate the relative catch coming from TX (i.e., is catch negligible compared to other states? )
  
  dummy$fracs
  dummy$fracs.fig
  ###     ...to evaluate stability of (B2:AB1) catch ratios across time (years) and space (states/regionally)
  ###         and to inform decisions on whether LA-specific or Gulf-wide ratios are more appropriate for this stock...
  
  
  ### ***************************************************
  
  ### -- NOTES ON DECISION --
  ##* *Discards in LA contributes a low proportion to the total discards compared to other states*
  ##* *As done in SEDAR 62, using the la_ratio to estimate discards for TX*

  ###   ...where, from the above analysis, we identify what (if any) imputation to apply...
  # method.TPWD.B2 = 'None'
  method.TPWD.B2 = 'la_ratio'
  # method.TPWD.B2 = 'gu_ratio'
  
  ### ***************************************************
  
  
  if( method.TPWD.B2 != 'None' ) {
    
    catch.table = impute.TPWD.B2v2( genrec.table = catch.table, method = method.TPWD.B2 )
    
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



###########################################
######     Calibrating State and     ######
######     MRIP FES/FHS estimates    ######
###########################################

source( paste0(dir,'/Functions/SECmodify_cal_MRIPstate.R') )

if( flag.cal.MRIPstate ) {
  
  cal.factors = extract.cal.ratio( new.com = new.com, region = region )
  
  for( i in 1:length(cal.factors) ) {
    
    ### LACR ###
    ### --------
    dummy.ratio = cal.factors[[i]][ which( grepl('lacr',names(cal.factors[[i]])) &
                                             !grepl('.EFF',names(cal.factors[[i]])) ) ]
    if( toupper( names(cal.factors)[i] ) == 'PRIV' ) {
      dummy.mode = c( str_to_title(names(cal.factors)[i]), "Priv/Shore" )
    } else {
      dummy.mode = str_to_title(names(cal.factors)[i])
    }
    catch.table = calibrate.MRIPstate( DS.filter = 'LA Creel', mode.filter = dummy.mode,
                                       cal.ratios = dummy.ratio, genrec.table = catch.table )
    rm( dummy.ratio, dummy.mode )
    
    
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




##* *Manually calibrating TX and LA Private outside of the function*

unique(catch.table$DS)  ##MRIP, LA Creel, TPWD


##* *Fields that need to be imputed - Private Only!!*
##* *AB1, B2, lbsest_SECgwt, lbest_SECwwt*


##* *Create duplicates of the original fields*
catch.table$AB1_orig           <- catch.table$AB1
catch.table$B2_orig            <- catch.table$B2
catch.table$lbsest_SECgwt_orig <- catch.table$lbsest_SECgwt
catch.table$lbsest_SECwwt_orig <- catch.table$lbsest_SECwwt



##* *Calibration Ratios*

tpwd.cal.ratio = 10.8989
la.cal <- 3.55



##* *Assign calibrations - Private Only*
unique(catch.table$NEW_MODEN)

catch.table <- catch.table %>%
  mutate(Cal_Factor = case_when(
    NEW_MODEN %in% c('Cbt', 'Hbt') ~ 1,
    DS == 'MRIP' ~ 1,
    DS == 'TPWD' & NEW_MODEN == 'Priv' ~  tpwd.cal.ratio,
    DS == 'LA Creel' & NEW_MODEN == 'Priv/Shore' ~  la.cal)) 



##* *Apply calibrations*
catch.table <- catch.table %>%
  mutate(AB1 = AB1_orig * Cal_Factor,
         B2  = B2_orig * Cal_Factor,
         lbsest_SECgwt = lbsest_SECgwt_orig * Cal_Factor,
         lbsest_SECwwt = lbsest_SECwwt_orig * Cal_Factor)










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
  
  
  
  mrip.dummy = CVs.catnum.MRIP( rdi.report = report.name, inc.modes = cv.modes, genrec.table = catch.table,
                                imp.1981w1 = method.MRIP.1981w1,
                                flag.unid    = flag.unid,     catch.table.unid    = catch.table.unid,
                                flag.forhire = flag.forhire,  catch.table.forhire = catch.table.forhire,
                                #report.type = 'annual',
                                loc.cv.forhire = NA,
                                loc.FH.ratios = paste0(dir,'/Functions/import_datasets/ForHire Partitioning Ratios.csv') )
  
  ###     ...where 'rdi.report' identifies the report to which the catch & CV estimates have been saved.
  ###             Note that for assessments with multiple SID boundaries, 'rdi.report' will be composed of multiple elements,
  ###             one report for each SID domain ( that should be ordered geographically, as arranged using NEW_ST ).
  ###             In this cases, 'mrip.dummy' will be a list with as many elements as there are unique SID domains...
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
  
  
  
 
  #cv.table   <- mrip.dummy
  
  
  if( 'SID' %in% colnames(catch.table) ) {
    mrip.dummy = lapply( mrip.dummy, FUN = convert.long.table.cat, survey = 'MRIP' )
    
  } else {
    mrip.dummy = convert.long.table.cat( mrip.dummy, survey = 'MRIP' )
  }
  
  


  
  
  ###   For SID assessments, we then assign names to 'mrip.dummy' using the unique values of SID in
  ###   'catch.table'. In particular, each of the CV-reports in 'report.name' are provided in geographic order
  ###   and so if we sort the SID domains in 'catch.table' (i.e., using the NEW_ST field ), we can apply this
  ###   sorting to our 'mrip.dummy' table to identify which CV report corresponds to which SID domain...
  
  
  if( 'SID' %in% colnames(catch.table) ) {
    
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
    
    if( flag.cal.MRIPstate ){
      
      for( i in 1:length(cal.factors) ) {
        ###   ...which cycles through calibration factors for each of the (i) modes in 'cal.factors'...
        
        dummy.ratio = cal.factors[[i]][ which( grepl('lacr',names(cal.factors[[i]])) &
                                              !grepl('.EFF',names(cal.factors[[i]])) ) ]
        dummy.mode = names(cal.factors)[i]
        dummy.mode = c( dummy.mode, str_to_title(dummy.mode) )
        if( 'Priv' %in% dummy.mode ) {
          dummy.mode = c( dummy.mode, "PRIV/SHORE","Priv/Shore" )
        }
        lacr.dummy = CVs.catnum.LACR( itis.code = itis.code, inc.modes = cv.modes, inc.years = 2014:term.year,
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
      lacr.dummy = CVs.catnum.LACR( itis.code = itis.code, inc.modes = cv.modes, inc.years = 2014:term.year,
                                    imp.LACR.B2 = method.LACR.B2, calc.ratios.from = ratio.yrs.LACR.B2,
                                    mrip.cv.table =  mrip.cv.table )
    }
    
    if( method.LACR.B2 != 'None' ) {    rm( mrip.cv.table )    }
    
    
    lacr.cv.tpwdB2 = lacr.dummy[[2]]
    ###     ...where raw estimates of LACR may be needed to impute TPWD discards ( before any MRIP:state calibration factors
    ###       are applied ) and so I made the raw estimates a separate output of the above function in case they're needed
    ###       (i.e., LA-specific B2:AB1 discard ratios used to impute TPWD discards )...
    
    lacr.dummy = lacr.dummy[[1]]
    
    cv.table <- lacr.dummy
    
    
    ###     ...where the LACR cv table is converted into a long-format...
    lacr.dummy = convert.long.table.cat( lacr.dummy, survey = 'LACR' )
    
    
    ###   For SID assessments, I then add a field to identify the SID domain to which LACR belongs...
    if( 'SID' %in% colnames(catch.table) ) {
      
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
    
    if( flag.cal.MRIPstate ){
      
      for( i in 1:length(cal.factors) ) {
        dummy.ratio = cal.factors[[i]][ which( grepl('tpwd',names(cal.factors[[i]])) &
                                              !grepl('.EFF',names(cal.factors[[i]])) ) ]
        dummy.mode = names(cal.factors)[i]
        dummy.mode = c( dummy.mode, str_to_title(dummy.mode) )
        tpwd.dummy = CVs.catnum.TPWD( tpwd.code = tpwd.code, inc.modes = cv.modes, inc.years = 1983:term.year,
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
      tpwd.dummy = CVs.catnum.TPWD( tpwd.code = tpwd.code, inc.modes = cv.modes, inc.years = 1983:term.year,
                                    genrec.cat.table = catch.table,
                                    imp.TPWD.8183 = method.TPWD.1981.83,
                                    imp.TPWD.B2 = method.TPWD.B2, genrec.cv.table = genrec.cv.table )
    }
    
    rm( lacr.cv.tpwdB2 )
    if( method.TPWD.B2 != 'None' ) {    rm( genrec.cv.table )    }
    
    
    ###     ...where the TPWD cv table is converted into a long-format...
    
    cv.table <- tpwd.dummy
    
    tpwd.dummy = convert.long.table.cat( tpwd.dummy, survey = 'TPWD' )
    
    
    ###   For SID assessments, I then add a field to identify the SID domain to which TPWD belongs...
    if( 'SID' %in% colnames(catch.table) ) {
      
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
  }
  
  ###     For SID assessments, I then collapse the multiple elements of 'cv.table' into a single table...
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
  
  
  ### Lastly, I make sure the final 'cv.table' includes ALL years, whether estimates exist or not...
  
  dummy = list( YEAR = seq( first.year, term.year, by=1 ) )
  if( 'SID' %in% colnames(catch.table) ) {   dummy$SID = unique(catch.table$SID)   }
  if( flag.open.closed ) {   dummy$fed_closed = unique(catch.table$fed_closed)   }
  dummy = expand.grid( dummy )
  
  cv.table = cv.table %>%
    full_join( dummy, by=colnames(dummy) ) %>%
    arrange( across( colnames(dummy) ) )
  rm(dummy)
  
  cv.table = cv.table %>%
    select( any_of( c('SID','YEAR','fed_closed','NEW_MODE','NEW_MODEN','CATCH_VAR','METRIC','value') ) )
  
  
  
  
  ### Replace catch values ( in 'cv.table' ) with those from 'catch.table'...
  blah1 = catch.table %>%
    mutate( NEW_MODEN = toupper(NEW_MODEN) ) %>%
    mutate( NEW_MODE  = ifelse( NEW_MODE  == 6, 4, NEW_MODE ),
            NEW_MODEN = ifelse( NEW_MODEN == "PRIV/SHORE", "PRIV", NEW_MODEN ) ) %>%
    group_by( across( any_of( c('SID','YEAR','fed_closed','NEW_MODE','NEW_MODEN','CATCH_VAR','METRIC','value') ) ) ) %>%
    summarize( AB1 = sum( AB1, na.rm=TRUE ),
               B2  = sum(  B2, na.rm=TRUE ) ) %>%
    ungroup()
  blah2 = catch.table %>%
    group_by( across( any_of( c('SID','YEAR','fed_closed','CATCH_VAR','METRIC','value') ) ) ) %>%
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
  
  join.vec = colnames(cv.table)[
    colnames(cv.table) %in% c('SID','YEAR','fed_closed','NEW_MODE','NEW_MODEN','CATCH_VAR','METRIC') ]
  dummy = cv.table %>% left_join( blah, by=join.vec )
  rm( join.vec, blah )
  
  # View( dummy[ !is.na(dummy$value.y) & !(as.integer(dummy$value.x)-as.integer(dummy$value.y)== 0) , ] )
  # sum( dummy$value.x, na.rm=TRUE ) - sum( dummy$value.y, na.rm=TRUE )
  
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
wgt.table = read_excel( path=paste0(dir, sedar.size.file ),
                        sheet="Weight Summary by Mode", trim_ws=FALSE, col_types="text" )

lbs.cv.table.2 = CVs.landwgt( approach = 2, num.table = num.table, wgt.table = wgt.table, catch.table = catch.table )
rm( num.table, wgt.table )



### Replace catch values ( in 'lbs.cv.table' ) with those from 'catch.table'...
blah1 = catch.table %>%
  mutate( NEW_MODEN = toupper(NEW_MODEN) ) %>%
  mutate( NEW_MODE  = ifelse( NEW_MODE  == 6, 4, NEW_MODE ),
          NEW_MODEN = ifelse( NEW_MODEN == "PRIV/SHORE", "PRIV", NEW_MODEN ) ) %>%
  group_by( across( any_of( c('SID','YEAR','fed_closed','NEW_MODE','NEW_MODEN','CATCH_VAR','METRIC','value') ) ) ) %>%
  summarize( LBS = sum( lbsest_SECwwt, na.rm=TRUE ) ) %>%
  ungroup() %>%
  rename_all( ~toupper(.) )
blah2 = catch.table %>%
  group_by( across( any_of( c('SID','YEAR','fed_closed','CATCH_VAR','METRIC','value') ) ) ) %>%
  summarize( LBS = sum( lbsest_SECwwt, na.rm=TRUE ) ) %>%
  ungroup() %>%
  mutate( NEW_MODE  = 99,
          NEW_MODEN = 'TOTAL' ) %>%
  rename_all( ~toupper(.) )
blah = bind_rows( blah1, blah2 )
rm( blah1, blah2 )

join.vec = colnames(lbs.cv.table.2)[
  colnames(lbs.cv.table.2) %in% c('SID','YEAR','FED_CLOSED','NEW_MODE','NEW_MODEN') ]
dummy = lbs.cv.table.2 %>% left_join( blah, by=join.vec )
rm( join.vec, blah )

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


table.ID <- paste0( "GTF_rec_catGEN_ss_",
                    substr( first.year, nchar(first.year)-1, nchar(first.year) ),
                    substr( term.year, nchar(term.year)-1, nchar(term.year) ),
                    "_", gsub("-","", Sys.Date() ) )

if( flag.unid ) {   tab.unid.dat <- paste( "BALISTIDAE", "_rec_catGEN" )   }


### _______________________________________________________________________________________


### IMPORT EXCEL TEMPLATE ###
### -------------------------
wb <- loadWorkbook( file=paste0( dir,"/Catch/Template_SEDAR_GenCatch_fromACL_v2.xlsx" ) )






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
  select( any_of( c('SID','YEAR','fed_closed','NEW_MODE','NEW_MODEN','CATCH_VAR',
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


saveWorkbook( wb, file=paste0( dir,"/Catch/",table.ID,".xlsx" ), overwrite=TRUE )




