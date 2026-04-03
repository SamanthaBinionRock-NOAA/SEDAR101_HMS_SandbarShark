


####################################################################################################################
####################################################################################################################
####################################################################################################################
#############################                                                          #############################
#############################                      -CARIBBEAN SEDAR-                   #############################
#############################                      GENREC SIZE FILES                   #############################
#############################                                                          #############################
####################################################################################################################
####################################################################################################################
####################################################################################################################


### Loading the required libraries...
library(tidyverse)
library(reshape2)
library(dplyr)
library(openxlsx)

library(haven)

###     ...and although I'm pulling (size) data from an ACL data file, I still load my Oracle credentials
###         so that I can import the "xref" data table, which I use to identify the spp of interest...
library(ROracle)

con = dbConnect(dbDriver("Oracle"), username = keyring::key_list("SECPR")[1,2],
                password = keyring::key_get("SECPR", keyring::key_list("SECPR")[1,2]), dbname = "SECPR")
spp.info = dbGetQuery(con, "SELECT * 
                     FROM RDI.v_species_xref@secapxdv_dblk.sfsc.noaa.gov")


'%notin%' = Negate( '%in%' )




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
###               - Spatial  - includes just Puerto Rico
###
###           ...what to do about unidentified snappers???
###                           ...which I don't seem to see in either file...
###                                 unique(  ACL_size$new_com[ grep( "trigger",  ACL_size$new_com ) ] )
###                                 unique( ACL_catch$new_com[ grep( "trigger", ACL_catch$new_com ) ] )



#############################
######     FILTERS     ######
#############################


# first.year <- 1981
first.year <- 2000
# first.year <- 2018
###       ...where "first.year" = 2000 for all Caribbean requests except projections (which only need a couple years)...
term.year <- 2017


region <- "Caribbean"
states <- c( "PR" )
###     ...which has options c( "TX","LA","MS","AL","FLW","FLE","GA","SC","NC","VA","MD","DE","PN","NJ","NY","CT","RI","MA","NH","ME" )
###     ...or c( "PR","VI" ) for Caribbean assessments...
# if( "FL" %in% states | "FLW" %in% states | "FLE" %in% states ) {  FL_sub <- c( 1,2,3 )  }
# if( "NC" %in% states ) {                                          NC_sub <- c( "N","S" )  }


mode_sub <- c( "Priv","Cbt","Shore" )
#       ...which has options c( "Priv","Cbt","Hbt","Shore" )
###           Note that the code below removes all HBT fishing from SUB_REG = 6 (SATL), FL_REG = 3 (FL Keys),
###           and SUB_REG = 7 from 1986+, all of which is designed to avoid overlap with SRHS...



### Moving onto the species-specific filter, I need to pull data for the species of interest...
###     Therefore, I start by searching for the appropriate identifiers...
# View( spp.info[grep( "MACKEREL", spp.info$COMMON ),] )
# View( spp.info[grep( "Centropristis striata", spp.info$SCIENTIFIC ),] )

taxa <- c( "SNAPPER,YELLOWTAIL","SNAPPERS,UNC" )


### FUNCTIONS ###
### ---------------------------------------------------------------------------------------------
new.com.info <- function( taxa, spp.table ) {
  info <- spp.table$NEW_COM[ grep( paste0("^",taxa,"$"), spp.table$COMMON ) ]
  # info <- spp.table$NEW_COM[ grep( paste0("^",taxa,"$"), spp.table$SCIENTIFIC ) ]
  info <- trimws( info, "both" )
  return( info )
}
new.sci.info <- function( taxa, spp.table ) {
  info <- spp.table$NEW_SCI[ grep( paste0("^",taxa,"$"), spp.table$COMMON ) ]
  # info <- spp.table$NEW_SCI[ grep( paste0("^",taxa,"$"), spp.table$SCIENTIFIC ) ]
  return( info )
}
nodc.code.info <- function( taxa, spp.table ) {
  info <- spp.table$NODC_CODE[ grep( paste0("^",taxa,"$"), spp.table$COMMON ) ]
  # info <- spp.table$NODC_CODE[ grep( paste0("^",taxa,"$"), spp.table$SCIENTIFIC ) ]
  return( info )
}
itis.code.info <- function( taxa, spp.table ) {
  info <- spp.table$SPECIES_ITIS[ grep( paste0("^",taxa,"$"), spp.table$COMMON ) ]
  # info <- spp.table$SPECIES_ITIS[ grep( paste0("^",taxa,"$"), spp.table$SCIENTIFIC ) ]
  return( info )
}
tpwd.code.info <- function( taxa, spp.table ) {
  info <- spp.table$TX_CODE[ grep( paste0("^",taxa,"$"), spp.table$COMMON ) ]
  # info <- spp.table$TX_CODE[ grep( paste0("^",taxa,"$"), spp.table$SCIENTIFIC ) ]
  return( info )
}
### ---------------------------------------------------------------------------------------------

new.com   <- sapply( taxa, FUN=new.com.info, spp.table=spp.info )
new.sci   <- sapply( taxa, FUN=new.sci.info, spp.table=spp.info )
nodc.code <- sapply( taxa, FUN=nodc.code.info, spp.table=spp.info )
itis.code <- sapply( taxa, FUN=itis.code.info, spp.table=spp.info )
tpwd.code <- sapply( taxa, FUN=tpwd.code.info, spp.table=spp.info )




### I then define a flag to identify SEDARs where:
###
###       *** the final "avgwgt.table" (for CVs) includes a different set of species than that in "size.table" ***
###
###     Specifically, a number of SEDARs assume some percentage of unidentified fish is comprised of the
###     assessed species (e.g., unidentified triggerfish in S82, for gray triggerfish ). In such cases, the provided size data
###     ( in 'size.table' ) includes these unidentified fish, but I do not include these in the CV calculation as its unclear
###     what sizes are from the species-of-interest (vs. related taxa). The objects below were developed to apply a different
###     (species) filter for the CV calculation, when appropriate...

flag.cv = TRUE

if( flag.cv ) {
  
  taxa.cv <- c( "SNAPPER,YELLOWTAIL" )
  
  nodc.cv <- sapply( taxa.cv, FUN=nodc.code.info, spp.table=spp.info )
}







####################################################################################################################
####################################################################################################################
####################################################################################################################
#############################                                                          #############################
#############################                   CONSTRUCT SIZE TABLE                   #############################
#############################                                                          #############################
####################################################################################################################
####################################################################################################################
####################################################################################################################



#############################
######     SORTING     ######
#############################

###   ...where I start by matching the sorting used by Vivian in her ACL SAS files. This isn't really important
###       for the "size.table" (that summarizes size data for all applicable observations in an assessment), but it
###       can be important in the number of trips tabs (e.g., "MRIP_meastrp" ) when we're dealing with more than
###       one species. In particular, the trip tabs are constructed by retaining only unique ID_CODE identifiers
###       (keeps the first observations/row it encounters). This could be either species depending on how the table
###       is sorted and so to "standardize" my pull, I use the same sorting Vivian does when generating the ACL tables...

dat <- ACL_size %>%
  arrange( SP_CODE, ID_CODE )





#############################
######     FILTERS     ######
#############################


### SPECIES ###
size.table <- dat[ which( dat$SP_CODE %in% nodc.code ), ]


### TEMPORAL ###
# size.table <- size.table[ which( size.table$year %in% (first.year:term.year) ), ]
size.table <- size.table[ which( size.table$YEAR %in% (first.year:term.year) ), ]


### SPATIAL ###

if( region == "Caribbean" ) {
  
  size.table <- size.table[ which( size.table$SUB_REG == 11 ), ]          ### ...where can look at all Caribbean...
  size.table <- size.table[ which( size.table$new_sta %in% states ), ]    ### ...or just a part of it (usually keep PR, but drop USVI)...
  
} else {
  
  # size.table <- size.table[ which( size.table$NEW_STA %in% states ), ]
  # 
  # if( "FL" %in% states | "FLW" %in% states | "FLE" %in% states ) {
  #   size.table <- size.table[ which(
  #     is.na(size.table$FL_REG) | size.table$FL_REG == "" | size.table$FL_REG %in% FL_sub ), ]
  # }
  # if( "NC" %in% states ) {
  #   size.table <- size.table[ which(
  #     is.na(size.table$NC_REG) | size.table$NC_REG == "" | size.table$NC_REG %in% NC_sub ), ]
  # }
  
}



### MODE ###

size.table <- size.table[ which( size.table$new_moden %in% mode_sub ), ]


# size.table <- size.table[ which( size.table$NEW_MODEN %in% mode_sub ), ]
# 
# ###     ...for which I exclude any MRIP sampling from LA during the years of the LA_Creel survey (2014+)...
# size.table <- size.table[ !( size.table$ds == "MRIP" & size.table$NEW_STA == "LA" & size.table$year >= 2014 ), ]
# ###     ...and any LDWF sampling during those years within which MRIP operated in LA (1981-2013)...
# size.table <- size.table[ !( size.table$ds %in% c("LA BIO","LA Creel") & size.table$NEW_STA == "LA" & size.table$year <= 2013 ), ]
# 
# ###     ...and to avoid duplicating SRHS information, I remove all "Hbt" fishing in SUB_REG==6...
# size.table <- size.table[ !( size.table$NEW_MODEN == "Hbt" & size.table$SUB_REG == 6 ), ]
# ###     ...and "Hbt" fishing from 1986+ in SUB_REG==7...
# size.table <- size.table[ !( size.table$NEW_MODEN == "Hbt" & size.table$SUB_REG == 7 & size.table$year >= 1986 ), ]
# ###     ...and "Hbt" fishing from Monroe County (FL_REG==3)...
# size.table <- size.table[ !( size.table$NEW_MODEN == "Hbt" & size.table$NEW_STA == "FLW" & size.table$FL_REG == 3 ), ]




### FINAL FILTERS ###

### I also remove any records where fish size is <NA>...
size.table <- size.table[ !( is.na(size.table$FL_mm) & is.na(size.table$TL_mm) & is.na(size.table$all_lbs) ), ]
###         ...where there shouldn't be any <NA> records in the completed RDI views that Vivian and I use,
###           but I add this line in case we have to use the raw tables (e.g., Caribbean assessments)...
###
###   ...and, as of Oct 6 2021, remove any records where weight observations are above the max (allowable) size,
###           which are identified with wgtflg = 'Y' and added as part of wgtest_step2 ...
size.table <- size.table[ size.table$wgtflg != 'Y' , ]






# ### PRE-EMPTIVE ERROR CHECKING ###
# size.table <- size.table[ !( is.na(size.table$FL_mm) & is.na(size.table$TL_mm) ), ]     ### ...no length information...
# size.table <- size.table[ !( size.table$ID_CODE == "" & size.table$DS == "MRIP" ), ]
# size.table <- size.table[ !( size.table$TRIP_KEY == "" & size.table$DS == "TPWD" ), ]







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









####################################################################################################################
####################################################################################################################
####################################################################################################################
#############################                                                          #############################
#############################                       NUMBER OF TRIPS                    #############################
#############################                                                          #############################
####################################################################################################################
####################################################################################################################
####################################################################################################################



##########################
######     MRIP     ######
##########################


mrip.trip.tab <- size.table[ which( size.table$ds %in% c("MRFSS","MRIP") ), ]
###   ...where DS = "MRFSS" is the current designation needed for Caribbean SEDARs, but this might be updated to "MRIP"
###       if the survey ever restarts in the Caribbean or if OST updates the old Caribbean data into the new file format...


mrip.trip.tab <- mrip.trip.tab %>%
  
  arrange(  SP_CODE, ID_CODE ) %>%
  ###   ...where I also match the sorting used by Vivian in her ACL SAS files. This isn't really important
  ###       for the "size.table" (that summarizes size data for all applicable observations in an assessment), but it
  ###       can be important in the trip tabs (e.g., "MRIP_meastrp" ) if we're dealing with more than one species.
  ###       In particular, the trip tabs are constructed by retaining only unique ID_CODE identifiers
  ###       (keeps the first observation/row it encounters). This could be either species depending on how the table
  ###       is sorted and so to "standardize" my pull, I use the same sorting Vivian does when generating the ACL tables...
  
  distinct( SP_CODE, ID_CODE, YEAR, .keep_all = TRUE )
  ###   ...where 'year' was added to the distinct() function to account for any MRIP records imputed to fill COVID data gaps,
  ###         for which MRIP updated STRAT_ID and PSU_ID with a '2020' identifier but not ID_CODE ( the year component of ID_CODE
  ###         was left at its original 2018/2019 value to identify which 2018/2019 records was used to fill the 2020 data gap )...






####################################################################################################################
####################################################################################################################
####################################################################################################################
#############################                                                          #############################
#############################               UNCERTAINTY in SEFSC AVGWGTs               #############################
#############################                                                          #############################
####################################################################################################################
####################################################################################################################
####################################################################################################################



###     As described in SEDAR 74-DW-12 (approach #2), the general method is as follows...
###
###       To start, I summarize the original 'size.table' at the trip/vessel level to account for any non-independence
###       in fish sizes (e.g., similar fish sizes when landed by the same vessel ). Once calculated, these
###       trip-level summaries (by vessel) are treated as "raw size data" and used to calculate a mean and
###       standard error of mean fish weight.
###
###       To match the approach used in SEFSC (average) weight estimation, this calculation is done at the RYSMWA level,
###       so long as sample sizes are sufficient for standard error estimates (i.e., few/no strata with n=1 and var=0 ).
###       However, in cases where size data is too sparse to support trip-level estimation at the RYSMWA level:
###         -- a YEAR-MODE stratification can be used, wherein YEAR-MODE tends to be the ultimate resolution
###               requested by SEDAR assessment analysts anyway
###         -- summaries may also be calculated at the observation (fish) level
###       ...both of which are possible in the code below using the 'method' and 'level' fields...
###
###       Note that this (trip-level) approach results in larger uncertainty estimates (for average weights)
###       than if done on raw size data (at the fish-level), in that we are looking at uncertainty in AVERAGE weight
###       ( standard error = stdev / sqrt(N) ) which decreases with increasing sample size -- the number of positive trips
###       cannot be larger than the number of fish sampled, so using trip-level summaries as my raw data ( N = #trips )
###       tends to result in larger standard errors than if calculated from the actual raw data ( N = #fish ).
###       This is important from an assessment perspective as the uncertainties being calculated from fish-level summaries
###       were too precise in many cases to be useful in the assessment model fitting process ( SE ~ 0.02 )...


if( flag.cv ) {
  
  avgwgt.table = size.table %>% filter( SP_CODE %in% nodc.cv )
  
} else {
  
  avgwgt.table = size.table
}


avgwgt.table = avgwgt.table %>%
  
  rename( Year = YEAR ) %>%
  mutate( MY_ID_CODE = ifelse( ds %in% c("MRIP","MRFSS"), paste0( "M",ID_CODE,Year ), NA ) )
                       # ifelse( ds == "TPWD", paste0( "T",TRIP_KEY ),
                       # ifelse( ds %in% c("LA Creel","LA BIO"), paste0( "L",SUPPLIER_SAMPLE_ID ), NA ))) )




### *********************************************************************************** ###
### *********************************************************************************** ###

###   This section of code defines the resolution at which CV estimates are to be calculated
###   (e.g., at the finest RYSMWA-level or at the assessment-requested Year-Mode level,
###           or across individual fish vs. trip-level summaries of fish )...

# level = 'RYSMWA'     ###   ...(trip-level) summaries calculated at the finest stratification
level = 'Year-Mode'   ###   ...(trip-level) summaries calculated at the year-mode level

# method = 'trip'       ###   ...raw data = trip-level summaries
method = 'fish'       ###   ...raw data = fish-level observations

### *********************************************************************************** ###
### *********************************************************************************** ###




###   Note that species (NEW_COM) is the only 'avgwgt strata' not included in the group_by() statements below
###     as I don't want to differentiate sizes calculated for different species (e.g., scamp & YMG treated as the
###     'same' species/stock in SEDAR 68 )...

if( method == 'fish' ) {
  
  ### Fish-level Summaries of Raw Size Data ###
  ###
  ###     ...wherein the code below doesn't summarize anything, it just formats the raw data to 'work' with subsequent code
  
  avgwgt.table = avgwgt.table %>%
    mutate( N   = 1,
            Avg = all_lbs ) %>%
    select( all_of( c('MY_ID_CODE','SUB_REG','Year','new_sta','new_mode','new_moden','WAVE','new_arean','N','Avg') ) ) %>%
    filter( N > 0 & !is.na(Avg) )
  
  
} else if( method == 'trip' ) {
  
  ### Trip-level Summaries of Raw Size Data ###
  
  avgwgt.table = avgwgt.table %>%
    group_by( MY_ID_CODE, SUB_REG, Year, new_sta, new_mode, new_moden, WAVE, new_arean ) %>%
    summarize( N   = length( all_lbs[ !is.na(all_lbs) ] ),
               Avg = mean( as.numeric(all_lbs), na.rm=TRUE ),
               SD  =   sd( as.numeric(all_lbs), na.rm=TRUE ) ) %>%
    ###   Note that the standard deviation of fish sizes (within a trip) are not considered in this approach...
    filter( N > 0 & !is.na(Avg) ) %>%
    ungroup()
  
}



if( level == 'RYSMWA' ) {
  
  
  ### Estimation at the Finest Resolution ###
  ###
  ###     ...where summaries are calculated at the region-year-state-mode-wave-area level when
  ###         sample sizes are adequate (i.e., when little/no strata have a n=1 and var=0 )...

  avgwgt.table = avgwgt.table %>%
    
    group_by( SUB_REG, Year, new_sta, new_mode, new_moden, WAVE, new_arean ) %>%
    # group_by( Year, new_mode, new_moden ) %>%
    
    summarize( Fish = sum( N, na.rm=TRUE ),
               Trip = length( unique( MY_ID_CODE[ !is.na(MY_ID_CODE) ] ) ),
               Wgt  = mean( as.numeric(Avg), na.rm=TRUE ),
               Var  =  var( as.numeric(Avg), na.rm=TRUE ) ) %>%
    ungroup()
  
  
  ###     ...wherein these (highly resolved) estimates are then aggregated into the desired strata
  ###         (i.e., YEAR-MODE summary from code directly below, and YEAR summary from code below that )...
  
  
  avgwgt.mode = avgwgt.table %>%
    
    group_by( Year, new_mode, new_moden ) %>%
    summarize( Fish = sum( Fish, na.rm=TRUE ),
               Trp  = sum( Trip, na.rm=TRUE ),
               Wgt  = weighted.mean( Wgt, Trip ),
               Var  = sum( Var , na.rm=TRUE ) ) %>%
    mutate( Var = ifelse( Var==0 & Wgt>0 & Trp==1, NA, Var ) )
  
  
  avgwgt.year <- avgwgt.table %>%
    
    group_by( Year ) %>%
    summarize( Fish = sum( Fish, na.rm=TRUE ),
               Trp  = sum( Trip, na.rm=TRUE ),
               Wgt  = weighted.mean( Wgt, Trip ),
               Var  = sum( Var, na.rm=TRUE ) ) %>%
    mutate( Var = ifelse( Var==0 & Wgt>0 & Trp==1, NA, Var ) )
  
  
} else if( level == 'Year-Mode' ) {
  
  
  ### (Direct) Estimation at the Desired Resolution ###
  ###
  ###       ...as applied when sample sizes are 'low' (i.e., too many year-mode strata with N=1 and var=0 )...
  
  
  avgwgt.mode = avgwgt.table %>%
    
    group_by( Year, new_mode, new_moden ) %>%
    
    summarize( Fish = sum( N, na.rm=TRUE ),
               Trip = length( unique( MY_ID_CODE[ !is.na(MY_ID_CODE) ] ) ),
               Wgt  = mean( as.numeric(Avg), na.rm=TRUE ),
               Var  =  var( as.numeric(Avg), na.rm=TRUE ) ) %>%
    ###   Note that the weighted.mean() function is not required here as summaries statistics are calculated
    ###   directly from the 'raw' size data (whehter that be fish-level observations or trip-level summaries),
    ###   and not from a previous summary of fish sizes (e.g., at the RYSMWA level )...
    
    rename( Trp = Trip )
  
  
  avgwgt.year <- avgwgt.table %>%

    group_by( Year ) %>%
    summarize( Fish = sum( N, na.rm=TRUE ),
               Trip = length( unique( MY_ID_CODE[ !is.na(MY_ID_CODE) ] ) ),
               Wgt  = mean( as.numeric(Avg), na.rm=TRUE ),
               Var  =  var( as.numeric(Avg), na.rm=TRUE ) ) %>%
    rename( Trp = Trip )
  
}




### CONVERTING VARIANCES TO STD.ERRORS AND CVs ###

if( method == 'trip' ) {
  
  avgwgt.mode = avgwgt.mode %>% mutate( SE = sqrt(Var) / sqrt(Trp) )
  avgwgt.year = avgwgt.year %>% mutate( SE = sqrt(Var) / sqrt(Trp) )
}
if( method == 'fish' ) {
  
  avgwgt.mode = avgwgt.mode %>% mutate( SE = sqrt(Var) / sqrt(Fish) )
  avgwgt.year = avgwgt.year %>% mutate( SE = sqrt(Var) / sqrt(Fish) )
}


avgwgt.mode = avgwgt.mode %>%
  mutate( CV = SE / Wgt ) %>%
  ungroup() %>%
  arrange_at( c('Year','new_mode') ) %>%
  select_at(  c('Year','new_moden', 'Wgt','SE','Trp','Fish') ) %>%
  pivot_wider( names_from=new_moden, values_from=c( Wgt,SE,Trp,Fish ) )

avgwgt.year <- avgwgt.year %>%
  mutate( CV = SE / Wgt ) %>%
  ungroup() %>%
  arrange_at( 'Year' ) %>%
  select_at(  c('Year', 'Wgt','SE','Trp','Fish') ) %>%
  rename(   Wgt_Total = Wgt,
            SE_Total = SE,
            Trp_Total = Trp,
            Fish_Total = Fish )




###############################
### JOIN & FINAL FORMATTING ###
###############################


avgwgt.table <- full_join( avgwgt.mode, avgwgt.year, by="Year" )
rm( avgwgt.mode, avgwgt.year )


### COMBINING the 'Fish' and 'Trp' fields into a SINGLE COLUMN (i.e., = Trp (Fish) )

dummy = avgwgt.table %>%
  mutate_at( vars(contains(c('Fish','Trp'))), list( ~ as.character( format( round(.,0), big.mark=',' ) ) ) ) %>%
  mutate_at( vars(contains(c('Fish','Trp'))), list( ~ ifelse( grepl('NA',.), NA, . ) ) )

modes = unique( gsub( '.*_','', colnames(dummy) ) )
modes = modes[ !( modes %in% c('Year') ) ]

for( i in 1:length(modes) ) {
  dummy <- unite( dummy, newcol,
                  c( paste0("Trp_",modes[i]),paste0("Fish_",modes[i]) ), sep=" (", remove=TRUE )
  dummy$newcol <- paste0( dummy$newcol,")" )
  colnames(dummy)[ which( colnames(dummy) == "newcol" ) ] <- paste0("N_",modes[i])
  
  dummy[ dummy[ colnames(dummy)==paste0("N_",modes[i]) ] == "NA (NA)" , paste0("N_",modes[i]) ] = NA
}

avgwgt.table = dummy
rm(dummy)



### RENAME & REORDER COLUMNS ###

avgwgt.table = avgwgt.table %>% rename_all( toupper )

loc = !( colnames(avgwgt.table) %in% c('YEAR') )
colnames(avgwgt.table)[loc] = paste0( gsub( ".*_","", colnames(avgwgt.table)[loc] ),"_",gsub( "_.*","", colnames(avgwgt.table)[loc] ) )
###     ...renaming the columns in 'avgwgt.table', which follow a variable-mode format, into a mode-variable format...
rm(loc)


avgwgt.cols <- vector()

col.IDs <- toupper(modes)
col.IDs = col.IDs[ order( match(col.IDs,c("CBT","CBTHBT","HBT","PRIV","PRIV_SHORE","SHORE","TOTAL")) ) ]

for( i in 1:length(col.IDs) ) {
  avgwgt.cols <- c( avgwgt.cols, paste0( col.IDs[i], c("_WGT","_SE","_N") ) )
}

avgwgt.table <- avgwgt.table %>% select_at( c( 'YEAR',avgwgt.cols ) )

rm( col.IDs, avgwgt.cols, modes )





####################################################################################################################
####################################################################################################################
####################################################################################################################
#############################                                                          #############################
#############################                   FINAL EXCEL WORKBOOK                   #############################
#############################                                                          #############################
####################################################################################################################
####################################################################################################################
####################################################################################################################


table.ID <- paste0( "YTL_rec_sizeGEN_",
                    substr( first.year, nchar(first.year)-1, nchar(first.year) ),
                    substr( term.year, nchar(term.year)-1, nchar(term.year) ),
                    "_", gsub("-","", Sys.Date() ) )


### Import template excel file with settings already saved for my pivots...
dir <- "C:/Users/matthew.nuttall/Desktop"
wb <- loadWorkbook( file=paste0( dir,"/Template_SEDAR_RawData_Size_Carib.xlsx" ) )


removeWorksheet( wb, sheet="MRIP_size" )
addWorksheet( wb, sheet='MRIP_size' )
writeData( wb, sheet='MRIP_size', x=size.table, colNames=TRUE )


writeData( wb, sheet="Weight Summary by Mode", x=avgwgt.table, colNames=TRUE )


removeWorksheet( wb, sheet="MRIP_meastrp" )
addWorksheet( wb, sheet="MRIP_meastrp" )
writeData( wb, sheet="MRIP_meastrp", x=mrip.trip.tab, colNames=TRUE )


saveWorkbook( wb, file=paste0( dir,"/",table.ID,".xlsx" ), overwrite=TRUE )



