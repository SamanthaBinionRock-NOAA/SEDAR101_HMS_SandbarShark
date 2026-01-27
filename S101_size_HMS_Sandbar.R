


####################################################################################################################
####################################################################################################################
####################################################################################################################
#############################                                                          #############################
#############################                      SIZE FILES                          #############################
#############################                                                          #############################
####################################################################################################################
####################################################################################################################
####################################################################################################################


### Loading the required libraries...
library(tidyverse)
library(reshape2)
#library(dplyr)
library(openxlsx)

library(haven)

###     ...and although I'm pulling (size) data from an ACL data file, I still load my Oracle credentials
###         so that I can import the "xref" data table, which I use to identify the spp of interest...
library(ROracle)

con = dbConnect(dbDriver("Oracle"), username = keyring::key_list("SECPR")[1,2],
                password = keyring::key_get("SECPR", keyring::key_list("SECPR")[1,2]), dbname = "SECPR")
spp.info = dbGetQuery(con, "SELECT * 
                     FROM RDI.v_species_xref@secapxdv_dblk.sfsc.noaa.gov")


`%notin%` <- Negate(`%in%`)



################################
######     ACL IMPORT     ######
################################


# ###   ...wherein the ACL file (on the desktop) might first need to be updated
# ###       (i.e., copy/paste current ACL size file to desktop, load into R, and save workspace )...
#
 dir <- getwd()
 dat <- read_sas(data_file = paste0( dir,"/Size/hms_size81_24final.sas7bdat" ))
# 
# save.image( file = paste0( dir,"/ACL_size.RData" ) )

#dir <- "C:/Users/matthew.nuttall/Desktop"
#load( paste0( dir,"/ACL_size.RData" ) )



##* *Are sharks in the average weight files - no*
 #s.dat <- read_sas(data_file = paste0( dir,"/Size/avgwgt_s.sas7bdat" ))

 
 
 unique(s.dat$NEW_COM)
 

####################################################################################################################
####################################################################################################################
####################################################################################################################
#############################                                                          #############################
#############################                       SEDAR 100                          #############################
#############################                   HMS Sandbar Shark                       #############################
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

states <- c("TX","LA","MS","AL","FLW","FLE","GA","SC","NC","VA","MD","DE","PA","NJ","NY","CT","RI","MA","NH","ME" )
###   ...which has options c( "TX","LA","MS","AL","FLW","FLE","GA","SC","NC","VA","MD","DE","PA","NJ","NY","CT","RI","MA","NH","ME" )
###   ...or c( "PR","VI" ) for Caribbean assessments...
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
 View( spp.info[grep( "SANDBAR", spp.info$COMMON ),] )
# View( spp.info[grep( "Centropristis striata", spp.info$SCIENTIFIC ),] )

#sppID.type = 'COMMON'
#taxa <- c( "SHARK,SANDBAR" )

sppID.type = 'SCIENTIFIC'
taxa <- c( "Carcharhinus plumbeus"  )

###   ...and then import (and apply) the relevant functions to identify the associated spp ID info for 'taxa'...
source( paste0(dir,'/Functions/lookup_sppID.R') )

new.com   <- sapply( taxa, FUN=  new.com.info, spp.field=sppID.type, spp.table=spp.info )
new.sci   <- sapply( taxa, FUN=  new.sci.info, spp.field=sppID.type, spp.table=spp.info )
nodc.code <- sapply( taxa, FUN=nodc.code.info, spp.field=sppID.type, spp.table=spp.info )
itis.code <- sapply( taxa, FUN=itis.code.info, spp.field=sppID.type, spp.table=spp.info )
tpwd.code <- sapply( taxa, FUN=tpwd.code.info, spp.field=sppID.type, spp.table=spp.info )

rm( sppID.type )



### ADDITIONAL FLAGS ###
###
### -------------------------------------------------------------------------------------------
###
###   ...to indicate...
###         (1) if the final "avgwgt.table" ( which contains CV estimates ) includes a different
###                 set of species than that included in "size.table" (i.e., the raw size data ).
###                 When 'flag.cv' = TRUE , this section of code also requires the user to identify...
###         (2) what species/taxa are to be included in the "avgwgt.table" of CVs...
###
###     The need for this flag is due to those SEDARs wherein some percentage of unidentified fish is to be allocated
###     to the assessed species (e.g., in S82, some percent of 'Balistidae' assumed = gray triggerfish ). In such cases,
###     size data for these unidentified fish are provided ( in 'size.table' ), but these unidentified records cannot be included
###     in the CV calculation because we cannot distinguish which records ( of unidentified fish ) should be treated as the
###     assessed species ( the only exception being if 100% of UnIDd fish are assumed to be the taxa-of-interest ).
###     The objects below, and the associated scripts, are developed to apply the necessary filters/processing...

flag.cv = TRUE

if( flag.cv ) {
  
  #sppID.type = 'COMMON'
  sppID.type = 'SCIENTIFIC'
  
  taxa.cv <- c( "Carcharhinus plumbeus" )
  
  nodc.cv <- sapply( taxa.cv, FUN=nodc.code.info, spp.field=sppID.type, spp.table=spp.info )
  rm( sppID.type )
  
} else {
  
  nodc.cv <- nodc.code
  
}

### -------------------------------------------------------------------------------------------
###
###   ...to flag SEDARs where catch & effort estimates are to be provided for a (combined) for-hire mode,
###       which largely corresponds to assessments that include the Mid- and North-Atlantic. The need for this flag
###       stems from the calculation of uncertainty estimates for SEFSC landings-in-weight, for which the resolution
###       (i.e., modes ) at which CVs for SEFSC avgwgts ( in the size file ) needs to match that at which CVs for
###       landings-in-number are provided ( in the corresponding catch file ).
###
###   *** This flag therefore identifies if additional steps are needed in aggregating raw size data so that CV estimates
###   *** for AVGWGT and AB1 are calculated at the same resolution. In particular, this flag identifies when size data for
###   *** the 'Cbt' and 'Hbt' modes (e.g., from the MATL/NATL) needs to be aggregated into a combined for-hire mode
###   *** (i.e., 'CbtHbt' = 'Cbt' + 'Hbt' )...
###
###   Briefly, OST combines Cbt and Hbt records for 'estimation purposes' in certain strata:
###
###     -- Gulf of Mexico (1981-1985) --
###       The SEFSC has its own method to partition combined for-hire estimates in the Gulf of Mexico,
###       which is routinely applied. No additional steps are therefore needed to modify for-hire
###       estimates in the GOM between 1981-1985.
###
###     -- North/Mid-Atlantic (1981-2003) --
###       Conversely, no 'best practices' approach has yet to be approved for partitioning 'CbtHbt'
###       estimates from the MATL/NATL. An effort-based (partitioning) approach was developed as part of S82,
###       but the appropriateness of this approach to other species/assessments has yet to be evaluated.
###
###   Nevertheless, the (effort-based) partitioned factors developed in S82 are not species-specific, and so
###       (conceptually) may be appropriate in other SEDARs as well. The 'flag.forhire' object identifies SEDARs
###       wherein additional steps are needed to aggregate the raw size data into a combined for-hire 'mode'
###       (i.e., to match that which will be provided in the catch file ).


flag.forhire = TRUE
###       ...where TRUE represents SEDARs where size data for the separate 'Cbt' & 'Hbt' modes need to be 
###         aggregated into a single 'CbtHbt' mode when estimating SEFSC avgwgt CVs...

###   *** NOTES ***
###
###     - This 'correction' should only be applied (flag=TRUE) when catch estimates from the MATL/NATL
###       include those for a combined for-hire mode. Those (combined) for-hire estimates from the GOM (1981-85)
###       are always partitioned (via SEFSC standard methods) and so the associated size data from the GOM
###       should not have to be aggregated.
###
###     - This 'correction' is only needed when estimating uncertainties (i.e., standard errors).
###       It is not applied when compiling the raw size data itself (i.e., in the 'size.table' object).


### -------------------------------------------------------------------------------------------







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

dat <- dat %>%
  arrange( SP_CODE, ID_CODE )





############################
######     FORMAT     ######
############################

###   ...where I apply a basic rename() command to ensure field names stay consistent across scripts
###     (e.g., the year field in our views varies between 'year', 'YEAR', 'INT_YEAR', etc ).
###     Note the use of any_of(), which only renames fields if they exist (ignore if otherwise)...

check.cols <- c( DS = 'ds',
                 NEW_COM = 'new_com', NEW_SCI = 'new_sci', SP_CODE = 'sp_code', ITIS_CODE = 'itis_code',
                 SUB_REG = 'sub_reg', NEW_ST  = 'new_st' , NEW_STA = 'new_sta',
                                      FL_REG  = 'fl_reg' , NC_REG  = 'nc_reg' ,
                 YEAR = 'year', YEAR = 'INT_YEAR',
                 WAVE = 'wave', WAVE = 'INT_WAVE',
                 NEW_MODE = 'new_mode', NEW_MODEN = 'new_moden',
                 NEW_AREA = 'new_area', NEW_AREAN = 'new_arean' )

dat <- dat %>% rename( any_of(check.cols) )

rm( check.cols )


if( 'SPECIES_ITIS' %notin% colnames(dat) & 'ITIS_CODE' %in% colnames(dat) ) {
  dat = dat %>% rename( SPECIES_ITIS = ITIS_CODE )
}


if( 'ALL_LBS_ORI' %in% colnames(dat) ) {
  ###     ...which was used to store observations of fish weight (in RDI)
  ###             before any length:weight conversions were applied to MRIP size data...
  
  if( 'ALL_LBS' %in% colnames(dat) ) {
    dat = dat %>% select( -ALL_LBS )
  }
    dat = dat %>% rename( ALL_LBS = ALL_LBS_ORI )
}

if( 'FL_MM' %in% colnames(dat) ) {    dat = dat %>% rename( FL_mm = FL_MM )   }
if( 'TL_MM' %in% colnames(dat) ) {    dat = dat %>% rename( TL_mm = TL_MM )   }
if( 'ALL_LBS' %in% colnames(dat) ) {  dat = dat %>% rename( all_lbs = ALL_LBS )   }
if( 'WGTFLG'  %in% colnames(dat) ) {  dat = dat %>% rename( wgtflg  = WGTFLG  )   }





#############################
######     FILTERS     ######
#############################


### SPECIES ###
size.table <- dat[ which( dat$SP_CODE %in% nodc.code ), ]


### TEMPORAL ###
size.table <- size.table[ which( size.table$YEAR %in% (first.year:term.year) ), ]
# size.table <- size.table[ which( size.table$year %in% (first.year:term.year) ), ]


### SPATIAL ###

if( region == "Caribbean" ) {
  
  size.table <- size.table[ which( size.table$SUB_REG == 11 ), ]          ### ...where can look at all Caribbean...
  size.table <- size.table[ which( size.table$new_sta %in% states ), ]    ### ...or just a part of it (usually keep PR, but drop USVI)...
  
} else {
  
  size.table <- size.table[ which( size.table$NEW_STA %in% states ), ]
  
  if( "FL" %in% states | "FLW" %in% states | "FLE" %in% states ) {
    size.table <- size.table[ which(
      is.na(size.table$FL_REG) | size.table$FL_REG == "" | size.table$FL_REG %in% FL_sub ), ]
  }
  if( "NC" %in% states ) {
    size.table <- size.table[ which(
      is.na(size.table$NC_REG) | size.table$NC_REG == "" | size.table$NC_REG %in% NC_sub ), ]
  }
  
}


### MODE ###
size.table <- size.table[ which( size.table$NEW_MODEN %in% mode_sub ), ]

###     ...for which I exclude any MRIP sampling from LA during the years of the LA_Creel survey (2014+)...
size.table <- size.table[ !( size.table$DS == "MRIP" & size.table$NEW_STA == "LA" & size.table$YEAR >= 2014 ), ]
###     ...and any LDWF sampling during those years within which MRIP operated in LA (1981-2013)...
size.table <- size.table[ !( size.table$DS %in% c("LA BIO","LA Creel") & size.table$NEW_STA == "LA" & size.table$YEAR <= 2013 ), ]

###     ...and to avoid duplicating SRHS information, I remove all "Hbt" fishing in SUB_REG==6...
size.table <- size.table[ !( size.table$NEW_MODEN == "Hbt" & size.table$SUB_REG == 6 ), ]
###     ...and "Hbt" fishing from 1986+ in SUB_REG==7...
size.table <- size.table[ !( size.table$NEW_MODEN == "Hbt" & size.table$SUB_REG == 7 & size.table$YEAR >= 1986 ), ]
###     ...and "Hbt" fishing from Monroe County (FL_REG==3)...
size.table <- size.table[ !( size.table$NEW_MODEN == "Hbt" & size.table$NEW_STA == "FLW" & size.table$FL_REG == 3 ), ]



### ADDITIONAL FILTERS ###

### I also remove any records where fish size is <NA>...
size.table <- size.table[ !( is.na(size.table$FL_mm) & is.na(size.table$TL_mm) & is.na(size.table$all_lbs) ), ]
###         ...where there shouldn't be any <NA> records in the completed RDI views that Vivian and I use,
###           but I add this line in case we have to use the raw tables (e.g., Caribbean assessments)...

###   ...and, as of Oct 6 2021, remove any records where weight observations are above the max (allowable) size,
###           which are identified with wgtflg = 'Y' and added as part of wgtest_step2 ...
size.table <- size.table[ size.table$wgtflg != 'Y' , ]




# ### FORMATTING ###
# ###
# ###     ...where I add the required size fields if they are not already defined
# ###           ( these are the fields that are summarized in my final pivot table )...
# 
# '%notin%' = Negate( '%in%' )
# 
# if( "fl_mm" %notin% tolower( colnames(size.table) ) ) {
#   size.table$FL_mm = size.table$LNGTH                                     ### native MRIP field
#   size.table$FL_mm[ size.table$ds %in% c("LA BIO","LA Creel") ] =
#     size.table$LENGTH_1[ size.table$ds %in% c("LA BIO","LA Creel") ]      ### native LA BIO field
# }
# if( "tl_mm" %notin% tolower( colnames(size.table) ) ) {
#   size.table$TL_mm = size.table$length                                    ### native TPWD field
#   size.table$TL_mm[ size.table$ds %in% c("LA BIO","LA Creel") ] =
#     size.table$LENGTH_3[ size.table$ds %in% c("LA BIO","LA Creel") ]      ### native LA BIO field
# }
# if( "lbs_wwt" %notin% tolower( colnames(size.table) ) ) {
#   size.table$lbs_wwt = size.table$WGT * 2.20462                           ### native MRIP field ( kg->lbs )
#   size.table$lbs_wwt[ size.table$ds %in% c("LA BIO","LA Creel") ] =
#     size.table$WHOLE_WEIGHT_KILO[ size.table$ds %in% c("LA BIO","LA Creel") ] * 2.20462     ### native LA BIO field ( kg->lbs )
# }
# ###     Note that this renaming convention won't work if imputations are turned on... Specifically, depending on the
# ###     LW conversion being applied, some TL estimates may be converted to FL before wgt estimates are calculated (and vice versa),
# ###     wherein simply defining FL_mm and TL_mm from the raw MRIP & TPWD data (respectively) will miss these imputations.
# ###     The above code won't overwrite these fields (simply create them if they're missing), but there are other issues that
# ###     will need to be sorted out if these fields do not exist...




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



### ---------------------------------------------------------------------------------------------------------------- ###
### ---------------------------------------------------------------------------------------------------------------- ###


####################################
######     Assign StockID     ######
######       Boundaries       ######
####################################

source( paste0(dir,'/Functions/assign_stockID.R') )

size.table <- assign.stockID( new.com = new.com, region = region, genrec.table = size.table )


### ---------------------------------------------------------------------------------------------------------------- ###
### ---------------------------------------------------------------------------------------------------------------- ###


########################################
######     Assign Open/Closed     ######
######       Federal Seasons      ######
########################################

source( paste0(dir,'/Functions/assign_FishingSeason.R') )

size.table <- assign.fishing.season( new.com = new.com, region = region, genrec.table = size.table )


### ---------------------------------------------------------------------------------------------------------------- ###
### ---------------------------------------------------------------------------------------------------------------- ###




# size.summary = size.table %>%
#   group_by( YEAR, NEW_MODEN ) %>%
#   summarise( LBS = mean( LBS, na.rm=TRUE ) ) %>%
#   pivot_wider( names_from = NEW_MODEN, values_from = LBS )





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
  
  sedar.comparison <- size.table %>%
    group_by( YEAR ) %>%
    summarize( N = length(!is.na(all_lbs)),
               #N     = length( all_lbs[ !is.na(all_lbs) ] ),
               AvgWt = mean( as.numeric( all_lbs), na.rm=TRUE ) ) %>%
    select( YEAR, N, AvgWt )
  
  ### I also make sure all the years (between "first.year" and "term.year") are accounted for in this table...
  assess.years <- first.year:term.year
  
  for( i in 1:length(assess.years) ) {
    if( assess.years[i] %notin% sedar.comparison$YEAR ) {
      sedar.comparison <- rbind( data.frame( sedar.comparison ), data.frame( YEAR=assess.years[i], N=0, AvgWt=0 ) )
    } }
  rm( assess.years )
  
  ### I then...
  sedar.comparison <- sedar.comparison %>%
    arrange( YEAR ) %>%                                             ### ...sort by year...
    mutate_at( c("N"    ), round, digits=0 ) %>%                   ### ...round the "N" column to zero decimal places...
    mutate_at( c("AvgWt"), round, digits=4 )                       ### ...round the "AvgWt" columns to four decimal places...
  sedar.comparison <- sedar.comparison %>%                          ### ...and add a new column to identify the old assessment
    add_column( old_sedar=sedar.comparison$YEAR, .after=3 )         ###   ( which has values=YEAR and colname='prev.sedar' )
  colnames(sedar.comparison)[ which( colnames(sedar.comparison)=="old_sedar" ) ] <- prev.sedar
  colnames(sedar.comparison)[ which( colnames(sedar.comparison)=="YEAR" ) ] <- current.sedar
  
}






####################################################################################################################
####################################################################################################################
####################################################################################################################
#############################                                                          #############################
#############################                       NUMBER OF TRIPS                    #############################
#############################                                                          #############################
####################################################################################################################
####################################################################################################################
####################################################################################################################


source( paste0(dir,'/Functions/format_TRIPdata.R') )


##########################
######     MRIP     ######
##########################

mrip.trip.tab <- size.table %>%
  filter( DS %in% c("MRFSS","MRIP") ) %>%
  arrange(  SP_CODE, YEAR, ID_CODE ) %>%
  distinct( SP_CODE, YEAR, ID_CODE, .keep_all = TRUE )
  ###     ...where 'SP_CODE' is included to account for SEDARs where data is pulled for more than one species,
  ###               ( in that both species could be landed on the same fishing trip ),
  ###     ...and 'YEAR' is included to account for any MRIP imputations that filled 2020 COVID data gaps
  ###             ( although OST updated STRAT_ID and PSU_ID with a '2020' identifier, ID_CODE was left at its
  ###               2018/2019 value to allow analysts to trace any 2020 imputations back to their original record )...

mrip.trip.tab = filter.MRIPtrip( mrip.trip.tab )


##########################
######     TPWD     ######
##########################

if( "TX" %in% states ) {
  
  tpwd.trip.tab <- size.table %>%
    filter( DS == 'TPWD' ) %>%
    arrange(  SP_CODE, ID_CODE ) %>%
    distinct( SP_CODE, ID_CODE, .keep_all = TRUE )
  
  tpwd.trip.tab = filter.TPWDtrip( tpwd.trip.tab )

}



##########################
######     LACR     ######
##########################

if( "LA" %in% states ) {
  
  lacr.trip.tab <- size.table %>%
    filter( DS %in% c("LA BIO","LA Creel") ) %>%
    arrange(  SP_CODE, ID_CODE ) %>%
    distinct( SP_CODE, ID_CODE, .keep_all = TRUE )
  
  lacr.trip.tab = filter.LACRtrip( lacr.trip.tab )

}






####################################################################################################################
####################################################################################################################
####################################################################################################################
#############################                                                          #############################
#############################                       AVGWGT SUMMARY                     #############################
#############################                                                          #############################
####################################################################################################################
####################################################################################################################
####################################################################################################################



dummy.table = size.table %>%
  
  filter( SP_CODE %in% nodc.cv ) %>%
  
  mutate( MY_ID_CODE = ifelse( DS %in% c("MRIP","MRFSS"),      paste0( "M", ID_CODE, YEAR ),
                       ifelse( DS     == "TPWD",               paste0( "T", ID_CODE ),
                       ifelse( DS %in% c("LA Creel","LA BIO"), paste0( "L", ID_CODE ), NA ))) )

if( flag.forhire ) {
  ###   ...where the presence of combined for-hire estimates in our standard data provisions only pertain
  ###       to the MATL/NATL regions between 1981-2003. When such (combined) for-hire estimates are provided,
  ###       a similar aggregation of the raw size data is applied...
  
  dummy.table = dummy.table %>%
    mutate( NEW_MODE  = ifelse( YEAR %in% 1981:2003 & NEW_MODEN %in% c('Cbt','Hbt') &
                                NEW_STA %in% c( "VA","MD","DE","PA","NJ","NY","CT","RI","MA","NH","ME" ), 5, NEW_MODE ),
            NEW_MODEN = ifelse( YEAR %in% 1981:2003 & NEW_MODEN %in% c('Cbt','Hbt') &
                                NEW_STA %in% c( "VA","MD","DE","PA","NJ","NY","CT","RI","MA","NH","ME" ), 'CbtHbt', NEW_MODEN ),
            MODE_FX   = ifelse( YEAR %in% 1981:2003 & NEW_MODEN %in% c('Cbt','Hbt') &
                                NEW_STA %in% c( "VA","MD","DE","PA","NJ","NY","CT","RI","MA","NH","ME" ), 6, MODE_FX ) )
}




source( paste0(dir,'/Functions/calc_CVs_avgwgt.R') )

dummy.table1 <- dummy.table %>%
  group_by(YEAR, NEW_MODEN) %>%
  summarise(nfish = n(),
            ntrip = length(unique(MY_ID_CODE)))




avgwgt.table <- CVs.avgwgt( genrec.table = dummy.table,
                            
                            # data = 'trip', estimation = 'RYSMWA',
                            #data = 'trip', estimation =     'YM',
                             data = 'fish', estimation =     'YM',
                            
                            add.strata = 'none'
                            # add.strata = c('SID')
                            # add.strata = c('SID','fed_closed')
                          )

###     ...where 'data' identifies the original 'data units' from which mean & SEs are to be calculated
###                 ( 'trip' = trip-level summaries  ;  'fish' = raw observations of fish size ),
###     ...'estimation' identifies the resolution at which mean & SE calculations are to be conducted
###                 ( 'RYSMWA' = region-year-state-mode-wave-area   ;   'YM' = year-mode ),
###     ...'add.strata' identifies any strata that are to be added to the estimation (e.g., 'SID' ), and


# ### ***********************************************************************************************
# ### Alternatively, the 'avgwgt.table' above can be created by applying different estimation approaches
# ### (i.e., by-trip vs. fish, 'RYSMWA' vs. 'YM' ) to different modes, which may be preferable in cases where
# ### sample size issues are limited to one/two modes. The script below was developed for such an approach...
# 
# ###     ...which requires identifying the modes contained in 'dummy.table' and how avgwgt CVs are to be estimated...
# unique.modes = unique( dummy.table$NEW_MODEN )
# data.aggregation = c( 'trip','fish' )
# est.resolution   = c( 'YM','YM' )
# 
# ###     ...which also requires specificaiton of how CVs are to be calculated for the annual 'TOTAL'...
# unique.modes = c( unique.modes, "Total" )
# data.aggregation = c( data.aggregation,'trip' )
# est.resolution   = c( est.resolution,'YM' )
# 
# for( i in 1:length(unique.modes) ) {
#   
#   if( unique.modes[i] != "Total" ) {
#     blah1 = dummy.table %>% filter( NEW_MODEN == unique.modes[i] )
#     blah2 = CVs.avgwgt( genrec.table = blah1,
#                         data = data.aggregation[i],
#                         estimation = est.resolution[i],
#                         
#                         add.strata = 'none'
#                         # add.strata = c('SID')
#                         # add.strata = c('SID','fed_closed')
#                       )
#     blah2 = blah2 %>% select( -contains('TOTAL') )
#     if( i==1 ) {
#       blah = blah2
#     } else {
#       blah = full_join( blah, blah2,
#                         by=c('YEAR')
#                         # by=c('SID','YEAR')
#                       )
#     }
#     rm( blah1, blah2 )
#     
#   } else {
#     blah2 = CVs.avgwgt( genrec.table = dummy.table,
#                         data = data.aggregation[i],
#                         estimation = est.resolution[i],
#                         
#                         add.strata = 'none'
#                         # add.strata = c('SID')
#                         # add.strata = c('SID','fed_closed')
#                       )
#     blah2 = blah2 %>% select( -contains( toupper(unique( dummy.table$NEW_MODEN )) ) )
#     blah = full_join( blah, blah2,
#                       by=c('YEAR')
#                       # by=c('SID','YEAR')
#                     )
#     rm( blah2 )
#   }
#   
# }
# 
# avgwgt.table = blah
# 
# rm( blah, unique.modes, data.aggregation, est.resolution )
# 
# ### ***********************************************************************************************


rm(dummy.table)



###   ...and, as a final step, the 'Average Weight' CV table is converted into a long-format...
avgwgt.table = convert.long.table.avgwt( avgwgt.table = avgwgt.table,
                                         possible.strata = "none"
                                          #possible.strata = c('SID')
                                        )
###     ...where 'possible.strata' is a running list of all fields/variables for which CVs have been requested
###                 in the past and might be requested in the future. Note that these variables don't have to be
###                 included in 'avgwgt.table', its just that the code will properly 'handle' them if they are...






####################################################################################################################
####################################################################################################################
####################################################################################################################
#############################                                                          #############################
#############################             APPLYING THE BIOLOGICAL TEMPLATE             #############################
#############################                                                          #############################
####################################################################################################################
####################################################################################################################
####################################################################################################################



source( paste0(dir,'/Functions/format_BIOtemplate.R') )

size.table.BIO <- format.BIOtemplate( genrec.table = size.table )
                                      # add.strata = c('fed_closed') )
###     ...where 'add.strata' identifies any additional strata that is not identified in the template,
###         but which has been requested to be retained for the assessment. Note that there is specific
###         code in the format.BIOtemplate() function to look for and retain 'SID', and so the 'SID' field
###         does not need to be included in 'add.strata'...









####################################################################################################################
####################################################################################################################
####################################################################################################################
#############################                                                          #############################
#############################                   FINAL EXCEL WORKBOOK                   #############################
#############################                                                          #############################
####################################################################################################################
####################################################################################################################
####################################################################################################################


table.ID <- paste0( "Size/SBS_rec_sizeGEN_",
                    substr( first.year, nchar(first.year)-1, nchar(first.year) ),
                    substr( term.year, nchar(term.year)-1, nchar(term.year) ),
                    "_", gsub("-","", Sys.Date() ) )


### Import template excel file with settings already saved for my pivots...
dir <- getwd()
wb <- loadWorkbook( file=paste0( dir,"/Size/Template_SEDAR_RawData_Size_fromACL_v2.xlsx" ) )


### Raw Size Data ###
###
removeWorksheet( wb, sheet="GenRec_size" )
# removeWorksheet( wb, sheet="MRIP_size" )

if( any( grepl( "TX|LA", states ) ) ) {   sheet.name='GenRec_size'   } else {    sheet.name='MRIP_size'   }
addWorksheet( wb, sheet=sheet.name )

if( exists('size.table.BIO') ) {
  writeData( wb, sheet=sheet.name, x=size.table.BIO, colNames=TRUE )
} else {
  writeData( wb, sheet=sheet.name, x=size.table, colNames=TRUE )
}
rm(sheet.name)


### Comparisons to Previous SEDAR ###
###
if( prev.sedar != "None" ) {
  writeData( wb, sheet="Compare Previous SEDARs", x=sedar.comparison, colNames=TRUE )
} else {
  removeWorksheet( wb, sheet="Compare Previous SEDARs" )
}


### AvgWt CVs ###
###
###     ...which was originally provided as separate tables (i.e., excel tabs ) for unique SID domains...
# 
# if( 'SID' %in% colnames(size.table) ) {
#   
#   SID.dummy = unique( size.table$SID[ order(size.table$NEW_ST) ] )
#   ###     ...where 'SID.dummy' identifies each SID boundary in geographic order, such that the
#   ###         associated (size) uncertainty tables will also be geographically ordered...
#   
#   for( i in 1:length(SID.dummy) ) {
#     
#     dummy.table = avgwgt.table %>% filter( SID == SID.dummy[i] ) %>% select( -SID )
#     
#     cloneWorksheet( wb,
#                     sheetName = paste0( toupper(SID.dummy[i]),"_AvgWgt by Mode" ),
#                     clonedSheet = "Weight Summary by Mode" )
#     writeData( wb, sheet=paste0( toupper(SID.dummy[i]),"_AvgWgt by Mode" ), x=dummy.table, colNames=TRUE )
#   }
#   
#   removeWorksheet( wb, sheet="Weight Summary by Mode" )
#   rm( SID.dummy, i, dummy.table )
#   
# } else {
#   
#   writeData( wb, sheet="Weight Summary by Mode", x=avgwgt.table, colNames=TRUE )
#   
# }
# 
###     ...but all estimates are now provided by a single (long-format) table, but where I still pivot the
###         'METRIC' field into distinct columns ( keeping YEAR/SID/.../MODE values as unique rows )...

if( 'METRIC' %in% colnames(avgwgt.table) ) {
  avgwgt.table = avgwgt.table %>% pivot_wider( names_from=METRIC, values_from=value )   }

writeData( wb, sheet="Weight Summary by Mode", x=avgwgt.table,
           startCol = 10 - dim(avgwgt.table)[2] + 1, colNames=TRUE )
###   ...where the first column at which the 'avgwgt.table' table is printed in the "Weight Summary by Mode" tab
###     is based on its dimensions (i.e., whether additional fields/strata are included in the table )...



### Sample Size - NTRIPS ###
###
removeWorksheet( wb, sheet="MRIP_meastrp" )
addWorksheet( wb, sheet="MRIP_meastrp" )
writeData( wb, sheet="MRIP_meastrp", x=mrip.trip.tab, colNames=TRUE )

if( 'TX' %in% states ) {
  
  removeWorksheet( wb, sheet="TPWD_meastrp" )
  if( dim(tpwd.trip.tab)[1] > 0 ) {
    addWorksheet( wb, sheet="TPWD_meastrp" )
    writeData( wb, sheet="TPWD_meastrp", x=tpwd.trip.tab, colNames=TRUE )
  } else {
    # removeWorksheet( wb, sheet="TPWD_meastrp" )
    removeWorksheet( wb, "TPWDtrp_pivot" )
  }
  
} else {
  removeWorksheet( wb, "TPWD_meastrp" )
  removeWorksheet( wb, "TPWDtrp_pivot" )
}

if( 'LA' %in% states ) {
  
  removeWorksheet( wb, sheet="LACR_meastrp" )
  if( dim(lacr.trip.tab)[1] > 0 ) {
    addWorksheet( wb, sheet="LACR_meastrp" )
    writeData( wb, sheet="LACR_meastrp", x=lacr.trip.tab, colNames=TRUE )
  } else {
    # removeWorksheet( wb, sheet="LACR_meastrp" )
    removeWorksheet( wb, "LACRtrp_pivot" )
  }
  
} else {
  removeWorksheet( wb, "LACR_meastrp" )
  removeWorksheet( wb, "LACRtrp_pivot" )
}


saveWorkbook( wb, file=paste0( dir,"/",table.ID,".xlsx" ), overwrite=TRUE )







# ####################################
# #####     TROUBLE-SHOOTING     #####
# ####################################
# 
# ### In developing the code, naturally there have been a few issues pop-up requiring attention.
# ###       Part of this process is in identifying problematic records and so to explore the data...
# 
# which( is.na(size.table$FL_mm) & is.na(size.table$TL_mm) )
# 
# which( size.table$ID_CODE == "" & size.table$ds == "MRIP" )
# which( size.table$TRIP_KEY == "" & size.table$ds == "TPWD" )





