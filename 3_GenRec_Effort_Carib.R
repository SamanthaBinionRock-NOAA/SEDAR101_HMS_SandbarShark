



####################################################################################################################
####################################################################################################################
####################################################################################################################
#############################                                                          #############################
#############################                      -CARIBBEAN SEDAR-                   #############################
#############################                     GENREC EFFORT FILES                  #############################
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

# ###     ...and in case I need to load RDI's "xref" table at any point...
# con = dbConnect(dbDriver("Oracle"), username = keyring::key_list("SECPR")[1,2],
#                 password = keyring::key_get("SECPR", keyring::key_list("SECPR")[1,2]), dbname = "SECPR")
# spp.info = dbGetQuery(con, "SELECT * 
#                      FROM RDI.v_species_xref@secapxdv_dblk.sfsc.noaa.gov")




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


# first.year <- 1981
first.year <- 2000
term.year  <- 2017


region <- "Caribbean"
states <- c( "PR" )
# states <- c( "TX","LA","MS","AL","FLW" )     ### ...where I retain "FLW" for FL_REG=3...
###     ...which has options c( "TX","LA","MS","AL","FLW","FLE","GA","SC","NC","VA","MD","DE","PN","NJ","NY","CT","RI","MA","NH","ME" )
###     ...or c( "PR","VI" ) for Caribbean assessments...
# if( "FL" %in% states | "FLW" %in% states | "FLE" %in% states ) {  FL_sub <- c( 1,2 )  }
# if( "NC" %in% states ) {                                          NC_sub <- c( "N","S" )  }


mode_sub <- c( "Priv","Cbt","Shore" )
# mode_sub <- c( "Priv","Cbt","Hbt","Shore" )


### Unlike the filtering applied in generating the size and catch files, effort estimates are not species-specific
###       and so there is no need to identify the species codes corresponding to the assessed species...





####################################################################################################################
####################################################################################################################
####################################################################################################################
#############################                                                          #############################
#############################                   GENERATE EFFORT TABLE                  #############################
#############################                                                          #############################
####################################################################################################################
####################################################################################################################
####################################################################################################################




################################
######     FORMATTING     ######
################################

###   ...formatting to combine the two effort files...
effort_old$ds = "MRIP"

colnames(effort_new)[ which( colnames(effort_new) == "mode_fx" ) ] = "MODE_FX"
colnames(effort_new)[ which( colnames(effort_new) == "area_x"  ) ] = "AREA_X"
colnames(effort_new)[ which( colnames(effort_new) == "estrips" ) ] = "ESTRIPS"
colnames(effort_new)[ which( colnames(effort_new) == "numvar"  ) ] = "NUMVAR"

###   ...and the join...
dat = full_join( effort_old,
                 effort_new,
                 by = colnames(effort_new)[ which( colnames(effort_new) %in% colnames(effort_old) ) ] )




################################
######     SUBSETTING     ######
################################



mrip.raw = dat[ which( dat$ds == "MRIP" ), ]


### TEMPORAL ###
mrip.effort <- mrip.raw[ which( mrip.raw$YEAR %in% (first.year:term.year) ), ]


### SPATIAL ###

if( region == "Caribbean" ) {
  
  mrip.effort <- mrip.effort[ which( mrip.effort$SUB_REG == 11 ), ]          ### ...where can look at all Caribbean...
  mrip.effort <- mrip.effort[ which( mrip.effort$new_sta %in% states ), ]    ### ...or just a part of it (usually keep PR, but drop USVI)...
  
} else {
  
  # mrip.effort <- mrip.effort[ which( mrip.effort$NEW_STA %in% states ), ]
  # 
  # if( "FL" %in% states | "FLW" %in% states | "FLE" %in% states ) {
  #   mrip.effort <- mrip.effort[ which(
  #     is.na(mrip.effort$FL_REG) | mrip.effort$FL_REG == "" | mrip.effort$FL_REG %in% FL_sub ), ]
  # }
  # if( "NC" %in% states ) {
  #   mrip.effort <- mrip.effort[ which(
  #     is.na(mrip.effort$NC_REG) | mrip.effort$NC_REG == "" | mrip.effort$NC_REG %in% NC_sub ), ]
  # }
  
}


### MODE ###

if( region == "Caribbean" ) {
  
  mrip.effort <- mrip.effort[ which( mrip.effort$new_moden %in% mode_sub ), ]
  
} else {
  
  # effort.modes = mode_sub
  # if( "Priv" %in% effort.modes ) {    effort.modes = c( effort.modes,"Priv/Shore" )   }
  # if( "Cbt" %in% effort.modes | "Hbt" %in% effort.modes ) {    effort.modes = c( effort.modes,"Cbt/Hbt" )    }
  # mrip.effort <- mrip.effort[ which( mrip.effort$NEW_MODEN %in% effort.modes ), ]
  # rm( effort.modes )
  # 
  # mrip.effort <- mrip.effort[ !( mrip.effort$DS == "MRIP" & mrip.effort$NEW_STA == "LA" & mrip.effort$YEAR >= 2014 ), ]
  # mrip.effort <- mrip.effort[ !( mrip.effort$DS == "MRIP" & mrip.effort$NEW_MODEN == "Hbt" & mrip.effort$SUB_REG == 6 ), ]
  # mrip.effort <- mrip.effort[ !( mrip.effort$DS == "MRIP" & mrip.effort$NEW_MODEN == "Hbt" &
  #                                  mrip.effort$SUB_REG == 7 & mrip.effort$YEAR >= 1986 ), ]
  # mrip.effort <- mrip.effort[ !( mrip.effort$DS == "MRIP" & mrip.effort$NEW_MODEN == "Hbt" &
  #                                  mrip.effort$NEW_STA == "FLW" & mrip.effort$FL_REG == 3 ), ]

}



# mrip.summary = mrip.effort %>%
#   group_by( YEAR, new_moden ) %>%
#   summarise( ANGTRP = sum( ESTRIPS, na.rm=TRUE ) ) %>%
#   pivot_wider( names_from=new_moden, values_from=ANGTRP )
# View( mrip.summary )







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
#############################                   FINAL EXCEL WORKBOOK                   #############################
#############################                                                          #############################
####################################################################################################################
####################################################################################################################
####################################################################################################################


table.ID <- paste0( "YTL_rec_effGEN_",
                    substr( first.year, nchar(first.year)-1, nchar(first.year) ),
                    substr( term.year, nchar(term.year)-1, nchar(term.year) ),
                    "_", gsub("-","", Sys.Date() ) )


### Import template excel file with settings already saved for my pivots...
dir <- "C:/Users/matthew.nuttall/Desktop"
wb <- loadWorkbook( file=paste0( dir,"/Template_SEDAR_GenEffort_Carib.xlsx" ) )

removeWorksheet( wb, sheet="MRIP" )
addWorksheet( wb, sheet="MRIP" )
writeData( wb, sheet="MRIP", x=mrip.effort, colNames=TRUE )


saveWorkbook( wb, file=paste0( dir,"/",table.ID,".xlsx" ), overwrite=TRUE )







