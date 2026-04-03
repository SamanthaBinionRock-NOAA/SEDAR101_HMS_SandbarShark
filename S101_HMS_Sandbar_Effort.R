


####################################################################################################################
####################################################################################################################
####################################################################################################################
#############################                                                          #############################
#############################                     EFFORT FILES                         #############################
#############################                                                          #############################
####################################################################################################################
####################################################################################################################
####################################################################################################################


### Loading the required libraries...
library(ROracle)
library(tidyverse)
library(reshape2)
library(dplyr)
library(openxlsx)

###     ..and my login credentials for Oracle...
con = dbConnect(dbDriver("Oracle"), username = keyring::key_list("SECPR")[1,2],
                password = keyring::key_get("SECPR", keyring::key_list("SECPR")[1,2]), dbname = "SECPR")

### I also load...
spp.info = dbGetQuery(con, "SELECT * 
                     FROM RDI.v_species_xref@secapxdv_dblk.sfsc.noaa.gov")
###     ...to identify appropriate ITIS/NODC codes, common & scientific names, whether the stock-of-interest
###          is considered an "ACL" or "HMS" species, etc...


`%notin%` <- Negate(`%in%`)


dir <- getwd()




####################################################################################################################
####################################################################################################################
####################################################################################################################
#############################                                                          #############################
#############################                       SEDAR 101                          #############################
#############################                      HMS Sandbar Shark                      #############################
#############################                                                          #############################
####################################################################################################################
####################################################################################################################
####################################################################################################################



###       ...for this assessment, data is pulled for...
###


###       ...for this assessment, data is pulled for...
###
###           Sandbar Shark - Carcharhinus plumbeus
###               - Temporal - include 1981-2024
###               - Modes    - includes charter, private, shore, and headboat 
###               - Spatial  - ME to TX




#############################
######     FILTERS     ######
#############################



### TEMPORAL ###
first.year <- 1981
# first.year <- 2000
# first.year <- 2018
###       ...where "first.year" = 1981 in all requests except projections (which only need a couple years)
###           and Caribbean assessments (wherein MRIP started in 2000)...
term.year <- 2024


### SPATIAL ###
region <- "Atlantic"
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
mode_sub <- c( "Priv","Cbt","Hbt","Shore")
#       ...which has options c( "Priv","Cbt","Hbt","Shore" )
###           Note that the code below removes all HBT fishing from SUB_REG = 6 (SATL), FL_REG = 3 (FL Keys),
###           and SUB_REG = 7 from 1986+, all of which is designed to avoid overlap with SRHS...








### ADDITIONAL OBJECTS ###
###
### -------------------------------------------------------------------------------------------


### IMPUTATIONS ###
### ---------------
###
###       ...where, as a "best practice" ( https://github.com/SEFSC/SFD-DAAS-GenRec/issues/8 ),
###         we always impute any 'missing' estimates of effort (i.e., MRIP 1981-wave1, TPWD 1981-1983 ),
###         even in cases where the associated catch estimates weren't imputed. Additionally, the
###         applied imputation method is always that which is considered the "best practice" method:
###
###   -- MRIP 1981-wave1 --
###       ...for which, based on SEDAR best practices ( SEDAR PW7 -- Recreational Issue #2 ), two approaches may be applied...
###         (1) the proportion of wave1 effort to that from other waves (2-6) in years 1982-1984 by fishing mode and area.
###             These proportions can then be applied to the total effort from waves 2-6 in 1981 to estimate 1981 wave 1 effort...
###         (2) the average (wave1) effort across years 1982-1984...
###       ...where the ratio method (#1) is the preferred method and applied when ratios are reasonably stable from year to year.
###       Sam did an evaluation of the stability of these (effort) ratio's, which are not species-specific and won't change
###       between SEDARs, and found them to be adequate
###
#method.MRIP.1981w1 = 'None'
#method.MRIP.1981w1 = 'avg_82_84'

method.MRIP.1981w1 = 'prop_w1_w26'


###   -- TPWD 1981-1983 --
###       ...for which, based on SEDAR best practices ( SEDAR PW7 -- Rec Issue #3 ), averages of TPWD estimates from 1983-1985
###       (by mode and wave) can be used to impute estimates for years before the TPWD survey was considered standard:
###         -- Texas effort in waves 1-2 for years 1981-1983 (imputed from TPWD estimates in 1984-1985 -- two-year avgs )
###         -- Texas effort in waves 3-6 for years 1981-1982 (imputed from TPWD estimates in 1983-1985 -- three-year avgs )
###
#method.TPWD.1981.83 = 'None'

method.TPWD.1981.83 = 'avg_83_85'


###   Note that there are other imputations that may be applied in a given SEDAR ( namely for TPWD & LACR discards ),
###   but these approaches do not need to be considered in this script as the survey was in operation for these strata
###   (i.e., effort estimates exist ), it's just that discard data wasn't being collected...

### -------------------------------------------------------------------------------------------





### ADDITIONAL FLAGS ###
###
### -------------------------------------------------------------------------------------------
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
###     ...where we may be asked to calibrate between effort estimates in state units vs. those from MRIP
###     (i.e., in MRIP-FES units), for which we have to apply calibration factors (e.g., STATE * CAL = MRIP )
###     that may have been developed as part of the SEDAR process...
###
###   *** NOTE ~ calibrations between MRIP:state survey units have (currently) only been applied in the
###   ***     Gulf red snapper research track assessment, but similar calibrations may be needed/requested
###   ***     in future assessments. Therefore, a function was created to keep track of relevant MRIP:state calibrations,
###   ***     and to apply them when applicable...

flag.cal.MRIPstate = FALSE


### -------------------------------------------------------------------------------------------









####################################################################################################################
####################################################################################################################
####################################################################################################################
#############################                                                          #############################
#############################                   GENERATE EFFORT TABLE                  #############################
#############################                                                          #############################
####################################################################################################################
####################################################################################################################
####################################################################################################################



source( paste0(dir,'/Functions/pull_GenRec_effort.R') )


con = dbConnect(dbDriver("Oracle"), username = keyring::key_list("SECPR")[1,2],
                password = keyring::key_get("SECPR", keyring::key_list("SECPR")[1,2]), dbname = "SECPR")


mrip.effort <- pull.GenRec.effort(  survey = 'MRIP',
                                    yr.filter = first.year:term.year,  mode.filter = mode_sub,
                                    reg.filter = region,   sta.filter = states,  fl.filter = FL_sub, nc.filter = NC_sub  )


if( 'TX' %in% states ) {
  
  tpwd.effort <- pull.GenRec.effort(  survey = 'TPWD',
                                      yr.filter = first.year:term.year,  mode.filter = mode_sub,
                                      reg.filter = region,   sta.filter = states,  fl.filter = FL_sub, nc.filter = NC_sub  )
}

if( 'LA' %in% states ) {
  
  lacr.effort <- pull.GenRec.effort(  survey = 'LACR',
                                      yr.filter = first.year:term.year,  mode.filter = mode_sub,
                                      reg.filter = region,   sta.filter = states,  fl.filter = FL_sub, nc.filter = NC_sub  )
}



###   Note that, generally speaking, the above effort tables are not 'final' as a number of SEDAR-specific modifications
###   are often needed (e.g., imputations of MRIP-wave1, partitioning combined for-hire amongst 'CBT' vs. 'HBT', etc ).
###   The method by which these modifications are made may be based on previous SEDARs (refer to past SAS scripts) and/or
###   SEDAR best practices ( SEDAR 2015 -- Procedural Workshop #7 -- http://sedarweb.org/pw-07 ), but all of these
###   modifications ( and its associated application to our effort tables ) are discussed below...






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


###########################################
######     Manual Adjustments to     ######
######        estimates from         ######
######       a specific strata       ######
###########################################


### 1981 (wave2) Alabama For-Hire ###
### ---------------------------------
###
###     ...the issue of which was identified when investigating a suspicious deviation in the trends of
###         FHS vs. CHTS charter estimates for the S74 red snapper research track ( Figure 1 of S74-DW-01 ).
###         In particular, MRIP did not observe any for-hire fishing effort in 1981 (wave2) FLW, whereas FLW
###         makes-up the majority of Gulf for-hire effort in wave2 of subsequent years. As a result,
###         the SEFSC FHS calibration model was scaling-up the AL estimate to make-up for this 'missing'
###         FLW effort -- the details of this issue and the suggested corrections are documented in a string of
###         emails with Kyle ( 'S74 -- 1981 CBT Estimate' ). The S74 Rec working group ultimately decided
###         to apply the suggested corrections to this strata, which are applied below...
###
###     Note that this adjustment may be needed in future SEDARs (of different species) as this issue was discovered
###         in the calibrated FHS effort estimates. If this issue reappears in future SEDARs, modify the steps
###         outlined in my 'S74 -- 1981 CBT Estimate' email string to be appropriate for other species/SEDARs...
###
###     In applying these corrections, note that an adjustment is needed to both CBT and HBT fishing...



### CBT ###
### -------

dummy = mrip.effort
rows = ( dummy$INT_YEAR == 1981 & dummy$WAVE == 2 &
           dummy$NEW_STA == "AL" & dummy$NEW_MODEN == "Cbt" & dummy$NEW_AREAN == "Ocean>3mi" )

dummy$ESTRIPS[ rows ] = 1896.78

mrip.effort = dummy
rm( dummy, rows )




### HBT ###
### -------

dummy = mrip.effort
rows = ( dummy$INT_YEAR == 1981 & dummy$WAVE == 2 &
           dummy$NEW_STA == "AL" & dummy$NEW_MODEN == "Hbt" & dummy$NEW_AREAN == "Ocean>3mi" )

dummy$ESTRIPS[ rows ] = 1186.32

mrip.effort = dummy
rm( dummy, rows )



# eff.summary = mrip.effort %>%
#   group_by( INT_YEAR, NEW_MODEN ) %>%
#   summarise( NTRIPS = sum( ESTRIPS, na.rm=TRUE) ) %>%
#   pivot_wider( names_from = NEW_MODEN, values_from = NTRIPS )


### ---------------------------------------------------------------------------------------------------------------- ###
### ---------------------------------------------------------------------------------------------------------------- ###


#######################################################
######     Partitioning combined CbtHbt Effort   ######
######     from MATL/NATL into separate Modes    ######
#######################################################


source( paste0(dir,'/Functions/SECmodify_allocate_forhire.R') )


# summary( as.factor(mrip.effort$NEW_MODEN) )

if( flag.forhire ) {
  
  mrip.effort = partition.forhire.effort( effort.table = mrip.effort )
}

# summary( as.factor(mrip.effort$NEW_MODEN) )


### ---------------------------------------------------------------------------------------------------------------- ###
### ---------------------------------------------------------------------------------------------------------------- ###


##########################################################
######         Imputations to Fill Data Gaps        ######
######            in Regional Rec Surveys           ######
######             -- MRIP 1981-wave1 --            ######
##########################################################


if( any( c('TX','LA','MS','AL','FLW','FLE') %in% states ) ) {
  
  source( paste0(dir,'/Functions/SECmodify_impute_MRIP1981w1.R') )
  
  if( method.MRIP.1981w1 != 'None' ) {
    
    mrip.effort = impute.MRIP.1981w1.effort( genrec.table = mrip.effort, method = method.MRIP.1981w1 )
    
  }
}


### ---------------------------------------------------------------------------------------------------------------- ###
### ---------------------------------------------------------------------------------------------------------------- ###


##########################################################
######         Imputations to Fill Data Gaps        ######
######            in Regional Rec Surveys           ######
######            -- TPWD 1981-May1983 --           ######
##########################################################


if( 'TX' %in% states ) {
  
  source( paste0(dir,'/Functions/SECmodify_impute_TPWD1981_83w2.R') )
  
  if( method.TPWD.1981.83 != 'None' ) {
    
    tpwd.effort = impute.TPWD.1981.1983.effort( genrec.table = tpwd.effort, method = method.TPWD.1981.83 )
    
  }
}


### ---------------------------------------------------------------------------------------------------------------- ###
### ---------------------------------------------------------------------------------------------------------------- ###


####################################
######     Assign StockID     ######
######       Boundaries       ######
####################################

source( paste0(dir,'/Functions/assign_stockID.R') )

mrip.effort <- assign.stockID( new.com = new.com, region = region, genrec.table = mrip.effort )

if( 'TX' %in% states ) {
  tpwd.effort <- assign.stockID( new.com = new.com, region = region, genrec.table = tpwd.effort )
}

if( 'LA' %in% states ) {
  lacr.effort <- assign.stockID( new.com = new.com, region = region, genrec.table = lacr.effort )
}


### ---------------------------------------------------------------------------------------------------------------- ###
### ---------------------------------------------------------------------------------------------------------------- ###


########################################
######     Assign Open/Closed     ######
######       Federal Seasons      ######
########################################

source( paste0(dir,'/Functions/assign_FishingSeason.R') )

if( flag.open.closed ) {
  
  # mrip.effort <- assign.fishing.season( new.com = new.com, region = region, genrec.table = mrip.effort )
  # 
  # if( 'TX' %in% states ) {
  #   tpwd.effort <- assign.fishing.season( new.com = new.com, region = region, genrec.table = tpwd.effort )
  # }
  # 
  # if( 'LA' %in% states ) {
  #   lacr.effort <- assign.fishing.season( new.com = new.com, region = region, genrec.table = lacr.effort )
  # }
}


### ---------------------------------------------------------------------------------------------------------------- ###
### ---------------------------------------------------------------------------------------------------------------- ###


###########################################
######     Calibrating State and     ######
######     MRIP FES/FHS estimates    ######
###########################################

source( paste0(dir,'/Functions/SECmodify_cal_MRIPstate.R') )


# if( grepl( "Gulf of America",region ) ) {
#   
#   if( flag.lacr.cal ) {
#     lacr.effort = calibrate.MRIPstate.effort( new.com = new.com, region = region,  DS = 'LACR', genrec.table = lacr.effort )
#   }
#   
#   if( flag.tpwd.cal ) {
#     tpwd.effort = calibrate.MRIPstate.effort( new.com = new.com, region = region,  DS = 'TPWD', genrec.table = tpwd.effort )
#   }
# 
# }



### LACreel:MRIP Calibration ###
### ----------------------------
###
###       ...in which LDWF provided an updated calibration on May 23, 2022 (email from Jason Adriance).
###           Note Jason modified the calibration originally submitted (in his May 11 email) to be
###           specific to the private mode (vs. being calculated the combined priv/shore mode)...
###
###   These calibrations are irrelevant for this assessment...

if( "LA" %in% states ) {

  dummy = lacr.effort
  rows = ( dummy$MODES == "Private" )

  ### Conversion of Priv/Shore into Private Estimates ###
  dummy = dummy %>% mutate( PRIVcal = ifelse( rows, 0.791152081, NA ) )
  dummy$EXPANDED_EFFORT[ rows ] = dummy$EXPANDED_EFFORT[ rows ] * dummy$PRIVcal[rows]
  dummy$EXPANDED_EFFORT_VAR[ rows ] = dummy$EXPANDED_EFFORT_VAR[ rows ] * ( dummy$PRIVcal[rows]^2 )
  ###     ...multiplying a random variable by a constant increases the variance by the square of the constant...

  ### LACR:FES Calibration ###
  dummy = dummy %>% mutate( FEScal = ifelse( rows, 2.700487117, NA ) )
  dummy$EXPANDED_EFFORT[ rows ] = dummy$EXPANDED_EFFORT[ rows ] * dummy$FEScal[rows]
  dummy$EXPANDED_EFFORT_VAR[ rows ] = dummy$EXPANDED_EFFORT_VAR[ rows ] * ( dummy$FEScal[rows]^2 )

  lacr.effort = dummy
  rm( dummy, rows )


  # eff.summary = lacr.effort %>%
  #   group_by( INT_YEAR, MODES ) %>%
  #   summarise( NTRIPS = sum( EXPANDED_EFFORT, na.rm=TRUE) ) %>%
  #   pivot_wider( names_from = MODES, values_from = NTRIPS )

}

##* *Effort Calibration for LA Creel by Wave*

##* *Calibration Ratios*
la.cal <- 3.55


##* *Assign calibrations*
lacr.effort <- lacr.effort %>%
  mutate(Cal_Factor = case_when(
    MODE_FX_F == 'Charter Boat' ~ 1,
    NEW_MODEN == 'Priv/Shore' ~  la.cal)) 



lacr.effort$EXPANDED_EFFORT_orig <- lacr.effort$EXPANDED_EFFORT


##* *Apply calibrations*
lacr.effort <- lacr.effort %>%
  mutate(EXPANDED_EFFORT = EXPANDED_EFFORT_orig * Cal_Factor)



### TPWD:MRIP Calibration ###
### -------------------------
###
###      ...which is only applicable to PRIV estimates ( ratio estimated b/w TPWD vs FES effort )
###       that are scaled using the ratio of 2016 FES:TPWD effort. This approach is described in S74-DW-10...
###
###   These calibrations are irrelevant for this assessment...

if( "TX" %in% states ) {

  dummy = tpwd.effort
  rows = ( dummy$NEW_MODEN == "Priv" )

  ### TPWD:FES Calibration ###
  dummy = dummy %>% mutate( FEScal = ifelse( rows, 10.8989, NA ) )
  dummy$ESTHRS[ rows ] = dummy$ESTHRS[ rows ] * dummy$FEScal[ rows ]
  dummy$ESTHRS_SE[ rows ] = sqrt( ( dummy$ESTHRS_SE[ rows ]^2 ) * ( dummy$FEScal[ rows ]^2 ) )
  ###     ...multiplying a random variable by a constant increases the variance by the square of the constant...

  dummy$NTRP[ rows ] = dummy$ESTHRS[ rows ] / dummy$TRIPLEN[ rows ]
  dummy$NPAR[ rows ] = dummy$NTRP[ rows ] / dummy$PARSIZE[ rows ]

  tpwd.effort = dummy
  rm( dummy, rows )


  # eff.summary = tpwd.effort %>%
  #   group_by( CYEAR, ACTIVITY ) %>%
  #   summarise( NTRIPS = sum( NTRP, na.rm=TRUE) ) %>%
  #   pivot_wider( names_from = ACTIVITY, values_from = NTRIPS )

}


### ---------------------------------------------------------------------------------------------------------------- ###
### ---------------------------------------------------------------------------------------------------------------- ###



# mrip.summary <- mrip.effort %>%
#   group_by( YEAR, NEW_ST, NEW_STA ) %>%
#   summarize( EFF = sum( as.numeric(ESTRIPS), na.rm=TRUE ) ) %>%
#   select( YEAR, NEW_ST, NEW_STA, EFF ) %>%
#   pivot_wider( names_from=c( "NEW_ST","NEW_STA" ), values_from=EFF )
# View( mrip.summary )

# tpwd.summary = tpwd.effort %>%
#   group_by( YEAR, NEW_MODE, NEW_MODEN ) %>%
#   summarise( EFF = sum( NTRP, na.rm=TRUE ) ) %>%
#   select( YEAR, NEW_MODE, NEW_MODEN, EFF ) %>%
#   pivot_wider( names_from=c("NEW_MODE","NEW_MODEN"), values_from=EFF )

# lacr.summary = lacr.effort %>%
#   group_by( YEAR, NEW_MODE, NEW_MODEN ) %>%
#   summarise( EFF = sum( EXPANDED_EFFORT, na.rm=TRUE ) ) %>%
#   select( YEAR, NEW_MODE, NEW_MODEN, EFF ) %>%
#   pivot_wider( names_from=c("NEW_MODE","NEW_MODEN"), values_from=EFF )








####################################################################################################################
####################################################################################################################
####################################################################################################################
#############################                                                          #############################
#############################                   FINAL EXCEL WORKBOOK                   #############################
#############################                                                          #############################
####################################################################################################################
####################################################################################################################
####################################################################################################################



table.ID <- paste0( "SBS_rec_effGEN_",
                    substr( first.year, nchar(first.year)-1, nchar(first.year) ),
                    substr( term.year, nchar(term.year)-1, nchar(term.year) ),
                    "_", gsub("-","", Sys.Date() ) )


### Import template excel file with settings already saved for my pivots...
dir <- getwd()
wb <- loadWorkbook( file=paste0( dir,"/Effort/Template_SEDAR_Effort_fromOracle_v2.xlsx" ) )

removeWorksheet( wb, sheet="MRIP" )
addWorksheet( wb, sheet="MRIP" )
writeData( wb, sheet="MRIP", x=mrip.effort, colNames=TRUE )

removeWorksheet( wb, sheet="TPWD" )
removeWorksheet( wb, sheet="LA_Creel" )
if( any( grepl( "TX|LA", states ) ) ) {

  addWorksheet( wb, sheet="TPWD" )
  writeData( wb, sheet="TPWD", x=tpwd.effort, colNames=TRUE )
  addWorksheet( wb, sheet="LA_Creel" )
  writeData( wb, sheet="LA_Creel", x=lacr.effort, colNames=TRUE )
  
} else {
  removeWorksheet( wb, sheet="TPWD_pivot" )
  removeWorksheet( wb, sheet="LACreel_pivot" )
}

saveWorkbook( wb, file=paste0( dir,"/Effort/",table.ID,".xlsx" ), overwrite=TRUE )






#############################################
######     WRITE EFFORT TO R-DRIVE     ######
#############################################

### As a last step, I then write the above data pulls to the R-drive. This will allow us to compare these tables
###       to future data pulls (e.g., across different SEDARs) or simply recreate these tables using different
###       filtering criteria (e.g., based on decisions made during the data workshop). Note that the raw
###       (unfiltered) data tables are those that are being written here, not the assessment-specific ones...

write.csv( mrip.raw, file=paste0( "R:/RecrSurveys/MRFSS/MRIP/Fully calibrated estimates/RDI/",
                                  "mrip_effort_", gsub( "-","_", as.Date( Sys.Date(), format = "%m/%d/%y" ) ),".csv" ), na="" )

if( any( grepl( "TX|LA", states ) ) ) {

  write.csv( tpwd.raw, file=paste0( "R:/RecrSurveys/Texas/estimates/Wave_estimates/RDI/",
                                    "tpwd_effort_", gsub( "-","_", as.Date( Sys.Date(), format = "%m/%d/%y" ) ),".csv" ), na="" )
  
  write.csv( lacr.raw, file=paste0( "R:/RecrSurveys/LA_Creel/Effort/RDI/",
                                    "lacr_effort_", gsub( "-","_", as.Date( Sys.Date(), format = "%m/%d/%y" ) ),".csv" ), na="" )

}






