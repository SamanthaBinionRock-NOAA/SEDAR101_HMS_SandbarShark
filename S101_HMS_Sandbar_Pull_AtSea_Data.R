
librarian::shelf(openxlsx, readxl, tidyverse, dplyr, haven, janitor, ROracle)




##______________________________________________________________________________
##* *Define directory*
dir <- getwd()



##* *Prevents R from converting any long numbers (ID codes) to scientific notation*
options(scipen=999)





##* *Pull at-sea data from RDI*
con = dbConnect(dbDriver("Oracle"), username = keyring::key_list("SECPR")[1,2],
                password = keyring::key_get("SECPR", keyring::key_list("SECPR")[1,2]), dbname = "SECPR")


i1s = dbGetQuery(con, "SELECT * 
                     FROM RDI.mrfss_st_rec_i1@secapxdv_dblk.sfsc.noaa.gov")



i2s = dbGetQuery(con, "SELECT * 
                     FROM RDI.mrfss_st_rec_i2@secapxdv_dblk.sfsc.noaa.gov")



i3s = dbGetQuery(con, "SELECT * 
                     FROM RDI.mrfss_st_rec_i3@secapxdv_dblk.sfsc.noaa.gov")




i9s = dbGetQuery(con, "SELECT * 
                     FROM RDI.mrfss_st_rec_i9@secapxdv_dblk.sfsc.noaa.gov")



##______________________________________________________________________________
##* *i9 files*


##* *Create TRIP ID by retaining the first 13 digits of the sample ID*
i9s$Sampling_Unit_ID <- i9s$ID_CODE

i9s$Sampling_Unit_ID <- substring(i9s$Sampling_Unit_ID, 1,13)



##* *Assign data type*
i9s$Record_Type <- 9



paste0("Table: Number of samples per subreg")
i9s%>%
  tabyl(INT_YEAR, SUB_REG)


paste0("Table: Number of samples per state")
i9s%>%
  tabyl(INT_YEAR, ST)


paste0("Table: Number of samples per species state")
i9s%>%
  tabyl(SP_CODE, ST)



paste0("Table: Number of samples by disposition and year")
i9s%>%
  tabyl(INT_YEAR, DISP9)





##__________________________
##* *Type 3 - Observed headboat landings records*



##* *Create TRIP ID by retaining the first 13 digits of the sample ID*
i3s$Sampling_Unit_ID <- i3s$ID_CODE

i3s$Sampling_Unit_ID <- substring(i3s$Sampling_Unit_ID, 1,13)



##* *Assign data type*
i3s$Record_Type <- 3



##__________________________
##* *Type 2 Records - Reported non-observed headboat discard records*


##* *Create TRIP ID by retaining the first 13 digits of the sample ID*
i2s$Sampling_Unit_ID <- i2s$ID_CODE

i2s$Sampling_Unit_ID <- substring(i2s$Sampling_Unit_ID, 1,13)


##* *Assign data type*
i2s$Record_Type <- 2


##___________________________________________
##* *Type 1 Records - Observed headboat discard records*


##* *No Records*


##* *Create TRIP ID by retaining the first 13 digits of the sample ID*
#i1s$Sampling_Unit_ID <- i1s$ID_CODE

#i1s$Sampling_Unit_ID <- substring(i1s$Sampling_Unit_ID, 1,13)


##* *Assign data type*
#i1s$Record_Type <- 1







##______________________________________________________________________________
##* *Combine the ifiles into a single file*
##* *i2s and i3s*
##* *Identify fields common to all the record types so I know what to merge by*

common.fields <- intersect(names(i2s), names(i3s))

common.fields

##* *NUM_TYP3 is all NAs for i2s and has values for i3s - remove from common.fields*
#common.fields <- common.fields[!common.fields %in% c('NUM_TYP3')]





##* *Merge i2s and i3s*
i2.i3 <- merge(i2s, i3s, by=common.fields, all=TRUE)




##* *Repeat process to add i9s*
##* *Identify duplicate fields*
common.fieldsb <- intersect(names(i2.i3), names(i9s))

common.fieldsb


##* *Merge*
atsea.dat <- merge(i2.i3, i9s, by=common.fieldsb, all=TRUE)



##__________________________________________________________________________
##* *Merge with common names*


con = dbConnect(dbDriver("Oracle"), username = keyring::key_list("SECPR")[1,2],
                password = keyring::key_get("SECPR", keyring::key_list("SECPR")[1,2]), dbname = "SECPR")
spp.info = dbGetQuery(con, "SELECT * 
                     FROM RDI.v_species_xref@secapxdv_dblk.sfsc.noaa.gov")


##* *Subset to only keep the fields I want to add to the atsea.dat*
spp.sub <- subset(spp.info, select = c('NODC_CODE', 'NEW_COM', 'NEW_SCI', 'SPECIES_ITIS'))




##* *Rename NODC_Code and merge the at-sea data and species info*
#colnames(spp.sub)[colnames(spp.sub)=="NODC_CODE"] <- "SP_CODE"


##* *Drop records where the NODC code is blank*
##* *It's just one line for dark-banded mantis shrimp*
spp.sub <- spp.sub %>%
  filter(!is.na(NODC_CODE))


##* *Create NA record for spp.sub that is not associated with a species*
##* *I don't want to drop rows in the atsea.dat when SP_CODE is blank after the merge*

spp.sub <- spp.sub %>%
  add_row(NEW_COM = NA, NEW_SCI = NA, NODC_CODE = NA, SPECIES_ITIS = NA)



##* *Rename NODC_Code and merge the at-sea data and species info*
colnames(spp.sub)[colnames(spp.sub)=="NODC_CODE"] <- "SP_CODE"


##* *Merge the data*
##* *Create flag for atsea data*

atsea.dat$atsea <- 1   ##atsea.dat has 8221544 obs

atsea.dat <- merge(spp.sub, atsea.dat, by='SP_CODE', all=TRUE)



##* *Remove rows added from merging with spp.sub*
atsea.dat <- atsea.dat %>%
  filter(atsea == 1)




##* *Create a flag to indicate which species were able to be identified based on NODC code*
atsea.dat <- atsea.dat %>%
  mutate(species.flag = case_when(
    !is.na(NEW_COM) ~ 1,
    .default = NA))


##* *Remove species that were not identified*
species.na <- atsea.dat %>%
  filter(is.na(species.flag))





##* *Change SP_CODE back to NODC_CODE and make ITIS_CODE SP_CODE instead*
colnames(spp.sub)[colnames(spp.sub)=="SP_CODE"]      <- "NODC_CODE"
colnames(spp.sub)[colnames(spp.sub)=="SPECIES_ITIS"] <- "SP_CODE"



##* *Drop NEW_COM, NEW_SCI, and SPECIES_ITIS from species.na*
species.na <- subset(species.na, select = -c(NEW_COM, NEW_SCI, SPECIES_ITIS))



##* *There are duplicate ITIS codes in spp.sub, need to figure out which ones are duplicated*
length(unique(spp.sub$SPECIES_ITIS)) == nrow(spp.sub)



dups <- spp.info %>%
  group_by(SPECIES_ITIS) %>%
  filter(n() >1) %>%
  ungroup()


##* *See which of the duplicated ITIS codes are in species.na*
##* *duplicate ITIS codes in species.na are 161030, 166991, and there is an*
##* *NA for a weird entry were NEW_COM = 8719103000 ?*
##* *Will create single entries for these ITIS codes*
species.na <- species.na %>%
  mutate(Dup_Flag = case_when(
    SP_CODE %in% dups$SPECIES_ITIS ~ 1,
    .default = 0))


species.dup <- species.na %>%
  filter(Dup_Flag == 1)


##* *Remove the duplicated ITIS codes from spp.sub*
spp.sub <- spp.sub %>%
  filter(SP_CODE != 161030)


spp.sub <- spp.sub %>%
  filter(SP_CODE != 166991)


spp.sub <- spp.sub %>%
  filter(NEW_COM != '8719103000 ?')


spp.sub.na <- spp.sub %>%
  filter(is.na(SP_CODE))



##* *Add single lines for NA, 161030, and 1666991*
spp.sub <- spp.sub %>%
  add_row(NEW_COM = NA, NEW_SCI = NA, NODC_CODE = NA, SP_CODE = NA)


spp.sub <- spp.sub %>%
  add_row(NEW_COM = 'Unidentified Fish', NEW_SCI = 'Unidentified Fish', 
          NODC_CODE = as.character(1000000000), SP_CODE = as.character(161030))


spp.sub <- spp.sub %>%
  add_row(NEW_COM = 'bigeye blackwing searobin', NEW_SCI = 'Prionotus rubio longispinosus', 
          NODC_CODE = as.character(8826020120), SP_CODE = as.character(166991))



##* *From evaluating the data after the merge, there are missing ITIS code in spp.sub*
##* *Create those values and merge the data*

spp.sub <- spp.sub %>%
  add_row(NEW_COM = 'winter skate', NEW_SCI = 'Raja ocellata', 
          NODC_CODE = NA, SP_CODE = as.character(160858))


spp.sub <- spp.sub %>%
  add_row(NEW_COM = 'Anguilliformes eels', NEW_SCI = 'Anguilliformes', 
          NODC_CODE = NA, SP_CODE = as.character(161123))


spp.sub <- spp.sub %>%
  add_row(NEW_COM = 'palespotted eel', NEW_SCI = 'Raja ocellata', 
          NODC_CODE = NA, SP_CODE = as.character(161481))


spp.sub <- spp.sub %>%
  add_row(NEW_COM = 'Atlantic flyigfish', NEW_SCI = 'Cheilopogon melanurus', 
          NODC_CODE = NA, SP_CODE = as.character(165447))


spp.sub <- spp.sub %>%
  add_row(NEW_COM = 'coney', NEW_SCI = 'Cephalopholis fulva', 
          NODC_CODE = NA, SP_CODE = as.character(167739))


spp.sub <- spp.sub %>%
  add_row(NEW_COM = 'darkfin hind', NEW_SCI = 'Cephalophis urodeta', 
          NODC_CODE = NA, SP_CODE = as.character(167754))


spp.sub <- spp.sub %>%
  add_row(NEW_COM = 'Australian grunters', NEW_SCI = 'Mesopristes', 
          NODC_CODE = NA, SP_CODE = as.character(168059))


spp.sub <- spp.sub %>%
  add_row(NEW_COM = 'glasseye snapper', NEW_SCI = 'Priacanthus cruentatus', 
          NODC_CODE = NA, SP_CODE = as.character(168179))


spp.sub <- spp.sub %>%
  add_row(NEW_COM = 'goldline darter', NEW_SCI = 'Percina aurolineata', 
          NODC_CODE = NA, SP_CODE = as.character(168478))



spp.sub <- spp.sub %>%
  add_row(NEW_COM = 'bar jack', NEW_SCI = 'Caranx ruber', 
          NODC_CODE = NA, SP_CODE = as.character(168614))


spp.sub <- spp.sub %>%
  add_row(NEW_COM = 'Australian salmon', NEW_SCI = 'Arripis', 
          NODC_CODE = NA, SP_CODE = as.character(168825))


spp.sub <- spp.sub %>%
  add_row(NEW_COM = 'Bermuda chub', NEW_SCI = 'Kyphosus sectatrix', 
          NODC_CODE = NA, SP_CODE = as.character(169506))


spp.sub <- spp.sub %>%
  add_row(NEW_COM = 'great barracuda', NEW_SCI = 'Sphyraena barracuda', 
          NODC_CODE = NA, SP_CODE = as.character(170429))


spp.sub <- spp.sub %>%
  add_row(NEW_COM = 'skipjack tuna', NEW_SCI = 'Euthynnus pelamis', 
          NODC_CODE = NA, SP_CODE = as.character(172400))


spp.sub <- spp.sub %>%
  add_row(NEW_COM = 'furrowed sash flounder', NEW_SCI = 'Trichopsetta orbisulcus', 
          NODC_CODE = NA, SP_CODE = as.character(172797))


spp.sub <- spp.sub %>%
  add_row(NEW_COM = 'filefishes', NEW_SCI = 'Monacanthus', 
          NODC_CODE = NA, SP_CODE = as.character(173178))


spp.sub <- spp.sub %>%
  add_row(NEW_COM = 'pufferfishes', NEW_SCI = 'Tetraodontidae', 
          NODC_CODE = NA, SP_CODE = as.character(173283))





##* *Merge the data*
species.na <- merge(spp.sub, species.na, by='SP_CODE', all=TRUE)



##* *Remove rows added from merging with spp.sub*
species.na <- species.na %>%
  filter(atsea == 1)



##* *Remove the species with NAs from atsea.dat and combine with species.na*
##* *Need to make sure the fields are the same*

##* *Remove NAs from atsea.dat and SPECIES_ITIS*
atsea.dat <- atsea.dat %>%
  filter(!is.na(NEW_COM))

atsea.dat <- subset(atsea.dat, select = -SPECIES_ITIS)





##* *Remove species.flag and NODC_CODE from species.na*
species.na <- subset(species.na, select = - species.flag)

species.na <- subset(species.na, select = - NODC_CODE)


species.na <- subset(species.na, select = - Dup_Flag)


##* *Remove species.flag from atsea.dat*
atsea.dat <- subset(atsea.dat, select = - species.flag)


##* *Merge the data*
atsea.dat <- rbind(atsea.dat, species.na)




##* *look into species codes that did not get a species name assigned*
species.na <- atsea.dat %>%
  filter(is.na(NEW_COM))


codes.na <- unique(species.na$SP_CODE)  ##* *Still 166 codes are not assigned*
codes.na






##______________________________________________________________

##* *Filter for Sandbar Shark*
sbs <- atsea.dat %>%
  filter(NEW_COM == 'sandbar shark') 




##* *Assign FIPS code to State Abbreviation*
sbs <- sbs %>%
  mutate(State_Landed = case_when(
    ST == 1  ~ 'AL',
    ST == 9  ~ 'CT',
    ST == 10 ~ 'DE',
    ST == 12 ~ 'FL',
    ST == 13 ~ 'GA',
    ST == 15 ~ 'HI',
    ST == 22 ~ 'LA',
    ST == 24 ~ 'MD',
    ST == 25 ~ 'MA',
    ST == 28 ~ 'MS',
    ST == 33 ~ 'NH',
    ST == 34 ~ 'NJ',
    ST == 36 ~ 'NY',
    ST == 37 ~ 'NC',
    ST == 44 ~ 'RI',
    ST == 45 ~ 'SC',
    ST == 48 ~ '',
    ST == 51 ~ 'VA',
    .default = NA))











##__________________________________________________________________________

##* *Evaluate number of lengths are associated with each record type*
sbs2 <- sbs %>%
  filter(!is.na(LNGTH)) %>%
  group_by(Record_Type) %>%
  dplyr::summarise(nfish = n())





##* *Exclude samples with no lengths recorded*
sbs <- sbs %>%
  filter(!is.na(LNGTH))



##* *Assign disposition codes*
##* *DISP9 value of 6 is thrown back dead/plan to throw away*
sbs <- sbs %>%
  mutate(Disposition = case_when(
    Record_Type == 3 ~ 'Harvest',
    Record_Type == 9 & DISP9 %in% c(1,2) ~ 'Discard',
    Record_Type == 9 & DISP9 == 6 ~ 'Harvest',
    .default = NA))





paste0("Table: Number of discarded and harvested fish by state")
sbs %>%
  tabyl(State_Landed, Disposition)



##* *Assign month and day*
sbs$Month <- substring(sbs$ID_CODE, 10,11)
sbs$Day   <- substring(sbs$ID_CODE, 12,13)



##* *Double check mode*
unique(sbs$MODE_F)


##* *Filter for headboat*
sbs.hb <- sbs %>%
  filter(MODE_F == 6)




paste0("Table: Number of discarded and harvested fish by state")
sbs.hb %>%
  tabyl(State_Landed, Disposition)



paste0("Table: Number of discarded and harvested fish by year")
sbs.hb %>%
  tabyl(INT_YEAR, Disposition)



##* *Filter fish measured after 2004*
sbs.hb <- sbs.hb %>%
  filter(INT_YEAR >= 2004)




##* *Subset to only keep the fields that I want*

sbs.hb <- subset(sbs.hb, select = c('CNTY',
                                        'SP_CODE',
                                        'NEW_COM',
                                        'NEW_SCI',
                                        'ID_CODE',
                                        'HRSF',
                                        'MODE_F',
                                        'ST', 
                                        'SUB_REG',
                                        'INT_TIME',
                                        'WAVE',
                                        'INT_YEAR',
                                        'PARTY',
                                        'LNGTH',
                                        'Sampling_Unit_ID',
                                        'Record_Type',
                                        'State_Landed',
                                        'Disposition',
                                        'Month',
                                        'Day'))



first.year <- 2004
term.year  <- 2024


##* *Export the data*

table.ID <- 'DiscardLenComps/SBS_rec_dsizAPAIS_0424_20260609.xlsx'


write.xlsx(sbs.hb, 
           file = table.ID,
           sheetName = "sbs", 
           colNames = TRUE, 
           rowNames = FALSE, 
           append = FALSE,
           overwrite=TRUE) 






