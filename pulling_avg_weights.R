

##* * Getting the Average Weight Files for Sandbar Sharks*


librarian::shelf(tidyverse, reshape2, openxlsx, haven, ROracle)


con = dbConnect(dbDriver("Oracle"), username = keyring::key_list("SECPR")[1,2],
                password = keyring::key_get("SECPR", keyring::key_list("SECPR")[1,2]), dbname = "SECPR")
acl.view = dbGetQuery(con, "SELECT * 
                     FROM RDI.v_rec_ACL_MRIP_FES@secapxdv_dblk.sfsc.noaa.gov")


sandbar <- acl.view %>%
  filter(NEW_COM == 'sandbar shark')



##* *See what the average weight files look like*
s.dat       <- read_sas(data_file = paste0(dir,"/Size/avgwgt_s.sas7bdat"))
sr.dat      <- read_sas(data_file = paste0(dir,"/Size/avgwgt_sr.sas7bdat"))
sry.dat     <- read_sas(data_file = paste0(dir,"/Size/avgwgt_sry.sas7bdat"))
srys.dat    <- read_sas(data_file = paste0(dir,"/Size/avgwgt_srys.sas7bdat"))
srysm.dat   <- read_sas(data_file = paste0(dir,"/Size/avgwgt_srysm.sas7bdat"))
srysmw.dat  <- read_sas(data_file = paste0(dir,"/Size/avgwgt_srysmw.sas7bdat"))
srysmwa.dat <- read_sas(data_file = paste0(dir,"/Size/avgwgt_srysmwa.sas7bdat"))



##* *create average weight files*
##* *s*
sandbar_s <- sandbar %>%
  filter(LBSEST_SECSOURCE == 's')


##* *keep only the fields normally found in the average weight file*
sandbar_s <- subset(sandbar_s, select = c('NEW_COM',
                                         'SAMPLE_SIZE_USED',
                                         'S_AVG_WGT'))



##* *Rename fields to match regular average weight files*
colnames(sandbar_s)[colnames(sandbar_s)=="SAMPLE_SIZE_USED"] <- "FREQ"
colnames(sandbar_s)[colnames(sandbar_s)=="S_AVG_WGT"]        <- "avgwgt_s"




##* *Remove duplicates*
sandbar_s <- sandbar_s %>%
  arrange(NEW_COM, FREQ, avgwgt_s) %>%
  distinct(NEW_COM, FREQ, avgwgt_s)


save.image( file = paste0(dir,"/Catch/sandbar_s.RData"))





##* *sr*
sandbar_sr <- sandbar %>%
  filter(LBSEST_SECSOURCE == 'sr')


##* *keep only the fields normally found in the average weight file*
sandbar_sr <- subset(sandbar_sr, select = c('NEW_COM',
                                            'SUB_REG',
                                            'SAMPLE_SIZE_USED',
                                            'SR_AVG_WGT'))



##* *Rename fields to match regular average weight files*
colnames(sandbar_sr)[colnames(sandbar_sr)=="SAMPLE_SIZE_USED"] <- "FREQ"
colnames(sandbar_sr)[colnames(sandbar_sr)=="SR_AVG_WGT"]       <- "avgwgt_sr"



##* *Remove duplicates*
sandbar_sr <- sandbar_sr %>%
  arrange(NEW_COM, SUB_REG, FREQ, avgwgt_sr) %>%
  distinct(NEW_COM, SUB_REG, FREQ, avgwgt_sr)



save.image( file = paste0(dir,"/Catch/sandbar_sr.RData"))





##* *sry*
sandbar_sry <- sandbar %>%
  filter(LBSEST_SECSOURCE == 'sry')


sandbar_sry <- subset(sandbar_sry, select = c('NEW_COM',
                                              'SUB_REG',
                                              'YEAR',
                                              'SAMPLE_SIZE_USED',
                                              'SRY_AVG_WGT'))



##* *Rename fields to match regular average weight files*
colnames(sandbar_sry)[colnames(sandbar_sry)=="SAMPLE_SIZE_USED"] <- "FREQ"
colnames(sandbar_sry)[colnames(sandbar_sry)=="SRY_AVG_WGT"]      <- "avgwgt_sry"
colnames(sandbar_sry)[colnames(sandbar_sry)=="YEAR"]             <- "year"




##* *Remove duplicates*
sandbar_sry <- sandbar_sry %>%
  arrange(NEW_COM, SUB_REG, year, FREQ, avgwgt_sry) %>%
  distinct(NEW_COM, SUB_REG, year, FREQ, avgwgt_sry)


save.image( file = paste0(dir,"/Catch/sandbar_sry.RData"))





##* *srys*
sandbar_srys <- sandbar %>%
  filter(LBSEST_SECSOURCE == 'srys')


sandbar_srys <- subset(sandbar_srys, select = c('NEW_COM',
                                                'SUB_REG',
                                                'YEAR',
                                                'NEW_STA',
                                                'SAMPLE_SIZE_USED',
                                                'SRYS_AVG_WGT'))



##* *Rename fields to match regular average weight files*
colnames(sandbar_srys)[colnames(sandbar_srys)=="SAMPLE_SIZE_USED"]  <- "FREQ"
colnames(sandbar_srys)[colnames(sandbar_srys)=="SRYS_AVG_WGT"]      <- "avgwgt_srys"
colnames(sandbar_srys)[colnames(sandbar_srys)=="YEAR"]              <- "year"




##* *Remove duplicates*
sandbar_srys <- sandbar_srys %>%
  arrange(NEW_COM, SUB_REG, year, NEW_STA, FREQ, avgwgt_srys) %>%
  distinct(NEW_COM, SUB_REG, year, NEW_STA, FREQ, avgwgt_srys)


save.image( file = paste0(dir,"/Catch/sandbar_srys.RData"))






##* *srysm*
sandbar_srysm <- sandbar %>%
  filter(LBSEST_SECSOURCE == 'srysm')


sandbar_srysm <- subset(sandbar_srysm, select = c('NEW_COM',
                                                  'SUB_REG',
                                                  'YEAR',
                                                  'NEW_STA',
                                                  'NEW_MODEN',
                                                  'SAMPLE_SIZE_USED',
                                                  'SRYSM_AVG_WGT'))



##* *Rename fields to match regular average weight files*
colnames(sandbar_srysm)[colnames(sandbar_srysm)=="SAMPLE_SIZE_USED"]  <- "FREQ"
colnames(sandbar_srysm)[colnames(sandbar_srysm)=="SRYSM_AVG_WGT"]     <- "avgwgt_srysm"
colnames(sandbar_srysm)[colnames(sandbar_srysm)=="YEAR"]              <- "year"




##* *Remove duplicates*
sandbar_srysm <- sandbar_srysm %>%
  arrange(NEW_COM, SUB_REG, year, NEW_STA, NEW_MODEN, FREQ, avgwgt_srysm) %>%
  distinct(NEW_COM, SUB_REG, year, NEW_STA, NEW_MODEN, FREQ, avgwgt_srysm)


save.image( file = paste0(dir,"/Catch/sandbar_srysm.RData"))





##* *srysmw*
sandbar_srysmw <- sandbar %>%
  filter(LBSEST_SECSOURCE == 'srysmw')


sandbar_srysmw <- subset(sandbar_srysmw, select = c('NEW_COM',
                                                    'SUB_REG',
                                                    'YEAR',
                                                    'NEW_STA',
                                                    'NEW_MODEN',
                                                    'WAVE',
                                                    'SAMPLE_SIZE_USED',
                                                    'SRYSMW_AVG_WGT'))



##* *Rename fields to match regular average weight files*
colnames(sandbar_srysmw)[colnames(sandbar_srysmw)=="SAMPLE_SIZE_USED"]  <- "FREQ"
colnames(sandbar_srysmw)[colnames(sandbar_srysmw)=="SRYSMW_AVG_WGT"]    <- "avgwgt_srysmw"
colnames(sandbar_srysmw)[colnames(sandbar_srysmw)=="YEAR"]              <- "year"




##* *Remove duplicates*
sandbar_srysmw <- sandbar_srysmw %>%
  arrange(NEW_COM, SUB_REG, year, NEW_STA, NEW_MODEN, WAVE, FREQ, avgwgt_srysmw) %>%
  distinct(NEW_COM, SUB_REG, year, NEW_STA, NEW_MODEN, WAVE, FREQ, avgwgt_srysmw)


save.image( file = paste0(dir,"/Catch/sandbar_srysmw.RData"))





##* *srysmwa*
sandbar_srysmwa <- sandbar %>%
  filter(LBSEST_SECSOURCE == 'srysmwa')


sandbar_srysmwa <- subset(sandbar_srysmwa, select = c('NEW_COM',
                                                    'SUB_REG',
                                                    'YEAR',
                                                    'NEW_STA',
                                                    'NEW_MODEN',
                                                    'WAVE',
                                                    'NEW_AREAN',
                                                    'SAMPLE_SIZE_USED',
                                                    'SRYSMWA_AVG_WGT'))



##* *Rename fields to match regular average weight files*
colnames(sandbar_srysmwa)[colnames(sandbar_srysmwa)=="SAMPLE_SIZE_USED"]  <- "FREQ"
colnames(sandbar_srysmwa)[colnames(sandbar_srysmwa)=="SRYSMWA_AVG_WGT"]   <- "avgwgt_srysmwa"
colnames(sandbar_srysmwa)[colnames(sandbar_srysmwa)=="YEAR"]              <- "year"




##* *Remove duplicates*
sandbar_srysmwa <- sandbar_srysmwa %>%
  arrange(NEW_COM, SUB_REG, year, NEW_STA, NEW_MODEN, WAVE, NEW_AREAN, FREQ, avgwgt_srysmwa) %>%
  distinct(NEW_COM, SUB_REG, year, NEW_STA, NEW_MODEN, WAVE, NEW_AREAN, FREQ, avgwgt_srysmwa)


save.image( file = paste0(dir,"/Catch/sandbar_srysmwa.RData"))



