##* *Code for nominal discard length comps*


##* *Have to produce 2cm, 5cm, and 10 cm comps*


librarian::shelf(openxlsx, readxl, tidyverse, dplyr, haven, doBy, xtable,
                 reshape2, ggplot2, ExPanDaR, knitr, png, grid, gridExtra,
                 ggpubr, stringr, here)




##* *Prevents R from converting any long numbers (ID codes) to scientific notation*
options(scipen=999)


##* *Define directory*
dir <- getwd()


##* *SEFSC 01 HMS Sandbar*
##* *Import the data*
rec.lengths <- readxl::read_xlsx(paste0(dir,'/DiscardLenComps/SBS_rec_dsizAPAIS_0424_20260609.xlsx'),
                                 sheet = 'sbs', guess_max = 500000)



##* *Create Final Length Fields*
rec.lengths$Final_Length_mm   <- rec.lengths$Observed_FL_mm
rec.lengths$Final_Length_Type <- 'FL'


##* *Assign Fleet*
rec.lengths$Fleet <- 'Rec'

#rec.lengths <- rec.lengths %>%
#  mutate(Fleet = case_when(
#    Fishing_Mode %in% c('CB', 'PR', 'SH') ~ 'GR',
#    Fishing_Mode == 'HB' ~ 'HB',
#        .default = NA))



##* *Assign time blocks based on management history*
range(rec.lengths$Year) ##2004-2023

rec.lengths <- rec.lengths %>%
  mutate(Time_Block = case_when(
           Year < 2008 ~ '2004to2007',
           Year >= 2008 ~ '2008to2023'))




##* *Ordering factors*
#rec.lengths$State_Landed <- factor(rec.lengths$State_Landed,
#                               levels=c('MS', 'AL', 'FL'))


#rec.lengths$Sub_Reg <- factor(rec.lengths$Sub_Reg,
#                            levels = c('West', 'East'))



##* *Make sure only discard lengths are included*
unique(rec.lengths$Disposition)

rec.lengths <- rec.lengths %>%
  filter(Disposition == 'Discard')



##* *Drop samples with no lengths*
#rec.lengths <- rec.lengths %>%
#  filter(!is.na(Final_Length_mm))
  

##* *2 cm comps*

##* *Define size bins*
min(rec.lengths$Final_Length_mm, na.rm=TRUE)
max(rec.lengths$Final_Length_mm, na.rm=TRUE)


bin_size <- 2

min_size <- 0  

len_plus <- 130

up_bin_limit <- 132 ##defining as len_plus + bin_size (e.g. 121 + 1 cm)


##* *Define 1 cm size bins*
##* *For SA, need to use rnd function to ensure proper rounding (0.5 gets rounded up)*
rnd = function(x) trunc(x+0.5)

#rec.lengths$Final_Length_cm <- rnd(x=rec.lengths$Final_Length_mm/10)


##* *For GOM, use trunc function to label the floor*

rec.lengths$Final_Length_cm <- trunc(rec.lengths$Final_Length_mm/10)




##* *Creating proper size bins based on assessment parameters*
##* *Labeling floor of bin for GOM and middle for SA*
rec.lengths <- rec.lengths %>%
  mutate(bin_range = cut(Final_Length_cm,
                         breaks = seq(min_size, up_bin_limit, by=bin_size),
                         include.lowest = T,
                         right = F),
         bin       = cut(Final_Length_cm,
                         breaks = seq(min_size, up_bin_limit, by=bin_size),
                         labels = seq(min_size, len_plus, by=bin_size),
                         include.lowest = T,
                         right = F))




unique(rec.lengths$bin)



##______________________________________________________________________________
##* *Calculate nominal length comps for each fleet*

rec.lengths.orig <- rec.lengths

##* *First doing a single comp for each fleet pooled across years*

##* *Define n_fish and n_trips*
rec.lengths <- rec.lengths %>% 
  group_by(Fleet) %>%
  mutate(n_fish = n(),
         n_trip = length(unique(Sampling_Unit_ID)))


##* *Filter out strata with < 30 samples*
##* *Providing all comps and letting the assessment analyst choose what to filter*
##* *Will filter these samples out in working paper*
#rec.lengths <- subset(rec.lengths, n_fish >=30)


##* *Calculate nom length comps*
nom.length <- rec.lengths %>%
  group_by(Fleet, bin) %>%
  summarise(nom = n()/unique(n_fish)) %>%
  arrange(bin) 


##* *Check that nom comps sum to 1*
check <- nom.length %>%
  group_by(Fleet) %>%
  summarise(nom_sum = sum(nom, na.rm=TRUE))




##* *Making sure all years and bins are included in the data*
##* *Adding place holders for bins with no fish*
##* *Have to make for each fleet separately, otherwise, years will be missing in*
##* *the final fleet-specific comps*


#first.year <- 1981
#last.year  <- 2024




full.set <- crossing(bin  = seq(from = 0, to = 130, by = 2),
                     Fleet = 'Rec',
                     Time_Block = 'pooled') 



#full.set.hb <- crossing(bin  = seq(from = 20, to = 160, by = 5),
#                        Fleet = 'HB') 



#full.set <- rbind(full.set.hb, full.set.gr)





##* *Merge nominal comps and Full_Set*
nom.length <- merge(full.set, nom.length, by=c("Fleet", "bin"), all=TRUE)

nom.length$bin <- as.numeric(nom.length$bin)

nom.length <- nom.length %>%
  arrange(bin)


##* *Arrange nominal comps so the bins are columns*
nom.length <- nom.length %>%
  arrange(bin) %>%
  pivot_wider(names_from = bin, values_from = nom) 





##* *Add n_fish and n_trips to nominal comps output*
trips <- subset(rec.lengths, select=c("Fleet",
                                      "n_fish",
                                      "n_trip"))


trips <- trips %>%
  arrange(Fleet, n_fish, n_trip) %>%
  distinct(Fleet, n_fish, n_trip)



nom.length <- merge(trips, nom.length, by=c("Fleet"), all=TRUE)

nom.length[is.na(nom.length)] <- 0




##* *save for export*
 
pooled_2cm <- nom.length


##________________________________________________________________________________
##* *Two block period*
##* *Split as 2004-2007 and 2008-2023. The closure occured in 2008*


rec.lengths <- rec.lengths.orig
  
  


##* *Define n_fish and n_trips*
rec.lengths <- rec.lengths %>% 
  group_by(Time_Block, Fleet) %>%
  mutate(n_fish = n(),
         n_trip = length(unique(Sampling_Unit_ID)))


##* *Filter out strata with < 30 samples*
##* *Providing all comps and letting the assessment analyst choose what to filter*
##* *Will filter these samples out in working paper*
#rec.lengths <- subset(rec.lengths, n_fish >=30)


##* *Calculate nom length comps*
nom.length <- rec.lengths %>%
  group_by(Time_Block, Fleet, bin) %>%
  summarise(nom = n()/unique(n_fish)) %>%
  arrange(bin) 


##* *Check that nom comps sum to 1*
check <- nom.length %>%
  group_by(Time_Block, Fleet) %>%
  summarise(nom_sum = sum(nom, na.rm=TRUE))




##* *Making sure all years and bins are included in the data*
##* *Adding place holders for bins with no fish*
##* *Have to make for each fleet separately, otherwise, years will be missing in*
##* *the final fleet-specific comps*


#first.year <- 1981
#last.year  <- 2024




full.set1 <- crossing(bin  = seq(from = 0, to = 130, by = 2),
                        Fleet = 'Rec',
                        Time_Block = '2004to2007') 


full.set2 <- crossing(bin  = seq(from = 0, to = 130, by = 2),
                       Fleet = 'Rec',
                       Time_Block = '2008to2023') 

full.set <- rbind(full.set1, full.set2)


#full.set.hb <- crossing(bin  = seq(from = 20, to = 160, by = 5),
#                        Fleet = 'HB') 



#full.set <- rbind(full.set.hb, full.set.gr)





##* *Merge nominal comps and Full_Set*
nom.length <- merge(full.set, nom.length, by=c("Time_Block", "Fleet", "bin"), all=TRUE)

nom.length$bin <- as.numeric(nom.length$bin)

nom.length <- nom.length %>%
  arrange(bin)


##* *Arrange nominal comps so the bins are columns*
nom.length <- nom.length %>%
  arrange(bin) %>%
  pivot_wider(names_from = bin, values_from = nom) %>%
  arrange(Time_Block)





##* *Add n_fish and n_trips to nominal comps output*
trips <- subset(rec.lengths, select=c("Fleet",
                                      "Time_Block",
                                      "n_fish",
                                      "n_trip"))


trips <- trips %>%
  arrange(Time_Block, Fleet, n_fish, n_trip) %>%
  distinct(Time_Block, Fleet, n_fish, n_trip)



nom.length <- merge(trips, nom.length, by=c("Time_Block", "Fleet"), all=TRUE)

nom.length[is.na(nom.length)] <- 0



tblocks_2cm <- nom.length 





##______________________________________________________________________________
##* *Annual time blocks - SS needs annual*

rec.lengths <- rec.lengths.orig



##* *Define n_fish and n_trips*
rec.lengths <- rec.lengths %>% 
  group_by(Year, Fleet) %>%
  mutate(n_fish = n(),
         n_trip = length(unique(Sampling_Unit_ID)))


##* *Filter out strata with < 30 samples*
##* *Providing all comps and letting the assessment analyst choose what to filter*
##* *Will filter these samples out in working paper*
#rec.lengths <- subset(rec.lengths, n_fish >=30)


##* *Calculate nom length comps*
nom.length <- rec.lengths %>%
  group_by(Year, Fleet, bin) %>%
  summarise(nom = n()/unique(n_fish)) %>%
  arrange(bin) 


##* *Check that nom comps sum to 1*
check <- nom.length %>%
  group_by(Year, Fleet) %>%
  summarise(nom_sum = sum(nom, na.rm=TRUE))




##* *Making sure all years and bins are included in the data*
##* *Adding place holders for bins with no fish*
##* *Have to make for each fleet separately, otherwise, years will be missing in*
##* *the final fleet-specific comps*


first.year <- 2004
last.year  <- 2023




full.set <- crossing(bin  = seq(from = 0, to = 130, by = 2),
                        Year = seq(from = first.year, to = last.year, by = 1),
                        Fleet = 'Rec') 





#full.set <- rbind(full.set.hb, full.set.gr)





##* *Merge nominal comps and Full_Set*
nom.length <- merge(full.set, nom.length, by=c("Year", "Fleet", "bin"), all=TRUE)

nom.length$bin <- as.numeric(nom.length$bin)

nom.length <- nom.length %>%
  arrange(bin)


##* *Arrange nominal comps so the bins are columns*
nom.length <- nom.length %>%
  arrange(bin) %>%
  pivot_wider(names_from = bin, values_from = nom) %>%
  arrange(Year)





##* *Add n_fish and n_trips to nominal comps output*
trips <- subset(rec.lengths, select=c("Year",
                                      "Fleet",
                                      "n_fish",
                                      "n_trip"))


trips <- trips %>%
  arrange(Year, Fleet, n_fish, n_trip) %>%
  distinct(Year, Fleet, n_fish, n_trip)



nom.length <- merge(trips, nom.length, by=c("Year", "Fleet"), all=TRUE)

nom.length[is.na(nom.length)] <- 0



##* *Save for export*
annual_2cm <- nom.length




##______________________________________________________________________________

##* *5 cm*


##* *Import the data*
rec.lengths <- readxl::read_xlsx(paste0(dir,'/DiscardLenComps/SBS_rec_dsizAPAIS_0424_20260609.xlsx'),
                                 sheet = 'sbs', guess_max = 500000)



##* *Create Final Length Fields*
rec.lengths$Final_Length_mm   <- rec.lengths$Observed_FL_mm
rec.lengths$Final_Length_Type <- 'FL'


##* *Assign Fleet*
rec.lengths$Fleet <- 'Rec'

#rec.lengths <- rec.lengths %>%
#  mutate(Fleet = case_when(
#    Fishing_Mode %in% c('CB', 'PR', 'SH') ~ 'GR',
#    Fishing_Mode == 'HB' ~ 'HB',
#        .default = NA))



##* *Assign time blocks based on management history*
range(rec.lengths$Year) ##2004-2023

rec.lengths <- rec.lengths %>%
  mutate(Time_Block = case_when(
    Year < 2008 ~ '2004to2007',
    Year >= 2008 ~ '2008to2023'))




##* *Ordering factors*
#rec.lengths$State_Landed <- factor(rec.lengths$State_Landed,
#                               levels=c('MS', 'AL', 'FL'))


#rec.lengths$Sub_Reg <- factor(rec.lengths$Sub_Reg,
#                            levels = c('West', 'East'))



##* *Make sure only discard lengths are included*
unique(rec.lengths$Disposition)

rec.lengths <- rec.lengths %>%
  filter(Disposition == 'Discard')



##* *Drop samples with no lengths*
#rec.lengths <- rec.lengths %>%
#  filter(!is.na(Final_Length_mm))


##* *5 cm comps*

##* *Define size bins*
min(rec.lengths$Final_Length_mm, na.rm=TRUE)
max(rec.lengths$Final_Length_mm, na.rm=TRUE)


bin_size <- 5

min_size <- 0  

len_plus <- 130

up_bin_limit <- 135 ##defining as len_plus + bin_size (e.g. 121 + 1 cm)


##* *Define 1 cm size bins*
##* *For SA, need to use rnd function to ensure proper rounding (0.5 gets rounded up)*
rnd = function(x) trunc(x+0.5)

#rec.lengths$Final_Length_cm <- rnd(x=rec.lengths$Final_Length_mm/10)


##* *For GOM, use trunc function to label the floor*

rec.lengths$Final_Length_cm <- trunc(rec.lengths$Final_Length_mm/10)




##* *Creating proper size bins based on assessment parameters*
##* *Labeling floor of bin for GOM and middle for SA*
rec.lengths <- rec.lengths %>%
  mutate(bin_range = cut(Final_Length_cm,
                         breaks = seq(min_size, up_bin_limit, by=bin_size),
                         include.lowest = T,
                         right = F),
         bin       = cut(Final_Length_cm,
                         breaks = seq(min_size, up_bin_limit, by=bin_size),
                         labels = seq(min_size, len_plus, by=bin_size),
                         include.lowest = T,
                         right = F))




unique(rec.lengths$bin)



##______________________________________________________________________________
##* *Calculate nominal length comps for each fleet*

rec.lengths.orig <- rec.lengths

##* *First doing a single comp for each fleet pooled across years*

##* *Define n_fish and n_trips*
rec.lengths <- rec.lengths %>% 
  group_by(Fleet) %>%
  mutate(n_fish = n(),
         n_trip = length(unique(Sampling_Unit_ID)))


##* *Filter out strata with < 30 samples*
##* *Providing all comps and letting the assessment analyst choose what to filter*
##* *Will filter these samples out in working paper*
#rec.lengths <- subset(rec.lengths, n_fish >=30)


##* *Calculate nom length comps*
nom.length <- rec.lengths %>%
  group_by(Fleet, bin) %>%
  summarise(nom = n()/unique(n_fish)) %>%
  arrange(bin) 


##* *Check that nom comps sum to 1*
check <- nom.length %>%
  group_by(Fleet) %>%
  summarise(nom_sum = sum(nom, na.rm=TRUE))




##* *Making sure all years and bins are included in the data*
##* *Adding place holders for bins with no fish*
##* *Have to make for each fleet separately, otherwise, years will be missing in*
##* *the final fleet-specific comps*


#first.year <- 1981
#last.year  <- 2024




full.set <- crossing(bin  = seq(from = 0, to = 130, by = 5),
                     Fleet = 'Rec',
                     Time_Block = 'pooled') 



#full.set.hb <- crossing(bin  = seq(from = 20, to = 160, by = 5),
#                        Fleet = 'HB') 



#full.set <- rbind(full.set.hb, full.set.gr)





##* *Merge nominal comps and Full_Set*
nom.length <- merge(full.set, nom.length, by=c("Fleet", "bin"), all=TRUE)

nom.length$bin <- as.numeric(nom.length$bin)

nom.length <- nom.length %>%
  arrange(bin)


##* *Arrange nominal comps so the bins are columns*
nom.length <- nom.length %>%
  arrange(bin) %>%
  pivot_wider(names_from = bin, values_from = nom) 





##* *Add n_fish and n_trips to nominal comps output*
trips <- subset(rec.lengths, select=c("Fleet",
                                      "n_fish",
                                      "n_trip"))


trips <- trips %>%
  arrange(Fleet, n_fish, n_trip) %>%
  distinct(Fleet, n_fish, n_trip)



nom.length <- merge(trips, nom.length, by=c("Fleet"), all=TRUE)

nom.length[is.na(nom.length)] <- 0




##* *save for export*

pooled_5cm <- nom.length


##________________________________________________________________________________
##* *Two block period*
##* *Split as 2004-2007 and 2008-2023. The closure occured in 2008*


rec.lengths <- rec.lengths.orig




##* *Define n_fish and n_trips*
rec.lengths <- rec.lengths %>% 
  group_by(Time_Block, Fleet) %>%
  mutate(n_fish = n(),
         n_trip = length(unique(Sampling_Unit_ID)))


##* *Filter out strata with < 30 samples*
##* *Providing all comps and letting the assessment analyst choose what to filter*
##* *Will filter these samples out in working paper*
#rec.lengths <- subset(rec.lengths, n_fish >=30)


##* *Calculate nom length comps*
nom.length <- rec.lengths %>%
  group_by(Time_Block, Fleet, bin) %>%
  summarise(nom = n()/unique(n_fish)) %>%
  arrange(bin) 


##* *Check that nom comps sum to 1*
check <- nom.length %>%
  group_by(Time_Block, Fleet) %>%
  summarise(nom_sum = sum(nom, na.rm=TRUE))




##* *Making sure all years and bins are included in the data*
##* *Adding place holders for bins with no fish*
##* *Have to make for each fleet separately, otherwise, years will be missing in*
##* *the final fleet-specific comps*


#first.year <- 1981
#last.year  <- 2024




full.set1 <- crossing(bin  = seq(from = 0, to = 130, by = 5),
                      Fleet = 'Rec',
                      Time_Block = '2004to2007') 


full.set2 <- crossing(bin  = seq(from = 0, to = 130, by = 5),
                      Fleet = 'Rec',
                      Time_Block = '2008to2023') 

full.set <- rbind(full.set1, full.set2)


#full.set.hb <- crossing(bin  = seq(from = 20, to = 160, by = 5),
#                        Fleet = 'HB') 



#full.set <- rbind(full.set.hb, full.set.gr)





##* *Merge nominal comps and Full_Set*
nom.length <- merge(full.set, nom.length, by=c("Time_Block", "Fleet", "bin"), all=TRUE)

nom.length$bin <- as.numeric(nom.length$bin)

nom.length <- nom.length %>%
  arrange(bin)


##* *Arrange nominal comps so the bins are columns*
nom.length <- nom.length %>%
  arrange(bin) %>%
  pivot_wider(names_from = bin, values_from = nom) %>%
  arrange(Time_Block)





##* *Add n_fish and n_trips to nominal comps output*
trips <- subset(rec.lengths, select=c("Fleet",
                                      "Time_Block",
                                      "n_fish",
                                      "n_trip"))


trips <- trips %>%
  arrange(Time_Block, Fleet, n_fish, n_trip) %>%
  distinct(Time_Block, Fleet, n_fish, n_trip)



nom.length <- merge(trips, nom.length, by=c("Time_Block", "Fleet"), all=TRUE)

nom.length[is.na(nom.length)] <- 0



tblocks_5cm <- nom.length 





##______________________________________________________________________________
##* *Annual time blocks - SS needs annual*

rec.lengths <- rec.lengths.orig



##* *Define n_fish and n_trips*
rec.lengths <- rec.lengths %>% 
  group_by(Year, Fleet) %>%
  mutate(n_fish = n(),
         n_trip = length(unique(Sampling_Unit_ID)))


##* *Filter out strata with < 30 samples*
##* *Providing all comps and letting the assessment analyst choose what to filter*
##* *Will filter these samples out in working paper*
#rec.lengths <- subset(rec.lengths, n_fish >=30)


##* *Calculate nom length comps*
nom.length <- rec.lengths %>%
  group_by(Year, Fleet, bin) %>%
  summarise(nom = n()/unique(n_fish)) %>%
  arrange(bin) 


##* *Check that nom comps sum to 1*
check <- nom.length %>%
  group_by(Year, Fleet) %>%
  summarise(nom_sum = sum(nom, na.rm=TRUE))




##* *Making sure all years and bins are included in the data*
##* *Adding place holders for bins with no fish*
##* *Have to make for each fleet separately, otherwise, years will be missing in*
##* *the final fleet-specific comps*


first.year <- 2004
last.year  <- 2023




full.set <- crossing(bin  = seq(from = 0, to = 130, by = 5),
                     Year = seq(from = first.year, to = last.year, by = 1),
                     Fleet = 'Rec') 





#full.set <- rbind(full.set.hb, full.set.gr)





##* *Merge nominal comps and Full_Set*
nom.length <- merge(full.set, nom.length, by=c("Year", "Fleet", "bin"), all=TRUE)

nom.length$bin <- as.numeric(nom.length$bin)

nom.length <- nom.length %>%
  arrange(bin)


##* *Arrange nominal comps so the bins are columns*
nom.length <- nom.length %>%
  arrange(bin) %>%
  pivot_wider(names_from = bin, values_from = nom) %>%
  arrange(Year)





##* *Add n_fish and n_trips to nominal comps output*
trips <- subset(rec.lengths, select=c("Year",
                                      "Fleet",
                                      "n_fish",
                                      "n_trip"))


trips <- trips %>%
  arrange(Year, Fleet, n_fish, n_trip) %>%
  distinct(Year, Fleet, n_fish, n_trip)



nom.length <- merge(trips, nom.length, by=c("Year", "Fleet"), all=TRUE)

nom.length[is.na(nom.length)] <- 0



##* *Save for export*
annual_5cm <- nom.length










##______________________________________________________________________________
##* *10 cm bin*

##* *Import the data*
rec.lengths <- readxl::read_xlsx(paste0(dir,'/DiscardLenComps/SBS_rec_dsizAPAIS_0424_20260609.xlsx'),
                                 sheet = 'sbs', guess_max = 500000)



##* *Create Final Length Fields*
rec.lengths$Final_Length_mm   <- rec.lengths$Observed_FL_mm
rec.lengths$Final_Length_Type <- 'FL'


##* *Assign Fleet*
rec.lengths$Fleet <- 'Rec'

#rec.lengths <- rec.lengths %>%
#  mutate(Fleet = case_when(
#    Fishing_Mode %in% c('CB', 'PR', 'SH') ~ 'GR',
#    Fishing_Mode == 'HB' ~ 'HB',
#        .default = NA))



##* *Assign time blocks based on management history*
range(rec.lengths$Year) ##2004-2023

rec.lengths <- rec.lengths %>%
  mutate(Time_Block = case_when(
    Year < 2008 ~ '2004to2007',
    Year >= 2008 ~ '2008to2023'))




##* *Ordering factors*
#rec.lengths$State_Landed <- factor(rec.lengths$State_Landed,
#                               levels=c('MS', 'AL', 'FL'))


#rec.lengths$Sub_Reg <- factor(rec.lengths$Sub_Reg,
#                            levels = c('West', 'East'))



##* *Make sure only discard lengths are included*
unique(rec.lengths$Disposition)

rec.lengths <- rec.lengths %>%
  filter(Disposition == 'Discard')



##* *Drop samples with no lengths*
#rec.lengths <- rec.lengths %>%
#  filter(!is.na(Final_Length_mm))


##* *10 cm comps*

##* *Define size bins*
min(rec.lengths$Final_Length_mm, na.rm=TRUE)
max(rec.lengths$Final_Length_mm, na.rm=TRUE)


bin_size <- 10

min_size <- 0  

len_plus <- 130

up_bin_limit <- 140 ##defining as len_plus + bin_size (e.g. 121 + 1 cm)


##* *Define 1 cm size bins*
##* *For SA, need to use rnd function to ensure proper rounding (0.5 gets rounded up)*
rnd = function(x) trunc(x+0.5)

#rec.lengths$Final_Length_cm <- rnd(x=rec.lengths$Final_Length_mm/10)


##* *For GOM, use trunc function to label the floor*

rec.lengths$Final_Length_cm <- trunc(rec.lengths$Final_Length_mm/10)




##* *Creating proper size bins based on assessment parameters*
##* *Labeling floor of bin for GOM and middle for SA*
rec.lengths <- rec.lengths %>%
  mutate(bin_range = cut(Final_Length_cm,
                         breaks = seq(min_size, up_bin_limit, by=bin_size),
                         include.lowest = T,
                         right = F),
         bin       = cut(Final_Length_cm,
                         breaks = seq(min_size, up_bin_limit, by=bin_size),
                         labels = seq(min_size, len_plus, by=bin_size),
                         include.lowest = T,
                         right = F))




unique(rec.lengths$bin)



##______________________________________________________________________________
##* *Calculate nominal length comps for each fleet*

rec.lengths.orig <- rec.lengths

##* *First doing a single comp for each fleet pooled across years*

##* *Define n_fish and n_trips*
rec.lengths <- rec.lengths %>% 
  group_by(Fleet) %>%
  mutate(n_fish = n(),
         n_trip = length(unique(Sampling_Unit_ID)))


##* *Filter out strata with < 30 samples*
##* *Providing all comps and letting the assessment analyst choose what to filter*
##* *Will filter these samples out in working paper*
#rec.lengths <- subset(rec.lengths, n_fish >=30)


##* *Calculate nom length comps*
nom.length <- rec.lengths %>%
  group_by(Fleet, bin) %>%
  summarise(nom = n()/unique(n_fish)) %>%
  arrange(bin) 


##* *Check that nom comps sum to 1*
check <- nom.length %>%
  group_by(Fleet) %>%
  summarise(nom_sum = sum(nom, na.rm=TRUE))




##* *Making sure all years and bins are included in the data*
##* *Adding place holders for bins with no fish*
##* *Have to make for each fleet separately, otherwise, years will be missing in*
##* *the final fleet-specific comps*


#first.year <- 1981
#last.year  <- 2024




full.set <- crossing(bin  = seq(from = 0, to = 130, by = 10),
                     Fleet = 'Rec',
                     Time_Block = 'pooled') 



#full.set.hb <- crossing(bin  = seq(from = 20, to = 160, by = 5),
#                        Fleet = 'HB') 



#full.set <- rbind(full.set.hb, full.set.gr)





##* *Merge nominal comps and Full_Set*
nom.length <- merge(full.set, nom.length, by=c("Fleet", "bin"), all=TRUE)

nom.length$bin <- as.numeric(nom.length$bin)

nom.length <- nom.length %>%
  arrange(bin)


##* *Arrange nominal comps so the bins are columns*
nom.length <- nom.length %>%
  arrange(bin) %>%
  pivot_wider(names_from = bin, values_from = nom) 





##* *Add n_fish and n_trips to nominal comps output*
trips <- subset(rec.lengths, select=c("Fleet",
                                      "n_fish",
                                      "n_trip"))


trips <- trips %>%
  arrange(Fleet, n_fish, n_trip) %>%
  distinct(Fleet, n_fish, n_trip)



nom.length <- merge(trips, nom.length, by=c("Fleet"), all=TRUE)

nom.length[is.na(nom.length)] <- 0




##* *save for export*

pooled_10cm <- nom.length


##________________________________________________________________________________
##* *Two block period*
##* *Split as 2004-2007 and 2008-2023. The closure occurred in 2008*


rec.lengths <- rec.lengths.orig




##* *Define n_fish and n_trips*
rec.lengths <- rec.lengths %>% 
  group_by(Time_Block, Fleet) %>%
  mutate(n_fish = n(),
         n_trip = length(unique(Sampling_Unit_ID)))


##* *Filter out strata with < 30 samples*
##* *Providing all comps and letting the assessment analyst choose what to filter*
##* *Will filter these samples out in working paper*
#rec.lengths <- subset(rec.lengths, n_fish >=30)


##* *Calculate nom length comps*
nom.length <- rec.lengths %>%
  group_by(Time_Block, Fleet, bin) %>%
  summarise(nom = n()/unique(n_fish)) %>%
  arrange(bin) 


##* *Check that nom comps sum to 1*
check <- nom.length %>%
  group_by(Time_Block, Fleet) %>%
  summarise(nom_sum = sum(nom, na.rm=TRUE))




##* *Making sure all years and bins are included in the data*
##* *Adding place holders for bins with no fish*
##* *Have to make for each fleet separately, otherwise, years will be missing in*
##* *the final fleet-specific comps*


#first.year <- 1981
#last.year  <- 2024




full.set1 <- crossing(bin  = seq(from = 0, to = 130, by = 10),
                      Fleet = 'Rec',
                      Time_Block = '2004to2007') 


full.set2 <- crossing(bin  = seq(from = 0, to = 130, by = 10),
                      Fleet = 'Rec',
                      Time_Block = '2008to2023') 

full.set <- rbind(full.set1, full.set2)


#full.set.hb <- crossing(bin  = seq(from = 20, to = 160, by = 5),
#                        Fleet = 'HB') 



#full.set <- rbind(full.set.hb, full.set.gr)





##* *Merge nominal comps and Full_Set*
nom.length <- merge(full.set, nom.length, by=c("Time_Block", "Fleet", "bin"), all=TRUE)

nom.length$bin <- as.numeric(nom.length$bin)

nom.length <- nom.length %>%
  arrange(bin)


##* *Arrange nominal comps so the bins are columns*
nom.length <- nom.length %>%
  arrange(bin) %>%
  pivot_wider(names_from = bin, values_from = nom) %>%
  arrange(Time_Block)





##* *Add n_fish and n_trips to nominal comps output*
trips <- subset(rec.lengths, select=c("Fleet",
                                      "Time_Block",
                                      "n_fish",
                                      "n_trip"))


trips <- trips %>%
  arrange(Time_Block, Fleet, n_fish, n_trip) %>%
  distinct(Time_Block, Fleet, n_fish, n_trip)



nom.length <- merge(trips, nom.length, by=c("Time_Block", "Fleet"), all=TRUE)

nom.length[is.na(nom.length)] <- 0



tblocks_10cm <- nom.length 





##______________________________________________________________________________
##* *Annual time blocks - SS needs annual*

rec.lengths <- rec.lengths.orig



##* *Define n_fish and n_trips*
rec.lengths <- rec.lengths %>% 
  group_by(Year, Fleet) %>%
  mutate(n_fish = n(),
         n_trip = length(unique(Sampling_Unit_ID)))


##* *Filter out strata with < 30 samples*
##* *Providing all comps and letting the assessment analyst choose what to filter*
##* *Will filter these samples out in working paper*
#rec.lengths <- subset(rec.lengths, n_fish >=30)


##* *Calculate nom length comps*
nom.length <- rec.lengths %>%
  group_by(Year, Fleet, bin) %>%
  summarise(nom = n()/unique(n_fish)) %>%
  arrange(bin) 


##* *Check that nom comps sum to 1*
check <- nom.length %>%
  group_by(Year, Fleet) %>%
  summarise(nom_sum = sum(nom, na.rm=TRUE))




##* *Making sure all years and bins are included in the data*
##* *Adding place holders for bins with no fish*
##* *Have to make for each fleet separately, otherwise, years will be missing in*
##* *the final fleet-specific comps*


first.year <- 2004
last.year  <- 2023




full.set <- crossing(bin  = seq(from = 0, to = 130, by = 10),
                     Year = seq(from = first.year, to = last.year, by = 1),
                     Fleet = 'Rec') 





#full.set <- rbind(full.set.hb, full.set.gr)





##* *Merge nominal comps and Full_Set*
nom.length <- merge(full.set, nom.length, by=c("Year", "Fleet", "bin"), all=TRUE)

nom.length$bin <- as.numeric(nom.length$bin)

nom.length <- nom.length %>%
  arrange(bin)


##* *Arrange nominal comps so the bins are columns*
nom.length <- nom.length %>%
  arrange(bin) %>%
  pivot_wider(names_from = bin, values_from = nom) %>%
  arrange(Year)





##* *Add n_fish and n_trips to nominal comps output*
trips <- subset(rec.lengths, select=c("Year",
                                      "Fleet",
                                      "n_fish",
                                      "n_trip"))


trips <- trips %>%
  arrange(Year, Fleet, n_fish, n_trip) %>%
  distinct(Year, Fleet, n_fish, n_trip)



nom.length <- merge(trips, nom.length, by=c("Year", "Fleet"), all=TRUE)

nom.length[is.na(nom.length)] <- 0



##* *Save for export*
annual_10cm <- nom.length






##______________________________________________________________________________
##* *Export the nominal comps*

first.year <- 2004
last.year  <- 2023

table.ID <- paste0("SBS_rec_dlfd_",
                   substr(first.year, nchar(first.year)-1, nchar(first.year)),
                   substr(last.year, nchar(last.year)-1, nchar(last.year)),
                   "_", gsub("-","", Sys.Date()))


wb = createWorkbook()

addWorksheet(wb, sheet='annual_2cm')
writeData(wb, sheet='annual_2cm', annual_2cm, colNames=TRUE)
addWorksheet(wb, sheet='pooled_2cm')
writeData(wb, sheet='pooled_2cm', pooled_2cm, colNames=TRUE)
addWorksheet(wb, sheet='tblocks_2cm')
writeData(wb, sheet='tblocks_2cm', tblocks_2cm, colNames=TRUE)

addWorksheet(wb, sheet='annual_5cm')
writeData(wb, sheet='annual_5cm', annual_5cm, colNames=TRUE)
addWorksheet(wb, sheet='pooled_5cm')
writeData(wb, sheet='pooled_5cm', pooled_5cm, colNames=TRUE)
addWorksheet(wb, sheet='tblocks_5cm')
writeData(wb, sheet='tblocks_5cm', tblocks_5cm, colNames=TRUE)

addWorksheet(wb, sheet='annual_10cm')
writeData(wb, sheet='annual_10cm', annual_10cm, colNames=TRUE)
addWorksheet(wb, sheet='pooled_10cm')
writeData(wb, sheet='pooled_10cm', pooled_10cm, colNames=TRUE)
addWorksheet(wb, sheet='tblocks_10cm')
writeData(wb, sheet='tblocks_10cm', tblocks_10cm, colNames=TRUE)



saveWorkbook(wb, file=paste0(dir, "/DiscardLenComps/", table.ID, ".xlsx"), overwrite=TRUE)




##______________________________________________________________________________
##* *Export single tab for easier graphing*

agg.comps_2cm <- rbind(pooled_2cm, tblocks_2cm)

agg.comps_5cm <- rbind(pooled_2cm, tblocks_5cm)

agg.comps_10cm <- rbind(pooled_2cm, tblocks_10cm)


table.ID <- paste0("TIL_rec_dlfd_agg_singletab_",
                   substr(first.year, nchar(first.year)-1, nchar(first.year)),
                   substr(last.year, nchar(last.year)-1, nchar(last.year)),
                   "_", gsub("-","", Sys.Date()))


wb = createWorkbook()

addWorksheet(wb, sheet='2cm')
writeData(wb, sheet='2cm', agg.comps_2cm, colNames=TRUE)


addWorksheet(wb, sheet='5cm')
writeData(wb, sheet='5cm', agg.comps_5cm, colNames=TRUE)


addWorksheet(wb, sheet='10cm')
writeData(wb, sheet='10cm', agg.comps_10cm, colNames=TRUE)


saveWorkbook(wb, file=paste0(dir, "/DiscardLenComps/", table.ID, ".xlsx"), overwrite=TRUE)


