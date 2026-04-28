
##* *Digging into outliers in the catch file*


librarian::shelf(openxlsx, readxl, tidyverse, dplyr, haven, janitor, ggplot2)


##* *Define directory*
dir <- getwd()


##* *Prevents R from converting any long numbers (ID codes) to scientific notation*
options(scipen=999)



##* *Import merged trip catch file*
trip.catch <- read_sas(data_file = "C:\\Users\\samantha.binion-rock\\Desktop\\WorkFiles\\GitHub_Repos\\SEDAR94_FL_Hogfish\\Catch\\trip_catch.sas7bdat")







##* *Only keep Atlantic Sandbar Shark*
sandbar <- trip.catch %>%
  filter(COMMON == 'SANDBAR SHARK' & SUB_REG %in% c(4,5,6,7))


#save.image(file = paste0( dir,"/Sandbar_Trip_Catch.RData"))

#load(paste0(dir,"/Sandbar_Trip_Catch.RData" ))


##* *Calculate AB1 and B2*
sandbar <- sandbar %>%
  mutate(claim.harvest = CLAIM + HARVEST)


sandbar <- sandbar %>%
  mutate(AB1 = claim.harvest * WP_CATCH,
         B2  = RELEASE * WP_CATCH)


##* *Calculate annual totals to make sure I calculated expanded catch correctly*
sandbar.annual <- sandbar %>%
  group_by(INT_YEAR) %>%
  summarise(AB1_ann = sum(AB1, na.rm = TRUE),
            B2_ann  = sum(B2,  na.rm = TRUE))



##* *Summarize annual landings and discards by state, mode, area fished, and wave*
sandbar.summary <- sandbar %>%
  group_by(INT_YEAR, WAVE, ST, AREA_X, MODE_FX, SUB_REG) %>%
  summarise(AB1_total = sum(AB1),
            B2_total  = sum(B2))


##* *Merge with annual summaries and calculate proportion of total each strata contributes*
sandbar.summary <- merge(sandbar.summary, sandbar.annual, by = 'INT_YEAR', all=TRUE)


sandbar.summary <- sandbar.summary %>%
  mutate(AB1_prop = round(AB1_total/AB1_ann, 3),
         B2_prop  = round(B2_total/B2_ann, 3))







##* *Metadata notes*
##* *CLAIM - Observed Harvest, Type A Catch*
##* *HARVEST - Unobserved Harvest, Type B1 Catch*
##* *RELEASE - Released Fish, Type B2 Catch*



##* *AREA_X*
##* *1 = Ocean <= 3 mi (all but WFL)*
##* *2 = Ocean > 3 mi(all but WFL)*
##* *3 = Ocean <= 10 mi (WFL only)*
##* *4 = Ocean > 10 mi (WFL only)*
##* *5 = Inland*



##* *MODE_FX*
##* *1 = Man-made*
##* *2 = Beach/bank*
##* *3 = Shore*
##* *4 = Headboat*
##* *5 = Charter*
##* *7 = Private*



##* *States FIPS Codes*
##* *1  = AL*
##* *9  = CT*
##* *10 = DE*
##* *12 = FL*
##* *13 = GA*
##* *22 = LA*
##* *23 = ME*
##* *24 = MD*
##* *25 = MA*
##* *28 = MS*
##* *33 = NH*
##* *34 = NJ*
##* *36 = NY*
##* *37 = NC*
##* *44 = RI*
##* *45 = SC*
##* *48 = TX*
##* *50 = VT*
##* *51 = VA*



##* *Years to dig into - Identify after you make a line plot*
##* *AB1 - 1983*
##* *B2  - 1983, 1986*


##* *Subset fields needed to dig into outliers*
sandbar.clean <- subset(sandbar, select = c('INT_YEAR', 'WAVE', 'MODE_FX', 'SUB_REG', 'ST',
                                    'AREA_X', 'CLAIM_UNADJ', 'HARVEST_UNADJ', 
                                    'RELEASE_UNADJ', 'WP_CATCH', 'AB1', 'B2'))








##* *1983 AB1*

prop <- sandbar.summary %>%
  filter(INT_YEAR == 1983 & AB1_prop > 0) %>%
  arrange(desc(AB1_prop))

check.1983 <- sandbar.clean %>%
  filter(INT_YEAR == 1983,
         WAVE     == 3,
         ST       == 45,
         AREA_X   == 5,
         MODE_FX  == 7,
         SUB_REG  == 6)


check.1983b <- sandbar.clean %>%
  filter(INT_YEAR == 1983,
         WAVE     == 3,
         ST       == 45,
         AREA_X   == 1,
         MODE_FX  == 7,
         SUB_REG  == 6)



all.1983 <- sandbar.clean %>%
  filter(INT_YEAR == 1983 & AB1 > 0)


##* *Calculate the proportion each intercept contributes to annual total*
all.1983 <- all.1983 %>%
  mutate(AB1_total = sum(AB1),
         AB1_prop  = round(AB1/AB1_total,3)) %>%
  arrange(desc(AB1_prop))
  


check <- all.1983 %>%
  filter(AB1_prop > 0.05)




