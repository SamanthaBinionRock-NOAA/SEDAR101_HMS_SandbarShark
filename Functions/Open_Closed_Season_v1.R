


Open_Closed_Season <- function(size.table, year, WAVE, NEW_MODEN, NEW_STA){
  

##* *Assign Open/Closed Federal Seasons*

##* *Gulf of Mexico Red Snapper* 
##   Refer to S74 Management History on the S-drive to determine 
##   which combinations of year/wave/mode are considered:
##             0 = open
##             1 = partially open
##             2 = closed
##   In previous SEDARs, Vivian assigned open/closed seasons to a couple 
##   wave0 (TPWD) records. Although wave0 should no longer be present in
##   'catch.table' (TPWD now provides wave catch), these assignments are
##   retained (in the script below) for record keeping.


 size.table = size.table %>%
   mutate(fed_closed = ifelse(year <= 1996 , 0,                               ### Before 1997, season open year round
                       ifelse(year == 1997 & 
                              WAVE %in% 1:5, 0,   ## 1997 -- Jan1 - Nov27
                       ifelse(year == 1997 & 
                              WAVE == 6, 1,
                       ifelse(year == 1998 & 
                              WAVE %in% 1:4, 0,   ##  1998 -- Jan1 - Sep30
                       ifelse(year == 1998 & 
                              WAVE == 5, 1,
                       ifelse(year == 1998 & 
                              WAVE == 6, 2,
                       ifelse(year == 1999 & 
                              WAVE %in% 1:4, 0,   ##  1999 -- Jan1 - Aug29
                       ifelse(year == 1999 & 
                              WAVE %in% 5:6, 2,
                       ifelse(year %in% 2000:2007 & 
                              WAVE %in% c(1,2,6), 2,    ##  2000-2007 -- Apr21 - Nov1
                       ifelse(year %in% 2000:2007 & 
                              WAVE %in% 3:5, 0,
                       ifelse(year %in% 2008:2009 & 
                              WAVE %in% c(1:2,5:6), 2,  ##  2008 -- Jun1 - Aug5
                       ifelse(year %in% 2008:2009 & 
                              WAVE %in% 3:4, 1,         ##  2009 -- Jun1 - Aug15
                       ifelse(year == 2010 & 
                              WAVE %in% 1:2, 2,    ##  2010 -- Jun1 - Jul24
                       ifelse(year == 2010 & 
                              WAVE %in% 3:6, 1,    ## plus 24-day fall season (DWH)
                       ifelse(year == 2010 & 
                              WAVE == 0, 2,    ## one instance of wave0 from TPWD low season (closed)
                       ifelse(year == 2011 & 
                              WAVE %in% c(1,2,5,6), 2,         ## 2011 -- Jun1 - Jul19
                       ifelse(year == 2011 & 
                              WAVE %in% 3:4, 1,
                       ifelse(year == 2012 & 
                              WAVE %in% c(1,2,5,6), 2,       ##  2012 -- Jun1 - Jul17
                       ifelse(year == 2012 & 
                              WAVE %in% 3:4, 1,
                       ifelse(year == 2013 & 
                              WAVE %in% c(1,2,4,6), 2,       ##  2013 -- Jun1 - Jun29
                       ifelse(year == 2013 & 
                              WAVE %in% c(3,5), 1,           ##  2013 -- Oct1 - Oct15
                       ifelse(year == 2014 & 
                              WAVE %in% c(1,2,4:6), 2,       ##  2014 -- Jun1 - Jun9
                       ifelse(year == 2014 & 
                              WAVE == 3, 1, NA
                               ))))))))))))))))))))))) )
 
## 2015 = first year where separate seasons set for private vs. charter 
  size.table = size.table %>%
   mutate(fed_closed = ifelse(year == 2015 &
                              NEW_MODEN %in% c("Priv","Priv/Shore") & 
                              WAVE %in% c(1,2,4:6), 2,  ## 2015 Priv - Jun1 - Jun11
                       ifelse(year == 2015 &
                              NEW_MODEN %in% c("Priv","Priv/Shore") & 
                              WAVE == 3, 1,
                       ifelse(year == 2015 & 
                              NEW_MODEN == "Cbt" & 
                              WAVE %in% c(1,2,5,6), 2,  ## 2015 Hire -- Jun1 - Jul15
                       ifelse(year == 2015 & 
                              NEW_MODEN == "Cbt" & 
                              WAVE %in% 3:4, 1,
                       ifelse(year == 2016 &
                              NEW_MODEN %in% c("Priv","Priv/Shore") & 
                              WAVE %in% c(1,2,4:6), 2,  ## 2016 Priv -- Jun1 - Jun12
                       ifelse(year == 2016 &
                              NEW_MODEN %in% c("Priv","Priv/Shore") & WAVE == 3, 1,
                       ifelse(year == 2016 & 
                              NEW_MODEN == "Cbt" & 
                              WAVE %in% c(1,2,5,6), 2,  ## 2016 Hire -- Jun1 - Jul17
                       ifelse(year == 2016 & 
                              NEW_MODEN == "Cbt" & 
                              WAVE %in% 3:4, 1,  
                       ifelse(year == 2016 & 
                              NEW_MODEN == "Cbt" & 
                              WAVE == 0, 2,   ## One instance of wave0 from TPWD low season (closed)
                       ifelse(year == 2017 &
                              NEW_MODEN %in% c("Priv","Priv/Shore") & 
                              WAVE %in% c(1,2,6), 2,  ## 2017 Priv -- Jun1 - Jul13
                       ifelse(year == 2017 &
                              NEW_MODEN %in% c("Priv","Priv/Shore") & 
                              WAVE %in% 3:5, 1,
                       ifelse(year == 2017 & 
                              NEW_MODEN == "Cbt" & 
                              WAVE %in% c(1,2,5,6), 2,    ## 2017 Hire -- Jun1 - Jul19
                       ifelse(year == 2017 & 
                              NEW_MODEN == "Cbt" & 
                              WAVE %in% 3:4, 1, fed_closed
                               ))))))))))))) )
 
## 2018 = first year with exempted fishing permits (EFPs), which allowed states 
## to set their own seasons 
 size.table = size.table %>%
   mutate(fed_closed = ifelse(year == 2018 & 
                              NEW_MODEN %in% c("Priv","Priv/Shore") &
                              NEW_STA == "FLW" & 
                              WAVE %in% c(1,2,5,6), 2,  ## 2018 - Priv -- State-specific
                       ifelse(year == 2018 & 
                              NEW_MODEN %in% c("Priv","Priv/Shore") &
                              NEW_STA == "FLW" & 
                              WAVE %in% 3:4, 1,    ## FWC -- Jun11 - Jul20
                       ifelse(year == 2018 & 
                              NEW_MODEN %in% c("Priv","Priv/Shore") &
                              NEW_STA == "AL" & 
                              WAVE %in% c(1,2,5,6), 2,   ## ALDNR -- Jun1 - Jul22
                       ifelse(year == 2018 & 
                              NEW_MODEN %in% c("Priv","Priv/Shore") &
                              NEW_STA == "AL" & 
                              WAVE %in% 3:4, 1,   ## -- weekends/holidays
                       ifelse(year == 2018 & 
                              NEW_MODEN %in% c("Priv","Priv/Shore") &            
                              NEW_STA == "MS" & 
                              WAVE %in% c(1,2,5,6), 2,   ## MSDMR -- May25 - Jul9
                       ifelse(year == 2018 & 
                              NEW_MODEN %in% c("Priv","Priv/Shore") &
                              NEW_STA == "MS" & 
                              WAVE %in% 3:4, 1,   ###  -- Jul22 - Sep3
                       ifelse(year == 2018 & 
                              NEW_MODEN %in% c("Priv","Priv/Shore") &
                              NEW_STA == "LA" & WAVE %in% c(1,2,5,6), 2,                        ###     ...LDWF -- May25 - Jul8
                       ifelse(year == 2018 & 
                              NEW_MODEN %in% c("Priv","Priv/Shore") &
                              NEW_STA == "LA" & 
                              WAVE %in% 3:4, 1,    ## -- Jul13 - Aug12 (weekends only)
                       ifelse(year == 2018 & 
                              NEW_MODEN %in% c("Priv","Priv/Shore") &
                              NEW_STA == "TX" & 
                              WAVE %in% c(1,2,5,6), 2,   ### TPWD -- Jun1 - Aug21
                       ifelse(year == 2018 & 
                              NEW_MODEN %in% c("Priv","Priv/Shore") &
                              NEW_STA == "TX" & 
                              WAVE %in% 3:4, 1,
                       ifelse(year == 2018 & 
                              NEW_MODEN == "Cbt" & 
                              WAVE %in% c(1,2,5,6), 2,   ## 2018 - Hire -- Jun1 - Jul22
                       ifelse(year == 2018 & 
                              NEW_MODEN == "Cbt" & 
                              WAVE %in% 3:4, 1,
                       ifelse(year == 2019 & 
                              NEW_MODEN %in% c("Priv","Priv/Shore") &
                              NEW_STA == "FLW" & 
                              WAVE %in% c(1,2,5,6), 2,   ## 2019 - Priv -- State-specific
                       ifelse(year == 2019 & 
                              NEW_MODEN %in% c("Priv","Priv/Shore") &
                             NEW_STA == "FLW" & 
                             WAVE %in% 3:4, 1,    ## FWC -- Jun11 - Jul12, Oct 12-13,19-20,26-27, Nov 2-3
                       ifelse(year == 2019 & 
                             NEW_MODEN %in% c("Priv","Priv/Shore") &
                             NEW_STA == "AL" & 
                             WAVE %in% c(1,2,5,6), 2,  ## ALDNR -- Jun1 - Aug5, Oct 4-5
                       ifelse(year == 2019 & 
                             NEW_MODEN %in% c("Priv","Priv/Shore") &
                             NEW_STA == "AL" & 
                             WAVE %in% 3:4, 1,   ## weekends/holidays
                       ifelse(year == 2019 & 
                             NEW_MODEN %in% c("Priv","Priv/Shore") &
                             NEW_STA == "MS" & 
                             WAVE %in% c(1,2,5,6), 2,  ## MSDMR -- May24 - Jul8
                       ifelse(year == 2019 & 
                             NEW_MODEN %in% c("Priv","Priv/Shore") &
                             NEW_STA == "MS" & 
                             WAVE %in% 3:4, 1,     ## -- Jul28 - Sep2
                       ifelse(year == 2019 & 
                             NEW_MODEN %in% c("Priv","Priv/Shore") &
                             NEW_STA == "LA" & 
                             WAVE %in% 1:2, 2,     ##  LDWF -- May24 - Sep3, Sep 27 - Nov 24 (weekends/holidays)
                       ifelse(year == 2019 & 
                             NEW_MODEN %in% c("Priv","Priv/Shore") &
                             NEW_STA == "LA" & 
                             WAVE %in% 3:6, 1,  ##  -- Nov28 - Dec31
                       ifelse(year == 2019 & 
                             NEW_MODEN %in% c("Priv","Priv/Shore") &
                             NEW_STA == "TX" & 
                             WAVE %in% c(1,2,5,6), 2,    ## TPWD -- Jun1 - Aug2
                       ifelse(year == 2019 & 
                             NEW_MODEN %in% c("Priv","Priv/Shore") &
                             NEW_STA == "TX" & 
                             WAVE %in% 3:4, 1,
                       ifelse(year == 2019 & 
                             NEW_MODEN == "Cbt" & 
                             WAVE %in% c(1,2,5,6), 2,  ## 2019 - Hire -- Jun1 - Aug2
                       ifelse(year == 2019 & 
                             NEW_MODEN == "Cbt" & 
                             WAVE %in% 3:4, 1, fed_closed
                              )))))))))))))))))))))))) )
 
 return(size.table)
 
 } 

 
 