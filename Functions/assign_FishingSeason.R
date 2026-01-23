

### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------
###
###     DESCRIPTION
###
###   ...where, for some SEDARs, data/estimates are requested to be summarized based on whether
###       they were collected during or estimated for a period of time coinciding with an
###       open vs. closed (federal) fishing season (i.e., was fishing permitted? ). The functions below
###       make such distinctions in the relevant GenRec tables, identified by a 'fed_closed' field...
###
###
###   assign.fishing.season( )
###
###       ...which assigns open/closed (fishing) designations to each elements in a given data table
###         (e.g., of raw size data or catch/effort estimates ). Note that depending on how these
###         assignments are made, the second function may also need to be run to ensure the required
###         fields are present in these tables (e.g., if assignments are made based on data collected
###         at the trip/day-level, we might need to reference the associated micro-data )...
###
###
###   partition.fishing.season( )
###
###       ...which references trip-level microdata to identify which days (of fishing) are responsible
###         for what percentage of a set of catch or effort estimates. These percentages are then applied
###         to wave-level estimates to partition them into open vs. closed seasonal designations...
###
### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------

assign.fishing.season = function( new.com, region, genrec.table ) {
  ###     ...where 'new.com' and 'region' identify the stock that is being assessed
  ###          and 'genrec.table' is the table (of data/estimates) to which the 'fed_closed' field may be added...
  
  
  ###   VALIDATION   ###
  ### ------------------
  ###     ...as a check of the open/closed dates being provided by multiple sources
  ###       (e.g., state agencies, websites, MH database ), I compared the length of time between listed dates
  ###       ( using the difftime() function ) to any estimates provided for the 'Federal Season Length'.
  ### 
  ###   For the length of days of a 'daily' season (i.e., open all days between two dates ):
  # difftime( as.Date('2020-06-01'),as.Date('2020-08-03'), units=c('days') )
  ###
  ###   ...or for those seasons that just run on certain days (e.g., only open weekends ):
  # Date1 = as.Date( '2020-06-01' )
  # Date2 = as.Date( '2020-06-30' )
  # sum( weekdays( seq(Date1,Date2,'days') ) %in% c('Friday','Saturday','Sunday') )
  # rm( Date1, Date2 )
  
  
  
  ### ---------------------------------------------------------------------------------------------------------
  ### SEDAR 74 -- Gulf of America RED SNAPPER ###
  ###
  ###       ...for which the S74 Management History table (on the S-drive) was used to split wave-level data/estimates
  ###         ( by year, mode, and state ) into designations of open/closed fishing seasons:
  ###             0 = open
  ###             1 = partially open
  ###             2 = closed
  ###       Note that, in previous SEDARs, Vivian assigned open/closed seasons to a couple wave0 records from TPWD
  ###       ( see 2010 & 2016 ). Although wave0 should no longer be present in 'catch.table' as TPWD now provides
  ###       wave-level catch, I retain these assignments in the script below for record keeping...
  # 
  # if( region == 'Gulf of America' & 'red snapper' %in% new.com ) {
  #   
  #   genrec.table = genrec.table %>%
  #     mutate( fed_closed = ifelse( YEAR <= 1996 , 0,                               ### Before 1997, season open year round
  #                          ifelse( YEAR == 1997 & WAVE %in% 1:5, 0,                ###  1997 -- Jan1 - Nov27
  #                          ifelse( YEAR == 1997 & WAVE == 6, 1,
  #                          ifelse( YEAR == 1998 & WAVE %in% 1:4, 0,                ###  1998 -- Jan1 - Sep30
  #                          ifelse( YEAR == 1998 & WAVE == 5, 1,
  #                          ifelse( YEAR == 1998 & WAVE == 6, 2,
  #                          ifelse( YEAR == 1999 & WAVE %in% 1:4, 0,                ###  1999 -- Jan1 - Aug29
  #                          ifelse( YEAR == 1999 & WAVE %in% 5:6, 2,
  #                          ifelse( YEAR %in% 2000:2007 & WAVE %in% c(1,2,6), 2,    ###  2000-2007 -- Apr21 - Nov1
  #                          ifelse( YEAR %in% 2000:2007 & WAVE %in% 3:5, 0,
  #                          ifelse( YEAR %in% 2008:2009 & WAVE %in% c(1:2,5:6), 2,  ###  2008 -- Jun1 - Aug5
  #                          ifelse( YEAR %in% 2008:2009 & WAVE %in% 3:4, 1,         ###  2009 -- Jun1 - Aug15
  #                          ifelse( YEAR == 2010 & WAVE %in% 1:2, 2,                ###  2010 -- Jun1 - Jul24
  #                          ifelse( YEAR == 2010 & WAVE %in% 3:6, 1,                ###     ...plus 24-day fall season (DWH)
  #                          ifelse( YEAR == 2010 & WAVE == 0, 2,                    ###     ...one instance of wave0 from TPWD low season (closed)
  #                          ifelse( YEAR == 2011 & WAVE %in% c(1,2,5,6), 2,         ###  2011 -- Jun1 - Jul19
  #                          ifelse( YEAR == 2011 & WAVE %in% 3:4, 1,
  #                          ifelse( YEAR == 2012 & WAVE %in% c(1,2,5,6), 2,         ###  2012 -- Jun1 - Jul17
  #                          ifelse( YEAR == 2012 & WAVE %in% 3:4, 1,
  #                          ifelse( YEAR == 2013 & WAVE %in% c(1,2,4,6), 2,         ###  2013 -- Jun1 - Jun29
  #                          ifelse( YEAR == 2013 & WAVE %in% c(3,5), 1,             ###       -- Oct1 - Oct15
  #                          ifelse( YEAR == 2014 & WAVE %in% c(1,2,4:6), 2,         ###  2014 -- Jun1 - Jun9
  #                          ifelse( YEAR == 2014 & WAVE == 3, 1, NA
  #                                  ))))))))))))))))))))))) )
  # 
  #   ### 2015 = first year where separate seasons set for private vs. charter ###
  #   genrec.table = genrec.table %>%
  #     mutate( fed_closed = ifelse( YEAR == 2015 &
  #                                  NEW_MODEN %in% c("Priv","Priv/Shore") & WAVE %in% c(1,2,4:6), 2,  ### 2015 - Priv -- Jun1 - Jun11
  #                          ifelse( YEAR == 2015 &
  #                                  NEW_MODEN %in% c("Priv","Priv/Shore") & WAVE == 3, 1,
  #                          ifelse( YEAR == 2015 & NEW_MODEN == "Cbt" & WAVE %in% c(1,2,5,6), 2,        ### 2015 - Hire -- Jun1 - Jul15
  #                          ifelse( YEAR == 2015 & NEW_MODEN == "Cbt" & WAVE %in% 3:4, 1,
  # 
  #                          ifelse( YEAR == 2016 &
  #                                  NEW_MODEN %in% c("Priv","Priv/Shore") & WAVE %in% c(1,2,4:6), 2,  ### 2016 - Priv -- Jun1 - Jun12
  #                          ifelse( YEAR == 2016 &
  #                                  NEW_MODEN %in% c("Priv","Priv/Shore") & WAVE == 3, 1,
  #                          ifelse( YEAR == 2016 & NEW_MODEN == "Cbt" & WAVE %in% c(1,2,5,6), 2,      ### 2016 - Hire -- Jun1 - Jul17
  #                          ifelse( YEAR == 2016 & NEW_MODEN == "Cbt" & WAVE %in% 3:4, 1,             ###   ...one instance of wave0 from
  #                          ifelse( YEAR == 2016 & NEW_MODEN == "Cbt" & WAVE == 0, 2,                 ###       TPWD low season (closed)
  # 
  #                          ifelse( YEAR == 2017 &
  #                                  NEW_MODEN %in% c("Priv","Priv/Shore") & WAVE %in% c(1,2,6), 2,  ### 2017 - Priv -- Jun1 - Jul13
  #                          ifelse( YEAR == 2017 &
  #                                  NEW_MODEN %in% c("Priv","Priv/Shore") & WAVE %in% 3:5, 1,
  #                          ifelse( YEAR == 2017 & NEW_MODEN == "Cbt" & WAVE %in% c(1,2,5,6), 2,      ### 2017 - Hire -- Jun1 - Jul19
  #                          ifelse( YEAR == 2017 & NEW_MODEN == "Cbt" & WAVE %in% 3:4, 1, fed_closed
  #                                  ))))))))))))) )
  # 
  #   ### 2018 = first year with exempted fishing permits (EFPs), which allowed states to set their own seasons ###
  #   genrec.table = genrec.table %>%
  #     mutate( fed_closed = ifelse( YEAR == 2018 & NEW_MODEN %in% c("Priv","Priv/Shore") &
  #                                  NEW_STA == "FLW" & WAVE %in% c(1,2,5,6), 2,                     ### 2018 - Priv -- State-specific
  #                          ifelse( YEAR == 2018 & NEW_MODEN %in% c("Priv","Priv/Shore") &
  #                                  NEW_STA == "FLW" & WAVE %in% 3:4, 1,                            ###      ...FWC -- Jun11 - Jul20
  #                          ifelse( YEAR == 2018 & NEW_MODEN %in% c("Priv","Priv/Shore") &
  #                                  NEW_STA == "AL" & WAVE %in% c(1,2,5,6), 2,                       ###    ...ALDNR -- Jun1 - Jul22
  #                          ifelse( YEAR == 2018 & NEW_MODEN %in% c("Priv","Priv/Shore") &
  #                                  NEW_STA == "AL" & WAVE %in% 3:4, 1,                              ###             -- weekends/holidays
  #                          ifelse( YEAR == 2018 & NEW_MODEN %in% c("Priv","Priv/Shore") &
  #                                  NEW_STA == "MS" & WAVE %in% c(1,2,5,6), 2,                       ###    ...MSDMR -- May25 - Jul9
  #                          ifelse( YEAR == 2018 & NEW_MODEN %in% c("Priv","Priv/Shore") &
  #                                  NEW_STA == "MS" & WAVE %in% 3:4, 1,                              ###             -- Jul22 - Sep3
  #                          ifelse( YEAR == 2018 & NEW_MODEN %in% c("Priv","Priv/Shore") &
  #                                  NEW_STA == "LA" & WAVE %in% c(1,2,5,6), 2,                        ###     ...LDWF -- May25 - Jul8
  #                          ifelse( YEAR == 2018 & NEW_MODEN %in% c("Priv","Priv/Shore") &
  #                                  NEW_STA == "LA" & WAVE %in% 3:4, 1,                               ###             -- Jul13 - Aug12 (weekends only)
  #                          ifelse( YEAR == 2018 & NEW_MODEN %in% c("Priv","Priv/Shore") &
  #                                  NEW_STA == "TX" & WAVE %in% c(1,2,5,6), 2,                        ###     ...TPWD -- Jun1 - Aug21
  #                          ifelse( YEAR == 2018 & NEW_MODEN %in% c("Priv","Priv/Shore") &
  #                                    NEW_STA == "TX" & WAVE %in% 3:4, 1,
  #                          ifelse( YEAR == 2018 & NEW_MODEN == "Cbt" & WAVE %in% c(1,2,5,6), 2,      ### 2018 - Hire -- Jun1 - Jul22
  #                          ifelse( YEAR == 2018 & NEW_MODEN == "Cbt" & WAVE %in% 3:4, 1,
  # 
  #                          ifelse( YEAR == 2019 & NEW_MODEN %in% c("Priv","Priv/Shore") &
  #                                  NEW_STA == "FLW" & WAVE %in% c(1,2,5,6), 2,                     ### 2019 - Priv -- State-specific
  #                          ifelse( YEAR == 2019 & NEW_MODEN %in% c("Priv","Priv/Shore") &
  #                                  NEW_STA == "FLW" & WAVE %in% 3:4, 1,                            ###      ...FWC -- Jun11 - Jul12, Oct 12-13,19-20,26-27, Nov 2-3
  #                          ifelse( YEAR == 2019 & NEW_MODEN %in% c("Priv","Priv/Shore") &
  #                                  NEW_STA == "AL" & WAVE %in% c(1,2,5,6), 2,                      ###    ...ALDNR -- Jun1 - Aug5, Oct 4-5
  #                          ifelse( YEAR == 2019 & NEW_MODEN %in% c("Priv","Priv/Shore") &
  #                                  NEW_STA == "AL" & WAVE %in% 3:4, 1,                             ###             -- weekends/holidays
  #                          ifelse( YEAR == 2019 & NEW_MODEN %in% c("Priv","Priv/Shore") &
  #                                  NEW_STA == "MS" & WAVE %in% c(1,2,5,6), 2,                      ###    ...MSDMR -- May24 - Jul8
  #                          ifelse( YEAR == 2019 & NEW_MODEN %in% c("Priv","Priv/Shore") &
  #                                  NEW_STA == "MS" & WAVE %in% 3:4, 1,                             ###             -- Jul28 - Sep2
  #                          ifelse( YEAR == 2019 & NEW_MODEN %in% c("Priv","Priv/Shore") &
  #                                  NEW_STA == "LA" & WAVE %in% 1:2, 2,                             ###     ...LDWF -- May24 - Sep3, Sep 27 - Nov 24 (weekends/holidays)
  #                          ifelse( YEAR == 2019 & NEW_MODEN %in% c("Priv","Priv/Shore") &
  #                                  NEW_STA == "LA" & WAVE %in% 3:6, 1,                             ###             -- Nov28 - Dec31
  #                          ifelse( YEAR == 2019 & NEW_MODEN %in% c("Priv","Priv/Shore") &
  #                                  NEW_STA == "TX" & WAVE %in% c(1,2,5,6), 2,                      ###     ...TPWD -- Jun1 - Aug2
  #                          ifelse( YEAR == 2019 & NEW_MODEN %in% c("Priv","Priv/Shore") &
  #                                  NEW_STA == "TX" & WAVE %in% 3:4, 1,
  #                          ifelse( YEAR == 2019 & NEW_MODEN == "Cbt" & WAVE %in% c(1,2,5,6), 2,      ### 2019 - Hire -- Jun1 - Aug2
  #                          ifelse( YEAR == 2019 & NEW_MODEN == "Cbt" & WAVE %in% 3:4, 1, fed_closed
  #                                  )))))))))))))))))))))))) )
  #   
  # }
  # 
  ### ---------------------------------------------------------------------------------------------------------
  ### SEDAR 98 -- Gulf of America RED SNAPPER ###
  ###
  ###       ...for which the S98 Management History table (on the S-drive) was used to identify those days
  ###         ( month-day-year ) over which a given year/mode/state-specific fishing season was open/closed:
  ###             0 = open
  ###             1 = closed
  ###       We then applied these designations to raw (trip-level) intercept data to quantify the relative catch
  ###       between open/closed fishing seasons, which were subsequently applied to partition our catch estimates.
  ###       This (day-level) approach was seen as an improvement over the previous wave-level assignments (in S74 ),
  ###       which required 'partially open' designations ( for data/estimates ) in waves that had seasons that
  ###       were 'equally' open & closed (e.g., open one month, closed other month ). These designations were
  ###       also problematic for assessment analysts, who had to apply some ad-hoc method to partition these
  ###       'partially open' data/estimates between 'open' vs. 'closed' seasons, which was done using %days-open
  ###       ( from the MH file ) and is feared to be inaccurate (e.g., if season was only open for 3-days, then
  ###         very little catch would be assigned to the 'open' season even if responsible for 99% of the catch )...
  ###
  ###       At the S98 Data Workshop, it was further decided to consider the state seasons for private fishing:
  ###             Private -- consider both the state & federal fishing seasons, in that we consider the season "open"
  ###               if it's open in either area. This is due to the admittance of anglers to fish where they want,
  ###               but simply claim activity in the area-fished that simply happened to be open, which was
  ###               identified and discussed by the RecWG. The group also highlighted that there was some initial
  ###               confusion in federal waters, with some states "claiming" waters out to 9 miles and so some anglers
  ###               believed they were permitted to fish in (what was really) federal waters during the closed season.
  ###             Charter -- only consider the federal fishing season, as "state charter" vessels make up a relatively
  ###               insignificant amount of catch ( relative to "federal charters", and other rec modes ).
  ###       The state seasons were identified by a MH file provided by SERO (Mike Larkin) at the DW:
  ###           ( file name = 'State_Fed_Seasons_Updated012021.xlsx' )...
  
  
  if( region == 'Gulf of America' & 'red snapper' %in% new.com ) {
    
    ###     ...for which month-day information is imported from the associated (trip-level) micro-data,
    ###       as is processed by the partition.fishing.season() function in this workspace...
    
    
    if( 'YEAR' %notin% colnames(genrec.table) ) {
      if( 'year' %in% colnames(genrec.table) ) {
        genrec.table = genrec.table %>% rename( YEAR = year )
      } else {
        rename.vec = c( YEAR = 'INT_YEAR', YEAR = 'int_year' )
        genrec.table = genrec.table %>% rename( any_of( rename.vec ) )
        rm( rename.vec )
      }
    }
    if( 'MONTH' %notin% colnames(genrec.table) ) {
      if( 'month' %in% colnames(genrec.table) ) {
        genrec.table = genrec.table %>% rename( MONTH = month )
      } else {
        rename.vec = c( MONTH = 'INT_MONTH', MONTH = 'int_month' )
        genrec.table = genrec.table %>% rename( any_of( rename.vec ) )
        rm( rename.vec )
      }
    }
    if( 'DAY' %notin% colnames(genrec.table) ) {
      if( 'day' %in% colnames(genrec.table) ) {
        genrec.table = genrec.table %>% rename( DAY = day )
      } else {
        rename.vec = c( DAY = 'INT_DAY', DAY = 'int_day' )
        genrec.table = genrec.table %>% rename( any_of( rename.vec ) )
        rm( rename.vec )
      }
    }
    
    
    if( 'date' %notin% colnames(genrec.table) ) {
      genrec.table = genrec.table %>% mutate( date = as.Date( paste0( YEAR,'-',MONTH,'-',DAY ) ) )
    }
    
    genrec.table = genrec.table %>%
      mutate( fed_closed = 1 ) %>%
      ###   ...if dates don't fall into any of the 'open' seasons listed below, which set fed_closed = 0,
      ###       the default 'closed' setting is used (i.e., fed_closed = 1 )...
      
      ### 1981-2014
      mutate( fed_closed = ifelse( YEAR <= 1996, 0,                                                      ### Before 1997, seasons open year-round
                                   
                           ifelse( date >= as.Date('1997-01-01') & date <= as.Date('2014-12-31') &       ### Texas "state" seasons were year-round
                                   NEW_STA == 'TX' &                                                     ###        in most years...
                                   NEW_MODEN %in% c('Priv','Shore','Priv/Shore'),                 0,
                                   
                           ifelse( date >= as.Date('1997-01-01') & date <= as.Date('1997-11-26') &       ### 1997 -- Jan01 - Nov26 -- federal
                                 ( NEW_MODEN == 'Cbt' | ( NEW_STA %in% c('FLW','AL','MS','LA') &
                                   NEW_MODEN %in% c('Priv','Shore','Priv/Shore') ) ),             0,
                           ifelse( date >= as.Date('1998-01-01') & date <= as.Date('1998-09-29') &       ### 1998 -- Jan01 - Sep29 -- federal
                                 ( NEW_MODEN == 'Cbt' | ( NEW_STA %in% c('FLW','AL','MS','LA') &
                                   NEW_MODEN %in% c('Priv','Shore','Priv/Shore') ) ),             0,
                           ifelse( date >= as.Date('1999-01-01') & date <= as.Date('1999-08-28') &       ### 1999 -- Jan01 - Aug28 -- federal
                                 ( NEW_MODEN == 'Cbt' | ( NEW_STA %in% c('FLW','AL','MS','LA') &
                                   NEW_MODEN %in% c('Priv','Shore','Priv/Shore') ) ),             0,
                                   
                           ifelse( date >= as.Date('2000-04-22') & date <= as.Date('2000-10-31') &       ### 2000 -- Apr22 - Oct31 -- federal
                                 ( NEW_MODEN %in% c('Cbt','Hbt','Cbt/Hbt') |
                                 ( NEW_STA %in% c('FLW','AL','MS','LA') &
                                   NEW_MODEN %in% c('Priv','Shore','Priv/Shore') ) ),             0,
                           ifelse( date >= as.Date('2001-04-21') & date <= as.Date('2001-10-31') &       ### 2001-2007 -- Apr21 - Oct31 -- federal
                                 ( NEW_MODEN %in% c('Cbt','Hbt','Cbt/Hbt') |
                                 ( NEW_STA %in% c('FLW','AL','MS','LA') &
                                   NEW_MODEN %in% c('Priv','Shore','Priv/Shore') ) ),             0,
                           ifelse( date >= as.Date('2002-04-21') & date <= as.Date('2002-10-31') &
                                 ( NEW_MODEN %in% c('Cbt','Hbt','Cbt/Hbt') |
                                 ( NEW_STA %in% c('FLW','AL','MS','LA') &
                                   NEW_MODEN %in% c('Priv','Shore','Priv/Shore') ) ),             0,
                           ifelse( date >= as.Date('2003-04-21') & date <= as.Date('2003-10-31') &
                                 ( NEW_MODEN %in% c('Cbt','Hbt','Cbt/Hbt') |
                                 ( NEW_STA %in% c('FLW','AL','MS','LA') &
                                   NEW_MODEN %in% c('Priv','Shore','Priv/Shore') ) ),             0,
                           ifelse( date >= as.Date('2004-04-21') & date <= as.Date('2004-10-31') &
                                 ( NEW_MODEN %in% c('Cbt','Hbt','Cbt/Hbt') |
                                 ( NEW_STA %in% c('FLW','AL','MS','LA') &
                                   NEW_MODEN %in% c('Priv','Shore','Priv/Shore') ) ),             0,
                           ifelse( date >= as.Date('2005-04-21') & date <= as.Date('2005-10-31') &
                                 ( NEW_MODEN %in% c('Cbt','Hbt','Cbt/Hbt') |
                                 ( NEW_STA %in% c('FLW','AL','MS','LA') &
                                   NEW_MODEN %in% c('Priv','Shore','Priv/Shore') ) ),             0,
                           ifelse( date >= as.Date('2006-04-21') & date <= as.Date('2006-10-31') &
                                 ( NEW_MODEN %in% c('Cbt','Hbt','Cbt/Hbt') |
                                 ( NEW_STA %in% c('FLW','AL','MS','LA') &
                                   NEW_MODEN %in% c('Priv','Shore','Priv/Shore') ) ),             0,
                           ifelse( date >= as.Date('2007-04-21') & date <= as.Date('2007-10-31') &
                                 ( NEW_MODEN %in% c('Cbt','Hbt','Cbt/Hbt') |
                                 ( NEW_STA %in% c('FLW','AL','MS','LA') &
                                   NEW_MODEN %in% c('Priv','Shore','Priv/Shore') ) ),             0,
                           ifelse( date >= as.Date('2007-04-15') & date <= as.Date('2007-04-20') &       ### 2007 -- FL "state" season started Apr15
                                   NEW_STA == 'FLW' &
                                   NEW_MODEN %in% c('Priv','Shore','Priv/Shore'),                 0,
                                   
                           ifelse( date >= as.Date('2008-06-01') & date <= as.Date('2008-08-04') &       ### 2008 -- Jun01 - Aug04 -- federal
                                 ( NEW_MODEN %in% c('Cbt','Hbt','Cbt/Hbt') |
                                 ( NEW_STA %in% c('FLW','AL','MS','LA') &
                                   NEW_MODEN %in% c('Priv','Shore','Priv/Shore') ) ),             0,
                           ifelse( date >= as.Date('2008-04-15') & date <= as.Date('2008-05-31') &       ### 2008 -- FL "state" season started Apr15
                                   NEW_STA == 'FLW' &
                                   NEW_MODEN %in% c('Priv','Shore','Priv/Shore'),                 0,
                           ifelse( date >= as.Date('2008-08-05') & date <= as.Date('2008-10-31') &       ### 2008 -- FL "state" season ended Oct31
                                   NEW_STA == 'FLW' &
                                   NEW_MODEN %in% c('Priv','Shore','Priv/Shore'),                 0,
                                   
                           ifelse( date >= as.Date('2009-06-01') & date <= as.Date('2009-08-14') &       ### 2009 -- Jun01 - Aug14 -- federal
                                 ( NEW_MODEN %in% c('Cbt','Hbt','Cbt/Hbt') |
                                 ( NEW_STA %in% c('FLW','AL','MS','LA') &
                                   NEW_MODEN %in% c('Priv','Shore','Priv/Shore') ) ),             0,
                           ifelse( date >= as.Date('2010-06-01') & date <= as.Date('2010-07-23') &       ### 2010 -- Jun01 - Jul23 -- federal
                                 ( NEW_MODEN %in% c('Cbt','Hbt','Cbt/Hbt') |
                                 ( NEW_STA %in% c('FLW','AL','MS','LA') &
                                   NEW_MODEN %in% c('Priv','Shore','Priv/Shore') ) ),             0,
                           ifelse( date >= as.Date('2010-10-01') & date <= as.Date('2010-11-21') &       ### 2010 -- Oct01 - Nov21 -- federal
                                   weekdays(date) %in% c('Friday','Saturday','Sunday') &                 ###  ...24-day fall season for weekends following DWH
                                 ( NEW_MODEN %in% c('Cbt','Hbt','Cbt/Hbt') |
                                 ( NEW_STA %in% c('FLW','AL','MS','LA') &
                                   NEW_MODEN %in% c('Priv','Shore','Priv/Shore') ) ),             0,
                           ifelse( date >= as.Date('2011-06-01') & date <= as.Date('2011-07-18') &       ### 2011 -- Jun01 - Jul18 -- federal
                                 ( NEW_MODEN %in% c('Cbt','Hbt','Cbt/Hbt') |
                                 ( NEW_STA %in% c('FLW','AL','MS','LA') &
                                   NEW_MODEN %in% c('Priv','Shore','Priv/Shore') ) ),             0,
                           ifelse( date >= as.Date('2012-06-01') & date <= as.Date('2012-07-16') &       ### 2012 -- Jun01 - Jul16 -- federal
                                 ( NEW_MODEN %in% c('Cbt','Hbt','Cbt/Hbt') |
                                 ( NEW_STA %in% c('FLW','AL','MS','LA') &
                                   NEW_MODEN %in% c('Priv','Shore','Priv/Shore') ) ),             0,
                                   
                           ifelse( date >= as.Date('2013-06-01') & date <= as.Date('2013-06-28') &       ### 2013 -- Jun01 - Jun28 -- federal
                                 ( NEW_MODEN %in% c('Cbt','Hbt','Cbt/Hbt') |
                                 ( NEW_STA %in% c('FLW','AL','MS','LA') &
                                   NEW_MODEN %in% c('Priv','Shore','Priv/Shore') ) ),             0,
                           ifelse( date >= as.Date('2013-06-29') & date <= as.Date('2013-07-14') &       ### 2013 -- FL "state" season ended Jul14
                                   NEW_STA == 'FLW' &
                                   NEW_MODEN %in% c('Priv','Shore','Priv/Shore'),                 0,
                           ifelse( date >= as.Date('2013-10-01') & date <= as.Date('2013-10-15') &       ### 2013 -- Oct01 - Oct15 -- federal
                                 ( NEW_MODEN %in% c('Cbt','Hbt','Cbt/Hbt') |
                                 ( NEW_STA %in% c('FLW','AL','MS','LA') &
                                   NEW_MODEN %in% c('Priv','Shore','Priv/Shore') ) ),             0,
                           ifelse( date >= as.Date('2013-10-16') & date <= as.Date('2013-10-21') &       ### 2013 -- FL "state" season ended Oct21
                                   NEW_STA == 'FLW' &
                                   NEW_MODEN %in% c('Priv','Shore','Priv/Shore'),                 0,
                           ifelse( date >= as.Date('2013-03-23') & date <= as.Date('2013-06-01') &       ### 2013 -- LA "state" season started Mar23
                                   NEW_STA == 'LA' & NEW_MODEN %in% c('Priv','Shore','Priv/Shore') &     ###     ...for weekends only
                                   weekdays(date) %in% c('Friday','Saturday','Sunday'),           0,
                           ifelse( date >= as.Date('2013-06-25') & date <= as.Date('2013-09-30') &       ### 2013 -- LA "state" season ended Sep30
                                   NEW_STA == 'LA' & NEW_MODEN %in% c('Priv','Shore','Priv/Shore') &     ###     ...for weekends only
                                   weekdays(date) %in% c('Friday','Saturday','Sunday'),           0,
                                   
                           ifelse( date >= as.Date('2014-06-01') & date <= as.Date('2014-06-09') &       ### 2014 -- Jun01 - Jun09 -- federal
                                 ( NEW_MODEN %in% c('Cbt','Hbt','Cbt/Hbt') |
                                 ( NEW_STA %in% c('FLW','AL','MS','LA') &
                                   NEW_MODEN %in% c('Priv','Shore','Priv/Shore') ) ),             0,
                           ifelse( date >= as.Date('2014-05-24') & date <= as.Date('2014-07-14') &       ### 2014 -- FL "state" season -- May24 - Jul14
                                   NEW_STA == 'FLW' &
                                   NEW_MODEN %in% c('Priv','Shore','Priv/Shore'),                 0,
                           ifelse( date >= as.Date('2014-07-04') & date <= as.Date('2014-07-27') &       ### 2014 -- AL "state" season -- Jul04 - Jul27
                                   NEW_STA == 'AL' & NEW_MODEN %in% c('Priv','Shore','Priv/Shore') &     ###    ...for weekends only
                                   weekdays(date) %in% c('Friday','Saturday','Sunday'),           0,
                           ifelse( date >= as.Date('2014-05-27') & date <= as.Date('2014-09-05') &       ### 2014 -- MS "state" season -- May27 - Sep05
                                   NEW_STA == 'MS' &
                                   NEW_MODEN %in% c('Priv','Shore','Priv/Shore'),                 0,
                           ifelse( date >= as.Date('2014-02-21') & date <= as.Date('2014-04-13') &       ### 2014 -- LA "state" season -- Feb21 - Apr13
                                   NEW_STA == 'LA' & NEW_MODEN %in% c('Priv','Shore','Priv/Shore') &     ###    ...for weekends only
                                   weekdays(date) %in% c('Friday','Saturday','Sunday'),           0,
                           ifelse( date >= as.Date('2014-04-14') & date <= as.Date('2014-12-31') &       ### 2014 -- LA "state" season -- Apr14 - Dec31
                                   NEW_STA == 'LA' &
                                   NEW_MODEN %in% c('Priv','Shore','Priv/Shore'),                 0,
                                   
                                   fed_closed )))))))))))))))))))))))))))))))))) ) %>%
      
      ### 2015-2017 -- separate seasons for private vs. charter
      mutate( fed_closed = ifelse( date >= as.Date('2015-01-01') & date <= as.Date('2016-12-31') &       ### Texas "state" seasons were year-round
                                   NEW_STA == 'TX' &                                                     ###        in most years...
                                   NEW_MODEN %in% c('Priv','Shore','Priv/Shore'),                 0,
                           ifelse( date >= as.Date('2017-01-01') & date <= as.Date('2017-06-15') &       ###    ...but in 2017, was only open weekends 
                                   NEW_STA == 'TX' &                                                     ###        for Jun16 - Sep04
                                   NEW_MODEN %in% c('Priv','Shore','Priv/Shore'),                 0,
                           ifelse( date >= as.Date('2017-06-16') & date <= as.Date('2017-09-04') &
                                   NEW_STA == 'TX' & NEW_MODEN %in% c('Priv','Shore','Priv/Shore') &
                                   weekdays(date) %in% c('Friday','Saturday','Sunday'),           0,
                           ifelse( date %in% c( as.Date('2017-07-03'),as.Date('2017-07-04') ) &
                                   NEW_STA == 'TX' &
                                   NEW_MODEN %in% c('Priv','Shore','Priv/Shore'),                 0,
                           ifelse( date >= as.Date('2017-09-05') & date <= as.Date('2017-12-31') &
                                   NEW_STA == 'TX' &
                                   NEW_MODEN %in% c('Priv','Shore','Priv/Shore'),                 0,
                                   
                                   fed_closed ))))) ) %>%
      
      mutate( fed_closed = ifelse( date >= as.Date('2015-06-01') & date <= as.Date('2015-07-14') &       ### 2015-Hire -- Jun01 - Jul14 -- federal
                                   NEW_MODEN %in% c("Cbt","Hbt","Cbt/Hbt"),                       0,
                           ifelse( date >= as.Date('2015-06-01') & date <= as.Date('2015-06-10') &       ### 2015-Priv -- Jun01 - Jun10 -- federal
                                   NEW_STA %in% c('FLW','AL','MS','LA') &
                                   NEW_MODEN %in% c('Priv','Shore','Priv/Shore'),                 0,
                           ifelse( date >= as.Date('2015-04-23') & date <= as.Date('2015-07-12') &       ### 2015 -- FL "state" season -- May23 - Jul12
                                   NEW_STA == 'FLW' &
                                   NEW_MODEN %in% c('Priv','Shore','Priv/Shore'),                 0,
                           ifelse( date >= as.Date('2015-09-01') & date <= as.Date('2015-11-01') &       ### 2015 -- FL "state" season -- Sep01 - Nov01
                                   weekdays(date) %in% c('Saturday','Sunday') &                          ###      ...opened for Sat/Sun trips
                                   NEW_STA == 'FLW' &
                                   NEW_MODEN %in% c('Priv','Shore','Priv/Shore'),                 0,
                           ifelse( date %in% c( as.Date('2015-09-07') ) & NEW_STA == 'FLW' &             ### 2015 -- FL "state" season -- also open Sep07
                                   NEW_MODEN %in% c('Priv','Shore','Priv/Shore'),                 0,
                           ifelse( date >= as.Date('2015-07-01') & date <= as.Date('2015-07-31') &       ### 2015 -- AL "state" season -- Jul01 - Jul31
                                   NEW_STA == 'AL' &
                                   NEW_MODEN %in% c('Priv','Shore','Priv/Shore'),                 0,
                           ifelse( date >= as.Date('2015-07-16') & date <= as.Date('2015-10-31') &       ### 2015 -- MS "state" season -- Jul16 - Oct31
                                   NEW_STA == 'MS' &
                                   NEW_MODEN %in% c('Priv','Shore','Priv/Shore'),                 0,
                           ifelse( date >= as.Date('2015-03-20') & date <= as.Date('2015-09-07') &       ### 2015 -- LA "state" season -- Mar20 - Sep07
                                   NEW_STA == 'LA' &
                                   NEW_MODEN %in% c('Priv','Shore','Priv/Shore'),                 0,
                           ifelse( date >= as.Date('2015-11-21') & date <= as.Date('2015-12-31') &       ### 2015 -- LA "state" season -- Nov21 - Dec31
                                   NEW_STA == 'LA' &
                                   NEW_MODEN %in% c('Priv','Shore','Priv/Shore'),                 0,
                                   
                                   fed_closed ))))))))) ) %>%
      
      mutate( fed_closed = ifelse( date >= as.Date('2016-06-01') & date <= as.Date('2016-07-16') &       ### 2016-Hire -- Jun01 - Jul16 -- federal
                                   NEW_MODEN %in% c("Cbt","Hbt","Cbt/Hbt"),                       0,
                           ifelse( date >= as.Date('2016-06-01') & date <= as.Date('2016-06-11') &       ### 2016-Priv -- Jun01 - Jun11 -- federal
                                   NEW_STA %in% c('FLW','AL','MS','LA') &
                                   NEW_MODEN %in% c('Priv','Shore','Priv/Shore'),                 0,
                           ifelse( date >= as.Date('2016-05-07') & date <= as.Date('2016-05-28') &       ### 2016 -- FL "state" season -- May07 - May28
                                   weekdays(date) %in% c('Saturday','Sunday') &                          ###      ...opened for Sat/Sun trips
                                   NEW_STA == 'FLW' &
                                   NEW_MODEN %in% c('Priv','Shore','Priv/Shore'),                 0,
                           ifelse( date >= as.Date('2016-05-28') & date <= as.Date('2016-07-10') &       ### 2016 -- FL "state" season -- May28 - Jul10
                                   NEW_STA == 'FLW' &
                                   NEW_MODEN %in% c('Priv','Shore','Priv/Shore'),                 0,
                           ifelse( date >= as.Date('2016-09-01') & date <= as.Date('2016-10-31') &       ### 2016 -- FL "state" season -- Sep01 - Oct
                                   weekdays(date) %in% c('Friday','Saturday','Sunday') &                 ###      ...opened for Fri/Sat/Sun trips
                                   NEW_STA == 'FLW' &
                                   NEW_MODEN %in% c('Priv','Shore','Priv/Shore'),                 0,
                           ifelse( date %in% c( as.Date('2016-09-05') ) & NEW_STA == 'FLW' &             ### 2016 -- FL "state" season -- also open Sep05
                                   NEW_MODEN %in% c('Priv','Shore','Priv/Shore'),                 0,
                           ifelse( date >= as.Date('2016-05-27') & date <= as.Date('2016-07-31') &       ### 2016 -- AL "state" season -- May27 - Jul31
                                   NEW_STA == 'AL' &
                                   NEW_MODEN %in% c('Priv','Shore','Priv/Shore'),                 0,
                           ifelse( date >= as.Date('2016-05-27') & date <= as.Date('2016-06-10') &       ### 2016 -- MS "state" season -- May27 - Jun10
                                   NEW_STA == 'MS' &
                                   NEW_MODEN %in% c('Priv','Shore','Priv/Shore'),                 0,
                           ifelse( date >= as.Date('2016-01-08') & date <= as.Date('2016-09-05') &       ### 2016 -- LA "state" season -- Jan08 - Sep05
                                   NEW_STA == 'LA' &
                                   NEW_MODEN %in% c('Priv','Shore','Priv/Shore'),                 0,
                           ifelse( date >= as.Date('2016-10-07') & date <= as.Date('2016-12-15') &       ### 2016 -- LA "state" season -- Oct07 - Dec15
                                   weekdays(date) %in% c('Friday','Saturday','Sunday') &                 ###      ...opened for Fri/Sat/Sun trips
                                   NEW_STA == 'LA' &
                                   NEW_MODEN %in% c('Priv','Shore','Priv/Shore'),                 0,
                                   
                                   fed_closed )))))))))) ) %>%
      
      mutate( fed_closed = ifelse( date >= as.Date('2017-06-01') & date <= as.Date('2017-07-19') &       ### 2017-Hire -- Jun01 - Jul19 -- federal
                                   NEW_MODEN %in% c("Cbt","Hbt","Cbt/Hbt"),                       0,
                           ifelse( date >= as.Date('2017-06-01') & date <= as.Date('2017-06-03') &       ### 2017-Priv -- Jun01 - Jun03 -- federal
                                   NEW_STA %in% c('FLW','AL','MS','LA') &
                                   NEW_MODEN %in% c("Priv","Shore","Priv/Shore"),                 0,
                           ifelse( date >= as.Date('2017-06-16') & date <= as.Date('2017-09-04') &       ### 2017-Priv -- Jun16 - Sep04 -- federal
                                   weekdays(date) %in% c('Friday','Saturday','Sunday') &                 ###   ...reopened for weekend trips
                                   NEW_STA %in% c('FLW','AL','MS','LA') &
                                   NEW_MODEN %in% c("Priv","Shore","Priv/Shore"),                 0,
                           ifelse( date %in% c( as.Date('2017-07-03'), as.Date('2017-07-04'),            ### 2017-Priv -- Jul03-Jul04, Sept04 -- federal
                                                as.Date('2017-09-04') ) &                                ###   ...opened for select dates
                                   NEW_STA %in% c('FLW','AL','MS','LA') &
                                   NEW_MODEN %in% c("Priv","Shore","Priv/Shore"),                 0,
                           ifelse( date >= as.Date('2017-05-06') & date <= as.Date('2017-05-27') &       ### 2017 -- FL "state" season -- May06 - May27
                                   weekdays(date) %in% c('Saturday','Sunday') &                          ###      ...opened for Sat/Sun trips
                                   NEW_STA == 'FLW' &
                                   NEW_MODEN %in% c('Priv','Shore','Priv/Shore'),                 0,
                           ifelse( date >= as.Date('2017-05-27') & date <= as.Date('2017-05-31') &       ### 2017 -- FL "state" season -- May27 - May31
                                   NEW_STA == 'FLW' &
                                   NEW_MODEN %in% c('Priv','Shore','Priv/Shore'),                 0,
                           ifelse( date >= as.Date('2017-05-26') & date <= as.Date('2017-05-31') &       ### 2017 -- AL "state" season -- May26 - May31
                                   NEW_STA == 'AL' &
                                   NEW_MODEN %in% c('Priv','Shore','Priv/Shore'),                 0,
                           ifelse( date >= as.Date('2017-02-01') & date <= as.Date('2017-06-15') &       ### 2017 -- LA "state" season -- Feb01 - Jun15
                                   NEW_STA == 'LA' &
                                   NEW_MODEN %in% c('Priv','Shore','Priv/Shore'),                 0,

                                   fed_closed )))))))) ) %>%
      
      ### 2018 -- first year with Exempted Fishing Permits (EFPs), allowing state to set their own seasons
      mutate( fed_closed = ifelse( NEW_MODEN %in% c("Priv","Shore","Priv/Shore") & NEW_STA == "FLW" &
                                   date >= as.Date('2018-06-11') & date <= as.Date('2018-07-20'), 0,     ### 2018-Priv-FWC -- Jun11 - Jul20
                           ifelse( NEW_MODEN %in% c("Priv","Shore","Priv/Shore") & NEW_STA == "AL" &
                                   weekdays(date) %in% c('Friday','Saturday','Sunday') &                 ### 2018-Priv-ADNR -- Jun01 - Jul22
                                   date >= as.Date('2018-06-01') & date <= as.Date('2018-07-22'), 0,     ###    ...largely open weekends/holidays
                           ifelse( NEW_MODEN %in% c("Priv","Shore","Priv/Shore") & NEW_STA == "AL" &     ### 2018-Priv-ADNR -- Jul01 - Jul08
                                   date >= as.Date('2018-07-01') & date <= as.Date('2018-07-08'), 0,     ###    ...open first week in July
                           ifelse( NEW_MODEN %in% c("Priv","Shore","Priv/Shore") & NEW_STA == "MS" &
                                   date >= as.Date('2018-05-25') & date <= as.Date('2018-07-08'), 0,     ### 2018-Priv-MDMR -- May25 - Jul08
                           ifelse( NEW_MODEN %in% c("Priv","Shore","Priv/Shore") & NEW_STA == "MS" &
                                   date >= as.Date('2018-07-24') & date <= as.Date('2018-08-17'), 0,     ### 2018-Priv-MDMR -- Jul24 - Aug17
                           ifelse( NEW_MODEN %in% c("Priv","Shore","Priv/Shore") & NEW_STA == "MS" &
                                   date >= as.Date('2018-09-01') & date <= as.Date('2018-09-02'), 0,     ### 2018-Priv-MDMR -- Sep01 - Sep02
                           ifelse( NEW_MODEN %in% c("Priv","Shore","Priv/Shore") & NEW_STA == "MS" &
                                   date >= as.Date('2018-09-14') & date <= as.Date('2018-09-16'), 0,     ### 2018-Priv-MDMR -- Sep14 - Sep16
                           ifelse( NEW_MODEN %in% c("Priv","Shore","Priv/Shore") & NEW_STA == "LA" &
                                   date >= as.Date('2018-05-25') & date <= as.Date('2018-07-08'), 0,     ### 2018-Priv-LDWF -- May25 - Jul08
                           ifelse( NEW_MODEN %in% c("Priv","Shore","Priv/Shore") & NEW_STA == "LA" &
                                   weekdays(date) %in% c('Friday','Saturday','Sunday') &                 ### 2018-Priv-LDWF -- Jul13 - Aug12
                                   date >= as.Date('2018-07-13') & date <= as.Date('2018-08-12'), 0,     ###    ...open for weekends only
                           ifelse( NEW_MODEN %in% c("Priv","Shore","Priv/Shore") & NEW_STA == "TX" &
                                   date >= as.Date('2018-06-01') & date <= as.Date('2018-08-21'), 0,     ### 2018-Priv-TPWD -- Jun01 - Aug21
                           ifelse( NEW_MODEN %in% c("Cbt","Hbt","Cbt/Hbt") &
                                   date >= as.Date('2018-06-01') & date <= as.Date('2018-07-21'), 0,     ### 2018-Hire -- Jun01 - Jul21
                                   fed_closed ))))))))))) ) %>%
      ### 2019
      mutate( fed_closed = ifelse( NEW_MODEN %in% c("Priv","Shore","Priv/Shore") & NEW_STA == "FLW" &
                                   date >= as.Date('2019-06-11') & date <= as.Date('2019-07-12'), 0,     ### 2019-Priv-FWC -- Jun11 - Jul12
                           ifelse( NEW_MODEN %in% c("Priv","Shore","Priv/Shore") & NEW_STA == "FLW" &
                                   date %in% c( as.Date('2019-10-12'), as.Date('2019-10-13'),            ### 2019-Priv-FWC -- Oct12 - Oct13
                                                as.Date('2019-10-19'), as.Date('2019-10-20'),            ### 2019-Priv-FWC -- Oct19 - Oct20
                                                as.Date('2019-10-26'), as.Date('2019-10-27'),            ### 2019-Priv-FWC -- Oct26 - Oct27
                                                as.Date('2019-11-02'), as.Date('2019-11-03') ),   0,     ### 2019-Priv-FWC -- Nov02 - Nov03
                           ifelse( NEW_MODEN %in% c("Priv","Shore","Priv/Shore") & NEW_STA == "AL" &
                                   weekdays(date) %in% c('Friday','Saturday','Sunday') &                 ### 2019-Priv-ADNR -- Jun01 - Aug05
                                   date >= as.Date('2019-06-01') & date <= as.Date('2019-08-05'), 0,     ###     ...largely open weekends/holidays
                           ifelse( NEW_MODEN %in% c("Priv","Shore","Priv/Shore") & NEW_STA == "AL" &
                                   date %in% c( as.Date('2019-07-04'),                                   ### 2019-Priv-ADNR -- Jul04
                                                as.Date('2019-08-01'), as.Date('2019-08-05'),            ### 2019-Priv-ADNR -- Aug01, Aug05
                                                as.Date('2019-08-31'), as.Date('2019-09-01') ),   0,     ### 2019-Priv-ADNR -- Aug31, Sep01
                           ifelse( NEW_MODEN %in% c("Priv","Shore","Priv/Shore") & NEW_STA == "AL" &
                                   weekdays(date) %in% c('Friday','Saturday','Sunday') &                 ### 2019-Priv-ADNR -- Oct04 - Oct06
                                   date >= as.Date('2019-10-04') & date <= as.Date('2019-10-06'), 0,     ###     ...last weekend/holiday open
                           ifelse( NEW_MODEN %in% c("Priv","Shore","Priv/Shore") & NEW_STA == "MS" &
                                   date >= as.Date('2019-05-24') & date <= as.Date('2019-07-07'), 0,     ### 2019-Priv-MDMR -- May24 - Jul07
                           ifelse( NEW_MODEN %in% c("Priv","Shore","Priv/Shore") & NEW_STA == "MS" &
                                   date >= as.Date('2019-07-29') & date <= as.Date('2019-08-25'), 0,     ### 2019-Priv-MDMR -- Jul29 - Aug25
                           ifelse( NEW_MODEN %in% c("Priv","Shore","Priv/Shore") & NEW_STA == "MS" &
                                   date >= as.Date('2019-08-31') & date <= as.Date('2019-09-02'), 0,     ### 2019-Priv-MDMR -- Aug31 - Sep02
                           ifelse( NEW_MODEN %in% c("Priv","Shore","Priv/Shore") & NEW_STA == "MS" &
                                   date >= as.Date('2019-09-06') & date <= as.Date('2019-09-08'), 0,     ### 2019-Priv-MDMR -- Sep06 - Sep08
                           ifelse( NEW_MODEN %in% c("Priv","Shore","Priv/Shore") & NEW_STA == "LA" &
                                   weekdays(date) %in% c('Friday','Saturday','Sunday') &                 ### 2019-Priv-LDWF -- May24 - Sep03
                                   date >= as.Date('2019-05-24') & date <= as.Date('2019-09-03'), 0,     ###     ...open for weekends only
                           ifelse( NEW_MODEN %in% c("Priv","Shore","Priv/Shore") & NEW_STA == "LA" &
                                   weekdays(date) %in% c('Friday','Saturday','Sunday') &                 ### 2019-Priv-LDWF -- Sep27 - Nov24
                                   date >= as.Date('2019-09-27') & date <= as.Date('2019-11-24'), 0,     ###     ...open for weekends only
                           ifelse( NEW_MODEN %in% c("Priv","Shore","Priv/Shore") & NEW_STA == "LA" &
                                   date >= as.Date('2019-11-28') & date <= as.Date('2019-12-31'), 0,     ### 2019-Priv-LDWF -- Nov28 - Dec31
                           ifelse( NEW_MODEN %in% c("Priv","Shore","Priv/Shore") & NEW_STA == "LA" &     ### 2019-Priv-LDWF -- holidays
                                   date %in% c( as.Date('2019-05-27'), as.Date('2019-07-04'),            ###    MemoDay (May27), July04, 
                                                as.Date('2019-09-02') ),                          0,     ###    LaborDay (Sept02)
                           ifelse( NEW_MODEN %in% c("Priv","Shore","Priv/Shore") & NEW_STA == "TX" &
                                   date >= as.Date('2019-06-01') & date <= as.Date('2019-08-01'), 0,     ### 2019-Priv-TPWD -- Jun01 - Aug01
                           ifelse( NEW_MODEN %in% c("Cbt","Hbt","Cbt/Hbt") &
                                   date >= as.Date('2019-06-01') & date <= as.Date('2019-08-01'), 0,     ### 2019-Hire -- Jun01 - Aug01
                                   fed_closed ))))))))))))))) ) %>%
      ### 2020
      mutate( fed_closed = ifelse( NEW_MODEN %in% c("Priv","Shore","Priv/Shore") & NEW_STA == "FLW" &
                                   date >= as.Date('2020-06-11') & date <= as.Date('2020-07-25'), 0,     ### 2020-Priv-FWC -- Jun11 - Jul25
                           ifelse( NEW_MODEN %in% c("Priv","Shore","Priv/Shore") & NEW_STA == "FLW" &
                                   date %in% c( as.Date('2020-10-17'), as.Date('2020-10-18'),            ### 2020-Priv-FWC -- Oct17 - Oct18
                                                as.Date('2020-10-24'), as.Date('2020-10-25'),            ### 2020-Priv-FWC -- Oct24 - Oct25
                                                as.Date('2020-10-31'), as.Date('2020-11-01'),            ### 2020-Priv-FWC -- Oct31 - Nov01
                                                as.Date('2020-11-22'), as.Date('2020-11-23'),            ### 2020-Priv-FWC -- Nov22 - Nov23
                                                as.Date('2020-11-27'), as.Date('2020-11-29') ),   0,     ### 2020-Priv-FWC -- Nov27, Nov29
                           ifelse( NEW_MODEN %in% c("Priv","Shore","Priv/Shore") & NEW_STA == "AL" &
                                   weekdays(date) %in% c('Friday','Saturday','Sunday','Monday') &        ### 2020-Priv-ADNR -- May22 - Jul03
                                   date >= as.Date('2020-05-22') & date <= as.Date('2020-07-03'), 0,     ###     ...four day weekends (Fri-Mon)
                           ifelse( NEW_MODEN %in% c("Priv","Shore","Priv/Shore") & NEW_STA == "AL" &
                                   weekdays(date) %in% c('Saturday','Sunday') &                          ### 2020-Priv-ADNR -- Oct10 - Dec06
                                   date >= as.Date('2020-10-10') & date <= as.Date('2020-12-06'), 0,     ###     ...two day weekends (Sat-Sun)
                           ifelse( NEW_MODEN %in% c("Priv","Shore","Priv/Shore") & NEW_STA == "AL" &
                                   date %in% c( as.Date('2020-10-12') ),                          0,     ### 2020-Priv-ADNR -- (Mon) Oct12
                           # ifelse( NEW_MODEN %in% c("Priv","Shore","Priv/Shore") & NEW_STA == "AL" &
                           #         date %in% c( as.Date('2020-10-10'), as.Date('2020-10-11'),            ### 2020-Priv-ADNR -- Oct10 - Oct12
                           #                      as.Date('2020-10-12'),
                           #                      as.Date('2020-10-17'), as.Date('2020-10-18'),            ### 2020-Priv-ADNR -- Oct17 - Oct18
                           #                      as.Date('2020-10-24'), as.Date('2020-10-25'),            ### 2020-Priv-ADNR -- Oct24 - Oct25
                           #                      as.Date('2020-10-31'), as.Date('2020-11-01'),            ### 2020-Priv-ADNR -- Oct31 - Nov01
                           #                      as.Date('2020-11-07'), as.Date('2020-11-08'),            ### 2020-Priv-ADNR -- Nov07 - Nov08
                           #                      as.Date('2020-11-14'), as.Date('2020-11-15'),            ### 2020-Priv-ADNR -- Nov14 - Nov15
                           #                      as.Date('2020-11-21'), as.Date('2020-11-22'),            ### 2020-Priv-ADNR -- Nov21 - Nov22
                           #                      as.Date('2020-11-28'), as.Date('2020-11-29'),            ### 2020-Priv-ADNR -- Nov28 - Nov29
                           #                      as.Date('2020-12-05'), as.Date('2020-12-06') ),   0,     ### 2020-Priv-ADNR -- Dec05 - Dec06
                           ifelse( NEW_MODEN %in% c("Priv","Shore","Priv/Shore") & NEW_STA == "MS" &
                                   date >= as.Date('2020-05-22') & date <= as.Date('2020-07-05'), 0,     ### 2020-Priv-MDMR -- May22 - Jul05
                           ifelse( NEW_MODEN %in% c("Priv","Shore","Priv/Shore") & NEW_STA == "MS" &
                                   date %in% c( as.Date('2020-09-05') ),                          0,     ### 2020-Priv-MDMR -- Sep05
                           ifelse( NEW_MODEN %in% c("Priv","Shore","Priv/Shore") & NEW_STA == "LA" &
                                   weekdays(date) %in% c('Friday','Saturday','Sunday') &                 ### 2020-Priv-LDWF -- May22 - Aug10
                                   date >= as.Date('2020-05-22') & date <= as.Date('2020-08-10'), 0,     ###     ...open for weekends only
                           ifelse( NEW_MODEN %in% c("Priv","Shore","Priv/Shore") & NEW_STA == "LA" &
                                   date >= as.Date('2020-09-04') & date <= as.Date('2020-09-07'), 0,     ### 2020-Priv-LDWF -- Sep04 - Sep07
                           ifelse( NEW_MODEN %in% c("Priv","Shore","Priv/Shore") & NEW_STA == "LA" &     ### 2020-Priv-LDWF -- holidays
                                   date %in% c( as.Date('2020-05-25'), as.Date('2020-07-04'),            ###    MemoDay (May25), July04, 
                                                as.Date('2020-09-07') ),                          0,     ###    LaborDay (Sept07)
                           ifelse( NEW_MODEN %in% c("Priv","Shore","Priv/Shore") & NEW_STA == "TX" &
                                   date >= as.Date('2020-06-01') & date <= as.Date('2020-08-02'), 0,     ### 2020-Priv-TPWD -- Jun01 - Aug02
                           ifelse( NEW_MODEN %in% c("Cbt","Hbt","Cbt/Hbt") &
                                   date >= as.Date('2020-06-01') & date <= as.Date('2020-08-01'), 0,     ### 2020-Hire -- Jun01 - Aug01
                                   fed_closed )))))))))))) ) %>%
      ### 2021
      mutate( fed_closed = ifelse( NEW_MODEN %in% c("Priv","Shore","Priv/Shore") & NEW_STA == "FLW" &
                                   date >= as.Date('2021-06-04') & date <= as.Date('2021-07-28'), 0,     ### 2021-Priv-FWC -- Jun04 - Jul28
                           ifelse( NEW_MODEN %in% c("Priv","Shore","Priv/Shore") & NEW_STA == "AL" &
                                   weekdays(date) %in% c('Friday','Saturday','Sunday','Monday') &        ### 2021-Priv-ADNR -- May28 - Dec27
                                   date >= as.Date('2021-05-28') & date <= as.Date('2021-12-27'), 0,     ###     ...four day weekends (Fri-Mon)
                           ifelse( NEW_MODEN %in% c("Priv","Shore","Priv/Shore") & NEW_STA == "MS" &
                                   date >= as.Date('2021-05-28') & date <= as.Date('2021-07-05'), 0,     ### 2021-Priv-MDMR -- May28 - Jul05
                           ifelse( NEW_MODEN %in% c("Priv","Shore","Priv/Shore") & NEW_STA == "MS" &
                                   date >= as.Date('2021-08-06') & date <= as.Date('2021-08-08'), 0,     ### 2021-Priv-MDMR -- Aug06 - Aug08
                           ifelse( NEW_MODEN %in% c("Priv","Shore","Priv/Shore") & NEW_STA == "MS" &
                                   date >= as.Date('2021-08-13') & date <= as.Date('2021-09-06'), 0,     ### 2021-Priv-MDMR -- Aug13 - Sep06
                           ifelse( NEW_MODEN %in% c("Priv","Shore","Priv/Shore") & NEW_STA == "MS" &
                                   date >= as.Date('2021-10-01') & date <= as.Date('2021-11-21'), 0,     ### 2021-Priv-MDMR -- Oct01 - Nov21
                           ifelse( NEW_MODEN %in% c("Priv","Shore","Priv/Shore") & NEW_STA == "LA" &
                                   weekdays(date) %in% c('Friday','Saturday','Sunday') &                 ### 2021-Priv-LDWF -- May28 - Sep06
                                   date >= as.Date('2021-05-28') & date <= as.Date('2021-09-06'), 0,     ###      ...open for weekends only
                           ifelse( NEW_MODEN %in% c("Priv","Shore","Priv/Shore") & NEW_STA == "LA" &
                                   date >= as.Date('2021-09-24') & date <= as.Date('2021-12-31'), 0,     ### 2021-Priv-LDWF -- Sep24 - Dec31
                           ifelse( NEW_MODEN %in% c("Priv","Shore","Priv/Shore") & NEW_STA == "LA" &     ### 2021-Priv-LDWF -- holidays
                                   date %in% c( as.Date('2021-05-31'), as.Date('2021-07-04'),            ###    MemoDay (May31), July04, 
                                                as.Date('2021-09-06') ),                          0,     ###    LaborDay (Sept06)
                           ifelse( NEW_MODEN %in% c("Priv","Shore","Priv/Shore") & NEW_STA == "TX" &
                                   date >= as.Date('2021-06-01') & date <= as.Date('2021-08-04'), 0,     ### 2021-Priv-TPWD -- Jun01 - Aug04
                           ifelse( NEW_MODEN %in% c("Cbt","Hbt","Cbt/Hbt") &
                                   date >= as.Date('2021-06-01') & date <= as.Date('2021-08-02'), 0,     ### 2021-Hire -- Jun01 - Aug02
                                   fed_closed ))))))))))) ) %>%
      ### 2022
      mutate( fed_closed = ifelse( NEW_MODEN %in% c("Priv","Shore","Priv/Shore") & NEW_STA == "FLW" &
                                   date >= as.Date('2022-06-17') & date <= as.Date('2022-07-31'), 0,     ### 2022-Priv-FWC -- Jun17 - Jul31
                           ifelse( NEW_MODEN %in% c("Priv","Shore","Priv/Shore") & NEW_STA == "FLW" &
                                   date %in% c( as.Date('2022-10-08'), as.Date('2022-10-09'),            ### 2022-Priv-FWC -- Oct08 - Oct09
                                                as.Date('2022-10-15'), as.Date('2022-10-16'),            ### 2022-Priv-FWC -- Oct15 - Oct16
                                                as.Date('2022-10-22'), as.Date('2022-10-23'),            ### 2022-Priv-FWC -- Oct22 - Oct23
                                                as.Date('2022-11-11'), as.Date('2022-11-12'),            ### 2022-Priv-FWC -- Nov11 - Nov12
                                                as.Date('2022-11-13'), as.Date('2022-11-25'),            ### 2022-Priv-FWC -- Nov13, Nov25
                                                as.Date('2022-11-26'), as.Date('2022-11-27') ),   0,     ### 2022-Priv-FWC -- Nov26 - Nov27
                           ifelse( NEW_MODEN %in% c("Priv","Shore","Priv/Shore") & NEW_STA == "AL" &
                                   weekdays(date) %in% c('Friday','Saturday','Sunday','Monday') &        ### 2022-Priv-ADNR -- May27 - Dec31
                                   date >= as.Date('2022-05-27') & date <= as.Date('2022-12-31'), 0,     ###     ...four-day weekends (Fri-Mon)
                           ifelse( NEW_MODEN %in% c("Priv","Shore","Priv/Shore") & NEW_STA == "MS" &
                                   date >= as.Date('2022-05-27') & date <= as.Date('2022-07-10'), 0,     ### 2022-Priv-MDMR -- May27 - Jul10
                           ifelse( NEW_MODEN %in% c("Priv","Shore","Priv/Shore") & NEW_STA == "MS" &
                                   date >= as.Date('2022-08-07') & date <= as.Date('2022-12-31'), 0,     ### 2022-Priv-MDMR -- Aug07 - Dec31
                           ifelse( NEW_MODEN %in% c("Priv","Shore","Priv/Shore") & NEW_STA == "LA" &
                                   weekdays(date) %in% c('Friday','Saturday','Sunday') &                 ### 2022-Priv-LDWF -- May27 - Sep18
                                   date >= as.Date('2022-05-27') & date <= as.Date('2022-09-18'), 0,     ###     ...open for weekends only
                           ifelse( NEW_MODEN %in% c("Priv","Shore","Priv/Shore") & NEW_STA == "LA" &
                                   date >= as.Date('2022-10-07') & date <= as.Date('2022-10-14'), 0,     ### 2022-Priv-LDWF -- Oct07 - Oct14
                           ifelse( NEW_MODEN %in% c("Priv","Shore","Priv/Shore") & NEW_STA == "LA" &     ### 2022-Priv-LDWF -- holidays
                                   date %in% c( as.Date('2022-05-30'), as.Date('2022-07-04'),            ###    MemoDay (May30), July04, 
                                                as.Date('2022-09-05') ),                          0,     ###    LaborDay (Sept05)
                           ifelse( NEW_MODEN %in% c("Priv","Shore","Priv/Shore") & NEW_STA == "TX" &
                                   date >= as.Date('2022-06-01') & date <= as.Date('2022-09-02'), 0,     ### 2022-Priv-TPWD -- Jun01 - Sep02
                           ifelse( NEW_MODEN %in% c("Cbt","Hbt","Cbt/Hbt") &
                                   date >= as.Date('2022-06-01') & date <= as.Date('2022-08-18'), 0,     ### 2022-Hire -- Jun01 - Aug18
                                   fed_closed )))))))))) ) %>%
      ### 2023
      mutate( fed_closed = ifelse( NEW_MODEN %in% c("Priv","Shore","Priv/Shore") & NEW_STA == "FLW" &
                                   date >= as.Date('2023-06-16') & date <= as.Date('2023-07-31'), 0,     ### 2023-Priv-FWC -- Jun16 - Jul31
                           ifelse( NEW_MODEN %in% c("Priv","Shore","Priv/Shore") & NEW_STA == "FLW" &
                                   date %in% c( as.Date('2023-09-01'), as.Date('2023-09-02'),            ### 2023-Priv-FWC -- Sep01 - Sep04
                                                as.Date('2023-09-03'), as.Date('2023-09-04'),
                                                as.Date('2023-09-08'), as.Date('2023-09-09'),            ### 2023-Priv-FWC -- Sep08 - Sep10
                                                as.Date('2023-09-10'),
                                                as.Date('2023-09-15'), as.Date('2023-09-16'),            ### 2023-Priv-FWC -- Sep15 - Sep17
                                                as.Date('2023-09-17'),
                                                as.Date('2023-09-22'), as.Date('2023-09-23'),            ### 2023-Priv-FWC -- Sep22 - Sep24
                                                as.Date('2023-09-24'),
                                                as.Date('2023-09-29'), as.Date('2023-09-30'),            ### 2023-Priv-FWC -- Sep29 - Oct01
                                                as.Date('2023-10-01'),
                                                as.Date('2023-10-06'), as.Date('2023-10-07'),            ### 2023-Priv-FWC -- Oct06 - Oct08
                                                as.Date('2023-10-08'),
                                                as.Date('2023-10-13'), as.Date('2023-10-14'),            ### 2023-Priv-FWC -- Oct13 - Oct15
                                                as.Date('2023-10-15'),
                                                as.Date('2023-10-20'), as.Date('2023-10-21'),            ### 2023-Priv-FWC -- Oct20 - Oct22
                                                as.Date('2023-10-22'),
                                                as.Date('2023-10-27'), as.Date('2023-10-28'),            ### 2023-Priv-FWC -- Oct27 - Oct29
                                                as.Date('2023-10-29'),
                                                as.Date('2023-11-03'), as.Date('2023-11-04'),            ### 2023-Priv-FWC -- Nov03 - Nov05
                                                as.Date('2023-11-05'),
                                                as.Date('2023-11-10'), as.Date('2023-11-11'),            ### 2023-Priv-FWC -- Nov10 - Nov12
                                                as.Date('2023-11-12'),
                                                as.Date('2023-11-17'), as.Date('2023-11-18'),            ### 2023-Priv-FWC -- Nov17 - Nov19
                                                as.Date('2023-11-19'),
                                                as.Date('2023-11-23'), as.Date('2023-11-24'),            ### 2023-Priv-FWC -- Nov23 - Nov26
                                                as.Date('2023-11-25'), as.Date('2023-11-26') ),   0,
                           ifelse( NEW_MODEN %in% c("Priv","Shore","Priv/Shore") & NEW_STA == "AL" &
                                   weekdays(date) %in% c('Friday','Saturday','Sunday','Monday') &        ### 2023-Priv-ADNR -- May26 - Sep04
                                   date >= as.Date('2023-05-26') & date <= as.Date('2023-09-04'), 0,     ###     ...four day weekends (Fri-Mon)
                           ifelse( NEW_MODEN %in% c("Priv","Shore","Priv/Shore") & NEW_STA == "AL" &
                                   weekdays(date) %in% c('Friday','Saturday','Sunday','Monday') &        ### 2023-Priv-ADNR -- Sep29 - Oct16
                                   date >= as.Date('2023-09-29') & date <= as.Date('2023-10-16'), 0,     ###     ...four day weekends (Fri-Mon)
                           ifelse( NEW_MODEN %in% c("Priv","Shore","Priv/Shore") & NEW_STA == "MS" &
                                   date >= as.Date('2023-05-26') & date <= as.Date('2023-07-07'), 0,     ### 2023-Priv-MDMR -- May26 - Jul07
                           ifelse( NEW_MODEN %in% c("Priv","Shore","Priv/Shore") & NEW_STA == "MS" &
                                   date >= as.Date('2023-09-01') & date <= as.Date('2023-09-30'), 0,     ### 2023-Priv-MDMR -- Sep01 - Sep30
                           ifelse( NEW_MODEN %in% c("Priv","Shore","Priv/Shore") & NEW_STA == "LA" &
                                   date >= as.Date('2023-05-26') & date <= as.Date('2023-12-31'), 0,     ### 2023-Priv-LDWF -- May26 - Dec31
                           ifelse( NEW_MODEN %in% c("Priv","Shore","Priv/Shore") & NEW_STA == "TX" &
                                   date >= as.Date('2023-06-01') & date <= as.Date('2023-09-01'), 0,     ### 2023-Priv-TPWD -- Jun01 - Sep01
                           ifelse( NEW_MODEN %in% c("Cbt","Hbt","Cbt/Hbt") &
                                   date >= as.Date('2023-06-01') & date <= as.Date('2023-08-24'), 0,     ### 2023-Hire -- Jun01 - Aug24
                                   fed_closed ))))))))) )
    
  }

  ### ---------------------------------------------------------------------------------------------------------
  
  return( genrec.table )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------


partition.fishing.season = function( new.com, region, genrec.table,
                                     method.LACR.B2 = 'None', ratio.yrs.LACR.B2 = 'None',
                                     method.TPWD.B2 = 'None' ) {
  ###     ...where 'new.com' and 'region' identify the stock that is being assessed, as is required by the
  ###               assign.fishing.season() function imported from assign.FishingSeason.R...
  ###          'genrec.table' is the table of wave-level catch estimates that are to be partitioned between
  ###               open & closed fishing seasons ( saved as a 'fed_closed' field )...
  ###          and 'method.LACR.B2', 'method.TPWD.B2', and 'ratios.yrs.LACR.B2' identifies the methods used
  ###               (if any) to impute discards for the LACR & TPWD surveys, as needed to determine what
  ###               intercept data should be applied in partitioning catch b/w open vs. closed seasons...
  
  
  con = dbConnect(dbDriver("Oracle"), username = keyring::key_list("SECPR")[1,2],
                  password = keyring::key_get("SECPR", keyring::key_list("SECPR")[1,2]), dbname = "SECPR")
  
  st_tab   = dbGetQuery( con, "SELECT * FROM RDI.MRIP_STATE_CODES@secapxdv_dblk.sfsc.noaa.gov" )
  mode_tab = dbGetQuery( con, "SELECT * FROM RDI.MRIP_MODE_CODES@secapxdv_dblk.sfsc.noaa.gov" )
  area_tab = dbGetQuery( con, "SELECT * FROM RDI.MRIP_AREA_CODES@secapxdv_dblk.sfsc.noaa.gov" )
  cnty_tab = dbGetQuery( con, "SELECT * FROM RDI.RDI_CNTY_EXT_CODES@secapxdv_dblk.sfsc.noaa.gov" ) %>%
    mutate( NEW_ST = as.integer(STATE_CODE),
            CNTY   = as.integer(COUNTY_CODE),
            NEW_CNTY  = as.integer(NEW_COUNTY_CODE),
            NEW_CNTYN = as.character(NEW_COUNTY_NAME) ) %>%
    arrange( NEW_ST, NEW_CNTY, CNTY ) %>% select( NEW_ST, CNTY, NEW_CNTY, NEW_CNTYN, FL_REG, NC_REG )
  
  
  ###   Because 'genrec.table' can be composed of multiple datasources (e.g., MRIP, TPWD, LACR ),
  ###   each with its own set of microdata, the script below is divided into multiple if() statements,
  ###   each coded to import/modify a unique table of trip-level catches to estimate the relative contribution
  ###   ( to landings & discards ) of fishing during open vs. closed seasons...
  
  nodc.code = unique( genrec.table$SP_CODE )
  
  first.year = min( genrec.table$YEAR, na.rm=TRUE )
  term.year  = max( genrec.table$YEAR, na.rm=TRUE )
  
  
  ####################
  ###     MRIP     ###
  ####################
  
  if( 'MRIP' %in% unique(genrec.table$DS) ) {
    
    
    ### Import & Filters ###
    ### --------------------
    
    reg.mrip = unique( genrec.table$SUB_REG[ genrec.table$DS == 'MRIP' ] )
    
    states.mrip = unique( genrec.table$NEW_STA[ genrec.table$DS == 'MRIP' ] )
    states.mrip = gsub( "TX", 48, states.mrip )
    states.mrip = gsub( "LA", 22, states.mrip )
    states.mrip = gsub( "MS", 28, states.mrip )
    states.mrip = gsub( "AL",  1, states.mrip )
    states.mrip = gsub( "FLW",12, states.mrip )
    states.mrip = gsub( "FLE",12, states.mrip )
    states.mrip = gsub( "GA", 13, states.mrip )
    states.mrip = gsub( "SC", 45, states.mrip )
    states.mrip = gsub( "NC", 37, states.mrip )
    states.mrip = gsub( "VA", 51, states.mrip )
    states.mrip = gsub( "MD", 24, states.mrip )
    states.mrip = gsub( "DE", 10, states.mrip )
    states.mrip = gsub( "PN", 42, states.mrip )
    states.mrip = gsub( "NJ", 34, states.mrip )
    states.mrip = gsub( "NY", 36, states.mrip )
    states.mrip = gsub( "CT",  9, states.mrip )
    states.mrip = gsub( "RI", 44, states.mrip )
    states.mrip = gsub( "MA", 25, states.mrip )
    states.mrip = gsub( "NH", 33, states.mrip )
    states.mrip = gsub( "ME", 23, states.mrip )
    
    modes.mrip = unique( genrec.table$NEW_MODEN[ genrec.table$DS == 'MRIP' ] )
    if( "Hbt" %in% modes.mrip | "Cbt" %in% modes.mrip ) {
      modes.mrip = c( 6, modes.mrip )     ###   ...where MODE_FX = 6 is reserved for combined 'CbtHbt'...
    }
    modes.mrip = gsub( "Hbt",  4, modes.mrip )
    modes.mrip = gsub( "Cbt",  5, modes.mrip )
    modes.mrip = gsub( "Priv", 7, modes.mrip )
    if( "Shore" %in% modes.mrip ) {
      modes.mrip = modes.mrip[ !( modes.mrip == "Shore" ) ]
      modes.mrip = c( 1:3, modes.mrip )
    }
    
    con = dbConnect(dbDriver("Oracle"), username = keyring::key_list("SECPR")[1,2],
                    password = keyring::key_get("SECPR", keyring::key_list("SECPR")[1,2]), dbname = "SECPR")
    
    raw.mrip.catch = dbGetQuery(con,
                                paste0("select *
                          from RDI.MRIP_ST_PUB_CATCH_CAL@secapxdv_dblk.sfsc.noaa.gov c
                          FULL JOIN RDI.MRIP_ST_PUB_TRIP_CAL@secapxdv_dblk.sfsc.noaa.gov t
                                       ON t.ID_CODE = c.ID_CODE AND t.YEAR = c.YEAR
                               where ",paste0( "c.SP_CODE = ",sprintf( "'%s'", nodc.code ), collapse=" OR " ),
                                       " AND c.SUB_REG IN (", sprintf("'%s'", paste(reg.mrip, collapse = "','")),")",
                                       " AND c.ST IN (", sprintf("'%s'", paste(states.mrip, collapse = "','")),")",
                                       " AND c.YEAR BETWEEN ",first.year," AND ",term.year,
                                       " AND c.MODE_FX IN (", sprintf("'%s'", paste(modes.mrip, collapse = "','")),")"
                                ))
    ###   ...which includes a join with MRIP trip data to identify FINS county codes and define FL_REG & NC_REG...
    
    
    ### Value-Added Fields ###
    ### ----------------------
    
    if( 'DAY' %notin% colnames(raw.mrip.catch) ) {
      raw.mrip.catch = cbind( raw.mrip.catch,
                              data.frame( DAY = substr( raw.mrip.catch$ID_CODE, 12,13 ) ) )
    }
    
    raw.mrip.catch = raw.mrip.catch %>%
      setNames( make.names( names(.), unique = TRUE ) ) %>%
      
      ### TEMPORAL ###
      mutate_at( vars( YEAR, MONTH, DAY, WAVE, ST, SUB_REG, MODE_FX, AREA_X ), list( ~ as.integer(.) ) ) %>%
      mutate( date = as.Date( paste0( MONTH,"/",DAY,"/",YEAR ), "%m/%d/%Y" ) ) %>%
      
      ### SPATIAL ###
      left_join( st_tab, by = "ST" ) %>%
      mutate( NEW_ST = ifelse( ST == 12 & SUB_REG == 7, 5,
                       ifelse( ST == 12 & SUB_REG == 6, 6, NEW_ST ) ),
              NEW_STA = ifelse( NEW_ST == 5, "FLW",
                        ifelse( NEW_ST == 6, "FLE", NEW_STA ) ) ) %>%
      select( -any_of( c("FL_REG","NC_REG") ) ) %>%
      left_join( cnty_tab, by = c("NEW_ST","CNTY") ) %>%
      # mutate( FL_REG = ifelse(ST == 12 & CNTY %in% c(33, 113, 91, 131, 5, 133, 59, 45, 13, 63, 37, 77, 39, 129,
      #                                                73, 65, 123, 79, 29),                                           1,
      #                  ifelse(ST == 12 & CNTY %in% c(67, 121, 47, 75, 41, 1, 125, 23, 83, 17, 119, 53, 69,
      #                                                101, 105, 103, 57, 81, 49, 55, 115, 27, 15, 43, 71, 51, 21),    2,
      #                  ifelse(ST == 12 & CNTY == 87,                                                                 3,
      #                  ifelse(ST == 12 & CNTY %in% c(25, 11, 99, 85, 111, 93, 61, 86),                               4,
      #                  ifelse(ST == 12 & CNTY %in% c(97, 9, 95, 117, 127, 35, 107, 109, 19, 7, 31, 89, 3),           5, NA ))))),
      #         NC_REG = ifelse(ST == 37 & CNTY %in% c(15, 29, 41, 53, 55, 139, 143, 177, 187),                        "N",
      #                  ifelse(ST == 37 & CNTY %in% c(13, 19, 31, 49, 95, 129, 133, 137, 141, 147),                   "S", NA )) ) %>%
      
      ### MODE ###
    left_join( mode_tab, by = "MODE_FX" ) %>%
      # mutate( NEW_MODE  = ifelse( MODE_F %in% 1:5, 1,           ifelse( MODE_F == 6, 2,
      #                     ifelse( MODE_F == 7, 3,               ifelse( MODE_F == 8, 4, NA )))),
      #         NEW_MODEN = ifelse( MODE_F %in% 1:5, "Shore",     ifelse( MODE_F == 6, "Hbt",
      #                     ifelse( MODE_F == 7, "Cbt",           ifelse( MODE_F == 8, "Priv", NA )))) ) %>%
      # rename( DS_MODE  = MODE_F ) %>%
      # mutate( DS_MODEN = ifelse( DS_MODE == 1, "Pier, dock",        ifelse( DS_MODE == 2, "Jetty, breakwater, breachway",
      #                    ifelse( DS_MODE == 3, "Bridge, causeway",  ifelse( DS_MODE == 4, "Other man-made",
      #                    ifelse( DS_MODE == 5, "Beach or bank",     ifelse( DS_MODE == 6, "Head boat",
      #                    ifelse( DS_MODE == 7, "Charter boat",      ifelse( DS_MODE == 8, "Private/Rental boat", NA )))))))) ) %>%
      
      ### AREA-FISHED ###
    mutate( AREA_X = ifelse( AREA_X == "U", 6, AREA_X ) ) %>%
      left_join( area_tab, by = "AREA_X" ) %>%
      # rename( DS_AREA  = AREA_X ) %>%
      # mutate( DS_AREAN = ifelse( DS_AREA == 1, "Ocean <= 3 mi (all but WFL)",
      #                    ifelse( DS_AREA == 2, "Ocean > 3 mi (all but WFL)",
      #                    ifelse( DS_AREA == 3, "Ocean <= 10 mi (WFL only)",
      #                    ifelse( DS_AREA == 4, "Ocean > 10 mi (WFL only)",
      #                    ifelse( DS_AREA == 5, "Inland", NA ))))) ) %>%
      
      mutate( A   = CLAIM * WP_CATCH,
              B1  = HARVEST * WP_CATCH,
              B2  = RELEASE * WP_CATCH,
              AB1 = LANDING * WP_CATCH )
    
    ### Open/Closed Season ###
    raw.mrip.catch = assign.fishing.season( new.com = new.com, region = region, genrec.table = raw.mrip.catch )
    
    
    ###   ...and a final filter for FL_REG & NC_REG, which were added above ( as value-added fields )
    ###       and so couldn't be applied as filters in the original data pull...
    raw.mrip.catch = raw.mrip.catch %>%
      filter( FL_REG %in% unique(genrec.table$FL_REG) ) %>%
      filter( NC_REG %in% unique(genrec.table$NC_REG) ) %>%
      mutate_at( vars( FL_REG, NC_REG ), list( ~ as.character(.) ) )
    
    
    ### %Catch by Fishing Season for **ACTUAL** MRIP Estimates ###
    ### ----------------------------------------------------------
    ###     ...for which (trip-level) microdata is available for the same set of strata at which
    ###       the estimate is meant to represent (i.e., all intercept data from 2000-wave5 being used
    ###       to partition an estimate for 2000-wave5 )...
    
    dummy.table = raw.mrip.catch %>%
      group_by( YEAR, WAVE, SUB_REG, NEW_STA, FL_REG, NC_REG, NEW_MODEN, NEW_AREAN, fed_closed ) %>%
      summarize( AB1 = sum( AB1, na.rm=TRUE ),
                 B2  = sum(  B2, na.rm=TRUE ) ) %>%
      ungroup() %>%
      
      pivot_longer( cols = c('AB1','B2'), names_to = 'CATCH_VAR', values_to = 'value' ) %>%
      mutate( fed_closed = ifelse( fed_closed == 0, 'open',
                           ifelse( fed_closed == 1, 'closed', NA )) ) %>%
      pivot_wider( names_from = fed_closed, values_from = value )
    
    ###   ...and as a check that both an 'open' and 'closed' column have been created ( =0 if not )...
    cols = c( open=0, closed=0 )
    dummy.table = add_column( dummy.table, !!!cols[ setdiff( names(cols),names(dummy.table) ) ] )
    rm( cols )
    
    dummy.table = dummy.table %>%
      mutate(   open = ifelse( is.na(  open), 0,   open ),
              closed = ifelse( is.na(closed), 0, closed ) ) %>%
      filter( (open+closed) > 0 ) %>%
      mutate( p.open   =   open / (open+closed),
              p.closed = closed / (open+closed) ) %>%
      
      select( YEAR, WAVE, SUB_REG, NEW_STA, FL_REG, NC_REG, NEW_MODEN, NEW_AREAN, CATCH_VAR, p.open, p.closed ) %>%
      pivot_wider( names_from = CATCH_VAR, values_from = c('p.open','p.closed') ) %>%
      mutate( p.open_AB1 = ifelse( is.na(p.open_AB1), 0, p.open_AB1 ),
              p.open_B2  = ifelse( is.na(p.open_B2 ), 0, p.open_B2  ),
              p.closed_AB1 = ifelse( is.na(p.closed_AB1), 0, p.closed_AB1 ),
              p.closed_B2  = ifelse( is.na(p.closed_B2 ), 0, p.closed_B2  ) ) %>%
      
      ###   ...and to match the formatting (of FL_REG & NC_REG) in 'genrec.table'...
      mutate( FL_REG = ifelse( is.na(FL_REG), "", FL_REG ),
              NC_REG = ifelse( is.na(NC_REG), "", NC_REG ) )
    
    
    ### %Catch by Fishing Season for **PARTITIONED** ForHire Estimates ###
    ### ------------------------------------------------------------------
    ###     ...for which the %catches calculated above for any combined 'CbtHbt' records are duplicated, such that
    ###       the same partitioning factor is applied to any individual 'Cbt' & 'Hbt' estimates calculated from it
    ###       ( to account for any partitioning of combined forhire that may have already been done in 'genrec.table' )...
    
    blah1 = dummy.table %>%
      filter( NEW_MODEN == 'Cbt/Hbt' ) %>%
      uncount(3)
    blah1 = blah1 %>%
      mutate( NEW_MODEN = rep( c('Cbt/Hbt','Hbt','Cbt'), times = (dim(blah1)[1])/3 ) )
    
    blah2 = dummy.table %>% filter( NEW_MODEN != 'Cbt/Hbt' )
    
    dummy.table = bind_rows( blah1, blah2 ) %>%
      arrange( YEAR, WAVE, SUB_REG, NEW_STA, FL_REG, NC_REG, NEW_MODEN, NEW_AREAN )
    rm( blah1, blah2 )
    
    
    ### JOIN %catches with 'genrec.table' ###
    ### -------------------------------------
    
    mrip.table = genrec.table %>%
      filter( DS == 'MRIP' ) %>%
      ###   ...and to ensure consistent formatting...
      mutate( FL_REG = ifelse( is.na(FL_REG), "", FL_REG ),
              NC_REG = ifelse( is.na(NC_REG), "", NC_REG ) ) %>%
      left_join( dummy.table, by = c( 'YEAR','WAVE','SUB_REG','NEW_STA','FL_REG','NC_REG','NEW_MODEN','NEW_AREAN' ) )
    
    ###   ...and saving a copy of the partitioning factors that have been estimated for MRIP...
    mrip.part.factors = dummy.table %>% mutate( DS = 'MRIP', SOURCE = 'EST' )
    rm( dummy.table )
    
    ###   ...and for completion, I assign %catch = 0 for any strata for which catch records did not exist
    ###     in the microdata to calculate a %catch. As examples, this includes any imputations of 1981-wave1 data
    ###     in the MRIP survey ( which are accounted for below ), but also any 'zero-catch' records that OST added
    ###     to the microdata to ensure effort estimates are properly calculated. Setting the default for these
    ###     percentages to zero ( vs. <NA> ) mitigates the chances of <NA> values being present in the final catch table
    ###     (i.e., AB1 * 0 = 0   vs.   AB1 * <NA> = <NA> )...
    
    mrip.table = mrip.table %>%
      mutate(   p.open_AB1 = ifelse( AB1==0 & B2==0 & is.na(  p.open_AB1), 0,   p.open_AB1 ),
              p.closed_AB1 = ifelse( AB1==0 & B2==0 & is.na(p.closed_AB1), 0, p.closed_AB1 ),
                p.open_B2  = ifelse( AB1==0 & B2==0 & is.na(  p.open_B2 ), 0,   p.open_B2  ),
              p.closed_B2  = ifelse( AB1==0 & B2==0 & is.na(p.closed_B2 ), 0, p.closed_B2  ) )
    
    
    ### %Catch by Fishing Season for **IMPUTED** MRIP Estimates ###
    ### -----------------------------------------------------------
    ###     ...for which the code below calculates an appropriate partitioning factor (i.e., %catch open vs. closed )
    ###       for any 1981-wave1 estimates imputed for the MRIP timeseries. In particular, there is obviously no
    ###       MRIP microdata associated with an imputed estimate ( which is why its imputed in the first place ),
    ###       so partitioning factors (instead) are calculated from the same data used to impute the estimate.
    ###   For this example, all wave1 data in subsequent years (1982-1984) are used to estimate a %catch for any
    ###       1981-wave1 imputations (i.e., from those states where the imputations were estimated, TX-FLE ).
    ###       Similarly, the stratification used in these calculations matches that used in the imputation
    ###       (e.g., 'best practice' approach for 1981-wave1 is to calculate percentages ( wave1 vs. others )
    ###         by mode and area-fished )...
    
    if( dim( genrec.table[ genrec.table$DS == 'MRIP' &
                           genrec.table$YEAR == 1981 & genrec.table$WAVE == 1, ] )[1] > 0 ) {
      
      dummy.table = raw.mrip.catch %>%
        filter( YEAR %in% 1982:1984 & WAVE == 1 ) %>%
        filter( NEW_STA %in% c("TX","LA","MS","AL","FLW","FLE") ) %>%
        
        # group_by( SUB_REG, NEW_STA, FL_REG, NC_REG, NEW_MODEN, NEW_AREAN, fed_closed ) %>%
        group_by( NEW_MODEN, NEW_AREAN, fed_closed ) %>%
        summarize( AB1 = sum( AB1, na.rm=TRUE ),
                   B2  = sum(  B2, na.rm=TRUE ) ) %>%
        ungroup() %>%
        
        pivot_longer( cols = c('AB1','B2'), names_to = 'CATCH_VAR', values_to = 'value' ) %>%
        mutate( fed_closed = ifelse( fed_closed == 0, 'open',
                             ifelse( fed_closed == 1, 'closed', NA )) ) %>%
        pivot_wider( names_from = fed_closed, values_from = value )
      
      ###   ...and as a check that both an 'open' and 'closed' column have been created ( =0 if not )...
      cols = c( open=0, closed=0 )
      dummy.table = add_column( dummy.table, !!!cols[ setdiff( names(cols),names(dummy.table) ) ] )
      rm( cols )
      
      dummy.table = dummy.table %>%
        mutate(   open = ifelse( is.na(  open), 0,   open ),
                closed = ifelse( is.na(closed), 0, closed ) ) %>%
        filter( (open+closed) > 0 ) %>%
        mutate( p.open   =   open / (open+closed),
                p.closed = closed / (open+closed) ) %>%
        
        mutate( YEAR = 1981, WAVE = 1 ) %>%
        
        select( YEAR, WAVE, NEW_MODEN, NEW_AREAN, CATCH_VAR, p.open, p.closed ) %>%
        pivot_wider( names_from = CATCH_VAR, values_from = c('p.open','p.closed') ) %>%
        mutate( p.open_AB1 = ifelse( is.na(p.open_AB1), 0, p.open_AB1 ),
                p.open_B2  = ifelse( is.na(p.open_B2 ), 0, p.open_B2  ),
                p.closed_AB1 = ifelse( is.na(p.closed_AB1), 0, p.closed_AB1 ),
                p.closed_B2  = ifelse( is.na(p.closed_B2 ), 0, p.closed_B2  ) )
      
      ###   ...accounting for any partitioning of combined 'CbtHbt' estimates...
      blah1 = dummy.table %>%
        filter( NEW_MODEN == 'Cbt/Hbt' ) %>%
        uncount(3)
      blah1 = blah1 %>%
        mutate( NEW_MODEN = rep( c('Cbt/Hbt','Hbt','Cbt'), times = (dim(blah1)[1])/3 ) )
      
      blah2 = dummy.table %>% filter( NEW_MODEN != 'Cbt/Hbt' )
      
      dummy.table = bind_rows( blah1, blah2 ) %>%
        arrange( YEAR, WAVE, NEW_MODEN, NEW_AREAN )
      rm( blah1, blah2 )
      
      
      ### JOIN %catches with 'genrec.table' ###
      ### -------------------------------------
      ###   ...wherein this join is broken up into two components:
      ###     -- left_join() with only 1981-wave1 estimates, replacing the current <NA> values for %catch
      ###     -- bind_rows() with all other estimates not for 1981-wave1, retaining the joined %catches from above...
      
      blah1 = mrip.table %>%
        filter( YEAR == 1981 & WAVE == 1 ) %>%
        select( -c( p.open_AB1, p.open_B2, p.closed_AB1, p.closed_B2 ) ) %>%
        ###   ...removing these fields from the previous join, which didn't account for 1981-wave1 imputations
        ###     since there is no microdata for this strata (i.e., these fields currently = <NA> )...
        
        left_join( dummy.table, by = c( 'YEAR','WAVE','NEW_MODEN','NEW_AREAN' ) ) %>%
        
        ###   ...and for completion, I assign %catch = 0 for any (1981-wave1) strata for which catch records
        ###     do not exist in the microdata to estimate a percentage. In particular, MRIP occasionally retains
        ###     records with 'zero-catch' (e.g., to ensure effort estimates are properly calculated ) and so
        ###     setting p.catch=0 prevents the presence of <NA> values in these columns...
        mutate(   p.open_AB1 = ifelse( AB1==0 & B2==0 & is.na(  p.open_AB1), 0,   p.open_AB1 ),
                p.closed_AB1 = ifelse( AB1==0 & B2==0 & is.na(p.closed_AB1), 0, p.closed_AB1 ),
                  p.open_B2  = ifelse( AB1==0 & B2==0 & is.na(  p.open_B2 ), 0,   p.open_B2  ),
                p.closed_B2  = ifelse( AB1==0 & B2==0 & is.na(p.closed_B2 ), 0, p.closed_B2  ) )
      
      blah2 = mrip.table %>%
        filter( !( YEAR == 1981 & WAVE == 1 ) )
      
      mrip.table = bind_rows( blah2, blah1 )
      rm( blah1, blah2 )
      
      
      ###   ...and updating the copy of partitioning factors that have been estimated for MRIP...
      dummy.table = dummy.table  %>% mutate( DS = 'MRIP', SOURCE = 'IMP_81' )
      mrip.part.factors = bind_rows( dummy.table, mrip.part.factors )
      rm( dummy.table )
      
    }
    
    
    ### Applying %Catch to Partition MRIP Catch Estimates ###
    ### -----------------------------------------------------
    
    blah = mrip.table %>%
      mutate(   open_AB1 = AB1 *   p.open_AB1,
              closed_AB1 = AB1 * p.closed_AB1,
                open_A   =   A *   p.open_AB1,
              closed_A   =   A * p.closed_AB1,
                open_B1  =  B1 *   p.open_AB1,
              closed_B1  =  B1 * p.closed_AB1,
                open_B2  =  B2 *   p.open_B2 ,
              closed_B2  =  B2 * p.closed_B2 ,
              
                open_chtsCL = CHTS_CL *   p.open_AB1,
              closed_chtsCL = CHTS_CL * p.closed_AB1,
                open_chtsH  = CHTS_H  *   p.open_AB1,
              closed_chtsH  = CHTS_H  * p.closed_AB1,
                open_chtsRL = CHTS_RL *   p.open_B2 ,
              closed_chtsRL = CHTS_RL * p.closed_B2 ,
              
                open_varAB1 = VAR_AB1 * (  p.open_AB1^2),
              closed_varAB1 = VAR_AB1 * (p.closed_AB1^2),
                open_varB2  = VAR_B2  * (  p.open_B2 ^2),
              closed_varB2  = VAR_B2  * (p.closed_B2 ^2),
              
                open_chtsVarCL = CHTS_VAR_CL * (  p.open_AB1^2),
              closed_chtsVarCL = CHTS_VAR_CL * (p.closed_AB1^2),
                open_chtsVarH  = CHTS_VAR_H  * (  p.open_AB1^2),
              closed_chtsVarH  = CHTS_VAR_H  * (p.closed_AB1^2),
                open_chtsVarRL = CHTS_VAR_RL * (  p.open_B2 ^2),
              closed_chtsVarRL = CHTS_VAR_RL * (p.closed_B2 ^2),
              
              ### ...we use the same %catches for lbsest_SEC (landings-in-weight) as applied to AB1.
              ###   Because   LBS = AB1*avgwgt   , and since we're not updating our SEFSC avgwgts to be specific
              ###   to open vs. closed seasons, we're essentially applying the same 'avgwgt' constant to each
              ###   ( open/closed ) AB1 estimate wherein changes in LBS should be proportional to changes in AB1...
                open_WWT = lbsest_SECwwt *   p.open_AB1,
              closed_WWT = lbsest_SECwwt * p.closed_AB1,
                open_GWT = lbsest_SECgwt *   p.open_AB1,
              closed_GWT = lbsest_SECgwt * p.closed_AB1,
              
                open_wgtAB1C = WGT_AB1C *   p.open_AB1,
              closed_wgtAB1C = WGT_AB1C * p.closed_AB1,
                open_wgtAB1H = WGT_AB1H *   p.open_AB1,
              closed_wgtAB1H = WGT_AB1H * p.closed_AB1,
              
                open_chtsWgtC = CHTS_WAB1C *   p.open_AB1,
              closed_chtsWgtC = CHTS_WAB1C * p.closed_AB1,
                open_chtsWgtH = CHTS_WAB1H *   p.open_AB1,
              closed_chtsWgtH = CHTS_WAB1H * p.closed_AB1 )
    
    ###   With the %catches now applied, we then drop the original ( unadjusted ) catch estimates,
    ###   which are to be replaced by the 'open_xxx' & 'closed_xxx' variables defined above...
    
    blah = blah %>%
      select( -c( p.open_AB1, p.closed_AB1, p.open_B2, p.closed_B2,
                  AB1, A, B1, B2, VAR_AB1, VAR_B2, lbsest_SECwwt, lbsest_SECgwt, WGT_AB1C, WGT_AB1H,
                  CHTS_CL, CHTS_H, CHTS_RL, CHTS_VAR_CL, CHTS_VAR_H, CHTS_VAR_RL, CHTS_WAB1C, CHTS_WAB1H ) )
    
    pivot.cols = colnames(blah)[ ( grepl( 'open_',(colnames(blah)) ) | grepl( 'closed_',(colnames(blah)) ) ) ]
    blah = blah %>% pivot_longer( cols = any_of( pivot.cols ), names_to = 'VARIABLE', values_to = 'value' )
    rm( pivot.cols )
    
    blah = blah %>%
      mutate( fed_closed = gsub( '_.*','', VARIABLE ),
              CAT_VAR    = gsub( '.*_','', VARIABLE ) ) %>%
      select( -VARIABLE ) %>%
      
      ###     ...renaming our 'CAT_VAR' fields to their original names...
      mutate( CAT_VAR = ifelse( CAT_VAR == 'chtsCL', 'CHTS_CL',
                        ifelse( CAT_VAR == 'chtsH' , 'CHTS_H' ,
                        ifelse( CAT_VAR == 'chtsRL', 'CHTS_RL',
                        ifelse( CAT_VAR == 'varAB1', 'VAR_AB1',
                        ifelse( CAT_VAR == 'varB2' , 'VAR_B2' ,
                        ifelse( CAT_VAR == 'chtsVarCL', 'CHTS_VAR_CL',
                        ifelse( CAT_VAR == 'chtsVarH' , 'CHTS_VAR_H' ,
                        ifelse( CAT_VAR == 'chtsVarRL', 'CHTS_VAR_RL',
                        ifelse( CAT_VAR == 'WWT', 'lbsest_SECwwt',
                        ifelse( CAT_VAR == 'GWT', 'lbsest_SECgwt',
                        ifelse( CAT_VAR == 'wgtAB1C', 'WGT_AB1C',
                        ifelse( CAT_VAR == 'wgtAB1H', 'WGT_AB1H',
                        ifelse( CAT_VAR == 'chtsWgtC', 'CHTS_WAB1C',
                        ifelse( CAT_VAR == 'chtsWgtH', 'CHTS_WAB1H', CAT_VAR )))))))))))))) ) %>%
      pivot_wider( names_from = CAT_VAR, values_from = value )
    
    mrip.table = blah
    rm( blah )
    
    
    ###   ...and as a last step, I delete any rows for which no associated catch was estimated, which can be true
    ###     for any strata where 100% of the catch was assigned to either the 'open' or 'closed' fishing season... 
    blah = mrip.table %>%
      mutate( add.AB1 = ifelse( is.na(AB1), 0, AB1 ),
              add.B2  = ifelse( is.na( B2), 0,  B2 ) ) %>%
      mutate( sum.catch = add.AB1 + add.B2 )
    
    drop.rows = which( blah$sum.catch == 0 )
    rm( blah )
    
    mrip.table = mrip.table[ -drop.rows, ]
    rm( drop.rows )
    
    # ### ...and as a check that this partitioning is not 'losing' any catch...
    # sum( mrip.table$AB1, na.rm=TRUE )
    # sum( genrec.table$AB1[ genrec.table$DS == 'MRIP' ], na.rm=TRUE )
    # sum( mrip.table$B2, na.rm=TRUE )
    # sum( genrec.table$B2[ genrec.table$DS == 'MRIP' ], na.rm=TRUE )
    # summary( as.factor( mrip.table$fed_closed ) )
    
    
    message( paste( "\n", "   ***  !!! --- MRIP Estimates Partitioned --- !!!   ***   ",
                    "\n", "            --- ",
                    round( 100 - ( ( sum( genrec.table$AB1[ genrec.table$DS == 'MRIP' ], na.rm=TRUE ) -
                                     sum( mrip.table$AB1, na.rm=TRUE ) ) /
                                     sum( genrec.table$AB1[ genrec.table$DS == 'MRIP' ], na.rm=TRUE ) * 100 ), 3 ),"% of AB1 - ",
                    round( 100 - ( ( sum( genrec.table$B2[ genrec.table$DS == 'MRIP' ], na.rm=TRUE ) -
                                     sum( mrip.table$B2, na.rm=TRUE ) ) /
                                     sum( genrec.table$B2[ genrec.table$DS == 'MRIP' ], na.rm=TRUE ) * 100 ), 3 ),"% of B2 --- ",
                    sep = '' ) )
    
  }
  
  
  ####################
  ###     LACR     ###
  ####################
  
  if( 'LA Creel' %in% unique(genrec.table$DS) ) {
    
    modes.lacr = mode_sub
    modes.lacr = modes.lacr[ modes.lacr %in% c("Priv","Cbt") ]
    if( "Priv" %in% modes.lacr ) {
      modes.lacr =  gsub( "Priv", "3: Private Offshore", modes.lacr )
      modes.lacr = c( "2: Shore", modes.lacr )
      modes.lacr = c( "1: Private Inshore", modes.lacr )
    }
    modes.lacr = gsub( "Cbt",  "4: Charter", modes.lacr )
    
    
    con = dbConnect(dbDriver("Oracle"), username = keyring::key_list("SECPR")[1,2],
                    password = keyring::key_get("SECPR", keyring::key_list("SECPR")[1,2]), dbname = "SECPR")
    
    raw.lacr.catch = dbGetQuery(con,
                                paste0("select *
                          from RDI.la_creel_raw@secapxdv_dblk.sfsc.noaa.gov t
                            where ",paste0( "t.ITIS_CODE = ",sprintf( "'%s'", itis.code ), collapse=" OR " ),
                                       " AND t.REF_DESC_4 IN (", sprintf("'%s'", paste(modes.lacr, collapse = "','")),")"
                                ))
    
    ###   ...which doesn't include a YEAR filter because there are a few records in the raw LACR intercept data
    ###     ( < 0.5% ) which have a SAMPLE_YEAR that doesn't match that in ASSIGN_DATE. Instead, we apply the
    ###     year filter below, after an appropriate adjustment has been made to address this discrepancy...
    
    raw.lacr.catch = raw.lacr.catch %>%
      mutate( ASSIGN_DATE = ifelse( as.numeric(SAMPLE_YEAR) > as.numeric(substr(ASSIGN_DATE,1,4)),
                                    paste0( as.character(SAMPLE_YEAR),"-01-01" ),
                            ifelse( as.numeric(SAMPLE_YEAR) < as.numeric(substr(ASSIGN_DATE,1,4)),
                                    paste0( as.character(SAMPLE_YEAR),"-12-31" ),
                                    substr( as.character(ASSIGN_DATE),1,10 ) ) ) ) %>%
      filter( SAMPLE_YEAR >= first.year & SAMPLE_YEAR <= term.year )
    
    
    ### Value-Added Fields ###
    ### ----------------------
    
    la_site_cnty_tab = read.csv( "C:/Users/matthew.nuttall/Desktop/Functions/import_datasets/LACR_SiteToCounty Key.csv" )
    
    raw.lacr.catch = raw.lacr.catch %>%
      
      rename( SPECIES_ITIS = ITIS_CODE,
              COMMON = COMMON_NAME,
              INT_SITE = SITE_CODE ) %>%
      mutate( YEAR  = as.integer( substr( ASSIGN_DATE, 1,4 ) ),
              MONTH = as.integer( substr( ASSIGN_DATE, 6,7 ) ),
              DAY   = as.integer( substr( ASSIGN_DATE, 9,10 ) ),
              DS_MODE  = as.character( substr( REF_DESC_4, 1,1 ) ),
              DS_MODEN = as.character( substr( REF_DESC_4, 4,100000 ) ),
              # DS_DISP  = as.character( substr( REF_DESC_5, 1,1 ) ),
              # DS_DISPN = as.character( substr( REF_DESC_5, 4,100000 ) ) ) %>%
              DS_DISPN = as.character( substr( OBSERVATIONTYPE, 4,100000 ) ) ) %>%
              ###  ...where OBSERVATIONTYPE ( formerly REF_DESC_5 ) identifies catch disposition and is needed to
              ###     distinguish catch types (i.e., A-fish vs. B1-fish vs. B2-fish )...
      
      mutate( TRIP_KEY = paste0( YEAR, "LA", sprintf( "%07s", INTERCEPT_DETAILS_ID ),
                                 "L", sprintf( "%04s", CONTROL_NBR ), sprintf( "%03s", TRIP_NBR ) ) ) %>%
      
      distinct( TRIP_KEY, SPECIES_ITIS, DS_DISPN, AMOUNT_CAUGHT, .keep_all = TRUE ) %>%
      
      mutate_at( vars( YEAR, MONTH, DAY, DS_MODE ), list( ~ as.integer(.) ) ) %>%
      mutate( DS = "LA Creel",
              SUB_REG =  7,
              ST      = 22,
              NEW_ST  =  2,
              NEW_STA = "LA" ) %>%
      
      ### TIME VARIABLES ###
      mutate( date = as.Date( paste0( MONTH,"/",DAY,"/",YEAR ), "%m/%d/%Y" ),
              WAVE = ifelse( MONTH %in% 1:2 , 1,        ifelse( MONTH %in%  3:4 , 2,
                     ifelse( MONTH %in% 5:6 , 3,        ifelse( MONTH %in%  7:8 , 4,
                     ifelse( MONTH %in% 9:10, 5,        ifelse( MONTH %in% 11:12, 6, NA )))))) ) %>%
      mutate( KOD = ifelse( DAY_OF_WEEK_TYPE == "Week", "wd",
                    ifelse( DAY_OF_WEEK_TYPE == "Weekend", "we", NA ) ) ) %>%
      
      ### SUB-STATE DOMAINS ###
      ###     ...wherein LACreel adopted the same site codes as those used by MRIP (when it operated in Louisiana).
      ###       Therefore, the LACreel site codes are state-wide (like MRIP), and not bay-specific (like TPWD)
      ###       which would have required the join to also be done by BASIN...
      left_join( la_site_cnty_tab %>% select(INT_SITE,CNTY), by = "INT_SITE" ) %>%
      left_join( cnty_tab %>% filter(NEW_ST==2) %>% select(CNTY,NEW_CNTY,NEW_CNTYN), by = c("CNTY") ) %>%
      
      ### MODE ###
      ###     ...which I do manually as the mode_tab table doesn't include LACreel mode codes
      ###         ( the numeric/character identifiers for LACreel mode are contained in the REF_DESC_4 field,
      ###           which were applied (above) to define DS_MODE and DS_MODEN )...
      mutate( NEW_MODE  = ifelse( DS_MODEN %in% c("Private Inshore","Private Offshore","Shore"), 4,
                          ifelse( DS_MODEN == "Charter", 3, NA )),
              NEW_MODEN = ifelse( DS_MODEN %in% c("Private Inshore","Private Offshore","Shore"), "Priv",
                          ifelse( DS_MODEN == "Charter", "Cbt", NA )) ) %>%
      
      ### AREA ###
      ###     ...which I do using the BOW_ID fields, based on (email) guidance received from LDWF
      ###         ( Jan 27, 2022 -- "Follow-up Questions -- LACreel Raw Intercept Data" ). In particular:
      ###             -- BOW_ID < 100 represents offshore fishing ( "Ocean>3mi" )
      ###             -- BOW_ID > 100 represents inshore fishing  ( "Inshore" ),
      ###                     unless otherwise noted in AF_DESC (i.e., 21101, 70601, 70602, 120801 = "Ocean<=3mi" )...
      mutate( DS_AREA   = BOW_ID,
              DS_AREAN  = ifelse( is.na(AF_DESC) | is.na(BASIN_ID), NA, paste0( BASIN_NAME,": ",AF_DESC ) ) ) %>%
      mutate( NEW_AREAN = ifelse( is.na(BOW_ID), NA,
                          ifelse( BOW_ID < 100, "Ocean>3mi",
                          ifelse( BOW_ID %in% c(21101, 70601, 70602, 120801), "Ocean<=3mi" , "Inshore" ))) ) %>%
      
      ### STANDARD CATCH VARIABLES ###
      mutate( CATCH_TYPE = ifelse( DS_DISPN %in% c("Counted"),             "A",
                           ifelse( DS_DISPN %in% c("Reported","Bait"),     "B1",
                           ifelse( DS_DISPN %in% c("Under Size","Other"),  "B2", NA ))) ) %>%
      ###     ...where those LACreel records with OBSERVATIONTYPE = DS_DISPN = <NA> represent trips with no catch:
      ### #         length( which( !is.na(raw.lacr.catch$OBSERVATIONTYPE) ) )
      ### #         length( which( !is.na(raw.lacr.catch$COMMON_NAME) ) )
      
      ### EFFORT VARIABLES ###
      mutate( CONTB = NBR_ANGLERS ) %>%
      
      ### OTHER VARIABLES ###
      mutate( DS_PRIM1 = PRIME1,
              DS_PRIM2 = PRIME2 ) %>%
      rename( NEW_PRIM1 = PRIME1,
              NEW_PRIM2 = PRIME2 ) %>%
      mutate( NEW_TOURN = TOURNAMENT ) %>%
      rename( DS_TOURN  = TOURNAMENT ) %>%
      ###     ...where TOURNAMENT is already setup in the desired format ( 0 = 'no', 1 = 'yes' )...
      
      select( -c( OBSERVATIONTYPE, COMMENT_3, DS_DISPN, INTERCEPT_SPECIES_ID, RDI_ID ) )
    
    ### Open/Closed Season ###
    raw.lacr.catch = assign.fishing.season( new.com = new.com, region = region, genrec.table = raw.lacr.catch )
    
    ### Matching the allowable values used by the LACR survey (i.e., that in 'genrec.table' )...
    raw.lacr.catch = raw.lacr.catch %>%
      mutate( NEW_AREAN = ifelse( NEW_AREAN == 'Ocean>3mi', 'Ocean>3mi',
                          ifelse( NEW_AREAN == 'Inshore',   'Inland+Ocean<=3mi',
                          ifelse( NEW_AREAN == 'Ocean<=3mi','Inland+Ocean<=3mi', NA ))) ) %>%
      mutate( NEW_MODEN = ifelse( NEW_MODEN == 'Priv' , 'Priv/Shore',
                          ifelse( NEW_MODEN == 'Shore', 'Priv/Shore', NEW_MODEN )) )
    
    
    ### PIVOT ###
    ###   ...which pivots 'raw.lacr.catch' to have distinct columns for each catch type (i.e., A vs. B1 vs. B2 )...
    raw.lacr.catch = raw.lacr.catch %>%
      filter( !is.na(CATCH_TYPE) ) %>%
      ###   ...which is a check that 'raw.lacr.catch' doesn't include any negative trips...
      
      pivot_wider( names_from = CATCH_TYPE, values_from = AMOUNT_CAUGHT, values_fn = sum ) %>%
      mutate( A   = ifelse( is.na( A), 0, A ),
              B1  = ifelse( is.na(B1), 0, B1 ),
              B2  = ifelse( is.na(B2), 0, B2 ) ) %>%
      mutate( AB1 = A + B1 ) %>%
      
      arrange( TRIP_KEY )
    
    ###   ...and as a check that columns have been created for all catch types ( =0 if not ). This check is
    ###     particularly relevant to those species for which LACR has never sampled discards...
    cols = c( A=0, B1=0, B2=0, AB1=0 )
    raw.lacr.catch = add_column( raw.lacr.catch, !!!cols[ setdiff( names(cols),names(raw.lacr.catch) ) ] )
    rm( cols )
    
    
    
    ### %Catch by Fishing Season for **ACTUAL** LACR Estimates ###
    ### ----------------------------------------------------------
    
    dummy.table = raw.lacr.catch %>%
      group_by( YEAR, WAVE, SUB_REG, NEW_STA, NEW_MODEN, NEW_AREAN, fed_closed ) %>%
      summarize( AB1 = sum( AB1, na.rm=TRUE ),
                 B2  = sum(  B2, na.rm=TRUE ) ) %>%
      ungroup() %>%
      
      pivot_longer( cols = c('AB1','B2'), names_to = 'CATCH_VAR', values_to = 'value' ) %>%
      mutate( fed_closed = ifelse( fed_closed == 0, 'open',
                           ifelse( fed_closed == 1, 'closed', NA )) ) %>%
      pivot_wider( names_from = fed_closed, values_from = value )
    
    ###   ...and as a check that both an 'open' and 'closed' column have been created ( =0 if not )...
    cols = c( open=0, closed=0 )
    dummy.table = add_column( dummy.table, !!!cols[ setdiff( names(cols),names(dummy.table) ) ] )
    rm( cols )
    
    dummy.table = dummy.table %>%
      mutate(   open = ifelse( is.na(  open), 0,   open ),
              closed = ifelse( is.na(closed), 0, closed ) ) %>%
      filter( (open+closed) > 0 ) %>%
      mutate( p.open   =   open / (open+closed),
              p.closed = closed / (open+closed) ) %>%
      
      select( YEAR, WAVE, SUB_REG, NEW_STA, NEW_MODEN, NEW_AREAN, CATCH_VAR, p.open, p.closed ) %>%
      pivot_wider( names_from = CATCH_VAR, values_from = c('p.open','p.closed') ) %>%
      mutate(   p.open_AB1 = ifelse( is.na(p.open_AB1), 0, p.open_AB1 ),
              p.closed_AB1 = ifelse( is.na(p.closed_AB1), 0, p.closed_AB1 ),
                p.open_B2  = ifelse( is.na(p.open_B2 ), 0, p.open_B2  ),
              p.closed_B2  = ifelse( is.na(p.closed_B2 ), 0, p.closed_B2  ) )
    
    
    ### JOIN %catches with 'genrec.table' ###
    ### -------------------------------------
    
    lacr.table = genrec.table %>%
      filter( DS == 'LA Creel' ) %>%
      left_join( dummy.table, by = c( 'YEAR','WAVE','SUB_REG','NEW_STA','NEW_MODEN','NEW_AREAN' ) )
    
    ###   ...and saving a copy of the partitioning factors that have been estimated for LACR...
    lacr.part.factors = dummy.table %>% mutate( DS = 'LACR', SOURCE = 'EST' )
    rm( dummy.table )
    
    
    ### %Catch by Fishing Season for **IMPUTED** LACR Estimates ###
    ### -----------------------------------------------------------
    ###     ...for which the code below calculates an appropriate partitioning factor (i.e., %catch open vs. closed )
    ###         for any discards imputed for this assessment (e.g., either 2014-2015 or 2014+ )...
    ###   As done for MRIP imputations, these partitioning factors are calculated from the same data and
    ###       at the same stratification as that used in the imputations. These are calculated by mode for any
    ###       LA discards imputed from LACR discard rates (B2:AB1), any by year & mode if Gulf-wide discard rates
    ###       were applied in the imputation...
    
    if( method.LACR.B2 != 'None' ) {
      
      if( sum( raw.lacr.catch$B2[ raw.lacr.catch$YEAR %in% 2016:term.year ], na.rm=TRUE ) > 0 ) {
        ###   ...in that LACR started sampling discards for this species in 2016,
        ###       and so LACR discards are only being imputed for years 2014 & 2015...
        LA.B2.impute.yrs = 2014:2015
      } else {
        LA.B2.impute.yrs = 2014:term.year
      }
      
      if( method.LACR.B2 == 'la_ratio' ) {
        dummy.table = raw.lacr.catch %>%
          filter( YEAR %in% ratio.yrs.LACR.B2 ) %>%
          group_by( NEW_MODEN, fed_closed )
      }
      
      if( method.LACR.B2 == 'gu_ratio' ) {
        dummy.table = raw.mrip.catch %>%
          filter( NEW_STA %in% c("MS","AL","FLW") ) %>%
          filter( YEAR %in% LA.B2.impute.yrs ) %>%
          group_by( YEAR, NEW_MODEN, fed_closed )
      }
      
      dummy.table = dummy.table %>%
        summarize( B2 = sum( B2, na.rm=TRUE ) ) %>%
        ungroup() %>%
        pivot_longer( cols = c('B2'), names_to = 'CATCH_VAR', values_to = 'value' ) %>%
        mutate( fed_closed = ifelse( fed_closed == 0, 'open',
                             ifelse( fed_closed == 1, 'closed', NA )) ) %>%
        pivot_wider( names_from = fed_closed, values_from = value )
      
      
      ###   ...and as a check that both an 'open' and 'closed' column have been created ( =0 if not )...
      cols = c( open=0, closed=0 )
      dummy.table = add_column( dummy.table, !!!cols[ setdiff( names(cols),names(dummy.table) ) ] )
      rm( cols )
      
      dummy.table = dummy.table %>%
        mutate(   open = ifelse( is.na(  open), 0,   open ),
                closed = ifelse( is.na(closed), 0, closed ) ) %>%
        filter( (open+closed) > 0 ) %>%
        mutate( p.open   =   open / (open+closed),
                p.closed = closed / (open+closed) )
      
      
      if( method.LACR.B2 == 'la_ratio' ) {
        
        dummy.table = dummy.table %>%
          select( NEW_MODEN, CATCH_VAR, p.open, p.closed ) %>%
          pivot_wider( names_from = CATCH_VAR, values_from = c('p.open','p.closed') ) %>%
          mutate( p.open_B2   = ifelse( is.na(p.open_B2   ), 0, p.open_B2    ),
                  p.closed_B2 = ifelse( is.na(p.closed_B2 ), 0, p.closed_B2  ) )
        
        
        ### JOIN %catches with 'genrec.table' ###
        ### -------------------------------------
        ###   ...wherein this join is broken up into two components:
        ###     -- left_join() for only those years with imputed LACR discard estimates, which will replace the
        ###           current 0 values for %catch in these strata...
        ###     -- bind_rows() with all other estimates not for these strata, retaining the joined %catches from above...
        
        blah1 = lacr.table %>%
          filter( YEAR %in% LA.B2.impute.yrs ) %>%
          select( -c( p.open_B2, p.closed_B2 ) ) %>%
          ###   ...removing these fields from the previous join, which didn't account for
          ###     any imputed LACR discards since there is no LACR microdata for these strata...
          left_join( dummy.table, by = c( 'NEW_MODEN' ) )
        blah2 = lacr.table %>% filter( !( YEAR %in% LA.B2.impute.yrs ) )
        
        lacr.table = bind_rows( blah1, blah2 )
        rm( blah1, blah2 )
        
        ###   ...and updating the copy of partitioning factors that have been estimated for LACR which,
        ###     although not specific to YEAR, are expanded to include a year column to match the format
        ###     of the 'dummy.table' produced when Gulf-wide partitioning factors are applied. For this,
        ###     we duplicate the values in 'dummy.table' for every year being imputed...
        
        blah = dummy.table %>% uncount( length(LA.B2.impute.yrs) )
        blah = blah %>%
          mutate( YEAR = rep( LA.B2.impute.yrs, times = (dim(blah)[1])/length(LA.B2.impute.yrs) ) ) %>%
          arrange( YEAR, NEW_MODEN )
        dummy.table = blah  %>% mutate( DS = 'LACR', SOURCE = 'IMP_B2' )
        lacr.part.factors = bind_rows( dummy.table, lacr.part.factors )
        rm( blah, dummy.table )
        
      }
      
      if( method.LACR.B2 == 'gu_ratio' ) {
        
        dummy.table = dummy.table %>%
          select( YEAR, NEW_MODEN, CATCH_VAR, p.open, p.closed ) %>%
          pivot_wider( names_from = CATCH_VAR, values_from = c('p.open','p.closed') ) %>%
          mutate( p.open_B2   = ifelse( is.na(p.open_B2   ), 0, p.open_B2   ),
                  p.closed_B2 = ifelse( is.na(p.closed_B2 ), 0, p.closed_B2 ) )
        
        ### JOIN %catches with 'genrec.table' ###
        ### -------------------------------------
        ###   ...wherein this join is broken up into two components:
        ###     -- left_join() for only those years with imputed LACR discard estimates, which will replace the
        ###           current 0 values for %catch in these strata...
        ###     -- bind_rows() with all other estimates not for these strata, retaining the joined %catches from above...
        
        blah1 = lacr.table %>%
          filter( YEAR %in% LA.B2.impute.yrs ) %>%
          select( -c( p.open_B2, p.closed_B2 ) ) %>%
          ###   ...removing these fields from the previous join, which didn't account for
          ###     any imputed LACR discards since there is no LACR microdata for these strata...
          left_join( dummy.table, by = c( 'YEAR','NEW_MODEN' ) )
        blah2 = lacr.table %>% filter( !( YEAR %in% LA.B2.impute.yrs ) )
        
        lacr.table = bind_rows( blah1, blah2 )
        rm( blah1, blah2 )
        
        ###   ...and updating the copy of partitioning factors that have been estimated for LACR...
        dummy.table = dummy.table  %>% mutate( DS = 'LACR', SOURCE = 'IMP_B2' )
        lacr.part.factors = bind_rows( dummy.table, lacr.part.factors )
        rm( dummy.table )
        
      }
      rm( LA.B2.impute.yrs )
    }
    
    
    ### Applying %Catch to Partition LACR Catch Estimates ###
    ### -----------------------------------------------------
    
    blah = lacr.table %>%
      mutate(   open_AB1 = AB1 *   p.open_AB1,
              closed_AB1 = AB1 * p.closed_AB1,
                open_B2  =  B2 *   p.open_B2 ,
              closed_B2  =  B2 * p.closed_B2 ,
              
              ### ...we use the same %catches for lbsest_SEC (landings-in-weight) as applied to AB1.
              ###   Because   LBS = AB1*avgwgt   , and since we're not updating our SEFSC avgwgts to be specific
              ###   to open vs. closed seasons, we're essentially applying the same 'avgwgt' constant to each
              ###   ( open/closed ) AB1 estimate wherein changes in LBS should be proportional to changes in AB1...
                open_WWT = lbsest_SECwwt *   p.open_AB1,
              closed_WWT = lbsest_SECwwt * p.closed_AB1,
                open_GWT = lbsest_SECgwt *   p.open_AB1,
              closed_GWT = lbsest_SECgwt * p.closed_AB1 )
    
    # ###   ...and to identify any strata where this partitioning seems to be 'losing' catch, most likely
    # ###     associated with an appropriate partitioning factor not being estimated (e.g., AB1 * 0 = 0 )...
    # dummy = blah %>%
    #   mutate( AB1.diff = round( blah$open_AB1 + blah$closed_AB1 - blah$AB1, 4 ),
    #           B2.diff  = round( blah$open_B2  + blah$closed_B2  - blah$B2, 4 ),
    #           LBS.diff = round( blah$open_WWT + blah$closed_WWT - blah$lbsest_SECwwt, 4 ) ) %>%
    #   filter( AB1.diff != 0 | B2.diff != 0 )
    # View( dummy )
    # View( dummy[,50:dim(dummy)[2]])
    # rm( dummy )
    # 
    # dummy1 = raw.lacr.catch %>%
    #   filter( YEAR == 2017 & WAVE == 4 & NEW_MODEN == 'Cbt' )
    #   # filter( YEAR == 2019 & WAVE == 5 & NEW_MODEN == 'Priv/Shore' )
    # View( dummy1 )
    # View( dummy1[,50:dim(dummy1)[2]])
    # rm( dummy1 )
    
    ###   With the %catches now applied, we then drop the original ( unadjusted ) catch estimates,
    ###   which are to be replaced by the 'open_xxx' & 'closed_xxx' variables defined above...
    
    blah = blah %>%
      select( -c( p.open_AB1, p.closed_AB1, p.open_B2, p.closed_B2,
                  AB1, A, B1, B2, VAR_AB1, VAR_B2, lbsest_SECwwt, lbsest_SECgwt, WGT_AB1C, WGT_AB1H,
                  CHTS_CL, CHTS_H, CHTS_RL, CHTS_VAR_CL, CHTS_VAR_H, CHTS_VAR_RL, CHTS_WAB1C, CHTS_WAB1H ) ) %>%
      ungroup()
    
    blah.open = blah %>%
      select( !contains('closed_') ) %>%
      rename( AB1 = open_AB1,
              B2  = open_B2 ,
              lbsest_SECwwt = open_WWT,
              lbsest_SECgwt = open_GWT ) %>%
      mutate( fed_closed = 'open' )
    
    blah.closed = blah %>%
      select( !contains('open_') ) %>%
      rename( AB1 = closed_AB1,
              B2  = closed_B2 ,
              lbsest_SECwwt = closed_WWT,
              lbsest_SECgwt = closed_GWT ) %>%
      mutate( fed_closed = 'closed' )
    
    lacr.table = bind_rows( blah.open, blah.closed )
    rm( blah, blah.open, blah.closed )
    
    
    ###   ...and as a last step, I delete any rows for which no associated catch was estimated, which can be true
    ###     for any strata where 100% of the catch was assigned to either the 'open' or 'closed' fishing season... 
    blah = lacr.table %>%
      mutate( add.AB1 = ifelse( is.na(AB1), 0, AB1 ),
              add.B2  = ifelse( is.na( B2), 0,  B2 ) ) %>%
      mutate( sum.catch = add.AB1 + add.B2 )
    
    drop.rows = which( blah$sum.catch == 0 )
    rm( blah )
    
    lacr.table = lacr.table[ -drop.rows, ]
    rm( drop.rows )
    
    # ### ...and as a check that this partitioning is not 'losing' any catch...
    # sum( lacr.table$AB1, na.rm=TRUE )
    # sum( genrec.table$AB1[ genrec.table$DS == 'LA Creel' ], na.rm=TRUE )
    # sum( lacr.table$B2, na.rm=TRUE )
    # sum( genrec.table$B2[ genrec.table$DS == 'LA Creel' ], na.rm=TRUE )
    # summary( as.factor( lacr.table$fed_closed ) )
    
    
    message( paste( "\n", "   ***  !!! --- LACR Estimates Partitioned --- !!!   ***   ",
                    "\n", "            --- ",
                    round( 100 - ( ( sum( genrec.table$AB1[ genrec.table$DS == 'LA Creel' ], na.rm=TRUE ) -
                                     sum( lacr.table$AB1, na.rm=TRUE ) ) /
                                     sum( genrec.table$AB1[ genrec.table$DS == 'LA Creel' ], na.rm=TRUE ) * 100 ), 3 ),"% of AB1 - ",
                    round( 100 - ( ( sum( genrec.table$B2[ genrec.table$DS == 'LA Creel' ], na.rm=TRUE ) -
                                     sum( lacr.table$B2, na.rm=TRUE ) ) /
                                     sum( genrec.table$B2[ genrec.table$DS == 'LA Creel' ], na.rm=TRUE ) * 100 ), 3 ),"% of B2 --- ",
                    sep = '' ) )
    
    summary.table = lacr.table %>%
      group_by( YEAR, fed_closed ) %>%
      summarize( AB1 = sum( AB1, na.rm=TRUE ),
                 B2 = sum(  B2, na.rm=TRUE ) ) %>%
      pivot_wider( names_from = fed_closed, values_from =c('AB1','B2') )
    
  }
  
  
  ####################
  ###     TPWD     ###
  ####################
  
  if( 'TPWD' %in% unique(genrec.table$DS) ) {
    
    modes.tpwd = mode_sub
    modes.tpwd = gsub( "Priv", 1, modes.tpwd )
    modes.tpwd = gsub( "Cbt",  2, modes.tpwd )
    modes.tpwd = modes.tpwd[ modes.tpwd %in% c(1,2) ]
    
    
    con = dbConnect(dbDriver("Oracle"), username = keyring::key_list("SECPR")[1,2],
                    password = keyring::key_get("SECPR", keyring::key_list("SECPR")[1,2]), dbname = "SECPR")
    
    raw.tpwd.catch = dbGetQuery(con,
                                paste0("select
                                  t.TRIP_KEY, t.INT_ID,
                                  t.SEASON, t.SYEAR, t.INT_YEAR, t.INT_MONTH, t.INT_DAY, t.WAVE, t.INT_TIME,
                                  t.STRATA, t.MAJOR, c.MINOR_BAY_OF_CATCH_CODE, t.INT_SITE, c.AREA_CAT, t.AREA_INT, t.ACTIVE,
                                  t.GEAR_SIZE_DAY_TYPE_NUM, t.GEAR_CODE, t.BAIT_CODE,
                                  t.TRIPLEN, t.TTLANGLR,
                                  c.SPECCODE, c.TTLCOT, c.LNGTHID, c.LD, c.INT_LENGTH
                          from RDI.tpwd_length@secapxdv_dblk.sfsc.noaa.gov c
                          FULL JOIN RDI.tpwd_party@secapxdv_dblk.sfsc.noaa.gov t
                                  ON t.TRIP_KEY = c.TRIP_KEY
                               where ",paste0( "c.SPECCODE = ",sprintf( "'%s'", tpwd.code ), collapse=" OR " ),
                                       " AND t.INT_YEAR BETWEEN ",first.year," AND ",term.year,
                                       " AND t.ACTIVE IN (", sprintf("'%s'", paste(modes.tpwd, collapse = "','")),")"
                                ))
    
    ### Value-Added Fields ###
    ### ----------------------
    
    tx_site_cnty_tab = dbGetQuery( con, "SELECT * FROM RDI.tpwd_site_county_xref@secapxdv_dblk.sfsc.noaa.gov" )
    tx_site_cnty_tab = tx_site_cnty_tab %>%
      mutate( MAJOR    = MAJOR_BAY,
              INT_SITE = SITE_CODE,
              CNTY     = as.integer(CNTY) ) %>%
      arrange( MAJOR, INT_SITE )
    
    
    raw.tpwd.catch = raw.tpwd.catch %>%
      
      rename( TX_CODE = SPECCODE,
              YEAR  = INT_YEAR,
              MONTH = INT_MONTH,
              DAY   = INT_DAY,
              TIME  = INT_TIME,
              ACTIVITY = ACTIVE,
              MINOR = MINOR_BAY_OF_CATCH_CODE ) %>%
      distinct( TRIP_KEY, TX_CODE, TTLCOT, .keep_all = TRUE ) %>%
      ###   ...where the distinct() statement removes quite a few rows (about 60%) from 'tpwd.catchtrip',
      ###       which originally included TPWD size records (multiple records per trip) that are not being
      ###       retained in my data file...
      
      mutate_at( vars( YEAR, MONTH, DAY, WAVE, ACTIVITY ), list( ~ as.integer(.) ) ) %>%
      mutate( DS = "TPWD",
              SUB_REG =  7,
              ST      = 48,
              NEW_ST  =  1,
              NEW_STA = "TX" ) %>%
      
      ### TIME VARIABLES ###
      mutate( date = as.Date( paste0( MONTH,"/",DAY,"/",YEAR ), "%m/%d/%Y" ) ) %>%
      
      ### SUB-STATE DOMAINS ###
      left_join( tx_site_cnty_tab %>% select(MAJOR,INT_SITE,CNTY), by = c("MAJOR","INT_SITE") ) %>%
      left_join( cnty_tab %>% filter(NEW_ST==1) %>% select(CNTY,NEW_CNTY,NEW_CNTYN), by = c("CNTY") ) %>%
      
      ### MODE ###
      ###     ...which I do manually as the mode_tab table doesn't include TPWD activity codes
      ###         ( ACTIVITY codes are defined in Figure 9 of the TPWD Operations Manual -- Green 2017 )...
      mutate( DS_MODE  = ACTIVITY ) %>%
      mutate( DS_MODEN = ifelse( DS_MODE == 1, "Sport fishing",     ifelse( DS_MODE == 2, "Party-boat fishing",
                         ifelse( DS_MODE == 3, "Tournament fishing", NA ))) ) %>%
      mutate( NEW_MODE  = ifelse( ACTIVITY %in% c(1,3), 4,          ifelse( ACTIVITY == 2, 3, NA )),
              NEW_MODEN = ifelse( ACTIVITY %in% c(1,3), "Priv",     ifelse( ACTIVITY == 2, "Cbt", NA )) ) %>%
      
      ### AREA ###
      ###     ...which I do manually as the area_tab table doesn't include TPWD area codes
      ###         ( AREA codes are defined in Figure 6 of the TPWD Operations Manual -- Green 2017 )...
      mutate( DS_AREA   = MINOR ) %>%
      mutate( NEW_AREAN = ifelse( MINOR %in% c(990,992,994,996,998), "Ocean<=10mi",
                          ifelse( MINOR %in% c(991,993,995,997,999), "Ocean>10mi" , "Inshore" )) ) %>%
      
      ### STANDARD CATCH VARIABLES ###
      mutate( A = TTLCOT ) %>%
      
      ### EFFORT VARIABLES ###
      mutate( CONTB = TTLANGLR,
              HRSF  = TRIPLEN  )
    
    ### Open/Closed Season ###
    raw.tpwd.catch = assign.fishing.season( new.com = new.com, region = region, genrec.table = raw.tpwd.catch )
    
    ###   ...and as a final adjustment, although TPWD catch largely equates to observed landings (i.e., A-fish ),
    ###     we rename this column as 'AB1' to match the naming convention in our final genrec catch table...
    raw.tpwd.catch = raw.tpwd.catch %>% rename( AB1 = A )
    
    
    ### %Catch by Fishing Season for **ACTUAL** TPWD Estimates ###
    ### ----------------------------------------------------------
    
    dummy.table = raw.tpwd.catch %>%
      group_by( YEAR, WAVE, SUB_REG, NEW_STA, NEW_MODEN, NEW_AREAN, fed_closed ) %>%
      summarize( AB1 = sum( AB1, na.rm=TRUE ) ) %>%
      ungroup() %>%
      
      pivot_longer( cols = c('AB1'), names_to = 'CATCH_VAR', values_to = 'value' ) %>%
      mutate( fed_closed = ifelse( fed_closed == 0, 'open',
                           ifelse( fed_closed == 1, 'closed', NA )) ) %>%
      pivot_wider( names_from = fed_closed, values_from = value )
    
    ###   ...and as a check that both an 'open' and 'closed' column have been created ( =0 if not )...
    cols = c( open=0, closed=0 )
    dummy.table = add_column( dummy.table, !!!cols[ setdiff( names(cols),names(dummy.table) ) ] )
    rm( cols )
    
    dummy.table = dummy.table %>%
      mutate(   open = ifelse( is.na(  open), 0,   open ),
              closed = ifelse( is.na(closed), 0, closed ) ) %>%
      filter( (open+closed) > 0 ) %>%
      mutate( p.open   =   open / (open+closed),
              p.closed = closed / (open+closed) ) %>%
      
      select( YEAR, WAVE, SUB_REG, NEW_STA, NEW_MODEN, NEW_AREAN, CATCH_VAR, p.open, p.closed ) %>%
      pivot_wider( names_from = CATCH_VAR, values_from = c('p.open','p.closed') ) %>%
      mutate(   p.open_AB1 = ifelse( is.na(p.open_AB1), 0, p.open_AB1 ),
              p.closed_AB1 = ifelse( is.na(p.closed_AB1), 0, p.closed_AB1 ) )
    
    
    ### JOIN %catches with 'genrec.table' ###
    ### -------------------------------------
    
    tpwd.table = genrec.table %>%
      filter( DS == 'TPWD' ) %>%
      left_join( dummy.table, by = c( 'YEAR','WAVE','SUB_REG','NEW_STA','NEW_MODEN','NEW_AREAN' ) )
    
    ###   ...and saving a copy of the partitioning factors that have been estimated for TPWD...
    tpwd.part.factors = dummy.table %>% mutate( DS = 'TPWD', SOURCE = 'EST' )
    rm( dummy.table )
    
    
    ### %Catch by Fishing Season for **IMPUTED** TPWD Estimates ###
    ### -----------------------------------------------------------
    ###     ...for which the code below calculates an appropriate partitioning factor
    ###       (i.e., %catch open vs. closed ) for any imputed TPWD estimates, including:
    ###           -- any TPWD landings imputed for 1981-(Apr)1983
    ###           -- any TPWD discards imputed across the timeseries
    ###   As done for MRIP imputations, these partitioning factors are calculated from the same data and
    ###       at the same stratification as that used in the imputations. For any 1981-1983 imputations,
    ###       the 'best practice' approach calculates average annual catches from 1983-1985 data by wave & mode.
    ###       For TPWD discards, we apply either LA-specific or Gulf-wide discard rates (B2:AB1) by year & mode.
    
    
    ### TPWD 1981-1983 ###
    ### ------------------
    if( dim( genrec.table[ genrec.table$DS == 'TPWD' &
                           ( genrec.table$YEAR %in% 1981:1982 |
                           ( genrec.table$YEAR == 1983 & genrec.table$WAVE %in% 1:2 ) ), ] )[1] > 0 ) {
      
      dummy.table = raw.tpwd.catch %>%
        filter( YEAR %in% 1983:1985 ) %>%
        
        # group_by( SUB_REG, NEW_STA, FL_REG, NC_REG, NEW_MODEN, NEW_AREAN, fed_closed ) %>%
        group_by( WAVE, NEW_MODEN, fed_closed ) %>%
        summarize( AB1 = sum( AB1, na.rm=TRUE ) ) %>%
        ungroup() %>%
        
        pivot_longer( cols = c('AB1'), names_to = 'CATCH_VAR', values_to = 'value' ) %>%
        mutate( fed_closed = ifelse( fed_closed == 0, 'open',
                             ifelse( fed_closed == 1, 'closed', NA )) ) %>%
        pivot_wider( names_from = fed_closed, values_from = value )
      
      ###   ...and as a check that both an 'open' and 'closed' column have been created ( =0 if not )...
      cols = c( open=0, closed=0 )
      dummy.table = add_column( dummy.table, !!!cols[ setdiff( names(cols),names(dummy.table) ) ] )
      rm( cols )
      
      dummy.table = dummy.table %>%
        mutate(   open = ifelse( is.na(  open), 0,   open ),
                closed = ifelse( is.na(closed), 0, closed ) ) %>%
        filter( (open+closed) > 0 ) %>%
        mutate( p.open   =   open / (open+closed),
                p.closed = closed / (open+closed) )
      
      ###   ...replicating 'dummy.table' to represent three distinct years ( of imputations, 1981-1983 )...
      blah = dummy.table %>% uncount(3)
      blah = blah %>%
        mutate( YEAR = rep( c(1981:1983), times = (dim(blah)[1])/3 ) ) %>%
        filter( !( YEAR == 1983 & WAVE %in% 3:6 ) )
      
      dummy.table = blah %>% arrange( YEAR, WAVE, NEW_MODEN )
      rm( blah )
      
      dummy.table = dummy.table %>%
        select( YEAR, WAVE, NEW_MODEN, CATCH_VAR, p.open, p.closed ) %>%
        pivot_wider( names_from = CATCH_VAR, values_from = c('p.open','p.closed') ) %>%
        mutate(   p.open_AB1 = ifelse( is.na(p.open_AB1), 0, p.open_AB1 ),
                p.closed_AB1 = ifelse( is.na(p.closed_AB1), 0, p.closed_AB1 ) )
      
      
      ### JOIN %catches with 'genrec.table' ###
      ### -------------------------------------
      ###   ...wherein this join is broken up into two components:
      ###     -- left_join() with only 1981-(Apr)1983 estimates, replacing the current <NA> values for %catch
      ###     -- bind_rows() with all other estimates not for 1981-(Apr)1983, retaining the joined %catches from above...
      
      blah1 = tpwd.table %>%
        filter( YEAR %in% 1981:1982 | ( YEAR == 1983 & WAVE %in% 1:2 ) ) %>%
        select( -c( p.open_AB1, p.closed_AB1 ) ) %>%
        ###   ...removing these fields from the previous join, which didn't account for 1981-(Apr)1983 imputations
        ###     since there is no microdata for this strata (i.e., these fields currently = <NA> )...
        left_join( dummy.table, by = c( 'YEAR','WAVE','NEW_MODEN' ) )
      blah2 = tpwd.table %>% filter( !( YEAR %in% 1981:1982 | ( YEAR == 1983 & WAVE %in% 1:2 ) ) )
      
      tpwd.table = bind_rows( blah2, blah1 )
      rm( blah1, blah2 )
      
      ###   ...and updating the copy of partitioning factors that have been estimated for LACR...
      dummy.table = dummy.table  %>% mutate( DS = 'TPWD', SOURCE = 'IMP_8183' )
      tpwd.part.factors = bind_rows( dummy.table, tpwd.part.factors )
      rm( dummy.table )
      
    }
    
    ### TPWD Discards ###
    ### -----------------
    
    if( sum( genrec.table$B2[ genrec.table$DS == 'TPWD' ], na.rm=TRUE ) > 0 ) {
      
      if( method.TPWD.B2 == 'la_ratio' ) {
        dummy.table = mrip.table %>% filter( NEW_STA == 'LA' )
      }
      if( method.TPWD.B2 == 'gu_ratio' ) {
        dummy.table = mrip.table %>% filter( NEW_STA %in% c('LA','MS','AL','FLW') )
      }
      
      if( 'LA Creel' %in% unique(genrec.table$DS) ) {
        dummy.table = bind_rows( dummy.table, lacr.table )
      }
      ###   ...where 'dummy.table' is constructed from 'mrip.table' ( and maybe 'lacr.table' ) to ensure any
      ###     imputed MRIP/LACR estimates are part of the calculations for the TPWD-B2 partitioning factors...
      
      dummy.table = dummy.table %>%
        mutate( NEW_MODEN = ifelse( NEW_MODEN == 'Priv/Shore', 'Priv', NEW_MODEN ) ) %>%
        
        group_by( YEAR, NEW_MODEN, fed_closed ) %>%
        summarize( B2 = sum( B2, na.rm=TRUE ) ) %>%
        ungroup() %>%
        
        pivot_longer( cols = c('B2'), names_to = 'CATCH_VAR', values_to = 'value' ) %>%
        pivot_wider( names_from = fed_closed, values_from = value )
      
      ###   ...and as a check that both an 'open' and 'closed' column have been created ( =0 if not )...
      cols = c( open=0, closed=0 )
      dummy.table = add_column( dummy.table, !!!cols[ setdiff( names(cols),names(dummy.table) ) ] )
      rm( cols )
      
      dummy.table = dummy.table %>%
        mutate(   open = ifelse( is.na(  open), 0,   open ),
                closed = ifelse( is.na(closed), 0, closed ) ) %>%
        filter( (open+closed) > 0 ) %>%
        mutate( p.open   =   open / (open+closed),
                p.closed = closed / (open+closed) ) %>%
        
        select( YEAR, NEW_MODEN, CATCH_VAR, p.open, p.closed ) %>%
        pivot_wider( names_from = CATCH_VAR, values_from = c('p.open','p.closed') ) %>%
        mutate( p.open_B2   = ifelse( is.na(p.open_B2   ), 0, p.open_B2   ),
                p.closed_B2 = ifelse( is.na(p.closed_B2 ), 0, p.closed_B2 ) )
      
      
      ###   ...and accounting for any partitioning of combined 'CbtHbt' estimates in the MRIP data...
      blah1 = dummy.table %>%
        filter( NEW_MODEN == 'Cbt/Hbt' ) %>%
        uncount(3)
      blah1 = blah1 %>%
        mutate( NEW_MODEN = rep( c('Cbt/Hbt','Hbt','Cbt'), times = (dim(blah1)[1])/3 ) )
      
      blah2 = dummy.table %>% filter( NEW_MODEN != 'Cbt/Hbt' )
      
      dummy.table = bind_rows( blah1, blah2 ) %>%
        arrange( YEAR, NEW_MODEN )
      rm( blah1, blah2 )
      
      
      ### JOIN %catches with 'genrec.table' ###
      ### -------------------------------------
      
      blah = tpwd.table %>%
        select( -any_of( c( 'p.open_B2', 'p.closed_B2' ) ) ) %>%
        ###   ...removing these fields from the previous join, which didn't account for
        ###     any imputed TPWD discards since there is no TPWD microdata for these strata...
        left_join( dummy.table, by = c( 'YEAR','NEW_MODEN' ) )
      
      tpwd.table = blah
      rm( blah )
      
      ###   ...and updating the copy of partitioning factors that have been estimated for LACR...
      dummy.table = dummy.table  %>% mutate( DS = 'TPWD', SOURCE = 'IMP_B2' )
      tpwd.part.factors = bind_rows( tpwd.part.factors, dummy.table )
      rm( dummy.table )
      
    }
    
    
    ### Applying %Catch to Partition TPWD Catch Estimates ###
    ### -----------------------------------------------------
    
    blah = tpwd.table %>%
      mutate( p.open_B2   = ifelse( is.na(p.open_B2   ), 0, p.open_B2   ),
              p.closed_B2 = ifelse( is.na(p.closed_B2 ), 0, p.closed_B2 ) ) %>%
      mutate(   open_AB1 = AB1 *   p.open_AB1,
              closed_AB1 = AB1 * p.closed_AB1,
                open_B2  =  B2 *   p.open_B2 ,
              closed_B2  =  B2 * p.closed_B2 ,
              
              ### ...we use the same %catches for lbsest_SEC (landings-in-weight) as applied to AB1.
              ###   Because   LBS = AB1*avgwgt   , and since we're not updating our SEFSC avgwgts to be specific
              ###   to open vs. closed seasons, we're essentially applying the same 'avgwgt' constant to each
              ###   ( open/closed ) AB1 estimate wherein changes in LBS should be proportional to changes in AB1...
                open_WWT = lbsest_SECwwt *   p.open_AB1,
              closed_WWT = lbsest_SECwwt * p.closed_AB1,
                open_GWT = lbsest_SECgwt *   p.open_AB1,
              closed_GWT = lbsest_SECgwt * p.closed_AB1 )
    
    # ###   ...and to identify any strata where this partitioning seems to be 'losing' catch, most likely
    # ###     associated with an appropriate partitioning factor not being estimated (e.g., AB1 * 0 = 0 )...
    # dummy = blah %>%
    #   mutate( AB1.diff = round( blah$open_AB1 + blah$closed_AB1 - blah$AB1, 4 ),
    #           B2.diff  = round( blah$open_B2  + blah$closed_B2  - blah$B2, 4 ),
    #           LBS.diff = round( blah$open_WWT + blah$closed_WWT - blah$lbsest_SECwwt, 4 ) ) %>%
    #   # filter( AB1>0 & AB1.diff != 0 )
    #   filter(  B2>0 &  B2.diff != 0 )
    # View( dummy )
    # View( dummy[,50:dim(dummy)[2]])
    # rm( dummy )
    
    ###   With the %catches now applied, we then drop the original ( unadjusted ) catch estimates,
    ###   which are to be replaced by the 'open_xxx' & 'closed_xxx' variables defined above...
    
    blah = blah %>%
      select( -c( p.open_AB1, p.closed_AB1, p.open_B2, p.closed_B2,
                  AB1, A, B1, B2, VAR_AB1, VAR_B2, lbsest_SECwwt, lbsest_SECgwt, WGT_AB1C, WGT_AB1H,
                  CHTS_CL, CHTS_H, CHTS_RL, CHTS_VAR_CL, CHTS_VAR_H, CHTS_VAR_RL, CHTS_WAB1C, CHTS_WAB1H ) ) %>%
      ungroup()
    
    blah.open = blah %>%
      select( !contains('closed_') ) %>%
      rename( AB1 = open_AB1,
              B2  = open_B2 ,
              lbsest_SECwwt = open_WWT,
              lbsest_SECgwt = open_GWT ) %>%
      mutate( fed_closed = 'open' )
    
    blah.closed = blah %>%
      select( !contains('open_') ) %>%
      rename( AB1 = closed_AB1,
              B2  = closed_B2 ,
              lbsest_SECwwt = closed_WWT,
              lbsest_SECgwt = closed_GWT ) %>%
      mutate( fed_closed = 'closed' )
    
    tpwd.table = bind_rows( blah.open, blah.closed )
    rm( blah, blah.open, blah.closed )
    
    
    ###   ...and as a last step, I delete any rows for which no associated catch was estimated, which can be true
    ###     for any strata where 100% of the catch was assigned to either the 'open' or 'closed' fishing season... 
    blah = tpwd.table %>%
      mutate( add.AB1 = ifelse( is.na(AB1), 0, AB1 ),
              add.B2  = ifelse( is.na( B2), 0,  B2 ) ) %>%
      mutate( sum.catch = add.AB1 + add.B2 )
    
    drop.rows = which( blah$sum.catch == 0 )
    rm( blah )
    
    tpwd.table = tpwd.table[ -drop.rows, ]
    rm( drop.rows )
    
    # sum( tpwd.table$AB1, na.rm=TRUE )
    # sum( genrec.table$AB1[ genrec.table$DS == 'TPWD' ], na.rm=TRUE )
    # sum( tpwd.table$B2, na.rm=TRUE )
    # sum( genrec.table$B2[ genrec.table$DS == 'TPWD' ], na.rm=TRUE )
    # summary( as.factor( tpwd.table$fed_closed ) )
    
    
    message( paste( "\n", "   ***  !!! --- TPWD Estimates Partitioned --- !!!   ***   ",
                    "\n", "            --- ",
                    round( 100 - ( ( sum( genrec.table$AB1[ genrec.table$DS == 'TPWD' ], na.rm=TRUE ) -
                                     sum( tpwd.table$AB1, na.rm=TRUE ) ) /
                                     sum( genrec.table$AB1[ genrec.table$DS == 'TPWD' ], na.rm=TRUE ) * 100 ), 3 ),"% of AB1 - ",
                    round( 100 - ( ( sum( genrec.table$B2[ genrec.table$DS == 'TPWD' ], na.rm=TRUE ) -
                                     sum( tpwd.table$B2, na.rm=TRUE ) ) /
                                     sum( genrec.table$B2[ genrec.table$DS == 'TPWD' ], na.rm=TRUE ) * 100 ), 3 ),"% of B2 --- ",
                    sep = '' ) )
    
  }
  
  
  dummy.catch = mrip.table
  if( 'LA Creel' %in% unique(genrec.table$DS) ) {  dummy.catch = bind_rows( lacr.table, dummy.catch )  }
  if( 'TPWD'     %in% unique(genrec.table$DS) ) {  dummy.catch = bind_rows( tpwd.table, dummy.catch )  }
  dummy.catch = dummy.catch %>% arrange( YEAR, WAVE, SUB_REG, NEW_ST, NEW_MODE, NEW_AREAN )
  
  dummy.part = mrip.part.factors
  if( 'LA Creel' %in% unique(genrec.table$DS) ) {  dummy.part = bind_rows( lacr.part.factors, dummy.part )  }
  if( 'TPWD'     %in% unique(genrec.table$DS) ) {  dummy.part = bind_rows( tpwd.part.factors, dummy.part )  }
  dummy.part = dummy.part %>%
    arrange( YEAR, WAVE, DS, SOURCE, NEW_MODEN ) %>%
    select( any_of( c('YEAR','WAVE','NEW_MODEN','NEW_AREAN','SUB_REG','NEW_STA','FL_REG','NC_REG',
                      'DS','SOURCE','p.open_AB1','p.closed_AB1','p.open_B2','p.closed_B2') ) )
  
  return.object = list()
  return.object$catch.table  = dummy.catch
  return.object$part.factors = dummy.part
  rm( dummy.catch, dummy.part )
  
  
  return( return.object )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------


cv.fishing.season = function( cv.table, genrec.table, DS.filter = c('MRIP','LA Creel','TPWD')  ) {
  ###     ...where 'cv.table' is the table of catch & CV estimates that are to be partitioned between
  ###               open & closed fishing seasons ( saved as a 'fed_closed' field )
  ###          'genrec.table' is the table of (trip-level) catch estimates that has been compiled
  ###               for this assessment (i.e., in the main script )...
  ###     ...and 'DS.filter' identifies the datasource of the provided catch & CV estimates,
  ###               as needed to properly filter 'genrec.table' in calculating %catches...
  
  
  ###   To start, I first check whether the 'cv.table' object is a single CV-table or a list of
  ###   multiple CV-tables, the latter corresponding to tables specific to individual SID domains...
  
  return.table = cv.table
  
  
  ### Assessment with SID Domains ###
  ### -------------------------------
  
  if( 'SID' %in% colnames(genrec.table) ) {
    
    ### Tables composed of multiple SID Domains ###
    ###     ...which typically correspond to 'MRIP' CV Tables, saved as a list of SID-specific tables...
    
    if( !is.data.frame(cv.table) ) {
      
      dummy.SIDs = unique( genrec.table$SID )
      ###   ...for which I remove any 'SID' domains corresponding to the state of Texas, which has never
      ###     been sampled by MRIP and so not applicable to this pull ( of MRIP-CVs )...
      dummy.SIDs = dummy.SIDs[ !grepl('TX',dummy.SIDs) ]
      
      for( i in 1:length(dummy.SIDs) ) {
        
        dummy.table = cv.table[[ which( names(cv.table) == dummy.SIDs[i] ) ]] %>% ungroup()
        
        ###   ...pivoting the mrip.cv table to have each catch variable as its own unique column,
        ###       which requires the 'ATtotal' and 'PSUtotal' values to be duplicated and assigned
        ###       to both 'AB1' and 'B2' summaries...
        blah1 = dummy.table %>%
          filter( CATCH_VAR == 'TOTAL' ) %>%
          mutate( METRIC = paste0( METRIC,"total" ) ) %>%
          uncount(2)
        blah1 = blah1 %>% mutate( CATCH_VAR = rep( c('AB1','B2'), times = (dim(blah1)[1])/2 ) )
        blah2 = dummy.table %>% filter( CATCH_VAR != 'TOTAL' )
        dummy.table = bind_rows( blah1, blah2 )
        rm( blah1, blah2 )
        
        dummy.table = dummy.table %>%
          pivot_wider( names_from = METRIC, values_from = value ) %>%
          mutate( CV = ifelse( CAT==0, 0, sqrt(VAR) / CAT ) )
        
        ###   ...calculating %catches ( open vs. closed ) from 'genrec.table'...
        dummy.ratios = genrec.table %>%
          filter( DS == DS.filter & SID == dummy.SIDs[i] ) %>%
          mutate( NEW_MODEN = toupper(NEW_MODEN) )
        ###   ...to match the uppercase format used in 'dummy.table'...
        blah1 = dummy.ratios %>%
          group_by( YEAR, NEW_MODEN, fed_closed ) %>%
          summarize( AB1 = sum( AB1, na.rm=TRUE ),
                     B2  = sum(  B2, na.rm=TRUE ) ) %>%
          ungroup()
        blah2 = dummy.ratios %>%
          group_by( YEAR, fed_closed ) %>%
          summarize( AB1 = sum( AB1, na.rm=TRUE ),
                     B2  = sum(  B2, na.rm=TRUE ) ) %>%
          ungroup() %>%
          mutate( NEW_MODEN = 'TOTAL' )
        dummy.ratios = bind_rows( blah1, blah2 )
        rm( blah1, blah2 )
        
        dummy.ratios = dummy.ratios %>%
          mutate( NEW_MODEN = ifelse( NEW_MODEN == 'PRIV/SHORE', 'PRIV', NEW_MODEN ) ) %>%
          pivot_longer( cols = c('AB1','B2'), names_to = 'CATCH_VAR', values_to = 'value' ) %>%
          pivot_wider( names_from = fed_closed, values_from = value )
        
        cols = c( open=0, closed=0 )
        dummy.ratios = add_column( dummy.ratios, !!!cols[ setdiff( names(cols),names(dummy.ratios) ) ] )
        rm( cols )
        
        dummy.ratios = dummy.ratios %>%
          mutate(   open = ifelse( is.na(  open), 0,   open ),
                  closed = ifelse( is.na(closed), 0, closed ) ) %>%
          filter( (open+closed) > 0 ) %>%
          mutate( p.open   =   open / (open+closed),
                  p.closed = closed / (open+closed) ) %>%
          select( YEAR, NEW_MODEN, CATCH_VAR, p.open, p.closed )
        
        ###   ...joining %catches ( in 'dummy.table' ) with MRIP-CV estimates ( in 'dummy.table' )...
        dummy.table = dummy.table %>%
          left_join( dummy.ratios, by = c('YEAR','NEW_MODEN','CATCH_VAR') )
        rm( dummy.ratios )
        
        # ###   ...and as a quick check that we're not missing any strata...
        # sum( dummy.table$CAT[ is.na(dummy.table$p.open  ) ], na.rm=TRUE )
        # sum( dummy.table$CAT[ is.na(dummy.table$p.closed) ], na.rm=TRUE )
        
        blah = dummy.table %>% uncount(2)
        blah = blah %>%
          mutate( fed_closed = rep( c('open','closed'), times = (dim(blah)[1])/2 ) ) %>%
          
          mutate( CAT_season = ifelse( fed_closed == 'open'  , CAT * p.open,
                               ifelse( fed_closed == 'closed', CAT * p.closed, NA )) ) %>%
          mutate( CAT_season = ifelse( is.na(CAT_season), 0, CAT_season ) ) %>%
          select( -c( 'p.open', 'p.closed', 'CAT' ) ) %>%
          rename( CAT = CAT_season ) %>%
          mutate( VAR = ( CV * CAT )^2 ) %>%
          select( -CV ) %>%
          
          pivot_longer( cols = c('CAT','VAR','AT','PSU','ATtotal','PSUtotal'),
                        names_to = 'METRIC', values_to = 'value' ) %>%
          
          ###   ...and, as a last step, removing the duplicated 'ATtotal' & 'PSUtotal' fields,
          ###     which are the same for 'AB1' & 'B2' summaries and so only provided once...
          mutate( CATCH_VAR = ifelse( METRIC %in% c('ATtotal','PSUtotal'), 'TOTAL', CATCH_VAR ) ) %>%
          mutate( METRIC = ifelse( METRIC %in% c('ATtotal','PSUtotal'), gsub('total','',METRIC), METRIC ) ) %>%
          select( SID, YEAR, NEW_MODEN, fed_closed, CATCH_VAR, METRIC, value ) %>%
          distinct( SID, YEAR, NEW_MODEN, fed_closed, CATCH_VAR, METRIC, value )
        
        dummy.table = blah
        rm( blah )
        
        return.table[[ which( names(return.table) == dummy.SIDs[i] ) ]] = dummy.table
        rm( dummy.table )
        
      }
      rm( i, dummy.SIDs )
    
    } else {
      
      ### Tables composed of a single SID Domain ###
      ###     ...which typically correspond to 'TPWD' or 'LACR' CV Tables...
      
      dummy.table = cv.table %>% ungroup()
      
      blah1 = dummy.table %>%
        filter( CATCH_VAR == 'TOTAL' ) %>%
        mutate( METRIC = paste0( METRIC,"total" ) ) %>%
        uncount(2)
      blah1 = blah1 %>% mutate( CATCH_VAR = rep( c('AB1','B2'), times = (dim(blah1)[1])/2 ) )
      blah2 = dummy.table %>% filter( CATCH_VAR != 'TOTAL' )
      dummy.table = bind_rows( blah1, blah2 )
      rm( blah1, blah2 )
      
      dummy.table = dummy.table %>%
        pivot_wider( names_from = METRIC, values_from = value ) %>%
        mutate( CV = ifelse( CAT==0, 0, sqrt(VAR) / CAT ) )
      
      dummy.ratios = genrec.table %>%
        filter( DS == DS.filter ) %>%
        mutate( NEW_MODEN = toupper(NEW_MODEN) )
      
      blah1 = dummy.ratios %>%
        group_by( SID, YEAR, NEW_MODEN, fed_closed ) %>%
        summarize( AB1 = sum( AB1, na.rm=TRUE ),
                   B2  = sum(  B2, na.rm=TRUE ) ) %>%
        ungroup()
      blah2 = dummy.ratios %>%
        group_by( SID, YEAR, fed_closed ) %>%
        summarize( AB1 = sum( AB1, na.rm=TRUE ),
                   B2  = sum(  B2, na.rm=TRUE ) ) %>%
        ungroup() %>%
        mutate( NEW_MODEN = 'TOTAL' )
      dummy.ratios = bind_rows( blah1, blah2 )
      rm( blah1, blah2 )
      
      dummy.ratios = dummy.ratios %>%
        mutate( NEW_MODEN = ifelse( NEW_MODEN == 'PRIV/SHORE', 'PRIV', NEW_MODEN ) ) %>%
        pivot_longer( cols = c('AB1','B2'), names_to = 'CATCH_VAR', values_to = 'value' ) %>%
        pivot_wider( names_from = fed_closed, values_from = value )
      
      cols = c( open=0, closed=0 )
      dummy.ratios = add_column( dummy.ratios, !!!cols[ setdiff( names(cols),names(dummy.ratios) ) ] )
      rm( cols )
      
      dummy.ratios = dummy.ratios %>%
        mutate(   open = ifelse( is.na(  open), 0,   open ),
                closed = ifelse( is.na(closed), 0, closed ) ) %>%
        filter( (open+closed) > 0 ) %>%
        mutate( p.open   =   open / (open+closed),
                p.closed = closed / (open+closed) ) %>%
        select( SID, YEAR, NEW_MODEN, CATCH_VAR, p.open, p.closed )
      
      ###   ...joining %catches ( in 'dummy.table' ) with MRIP-CV estimates ( in 'dummy.table' )...
      dummy.table = dummy.table %>%
        left_join( dummy.ratios, by = c('SID','YEAR','NEW_MODEN','CATCH_VAR') )
      rm( dummy.ratios )
      
      # ###   ...and as a quick check that we're not missing any strata...
      # sum( dummy.table$CAT[ is.na(dummy.table$p.open  ) ], na.rm=TRUE )
      # sum( dummy.table$CAT[ is.na(dummy.table$p.closed) ], na.rm=TRUE )
      
      blah = dummy.table %>% uncount(2)
      blah = blah %>%
        mutate( fed_closed = rep( c('open','closed'), times = (dim(blah)[1])/2 ) ) %>%
        
        mutate( CAT_season = ifelse( fed_closed == 'open'  , CAT * p.open,
                             ifelse( fed_closed == 'closed', CAT * p.closed, NA )) ) %>%
        mutate( CAT_season = ifelse( is.na(CAT_season), 0, CAT_season ) ) %>%
        select( -c( 'p.open', 'p.closed', 'CAT' ) ) %>%
        rename( CAT = CAT_season ) %>%
        mutate( VAR = ( CV * CAT )^2 ) %>%
        select( -CV ) %>%
        
        pivot_longer( cols = c('CAT','VAR','AT','PSU','ATtotal','PSUtotal'),
                      names_to = 'METRIC', values_to = 'value' ) %>%
        
        ###   ...and, as a last step, removing the duplicated 'ATtotal' & 'PSUtotal' fields,
        ###     which are the same for 'AB1' & 'B2' summaries and so only provided once...
        mutate( CATCH_VAR = ifelse( METRIC %in% c('ATtotal','PSUtotal'), 'TOTAL', CATCH_VAR ) ) %>%
        mutate( METRIC = ifelse( METRIC %in% c('ATtotal','PSUtotal'), gsub('total','',METRIC), METRIC ) ) %>%
        select( SID, YEAR, NEW_MODEN, fed_closed, CATCH_VAR, METRIC, value ) %>%
        distinct( SID, YEAR, NEW_MODEN, fed_closed, CATCH_VAR, METRIC, value )
      
      dummy.table = blah
      rm( blah )
      
      return.table = dummy.table
      rm( dummy.table )
      
    }
  
  
  ### Assessment without SID Domains ###
  ### ----------------------------------
  
  } else {
    
    dummy.table = cv.table %>% ungroup()
    
    blah1 = dummy.table %>%
      filter( CATCH_VAR == 'TOTAL' ) %>%
      mutate( METRIC = paste0( METRIC,"total" ) ) %>%
      uncount(2)
    blah1 = blah1 %>% mutate( CATCH_VAR = rep( c('AB1','B2'), times = (dim(blah1)[1])/2 ) )
    blah2 = dummy.table %>% filter( CATCH_VAR != 'TOTAL' )
    dummy.table = bind_rows( blah1, blah2 )
    rm( blah1, blah2 )
    
    dummy.table = dummy.table %>%
      pivot_wider( names_from = METRIC, values_from = value ) %>%
      mutate( CV = ifelse( CAT==0, 0, sqrt(VAR) / CAT ) )
    
    dummy.ratios = genrec.table %>%
      filter( DS == DS.filter ) %>%
      mutate( NEW_MODEN = toupper(NEW_MODEN) )
    
    blah1 = dummy.ratios %>%
      group_by( YEAR, NEW_MODEN, fed_closed ) %>%
      summarize( AB1 = sum( AB1, na.rm=TRUE ),
                 B2  = sum(  B2, na.rm=TRUE ) ) %>%
      ungroup()
    blah2 = dummy.ratios %>%
      group_by( YEAR, fed_closed ) %>%
      summarize( AB1 = sum( AB1, na.rm=TRUE ),
                 B2  = sum(  B2, na.rm=TRUE ) ) %>%
      ungroup() %>%
      mutate( NEW_MODEN = 'TOTAL' )
    dummy.ratios = bind_rows( blah1, blah2 )
    rm( blah1, blah2 )
    
    dummy.ratios = dummy.ratios %>%
      mutate( NEW_MODEN = ifelse( NEW_MODEN == 'PRIV/SHORE', 'PRIV', NEW_MODEN ) ) %>%
      pivot_longer( cols = c('AB1','B2'), names_to = 'CATCH_VAR', values_to = 'value' ) %>%
      pivot_wider( names_from = fed_closed, values_from = value )
    
    cols = c( open=0, closed=0 )
    dummy.ratios = add_column( dummy.ratios, !!!cols[ setdiff( names(cols),names(dummy.ratios) ) ] )
    rm( cols )
    
    dummy.ratios = dummy.ratios %>%
      mutate(   open = ifelse( is.na(  open), 0,   open ),
              closed = ifelse( is.na(closed), 0, closed ) ) %>%
      filter( (open+closed) > 0 ) %>%
      mutate( p.open   =   open / (open+closed),
              p.closed = closed / (open+closed) ) %>%
      select( YEAR, NEW_MODEN, CATCH_VAR, p.open, p.closed )
    
    ###   ...joining %catches ( in 'dummy.table' ) with MRIP-CV estimates ( in 'dummy.table' )...
    dummy.table = dummy.table %>%
      left_join( dummy.ratios, by = c('YEAR','NEW_MODEN','CATCH_VAR') )
    rm( dummy.ratios )
    
    # ###   ...and as a quick check that we're not missing any strata...
    # sum( dummy.table$CAT[ is.na(dummy.table$p.open  ) ], na.rm=TRUE )
    # sum( dummy.table$CAT[ is.na(dummy.table$p.closed) ], na.rm=TRUE )
    
    blah = dummy.table %>% uncount(2)
    blah = blah %>%
      mutate( fed_closed = rep( c('open','closed'), times = (dim(blah)[1])/2 ) ) %>%
      
      mutate( CAT_season = ifelse( fed_closed == 'open'  , CAT * p.open,
                           ifelse( fed_closed == 'closed', CAT * p.closed, NA )) ) %>%
      mutate( CAT_season = ifelse( is.na(CAT_season), 0, CAT_season ) ) %>%
      select( -c( 'p.open', 'p.closed', 'CAT' ) ) %>%
      rename( CAT = CAT_season ) %>%
      mutate( VAR = ( CV * CAT )^2 ) %>%
      select( -CV ) %>%
      
      pivot_longer( cols = c('CAT','VAR','AT','PSU','ATtotal','PSUtotal'),
                    names_to = 'METRIC', values_to = 'value' ) %>%
      
      ###   ...and, as a last step, removing the duplicated 'ATtotal' & 'PSUtotal' fields,
      ###     which are the same for 'AB1' & 'B2' summaries and so only provided once...
      mutate( CATCH_VAR = ifelse( METRIC %in% c('ATtotal','PSUtotal'), 'TOTAL', CATCH_VAR ) ) %>%
      mutate( METRIC = ifelse( METRIC %in% c('ATtotal','PSUtotal'), gsub('total','',METRIC), METRIC ) ) %>%
      select( YEAR, NEW_MODEN, fed_closed, CATCH_VAR, METRIC, value ) %>%
      distinct( YEAR, NEW_MODEN, fed_closed, CATCH_VAR, METRIC, value )
    
    dummy.table = blah
    rm( blah )
    
    return.table = dummy.table
    rm( dummy.table )
    
    
  }
  
  
  return( return.table )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------


partition.forhire.effort = function( effort.table ) {
  ###     ...where 'effort.table' is the (MRIP) effort table that includes estimates for a combined for-hire fleet
  ###         (in the MATL/NATL) that are to be partitioned using the effort-based method developed during SEDAR 82...
  
  
  ###     ...for which the allocation ratios were developed as part of the S82 research track,
  ###         the final result being an excel worksheet that I manually upload...
  ratios = read.csv( 'C://Users/matthew.nuttall/Desktop/Functions/import_datasets/ForHire Partitioning Ratios.csv' )
  
  ###     ...and modify for the join with my 'mrip.effort' table...
  ratios = ratios %>%
    rename( Hbt_est = Hbt,
            Cbt_est = Cbt ) %>%
    pivot_longer( cols = c("Hbt_est","Cbt_est","Hbt_var","Cbt_var"), names_to = "variable", values_to = "value" ) %>%
    mutate( NEW_MODEN = gsub( "_.*","", variable ),
            metric    = gsub( ".*_","", variable ) ) %>%
    filter( metric == 'est' ) %>%
    rename( ratio = value ) %>%
    select( YEAR, NEW_STA, NEW_MODEN, ratio ) %>%
    
    rename( INT_YEAR = YEAR )
  ###   ...where this rename is needed for the join with 'mrip.effort' below...
  
  
  
  
  ###    I then subset the (MRIP) 'effort.table' to only include records for the combined for-hire mode
  ###         from the MATL & NATL for years 1981-2003 (after which, MRIP has generated separate estimates)...
  
  dummy = effort.table %>%
    filter( SUB_REG %in% 4:5 & NEW_MODEN == 'Cbt/Hbt' & INT_YEAR %in% 1981:2003 ) %>%
    uncount(2)
  
  ###         ...apply the allocation ratios...
  
  dummy = dummy %>%
    mutate( MODE_FX   = rep(        c(4,5) , times = (dim(dummy)[1])/2 ),
            NEW_MODE  = rep(        c(2,3) , times = (dim(dummy)[1])/2 ),
            NEW_MODEN = rep( c('Hbt','Cbt'), times = (dim(dummy)[1])/2 ) ) %>%
    
    left_join( ratios, by=c("INT_YEAR","NEW_STA","NEW_MODEN") ) %>%
    
    mutate_at( vars( ESTRIPS, CHTS_TRIPS ), list( ~ ratio * . ) ) %>%
    mutate_at( vars(  NUMVAR, CHTS_VAR_TRIPS ), list( ~ (ratio^2) * . ) ) %>%
    select( -ratio )
  
  rm( ratios )
  
  
  
  ###       ...and replace the newly separated 'Cbt' & 'Hbt' estimates in the original 'catch.table'...
  
  dummy.other = effort.table %>%
    filter( !( SUB_REG %in% 4:5 & NEW_MODEN == 'Cbt/Hbt' & INT_YEAR %in% 1981:2003 ) )
  
  effort.table = rbind( dummy.other, dummy )
  
  rm( dummy, dummy.other )
  
  
  
  return( effort.table )
  
}



