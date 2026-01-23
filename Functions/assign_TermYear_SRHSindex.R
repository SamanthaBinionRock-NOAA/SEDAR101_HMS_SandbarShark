

### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------
###
###     DESCRIPTION
###
###   Constructed from fishery-dependent data, trends in the SRHS index can be influenced by a number of factors
###   beyond those associated with stock abundance (e.g., changes in management regulations ). As such, these
###   indices have been truncated in a number of SEDAR stock assessments. This function keeps track of these
###   decisions, and is meant to document the terminal year selected for individual stocks...
###
### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------

assign.termyear = function( new.com, region, genrec.table ) {
  ###     ...where 'new.com' and 'region' identify the stock that is being assessed...
  
  
  ### ---------------------------------------------------------------------------------------------------------
  ### SEDAR 88 -- Gulf of America RED GROUPER ###
  ###
  ###     ...Reef Fish Amendment 27 was passed in 2008 to mandate the use of circle hooks, which has been noted to increase the
  ###       catchability of red groupers ('by an order of magnitude' -- noted in the NMFS BLL Index in SEDAR 42). Coupled with the
  ###       bag limit on red grouper at this time (1 red grouper, 5 aggregate), this increase in catchability looks to be driving
  ###       the discards up in 2008. I also found it telling that in 2009, they both increased the bag limit on red grouper to 2 fish
  ###       and reduced the aggregate limit to 4 fish (Reef Fish Amendment 30B), probably to capitalize on the increased encounters
  ###       with red grouper. All of this to say that the mandate for circle hooks (in 2008) looks to have changed both the catchability
  ###       of red grouper and behavior of recreational anglers (e.g., discard rate).
  ###
  if( region == 'Gulf of America' & 'red grouper' %in% new.com ) {      truncate.year = 2007      }
  
  
  ### ---------------------------------------------------------------------------------------------------------
  ### SEDAR xx -- <Region> <SPECIES> ###
  ###
  ###     ...
  ###
  if( region == 'xxx' & 'xxx' %in% new.com ) {      truncate.year = xxx      }
  
  
  ### ---------------------------------------------------------------------------------------------------------
  
  
  return( truncate.year )
  
}



