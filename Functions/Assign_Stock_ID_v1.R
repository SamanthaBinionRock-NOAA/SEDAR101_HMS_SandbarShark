

##* *Assign StockID Boundaries*

Assign_Stock_ID <- function(states, FL_sub, NC_sub, size.table, stockID, NEW_STA){
  

##* *Gulf Red Snapper*
##  Three stock boundaries:
##    (1) West -- TX & LA
##    (2) East -- MS, AL, FL1
##    (3) FLW  -- FL2 & FL3


## Start by defining a new 'state' vector, which is used to pair each NEW_STA 
## with its corresponding StockID domain

   SID.states = states

   if("FLW" %in% states) {
     loc = which( SID.states == "FLW")
     SID.states = append(SID.states, FL_sub[FL_sub %in% 1:3], after=loc-1)
     SID.states = SID.states[ !( SID.states == "FLW")]
     rm(loc)
   }

   if("FLE" %in% states) {
     loc = which( SID.states == "FLE")
     SID.states = append(SID.states, FL_sub[FL_sub %in% 4:5], after=loc-1)
     SID.states = SID.states[!( SID.states == "FLE")]
     rm(loc)
   }

   if("NC" %in% states) {
     loc = which(SID.states == "NC")
     SID.states = append( SID.states, NC_sub, after=loc-1)
     SID.states = SID.states[!(SID.states == "NC")]
     rm(loc)
   }
   
   size.table = size.table %>%
     mutate(SID = ifelse(NEW_STA %in% c("FLW","FLE"), stockID[match(FL_REG, SID.states)],
                  ifelse(NEW_STA == "NC", stockID[ match(NC_REG, SID.states)],
                   stockID[match(NEW_STA, SID.states)])))
   return(size.table)

} 
 

   
