

### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------
###
###     DESCRIPTION
###
###   ...where, in some SEDARs, some fraction of 'unidentified' catch needs to be allocated to the
###       species-of-interest. In contribution to this work, this R script defines two functions:
###
###
###   summary.unid( )
###
###       To inform (the analyst) as to what fraction (of 'unidentified' catch) is likely to be comprised of
###       the species-of-interest, this function receives a data table of catch estimates for all (identified) species/taxa
###       that could be contributing to the 'unidentified' catch record(s) and returns a summary of the relative breakdown
###       of this species-specific catch: (1) %catch by species and (2) %catch by species & year. This summary will
###       ultimately be written (to the final excel spreadsheet) as an additional summary tab, but it also serves
###       to provide the GenRec data provider with an idea of what fraction is most appropriate, which is then
###       manually input into the next function...
###
###
###   allocate.unid( )
###
###       ...which applies the chosen ratio ( as informed from the summary.unid() table ) to the 'catch.table' being
###       constructed for a given SEDAR and returns it with all necessary modifications/formatting applied...
###
###
###   revert.unid( )
###
###       ...which "backs-out" the application of any (unidentified) allocation factors, reverting the catch table
###       to only those records that were explicitly identified as the species-of-interest
###       (i.e., removing all 'unidentified' records ), which will allow for a re-evaluation of the 'current' ratios
###       to determine if they're still appropriate...
###
### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------


summary.unid = function( genrec.table, nodc.unid.catch, nodc.unid ) {
  ###     ...where 'nodc.unid' identifies those species/taxa that may be contributing to the 'unidentified' catch record(s),
  ###     ...'nodc.unid.catch' identifies the 'unidentified' taxa (e.g., at the family or genus-level),
  ###         the catch estimates of which are to be partitioned amongst those taxa identified in 'nodc.unid', and
  ###     ...'genrec.table' is a table of catch estimates for all taxa identified in 'nodc.unid', which is to be summarized
  ###         (in this function) as a relative breakdown of catch across species (collapse year) and species/year...
  
  
  
  ###   The summaries constructed in (and provided by) this function are composed of three parts:
  ###       (1) the total catch of the 'unidentified' group (by year), namely that which is to be partitioned amongst species,
  ###       (2) the relative catch of the 'identified' species/taxa ( including the assessed species ) by species, and
  ###       (3) the relative catch of the 'identified' species/taxa ( including the assessed species ) by species and year.
  ###   Note that AB1.wgt is not included in this analysis because allocations (when needed) are applied to catch-in-numbers,
  ###       after which AB1.wgt estimates are repulled (wgt estimation may be rerun to include avg.wgt of unidentified catch)...
  
  
  ###   Starting with (1) -- 'unidentified' catch-by-year (AB1 & B2)
  unid.catch = genrec.table[ which( genrec.table$SP_CODE %in% nodc.unid.catch ), ] %>%
    group_by( YEAR ) %>%
    summarize( AB1 = sum( as.numeric(AB1), na.rm=TRUE ),
               B2  = sum( as.numeric(B2), na.rm=TRUE ) ) %>%
    # AB1.wgt = sum( as.numeric(lbsest_SECwwt), na.rm=TRUE ) ) %>%
    select( YEAR, AB1, B2 ) %>%
    arrange( YEAR )
  unid.catch = as.data.frame( unid.catch )
  
  
  ###   Moving on to (2) -- 'identified' catch-by-species (collapsed across year)
  ratio.spp = genrec.table[ which( genrec.table$SP_CODE %notin% nodc.unid.catch ), ] %>%
    group_by( NEW_COM ) %>%
    summarise( AB1 = sum( AB1, na.rm=TRUE ),
               B2  = sum(  B2, na.rm=TRUE ) )
               # AB1.wgt = sum( as.numeric(lbsest_SECwwt), na.rm=TRUE ) )

  dummy = colSums( ratio.spp[ ,c('AB1','B2')] )
  
  ratio.spp = ratio.spp %>%
    mutate( AB1.total = dummy['AB1'],
             B2.total = dummy['B2']  ) %>%
    mutate( p.AB1 = ifelse( AB1.total==0, NA, AB1 / AB1.total ),
            p.B2  = ifelse(  B2.total==0, NA,  B2 /  B2.total ) ) %>%
            # p.AB1.wgt = AB1.wgt / LBS.total ) ) %>%
    select( !c(AB1.total,B2.total) )
  rm(dummy)
  
  
  ###     ...and to (3) -- 'identified' catch by-species and by-year
  ratio.year = genrec.table[ which( genrec.table$SP_CODE %notin% nodc.unid.catch ), ] %>%
    group_by( NEW_COM, YEAR ) %>%
    summarise( AB1 = sum( AB1, na.rm=TRUE ),
               B2  = sum(  B2, na.rm=TRUE ) )
               # AB1.wgt = sum( as.numeric(lbsest_SECwwt), na.rm=TRUE ) )
  
  dummy = ratio.year %>%
    group_by( YEAR ) %>%
    summarise( AB1.total = sum( AB1, na.rm=TRUE ),
               B2.total  = sum(  B2, na.rm=TRUE ) )
               # LBS.total = sum( as.numeric(lbsest_SECwwt), na.rm=TRUE ) )
  
  ratio.year = ratio.year %>%
    full_join( dummy, by = 'YEAR' ) %>%
    mutate( p.AB1 = AB1 / AB1.total,
            p.B2  =  B2 /  B2.total ) %>%
            # p.AB1.wgt = AB1.wgt / LBS.total ) ) %>%
    select( !c(AB1.total,B2.total) )
  rm(dummy)
  
  
  
  ###   Lastly, I need to format the three tables (above) before joining them into the (final) 'unid.table' summary
  ###     (i.e., that printed to our final excel spreadsheet ), which will be of the form:
  ###
  ###              UNID CATCH      ID CATCH #1      ID CATCH #2     ID CATCH #3
  ###             -- NEWCOM --    -- NEWCOM --     -- NEWCOM --    -- NEWCOM --
  ###
  ###      YEAR     AB1   B2        AB1   B2        AB1   B2        AB1   B2
  ###
  ###     TOTAL     ...   ...       ...   ...       ...   ...       ...   ...
  ###
  ###      1981     ...   ...       ...   ...       ...   ...       ...   ...
  ###      1982     ...   ...       ...   ...       ...   ...       ...   ...
  ###      1983     ...   ...       ...   ...       ...   ...       ...   ...
  ###     < and so on... >          < and so on... >          < and so on... >
  
  
  ###     Starting with ( 1 = 'unid.catch' ), which is comprised of the 'unidentified' catch estimates
  ###     that are to be partitioned ( and represented by the first two columns in our summary 'unid.table' ),
  ###     the associated 'unid.catch' table is already set-up in the desired format (i.e., rows = YEAR, columns = AB1 & B2 )
  ###     and so no additional adjustments to formatting need to be applied. However, the current 'unid.catch' table
  ###     is missing the TOTAL summary (i.e., sum(catch) across years ), which I add now...
  
  unid.catch = unid.catch %>% mutate( YEAR = as.character(YEAR) )
  dummy = data.frame( YEAR = 'TOTAL', AB1 = sum( unid.catch$AB1,na.rm=TRUE ), B2 = sum( unid.catch$B2,na.rm=TRUE ) )
  unid.catch = rbind( dummy, unid.catch )
  rm(dummy)
  
  
  ###     Moving onto (2) and (3), which are to be printed into the "TOTAL" and YEAR-specific columns of 'unid.table' respectively,
  ###     we need to convert these tables from a long format to a wide format, using the NEW_COM field to perform the pivot.
  ###     Note that I also make sure that the species-of-interest is printed first in these summaries, as that is likely
  ###     to be of the most interest to assessment analysts...
  
  spp.order = ratio.spp$NEW_COM
  spp.order = c( as.character(new.com), as.character(spp.order[spp.order %notin% new.com]) )
  
  col.order = spp.order
  for( i in 1:length(spp.order) ) {
    col.order[ ((i-1)*4) + 1 ] = paste0( spp.order[i],"_AB1" )
    col.order[ ((i-1)*4) + 2 ] = paste0( spp.order[i],"_B2"  )
    col.order[ ((i-1)*4) + 3 ] = paste0( spp.order[i],"_p.AB1" )
    col.order[ ((i-1)*4) + 4 ] = paste0( spp.order[i],"_p.B2"  )
  }

  ratio.spp = ratio.spp %>%
    # arrange( factor( NEW_COM, levels = spp.order ) ) %>%
    # ###     ...which doesn't do anything as pivot_wider() only sorts columns based on what elements are seen first
    # ###         (factor level doesn't impact sorting). Instead, I apply a select() statement to get the desired sorting...
    pivot_wider( names_from = NEW_COM , values_from = c( AB1, B2, p.AB1, p.B2 ), names_glue = "{NEW_COM}_{.value}" ) %>%
    select( contains( col.order ) )
  
  ratio.year = ratio.year %>%
    pivot_wider( names_from = NEW_COM , values_from = c( AB1, B2, p.AB1, p.B2 ), names_glue = "{NEW_COM}_{.value}" ) %>%
    select( YEAR, contains( col.order ) )
  
  rm( col.order, spp.order )
  
  
  ### FINAL MERGE/JOIN ###
  ###
  ###   ...in preparation of joining the three summary tables above, we make sure all fields are formatted the same way...
  
  ###   The 'ratio.spp' table (which is a summary across years) is missing a YEAR field, which we set = 'TOTAL'...
  dummy = data.frame( YEAR = 'TOTAL' )
  ratio.spp = cbind( dummy, ratio.spp )
  rm( dummy )
  
  ###   Similarly, we also change the format of YEAR in 'ratio.year' to character, which is how it is be defined in 'ratio.spp'...
  ratio.year = ratio.year %>% mutate( YEAR = as.character(YEAR) )
  
  ###   With the two 'ratio' tables appropriately defined, we then join them...
  ratio.table = rbind( ratio.spp, ratio.year )
  rm( ratio.spp, ratio.year )
  
  ###   ...and join this combined 'ratio' table with 'unid.catch' to produce our final summary table, which is arranged by YEAR...
  unid.table = full_join( unid.catch, ratio.table, by='YEAR' ) %>% arrange( YEAR )
  rm( unid.catch, ratio.table )
  
  
  return( unid.table )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------


###   Note that the fraction by which 'unidentified' catch is reduced in this process (i.e., 'unid.ratio' )
###   is manually-defined in the function below. This is because SEDAR 'Best Practices' do not currently exist
###   for identifying the most appropriate estimate an allocation ratio (for unidentified catch).
###   However, if such guidance is ever developed, this script can be updated to automate such decisions.
###   The coding will obviously depend on the guidance, but I suspect it would require an additional function that:
###       (1) imports the summary table constructed above ( from summary.unid ), which may also require an update
###           depending on what information it needs to communicate, and
###       (2) apply whatever calculations are needed to determine an appropriate 'unid.ratio', which can then be
###           applied to and update our final 'catch.table' object (i.e., imported into allocate.unid )
###   Until such a time, the appropriate 'unid.ratio' is simply manually fed into the allocate.unid() function below...


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------


allocate.unid = function( genrec.table, unid.ratio, nodc.unid.catch, avgwgt.dir ) {
  ###     ...where 'genrec.table' is the 'catch.table' of estimates (for this particular SEDAR), some of which
  ###           represent records of 'unidentified' catch ( as identified by 'nodc.unid.catch' ) that need to be
  ###           reduced to a level assumed appropriate for the species-of-interest. To that end...
  ###     ...'unid.ratio' is the USER-DEFINED fraction of UNID catch that is believed to be composed of the assessed species.
  ###           Note that 'unid.ratio' can be a single number, in that all (unidentified) catch records are scaled
  ###           using the same number, or multiple numbers wherein different conversions are applied (e.g., AB1 vs. B2 )...
  ###     ...'avgwgt.dir' identifies the directory within which SEFSC avgwgt estimates from the most recent ACL files
  ###           are stored ( as needed when updating the lbsest_SEC fields in our new 'catch.table' object )
  
  
  ### Identify UNID Rows ###
  ### ----------------------
  
  ###   I start by identifying which rows need to be updated (i.e., those representing 'unidentified' catch )...
  rows = which( genrec.table$SP_CODE %in% as.character(nodc.unid.catch) )
  
  genrec.table$UNID_FLAG = NA
  genrec.table$UNID_FLAG[ rows ] = "Y"
  ###     ...which is a flag to distinguish records actually recorded as the species-of-interest
  ###         (i.e., the SEDAR-assessed species ) vs. those assumed to be (i.e., originally unidentified )...
  
  
  
  ### Update Species-Specific Fields ###
  ### ----------------------------------
  
  ###   With the appropriate rows/records identified, I then go field-by-field, starting with those
  ###   that carry species-specific information ( which needs to be updated to reflect the species-of-interest )...
  
  genrec.table$SP_CODE[ rows ]      = as.character( nodc.code[1] )   # '8860020201'
  genrec.table$NEW_COM[ rows ]      = as.character( new.com[1]   )   # "gray triggerfish"
  genrec.table$NEW_SCI[ rows ]      = as.character( new.sci[1]   )   # "Balistes capriscus"
  genrec.table$SPECIES_ITIS[ rows ]    = as.character( itis.code[1] )   # '173138'
  #genrec.table$SPECIES_CODE[ rows ] = as.character( tpwd.code[1] )   # '507'  -- TPWD species  - TPWD code is not a retained field
  
  ###   ...and for those species-ID fields that I typically don't populate (i.e., not a part of the GenRec workflow ),
  ###         we refer back to our 'spp.info' table and extract the relevant information...
  genrec.table$SPECIES[ rows ]   = as.character( srhs.code.info( taxa = taxa[1],
                                                 spp.field = 'COMMON', spp.table = spp.info ) )    # '77'  -- SRHS species code
  genrec.table$GOM_LABEL[ rows ] = as.character( spp.info$GOM_LABEL[
                                          grep( paste0("^",taxa[1],"$"), spp.info[,'COMMON'] ) ] ) # "Reef Fish" -- Manage. Label
  genrec.table$SA_LABEL[  rows ] = as.character( spp.info$SA_LABEL[
                                          grep( paste0("^",taxa[1],"$"), spp.info[,'COMMON'] ) ] ) # "Snapper Grouper" - Manage. Label
  
  
  ### Catch-in-Numbers ###
  ### --------------------
  
  if( length(unid.ratio) == 1 ) {     ### ...same ratio applied to all catch records
    
    genrec.table$AB1[ rows ] = genrec.table$AB1[ rows ] * unid.ratio
    genrec.table$B2[  rows ] = genrec.table$B2[  rows ] * unid.ratio
    genrec.table$A[   rows ] = genrec.table$A[   rows ] * unid.ratio
    genrec.table$B1[  rows ] = genrec.table$B1[  rows ] * unid.ratio
    #genrec.table$CHTS_CL[ rows ] = genrec.table$CHTS_CL[ rows ] * unid.ratio
    #genrec.table$CHTS_H[  rows ] = genrec.table$CHTS_H[  rows ] * unid.ratio
    #genrec.table$CHTS_RL[ rows ] = genrec.table$CHTS_RL[ rows ] * unid.ratio
    
    genrec.table$VAR_AB1[ rows ] = genrec.table$VAR_AB1[ rows ] * (unid.ratio^2)
    genrec.table$VAR_B2[  rows ] = genrec.table$VAR_B2[  rows ] * (unid.ratio^2)
    #genrec.table$CHTS_VAR_CL[ rows ] = genrec.table$CHTS_VAR_CL[ rows ] * (unid.ratio^2)
    #genrec.table$CHTS_VAR_H[  rows ] = genrec.table$CHTS_VAR_H[  rows ] * (unid.ratio^2)
    #genrec.table$CHTS_VAR_RL[ rows ] = genrec.table$CHTS_VAR_RL[ rows ] * (unid.ratio^2)
    
  } else {
    
    genrec.table$AB1[ rows ] = genrec.table$AB1[ rows ] * unid.ratio$AB1
    genrec.table$B2[  rows ] = genrec.table$B2[  rows ] * unid.ratio$B2
    genrec.table$A[   rows ] = genrec.table$A[   rows ] * unid.ratio$AB1
    genrec.table$B1[  rows ] = genrec.table$B1[  rows ] * unid.ratio$AB1
    #genrec.table$CHTS_CL[ rows ] = genrec.table$CHTS_CL[ rows ] * unid.ratio$AB1
    #genrec.table$CHTS_H[  rows ] = genrec.table$CHTS_H[  rows ] * unid.ratio$AB1
    #genrec.table$CHTS_RL[ rows ] = genrec.table$CHTS_RL[ rows ] * unid.ratio$B2
    
    genrec.table$VAR_AB1[ rows ] = genrec.table$VAR_AB1[ rows ] * (unid.ratio$AB1^2)
    genrec.table$VAR_B2[  rows ] = genrec.table$VAR_B2[  rows ] * (unid.ratio$B2^2)
    #genrec.table$CHTS_VAR_CL[ rows ] = genrec.table$CHTS_VAR_CL[ rows ] * (unid.ratio$AB1^2)
    #genrec.table$CHTS_VAR_H[  rows ] = genrec.table$CHTS_VAR_H[  rows ] * (unid.ratio$AB1^2)
    #genrec.table$CHTS_VAR_RL[ rows ] = genrec.table$CHTS_VAR_RL[ rows ] * (unid.ratio$B2^2)
  }
  
  
  
  ### Landings-in-Weight ###
  ### ----------------------
  ###
  ###     As per my chat with Vivian, the lbsest_SEC estimates for those catch records previously allocated as
  ###     'unidentified' will be populated not from 'unid.ratio', but by updating the AVGWGT (and NUMWGT) fields
  ###     to those estimated for the species-of-interest (vs. that imported for the 'unidentified' group ) and
  ###     then recalculating the lbsest_SEC fields ( AB1 * AVGWGT, where AB1 has already been updated based on 'unid.ratio' ).
  ###     Therefore, I start by importing the appropriate SEFSC avgwgt estimates to genrec.table...
  ###
  ###     These avgwgt files are then (sequentially) joined with our 'genrec.table', for which we need to define
  ###     the strata to be used in each join (i.e., as identified in 'dummy_label' )...

  load(paste0(avgwgt.dir,"/sandbar_s.RData" ))
  
  avgwgt.s.file = sandbar_s
  avgwgt.s.file$dummy_label = paste0( avgwgt.s.file$NEW_COM )
  genrec.table$dummy_label  = paste0(  genrec.table$NEW_COM )
  genrec.table$S_AVG_WGT[ rows ] = avgwgt.s.file$avgwgt_s[ match( genrec.table$dummy_label[ rows ], avgwgt.s.file$dummy_label ) ]
  genrec.table$S_SAMPLE_SIZE[ rows ] = avgwgt.s.file$numwgt_s[ match( genrec.table$dummy_label[ rows ], avgwgt.s.file$dummy_label ) ]
  #genrec.table$AVGWGT_S[ rows ] = avgwgt.s.file$avgwgt_s[ match( genrec.table$dummy_label[ rows ], avgwgt.s.file$dummy_label ) ]
  #genrec.table$NUMWGT_S[ rows ] = avgwgt.s.file$numwgt_s[ match( genrec.table$dummy_label[ rows ], avgwgt.s.file$dummy_label ) ]
  genrec.table = genrec.table %>% select( !dummy_label )
  rm( avgwgt.s.file )
  
  
  load(paste0(avgwgt.dir,"/sandbar_sr.RData" ))
  
  avgwgt.sr.file = sandbar_sr
  avgwgt.sr.file$dummy_label = paste0( avgwgt.sr.file$NEW_COM,"_",avgwgt.sr.file$SUB_REG )
  genrec.table$dummy_label   = paste0(   genrec.table$NEW_COM,"_",  genrec.table$SUB_REG )
  genrec.table$SR_AVG_WGT[ rows ] = avgwgt.sr.file$avgwgt_sr[ match( genrec.table$dummy_label[ rows ], avgwgt.sr.file$dummy_label ) ]
  genrec.table$SR_SAMPLE_SIZE[ rows ] = avgwgt.sr.file$numwgt_sr[ match( genrec.table$dummy_label[ rows ], avgwgt.sr.file$dummy_label ) ]
  #genrec.table$AVGWGT_SR[ rows ] = avgwgt.sr.file$avgwgt_sr[ match( genrec.table$dummy_label[ rows ], avgwgt.sr.file$dummy_label ) ]
  #genrec.table$NUMWGT_SR[ rows ] = avgwgt.sr.file$numwgt_sr[ match( genrec.table$dummy_label[ rows ], avgwgt.sr.file$dummy_label ) ]
  genrec.table = genrec.table %>% select( !dummy_label )
  rm( avgwgt.sr.file )
  
  
  load(paste0(avgwgt.dir,"/sandbar_sry.RData" ))
  
  avgwgt.sry.file = sandbar_sry
  avgwgt.sry.file$dummy_label = paste0( avgwgt.sry.file$NEW_COM,"_",avgwgt.sry.file$SUB_REG,"_",avgwgt.sry.file$year )
  genrec.table$dummy_label    = paste0(    genrec.table$NEW_COM,"_",   genrec.table$SUB_REG,"_",   genrec.table$YEAR )
  genrec.table$SRY_AVG_WGT[ rows ] = avgwgt.sry.file$avgwgt_sry[ match( genrec.table$dummy_label[ rows ], avgwgt.sry.file$dummy_label ) ]
  genrec.table$SRY_SAMPLE_SIZE[ rows ] = avgwgt.sry.file$numwgt_sry[ match( genrec.table$dummy_label[ rows ], avgwgt.sry.file$dummy_label ) ]
  #genrec.table$AVGWGT_SRY[ rows ] = avgwgt.sry.file$avgwgt_sry[ match( genrec.table$dummy_label[ rows ], avgwgt.sry.file$dummy_label ) ]
  #genrec.table$NUMWGT_SRY[ rows ] = avgwgt.sry.file$numwgt_sry[ match( genrec.table$dummy_label[ rows ], avgwgt.sry.file$dummy_label ) ]
  genrec.table = genrec.table %>% select( !dummy_label )
  rm( avgwgt.sry.file )
  
  
  
  load(paste0(avgwgt.dir,"/sandbar_srys.RData" ))
  
  avgwgt.srys.file = sandbar_srys
  avgwgt.srys.file$dummy_label = paste0( avgwgt.srys.file$NEW_COM,"_",avgwgt.srys.file$SUB_REG,"_",avgwgt.srys.file$year,"_",
                                         avgwgt.srys.file$NEW_STA )
  genrec.table$dummy_label     = paste0(     genrec.table$NEW_COM,"_",    genrec.table$SUB_REG,"_",    genrec.table$YEAR,"_",
                                             genrec.table$NEW_STA )
  genrec.table$SRYS_AVG_WGT[ rows ] = avgwgt.srys.file$avgwgt_srys[ match( genrec.table$dummy_label[ rows ], avgwgt.srys.file$dummy_label ) ]
  genrec.table$SRYS_SAMPLE_SIZE[ rows ] = avgwgt.srys.file$numwgt_srys[ match( genrec.table$dummy_label[ rows ], avgwgt.srys.file$dummy_label ) ]
  #genrec.table$AVGWGT_SRYS[ rows ] = avgwgt.srys.file$avgwgt_srys[ match( genrec.table$dummy_label[ rows ], avgwgt.srys.file$dummy_label ) ]
  #genrec.table$NUMWGT_SRYS[ rows ] = avgwgt.srys.file$numwgt_srys[ match( genrec.table$dummy_label[ rows ], avgwgt.srys.file$dummy_label ) ]
  genrec.table = genrec.table %>% select( !dummy_label )
  rm( avgwgt.srys.file )
  
  
  
  load(paste0(avgwgt.dir,"/sandbar_srysm.RData" ))
  
  avgwgt.srysm.file = sandbar_srysm
  avgwgt.srysm.file$dummy_label = paste0( avgwgt.srysm.file$NEW_COM,"_",avgwgt.srysm.file$SUB_REG,"_",avgwgt.srysm.file$year,"_",
                                          avgwgt.srysm.file$NEW_STA,"_",avgwgt.srysm.file$NEW_MODEN )
  genrec.table$dummy_label      = paste0(      genrec.table$NEW_COM,"_",     genrec.table$SUB_REG,"_",     genrec.table$YEAR,"_",
                                               genrec.table$NEW_STA,"_",     genrec.table$NEW_MODEN )
  genrec.table$SRYSM_AVG_WGT[ rows ] = avgwgt.srysm.file$avgwgt_srysm[ match( genrec.table$dummy_label[ rows ], avgwgt.srysm.file$dummy_label ) ]
  genrec.table$SRYSM_SAMPLE_SIZE[ rows ] = avgwgt.srysm.file$numwgt_srysm[ match( genrec.table$dummy_label[ rows ], avgwgt.srysm.file$dummy_label ) ]
  #genrec.table$AVGWGT_SRYSM[ rows ] = avgwgt.srysm.file$avgwgt_srysm[ match( genrec.table$dummy_label[ rows ], avgwgt.srysm.file$dummy_label ) ]
  #genrec.table$NUMWGT_SRYSM[ rows ] = avgwgt.srysm.file$numwgt_srysm[ match( genrec.table$dummy_label[ rows ], avgwgt.srysm.file$dummy_label ) ]
  genrec.table = genrec.table %>% select( !dummy_label )
  rm( avgwgt.srysm.file )
  
  
  
  load(paste0(avgwgt.dir,"/sandbar_srysmw.RData" ))
  
  avgwgt.srysmw.file = sandbar_srysmw
  avgwgt.srysmw.file$dummy_label = paste0( avgwgt.srysmw.file$NEW_COM,"_",avgwgt.srysmw.file$SUB_REG,  "_",avgwgt.srysmw.file$year,"_",
                                           avgwgt.srysmw.file$NEW_STA,"_",avgwgt.srysmw.file$NEW_MODEN,"_",avgwgt.srysmw.file$WAVE )
  genrec.table$dummy_label       = paste0(       genrec.table$NEW_COM,"_",      genrec.table$SUB_REG,  "_",      genrec.table$YEAR,"_",
                                                 genrec.table$NEW_STA,"_",      genrec.table$NEW_MODEN,"_",      genrec.table$WAVE )
  genrec.table$SRYSMW_AVG_WGT[ rows ] = avgwgt.srysmw.file$avgwgt_srysmw[ match( genrec.table$dummy_label[ rows ], avgwgt.srysmw.file$dummy_label ) ]
  genrec.table$SRYSMW_SAMPLE_SIZE[ rows ] = avgwgt.srysmw.file$numwgt_srysmw[ match( genrec.table$dummy_label[ rows ], avgwgt.srysmw.file$dummy_label ) ]
  #genrec.table$AVGWGT_SRYSMW[ rows ] = avgwgt.srysmw.file$avgwgt_srysmw[ match( genrec.table$dummy_label[ rows ], avgwgt.srysmw.file$dummy_label ) ]
  #genrec.table$NUMWGT_SRYSMW[ rows ] = avgwgt.srysmw.file$numwgt_srysmw[ match( genrec.table$dummy_label[ rows ], avgwgt.srysmw.file$dummy_label ) ]
  genrec.table = genrec.table %>% select( !dummy_label )
  rm( avgwgt.srysmw.file )
  
  
  load(paste0(avgwgt.dir,"/sandbar_srysmwa.RData" ))
  
  avgwgt.srysmwa.file = sandbar_srysmwa
  avgwgt.srysmwa.file$dummy_label = paste0( avgwgt.srysmwa.file$NEW_COM,"_",avgwgt.srysmwa.file$SUB_REG,  "_",avgwgt.srysmwa.file$year,"_",
                                            avgwgt.srysmwa.file$NEW_STA,"_",avgwgt.srysmwa.file$NEW_MODEN,"_",avgwgt.srysmwa.file$WAVE,"_",
                                            avgwgt.srysmwa.file$NEW_AREAN )
  genrec.table$dummy_label        = paste0(        genrec.table$NEW_COM,"_",       genrec.table$SUB_REG,  "_",       genrec.table$YEAR,"_",
                                                   genrec.table$NEW_STA,"_",       genrec.table$NEW_MODEN,"_",       genrec.table$WAVE,"_",
                                                   genrec.table$NEW_AREAN )
  genrec.table$SRYSMWA_AVG_WGT[ rows ] = avgwgt.srysmwa.file$avgwgt_srysmwa[ match( genrec.table$dummy_label[ rows ], avgwgt.srysmwa.file$dummy_label ) ]
  genrec.table$SRYSMWA_SAMPLE_SIZE[ rows ] = avgwgt.srysmwa.file$numwgt_srysmwa[ match( genrec.table$dummy_label[ rows ], avgwgt.srysmwa.file$dummy_label ) ]
  #genrec.table$AVGWGT_SRYSMWA[ rows ] = avgwgt.srysmwa.file$avgwgt_srysmwa[ match( genrec.table$dummy_label[ rows ], avgwgt.srysmwa.file$dummy_label ) ]
  #genrec.table$NUMWGT_SRYSMWA[ rows ] = avgwgt.srysmwa.file$numwgt_srysmwa[ match( genrec.table$dummy_label[ rows ], avgwgt.srysmwa.file$dummy_label ) ]
  genrec.table = genrec.table %>% select( !dummy_label )
  rm( avgwgt.srysmwa.file )
  
  
  ###   We then go row-by-row, identifying the SEFSC avgwgt estimates most appropriate for a given strata
  ###   ( as recorded in 'lbsest_SECsource' ) and saving it to a new 'AVGWGT_SEC' field such that it can then
  ###   be multiplied by the associated AB1 estimate ( to update our 'lbest_SECwwt' estimates )...
  
  n.size.threshold = 15
  
  genrec.table$AVGWGT_SEC = 0
  
  for( i in 1:length(rows) ) {
    
    if( !is.na(genrec.table$SRYSMWA_SAMPLE_SIZE[rows[i]]) & genrec.table$SRYSMWA_SAMPLE_SIZE[rows[i]] >= n.size.threshold ) {
      genrec.table$LBSEST_SECSOURCE[rows[i]] = "srysmwa"
      genrec.table$AVGWGT_SEC[rows[i]] = genrec.table$SRYSMWA_AVG_WGT[rows[i]]
    } else if( !is.na(genrec.table$SRYSMW_SAMPLE_SIZE[rows[i]]) & genrec.table$SRYSMW_SAMPLE_SIZE[rows[i]] >= n.size.threshold ) {
      genrec.table$LBSEST_SECSOURCE[rows[i]] = "srysmw"
      genrec.table$AVGWGT_SEC[rows[i]] = genrec.table$SRYSMW_AVG_WGT[rows[i]]
    } else if( !is.na(genrec.table$SRYSM_SAMPLE_SIZE[rows[i]]) & genrec.table$SRYSM_SAMPLE_SIZE[rows[i]] >= n.size.threshold ) {
      genrec.table$LBSEST_SECSOURCE[rows[i]] = "srysm"
      genrec.table$AVGWGT_SEC[rows[i]] = genrec.table$SRYSM_AVG_WGT[rows[i]]
    } else if( !is.na(genrec.table$SRYS_SAMPLE_SIZE[rows[i]]) & genrec.table$SRYS_SAMPLE_SIZE[rows[i]] >= n.size.threshold ) {
      genrec.table$LBSEST_SECSOURCE[rows[i]] = "srys"
      genrec.table$AVGWGT_SEC[rows[i]] = genrec.table$SRYS_AVG_WGT[rows[i]]
    } else if( !is.na(genrec.table$SRY_SAMPLE_SIZE[rows[i]]) & genrec.table$SRY_SAMPLE_SIZE[rows[i]] >= n.size.threshold ) {
      genrec.table$LBSEST_SECSOURCE[rows[i]] = "sry"
      genrec.table$AVGWGT_SEC[rows[i]] = genrec.table$SRY_AVG_WGT[rows[i]]
    } else if( !is.na(genrec.table$SR_SAMPLE_SIZE[rows[i]]) & genrec.table$SR_SAMPLE_SIZE[rows[i]] >= n.size.threshold ) {
      genrec.table$LBSEST_SECSOURCE[rows[i]] = "sr"
      genrec.table$AVGWGT_SEC[rows[i]] = genrec.table$SR_AVG_WGT[rows[i]]
    } else {
      genrec.table$LBSEST_SECSOURCE[rows[i]] = "s"
      genrec.table$AVGWGT_SEC[rows[i]] = genrec.table$S_AVG_WGT[rows[i]]
    }
    
  }
  
  genrec.table$lbsest_SECwwt[ rows ] = genrec.table$AB1[ rows ] * genrec.table$AVGWGT_SEC[ rows ]
  genrec.table = genrec.table %>% select( !AVGWGT_SEC )
  
  rm( rows, n.size.threshold )
  
  
  return( genrec.table )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------


revert.unid = function( genrec.table, unid.dat, nodc.unid.catch, unid.ratio.prev, new.com.unid, new.sci.unid ) {
  ###     ...where 'genrec.table' is the 'catch.table' of estimates (for this particular SEDAR), some of which
  ###           represent estimates that were originally recorded as 'unidentified' catch that are to be
  ###           identified and removed. To that end...
  ###     ...'unid.dat' is the table of catch records that was just pulled to include both unidentified taxa and
  ###           identified species groups, which will be filtered to only include the 'unidentified' taxa using the
  ###        'nodc.unid.catch' object. This table is important as its used to identify the strata at which
  ###           the unidentified catch records were coming, which is then matched to the strata in 'genrec.table'
  ###           to isolate those records that were originally unidentified...
  ###     ...'unid.ratio.prev' is the (known) fraction of UNID catch that was assumed to be composed of the
  ###           assessed species in the last/previous SEDAR stock assessment, which is used to "back-calculate"
  ###           the original identified vs. unidentified catch estimates and "back-out" of the previous approach...
  ###     ...and where 'new.com.unid' and 'new.sci.unid' are used to update these species-ID fields in the final
  ###           'unid.dat' table returned from this function...
  
  
  
  ### Identifying UNID Catch Records ###
  ### ----------------------------------
  
  ###   I start with the 'unid.dat' table, filtering it to only include 'unidentified' catch records as an indication
  ###   of the strata where UNID catch estimates have been added for the species-of-interest (i.e., in 'genrec.table' ).
  ###   Note that this approach only works if some UNID catch still remains in 'unid.dat', and so may not work if
  ###   the (unidentified) allocation ratio that has been applied is 100%...
  
  unid.dummy = unid.dat %>%
    filter( SP_CODE %in% as.character(nodc.unid.catch) ) %>%
    
    ###   ..."back-calculate" the original catch estimates for unidentified groups...
    mutate( AB1.orig.unid = AB1 / (1-unid.ratio.prev),
             B2.orig.unid =  B2 / (1-unid.ratio.prev) ) %>%
    ###   ...the amount of this unidentified catch estimate that was assigned to the species-of-interest...
    mutate( AB1.assign.id   = AB1.orig.unid * unid.ratio.prev,
             B2.assign.id   =  B2.orig.unid * unid.ratio.prev ) %>%
    
    ###   ...and add a STRATA.ID field that will be needed on the merge with 'genrec.table' (below),
    ###     which also includes the associated catch estimates (that were assigned to the species-of-interest)
    ###     as its possible for a single strata to have both unidentified and identified catches, such that
    ###     we need to include catch info for this distinction...
    mutate( STRATAID = paste0( YEAR,MONTH,WAVE, "_", SUB_REG,NEW_STA,FL_REG,NC_REG, "_", NEW_MODEN, "_", NEW_AREAN, "_",
                               round( AB1.assign.id,0 ), "_", round( B2.assign.id,0 ) ) ) %>%
    # summary( as.numeric( unid.dummy$AB1.orig.unid - (unid.dummy$AB1 + unid.dummy$AB1.assign.id) ) )
    # summary( as.numeric( unid.dummy$B2.orig.unid  - (unid.dummy$B2  + unid.dummy$B2.assign.id ) ) )
    
    select( STRATAID, AB1.orig.unid, B2.orig.unid, AB1.assign.id, B2.assign.id )
  
  
  
  ### Back-Calculating UNID Catch ###
  ### -------------------------------
  
  ###   Knowing which catch records were originally recorded as unidentified (as per 'unid.dummy'), and what the
  ###   original estimates were, I then merge this information back with 'genrec.table' to properly differentiate
  ###   which catch records were originally identified to species vs. those from an unidentified record...
  
  catch.dummy = genrec.table %>%
    
    mutate( STRATAID = paste0( YEAR,MONTH,WAVE, "_", SUB_REG,NEW_STA,FL_REG,NC_REG, "_", NEW_MODEN, "_", NEW_AREAN, "_",
                               round( AB1,0 ), "_", round( B2,0 ) ) ) %>%
    
    left_join( unid.dummy, by='STRATAID' )
  
  # ###   ...and just to make sure this join is being done properly...
  # dim( unid.dummy )[1] - dim( catch.dummy[ which( !is.na(catch.dummy$AB1.orig.unid) ), ] )[1]
  # summary( as.numeric( catch.dummy$AB1.orig.unid ) )
  
  
  ### UNID Catch Table ###
  
  catch.unid = catch.dummy %>%
    
    filter( !is.na(AB1.orig.unid) & !is.na(B2.orig.unid) ) %>%
    select( -one_of( c( 'STRATAID', 'AB1.orig.unid','B2.orig.unid', 'AB1.assign.id','B2.assign.id' ) ) )
  
  ###   ...for which we rename (some of) the species ID fields as those associated with the unidentified taxa...
  catch.unid$SP_CODE = as.character( nodc.unid.catch )
  catch.unid$NEW_COM = as.character( new.com.unid    )
  catch.unid$NEW_SCI = as.character( new.sci.unid    )
  catch.unid$ITIS_CODE = as.character( itis.unid     )
  
  catch.unid$SPECIES_CODE = NA   # TPWD species code
  catch.unid$SPECIES      = NA   # SRHS species code
  
  # catch.unid$GOM_LABEL = as.character( spp.info$GOM_LABEL[
  #   grep( paste0("^",taxa.unid.catch,"$"), spp.info[,'COMMON'] ) ] ) # "Reef Fish" -- Manage. Label
  # catch.unid$SA_LABEL = as.character( spp.info$SA_LABEL[
  #   grep( paste0("^",taxa.unid.catch,"$"), spp.info[,'COMMON'] ) ] ) # "Snapper Grouper" - Manage. Label
  
  
  ###   ...within which I then "back-calculate" the original values of unidentified catch...
  
  if( length(unid.ratio.prev) == 1 ) {
    
    catch.unid$AB1 = catch.unid$AB1 / unid.ratio.prev
    catch.unid$B2  = catch.unid$B2  / unid.ratio.prev
    catch.unid$A   = catch.unid$A   / unid.ratio.prev
    catch.unid$B1  = catch.unid$B1  / unid.ratio.prev
    catch.unid$CHTS_CL = catch.unid$CHTS_CL / unid.ratio.prev
    catch.unid$CHTS_H  = catch.unid$CHTS_H  / unid.ratio.prev
    catch.unid$CHTS_RL = catch.unid$CHTS_RL / unid.ratio.prev
    
    catch.unid$VAR_AB1 = catch.unid$VAR_AB1 / (unid.ratio.prev^2)
    catch.unid$VAR_B2  = catch.unid$VAR_B2  / (unid.ratio.prev^2)
    catch.unid$CHTS_VAR_CL = catch.unid$CHTS_VAR_CL / (unid.ratio.prev^2)
    catch.unid$CHTS_VAR_H  = catch.unid$CHTS_VAR_H  / (unid.ratio.prev^2)
    catch.unid$CHTS_VAR_RL = catch.unid$CHTS_VAR_RL / (unid.ratio.prev^2)
    
  } else {
    
    catch.unid$AB1 = catch.unid$AB1 / unid.ratio.prev$AB1
    catch.unid$B2  = catch.unid$B2  / unid.ratio.prev$B2
    catch.unid$A   = catch.unid$A   / unid.ratio.prev$AB1
    catch.unid$B1  = catch.unid$B1  / unid.ratio.prev$AB1
    catch.unid$CHTS_CL = catch.unid$CHTS_CL / unid.ratio.prev$AB1
    catch.unid$CHTS_H  = catch.unid$CHTS_H  / unid.ratio.prev$AB1
    catch.unid$CHTS_RL = catch.unid$CHTS_RL / unid.ratio.prev$B2
    
    catch.unid$VAR_AB1 = catch.unid$VAR_AB1 / (unid.ratio.prev$AB1^2)
    catch.unid$VAR_B2  = catch.unid$VAR_B2  / (unid.ratio.prev$B2^2)
    catch.unid$CHTS_VAR_CL = catch.unid$CHTS_VAR_CL / (unid.ratio.prev$AB1^2)
    catch.unid$CHTS_VAR_H  = catch.unid$CHTS_VAR_H  / (unid.ratio.prev$AB1^2)
    catch.unid$CHTS_VAR_RL = catch.unid$CHTS_VAR_RL / (unid.ratio.prev$B2^2)
  }
  
  
  ### Reconstructing our Catch Tables ###
  ### -----------------------------------
  ###     ...which essentially reconstructs our current 'catch.table' and 'unid.dat' tables, which
  ###       (repectively) are only meant to include catches for the species-of-interest (i.e., no allocation
  ###       from unidentified catch records ) and includes all unidentified and identified catch records so
  ###       that we might (re) evaluate an appropriate allocation factor to assign some UNID catch...
  
  
  ### CATCH.TABLE ###
  
  catch.id = catch.dummy %>%
    filter(  is.na(AB1.orig.unid) &  is.na(B2.orig.unid) ) %>%
    select( -one_of( c( 'STRATAID', 'AB1.orig.unid','B2.orig.unid', 'AB1.assign.id','B2.assign.id' ) ) )
    ###     ...to which no additional modifications are needed as these catch estimates have always been identified
    ###       for the species-of-interest (i.e., and so no allocation ratio has ever been applied )...
  
  rm( catch.dummy )
  
  
  ### UNID.DAT ###
  
  unid.dummy = unid.dat %>%
    ###   ...for which catches for the unidentified taxa and assessed species are to be replaced by those in
    ###       'catch.unid' and 'catch.id' respectively, and so they have to be dropped first...
    filter( SP_CODE %notin% c( as.character(nodc.unid.catch), as.character(nodc.code) ) ) %>%
    bind_rows( catch.id, catch.unid )
  
  
  ###   Note that, as a final step, I add the unidentified catch back into the 'catch.id' table as the next bit of code
  ###   (which allocates some percent of UNID catch to the species-of-interest) requires both UNID and ID catch...
  
  catch.id = catch.id %>% bind_rows( catch.unid )
  
  
  
  summary.table = list()
  summary.table$unid.dat    = unid.dummy
  summary.table$catch.table = catch.id
  
  return( summary.table )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------




