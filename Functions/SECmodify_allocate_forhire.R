

### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------
###
###     DESCRIPTION
###
###     OST combines Cbt+Hbt in some years/regions for 'estimation purposes' (i.e., small sample size ).
###     This can be an issue in some SEDARs (e.g., when the assessment model requires separate 'Cbt' & 'Hbt' modes ),
###     and so these combined for-hire estimates may need to be partitioned into individual components
###     (i.e., 'Cbt' vs. 'Hbt' ). The SEFSC already has an approach by which the combined for-hire estimates
###     of the Gulf of Mexico (1981-1985) are partitioned, which we (routinely) apply in our workflow. However,
###     our general workflow does not include a similar partitioning of for-hire estimates for the MATL & NATL
###     (1981-2003). To this end, an effort-based partitioning approach (for MATL/NATL for-hire) was developed
###     during SEDAR 82 ( SATL gray triggerfish ), some of the details of which are discussed below.
###     This function was developed to apply this approach if/when it's requested. Note that the applicability
###     of the 'S82 approach' to other stocks has yet to be evaluated, and so applying this function may
###     require additional justification (e.g., additional analyses and/or discussions during data workshops )...
###
###     The function(s) below were designed to partition for-hire estimates for the MATL/NATL regions (when requested)...
###
###
###   partition.forhire( )
###       ...which partitions the ***CATCH*** estimates of the combined for-hire mode...
###
###
###   partition.forhire.effort( )
###       ...which partitions the ***EFFORT*** estimates of the combined for-hire mode...
###
### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------


partition.forhire = function( genrec.table ) {
  ###     ...where 'genrec.table' is the catch table that includes estimates for a combined for-hire fleet
  ###         (in the MATL/NATL) that are to be partitioned using the effort-based method developed during SEDAR 82...
  
  
  
  ###   As part of the S82 research track, allocation ratios were estimated from the raw MRIP intercept data,
  ###       which retains separate records for the 'Cbt' & 'Hbt' modes ( OST's aggregation of these modes is for
  ###       'estimation purposes' and so is only needed in generating catch & effort estimates ). These ratios were
  ###       calculated as the relative contribution of each mode to the total (combined) effort estimate:
  ###
  ###                   Ratio[y,s,m] = nTRIPS[y,s,m] / ( nTRIPS[y,s, m='Cbt' ] + nTRIPS[y,s, m='Hbt' ] )
  ###
  ###       ...where the relative effort (in angler trips) of each of the for-hire modes ( m='Cbt' or m='Hbt' )
  ###       in each (s) state and (y) year is used to split the combined for-hire estimates in the MATL & NATL (1981-2003).
  ###   Although calculated from MRIP effort data (i.e., number of angler trips -- sum(CNTRBTRS*WP_INT) ), these ratios
  ###       are used to separate both catch and effort estimates. Catch-based ratios were considered, but ultimately discarded
  ###       as there didn't seem to be enough information in the catch data to support estimation of these ratios. Most years
  ###       had zero catch to reference (i.e., a lot of cells with a ratio = 'NA' ) but, and probably more importantly,
  ###       most of the strata for which ratios were available (i.e., catch > 0 ) resulted in ratios of either 0% or 100%,
  ###       suggesting that there weren't enough samples (i.e., positive trips) to accurately calculate a ratio.
  ###       Effort-based ratios, on the other hand, were consistently between 0 and 1 and so appear better estimated.
  ###
  ###   Note that this approach is similar to that applied in the Gulf of Mexico,
  ###       which splits 1981-1985 'CbtHbt' estimates from SUB_REG 7 using ( SEDAR Best Practices - REC ISSUE #1 ):
  ###           "a ratio of... headboat angler trip estimates to... charter boat angler trip estimates for 1986-1990.
  ###            Mean ratio is calculated by state... and then applied to the 1981-1985 MRFSS estimates..." (SEDAR PW-07, p77)
  ###       However, such an approach is not possible in the MATL/NATL as SRHS estimates are not available in these regions
  ###       (SRHS has never operated outside the GOM/SATL). Additionally, the S82 approach is based on ratios specific to
  ###       an individual year to retain any intra-annual variability in the individual 'Cbt' & 'Hbt' modes in its calculation;
  ###       these ratios are to be applied to a fairly large range of years (1981-2003) over which a static ratio
  ###       (e.g., mean( 1981-2003 ) ) is probably not representative. Although static ratios are used in the GOM approach,
  ###       these are only applied to years 1981-1985, wherein relative catch/effort may not change all that much anyway.
  ###       Additionally, such year-specific ratios are not possible in the GOM approach because the SRHS was first implemented
  ###       in the GOM in 1986 ; overlap in the SRHS & MRFSS surveys (which is needed to calculate ratios) doesn't begin
  ###       until after the period of interest (i.e., ratios estimated from 1986-1990 data, but applied to 1981-1985 )...
  
  
  
  ###   The first step of this adjustment is to import the table of ratios that are to be used in partitioning
  ###       any combined for-hire estimates from that MATL & NATL regions (1981-2003)...
  ratios = read.csv( 'C://Users/matthew.nuttall/Desktop/Functions/import_datasets/ForHire Partitioning Ratios.csv' )
  
  ###     ...which is then modified to allow it's join with our 'genrec.table'...
  ratios = ratios %>%
    rename( Hbt_est = Hbt,
            Cbt_est = Cbt ) %>%
    pivot_longer( cols = c("Hbt_est","Cbt_est","Hbt_var","Cbt_var"), names_to = "variable", values_to = "value" ) %>%
    mutate( NEW_MODEN = gsub( "_.*","", variable ),
            metric    = gsub( ".*_","", variable ) ) %>%
    filter( metric == 'est' ) %>%
    ###   ...to remove the variance-adjustment factors...
    rename( ratio = value ) %>%
    select( YEAR, NEW_STA, NEW_MODEN, ratio )
  
  
  
  ###   I then subset 'genrec.table' to only include records for the combined for-hire mode ( MAT/NATL, 1981-2003 )...
  
  dummy = genrec.table %>%
    filter( NEW_MODEN == 'Cbt/Hbt' & SUB_REG %in% 4:5 & YEAR %in% 1981:2003 ) %>%
    uncount(2)
  
  ###     ...to which the allocation ratios are applied...
  
  dummy = dummy %>%
    mutate( MODE_FX   = rep(        c(4,5) , times = (dim(dummy)[1])/2 ),
            NEW_MODE  = rep(        c(2,3) , times = (dim(dummy)[1])/2 ),
            NEW_MODEN = rep( c('Hbt','Cbt'), times = (dim(dummy)[1])/2 ) ) %>%
    
    left_join( ratios, by=c("YEAR","NEW_STA","NEW_MODEN") ) %>%
    
    mutate( SEC.AVGWGT.WWT    = ifelse( AB1 == 0 & !is.na(lbsest_SECwwt ), 0, lbsest_SECwwt / AB1 ),
            SEC.AVGWGT.GWT    = ifelse( AB1 == 0 & !is.na(lbsest_SECgwt ), 0, lbsest_SECgwt / AB1 ),
            OST.AVGWGT.FHS.C  = ifelse( A   == 0 & !is.na(WGT_AB1C      ), 0, WGT_AB1C      / A   ),
            OST.AVGWGT.FHS.H  = ifelse( B1  == 0 & !is.na(WGT_AB1H      ), 0, WGT_AB1H      / B1  ),
            OST.AVGWGT.CHTS.C = ifelse( CHTS_CL == 0 & !is.na(CHTS_WAB1C), 0, CHTS_WAB1C    / CHTS_CL ),
            OST.AVGWGT.CHTS.H = ifelse( CHTS_H  == 0 & !is.na(CHTS_WAB1H), 0, CHTS_WAB1H    / CHTS_H  ) ) %>%
    ###     ...where, before adjusting catch (by 'ratio'), I save the originally applied (SEFSC & MRIP) avgwgt estimates
    ###       so that the 'lbsest_SEC' estimates can be recalculated from the newly scaled landings-in-number...
    
    mutate_at( vars( CHTS_CL,CHTS_H,CHTS_RL, AB1,A,B1,B2 ), list( ~ ratio * . ) ) %>%
    mutate_at( vars( CHTS_VAR_CL,CHTS_VAR_H,CHTS_VAR_RL, VAR_AB1,VAR_B2 ), list( ~ (ratio^2) * . ) ) %>%
    mutate( lbsest_SECwwt = AB1 * SEC.AVGWGT.WWT,
            lbsest_SECgwt = AB1 * SEC.AVGWGT.GWT,
            WGT_AB1C = A  * OST.AVGWGT.FHS.C,
            WGT_AB1H = B1 * OST.AVGWGT.FHS.H,
            CHTS_WAB1C = CHTS_CL * OST.AVGWGT.CHTS.C,
            CHTS_WAB1H = CHTS_H  * OST.AVGWGT.CHTS.H ) %>%
    select( -c( ratio, SEC.AVGWGT.WWT,SEC.AVGWGT.GWT, OST.AVGWGT.FHS.C,OST.AVGWGT.FHS.H, OST.AVGWGT.CHTS.C,OST.AVGWGT.CHTS.H) )
  
  rm( ratios )
  
  
  
  ###   Lastly, the combined 'CbtHbt' estimates in our original 'genrec.table' are removed
  ###       and replaced with the newly separated 'Cbt' and 'Hbt' estimates...
  
  genrec.table = genrec.table %>%
    filter( !( NEW_MODEN == 'Cbt/Hbt' & SUB_REG %in% 4:5 & YEAR %in% 1981:2003 ) ) %>%
    bind_rows( dummy )

  rm( dummy )
  
  
  
  return( genrec.table )
  
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

