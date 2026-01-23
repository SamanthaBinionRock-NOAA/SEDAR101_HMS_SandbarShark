

### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------
###
###     DESCRIPTION
###
###   ...where, for some SEDARs, data/estimates from state surveys might need to be calibrated into
###     MRIP-FES units. This function serves to apply such calibrations, while also keeping track
###     of those calibrations that have been applied in previous SEDARs (and may be appropriate for re-use)...
###
###
###   extract.cal.ratio( )
###       ...which identifies (and extracts) the calibration factors appropriate for a given assessment...
###
###
###   calibrate.MRIPstate( )
###       ...which calibrates the ***CATCH*** estimates of a particular survey to 'MRIP units' (e.g., TPWD or LACR )...
###
###
###   calibrate.MRIPstate.effort( )
###       ...which calibrates the ***EFFORT*** estimates of a particular survey to 'MRIP units' (e.g., TPWD or LACR )...
###
### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------


extract.cal.ratio = function( new.com, region ) {
  ###     ...where 'new.com' and 'region' identify the stock that is being assessed...
  
  
  ### ---------------------------------------------------------------------------------------------------------
  ###
  ### SEDAR 74 and SEDAR 98 -- Gulf of Mexico RED SNAPPER ###
  ###
  ###     --- TEXAS --- S74-DW-10 ---
  ###         ...which is an effort-based calibration and not species-specific...
  ###
  ###     --- LOUISIANA --- S74-DW-04 ---
  ###         ...which is a catch-based calibration and *IS* species-specific...
  ###
  if( region == 'Gulf of America' & 'red snapper' %in% new.com ) {
    
    
    ### PRIVATE ###
    ### -----------
    
    ### TPWD ###
    ###   ...which is the same calibration ratio being applied to all (catch & effort) estimates
    tpwd.cal.ratio = 10.8989
    tpwd.cal.var   = 2.11009
    
    ### LACR ###
    ###   ...which applies unique calibration ratios to different estimates...
    lacr.cal.ratio.AB1 = 1.806392
    lacr.cal.var.AB1   = 0.181018
    lacr.cal.ratio.B2  = 7.11943249
    lacr.cal.var.B2    = 7.66398665
    lacr.cal.ratio.EFF = 2.12430769
    lacr.cal.var.EFF   = 2.12430769
    
    
    ### CHARTER ###
    ### -----------
    
    ### TPWD ###
    ###   ...there is currently no (potential) charter calibration available between MRIP and TPWD charter estimates...
    
    ### LACR ###
    ###   ...from S74-DW-04 -- "Because the charter fishing frame used by the LA Creel and MRIP surveys are
    ###       functionally equivalent, charter fishing estimates of the two surveys are assumed equivalent
    ###       and are not adjusted or presented." Additionally, although not noted in the WP, visual inspection
    ###       the (Louisiana) catch time series does not suggest a difference in scale between MRIP and LACR
    ###       charter estimates (e.g., at the S98 DW, the RecWG noted similar scales btw 2010-2013 vs. 2016+ estimates )...
    
    
    ### COMBINED ###
    ### ------------
    priv.vec = list( PRIV = c( tpwd.cal.ratio, tpwd.cal.var,
                               lacr.cal.ratio.AB1, lacr.cal.ratio.B2, lacr.cal.var.AB1, lacr.cal.var.B2,
                               lacr.cal.ratio.EFF, lacr.cal.var.EFF ) )
    names(priv.vec$PRIV) = c('tpwd.cal.ratio','tpwd.cal.var',
                             'lacr.cal.ratio.AB1','lacr.cal.ratio.B2','lacr.cal.var.AB1','lacr.cal.var.B2',
                             'lacr.cal.ratio.EFF','lacr.cal.var.EFF')
    
    return.object = c( priv.vec )
    rm( priv.vec )
    
  }
  ### ---------------------------------------------------------------------------------------------------------
  
  
  return( return.object )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------


calibrate.MRIPstate = function( DS.filter = c('TPWD','LA Creel'), mode.filter, cal.ratios, genrec.table ) {
  ###     ...where 'DS.filter' is the survey whose estimates are being calibrated,
  ###           'mode.filter' identifies the mode(s) for which the associated estimates need to be calibrated,
  ###           'cal.ratios' are the (list of) calibration factors to be applied for this stock,
  ###       and 'genrec.table' is the table containing the uncalibrated estimates...
  
  
  dummy = genrec.table
  dummy.filter = c( DS.filter, paste0(DS.filter,"sub") )
  ###     ...which is done to make sure any imputed estimates (e.g., DS='MRIPsub' ) are also calibrated...
  
  rows = which( dummy$DS %in% dummy.filter & dummy$NEW_MODEN %in% mode.filter )
  
  
  ### Separate Calibration Factors by Catch-Type
  ### ------------------------------------------
  
  if( any( grepl('AB1',names(cal.ratios)) | grepl('B2',names(cal.ratios)) |
           grepl('LBS',names(cal.ratios)) ) ) {
    
    
    ### Calibrate LANDINGS ###
    ### ----------------------
    
    ###   ...where I start by calculating the (original) SEFSC avgwgt estimates, which is needed
    ###     to calibrate our landings-in-weight estimates when no weight-specific calibration factor(s)
    ###     have been provided, in which case the updating landings-in-weight estimates are the
    ###     product of the calibrated landings-in-number & these (original) avgwgt estimates...
    dummy = dummy %>%
      mutate( AVGWWT_SEC = ifelse( AB1==0, 0, lbsest_SECwwt / AB1 ),
              AVGGWT_SEC = ifelse( AB1==0, 0, lbsest_SECgwt / AB1 ) )
    
    ### AB1 ###
    if( any( grepl('AB1',names(cal.ratios)) ) ) {
      dummy$AB1[ rows ] = dummy$AB1[ rows ] * as.numeric( cal.ratios[ grepl('ratio.AB1',names(cal.ratios)) ] )
    }
    
    ### LBS ###
    ###     ...which can either be done by a calibration factor ( if provided in 'cal.ratios' ),
    ###       or by multiplying the newly calibrated AB1 estimate by the SEFSC avgwgt ( computed above )...
    if( any( grepl('LBS',names(cal.ratios)) ) ) {
      
      ###   ...applying the 'ratio.LBS' calibration factor to lbsest_SECwwt...
      dummy$lbsest_SECwwt[ rows ] = dummy$lbsest_SECwwt[ rows ] *
        as.numeric( cal.ratios[ grepl('ratio.LBS',names(cal.ratios)) ] )
      
      ###   ...calculating the whole:gutted weight conversion factor from 'catch.table'...
      conv_GWTwwt = genrec.table %>%
        filter( DS %in% dummy.filter & NEW_MODEN %in% mode.filter ) %>%
        group_by( YEAR ) %>%
        summarise( WWT = sum( as.numeric(lbsest_SECwwt),na.rm=TRUE),
                   GWT = sum( as.numeric(lbsest_SECgwt),na.rm=TRUE) ) %>%
        mutate( WGT.ratio = WWT / GWT )
      conv_GWTwwt = unique(conv_GWTwwt$WGT.ratio)[1]
      
      ###   ...and applying the WWT:GWT conversion factor to lbsest_SECgwt...
      dummy$lbsest_SECgwt[ rows ] = dummy$lbsest_SECwwt[ rows ] / conv_GWTwwt
      rm( conv_GWTwwt )
      
    } else {
      dummy$lbsest_SECwwt[ rows ] = dummy$AB1[ rows ] * dummy$AVGWWT_SEC[ rows ]
      dummy$lbsest_SECgwt[ rows ] = dummy$AB1[ rows ] * dummy$AVGGWT_SEC[ rows ]
    }
    
    dummy = dummy %>% select( -c('AVGWWT_SEC','AVGGWT_SEC') )
    
    
    
    ### Calibrate DISCARDS ###
    ### ----------------------
    dummy$B2[ rows ] = dummy$B2[ rows ] * as.numeric( cal.ratios[ grepl('ratio.B2',names(cal.ratios)) ] )
    
    
    
  } else {
    
    ### Single Calibration Factor given for all Catch Types
    ### ---------------------------------------------------
    
    ### Calibrate AB1
    dummy$AB1[ rows ] = dummy$AB1[ rows ] * as.numeric( cal.ratios[ grepl('ratio',names(cal.ratios)) ] )
    
    ### Calibrate B2
    dummy$B2[ rows ] = dummy$B2[ rows ] * as.numeric( cal.ratios[ grepl('ratio',names(cal.ratios)) ] )
    
    ### Calibrate LBS Estimates
    dummy$lbsest_SECwwt[ rows ] = dummy$lbsest_SECwwt[ rows ] *
      as.numeric( cal.ratios[ grepl('ratio',names(cal.ratios)) ] )
    dummy$lbsest_SECgwt[ rows ] = dummy$lbsest_SECgwt[ rows ] *
      as.numeric( cal.ratios[ grepl('ratio',names(cal.ratios)) ] )
    
  }
  
  
  return.object = dummy
  rm( dummy, rows )
  
  return( return.object )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------


calibrate.MRIPstate.effort = function( new.com, region, DS = c('LACR','TPWD'), genrec.table ) {
  ###     ...where 'new.com' and 'region' identify the stock that is being assessed,
  ###           'DS' the survey that needs to be calibrated,
  ###       and 'genrec.table' the table that includes the uncalibrated estimates...
  
  
  ### ---------------------------------------------------------------------------------------------------------
  ###
  ### SEDAR 74 -- Gulf of Mexico RED SNAPPER ###
  ###
  ###     --- TEXAS --- S74-DW-10 ---
  ###         ...which is an effort-based calibration and not species-specific...
  ###
  ###     --- LOUISIANA --- S74-DW-04 ---
  ###         ...which is a catch-based calibration and *IS* species-specific...
  ###
  if( region == 'Gulf of America' & 'red snapper' %in% new.com ) {
    
    ### TPWD
    tpwd.cal.ratio = 10.8989
    
    ### LACR
    lacr.cal.ratio.PRSH = 0.791152081
    lacr.cal.ratio.MRIP = 2.700487117

  }
  ### ---------------------------------------------------------------------------------------------------------
  
  
  
  
  
  # if( "LA" %in% states ) {
  # 
  #   dummy = lacr.effort
  #   rows = ( dummy$MODES == "Private" )
  # 
  #   ### Conversion of Priv/Shore into Private Estimates ###
  #   dummy = dummy %>% mutate( PRIVcal = ifelse( rows, 0.791152081, NA ) )
  #   dummy$EXPANDED_EFFORT[ rows ] = dummy$EXPANDED_EFFORT[ rows ] * dummy$PRIVcal[rows]
  #   dummy$EXPANDED_EFFORT_VAR[ rows ] = dummy$EXPANDED_EFFORT_VAR[ rows ] * ( dummy$PRIVcal[rows]^2 )
  #   ###     ...multiplying a random variable by a constant increases the variance by the square of the constant...
  # 
  #   ### LACR:FES Calibration ###
  #   dummy = dummy %>% mutate( FEScal = ifelse( rows, 2.700487117, NA ) )
  #   dummy$EXPANDED_EFFORT[ rows ] = dummy$EXPANDED_EFFORT[ rows ] * dummy$FEScal[rows]
  #   dummy$EXPANDED_EFFORT_VAR[ rows ] = dummy$EXPANDED_EFFORT_VAR[ rows ] * ( dummy$FEScal[rows]^2 )
  # 
  #   lacr.effort = dummy
  #   rm( dummy, rows )
  # 
  # 
  #   # eff.summary = lacr.effort %>%
  #   #   group_by( INT_YEAR, MODES ) %>%
  #   #   summarise( NTRIPS = sum( EXPANDED_EFFORT, na.rm=TRUE) ) %>%
  #   #   pivot_wider( names_from = MODES, values_from = NTRIPS )
  # 
  # }
  # 
  # 
  # 
  # ### TPWD:MRIP Calibration ###
  # ### -------------------------
  # ###
  # ###      ...which is only applicable to PRIV estimates ( ratio estimated b/w TPWD vs FES effort )
  # ###       that are scaled using the ratio of 2016 FES:TPWD effort. This approach is described in S74-DW-10...
  # ###
  # ###   These calibrations are irrelevant for this assessment...
  # 
  # if( "TX" %in% states ) {
  # 
  #   dummy = tpwd.effort
  #   rows = ( dummy$NEW_MODEN == "Priv" )
  # 
  #   ### TPWD:FES Calibration ###
  #   dummy = dummy %>% mutate( FEScal = ifelse( rows, 10.8989, NA ) )
  #   dummy$ESTHRS[ rows ] = dummy$ESTHRS[ rows ] * dummy$FEScal[ rows ]
  #   dummy$ESTHRS_SE[ rows ] = sqrt( ( dummy$ESTHRS_SE[ rows ]^2 ) * ( dummy$FEScal[ rows ]^2 ) )
  #   ###     ...multiplying a random variable by a constant increases the variance by the square of the constant...
  # 
  #   dummy$NTRP[ rows ] = dummy$ESTHRS[ rows ] / dummy$TRIPLEN[ rows ]
  #   dummy$NPAR[ rows ] = dummy$NTRP[ rows ] / dummy$PARSIZE[ rows ]
  # 
  #   tpwd.effort = dummy
  #   rm( dummy, rows )
  # 
  # 
  #   # eff.summary = tpwd.effort %>%
  #   #   group_by( CYEAR, ACTIVITY ) %>%
  #   #   summarise( NTRIPS = sum( NTRP, na.rm=TRUE) ) %>%
  #   #   pivot_wider( names_from = ACTIVITY, values_from = NTRIPS )
  # 
  # }
  
  
}




