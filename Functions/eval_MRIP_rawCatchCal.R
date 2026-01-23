

### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------
###
###     DESCRIPTION
###
###   ...for which the functions included in this script provide common summaries of raw MRIP data
###     to inform the 'outlier' investigations required when publishing our GenRec (SEDAR) working papers...
###
### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------

ntrps.perStrata <- function( catch.table,
                             groupby.strata,
                             mode.filter = c('None',c(1:7)) ) {
  
  blah = catch.table
  
  if( !any( mode.filter == 'None' ) ) {
    blah = blah %>% filter( MODE_FX %in% mode.filter )
  }
  
  blah = blah %>%
    group_by( across( any_of(groupby.strata) ) ) %>%
    summarize( N = length( unique(ID_CODE) ) ) %>%
    ungroup()
  # sum( blah$N )                               ###  ...total number of intercepts
  # summary( as.factor(blah$N) )
  
  
  ### Frequency Table ###
  dummy.table = blah %>%
    group_by( N ) %>%
    summarize( nStrata = length( N ) ) %>%      ###  ...number of Strata informed by given #Trips
    rename( nTRIPs = N ) %>%
    mutate( pStrata = nStrata / sum(nStrata),   ###  ...percent Strata informed by a given #Trips
            colProd = nTRIPs * nStrata )
  # sum( dummy.table$colProd )                  ###  ...total number of intercepts
  dummy.table = dummy.table %>% select( nTRIPs, nStrata, pStrata )
  
  
  ### PLOT ###
  dummy.plot = ggplot( blah, aes(x=N) ) +
    geom_histogram( fill = 'transparent', color='black' ) +
    labs( title="Number of Strata informed by a given Number of Angler-Trips",
          x = "Number of Trips", y = "Number of Strata" ) +
    # geom_density( alpha=0.4, bw=0.25 ) +
    # labs( title="Proportion of Strata informed by a given Number of Angler-Trips",
    #       x = "Number of Trips", y = "Proportion of Strata" ) +
    
    # scale_x_log10( ) + labs( title="Frequency Distribution for log10(catch)" ) +
    coord_cartesian( xlim = c( 0, max(dummy.table$nTRIPs) ) ) +
    ###   ...which limits the x-axis to the range ( of trips informing individual strata ) in the data...
    
    facet_grid( SUB_REG ~ . ) +
    # facet_grid( SUB_REG ~ MODE_FX ) +
    theme_bw() +
    theme( text = element_text(size = 11),
           axis.text.x = element_text(angle = 90, vjust=0.5),
           # axis.title.x = element_blank(),
           # axis.title.y = element_blank(),
           legend.position = "bottom",
           panel.grid.major = element_line(colour = "grey", size = 0.5),
           panel.grid.minor = element_line(colour = "grey", size = 0.2),
           panel.border = element_rect(colour = "black", fill = NA) )
  
  
  return.object = list( dummy.table, dummy.plot )
  names(return.object) = c( "FreqTable","DensityPlot" )
  rm( blah, dummy.table, dummy.plot )
  
  
  return( return.object )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------

dist.obsCATCH <- function( catch.table ) {
  
  blah = catch.table %>%
    pivot_longer( cols=c("CLAIM","HARVEST","LANDING","RELEASE"), names_to = "METRIC", values_to = "value" ) %>%
    mutate( ESTIMATE = ifelse( METRIC %in% c( "CLAIM","HARVEST","LANDING" ), "AB1",
                       ifelse( METRIC %in% c( "RELEASE" ),                    "B2", NA ) ) )
  
  ### Frequency Table ###
  dummy.table = blah %>%
    group_by( METRIC, value ) %>%
    summarize( N = length( value ) ) %>%
    pivot_wider( names_from = c("METRIC"), values_from = N ) %>%
    rename( obsCATCH = value ) %>%
    arrange( obsCATCH )

  
  ### PLOT ###
  dummy.plot = ggplot( blah, aes(x=value) ) +
    geom_density( alpha=0.4, bw=0.25 ) +
    ###     ...where 'bw' controls the binwidth used in smoothing (e.g., if density plots look too chaotic,
    ###          might need a larger binwidth to increase the degree of smoothing between 'peaks' )...
    
    # scale_x_log10( ) + labs( title="Frequency Distribution for log10(OBS-CATCH)" ) +
    labs( title="Frequency Distribution for Catch Observations" ) +
    
    facet_wrap( METRIC ~ . , scales="free" ) +
    theme_bw() +
    scale_fill_manual( values = c('deeppink','darkblue') ) +
    theme( text = element_text(size = 11),
           axis.text.x = element_text(angle = 90, vjust=0.5),
           axis.title.x = element_blank(),
           axis.title.y = element_blank(),
           # legend.position = "bottom",
           panel.grid.major = element_line(colour = "grey", size = 0.5),
           panel.grid.minor = element_line(colour = "grey", size = 0.2),
           panel.border = element_rect(colour = "black", fill = NA) )
  
  
  return.object = list( dummy.table, dummy.plot )
  names(return.object) = c( "FreqTable","DensityPlot" )
  rm( blah, dummy.table, dummy.plot )
  
  
  return( return.object )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------

import.FHS.cals = function( new.com = new.com ) {

  fhs.adj = dbGetQuery( con, paste0( "SELECT
                           COMMON, YEAR, WAVE, ST, SUB_REG, MODE_FX, AREA_X, RATIO, VAR_RATIO
                      FROM RDI.MRIP_CATCH_CAL2018@secapxdv_dblk.sfsc.noaa.gov t
                         where t.COMMON IN ", toupper( sprintf("'%s'", paste(new.com, collapse = "','")) ) ) )
  fhs.adj = fhs.adj %>%
    mutate_at( vars( YEAR, WAVE, ST, SUB_REG ), list( ~ as.integer(.) ) ) %>%
    mutate_at( vars( COMMON, MODE_FX, AREA_X ), list( ~ as.character(.) ) ) %>%
    arrange( YEAR, WAVE, SUB_REG, ST, MODE_FX, AREA_X )

  return( fhs.adj)
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------

import.ForHire.part = function( ) {

  cbt.hbt.convert = dbGetQuery( con, "SELECT * FROM RDI.CBT_HBT_RATIOS@secapxdv_dblk.sfsc.noaa.gov" )
  cbt.hbt.convert = cbt.hbt.convert %>%
    pivot_longer( cols = c("HBT_R","CBT_R"), names_to = "NEW_MODEN", values_to = "Ratio" ) %>%
    mutate( NEW_MODEN = str_replace( NEW_MODEN, "_R","" ),
            MODE_FX = ifelse( NEW_MODEN == "CBT", 5, 4 ),
            ST = ifelse( NEW_STA == "TX", 48, ifelse( NEW_STA == "LA", 22, ifelse( NEW_STA == "MS", 28,
                 ifelse( NEW_STA == "AL",  1, ifelse( NEW_STA == "FLW",12, ifelse( NEW_STA == "FLE",12,
                 ifelse( NEW_STA == "GA", 13, ifelse( NEW_STA == "SC", 45, ifelse( NEW_STA == "NC", 37, NA ))))))))) )

  return( cbt.hbt.convert )
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------

filter.raw.mrip <- function( est.table, filters ) {
  
  dummy.table = est.table
  
  if( 'YEAR' %in% names(filters) ) {
    dummy.table = dummy.table %>% filter( YEAR %in% filters$YEAR )
  }
  if( 'WAVE' %in% names(filters) ) {
    dummy.table = dummy.table %>% filter( WAVE %in% filters$WAVE )
  }
  
  if( 'SUB_REG' %in% names(filters) ) {
    dummy.table = dummy.table %>% filter( SUB_REG %in% filters$SUB_REG )
  }
  if( 'ST' %in% names(filters) ) {
    dummy.table = dummy.table %>% filter( ST %in% filters$ST )
  }
  # if( 'FL_REG' %in% names(filters) ) {
  #   dummy.table = dummy.table %>% filter()
  # }
  # if( 'NC_REG' %in% names(filters) ) {
  #   dummy.table = dummy.table %>% filter()
  # }
  
  if( 'MODE_FX' %in% names(filters) ) {
    dummy.table = dummy.table %>% filter( MODE_FX %in% filters$MODE_FX )
  }
  if( 'AREA_X' %in% names(filters) ) {
    dummy.table = dummy.table %>% filter( AREA_X %in% filters$AREA_X )
  }
  
  
  return( dummy.table )
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------

plot.heatmap <- function( data.table, groupby.strata,
                          table.type = c('est','raw'), var.type = c('effort','catch'),
                          output.type = c('absolute','percentage') ) {
  
  blah = data.table %>%
    mutate_at( vars( YEAR, WAVE, SUB_REG, ST, MODE_FX, AREA_X ), list( ~ as.integer(.) ) )
  
  
  ### EFFORT SUMMARIES -- ESTIMATES or SAMPLING EFFORT ###
  ### ----------------------------------------------------
  if( var.type == 'effort' ) {
    
    blah = blah %>%
      ###   ...calculating total effort to convert strata-level efforts into percentages...
      mutate( sumEFF = ifelse( table.type == 'est', sum( WP_INT, na.rm=TRUE ),
                       ifelse( table.type == 'raw', length( unique( ID_CODE[ !is.na(ID_CODE) ] ) ), NA ) ) ) %>%
      group_by( across( any_of(groupby.strata) ) ) %>%
      mutate( EFFORT = ifelse( table.type == 'est', sum( WP_INT, na.rm=TRUE ),
                               ifelse( table.type == 'raw', length( unique( ID_CODE[ !is.na(ID_CODE) ] ) ), NA ) ) ) %>%
      mutate( pEFFORT = EFFORT / sumEFF ) %>%
      distinct( across( any_of( c( groupby.strata,"EFFORT","sumEFF","pEFFORT") ) ) ) %>%
      arrange(!!! rlang::syms( groupby.strata ) ) %>%
      
      mutate_at( groupby.strata, list( ~as.character(.) ) ) %>%
      filter( EFFORT > 0 )
    
    
    ### PLOT ###
    command.line = paste0(
      "dummy.plot = ggplot( blah, aes( x = ",groupby.strata[1],", y = ",groupby.strata[2],"," )
    if( output.type == 'absolute' ) {
      command.line = paste0( command.line," fill = EFFORT ) )" )
    } else if( output.type == 'percentage' ) {
      command.line = paste0( command.line," fill = pEFFORT ) )" )
    }
    eval( parse( text = command.line ) )
    rm( command.line )
    
    dummy.plot = dummy.plot + geom_tile() + coord_fixed() +
      scale_fill_gradient( low="white", high="red" )
      # scale_fill_gradient2( low="blue", mid="white", high="red" )
    if( table.type == 'est' ) {
      dummy.plot = dummy.plot + labs( title = "Distribution of Effort Estimates across Strata (angler trips)" )
    } else if( table.type == 'raw' ) {
      if( output.type == 'absolute' ) {
        dummy.plot = dummy.plot + labs( title = "Distribution of (Positive) Sampling Effort across Strata (nTRIPS)" )
      } else if( output.type == 'percentage' ) {
        dummy.plot = dummy.plot + labs( title = "Distribution of (Positive) Sampling Effort across Strata (%TRIPS)" )
      }
    }
    
    if( output.type == 'absolute' ) {
      dummy.plot = dummy.plot + geom_text( aes( label = round( EFFORT, 0 ) ) )
    } else if( output.type == 'percentage' ) {
      dummy.plot = dummy.plot + geom_text( aes( label = round( pEFFORT, 2 ) ) )
    }
    
    
  ### CATCH SUMMARIES -- AB1 and B2 ###
  ### ---------------------------------
  } else if( var.type == 'catch' ) {
    
    blah = blah %>%
      mutate( AB1 = LANDING * WP_CATCH,
              B2  = RELEASE * WP_CATCH ) %>%
      ###   ...calculating total catch to convert strata-level catches into percentages...
      mutate( sumAB1 = ifelse( table.type == 'est', sum( AB1, na.rm=TRUE ), NA ),
              sumB2  = ifelse( table.type == 'est', sum(  B2, na.rm=TRUE ), NA ) ) %>%
      group_by( across( any_of(groupby.strata) ) ) %>%
      mutate( LANDINGS = ifelse( table.type == 'est', sum( AB1, na.rm=TRUE ), NA ),
              DISCARDS = ifelse( table.type == 'est', sum(  B2, na.rm=TRUE ), NA ) ) %>%
      mutate( pAB1 = LANDINGS / sumAB1,
              pB2  = DISCARDS / sumB2  ) %>%
      distinct( across( any_of( c( groupby.strata,"LANDINGS","sumAB1","pAB1",
                                                  "DISCARDS","sumB2" ,"pB2"  ) ) ) ) %>%
      arrange(!!! rlang::syms( groupby.strata ) ) %>%
      
      mutate_at( groupby.strata, list( ~as.character(.) ) ) %>%
      filter( LANDINGS > 0 | DISCARDS > 0 )
    
    
    if( output.type == 'absolute' ) {
      blah = blah %>%
        select( -c( "pAB1","sumAB1", "pB2","sumB2" ) ) %>%
        pivot_longer( cols=c("LANDINGS","DISCARDS"), names_to = "CAT_VAR", values_to = "CATCH" )
      
      catch.vars = c('LANDINGS','DISCARDS')
      
    } else if( output.type == 'percentage' ) {
      blah = blah %>%
        select( -c( "LANDINGS","sumAB1", "DISCARDS","sumB2" ) ) %>%
        pivot_longer( cols=c("pAB1","pB2"), names_to = "CAT_VAR", values_to = "CATCH" )
      
      catch.vars = c('pAB1','pB2')
    }
    
    ### PLOTS ###
    dummy.plot = vector( "list", length=length(catch.vars) )
    names(dummy.plot) = catch.vars
    for( i in 1:length(catch.vars) ) {
      dummy.table = blah %>% filter( CAT_VAR == catch.vars[i] )
      
      command.line = paste0(
        "dummy.plot[[i]] = ggplot( dummy.table, aes( x = ",groupby.strata[1],",",
                                                   " y = ",groupby.strata[2],", fill = CATCH ) )" )
      eval( parse( text = command.line ) )
      rm( command.line )
      
      dummy.plot[[i]] = dummy.plot[[i]] + geom_tile() + coord_fixed() +
        scale_fill_gradient( low="white", high="red" )
        # scale_fill_gradient2( low="blue", mid="white", high="red" )
      if( output.type == 'absolute' ) {
        dummy.plot[[i]] = dummy.plot[[i]] + geom_text( aes( label = round( CATCH, 0 ) ) )
      } else if( output.type == 'percentage' ) {
        dummy.plot[[i]] = dummy.plot[[i]] + geom_text( aes( label = round( CATCH, 2 ) ) )
      }
      
      if( table.type == 'est' ) {
        if( catch.vars[i] %in% c('pAB1','LANDINGS') ) {
          dummy.plot[[i]] = dummy.plot[[i]] +
            labs( title = "Distribution of Landings Estimates across Strata (AB1)" )
        } else if( catch.vars[i] %in% c('pB2','DISCARDS') ) {
          dummy.plot[[i]] = dummy.plot[[i]] +
            labs( title = "Distribution of Discard Estimates across Strata (B2)" )
        }
      }
    }
    rm( dummy.table )
  }
  
  rm( blah )
  
  
  return( dummy.plot )
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------

  




