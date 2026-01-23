

### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------
###
###     DESCRIPTION
###
###     ...wherein this script initializes a number of R-objects required at various points throughout the main script...
###
###
###    *** area.replace( )           ...which replaces the alpha-numeric codes used to describe spatial boundaries
###                                       with the actual state/region names...
###
###    *** wp.init.misc( )           ...which initializes a number of R-objects, called at various times to inform
###                                       construction of other objects (e.g., text for table/figure captions )...
###
###    *** wp.text.geo( )            ...which constructs text describing the SPATIAL boundaries of the provided estimates,
###                                       as printed in the manuscript body ('parms.geo') and into captions ('caps.geo')...
###    *** wp.text.mode( )           ...which constructs text describing changes in coverage (of individual MODES) over time,
###                                       as printed in the manuscript body ('parms.mode') and into captions ('caps.mode')...
###
###    *** wp.text.cap2( )           ...which constructs text specific to the caption of Fig2, identifying differences in
###                                       domains of MRIP/OST estimates (from the website) and those provided by the SEFSC...
###
### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------

area.replace <- function( area ) {
  ###   ...where 'area' is a character (or vector of characters) for which abbreviated state boundaries are
  ###       to be replaced with the full names (e.g., 'TX' changed to 'Texas' )...
  
  area <- gsub( "TX","Texas", area )
  area <- gsub( "LA","Louisiana", area )
  area <- gsub( "MS","Mississippi", area )
  area <- gsub( "AL","Alabama", area )
  area <- gsub( "FLW","western Florida", area )
  area <- gsub( "FLE","eastern Florida", area )
  area <- gsub( "GA","Georgia", area )
  area <- gsub( "SC","South Carolina", area )
  area <- gsub( "SNC","southern North Carolina", area )
  area <- gsub( "NNC","northern North Carolina", area )
  area <- gsub( "NC","North Carolina", area )
  area <- gsub( "VA","Virginia", area )
  area <- gsub( "MD","Maryland", area )
  area <- gsub( "DE","Delaware", area )
  area <- gsub( "PA","Pennsylvania", area )
  area <- gsub( "NJ","New Jersey", area )
  area <- gsub( "NY","New York", area )
  area <- gsub( "CT","Connecticut", area )
  area <- gsub( "RI","Rhode Island", area )
  area <- gsub( "MA","Massachusetts", area )
  area <- gsub( "NH","New Hampshire", area )
  area <- gsub( "ME","Maine", area )
  area <- gsub( "PR","Puerto Rico", area )
  
  area <- gsub( "1","the Florida panhandle", area )
  area <- gsub( "2","western Florida", area )
  area <- gsub( "3","the Florida Keys", area )
  area <- gsub( "4","southeastern Florida", area )
  area <- gsub( "5","northeastern Florida", area )
  
  area <- gsub( "6","southern North Carolina", area )
  area <- gsub( "7","northern North Carolina", area )
  
  return( area )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------


wp.init.misc = function( params ) {
  ###     ...where 'params' is the R object that (amongst other things) identifies the filters applied in this SEDAR...
  
  
  ### TAXONOMIC OBJECTS ###
  ### ---------------------
  ###   I first define a character that describes the "full" common name of the assessed fish(es),
  ###   composed of one species if species.add == "None" but two species if species.add != "None"...
  if( params$species.add == 'None' ) {
    full.common <- params$species.name
  } else {
    full.common <- paste0( params$species.name," and ",params$species.add )
  }
  
  
  ### SPATIAL OBJECTS ###
  ### -------------------
  
  ###   I then define a function to replace the alpha-numeric codes used to describe spatial boundaries
  ###   with the actual state/region names, which is applied in a number of instances throughout the code...
  
  ###   The above function is then applied to identify the **BOUNDARIES** (across states) considered in this assessment...
  if( length(params$subset.states) == 1 ) {     ### ...which is true for most Caribbean assessments (only PR)
    inc.states = area.replace( params$subset.states )
  } else {
    inc.states <- c( area.replace( params$subset.states[1] ), 
                     area.replace( params$subset.states[length(params$subset.states)] ) )
  }
  
  ###   I also define a character to identify if the GOM is included in this assessment, which is important
  ###   in determining whether additional text/info is needed in some sections (e.g., TPWD, LACreel )...
  if( params$region %in% c("Gulf of America","Gulf of America and South Atlantic","Southeast") ) {
    inc.GOM <- TRUE
  } else {
    inc.GOM <- FALSE
  }
  
  ###   ...and identify whether (1) FLW is included in this data and (2) if so, whether only the FL Keys are considered.
  ###     In such cases, "FLW" will be replaced by "FLKeys" in the headers of my by-state tables/figures...
  FLK.only <- FALSE
  if( ( "FLW" %in% params$subset.states ) & ( 1 %notin% params$subset.FL ) & ( 2 %notin% params$subset.FL ) ) {
    FLK.only <- TRUE
  }
  
  
  ### MODE OBJECTS ###
  ### ----------------
  
  ### This character identifies the modes considered in this assessment (i.e., to construct text for manuscript )...
  inc.modes <- params$subset.modes
  inc.modes <- gsub( "Cbt","Charter", inc.modes )
  inc.modes <- gsub( "Hbt","Headboat", inc.modes )
  inc.modes <- gsub( "Priv","Private", inc.modes )
  # inc.modes <- gsub( "Shore","Shore", inc.modes )    ### ...which does nothing because 'Shore' is already named appropriately...
  
  
  ###   ...and the associated filter that can be applied to our GenRec data files (i.e., to filter to relevant modes )...
  mode.filter = params$subset.modes
  ###   ...so as to retain any "Priv" catch estimates from the LA_Creel survey...
  if( "Priv" %in% mode.filter & any( c("TX","LA","MS","AL","FLW") %in% params$subset.states ) ) {
    mode.filter = c( mode.filter,"Priv/Shore" )
  }
  ###   ...so as to retain (1981-2003) for-hire fishing in the Mid & North-Atlantic...
  if( ( ( "Cbt" %in% mode.filter ) | ( "Hbt" %in% mode.filter ) ) &
      any( c("VA","MD","DE","PA","NJ","NY","CT","RI","MA","NH","ME") %in% params$subset.states ) ) {
    mode.filter = c( mode.filter,"Cbt/Hbt" )
  }
  mode.filter = mode.filter[ order( match( mode.filter, c("Cbt","Cbt/Hbt","Hbt","Priv","Priv/Shore","Shore") ) ) ]
  
  
  
  return.object = list( full.common, inc.states, inc.GOM, FLK.only, inc.modes, mode.filter )
  names(return.object) = c( 'full.common', 'inc.states', 'inc.GOM', 'FLK.only', 'inc.modes', 'mode.filter' )
  
  return( return.object )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------


wp.text.geo = function( params, inc.states ) {
  ###     ...where 'params' is the R object that (amongst other things) identifies the filters applied in this SEDAR
  ###          and 'inc.states' identifies the spatial range of this assessment (i.e., = 'first.state' - 'last.state' )
  
  
  ### Spatial Boundaries ###
  ### ______________________
  ###
  ###     ...where "parms.geo" is written into the "Geographic Range" section of the manuscript body...
  ###       ...and "caps.geo" is added to the appropriate (by-state) table/figure captions...
  ###
  parms.geo <- ""
  caps.geo <- ""
  if( params$region == "Gulf of America" ) {
    
    ### GULF OF MEXICO ###
    ### ------------------
    
    ### ...wherein I extract any areas outside of the GOM from "subset.states"...
    outside.areas <- params$subset.states[ which( params$subset.states %notin% c("TX","LA","MS","AL","FLW") ) ]
    outside.areas <- area.replace( outside.areas )
    ###       ...which represents areas considered by the assessment but outside the GOM...
    if( length(outside.areas)==0 ) {
      parms.geo <- paste0( params$region," states from ",inc.states[1]," to ",inc.states[2],"." )
    } else {
      parms.geo <- paste0( params$region," states from ",inc.states[1],
                           " to western Florida, including ",paste( outside.areas, collapse=" and " ),"." )
      ###     ...where, in this case, "outside.areas" CAN ONLY BE Atlantic states (we don't assess Mexican waters).
      ###       Thus, logic states that FLW must be included in "params$subset.states"; we're assessing a GOM stock
      ###       that includes areas of the Atlantic and so if FLW is not in "params$subset.states", then we essentially
      ###       have a hole in the area being considered (by the assessment). Following this, I simply identify
      ###       'western Florida' in the text (instead of defining it dynamically)...
    }
    ### In cases where "western Florida" is one of the boundaries in this assessment (i.e., in "parms.geo"),
    ###     I evaluate whether "FLW" needs to be replaced by more specific text. Specifically, if...
    ###       (1) FLW includes the entirety of western Florida (including the FLKeys),
    ###       (2) FLW excludes the FLKeys but includes FL_REG 1 and 2,
    ###       (3) FLW includes only the panhandle (excludes FL_REG 2 and 3)...
    if( "western Florida" %in% inc.states ) {     ### ...where assessment boundaries start/end at "FLW"...
      if( 1 %in% params$subset.FL & 2 %in% params$subset.FL & 3 %in% params$subset.FL ) {
        parms.geo <- gsub( "western Florida", "western Florida, including the Florida Keys", parms.geo )
        # caps.geo <- " FLW includes the Florida Keys."
      } else if( ( 1 %in% params$subset.FL & 2 %in% params$subset.FL ) & 3 %notin% params$subset.FL ) {
        parms.geo <- gsub( "western Florida", "western Florida, excluding the Florida Keys", parms.geo )
        caps.geo <- " FLW excludes the Florida Keys."
      } else if( 1 %in% params$subset.FL & ( 2 %notin% params$subset.FL & 3 %notin% params$subset.FL ) ) {
        parms.geo <- gsub( "western Florida", "the Florida panhandle", parms.geo )
        caps.geo <- " FLW includes only the Florida panhandle."
      }
    }
    ###     ...and where I apply similar logic when "FLE" is one of the assessment boundaries...
    if( "eastern Florida" %in% inc.states ) {
      if( 4 %in% params$subset.FL & 5 %notin% params$subset.FL ) {
        parms.geo <- gsub( "eastern Florida", "southeastern Florida", parms.geo )
        caps.geo <- " FLE includes only southeastern Florida."
      }
    }
    rm( outside.areas )
    
  } else if( params$region == "South Atlantic" ) {
    
    ### SOUTH ATLANTIC ###
    ### ------------------
    
    outside.areas <- params$subset.states[ which( params$subset.states %notin% c("FLE","GA","SC","NC","SNC","NNC") ) ]
    outside.areas <- area.replace( outside.areas )
    if( length(outside.areas)==0 ) {
      parms.geo <- paste0( params$region," states from ",inc.states[1]," to ",inc.states[2],"." )
    } else {
      ###   ...where, unlike the Gulf, the boundaries of this assessment can extend both north and south of the boundaries
      ###         of the South Atlantic management region. Therefore, logical statements are needed to consider "outside.areas"
      ###         found in the Gulf, Mid/North ATL, and both...
      
      ### Starting with assessments that extend beyond the southern boundary (GOM) but not into the north Atlantic...
      if( "western Florida" %in% outside.areas & "Virginia" %notin% outside.areas ) {
        outside.gulf = outside.areas[ outside.areas %in% c("Texas","Louisiana","Mississippi","Alabama","western Florida") ]
        if( length(outside.gulf)==1 ) {
          parms.geo <- paste0( params$region," states from ",inc.states[2]," to eastern Florida",
                               ", including ",outside.gulf,"." )
        } else {
          parms.geo <- paste0( params$region," states from ",inc.states[2]," to eastern Florida",
                               ", including western Florida to ",outsides.gulf[1],"." )
        }
        rm(outside.gulf)
        
        ###    ...and for assessments extending into the north Atlantic but not the GOM...
      } else if( "western Florida" %notin% outside.areas & "Virginia" %in% outside.areas ) {
        outside.atl = outside.areas[ outside.areas %in% c("Virginia","Maryland","Delaware","Pennsylvania","New Jersey","New York",
                                                          "Connecticut","Rhode Island","Massachusetts","New Hampshire","Maine" ) ]
        if( length(outside.atl)==1 ) {
          parms.geo <- paste0( params$region," states from ",inc.states[1]," to North Carolina",
                               ", including ",outside.atl,"." )
        } else {
          parms.geo <- paste0( params$region," states from ",inc.states[1]," to North Carolina",
                               ", including Virginia to ",outside.atl[length(outside.atl)],"." )
        }
        rm(outside.atl)
        
        ###    ...and for assessments that extend into both the north Atlantic and GOM...
      } else if( "western Florida" %in% outside.areas & "Virginia" %in% outside.areas ) {
        outside.gulf = outside.areas[ outside.areas %in% c("Texas","Louisiana","Mississippi","Alabama","western Florida") ]
        if( length(outside.gulf)==1 ) {
          parms.geo <- paste0( "Atlantic states from ",outside.areas[length(outside.areas)]," to eastern Florida",
                               ", including ",outside.gulf,"." )
        } else {
          parms.geo <- paste0( "Atlantic states from ",outside.areas[length(outside.areas)]," to eastern Florida",
                               ", including western Florida to ",outside.gulf[1],"." )
        }
        rm(outside.gulf)
      }
    }
    rm( outside.areas )
    
    ### Using similar code as that applied for the GOM, I then evaluate whether I need to substitute any text
    ###       for "FLW" or "FLE", as decided based on which FL subregions are being considered in this assessment...
    if( "western Florida" %in% inc.states ) {     ### ...where assessment boundaries start/end at "FLW"...
      if( FLK.only == TRUE ) {
        parms.geo <- gsub( "western Florida", "the Florida Keys", parms.geo )
        ###   ...where in these cases, "FLW" is replaced by "FLKeys" in tables/figures and so I do not need
        ###       to add any additional text to the captions (i.e., nothing added to "caps.geo" )...
      } else if( ( 3 %in% params$subset.FL & 2 %in% params$subset.FL ) & 1 %notin% params$subset.FL ) {
        parms.geo <- gsub( "western Florida", "western Florida excluding the Florida panhandle", parms.geo )
        caps.geo <- " FLW excludes the Florida panhandle."
      }
    }
    if( "eastern Florida" %in% inc.states ) {
      if( 5 %in% params$subset.FL & 4 %notin% params$subset.FL ) {
        parms.geo <- gsub( "eastern Florida", "northeastern Florida", parms.geo )
        caps.geo <- " FLE includes only northeastern Florida."
      }
    }
    
    ### Similarly, I also evaluate whether this assessment included all of NC, or if it was "broken" at Cape Hatteras...
    if( "North Carolina" %in% inc.states ) {     ### ...where assessment boundaries start/end at "NC"...
      if( 6 %in% params$subset.NC & 7 %notin% params$subset.NC ) {
        parms.geo <- gsub( "North Carolina", "Cape Hatteras (NC)", parms.geo )
        caps.geo <- " NC only includes those areas south of Cape Hatteras."
      }
    }
    
  } else if( params$region == "Atlantic" ) {
    
    ### ATLANTIC ###
    ### ------------
    outside.areas <- params$subset.states[ which( params$subset.states %notin%
                                                    c("FLE","GA","SC","NC","SNC","NNC",
                                                      "VA","MD","DE","PA","NJ","NY","CT","RI","MA","NH","ME") ) ]
    outside.areas <- area.replace( outside.areas )
    if( length(outside.areas)==0 ) {
      parms.geo <- paste0( params$region," states from ",inc.states[1]," to ",inc.states[2],"." )
    } else {
      parms.geo <- paste0( params$region," states from ",inc.states[2],
                           " to eastern Florida, including ",paste( outside.areas, collapse=" and " ),"." )
      ###     ...where, in this case, "outside.areas" CAN ONLY BE Gulf of Mexico states (we don't assess Canadian waters).
      ###       Thus, logic states that FLE must be included in "params$subset.states"; we're assessing an ATL stock
      ###       that includes areas of the Atlantic and so if FLE is not in "params$subset.states", then we essentially
      ###       have a hole in the area being considered (by the assessment). Following this, I simply identify
      ###       'eastern Florida' in the text (instead of defining it dynamically)...
    }
    
    ### Using similar code as that applied for the GOM, I then evaluate whether I need to substitute any text
    ###       for "FLW" or "FLE", as decided based on which FL subregions are being considered in this assessment...
    if( "eastern Florida" %in% inc.states ) {     ### ...where assessment boundaries start/end at "FLE"...
      if( 5 %in% params$subset.FL & 4 %notin% params$subset.FL ) {
        parms.geo <- gsub( "eastern Florida", "northeastern Florida", parms.geo )
        caps.geo <- " FLE includes only northeastern Florida."
      }
    }
    
    if( "western Florida" %in% inc.states ) {
      if( FLK.only == TRUE ) {
        parms.geo <- gsub( "western Florida", "the Florida Keys", parms.geo )
        ###   ...where in these cases, "FLW" is replaced by "FLKeys" in tables/figures and so I do not need
        ###       to add any additional text to the captions (i.e., nothing added to "caps.geo" )...
      } else if( ( 3 %in% params$subset.FL & 2 %in% params$subset.FL ) & 1 %notin% params$subset.FL ) {
        parms.geo <- gsub( "western Florida", "western Florida excluding the Florida panhandle", parms.geo )
        caps.geo <- " FLW excludes the Florida panhandle."
      }
    }
    rm( outside.areas )
    
  } else {
    
    ### CARIBBEAN ###
    ### -------------
    if( length(inc.states)==1 ) {
      parms.geo <- paste0( inc.states[1]," (US ",params$region,")." )
      ###     ...which is essentially 'Caribbean' text as these assessments are the only ones considering a single area...
      
      ### OTHER ###
      ### ---------
      ###   ...which currently include assessments that cover...
      ###       -- 'SOUTHEAST' -- which covers the entire eastern US coastline ( TX-ME ),
      ###       -- 'GULF OF AMERICA AND SOUTH ATLANTIC' -- which covers both the GOM & SATL, but not the NATL or MATL,
    } else if( params$region %in% c( 'Southeast' , 'Gulf of America and South Atlantic' ) ) {
      parms.geo <- paste0( "Atlantic and Gulf coast states from ",inc.states[1]," to ",inc.states[2],"." )
      ###
      ###   ...and any other LOVs created for the params$region object ( this else() statement is largely a catch-all )
      ###       -- 'MID ATLANTIC'   -- which covers VA-NY
      ###       -- 'NORTH ATLANTIC' -- which covers CT-ME
      ###   ...and any other LOVs that may be created for the params$region object ( this else() statement is a catch-all
      ###       to prevent the code from breaking, but currently only applies to MATL and NATL assessments )...
    } else {
      parms.geo <- paste0( params$region," states from ",inc.states[1]," to ",inc.states[2],"." )
    }
    
  }
  
  return.object = list( parms.geo, caps.geo )
  names(return.object) = c( 'parms.geo','caps.geo' )
  
  return( return.object )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------


wp.text.mode = function( params, inc.modes ) {
  ###     ...where 'params' is the R object that (amongst other things) identifies the filters applied in this SEDAR...
  ###          and 'inc.modes' identifies the modes included in this assessment (e.g., = c( 'Charter','Headboat','Private' ) )
  
  
  ### Headboat Boundaries ###
  ### _______________________
  ###
  ###     ...where "parms.mode" is written into the "Fishing Modes" section of the manuscript body...
  ###       ...and "caps.mode" is added to the (by-mode) table/figure captions...
  ###
  parms.mode <- inc.modes
  caps.mode <- ""
  if( "Headboat" %in% inc.modes ) {
    
    ### ...where I start by making sure "params$subset.HBT" isn't formatted as a list...
    hbt.areas <- unlist( params$subset.HBT )
    
    assess.areas <- as.list(params$subset.states)
    assess.areas <- unlist( lapply( assess.areas, function(x) if( x == "FLW" ) ( params$subset.FL[ which(params$subset.FL %in% c(1,2,3) ) ] ) else x) )
    assess.areas <- unlist( lapply( assess.areas, function(x) if( x == "FLE" ) ( params$subset.FL[ which(params$subset.FL %in% c(4,5) ) ] ) else x) )
    
    if( any( hbt.areas %notin% assess.areas ) | any( assess.areas %notin% hbt.areas ) ) {
      ###   ...where text is only written to Table/Figure captions if HBT estimates are calculated over areas ( "hbt.areas" )
      ###        that differ from the areas considered by the assessment as a whole (i.e., in "assess.areas" )...
      parms.mode <- parms.mode[ which( parms.mode != "Headboat" ) ]
      
      if( length(hbt.areas)==1 ) {
        if(  any( c("TX","LA","MS","AL","1","2","3") %in% hbt.areas ) &
             !any( c("VA","MD","DE","PA","NJ","NY","CT","RI","MA","NH","ME") %in% hbt.areas ) ) {
          parms.mode <- c( parms.mode, paste0( "Headboat (1981-1985 from only ",area.replace(hbt.areas),")" ) )
          caps.mode <- paste0( " MRIP Headboat estimates (1981-1985) are only included from ",area.replace(hbt.areas),"." )
        } else if( !any( c("TX","LA","MS","AL","1","2","3") %in% hbt.areas ) &
                   any( c("VA","MD","DE","PA","NJ","NY","CT","RI","MA","NH","ME") %in% hbt.areas ) ) {
          parms.mode <- c( parms.mode, paste0( "Headboat (from only ",area.replace(hbt.areas),")" ) )
          caps.mode <- paste0( " MRIP Headboat estimates are only included from ",area.replace(hbt.areas),"." )
        }   ### ...wherein I don't need an else() statement because length(hbt.areas)==1, so can only be GOM or north ATL (but not both)...
      } else if( length(hbt.areas) > 1 ) {
        if(  any( c("TX","LA","MS","AL","1","2","3") %in% hbt.areas ) &
             !any( c("VA","MD","DE","PA","NJ","NY","CT","RI","MA","NH","ME") %in% hbt.areas ) ) {
          parms.mode <- c( parms.mode, paste0( "Headboat (1981-1985 from ",area.replace(hbt.areas[1])," to ",
                                               area.replace(hbt.areas[length(hbt.areas)]),")" ) )
          caps.mode <- paste0( " MRIP Headboat estimates (1981-1985) are included from ",area.replace(hbt.areas[1])," to ",
                               area.replace(hbt.areas[length(hbt.areas)]),"." )
        } else if( !any( c("TX","LA","MS","AL","1","2","3") %in% hbt.areas ) &
                   any( c("VA","MD","DE","PA","NJ","NY","CT","RI","MA","NH","ME") %in% hbt.areas ) ) {
          parms.mode <- c( parms.mode, paste0( "Headboat (",area.replace(hbt.areas[1])," to ",
                                               area.replace(hbt.areas[length(hbt.areas)]),")" ) )
          caps.mode <- paste0( " MRIP Headboat estimates are included from ",area.replace(hbt.areas[1])," to ",
                               area.replace(hbt.areas[length(hbt.areas)]),"." )
        } else {
          hbt.gulf = hbt.areas[ which( hbt.areas %in% c("TX","LA","MS","AL","1","2","3") ) ]
          hbt.natl = hbt.areas[ which( hbt.areas %in% c("VA","MD","DE","PA","NJ","NY","CT","RI","MA","NH","ME") ) ]
          if( length(hbt.gulf)==1 & length(hbt.natl)==1 ) {
            parms.mode <- c( parms.mode, paste0( "Headboat (1981-1985 from ",area.replace(hbt.gulf)," and",
                                                 " 1981+ from ",area.replace(hbt.natl),")" ) )
            caps.mode <- paste0( " MRIP Headboat estimates are included from ",area.replace(hbt.gulf)," (1981-1985) and ",
                                 area.replace(hbt.natl)," (1981+)." )
          } else if( length(hbt.gulf)>1 & length(hbt.natl)==1 ) {
            parms.mode <- c( parms.mode, paste0( "Headboat (1981-1985 from ",area.replace(hbt.gulf[1])," to ",
                                                 area.replace(hbt.gulf[length(hbt.gulf)])," and 1981+ from ",area.replace(hbt.natl),")" ) )
            caps.mode <- paste0( " MRIP Headboat estimates are included from ",area.replace(hbt.gulf[1])," to ",
                                 area.replace(hbt.gulf[length(hbt.gulf)])," (1981-1985) and ",area.replace(hbt.natl)," (1981+)." )
          } else if( length(hbt.gulf)==1 & length(hbt.natl)>1 ) {
            parms.mode <- c( parms.mode, paste0( "Headboat (1981-1985 from ",area.replace(hbt.gulf)," and ",
                                                 " 1981+ from ",area.replace(hbt.natl[1])," to ",area.replace(hbt.natl[length(hbt.natl)]),")" ) )
            caps.mode <- paste0( " MRIP Headboat estimates are included from ",area.replace(hbt.gulf)," (1981-1985) and ",
                                 area.replace(hbt.natl[1])," to ",area.replace(hbt.natl[length(hbt.natl)])," (1981+)." )
          } else {
            parms.mode <- c( parms.mode, paste0( "Headboat (1981-1985 from ",area.replace(hbt.gulf[1])," to ",area.replace(hbt.gulf[length(hbt.gulf)]),
                                                 " and 1981+ from ",area.replace(hbt.natl[1])," to ",area.replace(hbt.natl[length(hbt.natl)]),")" ) )
            caps.mode <- paste0( " MRIP Headboat estimates are included from ",area.replace(hbt.gulf[1])," to ",
                                 area.replace(hbt.gulf[length(hbt.gulf)]),
                                 " (1981-1985) and ",area.replace(hbt.natl[1])," to ",area.replace(hbt.natl[length(hbt.natl)])," (1981+)." )
          }
          rm( hbt.gulf,hbt.natl )
        }
      }
    } else if( params$region == "Gulf of America" ) {
      
      if( 3 %in% params$subset.FL ) {
        parms.mode = gsub( "Headboat","Headboat (1981-1985, excluding the Florida Key)", parms.mode )
      } else {
        parms.mode = gsub( "Headboat","Headboat (1981-1985)", parms.mode )
      }
      
    }
    rm( hbt.areas, assess.areas )
  }
  
  parms.mode = paste( parms.mode, collapse=", ")
  
  
  return.object = list( parms.mode, caps.mode )
  names(return.object) = c( 'parms.mode','caps.mode' )
  
  return( return.object )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------


wp.text.cap2 = function( params, inc.modes ) {
  ###     ...where 'params' is the R object that (amongst other things) identifies the filters applied in this SEDAR...
  ###          and 'inc.modes' identifies the modes included in this assessment (e.g., = c( 'Charter','Headboat','Private' ) )
  
  
  ### Figure-2 Caption ###
  ### ____________________
  ###
  ###     ...as a last step, and only for Figure 2, I check to see if the spatial coverage
  ###       of the assessment matches the domains used on the NMFS MRIP-query website, and insert text if not...
  ###
  caps.fig2 <- " (NMFS pers comm)."
  
  assess.areas <- as.list(params$subset.states)
  assess.areas <- unlist( lapply(
    assess.areas, function(x) if( x == "FLW" ) ( params$subset.FL[ which(params$subset.FL %in% c(1,2,3) ) ] ) else x) )
  assess.areas <- unlist( lapply(
    assess.areas, function(x) if( x == "FLE" ) ( params$subset.FL[ which(params$subset.FL %in% c(4,5) ) ] ) else x) )
  ###     ...where I exclude "FLW" & "FLE" because params$subset.FL will add the numeric FL_REG codes for these areas...
  if( all( c('NC','SNC','NNC') %in% unlist(assess.areas) ) ) {
    assess.areas = assess.areas[ assess.areas %notin% c('SNC','NNC') ]
  }
  assess.areas <- unlist( lapply(
    assess.areas, function(x) if( x == "NC" ) ( params$subset.NC[ which(params$subset.NC %in% c(6,7) ) ] ) else x) )
  
  natl.flag = FALSE
  if( params$region == "Caribbean" ) {
    website.domains = "PR"
  } else if( params$region == "Gulf of America" ) {
    website.domains <- sort(c("TX","LA","MS","AL","1","2","3"))
  } else if( params$region == "South Atlantic" ) {
    ### ...where some SATL assessments go as far north (into the ATL) as there is catch, wherein params$region == "South Atlantic"
    ###       but "website.domains" needs to include much more than just FLE-NC. Therefore, I add an additional if() statement and
    ###       define a "flag" so that my Fig2.caption reflects this distinction...
    if( all( c("VA","MD","DE","PA","NJ","NY","CT","RI","MA","NH","ME") %in% params$subset.states ) ) {
      website.domains <- sort(c("4","5","GA","SC","6","7","VA","MD","DE","PA","NJ","NY","CT","RI","MA","NH","ME"))
      natl.flag = TRUE
    } else {
      website.domains <- sort(c("4","5","GA","SC","6","7"))
    }
  } else if( params$region == "Mid Atlantic" ) {
    website.domains <- sort( c("VA","MD","DE","PA","NJ","NY") )
  } else if( params$region == "North Atlantic" ) {
    website.domains <- sort( c("CT","RI","MA","NH","ME") )
    
  } else if( params$region == "Gulf of America and South Atlantic" ) {
    website.domains <- sort( c("TX","LA","MS","AL","1","2","3","4","5","GA","SC","6","7") )
    
  } else if( params$region == "Atlantic" ) {
    website.domains <- sort( c("4","5","GA","SC","6","7","VA","MD","DE","PA","NJ","NY","CT","RI","MA","NH","ME") )
    
  } else if( params$region == "Southeast" ) {
    ###     ...which tends to be what's used to designate assessments that cover "everything"...
    website.domains <- sort( c("TX","LA","MS","AL","1","2","3",
                               "4","5","GA","SC","6","7","VA","MD","DE","PA","NJ","NY","CT","RI","MA","NH","ME") )
  }
  
  # if( any( assess.areas %notin% website.domains ) ) {
  if( any( assess.areas %notin% website.domains ) | any( website.domains %notin% assess.areas ) ) {
    
    extra.areas <- website.domains[ which( website.domains %notin% assess.areas ) ]
    ignore.areas <- assess.areas[ which( assess.areas %notin% website.domains ) ]
    
    if( length(ignore.areas) > 0 & length(extra.areas) > 0 ) {
      caps.fig2 <- paste0( ". Estimates in this figure do not include ",
                           paste( area.replace(ignore.areas), collapse=" or " ),
                           " but do include ",paste( area.replace(extra.areas), collapse=" and " ),
                           " as these domains are not separable from those used by the MRIP online comparison tool for the ",
                           if(natl.flag){"Atlantic"}else{params$region}," (NMFS pers comm)." )
    } else if( length(ignore.areas) > 0 ) {
      if( length(ignore.areas) == 1 ) {
        caps.fig2 <- paste0( ". Estimates in this figure do not include ",area.replace(ignore.areas),
                             " as that domain is not available from the MRIP online comparison tool (NMFS pers comm)." )
      } else {
        caps.fig2 <- paste0( ". Estimates in this figure do not include ",paste( area.replace(ignore.areas), collapse=" or " ),
                             " as these domains are not available from the MRIP online comparison tool (NMFS pers comm)." )
      }
    } else if( length(extra.areas) > 0 ) {
      if( length(extra.areas) == 1 ) {
        caps.fig2 <- paste0( ". Estimates in this figure include ",area.replace(extra.areas),
                             " as that domain is not separable from those used by the MRIP online comparison tool for the ",
                             if(natl.flag){"Atlantic"}else{params$region}," (NMFS pers comm)." )
      } else {
        caps.fig2 <- paste0( ". Estimates in this figure include ",paste( area.replace(extra.areas), collapse=" and " ),
                             " as these domains are not separable from those used by the MRIP online comparison tool for the ",
                             if(natl.flag){"Atlantic"}else{params$region}," (NMFS pers comm)." )
      }
    }
    
    ### I then check to see if there is a Figure-2a in this report and (if so) whether this plot describes some of the areas
    ###     represented by either "ignore.areas" or "extra.areas", wherein I add a bit more text...
    if( ( params$add.FL.CV.Fig == "East Florida" & ( any( ignore.areas %in% c(4,5) ) | any( extra.areas %in% c(4,5) ) ) ) |
        ( params$add.FL.CV.Fig == "West Florida" & ( any( ignore.areas %in% c(1,2,3) ) | any( extra.areas %in% c(1,2,3) ) ) ) ) {
      caps.fig2 <- paste0( caps.fig2," Refer to Figure 2a to compare calibrations of ",params$species.name,
                           " catch estimates from ",params$add.FL.CV.Fig,"." )
    }
    rm( ignore.areas, extra.areas )
  }
  rm( assess.areas, website.domains )
  
  ### I then add text to identify any modes included in the MRIP query that are not considered by the assessment.
  ###       The issue here is that Fig2 compares MRIP calibrations (base vs apais vs fes) and so doesn't look at
  ###       mode-specific estimates. Because I need total CV estimates (i.e., PSE) and cannot combine mode-specific CVs,
  ###       I have to extract "total" estimates from the MRIP query. Therefore, I flag missing modes in the caption...
  nmfs.modes <- c( "Charter","Headboat","Private","Shore" )
  if( any( nmfs.modes %notin% inc.modes ) ) {
    missing.modes <- tolower( nmfs.modes[ which( nmfs.modes %notin% inc.modes ) ] )
    mode.text <- paste0( missing.modes, collapse=" and " )
    mode.text <- paste( toupper( substr( mode.text,1,1 ) ), substr( mode.text,2,nchar(mode.text) ), sep="" )
    if( length(missing.modes) == 1 ) {
      caps.fig2 <- paste0( caps.fig2," The ",mode.text," mode is also included as uncertainty estimates for catch",
                           " across multiple modes are only available when all modes are selected." )
    } else {
      caps.fig2 <- paste0( caps.fig2," The ",mode.text," modes are also included as uncertainty estimates for catch",
                           " across multiple modes are only available when all modes are selected." )
    }
  }
  rm( missing.modes )
  
  ### Lastly, because the areas defined above considered FL subregions, I check to see if I can reduce the text by
  ###       substituting FLW or FLE into captions when all corresponding FL subregions are included...
  caps.fig2 <- gsub( "southeastern Florida or northeastern Florida", "East Florida", caps.fig2 )
  caps.fig2 <- gsub( "southeastern Florida and northeastern Florida", "East Florida", caps.fig2 )
  caps.fig2 <- gsub( "the Florida panhandle or western Florida or the Florida Keys", "West Florida", caps.fig2 )
  caps.fig2 <- gsub( "the Florida panhandle and western Florida and the Florida Keys", "West Florida", caps.fig2 )
  
  
  return.object = list( caps.fig2, natl.flag )
  names(return.object) = c( "caps.fig2","natl.flag" )
  
  return( return.object )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------





