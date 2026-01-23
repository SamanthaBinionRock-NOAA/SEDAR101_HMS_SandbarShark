

### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------
###
###     DESCRIPTION
###
###     ...wherein this script contains all functions needed to construct the various TABLE/FIGURE SUMMARIES
###         required of ALLREC data (e.g., as needed in the Rec section of SEDAR Data Workshop reports )...
###
###    ---------------------
###       DATA COMPILATION
###    ---------------------
###    *** compile.genrec.dat( )
###         ...which compiles raw MRIP data/estimates into summaries by year-state-mode...
###
###    *** compile.srfs.dat( )
###         ...which compiles raw SRFS data/estimates into summaries by year-state-mode...
###
###
###    *** compile.srhs.dat( )
###         ...which compiles raw SRHS data/estimates into summaries by year-state-mode...
###
###    *** compile.srhs.conf( )
###         ...which compiles the 'confidentiality table' for SRHS estimates...
###
###    *** join.srhs.conf( )
###         ...which joins SRHS estimate tables with confidentiality info for summary tables/figures...
###
###
###    ----------------
###       FLEXTABLES
###    ----------------
###    *** FT.allrec.catnum.yr.strata( )
###         ...which builds a flextable of AllRec catch estimates (i.e., AB1, B2, LBS ) for this SEDAR...
###
###
###    ----------------
###         FIGURES
###    ----------------
###    *** FIG.allrec.strata( )
###         ...which produces multiple figures summarizing (total) AllRec estimates (i.e., AB1, B2, EFF )
###           by different stratifications (i.e., by year-area, by year-mode, and by area-mode )...
###
###    *** FIG.allrec.byMode( )
###         ...which produce stacked barplots to compare AllRec estimates for individual modes
###           (i.e., timeseries of AB1/B2/EFF, by SID as appropriate )...
###
### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------

compile.genrec.dat = function ( table.type = c('catch','effort'),
                                group.vars = NA,
                                value.vars = c('AB1','B2','LBSEST_SECWWT','ESTRIPS','CHTS_TRIPS'),
                                params ) {
  ###     ...where 'table.type' identifies the type of table being constructed (e.g., catch or effort )...
  ###              'group.vars' identifies the strata over which (catch or effort) sums are to be calculated...
  ###              'value.vars' identifies the variables being summarized (when appropriate)...
  ###                    -- Note that when group.vars = NA, the entire (raw) GenRec table is returned
  ###                       by this function and so no 'value.vars' need to be summarized...
  ###       ...and 'params' is a list of inputs defined in the RMarkdown YAML, including the location
  ###                       (i.e., file name & associated tab ) of raw GenRec estimates...
  
  
  return.object = list()
  
  
  if( table.type == 'catch' ) {
    mrip.raw <- read_excel( path  = paste0( params$s.dir,"/",params$genrec.cat.file ),
                            sheet = params$genrec.cat.tab,
                            trim_ws = FALSE, col_types = "text" )
    
  } else if( table.type == 'effort' ) {
    mrip.raw <- read_excel( path  = paste0( params$s.dir,"/",params$genrec.eff.file ),
                            sheet = params$mrip.eff.tab,
                            trim_ws = FALSE, col_types = "text" )
  }
  
  if( params$genrec.est.flag != "None" ) {
    eval( parse( text = paste0( "mrip.raw = mrip.raw %>% filter( ",params$genrec.est.flag," == 'Y' )" ) ) )
  }
  
  mrip.raw = mrip.raw %>%
    rename( any_of( c( YEAR = "INT_YEAR" ) ) ) %>%
    mutate( across( any_of( c("YEAR") ), as.integer ) ) %>%
    mutate( across( any_of( c("NEW_ST","FL_REG","NEW_MODE",
                              "AB1","B2","lbsest_SECwwt","ESTRIPS","CHTS_TRIPS") ), as.numeric ) )
  
  if( table.type == 'effort' ) {
    
    if( params$region %in% c( "Gulf of America","Gulf of America and South Atlantic","Southeast" ) ){
      
      lacr.raw <- read_excel( path  = paste0( params$s.dir,"/",params$genrec.eff.file ),
                              sheet = params$lacr.eff.tab,
                              trim_ws = FALSE, col_types = "text" )
      lacr.raw <- lacr.raw %>%
        rename( YEAR = INT_YEAR,
                ESTRIPS = EXPANDED_EFFORT ) %>%
        
        mutate_at( vars(   YEAR), list( ~ as.integer(.) ) ) %>%
        mutate_at( vars(ESTRIPS), list( ~ as.numeric(.) ) ) %>%
        mutate_at( vars(NEW_ST,NEW_MODE), list( ~ as.numeric(.) ) )
      
      
      tpwd.raw <- read_excel( path  = paste0( params$s.dir,"/",params$genrec.eff.file ),
                              sheet = params$tpwd.eff.tab,
                              trim_ws = FALSE, col_types = "text" )
      tpwd.raw <- tpwd.raw %>%
        rename( YEAR = CYEAR,
                ESTRIPS = NTRP ) %>%
        
        mutate_at( vars(   YEAR), list( ~ as.integer(.) ) ) %>%
        mutate_at( vars(ESTRIPS), list( ~ as.numeric(.) ) ) %>%
        mutate_at( vars(NEW_ST,NEW_MODE), list( ~ as.numeric(.) ) )
      
      
      comb.effort <- bind_rows( tpwd.raw, lacr.raw, mrip.raw )
      mrip.raw = comb.effort
      rm(comb.effort)
    }
    
  }
  
  mrip.raw = mrip.raw %>%
    mutate( NEW_MODEN = toupper(NEW_MODEN) ) %>%
    mutate( NEW_MODEN = ifelse( NEW_MODEN == 'PRIV/SHORE', 'PRIV', NEW_MODEN ) )
  
  colnames(mrip.raw) = toupper( colnames(mrip.raw) )
  
  if( 'FED_CLOSED' %in% colnames(mrip.raw) ) {
    mrip.raw = mrip.raw %>% mutate( FED_CLOSED = toupper(FED_CLOSED) )
  }
  
  
  if( "SID" %in% colnames(mrip.raw) ) {
    ###   ...where if this assessment separates the stock into distinct SID domains, the associated "SID" field is
    ###     formatted as a factor, with levels defined geographically from the NEW_ST, FL_REG, & NC_REG fields. Note
    ###     that this order is saved ( to 'return.object' ) so it can be applied to other tables (e.g., SRHS )...
    
    mrip.raw = mrip.raw %>% mutate( SID = toupper(SID) )
    
    ### Formatting 'SID' as Factor ###
    mrip.raw = mrip.raw %>%
      mutate( INDEX = ifelse( !is.na(FL_REG), as.numeric(NEW_ST) + ( as.numeric(FL_REG)/10 ),
                      ifelse( !is.na(NC_REG), ifelse( NC_REG == 'S', as.numeric(NEW_ST) + 0.1,
                                              ifelse( NC_REG == 'N', as.numeric(NEW_ST) + 0.2, NA )),
                              as.numeric(NEW_ST) )) )
    
    area.order = unique( mrip.raw$SID[ order( mrip.raw$INDEX ) ] )
    mrip.raw = mrip.raw %>%
      mutate( SID = factor( SID, levels = area.order ) ) %>%
      select( -INDEX )
    
    return.object$area.order = area.order
  }
  
  
  if( params$srfs.cat.file != 'None' ) {
    ###   ...where SRFS only provides estimates for private, which need to be removed from 'mrip.raw'...
    mrip.raw = mrip.raw %>% filter( !( NEW_STA %in% c('FLE','FLW') & NEW_MODEN == 'PRIV' ) )
  }
  
  if( !all(is.na(group.vars)) ) {
    mrip.raw = mrip.raw %>%
      group_by( across( any_of(group.vars) ) ) %>%
      # summarise( across( all_of(value.vars), sum ) )
      summarize( across( all_of( toupper(value.vars) ), ~ sum( ., na.rm=TRUE ) ) )
  }
  
  
  ###   As a final step, I then check to see if any additional (spatial) information needs to be added to the
  ###   raw GenRec table. For example, it's possible that this assessment isn't for a "SID" species, but we
  ###   may need an additional "SID" field to match the resolution at which SRHS-proxy discards were estimated
  ###   (e.g., if not estimated at a state level )...
  if( "SID" %notin% colnames(mrip.raw) & params$srhs.proxy.level != "state" ) {
    mrip.raw = mrip.raw %>% mutate( SID = NA )
    for( i in 1:length( params$srhs.proxy.translations.state ) ) {
      mrip.raw$SID[ mrip.raw$NEW_STA == params$srhs.proxy.translations.state[i] ] =
                               toupper( params$srhs.proxy.translations.sid[i] )
    }
    rm(i)
  }
  
  
  return.object$genrec.raw = mrip.raw
  
  
  
  return( return.object )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------

compile.srhs.dat = function ( table.type = c('catch','effort'),
                              group.vars = c('state','region'),
                              value.vars = c('AB1','B2','LBSEST_SECWWT','ESTRIPS','ESTDAYS'),
                              params ) {
  ###     ...where 'table.type' identifies the type of table being constructed (e.g., catch or effort )...
  ###              'group.vars' identifies the strata over which (catch or effort) sums are being provided.
  ###                    -- Unlike the 'genrec' summary in the function above, SRHS estimates at different
  ###                       resolutions (i.e., state vs. region ) are provided in different tabs, and so
  ###                       this object identifies the tab from which SRHS estimates are to be extracted,
  ###                       (i.e., not the strata fields being used in a group_by() statement )...
  ###              'value.vars' identifies the catch variables being imported...
  ###       ...and 'params' is a list of inputs defined in the RMarkdown YAML, including the location
  ###                       (i.e., file name & associated tab ) of raw GenRec estimates...
  
  
  # return.object = list()
  
  
  ### ------------
  ### LANDINGS ###
  ### ------------
  
  if( 'AB1' %in% value.vars ) {
    
    if( group.vars == 'state' ) {
      srhs.land.dat <- read_excel( path  = paste0( params$s.dir,"/",params$srhs.cat.file ),
                                   sheet = 'land_state_n',
                                   trim_ws = FALSE, col_types = "text" )
      
    } else if( group.vars == 'region' ) {
      srhs.land.dat <- read_excel( path  = paste0( params$s.dir,"/",params$srhs.cat.file ),
                                   sheet = 'land_region_n',
                                   trim_ws = FALSE, col_types = "text" )
    }
    
    srhs.land.dat = srhs.land.dat %>%
      rename_with( toupper ) %>%
      
      mutate_at( vars( YEAR), list( ~as.integer(.) ) ) %>%
      mutate_at( vars(!YEAR), list( ~as.numeric(.) ) ) %>%
      
      select( -any_of( 'TOTAL' ) ) %>%
      pivot_longer( !YEAR, names_to = "AREA", values_to = "AB1" )
    
    ###   ...where the columns in the original SRHS tables ( now saved as 'AREA' ) tend to be
    ###     spatial summaries of either state or by region/SID. However, this 'AREA' field can
    ###     sometimes include info on an additional strata, in which case we need to break apart
    ###     the current 'AREA' field to separate this info. In these cases, we apply an additional
    ###     group_by() & summarize() statement to collapse this field...
    
    if( any( grepl( '_', srhs.land.dat$AREA ) ) ) {
      srhs.land.dat = srhs.land.dat %>%
        mutate( FIELD_TO_DROP = gsub( '.*_','', AREA ) ) %>%
        mutate( AREA = gsub( '_.*','', AREA ) ) %>%
        
        group_by( YEAR, AREA ) %>%
        summarize( AB1 = sum( AB1, na.rm=TRUE ) ) %>%
        ungroup()
      
      # ###     ...where the script below was written to retain the 'FIELD_TO_DROP' variable,
      # ###       the name of which was identified by an additional 'strata.add' argument
      # ###       provided in the initial list of functionarguments...
      # command.line = paste0( "srhs.land.dat = srhs.land.dat %>% ",
      #                        " mutate( ",strata.add," = gsub( '.*_','', AREA) ) %>% ",
      #                        " mutate( ",strata.add," = toupper(",strata.add,") ) %>% ",
      #                        " mutate( AREA = gsub( '_.*','', AREA) )" )
      # eval( parse( text = command.line ) )
      # rm( command.line )
    }
    
    srhs.land.dat = srhs.land.dat %>%
      # mutate( DS = 'SRHS' ) %>%
      mutate( NEW_MODEN = 'HBT' )
    
    
    ### IMPUTATION OF HISTORICAL (1981-85) TEXAS ESTIMATES ###
    if( params$srhs.TX.imp ) {
      
      srhs.TX.land <- read_excel( path  = paste0( params$s.dir,"/",params$srhs.cat.file ),
                                  sheet = 'land_state_n',
                                  trim_ws = FALSE, col_types = "text" )
      
      srhs.TX.land = srhs.TX.land %>%
        rename_with( toupper ) %>%
        mutate_at( vars( YEAR), list( ~as.integer(.) ) ) %>%
        mutate_at( vars(!YEAR), list( ~as.numeric(.) ) ) %>%
        select( YEAR, TX )
      
      avg.years = as.numeric( params$srhs.TX.imp.years )
      # avg.years = as.numeric( gsub(":.*","",avg.years) ):as.numeric( gsub(".*:","",avg.years) )
      
      imp.TX.catch = mean( srhs.TX.land$TX[ srhs.TX.land$YEAR %in% avg.years ] )
      imp.TX.table = data.frame( YEAR = 1981:1985,
                                 AREA = params$srhs.TX.imp.area,
                                 AB1  = imp.TX.catch )
      
      imp.TX.table = imp.TX.table %>%
        # mutate( DS = 'SRHS_sub' ) %>%
        mutate( NEW_MODEN = 'HBT' )
      
      srhs.land.dat = bind_rows( imp.TX.table, srhs.land.dat )
      rm( srhs.TX.land, imp.TX.table, imp.TX.catch, avg.years )
    }
    
    ###   Lastly, I check the resolution at which SRHS proxy discards were estimated, to ensure the spatial strata
    ###   used in all my tables match. In particular, if we're pulling SRHS estimates at a 'state' level, but
    ###   SRHS proxies were estimated at a region/SID level, then we need to collapse the SRHS tables to match
    ###   the spatial resolution of our proxy discards...
    if( group.vars == "state" & params$srhs.proxy.level != "state" ) {
      
      srhs.land.dat = srhs.land.dat %>% rename( NEW_STA = AREA ) %>% mutate( AREA = NA )
      for( i in 1:length( params$srhs.proxy.translations.state ) ) {
        srhs.land.dat$AREA[ srhs.land.dat$NEW_STA == params$srhs.proxy.translations.state[i] ] =
          toupper( params$srhs.proxy.translations.sid[i] )
      }
      srhs.land.dat = srhs.land.dat %>%
        group_by( across( any_of( c('NEW_MODEN','YEAR','AREA') ) ) ) %>%
        summarize( AB1 = sum( AB1, na.rm=TRUE ) )
      rm( i )
    }
    
    
    # return.object[[length(return.object)+1]] = srhs.land.dat
    # names(return.object)[length(return.object)] = 'srhs.land.num'
    # rm( srhs.land.dat )
    
  }
  
  
  ### ------------
  ### DISCARDS ###
  ### ------------
  
  if( 'B2' %in% value.vars ) {
    
    if( group.vars == 'state' ) {
      srhs.disc.dat <- read_excel( path  = paste0( params$s.dir,"/",params$srhs.cat.file ),
                                   sheet = 'disc_state_n',
                                   trim_ws = FALSE, col_types = "text" )
      
    } else if( group.vars == 'region' ) {
      srhs.disc.dat <- read_excel( path  = paste0( params$s.dir,"/",params$srhs.cat.file ),
                                   sheet = 'disc_region_n',
                                   trim_ws = FALSE, col_types = "text" )
    }
    
    srhs.disc.dat = srhs.disc.dat %>%
      rename_with( toupper ) %>%
      
      mutate_at( vars( YEAR), list( ~as.integer(.) ) ) %>%
      mutate_at( vars(!YEAR), list( ~as.numeric(.) ) ) %>%
      
      select( -any_of( 'TOTAL' ) ) %>%
      pivot_longer( !YEAR, names_to = "AREA", values_to = "B2" )
    
    if( any( grepl( '_', srhs.disc.dat$AREA ) ) ) {
      srhs.disc.dat = srhs.disc.dat %>%
        mutate( FIELD_TO_DROP = gsub( '.*_','', AREA ) ) %>%
        mutate( AREA = gsub( '_.*','', AREA ) ) %>%
        
        group_by( YEAR, AREA ) %>%
        summarize( B2 = sum( B2, na.rm=TRUE ) ) %>%
        ungroup()
    }
    
    srhs.disc.dat = srhs.disc.dat %>%
      # mutate( DS = 'SRHS' ) %>%
      mutate( NEW_MODEN = 'HBT' )
    
    
    if( group.vars == "state" & params$srhs.proxy.level != "state" ) {
      
      srhs.disc.dat = srhs.disc.dat %>% rename( NEW_STA = AREA ) %>% mutate( AREA = NA )
      for( i in 1:length( params$srhs.proxy.translations.state ) ) {
        srhs.disc.dat$AREA[ srhs.disc.dat$NEW_STA == params$srhs.proxy.translations.state[i] ] =
          toupper( params$srhs.proxy.translations.sid[i] )
      }
      srhs.disc.dat = srhs.disc.dat %>%
        group_by( across( any_of( c('NEW_MODEN','YEAR','AREA') ) ) ) %>%
        summarize( B2 = sum( B2, na.rm=TRUE ) )
      rm( i )
    }
    
    
    ### ------------------
    ### PROXY DISCARDS ###
    ### ------------------
    
    if( params$srhs.proxy.file != 'None' ) {
      
      srhs.prox.dat <- read_excel( path  = paste0( params$s.dir,"/",params$srhs.proxy.file ),
                                   sheet = params$srhs.proxy.tab,
                                   trim_ws = FALSE, col_types = "text" )
      
      srhs.prox.dat = srhs.prox.dat %>%
        mutate_at( vars( YEAR), list( ~as.integer(.) ) ) %>%
        mutate_at( vars( any_of( c('B2','CV_B2') ) ), list( ~as.numeric(.) ) )
      
      srhs.prox.dat = srhs.prox.dat %>%
        group_by( across( any_of( c('YEAR', 'AREA','SID','NEW_STA','STATE') ) ) ) %>%
        summarize( B2 = sum( B2, na.rm=TRUE ) ) %>%
        
        # mutate( DS = 'SRHS_proxy' ) %>%
        mutate( NEW_MODEN = 'HBT' )
      
      
      ### Renaming the "spatial" field to 'AREA', to match that in 'srhs.disc.dat'...
      area.field = colnames(srhs.prox.dat)[ colnames(srhs.prox.dat) %notin% c('YEAR','NEW_MODEN','B2') ]
      srhs.prox.dat = srhs.prox.dat %>% rename( AREA = area.field )
      rm( area.field )
      ###     ...and making sure this 'AREA' field is composed on only upper-case values...
      srhs.prox.dat = srhs.prox.dat %>% mutate( AREA = toupper(AREA) )
      
      
      srhs.disc.dat = bind_rows( srhs.disc.dat, srhs.prox.dat )
      rm( srhs.prox.dat )
      
      srhs.disc.dat = srhs.disc.dat %>% arrange( YEAR, AREA, NEW_MODEN )
      
    }
    
    # return.object[[length(return.object)+1]] = srhs.disc.dat
    # names(return.object)[length(return.object)] = 'srhs.disc.num'
    # rm( srhs.disc.dat )
    
  }
  
  
  ### ----------
  ### POUNDS ###
  ### ----------
  
  if( 'LBSEST_SECWWT' %in% value.vars ) {
    
    if( group.vars == 'state' ) {
      srhs.pounds.dat <- read_excel( path  = paste0( params$s.dir,"/",params$srhs.cat.file ),
                                     sheet = 'land_state_w_lbs',
                                     trim_ws = FALSE, col_types = "text" )
      
    } else if( group.vars == 'region' ) {
      srhs.pounds.dat <- read_excel( path  = paste0( params$s.dir,"/",params$srhs.cat.file ),
                                     sheet = 'land_region_w_lbs',
                                     trim_ws = FALSE, col_types = "text" )
    }
    
    srhs.pounds.dat = srhs.pounds.dat %>%
      rename_with( toupper ) %>%
      
      mutate_at( vars( YEAR), list( ~as.integer(.) ) ) %>%
      mutate_at( vars(!YEAR), list( ~as.numeric(.) ) ) %>%
      
      select( -any_of( 'TOTAL' ) ) %>%
      pivot_longer( !YEAR, names_to = "AREA", values_to = "LBSEST_SECWWT" )
    
    if( any( grepl( '_', srhs.pounds.dat$AREA ) ) ) {
      srhs.pounds.dat = srhs.pounds.dat %>%
        mutate( FIELD_TO_DROP = gsub( '.*_','', AREA ) ) %>%
        mutate( AREA = gsub( '_.*','', AREA ) ) %>%
        
        group_by( YEAR, AREA ) %>%
        summarize( LBSEST_SECWWT = sum( LBSEST_SECWWT, na.rm=TRUE ) ) %>%
        ungroup()
    }
    
    srhs.pounds.dat = srhs.pounds.dat %>%
      # mutate( DS = 'SRHS' ) %>%
      mutate( NEW_MODEN = 'HBT' )
    
    
    ### IMPUTATION OF HISTORICAL (1981-85) TEXAS ESTIMATES ###
    if( params$srhs.TX.imp ) {
      
      srhs.TX.land <- read_excel( path  = paste0( params$s.dir,"/",params$srhs.cat.file ),
                                  sheet = 'land_state_w_lbs',
                                  trim_ws = FALSE, col_types = "text" )
      
      srhs.TX.land = srhs.TX.land %>%
        rename_with( toupper ) %>%
        mutate_at( vars( YEAR), list( ~as.integer(.) ) ) %>%
        mutate_at( vars(!YEAR), list( ~as.numeric(.) ) ) %>%
        select( YEAR, TX )
      
      avg.years = as.numeric( params$srhs.TX.imp.years )
      # avg.years = as.numeric( gsub(":.*","",avg.years) ):as.numeric( gsub(".*:","",avg.years) )
      
      imp.TX.catch = mean( srhs.TX.land$TX[ srhs.TX.land$YEAR %in% avg.years ] )
      imp.TX.table = data.frame( YEAR = 1981:1985,
                                 AREA = params$srhs.TX.imp.area,
                                 LBSEST_SECWWT  = imp.TX.catch )
      imp.TX.table = imp.TX.table %>%
        # mutate( DS = 'SRHS_sub' ) %>%
        mutate( NEW_MODEN = 'HBT' )
      
      srhs.pounds.dat = bind_rows( imp.TX.table, srhs.pounds.dat )
      rm( srhs.TX.land, imp.TX.table, imp.TX.catch, avg.years )
    }
    
    
    if( group.vars == "state" & params$srhs.proxy.level != "state" ) {
      
      srhs.pounds.dat = srhs.pounds.dat %>% rename( NEW_STA = AREA ) %>% mutate( AREA = NA )
      for( i in 1:length( params$srhs.proxy.translations.state ) ) {
        srhs.pounds.dat$AREA[ srhs.pounds.dat$NEW_STA == params$srhs.proxy.translations.state[i] ] =
                                                toupper( params$srhs.proxy.translations.sid[i] )
      }
      srhs.pounds.dat = srhs.pounds.dat %>%
        group_by( across( any_of( c('NEW_MODEN','YEAR','AREA') ) ) ) %>%
        summarize( LBSEST_SECWWT = sum( LBSEST_SECWWT, na.rm=TRUE ) )
      rm( i )
    }
    
    
    # return.object[[length(return.object)+1]] = srhs.pounds.dat
    # names(return.object)[length(return.object)] = 'srhs.land.lbs'
    # rm( srhs.pounds.dat )
    
  }
  
  
  ### ----------------
  ### ANGLER TRIPS ###
  ### ----------------
  
  if( 'ESTRIPS' %in% value.vars ) {
    
    if( group.vars == 'state' ) {
      srhs.trip.dat <- read_excel( path  = paste0( params$s.dir,"/",params$srhs.eff.file ),
                                   sheet = 'angler_trips_by_state',
                                   trim_ws = FALSE, col_types = "text" )
      
    } else if( group.vars == 'region' ) {
      srhs.trip.dat <- read_excel( path  = paste0( params$s.dir,"/",params$srhs.eff.file ),
                                   sheet = 'angler_trips_by_region',
                                   trim_ws = FALSE, col_types = "text" )
    }
    
    srhs.trip.dat = srhs.trip.dat %>%
      rename_with( toupper ) %>%
      
      mutate_at( vars( YEAR), list( ~as.integer(.) ) ) %>%
      mutate_at( vars(!YEAR), list( ~as.numeric(.) ) ) %>%
      
      select( -any_of( 'TOTAL' ) ) %>%
      pivot_longer( !YEAR, names_to = "AREA", values_to = "ESTRIPS" )
    
    ###   ...where the columns in the original SRHS tables ( now saved as 'AREA' ) tend to be
    ###     spatial summaries of either state or by region/SID. However, this 'AREA' field can
    ###     sometimes include info on an additional strata, in which case we need to break apart
    ###     the current 'AREA' field to separate this info. In these cases, we apply an additional
    ###     group_by() & summarize() statement to collapse this field...
    
    if( any( grepl( '_', srhs.trip.dat$AREA ) ) ) {
      srhs.trip.dat = srhs.trip.dat %>%
        mutate( FIELD_TO_DROP = gsub( '.*_','', AREA ) ) %>%
        mutate( AREA = gsub( '_.*','', AREA ) ) %>%
        
        group_by( YEAR, AREA ) %>%
        summarize( ESTRIPS = sum( ESTRIPS, na.rm=TRUE ) ) %>%
        ungroup()
      
      # ###     ...where the script below was written to retain the 'FIELD_TO_DROP' variable,
      # ###       the name of which was identified by an additional 'strata.add' argument
      # ###       provided in the initial list of functionarguments...
      # command.line = paste0( "srhs.trip.dat = srhs.trip.dat %>% ",
      #                        " mutate( ",strata.add," = gsub( '.*_','', AREA) ) %>% ",
      #                        " mutate( ",strata.add," = toupper(",strata.add,") ) %>% ",
      #                        " mutate( AREA = gsub( '_.*','', AREA) )" )
      # eval( parse( text = command.line ) )
      # rm( command.line )
    }
    
    srhs.trip.dat = srhs.trip.dat %>%
      # mutate( DS = 'SRHS' ) %>%
      mutate( NEW_MODEN = 'HBT' )
    
    
    ### IMPUTATION OF HISTORICAL (1981-85) TEXAS ESTIMATES ###
    if( params$srhs.TX.imp ) {
      
      srhs.TX.eff <- read_excel( path  = paste0( params$s.dir,"/",params$srhs.eff.file ),
                                  sheet = 'angler_trips_by_state',
                                  trim_ws = FALSE, col_types = "text" )
      
      srhs.TX.eff = srhs.TX.eff %>%
        rename_with( toupper ) %>%
        mutate_at( vars( YEAR), list( ~as.integer(.) ) ) %>%
        mutate_at( vars(!YEAR), list( ~as.numeric(.) ) ) %>%
        select( YEAR, TX )
      
      avg.years = as.numeric( params$srhs.TX.imp.years )
      # avg.years = as.numeric( gsub(":.*","",avg.years) ):as.numeric( gsub(".*:","",avg.years) )
      
      imp.TX.effort = mean( srhs.TX.eff$TX[ srhs.TX.eff$YEAR %in% avg.years ] )
      imp.TX.table  = data.frame( YEAR = 1981:1985,
                                  AREA = params$srhs.TX.imp.area,
                                  ESTRIPS = imp.TX.effort )
      
      imp.TX.table = imp.TX.table %>%
        # mutate( DS = 'SRHS_sub' ) %>%
        mutate( NEW_MODEN = 'HBT' )
      
      srhs.trip.dat = bind_rows( imp.TX.table, srhs.trip.dat )
      rm( srhs.TX.eff, imp.TX.table, imp.TX.effort, avg.years )
    }
    
  }
  
  
  ### ---------------
  ### ANGLER DAYS ###
  ### ---------------
  
  if( 'ESTDAYS' %in% value.vars ) {
    
    if( group.vars == 'state' ) {
      srhs.days.dat <- read_excel( path  = paste0( params$s.dir,"/",params$srhs.eff.file ),
                                   sheet = 'angler_days_by_state',
                                   trim_ws = FALSE, col_types = "text" )
      
    } else if( group.vars == 'region' ) {
      srhs.days.dat <- read_excel( path  = paste0( params$s.dir,"/",params$srhs.eff.file ),
                                   sheet = 'angler_days_by_region',
                                   trim_ws = FALSE, col_types = "text" )
    }
    
    srhs.days.dat = srhs.days.dat %>%
      rename_with( toupper ) %>%
      
      mutate_at( vars( YEAR), list( ~as.integer(.) ) ) %>%
      mutate_at( vars(!YEAR), list( ~as.numeric(.) ) ) %>%
      
      select( -any_of( 'TOTAL' ) ) %>%
      pivot_longer( !YEAR, names_to = "AREA", values_to = "ESTDAYS" )
    
    
    if( any( grepl( '_', srhs.days.dat$AREA ) ) ) {
      srhs.days.dat = srhs.days.dat %>%
        mutate( FIELD_TO_DROP = gsub( '.*_','', AREA ) ) %>%
        mutate( AREA = gsub( '_.*','', AREA ) ) %>%
        
        group_by( YEAR, AREA ) %>%
        summarize( ESTDAYS = sum( ESTDAYS, na.rm=TRUE ) ) %>%
        ungroup()
      
      # ###     ...where the script below was written to retain the 'FIELD_TO_DROP' variable,
      # ###       the name of which was identified by an additional 'strata.add' argument
      # ###       provided in the initial list of functionarguments...
      # command.line = paste0( "srhs.days.dat = srhs.days.dat %>% ",
      #                        " mutate( ",strata.add," = gsub( '.*_','', AREA) ) %>% ",
      #                        " mutate( ",strata.add," = toupper(",strata.add,") ) %>% ",
      #                        " mutate( AREA = gsub( '_.*','', AREA) )" )
      # eval( parse( text = command.line ) )
      # rm( command.line )
    }
    
    srhs.days.dat = srhs.days.dat %>%
      # mutate( DS = 'SRHS' ) %>%
      mutate( NEW_MODEN = 'HBT' )
    
    
    ### IMPUTATION OF HISTORICAL (1981-85) TEXAS ESTIMATES ###
    if( params$srhs.TX.imp ) {
      
      srhs.TX.eff <- read_excel( path  = paste0( params$s.dir,"/",params$srhs.eff.file ),
                                 sheet = 'angler_days_by_state',
                                 trim_ws = FALSE, col_types = "text" )
      
      srhs.TX.eff = srhs.TX.eff %>%
        rename_with( toupper ) %>%
        mutate_at( vars( YEAR), list( ~as.integer(.) ) ) %>%
        mutate_at( vars(!YEAR), list( ~as.numeric(.) ) ) %>%
        select( YEAR, TX )
      
      avg.years = as.numeric( params$srhs.TX.imp.years )
      # avg.years = as.numeric( gsub(":.*","",avg.years) ):as.numeric( gsub(".*:","",avg.years) )
      
      imp.TX.effort = mean( srhs.TX.eff$TX[ srhs.TX.eff$YEAR %in% avg.years ] )
      imp.TX.table  = data.frame( YEAR = 1981:1985,
                                  AREA = params$srhs.TX.imp.area,
                                  ESTDAYS = imp.TX.effort )
      
      imp.TX.table = imp.TX.table %>%
        # mutate( DS = 'SRHS_sub' ) %>%
        mutate( NEW_MODEN = 'HBT' )
      
      srhs.days.dat = bind_rows( imp.TX.table, srhs.days.dat )
      rm( srhs.TX.eff, imp.TX.table, imp.TX.effort, avg.years )
    }
    
  }
  
  
  
  if( exists('srhs.land.dat') ) {
    return.object = srhs.land.dat
  }
  if( exists('srhs.disc.dat') ) {
    if( exists('return.object') ) {
      return.object = full_join( return.object, srhs.disc.dat, by=c('YEAR','AREA','NEW_MODEN') )
    } else {
      return.object = srhs.disc.dat
    }
  }
  if( exists('srhs.pounds.dat') ) {
    if( exists('return.object') ) {
      return.object = full_join( return.object, srhs.pounds.dat, by=c('YEAR','AREA','NEW_MODEN') )
    } else {
      return.object = srhs.pounds.dat
    }
  }
  if( exists('srhs.trip.dat') ) {
    if( exists('return.object') ) {
      return.object = full_join( return.object, srhs.trip.dat, by=c('YEAR','AREA','NEW_MODEN') )
    } else {
      return.object = srhs.trip.dat
    }
  }
  if( exists('srhs.days.dat') ) {
    if( exists('return.object') ) {
      return.object = full_join( return.object, srhs.days.dat, by=c('YEAR','AREA','NEW_MODEN') )
    } else {
      return.object = srhs.days.dat
    }
  }
  
  
  return( return.object )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------

compile.srhs.conf = function ( params ) {
  ###     ...where 'params' is a list of inputs defined in the RMarkdown YAML, including the location
  ###                       (i.e., file name & associated tab ) of the relevant SRHS data/estimates...
  
  
  raw.conf <- read_excel( path  = paste0( params$s.dir,"/",params$srhs.cat.file ),
                          sheet = params$srhs.conf.tab,
                          trim_ws = FALSE, col_types = "text" )
  
  # if( params$srhs.proxy.file != 'None' ) {
  #   raw.conf <- read_excel( path  = paste0( params$s.dir,"/",params$srhs.proxy.file ),
  #                           sheet = 'CONF_nVESSELS',
  #                           trim_ws = FALSE, col_types = "text" )
  # }
  
  raw.conf = raw.conf %>%
    rename_with( toupper ) %>%
    
    mutate_at( vars( YEAR), list( ~as.integer(.) ) ) %>%
    mutate_at( vars(!YEAR), list( ~as.numeric(.) ) ) %>%
    
    pivot_longer( !YEAR, names_to = "SID", values_to = "N_VESSEL" )
  
  if( length( unique(raw.conf$SID) ) == 1 ) {
    ###   ...which corresponds to an assessment where flextables are not being provided spatially,
    ###     and where the current (single) value in "SID" is likely to have defaulted to 'N_VESSEL'...
    raw.conf = raw.conf %>% select( -SID )
  }
  
  
  ### IMPUTATION OF HISTORICAL (1981-85) TEXAS ESTIMATES ###
  ###     ...which may create confidentiality concerns if *ONLY ONE* of the years over which the average(landings)
  ###       were calculated (e.g., as identified by 'imp.TX.years' ) were also confidential, in that you
  ###       could back-calculate the confidential SRHS landings from the confidential year if you were provided
  ###       with the average(landings) and the landings from all other years. Note that confidentiality isn't a concern
  ###       ( for average(landings) ) if two/more of the years were confidential, as the confidential landings
  ###       in either of these years would still be "hidden"...
  
  if( params$srhs.TX.imp ) {
    
    dummy <- read_excel( path  = paste0( params$s.dir,"/",params$srhs.cat.file ),
                         sheet = 'conf_state_land',
                         trim_ws = FALSE, col_types = "text" )
    dummy = dummy %>%
      rename_with( toupper ) %>%
      mutate_at( vars( YEAR), list( ~as.integer(.) ) ) %>%
      mutate_at( vars(!YEAR), list( ~as.numeric(.) ) ) %>%
      
      filter( YEAR %in% params$srhs.TX.imp.years ) %>%
      select( c('YEAR','TX' ) )
    
    if( length( which( dummy[,'TX'] < 2 ) ) == 1 ) {
      ###   ...for which we need to "hide" (some of) the estimates for the imputed years (of Texas data)...
      blah = data.frame( YEAR = 1981:1985, SID = params$srhs.TX.imp.area, N_VESSEL = 1 )
      
    } else {
      blah = data.frame( YEAR = 1981:1985,
                         SID = params$srhs.TX.imp.area,
                         N_VESSEL = sum( dummy[,'TX'], na.rm=TRUE ) )
    }
    
    if( "SID" %in% colnames(raw.conf) ) {
      raw.conf = bind_rows( blah, raw.conf )
      
    } else {
      blah = blah %>% select( -SID )
      
      raw.conf = bind_rows( blah, raw.conf )
      raw.conf = raw.conf %>%
        group_by( across( any_of( c('YEAR','SID') ) ) ) %>%
        summarize( N_VESSEL = sum( N_VESSEL, na.rm=TRUE ) )
    }
    rm( dummy, blah )
    
  }
  
  
  return( raw.conf )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------

join.srhs.conf = function( cat.table, conf.table, table.type = c('catch','effort') ) {
  ###     ...where 'cat.table' is the table of (AllRec) estimates...
  ###          and 'conf.table' the table of (SRHS) sample sizes, used to 'flag' confidential strata...
  
  
  if( 'SID' %in% colnames(cat.table) ) {
    sid.order = levels( cat.table$SID )
    
    dummy.table = cat.table %>% left_join( conf.table, by=c('YEAR','SID','FLEET') )
    dummy.table = dummy.table %>% mutate( SID = factor( SID, levels = sid.order ) )
    rm( sid.order )
    
  } else {
    dummy.table = cat.table %>% left_join( conf.table, by=c('YEAR','FLEET') )
  }
  
  
  ### HIDE CONFIDENTIAL STRATA ###
  ### ----------------------------
  
  if( table.type == 'catch' ) {
    
    if( 'FLEET' %in% colnames(dummy.table) ) {
      dummy.table = dummy.table %>%
        mutate( CAT = ifelse( FLEET %in% c('HBT','HB','FH','ALL') & ( !is.na(N_VESSEL) & N_VESSEL < 3 ), NA, CAT ),
                CV  = ifelse( FLEET %in% c('HBT','HB','FH','ALL') & ( !is.na(N_VESSEL) & N_VESSEL < 3 ), NA,  CV ) )
      ###   ...which 'hides' estimates for any strata corresponding to headboat fishing within which
      ###         SRHS catches are considered confidential...
      
    } else {
      dummy.table = dummy.table %>%
        mutate( AB1 = ifelse( NEW_MODEN == 'HBT' & ( !is.na(N_VESSEL) & N_VESSEL < 3 ), NA, AB1 ),
                B2  = ifelse( NEW_MODEN == 'HBT' & ( !is.na(N_VESSEL) & N_VESSEL < 3 ), NA,  B2 ) )
      ###   ...which 'hides' estimates for any strata corresponding to headboat fishing within which
      ###         SRHS catches are considered confidential...
    }
    
  } else if( table.type == 'effort' ) {
    
    if( 'FLEET' %in% colnames(dummy.table) ) {
      dummy.table = dummy.table %>%
        mutate( EFF = ifelse( FLEET %in% c('HBT','HB','FH','ALL') & ( !is.na(N_VESSEL) & N_VESSEL < 3 ), NA, EFF ) )
    } else {
      dummy.table = dummy.table %>%
        mutate( ESTRIPS = ifelse( NEW_MODEN == 'HBT' & ( !is.na(N_VESSEL) & N_VESSEL < 3 ), NA, ESTRIPS ) )
    }
  }
  
  
  return( dummy.table )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------

compile.srfs.dat = function () {
  
  
  
  # srfs.dat <- read.csv( paste0( dir,"/",srfs.catch.file ) )
  # 
  # srfs.dat = srfs.dat %>%
  #   mutate_at( vars(YEAR, SUB_REG,ST,NEW_MODE), list( ~ as.integer(.) ) ) %>%
  #   mutate_at( vars(ESTIMATE,SD,PSE), list( ~ as.numeric(.) ) ) %>%
  #   
  #   mutate( NEW_MODEN = toupper(NEW_MODEN) )
  # 
  # 
  # if( id.sid ) {
  #   
  #   potential.SID.fields = c( SID = 'Gulf', SID = 'ATL' )
  #   srfs.dat = srfs.dat %>%
  #     rename( any_of( potential.SID.fields) ) %>%
  #     mutate( SID = toupper(SID) )
  #   rm( potential.SID.fields )
  #   
  # }
  # 
  # # dummy.plot <- ggplot( data = srfs.dat, aes( x=YEAR ) ) +
  # #   geom_line( aes( y=ESTIMATE ), linewidth=1 ) +
  # #   facet_grid( VARNAME2 ~ . , scales='free' )
  # # rm(dummy.plot)
  # 
  # 
  # srfs.dat = srfs.dat %>%
  #   mutate( VAR = SD*SD ) %>%
  #   select( -c(SD,PSE) ) %>%
  #   
  #   select( !any_of(c('NEW_COM','SUB_REG','ST','NEW_MODE','DS')) ) %>%
  #   
  #   mutate( CAT_VAR = ifelse( VARNAME2 %in% c('AB1'), 'AB1',
  #                             ifelse( VARNAME2 %in% c( 'B2'),  'B2',
  #                                     ifelse( VARNAME2 %in% c('lbsest_wwt'), 'LBS', NA ))) ) %>%
  #   filter( !is.na(CAT_VAR) ) %>%
  #   select( -c(VARNAME,VARNAME2) ) %>%
  #   
  #   pivot_longer( any_of(c('ESTIMATE','VAR')), names_to = "METRIC", values_to = "value" ) %>%
  #   
  #   mutate( CAT_VAR = ifelse( METRIC == 'VAR', paste0('VAR_',CAT_VAR), CAT_VAR ) ) %>%
  #   select( -METRIC ) %>%
  #   
  #   pivot_wider( names_from = CAT_VAR, values_from = value )
  # 
  # 
  # srfs.dat = srfs.dat %>% mutate( DS = 'SRFS' )
  
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------

FT.allrec.catnum.yr.strata = function( catch.table,
                                       inc.years = NA,
                                       strata.vec = c('YEAR','FLEET'),
                                       catch.vars = c('AB1','B2','LBS') ) {
  ###     ...where 'catch.table' is the table of (allrec) catch estimates provided for this assessment,
  ###              'inc.years' identifies the years over which catch estimates need to be summarized,
  ###              'strata.vec' identifies the strata at which summaries are needed (e.g., fleet, SID ),
  ###              'catch.vars' identifies the catch variable to be summarized (e.g., AB1, B2, LBS )...
  
  
  return.object = list()
  
  
  dummy.table <- catch.table %>%
    filter( CAT_VAR %in% catch.vars ) %>%
    mutate( VAR = ( CV*CAT )^2 ) %>%
    
    group_by( across( any_of( c( strata.vec, 'CAT_VAR' ) ) ) ) %>%
    summarize( CAT = sum( CAT, na.rm=TRUE ),
               VAR = sum( VAR, na.rm=TRUE ) ) %>%
    mutate( CV = sqrt(VAR) / CAT ) %>%
    select( any_of( c( strata.vec, 'CAT_VAR','CAT','CV') ) ) %>%
    
    # mutate( FLEET = str_to_title(FLEET) ) %>%
    # pivot_wider( names_from = c(FLEET,CAT_VAR), values_from = c(CAT,CV),
    #              names_glue = sprintf('{FLEET}_{.value}_{CAT_VAR}' ) )
    
    pivot_wider( names_from = CAT_VAR, values_from = c(CAT,CV),
                 names_glue = sprintf('{.value}_{CAT_VAR}' ) )
  
  
  ###   Adding any 'missing' records/strata to 'dummy.table', for which I first need to identify
  ###   any additional strata ( beyond YEAR ) included in our 'allrec.dat' table...
  
  strata.add = strata.vec[ strata.vec %notin% c('YEAR') ]
  
  if( length(strata.add) == 0 ) {
    add.years <- data.frame( YEAR = inc.years )
    dummy.table = dummy.table %>%
      bind_rows( add.years %>% filter( YEAR %notin% dummy.table$YEAR ) ) %>%
      arrange( YEAR )
    rm( add.years )
    
  } else if( length(strata.add) > 0 ) {
    
    ### Identifying unique combinations of YEAR and 'strata.add'...
    eval( parse( text = paste0( "blah = expand.grid( YEAR = inc.years, ",
                                paste0( strata.add," = unique(unlist(catch.table['",strata.add,"']))", collapse=", " ),
                                ")" ) ) )
    
    ### Identifying those combinations in 'blah' that are currently *NOT* in 'dummy.table'...
    eval( parse( text = paste0( "blah = blah %>% mutate( IDENTIFIER = paste0( YEAR, ",
                                paste0( strata.add, collapse = ', ' )," ) )" ) ) )
    eval( parse( text = paste0( "dummy.table = dummy.table %>% mutate( IDENTIFIER = paste0( YEAR, ",
                                paste0( strata.add, collapse = ', ' )," ) )" ) ) )
    blah = blah[ blah$IDENTIFIER %notin% dummy.table$IDENTIFIER , ]
    
    blah = blah %>% select( -IDENTIFIER )
    dummy.table = dummy.table %>% select( -IDENTIFIER )
    
    ###   ...and adding any 'missing' combinations to 'dummy.table'...
    dummy.table = dummy.table %>% bind_rows( blah ) %>% arrange( across( any_of( c(strata.add,'YEAR') ) ) )
    rm( blah )
  }
  
  dummy.table[ is.na(dummy.table) ] <- 0
  
  
  ### ****************************************************************************
  ###   ...saving a copy of the current (long-format) table for AllRec figures...
  
  return.object$fig.dat = dummy.table
  
  ### ****************************************************************************
  
  
  ### PIVOT by Fleet/Mode ###
  ### -----------------------
  ###   ...which first requires identifying those 'catch columns' currently in 'dummy.table' and (unique)
  ###     'FLEET' values, both of which are needed (below) to sort columns in my final flextable...
  cat.cols = apply( expand.grid( c('CAT','CV'), catch.vars ), 1, paste, collapse='_' )
  fleet.vals = unique( str_to_title(dummy.table$FLEET) )
  
  dummy.table = dummy.table %>%
    mutate( FLEET = str_to_title(FLEET) ) %>%
    pivot_wider( names_from = FLEET, values_from = any_of( cat.cols ),
                 names_glue = sprintf('{FLEET}_{.value}' ) )
  rm( cat.cols )
  
  ###   ...the result of which is a 'dummy.table' resembling something like the table below,
  ###     assuming 'strata.vec' = ('SID','YEAR','FLEET') and 'catch.vars' = ('AB1','LBS')...
  ###
  ###   SID     YEAR    'FLEET1'_CAT_AB1  'FLEET1'_CAT_LBS  ...FLEET2+...   'FLEET1'_CV_AB1  'FLEET1'_CV_LBS  ...
  ###   EAST    1981    landings-number   landings-weight   ...       ...   landnum-CV       landwgt-CV       ...
  ###   EAST    1982    landings-number   landings-weight   ...       ...   landnum-CV       landwgt-CV       ...
  ###   ...     ...     landings-number   landings-weight   ...       ...   landnum-CV       landwgt-CV       ...
  ###   WEST    1981    landings-number   landings-weight   ...       ...   landnum-CV       landwgt-CV       ...
  ###   WEST    1982    landings-number   landings-weight   ...       ...   landnum-CV       landwgt-CV       ...
  ###   ...     ...     landings-number   landings-weight   ...       ...   landnum-CV       landwgt-CV       ...
  
  
  ###   ...and as a final check for any strata (i.e., states/modes ) providing no catch information...
  col.vec = strata.vec[ strata.vec %notin% c('FLEET') ]
  
  retain.cols = names( which(
    colSums( dummy.table[ , !( colnames(dummy.table) %in% col.vec ) ], na.rm=TRUE ) != 0 ) )
  dummy.table = dummy.table[ , colnames(dummy.table) %in% c(col.vec,retain.cols) ]
  rm( retain.cols, col.vec )
  
  
  ### SORTING COLUMNS ###
  ### -------------------
  ###   ...before creating my flextable, I must define the ORDER in which I want the columns of 'dummy.table'
  ###   to be displayed and the corresponding HEADERS to be printed for each of these columns (in FTxx)...
  
  ###   ...for which I first define the desired order of my individual fishing fleets...
  fleet.vals = fleet.vals[ order( match( fleet.vals,
                                         c("ALL",
                                           "FH","ForHire",  "Cbt", "CbtHbt","Cbt/Hbt","Cbt_Hbt", "HB","Hbt",
                                           "GR","GenRec", "PS","Priv","PrivShore","Priv/Shore","Priv_Shore","Shore") ) ) ]
  ### Order of STRATA Fields ###
  col.order = paste0( strata.vec[ strata.vec %notin% c('FLEET') ] )
  
  upper.hdr <- list()
  lower.hdr  <- list()
  for( i in 1:length(col.order) ) {
    eval( parse( text = paste0( "upper.hdr$",col.order[i]," = ''" ) ) )
    eval( parse( text = paste0( "lower.hdr$",col.order[i]," = '",col.order[i],"'" ) ) )
  }
  
  ### Order of VARIABLE Fields ###
  for( i in 1:length(fleet.vals) ) {
    for( j in 1:length(catch.vars) ) {
      col.order = c( col.order, paste0( fleet.vals[i],"_CAT_",catch.vars[j] ) )
      eval( parse( text = paste0( "upper.hdr$",fleet.vals[i],"_CAT_",catch.vars[j]," = '",fleet.vals[i],"'" ) ) )
      eval( parse( text = paste0( "lower.hdr$",fleet.vals[i],"_CAT_",catch.vars[j]," = '",catch.vars[j],"'" ) ) )
      
      col.order = c( col.order, paste0( fleet.vals[i],"_CV_",catch.vars[j] ) )
      eval( parse( text = paste0( "upper.hdr$",fleet.vals[i],"_CV_",catch.vars[j]," = '",fleet.vals[i],"'" ) ) )
      eval( parse( text = paste0( "lower.hdr$",fleet.vals[i],"_CV_",catch.vars[j]," = 'CV'" ) ) )
    }
  }
  rm( fleet.vals )
  
  dummy.table = dummy.table[ ,col.order ]
  
  ### ****************************************************************************
  ###   ...saving the objects needed for AllRec flextables...
  
  return.object$FT.dat = dummy.table
  return.object$FT.order = col.order
  return.object$FT.upper = upper.hdr
  return.object$FT.lower = lower.hdr
  
  ### ****************************************************************************
  
  rm( dummy.table, col.order, upper.hdr, lower.hdr )
  
  
  
  return( return.object )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------

FT.allrec.efftrip.yr.strata = function( effort.table,
                                        inc.years = NA,
                                        strata.vec = c('YEAR','FLEET'),
                                        params ) {
  ###     ...where 'effort.table' is the table of (allrec) effort estimates provided for this assessment,
  ###              'inc.years' identifies the years over which effort estimates need to be summarized,
  ###              'strata.vec' identifies the strata at which summaries are needed (e.g., fleet, SID ),
  ###       ...and 'params' is a list of inputs defined in the RMarkdown YAML, including the location
  ###                       (i.e., file name & associated tab ) of raw GenRec estimates...
  
  
  return.object = list()
  
  
  dummy.table <- effort.table %>% rename( ESTRIPS = EFF )
  
  ###   Adding any 'missing' records/strata to 'dummy.table', for which I first need to identify
  ###   any additional strata ( beyond YEAR ) included in our 'allrec.dat' table...
  
  strata.add = strata.vec[ strata.vec %notin% c('YEAR') ]
  
  if( length(strata.add) == 0 ) {
    add.years <- data.frame( YEAR = inc.years )
    dummy.table = dummy.table %>%
      bind_rows( add.years %>% filter( YEAR %notin% dummy.table$YEAR ) ) %>%
      arrange( YEAR )
    rm( add.years )
    
  } else if( length(strata.add) > 0 ) {
    
    ### Identifying unique combinations of YEAR and 'strata.add'...
    eval( parse( text = paste0( "blah = expand.grid( YEAR = inc.years, ",
                                paste0( strata.add," = unique(unlist(effort.table['",strata.add,"']))", collapse=", " ),
                                ")" ) ) )
    
    ### Identifying those combinations in 'blah' that are currently *NOT* in 'dummy.table'...
    eval( parse( text = paste0( "blah = blah %>% mutate( IDENTIFIER = paste0( YEAR, ",
                                paste0( strata.add, collapse = ', ' )," ) )" ) ) )
    eval( parse( text = paste0( "dummy.table = dummy.table %>% mutate( IDENTIFIER = paste0( YEAR, ",
                                paste0( strata.add, collapse = ', ' )," ) )" ) ) )
    blah = blah[ blah$IDENTIFIER %notin% dummy.table$IDENTIFIER , ]
    
    blah = blah %>% select( -IDENTIFIER )
    dummy.table = dummy.table %>% select( -IDENTIFIER )
    
    ###   ...and adding any 'missing' combinations to 'dummy.table'...
    dummy.table = dummy.table %>% bind_rows( blah ) %>% arrange( across( any_of( c(strata.add,'YEAR') ) ) )
    rm( blah )
  }
  
  dummy.table[ is.na(dummy.table) ] <- 0
  
  
  ### ****************************************************************************
  ###   ...saving a copy of the current (long-format) table for AllRec figures...
  
  return.object$fig.dat = dummy.table
  
  ### ****************************************************************************
  
  
  ### PIVOT by Fleet/Mode ###
  ### -----------------------
  ###   ...which first requires identifying those (unique) 'FLEET' and 'SID' values currently in 'dummy.table'...
  
  fleet.vals = unique( str_to_title(dummy.table$FLEET) )
  
  if( 'SID' %in% colnames(dummy.table) ) {
    area.vals = as.character( unique(dummy.table$SID) )
    
  } else {
    # area.vals = params$region
    # dummy.table = dummy.table %>% mutate( SID = params$region )
    area.vals = gsub( " ","", params$region)
    dummy.table = dummy.table %>% mutate( SID = gsub( " ","", params$region) )
  }
  
  dummy.table = dummy.table %>%
    mutate( FLEET = str_to_title(FLEET) ) %>%
    pivot_wider( names_from = c(FLEET,SID),
                 values_from = ESTRIPS,
                 names_glue = sprintf('{FLEET}_{.value}_{SID}' ) )
  
  
  ###   ...the result of which is a 'dummy.table' resembling something like the table below,
  ###     assuming 'strata.vec' = ('SID','YEAR','FLEET') and 'effort.vars' = ('AB1','LBS')...
  ###
  ###   SID     YEAR    'FLEET1'_ESTRIPS_'SID1'  'FLEET2'_ESTRIPS_'SID1'  ...SID2+...
  ###   EAST    1981    effort-angtrips           effort-angtrips         ...     ...
  ###   EAST    1982    effort-angtrips           effort-angtrips         ...     ...
  ###   ...     ...     effort-angtrips           effort-angtrips         ...     ...
  ###   WEST    1981    effort-angtrips           effort-angtrips         ...     ...
  ###   WEST    1982    effort-angtrips           effort-angtrips         ...     ...
  ###   ...     ...     effort-angtrips           effort-angtrips         ...     ...
  
  
  ###   ...and as a final check for any strata (i.e., states/modes ) providing no catch information...
  col.vec = strata.vec[ strata.vec %notin% c('FLEET','SID') ]
  
  retain.cols = names( which(
    colSums( dummy.table[ , !( colnames(dummy.table) %in% col.vec ) ], na.rm=TRUE ) != 0 ) )
  dummy.table = dummy.table[ , colnames(dummy.table) %in% c(col.vec,retain.cols) ]
  rm( retain.cols, col.vec )
  
  
  
  ### SORTING COLUMNS ###
  ### -------------------
  ###   ...before creating my flextable, I must define the ORDER in which I want the columns of 'dummy.table'
  ###   to be displayed and the corresponding HEADERS to be printed for each of these columns (in FTxx)...
  
  ###   ...for which I first define the desired order of my individual fishing fleets...
  fleet.vals = fleet.vals[ order( match( fleet.vals,
                                         c("ALL",
                                           "FH","ForHire",  "Cbt", "CbtHbt","Cbt/Hbt","Cbt_Hbt", "HB","Hbt",
                                           "GR","GenRec", "PS","Priv","PrivShore","Priv/Shore","Priv_Shore","Shore") ) ) ]
  ### Order of STRATA Fields ###
  col.order = paste0( strata.vec[ strata.vec %notin% c('FLEET','SID') ] )
  
  upper.hdr <- list()
  lower.hdr  <- list()
  for( i in 1:length(col.order) ) {
    eval( parse( text = paste0( "upper.hdr$",col.order[i]," = ''" ) ) )
    eval( parse( text = paste0( "lower.hdr$",col.order[i]," = '",col.order[i],"'" ) ) )
  }
  
  ### Order of VARIABLE Fields ###
  
  for( j in 1:length(area.vals) ) {
    for( i in 1:length(fleet.vals) ) {
      col.order = c( col.order, paste0( fleet.vals[i],"_ESTRIPS_",area.vals[j] ) )
      eval( parse( text = paste0( "upper.hdr$",fleet.vals[i],"_ESTRIPS_",area.vals[j]," = '",area.vals[j],"'" ) ) )
      eval( parse( text = paste0( "lower.hdr$",fleet.vals[i],"_ESTRIPS_",area.vals[j]," = '",fleet.vals[i],"'" ) ) )
    }
  }
  rm( fleet.vals )
  
  dummy.table = dummy.table[ ,col.order ]
  
  ### ****************************************************************************
  ###   ...saving the objects needed for AllRec flextables...
  
  return.object$FT.dat = dummy.table
  return.object$FT.order = col.order
  return.object$FT.upper = upper.hdr
  return.object$FT.lower = lower.hdr
  
  ### ****************************************************************************
  
  rm( dummy.table, col.order, upper.hdr, lower.hdr )
  
  
  
  return( return.object )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------

FIG.allrec.strata = function( catch.table, catch.title ) {
  ###     ...where 'catch.table' is a table of catch estimates provided by year, mode, and area...
  ###              'catch.title' identifies the catch variable being summarized, and the text to add
  ###                       as titles for axes...
  
  
  return.object = list()
  
  
  n.years <- length( unique(catch.table$YEAR) )
  
  #################################
  ###       SPATIAL PLOTS       ###
  #################################
  
  ### (Absolute) Stacked Bar Plot ###
  ### -------------------------------
  ###       ...by year and area
  
  dummy.table = catch.table %>%
    group_by( YEAR, AREA ) %>%
    summarize( value = sum( value, na.rm=TRUE ) )
  
  Fig1a.state <- ggplot( data=dummy.table, aes( x=YEAR, y=value, fill=AREA ) ) +
    geom_col( position = "stack", colour="black" ) +     ### ...where colour="black" draws black boxes around each area...
    #  geom_col( position = "stack" ) +                    ### ...with no boxes...
    #  facet_grid( variable ~ Area ) +
    labs( title="", x="Year", y=catch.title ) +
    scale_x_continuous( breaks = scales::pretty_breaks( n = n.years/2 ) ) +
    scale_fill_brewer( palette="Blues") +
    #  ### ...more palettes at    http://www.sthda.com/english/wiki/ggplot2-colors-how-to-change-colors-automatically-and-manually
    theme_bw() +
    theme( text = element_text(size = 11), axis.text.x = element_blank(), axis.title.x = element_blank(),
           # legend.position = "bottom",
           legend.position = "none",
           panel.grid.major = element_line(colour = "grey", linewidth = 0.5),
           panel.grid.minor = element_line(colour = "grey", linewidth = 0.2),
           panel.border = element_rect(colour = "black", fill = NA),
           plot.margin = unit( c(0,1,0,0.5),"cm" ) )
  
  
  ### SAVE FILL PATTERN ###
  ###     Before moving onto the next plot, I extract (and save) the FILL colors that are applied in "Fig1a.state".
  ###     Specifically, I am going to be making a number of area-specific figures and I want to ensure each apply
  ###     the same fill to the same states...
  color.code <- list()
  
  g <- ggplot_build( Fig1a.state )
  
  color.code[["area"]] <- data.frame( GROUP = unique( g$data[[1]]$group ) )
  color.code$area$HEX <- unique( g$data[[1]]$fill[ which( g$data[[1]]$group == color.code$area$GROUP ) ] )
  rm( g )
  
  color.code$area$COLOR <- sapply( color.code$area$HEX, plotrix::color.id )
  ###     ...where HEX is the hex code for RGB colors and so I add a column for the actual COLOR name...
  
  ### Similarly, I also match the GROUP code with the NAME of the factor these codes represent (e.g., 1 = TX ).
  ###       (I think) ggplot assigns colors based on the order in which each factor is encountered in the data
  ###       and so I refer back to the raw data table ( "dummy.table" ) to pull out the factors. Note that I
  ###       also make sure that "color.code" is sorted based on the GROUP field...
  color.code$area <- color.code$area[ order(color.code$area$GROUP), ]
  color.code$area$NAME  <- unique( dummy.table$AREA )
  
  rm( dummy.table )
  
  
  ### (Relative) Pie Chart ###
  ### ------------------------
  ###       ...by area
  
  dummy.table = catch.table %>%
    group_by( AREA ) %>%
    summarize( value = sum( value, na.rm=TRUE ) )
  
  Fig1a.pie <- ggplot( data=dummy.table, aes( x="", y=value, fill=AREA ) ) +
    geom_bar( width=1, stat="identity" ) + coord_polar( "y", start=0 ) + theme_void() +
    ###   ...pie charts (in R) are stacked bar plots, but are constructed along a
    ###       polar coordinate system ( circular x-axis )...
    scale_fill_manual( values = setNames( color.code$area$COLOR, color.code$area$NAME ) ) +
    #  scale_fill_brewer( palette="Blues" ) +
    geom_label_repel( aes( label=paste0( round( value/sum(value)*100, 1 ),"%" ) ),
                      position=position_stack(vjust=0.5), show.legend = FALSE ) +
    theme( plot.margin = unit( c(-1,0,-1,-1),"cm" ) )
    ###   ...where I use the "plot.margins" argument to expand the size of the pie chart, but leave
    ###     the right margin at 0 to ensure I don't lose any of my legend (printed to the right)...
  
  rm( dummy.table )
  
  
  
  #################################
  ###       BY-MODE PLOTS       ###
  #################################
  
  ### (Absolute) Stacked Bar Plot ###
  ### -------------------------------
  ###       ...by year and mode
  
  dummy.table = catch.table %>%
    group_by( YEAR, MODE ) %>%
    summarize( value = sum( value, na.rm=TRUE ) )
  
  Fig1b.mode <- ggplot( data=dummy.table, aes( x=YEAR, y=value, fill=MODE ) )
  
  Fig1b.mode <- Fig1b.mode +
    geom_col( position = "stack", colour="black" ) +     ### ...where colour="black" draws black boxes around each area...
    #  geom_col( position = "stack" ) +                    ### ...with no boxes...
    #  facet_grid( variable ~ Area ) +
    labs( title="", x="Year", y=catch.title ) +
    scale_x_continuous( breaks = scales::pretty_breaks( n = n.years/2 ) ) +
    scale_fill_brewer( palette="Reds") +
    theme_bw() +
    theme( text = element_text(size = 11),
           axis.text.x = element_text(angle = 90, vjust=0.5),
           # legend.position = "bottom",
           legend.position = "none",
           panel.grid.major = element_line(colour = "grey", linewidth = 0.5),
           panel.grid.minor = element_line(colour = "grey", linewidth = 0.2),
           panel.border = element_rect(colour = "black", fill = NA),
           plot.margin = unit( c(-0.5,1,0,0.5),"cm" ) )
  
  
  ### SAVE FILL PATTERN ###
  g <- ggplot_build( Fig1b.mode )
  
  color.code[["mode"]] <- data.frame( GROUP = unique( g$data[[1]]$group ) )
  color.code$mode$HEX <- unique( g$data[[1]]$fill[ which( g$data[[1]]$group == color.code$mode$GROUP ) ] )
  rm( g )
  
  color.code$mode$COLOR <- sapply( color.code$mode$HEX, plotrix::color.id )
  color.code$mode <- color.code$mode[ order(color.code$mode$GROUP), ]
  color.code$mode$NAME  <- unique( dummy.table$MODE )
  
  rm( dummy.table )
  
  
  ### (Relative) Pie Chart ###
  ### ------------------------
  ###       ...by mode
  
  dummy.table = catch.table %>%
    group_by( MODE ) %>%
    summarize( value = sum( value, na.rm=TRUE ) )
  
  Fig1b.pie <- ggplot( data=dummy.table, aes( x="", y=value, fill=MODE ) ) +
    geom_bar( width=1, stat="identity" ) + coord_polar( "y", start=0 ) + theme_void() +
    ###   ...pie charts (in R) are stacked bar plots, but are constructed along a
    ###       polar coordinate system ( circular x-axis )...
    scale_fill_manual( values = setNames( color.code$mode$COLOR, color.code$mode$NAME ) ) +
    #  scale_fill_brewer( palette="Reds" ) +
    geom_label_repel( aes( label=paste0( round( value/sum(value)*100, 1 ),"%" ) ),
                      position=position_stack(vjust=0.5), show.legend = FALSE ) +
    theme( plot.margin = unit( c(-1,0,-1,-1),"cm" ) )
  
  rm( dummy.table )
  
  
  
  ##################################
  ###       JOIN PLOTS FOR       ###
  ###     BY-STATE & BY-MODE     ###
  ##################################
  
  Fig1.ab <- ggpubr::ggarrange( Fig1a.state, Fig1a.pie, Fig1b.mode, Fig1b.pie,
                                nrow=2, ncol=2, widths=c( 2.5,1 ), heights=c( 1, 1.1 ) )
  ###       ...where "Fig1b.mode" is given a bit more space (than "Fig1a.state") as it contains the x-axis...
  rm( Fig1a.state,Fig1b.mode, Fig1a.pie,Fig1b.pie )
  
  ### ---------------------------------------------------------------------------------------
  ###   ...and save the final (combined) outputs, along with the applied color-coding...
  return.object$fig.yr.area.mode = Fig1.ab
  return.object$color.codes = color.code
  ### ---------------------------------------------------------------------------------------
  
  
  
  #######################################
  ###     BY-MODE & BY-STATE PLOTS    ###
  #######################################
  
  
  ### (Relative) Stacked Bar Plot ###
  ### -------------------------------
  ###         ...by area and mode
  
  dummy.table = catch.table %>%
    group_by( AREA, MODE ) %>%
    summarize( value = sum( value, na.rm=TRUE ) )
  
  Fig1c.both <- ggplot( data=dummy.table, aes( x=AREA, y=value, fill=MODE) ) +
    geom_bar( position = "fill", stat="identity", colour="black" ) +
    scale_y_continuous( labels = scales::percent ) +
    labs( title="", x="", y=paste0("Percent ",gsub(" .*","",catch.title) ) ) +
    scale_fill_manual( values = setNames( color.code$mode$COLOR, color.code$mode$NAME ) ) +
    #  scale_fill_brewer( palette="Reds") +
    #  ### ...more palettes at    http://www.sthda.com/english/wiki/ggplot2-colors-how-to-change-colors-automatically-and-manually
    theme_bw() +
    theme( text = element_text(size = 11), axis.text.x = element_text(angle = 0),
           # legend.position = "left",
           legend.position = "none",
           panel.grid.major = element_line(colour = "grey", linewidth = 0.5),
           panel.grid.minor = element_line(colour = "grey", linewidth = 0.2),
           panel.border = element_rect(colour = "black", fill = NA),
           plot.margin = unit( c(0,1,0,0.5),"cm" ) )
  
  ###   ...where the legend is not included in this sub-plot given its defined above (i.e., in 'Fig1.ab' ). However,
  ###     I did consider adding a legend at one point, applying the get_legend() function to a 'Fig1c.both' plot
  ###     that includes a legend and then removing it:
  ###           # Fig1c.legend <- cowplot::get_legend( Fig1c.both )
  ###           # Fig1c.both <- Fig1c.both + theme( legend.position = "none" )
  ###     However, space started getting 'tight' for this component of the plot, namely to ensure there was enough
  ###     space for the table of catch estimates below (i.e., in my 'plot.list' object )...
  
  
  ### (Absolute) Landings Table ###
  ### -----------------------------
  ###       ...by area and mode
  
  dummy.table <- dummy.table %>%
    ungroup() %>%
    mutate( MODE = str_to_title(MODE) ) %>%
    mutate( value = value * 1000 ) %>%
    ###   ...to remove the (1000s Fish) scalar applied to the previous plots...
    
    pivot_wider( names_from=MODE, values_from=value ) %>%
    mutate_if( is.numeric, round, digits=0 ) %>%
    mutate_if( is.numeric, format, nsmall=0, big.mark="," )
  
  ###     ...and in case some modes were not sampled in a particular state...
  # dummy.table[ dummy.table == "NA" | is.na(dummy.table) ] = 0
  dummy.table = dummy.table %>%
    mutate_at( vars( -one_of( c("AREA") ) ), ~ifelse( is.na(.) | ( .=="NA" ), 0, . ) )
  
  ###     ...and trying to combine "Fig1b.bars" with "Fig1c.table" so they're properly aligned, I follow the approach at:
  ### #         https://stackoverflow.com/questions/40265494/ggplot-grobs-align-with-tablegrob
  
  
  tab_base <- ggplot( dat=dummy.table, aes( x=AREA ) )
  
  tab_base = tab_base +
    xlab(NULL) + ylab("  ") +
    theme(plot.title = element_text(hjust = 0.5, size=12), ## centering title on text
          axis.text.y=element_text(color="white"), ## need text to be aligned with figure but white so it's invisible
          axis.text.x=element_blank(),
          axis.title.x=element_blank(),legend.position="none",
          axis.line=element_blank(),
          axis.ticks=element_blank(),
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank())
  
  plot.list <- list()
  
  if( "Cbt" %in% colnames(dummy.table) ) {
    p.cbt <- tab_base + geom_text( aes(y=1,label=Cbt ) ) + ylab( "Cbt" ) +
      theme( plot.margin = margin(0,1,0,0,"cm"),
             axis.title.y = element_text(angle=0,vjust=0.5) ) }
  if( "Hbt" %in% colnames(dummy.table) ) {
    p.hbt <- tab_base + geom_text( aes(y=1,label=Hbt ) ) + ylab( "Hbt" ) +
      theme( plot.margin = margin(0,1,0,0,"cm"),
             axis.title.y = element_text(angle=0,vjust=0.5) ) }
  if( "Priv" %in% colnames(dummy.table) ) {
    p.priv <- tab_base + geom_text( aes(y=1,label=Priv ) ) + ylab( "Priv" ) +
      theme( plot.margin = margin(0,1,0,0,"cm"),
             axis.title.y = element_text(angle=0,vjust=0.5) ) }
  if( "Shore" %in% colnames(dummy.table) ) {
    p.shore <- tab_base + geom_text( aes(y=1,label=Shore ) ) + ylab( "Shore" ) +
      theme( plot.margin = margin(0,1,0,0,"cm"),
             axis.title.y = element_text(angle=0,vjust=0.5) ) }
  
  if( "Cbt" %in% colnames(dummy.table) ) { plot.list[[ length(plot.list)+1 ]] <- p.cbt }
  if( "Hbt" %in% colnames(dummy.table) ) { plot.list[[ length(plot.list)+1 ]] <- p.hbt }
  if( "Priv" %in% colnames(dummy.table) ) { plot.list[[ length(plot.list)+1 ]] <- p.priv }
  if( "Shore" %in% colnames(dummy.table) ) { plot.list[[ length(plot.list)+1 ]] <- p.shore }
  
  
  ###     ...and now combining the above catch estimates (by-mode) with my stacked relative bar plot ("Fig1c.both")...
  Fig1.c <- eval( parse( text = paste0( "ggpubr::ggarrange( Fig1c.both,",
                                        paste0( "plot.list[[",1:length(plot.list),"]]", collapse=", " ),", ",
                                        "heights=c( ",length(plot.list)*3,",",paste0(rep(1,length(plot.list)),collapse="," ),
                                        "), ncol=1 )" ) ) )
  rm( dummy.table, tab_base, Fig1c.both, p.cbt,p.hbt,p.priv,p.shore )
  
  
  ### ---------------------------------------------------------------------------------------
  ###   ...and save the final (combined) outputs...
  return.object$fig.area.mode = Fig1.c
  ### ---------------------------------------------------------------------------------------
  
  
  
  return( return.object )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------

FIG.allrec.byMode = function( catch.table, catch.title ) {
  ###     ...where 'catch.table' is a table of catch estimates provided by year, mode, and area...
  ###              'catch.title' identifies the catch variable being summarized, and the text to add
  ###                       as titles for axes...
  
  
  # return.object = list()
  
  
  ### (Absolute) Stacked Bar Plot ###
  ### -------------------------------
  ###       ...by year and area, separated (via facets) for each mode
  
  fill.colors = c('darkblue','springgreen4','deeppink','yellow',"lightsteelblue1",'gray50',"royalblue2")
  fill.colors = fill.colors[ 1:length(unique(catch.table$AREA)) ]
  
  n.years <- length( unique(catch.table$YEAR) )
  
  Fig3.mode <- ggplot( data=catch.table, aes( x=YEAR, y=value, fill=AREA ) ) +
    geom_col( position = "stack", colour="black" ) +     ### ...where colour="black" draws black boxes around each area...
    #  geom_col( position = "stack" ) +                    ### ...with no boxes...
     facet_grid( MODE ~ . , scales='free' ) +
    labs( title="", x="Year", y=catch.title ) +
    scale_x_continuous( breaks = scales::pretty_breaks( n = n.years/2 ) ) +
    scale_fill_manual( values = fill.colors ) +
    # scale_fill_brewer( palette="Blues") +
    #  ### ...more palettes at    http://www.sthda.com/english/wiki/ggplot2-colors-how-to-change-colors-automatically-and-manually
    theme_bw() +
    theme( text = element_text(size = 11),
           axis.text.x = element_text(angle = 90, vjust=0.5),
           legend.position = "bottom",
           # legend.position = "none",
           panel.grid.major = element_line(colour = "grey", linewidth = 0.5),
           panel.grid.minor = element_line(colour = "grey", linewidth = 0.2),
           panel.border = element_rect(colour = "black", fill = NA),
           plot.margin = unit( c(0,1,0,0.5),"cm" ) )
  rm( fill.colors, n.years )
  
  
  return.object = Fig3.mode
  rm( Fig3.mode )
  
  return( return.object )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------





