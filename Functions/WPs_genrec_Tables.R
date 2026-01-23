

### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------
###
###     DESCRIPTION
###
###     ...wherein this script contains functions needed to construct the various TABLES of the GenRec working paper...
###
###    ----------------
###       FLEXTABLES
###    ----------------
###    *** FT.construct( )
###         ...which is the generic script I use to (initially) construct my flextables with the desired formatting.
###           In particular, the text size of the headers & body, the width of margins (of cells), the number of
###           digits/decimals to be printed (i.e., rounding), whether to add a 'big.mark' to separate the 1000s place
###           (e.g., with a comma)...
###
###    ----------------
###          CATCH
###    ----------------
###    *** FT.catnum.yr.strata( )
###         ...which summarizes catch-in-number (AB1 & B2) in the 'raw.catch' table by year & strata (state/mode),
###           with the total/annual estimates provided as the last column...
###
###    *** FT.catnum.cvs( )
###         ...which summarizes uncertainties ( for AB1 & B2 ) by year and strata, the latter being at either a
###           specific strata (i.e., state or mode ) or at an annual resolution...
###
###    *** FT.catnum.unid( )
###         ...which summarizes the relative catch of identified groups (to species), as compared to that of
###           the species-of-interest, such that some percentage of unidentified catch may be allocated to that species...
###
###    *** FT.catlbs.yr.strata( )
###         ...which summarizes landings-in-weight (LBS) in the 'raw.catch' table by year & strata (state/mode),
###           with the total/annual estimates provided as the last column...
###
###    *** FT.catlbs.cvs( )
###         ...which summarizes uncertainties ( for LBS ) by year and strata (i.e., state or mode ). Note that,
###           unlike the CV tables for catnum, the LBS-CV tables combine the strata-specific estimates with the
###           total/annual estimates (i.e., all provided in one table )...
###
###    ----------------
###          SIZE
###    ----------------
###    *** FT.size.yr.strata( )
###         ...which summarizes raw size data (lengths or weights) in the 'raw.size' table by year & strata,
###           the latter being at either a specific strata (i.e., state or mode ) or at an annual resolution...
###
###    *** FT.size.cvs( )
###         ...which summarizes uncertainties ( for SEFSC avgwgt estimates ) by year and strata, the latter being
###           at whatever strata (e.g., state or mode ) these estimates were provided in 'cv.AvgWt.table'...
###
###    *** FT.size.hier( )
###         ...which summarizes the amount of catch ( landings-in-weight ) originating from SEFSC avgwgts
###           estimated at various levels of the avgwgt estimation hierarchy (i.e., s/sr/sry/srys/etc. )...
###
###    ----------------
###          EFFORT
###    ----------------
###    *** FT.effort.yr.strata( )
###         ...which summarizes recreational effort estimates by year & strata (state/mode),
###           with the total/annual estimates provided as the last column...
###
### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------

FT.construct = function( my.data,
                         table.type=c("LEN","WGT","L&W","CV","ALLREC","OTHER"),
                         col.order=colnames(my.data),
                         main.hdr=colnames(my.data), sub.hdr=NA,
                         strata.add = NA ) {
  
  ###       ...where "my.data" is the data table being converted into a flextable
  ###       ..."col.order" is the order with which the columns ( in "my.data" ) are to be arranged,
  ###               which defaults to the current order of fields in "my.data"
  ###       ..."main.hdr" identifies the text to be printed in the header, which defaults to the current
  ###               column names. In cases where the flextable is to include two headers ( sub.hdr != NA ),
  ###               "main.hdr" is the upper header (top) and "sub.hdr" is the lower header (below)
  ###       ..."sub.hdr" identifies the text to be printed in the subheader (i.e., underneath "main.hdr" )
  ###               when multiple headers are needed for a given table
  ###       ...and "table.type" identifies the type of table being provided, which needs to be defined
  ###               as the size tables required different formatting to make them fit (e.g., smaller cells ).
  ###               For reference, the tables in my RMarkdown are formatted using:
  ###
  ###             (1) Catch by State            -- type = "OTHER"
  ###             (2) Catch by Mode             -- type = "OTHER"
  ###             (3) MRIP AB1 CVs by Mode      -- type = "CV"
  ###             (4) MRIP B2 CVs by Mode       -- type = "CV"
  ###             (5) MRIP Total CVs (by Year)  -- type = "OTHER"
  ###             (6) AB1_LBS by State          -- type = "OTHER"
  ###             (7) MRIP AB1_LBS CVs by Mode  -- type = "CV"
  ###             (8) MRIP Length by State      -- type = "LEN"
  ###             (9) MRIP Weight by State      -- type = "WGT"
  ###            (10) MRIP Length by Mode       -- type = "LEN"
  ###            (11) MRIP Weight by Mode       -- type = "WGT"
  ###            (12) MRIP Size by Year         -- type = "L&W"
  ###            (13) TPWD Length by Mode       -- type = "LEN"
  ###            (14) LACR Length by Mode       -- type = "LEN"
  ###            (15) LACR Weight by Mode       -- type = "WGT"
  ###            (16) AvgWgt CVs by Mode        -- type = "CV"
  ###            (17) AB1_LBS by Strata         -- type = "WGT"
  ###            (18) Effort by State           -- type = "OTHER"
  ###            (19) Effort by Mode            -- type = "OTHER"
  ###            (20) UnIDd Catch and %Species  -- type = "OTHER"
  ###            (21) Catch by Data Source      -- type = "OTHER"
  ###
  ###         Additionally...
  ###             ...I also include a flextable in Figure (9), to evaluate raw vs. imputed MRIP Data
  ###                                           -- type = "OTHER"
  ###             ...define settings for any "ALLREC" summaries that might be needed (e.g., in a DW Report )
  ###                                           -- type = "ALLREC"
  
  
  ###   Before constructing anything, I do a quick check that 'strata.add' is appropriately defined...
  if( length(strata.add) == 0 ) {   strata.add = NA  }
  
  
  FT <- flextable( my.data, col_keys=col.order )
  
  if( length(sub.hdr) != 1 ) {
    FT <- set_header_labels( FT, values=sub.hdr )         ### ...lower header
    FT <- add_header( FT, top=TRUE, values=main.hdr )     ### ...upper header
  }
  
  
  ### FORMATTING ###
  
  
  ### Alignment ###
  ### -------------
  FT <- align( FT, align="center", part="all" )
  FT <- bold( FT, part="header" )
  
  
  ### Text Size & Cell Height/Width ###
  ### ---------------------------------
  FT <- fontsize( FT, part="header", size=10 )
  FT <- fontsize( FT, part="body", size=8 )
  
  FT <- width( FT, width=0.6 )
  FT <- height_all( FT, height=0.19, part="body" )
  
  if( table.type %in% c("LEN","WGT","CV") ) {
    FT <- fontsize( FT, part="header", size=7 )
    FT <- fontsize( FT, part="body", size=7 )
    FT <- width( FT, width=0.39 )
  }
  
  cols <- which( gsub( "_.*","", FT$header$col_keys ) == "Trp" )
  FT <- width( FT, cols, width=0.35 )
  
  
  ### Number Format ###
  ### -----------------
  ###   ...where the four rows below essentially represent the 'default' formats...
  cols <- which( gsub( "_.*","", FT$header$col_keys ) %in% c("Year","YEAR","Wave","WAVE") )
  FT <- colformat_num( FT, j=cols, big.mark="" )                                     ###  YEAR  -- no decimals, no 'big.mark'...
  cols <- which( gsub( "_.*","", FT$header$col_keys ) %notin% c("Year","YEAR","Wave","WAVE") )
  FT <- colformat_double( FT, j=cols, digits=0, big.mark="," )                       ###  OTHER -- no decimals, use comma to separate 1000s...
  
  ### Formatting the CV fields -- Tables 3,4,5
  cols <- which( gsub( ".*_","", FT$header$col_keys ) == "CV" )
  FT <- colformat_double( FT, j=cols, digits=2, big.mark="" )                        ###  CV -- two decimals, no 'big.mark'...
  
  if( table.type == "ALLREC" ) {
    cols <- which( grepl( "CV", FT$header$col_keys ) )
    FT <- colformat_double( FT, j=cols, digits=2, big.mark="" )                        ###  CV -- two decimals, no 'big.mark'...
  }
  
  ### Formatting the Weight fields -- Tables 8,10,11,13,16
  ###     ...where the Length fields will follow the default formatting above (i.e., digits=0 and big.mark="," )
  if( table.type == "WGT" ) {
    cols <- which( gsub( "_.*","", FT$header$col_keys ) %in% c("Min","Avg","SD","Max","AvgWgt") )
    FT <- colformat_double( FT, j=cols, digits=1, big.mark="," )
    cols <- which( gsub( ".*_","", FT$header$col_keys ) %in% c("WGT") )
    FT <- colformat_double( FT, j=cols, digits=2, big.mark="," )
  } else if( table.type == "L&W" ) {
    # cols <- which( gsub( "_.*","", FT$header$col_keys ) %in% c("Min","Avg","Max","AvgWgt") &
    #                  gsub( ".*_","", FT$header$col_keys ) == "Length" )
    # FT <- colformat_double( FT, j=cols, digits=0, big.mark="," )
    cols <- which( gsub( "_.*","", FT$header$col_keys ) %in% c("Min","Avg","Max","SD","AvgWgt") &
                     gsub( ".*_","", FT$header$col_keys ) == "Weight" )
    FT <- colformat_double( FT, j=cols, digits=1, big.mark="," )               ###  WGT -- one decimal, use comma to separate 1000s...
  }
  
  ### Formatting Proportions -- Table19 (unidentified catch)
  cols <- which( gsub( "_.*","", FT$header$col_keys ) %in% c("p.AB1","p.B2","p.AB1.wgt") )
  FT <- colformat_double( FT, j=cols, digits=3, big.mark="" )                  ###  proportions -- three decimals, no 'big.mark'...
  
  rm(cols)
  
  
  ### PRIMARY (SOLID) BORDERS ###
  ### ---------------------------
  ###     ...which draw the primary borders (solid black lines that separate header vs. body).
  ###           Note that secondary borders are also drawn in most tables (see next section)...
  FT <- border_remove( FT )
  FT <- border( FT, part="header", i=1, border.top=fp_border(color='black',width=2) )
  FT <- border( FT, part="body", i=dim(FT$body$dataset)[1], border.bottom=fp_border(color='black',width=2) )
  
  if( length(sub.hdr) == 1 ) {
    FT <- border( FT, part="header", i=1, border.bottom=fp_border(color='black',width=2) )
  } else {
    FT <- border( FT, part="header", i=2, border.bottom=fp_border(color='black',width=2) )
  }
  
  
  ### MERGE HEADER & SECONDARY (DASHED) BORDERS ###
  ### ---------------------------------------------
  ###     ...which is only done if there are columns to merge (i.e., sub.hdr != NA )...
  
  if( length(sub.hdr) != 1 ) {
    
    ###   ...and for which I use the "main.hdr" to identify which cells to merge
    ###       and which columns to separate (from other columns) using dashed-borders...
    
    col.ids = summary( as.factor( unlist(main.hdr[main.hdr!=""]) ) )
    col.ids = col.ids[ order( match(names(col.ids),unlist(main.hdr[main.hdr!=""])) ) ]
    ###     ...which identifies both the unique strata and their frequency in "main.hdr"...
    
    ### Row Identifiers ###
    ###   ...which generally consist of only the 'YEAR' field (i.e., n.cols=1 ), but some tables may include
    ###           additional identifiers (e.g., I included a "N" sample size field for Table16 -- AvgWgt by Strata )...
    col.vec = c( "SID","Year","YEAR","N","Wave","WAVE" )
    if( !is.na(strata.add) ) {  col.vec = c( col.vec, strata.add )  }
    n.cols = length( which( col.order %in% col.vec ) )
    rm( col.vec )
    
    # FT <- border( FT, part="all", j=(n.cols),
    #               border.left=fp_border(color='black',style='dashed',width=1) )
    FT <- border( FT, part="all", j=(n.cols),
                  border.right=fp_border(color='black',style='dashed',width=1) )
    
    ### Summary Fields ###
    for( m in 1:length(col.ids) ) {
      
      FT <- merge_at( FT, i=1, j=( (n.cols+1):(n.cols+as.numeric(col.ids[m])) ), part="header" )
      
      FT <- border( FT, part="all", j=(n.cols+1),
                    border.left=fp_border(color='black',style='dashed',width=1) )
      FT <- border( FT, part="all", j=(n.cols+as.numeric(col.ids[m])),
                    border.right=fp_border(color='black',style='dashed',width=1) )
      
      n.cols <- n.cols + as.numeric(col.ids[m])
    }
    rm( col.ids, n.cols )
    
  }
  
  return( FT )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------

FT.catnum.yr.strata = function( catch.table, strata = c('state','mode'), strata.add = NA, params ) {
  ###     ...where 'catch.table' is the table of catch estimates being summarized at a year-strata level,
  ###              'strata' identifies the (state or mode) strata at which summaries are being generated,
  ###              'strata.add' identifies any additional strata to maintain while constructing the table,
  ###       and 'params' the R object that (amongst other things) identifies the filters applied in this SEDAR...
  
  
  ###   ...where the calculations below are done using the numeric 'NEW_ST' or 'NEW_MODE' fields
  ###     to allow for appropriate sorting (e.g., geographically )...
  
  if( strata == 'state' ) {
    strata.var = 'NEW_ST'
  } else if( strata == 'mode' ) {
    strata.var = 'NEW_MODE'
  }
  
  strata.vec = c('Year',strata.var)
  if( !is.na(strata.add) ) {
    strata.vec = c( strata.vec, strata.add )
  }
  
  if( strata == 'mode' ) {
    dummy.table = catch.table %>%
      mutate( NEW_MODE = ifelse( NEW_MODE == "6", "4", NEW_MODE ) )
      ###   ...where the combined "Priv/Shore" mode is changed to "Priv"...
  } else {
    dummy.table = catch.table
  }
  
  dummy.table <- dummy.table %>%
    group_by( across( any_of( strata.vec ) ) ) %>%
    summarize( AB1 = sum( as.numeric(AB1), na.rm=TRUE ),
               B2  = sum( as.numeric( B2), na.rm=TRUE ) ) %>%
    select( any_of( c( strata.vec,'AB1','B2') ) ) %>%
    mutate_at( vars( any_of( c('Year') ) ), list( ~ as.integer(as.character(.)) ) ) %>%
    mutate_at( vars( any_of( c(strata.var,'AB1','B2') ) ), list( ~ as.numeric(as.character(.)) ) ) %>%
    pivot_wider( names_from = any_of( strata.var ), values_from = c(AB1,B2),
                 names_glue = sprintf('{%s}_{.value}', strata.var ) )
  rm( strata.var )
  
  ### Add Annual Total columns to 'dummy.table'...
  dummy.table$Total_AB1 <- rowSums( dummy.table[ ,which( colnames(dummy.table) %notin% c("Year",strata.add) &
                                                         grepl( "AB1",colnames(dummy.table) ) ) ], na.rm=TRUE )
  dummy.table$Total_B2  <- rowSums( dummy.table[ ,which( colnames(dummy.table) %notin% c("Year",strata.add) &
                                                         grepl( "B2" ,colnames(dummy.table) ) ) ], na.rm=TRUE )
  
  ### Adding any 'missing' records/strata to 'dummy.table'...
  if( is.na(strata.add) ) {
    add.years <- data.frame( Year = 1981:params$term.year )
    dummy.table = dummy.table %>%
      bind_rows( add.years %>% filter( Year %notin% dummy.table$Year ) ) %>%
      arrange( Year )
    rm( add.years )
    
  } else {
    eval( parse( text = paste0( 'blah = expand.grid( Year = 1981:params$term.year,',
                                strata.add,' = unique(unlist(catch.table[strata.add])) )' ) ) )
    eval( parse( text = paste0( "blah = blah[ paste0( blah$Year,'_',blah$",strata.add," ) %notin% ",
                                            " paste0( dummy.table$Year,'_',dummy.table$",strata.add," ), ]" ) ) )
    dummy.table = dummy.table %>% bind_rows( blah ) %>% arrange( across( any_of( c(strata.add,'Year') ) ) )
    rm( blah )
  }
  
  dummy.table[ is.na(dummy.table) ] <- 0
  ###     ...where I replace any <NA> values corresponding to zero catch estimates with a "0"
  ###       but also ensure any records corresponding to 'no sampling' are properly left at <NA>...
  ###
  ###     For our 'state' tables...
  if( strata == 'state' ) {
    ###   ...TX was not sampled from 1981-2 and so if these elements are now zero's (i.e., we didn't impute estimates
    ###     for these years at the SEFSC), I change them back to <NA> values to reflect this lack of sampling...
    if( any( c("1_AB1","1_B2") %in% colnames(dummy.table) ) ) {
      if( sum( dummy.table[ which( dummy.table$Year %in% c(1981,1982) ),
                            which( colnames(dummy.table) %in% c("1_AB1","1_B2") ) ], na.rm=TRUE ) == 0 ) {
        dummy.table[ which( dummy.table$Year %in% c(1981,1982) ), which( colnames(dummy.table) %in% c("1_AB1","1_B2") ) ] <- NA
      }
    }
    
  ###     For our 'mode' tables...
  } else if( strata == 'mode' ) {
    ###   ...any Hbt sampling (by MRIP) in the GOM b/w 1981-1985 and in the NATL/MAT b/w 2004+ is set to <NA>
    ###     to avoid overlap with SRHS estimates...
    if( any( grepl( '2_',colnames(dummy.table) ) ) & grepl( "Gulf of America", params$region ) ) {
      ###     ...which corresponds to either 'Gulf of Mexico' or 'Gulf of Mexico & South Atlantic' assessments...
      dummy.table[ which( dummy.table$Year > 1985 ), c("2_AB1","2_B2") ] <- NA
    } else if( any( grepl( '2_',colnames(dummy.table) ) ) &
               any( c("VA","MD","DE","PA","NJ","NY","CT","RI","MA","NH","ME") %in% params$subset.states ) &
               any( c("TX","LA","MS","AL","FLW") %notin% params$subset.states ) ) {
      dummy.table[ which( dummy.table$Year < 2004 ), c("2_AB1","2_B2") ] <- NA
    } else if( any( grepl( '2_',colnames(dummy.table) ) ) &
               any( c("VA","MD","DE","PA","NJ","NY","CT","RI","MA","NH","ME") %in% params$subset.states ) &
               any( c("TX","LA","MS","AL","FLW") %in% params$subset.states ) ) {
      dummy.table[ which( dummy.table$Year > 1985 & dummy.table$Year < 2004 ), c("2_AB1","2_B2") ] <- NA
    }
    ###     Similar adjustments are needed for any combined ForHire estimates from the MAT/NATL b/w 1981-2003...
    if( any( grepl( '5_',colnames(dummy.table) ) ) ) {
      dummy.table[ ( dummy.table$Year > 2003 ) , ( grepl("5_",colnames(dummy.table)) ) ] <- NA
    }
  }
  
  
  ### SORTING COLUMNS ###
  ###   Before creating my flextable, I then define the ORDER in which I want the dummy.table columns to be displayed,
  ###   and the corresponding HEADERS to be printed out for each of these columns (in FTxx)...
  
  col.strata = gsub( "_.*","", colnames(dummy.table)[ !(
                               colnames(dummy.table) %in% c("Year",strata.add,"Total_AB1","Total_B2") ) ] )
  col.catch  = gsub( ".*_","", colnames(dummy.table)[ !(
                               colnames(dummy.table) %in% c("Year",strata.add,"Total_AB1","Total_B2") ) ] )
  
  col.order = "Year"
  if( !is.na(strata.add) ) {  col.order = c( col.order,strata.add )  }
  col.order = c( col.order,
                 paste0( sort( as.numeric(col.strata)[ which( col.catch == "AB1" ) ] ),"_AB1" ), "Total_AB1",
                 paste0( sort( as.numeric(col.strata)[ which( col.catch == "B2"  ) ] ),"_B2"  ), "Total_B2"  )
  rm( col.strata, col.catch )
  
  dummy.table <- dummy.table[ ,col.order[ which( col.order %in% colnames(dummy.table) ) ] ]
  rm( col.order )
  
  
  ### RENAMING COLUMNS ###
  if( strata == 'state' ) {
    colnames(dummy.table) <- str_replace( colnames(dummy.table),"9.1_","SNC_" )
    colnames(dummy.table) <- str_replace( colnames(dummy.table),"9.2_","NNC_" )
    colnames(dummy.table) <- str_replace( colnames(dummy.table), "10_","VA_" )
    colnames(dummy.table) <- str_replace( colnames(dummy.table), "11_","MD_" )
    colnames(dummy.table) <- str_replace( colnames(dummy.table), "12_","DE_" )
    colnames(dummy.table) <- str_replace( colnames(dummy.table), "13_","NJ_" )
    colnames(dummy.table) <- str_replace( colnames(dummy.table), "14_","NY_" )
    colnames(dummy.table) <- str_replace( colnames(dummy.table), "15_","CT_" )
    colnames(dummy.table) <- str_replace( colnames(dummy.table), "16_","RI_" )
    colnames(dummy.table) <- str_replace( colnames(dummy.table), "17_","MA_" )
    colnames(dummy.table) <- str_replace( colnames(dummy.table), "18_","NH_" )
    colnames(dummy.table) <- str_replace( colnames(dummy.table), "19_","ME_" )
    colnames(dummy.table) <- str_replace( colnames(dummy.table), "20_","PR_" )
    colnames(dummy.table) <- str_replace( colnames(dummy.table),  "1_","TX_" )
    colnames(dummy.table) <- str_replace( colnames(dummy.table),  "2_","LA_" )
    colnames(dummy.table) <- str_replace( colnames(dummy.table),  "3_","MS_" )
    colnames(dummy.table) <- str_replace( colnames(dummy.table),  "4_","AL_" )
    colnames(dummy.table) <- str_replace( colnames(dummy.table),  "5_","FLW_" )
    colnames(dummy.table) <- str_replace( colnames(dummy.table),  "6_","FLE_" )
    colnames(dummy.table) <- str_replace( colnames(dummy.table),  "7_","GA_" )
    colnames(dummy.table) <- str_replace( colnames(dummy.table),  "8_","SC_" )
    colnames(dummy.table) <- str_replace( colnames(dummy.table),  "9_","NC_" )
    
    ### Similarly, I check to see if the FL Keys are the only area of "FLW" being considered...
    if( FLK.only == TRUE ) {
      ###       ...and change "FLW" --> "FLKeys" if so...
      colnames(dummy.table) <- str_replace( colnames(dummy.table), "FLW","FLKeys" )
    }
    
    
  } else if( strata == 'mode' ) {
    colnames(dummy.table) <- str_replace( colnames(dummy.table), "1_","Shore_" )
    colnames(dummy.table) <- str_replace( colnames(dummy.table), "2_","Hbt_" )
    colnames(dummy.table) <- str_replace( colnames(dummy.table), "3_","Cbt_" )
    colnames(dummy.table) <- str_replace( colnames(dummy.table), "4_","Priv_" )
    colnames(dummy.table) <- str_replace( colnames(dummy.table), "5_","CbtHbt_" )
  }
  
  
  ###   ...and as a final check for any strata (i.e., states/modes ) providing no catch information...
  col.vec = c("SID","Year")
  if( !is.na(strata.add) ) {  col.vec = c( col.vec,strata.add )  }
  
  retain.cols = names( which(
    colSums( dummy.table[ , !( colnames(dummy.table) %in% col.vec ) ], na.rm=TRUE ) != 0 ) )
  dummy.table = dummy.table[ , colnames(dummy.table) %in% c(col.vec,retain.cols) ]
  rm( retain.cols )
  
  
  
  ### SAVING/APPLYING THE DESIRED COLUMN ORDER ###
  ### --------------------------------------------
  ###     ...because column order (in flextables) also depends on whether certain columns are missing data;
  ###       for example, if 1981 estimates are present in one vector but not the other, then the vector with 1981
  ###       estimates will always be provided first. To circumvent this default, I manually define column order,
  ###       which is done separately for any 'SID' domains in case the required 'strata' differs between areas...
  
  strat <- unique( gsub( "_.*","", colnames(dummy.table)[ colnames(dummy.table) %notin% col.vec ] ) )
  estim <- unique( gsub( ".*_","", colnames(dummy.table)[ colnames(dummy.table) %notin% col.vec ] ) )
  rm( col.vec )
  
  ###     ...which I use to re-label my flextable headers...
  col.order <- vector( "character" )
  upper.hdr <- list()
  lower.hdr  <- list()
  
  ###     ...where the first column represents "Year" and so is neglected in the below for() loop. However,
  ###       I still need to 'account' for it in both headers (to save the space)...
  col.order <- "Year"
  lower.hdr$Year <- "Year"
  upper.hdr$Year <- ""
  
  if( !is.na(strata.add) ) {
    col.order <- c( col.order, strata.add )
    lower.hdr[[strata.add]] <- strata.add
    upper.hdr[[strata.add]] <- ""
  }
  n.strata = length(col.order)
  
  for( i in 1:length(strat) ) {
    col.order[ (2*(i-1))+n.strata+1 ] <- paste0( strat[i],"_AB1" )
    col.order[ (2*(i-1))+n.strata+2 ] <- paste0( strat[i],"_B2" )
    
    ### In defining new headers, the flextable package seems to require piece-meal identification...
    ### #       FT1 <- set_header_labels( FT1, AL_AB1="AB1", FLW_AB1="AB1", LA_AB1="AB1", MS_AB1="AB1",
    ### #                                      TX_AB1="AB1", FLE_AB1="AB1", AL_B2="B2", FLW_B2="B2", LA_B2="B2",
    ### #                                      MS_B2="B2", TX_B2="B2", FLE_B2="B2" )
    ###     ...and so I use the eval(parse(text= )) trick to both define my new headers (in lists) and associate
    ###       these names with the original column name (in dummy.table)...
    eval( parse( text=paste0(
      "upper.hdr$",paste0( strat[i],"_AB1" ),"= '",strat[i],"'" ) ) )
    eval( parse( text=paste0(
      "upper.hdr$",paste0( strat[i],"_B2" ),"= '",strat[i],"'" ) ) )
    
    eval( parse( text=paste0(
      "lower.hdr$",paste0( strat[i],"_AB1" ),"= 'AB1'" ) ) )
    eval( parse( text=paste0(
      "lower.hdr$",paste0( strat[i],"_B2" ),"= 'B2'" ) ) )
  }
  rm( n.strata )
  
  dummy.table = dummy.table[ ,col.order ]
  
  
  return.object = list( dummy.table, strat, col.order, upper.hdr, lower.hdr )
  names(return.object) = c( 'catch.table','strata','col.order','upper.hdr','lower.hdr' )
  rm( dummy.table, strat, estim, col.order, upper.hdr, lower.hdr )
  
    
  return( return.object )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------

FT.catnum.cvs = function( catch.table, strata = c('strata','annual'), strata.add = NA, params ) {
  ###     ...where 'catch.table' is the table of catch & CV estimates being summarized at a year-strata level,
  ###              'strata' identifies the resolution (by state/mode, or year) at which summaries are being generated,
  ###              'strata.add' identifies any additional strata to maintain while constructing the table,
  ###       and 'params' the R object that (amongst other things) identifies the strata (i.e., state or mode)
  ###               at which CV summaries are being requested in this assessment ( as defined in params$CV.tables.by )...
  
  
  if( is.na(strata.add) ) {
    cv.table <- catch.table %>% arrange( Year )
  } else {
    cv.table <- catch.table %>% arrange( across( any_of( c(strata.add,'Year') ) ) )
  }
  
  cv.table = cv.table %>%
    mutate_at( vars( Year ), list( ~ as.integer(.) ) ) %>%
    mutate_at( vars( ( contains("AB1") & !contains( c("CV","PSU","TRP","SS") ) ) |
                     ( contains( "B2") & !contains( c("CV","PSU","TRP","SS") ) ) ),
               list( ~ as.integer( trunc( as.numeric(.)+0.5 ) ) ) ) %>%
    mutate_at( vars( contains("CV") ), list( ~ as.numeric(.) ) )
  
  colnames(cv.table) = gsub(  "CV_AB1", "CV", colnames(cv.table) )
  colnames(cv.table) = gsub(  "CV_B2" , "CV", colnames(cv.table) )
  colnames(cv.table) = gsub( "PSU_AB1","PSU", colnames(cv.table) )
  colnames(cv.table) = gsub( "PSU_B2" ,"PSU", colnames(cv.table) )
  colnames(cv.table) = gsub( "TRP_AB1","Trp", colnames(cv.table) )
  colnames(cv.table) = gsub( "TRP_B2" ,"Trp", colnames(cv.table) )
  colnames(cv.table) = gsub(  "SS_AB1", "SS", colnames(cv.table) )
  colnames(cv.table) = gsub(  "SS_B2" , "SS", colnames(cv.table) )
  
  
  ###   As a first check, I identify any modes for which catch estimates aren't provided/needed, for which the associated
  ###   sample size fields ('TRP','PSU','SS') should be composed entirely of <NA> values. As an example, in assessments
  ###   with SID domains, we may need to include the combined 'CbtHbt' mode to account for any catch north of VA
  ###   (and so its needed in the 'MRIP catCV' tab), but such a mode wouldn't be needed for CV summaries limited to
  ###   SID domains covering the GOM or SATL (and so should be excluded from the corresponding CV table)...
  drop.strata = unique( gsub( "_.*","", names( which(
    colSums( is.na( cv.table[ , gsub( ".*_","",colnames(cv.table) ) %in% c('PSU','Trp','SS') ] ) ) > 0 ) ) ) )
  cv.table = cv.table[ , gsub( "_.*","",colnames(cv.table) ) %notin% drop.strata ]
  rm( drop.strata )
  
  ###   ...I then replace all NA's that correspond to zero catch estimates to "0" in the table...
  cv.table[ is.na(cv.table) ] <- 0
  
  if( params$CV.tables.by == "Area" ) {
    
    ### STATE-SPECIFIC CVs ###
    ### ----------------------
    if( FLK.only == TRUE ) {
      colnames(cv.table) <- str_replace( colnames(cv.table), "FLW","FLKeys" )
    }
    ### ...which, when by-state, might be needed for TPWD as sampling did not start until 1983. There is a chance
    ###       that Vivian and I imputed these missing years and so I check to see if sum(1981-1982) == 0,
    ###       from which 1981-1982 values = NA if true or left at their imputed values if false...
    if( any( c("TX_AB1","TX_B2","TX_CV","TX_SS") %in% colnames(cv.table) ) ) {
      if( sum( cv.table[ which( cv.table$Year %in% c(1981,1982) ), 
                         which( colnames(cv.table) %in% c("TX_AB1","TX_B2","TX_CV","TX_PSU","TX_Trp") ) ], na.rm=TRUE ) == 0 ) {
        cv.table[ which( cv.table$Year %in% c(1981,1982) ), 
                  which( colnames(cv.table) %in% c("TX_AB1","TX_B2","TX_CV","TX_PSU","TX_Trp") ) ] <- NA
      }
    }
    

  } else if( params$CV.tables.by == "Mode" ) {
    
    ### MODE-SPECIFIC CVs ###
    ### ---------------------
    ### ...which, when by-mode, is needed for Hbt's to avoid overlap with SRHS estimates. In particular,
    ###     I want to include any Gulf HBTs 1981-1985 and any Mid/North-Atlantic HBTs 2004+...
    if( "Hbt" %in% params$subset.modes & grepl( "Gulf of America", params$region ) ) {
      cv.table[ which( cv.table$Year > 1985 ),
                colnames(cv.table) %in% c("Hbt_AB1","Hbt_B2","Hbt_CV","Hbt_PSU","Hbt_Trp") ] <- NA
    } else if( "Hbt" %in% params$subset.modes &
               any( c("VA","MD","DE","PA","NJ","NY","CT","RI","MA","NH","ME") %in% params$subset.states ) &
               any( c("TX","LA","MS","AL","FLW") %notin% params$subset.states ) ) {
      cv.table[ which( cv.table$Year < 2004 ),
                colnames(cv.table) %in% c("Hbt_AB1","Hbt_B2","Hbt_CV","Hbt_PSU","Hbt_Trp") ] <- NA
    } else if( "Hbt" %in% params$subset.modes &
               any( c("VA","MD","DE","PA","NJ","NY","CT","RI","MA","NH","ME") %in% params$subset.states ) &
               any( c("TX","LA","MS","AL","FLW") %in% params$subset.states ) ) {
      cv.table[ which( cv.table$Year > 1985 & cv.table$Year < 2004 ),
                colname(cv.table) %in% c("Hbt_AB1","Hbt_B2","Hbt_CV","Hbt_PSU","Hbt_Trp") ] <- NA
    }
    ###     Similarly, I also make sure any estimates for the combined Cbt/Hbt mode,
    ###     which are only considered in the Mid/North Atlantic from 1981-2003, are set to <NA> for 2004+ ...
    if('CbtHbt' %in% unique(gsub('_.*', '', colnames(cv.table)))) {
      cv.table[ which( cv.table$Year > 2003 ),
                colnames(cv.table) %in% c("CbtHbt_AB1","CbtHbt_B2","CbtHbt_CV","CbtHbt_PSU","CbtHbt_Trp") ] <- NA
    }
  }
  
  
  ### I then initialize objects to store the order in which I want my headers displayed in this flextable...
  col.order <- vector( "character" )
  upper.hdr <- list()
  lower.hdr  <- list()
  
  col.order <- "Year"
  lower.hdr$Year <- "Year"
  upper.hdr$Year <- ""
  
  if( !is.na(strata.add) ) {
    col.order <- c( col.order, strata.add )
    lower.hdr[[strata.add]] <- strata.add
    upper.hdr[[strata.add]] <- ""
  }
  n.strata = length(col.order)
  
  
  ###   Because (CV) tables became too big (in Word) after adding a second sample size column (PSU and AT),
  ###   the "Total" columns were moved to a separate cv.table. In this, different fields are retained
  ###   ( from cv.table ) whether the 'Total' or 'strata' specific summaries are being generated...
  
  col.vec = c("SID","Year")
  if( !is.na(strata.add) ) {  col.vec = c( col.vec,strata.add )  }
  
  if( strata == 'strata' ) {
    
    ### STATE/MODE SPECIFIC SUMMARIES ###
    ### ---------------------------------
    
    strat <- gsub( "_.*","", colnames(cv.table)[ colnames(cv.table) %notin% col.vec ] )
    strat <- unique( strat )
    
    strat = strat[ !( strat == "Total" ) ]
    ###   ...which are then sorted based on the desired order in the resultant CV table...
    if( params$CV.tables.by == "Area" ) {
      strat = strat[ order( match( strat, c("TX","LA","MS","AL","FLW","FLKeys",
                                            "FLE","GA","SC","NC","SNC","NNC",
                                            "VA","MD","DE","NJ","NY","CT","RI","MA","NH","ME", "PR") ) ) ]
    } else if( params$CV.tables.by == "Mode" ) {
      strat = strat[ order( match( strat, c("Cbt","CbtHbt","Hbt","Priv","Priv/Shore","Shore") ) ) ]
    }
    
    ###   ...for which I need to identify the catch metric being summarized in this table (i.e., AB1 or B2 )...
    if( any( gsub( ".*_","", colnames(cv.table) ) == 'AB1' ) ) {
      catch.metric = 'AB1'
    } else if( any( gsub( ".*_","", colnames(cv.table) ) == 'B2' ) ) {
      catch.metric = 'B2'
    }
    
    if( params$region != "Caribbean" ) {      ### ...which has two sample size columns (PSU & Trp)
      
      for( i in 1:length(strat) ) {
        col.order[ (4*(i-1))+n.strata+1 ] <- paste0( strat[i],"_",catch.metric )
        col.order[ (4*(i-1))+n.strata+2 ] <- paste0( strat[i],"_CV" )
        col.order[ (4*(i-1))+n.strata+3 ] <- paste0( strat[i],"_PSU" )
        col.order[ (4*(i-1))+n.strata+4 ] <- paste0( strat[i],"_Trp" )
        
        eval( parse( text=paste0(
          "upper.hdr$",paste0( strat[i],"_",catch.metric ),"= '",strat[i],"'" ) ) )
        eval( parse( text=paste0(
          "upper.hdr$",paste0( strat[i],"_CV" ),"= '",strat[i],"'" ) ) )
        eval( parse( text=paste0(
          "upper.hdr$",paste0( strat[i],"_PSU" ),"= '",strat[i],"'" ) ) )
        eval( parse( text=paste0(
          "upper.hdr$",paste0( strat[i],"_Trp" ),"= '",strat[i],"'" ) ) )
        eval( parse( text=paste0(
          "lower.hdr$",paste0( strat[i],"_",catch.metric ),"= '",catch.metric,"'" ) ) )
        eval( parse( text=paste0(
          "lower.hdr$",paste0( strat[i],"_CV" ),"= 'CV'" ) ) )
        eval( parse( text=paste0(
          "lower.hdr$",paste0( strat[i],"_PSU" ),"= 'PSU'" ) ) )
        eval( parse( text=paste0(
          "lower.hdr$",paste0( strat[i],"_Trp" ),"= 'Trp'" ) ) )
      }
      
    } else {
      
      for( i in 1:length(strat) ) {
        col.order[ (3*(i-1))+n.strata+1 ] <- paste0( strat[i],"_",catch.metric )
        col.order[ (3*(i-1))+n.strata+2 ] <- paste0( strat[i],"_CV" )
        col.order[ (3*(i-1))+n.strata+3 ] <- paste0( strat[i],"_SS" )
        
        eval( parse( text=paste0(
          "upper.hdr$",paste0( strat[i],"_",catch.metric ),"= '",strat[i],"'" ) ) )
        eval( parse( text=paste0(
          "upper.hdr$",paste0( strat[i],"_CV" ),"= '",strat[i],"'" ) ) )
        eval( parse( text=paste0(
          "upper.hdr$",paste0( strat[i],"_SS" ),"= '",strat[i],"'" ) ) )
        eval( parse( text=paste0(
          "lower.hdr$",paste0( strat[i],"_",catch.metric ),"= '",catch.metric,"'" ) ) )
        eval( parse( text=paste0(
          "lower.hdr$",paste0( strat[i],"_CV" ),"= 'CV'" ) ) )
        eval( parse( text=paste0(
          "lower.hdr$",paste0( strat[i],"_SS" ),"= 'SS'" ) ) )
      }
      
    }
    rm( catch.metric )
    
    
  } else if( strata == 'annual' ) {
    
    ### TOTAL/ANNUAL CV SUMMARIES ###
    ### -----------------------------
    
    strat = c('AB1','B2')
    
    if( params$region != "Caribbean" ) {      ### ...which has two sample size columns (PSU & Trp)
      
      for( i in 1:length(strat) ) {
        col.order[ (4*(i-1))+n.strata+1 ] <- paste0( strat[i],"_Total" )
        col.order[ (4*(i-1))+n.strata+2 ] <- paste0( strat[i],"_CV" )
        col.order[ (4*(i-1))+n.strata+3 ] <- paste0( strat[i],"_PSU" )
        col.order[ (4*(i-1))+n.strata+4 ] <- paste0( strat[i],"_Trp" )

        eval( parse( text=paste0(
          "upper.hdr$",paste0( strat[i],"_Total" ),"= '",strat[i],"'" ) ) )
        eval( parse( text=paste0(
          "upper.hdr$",paste0( strat[i],"_CV" ),"= '",strat[i],"'" ) ) )
        eval( parse( text=paste0(
          "upper.hdr$",paste0( strat[i],"_PSU" ),"= '",strat[i],"'" ) ) )
        eval( parse( text=paste0(
          "upper.hdr$",paste0( strat[i],"_Trp" ),"= '",strat[i],"'" ) ) )
        eval( parse( text=paste0(
          "lower.hdr$",paste0( strat[i],"_Total" ),"= 'Total'" ) ) )
        eval( parse( text=paste0(
          "lower.hdr$",paste0( strat[i],"_CV" ),"= 'CV'" ) ) )
        eval( parse( text=paste0(
          "lower.hdr$",paste0( strat[i],"_PSU" ),"= 'PSU'" ) ) )
        eval( parse( text=paste0(
          "lower.hdr$",paste0( strat[i],"_Trp" ),"= 'Trp'" ) ) )
      }

    } else {
        
        for( i in 1:length(strat) ) {
        col.order[ (3*(i-1))+n.strata+1 ] <- paste0( strat[i],"_Total" )
        col.order[ (3*(i-1))+n.strata+2 ] <- paste0( strat[i],"_CV" )
        col.order[ (3*(i-1))+n.strata+3 ] <- paste0( strat[i],"_SS" )
        
        eval( parse( text=paste0(
          "upper.hdr$",paste0( strat[i],"_Total" ),"= '",strat[i],"'" ) ) )
        eval( parse( text=paste0(
          "upper.hdr$",paste0( strat[i],"_CV" ),"= '",strat[i],"'" ) ) )
        eval( parse( text=paste0(
          "upper.hdr$",paste0( strat[i],"_SS" ),"= '",strat[i],"'" ) ) )
        eval( parse( text=paste0(
          "lower.hdr$",paste0( strat[i],"_Total" ),"= 'Total'" ) ) )
        eval( parse( text=paste0(
          "lower.hdr$",paste0( strat[i],"_CV" ),"= 'CV'" ) ) )
        eval( parse( text=paste0(
          "lower.hdr$",paste0( strat[i],"_SS" ),"= 'SS'" ) ) )
      }
    }
  }
  
  cv.table = cv.table[ ,col.order ]
  
  
  return.object = list( cv.table, col.order, upper.hdr, lower.hdr )
  names(return.object) = c( 'catch.table','col.order','upper.hdr','lower.hdr' )
  rm( cv.table, strat, col.order, upper.hdr, lower.hdr )
  
  
  return( return.object )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------

FT.catnum.unid = function( catch.table, params ) {
  ###     ...where 'catch.table' is the table of catch estimates for the species-of-interest, for the unidentified
  ###                 taxa ( some percent of which is assumed to be the species-of-interest ), and any other species
  ###                 that could also be contributed to the unidentified catch estimates...
  ###       and 'params' the R object that (amongst other things) identifies the species-of-interest
  ###                 ( params$species.name, params$species.add ) and unidentified taxa ( params$species.unid )...
  
  
  ###   This function works by constructing various summary tables ( from "catch.table" ) which provide:
  ###       (1) the total catch of unidentified groups (by year) and
  ###       (2) the catch of the species-of-interest (i.e., "params$species.name" & "params$species.add" )
  ###               relative to the catch of any other taxa (identified to species) that could be contributing
  ###               to the unidentified catch estimates summarized in (1)...
  
  
  ### Starting with (1)...
  dummy.table = catch.table[ which( catch.table$NEW_COM %in% tolower(params$species.unid) ), ] %>%
    group_by( Year ) %>%
    summarize( AB1 = sum( as.numeric(AB1), na.rm=TRUE ),
                B2 = sum( as.numeric(B2), na.rm=TRUE ) ) %>%
               # AB1.wgt = sum( as.numeric(lbsest_SECwwt), na.rm=TRUE ) ) %>%
    select( Year, AB1, B2 ) %>%
    arrange( Year )
  dummy.table = as.data.frame( dummy.table )
  
  
  ###   ...and moving on to (2)...
  all.sums = catch.table[ which( catch.table$NEW_COM %notin% tolower(params$species.unid) ), ] %>%
    group_by( Year ) %>%
    summarise( AB1 = sum( AB1, na.rm=TRUE ),
                B2 = sum(  B2, na.rm=TRUE ) )
               # AB1.wgt = sum( as.numeric(lbsest_SECwwt), na.rm=TRUE ) )
  ###   ...where "all.sums" is the sum of catch (by year) for all taxa in "catch.table" identified to species...
  
  spp.sums = catch.table[ which( catch.table$NEW_COM %in% tolower( c(params$species.name,params$species.add) ) ), ] %>%
    group_by( Year ) %>%
    summarise( AB1 = sum( AB1, na.rm=TRUE ),
                B2 = sum(  B2, na.rm=TRUE ) )
               # AB1.wgt = sum( as.numeric(lbsest_SECwwt), na.rm=TRUE ) )
  ###   ...where "spp.sums" is the sum of catch (by year) for just the species-of-interest...
  
  all.sums = left_join( all.sums, spp.sums, by="Year", suffix=c("_all","_spp") )
  rm( spp.sums )
  
  all.sums["p.AB1"] = all.sums$AB1_spp / all.sums$AB1_all
  all.sums["p.B2" ] = all.sums$B2_spp  / all.sums$B2_all
  # all.sums["p.AB1.wgt"] = all.sums$AB1.wgt_spp / all.sums$AB1.wgt_all
  
  all.sums[ is.na(all.sums) ] = 0
  
  
  ### I then combine (1) and (2)...
  dummy.table = full_join( all.sums[ ,c("Year","p.AB1","p.B2") ], dummy.table, by="Year" )
  # dummy.table = full_join( all.sums[ ,c("Year","p.AB1","p.B2","p.AB1.wgt") ], Table20, by="Year" )
  
  dummy.table[ is.na(dummy.table) ] = 0
  
  ###   ...and make sure the columns are properly ordered from the above join...
  dummy.table = dummy.table[ ,c("Year","AB1","B2","p.AB1","p.B2") ]
  # dummy.table = dummy.table[ ,c("Year","AB1","B2","AB1.wgt","p.AB1","p.B2","p.AB1.wgt") ]
  
  ### Lastly, I add a 'Grand Total' column (over all years) to the end of "Table20"...
  dummy.table = rbind( dummy.table,
                   data.frame( Year = "Grand Total",
                               AB1 = sum(dummy.table$AB1),
                               B2  = sum(dummy.table$B2),
                               # AB1.wgt = sum(dummy.table$AB1.wgt),
                               p.AB1 = sum(all.sums$AB1_spp)/sum(all.sums$AB1_all),
                               p.B2  = sum(all.sums$B2_spp)/sum(all.sums$B2_all) ) )
                               # p.AB1.wgt = sum(all.sums$AB1.wgt_spp)/sum(all.sums$AB1.wgt_all) ) )
  rm( all.sums )
  
  
  col.order <- colnames(dummy.table)
  
  upper.hdr <- list()
  upper.hdr$Year = ""
  
  lower.hdr  <- list()
  lower.hdr$Year = "Year"
  
  for( i in 2:length(col.order) ) {
    
    eval( parse( text=paste0(
      "lower.hdr$",col.order[i],"= '",gsub( "p.","%", col.order[i] ),"'" ) ) )
    
    if( any( grep( "p.", col.order[i] ) ) ) {
      eval( parse( text=paste0(
        "upper.hdr$",col.order[i],"= 'Ratio'" ) ) )
    } else {
      eval( parse( text=paste0(
        "upper.hdr$",col.order[i],"= 'UNID CATCH'" ) ) )
    }
  }
  
  
  dummy.table = dummy.table[ ,col.order ]
  
  
  return.object = list( dummy.table, col.order, upper.hdr, lower.hdr )
  names(return.object) = c( 'unid.table','col.order','upper.hdr','lower.hdr' )
  rm( dummy.table, col.order, upper.hdr, lower.hdr )
  
  
  return( return.object )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------

FT.catlbs.yr.strata = function( catch.table, strata = c('state','mode'), params ) {
  ###     ...where 'catch.table' is the table of catch estimates being summarized at a year-strata level,
  ###              'strata' identifies the (state or mode) strata at which summaries are being generated,
  ###       and 'params' the R object that (amongst other things) identifies the filters applied in this SEDAR...
  
  
  ###   Note that most of this code was simply copy-pasted from FT.catnum.yr.strata(), the logic of which
  ###   is essentially identical ( except we're summarizing LBS vs. AB1/B2 )...
  
  
  if( strata == 'state' ) {
    
    
    ### YEAR-STATE SUMMARIES ###
    ### ------------------------
    
    AB1.lbs <- catch.table %>% group_by( across( any_of( c('Year','NEW_ST') ) ) )
    
    if( params$wgt.metric == "whole weight" & "lbsest_SECwwt" %in% colnames(raw.catch) ) {
      AB1.lbs = AB1.lbs %>% summarize( LBS = sum( as.numeric(lbsest_SECwwt), na.rm=TRUE ) )
    } else if( params$wgt.metric == "gutted weight" & "lbsest_SECgwt" %in% colnames(raw.catch)) {
      AB1.lbs = AB1.lbs %>% summarize( LBS = sum( as.numeric(lbsest_SECgwt), na.rm=TRUE ) )
    } else {
      AB1.lbs = AB1.lbs %>% summarize( LBS = sum( as.numeric(lbsest_SEC), na.rm=TRUE ) )
    }
    
    AB1.lbs = AB1.lbs %>%
      select( any_of( c('Year','NEW_ST','LBS') ) ) %>%
      mutate_at( vars( Year,NEW_ST,LBS ), list( ~ as.numeric(as.character(.)) ) ) %>%
      pivot_wider( names_from=NEW_ST, values_from=LBS )
    
    AB1.lbs$Total <- rowSums( AB1.lbs[ ,which( colnames(AB1.lbs) %notin% c("Year") ) ], na.rm=TRUE )
    
    dummy.table = AB1.lbs
    rm( AB1.lbs )
    
    
    add.years <- data.frame( Year = 1981:params$term.year )
    dummy.table = dummy.table %>% bind_rows( add.years %>% filter( Year %notin% dummy.table$Year ) ) %>% arrange( Year )
    rm( add.years )
    
    dummy.table[ is.na(dummy.table) ] <- 0
    ###     ...where I add 0's to all strata for which catch was estimated at zero...
    ### However, TX was not sampled from 1981-2 and so if these elements are now zero's (i.e., we didn't impute estimates
    ###       for these years at the SEFSC), I change them back to <NA> values to reflect this lack of sampling...
    if( any( c("1_AB1","1_B2") %in% colnames(dummy.table) ) ) {
      if( sum( dummy.table[ which( dummy.table$Year %in% c(1981,1982) ),
                            which( colnames(dummy.table) %in% c("1") ) ], na.rm=TRUE ) == 0 ) {
        dummy.table[ which( dummy.table$Year %in% c(1981,1982) ), which( colnames(dummy.table) %in% c("1") ) ] <- NA
      }
    }
    
    
    ### SORTING COLUMNS ###
    ###     Before creating my flextable, I then define the ORDER in which I want the Table6 columns to be displayed
    ###     and the corresponding HEADERS to be printed out for each of these columns (in FT6). Note that, unlike Table1,
    ###     the column names of Table6 are simply the numeric state codes because I do not need to distinguish catch type
    ###     ( Table1 = AB1 vs B2 , Table6 = AB1.wgt ). Therefore, there is no need to separate the numeric state.codes
    ###     from the character catch.codes before sorting, so I just sort...
    col.order <- sort( as.numeric( colnames(dummy.table)[ !( colnames(dummy.table) %in% c("Year","Total") ) ] ) )
    col.order <- c( "Year", col.order, "Total" )    ### ...and making sure "Year" is the first column...
    
    dummy.table <- dummy.table[ ,col.order ]
    
    colnames(dummy.table) <- str_replace( colnames(dummy.table),"9.1","SNC" )
    colnames(dummy.table) <- str_replace( colnames(dummy.table),"9.2","NNC" )
    colnames(dummy.table) <- str_replace( colnames(dummy.table), "10","VA" )
    colnames(dummy.table) <- str_replace( colnames(dummy.table), "11","MD" )
    colnames(dummy.table) <- str_replace( colnames(dummy.table), "12","DE" )
    colnames(dummy.table) <- str_replace( colnames(dummy.table), "13","NJ" )
    colnames(dummy.table) <- str_replace( colnames(dummy.table), "14","NY" )
    colnames(dummy.table) <- str_replace( colnames(dummy.table), "15","CT" )
    colnames(dummy.table) <- str_replace( colnames(dummy.table), "16","RI" )
    colnames(dummy.table) <- str_replace( colnames(dummy.table), "17","MA" )
    colnames(dummy.table) <- str_replace( colnames(dummy.table), "18","NH" )
    colnames(dummy.table) <- str_replace( colnames(dummy.table), "19","ME" )
    colnames(dummy.table) <- str_replace( colnames(dummy.table), "20","PR" )
    colnames(dummy.table) <- str_replace( colnames(dummy.table),  "1","TX" )
    colnames(dummy.table) <- str_replace( colnames(dummy.table),  "2","LA" )
    colnames(dummy.table) <- str_replace( colnames(dummy.table),  "3","MS" )
    colnames(dummy.table) <- str_replace( colnames(dummy.table),  "4","AL" )
    colnames(dummy.table) <- str_replace( colnames(dummy.table),  "5","FLW" )
    colnames(dummy.table) <- str_replace( colnames(dummy.table),  "6","FLE" )
    colnames(dummy.table) <- str_replace( colnames(dummy.table),  "7","GA" )
    colnames(dummy.table) <- str_replace( colnames(dummy.table),  "8","SC" )
    colnames(dummy.table) <- str_replace( colnames(dummy.table),  "9","NC" )
    
    ### Similarly, I check to see if the FL Keys are the only area of "FLW" being considered...
    if( FLK.only == TRUE ) {
      ###       ...and change "FLW" --> "FLKeys" if so...
      colnames(dummy.table) <- str_replace( colnames(dummy.table), "FLW","FLKeys" )
    }
    
    col.order <- colnames(dummy.table)
    
    
  } else if( strata == 'mode' ) {
    
    
    ### YEAR-MODE SUMMARIES ###
    ### ------------------------
    
    ### Summarize AB1 ###
    AB1.lbs <- catch.table %>%
      mutate( NEW_MODE = ifelse( NEW_MODE == "6", "4", NEW_MODE ) ) %>%
      ###   ...where the combined "Priv/Shore" mode is changed to "Priv"...
      group_by( across( any_of( c('Year','NEW_MODE') ) ) )
    
    if( params$wgt.metric == "whole weight" & "lbsest_SECwwt" %in% colnames(raw.catch) ) {
      AB1.lbs = AB1.lbs %>% summarize( LBS = sum( as.numeric(lbsest_SECwwt), na.rm=TRUE ) )
    } else if( params$wgt.metric == "gutted weight" & "lbsest_SECgwt" %in% colnames(raw.catch)) {
      AB1.lbs = AB1.lbs %>% summarize( LBS = sum( as.numeric(lbsest_SECgwt), na.rm=TRUE ) )
    } else {
      AB1.lbs = AB1.lbs %>% summarize( LBS = sum( as.numeric(lbsest_SEC), na.rm=TRUE ) )
    }
    
    AB1.lbs = AB1.lbs %>%
      select( any_of( c('Year','NEW_MODE','LBS') ) ) %>%
      mutate_at( vars( Year,NEW_MODE,LBS ), list( ~ as.numeric(as.character(.)) ) ) %>%
      pivot_wider( names_from=NEW_MODE, values_from=LBS )
    
    AB1.lbs$Total <- rowSums( AB1.lbs[ ,which( colnames(AB1.lbs) %notin% c("Year") ) ], na.rm=TRUE )
    
    dummy.table = AB1.lbs
    rm( AB1.lbs )
    
    
    add.years <- data.frame( Year = 1981:params$term.year )
    dummy.table = dummy.table %>% bind_rows( add.years %>% filter( Year %notin% dummy.table$Year ) ) %>% arrange( Year )
    rm( add.years )
    
    ###   Replacing those <NA> values corresponding to zero catch estimates with a "0"...
    dummy.table[ is.na(dummy.table) ] <- 0
    ###   ...while also ensuring those records corresponding to 'no sampling' are properly left at "NA".
    ###     In the by-mode tables, this is needed for Hbt's to avoid overlap with SRHS estimates. In particular,
    ###     I want to include any Gulf HBTs 1981-1985 and any Mid/North-Atlantic HBTs 2004+...
    if( any( grepl( '2',colnames(dummy.table) ) ) & grepl( "Gulf of America", params$region ) ) {
      ###     ...which corresponds to either 'Gulf of Mexico' or 'Gulf of Mexico & South Atlantic' assessments...
      dummy.table[ which( dummy.table$Year > 1985 ), c("2") ] <- NA
    } else if( any( grepl( '2',colnames(dummy.table) ) ) &
               any( c("VA","MD","DE","PA","NJ","NY","CT","RI","MA","NH","ME") %in% params$subset.states ) &
               any( c("TX","LA","MS","AL","FLW") %notin% params$subset.states ) ) {
      dummy.table[ which( dummy.table$Year < 2004 ), c("2") ] <- NA
    } else if( any( grepl( '2',colnames(dummy.table) ) ) &
               any( c("VA","MD","DE","PA","NJ","NY","CT","RI","MA","NH","ME") %in% params$subset.states ) &
               any( c("TX","LA","MS","AL","FLW") %in% params$subset.states ) ) {
      dummy.table[ which( dummy.table$Year > 1985 & dummy.table$Year < 2004 ), c("2") ] <- NA
    }
    ###     Similarly, I also make sure any estimates for the combined Cbt/Hbt mode,
    ###     which are only considered in the Mid/North Atlantic from 1981-2003, are set to <NA> for 2004+ ...
    if( any( grepl( '5',colnames(dummy.table) ) ) ) {
      dummy.table[ ( dummy.table$Year > 2003 ) , ( grepl("5",colnames(dummy.table)) ) ] <- NA
    }
    
    
    ### SORTING COLUMNS ###
    ###     ...where, unlike the 'state' table, I don't sort based on some numeric code; state codes are
    ###       sorted geographically and represent the desired order whereas mode-codes are not
    ###       ( e.g., Cbt=3, Hbt=2, Priv=4, Shore=1 ). Instead, I convert the numeric codes to the mode names,
    ###       which are then manually sorted based on my desired order...
    colnames(dummy.table) <- str_replace( colnames(dummy.table), "1","Shore" )
    colnames(dummy.table) <- str_replace( colnames(dummy.table), "2","Hbt" )
    colnames(dummy.table) <- str_replace( colnames(dummy.table), "3","Cbt" )
    colnames(dummy.table) <- str_replace( colnames(dummy.table), "4","Priv" )
    colnames(dummy.table) <- str_replace( colnames(dummy.table), "5","CbtHbt" )
    
    col.order = colnames(dummy.table)[ !( colnames(dummy.table) %in% c("Year","Total") ) ]
    col.order = col.order[ order( match( col.order, c("Cbt","CbtHbt","Hbt","Priv","Priv/Shore","Shore") ) ) ]
    col.order <- c( "Year", col.order, "Total" )    ### ...and making sure "Year" is the first column...
    
    dummy.table <- dummy.table[ ,col.order[ which( col.order %in% colnames(dummy.table) ) ] ]
    
  }
  
  
  return.object = list( dummy.table, col.order )
  names(return.object) = c( 'catch.table','col.order' )
  rm( dummy.table, col.order )
  
  
  return( return.object )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------

FT.catlbs.cvs = function( catch.table, gwt.conv = NA, strata.add = NA, params ) {
  ###     ...where 'catch.table' is the table of catch & CV estimates being summarized at a year-strata level,
  ###           'gwt.conv' is the ratio applied to convert whole weight measurements (as observed in most surveys)
  ###               to gutted weight estimates (as required for some managed species),
  ###           'strata.add' identifies any additional strata to maintain while constructing the table,
  ###       and 'params' the R object that (amongst other things) identifies the strata (i.e., state or mode)
  ###               at which CV summaries are being requested in this assessment ( as defined in params$CV.tables.by )...
  
  
  dummy.table <- catch.table %>%
    mutate_at( vars( any_of( c('Year') ) ), list( ~ as.integer(as.character(.)) ) ) %>%
    mutate_at( vars( contains( c('LBS','CV') ) ), list( ~ as.numeric(as.character(.)) ) )
  
  if( is.na(strata.add) ) {
    dummy.table <- dummy.table %>% arrange( Year )
  } else {
    dummy.table <- dummy.table %>% arrange( across( any_of( c(strata.add,'Year') ) ) )
  }
  
  colnames(dummy.table) = gsub( "CV_LBS","CV", colnames(dummy.table) )
  
  
  if( params$wgt.metric == 'gutted weight' ) {
    dummy.table <- dummy.table %>% mutate_at( vars( contains("_LBS") ), list( ~ . / gwt.conv ) )
    ###   ...where only the 'LBS' estimates need to be updated as scaling a number by a constant
    ###     ( which is essentially what our WWT:GWT conversions are doing ) doesn't effect the 'CV' estimates...
  }
  
  
  dummy.table[ is.na(dummy.table) ] <- 0
  
  
  # if( params$CV.tables.by == "Area" ) {
  #   
  #   ### STATE-SPECIFIC CVs ###
  #   ### ----------------------
  #   if( FLK.only == TRUE ) {
  #     colnames(dummy.table) <- str_replace( colnames(dummy.table), "FLW","FLKeys" )
  #   }
  #   if( any( c("TX_LBS","TX_CV") %in% colnames(dummy.table) ) ) {
  #     if( sum( dummy.table[ which( dummy.table$Year %in% c(1981,1982) ),
  #                        which( colnames(dummy.table) %in% c("TX_LBS","TX_CV") ) ], na.rm=TRUE ) == 0 ) {
  #       dummy.table[ which( dummy.table$Year %in% c(1981,1982) ),
  #                 which( colnames(dummy.table) %in% c("TX_LBS","TX_CV") ) ] <- NA
  #     }
  #   }
  #   
  #   
  # } else if( params$CV.tables.by == "Mode" ) {
    
    ### MODE-SPECIFIC CVs ###
    ### ---------------------
    if( "Hbt" %in% params$subset.modes & grepl( "Gulf of America", params$region ) ) {
      dummy.table[ which( dummy.table$Year > 1985 ), c("Hbt_LBS","Hbt_CV") ] <- NA
    } else if( "Hbt" %in% params$subset.modes &
               any( c("VA","MD","DE","PA","NJ","NY","CT","RI","MA","NH","ME") %in% params$subset.states ) &
               any( c("TX","LA","MS","AL","FLW") %notin% params$subset.states ) ) {
      dummy.table[ which( dummy.table$Year < 2004 ), c("Hbt_LBS","Hbt_CV") ] <- NA
    } else if( "Hbt" %in% params$subset.modes &
               any( c("VA","MD","DE","PA","NJ","NY","CT","RI","MA","NH","ME") %in% params$subset.states ) &
               any( c("TX","LA","MS","AL","FLW") %in% params$subset.states ) ) {
      dummy.table[ which( dummy.table$Year > 1985 & dummy.table$Year < 2004 ), c("Hbt_LBS","Hbt_CV") ] <- NA
    }
    if( any( grepl( "CbtHbt_",colnames(dummy.table) ) ) ) {
      dummy.table[ ( dummy.table$Year > 2003 ) , c("CbtHbt_LBS","CbtHbt_CV") ] <- NA
    }
    
  # }
  
  
  ###   As a last check, I then drop any modes for which catch estimates aren't provided/needed (i.e., sum(col)= 0 ).
  ###   As an example, this check may be needed in assessments with SID domains, wherein combined 'CbtHbt' estimates
  ###   might be provided for one SID domain (i.e., any areas north of VA ) but not others (i.e., limited to GOM or SATL)...
  drop.strata = unique( gsub( "_.*","", names( which(
    colSums( dummy.table[ , gsub( ".*_","",colnames(dummy.table) ) %in% c('LBS') ], na.rm=TRUE ) == 0 ) ) ) )
  dummy.table = dummy.table[ , gsub( "_.*","",colnames(dummy.table) ) %notin% drop.strata ]
  rm( drop.strata )
  
  
  
  col.order <- vector( "character" )
  upper.hdr <- list()
  lower.hdr  <- list()
  
  col.order <- "Year"
  lower.hdr$Year <- "Year"
  upper.hdr$Year <- ""
  
  if( !is.na(strata.add) ) {
    col.order <- c( col.order, strata.add )
    lower.hdr[[strata.add]] <- strata.add
    upper.hdr[[strata.add]] <- ""
  }
  n.strata = length(col.order)
  
  
  col.vec = c("SID","Year")
  if( !is.na(strata.add) ) {  col.vec = c( col.vec,strata.add )  }
  
  strat <- unique( gsub( "_.*","", colnames(dummy.table)[ colnames(dummy.table) %notin% col.vec ] ) )
  rm( col.vec )
  
  strat <- unique( strat )
  
  # if( params$CV.tables.by == "Area" ) {
  #   strat = strat[ order( match( strat, c("TX","LA","MS","AL","FLW","FLKeys",
  #                                         "FLE","GA","SC","NC","SNC","NNC",
  #                                         "VA","MD","DE","NJ","NY","CT","RI","MA","NH","ME", "PR") ) ) ]
  # } else if( params$CV.tables.by == "Mode" ) {
    strat = strat[ order( match( strat, c("Cbt","CbtHbt","Hbt","Priv","Priv/Shore","Shore") ) ) ]
  # }
  
  for( i in 1:length(strat) ) {
    col.order[ (2*(i-1))+n.strata+1 ] <- paste0( strat[i],"_LBS" )
    col.order[ (2*(i-1))+n.strata+2 ] <- paste0( strat[i],"_CV" )
    
    eval( parse( text=paste0(
      "upper.hdr$",paste0( strat[i],"_LBS" ),"= '",strat[i],"'" ) ) )
    eval( parse( text=paste0(
      "upper.hdr$",paste0( strat[i],"_CV" ),"= '",strat[i],"'" ) ) )
    eval( parse( text=paste0(
      "lower.hdr$",paste0( strat[i],"_LBS" ),"= 'LBS'" ) ) )
    eval( parse( text=paste0(
      "lower.hdr$",paste0( strat[i],"_CV" ),"= 'CV'" ) ) )
  }
  rm( strat )
  
  
  dummy.table = dummy.table[ ,col.order ]
  
  
  return.object = list( dummy.table, col.order, upper.hdr, lower.hdr )
  names(return.object) = c( 'catch.table','col.order','upper.hdr','lower.hdr' )
  rm( dummy.table, col.order, upper.hdr, lower.hdr )
  
  
  return( return.object )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------


FT.size.yr.strata = function( size.table,
                              DS = c('MRIP','TPWD','LACR'),   metric = c('LEN','WGT'),
                              strata = c('state','mode','annual'),
                              strata.add = NA,    params ) {
  ###     ...where 'size.table' is the table of catch estimates being summarized at a year-strata level,
  ###              'DS' identifies the data source (i.e., survey) from which size data is being summarized,
  ###              'metric' identifies the type of size data being summarized (length or weight data)...
  ###              'strata' identifies the (state or mode) strata at which summaries are being generated,
  ###              'strata.add' identifies any additional strata to maintain while constructing the table,
  ###       and 'params' the R object that (amongst other things) identifies the filters applied in this SEDAR...
  
  
  ### FILTER BY DATA SOURCE ###
  ### -------------------------
  if( DS == 'MRIP' ) {
    subset.table <- size.table[ which( size.table$DS %in% c("MRIP","MRFSS") ), ]
  } else if( DS == 'TPWD' ) {
    subset.table <- size.table[ which( size.table$DS == "TPWD" ), ]
  } else if( DS == 'LACR' ) {
    subset.table <- size.table[ which( size.table$DS %in% c("LA Creel","LA BIO") ), ]
  }
  
  
  ### SUMMARIZE SIZE VARS ###
  ### -----------------------
  ###     ...the fields of which are specific to individual surveys...
  
  command.line = paste0( "subset.table = subset.table %>% group_by( across( any_of( c('Year'" )
  if( strata == 'state' ) {
    command.line = paste0( command.line, ", 'NEW_ST'" )
  } else if( strata == 'mode' ) {
    command.line = paste0( command.line, ", 'NEW_MODE'" )
  } else if( strata == 'annual' ) {
    ###   ...add nothing...
  }
  if( !is.na(strata.add) ) {
    command.line = paste0( command.line, ", '",strata.add,"'" )
  }
  command.line = paste0( command.line, " ) ) ) )" )
  
  eval( parse( text = command.line ) )
  rm( command.line )
  
  
  ### MRIP ###
  if( DS == 'MRIP' ) {
    if( metric == 'LEN' ) {
      subset.table = subset.table %>% filter( FL_mm > 0 & !is.na(FL_mm) )
        # filter( LNGTH > 0 & !is.na(LNGTH) )     ### ...which has been used in Caribbean assessments
      dummy.table <- subset.table %>%
        summarize( Fish = length( FL_mm[ !is.na(FL_mm) ] ),
                   Min = min( as.numeric(FL_mm), na.rm=TRUE ),
                   Avg = mean( as.numeric(FL_mm), na.rm=TRUE ),
                   SD  = sd( as.numeric(FL_mm), na.rm=TRUE ),
                   Max = max( as.numeric(FL_mm), na.rm=TRUE ) )
    } else if( metric == 'WGT' ) {
      subset.table = subset.table %>% filter( ALL_LBS > 0 & !is.na(ALL_LBS) )
        # filter( WGT > 0 & !is.na(WGT) )     ### ...which has been used in Caribbean assessments
      dummy.table <- subset.table %>%
        summarize( Fish = length( ALL_LBS[ !is.na(ALL_LBS) ] ),
                   Min = min( as.numeric(ALL_LBS), na.rm=TRUE ),
                   Avg = mean( as.numeric(ALL_LBS), na.rm=TRUE ),
                   SD = sd( as.numeric(ALL_LBS), na.rm=TRUE ),
                   Max = max( as.numeric(ALL_LBS), na.rm=TRUE ) )
    }
    ntrips = subset.table %>%
      distinct( ID_CODE, .keep_all = TRUE ) %>%
      summarize( Trp = length( ID_CODE[ !is.na(ID_CODE) ] ) )
    
  ### TPWD ###
  } else if( DS == 'TPWD' ) {
    if( metric == 'LEN' ) {
      subset.table = subset.table %>% filter( LENGTH > 0 & !is.na(LENGTH) )
      dummy.table <- subset.table %>%
        summarize( Fish = length( TL_mm[ !is.na(TL_mm) ] ),
                   Min = min( as.numeric(TL_mm), na.rm=TRUE ),
                   Avg = mean( as.numeric(TL_mm), na.rm=TRUE ),
                   SD  = sd( as.numeric(TL_mm), na.rm=TRUE ),
                   Max = max( as.numeric(TL_mm), na.rm=TRUE ) )
    } else if( metric == 'WGT' ) {
      ###   NOTHING -- TPWD doesn't collect weight' data...
    }
    ntrips = subset.table %>%
      distinct( TRIP_KEY, .keep_all = TRUE ) %>%
      summarize( Trp = length( TRIP_KEY[ !is.na(TRIP_KEY) ] ) )
    
  ### LACREEL ###
  } else if( DS == 'LACR' ) {
    if( metric == 'LEN' ) {
      subset.table = subset.table %>% filter( LENGTH_1 > 0 & !is.na(LENGTH_1) )
      dummy.table <- subset.table %>%
        filter( !is.na(FL_mm) ) %>%
        summarize( Fish = length( FL_mm[ !is.na(FL_mm) ] ),
                   Min = min( as.numeric(FL_mm), na.rm=TRUE ),
                   Avg = mean( as.numeric(FL_mm), na.rm=TRUE ),
                   SD  = sd( as.numeric(FL_mm), na.rm=TRUE ),
                   Max = max( as.numeric(FL_mm), na.rm=TRUE ) )
    } else if( metric == 'WGT' ) {
      subset.table = subset.table %>% filter( WHOLE_WEIGHT_KILO > 0 & !is.na(WHOLE_WEIGHT_KILO) )
      dummy.table <- subset.table %>%
        filter( !is.na(ALL_LBS) ) %>%
        summarize( Fish = length( ALL_LBS[ !is.na(ALL_LBS) ] ),
                   Min = min( as.numeric(ALL_LBS), na.rm=TRUE ),
                   Avg = mean( as.numeric(ALL_LBS), na.rm=TRUE ),
                   SD = sd( as.numeric(ALL_LBS), na.rm=TRUE ),
                   Max = max( as.numeric(ALL_LBS), na.rm=TRUE ) )
    }
    ntrips = subset.table %>%
      distinct( SUPPLIER_SAMPLE_ID, .keep_all = TRUE ) %>%
      summarize( Trp = length( SUPPLIER_SAMPLE_ID[ !is.na(SUPPLIER_SAMPLE_ID) ] ) )
    
  }
  
  dummy.table = dummy.table %>%
    mutate( Min = ifelse( is.infinite(Min), 0,Min ),
            Max = ifelse( is.infinite(Max), 0,Max ) )
  
  
  
  ###   I then join the trip counts (from 'ntrips') to 'dummy.table'...
  join.vec = c('Year')
  if( strata == 'state' ) {
    join.vec = c( join.vec,'NEW_ST' )
  } else if( strata == 'mode' ) {
    join.vec = c( join.vec,'NEW_MODE' )
  }
  if( 'SID' %in% colnames(dummy.table) ) {   join.vec = c( 'SID',join.vec )   }
  if( 'FED_CLOSED' %in% colnames(dummy.table) ) {   join.vec = c( join.vec,'FED_CLOSED' )   }
  
  dummy.table <- full_join( dummy.table, ntrips, by=join.vec )
  rm( ntrips, join.vec )
  
  
  if( strata == 'state' ) {
    dummy.table = dummy.table %>% pivot_wider( names_from=NEW_ST, values_from=c( Fish,Trp,Min,Avg,SD,Max ) )
  } else if( strata == 'mode' ) {
    dummy.table = dummy.table %>% pivot_wider( names_from=NEW_MODE, values_from=c( Fish,Trp,Min,Avg,SD,Max ) )
  } ### ...where no pivot is needed for tables providing 'annual' estimates...
  
  
  
  ### FORMATTING ###
  ### --------------
  
  ### Adding any 'missing' records/strata to 'dummy.table'...
  if( params$region == 'Caribbean' ) {   add.start = 2000
  } else if( DS == 'TPWD' ) {            add.start = 1983
  } else if( DS == 'LACR' ) {            add.start = 2014
  } else {                               add.start = 1981 }
  
  if( is.na(strata.add) ) {
    add.years <- data.frame( Year = add.start:params$term.year )
    dummy.table = dummy.table %>%
      bind_rows( add.years %>% filter( Year %notin% dummy.table$Year ) ) %>%
      arrange( Year )
    rm( add.years )
    
  } else {
    eval( parse( text = paste0( 'blah = expand.grid( Year = add.start:params$term.year,',
                                strata.add,' = unique(unlist(size.table[strata.add])) )' ) ) )
    eval( parse( text = paste0( "blah = blah[ paste0( blah$Year,'_',blah$",strata.add," ) %notin% ",
                                " paste0( dummy.table$Year,'_',dummy.table$",strata.add," ), ]" ) ) )
    dummy.table = dummy.table %>% bind_rows( blah ) %>% arrange( across( any_of( c(strata.add,'Year') ) ) )
    rm( blah )
  }
  rm( add.start )
  
  
  dummy.table[ is.na(dummy.table) ] <- 0
  
  
  ###   ...dropping states/modes within which size data wasn't collected...
  drop.strata = unique( gsub( ".*_","", names( which(
    colSums( dummy.table[ , gsub( "_.*","",colnames(dummy.table) ) %in% c('Fish','Trp') ], na.rm=TRUE ) == 0 ) ) ) )
  dummy.table = dummy.table[ , gsub( ".*_","",colnames(dummy.table) ) %notin% drop.strata ]
  rm( drop.strata )
  
  
  ###   Combining the 'Fish' and 'Trp' fields into a single column (i.e., N = Fish(Trp) ),
  ###   before which I change the format of these (numeric) fields to characters...
  dummy = dummy.table %>%
    ungroup() %>%
    select( contains(c('Fish','Trp')) ) %>%
    # mutate( across( where(is.numeric), round, 0 ) ) %>%
    # mutate( across( everything(), round, 0 ) ) %>%
    mutate_all( round, 0 ) %>%
    mutate_all( format, big.mark="," ) %>%
    mutate_all( as.character ) %>%
    mutate_all( trimws, which='both' )
  dummy[ dummy=='NA' ] = '0'
  dummy.table[ ,grep("Fish",colnames(dummy.table)) ] <- dummy[ ,grep("Fish",colnames(dummy)) ]
  dummy.table[ ,grep( "Trp",colnames(dummy.table)) ] <- dummy[ ,grep( "Trp",colnames(dummy)) ]
  rm(dummy)
  
  if( strata %in% c('state','mode') ) {
    
    col.vec = c("SID","Year")
    if( !is.na(strata.add) ) {  col.vec = c( col.vec,strata.add )  }
    strat <- unique( gsub( ".*_","", colnames(dummy.table)[ colnames(dummy.table) %notin% col.vec ] ) )
    rm( col.vec )
    
    if( strata == 'state' ) {
      strat <- sort( as.numeric( unique( strat ) ) )
    } else if( strata =='mode' ) {
      mode.order = c( 3,5,2,4,6,1 )
      strat = mode.order[ which( mode.order %in% strat ) ]
      rm( mode.order )
    }
    
    for( i in 1:length(strat) ) {
      dummy.table <- unite( dummy.table, newcol,
                            c( paste0("Fish_",strat[i]),paste0("Trp_",strat[i]) ), sep=" (", remove=TRUE )
      dummy.table$newcol <- paste0( dummy.table$newcol,")" )
      colnames(dummy.table)[ which( colnames(dummy.table) == "newcol" ) ] <- paste0("N_",strat[i])
    }
    
  } else if( strata == 'annual') {
    dummy.table <- unite( dummy.table, newcol, c( "Fish","Trp" ), sep=" (", remove=TRUE )
    dummy.table$newcol <- paste0( dummy.table$newcol,")" )
    colnames(dummy.table)[ which( colnames(dummy.table) == "newcol" ) ] <- "N"
  }
  
  
  ###   ...changing zero's to <NA> for any strata wherein sampling wasn't conducted (i.e., not 'real' zero's )...
  if( strata == 'state' ) {
    dummy.table[ which( dummy.table$Year %in% c(1981,1982) ),
                 which( gsub( ".*_","", colnames(dummy.table) ) %in% c("1") ) ] <- NA      ### Texas
    if( DS == 'MRIP' ) {
      dummy.table[ which( dummy.table$Year > 2013 ),
                   which( gsub( ".*_","", colnames(dummy.table) ) %in% c("2") ) ] <- NA    ### Louisiana
    } else if( DS == 'LACR' ) {
      dummy.table[ which( dummy.table$Year < 2014 ),
                   which( gsub( ".*_","", colnames(dummy.table) ) %in% c("2") ) ] <- NA    ### Louisiana
    }
    
  } else if( strata == 'mode' ) {
    if( "Hbt" %in% params$subset.modes & grepl( "Gulf of America", params$region ) ) {
      dummy.table[ which( dummy.table$Year > 1985 ),
                   which( gsub( ".*_","", colnames(dummy.table) ) %in% c("2") ) ] <- NA     ### Headboat
      
    } else if( "Hbt" %in% params$subset.modes & params$flag.forhire == 'No' &
               params$region %in% c("Atlantic","Mid Atlantic","North Atlantic") ) {
               # any( c("VA","MD","DE","PA","NJ","NY","CT","RI","MA","NH","ME") %in% params$subset.states ) &
               # any( c("TX","LA","MS","AL","FLW") %notin% params$subset.states ) ) {
        dummy.table[ which( dummy.table$Year < 2004 ),
                     which( gsub( ".*_","", colnames(dummy.table) ) %in% c("2") ) ] <- NA   ### Headboat
        dummy.table[ which( dummy.table$Year > 2003 ),
                     which( gsub( ".*_","", colnames(dummy.table) ) %in% c("5") ) ] <- NA   ### Combined Cbt/Hbt
    } else if( "Hbt" %in% params$subset.modes &  params$flag.forhire == 'No' &
               params$region %in% c("Southeast") ) {
               # any( c("VA","MD","DE","PA","NJ","NY","CT","RI","MA","NH","ME") %in% params$subset.states ) &
               # any( c("TX","LA","MS","AL","FLW") %in% params$subset.states ) ) {
      dummy.table[ which( dummy.table$Year > 1985 & dummy.table$Year < 2004 ),
                   which( gsub( ".*_","", colnames(dummy.table) ) %in% c("2") ) ] <- NA   ### Headboat
      dummy.table[ which( dummy.table$Year > 2003 ),
                   which( gsub( ".*_","", colnames(dummy.table) ) %in% c("5") ) ] <- NA   ### Combined Cbt/Hbt
    }
  }
  
  
  ### SORTING COLUMNS ###
  ### -------------------
  
  col.order <- vector( "character" )
  upper.hdr <- list()
  lower.hdr  <- list()
  
  col.order <- "Year"
  lower.hdr$Year <- "Year"
  upper.hdr$Year <- ""
  
  if( !is.na(strata.add) ) {
    col.order <- c( col.order, strata.add )
    lower.hdr[[strata.add]] <- strata.add
    upper.hdr[[strata.add]] <- ""
  }
  n.strata = length(col.order)
  
  
  if( strata %in% c('state','mode') ) {
    
    for( i in 1:length(strat) ) {
      col.order[ (5*(i-1))+n.strata+1 ] <- paste0( "N_",strat[i] )
      col.order[ (5*(i-1))+n.strata+2 ] <- paste0( "Min_",strat[i] )
      col.order[ (5*(i-1))+n.strata+3 ] <- paste0( "Avg_",strat[i] )
      col.order[ (5*(i-1))+n.strata+4 ] <- paste0( "SD_",strat[i] )
      col.order[ (5*(i-1))+n.strata+5 ] <- paste0( "Max_",strat[i] )
      
      eval( parse( text=paste0(
        "upper.hdr$",paste0( "N_",strat[i] ),"= '",strat[i],"'" ) ) )
      eval( parse( text=paste0(
        "upper.hdr$",paste0( "Min_",strat[i] ),"= '",strat[i],"'" ) ) )
      eval( parse( text=paste0(
        "upper.hdr$",paste0( "Avg_",strat[i] ),"= '",strat[i],"'" ) ) )
      eval( parse( text=paste0(
        "upper.hdr$",paste0( "SD_",strat[i] ),"= '",strat[i],"'" ) ) )
      eval( parse( text=paste0(
        "upper.hdr$",paste0( "Max_",strat[i] ),"= '",strat[i],"'" ) ) )
      eval( parse( text=paste0(
        "lower.hdr$",paste0( "N_",strat[i] ),"= 'N'" ) ) )
      eval( parse( text=paste0(
        "lower.hdr$",paste0( "Min_",strat[i] ),"= 'Min'" ) ) )
      eval( parse( text=paste0(
        "lower.hdr$",paste0( "Avg_",strat[i] ),"= 'Avg'" ) ) )
      eval( parse( text=paste0(
        "lower.hdr$",paste0( "SD_",strat[i] ),"= 'SD'" ) ) )
      eval( parse( text=paste0(
        "lower.hdr$",paste0( "Max_",strat[i] ),"= 'Max'" ) ) )
    }
    rm( strat, i )
    
    
  } else if( strata == 'annual' ) {
    
    if( DS == 'MRIP' ) {
      if( metric == 'LEN' ) { strat = 'Length' } else if( metric == 'WGT' ) { strat = 'Weight' }
    } else if( DS %in% c('TPWD','LACR') ) {
      strat = 'Total'
    }
    
    col.vec = c("SID","Year")
    if( !is.na(strata.add) ) {  col.vec = c( col.vec,strata.add )  }
    
    colnames(dummy.table)[ which( colnames(dummy.table) %notin% col.vec ) ] = paste0(
      colnames(dummy.table)[ which( colnames(dummy.table) %notin% col.vec ) ],"_",strat )
    rm( col.vec )
    
    col.order = c( col.order, paste0('N_',strat), paste0('Min_',strat), paste0('Avg_',strat),
                              paste0('SD_',strat), paste0('Max_',strat) )
    eval( parse( text=paste0( "upper.hdr$N_",strat,"   = '",strat,"'" ) ) )
    eval( parse( text=paste0( "upper.hdr$Min_",strat," = '",strat,"'" ) ) )
    eval( parse( text=paste0( "upper.hdr$Avg_",strat," = '",strat,"'" ) ) )
    eval( parse( text=paste0( "upper.hdr$SD_",strat,"  = '",strat,"'" ) ) )
    eval( parse( text=paste0( "upper.hdr$Max_",strat," = '",strat,"'" ) ) )
    eval( parse( text=paste0( "lower.hdr$N_",strat,"   = 'N'" ) ) )
    eval( parse( text=paste0( "lower.hdr$Min_",strat," = 'Min'" ) ) )
    eval( parse( text=paste0( "lower.hdr$Avg_",strat," = 'Avg'" ) ) )
    eval( parse( text=paste0( "lower.hdr$SD_",strat,"  = 'SD'" ) ) )
    eval( parse( text=paste0( "lower.hdr$Max_",strat," = 'Max'" ) ) )
    rm( strat )
  }
  
  
  ###   ...I then redefine the upper header to replace the numeric state/mode codes with the associated
  ###     character codes (e.g., substitute "1" with "TX" for 'state' summaries )...
  if( strata == 'state' ) {
    upper.hdr[ which( upper.hdr=="9.1") ] <- "SNC"
    upper.hdr[ which( upper.hdr=="9.2") ] <- "NNC"
    upper.hdr[ which( upper.hdr=="10" ) ] <- "VA"
    upper.hdr[ which( upper.hdr=="11" ) ] <- "MD"
    upper.hdr[ which( upper.hdr=="12" ) ] <- "DE"
    upper.hdr[ which( upper.hdr=="13" ) ] <- "NJ"
    upper.hdr[ which( upper.hdr=="14" ) ] <- "NY"
    upper.hdr[ which( upper.hdr=="15" ) ] <- "CT"
    upper.hdr[ which( upper.hdr=="16" ) ] <- "RI"
    upper.hdr[ which( upper.hdr=="17" ) ] <- "MA"
    upper.hdr[ which( upper.hdr=="18" ) ] <- "NH"
    upper.hdr[ which( upper.hdr=="19" ) ] <- "ME"
    upper.hdr[ which( upper.hdr=="20" ) ] <- "PR"
    upper.hdr[ which( upper.hdr== "1" ) ] <- "TX"
    upper.hdr[ which( upper.hdr== "2" ) ] <- "LA"
    upper.hdr[ which( upper.hdr== "3" ) ] <- "MS"
    upper.hdr[ which( upper.hdr== "4" ) ] <- "AL"
    upper.hdr[ which( upper.hdr== "5" ) ] <- "FLW"
    upper.hdr[ which( upper.hdr== "6" ) ] <- "FLE"
    upper.hdr[ which( upper.hdr== "7" ) ] <- "GA"
    upper.hdr[ which( upper.hdr== "8" ) ] <- "SC"
    upper.hdr[ which( upper.hdr== "9" ) ] <- "NC"
    
    if( FLK.only == TRUE ) {
      upper.hdr[ which( upper.hdr=="FLW" ) ] <- "FLKeys"
    }
    
  } else if( strata == 'mode' ) {
    upper.hdr[ which( upper.hdr=="1" ) ] <- "Shore"
    upper.hdr[ which( upper.hdr=="2" ) ] <- "Hbt"
    upper.hdr[ which( upper.hdr=="3" ) ] <- "Cbt"
    upper.hdr[ which( upper.hdr=="4" ) ] <- "Priv"
    upper.hdr[ which( upper.hdr=="5" ) ] <- "CbtHbt"
    upper.hdr[ which( upper.hdr=="6" ) ] <- "PrivShore"
  }
  
  dummy.table = dummy.table[ ,col.order ]
  
  
  return.object = list( dummy.table, col.order, upper.hdr, lower.hdr )
  names(return.object) = c( 'size.table','col.order','upper.hdr','lower.hdr' )
  rm( dummy.table, col.order, upper.hdr, lower.hdr )
  
  
  return( return.object )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------

FT.size.cvs = function( size.table, gwt.conv = NA, strata.add = NA, params ) {
  ###     ...where 'size.table' is the table of SEFSC avgwgts being summarized at a year-strata level,
  ###           'gwt.conv' is the ratio applied to convert whole weight measurements (as observed in most surveys)
  ###               to gutted weight estimates (as required for some managed species),
  ###           'strata.add' identifies any additional strata to maintain while constructing the table,
  ###       and 'params' the R object that (amongst other things) identifies the strata (i.e., state or mode)
  ###               at which CV summaries are being requested in this assessment ( as defined in params$CV.tables.by )...
  
  
  dummy.table <- size.table %>%
    mutate_at( vars( contains("_WGT") ), list( ~ as.numeric(.) ) ) %>%
    mutate_at( vars( contains("_CV" ) ), list( ~ as.numeric(.) ) )
  
  if( is.na(strata.add) ) {
    dummy.table <- dummy.table %>% arrange( Year )
  } else {
    dummy.table <- dummy.table %>% arrange( across( any_of( c(strata.add,'Year') ) ) )
  }
  
  if( params$wgt.metric == 'gutted weight' ) {
    dummy.table <- dummy.table %>% mutate_at( vars( contains("_WGT") ), list( ~ . / gwt.conv ) )
    ###   ...where only the average 'WGT' estimates need to be updated as scaling a number by a constant
    ###     ( which is essentially what our WWT:GWT conversions are doing ) doesn't effect the 'CV' estimates...
  }
  
  ### I also check to see if I need to change "FLW" --> "FLKeys"...
  if( FLK.only == TRUE ) {
    colnames(dummy.table) <- str_replace( colnames(dummy.table), "FLW","FLKeys" )
  }   ### ...where if 'dummy.table' is summarized by mode, then the above code does nothing (no "FLW" match)...
  
  
  
  col.vec = c("SID","Year")
  if( !is.na(strata.add) ) {  col.vec = c( col.vec,strata.add )  }
  strat <- unique( gsub( "_.*","", colnames(dummy.table)[ colnames(dummy.table) %notin% col.vec ] ) )
  strat <- unique( strat )
  
  
  ### I then replace all NA's that correspond to zero catch estimates to "0" in the table...
  dummy1 = dummy.table %>% select( !contains('_N') )        ### ...applying formatting to only catch & CV columns...
  dummy1[ is.na(dummy1) ] <- 0
  dummy2 = dummy.table %>% select( contains( c(col.vec,'_N') ) )
  
  col.vec = col.vec[ col.vec %in% colnames(dummy.table) ]
  dummy.table = full_join( dummy1, dummy2, by=col.vec )
  rm( dummy1,dummy2 )
  
  ###     ...and ensure those records corresponding to 'no sampling' are properly left as "NA"...
  if( params$CV.tables.by == "Mode" ) {
    if( "Hbt" %in% params$subset.modes & grepl( "Gulf of America", params$region ) ) {
      dummy.table[ which( dummy.table$Year > 1985 ), c("Hbt_WGT","Hbt_CV","Hbt_N") ] <- NA
    } else if( "Hbt" %in% params$subset.modes &
               any( c("VA","MD","DE","PA","NJ","NY","CT","RI","MA","NH","ME") %in% params$subset.states ) &
               any( c("TX","LA","MS","AL","FLW") %notin% params$subset.states ) ) {
      dummy.table[ which( dummy.table$Year < 2004 ), c("Hbt_WGT","Hbt_CV","Hbt_N") ] <- NA
    } else if( "Hbt" %in% params$subset.modes &
               any( c("VA","MD","DE","PA","NJ","NY","CT","RI","MA","NH","ME") %in% params$subset.states ) &
               any( c("TX","LA","MS","AL","FLW") %in% params$subset.states ) ) {
      dummy.table[ which( dummy.table$Year > 1985 & dummy.table$Year < 2004 ), c("Hbt_WGT","Hbt_CV","Hbt_N") ] <- NA
    }
    if( "CbtHbt" %in% strat ) {
      dummy.table[ ( dummy.table$Year > 2003 ) , c("CbtHbt_WGT","CbtHbt_CV","CbtHbt_N") ] <- NA
    }
    
  } else if( params$CV.tables.by == "Area" ) {
    if( any( c("TX_WGT","TX_CV","TX_N") %in% colnames(dummy.table) ) ) {
      if( sum( dummy.table[ which( dummy.table$Year %in% c(1981,1982) ),
                        which( colnames(dummy.table) %in% c("TX_WGT","TX_CV","TX_N") ) ], na.rm=TRUE ) == 0 ) {
        dummy.table[ which( dummy.table$Year %in% c(1981,1982) ),
                 which( colnames(dummy.table) %in% c("TX_WGT","TX_CV","TX_N") ) ] <- NA
      }
    }
  }
  
  ###     ...and lastly, I then set the 'WGT' & 'CV' fields to <NA> for which the corresponding 'N' field is <NA>...
  modes = unique( gsub( '_.*','', colnames(dummy.table)[ colnames(dummy.table) %notin% col.vec ] ) )
  for( i in 1:length(modes) ) {
    
    rows = which( is.na( dummy.table[ ,paste0(modes[i],'_N') ] ) )
    dummy.table[ rows, paste0(modes[i],'_WGT') ] = NA
    dummy.table[ rows, paste0(modes[i],'_CV' ) ] = NA
    rm(rows)
  }
  rm( modes )
  
  
  ### I then initialize objects to store the order in which I want my headers displayed in this flextable...
  col.order <- vector( "character" )
  upper.hdr <- list()
  lower.hdr  <- list()
  
  col.order <- "Year"
  lower.hdr$Year <- "Year"
  upper.hdr$Year <- ""
  
  if( !is.na(strata.add) ) {
    col.order <- c( col.order, strata.add )
    lower.hdr[[strata.add]] <- strata.add
    upper.hdr[[strata.add]] <- ""
  }
  n.strata = length(col.order)
  
  
  for( i in 1:length(strat) ) {
    col.order[ (3*(i-1))+n.strata+1 ] <- paste0( strat[i],"_WGT" )
    col.order[ (3*(i-1))+n.strata+2 ] <- paste0( strat[i],"_CV" )
    col.order[ (3*(i-1))+n.strata+3 ] <- paste0( strat[i],"_N" )
    
    eval( parse( text=paste0(
      "upper.hdr$",paste0( strat[i],"_WGT" ),"= '",strat[i],"'" ) ) )
    eval( parse( text=paste0(
      "upper.hdr$",paste0( strat[i],"_CV" ),"= '",strat[i],"'" ) ) )
    eval( parse( text=paste0(
      "upper.hdr$",paste0( strat[i],"_N"  ),"= '",strat[i],"'" ) ) )
    eval( parse( text=paste0(
      "lower.hdr$",paste0( strat[i],"_WGT" ),"= 'WGT'" ) ) )
    eval( parse( text=paste0(
      "lower.hdr$",paste0( strat[i],"_CV" ),"= 'CV'" ) ) )
    eval( parse( text=paste0(
      "lower.hdr$",paste0( strat[i],"_N"  ),"= 'N'" ) ) )
  }
  rm( strat )
  
  
  
  dummy.table = dummy.table[ ,col.order ]
  
  
  return.object = list( dummy.table, col.order, upper.hdr, lower.hdr )
  names(return.object) = c( 'size.table','col.order','upper.hdr','lower.hdr' )
  rm( dummy.table, col.order, upper.hdr, lower.hdr )
  
  
  return( return.object )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------

FT.size.hier = function( catch.table, size.table, params ) {
  ###     ...where 'catch.table' is the table of catch estimates (i.e., landings-in-weight ) being summarized
  ###               by the (SEFSC avgwgt estimation) hierarchy level at which they were estimated,
  ###           'size.table' is the table of raw size measurements used to calculate sample sizes,
  ###       and 'params' the R object that (amongst other things) identifies the strata (i.e., state or mode)
  ###               at which CV summaries are being requested in this assessment ( as defined in params$CV.tables.by )...
  
  
  ###  I start by subsetting "catch.table" to only include records with a landings estimate...
  dummy.table = catch.table[ which( catch.table$AB1 > 0 ), ]
  # summary( as.factor( dummy.table$lbsest_SECsource ) )
  
  
  if( params$wgt.metric == "whole weight" ) {
    
    if( "lbsest_SECwwt" %in% colnames(catch.table) ) {
      dummy.table = dummy.table %>%
        group_by( Year, lbsest_SECsource ) %>%
        summarise( AB1.lbs = sum( lbsest_SECwwt, na.rm=TRUE ) ) %>%
        #   N = length( lbsest_SECwwt[ !is.na(lbsest_SECwwt) ] ),
        # AB1 = sum( AB1, na.rm=TRUE ) ) %>%
        # mutate( AvgWgt = AB1.lbs / AB1 ) %>%
        select( Year, lbsest_SECsource, AB1.lbs )
    } else {
      dummy.table = dummy.table %>%
        group_by( Year, lbsest_SECsource ) %>%
        summarise( AB1.lbs = sum( lbsest_SEC, na.rm=TRUE ) ) %>%
        select( Year, lbsest_SECsource, AB1.lbs )
    }
    
  } else if( params$wgt.metric == "gutted weight" ) {
    
    if( "lbsest_SECgwt" %in% colnames(catch.table) ) {
      dummy.table = dummy.table %>%
        group_by( Year, lbsest_SECsource ) %>%
        summarise( AB1.lbs = sum( lbsest_SECgwt, na.rm=TRUE ) ) %>%
        select( Year, lbsest_SECsource, AB1.lbs )
    } else {
      dummy.table = dummy.table %>%
        group_by( Year, lbsest_SECsource ) %>%
        summarise( AB1.lbs = sum( lbsest_SEC, na.rm=TRUE ) ) %>%
        select( Year, lbsest_SECsource, AB1.lbs )
    }
  }
  
  
  #############################################################################################################
  ###   Note that the data compiled for this table is also used to construct a figure, and so I save
  ###     this (long-version) table to avoid the need for duplicating code...
  dummy.long = dummy.table
  #############################################################################################################
  
  
  dummy.table = dummy.table %>%
    pivot_wider( names_from=lbsest_SECsource, values_from="AB1.lbs" )
    # pivot_wider( names_from=lbsest_SECsource, values_from=c("AvgWgt","N","AB1.lbs") )
  colnames(dummy.table)[ which( colnames(dummy.table)!="Year") ] = paste0(
                         "AB1.lbs_",colnames(dummy.table)[ which( colnames(dummy.table)!="Year") ] )
  
  
  ###       ...to which I also add the corresponding sample size info (i.e., number of FISH & TRIPS )
  ###           that provided the weight information being summarized in this table...
  
  blah = size.table %>% group_by( Year )
  
  n.fish = blah %>% summarize( Fish = length( ALL_LBS[ !is.na(ALL_LBS) ] ) )
  n.trip = blah %>%
    filter( ALL_LBS > 0 & !is.na(ALL_LBS) ) %>%
    distinct( SAMPLING_UNIT_ID, .keep_all = TRUE ) %>%
    summarize( Trp = length( SAMPLING_UNIT_ID[ !is.na(SAMPLING_UNIT_ID) ] ) )
  
  dummy.table = dummy.table %>%
    full_join( n.fish, by="Year" ) %>%
    full_join( n.trip, by="Year" )
  rm( blah, n.fish, n.trip )
  
  
  dummy = dummy.table %>%
    ungroup() %>%
    select( contains(c('Fish','Trp')) ) %>%
    # mutate( across( where(is.numeric), round, 0 ) ) %>%
    # mutate( across( everything(), round, 0 ) ) %>%
    mutate_all( round, 0 ) %>%
    mutate_all( format, big.mark="," ) %>%
    mutate_all( as.character ) %>%
    mutate_all( trimws, which='both' )
  dummy[ dummy=='NA' ] = '0'
  dummy.table[ ,grep("Fish",colnames(dummy.table)) ] <- dummy[ ,grep("Fish",colnames(dummy)) ]
  dummy.table[ ,grep( "Trp",colnames(dummy.table)) ] <- dummy[ ,grep( "Trp",colnames(dummy)) ]
  rm(dummy)
  
  dummy.table <- unite( dummy.table, newcol, c( "Fish","Trp" ), sep=" (", remove=TRUE )
  dummy.table$newcol <- paste0( dummy.table$newcol,")" )
  colnames(dummy.table)[ which( colnames(dummy.table) == "newcol" ) ] <- "N"
  
  
  dummy.table[ is.na(dummy.table) ] = 0
  
  strat <- gsub( ".*_","", colnames(dummy.table) )
  strat <- strat[ which( strat %notin% c("Year","N") ) ]
  strat <- unique( strat )
  
  lvl.order = c( "s","sr","sry","srys","srysm","srysmw","srysmwa" )
  strat = lvl.order[ which( lvl.order %in% strat ) ]
  rm( lvl.order )
  
  
  col.order <- vector( "character" )
  for( i in 1:length(strat) ) {  col.order[i] <- paste0( "AB1.lbs_",strat[i] )  }
  col.order <- c( "Year","N", col.order )
  
  upper.hdr <- list()
  upper.hdr$Year = ""
  upper.hdr$N = ""
  
  lower.hdr  <- list()
  lower.hdr$Year = "Year"
  lower.hdr$N = "N"
  
  hdr.combos = paste( "AB1.lbs_", strat, sep="" )
  for( i in 1:length(hdr.combos) ) {
    eval( parse( text=paste0(
      "upper.hdr$",hdr.combos[i],"= '",gsub( "_.*","",hdr.combos[i] ),"'" ) ) )
    eval( parse( text=paste0(
      "lower.hdr$",hdr.combos[i],"= '",gsub( ".*_","",hdr.combos[i] ),"'" ) ) )
  }
  rm( strat, hdr.combos )
  
  
  
  dummy.table = dummy.table[ ,col.order ]
  
  
  return.object = list( dummy.table, dummy.long, col.order, upper.hdr, lower.hdr )
  names(return.object) = c( 'size.table','size.long','col.order','upper.hdr','lower.hdr' )
  rm( dummy.table, dummy.long, col.order, upper.hdr, lower.hdr )
  
  
  return( return.object )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------


FT.effort.yr.strata = function( mrip.table, tpwd.table = NA, lacr.table = NA,
                                strata = c('state','mode'), params ) {
  ###     ...where 'mrip.table' is the table of effort estimates provided by the MRIP survey,
  ###              'tpwd.table' is the table of effort estimates provided by the TPWD survey,
  ###              'lacr.table' is the table of effort estimates provided by the LACR survey,
  ###              'strata' identifies the (state or mode) strata at which summaries are being generated,
  ###       and 'params' the R object that (amongst other things) identifies the filters applied in this SEDAR...
  
  
  dummy.table = as.data.frame( mrip.table )
  dummy.table = dummy.table %>%
    mutate( NEW_ST   = as.character(NEW_ST),
            NEW_MODE = as.character(NEW_MODE) ) %>%
    mutate( EFFORT = as.numeric(ESTRIPS) )
  
  if( is.data.frame(tpwd.table) ) {
    dummy.tpwd = tpwd.table %>%
      mutate( EFFORT = as.numeric(NTRP) ) %>%
      mutate( NEW_ST = '1', NEW_STA = 'TX' ) %>%
      mutate( NEW_MODE  = ifelse( ACTIVITY %in% c(1,3), '4',        ifelse( ACTIVITY == 2, '3', NA )),
              NEW_MODEN = ifelse( ACTIVITY %in% c(1,3), "Priv",     ifelse( ACTIVITY == 2, "Cbt", NA )) )
    dummy.table = bind_rows( dummy.table, dummy.tpwd )
    rm(dummy.tpwd)
  }
  if( is.data.frame(lacr.table) ) {
    dummy.lacr = lacr.table %>%
      mutate( EFFORT = as.numeric(EXPANDED_EFFORT) ) %>%
      mutate( NEW_ST = '2', NEW_STA = 'LA' ) %>%
      mutate( NEW_MODE  = ifelse( MODES %in% c("Private Inshore","Private Offshore","Private","Shore"), '4',
                          ifelse( MODES == "Charter", '3', NA )),
              NEW_MODEN = ifelse( MODES %in% c("Private Inshore","Private Offshore","Private","Shore"), "Priv",
                          ifelse( MODES == "Charter", "Cbt", NA )) )
    dummy.table = bind_rows( dummy.table, dummy.lacr )
    rm(dummy.lacr)
  }
  
  
  if( strata == 'state' ) {
    dummy.table = dummy.table %>%
      group_by( Year, NEW_ST ) %>%
      summarize( EFFORT = sum( as.numeric(EFFORT), na.rm=TRUE ) ) %>%
      select( Year, NEW_ST, EFFORT ) %>%
      mutate_at( vars( Year,NEW_ST,EFFORT ), list( ~ as.numeric(as.character(.)) ) ) %>%
      pivot_wider( names_from=NEW_ST, values_from=EFFORT )
    
    ###   ...and sorting "dummy.table" geographically...
    dummy.table <- dummy.table[ ,c( "Year", sort( as.numeric( colnames(dummy.table)[ !(colnames(dummy.table)=="Year") ] ) ) ) ]
    
    colnames(dummy.table)[ which( colnames(dummy.table)=="9.1") ] <- "SNC"
    colnames(dummy.table)[ which( colnames(dummy.table)=="9.2") ] <- "NNC"
    colnames(dummy.table)[ which( colnames(dummy.table)=="10" ) ] <- "VA"
    colnames(dummy.table)[ which( colnames(dummy.table)=="11" ) ] <- "MD"
    colnames(dummy.table)[ which( colnames(dummy.table)=="12" ) ] <- "DE"
    colnames(dummy.table)[ which( colnames(dummy.table)=="13" ) ] <- "NJ"
    colnames(dummy.table)[ which( colnames(dummy.table)=="14" ) ] <- "NY"
    colnames(dummy.table)[ which( colnames(dummy.table)=="15" ) ] <- "CT"
    colnames(dummy.table)[ which( colnames(dummy.table)=="16" ) ] <- "RI"
    colnames(dummy.table)[ which( colnames(dummy.table)=="17" ) ] <- "MA"
    colnames(dummy.table)[ which( colnames(dummy.table)=="18" ) ] <- "NH"
    colnames(dummy.table)[ which( colnames(dummy.table)=="19" ) ] <- "ME"
    colnames(dummy.table)[ which( colnames(dummy.table)=="20" ) ] <- "PR"
    colnames(dummy.table)[ which( colnames(dummy.table)=="1" ) ] <- "TX"
    colnames(dummy.table)[ which( colnames(dummy.table)=="2" ) ] <- "LA"
    colnames(dummy.table)[ which( colnames(dummy.table)=="3" ) ] <- "MS"
    colnames(dummy.table)[ which( colnames(dummy.table)=="4" ) ] <- "AL"
    colnames(dummy.table)[ which( colnames(dummy.table)=="5" ) ] <- "FLW"
    colnames(dummy.table)[ which( colnames(dummy.table)=="6" ) ] <- "FLE"
    colnames(dummy.table)[ which( colnames(dummy.table)=="7" ) ] <- "GA"
    colnames(dummy.table)[ which( colnames(dummy.table)=="8" ) ] <- "SC"
    colnames(dummy.table)[ which( colnames(dummy.table)=="9" ) ] <- "NC"
    
    if( FLK.only == TRUE ) {
      colnames(dummy.table) <- str_replace( colnames(dummy.table), "FLW","FLKeys" )
    }
    
    
  } else if( strata == 'mode' ) {
    dummy.table = dummy.table %>%
      group_by( Year, NEW_MODE ) %>%
      summarize( EFFORT = sum( as.numeric(EFFORT), na.rm=TRUE ) ) %>%
      select( Year, NEW_MODE, EFFORT ) %>%
      mutate_at( vars( Year,NEW_MODE,EFFORT ), list( ~ as.numeric(as.character(.)) ) ) %>%
      pivot_wider( names_from=NEW_MODE, values_from=EFFORT )
    
    mode.order = c( 3,5,2,4,6,1 )
    mode.order = mode.order[ which( mode.order %in% colnames(dummy.table)[ !(colnames(dummy.table)=="Year") ] ) ]
    dummy.table <- dummy.table[ ,c( "Year", mode.order ) ]
    rm( mode.order )
    
    colnames(dummy.table)[ which( colnames(dummy.table)=="1" ) ] <- "Shore"
    colnames(dummy.table)[ which( colnames(dummy.table)=="2" ) ] <- "Hbt"
    colnames(dummy.table)[ which( colnames(dummy.table)=="3" ) ] <- "Cbt"
    colnames(dummy.table)[ which( colnames(dummy.table)=="4" ) ] <- "Priv"
    colnames(dummy.table)[ which( colnames(dummy.table)=="5" ) ] <- "CbtHbt"
    colnames(dummy.table)[ which( colnames(dummy.table)=="6" ) ] <- "PrivShore"
  }
  
  
  dummy.table <- round( dummy.table, digits=0 )
  
  
  ### For final touches...
  ###       ...I remove any years from the effort table that extend beyond params$term.year...
  dummy.table <- dummy.table[ which( dummy.table$Year <= params$term.year ), ]
  ###       ...convert all cells with <NA> to zero values...
  dummy.table[ is.na(dummy.table) ] <- 0
  
  if( strata == 'state' ) {
    ###   ...and reset "TX" 1981-1982 back to <NA> if there were no imputations for these years...
    if( "TX" %in% colnames(dummy.table) ) {
      if( sum( dummy.table[ which( dummy.table$Year %in% c(1981,1982) ),
                            which( colnames(dummy.table) %in% c("TX") ) ], na.rm=TRUE ) == 0 ) {
        dummy.table[ which( dummy.table$Year %in% c(1981,1982) ), which( colnames(dummy.table) %in% c("TX") ) ] <- NA
      }
    }
    
  } else if( strata == 'mode' ) {
    ###       ...but then "back-fill" certain cells with <NA> values (i.e., no MRIP sampling )...
    if( "Hbt" %in% colnames(dummy.table) & grepl( "Gulf of America", params$region ) ) {
      dummy.table[ which( dummy.table$Year > 1985 ), c("Hbt") ] <- NA
    } else if( "Hbt" %in% colnames(dummy.table) &
               any( c("VA","MD","DE","PA","NJ","NY","CT","RI","MA","NH","ME") %in% params$subset.states ) &
               any( c("TX","LA","MS","AL","FLW") %notin% params$subset.states ) ) {
      dummy.table[ which( dummy.table$Year < 2004 ), c("Hbt") ] <- NA
    } else if( "Hbt" %in% colnames(dummy.table) &
               any( c("VA","MD","DE","PA","NJ","NY","CT","RI","MA","NH","ME") %in% params$subset.states ) &
               any( c("TX","LA","MS","AL","FLW") %in% params$subset.states ) ) {
      dummy.table[ which( dummy.table$Year > 1985 & dummy.table$Year < 2004 ), c("Hbt") ] <- NA
    }
    if( "CbtHbt" %in% colnames(dummy.table) ) {
      dummy.table[ which( dummy.table$Year >= 2004 ), c("CbtHbt") ] <- NA
    }
  }
  ###   ... and finally add a "Total" (annual) field by summing across all columns...
  dummy.table$Total <- rowSums( dummy.table[ , which( colnames(dummy.table) != "Year" ) ], na.rm=TRUE )
  
  
  return.object = list( dummy.table )
  names(return.object) = c( 'effort.table' )
  rm( dummy.table )
  
  
  return( return.object )
  
}


### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------





