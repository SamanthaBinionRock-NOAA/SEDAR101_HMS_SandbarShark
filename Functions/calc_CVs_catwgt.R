

### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------
###
###     DESCRIPTION
###
###   ...where the functions below estimate uncertainties for the landings-in-weight estimates of
###       the (combined estimates from our) general recreational surveys...
###
###
###    *** CVs.landwgt( )
###           ...which combines the CV estimates for landings-in-number with those for average (fish) weight...
###             As described in S74-DW-12, there are two approaches by which this can be done, but only the
###             second approach has (thus far) been applied in a SEDAR stock assessment...
###
###
###   Note that these functions are set-up to handle stocks with multiple StockID boundaries, in that the script
###   checks if there are multiple elements in the imported CV object (one element for each SID domain)...
###
### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------

CVs.landwgt = function( approach = c(1,2),
                        avgwgt.dir,
                        num.table, wgt.table, catch.table ) {
  
  ###     ...where 'approach' identifies the method by which landings-in-weight CVs are to be calculated (S74-DW-12):
  ###                   (1) extension of approach used to calculate uncertainties for catch-in-number estimates,
  ###                       with SEFSC average weight estimates treated as constants (no uncertainty at the observation level)
  ###                   (2) uses variability in raw size data as a proxy for uncertainty of SEFSC average weight estimates,
  ###                       which are then combined with uncertainties in catch-in-number using the variance product law
  ###                       (Goodman 1960) to provide uncertainty estimates for landings-in-weight,
  ###
  ###         In regards to those inputs required for each of these methods...
  ###
  ###         -- APPROACH #1 --
  ###              'avgwgt.dir' identifies the folder/directory containing the latest SEFSC avgwgt estimates,
  ###                   which are joined with the raw catch observations when recalculating catch variability
  ###                   ( from the raw intercept data )...
  ###
  ###         -- APPROACH #2 --
  ###              'num.table' identifies the table(s) of estimates and associated CVs for catch-in-number,
  ###              'wgt.table' identifies the table(s) of estimates and associated CVs for average (fish) weight, and
  ###              'catch.table' is the table of catch estimates being compiled in the main script, from which the actual
  ###                     SEFSC average weights can be calculated ( those in 'wgt.table' are calculated from raw size data,
  ###                     and may not be the same as those in 'catch.table' which account for sampling weights )...
  
  
  
  ### APPROACH #1 ###
  ### ---------------
  ###       ...where SEFSC average weight estimates are treated as constants (with no uncertainty)
  
  if( approach == 1 ) {
    
    
    # ###     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *     ###
    # ###     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *     ###
    # ###                                                                                                     ###
    # ###           Based on discussions during the S74 (Gulf red snapper) data workshop, Approach #1         ###
    # ###           is not used in favor of Approach #2, which doesn't treat SEFSC avgwgt estimates as        ###
    # ###           constants. The code for Approach #1 is retained (below) in case a comparison is           ###
    # ###           requested at some point in the future, but the estimates from this approach are           ###
    # ###           largely considered inferior to those from Approach #2...                                  ###
    # ###                                                                                                     ###
    # ###     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *     ###
    # ###     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *     ###
    # 
    # 
    # 
    # ###   The code for Approach #1 was developed by modifying Kyle's original script, for which the CV calculations
    # ###       were done using the R survey package ( see my "CVs_Catch_from ACL" script for all notes/comments )...
    # 
    # library(survey)
    # 
    # 
    # 
    # ### Defining appropriate filters in MRIP 'currency'...
    # ###       For example, constructing a vector of (FIPS) state codes and MRIP (MODE_FX) fleet codes...
    # 
    # states.cv = states
    # states.cv = gsub( "TX", 48, states.cv )
    # states.cv = gsub( "LA", 22, states.cv )
    # states.cv = gsub( "MS", 28, states.cv )
    # states.cv = gsub( "AL",  1, states.cv )
    # states.cv = gsub( "FLW",12, states.cv )
    # states.cv = gsub( "FLE",12, states.cv )
    # states.cv = gsub( "GA", 13, states.cv )
    # states.cv = gsub( "SC", 45, states.cv )
    # states.cv = gsub( "NC", 37, states.cv )
    # states.cv = gsub( "VA", 51, states.cv )
    # states.cv = gsub( "MD", 24, states.cv )
    # states.cv = gsub( "DE", 10, states.cv )
    # states.cv = gsub( "PA", 42, states.cv )
    # states.cv = gsub( "NJ", 34, states.cv )
    # states.cv = gsub( "NY", 36, states.cv )
    # states.cv = gsub( "CT",  9, states.cv )
    # states.cv = gsub( "RI", 44, states.cv )
    # states.cv = gsub( "MA", 25, states.cv )
    # states.cv = gsub( "NH", 33, states.cv )
    # states.cv = gsub( "ME", 23, states.cv )
    # 
    # modes.cv = mode_sub
    # modes.cv = gsub( "Priv",  7, modes.cv )
    # modes.cv = gsub( "Hbt",   4, modes.cv )
    # modes.cv = gsub( "Cbt",   5, modes.cv )
    # modes.cv = gsub( "Shore", 3, modes.cv )
    # if( ( ( "Cbt" %in% mode_sub ) | ( "Hbt" %in% mode_sub ) ) &
    #     ( "Gulf of Mexico" %in% region | any( c("VA","MD","DE","PA","NJ","NY","CT","RI","MA","NH","ME") %in% states ) ) ) {
    #   modes.cv = c( modes.cv, 6 )         ### ...which is the combined for-hire mode (i.e., "Cbt/Hbt" )...
    # }
    # 
    # 
    # 
    # ### IMPORT ###
    # ### ----------
    # 
    # con = dbConnect(dbDriver("Oracle"), username = keyring::key_list("SECPR")[1,2],
    #                 password = keyring::key_get("SECPR", keyring::key_list("SECPR")[1,2]), dbname = "SECPR")
    # 
    # mrip.catchtrip = dbGetQuery(con,
    #                             paste0("select c.COMMON, c.SP_CODE, t.ID_CODE, t.STRAT_ID, t.VAR_ID, t.PSU_ID,
    #                         t.SUB_REG, t.YEAR, t.ST, t.MODE_FX, t.WAVE, t.AREA_X, t.CNTY,
    #                         NVL(c.CLAIM,0) CLAIM, NVL(c.HARVEST,0) HARVEST, NVL(c.RELEASE,0) RELEASE, NVL(c.WP_CATCH,0) WP_CATCH,
    #                         NVL(c.WGT_A,0) WGT_AB1C, NVL(c.WGT_B1,0) WGT_AB1H,
    #                         c.IMP_REC
    #                           from RDI.MRIP_ST_PUB_CATCH_CAL@secapxdv_dblk.sfsc.noaa.gov c
    #                           LEFT JOIN RDI.MRIP_ST_PUB_TRIP_CAL@secapxdv_dblk.sfsc.noaa.gov t
    #                                   ON t.ID_CODE = c.ID_CODE AND t.YEAR = c.YEAR and c.COMMON IS NOT NULL
    #                                where t.ST IN (",      sprintf("'%s'", paste(states.cv, collapse = "','")),")",
    #                                    # " AND t.MODE_FX IN (", sprintf("'%s'", paste(modes.cv, collapse = "','")),")",
    #                                    " AND t.YEAR BETWEEN ",first.year," AND ",term.year,
    #                                    " AND t.CNTY IS NOT NULL"
    #                             ))
    # 
    # ###     ...where I apply my (year & state) filters during the initial pull to reduce the size of "mrip.catchtrip".
    # ###         Note that the FL_REG & NC_REG filters cannot be applied here as the raw MRIP tables do not include the
    # ###         SEFSC value-added fields. Additionally, I don't apply the MODE_FX filter (at the import step) in case
    # ###         I want to calculate CVs for other modes later on ( without having to reimport "mrip.catchtrip" ).
    # ###         Instead, these additional (spatial and mode) filters are applied outside the initial pull ( see below )...
    # 
    # 
    # taxa.cv = mrip.catchtrip %>% distinct(COMMON, SP_CODE) %>% arrange(COMMON) %>% na.omit
    # taxa.cv = taxa.cv[ taxa.cv$SP_CODE %in% nodc.code , ]
    # 
    # 
    # mrip.catchtrip = mrip.catchtrip %>%
    #   mutate( COMMON = ifelse(SP_CODE == 8713010000, "SAWFISH FAMILY PRISTIDAE",
    #                    ifelse(SP_CODE == 8713010100, "SAWFISH GENUS PRISTIS",
    #                    ifelse(SP_CODE == 8713050300, "STINGRAY GENUS UROLOPHUS",
    #                    ifelse(SP_CODE == 8861010100, "PUFFER GENUS LAGOCEPHALUS",
    #                    ifelse(SP_CODE == 8861015357, "SHARPNOSE PUFFER",
    #                    ifelse(COMMON == "HALFBEAK GENUS", "HEMIRAMPHUS GENUS",
    #                    ifelse(COMMON == "REQUIEM SHARK", "REQUIEM SHARK FAMILY",
    #                    ifelse(COMMON == "SCALLOPED HAMMERHEAD", "SCALLOPED HAMMERHEAD SHARK", COMMON )))))))),
    #           SP_CODE = ifelse(SP_CODE == 8861015357, 8861010401, SP_CODE),
    #           DOM_ID = ifelse(ST == 12 & CNTY %in% c(33, 113, 91, 131, 5, 133, 59, 45, 13, 63, 37, 77, 39, 129, 73, 65, 123, 79, 29),  1,
    #                    ifelse(ST == 12 & CNTY %in% c(67, 121, 47, 75, 41, 1, 125, 23, 83, 17, 119, 53, 69,
    #                                                  101, 105, 103, 57, 81, 49, 55, 115, 27, 15, 43, 71, 51, 21),                      2,
    #                    ifelse(ST == 12 & CNTY == 87,                                                                                   3,
    #                    ifelse(ST == 12 & CNTY %in% c(25, 11, 99, 85, 111, 93, 61, 86),                                                 4,
    #                    ifelse(ST == 12 & CNTY %in% c(97, 9, 95, 117, 127, 35, 107, 109, 19, 7, 31, 89, 3),                             5,
    #                    ifelse(ST == 37 & CNTY %in% c(15, 29, 41, 53, 55, 139, 143, 177, 187),                                          6,
    #                    ifelse(ST == 37 & CNTY %in% c(13, 19, 31, 49, 95, 129, 133, 137, 141, 147),                                     7, 0 ))))))) ) %>%
    #   select( -c(CNTY) )
    # 
    # 
    # ###     ...I then apply the remaining filters to the raw "mrip.catchtrip" data (i.e., MODE_FX and DOM_ID )...
    # if( exists("FL_sub") & exists("NC_sub") ) {
    #   dom_sub = c( FL_sub, NC_sub )
    #   dom_sub = gsub( "N", 6, dom_sub )
    #   dom_sub = gsub( "S", 7, dom_sub )
    # } else if( exists("FL_sub") & !exists("NC_sub") ) {
    #   dom_sub = FL_sub
    # } else {
    #   dom_sub = NC_sub
    #   dom_sub = gsub( "N", 6, dom_sub )
    #   dom_sub = gsub( "S", 7, dom_sub )
    # }
    # 
    # mrip.catchtrip = mrip.catchtrip %>%
    #   filter( MODE_FX %in% modes.cv ) %>%
    #   filter( DOM_ID %in% dom_sub | DOM_ID == 0 )
    # 
    # 
    # rm( states.cv, modes.cv, dom_sub )
    # 
    # 
    # 
    # ### CHTS/FHS Calibration ###
    # ### ------------------------
    # ###     ...(i.e., standardizing all of the for-hire data into "FHS currency" )...
    # ###
    # ###   This requires multiplying all of the for-hire catch data provided in CHTS currency
    # ###       ( Cbt & Cbt/Hbt = MODE_FX %in% c(5,6) -- for years 1981-99 in the GOM and 1981-2003 in the SATL )
    # ###       by calibration ratios that convert to FHS currency:        CHTS * RATIO = FHS
    # ###       The required calibration ratios have already been built into RDI, and are downloaded here...
    # 
    # con = dbConnect(dbDriver("Oracle"), username = keyring::key_list("SECPR")[1,2],
    #                 password = keyring::key_get("SECPR", keyring::key_list("SECPR")[1,2]), dbname = "SECPR")
    # 
    # fhs.adj = dbGetQuery( con, "SELECT COMMON, YEAR, WAVE, ST, SUB_REG, MODE_FX, AREA_X, RATIO, VAR_RATIO
    #                     FROM RDI.MRIP_CATCH_CAL2018@secapxdv_dblk.sfsc.noaa.gov" )
    # fhs.adj = fhs.adj %>%
    #   mutate_at( vars( YEAR, WAVE, ST, SUB_REG ), list( ~ as.integer(.) ) ) %>%
    #   mutate_at( vars( COMMON, MODE_FX, AREA_X ), list( ~ as.character(.) ) )
    # 
    # dummy = mrip.catchtrip %>%
    #   left_join( fhs.adj, by = c( "COMMON", "YEAR", "WAVE", "ST", "SUB_REG", "MODE_FX", "AREA_X" ) ) %>%
    #   mutate( CHTS_CL = CLAIM, CHTS_H = HARVEST, CHTS_RL = RELEASE,
    #           CHTS_WAB1C = WGT_AB1C,   CHTS_WAB1H = WGT_AB1H ) %>%
    #   mutate_at( vars( CHTS_CL,CHTS_H,CHTS_RL,  CHTS_WAB1C,CHTS_WAB1H ), list( ~ ifelse(is.na(RATIO), NA, .) ) ) %>%
    #   mutate_at( vars( CLAIM,HARVEST,RELEASE,  WGT_AB1C,WGT_AB1H ), list( ~ ifelse( !is.na(RATIO), RATIO * ., . ) ) )
    # 
    # sefsc.catchtrip = dummy
    # rm( dummy, fhs.adj )
    # 
    # 
    # 
    # ### Separating the Combined CBT/HBT Mode ###
    # ### ----------------------------------------
    # ###
    # ###     ...for which I divide any catch from the combined CBT/HBT mode into separate CBT vs. HBT components.
    # ###         The required conversion factors are imported from RDI, but note that they are also on the N-drive
    # ###         ( see the 'gomratio' and 'saratio' SAS files at N:\FMB\RECREATIONAL\FHS calibration\Cbt_hbt ratios )...
    # 
    # cbt.hbt.convert = dbGetQuery( con, "SELECT * FROM RDI.CBT_HBT_RATIOS@secapxdv_dblk.sfsc.noaa.gov" )
    # cbt.hbt.convert = cbt.hbt.convert %>%
    #   pivot_longer( cols = c("HBT_R","CBT_R"), names_to = "NEW_MODEN", values_to = "Ratio" ) %>%
    #   mutate( NEW_MODEN = str_replace( NEW_MODEN, "_R","" ),
    #           MODE_FX = ifelse( NEW_MODEN == "CBT", 5, 4 ),
    #           ST = ifelse( NEW_STA == "TX", 48,   ifelse( NEW_STA == "LA", 22,   ifelse( NEW_STA == "MS", 28,
    #                ifelse( NEW_STA == "AL",  1,   ifelse( NEW_STA == "FLW",12,   ifelse( NEW_STA == "FLE",12,
    #                ifelse( NEW_STA == "GA", 13,   ifelse( NEW_STA == "SC", 45,   ifelse( NEW_STA == "NC", 37, NA ))))))))),
    #           DOM_ID  = 0 )
    # cbt.hbt.convert = rbind( cbt.hbt.convert %>% filter( NEW_STA %notin% c("FLW","FLE","NC") ) ,
    #                          cbt.hbt.convert %>% filter( NEW_STA == "FLW" ) %>% uncount( 3 ) ,
    #                          cbt.hbt.convert %>% filter( NEW_STA == "FLE" ) %>% uncount( 2 ) ,
    #                          cbt.hbt.convert %>% filter( NEW_STA == "NC"  ) %>% uncount( 2 ) )
    # cbt.hbt.convert$DOM_ID[ cbt.hbt.convert$NEW_STA == "FLW" ] = rep( c(1,2,3), times=2 )
    # cbt.hbt.convert$DOM_ID[ cbt.hbt.convert$NEW_STA == "FLE" ] = rep( c(4,5  ), times=2 )
    # cbt.hbt.convert$DOM_ID[ cbt.hbt.convert$NEW_STA == "NC"  ] = rep( c(6,7  ), times=2 )
    # cbt.hbt.convert = cbt.hbt.convert %>%
    #   mutate_at( vars( ST, MODE_FX, DOM_ID ), list( ~ as.integer(.) ) )
    # 
    # 
    # dummy.hire = sefsc.catchtrip %>%
    #   filter( MODE_FX == 6 & SUB_REG %in% c(6,7) ) %>%
    #   uncount( 2 )
    # dummy.hire = dummy.hire %>%
    #   mutate( MODE_FX = rep( c(4,5), times = (dim(dummy.hire)[1])/2 ) ) %>%
    #   left_join( cbt.hbt.convert[ ,c("ST","MODE_FX","DOM_ID","Ratio") ], by=c("ST","MODE_FX","DOM_ID") ) %>%
    #   mutate_at( vars( CLAIM,HARVEST,RELEASE,    WGT_AB1C,WGT_AB1H ), list( ~ ifelse( !is.na(Ratio), Ratio * ., .) ) ) %>%
    #   select( -c(Ratio) )
    # 
    # dummy.other = sefsc.catchtrip %>%
    #   filter( !( MODE_FX == 6 & SUB_REG %in% c(6,7) ) )
    # 
    # 
    # sefsc.catchtrip = rbind( dummy.other, dummy.hire )
    # rm( dummy.hire, dummy.other, cbt.hbt.convert )
    # 
    # 
    # 
    # ### ADDING SEFSC (AVERAGE) WEIGHT ESTIMATES ###
    # ### -------------------------------------------
    # ###
    # ###     ...which are pulled from the avg.wgt tables generated as part of the last batch of ACL files...
    # 
    # 
    # avgwgt.dir = gsub( "C:/Users/matthew.nuttall/Desktop/","O:/_Data/", avgwgt.dir )
    # ###   ...in cases where I need to pull avgwgt estimates from my personal O-drive (vs. my desktop)...
    # 
    # wgtest.srysmwa = read_sas( paste0( avgwgt.dir,"/avgwgt_srysmwa.sas7bdat" ) )
    # wgtest.srysmw  = read_sas( paste0( avgwgt.dir,"/avgwgt_srysmw.sas7bdat" ) )
    # wgtest.srysm   = read_sas( paste0( avgwgt.dir,"/avgwgt_srysm.sas7bdat" ) )
    # wgtest.srys    = read_sas( paste0( avgwgt.dir,"/avgwgt_srys.sas7bdat" ) )
    # wgtest.sry     = read_sas( paste0( avgwgt.dir,"/avgwgt_sry.sas7bdat" ) )
    # wgtest.sr      = read_sas( paste0( avgwgt.dir,"/avgwgt_sr.sas7bdat" ) )
    # wgtest.s       = read_sas( paste0( avgwgt.dir,"/avgwgt_s.sas7bdat" ) )
    # 
    # 
    # ### I then import some translation tables need to modify fields before the join...
    # con = dbConnect(dbDriver("Oracle"), username = keyring::key_list("SECPR")[1,2],
    #                 password = keyring::key_get("SECPR", keyring::key_list("SECPR")[1,2]), dbname = "SECPR")
    # spp.info = dbGetQuery( con, "SELECT * FROM RDI.v_species_xref@secapxdv_dblk.sfsc.noaa.gov" )
    # area_tab = dbGetQuery( con, "SELECT * FROM RDI.MRIP_AREA_CODES@secapxdv_dblk.sfsc.noaa.gov" )
    # st_tab   = dbGetQuery( con, "SELECT * FROM RDI.MRIP_STATE_CODES@secapxdv_dblk.sfsc.noaa.gov" )
    # mode_tab = dbGetQuery( con, "SELECT * FROM RDI.MRIP_MODE_CODES@secapxdv_dblk.sfsc.noaa.gov" )
    # 
    # spp.info$SP_CODE = spp.info$NODC_CODE
    # 
    # dummy = sefsc.catchtrip %>%
    #   mutate_at( vars( YEAR, WAVE, ST, SUB_REG, MODE_FX, AREA_X, DOM_ID ), list( ~ as.integer(.) ) ) %>%
    #   left_join( spp.info %>% select(SP_CODE,NEW_COM), by = "SP_CODE" ) %>%
    #   left_join( st_tab, by = "ST" ) %>%
    #   mutate( NEW_ST = ifelse( ST == 12 & SUB_REG == 7, 5,
    #                    ifelse( ST == 12 & SUB_REG == 6, 6, NEW_ST ) ),
    #           NEW_STA = ifelse( NEW_ST == 5, "FLW",
    #                     ifelse( NEW_ST == 6, "FLE", NEW_STA ) ) ) %>%
    #   left_join( mode_tab, by = "MODE_FX" ) %>%
    #   mutate( AREA_X = ifelse( AREA_X == "U", 6, AREA_X ) ) %>%
    #   left_join( area_tab, by = "AREA_X" )
    # 
    # dummy = dummy %>%
    #   left_join( wgtest.srysmwa[ ,colnames(wgtest.srysmwa) %notin% c("_TYPE_","_FREQ_") ] %>% rename_all(toupper),
    #              by = c("NEW_COM","SUB_REG","YEAR","NEW_STA","NEW_MODEN","WAVE","NEW_AREAN") ) %>%
    #   left_join( wgtest.srysmw[ ,colnames(wgtest.srysmw) %notin% c("_TYPE_","_FREQ_") ] %>% rename_all(toupper),
    #              by = c("NEW_COM","SUB_REG","YEAR","NEW_STA","NEW_MODEN","WAVE") ) %>%
    #   left_join( wgtest.srysm[ ,colnames(wgtest.srysm) %notin% c("_TYPE_","_FREQ_") ] %>% rename_all(toupper),
    #              by = c("NEW_COM","SUB_REG","YEAR","NEW_STA","NEW_MODEN") ) %>%
    #   left_join( wgtest.srys[ ,colnames(wgtest.srys) %notin% c("_TYPE_","_FREQ_") ] %>% rename_all(toupper),
    #              by = c("NEW_COM","SUB_REG","YEAR","NEW_STA") ) %>%
    #   left_join( wgtest.sry[ ,colnames(wgtest.sry) %notin% c("_TYPE_","_FREQ_") ] %>% rename_all(toupper),
    #              by = c("NEW_COM","SUB_REG","YEAR") ) %>%
    #   left_join( wgtest.sr[ ,colnames(wgtest.sr) %notin% c("_TYPE_","_FREQ_") ] %>% rename_all(toupper),
    #              by = c("NEW_COM","SUB_REG") ) %>%
    #   left_join( wgtest.s[ ,colnames(wgtest.s) %notin% c("_TYPE_","_FREQ_") ] %>% rename_all(toupper),
    #              by = c("NEW_COM") )
    # 
    # dummy[ ,grepl("AVGWGT_",colnames(dummy)) | grepl("NUMWGT_",colnames(dummy)) ][
    #   is.na( dummy[ ,grepl("AVGWGT_",colnames(dummy)) | grepl("NUMWGT_",colnames(dummy)) ] ) ] = 0
    # 
    # 
    # dummy = dummy %>%
    #   mutate( AVGWGT_SOURCE = ifelse( NUMWGT_SRYSMWA>=15 & AVGWGT_SRYSMWA > 0, "SRYSMWA",
    #                           ifelse( NUMWGT_SRYSMW >=15 & AVGWGT_SRYSMW  > 0, "SRYSMW",
    #                           ifelse( NUMWGT_SRYSM  >=15 & AVGWGT_SRYSM   > 0, "SRYSM",
    #                           ifelse( NUMWGT_SRYS   >=15 & AVGWGT_SRYS    > 0, "SRYS",
    #                           ifelse( NUMWGT_SRY    >=15 & AVGWGT_SRY     > 0, "SRY",
    #                           ifelse( NUMWGT_SR     >=15 & AVGWGT_SR      > 0, "SR",
    #                           ifelse( NUMWGT_S      >=15 & AVGWGT_S       > 0, "S", "NO WGT EST" ))))))),
    #           AVGWGT_SECwwt = ifelse( NUMWGT_SRYSMWA>=15 & AVGWGT_SRYSMWA > 0, AVGWGT_SRYSMWA,
    #                           ifelse( NUMWGT_SRYSMW >=15 & AVGWGT_SRYSMW  > 0, AVGWGT_SRYSMW,
    #                           ifelse( NUMWGT_SRYSM  >=15 & AVGWGT_SRYSM   > 0, AVGWGT_SRYSM,
    #                           ifelse( NUMWGT_SRYS   >=15 & AVGWGT_SRYS    > 0, AVGWGT_SRYS,
    #                           ifelse( NUMWGT_SRY    >=15 & AVGWGT_SRY     > 0, AVGWGT_SRY,
    #                           ifelse( NUMWGT_SR     >=15 & AVGWGT_SR      > 0, AVGWGT_SR,
    #                           ifelse( NUMWGT_S      >=15 & AVGWGT_S       > 0, AVGWGT_S, 0 ))))))),
    #           AVGWGT_NUM = ifelse( NUMWGT_SRYSMWA>=15 & AVGWGT_SRYSMWA > 0, NUMWGT_SRYSMWA,
    #                        ifelse( NUMWGT_SRYSMW >=15 & AVGWGT_SRYSMW  > 0, NUMWGT_SRYSMW,
    #                        ifelse( NUMWGT_SRYSM  >=15 & AVGWGT_SRYSM   > 0, NUMWGT_SRYSM,
    #                        ifelse( NUMWGT_SRYS   >=15 & AVGWGT_SRYS    > 0, NUMWGT_SRYS,
    #                        ifelse( NUMWGT_SRY    >=15 & AVGWGT_SRY     > 0, NUMWGT_SRY,
    #                        ifelse( NUMWGT_SR     >=15 & AVGWGT_SR      > 0, NUMWGT_SR,
    #                        ifelse( NUMWGT_S      >=15 & AVGWGT_S       > 0, NUMWGT_S, 0 ))))))) )
    # dummy = dummy %>%
    #   select( !c( "AVGWGT_SRYSMWA","NUMWGT_SRYSMWA", "AVGWGT_SRYSMW","NUMWGT_SRYSMW", "AVGWGT_SRYSM","NUMWGT_SRYSM",
    #               "AVGWGT_SRYS","NUMWGT_SRYS", "AVGWGT_SRY","NUMWGT_SRY", "AVGWGT_SR","NUMWGT_SR", "AVGWGT_S","NUMWGT_S" ) )
    # 
    # dummy$CLAIM_SECwwt = dummy$CLAIM * dummy$AVGWGT_SECwwt
    # dummy$HARVEST_SECwwt = dummy$HARVEST * dummy$AVGWGT_SECwwt
    # 
    # 
    # sefsc.catchtrip = dummy
    # rm( dummy, wgtest.srysmwa, wgtest.srysmw, wgtest.srysm, wgtest.srys, wgtest.sry, wgtest.sr, wgtest.s )
    # 
    # 
    # 
    # ### FUNCTION, FINAL ADJUSTMENTS, & ESTIMATION ###
    # ### ---------------------------------------------
    # 
    # 
    # options( survey.lonely.psu = "adjust" )
    # 
    # 
    # ### FUNCTION ###
    # ###
    # ### --------------------------------------------------------------------------------------------------------
    # 
    # estDomainRec = function( my.table, spp ) {
    #   
    #   pstrat_cat = my.table %>%
    #     mutate(  COMMON = ifelse( SP_CODE %in% nodc.code, COMMON, NA ) ) %>%
    #     arrange( COMMON ) %>%
    #     mutate( WP_CATCH = ifelse( is.na(COMMON), 0, WP_CATCH ) )
    #   
    #   pstrat_cat = pstrat_cat %>%
    #     distinct( ID_CODE, YEAR, COMMON, MODE_FX, .keep_all = TRUE ) %>%
    #     ###     ...where 'MODE_FX' is included to retain any CBT & HBT records from combined CBT/HBT sampling ( same trip = same ID_CODE )
    #     ###     ...'COMMON' is included to account for SEDARs where data is pulled for more than one species,
    #     ###               ( in that both species could be landed on the same fishing trip )
    #     ###     ...and 'YEAR' is included to account for any MRIP imputations that filled 2020 COVID data gaps
    #     ###             ( S&T left the year component of ID_CODE at its 2018/2019 identifier to allow analysts to
    #     ###               trace any 2020 imputations back to their original intercept record )...
    #     group_by( VAR_ID ) %>%
    #     filter( sum(WP_CATCH) > 0 ) %>%
    #     ungroup
    #   
    #   pstrat_cat = pstrat_cat %>%
    #     # gather( VARIABLE, Value, c(LANDINGS, DISCARDS) )               ### CVs for CATCH-IN-NUMBER
    #     gather( VARIABLE, Value, LANDINGS_SECwwt )                    ### CVs for CATCH-IN-WEIGHT
    # 
    #   estDomTotal = function(.) {
    #     design = svydesign( id = ~PSU_ID, strata = ~VAR_ID, weights = ~WP_CATCH, data = ., nest = TRUE )
    #     resulttotal = svyby( ~Value, by = ~MY_DOM_ID, design = design, FUN = svytotal, vartype = "var" )
    #   }
    #   
    #   op = options( dplyr.show_progress = FALSE )
    #   on.exit(options(op))
    #   
    #   # pstrat_cat = pstrat_cat %>% group_by(VARIABLE) %>% do(estDomTotal(.)) %>% mutate(COMMON = unlist(spp))
    #   pstrat_cat = pstrat_cat %>% group_by(VARIABLE) %>% do(estDomTotal(.))
    #   return( pstrat_cat )
    #   
    # }
    # 
    # ### --------------------------------------------------------------------------------------------------------
    # 
    # 
    # 
    # ### YEAR/MODE or YEAR/STATE ###
    # ###
    # ###             ***************************************************************************************************
    # ###             *** NOTE that this section of code includes defining the chosen stratification of the estimates ***
    # ###             *** and so is run twice -- once for YEAR-MODE and another for annual estimates (provided below) ***
    # ###             ***************************************************************************************************
    # 
    # ###     ...where the code below is setup to produce estimates by YEAR and MODE, but additional stratifications
    # ###         can also be (easily) calculated -- see the commented out definitions of MY_DOM_ID below...
    # 
    # dummy = sefsc.catchtrip %>%
    #   mutate(
    #     # MY_DOM_ID = paste0( YEAR, WAVE, sprintf("%02d", ST), SUB_REG, MODE_FX, AREA_X, DOM_ID ),   ### FINEST SCALE SUMMARY
    #     MY_DOM_ID = paste0( YEAR, MODE_FX ),                                                       ### YEAR-MODE SUMMARY
    #     # MY_DOM_ID = paste0( YEAR, sprintf("%02d", ST), SUB_REG, DOM_ID ),                          ### YEAR-STATE SUMMARY
    #     DOM_REGION = paste0( AREA_X, DOM_ID ) ) %>%
    #   arrange( COMMON, YEAR, WAVE, SUB_REG, ST, MODE_FX, AREA_X, DOM_ID ) %>%
    #   mutate( LANDINGS = CLAIM + HARVEST,  DISCARDS = RELEASE,
    #           LANDINGS_SECwwt = CLAIM_SECwwt + HARVEST_SECwwt ) %>%
    #   filter( !( MODE_FX == 4 & SUB_REG == 6 ) ) %>%
    #   filter( !( MODE_FX == 4 & SUB_REG == 7 & YEAR >= 1986 ) ) %>%
    #   filter( !( MODE_FX == 4 & DOM_ID == 3 ) ) %>%
    #   filter( !( ST == 22 & YEAR >= 2014 ) )
    # 
    # # cv.wgt.strata <- taxa.cv %>% rowwise %>% do(estDomainRec(.$COMMON))
    # cv.wgt.strata <- estDomainRec( my.table=dummy, spp=nodc.code )
    # rm( dummy )
    # 
    # 
    # 
    # ### YEAR ###
    # 
    # dummy = sefsc.catchtrip %>%
    #   mutate( MY_DOM_ID = YEAR,
    #           DOM_REGION = paste0( AREA_X, DOM_ID ) ) %>%
    #   arrange( COMMON, YEAR, WAVE, SUB_REG, ST, MODE_FX, AREA_X, DOM_ID ) %>%
    #   mutate( LANDINGS = CLAIM + HARVEST,  DISCARDS = RELEASE,
    #           LANDINGS_SECwwt = CLAIM_SECwwt + HARVEST_SECwwt ) %>%
    #   filter( !( MODE_FX == 4 & SUB_REG == 6 ) ) %>%
    #   filter( !( MODE_FX == 4 & SUB_REG == 7 & YEAR >= 1986 ) ) %>%
    #   filter( !( MODE_FX == 4 & DOM_ID == 3 ) ) %>%
    #   filter( !( ST == 22 & YEAR >= 2014 ) )
    # 
    # # cv.wgt.year <- taxa.cv %>% rowwise %>% do(estDomainRec(.$COMMON))
    # cv.wgt.year <- estDomainRec( my.table=dummy, spp=nodc.code )
    # rm( dummy )
    # 
    # 
    # 
    # 
    # ### Post-Processing ###
    # ### -------------------
    # 
    # 
    # ### YEAR/MODE SUMMARY ###
    # 
    # cv.wgt.strata = cv.wgt.strata %>%
    #   # left_join( taxa.cv, by = "COMMON" ) %>%
    #   rename_all(toupper) %>%
    #   mutate( VALUE = ifelse( VALUE < 0, 0, VALUE ),
    #           DS = "MRIP" )
    # 
    # ### ******************************************************************************************************************
    # ### ******************************************************************************************************************
    # 
    # ### CVs estimated at the finest strata ###
    # ### _____________________________________
    # ###
    # ###           MY_DOM_ID = paste0( YEAR, WAVE, sprintf("%02d", ST), SUB_REG, MODE_FX, AREA_X, DOM_ID )
    # 
    # # cv.wgt.strata = cv.wgt.strata %>%
    # # 
    # #   mutate(YEAR    = substr( MY_DOM_ID,  1,  4 ),
    # #          WAVE    = substr( MY_DOM_ID,  5,  5 ),
    # #          ST      = substr( MY_DOM_ID,  6,  7 ),
    # #          SUB_REG = substr( MY_DOM_ID,  8,  8 ),
    # #          MODE_FX = substr( MY_DOM_ID,  9,  9 ),
    # #          AREA_X  = substr( MY_DOM_ID, 10, 10 ),
    # #          DOM_ID  = substr( MY_DOM_ID, 11, 11 ) ) %>%
    # #   mutate_at( vars( YEAR, WAVE, ST, SUB_REG, MODE_FX, AREA_X, DOM_ID ), list( ~ as.integer(.) ) ) %>%
    # # 
    # #   left_join( st_tab, by = "ST" ) %>%
    # #   mutate( NEW_ST = ifelse( ST == 12 & SUB_REG == 7, 5,
    # #                    ifelse( ST == 12 & SUB_REG == 6, 6, NEW_ST )),
    # #           NEW_STA = ifelse( NEW_ST == 5, "FLW",
    # #                     ifelse( NEW_ST == 6, "FLE", NEW_STA )) ) %>%
    # #   mutate( NC_REG = ifelse( DOM_ID == 6, "N", ifelse(DOM_ID == 7, "S", NA)),
    # #           FL_REG = ifelse( is.na(NC_REG), DOM_ID, NA ) %>% ifelse( . == 0, NA, . ) ) %>%
    # # 
    # #   left_join( mode_tab, by = "MODE_FX" ) %>%
    # # 
    # #   mutate( AREA_X = ifelse( AREA_X == "U", 6, AREA_X ) ) %>%
    # #   left_join( area_tab, by = "AREA_X" ) %>%
    # # 
    # #   gather( variable, value, VALUE:VAR ) %>% unite( temp, VARIABLE, variable ) %>% spread( temp, value ) %>%
    # #   select(YEAR, DS, WAVE, ST, SUB_REG, MODE_FX, AREA_X,
    # #          NEW_MODEN, NEW_MODE,   DOM_ID, NEW_ST, NEW_STA, FL_REG, NC_REG, NEW_AREAN,
    # #          # AB1 = LANDINGS_VALUE, AB1.var = LANDINGS_VAR,
    # #          #  B2 = DISCARDS_VALUE,  B2.var = DISCARDS_VAR  ) %>%
    # #          LBS = LANDINGS_SECwwt_VALUE, LBS.var = LANDINGS_SECwwt_VAR ) %>%
    # #   mutate_at( vars( YEAR, WAVE, SUB_REG, ST, AREA_X, MODE_FX,
    # #                    FL_REG, NC_REG, NEW_ST, NEW_MODE ), list( ~ as.character(.) ) ) %>%
    # #   arrange( YEAR, WAVE, SUB_REG, ST, MODE_FX, AREA_X )
    # 
    # ### ******************************************************************************************************************
    # ### ******************************************************************************************************************
    # 
    # ### CVs estimated at the YEAR-MODE level ###
    # ### ________________________________________
    # ###
    # ###           MY_DOM_ID = paste0( YEAR, MODE_FX )
    # 
    # cv.wgt.strata = cv.wgt.strata %>%
    # 
    #   mutate(YEAR    = substr( MY_DOM_ID,  1, 4 ),
    #          MODE_FX = substr( MY_DOM_ID,  5, 5 ) ) %>%
    #   mutate_at( vars( YEAR, MODE_FX ), list( ~ as.integer(.) ) ) %>%
    #   left_join( mode_tab, by = "MODE_FX" ) %>%
    #   
    #   gather( variable, value, VALUE:VAR ) %>% unite( temp, VARIABLE, variable ) %>% spread( temp, value ) %>%
    #   select(YEAR, DS,
    #          MODE_FX, NEW_MODEN, NEW_MODE,
    #          # AB1 = LANDINGS_VALUE, AB1.var = LANDINGS_VAR,
    #          #  B2 = DISCARDS_VALUE,  B2.var = DISCARDS_VAR  ) %>%
    #          LBS = LANDINGS_SECwwt_VALUE, LBS.var = LANDINGS_SECwwt_VAR ) %>%
    #   mutate_at( vars( YEAR, MODE_FX, NEW_MODE ), list( ~ as.character(.) ) ) %>%
    #   arrange( YEAR, MODE_FX )
    # 
    # ### ******************************************************************************************************************
    # ### ******************************************************************************************************************
    # 
    # ### CVs estimated at the YEAR-STATE level ###
    # ### ________________________________________
    # ###
    # ###           MY_DOM_ID = paste0( YEAR, sprintf("%02d", ST), SUB_REG, DOM_ID )
    # 
    # # cv.wgt.strata = cv.wgt.strata %>%
    # # 
    # #   mutate(YEAR    = substr( MY_DOM_ID, 1, 4 ),
    # #          SUB_REG = substr( MY_DOM_ID, 7, 7 ),
    # #          ST      = substr( MY_DOM_ID, 5, 6 ),
    # #          DOM_ID  = substr( MY_DOM_ID, 8, 8 ) ) %>%
    # #   mutate_at( vars( YEAR, SUB_REG, ST, DOM_ID ), list( ~ as.integer(.) ) ) %>%
    # # 
    # #   left_join( st_tab, by = "ST" ) %>%
    # #   mutate( NEW_ST = ifelse( ST == 12 & SUB_REG == 7, 5,
    # #                    ifelse( ST == 12 & SUB_REG == 6, 6, NEW_ST )),
    # #           NEW_STA = ifelse( NEW_ST == 5, "FLW",
    # #                     ifelse( NEW_ST == 6, "FLE", NEW_STA )) ) %>%
    # # 
    # #   mutate( NC_REG = ifelse( DOM_ID == 6, "N", ifelse(DOM_ID == 7, "S", NA)),
    # #           FL_REG = ifelse( is.na(NC_REG), DOM_ID, NA ) %>% ifelse( . == 0, NA, . ) ) %>%
    # # 
    # #   gather( variable, value, VALUE:VAR ) %>% unite( temp, VARIABLE, variable ) %>% spread( temp, value ) %>%
    # #   select(YEAR, DS,
    # #          SUB_REG, ST, DOM_ID, NEW_ST, NEW_STA, FL_REG, NC_REG,
    # #          # AB1 = LANDINGS_VALUE, AB1.var = LANDINGS_VAR,
    # #          #  B2 = DISCARDS_VALUE,  B2.var = DISCARDS_VAR  ) %>%
    # #          LBS = LANDINGS_SECwwt_VALUE, LBS.var = LANDINGS_SECwwt_VAR ) %>%
    # #   mutate_at( vars( YEAR, SUB_REG, ST, NEW_ST, NEW_STA, FL_REG, NC_REG ), list( ~ as.character(.) ) ) %>%
    # #   arrange( YEAR, NEW_ST, FL_REG )
    # 
    # 
    # 
    # ### YEAR SUMMARY ###
    # 
    # cv.wgt.year = cv.wgt.year %>%
    #   # left_join( taxa.cv, by = "COMMON" ) %>%
    #   rename_all(toupper) %>%
    #   mutate( VALUE = ifelse( VALUE < 0, 0, VALUE ),
    #           DS = "MRIP" )
    # 
    # cv.wgt.year = cv.wgt.year %>%
    #   mutate(YEAR = substr( MY_DOM_ID,  1, 4 ) ) %>%
    #   mutate_at( vars( YEAR ), list( ~ as.integer(.) ) ) %>%
    #   gather( variable, value, VALUE:VAR ) %>% unite( temp, VARIABLE, variable ) %>% spread( temp, value ) %>%
    #   select(YEAR, DS,
    #          # AB1 = LANDINGS_VALUE, AB1.var = LANDINGS_VAR,
    #          #  B2 = DISCARDS_VALUE,  B2.var = DISCARDS_VAR  ) %>%
    #          LBS = LANDINGS_SECwwt_VALUE, LBS.var = LANDINGS_SECwwt_VAR ) %>%
    #   mutate_at( vars( YEAR ), list( ~ as.character(.) ) ) %>%
    #   arrange( YEAR )
    # 
    # 
    # 
    # 
    # 
    # ### Final Summary Table ###
    # ### -----------------------
    # 
    # 
    # ### YEAR/MODE SUMMARY ###
    # 
    # cv.wgt.strata = cv.wgt.strata %>%
    #   group_by( YEAR, NEW_MODEN ) %>%
    #   summarise( LBS     = sum( LBS ),
    #              LBS.var = sum( LBS.var ) ) %>%
    #   select( YEAR, NEW_MODEN, LBS, LBS.var ) %>%
    #   mutate( CV_LBS = sqrt(LBS.var) / LBS ) %>%
    #   select( YEAR, NEW_MODEN, LBS,CV_LBS ) %>%
    #   pivot_wider( names_from=c( NEW_MODEN ), values_from=c( LBS,CV_LBS ), names_glue = "{NEW_MODEN}_{.value}" )
    #   ###     ...where I use the names_glue() argument to specify how I want my columns named...
    # 
    # colnames(cv.wgt.strata) = toupper( colnames(cv.wgt.strata) )
    # 
    # 
    # ### YEAR SUMMARY ###
    # 
    # cv.wgt.year = cv.wgt.year %>%
    #   group_by( YEAR ) %>%
    #   summarise( LBS     = sum( LBS ),
    #              LBS.var = sum( LBS.var ) ) %>%
    #   select( YEAR, LBS, LBS.var ) %>%
    #   mutate( CV_LBS = sqrt(LBS.var) / LBS ) %>%
    #   select( YEAR, LBS,CV_LBS )
    # 
    # colnames(cv.wgt.year)[ -1 ] = paste0( "TOTAL_", colnames(cv.wgt.year)[ -1 ] )
    # 
    # 
    # ### JOIN ###
    # 
    # cv.wgt.table.1 = full_join( cv.wgt.strata, cv.wgt.year, by="YEAR" )
    # 
    # 
    # ### ORDER COLUMNS ###
    # 
    # lbest.cols <- vector()
    # 
    # col.IDs <- c( toupper(cv.cols),"TOTAL" )
    # ###     ...where "cv.cols" was defined when constructing the other CV table (for catch-in-number)...
    # col.IDs = col.IDs[ order( match(col.IDs,c("CBT","CBT_HBT","HBT","PRIV","PRIV_SHORE","SHORE","TOTAL")) ) ]
    # 
    # for( i in 1:length(col.IDs) ) {
    #   lbest.cols <- c( lbest.cols, paste0( col.IDs[i], c("_LBS","_CV_LBS") ) )
    # }
    # cv.wgt.table.1 <- cv.wgt.table.1[ ,c("YEAR",lbest.cols[ which( lbest.cols %in% colnames(cv.wgt.table.1) ) ] ) ]
    # ###  ...which includes a check that each of the modes (in "lbest.cols") are included in "cv.wgt.table.1".
    # ###       For example, some assessments may want us to include areas/states or mode for which catch
    # ###       was not actually observed/estimated. For example, FWC requested all available data for
    # ###       S79 mutton snapper (TX-ME) and so our pull included any CBT/HBT catch of mutton from 1981-2003
    # ###       in the MATL & NATL, but there wasn't any catch to consider and so these columns are missing
    # ###       from "cv.wgt.table.1". The check above checks for such cases...
    # 
    # 
    # rm( cv.wgt.strata, cv.wgt.year, col.IDs, lbest.cols )
    # 
    # 
    # 
    # 
    # cv.wgt.table.1[ is.na(cv.wgt.table.1) | cv.wgt.table.1 == 0 ] = NA
    
    
  }
  
  
  
  ### APPROACH #2 ###
  ### ---------------
  ###       ...where variability in raw size data is used as a proxy for the uncertainty in SEFSC average weight estimates
  
  if( approach == 2 ) {
    
    
    ###########################
    ### Landings-in-Numbers ###
    ###########################
    
    AB1.cv.table = num.table %>%
      rename_all( ~toupper(.) ) %>%
      mutate(     YEAR = as.integer(YEAR),
              NEW_MODE = as.integer(NEW_MODE),
                 VALUE = as.numeric(VALUE) ) %>%
      
      ###   ...where catch-in-weight estimates are only available for landings as there is no (average) size data for
      ###       discarded fish, and so the first step is to filter 'num.table' to remove all estimates associated with B2...
      filter( CATCH_VAR != 'B2' ) %>%
      
      ###   ...I also removed all sample size fields from this table as sample size summaries are already provided in the
      ###       catch-in-number and average (fish) weight tables, and aren't needed (again) in the landings-in-weight table...
      filter( !( METRIC %in% c('AT','PSU','ATtotal','PSUtotal') ) ) %>%
      
      ###   ...and as a final check that there are no <NA> values in the 'AB1.cv.table'...
      mutate( VALUE = ifelse( is.na(VALUE), 0, VALUE ) ) %>%
      
      ###   Lastly, I pivot (wide) the table so that catch & variance estimates are provided on the same row...
      pivot_wider( names_from = c(CATCH_VAR,METRIC), values_from = VALUE, names_glue = "{CATCH_VAR}_{METRIC}" )
    
    if( 'AB1_VAR' %notin% colnames(AB1.cv.table) ) {
      AB1.cv.table = AB1.cv.table %>%
        mutate( AB1_VAR = ( AB1_CAT * AB1_CV )^2 ) %>%
        select( -AB1_CV )
    }
    
    
    
    ######################
    ### Average Weight ###
    ######################
    
    avgwt.cv.table <- wgt.table %>%
      rename_all( ~toupper(.) ) %>%
      
      ###   ...where I start by pivoting the 'weight summary' fields into a new 'METRIC' field (i.e., long-format )...
      pivot_longer( cols = c('WGT','SE','FISH','TRP'), names_to = 'METRIC', values_to = 'VALUE' ) %>%
      
      mutate(     YEAR = as.integer(YEAR),
              NEW_MODE = as.integer(NEW_MODE),
                 VALUE = as.numeric(VALUE) ) %>%
      
      ###   ...of which we only retain the average weight & associated (standard) error estimates...
      filter( METRIC %in% c('WGT','SE') ) %>%
      mutate( METRIC = ifelse( METRIC == 'WGT', 'CAT', METRIC ) ) %>%
      
      ###   ...and just as a final check that there are no <NA> values in the 'AB1.cv.table'...
      mutate( VALUE = ifelse( is.na(VALUE), 0, VALUE ) ) %>%
      
      ###   ...I then add a CATCH_VAR field to ensure 'LBS' is included in the column names of our pivot (wide) table...
      mutate( CATCH_VAR = 'WGT' ) %>%
      pivot_wider( names_from = c(CATCH_VAR,METRIC), values_from = VALUE, names_glue = "{CATCH_VAR}_{METRIC}" ) %>%
      
      ###   Lastly, I convert the SE field to a CV estimate, which will be converted to a VAR estimate once the appropriate
      ###       (SEFSC) average weight estimate has been loaded (i.e., from 'catch.table' -- see below )...
      mutate( WGT_CV = ifelse( WGT_CAT == 0, 0, WGT_SE / WGT_CAT ) ) %>%
      select( -WGT_SE )
    
    
    ###   ...and just a check that the values in 'fed_closed' (if field exists) match b/w the two CV tables...
    if( 'fed_closed' %in% colnames(catch.table) ) {
      if( !all( unique(avgwt.cv.table$FED_CLOSED) %in% unique(AB1.cv.table$FED_CLOSED) ) ) {
        avgwt.cv.table = avgwt.cv.table %>%
          mutate( FED_CLOSED = ifelse( FED_CLOSED == 0, 'open',
                               ifelse( FED_CLOSED == 1, 'closed', FED_CLOSED )) )
        AB1.cv.table = AB1.cv.table %>%
          mutate( FED_CLOSED = ifelse( FED_CLOSED == 0, 'open',
                               ifelse( FED_CLOSED == 1, 'closed', FED_CLOSED )) )
      }
    }
    
    
    ### Substitute SEFSC AvgWgts into Table ###
    
    join.vec = colnames(avgwt.cv.table)[ which(
      colnames(avgwt.cv.table) %in% c('SID','YEAR','FED_CLOSED','NEW_MODEN','NEW_STA','WAVE') ) ]
    
    dummy = catch.table %>%
      mutate( NEW_MODEN = toupper(NEW_MODEN) ) %>%
      mutate( NEW_MODEN = ifelse( NEW_MODEN ==  'PRIVSHORE', 'PRIV',
                          ifelse( NEW_MODEN == 'PRIV/SHORE', 'PRIV', NEW_MODEN )) )
    
    dummy = dummy %>%
      # group_by( across( any_of( c('SID','YEAR','fed_closed','NEW_MODEN','NEW_STA','WAVE') ) ) ) %>%
      group_by( across( any_of( join.vec ) ) ) %>%
      summarise( WGT = sum( lbsest_SECwwt, na.rm=TRUE ),
                 AB1 = sum( AB1, na.rm=TRUE ) ) %>%
      # mutate( LBS = WGT / AB1 )
      mutate( LBS = ifelse( AB1==0, NA, WGT / AB1 ) )
    
    if( 'TOTAL' %in% avgwt.cv.table$NEW_MODEN ) {
      blah = dummy %>%
        group_by( across( any_of( c('SID','YEAR','fed_closed','NEW_STA','WAVE') ) ) ) %>%
        summarise( WGT = sum( WGT, na.rm=TRUE ),
                   AB1 = sum( AB1, na.rm=TRUE ) ) %>%
        mutate( NEW_MODEN = "TOTAL",
                # LBS = WGT / AB1 )
                LBS = ifelse( AB1==0, NA, WGT / AB1 ) )
      
      dummy = bind_rows( dummy, blah )
      rm( blah )
    }
    
    dummy = dummy %>%
      rename_all( ~toupper(.) ) %>%
      arrange( across( any_of( c('SID','YEAR','FED_CLOSED','NEW_MODEN','NEW_STA','WAVE') ) ) ) %>%
      select( -c(AB1,WGT) )
    
    dummy = avgwt.cv.table %>% full_join( dummy, by = join.vec )
    rm( join.vec )
    
    
    # ### Summary Figure -- AvgWt from Raw Data vs. Catch File ###
    # 
    # blah = dummy %>%
    #   select( -WGT_CV ) %>%
    #   rename( RAW = WGT_CAT,
    #           EST = LBS ) %>%
    #   pivot_longer( cols = c("RAW","EST"), names_to = "SOURCE", values_to = "value" )
    # 
    # if( 'fed_closed' %in% colnames(catch.table) ) {
    #   dummy.plot = ggplot( blah, aes( x=YEAR , y=value , colour=SOURCE, linetype = FED_CLOSED ) ) + geom_line()
    # } else {
    #   dummy.plot = ggplot( blah, aes( x=YEAR , y=value , colour=SOURCE ) ) + geom_line()
    # }
    # 
    # # dummy.plot = ggplot( blah, aes( x=YEAR , y=value , colour=SOURCE ) ) + geom_line()
    # if( 'SID' %in% colnames(catch.table) ) {
    #   dummy.plot = dummy.plot + facet_grid( NEW_MODEN ~ SID, scales="free" )
    # } else {
    #   dummy.plot = dummy.plot + facet_grid( NEW_MODEN ~ ., scales="free" )
    # }
    # 
    # rm( blah, dummy.plot )
    
    
    dummy = dummy %>%
      select( -WGT_CAT ) %>%
      rename( WGT_CAT = LBS ) %>%
      select( any_of( c( 'SID','YEAR','FED_CLOSED','NEW_MODE','NEW_MODEN','NEW_STA','WAVE','WGT_CAT','WGT_CV' ) ) )
    
    
    ### Convert CV Estimates to Variances ###
    
    dummy = dummy %>%
      mutate( WGT_VAR = ( WGT_CV * WGT_CAT )^2 ) %>%
      select( -WGT_CV )
    
    
    avgwt.cv.table = dummy
    rm( dummy )
    
    avgwt.cv.table[ is.na(avgwt.cv.table) ] = 0
    
    
    
    ######################
    ###      JOIN      ###
    ######################
    
    join.vec = colnames(avgwt.cv.table)[ which(
      colnames(avgwt.cv.table) %in% c('SID','YEAR','FED_CLOSED','NEW_MODE','NEW_MODEN','NEW_STA','WAVE') ) ]
    
    lbs.cv.table.2 = AB1.cv.table %>% full_join( avgwt.cv.table, by = join.vec )
    rm( join.vec )
    # rm( AB1.cv.table, avgwt.cv.table )
    
    
    ###   ...where, after the join, I do a quick check to remove any MODES for which catch estimates
    ###     weren't calculated (i.e., no observed catch records -- sum(CAT) = 0 ). As an example, this can
    ###     happen in SATL assessment where the HBT mode is being included (i.e., in the assessment )
    ###     but for which MRIP estimates don't exist ( SATL HBT estimates come from SRHS )...
    null.modes = lbs.cv.table.2 %>%
      group_by( NEW_MODEN ) %>%
      summarize( SUMCAT = sum( AB1_CAT, na.rm=TRUE ) )
    null.modes = null.modes$NEW_MODEN[ null.modes$SUMCAT == 0 ]
    
    if( length(null.modes)>0 ) {
      lbs.cv.table.2 = lbs.cv.table.2 %>% filter( NEW_MODEN %notin% null.modes )
    }
    rm( null.modes )
    
    
    ### Calculating landings-in-weight & associated variances ###
    ###     ...for which the calculation assumes our estimates for landings-in-number and
    ###       average (fish) weights are both random & independent...
    ###             var( LBS[yr,mode] ) = ( AB1^2 )*var(LBS) + ( LBS^2 )*var(AB1) - var(AB1)*var(LBS)
    
    dummy = lbs.cv.table.2 %>%
      mutate( WGT_VAR = ifelse( is.nan(WGT_VAR) | is.na(WGT_VAR), 0, WGT_VAR ) ) %>%
      
      mutate( LBS_CAT = AB1_CAT * WGT_CAT,
              LBS_VAR = ( (WGT_CAT^2)*AB1_VAR ) + ( (AB1_CAT^2)*WGT_VAR ) - ( AB1_VAR*WGT_VAR ) ) %>%
      mutate( LBS_CV = ifelse( LBS_CAT == 0, 0, sqrt(LBS_VAR) / LBS_CAT ) ) %>%
      
      select( any_of( c('SID','YEAR','FED_CLOSED','NEW_MODE','NEW_MODEN','NEW_STA','WAVE','LBS_CAT','LBS_CV') ) )
    lbs.cv.table.2 = dummy
    rm( dummy )
    
    
    lbs.cv.table = lbs.cv.table.2
    
  }
  
  
  return( lbs.cv.table )
  
}

