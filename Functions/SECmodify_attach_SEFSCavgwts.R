

### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------
###
###     DESCRIPTION
###
###   ...where the function below was developed to attach SEFSC avgwgts to a given dataset
###     ( at whatever resolution the associated data is provided ) and the associated LBS estimate calculated...
###
### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------


attach.SEFSC.avgwgts = function( genrec.table, avgwgt.dir ) {
  ###     ...where 'genrec.table' is a table of (AB1) catch estimates to which SEFSC avgwgts are being attached
  ###     ...and 'avgwgt.dir' identifies the directory within which SEFSC avgwgt estimates have been saveed...
  
  
  avgwgt.s.file = read_sas( data_file = paste0( avgwgt.dir,"/avgwgt_s.sas7bdat" ) )
  avgwgt.s.file$dummy_label = paste0( avgwgt.s.file$NEW_COM )
  genrec.table$dummy_label  = paste0(  genrec.table$NEW_COM )
  genrec.table$AVGWGT_S = avgwgt.s.file$avgwgt_s[ match( genrec.table$dummy_label, avgwgt.s.file$dummy_label ) ]
  genrec.table$NUMWGT_S = avgwgt.s.file$numwgt_s[ match( genrec.table$dummy_label, avgwgt.s.file$dummy_label ) ]
  genrec.table = genrec.table %>% select( !dummy_label )
  rm( avgwgt.s.file )
  
  if( 'SUB_REG' %in% colnames(genrec.table) ) {
    avgwgt.sr.file = read_sas( data_file = paste0( avgwgt.dir,"/avgwgt_sr.sas7bdat" ) )
    avgwgt.sr.file$dummy_label = paste0( avgwgt.sr.file$NEW_COM,"_",avgwgt.sr.file$SUB_REG )
    genrec.table$dummy_label   = paste0(   genrec.table$NEW_COM,"_",  genrec.table$SUB_REG )
    genrec.table$AVGWGT_SR = avgwgt.sr.file$avgwgt_sr[ match( genrec.table$dummy_label, avgwgt.sr.file$dummy_label ) ]
    genrec.table$NUMWGT_SR = avgwgt.sr.file$numwgt_sr[ match( genrec.table$dummy_label, avgwgt.sr.file$dummy_label ) ]
    genrec.table = genrec.table %>% select( !dummy_label )
    rm( avgwgt.sr.file )
  } else {
    genrec.table$AVGWGT_SR = NA
    genrec.table$NUMWGT_SR = NA
  }
  
  if( all( c('SUB_REG','YEAR') %in% colnames(genrec.table) ) ) {
    avgwgt.sry.file = read_sas( data_file = paste0( avgwgt.dir,"/avgwgt_sry.sas7bdat" ) )
    avgwgt.sry.file$dummy_label = paste0( avgwgt.sry.file$NEW_COM,"_",avgwgt.sry.file$SUB_REG,"_",avgwgt.sry.file$year )
    genrec.table$dummy_label    = paste0(    genrec.table$NEW_COM,"_",   genrec.table$SUB_REG,"_",   genrec.table$YEAR )
    genrec.table$AVGWGT_SRY = avgwgt.sry.file$avgwgt_sry[ match( genrec.table$dummy_label, avgwgt.sry.file$dummy_label ) ]
    genrec.table$NUMWGT_SRY = avgwgt.sry.file$numwgt_sry[ match( genrec.table$dummy_label, avgwgt.sry.file$dummy_label ) ]
    genrec.table = genrec.table %>% select( !dummy_label )
    rm( avgwgt.sry.file )
  } else {
    genrec.table$AVGWGT_SRY = NA
    genrec.table$NUMWGT_SRY = NA
  }
  
  if( all( c('SUB_REG','YEAR','NEW_STA') %in% colnames(genrec.table) ) ) {
    avgwgt.srys.file = read_sas( data_file = paste0( avgwgt.dir,"/avgwgt_srys.sas7bdat" ) )
    avgwgt.srys.file$dummy_label = paste0( avgwgt.srys.file$NEW_COM,"_",avgwgt.srys.file$SUB_REG,"_",avgwgt.srys.file$year,"_",
                                           avgwgt.srys.file$NEW_STA )
    genrec.table$dummy_label     = paste0(     genrec.table$NEW_COM,"_",    genrec.table$SUB_REG,"_",    genrec.table$YEAR,"_",
                                               genrec.table$NEW_STA )
    genrec.table$AVGWGT_SRYS = avgwgt.srys.file$avgwgt_srys[ match( genrec.table$dummy_label, avgwgt.srys.file$dummy_label ) ]
    genrec.table$NUMWGT_SRYS = avgwgt.srys.file$numwgt_srys[ match( genrec.table$dummy_label, avgwgt.srys.file$dummy_label ) ]
    genrec.table = genrec.table %>% select( !dummy_label )
    rm( avgwgt.srys.file )
  } else {
    genrec.table$AVGWGT_SRYS = NA
    genrec.table$NUMWGT_SRYS = NA
  }
  
  if( all( c('SUB_REG','YEAR','NEW_STA','NEW_MODEN') %in% colnames(genrec.table) ) ) {
    avgwgt.srysm.file = read_sas( data_file = paste0( avgwgt.dir,"/avgwgt_srysm.sas7bdat" ) )
    avgwgt.srysm.file$dummy_label = paste0( avgwgt.srysm.file$NEW_COM,"_",avgwgt.srysm.file$SUB_REG,"_",avgwgt.srysm.file$year,"_",
                                            avgwgt.srysm.file$NEW_STA,"_",avgwgt.srysm.file$NEW_MODEN )
    genrec.table$dummy_label      = paste0(      genrec.table$NEW_COM,"_",     genrec.table$SUB_REG,"_",     genrec.table$YEAR,"_",
                                                 genrec.table$NEW_STA,"_",     genrec.table$NEW_MODEN )
    genrec.table$AVGWGT_SRYSM = avgwgt.srysm.file$avgwgt_srysm[ match( genrec.table$dummy_label, avgwgt.srysm.file$dummy_label ) ]
    genrec.table$NUMWGT_SRYSM = avgwgt.srysm.file$numwgt_srysm[ match( genrec.table$dummy_label, avgwgt.srysm.file$dummy_label ) ]
    genrec.table = genrec.table %>% select( !dummy_label )
    rm( avgwgt.srysm.file )
  } else {
    genrec.table$AVGWGT_SRYSM = NA
    genrec.table$NUMWGT_SRYSM = NA
  }
  
  if( all( c('SUB_REG','YEAR','NEW_STA','NEW_MODEN','WAVE') %in% colnames(genrec.table) ) ) {
    avgwgt.srysmw.file = read_sas( data_file = paste0( avgwgt.dir,"/avgwgt_srysmw.sas7bdat" ) )
    avgwgt.srysmw.file$dummy_label = paste0( avgwgt.srysmw.file$NEW_COM,"_",avgwgt.srysmw.file$SUB_REG,  "_",avgwgt.srysmw.file$year,"_",
                                             avgwgt.srysmw.file$NEW_STA,"_",avgwgt.srysmw.file$NEW_MODEN,"_",avgwgt.srysmw.file$WAVE )
    genrec.table$dummy_label       = paste0(       genrec.table$NEW_COM,"_",      genrec.table$SUB_REG,  "_",      genrec.table$YEAR,"_",
                                                   genrec.table$NEW_STA,"_",      genrec.table$NEW_MODEN,"_",      genrec.table$WAVE )
    genrec.table$AVGWGT_SRYSMW = avgwgt.srysmw.file$avgwgt_srysmw[ match( genrec.table$dummy_label, avgwgt.srysmw.file$dummy_label ) ]
    genrec.table$NUMWGT_SRYSMW = avgwgt.srysmw.file$numwgt_srysmw[ match( genrec.table$dummy_label, avgwgt.srysmw.file$dummy_label ) ]
    genrec.table = genrec.table %>% select( !dummy_label )
    rm( avgwgt.srysmw.file )
  } else {
    genrec.table$AVGWGT_SRYSMW = NA
    genrec.table$NUMWGT_SRYSMW = NA
  }
  
  if( all( c('SUB_REG','YEAR','NEW_STA','NEW_MODEN','WAVE','NEW_AREAN') %in% colnames(genrec.table) ) ) {
    avgwgt.srysmwa.file = read_sas( data_file = paste0( avgwgt.dir,"/avgwgt_srysmwa.sas7bdat" ) )
    avgwgt.srysmwa.file$dummy_label = paste0( avgwgt.srysmwa.file$NEW_COM,"_",avgwgt.srysmwa.file$SUB_REG,  "_",avgwgt.srysmwa.file$year,"_",
                                              avgwgt.srysmwa.file$NEW_STA,"_",avgwgt.srysmwa.file$NEW_MODEN,"_",avgwgt.srysmwa.file$WAVE,"_",
                                              avgwgt.srysmwa.file$NEW_AREAN )
    genrec.table$dummy_label        = paste0(        genrec.table$NEW_COM,"_",       genrec.table$SUB_REG,  "_",       genrec.table$YEAR,"_",
                                                     genrec.table$NEW_STA,"_",       genrec.table$NEW_MODEN,"_",       genrec.table$WAVE,"_",
                                                     genrec.table$NEW_AREAN )
    genrec.table$AVGWGT_SRYSMWA = avgwgt.srysmwa.file$avgwgt_srysmwa[ match( genrec.table$dummy_label, avgwgt.srysmwa.file$dummy_label ) ]
    genrec.table$NUMWGT_SRYSMWA = avgwgt.srysmwa.file$numwgt_srysmwa[ match( genrec.table$dummy_label, avgwgt.srysmwa.file$dummy_label ) ]
    genrec.table = genrec.table %>% select( !dummy_label )
    rm( avgwgt.srysmwa.file )
  } else {
    genrec.table$AVGWGT_SRYSMWA = NA
    genrec.table$NUMWGT_SRYSMWA = NA
  }
  
  
  ###   We then go row-by-row, identifying the SEFSC avgwgt estimates most appropriate for a given strata
  ###   ( as recorded in 'lbsest_SECsource' ) and saving it to a new 'AVGWGT_SEC' field such that it can then
  ###   be multiplied by the associated AB1 estimate ( to update our 'lbest_SECwwt' estimates )...
  
  n.size.threshold = 15
  
  genrec.table$AVGWGT_SEC = 0
  
  for( i in 1:dim(genrec.table)[1] ) {
    
    if( !is.na(genrec.table$NUMWGT_SRYSMWA[i]) & genrec.table$NUMWGT_SRYSMWA[i] >= n.size.threshold ) {
      genrec.table$LBSEST_SECSOURCE[i] = "srysmwa"
      genrec.table$AVGWGT_SEC[i] = genrec.table$AVGWGT_SRYSMWA[i]
    } else if( !is.na(genrec.table$NUMWGT_SRYSMW[i]) & genrec.table$NUMWGT_SRYSMW[i] >= n.size.threshold ) {
      genrec.table$LBSEST_SECSOURCE[i] = "srysmw"
      genrec.table$AVGWGT_SEC[i] = genrec.table$AVGWGT_SRYSMW[i]
    } else if( !is.na(genrec.table$NUMWGT_SRYSM[i]) & genrec.table$NUMWGT_SRYSM[i] >= n.size.threshold ) {
      genrec.table$LBSEST_SECSOURCE[i] = "srysm"
      genrec.table$AVGWGT_SEC[i] = genrec.table$AVGWGT_SRYSM[i]
    } else if( !is.na(genrec.table$NUMWGT_SRYS[i]) & genrec.table$NUMWGT_SRYS[i] >= n.size.threshold ) {
      genrec.table$LBSEST_SECSOURCE[i] = "srys"
      genrec.table$AVGWGT_SEC[i] = genrec.table$AVGWGT_SRYS[i]
    } else if( !is.na(genrec.table$NUMWGT_SRY[i]) & genrec.table$NUMWGT_SRY[i] >= n.size.threshold ) {
      genrec.table$LBSEST_SECSOURCE[i] = "sry"
      genrec.table$AVGWGT_SEC[i] = genrec.table$AVGWGT_SRY[i]
    } else if( !is.na(genrec.table$NUMWGT_SR[i]) & genrec.table$NUMWGT_SR[i] >= n.size.threshold ) {
      genrec.table$LBSEST_SECSOURCE[i] = "sr"
      genrec.table$AVGWGT_SEC[i] = genrec.table$AVGWGT_SR[i]
    } else {
      genrec.table$LBSEST_SECSOURCE[i] = "s"
      genrec.table$AVGWGT_SEC[i] = genrec.table$AVGWGT_S[i]
    }
    
  }
  
  genrec.table$lbsest_SECwwt = genrec.table$AB1 * genrec.table$AVGWGT_SEC
  genrec.table = genrec.table %>% select( !AVGWGT_SEC )
  
  rm( n.size.threshold )
  
  
  return( genrec.table )
  
  
}