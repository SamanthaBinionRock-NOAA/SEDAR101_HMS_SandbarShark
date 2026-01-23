

### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------
###
###     DESCRIPTION
###
###   In accordance with SEDAR Best Practices, an (excel) template was developed for life history data
###   in an effort to standardize data provision across the southeast region (i.e., as provided by different surveys).
###   The GenRec size data submitted for SEDAR stock assessments will therefore follow this template, constructed with
###   input from the NMFS biological groups (i.e., Panama City & Beaufort ) and SEDAR data providers. This function
###   was developed to ensure GenRec size files follow the (biological) template...
###
### -----------------------------------------------------------------------------------------------------------
### -----------------------------------------------------------------------------------------------------------

format.BIOtemplate = function( genrec.table, add.strata = NA ) {
  ###     ...where 'genrec.table' is the table of raw size data that is to be modified in accordance with
  ###         the SEDAR-BIO template. Note that the required/requested fields in the BIO template are described
  ###         in the associated 'Morphometrics Data Fields' excel spreadsheet...
  ###     ...and 'add.strata' identifies any additional strata that is not identified in the template,
  ###         but which has been requested to be retained for the assessment...
  
  
  SID.flag = 'SID' %in% colnames(genrec.table)
  
  genrec.table = genrec.table %>%
    
    mutate( SEDAR = paste0( format( Sys.Date(), '%Y' ), 'SEDAR',
                               # sprintf( '%03d', as.numeric( gsub( 'SEDAR ','', current.sedar ) ) ) ),
                               sprintf( '%04s', as.character( gsub( 'SEDAR ','', current.sedar ) ) ) ),
            SEDAR_Date_Submit = format( Sys.Date(), '%m/%Y' ),
            
            Species = NEW_SCI,
            Species_ITIS = SPECIES_ITIS,
            Species_NODC = SP_CODE )
  
  if( SID.flag ) {
    genrec.table = genrec.table %>% mutate( Stock = paste0( region,' - ',SID ) )
  } else {
    genrec.table = genrec.table %>% mutate( Stock = region )
  }
  
  
  ###   ...and making sure the month-day-year information is being properly read...
  if( 'YEAR' %notin% colnames(genrec.table) ) {
    if( 'year' %in% colnames(genrec.table) ) {
      genrec.table = genrec.table %>% rename( YEAR = year )
    } else {
      rename.vec = c( YEAR = 'INT_YEAR', YEAR = 'int_year' )
      genrec.table = genrec.table %>% rename( any_of( rename.vec ) )
      rm( rename.vec )
    }
  }
  if( 'MONTH' %notin% colnames(genrec.table) ) {
    if( 'month' %in% colnames(genrec.table) ) {
      genrec.table = genrec.table %>% rename( MONTH = month )
    } else {
      rename.vec = c( MONTH = 'INT_MONTH', MONTH = 'int_month' )
      genrec.table = genrec.table %>% rename( any_of( rename.vec ) )
      rm( rename.vec )
    }
  }
  if( 'DAY' %notin% colnames(genrec.table) ) {
    if( 'day' %in% colnames(genrec.table) ) {
      genrec.table = genrec.table %>% rename( DAY = day )
    } else {
      rename.vec = c( DAY = 'INT_DAY', DAY = 'int_day' )
      genrec.table = genrec.table %>% rename( any_of( rename.vec ) )
      rm( rename.vec )
    }
  }
  
  
  genrec.table = genrec.table %>%
    
    mutate( Data_Provider = 'NMFS SEFSC-SFD',
            
            Sampling_Program = DS,
            
            
            ### ---------------------------------------------------------------------------------------------
            ###   The next three fields are treated as a 'grouping' by the LH group, providing information on how samples
            ###       were collected in a given survey and informing decisions on how best to treat such size data...
            
            Sample_Method_Type = ifelse( DS == 'MRIP', 'MRIP Sampling',
                                 ifelse( DS == 'TPWD', 'TPWD Sampling',
                                 ifelse( DS %in% c('LA Creel','LA BIO'), 'Targeted Biological', NA ) ) ),
            ###   MRIP protocols instruct dockside samplers to sample all fish (of a given species) up to n=15 ,
            ###       but a 'simple random' sample is to be taken when n > 15 ( samplers randomly select 15 fish ).
            ###       These guidelines aren't always followed (e.g., if the sampler is at a busy site and needs to move on
            ###       or if the angler is getting impatient and wants to leave), but this is the basic sampling protocol.
            ###   TPWD follows a similar protocol to that described for MRIP (above), except dockside samplers are instructed
            ###       to sample up to six lengths for each species in each party (vs. 15 in MRIP).
            ###   LABIO protocols are based on a list of species (i.e., BIOFIN species ), for which sampling is largely
            ###       limited to those taxa for which LDWF has federal funding to collect (i.e., 'Targeted Biological' ).
            
            Random =  ifelse( DS == 'MRIP', 'Y',
                      ifelse( DS == 'TPWD', 'Y',
                      ifelse( DS %in% c('LA Creel','LA BIO'), 'Y', NA ) ) ),
            ###   ...where all three surveys follow some degree of randomness and so are assigned Random = 'Y'...
            ###         -- MRIP & TPWD -- dockside samplers either sample a random subset of all fish ( when n is 'high' ) or
            ###             sample everything ( when n is 'low' ), wherein random sampling isn't really possible in the latter
            ###             (there's no random subset if you sample everything) and so there's no problem to flag here
            ###             (i.e., no need to flag as non-random )
            ###         -- LABIO -- field samplers are instructed to use their field experience to randomly distribute
            ###             (biological sampling) effort across the designated sampling period.
            ###       Additionally, the sampling schedules of all three of these surveys are constructed from random draws,
            ###       with sampling weights a function of the distribution of (relative) fishing effort of rec anglers, and
            ###       so there's also randomness in site selection (in addition to sampling).

            Bias_Type = 'No Bias Known',
            ###   ...wherein every sampling program is going to miss certain aspects of the population of interest
            ###       (e.g., omission of private access sites from site selection ), but the real question is whether
            ###       there are any known biases from the perspective of the defined scope of the survey, to which the
            ###       answer is 'no' for our surveys. This isn't to say that these surveys are 100% unbiased, its just
            ###       that such biases haven't been conclusively identified and/or documented...
            ### ---------------------------------------------------------------------------------------------
            
            
            Fishery = 'REC',
            
            Fishing_Mode = ifelse( NEW_MODEN == 'Shore', 'SH',
                           ifelse( NEW_MODEN ==   'Cbt', 'CB',
                           ifelse( NEW_MODEN ==   'Hbt', 'HB',
                           ifelse( NEW_MODEN ==  'Priv', 'PR', NA )))),
            
            Sampling_Unit_ID = ifelse( DS == 'MRIP', ID_CODE,
                               ifelse( DS == 'TPWD', ID_CODE,
                               ifelse( DS %in% c('LA Creel','LA BIO'), ID_CODE, NA ))),
            
            Month = MONTH,
            Day = DAY,
            Year = YEAR,
            
            State_Landed = NEW_STA,
            County_Landed = toupper(NEW_CNTYN),
            
            Gear_Group_Code = ifelse( NEW_GEARN == 'hook & line', 'HL',
                              ifelse( NEW_GEARN ==       'spear', 'SP',
                              ifelse( NEW_GEARN ==        'trap', 'TR',
                              ifelse( NEW_GEARN ==       'trawl', 'TW',
                              ifelse( NEW_GEARN ==    'gill net', 'GN',
                              ifelse( NEW_GEARN ==    'cast net', 'CN',
                              ifelse( NEW_GEARN ==       'seine', 'SN',
                              # ifelse( NEW_GEARN ==     'dip net', 'DN',
                              # ifelse( NEW_GEARN ==        'hand', 'HD',
                              ifelse( NEW_GEARN %in% c( 'other','unknown','dip net','hand' ),  'U', NA )))))))),
            ###     ...although requested by the LH group, we're unable to distinguish:
            ###           -- longlines & vertical lines ( 'LL' & 'VL' ) from 'other' types of gears
            ###                   ( these gears are part of the MRIP 'other' category )
            ###           -- trammel nets ( 'TN' ) from 'gill net'
            ###         None of these gears are a 'major' part of the REC sector, and so we're just lumping them into
            ###         Gear_Group_Code='OTHER', but it is a caveat to keep in mind...
            ###     Conversely, our (NEW_GEARN) fields allows us to distinguish 'dip net' and 'hand' fishing from other gears,
            ###         which we could also add as gear options to the BIO template. However, the chosen gear groups (in the template)
            ###         are reserved for those gears needed for an assessment, of which 'dip net' and 'hand' fishing (by themselves)
            ###         are not believed important. Therefore, they're also lumped with the Gear_Group_Code='OTHER' category...
            
            Jurisdictional_Waters = ifelse( NEW_AREAN %in% c('Inshore','Ocean<=3mi','Ocean<=10mi'), 'State',
                                    ifelse( NEW_AREAN %in% c(           'Ocean>3mi', 'Ocean>10mi'), 'Federal', NA )),
            
            
            Original_Length_Unit = ifelse( DS == 'MRIP', 'mm',
                                   ifelse( DS == 'TPWD', 'mm',
                                   ifelse( DS %in% c('LA Creel','LA BIO'), 'mm', NA ))),
            ###     MRIP Field Manual (GSMFC 2021, p54-55) -- "Fish lengths must be taken using a measuring board and
            ###           recorded to the nearest millimeter... Never round lengths to the nearest centimeter or half centimeter.
            ###           Rounding fish measurements will introduce a 'digit bias.'"
            ###     TPWD Operations Manual (Green 2017, p33) -- Dockside samplers are instructed to "enter individual lengths
            ###           (mm) of each species landed"
            ###     LABIO Sampling Program (LDWF 2018) -- Although never expressly stated, all length measurements in LABIO
            ###           appear to be in millimeters as all references to 'length' (in the 2018 metadata document) are followed
            ###           by 'millimeters fork length or appropriate measure', wherein different length types may be used
            ###           (as we know from their raw data files) but all are provided in units of millimeters.
            
            
            ### Note that I considered populating the (numeric) length fields from the native (survey-specific) fields:
            ### 
            ### # Observed_Maximum_TL_mm = ifelse( DS == 'TPWD', length,
            ### #                          ifelse( DS %in% c('LA Creel','LA BIO'), LENGTH_3, NA )),
            ### #         Observed_FL_mm = ifelse( DS == 'MRIP', LNGTH,
            ### #                          ifelse( DS %in% c('LA Creel','LA BIO'), LENGTH_1, NA )),
            ### #         Observed_SL_mm = ifelse( DS %in% c('LA Creel','LA BIO'), LENGTH_2, NA ),
            ### 
            ###       ...but to decided to use our 'standard' (SEFSC) length fields ( 'FL_mm' & 'TL_mm' ) to ensure that the
            ###     LH group is using the same fields as we use in our (GenRec) data flows, wherein we may apply additional filters
            ###     (e.g., remove lengths that are 'too large' for a species ) or additional processing (e.g., LW conversions )
            ###     that may not be captured by the native length fields.
            
            
            Observed_Maximum_TL_mm = TL_mm,
            ###     TPWD Operations Manual (Green 2017, p33) -- "Total lengths are preferred... [wherein] fish total length is
            ###          tip of snout (mouth closed) to tip of longest caudal ray (caudal fin compressed)."
            
            Observed_FL_mm = FL_mm,
            ###     MRIP Field Manual (GSMFC 2021, p52) -- "Fork length is recorded for all fish. Fork length is the length
            ###           of the fish from the tip of the snout to the fork of the tail (Figure 4.3)." The manual does note that
            ###           "different species of fish have different types of tails and snouts (Figure 4.4)", but fork length is
            ###           still the default length type for the MRIP survey.
            ###     LABIO Sampling Program (LDWF 2018) -- The raw LABIO data files provided to the SEFSC (via GulfFIN) suggest
            ###           that fork length is the preferred length type in this survey, but standard and total lengths can
            ###           also be recorded. This is supported by the 2018 metadata document, which explicitly identifies fork length
            ###           but also allows samplers to collect other (appropriate) measures (i.e., multiple references to
            ###           'millimeters fork length or appropriate measure' throughout the document).
            
            
            Original_Weight_Unit = ifelse( DS == 'MRIP', 'kg',
                                   ifelse( DS == 'TPWD',  'g',
                                   ifelse( DS %in% c('LA Creel','LA BIO'), 'kg', NA ))),
            ###     MRIP Field Manual (GSMFC 2021, p55) -- "...always take readings from the metric side in kilograms - not in pounds...
            ###           Each state will provide a minimum of two scales to FIs - large scale (maximum weight of 12.5 kg)
            ###           and a small one (maximum weight of 2.0 kg). The larger scale should only be used for fish weighing
            ###           more than the weight capacity of the smaller scale. Fish weights have to be recorded to the nearest
            ###           five one-hundredth (0.05) of a kilogram when the smaller scale is used, and the nearest tenth (0.10)
            ###           of a kilogram when the larger scale is used. In the case of fish weighing less than 0.05kg, the length
            ###           should be recorded but the weight should be left blank."
            ###     TPWD Report (MDS#257, Appendix A3) -- "Beginning on 15 May 1983, landed fish of each species from each fishing party
            ###           were counted and up to sixe randomly selected specimens were measured for total length to the nearest mm.
            ###           Standard or fork lengths to the nearest mm were taken when total lengths were not available. Weights were not
            ###           taken. [For determination of weights], standard and fork lengths were converted to total lengths [and]...
            ###           total lengths converted to whole weights in grams" using standard (TPWD-specific) conversions (Table A5, p102).
            ###     LABIO Sampling Program (LDWF 2018) -- Although never expressly stated, all weight measurements in LABIO appear to be
            ###           in kilograms as all references to 'weight' (in the 2018 metadata document) are followed by 'kilograms'...
            
            Whole_Weight = all_lbs * 453.59237,
            ###     ...which is our 'all_lbs' field converted to units of grams (as requested by the LH group)...

            Condition_Type = 'Whole (round)'
            
          ) %>%
    
    select(
      any_of( c(
            "SEDAR", "SEDAR_Date_Submit",
            "Species", "Species_ITIS", "Species_NODC", "Stock",
            "Data_Provider",
            "Fishery", "Sampling_Program", "Sample_Method_Type", "Random", "Bias_Type",
            "Sampling_Unit_ID", "PRT_CODE", "LEADER", "PSU_ID", "WP_INT",
            "Month", "Day", "Year", "WAVE",
            "SUB_REG", "State_Landed", "NEW_ST", "County_Landed", "FL_REG", "NC_REG",
            "Fishing_Mode", "NEW_MODE", "NEW_MODEN",
            "Gear_Group_Code", "NEW_GEAR", "NEW_GEARN",
            "Jurisdictional_Waters", "NEW_AREA", "NEW_AREAN",
            "Original_Length_Unit", "Observed_FL_mm", "Observed_Maximum_TL_mm", "LNGTH_IMP",
            "Original_Weight_Unit", "Whole_Weight", "all_lbs", "WGT_IMP", "Condition_Type",
            "WP_SIZE", add.strata[ !is.na(add.strata) & add.strata != 'SID' ]
      ) ) )
  
  rm( SID.flag )
  
  return( genrec.table )
  
}




