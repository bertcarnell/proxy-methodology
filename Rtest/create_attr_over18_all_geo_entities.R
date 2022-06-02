# * This script uses the base information from the census flat files for block group, tract, and ZIP code and allocates "Some Other Race"
# * to each group in proportion.  It creates three files (one each for block group, tract, and ZIP code) containing the geography-only
# * proxy as well as the proportion of population for a given race and ethnicity residing in a given geographic area, which is 
# * used to build the BISG proxy.   
# 
# set more off
# set type double
# 
# * Set input and output directories.
# local indir = "../input_files/"
# local outdir = "../input_files/created/"

require(dplyr)
require(foreign)
require(assertthat)

create_attr_over18_all_geo_entities <- function()
{
  indir <- file.path("input_files")
  outdir <- file.path("input_files", "created")
  
  # * Set names for geo entity files.
  # local geo_files = "blkgrp tract zip"
  
  geo_files <- c("blkgrp", "tract", "zip")
  
  # foreach file in `geo_files'{
  #    use "`indir'`file'_over18_race_dec10.dta", clear
  #    	gen file = "`file'"
  for (geo_file in geo_files)
  {
    # geo_file <- "zip"
    
    X <- foreign::read.dta(file.path(indir, paste0(geo_file, "_over18_race_dec10.dta")))
    # * Step 1: From the SF1, retain population counts for the contiguous U.S., Alaska, and Hawaii in order to ensure consistency with the population
    # * covered by the census surname list.
    # drop if State_FIPS10 == "72"
    
    X <- X %>%
      dplyr::filter(State_FIPS10 != "72")
    
    # if "`file'" == "zip" {
    #   drop if inlist(substr(ZCTA5,1,3),"006","007","008","009")
    # }
    if (geo_file == "zip")
    {
      X <- X %>%
        dplyr::filter(!(substr(ZCTA5, 1, 3) %in% c("006", "007", "008", "009")))
    }
    
    # * Step 2: Address "Other" category from 2010 Census; what is done here follows Word(2008).
    # foreach x in NH_White NH_Black NH_AIAN NH_API {
    #   replace `x'_alone = `x'_alone + `x'_Other
    # }
    
    # * Census breaks out Asian and PI separately; since we consider them as one, we correct for this.
    # replace NH_API_alone = NH_API_alone + NH_Asian_HPI + NH_Asian_HPI_Other
    
    # * Replace multiracial total to account for the fact that we have suppressed the Other category.
    # replace NH_Mult_Total = NH_Mult_Total - (NH_White_Other + NH_Black_Other + NH_AIAN_Other + NH_Asian_HPI + NH_API_Other + NH_Asian_HPI_Other)
    
    X <- X %>%
      dplyr::mutate(NH_White_alone = NH_White_alone + NH_White_Other,
                    NH_Black_alone = NH_Black_alone + NH_Black_Other,
                    NH_AIAN_alone = NH_AIAN_alone + NH_AIAN_Other,
                    NH_API_alone = NH_API_alone + NH_API_Other + NH_Asian_HPI + NH_Asian_HPI_Other,
                    NH_Mult_Total = NH_Mult_Total - (NH_White_Other + NH_Black_Other + NH_AIAN_Other + NH_Asian_HPI + NH_API_Other + NH_Asian_HPI_Other))
    
    # * Verify the steps above by confirming that the Total Population still matches.
    # assert Total_Pop == (NH_White_alone + NH_Black_alone + NH_API_alone + NH_AIAN_alone + NH_Mult_Total + NH_Other_alone + Hispanic_Total)
    
    assertthat::assert_that(with(X, all(Total_Pop == (NH_White_alone + NH_Black_alone + NH_API_alone + NH_AIAN_alone + NH_Mult_Total + NH_Other_alone + Hispanic_Total))),
                            msg = "Error in the race redistribution step")
    
    #   * Step 3: Proportionally redistribute Non-Hispanic Other population to remaining Non-Hispanic groups within each block.
    #   foreach x in NH_White_alone NH_Black_alone NH_AIAN_alone NH_API_alone NH_Mult_Total {
    #      replace `x' = `x' + (`x' / (Total_Pop-Hispanic_Total-NH_Other_alone)) * NH_Other_alone
    #      replace `x' = 0 if Total_Pop == 0
    # 	   replace `x' = NH_Other_alone / 5 if Non_Hispanic_Total == NH_Other_alone
    #   }
    #   
    #   egen pop_check = rowtotal(NH_White_alone NH_Black_alone NH_AIAN_alone NH_API_alone NH_Mult_Total Hispanic_Total)
    #   
    #   assert round(pop_check,1) == Total_Pop
    #   
    #   preserve
    
    redistribute <- function(x, Total_Pop, Hispanic_Total, NH_Other_alone, Non_Hispanic_Total)
    {
      temp <- x + (x / (Total_Pop - Hispanic_Total - NH_Other_alone)) * NH_Other_alone
      temp <- ifelse(Total_Pop == 0, 0, temp)
      temp <- ifelse(Non_Hispanic_Total == NH_Other_alone, NH_Other_alone / 5, temp)
      return(temp)
    }
    
    X <- X %>%
      dplyr::mutate(NH_White_alone = redistribute(NH_White_alone, Total_Pop, Hispanic_Total, NH_Other_alone, Non_Hispanic_Total),
                    NH_Black_alone = redistribute(NH_Black_alone, Total_Pop, Hispanic_Total, NH_Other_alone, Non_Hispanic_Total),
                    NH_AIAN_alone = redistribute(NH_AIAN_alone, Total_Pop, Hispanic_Total, NH_Other_alone, Non_Hispanic_Total),
                    NH_API_alone = redistribute(NH_API_alone, Total_Pop, Hispanic_Total, NH_Other_alone, Non_Hispanic_Total),
                    NH_Mult_Total = redistribute(NH_Mult_Total, Total_Pop, Hispanic_Total, NH_Other_alone, Non_Hispanic_Total),
                    pop_check = NH_White_alone + NH_Black_alone + NH_AIAN_alone + NH_API_alone + NH_Mult_Total + Hispanic_Total)
    
    assertthat::assert_that(with(X, all(round(pop_check, 1) == Total_Pop)))
    
    # * Collapse dataset to get Population Totals for each group.
    # collapse (sum) NH_White_alone NH_Black_alone NH_AIAN_alone NH_API_alone NH_Mult_Total Hispanic_Total Total_Pop
    # 
    # local national_pop = Total_Pop
    # local national_nh_white_alone = NH_White_alone
    # local national_nh_black_alone = NH_Black_alone
    # local national_nh_aian_alone = NH_AIAN_alone
    # local national_nh_asian_alone = NH_API_alone
    # local national_nh_hawn_alone = 0
    # local national_hispanic_pop = Hispanic_Total
    # * End constants.
    # 
    # restore
    # gen geo_pr_white = NH_White_alone / Total_Pop
    # gen geo_pr_black = NH_Black_alone / Total_Pop
    # gen geo_pr_aian = NH_AIAN_alone / Total_Pop
    # gen geo_pr_api = NH_API_alone / Total_Pop
    
    # * Multiple races or "some other race" (and not Hispanic).
    # gen geo_pr_mult_other = (NH_Mult_Total) / Total_Pop
    # gen geo_pr_hispanic = Hispanic_Total / Total_Pop
    
    # * When updating geocoded race probabilities, we require the probability that someone of a particular race lives in that block group, tract, or ZIP code. 
    # * Our race counts are single race reported counts, therefore we divide the single race population within each block by the total single race population
    # * for each group.
    
    # local national_nh_mult_other = `national_pop' - `national_hispanic_pop' - `national_nh_white_alone' - `national_nh_black_alone' - `national_nh_aian_alone' - `national_nh_asian_alone' - `national_nh_hawn_alone'
    #   n di "Number of other-race or multiple-race non-Hispanics: `national_nh_mult_other'"
    
    # local national_nh_api_alone = `national_nh_asian_alone' + `national_nh_hawn_alone'
    # 
    # gen here = Total_Pop / `national_pop'
    # gen here_given_white = NH_White_alone / `national_nh_white_alone'
    # gen here_given_black = NH_Black_alone / `national_nh_black_alone'
    # gen here_given_aian = NH_AIAN_alone / `national_nh_aian_alone'
    # gen here_given_api = NH_API_alone / `national_nh_api_alone'
    # gen here_given_mult_other = (NH_Mult_Total) / `national_nh_mult_other'
    # gen here_given_hispanic = Hispanic_Total / `national_hispanic_pop'
    
    totals <- X %>%
      dplyr::summarize(national_pop = sum(Total_Pop),
                       national_nh_white_alone = sum(NH_White_alone),
                       national_nh_black_alone = sum(NH_Black_alone),
                       national_nh_aian_alone = sum(NH_AIAN_alone),
                       national_nh_asian_alone = sum(NH_API_alone),
                       national_nh_hawn_alone = 0,
                       national_hispanic_pop = sum(Hispanic_Total),
                       national_nh_mult_other = national_pop - national_hispanic_pop - national_nh_white_alone - national_nh_black_alone - national_nh_aian_alone - national_nh_asian_alone - national_nh_hawn_alone,
                       national_nh_api_alone = national_nh_asian_alone + national_nh_hawn_alone)
    
    print(paste("Number of other-race or multiple-race non-Hispanics:", totals$national_nh_mult_other))
    
    X <- X %>%
      dplyr::mutate(geo_pr_white = NH_White_alone / Total_Pop,
                    geo_pr_black = NH_Black_alone / Total_Pop,
                    geo_pr_aian = NH_AIAN_alone / Total_Pop,
                    geo_pr_api = NH_API_alone / Total_Pop,
                    geo_pr_mult_other = NH_Mult_Total / Total_Pop,
                    geo_pr_hispanic = Hispanic_Total / Total_Pop,
                    here = Total_Pop / totals$national_pop,
                    here_given_white = NH_White_alone / totals$national_nh_white_alone,
                    here_given_black = NH_Black_alone / totals$national_nh_black_alone,
                    here_given_aian = NH_AIAN_alone / totals$national_nh_aian_alone,
                    here_given_api = NH_API_alone / totals$national_nh_api_alone,
                    here_given_mult_other = NH_Mult_Total / totals$national_nh_mult_other,
                    here_given_hispanic = Hispanic_Total / totals$national_hispanic_pop)
    
    
    
    
    # if file == "blkgrp"{
    #   rename GEOID10_BlkGrp GeoInd
    # }
    # if file == "tract"{
    #   rename GEOID10_Tract GeoInd
    # }
    # if file=="zip"{
    #   rename ZCTA5 GeoInd
    # }     
    
    if (geo_file == "blkgrp")
    {
      X <- X %>% dplyr::rename(GeoInd = GEOID10_BlkGrp)
    } else if (geo_file == "tract")
    {
      X <- X %>% dplyr::rename(GeoInd = GEOID10_Tract)
    } else if (geo_file == "zip")
    {
      X <- X %>% dplyr::rename(GeoInd = ZCTA5)
    }
    
    # keep GeoInd geo_pr* here*
    #   compress
    # save "`outdir'`file'_attr_over18.dta", replace
    
    save(X, file = file.path(outdir, paste0(geo_file, "_attr_over18.Rdata")))
  }
}

