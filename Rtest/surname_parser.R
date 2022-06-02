# * This program is called from an external script, creates the surname list from the input data, and standardizes names.
# *
# * Input arguments:
# *
# * matchvars() - unique record identifier
# * app_lname() - applicant last name variable
# * coapp_lname() -  coapplicant last name variable
# * maindir() - output file directory
# * readdir() - directory containing individual or application data
# * readfile() - individual or application data file name containing surname data
# * censusdir() - directory containing prepared input census geography and surname data
# * keepvars() - variables to keep from input dataset (can leave missing to keep all variables)
# 
# capture program drop name_parse
# program define name_parse, byable(recall)
#     version 11
#     syntax [if] [in] [, matchvars(string) app_lname(string) coapp_lname(string) maindir(string) readdir(string) readfile(string) censusdir(string) keepvars(string)]

#delimit cr

require(assertthat)
require(dplyr)

name_parse <- function(matchvars, app_lname, coapp_lname = NA, maindir, readdir, readfile, censusdir, keepvars = NULL)
{
  # matchvars <- "rownum"
  # app_lname <- "name1"
  # coapp_lname <- "name2"
  # maindir <- "test_output"
  # readdir <- "test_output"
  # readfile <- "fictitious_sample_data.Rdata"
  # censusdir <- file.path("input_files", "created")
  # keepvars <- NULL
  
  # timer off 1
  # noisily di "1. Read files in"
  # timer list
  # n di r(t1)
  # timer clear 1
  # timer on 1
  print("1. Read files in")
  print(Sys.time())
  
  # use "`readdir'/`readfile'.dta", clear
  load(file.path(readdir, readfile))
  assertthat::assert_that("t5" %in% ls())
  
  # * Reformat to create a separate record for each applicant and coapplicant and drop middle initials.
  # 
  # keep `matchvars'  `coapp_lname' `keepvars'
  
  # drop if mi(`coapp_lname')
  # 
  # rename `matchvars' appid
  # rename `coapp_lname' lname
  # 
  # * Generating an applicant/coapplicant indicator for ease of identification post-merge.
  # gen appl_coapp_cd_enum = "C"
  # save "`maindir'/coapps.dta", replace
  Xco <- t5[, c(matchvars, coapp_lname, keepvars)] %>%
    dplyr::rename(appid = dplyr::sym(matchvars),
                  lname = dplyr::sym(coapp_lname)) %>%
    dplyr::filter(!is.na(lname))%>%
    dplyr::mutate(appl_coapp_cd_enum = "C")
  
  # use "`readdir'/`readfile'.dta", clear
  # keep `matchvars' `app_lname' `keepvars'
  Xapp <- t5[, c(matchvars, app_lname, keepvars)]
  
  # count if mi(`app_lname')
  # if r(N)>0{
  #   n di "NOTE: `r(N)' applications had no last name for the main applicant!"
  # }
  
  if (length(which(is.na(Xapp$app_lname))) > 0)
  {
    print(paste("NOTE:", length(which(is.na(Xapp$app_lname))), "applications had no last name for the main applicant!"))
  }
  
  # rename `matchvars' appid
  # rename `app_lname' lname

  # * Generating an applicant/coapplicant indicator for ease of identification post-merge (see line 28).
  # gen appl_coapp_cd_enum = "A"

  Xapp <- Xapp %>%
    dplyr::rename(appid = dplyr::sym(matchvars),
                  lname = dplyr::sym(app_lname)) %>%
    dplyr::mutate(appl_coapp_cd_enum = "A")
  
  Xapps <- rbind(Xapp, Xco)
  
  # append using "`maindir'/coapps.dta"
  save(Xapps, file = file.path(maindir, "coapps.Rdata"))
  
  # timer off 1
  # noisily di "2. Reformatted data in "
  # timer list
  # n di r(t1)
  # timer clear 1
  # timer on 1

  print("2. Reformatted data in")
  print(Sys.time())

  # /*
  #   Clean last names.
  # */
  #   
  # timer on 1
  
  # replace lname = lower(lname)
  # replace lname = " "+lname+" "
  
  # * Replace common non-letter, non-hyphen characters with spaces.
  # gen regexremaining = 1
  # local anyremaining = 1
  # while `anyremaining' == 1{
  #   replace lname = regexr(lname,"[\`\{}\\,.0-9]"," ") if regexremaining == 1
  #   replace lname = regexr(lname,"[']","") if regexremaining == 1
  #   *Deals with apostrophes separately in order to avoid potential problems with names like D'Angelo, O'Leary, etc., having the first letter dropped.
  #   replace regexremaining = regexm(lname,"[\`\'{}\\,.0-9]") if regexremaining == 1
  #   sum regexremaining, meanonly
  #   local anyremaining = r(max)
  #   }
  # drop regexremaining

  # * Remove double quotes.
  # replace lname = subinstr(lname, char(34), " ", .)

  Xapps <- Xapps %>%
    dplyr::mutate(lname = paste0(" ", tolower(lname), " ")) %>%
    dplyr::mutate(lname = gsub("[`{}\\,.0-9]", " ", lname)) %>%
    dplyr::mutate(lname = gsub("[']", "", lname)) %>%
    dplyr::mutate(lname = gsub('["]', "", lname))
  
  # * Replace common suffixes with spaces.
  # foreach k in jr sr ii iii iv dds md phd {
  #   replace lname = subinstr(lname," `k' "," ",.)
  # }
  for (i in c("jr", "sr", "ii", "iii", "iv", "dds", "md", "phd"))
  {
    Xapps$lname <- gsub(paste0(" ", i, " "), "", Xapps$lname)
  }
  
  # * Any lone letters in lname are most likely initials (in most cases, middle initials); remove them unless they are the letters "o" or "d".
  # gen regexremaining = 1
  # local anyremaining = 1
  # while `anyremaining' == 1{
  #   replace lname = regexr(lname," [a-ce-np-z] ","") if regexremaining == 1
  #   replace regexremaining = regexm(lname," [a-ce-np-z] ") if regexremaining == 1
  #   sum regexremaining, meanonly
  #   local anyremaining = r(max)
  #   }
  # * Clean up lone letters "o" or "d".
  #    replace lname = "" if strtrim(lname) == "o" | strtrim(lname) == "d"
  # drop regexremaining
  # * Remove all spaces.
  # replace lname = subinstr(lname," ","",.)
  
  Xapps <- Xapps %>%
    dplyr::mutate(lname = gsub(" [a-ce-np-z] ", "", lname)) %>%
    dplyr::mutate(lname = ifelse(trimws(lname) == "o" | trimws(lname) == "d", "", lname)) %>%
    dplyr::mutate(lname = trimws(lname))

    
  # timer off 1
  # n di "3. Cleaned lname in"
  # 
  # timer list
  # n di r(t1)
  # timer clear 1
  
  print("3. Cleaned lname in")
  print(Sys.time())
  
  # * Split hyphenated last names, then match race separately on each part.
  # gen lname2 = ""
  # 
  # timer on 1
  # replace lname = subinstr(lname,"-"," ",.)
  # replace lname2 = word(lname,2)
  # replace lname = word(lname,1)
  # 
  # rename lname lname1

  temp <- strsplit(Xapps$lname, "-")
  Xapps$lname1 <- sapply(temp, function(x) x[1])
  Xapps$lname2 <- sapply(temp, function(x) if (length(x) > 1) return(x[2]) else return(""))

  # timer off 1
  # noisily di "4. Processed hyphens in"
  # timer list
  # n di r(t1)
  # timer clear 1
  # 
  # timer on 1

  print("4. Processed hypens in")
  print(Sys.time())
  
  
  # * Merge in each name's race probabilities.
  # 
  # timer on 1
  # 
  # forvalues i = 1/2{
  #     rename lname`i' name
  #   merge m:1 name using "`censusdir'/census_surnames_lower", keep(match master) keepusing(pctwhite pctblack pctapi pctaian pcthispanic pct2prace)
  #   drop _merge
  #   rename name lname`i'
  # 
  #     foreach race in white black api aian 2prace hispanic{
  #         rename pct`race' pct`race'`i'
  #     }
  # }
  # 
  # compress
  # save "`maindir'/race_probs_by_person.dta", replace

  load(file.path(censusdir, "census_surnames_lower.Rdata"))
  Xapps <- Xapps %>%
    dplyr::left_join(surnames_2010_new %>% dplyr::select(name, pctwhite, pctblack, pctapi, pctaian, pct2prace, pcthispanic), by = c("lname1" = "name")) %>%
    dplyr::rename_with(.fn = function(x) paste0(x, "1"), .cols = c(pctwhite, pctblack, pctapi, pctaian, pct2prace, pcthispanic)) %>%
    dplyr::left_join(surnames_2010_new %>% dplyr::select(name, pctwhite, pctblack, pctapi, pctaian, pct2prace, pcthispanic), by = c("lname2" = "name")) %>%
    dplyr::rename_with(.fn = function(x) paste0(x, "2"), .cols = c(pctwhite, pctblack, pctapi, pctaian, pct2prace, pcthispanic))
  
  save(Xapps, file = file.path(maindir, "race_probs_by_person.Rdata"))

  # timer off 1
  # noisily di "5. Matched race probabilities in"
  # timer list
  # n di r(t1)
  # timer clear 1
  # timer on 1
  
  print("5. Matched race probabilities in")
  print(Sys.time())

  # * Reorganize data in the format:
  #   * appid a_lname1 a_lname2 c_lname1 c_lname2
  # 
  # keep appid lname1 lname2 appl_coapp_cd_enum pct* `keepvars'
  # keep if appl_coapp_cd_enum == "C"
  # drop appl_coapp_cd_enum
  # foreach x of varlist _all{
  # 	rename `x' c_`x'
  # }
  # rename c_appid appid
  # foreach z in `keepvars' {
  #   rename c_`z' `z'
  # }
  # 
  # compress
  # save "`maindir'/coapps2.dta", replace
  
  Xco <- Xapps %>%
    dplyr::filter(appl_coapp_cd_enum == "C") %>%
    dplyr::select(!matches("appl_coapp_cd_enum")) %>%
    dplyr::rename_with(.fn = function(x) paste("c_", x), .cols = !matches("appid"))
  
  save(Xco, file = file.path(maindir, "coapps2.Rdata"))

  # use "`maindir'/race_probs_by_person.dta", clear
  
  # keep appid lname1 lname2 appl_coapp_cd_enum pct* `keepvars'
  # keep if appl_coapp_cd_enum == "A"
  # drop appl_coapp_cd_enum
  # foreach x of varlist _all{
  # 	rename `x' a_`x'
  # }
  # rename a_appid appid
  # foreach z in `keepvars' {
  #   rename a_`z' `z'
  # }
  # 
  # merge 1:1 appid using "`maindir'/coapps2.dta"
  # drop _merge
  Xapp <- Xapps %>%
    dplyr::filter(appl_coapp_cd_enum == "A") %>%
    dplyr::select(!matches("appl_coapp_cd_enum")) %>%
    dplyr::rename_with(.fn = function(x) paste("a_", x), .cols = !matches("appid"))
  
}



timer off 1
noisily di "6. Reorganized data in"
timer list
n di r(t1)
timer clear 1
timer on 1

* Each namematch variable is set to 1 if we matched the given name to the Census file and the name is not a duplicate of a previous name on the application.
* If the joint applicants share a name, this name is not providing new information (it is likely a family member), and all additional instances of 
* the name should be discarded.

foreach k in a c {
    forvalues i = 1/2{
        gen namematch_`k'`i' = !mi(`k'_pctwhite`i')
    }
}

replace namematch_c2 = 0 if inlist(c_lname2,a_lname1, a_lname2, c_lname1)
replace namematch_c1 = 0 if inlist(c_lname1,a_lname1, a_lname2)
replace namematch_a2 = 0 if a_lname2 == a_lname1
gen namematch_any = namematch_c2 != 0 | namematch_c1 !=0 | namematch_a2 != 0 | namematch_a1 != 0

local races "hispanic white black api aian 2prace"

foreach k in a c {
    forvalues i = 1/2{
        foreach race of local races{
            replace `k'_pct`race'`i' = 0 if namematch_`k'`i' == 0
        }
    }
}

timer off 1
noisily di "7. Set up namematch variables in"
timer list
n di r(t1)
timer clear 1
timer on 1

foreach race of local races{
* Denominator below should be approximately equal to 1. It is added to reduce rounding errors.
    gen post_pr_`race' = .

    replace post_pr_`race' = a_pct`race'1 if namematch_a1 == 1
    replace post_pr_`race' = a_pct`race'2 if namematch_a2 == 1 & mi(post_pr_`race')
    replace post_pr_`race' = c_pct`race'1 if namematch_c1 == 1 & mi(post_pr_`race')
    replace post_pr_`race' = c_pct`race'2 if namematch_c2 == 1 & mi(post_pr_`race')

}

timer off 1
noisily di "8. Populated final surname probability based on availability of applicant and coapplicant name"
timer list
n di r(t1)
timer clear 1
timer on 1

drop namematch* a_* c_*
rename appid `matchvars'
save "`maindir'/proxy_name.dta", replace

* Remove intermediate files.
erase "`maindir'/coapps.dta"
erase "`maindir'/coapps2.dta"
erase "`maindir'/race_probs_by_person.dta"

timer off 1
noisily di "Finished cleanup in"
timer list
n di r(t1)
timer clear 1

di "`timestart'"
di c(current_time)

timer off 2
noisily di "Finished program in"
timer list
n di r(t2)
timer clear 2

end

*END



