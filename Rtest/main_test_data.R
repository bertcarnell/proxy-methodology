# * This Stata script executes a series of Stata scripts and subroutines that
# * prepare input public use census geography and surname data
# * and constructs the surname-only, geography-only, and BISG proxies for
# * race and ethnicity.
# *
# * This file is set up to execute the proxy building code sequence on a set of
# * fictitious data constructed by create_test_data.do from the publicly available census surname
# * list and geography data. It is provided to illustrate how the main.do
# * is set up to run the proxy building code.

# set more off
# set trace off
# set type double 
# set mem 12g

#* Identify the input directory that contains the individual or application level data containing name and geocodes.
sourcedir <- file.path("output")

#* Identify the output directory for processing.
outdir <- file.path("output")

#* Identify the location of the prepared input census files.
censusdata <- file.path("input_files", "created")

# * Run the script that prepares the analysis version of the census surname list,
# * including the proportions of individuals by race and ethnicity by surname.
# do "surname_creation_lower.do"
source(file.path("Rtest", "surname_creation_lower.R"))
surname_creation_lower()

# * Run the script that prepares the analysis version of the census geography data files.
# do "create_attr_over18_all_geo_entities.do"
source(file.path("Rtest", "create_attr_over18_all_geo_entities.R"))
create_attr_over18_all_geo_entities()

source(file.path("Rtest", "create_test_data.R"))
create_test_data()

# * Read in the file that defines the program "name_parse" that contains the name standardization routines and merges surname probabilities
# * from the census surname list.
# * See script for details on arguments that need to be supplied to the program.
# do "surname_parser.do"
source(file.path("Rtest", "surname_parser.R"))

* Execute name_parse.
name_parse, matchvars(rownum) app_lname(name1) coapp_lname(name2) maindir(`outdir') readdir(`sourcedir') readfile(fictitious_sample_data) censusdir(`censusdata') keepvars()

* Read in the file (for each level of geography) that defines the program "geo_parse" that merges the precalculated geographic 
* and name probabilities and generates the Bayesian probability and run program.
* See script for details on arguments that need to be supplied to the program.
do "geo_name_merger_all_entities_over18.do"
* Execute geo_parse to construct block group, tract, and ZIP code based BISG probabilities.
geo_parse, matchvars(rownum) maindir(`outdir') readdir(`sourcedir') readfile(fictitious_sample_data) geodir(`sourcedir') geofile(fictitious_sample_data) inst_name(test) censusdir(`censusdata') geo_ind_name(GEOID10_BlkGrp) geo_switch(blkgrp)
geo_parse, matchvars(rownum) maindir(`outdir') readdir(`sourcedir') readfile(fictitious_sample_data) geodir(`sourcedir') geofile(fictitious_sample_data) inst_name(test) censusdir(`censusdata') geo_ind_name(GEOID10_Tract) geo_switch(tract)
geo_parse, matchvars(rownum) maindir(`outdir') readdir(`sourcedir') readfile(fictitious_sample_data) geodir(`sourcedir') geofile(fictitious_sample_data) inst_name(test) censusdir(`censusdata') geo_ind_name(ZCTA5) geo_switch(zip)

* Read in file that defines the program "combine_probs" that merges together the block group, tract, and zip based BISG proxies and 
* chooses the most precise proxy given the precision of geocoding.
* See script for details on arguments that need to be supplied to the function.
do "combine_probs.do"
* Execute combine_bisg.
combine_bisg, matchvars(rownum) maindir(`outdir') geodir(`sourcedir') geofile(fictitious_sample_data) geoprecvar(geo_code_precision) inst_name(test)




