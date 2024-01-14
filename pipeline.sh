## This pipeline runs our r scripts

## Due to issues of using renv when in the environment folder, pipeline 
## moves it out to use it then returns it
mv environment/renv .
mv environment/renv.lock .
## Perform data cleaning
Rscript ./code/data_preparation.R
## Produce and save viualisations
Rscript ./code/static_vis_code.R
## Return renv
mv renv environment/renv/
mv renv.lock environment/
