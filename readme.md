# Readme File for AHDS Assessment 2

The objective of this project is to observe associations between Household Income and Junk Food, and then the effect of junk food on BMI, using data from the National Health and Nutrition Examination Survey (NHANES). We create a pipeline in bash which can be run in terminal to conduct the analysis. This will perform data preparation and produce static plots which we include in a report outlining methods and findings. We also produce a ShinyApp visualization that can be run and viewed seperately. 

**Elements of the Compendium**
- *'raw'* - Contains the raw .csv data provided from NHANES.
- *'clean'* - The dataset which we produce in the *'data_preperation.R'* that is used to conduct our analysis.
- *'code'* - Includes 2 R files: *'data_preperation.R'* extracts and cleans the required data, it saves these files to *'clean'*; and *'static_vis_code.R'* which takes our clean data and conducts the anaylsis to produce the static plots for our report - it saves these to the *'visualisation'* directory.
- *'visualisation'* - We store static plots and an R file for producing the shiny app. To run the shiny app load the R file in Rstudio and click 'runApp', or open the script in R and run in its entirety. You can use the radio buttons and drop down menus to customize the plots.
- *'environment'* - Includes *'renv'* and *'renv.lock'* which store information on the research environment for the code to run in, the packages and versions required etc.
- *'pipeline'* - This is a bash script to be excecuted in terminal to conduct the analysis  we have outlined.

Please run the pipeline in terminal, and the shinyapp seperately, to view the results and a commentary on the process.