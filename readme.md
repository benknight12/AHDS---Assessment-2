# Readme File for AHDS Assessment 2

The objective of this project is to observe associations between Household Income and Junk Food, and consequently the effect on BMI, found in data from the National Health and Nutrition Examination Survey (NHANES). We create a pipeline in SnakeMake which can be run in terminal to conduct the analysis. This will produce static plots and a ShinyApp visualization that can be run and viewed seperately. We conclude the project with a written report to describe our methods and findings.

**Elements of the Compendium**
- *'raw'* - This contains the raw .csv data provided from NHANES
- *'clean'* - The datasets which we produce in the 'data_preperation.R' document that we will use to conduct our analysis
- *'code'* - We have 2 R files: *'data_preperation.R'* extracts and cleans the required data, it saves these files to *'clean'*; and *'static_vis_code.R'* which takes our clean data and conducts the anaylsis to produce the static plots for our report - it saves these to the *'visualisations'* folder
- *'visualisations'* - We store static plots and an R file for producing the shiny app. To run this load the R file in Rstudio and click 'runApp', or open the script in R and run in its entirety. You can use the radio buttons and drop down menus to customize the plots.
- *'renv'* and *'renv.lock'* - These store information on the research environment for the code to run in, the packages and versions required etc.
- *'pipeline'* - This can be excecuted in terminal to produce conduct the steps we have explained.

Please run the pipeline in terminal, and the shinyapp seperately, to view the results and a commentary on the process.