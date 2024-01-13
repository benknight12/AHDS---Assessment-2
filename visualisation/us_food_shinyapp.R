#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

## Assuming the file is saved within the visualizations folder we set working directory to 
## the next level up directory which is the overall project directory
setwd('..')
## Checks if data already loaded in session, if not loads from the clean data directory
if (!exists("clean_data")) {
  clean_data <- read.csv("clean/us_food.csv")
}
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Food"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            ## First is dropdown menu to toggle confidence intervals on and off
            selectInput("confint",
                        "Show confidence intervals?",
                        c("Yes","No"),
                        selected = "No"),
            ## Drop down menu to toggle what type of regression is shown on the plot
            selectInput("regressiontype",
                        "Would you like to show a linear or categorical model?",
                        c("Linear","Categorical"),
                        selected = "Linear"),
            ## Radio button to toggle first variable of interest
            radioButtons("radio1",
                         label = "Toggle which is the primary food model to view",
                         choices = c("Fries",
                                     "Pancakes",
                                     "Mac_and_Cheese",
                                     "Peanut_Butter",
                                     "Hamburgers",
                                     "Spareribs",
                                     "Fried_Fish",
                                     "Cake",
                                     "Cookies",
                                     "Choc_Candy"),
                         selected = "Fries",
                         inline = TRUE),
            ## Radio button to toggle second variable of interest
            radioButtons("radio2",
                         label = "1st Comparison Model",
                         choices = c("Fries",
                                     "Pancakes",
                                     "Mac_and_Cheese",
                                     "Peanut_Butter",
                                     "Hamburgers",
                                     "Spareribs",
                                     "Fried_Fish",
                                     "Cake",
                                     "Cookies",
                                     "Choc_Candy"),
                         selected = "Fries",
                         inline = TRUE),
            ## Radio button to toggle third variable of interest
            radioButtons("radio3",
                         label = "2nd Comparison Model",
                         choices = c("Fries",
                                     "Pancakes",
                                     "Mac_and_Cheese",
                                     "Peanut_Butter",
                                     "Hamburgers",
                                     "Spareribs",
                                     "Fried_Fish",
                                     "Cake",
                                     "Cookies",
                                     "Choc_Candy"),
                         selected = "Fries",
                         inline = TRUE)
        ),

        
        ## This constructs the main section of our app, combining text and plots
        mainPanel(
            strong("Study Investigating the Effect on Salary on the Amount of Different 'Junk' food eaten"),
            plotOutput("originalplot"),
            p("Our first plot investigates allows you to observe the raw data of any junk food type included in the study
              against household income. We display the raw data as box plots and fit on top a regression line of the 
              type choosen in the drop down menu (linear or categorical). The data is displated for the 'primary food'
              which is choosen in the first set of radio buttons. This theme can be investigated further in our second 
              plot. \n"),
            plotOutput("comparisonplot"),
            p("The second plot shows the regression models of food against household income for the three junk food 
              types selected using the radio buttons. The line corresponding to the selection from the first set of 
              radio buttons will be black, the second red and the third blue. This plot can be used to investigate 
              comparisons in how much of different food types are consumed accoding to an individual's salary. We 
              highlight the following trends in particular that we found to be of interest: \n"),
            tags$div(
              tags$ul(
                tags$li("The amount eaten of the following foods show a negative relationship with household income: 
                        Fries, Pancakes, Mac and Cheese, Hamburgers, Spareribs, Fried Fish and Cake. A positive trend is
                        observed for Peanut Butter, Cookies and Chocolate Candy."),
                tags$li("The least commonly eaten foods were Spareribs and Fried Fish where most common was Fries"),
                tags$li("We observe a very similar trend between Cookies and Chocolate candy, which dues to the 
                        similarity in the product is plausible."),
                tags$li("However, we also observe very similar trends in both Fried Fish and Spareribs despite not 
                        being similar products."),
                tags$li("The increase in peanut butter with household income could be explained by price, it is
                        amongst the most expensive of the foods included in our study, it also would be considered
                        healthier than the other types of junk food."),
                tags$li("The preconception that rich people eat less junk food seems to hold in general.")
              )
            ),
            p("See some further discussion in the report."),
            strong("Additional Plot to show relationship between overall JFS and household income"),
            plotOutput("jfsplot"),
            p("Here we plot the raw data for the 'Junk Food Score', in box plot form, with a model imposed on top of it. 
              This model can be toggled as linear or categorical, in both we observe very tight confidence intervals due
              to our sample size. Focussing on the linear model we observe a beta_1 parameter (gradient term for the
              regression) of -0.112 (95% CI: -0.207, -0.017), with a corresponding p-value of 0.021. Therefore we 
              conclude there is reasonable evidence that an individual with higher hosehold salary will have a lower 
              overall 'Junk Food Score'. The size of this effect seems somewhat small as some junk foods actually 
              increase with household income. You can experiment with which there are in the first two plots.")
        )
    )
)
## Server code to produce plots
server <- function(input, output) {
    ## First plot shows raw data of any junk food (in our study) against household income
    output$originalplot <- renderPlot({
      ## Establish variables in model
      xvar <- clean_data$Household_Income
      gap <-1:11
      ## Initialize 'as.factor' if categorical model is desired
      if (input$regressiontype == "Categorical"){
        xvar <- as.factor(clean_data$Household_Income)
        gap <- as.factor(gap)  
      }
      ## Define which food we model for, based of radio inpur
      clean_data$y1<- clean_data[,input$radio1]
      ## Plot the raw data
      g<- ggplot(data=clean_data,aes(x = as.factor(xvar),y=y1)) +
        geom_boxplot()+
        labs(title="Plot demonstrating the raw data and fitted model for the amount of \n the 'primary food' against salary",
             x ="Salary Category", y = "Quantity of food eaten")
      ## Create prediction data.frame using the predict function which we then plot
      predictions <- data.frame(Index = 1:11)
      predictions1 <- cbind(predictions,predict(lm(y1~xvar,data=clean_data),newdata = data.frame(xvar=gap),interval = "confidence"))
      ## Add line of predictions
      g<- g +
        geom_line(data = predictions1, mapping=aes(x = Index ,y = fit))
      ## If desired add confidence intervals
      if(input$confint == "Yes"){
        g<- g + 
          geom_ribbon(data=predictions1,aes(x = Index, ymin = lwr, ymax = upr),inherit.aes = FALSE, linetype=2, alpha=0.1,col="black")
      }
      ## Output plot
      g
    })
    ## Second plot allows for comparisons between different food's models against household 
    ## income
    output$comparisonplot <- renderPlot({
      ## Similar set up to first part just some variables require 3x of each so we can 
      ## producte up to 3 models
      xvar <- clean_data$Household_Income
      gap = 1:11
      if (input$regressiontype == "Categorical"){
        xvar <- as.factor(clean_data$Household_Income)  
        gap <- as.factor(gap)
      }
      clean_data$y1<- clean_data[,input$radio1]
      clean_data$y2<- clean_data[,input$radio2]
      clean_data$y3<- clean_data[,input$radio3]
      predictions <- data.frame(Index = 1:11)
      ## Add predictions for first linear model
      predictions1 <- cbind(predictions,predict(lm(y1~xvar,data=clean_data),newdata = data.frame(xvar=gap),interval = "confidence"))
      ## Plot predictions of first model, with CIs if desired
      g <- ggplot() +
        geom_line(data = predictions1, mapping=aes(x = Index ,y = fit))+
        labs(title="Plot demonstrating the relationships between amounts of different foods eaten and Salary",
             x ="Salary Category", y = "Quantity of food eaten")
      if(input$confint == "Yes"){
        g<- g + 
          geom_ribbon(data=predictions1,aes(x = Index, ymin = lwr, ymax = upr),inherit.aes = FALSE, linetype=2, alpha=0.1,col="black")
      }
      ## Now generate predictions and add plot for second food if different to 1st
      if (input$radio1 != input$radio2){
        predictions2 <- cbind(predictions,predict(lm(y2~xvar,data=clean_data),newdata = data.frame(xvar=gap),interval = "confidence"))
        g <- g +
          geom_line(data = predictions2,mapping=aes(x = Index ,y = fit),col="red")
        if(input$confint == "Yes"){
          g<- g + 
            geom_ribbon(data=predictions2,aes(x = Index, ymin = lwr, ymax = upr),inherit.aes = FALSE, linetype=2, alpha=0.1, fill = 'red',col="red")
        }
      }
      ## Now generate predictions and add plot for third food if different to 1st and 2nd
      if (input$radio2 != input$radio3 && input$radio1 != input$radio3){
          predictions3 <- cbind(predictions,predict(lm(y3~xvar,data=clean_data),newdata = data.frame(xvar=gap),interval = "confidence"))
          g<- g + 
            geom_line(data = predictions3,mapping=aes(x = Index ,y = fit),col="blue")
          
          if(input$confint == "Yes"){
            g<- g + 
              geom_ribbon(data=predictions3,aes(x = Index, ymin = lwr, ymax = upr),inherit.aes = FALSE, linetype=2, alpha=0.1, fill = 'blue',col="blue")
          }
      }
      ## Output plot
      g
    })
    ## Final plot shows the relationship betweren overall JFS and household income
    output$jfsplot <- renderPlot({
      ## Again similar set up of variables
      xvar <- clean_data$Household_Income
      gap <-1:11
      if (input$regressiontype == "Categorical"){
        xvar <- as.factor(clean_data$Household_Income)
        gap <- as.factor(gap)  
      }
      ## Produce baseline boxplot of jfs data, grouped by household income
      j<- ggplot(data=clean_data,aes(x = as.factor(xvar),y=jfs)) +
        geom_boxplot()+
        labs(title="Plot demonstrating the raw data and fitted model for the amount \n of the 'primary food' against salary",
             x ="Salary Category", y = "Quantity of food eaten")
      ## Create prediction data frame to fit model
      predictions <- data.frame(Index = 1:11)
      predictions1 <- cbind(predictions,predict(lm(jfs~xvar,data=clean_data),newdata = data.frame(xvar=gap),interval = "confidence"))
      ## Some values which are used for write up but don't want to display
      # model <- lm(jfs~xvar, data=clean_data)
      # summary(model)
      ## Produce plot with fitted values plotted ontop of boxplots
      j<- j +
        geom_line(data = predictions1, mapping=aes(x = Index ,y = fit),col="red")
      if(input$confint == "Yes"){
        j<- j + 
          geom_ribbon(data=predictions1,aes(x = Index, ymin = lwr, ymax = upr),inherit.aes = FALSE, linetype=2, alpha=0.1,col="red")
      }
      ## Output plot
      j
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

