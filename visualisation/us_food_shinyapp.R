#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
setwd('..')
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
                         inline = TRUE),
            
            selectInput("confint",
                        "Show confidence intervals?",
                        c("Yes","No"),
                        selected = "No"),
            
            selectInput("regressiontype",
                        "Would you like to show a linear or categorical model?",
                        c("Linear","Categorical"),
                        selected = "Linear")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            strong("Study Investingating the Effect on Salary on the Amount of Different 'Junk' food eaten"),
            plotOutput("originalplot"),
            plotOutput("comparisonplot"),
        )
    )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
    library(ggplot2)
    output$originalplot <- renderPlot({
      xvar <- clean_data$Household_Income
      gap <-1:11
      if (input$regressiontype == "Categorical"){
        xvar <- as.factor(clean_data$Household_Income)
        gap <- as.factor(gap)  
      }
      clean_data$y1<- clean_data[,input$radio1]
      g<- ggplot(data=clean_data,aes(x = as.factor(xvar),y=y1)) +
        geom_boxplot()+
        labs(title="Plot demonstrating the raw data and fitted model for the amount of the 'primary food' against salary",
             x ="Salary Category", y = "Quantity of food eaten")
      predictions <- data.frame(Index = 1:11)
      predictions1 <- cbind(predictions,predict(lm(y1~xvar,data=clean_data),method=binomial(family="logit"),newdata = data.frame(xvar=gap),interval = "confidence"))
      g<- g +
        geom_line(data = predictions1, mapping=aes(x = Index ,y = fit))
      if(input$confint == "Yes"){
        g<- g + 
          geom_ribbon(data=predictions1,aes(x = Index, ymin = lwr, ymax = upr),inherit.aes = FALSE, linetype=2, alpha=0.1,col="black")
      }
      g
    })
    output$comparisonplot <- renderPlot({
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
      predictions1 <- cbind(predictions,predict(lm(y1~xvar,data=clean_data),method=binomial(family="logit"),newdata = data.frame(xvar=gap),interval = "confidence"))
      predictions2 <- cbind(predictions,predict(lm(y2~xvar,data=clean_data),method=binomial(family="logit"),newdata = data.frame(xvar=gap),interval = "confidence"))
      predictions3 <- cbind(predictions,predict(lm(y3~xvar,data=clean_data),method=binomial(family="logit"),newdata = data.frame(xvar=gap),interval = "confidence"))
      g <- ggplot() +
        geom_line(data = predictions1, mapping=aes(x = Index ,y = fit))+
        labs(title="Plot demonstrating the relationships between amounts of different foods eaten and Salary",
             x ="Salary Category", y = "Quantity of food eaten")
      if(input$confint == "Yes"){
        g<- g + 
          geom_ribbon(data=predictions1,aes(x = Index, ymin = lwr, ymax = upr),inherit.aes = FALSE, linetype=2, alpha=0.1,col="black")
      }
        
      if (input$radio1 != input$radio2){
        g <- g +
          geom_line(data = predictions2,mapping=aes(x = Index ,y = fit),col="red")
        if(input$confint == "Yes"){
          g<- g + 
            geom_ribbon(data=predictions2,aes(x = Index, ymin = lwr, ymax = upr),inherit.aes = FALSE, linetype=2, alpha=0.1, fill = 'red',col="red")
        }
      }
      if (input$radio2 != input$radio3 && input$radio1 != input$radio3){
          g<- g + 
            geom_line(data = predictions3,mapping=aes(x = Index ,y = fit),col="blue")
          if(input$confint == "Yes"){
            g<- g + 
              geom_ribbon(data=predictions3,aes(x = Index, ymin = lwr, ymax = upr),inherit.aes = FALSE, linetype=2, alpha=0.1, fill = 'blue',col="blue")
          }
      }
      g
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

