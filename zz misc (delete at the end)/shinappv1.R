#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Juice Data Plots"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("Foodtype",
                        "What food do you want to see data for?",
                        c("Lettuce","French Fries","Salsa","Pickles","Pancakes","Mac and Cheese")),

            selectInput("Regtype",
                        "Categorical or Numeric EV in Regression model",
                        c("Categorical","Numeric")),
            checkboxInput("Study",
                          "Which study would you like to view",
                          c("BMI Study","Length in US Study")),
            selectInput("Regtype2",
                        "Categorical or Numeric EV in Regression model",
                        c("gam","glm","lm"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("plot1"),
           plotOutput("plot2"),
           plotOutput("plot3"),
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ## Mac and cheese
      relevantdata
      modelmaccat <- lm(data = relevantdata,(Mac_Cheese_Consumed)~as.factor(Years_In_US))
      relevantdata$fittedmaccat <-modelmaccat$fitted.values
      modelmacnum <- lm(data = relevantdata,(Mac_Cheese_Consumed)~(Years_In_US))
      relevantdata$fittedmacnum <-modelmacnum$fitted.values
      mac <- ggplot(relevantdata)  +
        geom_boxplot(aes(x=as.factor(Years_In_US),y=Mac_Cheese_Consumed))
      mac <- mac +
        geom_line(aes(x=(Years_In_US),y=fittedmaccat),col="red")+
        geom_line(aes(x=(Years_In_US),y=fittedmacnum),col="blue")
      
      ## Lettuce
      modelletcat <- lm(data = relevantdata,(Lettuce)~as.factor(Years_In_US))
      relevantdata$fittedletcat <- modelletcat$fitted.values
      modelletnum <- lm(data = relevantdata,(Lettuce)~(Years_In_US))
      relevantdata$fittedletnum <-modelletnum$fitted.values
      let <- ggplot(relevantdata)  +
        geom_boxplot(aes(x=as.factor(Years_In_US),y=Lettuce))
      let <- let +
        geom_line(aes(x=(Years_In_US),y=fittedletcat),col="red")+
        geom_line(aes(x=(Years_In_US),y=fittedletnum),col="blue")
      
      ## French Fries
      modelFFcat <- lm(data = relevantdata,(French_Fries)~as.factor(Years_In_US))
      relevantdata$fittedFFcat <- modelFFcat$fitted.values
      modelFFnum <- lm(data = relevantdata,(French_Fries)~(Years_In_US))
      relevantdata$fittedFFnum <-modelFFnum$fitted.values
      FF <- ggplot(relevantdata)  +
        geom_boxplot(aes(x=as.factor(Years_In_US),y=French_Fries))
      FF <- FF +
        geom_line(aes(x=(Years_In_US),y=fittedFFcat),col="red")+
        geom_line(aes(x=(Years_In_US),y=fittedFFnum),col="blue")
      
      ## Salsa
      modelsalcat <- lm(data = relevantdata,(Salsa)~as.factor(Years_In_US))
      relevantdata$fittedsalcat <- modelsalcat$fitted.values
      modelsalnum <- lm(data = relevantdata,(Salsa)~(Years_In_US))
      relevantdata$fittedsalnum <-modelsalnum$fitted.values
      sal <- ggplot(relevantdata)  +
        geom_boxplot(aes(x=as.factor(Years_In_US),y=Salsa))
      sal <- sal +
        geom_line(aes(x=(Years_In_US),y=fittedsalcat),col="red")+
        geom_line(aes(x=(Years_In_US),y=fittedsalnum),col="blue")
      
      ## Pickles
      modelpiccat <- lm(data = relevantdata,(Pickles)~as.factor(Years_In_US))
      relevantdata$fittedpiccat <- modelpiccat$fitted.values
      modelpicnum <- lm(data = relevantdata,(Pickles)~(Years_In_US))
      summary(modelpicnum)
      relevantdata$fittedpicnum <-modelpicnum$fitted.values
      pic <- ggplot(relevantdata)  +
        geom_boxplot(aes(x=as.factor(Years_In_US),y=Pickles))
      pic <- pic +
        geom_line(aes(x=(Years_In_US),y=fittedpiccat),col="red")+
        geom_line(aes(x=(Years_In_US),y=fittedpicnum),col="blue")
      ## Pancakes
      modelpancat <- lm(data = relevantdata,(Pancakes)~as.factor(Years_In_US))
      relevantdata$fittedpancat <- modelpancat$fitted.values
      modelpannum <- lm(data = relevantdata,(Pancakes)~(Years_In_US))
      summary(modelpannum)
      relevantdata$fittedpannum <-modelpannum$fitted.values
      pan <- ggplot(relevantdata)  +
        geom_boxplot(aes(x=as.factor(Years_In_US),y=Pancakes))
      pan <- pan +
        geom_line(aes(x=(Years_In_US),y=fittedpancat),col="red")+
        geom_line(aes(x=(Years_In_US),y=fittedpannum),col="blue")
      
      print(cbind(coef(modelletcat),coef(modelFFcat),coef(modelsalcat),coef(modelpiccat),coef(modelpancat),coef(modelmaccat)))
      print(cbind(coef(modelletnum),coef(modelFFnum),coef(modelsalnum),coef(modelpicnum),coef(modelpannum),coef(modelmacnum)))
      
      if(input$Study == "Length in US Study"){
        output$plot1 <- renderPlot({
          food <- input$Foodtype
          if (food == "Lettuce"){
            let
          } else if (food == "French Fries"){
            FF
          }else if (food == "Salsa"){
            sal
          }else if (food == "Pickles"){
            pic
          }else if (food == "Pancakes"){
            pan
          }else if (food == "Mac and Cheese"){
            mac
          }
        })

    output$plot2 <- renderPlot({
        sumcat <- ggplot(relevantdata) +
          geom_line(aes(x=(Years_In_US),y=fittedletcat,color="Lettuce"))+
          geom_line(aes(x=(Years_In_US),y=fittedFFcat,color="French Fries"))+
          geom_line(aes(x=(Years_In_US),y=fittedsalcat,color="Salsa"))+
          geom_line(aes(x=(Years_In_US),y=fittedpiccat,color="Pickle"))+
          geom_line(aes(x=(Years_In_US),y=fittedpancat,color="Pancake"))+
          geom_line(aes(x=(Years_In_US),y=fittedmaccat,color="Mac and Cheese"))+
          scale_colour_manual(name="legend", values=c("blue", "red","cyan","pink","orange","green"))
  
  
        sumnum <- ggplot(relevantdata) +
          geom_line(aes(x=(Years_In_US),y=fittedletnum,color="Lettuce"))+
          geom_line(aes(x=(Years_In_US),y=fittedFFnum,color="French Fries"))+
          geom_line(aes(x=(Years_In_US),y=fittedsalnum,color="Salsa"))+
          geom_line(aes(x=(Years_In_US),y=fittedpicnum,color="Pickle"))+
          geom_line(aes(x=(Years_In_US),y=fittedpannum,color="Pancake"))+
          geom_line(aes(x=(Years_In_US),y=fittedmacnum,color="Mac and Cheese"))+
          scale_colour_manual(name="legend", values=c("blue", "red","cyan","pink","orange","green"))
        #
        if (input$Regtype == "Categorical"){
          sumcat
        } else if (input$Regtype == "Numeric"){
          sumnum
        }
      })
      }
  else {
  output$plot3 <-
    renderPlot({
      ggplot(linked_data, aes(x=Lettuce,y=BMI, colour="blue"))  +
        geom_point() +
        facet_grid(sex ~ .) +
        scale_x_log10() +
        scale_y_log10() +
        stat_smooth(method=input$Regtype2) 
    })
  }
}

# Run the application 
shinyApp(ui = ui, server = server)
