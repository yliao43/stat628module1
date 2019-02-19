rm(list = ls())
#install.packages("shinythemes")
library(shinythemes)
library(shiny)


ui <- fluidPage(
  shinythemes::themeSelector(),
  headerPanel('Body Fat Calculator'),
  sidebarPanel(
    radioButtons("gender", "Gender:",
                 c("Male" = "male","Female" = "female"),inline = T),
    hr(),
    radioButtons("wgtunit", "Unit:",
                 c("kg" = "kg","lbs" = "lbs"),inline = T),
    numericInput("num1", label = p("Weight:"), value = 0, min = 0),
    hr(),
    radioButtons("lgtunit", "Unit:",
                 c("cm" = "cm","inch" = "in"),inline = T),
    numericInput("num2", label = p("Abdomen:"), value = 0, min = 0),
    numericInput("num3", label = p("Wrist:"), value = 0, min = 0)),
  mainPanel(
    p(strong('Result'),style = "font-size: 20px;"),
    strong(htmlOutput("txtout"),style = "font-size: 20px;"),
    strong(htmlOutput("txtout2"),style = "font-size: 20px;"),
    hr(),
    helpText(br(),p(strong("Reference:")),p("Male:"),p("Essential fat: 3-5%"),
             p("Athletes: 6-13%"),p("Fitness: 14-17%"),p("Average: 18-25%"),
             p("Obese: 25-100%"),br(),p("The standard is from:",style = "font-size: 12px;"),
             p("ACE (2009) What are the guidelines for percentage of body fat loss?
               American Council on Exercise (ACE). Ask the Expert Blog. December 2, 2009.",
               style = "font-size: 12px;"),
             br(),p("If you have further question about this calculator,
                    please contact: lzeng32@wisc.edu."))
            )
)
server <- function(input, output) {
  bodyfat = function(a,b,c){
    if(a <= 0 | b <= 0 | c <= 0 | is.na(a) == 1 | is.na(b) == 1 | is.na(c) == 1){
      return(-1)
    }else{
      if(input$gender == "female"){
        return(NA)
      }else{
        if(input$wgtunit == "lbs" && input$lgtunit == "cm"){
          a = a; b = b;c = c;
        }else if(input$wgtunit == "kg" && input$lgtunit == "cm"){
          a = a * 2.2046
        }else if(input$wgtunit == "kg" && input$lgtunit == "in"){
          a = a * 2.2046;b = b * 2.54;c = c * 2.54
        }else if(input$wgtunit == "lbs" && input$lgtunit == "in"){
          b = b * 2.54;c = c * 2.54;
        }else{break}
        y = - 0.09372757*a + 0.89537024*b - 1.10949215 *c - 26.80562419 
        return(y)
      }
    }
    
  } 
  
  txtprint = function(y){
    if(input$gender == "female"){
      return("Lacking data of females. Please try male's.")
    }else{
      if(3 <= y && y < 5){
      return("Your body fat is in the range of essential fat.")
    }else if(5 <= y && y < 13){
      return("Your body fat is in the range of athletes.")
    }else if(13 <= y && y < 17){
      return("Your body fat is in the range of fitness.")
    }else if(17 <= y && y < 25){
      return("Your body fat is in the range of average.")
    }else if(y > 25 && y < 100){
      return("Your body fat is in a range of obesity.")
    }else if(y == -1){
      return("You should fill in all the blanks. Please fill in.")
    }else{
      return("Something wrong with your data. Please check again.")
    }
    }
  }
  output$txtout <- renderText({paste("Your body fat is ", "<font color=\"#FF0000\"><b>",bodyfat(input$num1,input$num2,input$num3),"%.")})
  output$txtout2 <- renderText({paste(txtprint(bodyfat(input$num1,input$num2,input$num3)))})
}

shinyApp(ui = ui, server = server)
#> Data[1,]
#BODYFAT AGE WEIGHT HEIGHT ADIPOSITY NECK CHEST ABDOMEN  HIP THIGH KNEE ANKLE BICEPS
#1    12.6  23 154.25  67.75      23.7 36.2  93.1    85.2 94.5    59 37.3  21.9     32
#FOREARM WRIST
#1    27.4  17.1

