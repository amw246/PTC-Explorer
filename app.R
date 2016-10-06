

library(shiny)
library(xtable)
library(shinythemes)
library(lubridate)
library(hash)
library(dplyr)

# 
library(foreign)
# library(plyr)
# PTC <- read.dta("O:/!Policy/Projects/Policy Tracking Cohort/20160907_PTC_subset.dta",
#                 convert.factors=FALSE, convert.underscore=TRUE)
# save(PTC, file = "O:/!Policy/Projects/Policy Tracking Cohort/PTC Explorer App/PTC.RData")

# load("O:/!Policy/Projects/Policy Tracking Cohort/PTC Explorer App/PTC.RData")

# Add a page of text explaining the PTC, it's population, areas, and limits
# Have a list of notes with the variable names as the names so that when 
# a variable is used, any explanation needed is included.

#figure out how to choose an area and then a variable
#should we limit the tables to 2 variables at a time? I think yes.
#figure out how to have a button that starts the process
#add error checking logic that it gives a pop up if there aren't enough/the right variables
#when the user presses the Run analysis button. 

#for the variable choice button, have the user choose an area first. Then it should
#pop up a selectinput with the variables in that area. 

# Possibly have third and fourth sets of list for DV areas and IV areas

#Figure out why it gives an warning when it first loads

#Also, why is it missing 120k obs?

#Have choices of descriptives (percents, means, sd, min, max).
#have limits on the choices (mean not appropriate for categorical variable, etc.)

#should we allow three (or more) way tables or is that just asking for trouble in terms of PII

#rename variables to legible terms "Entry Date" instead of "entry.date" etc.

#Add the ability to filter on values of variables without including them in a table/plot

#And consider treating time as a separate dimension that can be included as a third part of table

#Consider asking how many variables they would like to crosstab first and then populating the selects
#could the select population then be dynamic?
#Should there then be a limit on how many variables to allow? Maybe moot if cell size rule is enough

#Column names for 3 variables suck. Fix them

# It can dynamically produce percentages for categorical variables. Figure out how to deal with
# numeric variables

#add a condition for if all vars are character, move forward, if one is numeric, reorder so it is 
#first, if two or more are numeric provide two or three summaries. or two summaries by the third var
#rbind the two finaldfs?

# fix xtableSummary2vars

# for regression have if clauses for character vs numeric for the DV
# for regression have the user choose a DV and then choose an arbitrary number of IVs
# figure out how to deal with char vs num IVs

# for the NSC data, create college_id vars with scrambled FICE codes (a csv crosswalk to merge?)
# also create ret_sem2-20 which have large number of categories
# still enrolled at same CUNY school
# still enrolled at different CUNY school
# enrolled at 2yr public non-CUNY school
# enrolled at 2yr private non-CUNY school
# ...
# highest degree earned is a certificate from CUNY 
# highest degree earned is a associate from CUNY 
# ...
# highest degree earned is an associate from non-CUNY 2 yr public
# ..
# all combinations of CUNY, non-CUNY 2/4 public/private and orders
# 
# Then provide syntax to aggregate into simpler versions like 
# enrolled at non-CUNY 
# earned degree at non-CUNY
# etc


#Give the term filter teeth

#Add more variables

workingDF <- PTC

# names(areas)

# http://www.showmeshiny.com/bivariate-regression/

source("variableNameLists.R", local = TRUE)

shinyUI <- pageWithSidebar(

  # Application title
  titlePanel("PTC Explorer"),
  
    sidebarPanel(
      dateRangeInput("dates", label = h3("Choose a range of terms to explore")
                     , start =  min(PTC$entry.date), end =  max(PTC$entry.date)
                     ),
      helpText("Note: The explorer will allow you to input any calendar date.",
               "However, the dates for semesters in this data are always Sept. 1st for fall",
               "and Feb. 1st for spring. Choosing a date other than those two days",
               "may not result in the date range intended.")
      , hr()
      , conditionalPanel(condition="input.conditionedPanels==2"
                         , selectInput('area1', 
                                      'Choose an Area: ', 
                                      c(" "='', names(areas)), selectize=FALSE)
                         , uiOutput("uiVar1")
                         , selectInput('area2', 
                                      'Choose an Area: ', 
                                      c(" "='', names(areas)), selectize=FALSE)
                         , uiOutput("uiVar2")
                         , selectInput('area3', 
                                       'Choose an Area: ', 
                                       c(" "='', names(areas)), selectize=FALSE)
                         , uiOutput("uiVar3")
                         )
      # ,actionButton("action", label = "Run Analysis")
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Introduction", tableOutput("introText"), value=1), 
                  tabPanel("Descriptive Statistics", tableOutput("descTable"), value=2,
                           tags$footer("Cohort Size ('C.S.'): If applicable, this value indicates 
                              that this metric is based on a denominator of fewer than 100. 
                              It is being obscured for privacy and generalizability reasons")), 
                  tabPanel("Visualization", plotOutput("ptcPlot"), value=3),
                  tabPanel("Regression Models", tableOutput("regTable"), value=4),
                  id = "conditionedPanels"
                  )
    )
)

shinyServer <- function(input, output) ({
  
  source("helperFunctions.R", local = TRUE)
  
  df <- reactive({
    
    subsetDf <- workingDF %>% filter(workingDF$entry.date >=  input$dates[[1]] &
                                     workingDF$entry.date <=  input$dates[[2]])
    subsetDf
  })

  output$uiVar1 = renderUI(
    selectInput('var1', "Choose a Variable: ", c(" " = '', areas[[input$area1]]))
  )

  output$uiVar2 = renderUI(
      selectInput('var2', "Choose a Variable: ", c(" " = '', areas[[input$area2]]))
  )

  output$uiVar3 = renderUI(
      selectInput('var3', "Choose a Variable: ", c(" " = '', areas[[input$area3]]))
  )
  
  output$introText = renderText({
    c("This app is designed to allow the user to explore CUNY data without needed to have individual-level data on their 
    personal computer.",
    
    "The data available here is comprised of all first-time freshman records for CUNY undergraduate, degree-seeking 
    students who entered in the fall or spring since fall 1999.")
    
  })
  
  output$descTable = renderTable({
    varList <- c(input$var1, input$var2, input$var3)
    tempTable <- createMetricTable(df(), varList)

  }, rownames = TRUE, na = "" ) 
  
  output$ptcPlot = renderPlot({
    
  })
  
  
})


shinyApp(ui = shinyUI, server = shinyServer)