
library(shiny)
library(markdown)
shinyUI(navbarPage("Coursera JHU Data Science Capstone Project",
                   tabPanel("My predition for next word",
                            HTML("<strong>Author: Xuanru Shen</strong>"),
                            br(),
                            HTML("<strong>Date: 18 June 2017 </strong>"),
                            br(),
                            # Sidebar
                            sidebarLayout(
                              sidebarPanel(
                                helpText("Enter a phrase (multiple words) to get the next word prediction"),
                                textInput("inputString", "Enter a sentence or phrase here",value = ""),
                                br(),
                                br(),
                                br(),
                                br()
                              ),
                              mainPanel(
                                h2("My predition for next word"),
                                verbatimTextOutput("prediction"),
                                strong("String Entered:"),
                                tags$style(type='text/css', '#text1 {background-color: rgba(255,255,0,0.40); color: red;}'), 
                                textOutput('text1'),
                                br(),
                                strong("Info:"),
                                tags$style(type='text/css', '#text2 {background-color: rgba(255,255,0,0.40); color: black;}'),
                                textOutput('text2')
                              )
                            )
                            
                   )
                   )
)
