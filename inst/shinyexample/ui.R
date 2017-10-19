#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(condformat)
data(iris)

# Use
# install.packages("colourpicker")
# to get a nice colour picker in shiny. This is a workaround
colourpick <- function(inputId, label, value = "purple") {
  if (!requireNamespace("colourpicker")) {
    return(textInput(inputId = inputId, label = label, value = value))
  } else {
    colourpicker::colourInput(inputId = inputId, label = label, value = value)
  }
}

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("Condformat example"),

  # Sidebar with options
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "gradientcol1", label = "Colour gradient1", choices = c("None", colnames(iris)[1:4]), selected = "Sepal.Width"),
      colourpick(inputId = "colour_low1", label = "Low colour 1", value = "orange"),
      colourpick(inputId = "colour_high1", label = "High colour 1", value = "blue"),
      selectInput(inputId = "gradientcol2", label = "Colour gradient2", choices = c("None", colnames(iris)[1:4]), selected = "None"),
      colourpick(inputId = "colour_low2", label = "Low colour 2", value = "red"),
      colourpick(inputId = "colour_high2", label = "High colour 2", value = "green"),
      checkboxInput(inputId = "rule_color_for_species", label = "Colour Species", value = TRUE, width = NULL),
      numericInput(inputId = "num_entries_per_page", label = "Rows per page", value = 0, min = 0, max = 150, step = 1)
    ),

    # Show the conditional formatting table
    mainPanel(
       condformatOutput("iris")
    )
  )
))
