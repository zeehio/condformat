#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(condformat)
data(iris)

# Define server logic required to show condformat usage
shinyServer(function(input, output) {
  output$iris <- renderCondformat({
    x <- condformat(iris)
    column1 <- input$gradientcol1
    if (column1 != "None") {
      lowcol1 <- input$colour_low1
      highcol1 <- input$colour_high1
      x <- x %>% rule_fill_gradient(!! column1, low = lowcol1, high = highcol1)
    }
    column2 <- input$gradientcol2
    if (column2 != "None") {
      lowcol2 <- input$colour_low2
      highcol2 <- input$colour_high2
      x <- x %>% rule_fill_gradient(!! column2, low = lowcol2, high = highcol2)
    }


    if (input$rule_color_for_species) {
      x <- x %>% rule_fill_discrete("Species")
    }
    if (input$num_entries_per_page == 0) {
      num_entries <- c(10, 20, 50)
    } else {
      num_entries <- input$num_entries_per_page
    }
    x %>% theme_htmlWidget(number_of_entries = num_entries)
    })
})
