#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
suppressPackageStartupMessages({
  library(shiny)
  library(shinyjs)
  library(shinydashboard)
  library(here)
})


# setup ----
demo_tab_2 <- tabItem(
  tabName = "demo_tab_2",
  imageOutput("logo"))

# functions ----
source("scripts/func.R") # helper functions

# tabs ----

skin_color <- sample(c("red", "yellow", "green", "blue", "purple", "black"), 1)
random_icon <- sample(c("canadian-maple-leaf", "dragon", "user", "cog", 
                        "dice-d20", "dumpster-fire", "pastafarianism"), 1)


## UI ----


ui <- dashboardPage(
  skin = skin_color,
  # dashboardHeader(disable = TRUE),
  dashboardHeader(title = "This is the header", 
                  titleWidth = "calc(100% - 44px)" # puts sidebar toggle on right
  ),
  dashboardSidebar(
    # https://fontawesome.com/icons?d=gallery&m=free
    sidebarMenu(
      id = "tabs",
      menuItem("Background to the ADI", tabName = "tab_1", icon = icon(random_icon)),
      menuItem("Inequality measures", tabName = "tab_2", icon = icon(random_icon)),
      menuItem("LSOA-level analysis", tabName = "tab_3", icon = icon("dragon")),
      menuItem("District-level analysis", tabName = "tab_4", icon = icon("hand-sparkles"))
    ),
    tags$a(href = "https://www.annualdeprivationindex.co.uk/", 
           "Annual Deprivation Index website", style="padding: 1em;")
  ),
  dashboardBody(
    shinyjs::useShinyjs(),
    tags$head(
      # links to files in www/
      tags$link(rel = "stylesheet", type = "text/css", href = "basic_template.css"), 
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"), 
      tags$script(src = "custom.js")
    ),
    tabItems(
      tabItem(
        tabName = "tab_1",
        box(
          title = "Personal info",
          textInput("given", "Given Name"),
          textInput("surname", "Surname"),
          selectInput("pet", "What is your favourite pet?",
                      c("cats", "dogs", "ferrets"))
        ),
        box(
          title = "Biography",
          solidHeader = TRUE,
          status = "info",
          collapsible = TRUE,
          textAreaInput("bio", NULL,
                        height = "100px",
                        placeholder = "brief bio")
        )
      ),
      demo_tab_2, 
      tabItem(
        tabName = "demo_tab_3",
        infoBox(title = "Default InfoBox", value = "Value", subtitle = "Subtitle", 
                color = "orange", fill = FALSE, icon = icon("dragon")),
        valueBox("Default ValueBox", "With subtitle", color = "red", width = 5),
        valueBox("No subtitle", "")
      ),
      tabItem(
        tabName = "demo_tab_4",
        tabBox(
          title = "Test Yourself 1",
          tabPanel("Question", "What function creates tabBox contents?"),
          tabPanel("Answer", "tabPanel()")
        ),
        tabBox(
          title = "Test Yourself 2",
          side = "right",
          selected = "Question",
          tabPanel("Question", "What attribute changes the default tab?"),
          tabPanel("Answer", "selected")
        )
      ),
      tabItem(
        tabName = "demo_tab_5",
        fluidRow(
          box("A", title = "2x100", width = 2, height = 100),
          box("B", title = "1x100", width = 1, height = 100),
          box("C", title = "2x200", width = 2, height = 200),
          box("D", title = "3x300", width = 3, height = 300),
          box("E", title = "4x100", width = 4, height = 100),
          box("F", title = "5x100", width = 5, height = 100),
          box("G", title = "7x100", width = 7, height = 100)
        )
      ),
      tabItem(
        tabName = "demo_tab_6",
        column(width = 6,
               box("A", title = "12x100", width = 12, height = 100),
               box("B", title = "6x100", width = 6, height = 100),
               box("C", title = "6x200", width = 6, height = 200)
        ), 
        column(width = 4,
               box("D", title = "12x300", width = 12, height = 300),
               box("E", title = "12x100", width = 12, height = 100)
        ),
        column(width = 2,
               box("F", title = "12x100", width = 12, height = 100),
               box("G", title = "12x100", width = 12, height = 100)
        )
      )
    )
  )
)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
