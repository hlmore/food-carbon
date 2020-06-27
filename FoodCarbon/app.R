#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Started by Heather More on 2020-06-17
# See FoodCarbon_plots.R for practice code to generate plots.
#
# Data from:
#    Reducing food's environmental impacts through producers and consumers
#    Table S2
#    J. Poore, T. Nemecek
#    Science 2018-06-01: 987-992

# <!-- ===================================================================== -->
# 
# Load required packages
library(shiny)      # for app
library(readxl)     # read Excel spreadsheets
library(dplyr)      # data manipulation
library(tidyr)      # tidying data
library(ggplot2)    # nice plots
library(ggrepel)    # separate labels of points in scatterplot

# <!-- ===================================================================== -->
# 
# File information.  On Windows, change \ in file paths to / or \\
filePath  <- "D:/Documents/Code/Food carbon"
fileName  <- "Poore_2018_DataS2_tidy.xls"
dataSheetName <- "GlobalTotals_tidy"  # data
metaSheetName <- "GlobalTotals_meta"  # metadata
colourSheetName <- "Type_meta"  # colours

# <!-- ===================================================================== -->
# 
# Load data
#
# See info on scoping:
# http://rstudio.github.io/shiny/tutorial/#scoping

# Load data
df <- read_excel(file.path(filePath, fileName, 
                           fsep = .Platform$file.sep), sheet = dataSheetName)
df_meta <- read_excel(file.path(filePath, fileName, 
                                fsep = .Platform$file.sep), sheet = metaSheetName)
df_colours <- read_excel(file.path(filePath, fileName, 
                                   fsep = .Platform$file.sep), sheet = colourSheetName)
# Check variables
# str(df)
# str(df_meta)
# str(df_colours)

# Make new variable containing sum of all greenhouse gas emissions
df$ghg_total <- df %>%  
    select(starts_with("ghg")) %>% 
    rowSums(., na.rm = TRUE)

# <!-- ===================================================================== -->
# 
# Define UI
ui <- fluidPage(

    # Application title
    titlePanel("Food carbon emissions"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        position = "right",
        sidebarPanel(
            checkboxGroupInput("selectedType", 
                               h3("Type of product"), 
                               choices = c("all", "none", unique(df$type)),
                               selected = "all")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("mainPlot")
        )
    )
)

# <!-- ===================================================================== -->
# 
# Define server logic
server <- function(input, output, session) {
    
    # Point R to custom functions
    # https://stackoverflow.com/questions/38081580/calling-other-function-in-shiny-server
    source("CustomFunctions.R")
    
    # Make list of colours to use when plotting
    # https://www.r-bloggers.com/a-detailed-guide-to-ggplot-colors/
    # https://www.datamentor.io/r-programming/list/
    colours_type <- c()
    for (item in unique(df$type)){
        colours_type[[item]] <- GetColour(item, df_colours)
    }
    
    # Update selection list if "all" or "none" is selected
    # https://stackoverflow.com/a/26884455
    observe(({
        if ("all" %in% input$selectedType) {
            updateCheckboxGroupInput(session = session,
                                     inputId = "selectedType",
                                     selected = c(unique(df$type))
            )
        } else if ("none" %in% input$selectedType) {
            updateCheckboxGroupInput(session = session,
                                     inputId = "selectedType",
                                     selected = ""
            )
        }
    }))

    output$mainPlot <- renderPlot({
        ggplot(data = df) +
            aes(x = mass, y = mass*ghg_total, colour = type) +
            scale_x_log10() +
            scale_y_log10() +
            scale_color_manual(values = colours_type) +
            geom_point(size=3, alpha=0.7) +
            geom_point(size=3, shape=1) +
            #geom_text(aes(label=Product),hjust=0, vjust=0) +  # label points:  https://stackoverflow.com/a/15625149
            geom_text_repel(aes(label = Product),
                            box.padding   = 0.35, 
                            point.padding = 0.5,
                            show.legend = FALSE) +  # label points with auto-repelling text:  https://stackoverflow.com/a/48762376
            theme_light() +
            labs(x = paste("Total food mass produced (", GetUnits("mass", df_meta), ")"),
                 y = paste("Total GHG mass produced (", GetUnits("ghg", df_meta), ")"), 
                 colour = "Type",
                 title =  input$selectedType)#"Totals produced per year")
    })
}

# <!-- ===================================================================== -->
# 
# Run the application 
shinyApp(ui = ui, server = server)
