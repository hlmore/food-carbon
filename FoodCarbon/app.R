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
library(shinyWidgets) # for extra cool widgets:  https://github.com/dreamRs/shinyWidgets
library(shinyjs)    # use Javascript functionality without needing .js :  https://www.rdocumentation.org/packages/shinyjs/versions/1.1
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
    
    # Let Shiny know you want to use JavaScript functionality
    # https://www.rdocumentation.org/packages/shinyjs/versions/1.1
    useShinyjs(),
    
    # Application title
    titlePanel("Food carbon emissions"),
    
    fluidRow(

        # Sidebar with checkboxes to select which types of food to show
        # https://stackoverflow.com/a/26884455
        column(3,
               h3("Type of product"),
               actionButton("selectAll", label = "Select all"),
               actionButton("selectNone", label = "Select none"),
               materialSwitch(
                   inputId = "selectOne",
                   label = "Limit selection to one type", 
                   right = TRUE
               ),
               checkboxGroupButtons("selectedType", 
                                    label = "",
                                    choices = unique(df$type),
                                    selected = unique(df$type),
                                    direction = "vertical"
                                 )
        ),

        # Plot of GHG vs. mass of food
        column(9,
               plotOutput("totalsPlot")
        )
    ),
    
    fluidRow(
        
        # Plot of GHGs at each stage of production
        column(12,
               plotOutput("stagesPlot")
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
    
    # Initialize variable to store the most recently clicked type(s)
    # Use <<- to save a variable between invocations:
    # https://stackoverflow.com/q/36240172
    mostRecentlySelectedType <<- ""
    
    # If "all" or "none" is selected, update selection list and turn off locking to a single selection
    # https://shiny.rstudio.com/articles/action-buttons.html
    # https://stackoverflow.com/a/26884455
    # See info on reactive events and observing:
    # https://stackoverflow.com/a/53016939
    observeEvent(input$selectAll, {
            mostRecentlySelectedType <<- c(unique(df$type))
            updateCheckboxGroupButtons(session = session,
                                     inputId = "selectedType",
                                     selected = mostRecentlySelectedType
            )
            updateMaterialSwitch(session = session,
                                 inputId = "selectOne",
                                 value = FALSE
            )
        })
    observeEvent(input$selectNone, {
            mostRecentlySelectedType <<- ""
            updateCheckboxGroupButtons(session = session,
                                     inputId = "selectedType",
                                     selected = mostRecentlySelectedType
            )
            updateMaterialSwitch(session = session,
                                 inputId = "selectOne",
                                 value = FALSE
            )
        })
    
    # If you are turning ON the option to select only one food type, set a single selected type and reset the list of previously selected types so you don't screw up the comparisons later.
    # If you are turning OFF this option, do nothing.
    observeEvent(input$selectOne, {
        if (input$selectOne) {
            mostRecentlySelectedType <<- ""
            # If multiple types are currently selected, take the first one
            if (length(input$selectedType) > 1) {
                mostRecentlySelectedType <<- input$selectedType[1]
            # If no types are currently selected, choose the first option
            } else if (length(input$selectedType) == 0) {
                mostRecentlySelectedType <<- unique(df$type)[1]
            # If one type is currently selected, don't change anything
            } else {
                mostRecentlySelectedType <<- input$selectedType
            }
            updateCheckboxGroupButtons(session = session,
                                       inputId = "selectedType",
                                       selected = mostRecentlySelectedType
            )
        }
    })
    
    # Keep track of which food type is being selected.
    # If locked to a single selection, only allow the last clicked food type to be selected
    # Use Javascript functionality to run this on clicking the button group, rather than using observeEvent, because using ObserveEvent on selectedType to update selectedType results in an infinite loop.
    # https://www.rdocumentation.org/packages/shinyjs/versions/1.1/topics/onevent
    onclick("selectedType", {
        # Store the type that was just clicked.  At this point, mostRecentlySelectedType has not been updated, so it stores the list of all types selected after the LAST (not current) click
        # There isn't a command to find elements that aren't duplicated between lists, so make one up by using the asymmetric setdiff() command twice.
        # https://stat.ethz.ch/R-manual/R-devel/library/base/html/sets.html
        # https://www.rdocumentation.org/packages/prob/versions/1.0-1/topics/setdiff
        mostRecentlySelectedType <<- c(setdiff(input$selectedType, mostRecentlySelectedType), setdiff(mostRecentlySelectedType, input$selectedType))
        if (input$selectOne) {
            # Update buttons to only show the most recently clicked type
            updateCheckboxGroupButtons(session = session,
                                       inputId = "selectedType",
                                       selected = mostRecentlySelectedType
        )}
    })
   
    # Make a background dataframe to plot non-selected data in grey
    # https://drsimonj.svbtle.com/plotting-background-data-for-groups-with-ggplot2
    background_df <- df %>% 
        select(-type)
    
    # Make a dataframe with only data for the selected food types
    # http://www.datasciencemadesimple.com/filter-subsetting-rows-r-using-dplyr/
    selected_df <- reactive({
        df %>% 
            filter(type %in% input$selectedType)
    })
    
    # Make a dataframe with only data for the unselected food types
    # https://stackoverflow.com/questions/34444295/how-to-specify-does-not-contain-in-dplyr-filter
    unselected_df <- reactive({
        df %>% 
            filter(!(type %in% input$selectedType))
    })
    
    # Select variables and convert to long form
    df_ghg <- reactive({
        selected_df() %>%
        select(starts_with("ghg") | "Product" | "type")
    })
    df_ghg_long <- reactive({
        df_ghg() %>%
        select(-ends_with("total")) %>%
        pivot_longer(cols = starts_with("ghg"),
                     names_to = "stage",
                     values_to = "stage_ghg_emissions") %>%
        mutate(stage = factor(stage, levels=unique(stage)))
    })
    df_ghg_unselected <- reactive({
        unselected_df() %>%
            select(starts_with("ghg") | "Product" | "type") %>% 
        select(-ends_with("total")) %>%
        pivot_longer(cols = starts_with("ghg"),
                     names_to = "stage",
                     values_to = "stage_ghg_emissions") %>%
        mutate(stage = factor(stage, levels=unique(stage)))
    })
    
    # Select variables, compute % of GHG produced at each stage, and convert to long form
    df_ghg_percents <-  reactive({
        df_ghg() %>% 
        mutate_at(vars(contains("ghg") & !contains("total")), funs(./ ghg_total))
    })
    df_ghg_percents_long <-  reactive({
        df_ghg_percents() %>% 
        pivot_longer(cols = (starts_with("ghg") & !ends_with("total")), 
                     names_to = "stage", 
                     values_to = "stage_ghg_emissions") %>% 
        mutate(stage = factor(stage, levels=unique(stage)))  # convert stages to factors so they will be plotted in the correct order; https://www.r-graph-gallery.com/267-reorder-a-variable-in-ggplot2
    })
    df_ghg_unselected_percents <- reactive({
        unselected_df() %>%
        select(starts_with("ghg") | "Product" | "type") %>% 
        mutate_at(vars(contains("ghg") & !contains("total")), funs(./ ghg_total)) %>% 
        pivot_longer(cols = (starts_with("ghg") & !ends_with("total")), 
                     names_to = "stage", 
                     values_to = "stage_ghg_emissions") %>% 
        mutate(stage = factor(stage, levels=unique(stage)))
    })
    
    # Plot GHG vs. amount of food produced
    output$totalsPlot <- renderPlot({
        ggplot(data = selected_df()) +
            aes(x = mass, 
                y = mass*ghg_total, 
                colour = type) +
            geom_point(data = unselected_df(), size=3, colour="grey") +
            geom_point(size=3, alpha=0.7) +
            geom_point(size=3, shape=1) +
            scale_color_manual(values = colours_type) +
            scale_x_log10() +
            scale_y_log10() +
            #geom_text(aes(label=Product),hjust=0, vjust=0) +  # label points:  https://stackoverflow.com/a/15625149
            geom_text_repel(aes(label = Product),
                            box.padding   = 0.35, 
                            point.padding = 0.5,
                            show.legend = FALSE) +  # label points with auto-repelling text:  https://stackoverflow.com/a/48762376
            guides(fill = FALSE, color = FALSE) +
            theme_light() +
            labs(x = paste("Total food mass produced (", GetUnits("mass", df_meta), ")"),
                 y = paste("Total GHG mass produced (", GetUnits("ghg", df_meta), ")"), 
                 colour = "Type",
                 title = "Totals produced per year")
    })
    
    # Plot GHG at each stage
    output$stagesPlot <- renderPlot({
        ggplot(data = df_ghg_percents_long()) +
            aes(x = stage, 
                y = stage_ghg_emissions, 
                group = Product, 
                color = type) +   # group = Product is important!
            geom_path(data = df_ghg_unselected_percents(), color = "grey") +
            geom_path(size = 1) +
            guides(color = FALSE) +
            theme_light() +
            scale_color_manual(values = colours_type) +
            coord_cartesian(ylim = c(-0.3, 1), 
                            expand = FALSE) +
            scale_y_continuous(breaks = seq(-0.3, 1, 0.1), 
                               minor_breaks = NULL, 
                               labels = round(seq(-0.3, 1, 0.1)*100)) +
            labs(x = "Production stage",
                 y = paste("% lifespan GHG emissions (%)"))
        
        # ggplot(data = df_ghg_percents_long()) +
        #     aes(x = stage, y = stage_ghg_emissions, group = Product, color = type) +   # group = Product is important!
        #     geom_path() +
        #     theme_light() +
        #     scale_color_manual(values = colours_type) +
        #     labs(x = "Production stage",
        #          y = paste("GHG emissions per kg food (", GetUnits("ghg", df_meta), ")")) #+
    #scale_x_discrete(labels=unique("stage"))
    })
}

# <!-- ===================================================================== -->
# 
# Run the application 
shinyApp(ui = ui, server = server)
