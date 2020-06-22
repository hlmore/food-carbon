# This code reads data on food carbon emissions, and creates plots.
#
# It will hopefully be integrated into a Shiny app at some point.
#
# Started by Heather More on 2020-06-17
#
# Inspired by a Makeover Monday challenge, which was based on this article:
#    https://ourworldindata.org/food-choice-vs-eating-local
#
# Data from:
#    Reducing food's environmental impacts through producers and consumers
#    Table S2
#    J. Poore, T. Nemecek
#    Science 2018-06-01: 987-992

#' <!-- ==================================================================== -->
#' 
#' ## Load required packages
library(readxl)     # read Excel spreadsheets
library(dplyr)      # data manipulation
library(tidyr)      # tidying data
library(ggplot2)    # nice plots
library(ggrepel)    # separate labels of points in scatterplot

#' <!-- ==================================================================== -->
#' 
#' File information.  On Windows, change \ in file paths to / or \\

filePath  <- "D:/Documents/Code/Food carbon"
fileName  <- "Poore_2018_DataS2_tidy.xls"
dataSheetName <- "GlobalTotals_tidy"  # data
metaSheetName <- "GlobalTotals_meta"  # metadata
colourSheetName <- "Type_meta"  # colours

#' <!-- ==================================================================== -->
#' 
#' ## Read in data and metadata

df <- read_excel(file.path(filePath, fileName, 
                           fsep = .Platform$file.sep), sheet = dataSheetName)
df_meta <- read_excel(file.path(filePath, fileName, 
                           fsep = .Platform$file.sep), sheet = metaSheetName)
df_colours <- read_excel(file.path(filePath, fileName, 
                                fsep = .Platform$file.sep), sheet = colourSheetName)
#' Check variables
str(df)
str(df_meta)
str(df_colours)

#' Function to get units for a given variable name
GetUnits <- function(string_in) {
  varName <- strsplit(string_in, "_")[[1]][1]
  string_out <- df_meta %>% 
    filter(VariableName==varName) %>% 
    select(Units) %>% 
    toString()
  return(string_out)
}

#' Function to get colour for a given variable name
GetColour <- function(string_in) {
  string_out <- df_colours %>% 
    filter(Type==string_in) %>% 
    select(Colour) %>% 
    toString()
  return(string_out)
}

#' <!-- ==================================================================== -->
#' 
#' ## Plot all mass vs. total greenhouse gas emissions, and label foods

# Make new variable containing sum of all greenhouse gas emissions
df$ghg_total <- df %>%  
  select(starts_with("ghg")) %>% 
  rowSums(., na.rm = TRUE)

#' Make list of colours to use when plotting
#' https://www.r-bloggers.com/a-detailed-guide-to-ggplot-colors/
#' https://www.datamentor.io/r-programming/list/
colours_type <- c()
for (item in unique(df$type)){
  colours_type[[item]] <- GetColour(item)
}

# Plot
plot_mass_ghg <- ggplot(data = df) +
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
  labs(x = paste("Total food mass produced (", GetUnits("mass"), ")"),
       y = paste("Total GHG mass produced (", GetUnits("ghg"), ")"), 
       colour = "Type",
       title = "Totals produced per year")
x11()
print(plot_mass_ghg)

#' <!-- ==================================================================== -->
#' 
#' ## Plot kg greenhouse gas emissions at each stage of production

df_ghg <- df %>% 
  select(starts_with("ghg") | "Product" | "type")
df_ghg_long <- df_ghg %>% 
  select(-ends_with("total")) %>% 
  pivot_longer(cols = starts_with("ghg"), 
               names_to = "stage", 
               values_to = "stage_ghg_emissions") %>% 
  mutate(stage = factor(stage, levels=unique(stage)))  # convert stages to factors so they will be plotted in the correct order; https://www.r-graph-gallery.com/267-reorder-a-variable-in-ggplot2

# Parallel coordinates plot
# https://datascience.blog.wzb.eu/2016/09/27/parallel-coordinate-plots-for-discrete-and-categorical-data-in-r-a-comparison/
stages_plot <- ggplot(data = df_ghg_long) +
  aes(x = stage, y = stage_ghg_emissions, group = Product, color = type) +   # group = Product is important!
  geom_path() +
  theme_light() +
  scale_color_manual(values = colours_type) +
  labs(x = "Production stage",
       y = paste("GHG emissions per kg food (", GetUnits("ghg"), ")")) #+
  #scale_x_discrete(labels=unique("stage"))
x11()
print(stages_plot)

#' <!-- ==================================================================== -->
#' 
#' ## Plot % greenhouse gas emissions for each product produced at each stage

df_ghg_percents <- df_ghg %>% 
  mutate_at(vars(contains("ghg") & !contains("total")), funs(./ ghg_total))
df_ghg_percents_long <- df_ghg_percents %>% 
  pivot_longer(cols = (starts_with("ghg") & !ends_with("total")), 
               names_to = "stage", 
               values_to = "stage_ghg_emissions") %>% 
  mutate(stage = factor(stage, levels=unique(stage)))  # convert stages to factors so they will be plotted in the correct order; https://www.r-graph-gallery.com/267-reorder-a-variable-in-ggplot2

# Parallel coordinates plot
# https://datascience.blog.wzb.eu/2016/09/27/parallel-coordinate-plots-for-discrete-and-categorical-data-in-r-a-comparison/
stages_percents_plot <- ggplot(data = df_ghg_percents_long) +
  aes(x = stage, 
      y = stage_ghg_emissions, 
      group = Product, 
      color = type) +   # group = Product is important!
  geom_path() +
  theme_light() +
  scale_color_manual(values = colours_type) +
  coord_cartesian(ylim = c(-0.3, 1), 
                  expand = FALSE) +
  scale_y_continuous(breaks = seq(-0.3, 1, 0.1), 
                     minor_breaks = NULL, 
                     labels = round(seq(-0.3, 1, 0.1)*100)) +
  labs(x = "Production stage",
       y = paste("% lifespan GHG emissions per kg food (", GetUnits("ghg"), ")")) #+
#scale_x_discrete(labels=unique("stage"))
x11()
print(stages_percents_plot)