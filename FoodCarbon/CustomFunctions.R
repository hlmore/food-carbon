# Function to get units for a given variable name
GetUnits <- function(string_in) {
  varName <- strsplit(string_in, "_")[[1]][1]
  string_out <- df_meta %>% 
    filter(VariableName==varName) %>% 
    select(Units) %>% 
    toString()
  return(string_out)
}

# Function to get colour for a given variable name
GetColour <- function(string_in) {
  string_out <- df_colours %>% 
    filter(Type==string_in) %>% 
    select(Colour) %>% 
    toString()
  return(string_out)
}