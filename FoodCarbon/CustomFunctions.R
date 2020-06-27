# Function to get units for a given variable name
GetUnits <- function(string_in, df_lookup) {
  varName <- strsplit(string_in, "_")[[1]][1]
  string_out <- df_lookup %>% 
    filter(VariableName==varName) %>% 
    select(Units) %>% 
    toString()
  return(string_out)
}

# Function to get colour for a given variable name
GetColour <- function(string_in, df_lookup) {
  string_out <- df_lookup %>% 
    filter(Type==string_in) %>% 
    select(Colour) %>% 
    toString()
  return(string_out)
}