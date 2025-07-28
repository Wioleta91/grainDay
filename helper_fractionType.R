# if else function to calculate what type of sediment it is based on percentage of fractions
# Initially only Shepherd scale will be used 


#shepard classification
# 1) Sand
# 2) Clay
# 3) Silty Sand <20 % Clay
# 4) Clayey Sand
# 5) Silt
# 6) Sandy Silt <20 % Clay
# 7) Clayey Silt
# 8) Silty Clay
# 9) Sand Silt Clay
# 10) Sand-Silt-Clay


names_class <- c(
  "Sand",
  "Clay",
  "Silt",
  "Silty Sand",
  "Clayey Sand",
  "Sandy Silt",
  "Clayey Silt",
  "Sandy Clay",
  "Silty Clay",
  "Sand-Silt-Clay"  # central mixed zone
)

#source("grainDay/grainDay.R")

clay_func <- function(Sand, Silt, Clay) {
  if (Sand > 75) {
    return("Sand") #define Sand
  } else if (Clay > 75) {
    return("Clay") #define Clay
  } else if (Silt > 75) {
      return("Silt") #define Silt
  } else if (Clay < 50 & Silt <50 & Sand <50) {
      return("Sand-Clay-Silt") #define Mix
  } else if ( Silt > 70 & Sand > 30 ) {
      return("Silty Sand")
  } else
      return("Unclassified")
  }
  
  
  
  
  
  
  
  





#triggerError <- if ((sand + silt + clay) > initial_mass) {
                # trigger error message
#              } else {
                # proceed with calculations
#              }












