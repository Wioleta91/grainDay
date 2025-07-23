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
  "Silt",
  "Clay",
  "Silty Sand",
  "Clayey Sand",
  "Sandy Silt",
  "Clayey Silt",
  "Sandy Clay",
  "Silty Clay",
  "Sand-Silt-Clay"  # central mixed zone
)

source("grainDay/grainDay.R")

clay_func <- function(Sand_perc, Silt_perc, Clay_perc) {
  if (Sand_perc > 75) {
    return("Sand")
  } else if (Clay_perc > 75) {
    return("Clay") 
  } else if (Silt_perc > 75) {
      return("Silt")
    
    }
  }
  
  
  
  
  
  
  
  





#triggerError <- if ((sand + silt + clay) > initial_mass) {
                # trigger error message
#              } else {
                # proceed with calculations
#              }







say_hello <- function() {
  print("Hello from the helper!")
}









