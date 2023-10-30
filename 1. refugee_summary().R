refugee_summary <- function(data) {
  
  #If tidyverse isn't installed, install it
  if (!requireNamespace('tidyverse', quietly = TRUE)) {
    install.packages('tidyverse')
    library(tidyverse)
    
    #Otherwise load tidyverse
  } else {
    library(tidyverse)
  }
  
  #If a year-country-pair has no external displacement, remove it
  data <- data %>%
    filter(!(Refugees.under.UNHCR.s.mandate == 0 & Asylum.seekers  == 0))
  
  #Create a new dataset data_summarised
  data_summarised <- data %>%
    #Group according to the variables Year, Destination Country and Destination Code
    group_by_at(vars(1, ncol(.) - 2, ncol(.) - 3)) %>%
    #For each destination country per year, calculate the number of refugees
    summarise(refugee_summed = sum(Refugees.under.UNHCR.s.mandate), 
              #Of asylum seekers
              asylum_seekers_summed = sum(Asylum.seekers),
              #Of overall external displacement
              external_displacement_total = sum(refugee_summed + asylum_seekers_summed))
  
  #Return the new dataset
  return(data_summarised)
}
