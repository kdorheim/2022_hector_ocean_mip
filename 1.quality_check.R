
library(data.table)
library(ggplot2)
#library(hector)
library(magrittr)


# Read in the comparison data. 
pctCO2     <- read.csv(here::here("comparison_data", "rcmip_phase-1_fair-1.5-default-1pctCO2_v1-0-0.csv"), stringsAsFactors = FALSE)
pctCO2_cdr <- read.csv(here::here("comparison_data", "rcmip_phase-1_fair-1.5-default-1pctCO2-cdr_v1-0-1.csv"), stringsAsFactors = FALSE)
names(pctCO2_cdr) <- gsub(pattern = ".01.01.00.00.00", replacement = "", x = names(pctCO2_cdr))
abrupt4    <- read.csv(here::here("comparison_data", "rcmip_phase-1_fair-1.5-default-abrupt-4xCO2_v1-0-0.csv"), stringsAsFactors = FALSE)
historical <- read.csv(here::here("comparison_data", "rcmip_phase-1_fair-1.5-default-esm-ssp119_v1-0-1.csv"), stringsAsFactors = FALSE)
names(historical) <- gsub(pattern = ".01.01.00.00.00", replacement = "", x = names(historical))
historical$Scenario <- "historical"

# Format into a single data frame. 
comparison_data_wide <- as.data.table(dplyr::bind_rows(list(pctCO2, pctCO2_cdr, abrupt4, historical)))

# Reformat into a long data frame  
id_vars <- which(!grepl(pattern = "^X[[:digit:]]{4}", x = names(comparison_data_wide)))
long_inputs <- data.table::melt.data.table(data = comparison_data_wide, id.vars = id_vars,
                                           variable.name = "year", value.name = "value",
                                           variable.factor = FALSE)[(Region == "World" & Variable == "Heat Uptake")]
long_inputs[ , year := as.integer(gsub(pattern = "X", replacement = "", x = year))]
long_inputs <- long_inputs[ ,.(Scenario, Variable, Unit, year, value)]
names(long_inputs) <- c("scenario", "unit", "variable", "year", "value")
long_inputs$model <- "fair"
long_inputs$variable <- "hfds"

hector_df <- read.csv(here::here("hector_mip_results.csv"), stringsAsFactors = FALSE)
hector_df$model <- "hector"


dplyr::bind_rows(long_inputs, hector_df) %>%  
  dplyr::filter(variable == "hfds") %>% 
  ggplot(aes(year, value, color = scenario, linetype = model)) + 
  geom_line()

# Not quite sure what is going on with the hfds and abrtupt 4xCO2 