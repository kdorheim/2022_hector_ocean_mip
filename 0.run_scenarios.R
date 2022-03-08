# Load the required version of Hector, the Hector v3 alpha. 
# TODO pull from the Hector v3 alpha release. 
#remotes::install_github('jgcri/hector@4410ccf15d4e3219583f54cbb0a639c9361d593a', force = TRUE)
library(hector)
library(magrittr)
library(dplyr)
library(ggplot2)
#devtools::load_all("/Users/dorh012/Documents/Hector-Versions/R/hector")

# Need to figure out which other variables to collect and compare them with FAIR... are 
# the units correct? 

vars <- c(HEAT_FLUX(), OCEAN_CFLUX(), GLOBAL_TEMP())

core <- newcore(here::here("hector_inputs", "rcmip_1pctCO2.ini"), name = "1pctCO2")
run(core, runtodate = 2100)
out1 <- fetchvars(core, dates = 1745:2100, vars = vars)

core <- newcore(here::here("hector_inputs", "CDRCMIP_1pctCO2-cdr.ini"), name = "1pctCO2-cdr")
run(core, runtodate = 2150)
out2 <- fetchvars(core, dates = 1745:2150, vars = vars)

core <- newcore(here::here("hector_inputs", "rcmip_abrupt-4xCO2.ini"), name = "abrupt-4xCO2")
run(core, runtodate = 2150)
out3 <- fetchvars(core, dates = 1745:2150, vars = vars)

core <- newcore(here::here("hector_inputs", "rcmip_ssp119.ini"), name = "historical")
run(core, runtodate = 2019)
out4 <- fetchvars(core, dates = 1745:2019, vars = vars)


# --- 
core <- newcore(here::here("hector_inputs", "rcmip_1pctCO2.ini"), name = "1pctCO2")
setvar(core, NA, var = BETA(), values = 0, "(unitless)")
reset(core)
run(core, runtodate = 2100)
out5 <- fetchvars(core, dates = 1745:2150, vars = vars)

core <- newcore(here::here("hector_inputs", "CDRCMIP_1pctCO2-cdr.ini"), name = "1pctCO2-cdr")
setvar(core, NA, var = BETA(), values = 0, "(unitless)")
reset(core)
run(core, runtodate = 2150)
out6 <- fetchvars(core, dates = 1745:2150, vars = vars)

core <- newcore(here::here("hector_inputs", "rcmip_abrupt-4xCO2.ini"), name = "abrupt-4xCO2")
setvar(core, NA, var = BETA(), values = 0, "(unitless)")
reset(core)
run(core, runtodate = 2150)
out7 <- fetchvars(core, dates = 1745:2150, vars = vars)

core <- newcore(here::here("hector_inputs", "rcmip_ssp119.ini"), name = "historical")
setvar(core, NA, var = BETA(), values = 0, "(unitless)")
reset(core)
run(core, runtodate = 2019)
out8 <- fetchvars(core, dates = 1745:2019, vars = vars)
# ---- 

out_beta <- rbind(out5, out6, out7, out8)
out_beta$beta <- 0


hec_results <- rbind(out1, out2, out3, out4)
hec_results$beta <- "default"
out_beta %>%  
  rbind(hec_results) -> 
  hec_results

hector_heatflux <- hec_results[hec_results$variable == hector::HEAT_FLUX(), ]
hector_heatflux$value <- udunits2::ud.convert(hector_heatflux$value * 5100656e8 * (1 - 0.29), "W", "ZJ year-1")   # Constants derived from `temperature_component.hpp`
hector_heatflux$units <- "ZJ year-1" 
hector_heatflux$variable <- "hfds"

hector_tas <- hec_results[hec_results$variable == hector::GLOBAL_TEMP(), ]
hector_tas$variable <- "tas"

hector_fgco2 <- hec_results[hec_results$variable == "atm_ocean_flux", ]
hector_fgco2$variable <- "fgco2"

results <- dplyr::bind_rows(hector_heatflux, hector_tas, hector_fgco2)
results$year <- paste0("X", results$year)


write.csv(results, "hector_mip_results.csv", row.names = FALSE)

submission <- data.table::dcast(results, scenario + variable + units ~ year ) 
submission$model <- "hector v3 alpha" 
write.csv(submission, "hectorv3alpha_submission.csv", row.names = FALSE)



results %>%  
  mutate(year= as.integer(gsub(pattern = "X", replacement = "",x = year))) %>% 
  ggplot(aes(year, value, color = scenario)) + 
  geom_line() +
  facet_wrap("variable", scales = "free")



