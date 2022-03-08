# Load the required version of Hector, the Hector v3 alpha with MLD. 
devtools::load_all("/Users/dorh012/projects/Hector-Versions/ocean-mip-version/hector")
if (!is.character(MLD_OCEAN_CARBON())){
  stop("Wrong version of Hector")
}

library(magrittr)
library(dplyr)
library(ggplot2)

# Need to figure out which other variables to collect and compare them with FAIR... are 
# the units correct? 

vars <- c(HEAT_FLUX(), OCEAN_CFLUX(), GLOBAL_TEMP())
dates <- 1745:2100
theme_set(theme_bw())

# Default ----------------------------------------------------------------------------------------
core <- newcore(here::here("hector_inputs", "rcmip_1pctCO2.ini"), name = "1pctCO2")
run(core, runtodate = 2100)
out1 <- fetchvars(core, dates, vars)

core <- newcore(here::here("hector_inputs", "CDRCMIP_1pctCO2-cdr.ini"), name = "1pctCO2-cdr")
run(core, runtodate = 2150)
out2 <- fetchvars(core, dates, vars)

core <- newcore(here::here("hector_inputs", "rcmip_abrupt-4xCO2.ini"), name = "abrupt-4xCO2")
run(core, runtodate = 2150)
out3 <- fetchvars(core, dates, vars)

core <- newcore(here::here("hector_inputs", "rcmip_ssp119.ini"), name = "historical")
run(core, runtodate = 2100)
out4 <- fetchvars(core, dates, vars)

default_out <- rbind(out1, out2, out3, out4)
default_out$name <- "thermal mld = 60, ocean carbon mld = 100"

# MLD ----------------------------------------------------------------------------------------
core <- newcore(here::here("hector_inputs", "rcmip_1pctCO2.ini"), name = "1pctCO2")
setvar(core, NA, MLD_OCEAN_CARBON(), 60, "(unitless)")
reset(core)
run(core, runtodate = 2100)
out1 <- fetchvars(core, dates, vars)

core <- newcore(here::here("hector_inputs", "CDRCMIP_1pctCO2-cdr.ini"), name = "1pctCO2-cdr")
setvar(core, NA, MLD_OCEAN_CARBON(), 60, "(unitless)")
reset(core)
run(core, runtodate = 2150)
out2 <- fetchvars(core, dates, vars)

core <- newcore(here::here("hector_inputs", "rcmip_abrupt-4xCO2.ini"), name = "abrupt-4xCO2")
setvar(core, NA, MLD_OCEAN_CARBON(), 60, "(unitless)")
reset(core)
run(core, runtodate = 2150)
out3 <- fetchvars(core, dates, vars)

core <- newcore(here::here("hector_inputs", "rcmip_ssp119.ini"), name = "historical")
setvar(core, NA, MLD_OCEAN_CARBON(), 60, "(unitless)")
reset(core)
run(core, runtodate = 2100)
out4 <- fetchvars(core, dates, vars)

 new_out <- rbind(out1, out2, out3, out4)
 new_out$name <-  "thermal mld = 60, ocean carbon mld = 60"
 
# Output -------------------------------------------------------
 
 rbind(default_out, new_out) %>% 
   ggplot(aes(year, value, color = name, linetype = name, group_by = scenario)) + 
   geom_line(size = 1) + 
   facet_wrap("variable", scales = "free") + 
   labs(y = NULL) + 
   theme(legend.position = "bottom", legend.title = element_blank()) 

 
# Trying again ---- 
path <- system.file("input/hector_ssp460.ini", package = "hector") 
core <- newcore(path, name = "ssp460")
run(core)
dates <- 1850:2150
run(core) 
out1 <- fetchvars(core, dates, vars)
out1$name <- "thermal mld = 60, ocean carbon mld = 100"


setvar(core, NA, MLD_OCEAN_CARBON(), 60, unit = getunits(BETA()))
reset(core)
run(core)
out2 <- fetchvars(core, dates, vars)
out2$name <-  "thermal mld = 60, ocean carbon mld = 60"


rbind(out1, out2) %>% 
  ggplot(aes(year, value, color = name, linetype = name)) + 
  geom_line(size = 1) + 
  facet_wrap("variable", scales = "free") + 
  labs(y = NULL) + 
  theme(legend.position = "bottom", legend.title = element_blank()) 



# Format the output -------------------------------------------------------------------------------

hec_results <- do.call(what = "rbind", args = list(out1, out2))

hector_heatflux <- hec_results[hec_results$variable == hector::HEAT_FLUX(), ]
hector_heatflux$value <- udunits2::ud.convert(hector_heatflux$value * 5100656e8 * (1 - 0.29), "W", "ZJ year-1")   # Constants derived from `temperature_component.hpp`
hector_heatflux$units <- "ZJ year-1" 
hector_heatflux$variable <- "hfds"

hector_tas <- hec_results[hec_results$variable == hector::GLOBAL_TEMP(), ]
hector_tas$variable <- "tas"

hector_fgco2 <- hec_results[hec_results$variable == "atm_ocean_flux", ]
hector_fgco2$variable <- "fgco2"

results <- dplyr::bind_rows(hector_heatflux, hector_tas, hector_fgco2)
write.csv(results, "hector_mip_results-mld.csv", row.names = FALSE)

results$year <- paste0("X", results$year)
submission1 <- data.table::dcast(results, name + scenario + variable + units ~ year ) 
submission1$model <- "hector v3 alpha" 


hec_results <- do.call(what = "rbind", args = list(new_out, default_out))

hector_heatflux <- hec_results[hec_results$variable == hector::HEAT_FLUX(), ]
hector_heatflux$value <- udunits2::ud.convert(hector_heatflux$value * 5100656e8 * (1 - 0.29), "W", "ZJ year-1")   # Constants derived from `temperature_component.hpp`
hector_heatflux$units <- "ZJ year-1" 
hector_heatflux$variable <- "hfds"

hector_tas <- hec_results[hec_results$variable == hector::GLOBAL_TEMP(), ]
hector_tas$variable <- "tas"

hector_fgco2 <- hec_results[hec_results$variable == "atm_ocean_flux", ]
hector_fgco2$variable <- "fgco2"

results <- dplyr::bind_rows(hector_heatflux, hector_tas, hector_fgco2)
write.csv(results, "hector_mip_results-mld.csv", row.names = FALSE)

results$year <- paste0("X", results$year)
submission2 <- data.table::dcast(results, name + scenario + variable + units ~ year ) 
submission2$model <- "hector v3 alpha" 

submission <- dplyr::bind_rows(submission1, submission2)
write.csv(submission, "hector_mip_results-mld_submission.csv", row.names = FALSE)

