library(haven)
library(tidyverse)
library(skimr)
library(Synth)

dati = read_dta("vdem_merged_full.dta")



dati <- dati %>% mutate(log_gdp=log(rgdpe/pop), log_pop=log(pop), dist_eq=abs(latitude/90), 
                        ccsi_t_1=v2xcs_ccsi, ccsi_t_8=v2xcs_ccsi, ccsi_t_15=v2xcs_ccsi) %>% 
  select(v2xcs_ccsi, year, log_gdp, log_pop, v2x_polyarchy, dist_eq, legor07, hc,  
         countryname, ccsi_t_1, ccsi_t_8, ccsi_t_15, ccode)




dati.desc <- dati %>% select(v2xcs_ccsi, log_gdp, log_pop, v2x_polyarchy, dist_eq, hc)

summary_stats <- dati.desc %>%
  summarise(
    across(
      everything(),
      list(
        mean = ~ mean(., na.rm = TRUE),
        sd = ~ sd(., na.rm = TRUE),
        max = ~ max(., na.rm = TRUE),
        min = ~ min(., na.rm = TRUE)
      )
    )
  )

summary_stats <- as.matrix(summary_stats, nrow=6, byrow=T)
summary(dati.desc)   
# in ordine seguendo le colonne del dataset: N = 
# 8068
# 7153
# 7153 (queste due variabili nella tabella del paper hanno gli stessi NA di hc)
# 8049
# 8071
# 6406 (hc, unico che corrisponde)


##### Grafici

uganda_index <- dati %>%
  filter(countryname == "Uganda") %>%
  select(year, v2xcs_ccsi)

liberia_index <- dati %>%
  filter(countryname == "Liberia") %>%
  select(year, v2xcs_ccsi)

sierra_index <- dati %>%
  filter(countryname == "Sierra Leone") %>%
  select(year, v2xcs_ccsi)

burundi_index <- dati %>%
  filter(countryname == "Burundi") %>%
  select(year, v2xcs_ccsi)


ssa_data <- dati %>%
  filter(ccode > 401 & ccode < 592) %>%  # Filtra solo paesi SSA
  select(year, ccode, v2xcs_ccsi)         # Mantieni anno, codice paese e indice

# Calcola la media annuale del core Civil Society Index
ssa_mean <- ssa_data %>%
  group_by(year) %>%                        # Raggruppa per anno
  summarise(mean_cssi = mean(v2xcs_ccsi, na.rm = TRUE))  # Calcola la media


# Visualizza i risultati
head(ssa_mean)

plot(ssa_mean$year, ssa_mean$mean_cssi, type = "l", ylim = c(0,1), lwd = 4, col = "darkblue", main = "Subsaharan Africa", xlab = "Year", ylab = "Core Civil Society Index")
lines(uganda_index$year, uganda_index$v2xcs_ccsi, lty = 3, col = 3, lwd = 2)
lines(liberia_index$year, liberia_index$v2xcs_ccsi, lty = 5, col = "darkred", lwd = 2)
lines(sierra_index$year, sierra_index$v2xcs_ccsi, lty = 2, col = "darkgoldenrod", lwd = 2)
lines(burundi_index$year, burundi_index$v2xcs_ccsi, lty = 1, col = "darkgreen", lwd = 2)

legend("bottomright", 
       legend = c("SSA mean", "Uganda", "Liberia", "Sierra Leone", "Burundi"),
       lty = c(1, 3, 5, 2, 1), col = c("darkblue", 3, "darkred", "darkgoldenrod", "darkgreen"), lwd = c(2, 2, 2, 2, 2), cex = 0.8)



# Filtra i dati per il nepal
nepal_index <- dati %>%
  filter(countryname == "Nepal") %>%
  select(year, v2xcs_ccsi)



ssa_data <- dati %>%
  filter(ccode %in% c(700, 750, 760, 770, 771, 780, 781, 790)) %>%  # Filtra solo paesi SSA
  select(year, ccode, v2xcs_ccsi)         # Mantieni anno, codice paese e indice

# Calcola la media annuale del Civil Society Participation Index
ssa_mean <- ssa_data %>%
  group_by(year) %>%                        # Raggruppa per anno
  summarise(mean_cssi = mean(v2xcs_ccsi, na.rm = TRUE))  # Calcola la media

# Visualizza i risultati
head(ssa_mean)

plot(ssa_mean$year, ssa_mean$mean_cssi, type = "l", ylim = c(0,1), lwd = 4, col = "darkblue", main = "South Asia", xlab = "Year", ylab = "Core Civil Society Index")
lines(nepal_index$year, nepal_index$v2xcs_ccsi, lty = 5, col = "darkred", lwd = 2)

legend("bottomright", 
       legend = c("South Asia mean", "Nepal"),
       lty = c(1, 5), col = c("darkblue", "darkred"), lwd = c(2, 2), cex = 1)