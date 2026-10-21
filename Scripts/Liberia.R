################### Liberia #########################
#### SCM a mano ####

# seleziona controlli con pesi nel paper
li_dat <- dati %>% filter(countryname %in% c("Liberia", "Malawi", "Tanzania", "Swaziland") & year %in% 1987:2010)

#crea una matrice con i valori di ccsi nei controlli negli anni di interesse
control.out.li <- matrix(li_dat$v2xcs_ccsi, ncol = 4)
row.names(control.out.li) <- 1987:2010
colnames(control.out.li) <- c("Liberia", "Tanzania", "Malawi", "Swaziland")

# calcola stima scm con i pesi del paper
synth.li <- control.out.li %*% c(0, 0.48, 0.02, 0.49)

#crea il grafico
plot(1987:2010, synth.li, type = "l", ylim = c(.4, 1), xlab = "Year", ylab = "Core Civil Society Index", main = "Synthetic Control Manuale: Liberia", lwd = 2, lty = 3, xaxt = "n")
axis(1, at = c(1987, 1992, 1997, 2002, 2007, 2010))
lines(1987:2010, control.out.li[, 1], type = "l",  lwd = 2) 
abline(v = 2002, lty = 2, lwd = 2)
legend("topleft", legend = c("Liberia", "Synthetic Liberia"),
       lty = c(1, 3), col = c("black","black"), lwd = c(2,2), cex = .8)



#### SCM con synth ####

# year > 1986 & year < 2003 periodo pre-trattamento

dati %>% filter(countryname == "Liberia") %>% select(ccode) %>% str() # code liberia 450

dati %>% select(countryname, ccode) %>% distinct %>% print(n = Inf) # ho nome e codice nazioni dataset

# uso come controllo 402-591 ccode nazioni africa subsahariana

dati %>% filter(ccode > 401 & ccode < 592, year > 1986 & year < 2003) %>% skim()

c_control.li <- dati %>% filter(ccode > 401 & ccode < 592, year > 1986 & year < 2003) %>% select(log_gdp, log_pop, hc, v2x_polyarchy, ccode, countryname) 
# no 402-411, Guinea 438, Chad 483, Djibouti 522, Comoros 581, Seychelles 591  (NA hc) , Somalia 520, Eritra 531 (NA tutto tranne poliarchy), 
#Mozambique 541(Na poliarchy, non su tutti gli anni, lo abbiamo messo)

control.li = c(420, 432:437, 439, 450, 451, 452, 461, 471, 481, 482, 484, 490, 
          500, 501, 510, 516, 517, 530, 540, 541, 551:553, 560, 565, 570:572, 580, 590)

d_li <- dati %>% filter(ccode %in% control.li & year %in% 1987:2010) # prendo solo le unità e gli anni di interesse 

d_li <- d_li %>% as.data.frame() # per evitare errori in data_prep

d_li %>%
  group_by(ccode) %>%
  summarize(
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE),
    total_years = n_distinct(year)
  ) %>%
  arrange(min_year)%>%
  print(n = Inf) # controlla presenza di tutti gli anni necessari per le unità di controllo e trattamento

dataprep.out.li <- dataprep(
  foo = d_li,  
  
  predictors = c("log_gdp", "log_pop", "v2x_polyarchy", "dist_eq", "legor07", "hc"),
  
  predictors.op = "mean",  
  
  time.predictors.prior = 1987:2002,  
  
  special.predictors = list(
    list("ccsi_t_1", 2001, "mean"),
    list("ccsi_t_8", 1994, "mean"),
    list("ccsi_t_15", 1987, "mean")
  ),
  dependent = "v2xcs_ccsi",  
  
  unit.variable = "ccode", 
  
  unit.names.variable = "countryname", 
  
  time.variable = "year", 
  
  treatment.identifier = 450,  
  
  controls.identifier = c(420, 432:437, 439, 451, 452, 461, 471, 481, 482, 484, 490,
                           500, 501, 510, 516, 517, 530, 540, 541, 551:553, 560, 565, 
                           570:572, 580, 590), 
  
  time.optimize.ssr = 1987:2002,  
  
  time.plot = 1987:2010  
)


# calcola stime scm
synth.out.li <- synth(data.prep.obj = dataprep.out.li, method = "BFGS")

synth.tables.li <- synth.tab(dataprep.res = dataprep.out.li, synth.res = synth.out.li)

synth.tables.li$tab.pred # tabella valori predittori synthetic
synth.tables.li$tab.w # pesi nazioni controllo: Gambia 0.234, Uganda 0.436, Rwanda 0.187, Malawi 0.104, lesotho 0.038
synth.tables.li$tab.v

# Estrai i dati per il grafico
years <- dataprep.out.li$tag$time.plot
observed <- dataprep.out.li$Y1plot
synthetic <- dataprep.out.li$Y0plot %*% synth.out.li$solution.w

# Crea il grafico manualmente
plot(years, observed, type = "l", col = "black", lwd = 2, ylim = c(0.4, 1),
     xlab = "Year", ylab = "Core Civil Society Index",
     main = "Synthetic Control Results: Liberia", xaxt = "n")  # Disattiva l'asse x predefinito

# Aggiungi la linea del Nepal sintetico
lines(years, synthetic, lwd = 2, lty = 3)

# Aggiungi una linea verticale al 2001
abline(v = 2002, lty = 2, lwd = 2)

# Personalizza le etichette dell'asse x
axis(1, at = c(1987, 1992, 1997, 2002, 2007, 2010))

# Aggiungi una legenda
legend("topleft", legend = c("Liberia", "Synthetic Liberia"),
       col = c("black", "black"), lty = c(1, 3), lwd = c(2, 2), cex = 0.8)

#calcolo rmspe
RMSPE.li <- sqrt(synth.out.li$loss.v) # 0.03677711



#### TEST PLACEBO ####

# Prepara la matrice per memorizzare i gap
store.li <- matrix(NA, length(1987:2010), 35)
colnames(store.li) <- unique(d_li$countryname)
country_code.li <- unique(d_li$ccode) # crea vettore con i codici delle nazioni

# Itera su ogni unità 
for(iter in country_code.li) {
  # Prepara i dati
  dataprep.out <- dataprep(
    foo = d_li, 
    
    predictors = c("log_gdp", "log_pop", "v2x_polyarchy", "dist_eq", "hc" ),
    
    predictors.op = "mean", 
    
    time.predictors.prior = 1987:2002,  
    
    special.predictors = list(
      list("ccsi_t_1", 2001, "mean"),
      list("ccsi_t_8", 1994, "mean"),
      list("ccsi_t_15", 1987, "mean")
    ),
    dependent = "v2xcs_ccsi",
    
    unit.variable = "ccode", 
    
    unit.names.variable = "countryname",
    
    time.variable = "year",
    
    treatment.identifier = iter,  # L'unità corrente come trattata
    controls.identifier = country_code.li[country_code.li != iter],  # Esclude l'unità corrente dai controlli
    time.optimize.ssr = 1987:2002,
    time.plot = 1987:2010
  )
  
  # esegue scm
  synth.out <- synth(data.prep.obj = dataprep.out, method = "BFGS")
  
  # Calcola i gap e li memorizza
  store.li[, which(country_code.li == iter)] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)
}

# Prepara i dati per il grafico
data.li <- store.li  # Usa la matrice `store` generata con i gap
rownames(data.li) <- 1987:2010 

# Limiti per i gap nel periodo di analisi
gap.start <- 1
gap.end <- nrow(data.li)
years <- 1987:2010
gap.end.pre  <- which(rownames(data.li) == "2002") # indice anno trattamento

# Calcola MSPE pre-trattamento per ciascun paese
mse <- apply(data.li[gap.start:gap.end.pre, ]^2, 2, mean)

# MSPE per la Liberia
li.mse <- as.numeric(mse[which(colnames(data.li) == "Liberia")])

# Esclusi stati con MSPE pre-trattamento 5 volte maggiore rispetto all'Uganda
data.li <- data.li[, mse < 5 * li.mse]

# Crea il grafico
plot(years, years,
     ylim = c(-1, 1), xlab = "Year",
     xlim = c(1987, 2010), ylab = "Gap Core Civil Society Index",
     main = "In Space Placebo: Liberia",
     type = "l", lwd = 2, col = "black",
     xaxt = "n")
axis(1, at = c(1987, 1992, 1997, 2002, 2007, 2010))

# Aggiunge andamenti gap delle unità di controllo
for (i in 1:ncol(data.li)) {
  lines(years, data.li[gap.start:gap.end, i], col = "gray")
}

# traccia andamento Liberia
lines(years, data.li[gap.start:gap.end, which(colnames(data.li) == "Liberia")], lwd = 2, col = "black")

# aggiungono legenda e dettagli
abline(v = 2002, lty = 2, lwd = 2)
legend("bottomright", legend = c("Liberia", "Control Regions"),
       lty = c(1, 1), col = c("black", "gray"), lwd = c(2, 1), cex = 0.8)
arrows(2000, -0.5, 2001.5, -0.5, col = "black", length = 0.1)
text(1998, -0.49, "1000 deaths", cex = 0.8)




#### P-VALUE ####

# Gap per il paese trattato e per i controlli
treated_gap.li <- data.li[, "Liberia"]  # Gap del paese trattato
control_gaps.li <- data.li[, colnames(data.li) != "Liberia"]  # Gap dei controlli

# Periodo post-trattamento
post_treatment_years.li <- which(rownames(data.li) > 2002)

# Calcolo del massimo gap assoluto nel periodo post-trattamento
treated_max_gap.li <- max(abs(treated_gap.li[post_treatment_years.li]))
control_max_gaps.li <- apply(abs(control_gaps.li[post_treatment_years.li, ]), 2, max)

# Calcolo del p-value
p_value <- mean(control_max_gaps.li >= treated_max_gap.li)

# Calcolo dei p-value per ciascun anno post-trattamento
p_values_per_year.li <- sapply(post_treatment_years.li, function(years) {
  treated_year_gap.li <- abs(treated_gap.li[years])
  mean(abs(control_gaps.li[years, ]) >= treated_year_gap.li)
})

# Grafico dei p-value
plot(1:8, 
     p_values_per_year.li, 
     type = "p", 
     pch = 19, 
     col = "blue", 
     xlab = "Number of periods after event", 
     ylab = "P-value", 
     main = "P-values - In Space Placebo: Liberia",
     xaxt = "n"
)
axis(1, at = 1:8)



#### F-test ####

# Periodi di interesse
pre_treatment_years.li <- which(years <= 2002)  # Anni pre-trattamento

# Varianze pre e post-trattamento
liberia_variance_pre <- var(treated_gap.li[pre_treatment_years.li])
liberia_variance_post <- var(treated_gap.li[post_treatment_years.li])

# Calcolo del test-F
f_stat_liberia <- liberia_variance_post / liberia_variance_pre

# Calcolo del p-value
df1 <- length(post_treatment_years.li) - 1  # gradi di libertà per il numeratore
df2 <- length(pre_treatment_years.li) - 1  # gradi di libertà per il denominatore
p_value_liberia <- pf(f_stat_liberia, df1 = df1, df2 = df2, lower.tail = FALSE)

# Mostra i risultati
print(f_stat_liberia)
print(p_value_liberia)
