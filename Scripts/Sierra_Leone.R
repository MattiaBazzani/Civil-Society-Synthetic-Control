################### SIERRA LEONE #########################


#### SCM a mano ####

# seleziona controlli con pesi nel paper
sl_dat <- dati %>% filter(countryname %in% c("Sierra Leone", "Malawi", "Zambia", "Gabon", "Tanzania") & year %in% 1978:2010)

#crea una matrice con i valori di ccsi nei controlli negli anni di interesse
control.out.sl <- matrix(sl_dat$v2xcs_ccsi, ncol = 5)
row.names(control.out.sl) <- 1978:2010
colnames(control.out.sl) <- c("Sierra Leone", "Gabon", "Tanzania", "Zambia", "Malawi")

# calcola stima scm con i pesi del paper
synth.sl <- control.out.sl %*% c(0, 0.23, 0.19, 0.29, 0.29)

#crea il grafico
plot(1978:2010, synth.sl, type="l", ylim = c(0.2, 1), xlab = "Year", ylab = "Core Civil Society Index", main = "Synthetic Control Manuale: Sierra Leone", lwd = 2, lty = 3, xaxt = "n")
axis(1, at = c(1978, 1983, 1988, 1993, 1998, 2004, 2010))
lines(1978:2010, control.out.sl[, 1], type = "l",  lwd = 2) 
abline(v = 1993, lty = 2, lwd = 2)
legend("bottomright", legend = c("Sierra Leone", "Synthetic Sierra Leone"),
       lty = c(1, 3), col = c("black", "black"), lwd = c(2, 2) , cex = 0.8)



#### SCM con synth ####

#year > 1977 & year < 1994 periodo pre-trattamento

dati %>% filter(countryname == "Sierra Leone") %>% select(ccode) %>% str() # code sierra leone 451

dati %>% select(countryname, ccode) %>% distinct %>% print(n = Inf) # ho nome e codice nazioni dataset
# usa come controllo 402-591 ccode nazioni africa subsahariana

dati %>% filter(ccode > 401 & ccode < 592, year > 1977 & year < 1994) %>% skim()

c_control.sl <- dati %>% filter(ccode > 401 & ccode < 592, year > 1977 & year < 1993) %>% select(log_gdp, log_pop, hc, v2x_polyarchy, ccode, countryname)  #prendo solo le righe senza NA
# no 402-411, Guinea 438, Chad 483, Djibouti 522, Comoros 581, Seychelles 591  (NA hc) , Somalia 520, Eritra 531 (NA tutto tranne poliarchy), Mozambique 541(Na poliarchy)

control.sl = c(420, 432:437, 439, 450, 451, 452, 461, 471, 475, 481, 482, 484, 490, 
               500, 501, 510, 516, 517, 530, 540, 551:553, 560, 565, 570:572, 580, 590)

d_sl <- dati %>% filter(ccode %in% control.sl & year %in% 1978:2010) #prendo solo le unità e gli anni di interesse 


d_sl <- d_sl %>% as.data.frame() # per evitare errori in data_prep

d_sl %>%
  group_by(ccode) %>%
  summarize(
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE),
    total_years = n_distinct(year)
  ) %>%
  arrange(min_year)%>%
  print(n = Inf) # controlla presenza di tutti gli anni necessari per le unità di controllo e trattamento

dataprep.out.sl <- dataprep(
  foo = d_sl,  # dataset da utilizzare
  
  # predittori del modello
  predictors = c("log_gdp", "log_pop", "v2x_polyarchy", "dist_eq", "legor07", "hc"),
  
  predictors.op = "mean",  # operazione da applicare ai predittori
  
  time.predictors.prior = 1978:1993,  # periodo a pre trattamento 
  
  # predittori speciali (risposta  tempi ritardati rispetto all'anno del trattamento)
  special.predictors = list(
    list("ccsi_t_1", 1992, "mean"),
    list("ccsi_t_8", 1985, "mean"),
    list("ccsi_t_15", 1978, "mean")
  ),
  
  dependent = "v2xcs_ccsi",  # variabile dipendente
  
  unit.variable = "ccode",  # variabile contenente l'identificativo delle unità
  
  unit.names.variable = "countryname",  # variabile contenente il nome delle unità
  
  time.variable = "year",  # variabile temporale
  
  treatment.identifier = 451,  # identoficatore nazione di trattamento
  
  controls.identifier = c(420, 432, 433, 434, 435, 436, 437, 439, 450, 452,
                          461, 471, 475, 481, 482, 484, 490, 500, 501, 510, 516,
                          517, 530, 540, 551, 552, 553, 560, 565, 570, 571, 572, 580, 590),  # identificatori nazioni di controllo, africa sub-sahariana
  
  time.optimize.ssr = 1978:1993,  # periodo rispetto cui è minimizzata la somma dei quadrati dei residui per stimare i pesi t
  
  time.plot = 1978:2010  # periodo per i grafici
)


# calcola stime scm
synth.out.sl <- synth(data.prep.obj = dataprep.out.sl,
                      method = "BFGS")

synth.tables.sl <- synth.tab(dataprep.res = dataprep.out.sl,
                             synth.res = synth.out.sl)

synth.tables.sl$tab.pred # tabella valori predittori synthetic
synth.tables.sl$tab.w # pesi nazioni controllo: Gambia 0.12, Niger 0.186, Liberia 0.15, Gabon 0.076, Angola 0.169, Uganda 0.134, Burundi 0.163, Malawi 0.001, Namibia 0.001
synth.tables.sl$tab.v

# Estrai i dati per il grafico
years <- dataprep.out.sl$tag$time.plot
observed <- dataprep.out.sl$Y1plot
synthetic <- dataprep.out.sl$Y0plot %*% synth.out.sl$solution.w

# Crea il grafico manualmente
plot(years, observed, type = "l", col = "black", lwd = 2, ylim = c(0.2, 1),
     xlab = "Year", ylab = "Core Civil Society Index",
     main = "Synthetic Control Results: Sierra Leone", xaxt = "n")  # Disattiva l'asse x predefinito

# Aggiungi la linea del Nepal sintetico
lines(years, synthetic, lwd = 2, lty = 3)

# Aggiungi una linea verticale al 2001
abline(v = 1993, lty = 2, lwd = 2)

# Personalizza le etichette dell'asse x
axis(1, at = c(1978, 1983, 1988, 1993, 1998, 2004, 2010))

# Aggiungi una legenda
legend("bottomright", legend = c("Sierra Leone", "Synthetic Sierra Leone"),
       col = c("black", "black"), lty = c(1, 3), lwd = c(2, 2), cex = 0.8)

#calcolo rmspe
RMSPE.sl <- sqrt(synth.out.sl$loss.v) # 0.04500367



#### TEST PLACEBO ####

# Prepara la matrice per memorizzare i gap
store.sl <- matrix(NA, length(1978:2010), 35) 
colnames(store.sl) <- unique(d_sl$countryname)
country_code.sl <- unique(d_sl$ccode) # crea vettore con i codici delle nazioni

# itera su ogni unità
for(iter in c(country_code.sl)) {
  # Prepara i dati
  dataprep.out <- dataprep(
    foo = d_sl, 
    
    predictors = c("log_gdp", "log_pop", "v2x_polyarchy", "dist_eq", "legor07", "hc"),
    
    predictors.op = "mean",  
    
    time.predictors.prior = 1978:1993, 
    
    special.predictors = list(
      list("ccsi_t_1", 1992, "mean"),
      list("ccsi_t_8", 1985, "mean"),
      list("ccsi_t_15", 1978, "mean")
    ),
    
    dependent = "v2xcs_ccsi",
    
    unit.variable = "ccode",
    
    unit.names.variable = "countryname",
    
    time.variable = "year",
    
    treatment.identifier = iter, # L'unità corrente come trattata
    controls.identifier = country_code.sl[country_code.sl != iter],  # Esclude l'unità corrente dai controlli
    time.optimize.ssr = 1978:1993,  
    time.plot = 1978:2010  
  )
  
  # esegue scm
  synth.out <- synth(data.prep.obj = dataprep.out, method = "BFGS")
  
  # Calcola i gap e li memorizz
  store.sl[, which(country_code.sl == iter)] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)
}

# Prepara i dati per il grafico
data.sl <- store.sl # Usa la matrice `store` generata con i gap
rownames(data.sl) <- 1978:2010

# Limiti per i gap nel periodo di analisi
gap.start <- 1 
gap.end <- nrow(data.sl) 
years <- 1978:2010
gap.end.pre <- which(rownames(data.sl) == "1993") # indice anno trattamento


# Calcola MSPE pre-Trattamento per ciascun paese
mse <- apply(data.sl[gap.start:gap.end.pre,]^2, 2, mean)

# MSPE per Sierra Leone
sl.mse <- as.numeric(mse[which(colnames(data.sl) == "Sierra Leone")])

# Esclusi stati con MSPE pre-trattamento 5 volte maggiore rispetto al Sierra Leone
data.sl <- data.sl[, mse < 5 * sl.mse]


# Crea il grafico per confrontare synthetic Sierra con gli altri 
plot(years, years,
     ylim = c(-1, 1), xlab = "Year",
     xlim = c(1978, 2010), ylab = "Gap Core Civil Society Index",
     main = "In Space Placebo: Sierra Leone",
     type = "l", lwd = 2, col = "black",
     xaxt = "n")
axis(1, at = c(1978, 1983, 1988, 1993, 1998, 2004, 2010))

# Aggiunge andamenti gap delle unità di controllo
for (i in 1:ncol(data.sl)) { 
  lines(years,data.sl[gap.start:gap.end,i], col = "gray") 
  }

# traccia andamento Sierra Leone
lines(years, data.sl[gap.start:gap.end, which(colnames(data.sl) == "Sierra Leone")], lwd = 2, col = "black")

# aggiungono legenda e dettagli
abline(v = 1993, lty = 2, lwd = 2)
legend("bottomright", legend=c("Sierra Leone", "Control Regions"),
       lty = c(1, 1), col = c("black", "gray"), lwd = c(2, 2), cex = 0.8)
arrows(1991, -0.5, 1992.5, -0.5, col = "black", length = 0.1)
text(1988.5, -0.49, "1000 deaths", cex = 0.8)




#### P-VALUE ####


# Gap per il paese trattato e per i controlli
treated_gap.sl <- data.sl[, "Sierra Leone"]  # Gap del paese trattato
control_gaps.sl <- data.sl[, colnames(data.sl) != "Sierra Leone"]  # Gap dei controlli

# Periodo post-trattamento
post_treatment_years.sl <- which(rownames(data.sl) >= 1994)

# Calcolo del massimo gap assoluto nel periodo post-trattamento
treated_max_gap.sl <- max(abs(treated_gap.sl[post_treatment_years.sl]))
control_max_gaps.sl <- apply(abs(control_gaps.sl[post_treatment_years.sl, ]), 2, max)

# Calcolo del p-value
p_value.sl <- mean(control_max_gaps.sl >= treated_max_gap.sl)

# Calcolo dei p-value per ciascun anno post-trattamento
p_values_per_year.sl <- sapply(post_treatment_years.sl, function(year) {
  treated_year_gap.sl <- abs(treated_gap.sl[year])
  mean(abs(control_gaps.sl[year, ]) >= treated_year_gap.sl)
})

# Grafico dei p-value
plot(1:17, 
     p_values_per_year.sl, 
     type = "p", 
     pch = 19, 
     col = "blue", 
     xlab = "Number of periods after event", 
     ylab = "P-value", 
     main = "P-values - In Space Placebo: Sierra Leone",
     xaxt = "n"
)
axis(1, at = 1:17)



#### F-test ####

# Periodi di interesse
pre_treatment_years.sl <- which(years <= 1993)  # Anni pre-trattamento

# Varianze pre e post-trattamento
sierra_variance_pre <- var(treated_gap.sl[pre_treatment_years.sl])
sierra_variance_post <- var(treated_gap.sl[post_treatment_years.sl])

# Calcolo del test-F
f_stat_sierra <- sierra_variance_post / sierra_variance_pre

# Calcolo del p-value
df1 <- length(post_treatment_years.sl) - 1  # gradi di libertà per il numeratore
df2 <- length(pre_treatment_years.sl) - 1  # gradi di libertà per il denominatore
p_value_sierra <- pf(f_stat_sierra, df1 = df1, df2 = df2, lower.tail = FALSE)

# Mostra i risultati
print(f_stat_sierra)
print(p_value_sierra)

