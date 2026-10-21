################### UGANDA #########################
#### SCM a mano ####

# seleziona controlli con pesi nel paper
ug_dat <- dati %>% filter(countryname %in% c("Uganda", "Malawi", "Benin", "Tanzania") & year %in% 1964:1999)

#crea una matrice con i valori di ccsi nei controlli negli anni di interesse
control.out.ug <- matrix(ug_dat$v2xcs_ccsi, ncol=4)
row.names(control.out.ug) <- 1964:1999
colnames(control.out.ug) <- c("Benin", "Uganda", "Tanzania", "Malawi")

# calcola stima scm con i pesi del paper
synth.ug <- control.out.ug %*% c(0.58, 0, 0.13, 0.29)

#crea il grafico
plot(1964:1999, synth.ug, type="l", ylim=c(.2, .85), xlab="year", ylab="core civil society index", main="Syntethic control a mano: Uganda", lwd=2, lty=3)
lines(1964:1999, control.out.ug[,2], type = "l",  lwd=2) 
abline(v=1979, lty=2, lwd=2)
legend("bottomright",legend=c("Uganda", "synthetic Uganda"),
       lty=c(1,3),col=c("black","black"),lwd=c(2,2),cex=.8)

#### Scm con synth ####

#year>1963 & year<1980 periodo pre trattamento

dati%>%filter(countryname=="Uganda") %>% select(ccode) %>% str() # code uganda 500

dati%>%select(countryname, ccode )%>% distinct %>% print(n=Inf) # tolgo righe duplicate, ho nome e codice nazioni dataset
# uso come controllo 402-591 ccode nazioni africa subsahariana

dati %>% filter(ccode>401 & ccode<592, year>1963 & year<1980) %>% skim()

c_control.ug <- dati %>% filter(ccode>401 & ccode<592, year>1963 & year<1980) %>% select(gdp, log_pop, hc, v2x_polyarchy, ccode, countryname) 
# no 402-411, Guinea 438, Chad 483, Djibouti 522, Comoros 581, Seychelles 591  (NA hc) , Somalia 520, Eritrea 531 (NA tutto tranne poliarchy), Mozambique 541(Na poliarchy) # ANGOLA E SWAZILAND (540 E 572) contengono dei missing ma tenuti perché aggiungono informazioni

d_ug <- dati %>% filter(ccode %in% c(500, 420, 432:437, 439, 450, 451, 452, 461, 471, 475, 481, 482, 484, 490, 501, 510, 516, 517, 530, 540, 541, 551:553, 560, 565, 570:572, 580, 590) & year %in% 1964:1999) 
# prendo solo le unità e gli anni di interesse 

d_ug <- d_ug %>% as.data.frame() # per evitare errori in data_prep

d_ug %>%
  group_by(ccode) %>%
  summarize(
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE),
    total_years = n_distinct(year)
  ) %>%
  arrange(min_year)%>%
  print(n=34) # controlla presenza di tutti gli anni necessari per le unità di controllo e trattamento


dataprep.out.ug <- dataprep(
  foo = d_ug,  
  
  predictors = c("gdp", "log_pop", "v2x_polyarchy", "dist_eq", "legor07", "hc" ),
  
  predictors.op = "mean",  
  
  time.predictors.prior = 1964:1979, 
  
  special.predictors = list(
    list("ccsi_t_1", 1978, "mean"),
    list("ccsi_t_8", 1971, "mean"),
    list("ccsi_t_15", 1964, "mean")
  ),
  dependent = "v2xcs_ccsi",
  
  unit.variable = "ccode",  
  
  unit.names.variable = "countryname",  
  
  time.variable = "year",  
  
  treatment.identifier = 500,  
  
  controls.identifier = c(420, 432:437, 439, 450, 451, 452, 461, 471, 475, 481, 482, 484, 490, 501, 510, 516, 517, 530, 540, 541, 551:553, 560, 565, 570:572, 580, 590),  #
  
  time.optimize.ssr = 1964:1979, 
  
  time.plot = 1964:1999  
)

# calcola stime scm
synth.out.ug <- synth(data.prep.obj = dataprep.out.ug, method = "BFGS")

synth.out.ug$solution.w #PESI GRUPPI DI CONTROLLO
synth.out.ug$solution.v #VALORI STIMATI POST TRATTAMENTO GRUPPI DI CONTROLLO
synth.out.ug$loss.v     #ADATTAMENTO GRUPPO DI CONTROLLO AL TRATTATO


synth.tables.ug <- synth.tab(dataprep.res = dataprep.out.ug, synth.res = synth.out.ug)

synth.tables.ug$tab.pred # tabella valori predittori synthetic
synth.tables.ug$tab.w # pesi nazioni controllo: Mauritania 0.003, Nigeria 0.026, Gabon 0.773, Rwanda 0.140, Zambia 0.058

#Fa grafico confronto Uganda e synthetic Uganda ottenuto
path.plot(synth.res = synth.out.ug,
          dataprep.res = dataprep.out.ug,
          Ylab = "core civil society index",
          Xlab = "Year", 
          Ylim = c(0,1),
          Main = "Synthetic Control Results: Uganda",
          Legend = c("Uganda","Synthetic Uganda"),
          Legend.position = "bottomright")


abline(v = 1979, col="red", lty = 2, lwd = 2)

#calcolo rmspe
RMSPE.ug <- sqrt(synth.out.ug$loss.v) #0.04001802

#### TEST PLACEBO ####

# Prepara la matrice per memorizzare i gap
store.ug <- matrix(NA, length(1964:1999), 35)
d_ug <- d_ug %>% filter(countryname!="Burundi") # il Burundi da problemi, lo eliminiamo
colnames(store.ug) <- unique(d_ug$countryname)
country_code.ug <- unique(d_ug$ccode) # crea vettore con i codici delle nazioni

# Itera su ogni unità 
for (iter in country_code.ug) {
  # Prepara i dati
  dataprep.out <- dataprep(
    foo = d_ug,
    predictors = c("gdp", "log_pop", "v2x_polyarchy", "dist_eq", "legor07", "hc" ),
    
    predictors.op = "mean",  
    
    time.predictors.prior = 1964:1979,  
    
    
    special.predictors = list(
      list("ccsi_t_1", 1978, "mean"),
      list("ccsi_t_8", 1971, "mean"),
      list("ccsi_t_15", 1964, "mean")
    ),
    dependent = "v2xcs_ccsi", 
    
    unit.variable = "ccode", 
    
    unit.names.variable = "countryname", 
    
    time.variable = "year", 
    treatment.identifier = iter,  # L'unità corrente come trattata
    controls.identifier = country_code.ug[country_code.ug != iter],  # Esclude l'unità corrente dai controlli
    time.optimize.ssr = 1964:1979,
    time.plot = 1964:1999
  )
  
  # Esegue scm
  synth.out <- synth(data.prep.obj = dataprep.out, method = "BFGS")
  
  # Calcola i gap e li memorizza
  store.ug[, which(country_code.ug == iter)] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)
}

# Prepara i dati per il grafico
data.ug <- store.ug  # Usa la matrice `store` generata con i gap
rownames(data.ug) <- 1964:1999  

# Limiti per i gap nel periodo di analisi
gap.start <- 1
gap.end <- nrow(data.ug)
years <- 1964:1999
gap.end.pre <- which(rownames(data.ug) == "1979")  # indice anno trattamento

# Calcola MSPE pre-trattamento per ciascun paese
mse <- apply(data.ug[gap.start:gap.end.pre, ]^2, 2, mean)

# MSPE per l'Uganda
ug.mse <- as.numeric(mse[which(colnames(data.ug) == "Uganda")])

# Esclusi stati con MSPE pre-trattamento 5 volte maggiore rispetto all'Uganda
data.ug <- data.ug[, mse < 5 * ug.mse]

# Imposta le dimensioni del testo
cex.set <- 0.75

# Crea il grafico
plot(years, years,
     ylim = c(-1, 1), xlab = "Year",
     xlim = c(1964, 1999), ylab = "Gap core civil society index",
     main = "In Space Placebo: Uganda",
     type = "l", lwd = 2, col = "black",
     xaxs = "i", yaxs = "i")

# Aggiunge andamento gap delle unità di controllo
for (i in 1:ncol(data.ug)) {
  lines(years, data.ug[gap.start:gap.end, i], col = "gray")
}

# traccia andamento Uganda
lines(years, data.ug[gap.start:gap.end, which(colnames(data.ug) == "Uganda")], lwd = 2, col = "black")

# aggiungono legenda e dettagli
abline(v = 1979, lty = "dotted", lwd = 1)
legend("bottomright", 
       legend = c("Uganda", "control regions"),
       lty = c(1, 1), col = c("black", "gray"), lwd = c(2, 1), cex = 0.8)
arrows(1977,-0.5,1978.5,-0.5,col="black",length=.1)
text(1974.5,-0.5,"War Onset",cex=cex.set)


#### P-VALUE ####


# Gap per il paese trattato e per i controlli
treated_gap.ug <- data.ug[, "Uganda"]  # Gap del paese trattato
control_gaps.ug <- data.ug[, colnames(data.ug) != "Uganda"]  # Gap dei controlli

# Periodo post-trattamento
post_treatment_years.ug <- which(rownames(data.ug) >= 1979)

# Calcolo del massimo gap assoluto nel periodo post-trattamento
treated_max_gap.ug <- max(abs(treated_gap.ug[post_treatment_years.ug]))
control_max_gaps.ug <- apply(abs(control_gaps.ug[post_treatment_years.ug, ]), 2, max)

# Calcolo del p-value
p_value.ug <- mean(control_max_gaps.ug >= treated_max_gap.ug)

# Calcolo dei p-value per ciascun anno post-trattamento
p_values_per_year.ug <- sapply(post_treatment_years.ug, function(year) {
  treated_year_gap.ug <- abs(treated_gap.ug[year])
  mean(abs(control_gaps.ug[year, ]) >= treated_year_gap.ug)
})

# Grafico dei p-value
plot(rownames(data.ug)[post_treatment_years.ug], 
     p_values_per_year.ug, 
     type = "p", 
     pch = 19, 
     col = "blue", 
     xlab = "Anno", 
     ylab = "P-value", 
     main = "Grafico dei P-value")

#### F-test ####

# Periodi di interesse
pre_treatment_years.ug <- which(years < 1979)  # Anni pre-trattamento

# Varianze pre e post-trattamento
uganda_variance_pre <- var(treated_gap.ug[pre_treatment_years.ug])
uganda_variance_post <- var(treated_gap.ug[post_treatment_years.ug])

# Calcolo del test-F
f_stat_uganda <- uganda_variance_post / uganda_variance_pre

# Calcolo del p-value
df1 <- length(post_treatment_years.ug) - 1  # gradi di libertà per il numeratore
df2 <- length(pre_treatment_years.ug) - 1  # gradi di libertà per il denominatore
p_value_uganda <- pf(f_stat_uganda, df1 = df1, df2 = df2, lower.tail = FALSE)

# Mostra i risultati
cat("F-Statistic (Uganda):", f_stat_uganda, "\n")
cat("P-Value (Uganda):", p_value_uganda, "\n")
