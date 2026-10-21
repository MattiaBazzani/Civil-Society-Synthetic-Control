###### NEPAL ###### 

#### SCM a mano ####

# seleziona controlli con pesi nel paper
nep_dat <- dati %>% filter(countryname %in% c("Nepal", "Mongolia", "Fiji", "Vietnam") & year %in% 1986:2010)

#crea una matrice con i valori di ccsi nei controlli negli anni di interesse
control.out.ne <- matrix(nep_dat$v2xcs_ccsi, ncol = 4)
row.names(control.out.ne) <- 1986:2010
colnames(control.out.ne) <- c("Mongolia", "Nepal", "Vietnam", "Fiji")

# calcola stima scm con i pesi del paper
synth.ne <- control.out.ne %*% c(0.41, 0, 0.12, 0.47)

#crea il grafico
plot(1986:2010, synth.ne, type = "l", ylim = c(0.4, 1), xlab = "Year", ylab = "Core Civil Society Index", main = "Synthetic Control Manuale: Nepal", lwd = 2, lty = 3, xaxt = "n")
axis(1, at = c(1986, 1991, 1996, 2001, 2006, 2010))
lines(1986:2010, control.out.ne[, 2], type = "l", lwd = 2) 
abline(v = 2001, lty = 2, lwd = 2)
legend("bottomright", legend = c("Nepal", "Synthetic Nepal"),
       lty = c(1, 3), col = c("black","black"),lwd = c(2, 2), cex = .8)



#### SCM con synth ####

# year > 1985 & year < 2002 periodo pre-trattamento

dati%>%filter(countryname == "Nepal") %>% select(ccode) %>% str() # code nepal 790

dati%>%select(countryname, ccode) %>% distinct %>% print(n = Inf) #tolgo righe duplicate, ho nome e codice nazioni dataset
# uso come controllo 630-860 ccode nazioni asia meridionale
#630, 640, 645, 651, 652, 660, 663, 666,  

dati %>% filter(ccode %in% c(700, 712, 750, 760, 770, 771, 780, 781, 790, 816, 900, 950), year > 1985 & year < 2002) %>% skim()

c_control.ne <- dati %>% filter(ccode %in% c(700, 712, 750, 760, 770, 771, 780, 781, 790, 816, 900, 950), year > 1985 & year < 2002) %>% select(log_gdp, log_pop, hc, ccode, countryname) 
# 700 afghanistan no log_pop e gdp, 760 bhutan no hc, 

control.ne <- c(712, 750, 770, 771, 780, 781, 790, 900, 816, 950)

d_ne <- dati %>% filter(ccode %in% control.ne & year %in% 1986:2010) # prendo solo le unità e gli anni di interesse

d_ne %>%
  group_by(ccode) %>%
  summarize(
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE),
    total_years = n_distinct(year)
  ) %>%
  arrange(min_year) # controlla presenza di tutti gli anni necessari per le unità di controllo e trattamento

d_ne <- d_ne %>% as.data.frame() # per evitare errori in data_prep

dataprep.out.ne <- dataprep(
  foo = d_ne,  
  
  predictors = c("log_gdp", "log_pop", "v2x_polyarchy", "dist_eq", "legor07", "hc"),
  
  predictors.op = "mean",  
  
  time.predictors.prior = 1986:2001,  
  
  special.predictors = list(
    list("ccsi_t_1", 2000, "mean"),
    list("ccsi_t_8", 1993, "mean"),
    list("ccsi_t_15", 1986, "mean")
  ),
  
  dependent = "v2xcs_ccsi",  
  
  unit.variable = "ccode",  
  
  unit.names.variable = "countryname",  
  
  time.variable = "year",  
  
  treatment.identifier = 790,  
  
  controls.identifier = c(712, 750, 770, 771, 780, 781, 816, 900, 950),  
  
  time.optimize.ssr = 1986:2001,  
  
  time.plot = 1986:2010  
)

# calcola stime scm

synth.out.ne <- synth(data.prep.obj = dataprep.out.ne,
                      method = "BFGS")


synth.tables.ne <- synth.tab(dataprep.res = dataprep.out.ne,
                             synth.res = synth.out.ne)

synth.tables.ne$tab.pred # tabella valori predittori synthetic
synth.tables.ne$tab.w # pesi nazioni controllo: Mongolia 0.324, India 0.326, Pakistan 0.057, Bangladesh 0.196, Maldives 0.095, Fiji 0.002
synth.tables.ne$tab.v

 
# Estrai i dati per il grafico
years <- dataprep.out.ne$tag$time.plot
observed <- dataprep.out.ne$Y1plot
synthetic <- dataprep.out.ne$Y0plot %*% synth.out.ne$solution.w

# Crea il grafico manualmente
plot(years, observed, type = "l", col = "black", lwd = 2, ylim = c(0.4, 1),
     xlab = "Year", ylab = "Core Civil Society Index",
     main = "Synthetic Control Results: Nepal", xaxt = "n")  # Disattiva l'asse x predefinito

# Aggiungi la linea del Nepal sintetico
lines(years, synthetic, lwd = 2, lty = 3)

# Aggiungi una linea verticale al 2001
abline(v = 2001, lty = 2, lwd = 2)

# Personalizza le etichette dell'asse x
axis(1, at = c(1986, 1991, 1996, 2001, 2006, 2010))

# Aggiungi una legenda
legend("bottomright", legend = c("Nepal", "Synthetic Nepal"),
       col = c("black", "black"), lty = c(1, 3), lwd = c(2, 2), cex = 0.8)


#calcolo rmspe
RMSPE.ne <- sqrt(synth.out.ne$loss.v) #0.07654419


#### TEST PLACEBO ####

# Prepara la matrice per memorizzare i gap
store.ne <- matrix(NA, length(1986:2010), 10)
colnames(store.ne) <- unique(d_ne$countryname)
country_codes.ne <- unique(d_ne$ccode) # crea vettore con i codici delle nazioni

# itera su ogni unità
for(iter in country_codes.ne) {
  # Prepara i dati
  dataprep.out <- dataprep(
    foo = d_ne,  
    
    predictors = c("log_gdp", "log_pop", "v2x_polyarchy", "dist_eq", "legor07", "hc"),
    
    predictors.op = "mean",  
    
    time.predictors.prior = 1986:2001,  
    
    special.predictors = list(
      list("ccsi_t_1", 2000, "mean"),
      list("ccsi_t_8", 1993, "mean"),
      list("ccsi_t_15", 1986, "mean")
    ),
    
    dependent = "v2xcs_ccsi",  
    
    unit.variable = "ccode",  
    
    unit.names.variable = "countryname", 
    
    time.variable = "year",  
    
    treatment.identifier = iter, # L'unità corrente come trattata
    controls.identifier = country_codes.ne[country_codes.ne != iter], # Esclude l'unità corrente dai controlli
    time.optimize.ssr = 1986:2001,  
    time.plot = 1986:2010 
  )
  
  # Esegue scm
  synth.out <- synth(data.prep.obj = dataprep.out, method = "BFGS")
  
  # Calcola i gap e li memorizza
  store.ne[, which(country_codes.ne == iter)] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)
}


# Prepara i dati per il grafico
data.ne <- store.ne
rownames(data.ne) <- 1986:2010

# Limiti per i gap nel periodo di analisi
gap.start <- 1
gap.end <- nrow(data.ne)
years <- 1986:2010
gap.end.pre <- which(rownames(data.ne) == "2001")  # indice anno trattamento

# Calcola MSPE pre-trattamento per ciascun paese
mse <- apply(data.ne[gap.start:gap.end.pre, ]^2, 2, mean)
 
# MSPE per Sierra Leone
ne.mse <- as.numeric(mse[which(colnames(data.ne) == "Nepal")])
 
# Esclusi stati con MSPE pre-trattamento 5 volte maggiore rispetto al Nepal
data.ne <- data.ne[, mse < 5 * ne.mse]


# Crea il grafico per confrontare synthetic Nepal con gli altri 
plot(years, years,
     ylim = c(-1, 1), xlab = "Year",
     xlim = c(1986, 2010), ylab = "Gap Core Civil Society Index",
     main = "In Space Placebo: Nepal",
     type = "l", lwd = 2, col = "black", xaxt = "n")
axis(1, at = c(1986, 1991, 1996, 2001, 2006, 2010))

# Aggiunge andamento delle unità di controllo
for (i in 1:ncol(data.ne)) { 
  lines(years, data.ne[gap.start:gap.end, i], col = "gray") 
  }

# traccia andamento Nepal
lines(years, data.ne[gap.start:gap.end, which(colnames(data.ne) == "Nepal")], lwd = 2, col="black")

# aggiungono legenda e dettagli
abline(v = 2001, lty = 2, lwd = 2)
legend("bottomright", legend = c("Nepal", "Control Regions"),
       lty = c(1, 1), col = c("black", "gray"), lwd = c(2, 2), cex = 0.8)
arrows(1999, -0.5, 2000.5, -0.5, col = "black", length = 0.1)
text(1997, -0.49, "1000 deaths" ,cex = 0.8)



#### P-VALUE ####


# Gap per il paese trattato e per i controlli
treated_gap.ne <- data.ne[, "Nepal"]  # Gap del paese trattato
control_gaps.ne <- data.ne[, colnames(data.ne) != "Nepal"]  # Gap dei controlli

# Periodo post-trattamento
post_treatment_years.ne <- which(rownames(data.ne) > 2001)

# Calcolo del massimo gap assoluto nel periodo post-trattamento
treated_max_gap.ne <- max(abs(treated_gap.ne[post_treatment_years.ne]))
control_max_gaps.ne <- apply(abs(control_gaps.ne[post_treatment_years.ne, ]), 2, max)

# Calcolo del p-value
p_value <- mean(control_max_gaps.ne >= treated_max_gap.ne)

# Calcolo dei p-value per ciascun anno post-trattamento
p_values_per_year.ne <- sapply(post_treatment_years.ne, function(years) {
  treated_year_gap.ne <- abs(treated_gap.ne[years])
  mean(abs(control_gaps.ne[years, ]) >= treated_year_gap.ne)
})

# Grafico dei p-value
plot(1:9, 
     p_values_per_year.ne, 
     type = "p", 
     pch = 19, 
     col = "blue", 
     xlab = "Number of periods after event", 
     ylab = "P-value", 
     main = "P-values - In Space Placebo: Nepal",
     xaxt = "n"
     )
axis(1, at = 1:9)

#### F-test ####

# Periodi di interesse
pre_treatment_years.ne <- which(years <= 2001)  # Anni pre-trattamento

# Varianze pre e post-trattamento
nepal_variance_pre <- var(treated_gap.ne[pre_treatment_years.ne])
nepal_variance_post <- var(treated_gap.ne[post_treatment_years.ne])

# Calcolo del test-F
f_stat_nepal <- nepal_variance_post / nepal_variance_pre

# Calcolo del p-value
df1 <- length(post_treatment_years.ne) - 1  # gradi di libertà per il numeratore
df2 <- length(pre_treatment_years.ne) - 1  # gradi di libertà per il denominatore
p_value_nepal <- pf(f_stat_nepal, df1 = df1, df2 = df2, lower.tail = FALSE)

# Mostra i risultati
print(f_stat_nepal)
print(p_value_nepal)
