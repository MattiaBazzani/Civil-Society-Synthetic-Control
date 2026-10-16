###### Burundi ######
#### SCM a mano ####

# seleziona controlli con pesi nel paper
bu_dat <- dati %>% filter(countryname %in% c("Burundi", "Malawi", "Gabon", "Swaziland") & year %in% 1981:2009)

#crea una matrice con i valori di ccsi nei controlli negli anni di interesse
control.out.bu <- matrix(bu_dat$v2xcs_ccsi, ncol=4)
row.names(control.out.bu) <- 1981:2009
colnames(control.out.bu) <- c("Gabon", "Burundi", "Malawi", "Swaziland")

# calcola stima scm con i pesi del paper
synth.bu <- control.out.bu %*% c(0.28, 0, 0.45, 0.26)

#crea il grafico
plot(1981:2009, synth.bu, type="l", ylim=c(0, .8), xlab="year", ylab="core civil society index", main="Syntethic control a mano: Burundi", lwd=2, lty=3)
lines(1981:2009, control.out.bu[,2], type = "l",  lwd=2) 
abline(v=1996, lty=2, lwd=2)
legend("bottomright",legend=c("Burundi", "synthetic Burundi"),
       lty=c(1,3),col=c("black","black"),lwd=c(2,2),cex=.8)

#### SCM con synth ####

# year>1980 & year<1997 periodo pre trattamento

dati%>%filter(countryname=="Burundi") %>% select(ccode) %>% str() # code burundi 516

dati%>%select(countryname, ccode )%>% distinct %>% print(n=Inf) # tolgo righe duplicate, ho nome e codice nazioni dataset
# uso come controllo 402-591 ccode nazioni africa subsahariana

dati %>% filter(ccode>401 & ccode<592, year>1980 & year<1996) %>% skim()

c_control.bu <- dati %>% filter(ccode>401 & ccode<592, year>1980 & year<1996) %>% select(gdp, log_pop, hc, v2x_polyarchy, ccode, countryname) 
# no 402-411, Guinea 438, Chad 483, Djibouti 522, Comoros 581, Seychelles 591  (NA hc) , Somalia 520, Eritrea 531 (NA tutto tranne poliarchy), Mozambique 541 contiene dei missing su v2x_polyarchy

d_bu <- dati %>% filter(ccode %in% c(516, 420, 432:437, 439, 450, 451, 452, 461, 471, 475, 481, 482, 484, 490, 501, 500, 510, 516, 517, 530, 540, 541, 551:553, 560, 565, 570:572, 580, 590) & year %in% 1981:2009) 
# prendo solo le unità e gli anni di interesse 

d_bu <- d_bu %>% as.data.frame() # per evitare errori in data_prep

d_bu %>%
  group_by(ccode) %>%
  summarize(
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE),
    total_years = n_distinct(year)
  ) %>%
  arrange(min_year)%>%
  print(n=36) # controlla presenza di tutti gli anni necessari per le unità di controllo e trattamento

dataprep.out.bu <- dataprep(
  foo = d_bu,  
  
  predictors = c("gdp", "log_pop", "v2x_polyarchy", "dist_eq", "legor07", "hc" ),
  
  predictors.op = "mean", 
  
  time.predictors.prior = 1981:1996, 
  
  special.predictors = list(
    list("ccsi_t_1", 1995, "mean"),
    list("ccsi_t_8", 1988, "mean"),
    list("ccsi_t_15", 1981, "mean")
  ),
  dependent = "v2xcs_ccsi", 
  
  unit.variable = "ccode", 
  
  unit.names.variable = "countryname", 
  
  time.variable = "year", 
  
  treatment.identifier = 516, 
  
  controls.identifier = c(420, 432:437, 439, 450, 451, 452, 461, 471, 475, 481, 482, 484, 490, 500, 501, 510,                            517, 530, 540, 541, 551:553, 560, 565, 570:572, 580, 590),  # the identifiers for the control groups
  
  time.optimize.ssr = 1981:1996,  
  
  time.plot = 1981:2009 
)

# calcola stime scm

synth.out.bu <- synth(data.prep.obj = dataprep.out.bu, method = "BFGS")

synth.out.bu$solution.w #PESI GRUPPI DI CONTROLLO
synth.out.bu$solution.v #VALORI STIMATI POST TRATTAMENTO GRUPPI DI CONTROLLO
synth.out.bu$loss.v     #ADATTAMENTO GRUPPO DI CONTROLLO AL TRATTATO

synth.tables.bu <- synth.tab(dataprep.res = dataprep.out.bu,
                             synth.res    = synth.out.bu)

synth.tables.bu$tab.pred # tabella valori predittori synthetic, vengono diversi i valori di treated per legor07 e dist_eq
synth.tables.bu$tab.w # pesi nazioni controllo: Niger 0.117, Sierra Leone 0.019, Congo 0.120, Ethiopia 0.036, Rwanda 0.119, Ethipia 0.001, Angola 0.603, Mozambique 0.097

# Fa grafico confronto Burundi e synthetic Burundi ottenuto
path.plot(synth.res = synth.out.bu,
          dataprep.res = dataprep.out.bu,
          Ylab = "core civil society index",
          Xlab = "year",
          Ylim = c(0,1),
          Legend = c("Burundi","synthetic Burundi"),
          Legend.position = "bottomright"
)

abline(v = 2001, col = "red", lty = 2, lwd = 2)

#calcolo rmspe
RMSPE.bu <- sqrt(synth.out.bu$loss.v) #0.03725557

#### TEST PLACEBO ####

# Prepara la matrice per memorizzare i gap
store.bu <- matrix(NA,length(1981:2009),35)
d_bu <- d_bu %>% filter(countryname!="Tanzania")#Tanzania crea problemi, la togliamo
colnames(store.bu) <- unique(d_bu$countryname)
country_codes.bu <- unique(d_bu$ccode) # crea vettore con i codici delle nazioni


# Itera su ogni unità, 
for (iter in country_codes.bu) {
  # Prepara i dati
  dataprep.out <- dataprep(
    foo = d_bu,
    predictors = c("gdp", "log_pop", "v2x_polyarchy", "dist_eq", "legor07", "hc" ),
    
    predictors.op = "mean",  
    
    time.predictors.prior = 1981:1996, 
    
    special.predictors = list(
      list("ccsi_t_1", 1995, "mean"),
      list("ccsi_t_8", 1988, "mean"),
      list("ccsi_t_15", 1981, "mean")
    ),
    dependent = "v2xcs_ccsi",  
    
    unit.variable = "ccode",
    
    unit.names.variable = "countryname", 
    
    time.variable = "year", 
    treatment.identifier = iter,  # L'unità corrente come trattata
    controls.identifier = country_codes.bu[country_codes.bu != iter],  # Esclude l'unità corrente dai controlli
    time.optimize.ssr = 1981:1996,
    time.plot = 1981:2009
  )
  
  # Esegue scm
  synth.out <- synth(data.prep.obj = dataprep.out, method = "BFGS")
  
  # Calcola i gap e li memorizza
  store.bu[, which(country_codes.bu == iter)] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)
}



# Prepara i dati per il grafico
data.bu <- store.bu  
rownames(data.bu) <- 1981:2009  

# Limiti per i gap nel periodo di analisi
gap.start <- 1
gap.end <- nrow(data.bu)
years <- 1981:2009
gap.end.pre <- which(rownames(data.bu) == "1996")  # indice anno trattamento

# Calcola MSPE pre-trattamento per ciascun paese
mse <- apply(data.bu[gap.start:gap.end.pre, ]^2, 2, mean)

# MSPE per il burundi
bu.mse <- as.numeric(mse[which(colnames(data.bu) == "Burundi")])

# Esclusi stati con MSPE pre-trattamento 5 volte maggiore rispetto al burundi
data.bu <- data.bu[, mse < 5 * bu.mse]

# Imposta le dimensioni del testo
cex.set <- 0.75

# Crea il grafico per confrontare synthetic Burundi con gli altri 
plot(years, years,
     ylim = c(-1, 1), xlab = "Year",
     xlim = c(1981, 2009), ylab = "Gap core civil society index",
     main = "In Space Placebo: Burundi",
     type = "l", lwd = 2, col = "black",
     xaxs = "i", yaxs = "i")

# Aggiungi andamento unità di controllo
for (i in 1:ncol(data.bu)) {
  lines(years, data.bu[gap.start:gap.end, i], col = "gray")      
}

# traccia andamento Burundi
lines(years, data.bu[gap.start:gap.end, which(colnames(data.bu) == "Burundi")], lwd = 2, col = "black")

# aggiungono legenda e dettagli
abline(v=1996,lty="dotted",lwd=2)
legend("bottomright",legend=c("Burundi", "control regions"),
       lty=c(1,1),col=c("black","gray"),lwd=c(2,1),cex=.8)
arrows(1994,-0.5,1995.5,-0.5,col="black",length=.1)
text(1992.5,-0.5,"War Onset",cex=cex.set)


#### P-VALUE ####


# Gap per il paese trattato e per i controlli
treated_gap.bu <- data.bu[, "Burundi"]  # Gap del paese trattato
control_gaps.bu <- data.bu[, colnames(data.bu) != "Burundi"]  # Gap dei controlli

# Periodo post-trattamento
post_treatment_years.bu <- which(rownames(data.bu) > 1996)  # diverso e in più ho 13 valori mentre nel paper 14

# Calcolo del massimo gap assoluto nel periodo post-trattamento
treated_max_gap.bu <- max(abs(treated_gap.bu[post_treatment_years.bu]))
control_max_gaps.bu <- apply(abs(control_gaps.bu[post_treatment_years.bu, ]), 2, max)

# Calcolo del p-value
p_value <- mean(control_max_gaps.bu >= treated_max_gap.bu)

# Calcolo dei p-value per ciascun anno post-trattamento
p_values_per_year.bu <- sapply(post_treatment_years.bu, function(year) {
  treated_year_gap.bu <- abs(treated_gap.bu[year])
  mean(abs(control_gaps.bu[year, ]) >= treated_year_gap.bu)
})

# Grafico dei p-value
plot(rownames(data.bu)[post_treatment_years.bu], 
     p_values_per_year.bu, 
     type = "p", 
     pch = 19, 
     col = "blue", 
     xlab = "Anno", 
     ylab = "P-value", 
     main = "Grafico dei P-value")

#### F-test ####

# Periodi di interesse
pre_treatment_years.bu <- which(years < 1997)  # Anni pre-trattamento

# Varianze pre e post-trattamento
bu_variance_pre <- var(treated_gap.bu[pre_treatment_years.bu])
bu_variance_post <- var(treated_gap.bu[post_treatment_years.bu])

# Calcolo del test-F
f_stat_bu <- bu_variance_post / bu_variance_pre

# Calcolo del p-value
df1 <- length(post_treatment_years.bu) - 1  # gradi di libertà per il numeratore
df2 <- length(pre_treatment_years.bu) - 1  # gradi di libertà per il denominatore
p_value_bu <- pf(f_stat_bu, df1 = df1, df2 = df2, lower.tail = FALSE)

# Mostra i risultati
cat("F-Statistic (Burundi):", f_stat_bu, "\n")
cat("P-Value (Burundi):", p_value_bu, "\n")
