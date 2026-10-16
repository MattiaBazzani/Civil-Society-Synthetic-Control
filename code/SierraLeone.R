################### SIERRA LEONE #########################
#### SCM a mano ####

# seleziona controlli con pesi nel paper
sl_dat <- dati %>% filter(countryname %in% c("Sierra Leone", "Malawi", "Zambia", "Gabon", "Tanzania") & year %in% 1978:2010)

#crea una matrice con i valori di ccsi nei controlli negli anni di interesse
control.out.sl <- matrix(sl_dat$v2xcs_ccsi, ncol=5)
row.names(control.out.sl) <- 1978:2010
colnames(control.out.sl) <- c("Sierra Leone", "Gabon", "Tanzania", "Zambia", "Malawi")

# calcola stima scm con i pesi del paper
synth.sl <- control.out.sl %*% c(0, 0.23, 0.19, 0.29, 0.29)

#crea il grafico
plot(1978:2010, synth.sl, type="l", ylim=c(0.2, 1), xlab="year", ylab="core civil society index", main="Syntethic control a mano: Sierra Leone", lwd=2, lty=3)
lines(1978:2010, control.out.sl[,1], type = "l",  lwd = 2) 
abline(v=1993, lty=2, lwd=2)
legend("bottomright",legend=c("Sierra Leone", "synthetic Sierra Leone"),
       lty=c(1,3),col=c("black","black"),lwd=c(2,2),cex=.8)

#### SCM con synth ####

#year>1977 & year<1994 periodo pre trattamento

dati%>%filter(countryname=="Sierra Leone") %>% select(ccode) %>% str() # code sierra leone 451

dati%>%select(countryname, ccode )%>% distinct %>% print(n=Inf) #tolglie righe duplicate, ho nome e codice nazioni dataset
# usa come controllo 402-591 ccode nazioni africa subsahariana

dati %>% filter(ccode>401 & ccode<592, year>1977 & year<1994) %>% skim()

c_control.sl <- dati %>% filter(ccode>401 & ccode<592, year>1977 & year<1993)%>%filter(complete.cases(.)) #prendo solo le righe senza NA
# no 402-411, Guinea 438, Chad 483, Djibouti 522, Comoros 581, Seychelles 591  (NA hc) , Somalia 520, Eritra 531 (NA tutto tranne poliarchy), Mozambique 541(Na poliarchy)

d_sl <- dati %>% filter(ccode %in% c(420, 432, 433, 434, 435, 436, 437, 439, 450, 451, 452,
                                     461, 471, 475, 481, 482, 484, 490, 500, 501, 510, 516,
                                     517, 530, 540, 551, 552, 553, 560, 565, 570, 571, 572) & year %in% 1978:2010) #prendo solo le unità e gli anni di interesse 


d_sl <- d_sl %>% as.data.frame() # per evitare errori in data_prep

d_sl %>%
  group_by(ccode) %>%
  summarize(
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE),
    total_years = n_distinct(year)
  ) %>%
  arrange(min_year)%>%
  print(n=34) # controlla presenza di tutti gli anni necessari per le unità di controllo e trattamento

dataprep.out.sl <- dataprep(
  foo = d_sl,  # dataset da utilizzare
  
  # predittori del modello
  predictors = c("gdp", "log_pop", "v2x_polyarchy", "dist_eq", "legor07", "hc"),
  
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
                          517, 530, 540, 551, 552, 553, 560, 565, 570, 571, 572),  # tidentificatori nazioni di controllo, africa sub-sahariana
  
  time.optimize.ssr = 1978:1993,  # periodo rispetto cui è minimizzata la somma dei quadrati dei residui per stimare i pesi t
  
  time.plot = 1978:2010  # periodo per i grafici
)


# calcola stime scm
synth.out.sl <- synth(data.prep.obj = dataprep.out.sl,
                      method = "BFGS")

synth.out.sl$solution.w #PESI GRUPPI DI CONTROLLO
synth.out.sl$solution.v #VALORI STIMATI POST TRATTAMENTO GRUPPI DI CONTROLLO
synth.out.sl$loss.v     #ADATTAMENTO GRUPPO DI CONTROLLO AL TRATTATO

synth.tables.sl <- synth.tab(dataprep.res = dataprep.out.sl,
                             synth.res    = synth.out.sl)

synth.tables.sl$tab.pred # tabella valori predittori synthetic
synth.tables.sl$tab.w # pesi nazioni controllo: Gambia 0.119, Benin 0.013, Niger 0.169, Liberia 0.127, Gabon 0.046, Angola 0.284, Uganda 0.182, Burundi 0.121, Angola 0.221, Malawi 0.002

# Fa grafico confronto Sierra-Leone e synthetic Sierra-Leone ottenuto
path.plot(synth.res = synth.out.sl,
          dataprep.res = dataprep.out.sl,
          Ylab = "core civil society index",
          Xlab = "year",
          Ylim = c(0,1),
          Main = "Synthetic Control Results: Sierra Leone",
          Legend = c("Sierra Leone","synthetic Sierra Leone"),
          Legend.position = "bottomright"
)

abline(v = 1993, col = "red", lty = 2, lwd = 2)

#calcolo rmspe
RMSPE.sl <- sqrt(synth.out.sl$loss.v) #0.04273016


#### TEST PLACEBO ####

# Prepara la matrice per memorizzare i gap
store.sl <- matrix(NA,length(1978:2010),33) 
colnames(store.sl) <- unique(d_sl$countryname)
country_code.sl <- unique(d_sl$ccode) # crea vettore con i codici delle nazioni

# itera su ogni unità
for(iter in c(country_code.sl)) {
  # Prepara i dati
  dataprep.out <- dataprep(
    foo = d_sl, 
    
    predictors = c("gdp", "log_pop", "v2x_polyarchy", "dist_eq", "legor07", "hc"),
    
    predictors.op = "mean",  
    
    time.predictors.prior = 1978:1992, 
    
    special.predictors = list(
      list("ccsi_t_1", 1992, "mean"),
      list("ccsi_t_8", 1984, "mean"),
      list("ccsi_t_15", 1978, "mean")
    ),
    
    dependent = "v2xcs_ccsi",
    
    unit.variable = "ccode",
    
    unit.names.variable = "countryname",
    
    time.variable = "year",
    
    treatment.identifier = iter, # L'unità corrente come trattata
    controls.identifier = country_code.sl[country_code.sl != iter],  # Esclude l'unità corrente dai controlli
    time.optimize.ssr = 1978:1992,  
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
gap.start     <- 1 
gap.end       <- nrow(data.sl) 
years         <- 1978:2010
gap.end.pre  <- which(rownames(data.sl)=="1993") # indice anno trattamento


# Calcola MSPE pre-Trattamento per ciascun paese
mse <- apply(data.sl[ gap.start:gap.end.pre,]^2,2,mean)

# MSPE per Sierra Leone
sl.mse <- as.numeric(mse[which(colnames(data.sl) == "Sierra Leone")])

# Esclusi stati con MSPE pre-trattamento 5 volte maggiore rispetto al Sierra Leone
data.sl <- data.sl[,mse<5*sl.mse]

# Imposta le dimensioni del testo
cex.set <- .75

# Crea il grafico per confrontare synthetic Sierra con gli altri 
plot(years,years,
     ylim=c(-1,1),xlab="Year",
     xlim=c(1978,2010),ylab="Gap core civil society index",
     main = "In Space Placebo: Sierra Leone",
     type="l",lwd=2,col="black",
     xaxs="i",yaxs="i")

# Aggiunge andamenti gap delle unità di controllo
for (i in 1:ncol(data.sl)) { 
  lines(years,data.sl[gap.start:gap.end,i],col="gray") 
  }

# traccia andamento Sierra Leone
lines(years, data.sl[gap.start:gap.end, which(colnames(data.sl)=="Sierra Leone")], lwd=2, col="black")

# aggiungono legenda e dettagli
abline(v=1993,lty="dotted",lwd=2)
legend("bottomright",legend=c("Sierra Leone", "control regions"),
       lty=c(1,1),col=c("black","gray"),lwd=c(2,1),cex=.8)
arrows(1991,-0.5,1992.5,-0.5,col="black",length=.1)
text(1988.5,-0.5,"War Onset",cex=cex.set)


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
plot(rownames(data.sl)[post_treatment_years.sl], 
     p_values_per_year.sl, 
     type = "p", 
     pch = 19, 
     col = "blue", 
     xlab = "Anno", 
     ylab = "P-value", 
     main = "Grafico dei P-value")

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
cat("F-Statistic (Sierra):", f_stat_sierra, "\n")
cat("P-Value (Sierra):", p_value_sierra, "\n")

