# Replica: Violent Conflict and the Strength of Civil Society

> **Progetto universitario di replica dell'analisi empirica di O'Reilly (2021) per valutare l'impatto dei conflitti civili sulla società civile tramite Synthetic Control Method.**

---

## Citazione del Paper e Note sul Copyright

> **O'Reilly, C. (2021).** *Violent Conflict and the Strength of Civil Society*. [10.2139/ssrn.3925357]

*Nota sul Copyright:* In rispetto dei diritti di autore e di pubblicazione dell'editore, il PDF del paper originale **non è stato incluso** in questa repository. L'articolo è consultabile tramite le banche dati accademiche ufficiali o tramite il relativo codice DOI allegato sopra.

**Autori dello studio di replica:**
* **Mattia Bazzani**
* **Alberto Borgese**
* **Andrea Gaspari**

---

## Panoramica del Progetto

| | |
|---|---|
| **Obiettivo** | Rispondere alla domanda *"Can violent conflict in the form of civil war affect the strength of civil society at the country level?"* replicando lo studio su 5 paesi: Burundi, Liberia, Nepal, Sierra Leone e Uganda. |
| **Dati** | Dataset composito basato su Armed Conflict Dataset (UCDP), V-Dem (*Core Civil Society Index*) e Penn World Tables (GDP, Human Capital). |
| **Metodologia** | Synthetic Control Method (SCM) per la stima causale e In-Space Placebo Tests per l'inferenza statistica. |
| **Stack Tecnologico** | **R** (`Synth`, `tidyverse`, `haven`, `skimr`) per l'elaborazione dati e la modellazione statistica. |
| **Deliverable** | Presentazione completa dei risultati e delle discrepanze: [`docs/Report.pdf`](docs/Report.pdf) |

---

## Pipeline Analitica

1. **Preparazione Dati:** Importazione del dataset unificato (`vdem_merged_full.dta`), gestione dei valori mancanti (NA) e isolamento delle covariate pre-trattamento (PIL, Capitale Umano, Democrazia Elettorale).
2. **Analisi Esplorativa (EDA):** Generazione delle statistiche descrittive e mappatura visiva del *Core Civil Society Index* dei paesi trattati rispetto alla media regionale.
3. **Synthetic Control Method:** Configurazione del problema di ottimizzazione (`dataprep`) per generare un "controllo sintetico" (una combinazione lineare pesata dei paesi del *donor pool*) che approssimi la traiettoria pre-conflitto di ogni paese trattato.
4. **Placebo Test (In-Space):** Riapplicazione iterativa del modello SCM a tutti i paesi di controllo non trattati. Questo passaggio genera la distribuzione di riferimento per testare la significatività del trattamento calcolando il rapporto MSPE e il p-value (F-test).
5. **Analisi delle Discrepanze:** Confronto critico dei pesi e dei controfattuali ottenuti in ambiente R rispetto a quelli pubblicati dall'autore originale utilizzando Stata.

---

## Risultati Chiave

* **Effetto del Trattamento:** I path plot rivelano deviazioni rilevanti nel *Core Civil Society Index* tra la traiettoria del paese reale e quella del suo alter ego sintetico in concomitanza con lo scoppio delle ostilità.
* **Significatività Statistica:** Gli *In-Space Placebo Test* hanno convalidato la robustezza dei risultati, dimostrando che i gap post-conflitto osservati nei paesi trattati (es. Nepal, Liberia) sono statisticamente insoliti rispetto a quelli dei paesi non colpiti da conflitti.
* **Conferma dei Trend:** Nonostante le differenze nell'assegnazione puntuale dei pesi, i trend generali e la significatività causale evidenziati nel paper originale sono stati ampiamente confermati dalla replica.

---

## Analisi delle Discrepanze (R vs Stata)

Durante la replica dello studio sono emerse alcune differenze rispetto ai risultati originali di O'Reilly (2021). Tali discrepanze non invalidano l'analisi, ma sono riconducibili a quattro fattori metodologici chiave:

* **1. Pacchetto in R e non in Stata:** Il *Synthetic Control Method* si basa sulla risoluzione di un complesso problema di ottimizzazione per minimizzare la distanza pre-trattamento. I pacchetti di Stata ed R utilizzano *solver* matematici e algoritmi di ottimizzazione differenti (es. l'algoritmo BFGS in R). Di conseguenza, partendo dagli stessi identici dati, i due software possono convergere verso "minimi locali" leggermente diversi, producendo differenze nei pesi finali assegnati ai paesi donatori.
* **2. Unità di controllo:** Il controfattuale sintetico è estremamente sensibile ai paesi presenti nel bacino dei donatori (*donor pool*). Una minima variazione nella lista dei paesi ammessi all'analisi altera in modo automatico la distribuzione generale dei pesi, poiché la loro somma deve sempre essere vincolata a 1.
* **3. Gestione NA (Valori mancanti):** I dataset di origine (come V-Dem o Penn World Tables) presentano spesso dati mancanti in anni o variabili specifiche (es. Capitale Umano). Se l'ambiente di lavoro applica regole diverse di *listwise deletion* (esclusione automatica di un paese a fronte di un singolo NA), il gruppo di nazioni effettivamente disponibili per creare la "regione sintetica" cambia radicalmente rispetto a quello a disposizione dell'autore originale.
* **4. Aggiornamenti del pacchetto:** Il paper originale è stato pubblicato nel 2021. I pacchetti open-source ricevono continui aggiornamenti che correggono bug, alterano le logiche di default e modificano le tolleranze di calcolo. Una replica matematica identica al 100% richiederebbe l'utilizzo non solo dello stesso codice, ma anche della medesima e specifica versione dell'ambiente di sviluppo e delle librerie usate all'epoca.

---

## Struttura della Repository

```text
.
├── data/
│   └── vdem_merged_full.dta                  # Dataset completo in formato Stata
├── scripts/
│   ├── SA_SSA.R                              # confronto nazioni di interesse con media regionale e caricamento dataset
│   ├── Burundi.R                             # Replica SCM e Placebo per il Burundi
│   ├── Liberia.R                             # Replica SCM e Placebo per la Liberia
│   ├── Nepal.R                               # Replica SCM e Placebo per il Nepal
│   ├── Sierra_Leone.R                        # Replica SCM e Placebo per la Sierra Leone
│   └── Uganda.R                              # Replica SCM e Placebo per l'Uganda
├── docs/
│   └── Report.pdf           # Slide deck con path plot e considerazioni finali
└── README.md

## Come riprodurre l'analisi

**Requisiti**: R (≥ 4.x) e i seguenti pacchetti:

```r
install.packages(c(
  "Synth", "tidyverse", "haven", "skimr"
))
```

1. Clona la repository:
   ```bash
   git clone https://github.com/MattiaBazzani/Civil-Society-Synthetic-Control.git
   ```
2. **Caricamento Base**: Apri il progetto in RStudio ed esegui per primo lo script scripts/SA_SSA.R per caricare le librerie necessarie e importare il dataset vdem_merged_full.dta.
4. **Esecuzione Modelli Locali**: Apri i singoli file dei paesi (es. scripts/Nepal.R) per eseguire l'estrazione delle matrici, il calcolo dei pesi ottimali e la generazione del path plot.
5. **Analisi In-Space Placebo**: All'interno dei file di analisi è inclusa l'esecuzione in loop della procedura SCM sui paesi di controllo per replicare i test Placebo (Grafici a spaghetti e P-values).

---
