# Violent Conflict and the Strength of Civil Society - Replication Study

Questo progetto contiene il codice R e il report per la replica e l'estensione delle analisi presentate nel paper originale di O'Reilly (2021). Il focus dello studio è analizzare l'impatto dei conflitti violenti (guerre civili) sulla forza della società civile a livello nazionale in cinque paesi: Sierra Leone, Burundi, Uganda, Nepal e Liberia.

## Paper Originale
O'Reilly, C. (2021). *Violent Conflict and the Strength of Civil Society*. [10.2139/ssrn.3925357]

**Nota:** Il PDF del paper originale non è incluso in questa repository per motivi di copyright. Si rimanda al link sovrastante per la consultazione.

## Autori della Replica
* Mattia Bazzani
* Alberto Borgese
* Andrea Gaspari

*(Università degli Studi di Padova)*

## Descrizione del Progetto
L'analisi utilizza il **Synthetic Control Method (SCM)** per creare controlli sintetici per i paesi trattati (colpiti da guerra civile) e confrontare l'andamento del *Core Civil Society Index* (V-Dem dataset) pre e post conflitto. Il progetto valuta anche la robustezza dei risultati tramite *Placebo Test* spaziali (In-Space Placebo).

## Struttura della Repository
* `code.R`: Script principale per l'importazione dei dati (`vdem_merged_full.dta`), la manipolazione delle variabili e la generazione delle statistiche descrittive generali.
* `Burundi.R`, `Liberia.R`, `Nepal.R`, `Sierra_Leone.R`, `Uganda.R`: Script specifici per paese che eseguono il Synthetic Control Method e i test Placebo (RMSPE, P-value, F-test).
* `Bazzani_Borgese_gaspari.pdf`: Presentazione contenente il report finale, la sintesi dei risultati e l'analisi delle discrepanze rispetto al paper originale.

## Dati Utilizzati
* **Armed Conflict Dataset:** (Uppsala Conflict Data Program) per l'inizio dei conflitti.
* **V-Dem dataset:** per il *Core Civil Society Index* e *Electoral Democracy*.
* **Penn World Tables:** per Log GDP, Log Population e Human Capital.
* **LaPorta et al. 2008:** per Legal Origin.

## Requisiti
Il codice è scritto in R. I pacchetti necessari per eseguire le analisi sono:
`haven`, `tidyverse`, `skimr`, `Synth`

## Risultati e Discrepanze
I risultati confermano parzialmente le dinamiche descritte nel paper originale, tuttavia sono emerse delle discrepanze nei pesi dei paesi di controllo e nei risultati dei test Placebo. Come documentato nel report, queste differenze sono attribuibili all'uso del pacchetto `Synth` in R anziché in Stata, alle differenze negli algoritmi di ottimizzazione, ai criteri di selezione delle unità di controllo e alla gestione dei valori mancanti (NA).
