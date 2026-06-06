# Teoria del Ferro di Cavallo e Estremismo Politico

Questo repository contiene un’analisi statistica della teoria del ferro di cavallo utilizzando dati dell’European Social Survey, Round 10.

Il progetto confronta gli individui collocati agli estremi dello spettro politico con individui più moderati, con l’obiettivo di verificare se esistano differenze rilevanti in termini di caratteristiche socio-economiche e atteggiamenti politici.

## Contenuto della repository

- `code/`: codice R utilizzato per l’analisi
- `data/`: dataset ESS10
- `report/`: report finale in formato PDF

## Dati

Il dataset utilizzato proviene dall’European Social Survey, Round 10 (ESS10).

Il file presente nella repository è:

- `ESS10e03_3.csv`

Il file dati utilizzato nello studio originale era diverso da quello attualmente presente nella repository. Tuttavia, trattandosi dello stesso round ESS10, i dati non dovrebbero differire in modo significativo.

Eventuali differenze minori nei risultati potrebbero dipendere dall’edizione del dataset, dal formato del file, dalla gestione dei valori mancanti o da aggiornamenti successivi dei dati.

Per rieseguire l’analisi, assicurarsi che il file sia presente nella cartella `data/` e aggiornare, se necessario, il percorso del dataset nel codice R.

## Metodi utilizzati

L’analisi è svolta in R e include:

- selezione di Italia, Spagna, Portogallo e Grecia;
- costruzione della variabile di estremismo politico tramite `lrscale`;
- standardizzazione delle variabili;
- analisi fattoriale;
- costruzione di indici compositi;
- weighted matching;
- test statistici pesati.

Le aree analizzate sono:

- fiducia;
- soddisfazione generale;
- istruzione;
- atteggiamenti verso il Covid-19;
- percezione della democrazia.

## Pacchetti R

I principali pacchetti utilizzati sono:

- `haven`
- `GPArotation`
- `MatchIt`
- `cobalt`
- `psych`
- `weights`

## Risultati principali

L’analisi non mostra evidenze forti di grandi differenze tra estremisti politici e moderati nelle variabili considerate.

Alcune differenze emergono in aree specifiche, ma nel complesso i risultati non sembrano supportare in modo netto la teoria del ferro di cavallo.

## Autore

Luca Alberti
