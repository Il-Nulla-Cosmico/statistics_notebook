# TEMPLATE — Statistica per le Scienze della Vita

## Come iniziare una nuova lezione con Claude

Copia e incolla questo messaggio all'inizio di ogni nuova conversazione:

---

> Sto continuando la serie **Statistica per le Scienze della Vita**.
> Devo creare la **Lezione N — [TITOLO ARGOMENTO]**.
>
> Mantieni esattamente il tema grafico della serie:
> - Font: Playfair Display (titoli) + Lora (testo) + JetBrains Mono (codice)
> - Colore principale: verde `#2d6a4f`
> - Sfondo pagina: `#f8f7f3`
> - R box: sfondo grigio `#f0f0ec`, bordo sinistro verde `#2d6a4f`
> - Header: verde pieno con testo bianco
> - Nav: sticky in alto, bianca con ombra leggera
> - Formule: box con bordo verde e label mono in alto
> - Tabelle: header verde pieno, righe alternate `#f6faf7`
> - Callout: tre varianti — verde (nota), arancio/ruggine (attenzione), blu (info)
>
> Il CSS è in un file separato `style.css` nella root del repository.
> Ogni lezione vive in una propria cartella: `lezione-0N-titolo/index.html`
> che importa il CSS con: `<link rel="stylesheet" href="../style.css">`
>
> Il pubblico è composto da biologi, entomologi e agronomi.
> Struttura della lezione: teoria → formule → proprietà → esempio R con dataset reale → interpretazione → uso pratico.

---

## Struttura del repository

```
/
├── index.html                     ← homepage della serie (opzionale)
├── style.css                      ← CSS condiviso ⚠️ non modificare
├── TEMPLATE.md                    ← questo file
│
├── lezione-01-sd-se/
│   ├── index.html
│   └── img/
│       ├── grafico1_distribuzione_sd.png
│       ├── grafico2_sd_vs_se.png
│       ├── grafico3_bootstrap_se.png
│       └── grafico4_confronto_specie.png
│
├── lezione-02-[titolo]/
│   ├── index.html
│   └── img/
│
└── R/
    ├── 01_calcolo_sd_se.R
    ├── 02_grafici_sd_se.R
    └── ...script delle lezioni successive

```

## Token di design (per riferimento rapido)

| Token | Valore |
|-------|--------|
| Verde principale | `#2d6a4f` |
| Verde medio | `#40916c` |
| Verde chiaro | `#52b788` |
| Verde pallido (sfondi) | `#d8f3dc` |
| Ruggine accento | `#b5451b` |
| Ruggine pallido | `#fde8e0` |
| Sfondo pagina | `#f8f7f3` |
| R box sfondo | `#f0f0ec` |
| R box header | `#e4e4df` |
| R output (dark) | `#1e2820` |
| Footer | `#1a2520` |

## Argomenti previsti per la serie

- [x] Lezione 01 — Deviazione Standard vs Errore Standard
- [ ] Lezione 02 — Distribuzione t di Student e test t
- [ ] Lezione 03 — ANOVA a una via
- [ ] Lezione 04 — Confronti multipli e test post-hoc
- [ ] Lezione 05 — Correlazione e regressione lineare semplice
- [ ] Lezione 06 — Potenza statistica e dimensione campionaria
- [ ] ...

## Note

- Gli script R vanno nella cartella `R/` nella root, non dentro le cartelle lezione
- Le immagini dei grafici vanno in `lezione-0N/img/`
- Aggiornare sempre questa lista con i nuovi argomenti
