# Detecting Item Parameter Drift in Large-Scale Assessments
### A Comparison of IRT-Based and Machine Learning Diagnostic Approaches

This repository contains the R code used to reproduce the simulation study conducted for a master's thesis submitted to Utrecht University.

**Author:** Monika Bagociute  
**Contact:** monikabagociute@gmail.com  
**GitHub:** github.com/bagociutemonika

---

# Project Overview

This study investigates whether machine learning methods — specifically neural networks and XGBoost — can serve as effective diagnostic tools for detecting item parameter drift (IPD) in comparison to the traditional IRT-based Wald test.

Five simulation conditions were generated under progressively more complex data-generating models (DGMs), varying:

- drift magnitude,
- drift proportion,
- drift type,
- multidimensionality,
- and multi-country assessment structure.

The study compares the sensitivity, specificity, and overall robustness of machine learning approaches against classical IRT-based detection methods.

---

# Simulation Design

## Assessment Design

- **75 items**
  - 15 anchor items (`I1–I15`)
  - 60 non-anchor items (`I16–I75`)
- **4 booklets**
  - each booklet contains all anchor items plus 15 unique non-anchor items
- **Assessment cycles:** 2015 and 2018
- **Countries:** Japan, Spain, and Turkey (Simulation 2.3 only)
- **Sample size:** 2,000 respondents per country per cycle

---

## Simulation Conditions

| Simulation | DGM | Multidimensional | Multi-country | Drift Types | Datasets |
|---|---|---|---|---|---|
| 1.1 | Rasch | No | No | b-shift | 9 |
| 1.2 | Rasch | Yes (ρ = 0.8) | No | b-shift | 9 |
| 2.1 | 2PL | No | No | a-, b-, ab-shift | 27 |
| 2.2 | 2PL | Yes (ρ = 0.8) | No | a-, b-, ab-shift | 27 |
| 2.3 | 2PL | Yes (ρ = 0.8) | Yes | a-, b-, ab-shift | 27 |

---

## Drift Conditions

### Drift Magnitudes
- 0.2 logits
- 0.5 logits
- 1.0 logits

### Drift Proportions
- 10%
- 20%
- 30% of anchor items

### Total Datasets
- **99 simulated datasets**

---

# Repository Structure

```text
├── 01_data_generation/
│   ├── sim11/
│   ├── sim12/
│   ├── sim21/
│   ├── sim22/
│   └── sim23/
│
├── 02_data_investigation/
│   ├── investigate_dataset.R
│   └── run_all_investigations.R
│
├── 03_irt_detection/
│   ├── sim11/
│   ├── sim12/
│   ├── sim21/
│   ├── sim22/
│   └── sim23/
│
├── 04_ml_detection/
│   ├── sim11/
│   ├── sim12/
│   ├── sim21/
│   ├── sim22/
│   └── sim23/
│
├── 05_results/
│   ├── sim11/
│   ├── sim12/
│   ├── sim21/
│   ├── sim22/
│   ├── sim23/
│   ├── heatmaps.R
│   ├── sensitivity_figure.R
│   ├── boundary_table.R
│   └── export_all_latex_tables.R
│
├── Outputs/
│   ├── figures/
│   ├── latex_tables/
│   └── boundary_table.tex
│
└── README.md
```

---

# Reproducing the Study

## Requirements

### Software
- R version 4.5.2 or newer

### Required Packages

```r
install.packages(c(
  "mirt",
  "tidyverse",
  "MASS",
  "keras",
  "xgboost",
  "caret",
  "ggplot2",
  "patchwork",
  "viridis",
  "xtable",
  "kableExtra"
))
```

---

# Workflow

```text
01_data_generation
        ↓
02_data_investigation
        ↓
03_irt_detection
        ↓
04_ml_detection
        ↓
05_results
        ↓
Outputs
```

Where:

- `01_data_generation` generates the simulated datasets,
- `02_data_investigation` validates and explores the generated data,
- `03_irt_detection` applies IRT-based drift detection methods,
- `04_ml_detection` applies machine learning detection methods,
- `05_results` aggregates and visualizes findings,
- `Outputs` stores the final thesis-ready figures and tables.

---

# Step 1 — Data Generation

Run:

```r
source("01_data_generation/generate_datasets.R")
```

This script:

- sources all functions in `01_data_generation/functions/`,
- generates all simulated datasets,
- stores datasets as list objects,
- and saves them for later analyses.

---

# Step 2 — Data Investigation

Run:

```r
source("02_data_investigation/run_all_investigations.R")
```

This step produces:

- booklet design diagnostics,
- classical test theory summaries,
- item p-value plots,
- interaction plots,
- and drift-condition inspections.

These scripts are intended for exploratory validation of the simulated datasets prior to model estimation.

---

# Step 3 — IRT-Based Drift Detection

Run the scripts inside:

```text
03_irt_detection/
```

These scripts:

- fit multigroup IRT models using the `mirt` package,
- estimate item parameters,
- and apply Wald tests for drift detection.

## Calibration Conditions

### Simulations 1.1 and 1.2
- Rasch calibration only
- difficulty drift (`b-shift`)

### Simulations 2.1–2.3
- Rasch and 2PL calibration
- discrimination (`a-shift`),
  difficulty (`b-shift`),
  and combined (`ab-shift`) drift

---

# Step 4 — Machine Learning Detection

Run the scripts inside:

```text
04_ml_detection/
```

The machine learning pipeline:

1. obtains EAP ability estimates from IRT calibration,
2. constructs ML feature matrices,
3. trains neural network or XGBoost models,
4. evaluates detection performance.

The ML models use:

- estimated ability values,
- and administration information.

---

# Step 5 — Results, Tables, and Figures

This step generates the final thesis figures and appendix tables from the simulation and detection outputs.

## Thesis Figures

Run:

```r
source("05_results/heatmaps.R")
source("05_results/sensitivity_figure.R")
```

These scripts reproduce:

- sensitivity heatmaps,
- robustness figures,
- specificity plots,
- and other thesis visualizations.

Generated figures are saved in:

```text
Outputs/figures/
```

---

## Appendix Tables

Run:

```r
source("05_results/boundary_table.R")
source("05_results/export_all_latex_tables.R")
```

These scripts export:

- sensitivity tables,
- specificity tables,
- accuracy tables,
- and boundary-condition summaries.

Generated tables are saved in:

```text
Outputs/latex_tables/
```

---

# Outputs

The `Outputs/` directory contains all final materials used in the thesis manuscript.

```text
Outputs/
│
├── figures/
│   ├── heatmaps
│   ├── sensitivity plots
│   └── robustness figures
│
├── latex_tables/
│   ├── sensitivity tables
│   ├── specificity tables
│   └── accuracy tables
│
└── boundary_table.tex
```

All files in this directory are automatically generated by the scripts in `05_results/`.

---

# Ethics

Ethics approval was granted by the Ethics Review Board of the Faculty of Social and Behavioural Sciences at Utrecht University (25-1995).

---

# License

This repository is publicly available for academic and educational purposes.

Full responsibility for the content lies with the author.

For questions or collaboration inquiries, contact:

monikabagociute@gmail.com
