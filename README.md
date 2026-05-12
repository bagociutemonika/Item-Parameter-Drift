# Detecting Item Parameter Drift in Large-Scale Assessments
### A Comparison of IRT-Based and Machine Learning Diagnostic Approaches

This repository contains the R code used to reproduce the simulation study conducted for a master's thesis submitted to Utrecht University.

**Author:** Monika Bagociute  
**Contact:** monikabagociute@gmail.com  
**GitHub:** github.com/bagociutemonika

---

# Project Overview

This study evaluates whether machine learning methods — specifically neural networks and XGBoost — can serve as diagnostic tools for detecting item parameter drift in comparison to the traditional IRT-based Wald test.

Five simulation conditions were generated under progressively complex data-generating models (DGMs), varying:

- drift magnitude,
- drift proportion,
- drift type,
- multidimensionality,
- and multi-country assessment structure.

Across all simulation conditions, the study generated a total of **99 simulated datasets**.

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
│   ├── generate_thesis_figures.R
│   └── export_all_appendix_tables.R
│
└── README.md
```
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

| Simulation | DGM | Multidimensional | Multi-country | Drift types | Datasets |
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

## Step 1 — Data Generation

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

## Step 2 — Data Investigation

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

These scripts are intended for exploratory validation of the simulated datasets before model estimation.

---

## Step 3 — IRT-Based Drift Detection

Run the scripts inside:

```text
03_irt_detection/
```

These scripts:
- fit multigroup IRT models using the `mirt` package,
- estimate item parameters,
- and apply Wald tests for drift detection.

### Calibration Conditions

- Simulations 1.1 and 1.2:
  - Rasch calibration only
  - difficulty drift (b-shift)

- Simulations 2.1–2.3:
  - Rasch and 2PL calibration
  - discrimination, difficulty, and combined drift

---

## Step 4 — Machine Learning Detection

Run the scripts inside:

```text
04_ml_detection/
```

The ML pipeline:
1. obtains EAP ability estimates from IRT calibration,
2. constructs ML feature matrices,
3. trains neural network models or trains XGBoost models,
4. evaluates detection performance.

The ML models use:
- estimated ability,
- and administration indicators.

---

## Step 5 — Results, Tables, and Figures

### Thesis Figures

Run:

```r
source("05_results/generate_thesis_figures.R")
```

This script reproduces:
- sensitivity heatmaps,
- robustness plots,
- specificity plots
---

### Appendix Tables

Run:

```r
source("05_results/export_all_appendix_tables.R")
```

This script exports:
- sensitivity,
- specificity,
- and accuracy tables


---

# Ethics

Ethics approval was granted by the Ethics Review Board of the Faculty of Social and Behavioural Sciences at Utrecht University.

---

# License

This repository is publicly available for academic and educational purposes.

Full responsibility for the content lies with the author.

For questions or collaboration inquiries, contact:
monikabagociute@gmail.com
