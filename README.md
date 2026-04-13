# Farmers' experience and perception shape adoption and use of precision laser land levelling technology in India

Replication materials for Surendran-Padmaja, S. & Parlasca, M. C. *Q Open* (forthcoming).

## Overview

This repository contains the R code and data required to reproduce the quantitative results in the paper: Tables 1–2, Figures 2–5, and Supplementary Figure 3 and Tables 3–5. The analysis uses a multi-arm causal forest (`grf` package) to estimate the conditional average treatment effect (CATE) of laser land levelling (LLL) at different frequencies of use on rice and wheat yield and irrigation hours, alongside OLS benchmarks.

## Repository structure

```
.
├── README.md                  <- this file
├── LICENSE                    <- MIT license
├── .gitignore                 <- files excluded from git
├── code/
│   └── Replication_qopen.R    <- main replication script
├── data/
│   └── README.md              <- data access instructions
└── output/
    ├── figures/               <- figures produced by the script
    └── tables/                <- tables produced by the script
```

## Data

The primary dataset is `LLL Punjab plot data_revised2_paper1_adoption3.dta`, a Stata file containing plot-level survey data from 1,021 farm households in four districts of Punjab, India (Ludhiana, Fatehgarh Sahib, Sangrur, Patiala), collected June–August 2021. See `data/README.md` for access details.

## Software

- R ≥ 4.2.0
- Required packages (installed automatically by the script if missing): `haven`, `tidyverse`, `ggplot2`, `ggpubr`, `tidyr`, `dplyr`, `reshape2`, `fBasics`, `fastDummies`, `stargazer`, `modelsummary`, `grf`, `ggridges`, `scales`.

## How to reproduce

1. Clone the repository.
2. Place the data file in the `data/` folder.
3. Open `code/Replication_qopen.R`, set the working directory at the top to the root of this repository, and run end-to-end.

Expected run time: 5–15 minutes on a laptop, depending on hardware. The causal forests use `seed = 2` so results are reproducible.

## What the script produces

| Script section | Manuscript output |
|---|---|
| §2 | Table 1, Supplementary Table 3 |
| §3 | Figure 2 |
| §4 | Figure 3 |
| §5 | Figure 4 |
| §6 | Table 2 (OLS columns) |
| §7 | Table 2 (CATE columns) |
| §8 | Supplementary Figure 3 |
| §9 | Figure 5 |
| §10 | Supplementary Table 4 |

## Citation

If you use this code, please cite:

> Surendran-Padmaja, S. and Parlasca, M. C. (2025). Farmers' experience and perception shape adoption and use of precision laser land levelling technology in India. *Q Open*.

## Contact

Subash Surendran-Padmaja — s17ssure@uni-bonn.de

## License

Code is released under the MIT License (see `LICENSE`). The survey data is subject to separate terms; see `data/README.md`.
