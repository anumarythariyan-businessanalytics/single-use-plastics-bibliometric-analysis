# Bibliometric Analysis of Single-Use Plastics (2019–2025)

This repository contains the data, R code, and outputs used to reproduce the
bibliometric analyses conducted for an MSc Business Analytics dissertation.



# Project Overview
The study applies bibliometric and science-mapping techniques to analyse
research trends, collaboration patterns, and thematic evolution in the
single-use plastics literature published between 2019 and 2025.



# Repository Structure
- `Code/` – R scripts for data cleaning, merging, and bibliometric analysis
- `data/raw/` – Raw data files and documentation
- `data/processed/` – Intermediate datasets generated during analysis
- `outputs/` – Figures reproduced in the dissertation



# Data Sources
- Web of Science Core Collection (BibTeX exports)
- Rayyan systematic screening output (CSV)

Due to licensing restrictions, raw Web of Science data cannot be publicly
redistributed. Instructions for data retrieval are provided in
`data/raw/README.md`.



## Reproducibility
All analyses can be reproduced by running the script:

`Code/bibliometric_analysis_single_use_plastics.R`

The script performs:
- Data cleaning and merging
- Bibliometric indicator analysis
- Country collaboration network analysis
- Keyword co-occurrence analysis
- Thematic mapping
- Keyword evolution analysis



# Software Requirements
- R (version 4.0 or higher version)
- R packages:
  - bibliometrix
  - readr
  - dplyr
  - stringr
  - tidyr
  - ggplot2
