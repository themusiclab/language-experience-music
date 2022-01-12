# Language experience shapes music processing across 40 tonal, pitch-accented, and non-tonal languages 

This repository contains data and code for Liu, Hilton, Bergelson, & Mehr (2021). You can find the preprint at https://www.biorxiv.org/content/10.1101/2021.10.18.464888v1. The pre-registration is available at https://osf.io/xurdb. 

## Anatomy of the repo

- `tonelang.Rmd` contains the manuscript and all analyses. It also contains the code (commented out at the beginning) that generates the post-exclusion data, one-to-one matched data, and inverse probability weighted data from the pre-exclusion data. These generated datasets are read in directly into the manuscript from  `/data` due to the time and memory it takes for them to be generated.
- `tonelang_table1.csv`: Table 1 in the manuscript

`/data` contains all the data: 

- `/Exploratory` contains the exploratory datasets: exploratory pre-exclusion data (`Explore_full.csv`), exploratory post-exclusion data (`Explore_filtered.csv`), exploratory one-to-one matched data (`Explore_matched.csv`), and exploratory inverse-probability weighted data (`ipw_explore.RData`). 

- `/Confirmatory` contains the confirmatory datasets: confirmatory pre-exclusion data (`Confirm_full.csv`), confirmatory post-exclusion data (`Confirm_filtered.csv`), confirmatory one-to-one matched data (`Confirm_matched.csv`), and confirmatory inverse-probability weighted data (`ipw_confirm.RData`)
- Language features and classification (`language.csv`)
- Headphone check scores (`headphone_scores.csv`)

## Asistance

if you have any questions, or need help with any of the data, code, or material associated, please contact Jingxuan Liu (jingxuan.liu@duke.edu), Courtney Hilton (courtneyhilton@g.harvard.edu), and/or Samuel Mehr (sam@wjh.harvard.edu).
