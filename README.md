# Language experience predicts music processing in a half-million speakers of fifty-four languages

This is the repository for Liu, Hilton, Bergelson, & Mehr (2023, Current Biology). The paper is available at https://www.cell.com/current-biology/fulltext/S0960-9822(23)00387-1 and the preprint is at https://www.biorxiv.org/content/10.1101/2021.10.18.464888v2.

This repo contains:
- an R Markdown file that generates the manuscript
- analysis, and visualization code to produce the results reported in the manuscript
- a script that automatically downloads the data required to run the analyses and visualizations

Further data and information are available elsewhere: 
- the data required for the analyses and visualizations is at https://zenodo.org/record/7614189#.Y-LAp-xBz0p
- the pre-registration is available at https://osf.io/xurdb.
- you can participate in the music perception test at <https://themusiclab.org/quizzes/miq>.

**For assistance, please contact the corresponding authors: Jingxuan Liu (jl6297@gsb.columbia.edu), Courtney Hilton (courtney.hilton@auckland.ac.nz), and Samuel Mehr (sam@yale.edu).**

## Anatomy of the repo

Upon first downloading this repository, you should run the `data_downloader.R` script in the home directory. This will download all the required data (from https://zenodo.org/record/7614189#.Y-LAp-xBz0p) and move it to the correct locations.

After you have done this, to render the paper, run the code in `/writing/manuscript.Rmd`.

> **Warning**  
> The manuscript file combines output from several `.Rmd` files devoted to analysis, visualization, and the like. The `full_run` flag in `manuscript.Rmd` determines whether analyses and figures should be generated from scratch (which can take > 30 minutes), or not. By default, it is set to `FALSE`, to save knitting time. If you set it to `TRUE`, all preprocessing, analysis, and visualization code will be run.

### Data and analysis code

`/data` contains all the data (the `data_downloader.R` needs to be run to fully populate): 

- `/Exploratory` contains the exploratory datasets: exploratory pre-exclusion data (`Explore_full.csv`), exploratory post-exclusion data (`Explore_filtered.csv`), exploratory one-to-one matched data (`Explore_matched.csv`), and exploratory inverse-probability weighted data (`ipw_explore.RData`). 

- `/Confirmatory` contains the confirmatory datasets: confirmatory pre-exclusion data (`Confirm_full.csv`), confirmatory post-exclusion data (`Confirm_filtered.csv`), confirmatory one-to-one matched data (`Confirm_matched.csv`), and confirmatory inverse-probability weighted data (`ipw_confirm.RData`)
- `/Combined` contains the combined dataset (`Combined_filtered.csv`), needed for the main analyes.
- Language features and classification (`language.csv`)
- Headphone check scores (`headphone_scores.csv`)
- `/meta-analysis` contains the data used in the meta-analysis.

`/results` contains all the pre-saved results from previous runs of the analysis scripts (the `data_downloader.R` needs to be run to fully populate): 
- `analyses.RData` contains the results from the `analysis/analysis.R` script.
- `meta-analyses.RData` contains the results from the `analysis/meta-analysis.Rmd` script.
- `permutation_tests.RData` contains the results from the permuted Discriminant Function Analysis, also in the `analysis/analysis.R` script.


### Visualizations

Visualization code is in `/viz`, along with images and static data used for non-dynamic visualizations. The `/viz/figures` subdirectory contains static images produced by `figures.Rmd`, which can be regenerated with a `full_run` of `manuscript.Rmd`.

[![DOI](https://zenodo.org/badge/394695004.svg)](https://zenodo.org/doi/10.5281/zenodo.11218599)
