# Language experience predicts music processing in 1/2 million speakers of 54 languages

This is the repository for Liu, Hilton, Bergelson, & Mehr (2023). You can find the preprint at https://www.biorxiv.org/content/10.1101/2021.10.18.464888v1.

You can find the following here:
- an R Markdown file that generates the manuscript
- analysis, and visualization code to produce the results reported in the manuscript
- a script that automatically downloads the data required to run the analyses and visualizations

Further data and information are available elsewhere: 
- the data required for the analyses and visualizations is at https://zenodo.org/record/7614189#.Y-LAp-xBz0p
- the pre-registration is available at https://osf.io/xurdb.
- you can participate in the naïve listener experiment at <https://themusiclab.org/quizzes/ids>.

**For assistance, please contact the corresponding authors: Jingxuan Liu (jl6297@gsb.columbia.edu), Courtney Hilton (courtney.hilton@auckland.ac.nz), and Samuel Mehr (sam@yale.edu).**

## Anatomy of the repo

Upon first downloading this repository, you should run the `data_downloader.R` script in the home directory. This will download all the required data (from https://zenodo.org/record/7614189#.Y-LAp-xBz0p) and move it to the correct locations.

After you have done this, to render the paper, run the code in `/writing/manuscript.Rmd`.
\

`/data` contains all the data: 

- `/Exploratory` contains the exploratory datasets: exploratory pre-exclusion data (`Explore_full.csv`), exploratory post-exclusion data (`Explore_filtered.csv`), exploratory one-to-one matched data (`Explore_matched.csv`), and exploratory inverse-probability weighted data (`ipw_explore.RData`). 

- `/Confirmatory` contains the confirmatory datasets: confirmatory pre-exclusion data (`Confirm_full.csv`), confirmatory post-exclusion data (`Confirm_filtered.csv`), confirmatory one-to-one matched data (`Confirm_matched.csv`), and confirmatory inverse-probability weighted data (`ipw_confirm.RData`)
- Language features and classification (`language.csv`)
- Headphone check scores (`headphone_scores.csv`)

## Asistance

if you have any questions, or need help with any of the data, code, or material associated, please contact Jingxuan Liu (jingxuan.liu@duke.edu), Courtney Hilton (courtneyhilton@g.harvard.edu), and/or Samuel Mehr (sam@wjh.harvard.edu).
