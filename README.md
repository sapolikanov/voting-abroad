# Voting Abroad

This project was prepared in collaboration with my (amazing) wife Vera (also a student).

## Data analysis of exit polls and official results of the 2024 Russian presidential election abroad

This project is organized to be reproducible, following [reproducible research workflow from Aaron Gullickson](https://github.com/AaronGullickson/research-template). 

Some of the computations in this project are computationally expensive. The total runtime when rendering the project from scratch is around 2.5 hours on my machine (16 GB RAM, AMD Ryzen 7 5800H). There are two ways in which this is tackled to aid reproducibility experience. The first one is caching, which is turned on a per-chunk basis, allowing for most models, printing of large text outputs and other expensive operations to be cached on first render and then loaded from cache. Cache is not checked into version control and the releases. This means that when downloading a release version of the project and running  `quarto render` or (in RStudio) going `Build -> Render Project` for the first time, the project will take a long time and may exceed your machine's in-memory storage (RAM) or processing capabilities.  

To make the project friendly to those who can't reproduce expensive computations, I write results of models into a separate folder, `data/models_norender/`. If a user wishes to use these pre-loaded versions in place of the preferred setup, they will need to manually edit scripts to load these files in place of computations. This would typically involve manipulating `eval: ` flags in chunk options.

Quarto's `freeze` capabilities are not implemented, as they disallow reproducible setup used here. The `remove_artifacts.ts` script removes the folder with the built data (`data_built/`) on project render. `freeze` can only be enabled at project level and it prevents the built data from being written again. 
 
Raw data comes from data/data_raw folder. data/data_built is a folder with built datasets with `raw_data_prep.qmd` and `data_building.qmd` from `data_raw`. Quarto notebooks take care of data building, processing, analysis and modelling. They are (in order of rendering):

- `scripts/raw_data_prep.qmd` prepares individual-level exit poll data. dubbed raw by exit poll organization.
- `scripts/data_building` cleans, combines and aggregates data from all sources and on all levels. Dataset generated here is generally used for all subsequent analyses.
- `scripts/variables_descriptives.qmd`, responsible for recoding exit poll questionnaire, broad data missingness diagnostics and some narrative as to what things mean.
- `scripts/imputation.qmd`, needed to try and address the data missingness issues. Note that running it and not rendering is computationally intensive. There is also a (light) parallelization set to nCores() - 1.
- `scripts/simple_models.qmd` - here you can find individual-level (binary, multinomial and nested logit) models.
- `scripts/mixed_effects_models.qmd` present mixed effects (nested logit) models, including extensive convergence diagnostics.
- `scripts/models_country.qmd`, responsible for running aggregated-to-country-level linear models and exit poll selection logit. Those rewrite their outputs in scripts/models.
- `presentation/presentation.qmd` - a presentation, fully reproducible and untouched since presented before the project was finished.
- `paper/paper.qmd` - the final paper, a compilation of all of the above.
