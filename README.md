# Voting Abroad
## Data analysis of exit polls and official results of the 2024 Russian presidential election abroad

This project is organized to be reproducible, following [reproducible research workflow from Aaron Gullickson](https://github.com/AaronGullickson/research-template). Below is a non-technical breakdown of the repository. 

Raw data comes from data/data_raw folder. data/data_built is a folder with built datasets from data_raw. raw_data_prep.R and data_building.R are responsible for that - they live in scripts/data_building. data_built is fully wiped on project render only. Then there are quarto notebooks for different purposes. They are (in order of rendering):

- individual_level/variables_descriptives.qmd, responsible for recoding exit poll questionnaire, broad data missingness diagnostics and some narrative as to what things mean
- individual_level/imputation.qmd, needed to try and address the data missingness issues. Note that running it and not rendering is computationally intensive. There is also a (light) parallelization set to nCores() - 1
- individual_level/models.qmd - here you can find individual-level (binary, multinomial and nested logit) models and mixed effects (nested logit) models. The latter include extensive convergence diagnostics. Models saved within this script go into scripts/models and are reproducible from code but not when building the project - too computationally intensive.
- country_level/models_country.qmd, responsible for running aggregated-to-country-level linear models and exit poll selection logit. Those rewrite their outputs in scripts/models.
- scripts/graphs/graphs.qmd - a bit of a hacky way to avoid having too much code in the main paper doc. Imports data from most previous scripts and modifies it so that only ggplot and kable need to be run in the paper.qmd.
- presentation/presentation.qmd - a presentation, fully reproducible and untouched since presented before the project was finished.
- paper/paper.qmd - the final paper, a compilation of all of the above.
