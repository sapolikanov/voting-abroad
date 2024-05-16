# Voting Abroad
## Data analysis of exit polls and official results of the 2024 Russian presidential election abroad

Patrick, welcome. This is organized to be as reproducible as possible (unfortunately not fully). Here is a non-technical description of how everything works. 

Raw data lives in the data/data_raw folder. It is untouched and sacred, even though some of it was hand-coded. data/data_built is a folder with built datasets from data_raw. raw_data_prep.R and data_building.R are responsible for that - they live in scripts/data_building. data_built is fully wiped on project render only. Then there are quarto notebooks for different purposes. They are (in order of rendering):

- individual_level/variables_descriptives.qmd, responsible for recoding exit poll questionnaire, broad data missingness diagnostics and some narrative as to what things mean
- individual_level/imputation.qmd, needed to try and (unsuccessfully) fix the data missingness issues. Note that running it and not rendering is computationally intensive. There is also a (light) parallelization set to nCores() - 1
- individual_level/models.qmd - here you can find individual-level (binary, multinomial and nested logit) models and mixed effects (nested logit) models. The latter include extensive convergence diagnostics. Models saved within this script go into scripts/models and are reproducible from code but not when building the project - too computationally intensive.
- country_level/models_country.qmd, responsible for running aggregated-to-country-level linear models and exit poll selection logit. Those rewrite their outputs in scripts/models
- scripts/graphs/graphs.qmd - a bit of a hacky way to avoid having too much code in the main paper doc. Imports data from most previous scripts and modifies it so that only ggplot and kable need to be run in the paper.qmd
- presentation/presentation.qmd - a presentation, fully reproducible and untouched since presented
- paper/paper.qmd - the final paper, a compilation of all of the above.

We use additional scripts to help with reproducibility and easy loading of things. 

- _products is where pdf outputs from scripts, presentation and paper go. 
- _quarto.yml defines order of rendering and creates metadata
- renv/ is... well, renv, I hope everything will work with it on your machine...
- utilities/check_packages.R checks installation statuses of packages in renv and loads them for each script individually
- utilities/functions.R defines our one function (long live stargazer)
- utilities/remove_artifacts.ts wipes data_built and _products and rebuilds them empty on every project render.

The paper is overweight with ~ 4500 words instead of 3000. Cutting down stuff presents itself as an impossible undertacking. Very sorry!

GitHub Co-pilot was used for the majority of the time only by Stepan. It was extremely unhelpful.
