
## Updated risk distribution model for *Plasmodium knowlesi* infection across Southeast Asia

This repository contains the code necessary to fit, predict and evaluate the *Plasmodium knowlesi* risk model as published in:

[link]()

The codebase is built upon work by Freya Shearer as published in [Estimating Geographical Variation in the Risk of Zoonotic Plasmodium knowlesi Infection in Countries Eliminating Malaria](https://journals.plos.org/plosntds/article?id=10.1371/journal.pntd.0004915), and relies heavily upon the packages `seegSDM`, `dismo` and `gbm`.

### Data Requirements

 - Parasite infection/species occurrence data (not necessarily *Plasmodium knowlesi*) with longitude/latitude
 - Some number of environmental covariate rasters

### Data Processing

A number of data preprocessing steps must be performed before the model can be fit. This consists of:

- Collating *Pk* occurrence data into a common database and filtering under some requirements (see `code/R/preprocessing`)

- Producing a number of raster covariates from disparate sources under a common grid projection (see `code/R/covariate_formation`)

### Model fitting and prediction

The process of fitting the boosted regression tree model has been broken down into three stages to allow for the running of the model on a cluster system (SPARTAN). These are:

- Splitting the data into *m* sets of *n* bootstraps each (`split_bootstraps.R`, `split_bootstraps.slurm`)
- Fitting the model *mn* times (`run_model.R`, `model_run.slurm`)
- Producing predictions for the model *mn* times (`produce_predictions.R`, `produce_predictions.slurm`)

### Analysis

The directory `code/R/figures` contains a number of R scripts to produce plots, tables and maps.



## Packages

Ahlmann-Eltze, Constantin. 2021. *sparseMatrixStats: Summary Statistics
for Rows and Columns of Sparse Matrices*.

Bates, Douglas, and Martin Maechler. 2021. *Matrix: Sparse and Dense
Matrix Classes and Methods*. <http://Matrix.R-forge.R-project.org/>.

Bivand, Roger S., Edzer Pebesma, and Virgilio Gomez-Rubio. 2013.
*Applied Spatial Data Analysis with R, Second Edition*. Springer, NY.
<https://asdar-book.org/>.

Bivand, Roger, Tim Keitt, and Barry Rowlingson. 2021. *Rgdal: Bindings
for the Geospatial Data Abstraction Library*.
<https://CRAN.R-project.org/package=rgdal>.

Bivand, Roger, and Colin Rundel. 2020. *Rgeos: Interface to Geometry
Engine - Open Source (GEOS)*.
https://r-forge.r-project.org/projects/rgeos/

Freya Shearer, Nick Golding &. 2015. *seegSDM: Streamlined Functions for
Species Distribution Modelling in the SEEG Research Group*.

Garnier, Simon. 2018. *Viridis: Default Color Maps from Matplotlib*.
<https://github.com/sjmgarnier/viridis>.

Hickey, James, Paul Metcalfe, Greg Ridgeway, Stefan Schroedl, Harry
Southworth, and Terry Therneau. 2016. *Gbm3: Generalized Boosted
Regression Models*. <https://github.com/gbm-developers/gbm3>.

Hijmans, Robert J. 2020. *Raster: Geographic Data Analysis and
Modeling*. <https://rspatial.org/raster>.

Hijmans, Robert J., Steven Phillips, John Leathwick, and Jane Elith.
2020. *Dismo: Species Distribution Modeling*.
<https://rspatial.org/raster/sdm/>.

Pebesma, Edzer. 2018. “<span class="nocase">Simple Features for R:
Standardized Support for Spatial Vector Data</span>.” *The R Journal* 10
(1): 439–46. <https://doi.org/10.32614/RJ-2018-009>.

———. 2021. *Sf: Simple Features for r*.
<https://CRAN.R-project.org/package=sf>.

Pebesma, Edzer J., and Roger S. Bivand. 2005. “Classes and Methods for
Spatial Data in R.” *R News* 5 (2): 9–13.
<https://CRAN.R-project.org/doc/Rnews/>.

Pebesma, Edzer, and Roger Bivand. 2021. *Sp: Classes and Methods for
Spatial Data*. <https://CRAN.R-project.org/package=sp>.

Wickham, Hadley. 2016. *Ggplot2: Elegant Graphics for Data Analysis*.
Springer-Verlag New York. <https://ggplot2.tidyverse.org>.

———. 2019. *Tidyverse: Easily Install and Load the Tidyverse*.
<https://CRAN.R-project.org/package=tidyverse>.

Wickham, Hadley, Mara Averick, Jennifer Bryan, Winston Chang, Lucy
D’Agostino McGowan, Romain François, Garrett Grolemund, et al. 2019.
“Welcome to the <span class="nocase">tidyverse</span>.” *Journal of Open
Source Software* 4 (43): 1686. <https://doi.org/10.21105/joss.01686>.

Wickham, Hadley, Winston Chang, Lionel Henry, Thomas Lin Pedersen,
Kohske Takahashi, Claus Wilke, Kara Woo, Hiroaki Yutani, and Dewey
Dunnington. 2020. *Ggplot2: Create Elegant Data Visualisations Using the
Grammar of Graphics*. <https://CRAN.R-project.org/package=ggplot2>.

Wilke, Claus O. 2020. *Cowplot: Streamlined Plot Theme and Plot
Annotations for Ggplot2*. <https://wilkelab.org/cowplot/>.
