MedievalAvignon
=======
*Reconstruction of the map of Avignon during medieval times*

* Copyright 2020-2024 Vincent Labatut & Margot Ferrand

MedievalAvignon is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation. For source availability and license information see `licence.txt`

* Lab site: http://lia.univ-avignon.fr/
* GitHub repo: https://github.com/CompNet/MedievalAvignon
* Data: https://doi.org/10.5281/zenodo.12804379
* Contact: Vincent Labatut <vincent.labatut@univ-avignon.fr>, Margot Ferrand <margot.ferrand@alumni.univ-avignon.fr>

-----------------------------------------------------------------------

If you use this source code or the associated dataset, please cite reference [[FL'24](#references)].

![MedievalAvignonNet](/out/graph_both.jpg)

# Description
This set of R scripts aims at extracting and analyzing confront networks extracted from raw historical tables. It does the following:
1. Extracts various networks based on some tabular data built based on historical sources.
2. Computes a number of statistics and generates the corresponding plots.
3. Performs additional analysis of the networks.


# Data
The raw dataset was manually constituted by Margot Ferrand during her PhD. The detail of her historical sources is given in her PhD [[F'22](#references)]. The output files (graphs, plots, tables...) can be obtained by running the scripts, but they are also directly available on [Zenodo](https://doi.org/10.5281/zenodo.12804379).


# Organization
Here are the folders composing the project:
* Folder `in`: input data, including some actually used (subfolder `analysis`) and some currently not used (subfolder `positioning`).
* Folder `log`: logs produced when running the scripts.
* Folder `out`: contains the files produced by the `R` scripts. Subfolder `social` is not currently used (only subfolder `estate` is). Each subfolder contains the networks, stats, and plots for a specific extraction scheme. Its name indicates which parameters are used for the extraction:
  * `split` vs. `whole`: linear/surface vertices are split to match spatial spread vs. they are kept as extracted from historical sources.
  * `ext vs. raw`: additional edges complementing the raw historical sources (e.g. between streets) vs. no additional edge (only those explicitly appearing in the historical sources).
  * `full`: all types of vertices and edges are kept.
  * `estate`: keep only the vertices defined at spatial the level of the building (i.e. no long streets or other linear entities, surface entities such as villages).
  * `flat_relations`: keep only the edges representing flat relations (by opposition to hierarchical relations, such as "belonging to"). Also get read of long distance relationships (even if flat). 
  * `flat_minus`: same as `flat_relations`, but certain objects are also removed (walls, rivers), as well as the longest streets. The latter are indicated by a numeric value `x` corresponding to the top `x` longest streets (with `whole`) or to a minimal length threshold (with `split`).
  * `filtered`: remove isolated vertices and small components.
* Folder `res`: contains the `R` source code.
* Folders `lib` and `src`: contains the `Java` libraries and source code, currently not used.


# Installation
You first need to install the `R` language, as well as the required packages:

1. Install the [`R` language](https://www.r-project.org/)
2. Download this project from GitHub and unzip.
3. Install the required packages: 
   1. Open the `R` console.
   2. Set the unzipped directory as the working directory, using `setwd("<my directory>")`.
   3. Run the install script `res/_install.R` (that may take a while).


# Use
In order to extract the networks from the raw data, compute the statistics, and generate the plots:

1. Open the `R` console.
2. Set the current directory as the working directory, using `setwd("<my directory>")`.
3. Run the main script `res/main.R`.

The scripts will produce a number of files in folder `out/analysis/estate`. They are grouped in subsubfolders, each one corresponding to a specific topological measure (degree, closeness, etc.). 

<!---
The script `src/Labatut2022.R` reproduces the computations described in article [[L'22](#references)]. Please, use [v1.0.2](https://github.com/CompNet/NaNet/releases/tag/v1.0.2) of the source code in the *Releases* page. Be warned that this will take a while (possibly several days). You can directly retrieve the data resulting from this process on [Zenodo](https://doi.org/10.5281/zenodo.12804379). 
--->

# Dependencies
Tested with `R` version 4.0.5, with the following packages:
* [`CINNA`](https://cran.r-project.org/web/packages/CINNA/): version 1.1.54.
* [`DescTools`](https://cran.r-project.org/web/packages/DescTools/): version 0.99.39.
* [`future.apply`](https://cran.r-project.org/web/packages/future.apply/): version 1.6.0.
* [`ggplot2`](https://cran.r-project.org/web/packages/ggplot2/): version 3.3.3.
* [`geometry`](https://cran.r-project.org/web/packages/geometry/): version 0.4.5.
* [`GoodmanKruskal`](https://cran.r-project.org/web/packages/GoodmanKruskal/): version 0.0.3.
* [`Hmisc`](https://cran.r-project.org/web/packages/Hmisc/): version 4.5.0.
* [`igraph`](http://igraph.org/r/) package: version 1.2.6.
* [`pcaPP`](https://cran.r-project.org/web/packages/pcaPP/): version 2.0.1.
* [`plotfunctions`](https://cran.r-project.org/web/packages/plotfunctions): version 1.4.
* [`rPref`](https://cran.r-project.org/web/packages/rPref/): version 1.3.
* [`SDMTools`](https://cran.rstudio.com/web/packages/SDMTools): version 1.1.221.
* [`sjstats`](https://cran.r-project.org/web/packages/sjstats/): version 0.18.0.
* [`stringr`](https://cran.r-project.org/web/packages/stringr/): version 1.4.0.
* [`viridis`](https://cran.r-project.org/web/packages/viridis/): version 0.6.0.


# To-do List
* ...


# References
* **[F'22]** M. Ferrand *Usages et représentations de l'espace urbain médiéval : Approche interdisciplinaire et exploration de données géo-historiques d’Avignon à la fin du Moyen Âge*, PhD. Thesis, Avignon University, 2022. [Web Page](https://www.theses.fr/2022AVIG1002)
* **[FL'24]** M. Ferrand and V. Labatut. *Approximating Spatial Distance Through Confront Networks: Application to the Segmentation of Medieval Avignon*. Journal of Complex Networks, 13(1):cnae046, 2025. DOI: [10.1093/comnet/cnae046](http://doi.org/10.1093/comnet/cnae046) - ⟨[hal-04786705](https://hal.archives-ouvertes.fr/hal-04786705)⟩
