# Code and data associated with _An open-source approach for measuring corporate impacts on ecosystem services and biodiversity_

>Code and data for: Mandle L, Shea A, Soth E, Goldstein, JA, Wolny S, Smith JR, Chaplin-Kramer R, Sharp RP, and Patel M and Goldstein JA. (2024). An open-source approach for measuring corporate impacts on ecosystem services and biodiversity. _Communications Earth & Environment_ 5, 625. https://doi.org/10.1038/s43247-024-01797-7 . 

>Code written by: Emily Soth, Jeffrey R. Smith, Lisa Mandle, and Jesse A. Goldstein

This repository contains the following files:

* `potentialVegSherlock.py` and `merge.py` : Code for creating the potential natural vegetation scenario; merge.py merges the country-level scenarios into a global map
* `endemic.py`, `kbas.py`, `redList.py`, `speciesRichness.py` and `merge.py`: Code to create global biodiversity impact maps for species richness, endemic species (range-weighted species richness), Red List species (threatened and endangered species), and Key Biodiversity Areas (KBAs); merge.py merges the country-level maps into a global map
* `final.ipynb` : Jupyter notebook for processing asset-level data from the S&P Physical Risk dataset, processing the global impact maps, and running the Ecosystem Services Footprinting Tool for assets belonging to companies in the MSCI ACWI index
* `MSCI_ACWI_analyses_paper.R` : Code for analyzing company-level impacts of MSCI ACWI companies by sector and industry and creating Figures 2 and 3 in the paper
  * `fig2_sector_plot.csv` contains the company-level values used to generate Figure 2, excluding revenue data to calculate revenue-adjusted values that licenses prevent us from redistributing
  * `fig3_materials_plot.csv` contains the company-level values used to generate Figure 3, excluding revenue data to calculate revenue-adjusted values that licenses prevent us from redistributing
* `comparisonPlot.R` : Code for generating Figure 4 of lithium mine impacts comparisons
  * `lithiumMineComparisonPlot.csv` : CSV file containing input data to run `comparisonPlot.R` script and create plots comparing lithium mine total impacts.
  * `lithiumMineComparisonPlot_Rank.csv` : CSV file containing input data to run `comparisonPlot.R` script and create plots comparing lithium mines ranked by their impacts.
* `timeseries_PUBLIC.R` : Code to create Figure 5 plots of Greenbushes, Australia lithium mine impacts through time; excluding proprietary production data from S&P.
  * `Greenbushes_Impact_trends_PUBLIC.csv` : CSV file containing input data to run `timeseries_PUBLIC.R` script and create plots comparing the change in impacts of the Greenbushes lithium mine through time (2016-2023); excluding proprietary production data from S&P.

Additional related material:
* The Ecosystem Services Footprinting Tool and global ecosystems services impact maps (coastal risk reduction, sediment retention, nutrient retention, and nature access) are available at https://github.com/natcap/natural-capital-footprint-impact.

