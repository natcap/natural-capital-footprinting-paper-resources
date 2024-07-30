# natural-capital-footprint-paper-resources

>Code for: _Expanding the E in ESG with high-resolution global mapping of ecosystem services and corporate physical assets._ 2024. Mandle L, Shea A, Soth E, Wolny S, Smith JR, Chaplin-Kramer R, Sharp RP, Patel M and Goldstein JA. preprint: doi: https://doi.org/10.22541/au.170967630.06341452/v1 . 

>Code written by: Emily Soth, Jeffrey R. Smith, and Lisa Mandle

This repository contains the following files:

1. potentialVegSherlock.py : Code for creating the potential natural vegetation scenario
2. endemic.py, kbas.py, redList.py, speciesRichness.py and merge.py: code to create global biodiversity impact maps for species richness, endemic species (range-weighted species richness), Red List species (threatened and endangered species), and Key Biodiversity Areas (KBAs); merge.py merges the country-level maps into a global map.
3. final.ipynb : Jyputer notebook for processing asset-level data from the S&P Physical Risk dataset, processing the global impact maps, and running the ecosystem services footprinting tool for assets belonging to companies in the MSCI ACWI index
4. MSCI_ACWI_analyses_paper.R : code for analyzing company-level impacts of MSCI ACWI companies by sector and industry and creating Figures 2 and 3 in the paper

   
