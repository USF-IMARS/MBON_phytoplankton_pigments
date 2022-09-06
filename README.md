# MBON_phytoplankton_pigments

This is a repo to process high performance liquid chromatography (HPLC) data collected from a joing collaboration with the Marine Biodiversity Observation Network (MBON) and NOAA's Atlantic Oceanographic and Meteorological Laboratory (NOAA AOML). The years are between 2016 to 2021 focusing primarily on South Florida waters. 

# Analysis
1. Data Rangling: [`load_data.Rmd`](https://github.com/USF-IMARS/MBON_phytoplankton_pigments/blob/main/Rmd/load_data.Rmd)

2. Merging CHEMTAX-HPLC with microscopy to examine differences between the two methods: [`merg_pig_micro.Rmd`](https://github.com/USF-IMARS/MBON_phytoplankton_pigments/blob/main/Rmd/merg_pig_micro.Rmd). 

  - This was somewhat successful, but was not very informative. 
  - Ref: [Armbrecht, Linda H., et al. "A new approach to testing the agreement of two phytoplankton quantification techniques: Microscopy and CHEMTAX." Limnology and     Oceanography: Methods 13.8 (2015): 425-437.](https://aslopubs.onlinelibrary.wiley.com/doi/full/10.1002/lom3.10037)

3. Mapping date: [`map_template.Rmd`](https://github.com/USF-IMARS/MBON_phytoplankton_pigments/blob/main/Rmd/map_template.Rmd)
- This includes a base template for mapping anywhere using `ggplot2`. Here we specify south Florida. 
- There is also code for interpolation (`fields` library), that will be modified in the future. Currently, only works for base plotting, but will be updated for `ggplot2`.

4. Plotting and simple analysis of data: [`chemtax_analysis.Rmd`](https://github.com/USF-IMARS/MBON_phytoplankton_pigments/blob/main/Rmd/chemtax_analysis.Rmd)
- Here are graphs for phytoplankton functional groups (boxplots, line graphs)
- Includes analysis using Kruskal-Wallis with Dunn's test

5. More plotting: [`plotting_data.Rmd`](https://github.com/USF-IMARS/MBON_phytoplankton_pigments/blob/main/Rmd/plotting_data.Rmd)
- Size fractionation (boxplots and bar charts)
- Histograms
- Scatter plots
- Photoprotective Index
- Ratio of Photoprotective Cartenoid to Photosynthetic Carotenoids PPC:PSC (?)

5. Ordination using PCoA and CCA: [`ordination.Rmd`](https://github.com/USF-IMARS/MBON_phytoplankton_pigments/blob/main/Rmd/ordination.Rmd)
- PCoA is performed
- PerMANOVA is performed
- Canoncial Correspondence Analysis is performed

6. Seascapes: (`seascapes.Rmd`)[https://github.com/USF-IMARS/MBON_phytoplankton_pigments/blob/main/Rmd/seascapes.Rmd]
- loading and extracting of pixels from seascapes concurrent with the CHEMTAX-HPLC data
- plotting

