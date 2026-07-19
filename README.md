# DataVisual_DeconstructingReconstructingwebpage
Deconstructing and reconstructing the 2024 Global Hunger Index map: colour accessibility, projection choice, and direct-labelled findings. Built in R with ggplot2.
# Deconstructing and Reconstructing the 2024 Global Hunger Index

A deconstruction and reconstruction of the *2024 Global Hunger Index by Severity* poster map
(Welthungerhilfe, Concern Worldwide, & IFHV), submitted for RMIT 050646 Data Visualisation
and Communication, Assignment 2.

## Three improvements

1. **Colour and accessibility** — replaced the diverging green-to-red palette with a
   sequential YlOrRd ramp (Brewer & Harrower, 2013), colour-vision-safe under deutan,
   protan, and tritan simulation. Split the single "not designated" grey into two
   distinguishable neutrals: high-income excluded vs. missing data.

2. **Information density** — direct-labelled the seven Alarming countries with their
   2025 scores, and added an adjacent slope panel showing the eight largest reductions
   in GHI score since 2000.

3. **Projection and typography** — switched from cylindrical to Robinson projection
   (ESRI:54030), removed the dense country-name overlay, and replaced the placeholder
   title with a finding-led headline.

## Data sources
- 2025 Global Hunger Index rankings (Welthungerhilfe et al., 2025)
- Natural Earth 1:50m country boundaries

## Built with
- R (tidyverse, sf, ggplot2)
- rnaturalearth for geographies
- ggrepel for direct labels
- patchwork for map + panel composition
