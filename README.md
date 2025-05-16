# Crossing lines Annotating with Tanglegrams on Trees (CatTrees)

## Overview
This Shiny application, **CatTrees**, is designed for analyzing and visualizing phylogenetic trees with a focus on reassortment events. It integrates clade information and highlights reassortment strains in a customizable, interactive interface.

---

## Features
- Upload and process multiple phylogenetic trees in `.nwk` format.
- Integrate clade annotations to colorize and categorize taxa. Highlight reassortment strains with customizable line sizes and colors.
- Generate combined tree plots with annotations for better visualization of evolutionary relationships.
- Export publication-quality tree plots with adjustable resolution, width, and height.

---

## Prerequisites
Ensure the following R packages are installed in your R environment:
- `shiny`, `shinydashboard`, `ggplot2`, `ggtree`, `plotly`, `phytools`, `treeio`, `phangorn`, `dplyr`, `tidyverse`, `highcharter`, and `shinyBS`.

---

## Input File Requirements
1. **Phylogenetic Trees (`file1`)**:
   - Accepts multiple tree files in `.nwk` format.
   - Each tree should contain fewer than 1000 sequences for optimal performance.

2. **Reassortment Strains (`file3`)**:
   - A CSV file containing a list of reassortment strains.
   - Must include a column titled `strain` with strain identifiers.

3. **Clade Information (`file4`)**:
   - A CSV file specifying clade information.
   - Must include columns titled `Taxa` and `Clade`.

---

## Installation
1. Clone or download the repository containing `ui.R` and `server.R`.
2. Place both files in the same directory.

---

## Running the App
Option 1: Use Online Version
You can access the app directly via web browser:
https://5gi4j5-debbie-li.shinyapps.io/CatTrees/

Note: The maximum file upload size is 512MB.

Option 2: Run Locally in RStudio
1. Open R or RStudio.
2. Set your working directory to the folder containing `ui.R` and `server.R`.
3. Run the following command:
   ```R
   shiny::runApp()

---

## Authors and Current Work

This tool is developed by Tai-Jung Li and and Yu-Nong Gong. The current work, titled **"Visualizing and Deciphering Influenza A(H1N1) pdm09 Reassortment in the 2019-2023 Seasons"**, is under submission and highlights the use of this application for analyzing influenza reassortment patterns. For additional help or bug reports, contact the developers at deb03090215@gmail.com and yngong@mail.cgu.edu.tw.
