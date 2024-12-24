---
title: R Code for Tussey and Yan (2025)
subtitle: Principles for Open Data Curation
date: January 2, 2025
---

This fold contains the R code for reproducing the analysis in the paper.

# File structure

The raw data are available from:

+ ]https://figshare.com/ndownloader/files/50756982
+ https://figshare.com/ndownloader/files/50765283

The R codes are in the `code` directory.

# Reproducing the analysis

+ Clone this repository:

```
git clone git@github.com:jun-yan/nyc311clean.git
```

+ Download the data from `figshare.com`.

(We can write an R script to download the data using native R
function `download.file()`; set them up under the right folders.)

+ Run the scripts

```
cd nyc311clean/code
Rscript -e "data_cleansing.R" # run the code in batch mode
```




