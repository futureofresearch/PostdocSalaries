# PostdocSalaries

This repository contains the code for the analysis on postdoc salary data performed for the paper: *Assessing the Landscape of U.S. Postdoctoral Salaries* ([preprint](https://www.biorxiv.org/content/early/2017/12/03/227694)).

• Preprocessing of the raw salary data is in the Jupyter notebook _Pre-processing.ipynb_ run on Python version 3.5.2. The package `pandas` needs to be installed for the code to run.

To protect the identity and sensitive information of the postdocs, the raw salary table containing postoc salaries, names and affiliations is not posted here, but description of the fields can be found in the Jupyter notebook. The table without postdoc names can be found at www.futureofresearch.org.

• The analysis of the preprocessed data is in the file _Analysis.R_ run using R version 3.4.3. The packages `dplyr`, `tidyr`, and `ggplot2` need to be installed for the code to run.

• The `Tables/` folder holds auxiliary tables used for the analysis of these data: 

1. _STEM.csv_ carries the STEM/non-STEM annotation used for each department in the dataset

2. _TableNIH.csv_ contains the NIH and NSF funding information as described in the manuscript

• The `Figures/` folder holds the figures as they appear in the table. Feel free to download and use for your presentations but please reference this github repo or the original paper.

The folder also contains variations of the figures or auxiliary figures that relate to this work. Details of how these figures were created will be found at _Analysis.R_



