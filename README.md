
# DedooseR 

<!-- badges: start -->
[![R-CMD-check](https://github.com/abiraahmi/DedooseR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/abiraahmi/DedooseR/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

**DedooseR** is an R package that connects with 
[Dedoose](https://www.dedoose.com/) to support the analysis of qualitative data.
It was built to help researchers streamline workflows, explore qualitative data flexibly,
and conduct qualitative coding and analysis with rigor. 

## Key Features

DedooseR currently has 8 key functions that allow you to:

- `clean_data`: standardizes column names, keeps the highest ranked coder per transcript, drops range/weight columns, prefixes code variables with c_, and returns both a cleaned dataset and codebook
- `recode_themes`: combines selected codes into a single logical column and updates the codebook.
- `view_excerpts`: create an interactive, filterable datatable to view the excerpts behind each code
- `wordcloud`: filters excerpts for a selected code, removes common stop words, and renders the result into a beautiful word cloud
- `create_code_summary` to summarize code counts and the proportion of transcripts/media objects they come from, set a min count or proportion for the summary output and plot counts and proportions (or both!)
- `set_saturation`: uses the output of create_code_summary to filter and visualize codes that meet minimum appearance targets
- `compare_saturation`: builds on the summary table to check multiple threshold sets at once - useful when you want compare a strict bar versus a more liberal bar. You can also plot these different bars against each other
- `cooccurence`: helps you see which codes travel together within the same transcript or media title, building both a matrix and a network plot

## Why This Package?

Ongoing challenges in qualitative research include defining what constitutes high-quality data and demonstrating transparency in how saturation is  reached (Small & Calarco, 2022). Informed by guidelines for high-quality  qualitative research (Hennink & Kaiser, 2022), DedooseR allows you to
better understand your data with quality tags in Dedoose like:

- The **concreteness** of excerpts (priority code)
- The **heterogeneity** within codes  (heterogeneity code)

By tagging these indicators in Dedoose and exploring them in R, 
this allows for gain greater confidence in both the **depth** and **diversity** 
of datasets.

## Installation

You can install the released version of DedooseR from CRAN:

```r
install.packages("DedooseR")
```

And load it using:

```r
library(DedooseR)
```

## How do I use the package?
The vignettes walk you through how to use each of the functions, from cleaning to recoding to viewing excerpts to assessing saturation and creating code co-occurence network maps, so do check them out!

## Acknowledgements
We sincerely thank Ritvik Kammend, Karen Edema, and Safalta Shukla for their contributions to testing the package and refining its documentation and examples. Their care and curiosity helped the project take a clearer, steadier shape.

## References
Hennink, M., & Kaiser, B. N. (2022). Sample sizes for saturation in qualitative  research: A systematic review of empirical tests. Social science & medicine, 292, 114523.

Small, M. L., & Calarco, J. M. (2022). Qualitative Literacy: A Guide to Evaluating Ethnographic and Interview Research (1st ed.). University of California Press. https://doi.org/10.2307/j.ctv2vr9c4x 

