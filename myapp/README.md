# STATCAL ONLINE - Panel Data Fixed Effect Regression Analyzer

This version adds an EViews-style **Effect Specification** feature.

## New Feature

The app now allows users to choose:

- Cross-section: None / Fixed
- Period: None / Fixed

This produces the following model settings:

| Cross-section | Period | R/plm model |
|---|---|---|
| Fixed | None | `plm(..., model = "within", effect = "individual")` |
| None | Fixed | `plm(..., model = "within", effect = "time")` |
| Fixed | Fixed | `plm(..., model = "within", effect = "twoways")` |
| None | None | `plm(..., model = "pooling")` |

## Other Outputs

The app also displays:

- FEM coefficient table
- FEM coefficient table with robust standard error
- Common Constant C / EViews-style intercept
- Fixed-effect intercepts based on the selected effect specification
- Model fit and fixed-effect test
- Automatic interpretation
- Excel/CSV export

## Required Packages

```r
install.packages(c(
  "shiny", "shinydashboard", "DT", "readxl", "dplyr", "plm",
  "lmtest", "sandwich", "openxlsx", "ggplot2", "shinycssloaders"
))
```

## Run

```r
shiny::runApp(".")
```
