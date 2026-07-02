# STATCAL ONLINE Panel Data Fixed Effect Regression Analyzer

This R Shiny application estimates panel data regression using the Fixed Effect Model. It is designed for financial panel data, especially issuer-level data such as IDX listed companies.

## Main features

- Upload Excel data (`.xlsx` or `.xls`)
- Select worksheet
- Select entity variable, for example `Kode Emiten`
- Select time variable, for example `Tahun Laporan`
- Select dependent variable, for example `Earning per Share`
- Select independent variables, for example `Return on Asset (ROA)` and `Debt to Asset Ratio (DAR)`
- Estimate Fixed Effect Model with individual/entity effect
- Display FEM coefficient table
- Display robust standard error using `vcovHC`
- Display Intercept Fixed Effect for each entity
- Display automatic interpretation
- Export results to Excel and CSV

## Required packages

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

## Suggested variables for the provided dataset

- Entity variable: `Kode Emiten`
- Time variable: `Tahun Laporan`
- Entity label: `Nama Perusahan`
- Dependent variable: `Earning per Share`
- Independent variables: `Return on Asset (ROA)` and `Debt to Asset Ratio (DAR)`
