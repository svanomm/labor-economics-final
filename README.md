# Labor Economics Final Project: Education Effects on Female Wages

**Final Project for Labor Economics class, 2025**  

## Project Overview

This project examines the causal effect of education on female wages using advanced econometric methods to address common sources of bias in wage equations. The analysis demonstrates the first known application of Conditional Mixed Process (CMP) methodology to simultaneously correct for both ability bias and employment selection bias.

### Research Question
How does additional education affect female wages when controlling for both ability bias (self-selection into education) and employment selection bias?

### Key Findings
- **OLS estimates** may underestimate the true education effect due to selection biases
- **Instrumental Variables (IV)** correction increases the measured education effect compared to OLS
- **Heckman correction** for employment selection decreases the education effect
- **Combined CMP approach** controlling for both biases yields an education effect slightly larger than OLS and consistent with existing literature

## Data Source

This project uses the **National Longitudinal Survey of Youth 1997 (NLSY97)** from the Bureau of Labor Statistics:
- **Sample**: 8,984 individuals born 1980-1984, surveyed from 1997-2022
- **Focus**: Female participants only for employment selection analysis
- **Panel Structure**: Up to 7 years of wage/education data per person (2010-2021)
- **Key Variables**: Hourly wages, years of education, demographics, family background, childhood characteristics

## Methodology

The project employs four main econometric approaches:

1. **Ordinary Least Squares (OLS)**: Baseline Mincer wage equation
2. **Instrumental Variables (IV)**: Controls for ability bias using parental education, childhood drug use, school quality, and family structure
3. **Heckman Correction**: Controls for employment selection bias 
4. **Conditional Mixed Process (CMP)**: Novel application combining IV and Heckman corrections simultaneously

## Repository Structure

```
├── data/                          # Raw and processed NLSY97 data
│   ├── nlsy97-raw.*              # Original NLSY97 data files
│   ├── NLS_data.rds              # Processed R data file
│   └── reg_data.csv              # Analysis-ready dataset
├── scripts/                       # Analysis code
│   ├── 00 NLSY97 load.R          # Load raw NLSY97 data
│   ├── 01 data processing.R      # Data cleaning and variable creation
│   ├── 02 analysis.do            # Main econometric analysis (Stata)
│   └── 03 summary table.R        # Generate summary statistics
├── output/                        # Results and tables
│   ├── results.pdf               # Main regression results
│   ├── *.tex                     # LaTeX tables for paper
│   ├── *.dta                     # Stata result datasets
│   └── *.txt                     # Text output files
├── 20250508___VanOmmeren_Labor_Economics_Final.pdf  # Final paper
├── Drafting.md                   # Paper content in markdown
└── README.md                     # This file
```

## How to Reproduce the Analysis

### Prerequisites
- **R** (with packages: here, tidyverse, dplyr, stargazer, ivreg, kableExtra)
- **Stata** (version 18+, with packages: cmp, estout, texsave, ghk2)

### Steps to Reproduce

1. **Data Preparation**:
   ```r
   # In R, run the data processing scripts:
   source("scripts/00 NLSY97 load.R")      # Load raw NLSY97 data
   source("scripts/01 data processing.R")   # Clean and process data
   source("scripts/03 summary table.R")     # Generate summary statistics
   ```

2. **Main Analysis**:
   ```stata
   * In Stata, run the econometric analysis:
   do "scripts/02 analysis.do"              # Estimate all models
   ```

3. **Output**: Results will be generated in the `output/` directory, including:
   - Regression tables in LaTeX format
   - Summary statistics
   - Model comparison results

### Key Variables Created

- **Drug Intensity Score**: Composite measure of childhood drug use (0-105 scale)
- **School Quality Score**: Measure of positive school experience (-7 to +5 scale)  
- **Employment Status**: Binary indicator for wage earners vs. non-workers
- **Experience Variables**: Work experience and experience squared

## Instruments for IV Estimation

The analysis uses the following instruments for education:
- **Parental Education**: Father's and mother's years of education
- **Family Structure**: Parental divorce status in 2002
- **Childhood Behaviors**: Drug intensity and school quality scores

These instruments satisfy the relevance condition (correlated with education) while plausibly satisfying the exclusion restriction (uncorrelated with wage equation residuals after controlling for other factors).

## Key Contributions

1. **Methodological Innovation**: First application of CMP to combine IV and Heckman corrections in wage analysis
2. **Novel Instruments**: Use of childhood drug use and school quality measures as education instruments
3. **Comprehensive Bias Correction**: Simultaneous control for ability bias and employment selection
4. **Robust Results**: Consistent findings across multiple sensitivity specifications

## Citation

If you use this code or methodology, please cite:
```
VanOmmeren (2025). "Education Effects on Female Wages: A Conditional Mixed Process Approach 
to Ability Bias and Employment Selection."
```

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
