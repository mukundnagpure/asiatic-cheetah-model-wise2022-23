# asiatic-cheetah-model-wise2022-23
# Asiatic Cheetah Population Dynamics Model

A modified Lotka-Volterra predator-prey model investigating the impact of car collisions and prey population dynamics on the critically endangered Asiatic cheetah in Iran.

## About

This project was completed as part of the **Models in Conservation Biology** module during the **Winter Semester 2022-23** for my Master's degree.

## Repository Contents

- `cheetah_code.R` - R implementation of the population dynamics model
- `Cheetah_report.pdf` - Detailed report with full methodology, results, and discussion

## Model Overview

The model examines two major threats to the last remaining Asiatic cheetah population in Iran:
- Vehicle collisions on roads crossing cheetah habitat
- Declining prey populations (gazelles, wild sheep, and ibex)

The model also incorporates the effect of inbreeding depression on individual susceptibility to car collisions.

## Requirements

```r
install.packages("deSolve")
```

## Usage

```r
source("cheetah_code.R")
```

## Details

For complete information about the model equations, parameters, assumptions, results, and conservation implications, please refer to the accompanying report (`Cheetah_report.pdf`).

## Author

Mukund Nagpure  
Master's in Ecology and Environmental Change
