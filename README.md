# E401/M518 Empirical Challenge: Cross Validation

## Overview

This repository contains an R script that conducts an empirical analysis of data on Portuguese secondary school students. The analysis aims to investigate the impact of various student characteristics, including alcohol consumption, on their test scores in Math and Portuguese subjects. The project was carried out as part of the E401/M518 Empirical Challenge for Fall 2023.

## Table of Contents

- [Getting Started](#getting-started)
- [Data Description](#data-description)
- [Policy Questions](#policy-questions)
- [Methodology](#methodology)
- [Results](#results)
- [Discussion](#discussion)
- [Future Steps](#future-steps)
- [Conclusion](#conclusion)

## Getting Started

To get started, clone this repository to your local machine. The main script is written in R and uses several packages, which you can install using the following commands:

```R
install.packages(c("boot", "dplyr", "tidyverse", "readr", "corrplot", "caret", "leaps"))

## Data Description

The dataset comes from the [UCI Machine Learning Repository](https://archive.ics.uci.edu/ml/datasets/STUDENT+ALCOHOL+CONSUMPTION) and is divided into two files:

- `studentMat.csv`: Contains performance scores in math courses.
- `studentPor.csv`: Contains test scores in Portuguese language courses.

For a complete list of variable definitions, please refer to the original dataset documentation.

## Policy Questions

The Portuguese Ministry of Health is interested in the following:

1. Which student characteristics have the most significant effects on test scores?
2. Does alcohol consumption significantly affect student performance?
3. Which groups should be targeted most for campaigns against alcohol consumption?

## Methodology

The analysis uses cross-validation techniques for model selection. The criteria for model selection are based on minimizing the cross-validation error, specifically the Mean Squared Error (MSE).

## Results

The results include models that predict the final grades (`G3`) in math and Portuguese subjects based on various student characteristics, including alcohol consumption levels.

## Discussion

The discussion elaborates on the policy implications of the models. It also includes the limitations of the current analysis and suggests steps for further studies.

## Future Steps

Further analysis could involve:

- Trying other model selection techniques like Lasso or Ridge regression.
- Incorporating additional data sources for a more comprehensive analysis.
- Testing the robustness of the models to different conditions and assumptions.

## Conclusion

The analysis aims to provide comprehensive insights into the factors affecting student performance in Math and Portuguese subjects. It is intended to aid the Portuguese Ministry of Health in its policy decision-making.

