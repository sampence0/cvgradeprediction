# M518 Empirical Challenge: Cross Validation

## Overview

This repository contains an R script that conducts an empirical analysis of data on Portuguese secondary school students. The analysis aims to investigate the impact of various student characteristics, including alcohol consumption, on their test scores in Math and Portuguese subjects. The project was carried out as part of the E401/M518 Empirical Challenge for Fall 2023.

## Getting Started

To get started, clone this repository to your local machine. The main script is written in R and uses several packages, which you can install using the following commands:

```R
install.packages(c("boot", "dplyr", "tidyverse", "readr", "corrplot", "caret", "leaps"))
```

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

1. **Forward Selection**: We initially perform forward selection to identify a subset of predictors that contribute the most to explaining the variance in the final grades (`G3`). This method starts with no predictors and iteratively adds in variables that improve the model's performance based on a chosen criterion, such as the Cp statistic.

2. **Cross-Validation**: After identifying the best subset of predictors, we use K-Fold Cross-Validation to assess the model's generalizability. This technique partitions the original training data set into 'K' equally sized folds. A model is trained on 'K-1' of these folds and validated on the remaining one. The process is repeated 'K' times, each time with a different fold serving as the validation set. The criterion for model selection is the Mean Squared Error (MSE), averaged over all 'K' folds.


## Results

The results include models that predict the final grades (`G3`) in math and Portuguese subjects based on various student characteristics, including alcohol consumption levels.


## Future Steps

Further analysis could involve:

- Trying other model selection techniques like Lasso or Ridge regression.
- Incorporating additional data sources for a more comprehensive analysis.
- Testing the robustness of the models to different conditions and assumptions.



