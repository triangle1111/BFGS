# Broyden–Fletcher–Goldfarb–Shanno (BFGS) algorithm

### Purpose
This repository contains all the data, functions, scripts to run simulations and analysis, and scripts to generate plots for the paper "Online Statistical Inference in Federated Learning with Differential Privacy".

### Pre-requisites 
1. Installation of R (Version 4.4.2) 
2. RStudio

### Dependencies
- MASS
- dplyr
- openxlsx

### Instructions
- Environment Requirements
Make sure you have the R environment and corresponding packages installed on your computer. The package and downstream analysis and simulations depend on several packages, including `MASS`, `dplyr`, etc. You can install the necessary packages using the following command:
```R
install.packages("MASS")
```
- Steps for Execution  
Open R or RStudio and load the corresponding script file. You can run the script with the following command:
```R
source("localSGD_LinR.R")
```
### Data
The two type datasets used in this article.The first one is payment_fraud_dataset(downloaded from [Kaggle](https://www.kaggle.com/)), using a  logistic regression to detect online payments fraud. The second one is generating dataset,using logistic regression and Linear model.

### Project Structure
project/
├── exp1-exp4/ 
├── other_metrics/ 
├──  real_dataset/
└── README.md

### File List
For easier understanding, the above code is modularized:  
- LocalSGD_Lin.R  
- FedSGD_Lin.R 
- FedCSL_Lin.R 
- FedBFGS_Lin.R   
- LocalSGD_LogR.R  
- FedSGD_LogR.R 
- FedCSL_LogR.R 
- FedBFGS_LogR.R     
Implemented a local stochastic gradient descent algorithm, FedSGD algorithm, CSL and BFGS algorithm for training linear models and Logistic regression model, using both generated and real datasets.


#### Contributions -  
Contributions of any kind are welcome, including issue reports, feature requests, and code contributions.

#### License
This project uses the MIT license, please refer to the LICENSE file for details.

For any further queries and difficulties that you face on executing any code, feel free to post it under the issue tab above and we will get back to you as soon as possible.
