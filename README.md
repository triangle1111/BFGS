# Broyden–Fletcher–Goldfarb–Shanno (BFGS) algorithm

The R project for Online Statistical Inference in Federated Learning with Differential Privacy.This project contains multiple R script files that implement different algorithms for the paper, including LocalSGD, FedSGD, CSL and BFGS algorithms. Each script file implements the core logic of the corresponding algorithm and provides examples of related functions.

### Pre-requisites 
1. Installation of R (Version 4.4.2) 
2. RStudio

### Dependencies
- MASS
- dplyr
- openxlsx

### Instructions
- Environment Requirements
Make sure you have the R environment and corresponding packages installed on your computer. You can install the necessary packages using the following command:
install.packages("MASS")

- Steps for Execution  
Open R or RStudio and load the corresponding script file. You can run the script with the following command:
source("localSGD_LinR.R")


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
