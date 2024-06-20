# Code for Paper (ML-MCPD)

This repository contains the code used in the paper titled "Internal and External Exposure Associations of 3-Monochloropropane-1,2-Diol and Its Esters Using Machine Learning." In this study, various machine learning methods were employed to explore the relationships between internal and external exposures of 3-monochloropropane-1,2-diol (3-MCPD).

The code is organized into three main parts:

1. **Data Cleaning and Exploration**: This section focuses on cleaning the data and exploring its characteristics. Highlight the importance of interaction term between DHPMA and covariates in the MLR and GAM models.
2. **Parameters Optimization**: Here, we optimized parameters of machine learning method to overcome overfitting and better understand the associations between internal biomarkers and external exposure.
5. **Association Analysis**: This part evaluates the internal and external associations using optimized ML models in randomly segmented data sets.

The research data is not publicly available. However, those interested can use their own data to replicate the analyses.

### Author

- Sunan Gao

### Methods

The following machine learning methods are used in the study:

- Multiple Linear Regression (MLR)
- Support Vector Regression (SVR)
- Generalized additive models (GAM)
- Random Forest
- XGBoost
- LightGBM
- CatBoost

### Highlights
- Urinary DHPMA concentration exhibits a good associations with diet-sourced 3-MCPD exposure in the machine learning models, indicating its potential as a biomarker of dietary 3-MCPD exposure
- The association between dietary 3-MCPD exposure (external) and urinary DHPMA (internal) is modified by various covariates, as revealed by the variable importance analysis in the machine learning models and the significance of intercation term in MLR/GAM.
