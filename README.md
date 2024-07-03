# Code for Paper (ML-MCPD-Glycidol)

This repository contains the code used in the paper titled "Internal and external exposure associations of 3-monochloropropane-1,2-diol and glycidol and their esters using machine learning." In this study, various machine learning methods were employed to explore the relationships between internal (2,3-dihydroxypropyl mercapturic acid, DHPMA) and external exposures of 3-monochloropropane-1,2-diol (3-MCPD), glycidol, and their esters.

The code is organized into three main parts:

1. **Data Cleaning and Exploration**: This section focuses on cleaning the data and exploring its characteristics. It also highlights the importance of interaction terms between DHPMA and covariates in the MLR and GAM models.
2. **Parameters Optimization**: Here, we optimized parameters of machine learning methods to overcome overfitting and better understand the associations between internal biomarkers and external exposure.
3. **Association Analysis**: This part evaluates the internal and external associations using optimized ML models in randomly segmented data sets.

The research data is not publicly available. However, those interested can use their own data to replicate the analyses.

### Authors

- Sunan Gao
- Yimei Tian

### Methods

The following machine learning methods are used in the study:

- Multiple Linear Regression (MLR)
- Support Vector Regression (SVR)
- Generalized Additive Models (GAM)
- Random Forest
- XGBoost
- LightGBM
- CatBoost

### Highlights

- Urinary DHPMA concentration exhibits good associations with diet-sourced 3-MCPD and glycidol exposure in the machine learning models, indicating its potential as a biomarker of dietary exposure.
- The association between dietary 3-MCPD and glycidol exposure (external) and urinary DHPMA (internal) is modified by various covariates, as revealed by the variable importance analysis in the machine learning models and the significance of interaction terms in MLR/GAM.
- Seven machine learning models demonstrated strong predictive capability for internal and external exposure associations (average R > 0.6).
- GAM and XGBoost models showed the best association and highest accuracy for predicting the relationships between internal and external exposures.
- This study provides a more innovative and accurate method for exposure risk assessment of 3-MCPD, glycidol, and their esters using machine learning approaches.

### Key Findings

- The study included demographic characteristics of 1,587 participants.
- Urinary dihydroxypropyl mercapturic acid (DHPMA) concentration, edible oil intake, and total energy were identified as crucial predictors of dietary intake of 3-MCPD, glycidol, and their esters in the models (P < 0.001).
- The machine learning approach successfully linked dietary external exposure of 3-MCPD, glycidol, and their esters with biomonitoring internal exposure.

This research contributes to the field by offering an innovative methodology for assessing exposure risks associated with 3-MCPD, glycidol, and their esters, which are extensively present in refined oils and heated foods.
