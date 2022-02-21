# MS-Network Control Theory

Here, we provided the codes for the logistic regression with ridge regularization technique (the codes can be used for any two class problems) and the figures including the radial and heatmaps used for the preprint https://www.biorxiv.org/content/10.1101/2022.02.17.480876v1. The codes and library are applicable on R version 3.4.4. The codes that were used for the clustering and network control theory approach were introduced in Cornblath et al. (2020) paper. Please check https://github.com/ejcorn/brain_states for the codes related to k-means approach and network control theory.

For the classification analysis, the method was trained with outer and inner loops of k-fold cross validation (k = 5) to optimize the hyperparameters and test model performance. The folds for both inner and outer loops were stratified to ensure that each fold contained the same proportion of subjects in the two classes as the original dataset. The inner loop (repeated over 5 different partitions of the training dataset only) optimized the set of hyperparameters that maximized validation AUC. A model was then fitted using the entire training dataset and those optimal hyperparameters, which was then assessed on the hold-out test set from the outer loop. The outer loop was repeated for 100 different random partitions of the data.

---
The performance of the classification method was assessed with AUC. Parameter coefficients (beta values of the classification models) were used transformed with Haufe transformation to assess variable importance.

---

In case to the classes are imbalance, the over-sampling approach Synthetic Majority Over-sampling Technique (SMOTE) function is also available to obtain a balanced training dataset during cross-validation. SMOTE compensates for imbalanced classes by creating synthetic examples using nearest neighbor information and has been shown to be among the most robust and accurate methods with which to control for imbalanced data. 

---

Please contact Ceren Tozlu (tozluceren@gmail.com) for any questions about the repository.

