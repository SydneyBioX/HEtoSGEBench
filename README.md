## _Benchmarking the translational potential of spatial gene expression prediction from histology_

This benchmarking pipeline is designed to provide a comprehensive evaluation of methods that predict spot-based spatial gene expression using histology images

### We employed a hierarchy of evaluation categories: 
(1) within image SGE prediction performance for lower-resolution spatial transcriptomics (ST) and higher-resolution 10x Visium data; 
(2) cross-study model generalisability, evaluated by applying models trained on ST data to predict Visium tissues, as well as to predict TCGA images to identify whether models were useful for predicting existing H&E images;
(3) clinical translational impact through the prediction of survival outcomes and canonical pathological regions using predicted SGE from TCGA; 
(4) usability of the methods encompassing code, documentation and the manuscript; 
(5) the computational efficiency.

We provide the code to reproduce the evaluation results and figures from our work. Please follow the order of the .Rmd files to process your raw prediction data and obtain the results.

If you have any questions, particularly regarding data processing, please contact chuhan.wang@sydney.edu.au. We welcome any suggestions and comments.
