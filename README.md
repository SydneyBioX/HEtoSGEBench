## _Benchmarking the translational potential of spatial gene expression prediction from histology_

This benchmarking pipeline is designed to provide a comprehensive evaluation of methods that predict spot-based spatial gene expression using histology images

### We employed a hierarchy of evaluation categories: 
(1) within image spatial gene expression (SGE) prediction performance for lower-resolution spatial transcriptomics (ST) and higher-resolution 10x Visium data<br> 
(2) cross-study model generalisability, evaluated by applying models trained on ST data to predict Visium tissues, as well as to predict TCGA images to identify whether models were useful for predicting existing H&E images;<br>
(3) clinical translational impact through the prediction of survival outcomes and canonical pathological regions using predicted SGE from TCGA;<br>
(4) usability of the methods encompassing code, documentation and the manuscript;<br>
(5) the computational efficiency.<br>
![image](https://github.com/SydneyBioX/HEtoSGEBench/blob/main/Framework.jpg)

### Processed Data
The processed datasets required for reproduction are available on Zenodo and can be accessed via this DOI link:<br>
https://doi.org/10.5281/zenodo.14602489<br>
Please download and store them in the appropriate directories as required by the scripts.

### Reproduction Steps
We provide the code to reproduce the evaluation results and figures from our work. Please follow the order of the .Rmd files to process your raw prediction data and obtain the results:

- 00-CombineDat.Rmd contains an example dataset and the code used to calculate several evaluation metrics between the predicted SGE and the ground truth.
- 01-BenchmarkUsability.Rmd contains the code used to generate usability plot for each method.
- 02-BenchmarkPredictedExprs.Rmd contains the code used to generate ST and 10x Visium Spatial Gene Expression metrics.
- 03-BenchmarkTCGA.Rmd contains the code used to perform survival analysis using TCGA data.
- 04-BenchmarkRanks.Rmd contains the code used to rank each method based on six categories: ST SGE prediction, Visium SGE prediction, model generalisability, clinical impact, usability, and efficiency. The rankings are visualized using a funky heatmap.

### Reference
If you have any questions, particularly regarding data processing, please contact chuhan.wang@sydney.edu.au. We welcome any suggestions and comments.

Wang, C., & Chan, A. (2025). Benchmarking the translational potential of spatial gene expression prediction from histology (3.0). Zenodo. https://doi.org/10.5281/zenodo.14602489
