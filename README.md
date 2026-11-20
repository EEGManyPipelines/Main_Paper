# Many Pipelines, One Dataset: Code Written for the EEGManyPipelines Main Paper

Here you can find the code used for the main EEGManyPipelines paper "Many Pipelines, One Dataset: Analytic Flexibility and Its Impact in EEG Research". 

## Description of the repository
The code was produced using R 4.4.1 and MATLAB_R2022a languages. The repository contains code for statistical analyses and data visualisation. Here is a more detailed breakdown:

- Link pipeline choices to results. The folder contains code for Linear model and BIC analyses that used effect sizes, p-values, and GA ERP difference wave amplitudes as dependent variables and pipeline choices as independent variables. These analyses correspond to the manuscript sections 2.3.1 - 2.3.3.
- Plot demographic information. The folder contains code to visualise Figure 8.
- Quantify pipelines. The folder contains code to visualise pipeline choices and normalized entropy index. It corresponds to the manuscript section 2.1.
- Quantify results. The folder contains code to calcuate and visualise GA ERP difference waves for three hypotheses. It also stores a folder for the quantification and visualisation of the variablity across participants and teams (manuscript section 2.2.2).
- Unusual pipelines and results. The folder contains code to quantify and visualise the prototypical order of steps (using Levenshtein distance) and choice prototypicality index. It also contains code to calculate and visualise transtion probability matrix (sections 2.1 and 2.3.4).


The data curation code can be found here: https://github.com/EEGManyPipelines/Dataset_paper/tree/main

The project is run by the current Steering Committee members (in alphabetical order): Johannes Algermissen, Niko A. Busch, Elena Cesnaite, Nastassja L. Fischer, Claudia Gianelli, Gustav Nilsonne, Annalisa Pascarella, Darinka Trübutschek, Mikkel C. Vinding, Andrea Vitale, and Yu-Fang Yang.

More about the project: https://eegmanypipelines.github.io/

## Description of the project
Researchers analyzing electroencephalographic (EEG) data are faced with a choice between countless alternative and plausible analysis approaches. The variability of these analytical approaches in real-life practices and the extent to which they affect the results is unknown. The EEGManyPipelines project is a many-analyst project that aims to investigate the robustness of EEG findings across different analytical approaches. With this aim, a single EEG dataset and a set of hypotheses were sent to expert teams. In total, 168 teams completed the task, analyzed the data, and returned their results. We collected data including preprocessed EEG data, processing scripts, analysis questionnaire, and outcome of hypotheses testing. In our analysis approach, we linked the size of a p-value and the size of an ERP difference wave to different analysis pipeline choices.  

## Data availability
The raw and curated data can be downloaded from the brainlife.io platform at https://brainlife.io/project/6863bf5c1521e536327bfea7 under the Creative Commons Attribution (CC-BY) license.
The description on the dataset and its structure, can be found at https://osf.io/preprints/metaarxiv/c4xwg_v1
