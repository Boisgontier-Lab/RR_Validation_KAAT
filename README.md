# RR_Validation_KAAT
**Authors:** François Jabouille, Timothée Dumas, Kayne Park, Matthieu P. Boisgontier

**Contact:** fjabouil@uottawa.ca

## Description

Code and data for the article: "Comparing the KINARM approach-avoidance task with the manikin task to measure automatic motivation: a registered report"

## Usage

1. Run the following command :
    ```sh
    python3 main.py
    ```
2. Output plots will be saved to `Rplots.pdf`

## File Structure

- `analysis_scripts/` : MATLAB scripts for extracting and analyzing kinematic data from the KINARM approach-avoidance task (e.g., reaction time, maximum speed, maximum absolute deviation, initial direction, speed peaks, x-flips).
- `figures/` : Independant figures files in vector PDF format. 
- `manikin_and_kaat_demonstration_videos/` : Demonstration videos of the two tasks compared in the study (KINARM approach-avoidance task and manikin task).
- `paper/` : Latest version of the manuscript.
- `R/` : R scripts for preprocessing, statistical analysis (linear mixed models, simulation-based power analysis with `simr`), and visualization of the manikin task data from Cheval et al. (2018). The main entry point is `main.R`.

## Source

 The data set originates from the following article: Cheval B, Tipura E, Burra N, Frossard J, Chanal J, Orsholits D, Radel R, Boisgontier MP. 2018. Avoiding sedentary behaviors requires more cortical resources than avoiding physical activity: an EEG study. Neuropsychologia. 119:68–80. [This article is accessible here](https://doi.org/10.1016/j.neuropsychologia.2018.07.029)
