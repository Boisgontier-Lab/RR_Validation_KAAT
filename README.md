# RR_Validation_KAAT
**Authors:** François Jabouille, Timothée Dumas, Jérémy Fortier, Michaël Patrick Robichaud, Brian Nhat Thien Nguyen, Frédérik Jolicoeur, Silvio Maltagliati, Boris Cheval, Kayne Park, Matthieu P. Boisgontier

**Contact:** fjabouil@uottawa.ca

## Description

Code and data for the article: "Comparing the KINARM approach-avoidance task with the manikin task to measure automatic motivation: a registered report"

## Installation

1. Unzip `2018_cheval_code_and_data.zip`
2. Copy the absolute path to the unzipped directory (e.g., `/Users/YourUserName/Documents/RR_Validation_KAAT/2018_cheval_code_and_data`)
3. In `R/main.R` at line 28, paste the path:
    ```r
    path_dir <- file.path("/Users/YourUserName/Documents/RR_Validation_KAAT/2018_Cheval_code_and_data")
    ```

## Usage

1. Run `R/main.R`
2. Output plots will be saved to `Rplots.pdf`

## File Structure

- `analysis_scripts/` : MATLAB scripts for extracting and analyzing kinematic data from the KINARM approach-avoidance task (e.g., reaction time, maximum speed, maximum absolute deviation, initial direction, speed peaks, x-flips).
- `manikin_and_kaat_demonstration_videos/` : Demonstration videos of the two tasks compared in the study (KINARM approach-avoidance task and manikin task).
- `R/` : R scripts for preprocessing, statistical analysis (linear mixed models, simulation-based power analysis with `simr`), and visualization of the manikin task data from Cheval et al. (2018). The main entry point is `main.R`.