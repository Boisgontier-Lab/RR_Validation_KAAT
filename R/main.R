############################################################
# This code runs two linear mixed-effects models (LMMs), one for active stimuli
# and one for sedentary stimuli, on the data from Cheval et al. (2018) to obtain
# reliable parameter estimates for conducting a simulation-based sensitivity
# power analysis using the simr package.
############################################################

rm(list = ls())

source("R/00_functions_preprocess.R")
source("R/01_functions_analysis.R")
source("R/02_functions_plots.R")

############################################################
## 0) INSTALL & LOAD PACKAGES, SET THE PATH
############################################################

options(repos = c(CRAN = "https://cloud.r-project.org"))

packages <- c("plyr", "dplyr", "lme4", "lmerTest", "simr")
to_install <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(to_install)) install.packages(to_install)
invisible(lapply(packages, library, character.only = TRUE))

# Ensure simr uses Satterthwaite df via lmerTest for method="t"
simr::simrOptions(lmerTestDdf = "Satterthwaite")

path_dir <- file.path("/Users/timotheedumas/Documents/University/UOttawa/Recherche/RR_Validation_KAAT/2018_cheval_code_and_data")
raw_file <- file.path(path_dir, "raw_data_eprime_zen.csv")

############################################################
## 1) LOAD RAW DATA
############################################################
img_sets <- get_image_sets()

############################################################
## 2) IMAGE LISTS — same as Cheval
## - 5 PA targets
## - 5 SED targets
## - 20 neutral files (10 scenes × 2 versions r/c)
############################################################
neutral_all <- c(img_sets$neutral_r, img_sets$neutral_c)

data_main <- load_raw_data(raw_file) %>%
  ############################################################
  ## 3) RECODING Stimuli — only used to reconstruct Movement
  ############################################################
  recode_stimuli(img_sets) %>%

  ############################################################
  ## 4) RECODING Movement (Approach/Avoid) — Cheval mapping (compat/incomp)
  ############################################################
  recode_movement() %>%

  ############################################################
  ## 5) VARIABLES + CLEANING
  ## - TrailProc: main trials
  ## - ACC==1: correct responses only
  ## - RT window: 150–1500
  ############################################################
  clean_data() %>%

  ############################################################
  ## 6) CORRECTED item_id (neutral vs target) + treat r/c as distinct items
  ##
  ## - neutral: NEU_<filename_without_extension>
  ## - targets: TGT_<filename_without_extension>
  ############################################################
  create_item_ids(neutral_all)

############################################################
## 7) DATASETS FOR THE TWO MODELS
############################################################
dat_sed <- create_stimulus_dataset(
    data_main,
    img_sets$SB,
    neutral_all,
    "SED"
) %>%
############################################################
## 7bis) -0.5 / +0.5 CODING 
############################################################
set_coding_pm05(c("NEU", "SED"))

############################################################
## 7) DATASETS FOR THE TWO MODELS
############################################################
dat_pa <- create_stimulus_dataset(
  data_main,
  img_sets$PA,
  neutral_all,
  "PA"
) %>%
############################################################
## 7bis) -0.5 / +0.5 CODING 
############################################################
set_coding_pm05(c("NEU", "PA"))

############################################################
## 8) SANITY CHECKS 
############################################################
run_sanity_checks(dat_sed, "SED")
run_sanity_checks(dat_pa, "PA")





#######################################################################
#######################################################################
## PART 1 — PILOT LMMs (estimates + variance components)
## + residual diagnostics (to justify using raw RTs)
#######################################################################
#######################################################################

############################################################
## 1A) Pilot models
############################################################

m_sed_main <- lmer(
  RT ~ Movement * StimulusType + (1 + Movement | subject) + (1|item),
  data = dat_sed,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa")
)

m_pa_main <- lmer(
  RT ~ Movement * StimulusType + (1 + Movement | subject) + (1|item),
  data = dat_pa,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa")
)

cat("\n===== MAIN PILOT LMM: SED vs NEU =====\n")
print(summary(m_sed_main))
cat("\nSingular fit (SED main)? ", isSingular(m_sed_main, tol = 1e-5), "\n")
cat("\nVarCorr (SED main):\n")
print(VarCorr(m_sed_main))

cat("\n===== MAIN PILOT LMM: PA vs NEU =====\n")
print(summary(m_pa_main))
cat("\nSingular fit (PA main)? ", isSingular(m_pa_main, tol = 1e-5), "\n")
cat("\nVarCorr (PA main):\n")
print(VarCorr(m_pa_main))

############################################################
## 1B) Residual diagnostics (raw RTs)
## Goal: document that a Gaussian model on raw RTs is acceptable
## (QQ-plot + residuals vs fitted)
############################################################

diagnose_residuals(m_sed_main, "SED vs NEU (pilot)")
diagnose_residuals(m_pa_main,  "PA vs NEU (pilot)")

############################################################
## NOTE (-0.5/+0.5 coding)
## The Movement:StimulusType coefficient equals:
##   (Avoid - Approach)_Stim - (Avoid - Approach)_NEU in milliseconds
############################################################





#######################################################################
#######################################################################
## PART 2 — SIMR POWER (N=90) + BALANCED DESIGN
## + Satterthwaite t-test 
#######################################################################
#######################################################################

############################################################
## 2A) Balanced design consistent with the protocol
##
## balance item × Movement (stratified)
## - NEU: 96 trials (48 Approach / 48 Avoid)
## - Stim: 48 trials (24 Approach / 24 Avoid)
## Total = 144 trials / subject
##
## check: nrow(df) == 144*N
############################################################

## Check function make_design_balanced(n_subjects, stim_label) in 01_functions_analysis.R
## Check function get_interaction_term(m) in 01_functions_analysis.R

############################################################
## 2B) RUN SIMR POWER (fixed N)
## - nsim = 1000 
## - set seed before each simulation (reproducible)
## - Satterthwaite t-test (method='t') for LMM via lmerTest
############################################################

## Check function run_power_simr(pilot_model, stim_label, n_subjects, n_simulations, seed) in 01_functions_analysis.R

############################################################
## 2C) POWER at N=90 — nsim=1000
############################################################

power90_sed <- run_power_simr(m_sed_main, "SED", n_subjects = 90, n_simulations = 1000, seed = 1001)
power90_pa  <- run_power_simr(m_pa_main,  "PA",  n_subjects = 90, n_simulations = 1000, seed = 1002)

power90_sed
power90_pa





#######################################################################
#######################################################################
## PARTIE 3 — POWER CURVE (SIMR) — nsim=1000 + seed fixed
#######################################################################
#######################################################################

############################################################
## General parameters
############################################################

n_simulations_curve  <- 1000
sample_sizes         <- seq(20, 60, by = 5)

############################################################
## Run the curves (distinct seeds)
############################################################

pc_sed <- run_power_curve(
    m_sed_main,
    "SED",
    sample_sizes = sample_sizes,
    n_simulations = n_simulations_curve,
    seed = 3001
)
pc_pa  <- run_power_curve(
    m_pa_main,
    "PA",
    sample_sizes = sample_sizes,
    n_simulations = n_simulations_curve,
    seed = 3002
)





#######################################################################
#######################################################################
## PART 4 — Required N for 90% + FIGURES (power curves)
#######################################################################
#######################################################################

############################################################
## 1) Extract the interval [n_lower, n_upper] that brackets 90% (from the power curve)
##    then re-simulate with step = 1 within that interval
############################################################

## Check function refine_N_by_resimulation(power_curve_obj, pilot_model, stim_label, target_power, n_simulations_refine, base_seed) in 01_functions_analysis.R

############################################################
## 2) Figure: curve in step of 5 + CI + threshold (90%) + exact re-simulated N
############################################################

## Check function plot_power_curve_step5_with_Nexact(power_summary, n_exact, stim_label, target_power) in 02_functions_plots.R

############################################################
## 3) Compute exact N (local re-simulation) + summary
############################################################

cat("\n=============================\nSED vs NEU\n=============================\n")
res_sed <- refine_N_by_resimulation(
  power_curve_obj = pc_sed,
  pilot_model = m_sed_main,
  stim_label = "SED",
  target_power = 0.90,
  n_simulations_refine = 1000,
  base_seed = 91000
)

cat("\n=============================\nPA vs NEU\n=============================\n")
res_pa <- refine_N_by_resimulation(
  power_curve_obj = pc_pa,
  pilot_model = m_pa_main,
  stim_label = "PA",
  target_power = 0.90,
  n_simulations_refine = 1000,
  base_seed = 92000
)

cat("\n=============================\nFINAL SUMMARY\n=============================\n")
cat("SED  N90 =", res_sed$n_exact, "\n")
cat("PA   N90 =", res_pa$n_exact,  "\n")

############################################################
## 4) Figures (one per model) — DISPLAY STEP OF 5 + exact N
############################################################

par(mfrow = c(1, 2))
plot_power_curve_step5_with_Nexact(
    res_sed$power_summary,
    res_sed$n_exact,
    stim_label = "SED",
    target_power = 0.90)

plot_power_curve_step5_with_Nexact(
    res_pa$power_summary,
    res_pa$n_exact,
    stim_label = "PA",
    target_power = 0.90)
par(mfrow = c(1, 1))

#### Figure caption ####
# Power curves for the Movement × StimulusType interaction estimated by
# Monte-Carlo simulation (simr). Points represent the mean estimated statistical
# power at each sample size (N), computed across 1000 simulated datasets. Solid
# lines show the estimated power trajectory, and dashed lines indicate the 95%
# Monte-Carlo confidence intervals around these estimates. The horizontal dotted
# line marks the target power (90%). To obtain an exact N at the integer level,
# we re-ran simr power simulations (nsim = 1000) for each N in steps of 1 between
# the two adjacent curve points that bracket the 90% threshold, and selected the
# smallest N for which mean power reached or exceeded 90%. The vertical dotted line
# indicates this re-simulated N, and the segments project the intersection onto
# the x- and y-axes for readability.

############################################################
## Export to PDF
############################################################

pdf(paste0(path_dir, "/power curves.pdf"), width = 10, height = 4)
par(mfrow = c(1, 2))
plot_power_curve_step5_with_Nexact(
    res_sed$power_summary,
    res_sed$n_exact,
    stim_label = "SED",
    target_power = 0.90
)
plot_power_curve_step5_with_Nexact(
    res_pa$power_summary,
    res_pa$n_exact,
    stim_label = "PA",
    target_power = 0.90
)
par(mfrow = c(1, 1))
dev.off()
