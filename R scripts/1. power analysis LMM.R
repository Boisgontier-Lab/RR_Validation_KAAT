############################################################
# This code runs two linear mixed-effects models (LMMs), one for active stimuli
# and one for sedentary stimuli, on the data from Cheval et al. (2018) to obtain
# reliable parameter estimates for conducting a simulation-based sensitivity
# power analysis using the simr package.
############################################################

rm(list=ls())

############################################################
## INSTALL & LOAD PACKAGES, SET THE PATH
############################################################

packages <- c("plyr", "dplyr", "lme4", "lmerTest", "simr")
to_install <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(to_install)) install.packages(to_install)
lapply(packages, library, character.only = TRUE)


path_dir <- "YOUR_PATH" # UPDATE YOUR PATH HERE
raw_file <- file.path(path_dir, "raw_data_eprime_zen.csv")

############################################################
## 1) LOAD RAW DATA
############################################################
dataMT <- read.csv(raw_file, sep=";", stringsAsFactors = FALSE)
dataMT$image <- as.character(dataMT$image)

############################################################
## 2) IMAGE LISTS (Similar Cheval et al. (2018))
## - 5 PA targets
## - 5 SED targets
## - 20 neutral images (10 scenes × 2 r/c renderings)
############################################################
PA_set <- c("AP-COUR.jpg","AP-FOOT.jpg","AP-NAT.jpg","AP-RANDO.jpg","AP-VEL.jpg")
SB_set <- c("SED-CANAP.jpg","SED-HAMAC.jpg","SED-JVID.jpg","SED-LECT.jpg","SED-TV.jpg")

neutral_r <- c("AP-COURr.jpg","AP-FOOTr.jpg","AP-NATr.jpg","AP-RANDOr.jpg","AP-VELr.jpg",
               "SED-CANAPr.jpg","SED-HAMACr.jpg","SED-JVIDr.jpg","SED-LECTr.jpg","SED-TVr.jpg")

neutral_c <- c("AP-COURc.jpg","AP-FOOTc.jpg","AP-NATc.jpg","AP-RANDOc.jpg","AP-VELc.jpg",
               "SED-CANAPc.jpg","SED-HAMACc.jpg","SED-JVIDc.jpg","SED-LECTc.jpg","SED-TVc.jpg")

neutral_all <- c(neutral_r, neutral_c)

############################################################
## 3) RECODING Stimuli 
############################################################
dataMT$Stimuli <- NA
dataMT$Stimuli[dataMT$image %in% PA_set]    <- "PA"
dataMT$Stimuli[dataMT$image %in% SB_set]    <- "SB"
dataMT$Stimuli[dataMT$image %in% neutral_r] <- "stimuli_rond"
dataMT$Stimuli[dataMT$image %in% neutral_c] <- "stimuli_carre"
dataMT$Stimuli <- factor(dataMT$Stimuli)

############################################################
## 4) RECODING Movement factor (Approach or Avoid) - mapping Cheval (compat/incomp)
############################################################
dataMT$groups <- interaction(factor(dataMT$Stimuli),
                             factor(dataMT$Procedure.Trial.))

dataMT$Movement <- plyr::revalue(
  as.character(dataMT$groups),
  c("PA.compat1"="Approach",
    "PA.compat2"="Approach",
    "SB.compat1"="Avoid",
    "SB.compat2"="Avoid",
    "stimuli_carre.compat1"="Avoid",
    "stimuli_carre.compat2"="Avoid",
    "stimuli_rond.compat1"="Approach",
    "stimuli_rond.compat2"="Approach",
    "PA.incomp1"="Avoid",
    "PA.incomp2"="Avoid",
    "SB.incomp1"="Approach",
    "SB.incomp2"="Approach",
    "stimuli_carre.incomp1"="Approach",
    "stimuli_carre.incomp2"="Approach",
    "stimuli_rond.incomp1"="Avoid",
    "stimuli_rond.incomp2"="Avoid"),
  warn_missing = FALSE
)
dataMT$Movement <- factor(dataMT$Movement, levels=c("Approach","Avoid"))

############################################################
## 5) VARIABLES + CLEANING
## - TrailProc : main trials removing training trials
## - ACC==1 : correct
## - RT window: 150–3000ms
############################################################
dataMT$subject  <- factor(dataMT$Subject)
dataMT$RT       <- as.numeric(as.character(dataMT$Slide4.RT))
dataMT$ACC      <- as.numeric(as.character(dataMT$Slide4.ACC))
dataMT$Training <- as.character(dataMT$Procedure.SubTrial.)

limit_RT <- c(150, 3000)

data_main <- data.frame(
  subject  = dataMT$subject,
  Movement = dataMT$Movement,
  RT       = dataMT$RT,
  image    = as.character(dataMT$image),
  ACC      = dataMT$ACC,
  Training = dataMT$Training
) %>%
  filter(
    Training == "TrailProc",
    ACC == 1,
    !is.na(RT),
    RT > limit_RT[1],
    RT < limit_RT[2],
    !is.na(Movement)
  ) %>%
  droplevels()

############################################################
## 6) item_id CORRECTED (neutral vs target) + r/c AS SEPARATE ITEMS
##
## - neutral: NEU_<filename_without_extension>
## - targets: TGT_<filename_without_extension>
############################################################
strip_ext <- function(x) sub("\\.jpg$", "", x)

is_neu_file <- data_main$image %in% neutral_all
base_name <- strip_ext(data_main$image)

data_main$item_id <- ifelse(
  is_neu_file,
  paste0("NEU_", base_name),
  paste0("TGT_", base_name)
)
data_main$item_id <- factor(data_main$item_id)

############################################################
## 7) BUILDING DATASETS FOR THE 2 MODELS
############################################################
dat_sed <- data_main %>%
  filter(image %in% c(SB_set, neutral_all)) %>%
  mutate(
    StimulusType = if_else(image %in% SB_set, "SED", "NEU"),
    StimulusType = factor(StimulusType, levels=c("NEU","SED")),
    Movement     = factor(Movement, levels=c("Approach","Avoid")),
    item         = factor(item_id),
    subject      = factor(subject)
  ) %>%
  droplevels()

dat_pa <- data_main %>%
  filter(image %in% c(PA_set, neutral_all)) %>%
  mutate(
    StimulusType = if_else(image %in% PA_set, "PA", "NEU"),
    StimulusType = factor(StimulusType, levels=c("NEU","PA")),
    Movement     = factor(Movement, levels=c("Approach","Avoid")),
    item         = factor(item_id),
    subject      = factor(subject)
  ) %>%
  droplevels()

############################################################
## 7bis) CODING DEVIATION EFFECT -0.5 / +0.5 
############################################################
set_coding_pm05 <- function(df, stim_levels){
  df$Movement     <- factor(as.character(df$Movement), levels=c("Approach","Avoid"))
  df$StimulusType <- factor(as.character(df$StimulusType), levels=stim_levels)
  
  stopifnot(nlevels(df$Movement)==2, nlevels(df$StimulusType)==2)
  
  contrasts(df$Movement)     <- matrix(c(-0.5, 0.5), ncol=1)
  contrasts(df$StimulusType) <- matrix(c(-0.5, 0.5), ncol=1)
  
  df
}

dat_sed <- set_coding_pm05(dat_sed, c("NEU","SED"))
dat_pa  <- set_coding_pm05(dat_pa,  c("NEU","PA"))

contrasts(dat_sed$Movement)
contrasts(dat_pa$Movement)

############################################################
## NOTE (coding -0.5/+0.5)
## The coefficient Movement:StimulusType is therefore equivalent to :
##   (Avoid-Approach)_Stim - (Avoid-Approach)_NEU  in ms
############################################################

############################################################
## 8) SANITY CHECKS 
############################################################
cat("\nSED counts (StimulusType x Movement):\n"); print(table(dat_sed$StimulusType, dat_sed$Movement))
cat("SED #subjects:", length(unique(dat_sed$subject)), "\n")
cat("SED #items:", length(unique(dat_sed$item)), "\n")
cat("SED item levels (head):\n"); print(head(levels(dat_sed$item), 10))

cat("\nPA counts (StimulusType x Movement):\n"); print(table(dat_pa$StimulusType, dat_pa$Movement))
cat("PA #subjects:", length(unique(dat_pa$subject)), "\n")
cat("PA #items:", length(unique(dat_pa$item)), "\n")
cat("PA item levels (head):\n"); print(head(levels(dat_pa$item), 10))

stopifnot(length(unique(dat_sed$item)) == 25)
stopifnot(length(unique(dat_pa$item))  == 25)

#######################################################################
#######################################################################
## PART 1 — LMM ON PILOT DATA (estimates + variance components)
## + residual diagnostics (justify raw RT)
#######################################################################
#######################################################################

############################################################
## 1A) MODELS ON THE PILOT DATA (the most complex without singular fits)
############################################################
m_sed_main <- lmer(
  RT ~ Movement * StimulusType + (1 + Movement | subject) + (1|item),
  data = dat_sed,
  REML = FALSE,
  control = lmerControl(optimizer="bobyqa")
)

m_pa_main <- lmer(
  RT ~ Movement * StimulusType + (1 + Movement | subject) + (1|item),
  data = dat_pa,
  REML = FALSE,
  control = lmerControl(optimizer="bobyqa")
)

cat("\n===== MAIN PILOT LMM: SED vs NEU =====\n"); print(summary(m_sed_main))
cat("\nSingular fit (SED main)? ", isSingular(m_sed_main, tol=1e-5), "\n")
cat("\nVarCorr (SED main):\n"); print(VarCorr(m_sed_main))

cat("\n===== MAIN PILOT LMM: PA vs NEU =====\n"); print(summary(m_pa_main))
cat("\nSingular fit (PA main)? ", isSingular(m_pa_main, tol=1e-5), "\n")
cat("\nVarCorr (PA main):\n"); print(VarCorr(m_pa_main))

############################################################
## 1B) Residual diagnostics
## Objective: document that the Gaussian model on raw RT is acceptable
## (QQ-plot + residuals vs. adjusted values)
############################################################
diagnose_residuals <- function(m, label){
  op <- par(no.readonly = TRUE)
  par(mfrow = c(1, 2))
  
  # 1) QQ-plot of residuals
  qqnorm(resid(m), main = paste0("QQ-plot résidus — ", label))
  qqline(resid(m))
  
  # 2) Residual vs fitted
  plot(
    fitted(m), resid(m),
    xlab = "Adjusted values (fitted)",
    ylab = "Residuals",
    main = paste0("Residuals vs fitted — ", label)
  )
  abline(h = 0, lty = 2)
  
  par(op)
}

diagnose_residuals(m_sed_main, "SED vs NEU (pilot)")
diagnose_residuals(m_pa_main,  "PA vs NEU (pilot)")

#######################################################################
#######################################################################
## PART 2 — POWER SIMR (for N = 90) 
#######################################################################
#######################################################################

############################################################
## 2A) Balanced design in accordance with our protocol
##
## Item × Movement balancing (
## - NEU: 96 trials (48 Approach / 48 Avoid)
## - Stimulus: 48 trials (24 Approach / 24 Avoid)
## Total = 144 trials/subject
##
## Check: nrow(df) == 144*N
############################################################
make_design_balanced <- function(N, stim_label){
  
  neu_levels <- paste0("NEU_", 1:20)  # 20 neutral (r/c separated)
  tgt_levels <- paste0("TGT_", 1:5)   # 5 target per category
  
  rows <- vector("list", N)
  
  for(i in seq_len(N)){
    
    # NEU: 48 trials per Movement 
    neu_app <- sample(rep(neu_levels, length.out = 48))
    neu_avo <- sample(rep(neu_levels, length.out = 48))
    
    neu <- rbind(
      data.frame(subject=i, Movement="Approach", StimulusType="NEU",       item=neu_app),
      data.frame(subject=i, Movement="Avoid",    StimulusType="NEU",       item=neu_avo)
    )
    
    # TGT: 24 trials per Movement  
    tgt_app <- sample(rep(tgt_levels, length.out = 24))
    tgt_avo <- sample(rep(tgt_levels, length.out = 24))
    
    stim <- rbind(
      data.frame(subject=i, Movement="Approach", StimulusType=stim_label,  item=tgt_app),
      data.frame(subject=i, Movement="Avoid",    StimulusType=stim_label,  item=tgt_avo)
    )
    
    # randomize trial order
    df_i <- rbind(neu, stim)
    df_i <- df_i[sample.int(nrow(df_i)), ]
    rows[[i]] <- df_i
  }
  
  df <- bind_rows(rows)
  
  df$subject <- factor(df$subject)
  df$Movement <- factor(df$Movement, levels=c("Approach","Avoid"))
  df$StimulusType <- factor(df$StimulusType, levels=c("NEU", stim_label))
  df$item <- factor(df$item)
  
  # same coding -0.5/+0.5 as in the pilot models
  contrasts(df$Movement)     <- matrix(c(-0.5, 0.5), ncol=1)
  contrasts(df$StimulusType) <- matrix(c(-0.5, 0.5), ncol=1)
  
  # Expected check: 144 trials/subject
  stopifnot(nrow(df) == 144 * N)
  
  df
}

############################################################
## 2B) RUN POWER SIMR (N fixe)
## - nsim = 1000 
## - seed fixed before each simulation (reproducible)
############################################################
run_power_simr <- function(m_pilot, stim_label, N = 90, nsim = 1000, seed = 123){
  
  set.seed(seed)
  
  design_df <- make_design_balanced(N, stim_label)
  
  m_sim <- makeLmer(
    formula(m_pilot),
    fixef(m_pilot),
    VarCorr(m_pilot),
    sigma(m_pilot),
    data = design_df
  )
  
  term <- grep("Movement.*StimulusType", names(fixef(m_sim)), value=TRUE)
  stopifnot(length(term) == 1)
  
  powerSim(
    m_sim,
    test = fixed(term, method="z"),
    nsim = nsim,
    fitOpts = list(control = lmerControl(optimizer="bobyqa"))
  )
}

############################################################
## 2C) POWER à N=90 — nsim = 1000
############################################################
power90_sed <- run_power_simr(m_sed_main, "SED", N = 90, nsim = 1000, seed = 1001)
power90_pa  <- run_power_simr(m_pa_main,  "PA",  N = 90, nsim = 1000, seed = 1002)

power90_sed
power90_pa

#######################################################################
#######################################################################
## PART 3 — POWER CURVE (SIMR) — nsim = 1000 + seed fixed
#######################################################################
#######################################################################

############################################################
## General parameters (demande : nsim = 1000)
############################################################
nsim_curve  <- 1000
N_seq       <- seq(40, 120, by = 10)

############################################################
## Function: power curve for a given model
# on simulated model (makeLmer) based on balanced design
############################################################
run_power_curve <- function(m_pilot, stim_label, N_seq, nsim = 1000, seed = 2001){
  
  set.seed(seed)
  
  cat("\n=============================\n")
  cat("POWER CURVE —", stim_label, "vs NEU\n")
  cat("=============================\n")
  
  # Design équilibré au N max
  design_df <- make_design_balanced(max(N_seq), stim_label)
  
  # Modèle simulé calé sur le pilote
  m_sim <- makeLmer(
    formula(m_pilot),
    fixef(m_pilot),
    VarCorr(m_pilot),
    sigma(m_pilot),
    data = design_df
  )
  
  term <- grep("Movement.*StimulusType", names(fixef(m_sim)), value = TRUE)
  stopifnot(length(term) == 1)
  
  # Étendre si besoin (robustesse)
  m_ext <- extend(m_sim, along = "subject", n = max(N_seq))
  
  pc <- powerCurve(
    m_ext,
    test = fixed(term, method = "z"),
    along = "subject",
    breaks = N_seq,
    nsim = nsim,
    fitOpts = list(control = lmerControl(optimizer="bobyqa"))
  )
  
  print(summary(pc))
  plot(pc)
  
  pc
}

############################################################
## Launch the curves (separate seeds)
############################################################
pc_sed <- run_power_curve(m_sed_main, "SED", N_seq = N_seq, nsim = nsim_curve, seed = 3001)
pc_pa  <- run_power_curve(m_pa_main,  "PA",  N_seq = N_seq, nsim = nsim_curve, seed = 3002)

#######################################################################
#######################################################################
## PART 4 — N required for 90% power + FIGURES (power curves)
#######################################################################
#######################################################################

############################################################
## 1) Extract N for a target power
############################################################
get_N_for_power <- function(power_curve_obj, target_power = 0.90){
  
  tab <- as.data.frame(summary(power_curve_obj))
  
  # colonne N: parfois "subjects", parfois "nlevels"
  if(!("subjects" %in% names(tab)) && ("nlevels" %in% names(tab))){
    tab$subjects <- tab$nlevels
  }
  
  # colonne power: parfois "power", parfois "mean"
  if(!("power" %in% names(tab)) && ("mean" %in% names(tab))){
    tab$power <- tab$mean
  }
  
  stopifnot("subjects" %in% names(tab), "power" %in% names(tab))
  
  tab <- tab[order(tab$subjects), ]
  
  idx <- which(tab$power >= target_power)[1]
  
  if(is.na(idx)){
    cat("\n⚠️  Target power not achieved in the tested range.\n")
    return(list(N = NA, tab = tab))
  }
  
  N_needed <- tab$subjects[idx]
  cat("\n✅ Minimum N for", target_power*100, "% power =", N_needed, "subjects\n")
  
  list(N = N_needed, tab = tab)
}

############################################################
## 2) Figure: power curve + IC + threshold (90% default) + target N
############################################################
plot_power_curve <- function(tab, N_target, stim_label, target_power = 0.90){
  
  # column N: sometimes “subjects,” sometimes “nlevels”
  if(!("subjects" %in% names(tab)) && ("nlevels" %in% names(tab))){
    tab$subjects <- tab$nlevels
  }
  
  # power column: sometimes “power,” sometimes “mean”
  if(!("power" %in% names(tab)) && ("mean" %in% names(tab))){
    tab$power <- tab$mean
  }
  
  stopifnot("subjects" %in% names(tab), "power" %in% names(tab))
  
  tab <- tab[order(tab$subjects), ]
  
  plot(
    x = tab$subjects,
    y = tab$power,
    type = "b", pch = 19,
    xlab = "Number of participants (N)",
    ylab = "Power",
    ylim = c(0, 1),
    main = paste0("Power curve — ", stim_label, " vs NEU (interaction effect)")
  )
  
  # IC 95%
  lines(tab$subjects, tab$lower, lty = 2)
  lines(tab$subjects, tab$upper, lty = 2)
  
  # threshold (90% default)
  abline(h = target_power, lty = 3)
  
  # Required N
  if(!is.na(N_target)){
    abline(v = N_target, lty = 3)
    text(
      x = N_target,
      y = target_power,
      labels = paste0("N", target_power*100, " = ", N_target),
      pos = 4
    )
  }
  
  legend(
    "bottomright",
    legend = c("Power", "IC 95% (lower/upper)", paste0("Threshold ", target_power*100, "%")),
    lty = c(1, 2, 3),
    pch = c(19, NA, NA),
    bty = "n"
  )
}

############################################################
## 3) Calculation for N = 90 + summary
############################################################
cat("\n=============================\nSED vs NEU\n=============================\n")
res_sed <- get_N_for_power(pc_sed, target_power = 0.90)

cat("\n=============================\nPA vs NEU\n=============================\n")
res_pa  <- get_N_for_power(pc_pa,  target_power = 0.90)

cat("\n=============================\nFINAL SUMMARY\n=============================\n")
cat("SED  N =", res_sed$N, "\n")
cat("PA   N =", res_pa$N,  "\n")

############################################################
## 4) Figures  
############################################################
par(mfrow = c(1, 2))
plot_power_curve(res_sed$tab, res_sed$N, stim_label = "SED", target_power = 0.90)
plot_power_curve(res_pa$tab,  res_pa$N,  stim_label = "PA",  target_power = 0.90)
par(mfrow = c(1, 1))

#### Figure caption ####
# Power curves for the Movement × StimulusType interaction estimated by 
# Monte-Carlo simulation (simr). Points represent the mean estimated statistical 
# power at each sample size (N), computed across 1000 simulated datasets. Solid 
# lines show the estimated power trajectory, and dashed lines indicate the 95% 
# Monte-Carlo confidence intervals around these estimates. The horizontal dotted
# line marks the target power (90%), and the vertical dotted line indicates the 
# smallest sample size at which the lower bound of the confidence interval reaches 
# this threshold. Sample size selection was based on this lower bound to ensure 
# conservative and robust power despite simulation uncertainty.

############################################################
## Export in PDF 
############################################################
pdf("YOUR_PATH.pdf", width = 10, height = 4) # UPDATE YOUR PATH HERE
par(mfrow = c(1, 2))
plot_power_curve(res_sed$tab, res_sed$N, stim_label = "SED", target_power = 0.90)
plot_power_curve(res_pa$tab,  res_pa$N,  stim_label = "PA",  target_power = 0.90)
par(mfrow = c(1, 1))
dev.off()
