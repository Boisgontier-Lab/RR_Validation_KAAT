############################################################
# This code runs two linear mixed-effects models (LMMs), one for active stimuli
# and one for sedentary stimuli, on the data from Cheval et al. (2018) to obtain
# reliable parameter estimates for conducting a simulation-based sensitivity
# power analysis using the simr package.
############################################################

rm(list=ls())

############################################################
## 0) INSTALL & LOAD PACKAGES, SET THE PATH
############################################################

packages <- c("plyr", "dplyr", "lme4", "lmerTest", "simr")
to_install <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(to_install)) install.packages(to_install)
invisible(lapply(packages, library, character.only = TRUE))

# Ensure simr uses Satterthwaite df via lmerTest for method="t"
simr::simrOptions(lmerTestDdf = "Satterthwaite")

path_dir <- "C:/Users/franc/OneDrive - University of Ottawa/Doctorat Ottawa/3 - Article editing/AAT validation study/Registered report/2018_Cheval_code and data"
raw_file <- file.path(path_dir, "raw_data_eprime_zen.csv")

############################################################
## 1) LOAD RAW DATA
############################################################
dataMT <- read.csv(raw_file, sep=";", stringsAsFactors = FALSE)
dataMT$image <- as.character(dataMT$image)

############################################################
## 2) IMAGE LISTS — same as Cheval
## - 5 PA targets
## - 5 SED targets
## - 20 neutral files (10 scenes × 2 versions r/c)
############################################################
PA_set <- c("AP-COUR.jpg","AP-FOOT.jpg","AP-NAT.jpg","AP-RANDO.jpg","AP-VEL.jpg")
SB_set <- c("SED-CANAP.jpg","SED-HAMAC.jpg","SED-JVID.jpg","SED-LECT.jpg","SED-TV.jpg")

neutral_r <- c("AP-COURr.jpg","AP-FOOTr.jpg","AP-NATr.jpg","AP-RANDOr.jpg","AP-VELr.jpg",
               "SED-CANAPr.jpg","SED-HAMACr.jpg","SED-JVIDr.jpg","SED-LECTr.jpg","SED-TVr.jpg")

neutral_c <- c("AP-COURc.jpg","AP-FOOTc.jpg","AP-NATc.jpg","AP-RANDOc.jpg","AP-VELc.jpg",
               "SED-CANAPc.jpg","SED-HAMACc.jpg","SED-JVIDc.jpg","SED-LECTc.jpg","SED-TVc.jpg")

neutral_all <- c(neutral_r, neutral_c)

############################################################
## 3) RECODING Stimuli — only used to reconstruct Movement
############################################################
dataMT$Stimuli <- NA
dataMT$Stimuli[dataMT$image %in% PA_set]    <- "PA"
dataMT$Stimuli[dataMT$image %in% SB_set]    <- "SB"
dataMT$Stimuli[dataMT$image %in% neutral_r] <- "stimuli_rond"
dataMT$Stimuli[dataMT$image %in% neutral_c] <- "stimuli_carre"
dataMT$Stimuli <- factor(dataMT$Stimuli)

############################################################
## 4) RECODING Movement (Approach/Avoid) — Cheval mapping (compat/incomp)
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
## - TrailProc: main trials
## - ACC==1: correct responses only
## - RT window: 150–1500
############################################################
dataMT$subject  <- factor(dataMT$Subject)
dataMT$RT       <- as.numeric(as.character(dataMT$Slide4.RT))
dataMT$ACC      <- as.numeric(as.character(dataMT$Slide4.ACC))
dataMT$Training <- as.character(dataMT$Procedure.SubTrial.)

limit_RT <- c(150, 1500)

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
## 6) CORRECTED item_id (neutral vs target) + treat r/c as distinct items
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
## 7) DATASETS FOR THE TWO MODELS
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
## 7bis) -0.5 / +0.5 CODING 
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
## 1B) Residual diagnostics (raw RTs)
## Goal: document that a Gaussian model on raw RTs is acceptable
## (QQ-plot + residuals vs fitted)
############################################################
diagnose_residuals <- function(m, label){
  op <- par(no.readonly = TRUE)
  par(mfrow = c(1, 2))
  
  # 1) QQ-plot of residuals
  qqnorm(resid(m), main = paste0("QQ-plot résidus — ", label))
  qqline(resid(m))
  
  # 2) Residuals vs fitted
  plot(
    fitted(m), resid(m),
    xlab = "Valeurs ajustées (fitted)",
    ylab = "Résidus",
    main = paste0("Résidus vs fitted — ", label)
  )
  abline(h = 0, lty = 2)
  
  par(op)
}

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
make_design_balanced <- function(N, stim_label){
  
  neu_levels <- paste0("NEU_", 1:20)  
  tgt_levels <- paste0("TGT_", 1:5)   
  
  rows <- vector("list", N)
  
  for(i in seq_len(N)){
    
    neu_app <- sample(rep(neu_levels, length.out = 48))
    neu_avo <- sample(rep(neu_levels, length.out = 48))
    
    neu <- rbind(
      data.frame(subject=i, Movement="Approach", StimulusType="NEU",      item=neu_app),
      data.frame(subject=i, Movement="Avoid",    StimulusType="NEU",      item=neu_avo)
    )
    
    tgt_app <- sample(rep(tgt_levels, length.out = 24))
    tgt_avo <- sample(rep(tgt_levels, length.out = 24))
    
    stim <- rbind(
      data.frame(subject=i, Movement="Approach", StimulusType=stim_label, item=tgt_app),
      data.frame(subject=i, Movement="Avoid",    StimulusType=stim_label, item=tgt_avo)
    )
    
    df_i <- rbind(neu, stim)
    df_i <- df_i[sample.int(nrow(df_i)), ]
    rows[[i]] <- df_i
  }
  
  df <- bind_rows(rows)
  
  df$subject <- factor(df$subject)
  df$Movement <- factor(df$Movement, levels=c("Approach","Avoid"))
  df$StimulusType <- factor(df$StimulusType, levels=c("NEU", stim_label))
  df$item <- factor(df$item)
  
  contrasts(df$Movement)     <- matrix(c(-0.5, 0.5), ncol=1)
  contrasts(df$StimulusType) <- matrix(c(-0.5, 0.5), ncol=1)
  
  stopifnot(nrow(df) == 144 * N)
  
  df
}

############################################################
## Robust helper: identify the interaction term name in fixef()
############################################################
get_interaction_term <- function(m){
  term <- grep("Movement.*StimulusType", names(fixef(m)), value=TRUE)
  if(length(term) != 1){
    stop("Interaction term not uniquely identified. Found: ", paste(term, collapse=", "))
  }
  term
}

############################################################
## 2B) RUN SIMR POWER (fixed N)
## - nsim = 1000 
## - set seed before each simulation (reproducible)
## - Satterthwaite t-test (method='t') for LMM via lmerTest
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
  
  term <- get_interaction_term(m_sim)
  
  powerSim(
    m_sim,
    test = fixed(term, method="t"),  # Satterthwaite via lmerTest
    nsim = nsim,
    fitOpts = list(control = lmerControl(optimizer="bobyqa"))
  )
}

############################################################
## 2C) POWER at N=90 — nsim=1000
############################################################
power90_sed <- run_power_simr(m_sed_main, "SED", N = 90, nsim = 1000, seed = 1001)
power90_pa  <- run_power_simr(m_pa_main,  "PA",  N = 90, nsim = 1000, seed = 1002)

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
nsim_curve  <- 1000
N_seq       <- seq(20, 60, by = 5)

############################################################
## Function: power curve for a given model
##  based on a simulated model (makeLmer) using a balanced design
##  Satterthwaite t-test
############################################################
run_power_curve <- function(m_pilot, stim_label, N_seq, nsim = 1000, seed = 2001){
  
  set.seed(seed)
  
  cat("\n=============================\n")
  cat("POWER CURVE —", stim_label, "vs NEU\n")
  cat("=============================\n")
  
  design_df <- make_design_balanced(max(N_seq), stim_label)
  
  m_sim <- makeLmer(
    formula(m_pilot),
    fixef(m_pilot),
    VarCorr(m_pilot),
    sigma(m_pilot),
    data = design_df
  )
  
  term <- get_interaction_term(m_sim)
  
  m_ext <- extend(m_sim, along = "subject", n = max(N_seq))
  
  pc <- powerCurve(
    m_ext,
    test = fixed(term, method = "t"),   
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
## Run the curves (distinct seeds)
############################################################
pc_sed <- run_power_curve(m_sed_main, "SED", N_seq = N_seq, nsim = nsim_curve, seed = 3001)
pc_pa  <- run_power_curve(m_pa_main,  "PA",  N_seq = N_seq, nsim = nsim_curve, seed = 3002)

#######################################################################
#######################################################################
## PART 4 — Required N for 90% + FIGURES (power curves)
#######################################################################
#######################################################################

############################################################
## 1) Extract the interval [N_lo, N_hi] that brackets 90% (from the power curve)
##    then re-simulate with step = 1 within that interval
############################################################
refine_N_by_resimulation <- function(power_curve_obj, m_pilot, stim_label,
                                     target_power = 0.90,
                                     nsim_refine = 1000,
                                     seed_base = 9000){
  
  tab <- as.data.frame(summary(power_curve_obj))
  
  if(!("subjects" %in% names(tab)) && ("nlevels" %in% names(tab))){
    tab$subjects <- tab$nlevels
  }
  
  if(!("power" %in% names(tab)) && ("mean" %in% names(tab))){
    tab$power <- tab$mean
  }
  
  stopifnot("subjects" %in% names(tab), "power" %in% names(tab))
  
  tab <- tab[order(tab$subjects), ]
  
  if(max(tab$power, na.rm=TRUE) < target_power){
    cat("\n⚠️  Target power not achieved in the tested range.\n")
    return(list(N_exact = NA, tab = tab, refine_tab = NULL,
                N_lo = NA, N_hi = NA))
  }
  
  idx_hi <- which(tab$power >= target_power)[1]
  idx_lo <- idx_hi - 1
  
  if(tab$power[idx_hi] == target_power){
    N_exact <- tab$subjects[idx_hi]
    cat("\n✅ N exact = ", N_exact, " n", sep="")
    return(list(N_exact = N_exact, tab = tab, refine_tab = NULL,
                N_lo = N_exact, N_hi = N_exact))
  }
  
  N_lo <- tab$subjects[idx_lo]; p_lo <- tab$power[idx_lo]
  N_hi <- tab$subjects[idx_hi]; p_hi <- tab$power[idx_hi]
  
  cat("\nDetected framing (power curve) :\n")
  cat("  N =", N_lo, " → power_mean =", round(p_lo,4), "\n")
  cat("  N =", N_hi, " → power_mean =", round(p_hi,4), "\n")
  cat("\n🔁 SIMR RE-SIMULATION in steps of 1 between ", N_lo, " and ", N_hi, "...\n", sep="")
  
  extract_power_from_powersim <- function(ps_obj){
    s <- summary(ps_obj)
    p  <- if("power" %in% names(s)) as.numeric(s[["power"]]) else
      if("mean"  %in% names(s)) as.numeric(s[["mean"]])  else NA_real_
    lo <- if("lower" %in% names(s)) as.numeric(s[["lower"]]) else NA_real_
    hi <- if("upper" %in% names(s)) as.numeric(s[["upper"]]) else NA_real_
    list(power = p, lower = lo, upper = hi)
  }
  
  N_grid <- seq(N_lo, N_hi, by = 1)
  refine_rows <- vector("list", length(N_grid))
  
  for(i in seq_along(N_grid)){
    N_i <- N_grid[i]
    seed_i <- seed_base + N_i
    
    ps <- run_power_simr(m_pilot, stim_label, N = N_i, nsim = nsim_refine, seed = seed_i)
    ex <- extract_power_from_powersim(ps)
    
    refine_rows[[i]] <- data.frame(
      subjects = N_i,
      power = ex$power,
      lower = ex$lower,
      upper = ex$upper
    )
    
    cat("  N=", N_i, " → power_mean=", round(ex$power,4),
        " [", round(ex$lower,4), ", ", round(ex$upper,4), "]\n", sep="")
  }
  
  refine_tab <- bind_rows(refine_rows)
  
  idx <- which(refine_tab$power >= target_power)[1]
  if(is.na(idx)){
    cat("\n⚠️  Even after refinement, the target is not achieved.\n")
    N_exact <- NA
  } else {
    N_exact <- refine_tab$subjects[idx]
    cat("\n✅ N exact (re-simulated) for power_mean  ", target_power*100, "% = ", N_exact, "\n", sep="")
  }
  
  list(N_exact = N_exact, tab = tab, refine_tab = refine_tab,
       N_lo = N_lo, N_hi = N_hi)
}

############################################################
## 2) Figure: curve in step of 5 + CI + threshold (90%) + exact re-simulated N
############################################################
plot_power_curve_step5_with_Nexact <- function(tab, N_exact, stim_label, target_power = 0.90){
  
  if(!("subjects" %in% names(tab)) && ("nlevels" %in% names(tab))){
    tab$subjects <- tab$nlevels
  }
  if(!("power" %in% names(tab)) && ("mean" %in% names(tab))){
    tab$power <- tab$mean
  }
  stopifnot("subjects" %in% names(tab), "power" %in% names(tab))
  
  tab <- tab[order(tab$subjects), ]
  
  N_min <- min(tab$subjects)
  N_max <- max(tab$subjects)
  N_seq5 <- seq(N_min, N_max, by = 5)
  
  tab5 <- tab[tab$subjects %in% N_seq5, , drop=FALSE]
  if(nrow(tab5) < 2) tab5 <- tab
  
  plot(
    x = tab5$subjects,
    y = tab5$power,
    type = "b", pch = 19,
    xlab = "Number of participants (N)",
    ylab = "Power",
    ylim = c(0, 1),
    main = paste0("Power curve — ", stim_label, " vs NEU (interaction effect)")
  )
  
  if(all(c("lower","upper") %in% names(tab5))){
    lines(tab5$subjects, tab5$lower, lty = 2)
    lines(tab5$subjects, tab5$upper, lty = 2)
  }
  
  abline(h = target_power, lty = 3)
  
  if(!is.na(N_exact)){
    abline(v = N_exact, lty = 3)
    points(N_exact, target_power, pch = 19)
    
    segments(x0 = N_exact, y0 = 0, x1 = N_exact, y1 = target_power, lty = 3)
    segments(x0 = par("usr")[1], y0 = target_power, x1 = N_exact, y1 = target_power, lty = 3)
    
    text(
      x = N_exact,
      y = target_power,
      labels = paste0("N = ", N_exact),
      pos = 4
    )
  }
  
  legend(
    "bottomright",
    legend = c("Power (step 5)", "IC 95% (lower/upper)", paste0("Seuil ", target_power*100, "%"), "N exact"),
    lty = c(1, 2, 3, 3),
    pch = c(19, NA, NA, 19),
    bty = "n"
  )
}

############################################################
## 3) Compute exact N (local re-simulation) + summary
############################################################
cat("\n=============================\nSED vs NEU\n=============================\n")
res_sed <- refine_N_by_resimulation(
  power_curve_obj = pc_sed,
  m_pilot = m_sed_main,
  stim_label = "SED",
  target_power = 0.90,
  nsim_refine = 1000,
  seed_base = 91000
)

cat("\n=============================\nPA vs NEU\n=============================\n")
res_pa <- refine_N_by_resimulation(
  power_curve_obj = pc_pa,
  m_pilot = m_pa_main,
  stim_label = "PA",
  target_power = 0.90,
  nsim_refine = 1000,
  seed_base = 92000
)

cat("\n=============================\nFINAL SUMMARY\n=============================\n")
cat("SED  N90 =", res_sed$N_exact, "\n")
cat("PA   N90 =", res_pa$N_exact,  "\n")

############################################################
## 4) Figures (one per model) — DISPLAY STEP OF 5 + exact N
############################################################
par(mfrow = c(1, 2))
plot_power_curve_step5_with_Nexact(res_sed$tab, res_sed$N_exact, stim_label = "SED", target_power = 0.90)
plot_power_curve_step5_with_Nexact(res_pa$tab,  res_pa$N_exact,  stim_label = "PA",  target_power = 0.90)
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
pdf("C:/Users/franc/OneDrive - University of Ottawa/Doctorat Ottawa/3 - Article editing/AAT validation study/Registered report/power_curves_N90.pdf", width = 10, height = 4)
par(mfrow = c(1, 2))
plot_power_curve_step5_with_Nexact(res_sed$tab, res_sed$N_exact, stim_label = "SED", target_power = 0.90)
plot_power_curve_step5_with_Nexact(res_pa$tab,  res_pa$N_exact,  stim_label = "PA",  target_power = 0.90)
par(mfrow = c(1, 1))
dev.off()
