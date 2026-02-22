#' Generate a balanced simulated dataset for power analysis
#'
#' This function creates a balanced experimental design consistent with the protocol,
#' for use in simulation-based power analysis (e.g., with simr).
#' For each subject, it generates:
#'   - 96 neutral trials (48 Approach, 48 Avoid) distributed across 20 neutral items
#'   - 48 target trials (24 Approach, 24 Avoid) distributed across 5 target items
#' The items are randomly sampled for each condition and subject.
#' All variables are coded as factors, with -0.5/+0.5 contrasts for Movement and StimulusType.
#' The function checks that the total number of rows is 144 * N.
#'
#' @param N Integer. Number of subjects to simulate.
#' @param stim_label Character. Label for the target stimulus type (e.g., "PA" or "SED").
#' @return A data frame with the balanced design, ready for power simulation.
#' @examples
#' df <- make_design_balanced(90, "SED")
make_design_balanced <- function(N, stim_label){
  
  neu_levels <- paste0("NEU_", 1:20)
  tgt_levels <- paste0("TGT_", 1:5)
  
  rows <- vector("list", N)
  
  for(i in seq_len(N)){
    
    neu_app <- sample(rep(neu_levels, length.out = 48))
    neu_avo <- sample(rep(neu_levels, length.out = 48))
    
    neu <- rbind(
      data.frame(
        subject = i,
        Movement = "Approach",
        StimulusType = "NEU",
        item = neu_app
      ),
      data.frame(
        subject = i,
        Movement = "Avoid",
        StimulusType = "NEU",
        item = neu_avo
      )
    )
    
    tgt_app <- sample(rep(tgt_levels, length.out = 24))
    tgt_avo <- sample(rep(tgt_levels, length.out = 24))
    
    stim <- rbind(
      data.frame(
        subject = i,
        Movement = "Approach",
        StimulusType = stim_label,
        item = tgt_app
      ),
      data.frame(
        subject = i,
        Movement = "Avoid",
        StimulusType = stim_label,
        item = tgt_avo
      )
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

#' Identify the interaction term name in model fixed effects
#'
#' This helper function searches for the interaction term between Movement and StimulusType
#' in the fixed effects of a linear mixed model. It returns the exact term name as used in the model.
#' If the interaction term is not uniquely identified, the function stops with an error.
#'
#' @param m A fitted linear mixed model (e.g., from lmer).
#' @return Character string. The name of the interaction term in the model.
#' @examples
#' term <- get_interaction_term(m)
get_interaction_term <- function(m){
  term <- grep("Movement.*StimulusType", names(fixef(m)), value=TRUE)
  if(length(term) != 1){
    stop("Interaction term not uniquely identified. Found: ", paste(term, collapse=", "))
  }
  term
}

#' Run a power simulation for a linear mixed model using simr
#'
#' This function performs a power analysis for a given pilot linear mixed model,
#' using a balanced simulated dataset and the simr package. It generates the design,
#' simulates the model, identifies the interaction term, and computes power using
#' the Satterthwaite t-test (via lmerTest).
#'
#' @param m_pilot A fitted pilot linear mixed model (from lmer).
#' @param stim_label Character. Label for the target stimulus type (e.g., "PA" or "SED").
#' @param N Integer. Number of subjects to simulate (default: 90).
#' @param nsim Integer. Number of simulations to run (default: 1000).
#' @param seed Integer. Random seed for reproducibility (default: 123).
#' @return A simr powerSim object containing the power estimate and confidence interval.
#' @examples
#' ps <- run_power_simr(m_pilot, "SED", N = 90, nsim = 1000, seed = 123)
run_power_simr <- function(
    m_pilot,
    stim_label,
    N = 90,
    nsim = 1000,
    seed = 123
){
  
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

#' Run a power curve simulation for a linear mixed model using simr
#'
#' This function computes a power curve for a given pilot linear mixed model,
#' based on a balanced simulated dataset and the simr package. It generates the design,
#' simulates the model, extends it to different sample sizes, and estimates power
#' for the interaction effect across a sequence of participant numbers.
#' The Satterthwaite t-test (via lmerTest) is used for significance testing.
#' The function prints the summary and plots the power curve.
#'
#' @param m_pilot A fitted pilot linear mixed model (from lmer).
#' @param stim_label Character. Label for the target stimulus type (e.g., "PA" or "SED").
#' @param N_seq Integer vector. Sequence of sample sizes (number of subjects) to test.
#' @param nsim Integer. Number of simulations per sample size (default: 1000).
#' @param seed Integer. Random seed for reproducibility (default: 2001).
#' @return A simr powerCurve object containing power estimates for each sample size.
#' @examples
#' pc <- run_power_curve(m_pilot, "SED", N_seq = seq(40, 120, by = 5), nsim = 1000, seed = 2001)
run_power_curve <- function(
    m_pilot,
    stim_label,
    N_seq,
    nsim = 1000,
    seed = 2001
){
  
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

#' Refine sample size estimation by re-simulation around the power threshold
#'
#' This function takes a power curve object and identifies the interval of sample sizes
#' that brackets the target power (e.g., 90%). It then re-simulates power in steps of 1
#' participant within this interval to estimate the exact sample size required to reach
#' the target power. Results are printed and returned as a list.
#'
#' @param power_curve_obj A simr powerCurve object (from run_power_curve).
#' @param m_pilot A fitted pilot linear mixed model (from lmer).
#' @param stim_label Character. Label for the target stimulus type (e.g., "PA" or "SED").
#' @param target_power Numeric. Desired power threshold (default: 0.90).
#' @param nsim_refine Integer. Number of simulations per sample size for refinement (default: 1000).
#' @param seed_base Integer. Base random seed for reproducibility (default: 9000).
#' @return A list with the exact sample size, summary tables, and interval.
#' @examples
#' res <- refine_N_by_resimulation(pc, m_pilot, "SED", target_power = 0.90, nsim_refine = 1000)
refine_N_by_resimulation <- function(
    power_curve_obj,
    m_pilot,
    stim_label,
    target_power = 0.90,
    nsim_refine = 1000,
    seed_base = 9000
){
  
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
