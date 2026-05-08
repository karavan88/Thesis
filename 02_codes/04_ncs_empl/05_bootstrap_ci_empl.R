# ==============================================================================
# CHAPTER 4 - BOOTSTRAP CIs FOR RANDOM-SLOPE MODELS
# ==============================================================================
#
# File:         05_bootstrap_ci_empl.R
# Purpose:      Parametric bootstrap confidence intervals for the per-group
#               random-slope coefficients of the chapter-4 mixed-effects
#               models that vary the NCS effects by SES, education, sex, and
#               (in the supplementary set) by SES on the white-collar HS
#               subsample.
#
# Method:       Same as the chapter-6 bootstrap (see
#               02_codes/06_ncs_job_satisf/05_bootstrap_ci_jobsatisf.R for
#               the full methodological narrative):
#                 - lme4::bootMer(type = "parametric", use.u = TRUE) so each
#                   replication conditions on the OBSERVED random effects
#                   (right setting for inference about the specific groups
#                   in the data, not a hypothetical new group).
#                 - nsim = 1000 (tunable).
#                 - For each refit: extract the group-specific slopes
#                   (fixef + ranef) for the five NCS traits.
#                 - Build percentile CIs at 90% / 95% / 99% from the
#                   empirical distribution and tag each slope with .,*,**.
#                 - Persist the raw nsim x (n_groups * 5) matrix as an
#                   attribute so future code can recompute CIs at any
#                   threshold without re-running.
#
# Models bootstrapped:
#   m2_empl - NCS x SES quintile, full sample, unweighted
#   m3_empl - NCS x education level, full sample, unweighted
#   m4_empl - NCS x sex, full sample, unweighted
#   m6_empl - NCS x SES quintile, white-collar HS subsample, IPW weights
#
# Inputs:       03_output/empl_outputs/
#                 - models_ncs_empl_tab2.rds  (M2, M3, M4 bundle)
#                 - models_ncs_empl_suppl.rds (M5, M6, "Educational Mismatch")
#               - youth_empl + youth_empl_suppl (provided by 01_data_prep_empl.R)
#
# Outputs:      03_output/empl_outputs/
#                 - m2_empl_bootCI.rds (SES x NCS, full sample)
#                 - m3_empl_bootCI.rds (Edu x NCS, full sample)
#                 - m4_empl_bootCI.rds (Sex x NCS, full sample)
#                 - m6_empl_bootCI.rds (SES x NCS, white-collar HS, IPW)
#               Each .rds is a data frame with the same schema:
#                 group, trait, estimate, boot_mean, se_boot,
#                 ci_low_90, ci_high_90, sig_10,
#                 ci_low_95, ci_high_95, sig_05,
#                 ci_low_99, ci_high_99, sig_01,
#                 p_boot,                # exact 2-sided percentile p
#                 p_holm,                # Holm-corrected (FWER)
#                 p_maxT,                # max-T corrected (FWER)
#                 stars (".", "*", "**", "")  # based on marginal CIs
#               Attributes: "boot_meta" (run config), "draws" (raw matrix).
#
# Multiple comparisons:
#                 - p_boot is the per-slope p-value (no correction).
#                 - p_holm controls within-model FWER via Holm's step-down.
#                 - p_maxT controls within-model FWER using the joint
#                   bootstrap distribution of max |t*| across the K slopes
#                   (Westfall & Young, 1993). Recommended over Holm when
#                   slopes are positively correlated (typical).
#
# Cache logic:  per-model caching - if a model's output .rds already exists,
#               that model is skipped on this run. options(thesis.refit = TRUE)
#               forces a re-bootstrap of every model. Delete a single .rds
#               to refresh just that one.
#
# Configurable: options(thesis.boot.nsim   = 1000)   # default 1000
#               options(thesis.boot.ncpus  = N)      # default detectCores()-1
#               options(thesis.boot.seed   = 42)     # default 42
#               options(thesis.boot.use_u  = TRUE)   # default TRUE
#
# Compute cost: ~30-60 min per model on the full sample (~6000 obs, 15 random
#               parameters in the (1 + O+C+E+A+ES | g) covariance). Sex (2
#               groups) is faster than SES (5) or edu (4). Total wall time on
#               7 cores: roughly 2.5-3.5 hours for all four.
#
# Dependencies: lme4, lmerTest, parallel (base R).
# ==============================================================================

cat("\n", rep("=", 80), "\n")
cat("CHAPTER 4 - BOOTSTRAP CIs FOR RANDOM-SLOPE MODELS\n")
cat("Script: 05_bootstrap_ci_empl.R\n")
cat("Start time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(rep("=", 80), "\n\n")

script_start_time <- Sys.time()

# ----- Global config ------------------------------------------------------
nsim_boot  <- as.integer(getOption("thesis.boot.nsim", 1000L))
ncpus_boot <- as.integer(getOption("thesis.boot.ncpus",
                                   max(1L, parallel::detectCores() - 1L)))
seed_boot  <- as.integer(getOption("thesis.boot.seed", 42L))
use_u_boot <- isTRUE(getOption("thesis.boot.use_u", TRUE))
.refit_all <- isTRUE(getOption("thesis.refit", FALSE))

trait_vars <- c("O", "C", "E", "A", "ES")

# ----- Per-model bootstrap helper ----------------------------------------
# Runs the bootstrap on a single random-slope model, builds the tidy CI
# frame, and saves to disk. Returns the data frame invisibly.
run_bootstrap_for_model <- function(label, model, group_var, data, output_path) {

  cat("\n", strrep("-", 76), "\n", sep = "")
  cat(label, " :: ", group_var, " random-slope model\n", sep = "")
  cat(strrep("-", 76), "\n", sep = "")

  if (!.refit_all && file.exists(output_path)) {
    cat("📦 Cache hit. Skipping bootstrap for ", label, ".\n", sep = "")
    cat("   (Delete ", basename(output_path),
        " or set options(thesis.refit = TRUE) to refresh.)\n", sep = "")
    out <- readRDS(output_path)
    print(attr(out, "boot_meta"))
    return(invisible(out))
  }

  stopifnot(inherits(model, "merMod"))

  # --- Data hygiene: refit on complete-cases subset -----------------------
  vars_formula <- all.vars(formula(model))
  weight_call  <- getCall(model)$weights
  weight_var   <- if (!is.null(weight_call)) all.vars(weight_call) else character(0)
  needed       <- intersect(unique(c(vars_formula, weight_var)),
                            names(data))
  n_total      <- nrow(data)
  clean_data   <- data[
    complete.cases(data[, needed, drop = FALSE]), ,
    drop = FALSE
  ]
  n_clean <- nrow(clean_data)
  cat("📋 Data hygiene: ", n_clean, "/", n_total,
      " rows have non-NA values for all model variables.\n", sep = "")
  if (n_clean < n_total) {
    cat("   Refitting on complete-cases subset...\n")
    model <- update(model, data = clean_data)
    cat("   Refit done. nobs =", stats::nobs(model), "\n")
  }

  # --- Group-specific slope extractor (handles | and || syntax) -----------
  extract_group_slopes <- function(m) {
    fe       <- lme4::fixef(m)
    re_list  <- lme4::ranef(m)
    matching <- names(re_list)[startsWith(names(re_list), group_var)]
    if (length(matching) == 0) {
      stop("No ranef table matching group_var = ", group_var,
           " for model ", label)
    }
    levs <- rownames(re_list[[matching[1]]])
    rs <- matrix(0, nrow = length(levs), ncol = length(trait_vars),
                 dimnames = list(levs, trait_vars))
    for (nm in matching) {
      df <- re_list[[nm]]
      for (tv in trait_vars) {
        if (tv %in% colnames(df)) {
          rs[, tv] <- rs[, tv] + df[levs, tv]
        }
      }
    }
    for (tv in trait_vars) {
      if (tv %in% names(fe)) rs[, tv] <- rs[, tv] + fe[[tv]]
    }
    v <- as.numeric(t(rs)) * 100   # in pp; row-major: g1.O, g1.C, ..., g1.ES, g2.O, ...
    names(v) <- paste0(rep(levs, each = length(trait_vars)),
                       ".", rep(trait_vars, times = length(levs)))
    v
  }

  t0_vec <- extract_group_slopes(model)
  cat("📐 Extracted ", length(t0_vec), " group-specific slopes.\n", sep = "")
  cat("   formula: ", deparse1(formula(model)), "\n", sep = "")

  # --- Bootstrap ----------------------------------------------------------
  cat("🥾 bootMer: nsim = ", nsim_boot, ", ncpus = ", ncpus_boot,
      ", seed = ", seed_boot, ", use.u = ", use_u_boot, "\n", sep = "")
  t0 <- Sys.time()
  boot_obj <- lme4::bootMer(
    model,
    FUN      = extract_group_slopes,
    nsim     = nsim_boot,
    type     = "parametric",
    use.u    = use_u_boot,
    seed     = seed_boot,
    parallel = if (.Platform$OS.type == "windows") "snow" else "multicore",
    ncpus    = ncpus_boot
  )
  boot_runtime <- difftime(Sys.time(), t0, units = "mins")
  cat("✅ bootMer finished in ", round(boot_runtime, 2), " min\n", sep = "")

  # --- Build tidy CI table ------------------------------------------------
  re_first <- names(lme4::ranef(model))[
    startsWith(names(lme4::ranef(model)), group_var)][1]
  group_levels <- rownames(lme4::ranef(model)[[re_first]])

  quant <- function(p) apply(boot_obj$t, 2, quantile, probs = p, na.rm = TRUE)
  ci_low_90 <- quant(0.05);   ci_high_90 <- quant(0.95)
  ci_low_95 <- quant(0.025);  ci_high_95 <- quant(0.975)
  ci_low_99 <- quant(0.005);  ci_high_99 <- quant(0.995)
  se_boot   <- apply(boot_obj$t, 2, sd,   na.rm = TRUE)
  boot_mean <- apply(boot_obj$t, 2, mean, na.rm = TRUE)

  excl0 <- function(lo, hi) sign(lo) == sign(hi) & lo != 0 & hi != 0
  sig_10 <- excl0(ci_low_90, ci_high_90)
  sig_05 <- excl0(ci_low_95, ci_high_95)
  sig_01 <- excl0(ci_low_99, ci_high_99)
  stars  <- ifelse(sig_01, "**",
            ifelse(sig_05, "*",
            ifelse(sig_10, ".", "")))

  # --- Bootstrap p-values + multiple-comparisons corrections ------------
  # See 06_ncs_job_satisf/05_bootstrap_ci_jobsatisf.R for the methodology
  # narrative. p_boot is the marginal 2-sided percentile-bootstrap p;
  # p_holm controls within-model FWER via Holm's step-down; p_maxT applies
  # the single-step max-T correction over the joint bootstrap distribution
  # (Westfall & Young, 1993).
  p_boot <- vapply(seq_len(ncol(boot_obj$t)), function(j) {
    v <- boot_obj$t[, j]
    v <- v[!is.na(v)]
    if (length(v) == 0) return(NA_real_)
    2 * min(mean(v <= 0), mean(v >= 0))
  }, numeric(1))

  p_holm <- p.adjust(p_boot, method = "holm")

  .t_star <- abs(sweep(sweep(boot_obj$t, 2, boot_obj$t0, "-"),
                       2, se_boot, "/"))
  .M_null <- apply(.t_star, 1, max, na.rm = TRUE)
  .t_obs  <- abs(boot_obj$t0 / se_boot)
  p_maxT  <- vapply(.t_obs, function(t) mean(.M_null >= t, na.rm = TRUE),
                    numeric(1))

  out <- data.frame(
    group     = rep(group_levels, each  = length(trait_vars)),
    trait     = rep(trait_vars,   times = length(group_levels)),
    estimate  = boot_obj$t0,
    boot_mean = boot_mean,
    se_boot   = se_boot,
    ci_low_90 = ci_low_90, ci_high_90 = ci_high_90, sig_10 = sig_10,
    ci_low_95 = ci_low_95, ci_high_95 = ci_high_95, sig_05 = sig_05,
    ci_low_99 = ci_low_99, ci_high_99 = ci_high_99, sig_01 = sig_01,
    p_boot    = p_boot,
    p_holm    = p_holm,
    p_maxT    = p_maxT,
    stars     = stars,
    row.names = NULL,
    stringsAsFactors = FALSE
  )

  attr(out, "boot_meta") <- list(
    label         = label,
    group_var     = group_var,
    formula       = deparse1(formula(model)),
    nobs          = stats::nobs(model),
    nsim          = nsim_boot,
    type          = "parametric",
    use.u         = use_u_boot,
    seed          = seed_boot,
    ncpus         = ncpus_boot,
    runtime_min   = as.numeric(boot_runtime),
    fitted_at     = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )
  attr(out, "draws") <- boot_obj$t

  saveRDS(out, output_path)
  cat("💾 Saved ", basename(output_path), "\n", sep = "")

  # --- Diagnostics --------------------------------------------------------
  gap <- abs(out$estimate - out$boot_mean)
  cat("   |estimate - boot_mean|: max = ", round(max(gap), 3),
      " pp, mean = ", round(mean(gap), 3), " pp\n", sep = "")
  cat("   CIs excluding 0: ",
      sum(out$sig_10), " (90%), ",
      sum(out$sig_05), " (95%), ",
      sum(out$sig_01), " (99%) of ",
      nrow(out), " slopes.\n", sep = "")
  cat("   At p < 0.05: p_boot=",
      sum(out$p_boot  < 0.05, na.rm = TRUE), ", p_holm=",
      sum(out$p_holm  < 0.05, na.rm = TRUE), ", p_maxT=",
      sum(out$p_maxT  < 0.05, na.rm = TRUE), " of ",
      nrow(out), " (uncorr / Holm / max-T)\n", sep = "")

  invisible(out)
}

# ----- Load model bundles + raw data -------------------------------------
cat("📦 Loading chapter-4 model bundles + youth_empl data...\n")
if (!exists("youth_empl")) {
  invisible(capture.output(
    source(file.path(codesEmplNcs, "01_data_prep_empl.R"))
  ))
}
stopifnot(exists("youth_empl"))

# `youth_empl_suppl` is created by 03_regression_supplement.R as the
# employed-only subsample (filter(employed_officially == 1)). Recreate
# the same filter here so the M6 bootstrap call below uses the dataset
# the model was actually fit on — passing the full youth_empl triggers
# a "new levels detected" error in bootMer because individual IDs not
# in the M6 fit appear in the refit data.
if (!exists("youth_empl_suppl")) {
  youth_empl_suppl <- youth_empl[!is.na(youth_empl$employed_officially) &
                                 youth_empl$employed_officially == 1, ]
}

models_main <- readRDS(file.path(outputsEmplNcs, "models_ncs_empl_tab2.rds"))
models_supp <- readRDS(file.path(outputsEmplNcs, "models_ncs_empl_suppl.rds"))

# ----- Bootstrap each random-slope model ---------------------------------
run_bootstrap_for_model(
  label       = "M2_empl (SES quintile)",
  model       = models_main[["M2"]],
  group_var   = "ses5",
  data        = youth_empl,
  output_path = file.path(outputsEmplNcs, "m2_empl_bootCI.rds")
)

run_bootstrap_for_model(
  label       = "M3_empl (education level)",
  model       = models_main[["M3"]],
  group_var   = "edu_lvl",
  data        = youth_empl,
  output_path = file.path(outputsEmplNcs, "m3_empl_bootCI.rds")
)

run_bootstrap_for_model(
  label       = "M4_empl (sex)",
  model       = models_main[["M4"]],
  group_var   = "sex",
  data        = youth_empl,
  output_path = file.path(outputsEmplNcs, "m4_empl_bootCI.rds")
)

run_bootstrap_for_model(
  label       = "M6_empl (SES; white-collar HS, IPW)",
  model       = models_supp[["M6"]],
  group_var   = "ses5",
  data        = youth_empl_suppl,
  output_path = file.path(outputsEmplNcs, "m6_empl_bootCI.rds")
)

# ----- COMPLETION --------------------------------------------------------
total_time <- difftime(Sys.time(), script_start_time, units = "mins")
cat("\n", strrep("=", 80), "\n", sep = "")
cat("✅ Chapter 4 bootstrap CIs ready (", round(total_time, 2), " min total)\n", sep = "")
cat("   Caches in:\n")
cat("     ", file.path(outputsEmplNcs, "m2_empl_bootCI.rds"), "\n", sep = "")
cat("     ", file.path(outputsEmplNcs, "m3_empl_bootCI.rds"), "\n", sep = "")
cat("     ", file.path(outputsEmplNcs, "m4_empl_bootCI.rds"), "\n", sep = "")
cat("     ", file.path(outputsEmplNcs, "m6_empl_bootCI.rds"), "\n", sep = "")
cat("\n   Each .rds: tidy CI frame + boot_meta + raw 'draws' matrix attribute.\n")
cat(strrep("=", 80), "\n\n", sep = "")
