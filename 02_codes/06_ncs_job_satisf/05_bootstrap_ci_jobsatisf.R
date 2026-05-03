# ==============================================================================
# CHAPTER 6 - BOOTSTRAP CIs FOR M3 RANDOM-SLOPE COEFFICIENTS
# ==============================================================================
#
# File:         05_bootstrap_ci_jobsatisf.R
# Purpose:      Parametric bootstrap confidence intervals for the per-quintile
#               group-specific slopes of m3_satisf (the random-slope model that
#               varies the NCS effects by hourly_wage_quintile). Supplies the
#               CI columns needed to extend tbl-js-rs-coefs in chapter 6.
#
# Method:       lme4::bootMer(type = "parametric", use.u = TRUE), simulating
#               from the fitted model with the OBSERVED random effects held
#               fixed, then refitting and re-extracting the 5 quintiles x
#               5 traits = 25 group-specific slopes on each refit. Percentile
#               CIs are computed from the empirical distribution.
#
#               Why use.u = TRUE: we want CIs for the slope IN EACH OBSERVED
#               quintile (conditional inference). use.u = FALSE redraws the
#               random effects from their prior on every iteration, which
#               answers the different question "what slope would I see in a
#               new, hypothetical quintile from the same population?". For a
#               within-sample summary table that's the wrong CI; the CI ends
#               up centred on the fixed effect rather than on the BLUP, and
#               the resulting "significance" pattern is misleading.
#
#               Group-specific slope = fixef + ranef. With the |  syntax
#               coef(m)$<group> already does the addition for us. With the
#               || (uncorrelated) syntax, lme4 splits one term into several
#               ranef tables under the same group name, and coef() doesn't
#               combine them cleanly, so we walk ranef() + fixef() ourselves.
#
# Inputs:       03_output/jobsatisf_outputs/jobsatisf_models.rds
#                 - bundle from 03_regressions_jobsatisf.R; auto-built by
#                   sourcing 03 if missing.
#
# Output:       03_output/jobsatisf_outputs/m3_satisf_bootCI.rds
#                 - data frame columns:
#                     quintile, trait, estimate, boot_mean, se_boot,
#                     ci_low_90, ci_high_90, sig_10,    # 90% CI / p<0.10
#                     ci_low_95, ci_high_95, sig_05,    # 95% CI / p<0.05
#                     ci_low_99, ci_high_99, sig_01,    # 99% CI / p<0.01
#                     p_boot,                           # exact 2-sided percentile p
#                     p_holm,                           # Holm-corrected (FWER)
#                     p_maxT,                           # max-T corrected (FWER)
#                     stars  ("", ".", "*", "**" — based on marginal CIs)
#                 - all CI/estimate values in percentage points (estimate * 100).
#                 - attribute "boot_meta" stores nsim, type, seed, runtime,
#                   use_u, and the model formula at fit time.
#                 - attribute "draws" stores the raw nsim x 25 bootstrap
#                   matrix so any future code can compute CIs / corrections
#                   at additional thresholds without re-running. Access:
#                     attr(readRDS(...), "draws")
#
# Multiple comparisons:
#                 - p_boot is the per-slope p-value (no correction).
#                 - p_holm controls within-model family-wise error rate via
#                   Holm's step-down procedure on the K slopes.
#                 - p_maxT controls within-model FWER using the joint
#                   bootstrap distribution of the maximum |t*| across K
#                   slopes (Westfall & Young, 1993). Recommended over Holm
#                   when slopes are positively correlated (typical here).
#                 - Use whichever standard the chapter reports.
#
# Cache logic:  if the output .rds already exists, the script skips the
#               bootstrap and loads it back; set options(thesis.refit = TRUE)
#               to force a re-bootstrap, or simply delete the .rds. Cache hits
#               print the stored boot_meta so you can see what produced them.
#               IMPORTANT: cache files written by earlier versions of this
#               script (use.u = FALSE, with a label/value alignment bug) are
#               stale. Delete them before re-running.
#
# Configurable: options(thesis.boot.nsim   = 1000)   # default 1000
#               options(thesis.boot.ncpus  = N)      # default detectCores()-1
#               options(thesis.boot.seed   = 42)     # default 42
#               options(thesis.boot.use_u  = TRUE)   # default TRUE
#
# Compute cost: ~60-90 min for nsim = 1000 on 7 cores when m3_satisf has the
#               full (O+C+E+A+ES | hourly_wage_quintile) covariance (15 random
#               parameters). The diagonal (||) version (5 random parameters)
#               is faster -- expect ~30-40 min. Set nsim = 200 for a quick
#               sanity check.
#
# Dependencies: lme4, lmerTest (both already used by 03), parallel (base R).
# ==============================================================================

cat("\n", rep("=", 80), "\n")
cat("CHAPTER 6 - BOOTSTRAP CIs FOR M3 RANDOM-SLOPE COEFFICIENTS\n")
cat("Script: 05_bootstrap_ci_jobsatisf.R\n")
cat("Start time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(rep("=", 80), "\n\n")

script_start_time <- Sys.time()

# ----- Cache --------------------------------------------------------------
boot_cache_path <- file.path(outputsJobSatisfNcs, "m3_satisf_bootCI.rds")
.refit_boot     <- isTRUE(getOption("thesis.refit", FALSE))
.boot_cache_hit <- !.refit_boot && file.exists(boot_cache_path)

if (.boot_cache_hit) {
  cat("📦 Cache hit. Loading bootstrap CIs from:\n   ", boot_cache_path, "\n")
  m3_satisf_bootCI <- readRDS(boot_cache_path)
  print(attr(m3_satisf_bootCI, "boot_meta"))
} else {

# ----- Load m3_satisf from the chapter-6 model bundle ---------------------
jobsatisf_cache_path <- file.path(outputsJobSatisfNcs, "jobsatisf_models.rds")
if (!file.exists(jobsatisf_cache_path)) {
  cat("⚠ Model bundle not found - running 03_regressions_jobsatisf.R...\n")
  source(file.path(codesJobSatisfNcs, "03_regressions_jobsatisf.R"))
} else {
  .cached <- readRDS(jobsatisf_cache_path)
  m3_satisf <- .cached$m3_satisf
  rm(.cached)
}

stopifnot(inherits(m3_satisf, "merMod"))
group_var  <- "hourly_wage_quintile"
trait_vars <- c("O", "C", "E", "A", "ES")

# ----- Data hygiene: refit on complete-cases subset -----------------------
# bootMer's simulate step reconstructs predictions on the original `data`
# frame and errors on NAs in any grouping variable ("NAs are not allowed in
# prediction data for grouping variables..."), even though lme4 silently
# dropped those rows at fit time via na.omit. Refit m3 on a subset where
# every model variable + weight is non-NA so simulate can do its job.

if (!exists("youth_job_satisf")) {
  cat("ℹ youth_job_satisf not in scope - sourcing 01_data_prep_jobsatisf.R\n")
  invisible(capture.output(
    source(file.path(codesJobSatisfNcs, "01_data_prep_jobsatisf.R"))
  ))
}

# Match the edu_lvl reorder applied by 03_regressions_jobsatisf.R, so the
# refit uses the same reference category as the cached fit.
edu_lvl_levels <- c("4. Tertiary", "1. No school",
                    "2. Secondary School", "3. Secondary Vocational")
youth_job_satisf$edu_lvl <- factor(youth_job_satisf$edu_lvl,
                                   levels = edu_lvl_levels)

vars_formula <- all.vars(formula(m3_satisf))
weight_call  <- getCall(m3_satisf)$weights
weight_var   <- if (!is.null(weight_call)) all.vars(weight_call) else character(0)
needed       <- intersect(unique(c(vars_formula, weight_var)),
                          names(youth_job_satisf))

n_total <- nrow(youth_job_satisf)
clean_data <- youth_job_satisf[
  complete.cases(youth_job_satisf[, needed, drop = FALSE]), ,
  drop = FALSE
]
n_clean <- nrow(clean_data)

cat("📋 Data hygiene: ", n_clean, "/", n_total,
    " rows have non-NA values for all model variables.\n", sep = "")
if (n_clean < n_total) {
  cat("   Refitting m3_satisf on the complete-cases subset...\n")
  m3_satisf <- update(m3_satisf, data = clean_data)
  cat("   Refit done. nobs =", stats::nobs(m3_satisf), "\n\n")
} else {
  cat("   No NA rows to drop; using the cached fit as-is.\n\n")
}

# ----- Custom statistic: extract group-specific slopes --------------------
# Robust to both `(traits | g)` and `(traits || g)`. The latter splits one
# random-effects term into several ranef tables under the same grouping
# factor, so coef(m) doesn't combine them. We walk ranef() ourselves, sum
# the random deviations across all matching tables, then add the fixed
# effect for each trait. Output is a flat numeric vector in row-major
# (level, trait) order so labels and values stay aligned.
.extract_group_slopes <- function(m) {
  fe       <- lme4::fixef(m)
  re_list  <- lme4::ranef(m)
  re_names <- names(re_list)

  # Tables keyed on the focal grouping factor. lme4's `||` may name them
  # like "g", "g.1", "g.2", etc. — match by leading prefix and verify the
  # rownames look like the same level set as the first match.
  matching <- re_names[startsWith(re_names, group_var)]
  if (length(matching) == 0) {
    stop("No ranef table matching group_var = ", group_var,
         ". ranef names are: ", paste(re_names, collapse = ", "))
  }

  levs <- rownames(re_list[[matching[1]]])

  # Sum random deviations across matching tables (each ranef element holds
  # one or more of the trait columns; `||` puts each in its own table).
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

  # Add the fixed-effect part of each trait (these contribute their own
  # bootstrap variation on every refit).
  for (tv in trait_vars) {
    if (tv %in% names(fe)) rs[, tv] <- rs[, tv] + fe[[tv]]
  }

  # Flatten in (level, trait) row-major order so labels match values:
  # Q1.O, Q1.C, Q1.E, Q1.A, Q1.ES, Q2.O, Q2.C, ..., Q5.ES.
  v <- as.numeric(t(rs)) * 100   # in percentage points
  names(v) <- paste0(rep(levs, each = length(trait_vars)),
                     ".", rep(trait_vars, times = length(levs)))
  v
}

# Sanity check on the original model
.t0_vec <- .extract_group_slopes(m3_satisf)
cat("📐 Extracted", length(.t0_vec), "group-specific slopes from m3_satisf:\n")
cat("   groups:", paste(rownames(lme4::ranef(m3_satisf)[[
        names(lme4::ranef(m3_satisf))[
          startsWith(names(lme4::ranef(m3_satisf)), group_var)][1]]]),
      collapse = ", "), "\n")
cat("   traits:", paste(trait_vars, collapse = ", "), "\n")
cat("   formula:", deparse1(formula(m3_satisf)), "\n\n")

# ----- Bootstrap ----------------------------------------------------------
nsim_boot  <- as.integer(getOption("thesis.boot.nsim", 1000L))
ncpus_boot <- as.integer(getOption("thesis.boot.ncpus",
                                   max(1L, parallel::detectCores() - 1L)))
seed_boot  <- as.integer(getOption("thesis.boot.seed", 42L))
use_u_boot <- isTRUE(getOption("thesis.boot.use_u", TRUE))

cat("🥾 Parametric bootstrap: nsim =", nsim_boot,
    ", ncpus =", ncpus_boot,
    ", seed =", seed_boot,
    ", use.u =", use_u_boot, "\n")
cat("   (use.u = TRUE conditions on the OBSERVED random effects for each\n")
cat("    quintile — the right setting when the table's quintiles are the\n")
cat("    target of inference rather than a sample from a population.)\n\n")

t0 <- Sys.time()
boot_obj <- lme4::bootMer(
  m3_satisf,
  FUN      = .extract_group_slopes,
  nsim     = nsim_boot,
  type     = "parametric",
  use.u    = use_u_boot,
  seed     = seed_boot,
  parallel = if (.Platform$OS.type == "windows") "snow" else "multicore",
  ncpus    = ncpus_boot
)
boot_runtime <- difftime(Sys.time(), t0, units = "mins")
cat("✅ bootMer finished in", round(boot_runtime, 2), "min\n\n")

# ----- Build tidy CI table ------------------------------------------------
# Column order in boot_obj$t / boot_obj$t0 is set by .extract_group_slopes
# above: level varies slowest, trait fastest. Build the tidy frame in the
# same order so labels and values line up.
re_first_match <- names(lme4::ranef(m3_satisf))[
  startsWith(names(lme4::ranef(m3_satisf)), group_var)][1]
quintile_labels <- rownames(lme4::ranef(m3_satisf)[[re_first_match]])

# Three sets of percentile CIs straight from the raw bootstrap matrix:
#   90% (p<0.10), 95% (p<0.05), 99% (p<0.01).
# A slope is "significant at level X" iff the CI at that level excludes 0
# (i.e. ci_low and ci_high have the same sign and neither is exactly 0).
quant <- function(p) apply(boot_obj$t, 2, quantile, probs = p, na.rm = TRUE)
ci_low_90  <- quant(0.05);   ci_high_90 <- quant(0.95)
ci_low_95  <- quant(0.025);  ci_high_95 <- quant(0.975)
ci_low_99  <- quant(0.005);  ci_high_99 <- quant(0.995)
se_boot    <- apply(boot_obj$t, 2, sd,   na.rm = TRUE)
boot_mean  <- apply(boot_obj$t, 2, mean, na.rm = TRUE)

excl0 <- function(lo, hi) sign(lo) == sign(hi) & lo != 0 & hi != 0

# Conventional star marker, mirroring modelsummary's default scheme:
#   ***  p < 0.001 (not estimated here — would need finer percentiles)
#   **   p < 0.01
#   *    p < 0.05
#   .    p < 0.10
make_stars <- function(s10, s05, s01) {
  ifelse(s01, "**",
  ifelse(s05, "*",
  ifelse(s10, ".", "")))
}

sig_10 <- excl0(ci_low_90, ci_high_90)
sig_05 <- excl0(ci_low_95, ci_high_95)
sig_01 <- excl0(ci_low_99, ci_high_99)

# ----- Bootstrap p-values + multiple-comparisons corrections --------------
# p_boot: exact two-sided percentile-bootstrap p-value, defined as
#   2 * min(P(theta* <= 0), P(theta* >= 0)).
# p_holm: Holm step-down correction within this model (controls FWER).
# p_maxT: single-step max-T correction (Westfall & Young, 1993). Each draw
#   is recentered at the original estimate to mimic the joint null; the
#   maximum |t*| across the K slopes per draw forms the null distribution
#   of the maximum, against which each observed |t_obs| = |theta_hat / se|
#   is compared. Properly accounts for between-slope correlation.
p_boot <- vapply(seq_len(ncol(boot_obj$t)), function(j) {
  v <- boot_obj$t[, j]
  v <- v[!is.na(v)]
  if (length(v) == 0) return(NA_real_)
  2 * min(mean(v <= 0), mean(v >= 0))
}, numeric(1))

p_holm <- p.adjust(p_boot, method = "holm")

# Center each bootstrap draw at the original estimate, scale by SE
.t_star  <- abs(sweep(sweep(boot_obj$t, 2, boot_obj$t0, "-"),
                      2, se_boot, "/"))
.M_null  <- apply(.t_star, 1, max, na.rm = TRUE)   # null dist of the max
.t_obs   <- abs(boot_obj$t0 / se_boot)
p_maxT   <- vapply(.t_obs, function(t) mean(.M_null >= t, na.rm = TRUE),
                   numeric(1))

m3_satisf_bootCI <- data.frame(
  quintile   = rep(quintile_labels, each  = length(trait_vars)),
  trait      = rep(trait_vars,      times = length(quintile_labels)),
  estimate   = boot_obj$t0,          # original group-specific slope (pp)
  boot_mean  = boot_mean,            # bootstrap-distribution mean (sanity)
  se_boot    = se_boot,
  ci_low_90  = ci_low_90,  ci_high_90 = ci_high_90,  sig_10 = sig_10,
  ci_low_95  = ci_low_95,  ci_high_95 = ci_high_95,  sig_05 = sig_05,
  ci_low_99  = ci_low_99,  ci_high_99 = ci_high_99,  sig_01 = sig_01,
  p_boot     = p_boot,               # marginal two-sided p
  p_holm     = p_holm,               # Holm-corrected within model
  p_maxT     = p_maxT,               # max-T corrected within model
  stars      = make_stars(sig_10, sig_05, sig_01),
  row.names  = NULL,
  stringsAsFactors = FALSE
)

attr(m3_satisf_bootCI, "boot_meta") <- list(
  model         = "m3_satisf",
  formula       = deparse1(formula(m3_satisf)),
  nsim          = nsim_boot,
  type          = "parametric",
  use.u         = use_u_boot,
  seed          = seed_boot,
  ncpus         = ncpus_boot,
  runtime_min   = as.numeric(boot_runtime),
  fitted_at     = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
)

# Save the raw bootstrap matrix as an attribute so future code can compute
# CIs at any threshold without re-running. nsim x 25 doubles is small (~200
# KB at nsim=1000); cheap to carry. Access via `attr(m3_satisf_bootCI, "draws")`.
attr(m3_satisf_bootCI, "draws") <- boot_obj$t

# ----- Save ---------------------------------------------------------------
saveRDS(m3_satisf_bootCI, boot_cache_path)
cat("💾 Saved bootstrap CIs to:\n   ", boot_cache_path, "\n\n")

}  # end if (.boot_cache_hit) else { bootstrap + save }

# ----- Diagnostics --------------------------------------------------------
# With use.u = TRUE the bootstrap mean should sit very close to the original
# point estimate; large gaps would suggest something is off (e.g. unstable
# refits, label/value misalignment).
cat("📊 Bootstrap CI table (95% by default; stars: . p<0.10, * p<0.05, ** p<0.01):\n")
print(m3_satisf_bootCI[, c("quintile","trait","estimate",
                           "ci_low_95","ci_high_95","stars")],
      row.names = FALSE, digits = 3)

if ("boot_mean" %in% colnames(m3_satisf_bootCI)) {
  gap <- with(m3_satisf_bootCI, abs(estimate - boot_mean))
  cat("\n   |estimate - boot_mean|: max =", round(max(gap), 3),
      "pp, mean =", round(mean(gap), 3), "pp\n")
  cat("   (with use.u = TRUE these should both be small.)\n")
}

cat("\n   Slopes whose CI excludes 0 at:\n")
cat("     - 90% (p<0.10):", sum(m3_satisf_bootCI$sig_10), "/", nrow(m3_satisf_bootCI), "\n")
cat("     - 95% (p<0.05):", sum(m3_satisf_bootCI$sig_05), "/", nrow(m3_satisf_bootCI), "\n")
cat("     - 99% (p<0.01):", sum(m3_satisf_bootCI$sig_01), "/", nrow(m3_satisf_bootCI), "\n")

if ("p_holm" %in% colnames(m3_satisf_bootCI)) {
  cat("\n   Slopes surviving multiple-comparisons correction (p < 0.05):\n")
  cat("     - p_boot (uncorrected):    ",
      sum(m3_satisf_bootCI$p_boot < 0.05, na.rm = TRUE),
      "/", nrow(m3_satisf_bootCI), "\n")
  cat("     - p_holm (Holm step-down): ",
      sum(m3_satisf_bootCI$p_holm < 0.05, na.rm = TRUE),
      "/", nrow(m3_satisf_bootCI), "\n")
  cat("     - p_maxT (max-T joint):    ",
      sum(m3_satisf_bootCI$p_maxT < 0.05, na.rm = TRUE),
      "/", nrow(m3_satisf_bootCI), "\n")
}
cat("     - 99% (p<0.01):", sum(m3_satisf_bootCI$sig_01), "/", nrow(m3_satisf_bootCI), "\n\n")

# ----- COMPLETION ---------------------------------------------------------
total_time <- difftime(Sys.time(), script_start_time, units = "mins")
cat(rep("=", 80), "\n")
cat("✅ Chapter 6 bootstrap CIs ready (", round(total_time, 2), "min)\n", sep = "")
cat("   → m3_satisf_bootCI is in scope; merge it into m3_satisf_coefs in\n")
cat("     04_outputs_jobsatisf.R to add ci_low/ci_high columns to\n")
cat("     tbl-js-rs-coefs.\n")
cat(rep("=", 80), "\n\n")
