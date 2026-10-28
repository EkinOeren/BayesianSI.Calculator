# Synthetic example data for the bayesintervals documentation.
#
# Nothing here comes from a real study. A two-arm trial with a continuous
# marker is invented, the outcome data are simulated, and a Gibbs sampler is run
# to obtain posterior (MCMC) draws, the kind of file bayesintervals expects.
#
# Output (written next to this script):
#   example_repeated_measures_mcmc.csv   one chain, random patient effect, covariate
#   example_by_week_mcmc.csv             one chain per week stacked, column "Week"
#   simulated_trial_data.csv             the simulated observations (for reference)

set.seed(20261028)
out_dir <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) ".")
if (!nzchar(out_dir) || out_dir == ".") out_dir <- getwd()

# ---- Gibbs sampler for  y = X beta + Z u + e,  u_i ~ N(0, s2u), e ~ N(0, s2e) ----------
gibbs_lmm <- function(y, X, id = NULL, n_iter = 22000, burn = 2000, thin = 4,
                      prior_u = c(2, 2), prior_e = c(0.001, 0.001)) {
  n <- length(y); p <- ncol(X)
  has_u <- !is.null(id)
  if (has_u) { id <- as.integer(factor(id)); m <- max(id); n_i <- tabulate(id, m) }
  XtXi <- solve(crossprod(X)); L <- chol(XtXi)
  beta <- drop(XtXi %*% crossprod(X, y)); u <- if (has_u) rep(0, m) else NULL
  s2e <- var(y); s2u <- if (has_u) var(y) / 2 else NA
  keep <- seq(burn + thin, n_iter, by = thin)
  out <- matrix(NA_real_, length(keep), p + 1 + has_u,
                dimnames = list(NULL, c(colnames(X), if (has_u) "Random.Var", "Residual.Var")))
  k <- 0
  for (it in seq_len(n_iter)) {
    zu <- if (has_u) u[id] else 0
    beta_hat <- drop(XtXi %*% crossprod(X, y - zu))
    beta <- beta_hat + sqrt(s2e) * drop(t(L) %*% rnorm(p))        # beta | rest
    r <- y - drop(X %*% beta)
    if (has_u) {
      prec <- n_i / s2e + 1 / s2u
      u <- rnorm(m, tapply(r, id, sum) / s2e / prec, sqrt(1 / prec))  # u | rest
      s2u <- 1 / rgamma(1, prior_u[1] + m / 2, prior_u[2] + sum(u^2) / 2)
      r <- r - u[id]
    }
    s2e <- 1 / rgamma(1, prior_e[1] + n / 2, prior_e[2] + sum(r^2) / 2)
    if (it %in% keep) { k <- k + 1; out[k, ] <- c(beta, if (has_u) s2u, s2e) }
  }
  out
}

# ---- 1. Repeated measures: 48 patients, 4 visits, random patient intercept -----------
n_pat <- 48
pat <- data.frame(Patient = seq_len(n_pat),
                  Treatment = rep(c("A", "B"), each = n_pat / 2),
                  Baseline = round(rnorm(n_pat, 5, 1), 2))
trial <- merge(pat, data.frame(Visit = 1:4))
trial <- trial[order(trial$Patient, trial$Visit), ]
u_true <- rnorm(n_pat, 0, 0.9)
visit_eff <- c(0, 0.3, 0.5, 0.6)
trt_eff <- c(0, -0.2, -0.5, -0.8)                      # treatment B effect by visit
trial$Marker <- with(trial, 1.0 + 0.6 * Baseline + visit_eff[Visit] +
                       (Treatment == "B") * trt_eff[Visit] + u_true[Patient] + rnorm(nrow(trial), 0, 1.1))
trial$Marker <- round(trial$Marker, 3)

X <- model.matrix(~ Baseline + I(Treatment == "B") + factor(Visit) + I(Treatment == "B"):factor(Visit), trial)
colnames(X) <- c("Intercept", "Baseline", "TreatB", "Visit2", "Visit3", "Visit4",
                 "TreatB.Visit2", "TreatB.Visit3", "TreatB.Visit4")
draws1 <- gibbs_lmm(trial$Marker, X, id = trial$Patient)
d1 <- data.frame(Iteration = seq_len(nrow(draws1)), draws1)
d1[] <- lapply(d1, function(x) if (is.numeric(x)) round(x, 5) else x)
write.csv(d1, file.path(out_dir, "example_repeated_measures_mcmc.csv"), row.names = FALSE)
write.csv(trial, file.path(out_dir, "simulated_trial_data.csv"), row.names = FALSE)

# ---- 2. One analysis per week, stacked (By identifier = Week) ------------------------
weeks <- c(2, 4, 8, 12, 16, 24)
n_w <- 60
trt_by_week <- c(-0.2, -0.5, -0.9, -1.2, -1.3, -1.4)
icpt_by_week <- c(1.0, 0.8, 0.6, 0.5, 0.3, 0.2)
sd_by_week <- c(1.0, 1.0, 1.1, 1.2, 1.2, 1.3)
by_list <- lapply(seq_along(weeks), function(k) {
  dat <- data.frame(Baseline = round(rnorm(n_w, 5, 1), 2), TreatB = rep(c(0, 1), each = n_w / 2))
  dat$y <- icpt_by_week[k] + 0.6 * dat$Baseline + trt_by_week[k] * dat$TreatB + rnorm(n_w, 0, sd_by_week[k])
  Xk <- cbind(Intercept = 1, Baseline = dat$Baseline, TreatB = dat$TreatB)
  dr <- gibbs_lmm(dat$y, Xk, id = NULL, n_iter = 8000, burn = 1000, thin = 4)
  data.frame(Week = weeks[k], Iteration = seq_len(nrow(dr)), dr)
})
d2 <- do.call(rbind, by_list)
d2[] <- lapply(d2, function(x) if (is.numeric(x)) round(x, 5) else x)
write.csv(d2, file.path(out_dir, "example_by_week_mcmc.csv"), row.names = FALSE)

# ---- quick sanity checks ---------------------------------------------------------------
cat("repeated measures:", nrow(d1), "draws;  posterior means vs. truth\n")
tr <- c(Intercept = 1.0, Baseline = 0.6, TreatB = 0, Visit2 = 0.3, Visit3 = 0.5, Visit4 = 0.6,
        TreatB.Visit2 = -0.2, TreatB.Visit3 = -0.5, TreatB.Visit4 = -0.8, Random.Var = 0.81, Residual.Var = 1.21)
print(round(rbind(posterior_mean = colMeans(d1[names(tr)]), true_value = tr), 2))
cat("\nby week:", nrow(d2), "draws in", length(weeks), "groups\n")
print(aggregate(cbind(Intercept, Baseline, TreatB, Residual.Var) ~ Week, d2, mean), digits = 3)
