library(shiny)
library(ggplot2)

# ============================================================
#  DISTRIBUTION CATALOGUE
#  Each: label, F (CDF), F_inv (quantile), f (pdf),
#        support, cdf_expr, finv_expr, deriv_html,
#        c_code (how to derive F_inv), sample_code
# ============================================================
DISTS <- list(

  list(
    label   = "Exponential(λ)",
    f       = function(x, p) dexp(x, rate=p[1]),
    F       = function(x, p) pexp(x, rate=p[1]),
    F_inv   = function(u, p) -log(1-u) / p[1],
    support = c(0, Inf),
    params  = list(list(id="lam", label="Rate λ", min=0.1, max=5, val=1, step=0.1)),
    cdf_expr = "F(x) = 1 - e^(-λx),   x ≥ 0",
    finv_expr = "F⁻¹(u) = -ln(1-u) / λ",
    deriv_html = "
      <b>CDF:</b> F(x) = 1 - e<sup>-λx</sup><br><br>
      <b>Step 1.</b> Set F(x) = u:<br>
      &nbsp;&nbsp; 1 - e<sup>-λx</sup> = u<br><br>
      <b>Step 2.</b> Solve for x:<br>
      &nbsp;&nbsp; e<sup>-λx</sup> = 1 - u<br>
      &nbsp;&nbsp; -λx = ln(1 - u)<br>
      &nbsp;&nbsp; x = -ln(1-u) / λ<br><br>
      <b>Result:</b><br>
      &nbsp;&nbsp; F<sup>-1</sup>(u) = -ln(1-u) / λ<br><br>
      <b>Note:</b> Since U ~ Uniform(0,1), 1-U also ~ Uniform(0,1),
      so we often write F<sup>-1</sup>(u) = -ln(U) / λ.",
    c_code = '# Exponential(lambda) — derive F^{-1}
# CDF: F(x) = 1 - exp(-lambda * x)
# Set F(x) = u and solve for x:
#   1 - exp(-lambda*x) = u
#   exp(-lambda*x) = 1 - u
#   x = -log(1-u) / lambda

lambda <- 1   # rate parameter

F_inv <- function(u, lambda) -log(1 - u) / lambda

# Verify: F(F_inv(u)) should equal u
u_test <- 0.7
x_test <- F_inv(u_test, lambda)
cat("F_inv(0.7) =", x_test, "\n")
cat("F(F_inv(0.7)) =", pexp(x_test, lambda), "\n")  # should be 0.7',
    sample_code = '# Inverse Transform Sampler — Exponential(lambda)
set.seed(361)
lambda <- 1
n      <- 10000

U <- runif(n)                      # Step 1: generate U ~ Uniform(0,1)
X <- -log(1 - U) / lambda         # Step 2: apply F^{-1}

cat("Sample mean:", round(mean(X), 4),
    "  (theoretical:", 1/lambda, ")\n")
cat("Sample SD  :", round(sd(X),   4),
    "  (theoretical:", 1/lambda, ")\n")

hist(X, breaks=50, prob=TRUE, col="steelblue", border=NA,
     main="Exponential sample via Inverse Transform")
curve(dexp(x, lambda), add=TRUE, col="red", lwd=2)'
  ),

  list(
    label   = "Uniform(a, b)",
    f       = function(x, p) dunif(x, p[1], p[2]),
    F       = function(x, p) punif(x, p[1], p[2]),
    F_inv   = function(u, p) p[1] + (p[2]-p[1])*u,
    support = c(0, 1),
    params  = list(
      list(id="a", label="Lower a", min=-5, max=4,  val=0, step=0.5),
      list(id="b", label="Upper b", min=-4, max=5,  val=1, step=0.5)
    ),
    cdf_expr  = "F(x) = (x-a)/(b-a),   a ≤ x ≤ b",
    finv_expr = "F⁻¹(u) = a + (b-a)·u",
    deriv_html = "
      <b>CDF:</b> F(x) = (x - a)/(b - a)<br><br>
      <b>Step 1.</b> Set F(x) = u:<br>
      &nbsp;&nbsp; (x - a)/(b - a) = u<br><br>
      <b>Step 2.</b> Solve for x:<br>
      &nbsp;&nbsp; x - a = u(b - a)<br>
      &nbsp;&nbsp; x = a + (b-a)u<br><br>
      <b>Result:</b><br>
      &nbsp;&nbsp; F<sup>-1</sup>(u) = a + (b-a)·u<br><br>
      <b>Interpretation:</b> We are just rescaling and shifting
      a Uniform(0,1) sample to the interval [a, b].",
    c_code = '# Uniform(a, b) — derive F^{-1}
# CDF: F(x) = (x - a) / (b - a)
# Set F(x) = u:
#   (x-a)/(b-a) = u  =>  x = a + (b-a)*u

a <- 0;  b <- 3

F_inv <- function(u, a, b) a + (b - a) * u

u_test <- 0.6
cat("F_inv(0.6) =", F_inv(u_test, a, b), "\n")  # = 1.8
cat("F(1.8)     =", punif(1.8, a, b), "\n")      # = 0.6',
    sample_code = 'set.seed(361)
a <- 0;  b <- 3;  n <- 10000

U <- runif(n)
X <- a + (b - a) * U   # F^{-1}(U) — rescale to [a,b]

cat("Mean:", round(mean(X), 4), " (theoretical:", (a+b)/2, ")\n")

hist(X, breaks=40, prob=TRUE, col="steelblue", border=NA,
     main="Uniform(a,b) via Inverse Transform")
abline(h = 1/(b-a), col="red", lwd=2)'
  ),

  list(
    label   = "Weibull(shape k, scale λ)",
    f       = function(x, p) dweibull(x, shape=p[1], scale=p[2]),
    F       = function(x, p) pweibull(x, shape=p[1], scale=p[2]),
    F_inv   = function(u, p) p[2] * (-log(1-u))^(1/p[1]),
    support = c(0, Inf),
    params  = list(
      list(id="k",   label="Shape k",   min=0.5, max=5, val=2,   step=0.5),
      list(id="lam", label="Scale λ",   min=0.5, max=5, val=1,   step=0.5)
    ),
    cdf_expr  = "F(x) = 1 - exp(-(x/λ)^k),   x ≥ 0",
    finv_expr = "F⁻¹(u) = λ · (-ln(1-u))^(1/k)",
    deriv_html = "
      <b>CDF:</b> F(x) = 1 - exp(-(x/λ)<sup>k</sup>)<br><br>
      <b>Step 1.</b> Set F(x) = u:<br>
      &nbsp;&nbsp; 1 - exp(-(x/λ)<sup>k</sup>) = u<br><br>
      <b>Step 2.</b> Solve for x:<br>
      &nbsp;&nbsp; exp(-(x/λ)<sup>k</sup>) = 1 - u<br>
      &nbsp;&nbsp; -(x/λ)<sup>k</sup> = ln(1-u)<br>
      &nbsp;&nbsp; (x/λ)<sup>k</sup> = -ln(1-u)<br>
      &nbsp;&nbsp; x/λ = (-ln(1-u))<sup>1/k</sup><br>
      &nbsp;&nbsp; x = λ·(-ln(1-u))<sup>1/k</sup><br><br>
      <b>Result:</b><br>
      &nbsp;&nbsp; F<sup>-1</sup>(u) = λ·(-ln(1-u))<sup>1/k</sup>",
    c_code = '# Weibull(k, lambda) — derive F^{-1}
# CDF: F(x) = 1 - exp(-(x/lambda)^k)
# Set u = F(x):
#   1 - exp(-(x/lambda)^k) = u
#   (x/lambda)^k = -log(1-u)
#   x = lambda * (-log(1-u))^(1/k)

k <- 2;  lambda <- 1

F_inv <- function(u, k, lambda) lambda * (-log(1 - u))^(1/k)

u_test <- 0.5
cat("F_inv(0.5)  =", round(F_inv(u_test, k, lambda), 6), "\n")
cat("Check F(x)  =", round(pweibull(F_inv(u_test,k,lambda), k, lambda), 6), "\n")',
    sample_code = 'set.seed(361)
k <- 2;  lambda <- 1;  n <- 10000

U <- runif(n)
X <- lambda * (-log(1 - U))^(1/k)   # F^{-1}(U)

cat("Mean:", round(mean(X), 4),
    "  (theoretical:", lambda * gamma(1 + 1/k), ")\n")

hist(X, breaks=50, prob=TRUE, col="steelblue", border=NA,
     main="Weibull via Inverse Transform")
curve(dweibull(x, k, lambda), add=TRUE, col="red", lwd=2)'
  ),

  list(
    label   = "Logistic(μ, s)",
    f       = function(x, p) dlogis(x, location=p[1], scale=p[2]),
    F       = function(x, p) plogis(x, location=p[1], scale=p[2]),
    F_inv   = function(u, p) p[1] + p[2]*log(u/(1-u)),
    support = c(-Inf, Inf),
    params  = list(
      list(id="mu", label="Location μ", min=-3, max=3, val=0,   step=0.5),
      list(id="s",  label="Scale s",    min=0.2, max=3, val=1,  step=0.2)
    ),
    cdf_expr  = "F(x) = 1 / (1 + e^(-(x-μ)/s))",
    finv_expr = "F⁻¹(u) = μ + s·ln(u/(1-u))",
    deriv_html = "
      <b>CDF:</b> F(x) = 1 / (1 + e<sup>-(x-μ)/s</sup>) = σ((x-μ)/s)<br><br>
      <b>Step 1.</b> Set F(x) = u:<br>
      &nbsp;&nbsp; 1 / (1 + e<sup>-(x-μ)/s</sup>) = u<br><br>
      <b>Step 2.</b> Solve for x:<br>
      &nbsp;&nbsp; 1 + e<sup>-(x-μ)/s</sup> = 1/u<br>
      &nbsp;&nbsp; e<sup>-(x-μ)/s</sup> = 1/u - 1 = (1-u)/u<br>
      &nbsp;&nbsp; -(x-μ)/s = ln((1-u)/u)<br>
      &nbsp;&nbsp; x = μ + s·ln(u/(1-u))<br><br>
      <b>Result:</b><br>
      &nbsp;&nbsp; F<sup>-1</sup>(u) = μ + s·ln(u/(1-u))<br><br>
      The function ln(u/(1-u)) is called the <b>logit</b> function.",
    c_code = '# Logistic(mu, s) — derive F^{-1}
# CDF: F(x) = 1/(1 + exp(-(x-mu)/s))
# Set u = F(x) and solve:
#   u(1 + exp(-(x-mu)/s)) = 1
#   exp(-(x-mu)/s) = (1-u)/u
#   -(x-mu)/s = log((1-u)/u)
#   x = mu + s*log(u/(1-u))   [the logit transform]

mu <- 0;  s <- 1

F_inv <- function(u, mu, s) mu + s * log(u / (1 - u))

u_test <- 0.8
cat("F_inv(0.8) =", round(F_inv(u_test, mu, s), 6), "\n")
cat("Check      =", round(plogis(F_inv(u_test,mu,s), mu, s), 6), "\n")',
    sample_code = 'set.seed(361)
mu <- 0;  s <- 1;  n <- 10000

U <- runif(n)
X <- mu + s * log(U / (1 - U))   # logit transform = F^{-1}(U)

cat("Mean:", round(mean(X), 4), "  (theoretical:", mu, ")\n")
cat("SD  :", round(sd(X),   4), "  (theoretical:", s*pi/sqrt(3), ")\n")

hist(X, breaks=60, prob=TRUE, col="steelblue", border=NA,
     main="Logistic via Inverse Transform", xlim=c(-6,6))
curve(dlogis(x, mu, s), add=TRUE, col="red", lwd=2)'
  ),

  list(
    label   = "Cauchy(x₀, γ)",
    f       = function(x, p) dcauchy(x, location=p[1], scale=p[2]),
    F       = function(x, p) pcauchy(x, location=p[1], scale=p[2]),
    F_inv   = function(u, p) p[1] + p[2]*tan(pi*(u-0.5)),
    support = c(-Inf, Inf),
    params  = list(
      list(id="x0",  label="Location x₀", min=-3, max=3, val=0,   step=0.5),
      list(id="gam", label="Scale γ",     min=0.2, max=3, val=1,  step=0.2)
    ),
    cdf_expr  = "F(x) = 1/π · arctan((x-x₀)/γ) + 1/2",
    finv_expr = "F⁻¹(u) = x₀ + γ·tan(π(u - 1/2))",
    deriv_html = "
      <b>CDF:</b> F(x) = (1/π)·arctan((x-x₀)/γ) + 1/2<br><br>
      <b>Step 1.</b> Set F(x) = u:<br>
      &nbsp;&nbsp; (1/π)·arctan((x-x₀)/γ) + 1/2 = u<br><br>
      <b>Step 2.</b> Solve for x:<br>
      &nbsp;&nbsp; arctan((x-x₀)/γ) = π(u - 1/2)<br>
      &nbsp;&nbsp; (x-x₀)/γ = tan(π(u - 1/2))<br>
      &nbsp;&nbsp; x = x₀ + γ·tan(π(u - 1/2))<br><br>
      <b>Result:</b><br>
      &nbsp;&nbsp; F<sup>-1</sup>(u) = x₀ + γ·tan(π(u - 1/2))",
    c_code = '# Cauchy(x0, gamma) — derive F^{-1}
# CDF: F(x) = (1/pi)*atan((x-x0)/gamma) + 0.5
# Set u = F(x):
#   atan((x-x0)/gamma) = pi*(u - 0.5)
#   (x-x0)/gamma = tan(pi*(u - 0.5))
#   x = x0 + gamma*tan(pi*(u - 0.5))

x0 <- 0;  gamma <- 1

F_inv <- function(u, x0, gamma) x0 + gamma * tan(pi * (u - 0.5))

u_test <- 0.75
cat("F_inv(0.75) =", round(F_inv(u_test, x0, gamma), 6), "\n")
cat("Check       =", round(pcauchy(F_inv(u_test,x0,gamma), x0, gamma), 6), "\n")',
    sample_code = 'set.seed(361)
x0 <- 0;  gamma <- 1;  n <- 10000

U <- runif(n)
X <- x0 + gamma * tan(pi * (U - 0.5))   # F^{-1}(U)

# Note: Cauchy has no finite mean or variance!
cat("Median:", round(median(X), 4), "  (theoretical:", x0, ")\n")

hist(X[abs(X) < 20], breaks=80, prob=TRUE,
     col="steelblue", border=NA,
     main="Cauchy via Inverse Transform  (|X|<20 shown)")
curve(dcauchy(x, x0, gamma), add=TRUE, col="red", lwd=2)'
  ),

  list(
    label   = "Pareto(α, x_m)",
    f       = function(x, p) {
      ifelse(x >= p[2], p[1]*p[2]^p[1] / x^(p[1]+1), 0)
    },
    F       = function(x, p) ifelse(x >= p[2], 1-(p[2]/x)^p[1], 0),
    F_inv   = function(u, p) p[2] / (1-u)^(1/p[1]),
    support = c(1, Inf),
    params  = list(
      list(id="al", label="Shape α",    min=1, max=5, val=2, step=0.5),
      list(id="xm", label="Min value x_m", min=1, max=3, val=1, step=0.5)
    ),
    cdf_expr  = "F(x) = 1 - (x_m/x)^α,   x ≥ x_m",
    finv_expr = "F⁻¹(u) = x_m / (1-u)^(1/α)",
    deriv_html = "
      <b>CDF:</b> F(x) = 1 - (x<sub>m</sub>/x)<sup>α</sup>,  x ≥ x<sub>m</sub><br><br>
      <b>Step 1.</b> Set F(x) = u:<br>
      &nbsp;&nbsp; 1 - (x<sub>m</sub>/x)<sup>α</sup> = u<br><br>
      <b>Step 2.</b> Solve for x:<br>
      &nbsp;&nbsp; (x<sub>m</sub>/x)<sup>α</sup> = 1 - u<br>
      &nbsp;&nbsp; x<sub>m</sub>/x = (1-u)<sup>1/α</sup><br>
      &nbsp;&nbsp; x = x<sub>m</sub> / (1-u)<sup>1/α</sup><br><br>
      <b>Result:</b><br>
      &nbsp;&nbsp; F<sup>-1</sup>(u) = x<sub>m</sub> / (1-u)<sup>1/α</sup>",
    c_code = '# Pareto(alpha, x_m) — derive F^{-1}
# CDF: F(x) = 1 - (x_m/x)^alpha,  x >= x_m
# Set u = F(x):
#   (x_m/x)^alpha = 1 - u
#   x_m/x = (1-u)^(1/alpha)
#   x = x_m / (1-u)^(1/alpha)

alpha <- 2;  x_m <- 1

F_inv <- function(u, alpha, x_m) x_m / (1 - u)^(1/alpha)

u_test <- 0.5
cat("F_inv(0.5) =", round(F_inv(u_test, alpha, x_m), 6), "\n")
cat("Check      =", round(1-(x_m/F_inv(u_test,alpha,x_m))^alpha, 6), "\n")',
    sample_code = 'set.seed(361)
alpha <- 2;  x_m <- 1;  n <- 10000

U <- runif(n)
X <- x_m / (1 - U)^(1/alpha)   # F^{-1}(U)

cat("Mean:", round(mean(X), 4),
    "  (theoretical:", alpha*x_m/(alpha-1), ")\n")

hist(X[X < 10], breaks=60, prob=TRUE,
     col="steelblue", border=NA,
     main="Pareto via Inverse Transform  (X<10 shown)")
curve(ifelse(x>=x_m, alpha*x_m^alpha/x^(alpha+1), 0),
      add=TRUE, col="red", lwd=2)'
  ),

  list(
    label   = "Discrete: Geometric(p)",
    f       = function(x, p) dgeom(x-1, prob=p[1]),
    F       = function(x, p) pgeom(floor(x)-1, prob=p[1]),
    F_inv   = function(u, p) ceiling(log(1-u)/log(1-p[1])),
    support = c(1, Inf),
    params  = list(
      list(id="p", label="Success prob p", min=0.05, max=0.95, val=0.3, step=0.05)
    ),
    cdf_expr  = "F(k) = 1 - (1-p)^k,   k = 1,2,3,...",
    finv_expr = "F⁻¹(u) = ⌈ln(1-u) / ln(1-p)⌉",
    deriv_html = "
      <b>PMF:</b> P(X=k) = p(1-p)<sup>k-1</sup>, k = 1,2,...<br>
      <b>CDF:</b> F(k) = 1-(1-p)<sup>k</sup><br><br>
      <b>Discrete Inverse Transform:</b><br>
      Find the smallest k such that F(k) ≥ u:<br>
      &nbsp;&nbsp; 1-(1-p)<sup>k</sup> ≥ u<br>
      &nbsp;&nbsp; (1-p)<sup>k</sup> ≤ 1-u<br>
      &nbsp;&nbsp; k·ln(1-p) ≤ ln(1-u)   [ln(1-p) < 0, flip inequality]<br>
      &nbsp;&nbsp; k ≥ ln(1-u) / ln(1-p)<br><br>
      <b>Result:</b><br>
      &nbsp;&nbsp; F<sup>-1</sup>(u) = ⌈ln(1-u) / ln(1-p)⌉<br><br>
      ⌈·⌉ denotes the ceiling function (round up to nearest integer).",
    c_code = '# Geometric(p): P(X=k) = p*(1-p)^(k-1), k=1,2,...
# CDF: F(k) = 1 - (1-p)^k
# Inverse: smallest k with F(k) >= u
#   1-(1-p)^k >= u  =>  (1-p)^k <= 1-u
#   k >= log(1-u)/log(1-p)  [log(1-p)<0 flips inequality]
#   k = ceiling(log(1-u)/log(1-p))

prob <- 0.3

F_inv <- function(u, prob) ceiling(log(1-u) / log(1-prob))

u_test <- 0.7
cat("F_inv(0.7) =", F_inv(u_test, prob), "\n")
cat("Check CDF  =", 1-(1-prob)^F_inv(u_test, prob), ">= 0.7?", "\n")',
    sample_code = 'set.seed(361)
prob <- 0.3;  n <- 10000

U <- runif(n)
X <- ceiling(log(1 - U) / log(1 - prob))   # F^{-1}(U)

cat("Mean:", round(mean(X), 4),
    "  (theoretical:", 1/prob, ")\n")
cat("Var :", round(var(X),  4),
    "  (theoretical:", (1-prob)/prob^2, ")\n")

barplot(table(X[X<=15])/n, col="steelblue",
        main="Geometric sample via Inverse Transform")'
  ),

  list(
    label   = "Discrete: Poisson(λ)",
    f       = function(x, p) dpois(round(x), lambda=p[1]),
    F       = function(x, p) ppois(floor(x), lambda=p[1]),
    F_inv   = function(u, p) qpois(u, lambda=p[1]),
    support = c(0, Inf),
    params  = list(
      list(id="lam", label="Mean λ", min=0.5, max=10, val=3, step=0.5)
    ),
    cdf_expr  = "F(k) = e^(-λ) Σ λʲ/j!,   k = 0,1,2,...",
    finv_expr = "F⁻¹(u) = min{k : F(k) ≥ u}",
    deriv_html = "
      <b>PMF:</b> P(X=k) = e<sup>-λ</sup>λ<sup>k</sup>/k!<br>
      <b>CDF:</b> F(k) = e<sup>-λ</sup> Σ<sub>j=0</sub><sup>k</sup> λ<sup>j</sup>/j!<br><br>
      <b>Discrete Inverse Transform:</b><br>
      Unlike the continuous case, there is no closed-form F<sup>-1</sup>.<br>
      We find the smallest k such that F(k) ≥ u:<br><br>
      <b>Algorithm:</b><br>
      &nbsp;&nbsp; k = 0;  p₀ = e<sup>-λ</sup>;  F = p₀<br>
      &nbsp;&nbsp; while F < u: k = k+1; F = F + P(X=k)<br>
      &nbsp;&nbsp; return k<br><br>
      In R: <code>qpois(u, lambda)</code> does exactly this.<br><br>
      <b>Key idea:</b> For any discrete distribution, walking up the CDF
      until it exceeds u gives the correct inverse.",
    c_code = '# Poisson(lambda) — no closed-form F^{-1}
# Use sequential search (walk up the CDF)

lambda <- 3

# Method 1: use R built-in
F_inv_builtin <- function(u) qpois(u, lambda)

# Method 2: manual sequential search (educational)
F_inv_manual <- function(u, lambda) {
  k  <- 0
  Fk <- exp(-lambda)   # P(X=0)
  pk <- exp(-lambda)
  while (Fk < u) {
    k  <- k + 1
    pk <- pk * lambda / k   # P(X=k) = P(X=k-1)*lambda/k
    Fk <- Fk + pk
  }
  k
}

u_test <- 0.8
cat("qpois :", F_inv_builtin(u_test), "\n")
cat("manual:", F_inv_manual(u_test, lambda), "\n")',
    sample_code = 'set.seed(361)
lambda <- 3;  n <- 10000

U <- runif(n)
X <- qpois(U, lambda)   # F^{-1}(U) via built-in quantile function

cat("Mean:", round(mean(X), 4), "  (theoretical:", lambda, ")\n")
cat("Var :", round(var(X),  4), "  (theoretical:", lambda, ")\n")

barplot(table(X[X<=12])/n, col="steelblue",
        main="Poisson sample via Inverse Transform")'
  )
)

# ============================================================
#  HELPERS
# ============================================================

# safe support bounds for plotting
plot_support <- function(d, pv) {
  lo <- d$support[1]
  hi <- d$support[2]
  if (!is.finite(lo)) lo <- qfn(d, pv, 0.001)
  if (!is.finite(hi)) hi <- qfn(d, pv, 0.999)
  c(lo, hi)
}
qfn <- function(d, pv, p) d$F_inv(p, pv)

# draw samples via inverse transform
itm_sample <- function(d, pv, n, seed) {
  set.seed(seed)
  u <- runif(n)
  x <- tryCatch(d$F_inv(u, pv), error=function(e) rep(NA, n))
  x[is.finite(x)]
}

dark_theme <- function(base=12) {
  theme_minimal(base_size=base) +
    theme(
      plot.background  = element_rect(fill="#111318", colour=NA),
      panel.background = element_rect(fill="#111318", colour=NA),
      panel.grid.major = element_line(colour="#1d2030"),
      panel.grid.minor = element_blank(),
      axis.text        = element_text(colour="#6b7190"),
      axis.title       = element_text(colour="#9099b8"),
      legend.background= element_rect(fill="#111318", colour=NA),
      legend.text      = element_text(colour="#9099b8"),
      plot.title       = element_text(colour="#c8cce8", size=base, face="bold"),
      plot.subtitle    = element_text(colour="#6b7190", size=base-1)
    )
}

# ============================================================
#  UI
# ============================================================
ui <- fluidPage(

  tags$head(tags$style(HTML('
    @import url("https://fonts.googleapis.com/css2?family=Libre+Baskerville:wght@400;700&family=JetBrains+Mono:wght@400;500&family=Outfit:wght@300;400;500;600&display=swap");

    *, *::before, *::after { box-sizing: border-box; margin: 0; padding: 0; }

    body {
      background: #0b0d13;
      color: #c8cce8;
      font-family: "Outfit", sans-serif;
      font-size: 14px;
      min-height: 100vh;
    }

    /* ── Header ── */
    .app-header {
      background: linear-gradient(160deg, #0f1520 0%, #0b0d13 60%);
      border-bottom: 1px solid #1a1e30;
      padding: 22px 32px 18px;
      margin-bottom: 22px;
      position: relative;
      overflow: hidden;
    }
    .app-header::before {
      content: "";
      position: absolute; top: 0; right: 0;
      width: 320px; height: 100%;
      background: radial-gradient(ellipse at 80% 50%, rgba(99,102,241,.08) 0%, transparent 70%);
      pointer-events: none;
    }
    .app-header h1 {
      font-family: "Libre Baskerville", serif;
      font-size: 28px; font-weight: 700;
      color: #e8eaf8; letter-spacing: -0.5px;
      margin-bottom: 5px;
    }
    .app-header .subtitle {
      color: #6b7190; font-size: 13.5px; font-weight: 300;
      margin-bottom: 10px;
    }
    .chip {
      display: inline-block;
      background: rgba(99,102,241,.12);
      border: 1px solid rgba(99,102,241,.25);
      color: #8b8ff4; border-radius: 20px;
      padding: 3px 10px; font-size: 11.5px;
      font-weight: 500; margin: 2px 3px 2px 0;
    }

    /* ── Sidebar ── */
    .sidebar {
      background: #0f1118;
      border: 1px solid #1a1e30;
      border-radius: 10px;
      padding: 18px;
    }
    .sidebar h5 {
      font-family: "Libre Baskerville", serif;
      font-size: 14px; color: #c0c4e0;
      margin-bottom: 14px; padding-bottom: 8px;
      border-bottom: 1px solid #1a1e30;
    }
    .sidebar label {
      color: #6b7190 !important;
      font-size: 11.5px !important;
      font-weight: 500 !important;
      text-transform: uppercase !important;
      letter-spacing: .6px !important;
      display: block !important;
      margin-bottom: 4px !important;
    }
    .sidebar select, .sidebar input[type=number] {
      background: #0b0d13 !important;
      border: 1px solid #1a1e30 !important;
      color: #c8cce8 !important;
      border-radius: 6px !important;
      padding: 7px 10px !important;
      font-size: 13px !important;
      width: 100% !important;
      margin-bottom: 12px !important;
    }
    .sidebar .irs--shiny .irs-bar { background: #6366f1 !important; }
    .sidebar .irs--shiny .irs-handle { background: #6366f1 !important; border-color: #6366f1 !important; }
    .sidebar .irs--shiny .irs-single { background: #6366f1 !important; }
    .sidebar hr { border-color: #1a1e30; margin: 14px 0; }

    .run-btn {
      background: linear-gradient(135deg, #4f46e5, #6366f1) !important;
      border: none !important;
      color: white !important;
      font-weight: 600 !important;
      font-size: 13px !important;
      letter-spacing: .4px !important;
      border-radius: 7px !important;
      width: 100% !important;
      padding: 10px 0 !important;
      cursor: pointer !important;
      transition: opacity .2s !important;
    }
    .run-btn:hover { opacity: .88 !important; }

    /* ── Stats grid ── */
    .stats-grid {
      display: grid;
      grid-template-columns: 1fr 1fr;
      gap: 8px;
      margin-top: 14px;
    }
    .stat-cell {
      background: #0f1118;
      border: 1px solid #1a1e30;
      border-radius: 7px;
      padding: 9px 12px;
      text-align: center;
    }
    .stat-cell .sv {
      font-size: 16px; font-weight: 600;
      color: #8b8ff4; display: block;
    }
    .stat-cell .sl {
      font-size: 10px; color: #4a4f6a;
      text-transform: uppercase; letter-spacing: .5px;
      margin-top: 2px; display: block;
    }

    /* ── Tabs ── */
    .nav-tabs {
      border-bottom: 1px solid #1a1e30 !important;
      margin-bottom: 18px !important;
    }
    .nav-tabs > li > a {
      background: transparent !important;
      border: none !important;
      color: #4a4f6a !important;
      font-size: 13px !important;
      font-weight: 500 !important;
      padding: 9px 18px !important;
      border-bottom: 2px solid transparent !important;
      border-radius: 0 !important;
    }
    .nav-tabs > li.active > a {
      color: #8b8ff4 !important;
      border-bottom-color: #6366f1 !important;
      background: transparent !important;
    }
    .nav-tabs > li > a:hover { color: #c0c4e0 !important; }

    /* ── Cards ── */
    .card {
      background: #0f1118;
      border: 1px solid #1a1e30;
      border-radius: 10px;
      padding: 18px;
      margin-bottom: 16px;
    }
    .card h4 {
      font-family: "Libre Baskerville", serif;
      font-size: 14px; font-weight: 700;
      color: #c0c4e0;
      margin-bottom: 14px;
      padding-bottom: 8px;
      border-bottom: 1px solid #1a1e30;
    }

    /* ── Formula / highlight ── */
    .fml {
      background: #13172a;
      border: 1px solid #252a45;
      border-left: 3px solid #6366f1;
      border-radius: 0 6px 6px 0;
      padding: 9px 14px;
      font-family: "JetBrains Mono", monospace;
      font-size: 12.5px;
      color: #b0b4d8;
      display: block;
      margin: 8px 0;
    }

    /* ── Derivation ── */
    .deriv-box {
      background: #0b0d13;
      border-left: 3px solid #6366f1;
      border-radius: 0 7px 7px 0;
      padding: 16px 18px;
      font-size: 13.5px;
      line-height: 2;
      color: #b8bcd8;
    }

    /* ── Code ── */
    .code-box {
      background: #080a10;
      border: 1px solid #1a1e30;
      border-radius: 7px;
      padding: 16px 18px;
      font-family: "JetBrains Mono", monospace;
      font-size: 12px;
      line-height: 1.8;
      color: #9099b8;
      white-space: pre;
      overflow-x: auto;
    }

    /* ── Alerts ── */
    .info-box {
      background: #101428;
      border: 1px solid #1e2545;
      border-left: 3px solid #6366f1;
      border-radius: 0 7px 7px 0;
      padding: 11px 16px;
      font-size: 13px;
      color: #b0b4d8;
      margin-bottom: 14px;
      line-height: 1.6;
    }
    .warn-box {
      background: #1a1408;
      border: 1px solid #352a10;
      border-left: 3px solid #f59e0b;
      border-radius: 0 7px 7px 0;
      padding: 11px 16px;
      font-size: 13px;
      color: #c8a860;
      margin-bottom: 14px;
    }
    .ok-box {
      background: #0a1a10;
      border: 1px solid #1a3520;
      border-left: 3px solid #22c55e;
      border-radius: 0 7px 7px 0;
      padding: 11px 16px;
      font-size: 13px;
      color: #7ddb9e;
      margin-bottom: 14px;
    }

    /* ── Steps ── */
    .step {
      display: flex; gap: 14px;
      align-items: flex-start;
      margin-bottom: 16px;
    }
    .step-badge {
      background: #6366f1;
      color: white; border-radius: 50%;
      width: 28px; height: 28px; min-width: 28px;
      display: flex; align-items: center; justify-content: center;
      font-size: 12px; font-weight: 700;
    }
    .step-body { line-height: 1.7; color: #b8bcd8; }
    .step-body b { color: #8b8ff4; }

    /* ── Quiz ── */
    .fb-ok  { color: #22c55e; font-weight: 600; font-size: 13px; margin-top: 6px; }
    .fb-no  { color: #ef4444; font-weight: 600; font-size: 13px; margin-top: 6px; }

    /* ── scrollbar ── */
    ::-webkit-scrollbar { width: 5px; height: 5px; }
    ::-webkit-scrollbar-track { background: #0b0d13; }
    ::-webkit-scrollbar-thumb { background: #1a1e30; border-radius: 3px; }
    .shiny-plot-output { border-radius: 7px; overflow: hidden; }
  '))),

  # ── HEADER ──────────────────────────────────────────────────────────────────
  div(class = "app-header",
    h1("Inverse Transform Method"),
    div(class = "subtitle",
      "Generate random samples from any distribution by inverting the CDF"
    ),
    span(class="chip", "📐 CDF Inversion"),
    span(class="chip", "💻 R Code"),
    span(class="chip", "📊 Visualise"),
    span(class="chip", "🔁 CDF ↔ Quantile"),
    span(class="chip", "🎓 Quiz")
  ),

  fluidRow(

    # ── SIDEBAR ───────────────────────────────────────────────────────────────
    column(3,
      div(class = "sidebar",
        h5("Distribution"),
        tags$label("Select"),
        selectInput("dist_idx", NULL,
          choices = setNames(as.character(seq_along(DISTS)),
                             sapply(DISTS, `[[`, "label")),
          selected = "1"
        ),
        uiOutput("param_sliders"),

        tags$hr(),
        h5("Sampling"),
        tags$label("Sample size n"),
        sliderInput("n_samp", NULL, 200, 20000, 5000, 200),
        tags$label("Random seed"),
        numericInput("seed_val", NULL, 361, 1, 99999, 1),
        tags$br(),
        actionButton("go_btn", "▶  Run Sampling", class="run-btn"),

        tags$hr(),
        h5("Results"),
        uiOutput("stats_ui")
      )
    ),

    # ── MAIN ──────────────────────────────────────────────────────────────────
    column(9,
      tabsetPanel(id="main_tabs",

        # ── TAB 1: ALGORITHM ──────────────────────────────────────────────────
        tabPanel("📖 Algorithm",
          tags$br(),
          div(class="info-box",
            "The Inverse Transform Method (ITM) is the most fundamental technique
             for random variate generation. It works for any distribution with an
             invertible CDF — continuous or discrete."
          ),
          fluidRow(
            column(6,
              div(class="card",
                h4("The Core Theorem"),
                div(class="step",
                  div(class="step-badge","T"),
                  div(class="step-body",
                    tags$b("Probability Integral Transform"), tags$br(),
                    "If X has CDF F(x), then F(X) ~ Uniform(0,1).", tags$br(),
                    "Conversely, if U ~ Uniform(0,1), then X = F⁻¹(U) has CDF F."
                  )
                ),
                tags$span(class="fml",
                  "X = F⁻¹(U),   U ~ Uniform(0,1)"
                ),
                tags$p(style="color:#6b7190; font-size:13px; margin-top:10px;",
                  "This works because P(F⁻¹(U) ≤ x) = P(U ≤ F(x)) = F(x). ✓"
                )
              ),
              div(class="card",
                h4("Why It Works — Proof"),
                div(class="step-body",
                  "Let U ~ Uniform(0,1) and X = F⁻¹(U). Then:", tags$br(), tags$br(),
                  tags$span(class="fml","P(X ≤ x) = P(F⁻¹(U) ≤ x)"),
                  tags$span(class="fml","= P(U ≤ F(x))   [since F is non-decreasing]"),
                  tags$span(class="fml","= F(x)   [since U ~ Uniform(0,1)]   ✓")
                )
              )
            ),
            column(6,
              div(class="card",
                h4("The 3-Step Algorithm"),
                div(class="step",
                  div(class="step-badge","1"),
                  div(class="step-body",
                    tags$b("Find the CDF F(x)"), tags$br(),
                    "Integrate the PDF: F(x) = ∫ f(t) dt"
                  )
                ),
                div(class="step",
                  div(class="step-badge","2"),
                  div(class="step-body",
                    tags$b("Derive the Quantile Function F⁻¹(u)"), tags$br(),
                    "Set F(x) = u and solve for x algebraically."
                  )
                ),
                div(class="step",
                  div(class="step-badge","3"),
                  div(class="step-body",
                    tags$b("Generate samples"), tags$br(),
                    "Draw U ~ Uniform(0,1), compute X = F⁻¹(U)."
                  )
                ),
                div(class="ok-box", style="margin-top:8px;",
                  "✅ Produces exact samples — no approximation, no rejection!"
                )
              ),
              div(class="card",
                h4("Continuous vs Discrete"),
                div(class="step-body",
                  tags$b("Continuous:"), " solve F(x) = u analytically.", tags$br(), tags$br(),
                  tags$b("Discrete:"), " find the smallest k with F(k) ≥ u:", tags$br(),
                  tags$span(class="fml","k = min{j : F(j) ≥ u}"),
                  "Can be found by walking up the PMF or using", tags$code("qXXX()"), "in R."
                )
              )
            )
          )
        ),

        # ── TAB 2: DERIVE F⁻¹ ────────────────────────────────────────────────
        tabPanel("📐 Derive F⁻¹",
          tags$br(),
          fluidRow(
            column(6,
              div(class="card",
                h4("Step-by-Step Derivation"),
                div(class="deriv-box", uiOutput("deriv_ui"))
              ),
              div(class="card",
                h4("Key Formulas"),
                uiOutput("formulas_ui")
              )
            ),
            column(6,
              div(class="card",
                h4("CDF Plot — F(x)"),
                plotOutput("cdf_plot", height="260px")
              ),
              div(class="card",
                h4("Quantile Function — F⁻¹(u)"),
                plotOutput("quantile_plot", height="220px")
              )
            )
          )
        ),

        # ── TAB 3: VISUALISE ──────────────────────────────────────────────────
        tabPanel("📊 Visualise",
          tags$br(),
          div(class="info-box", "Click ▶ Run Sampling first."),
          fluidRow(
            column(6,
              div(class="card",
                h4("Sample Histogram vs f(x)"),
                plotOutput("hist_plot", height="280px")
              )
            ),
            column(6,
              div(class="card",
                h4("Empirical CDF vs Theoretical CDF"),
                plotOutput("ecdf_plot", height="280px")
              )
            )
          ),
          fluidRow(
            column(6,
              div(class="card",
                h4("U → X Transformation  (first 500 points)"),
                plotOutput("transform_plot", height="260px")
              )
            ),
            column(6,
              div(class="card",
                h4("Q-Q Plot: Sample vs Theoretical"),
                plotOutput("qq_plot", height="260px")
              )
            )
          )
        ),

        # ── TAB 4: CDF ↔ QUANTILE INTERACTIVE ────────────────────────────────
        tabPanel("🔁 CDF ↔ Quantile",
          tags$br(),
          div(class="info-box",
            "Move the slider to pick a value of u ∈ (0,1).
             Watch how F⁻¹(u) is read off the CDF curve, and how
             the same u value maps onto the sample histogram."
          ),
          fluidRow(
            column(4,
              div(class="card",
                h4("Pick u"),
                sliderInput("u_val", NULL, 0.01, 0.99, 0.5, 0.01),
                uiOutput("u_readout")
              )
            ),
            column(8,
              div(class="card",
                h4("Reading F⁻¹(u) from the CDF"),
                plotOutput("cdf_annotated", height="320px")
              )
            )
          )
        ),

        # ── TAB 5: R CODE ──────────────────────────────────────────────────────
        tabPanel("💻 R Code",
          tags$br(),
          div(class="card",
            h4("Step 1 — Derive F⁻¹(u) analytically"),
            div(class="code-box", textOutput("ccode_out"))
          ),
          div(class="card",
            h4("Step 2 — Sample using F⁻¹"),
            div(class="code-box", textOutput("scode_out"))
          ),
          div(class="card",
            h4("Generic ITM template (works for any distribution)"),
            div(class="code-box",
'# Generic Inverse Transform Method template
# ─────────────────────────────────────────
# 1. Define F_inv (quantile function)
F_inv <- function(u) {
  # ... your derivation here ...
}

# 2. Generate samples
set.seed(361)
n <- 10000
U <- runif(n)        # Uniform(0,1) samples
X <- F_inv(U)        # Apply quantile function

# 3. Validate
hist(X, prob=TRUE, breaks=50, col="steelblue", border=NA)
curve(f(x), add=TRUE, col="red", lwd=2)  # overlay true density

# Check moments
cat("Mean:", mean(X), "\n")
cat("SD  :", sd(X),   "\n")')
          )
        ),

        # ── TAB 6: QUIZ ────────────────────────────────────────────────────────
        tabPanel("🎓 Quiz",
          tags$br(),
          div(class="card",
            h4("Q1 — What is the key theorem behind the Inverse Transform Method?"),
            radioButtons("q1", NULL, selected=character(0),
              choiceNames = list(
                "If U ~ Uniform(0,1), then X = F⁻¹(U) has CDF F",
                "If X has CDF F, then F⁻¹(X) is uniform",
                "The CDF must be differentiable",
                "It only works for exponential distributions"
              ),
              choiceValues = list("a","b","c","d")
            ),
            uiOutput("q1fb")
          ),
          div(class="card",
            h4("Q2 — For Exponential(λ), what is F⁻¹(u)?"),
            radioButtons("q2", NULL, selected=character(0),
              choiceNames = list(
                "-ln(u) / λ",
                "-ln(1-u) / λ",
                "1 - exp(-λu)",
                "λ · ln(u)"
              ),
              choiceValues = list("a","b","c","d")
            ),
            uiOutput("q2fb")
          ),
          div(class="card",
            h4("Q3 — For a discrete distribution, how do we apply the ITM?"),
            radioButtons("q3", NULL, selected=character(0),
              choiceNames = list(
                "Solve F(k) = u algebraically",
                "Find the smallest k such that F(k) ≥ u",
                "Generate U and round to nearest integer",
                "It cannot be applied to discrete distributions"
              ),
              choiceValues = list("a","b","c","d")
            ),
            uiOutput("q3fb")
          ),
          div(class="card",
            h4("Q4 — What is the main advantage of ITM over Acceptance-Rejection?"),
            radioButtons("q4", NULL, selected=character(0),
              choiceNames = list(
                "ITM always works regardless of the distribution",
                "ITM produces exact samples with no wasted uniform draws",
                "ITM is faster in all cases",
                "ITM does not require a CDF"
              ),
              choiceValues = list("a","b","c","d")
            ),
            uiOutput("q4fb")
          ),
          div(class="card",
            h4("Q5 — Why does P(F⁻¹(U) ≤ x) = F(x)?"),
            radioButtons("q5", NULL, selected=character(0),
              choiceNames = list(
                "Because F is always less than 1",
                "Because P(U ≤ F(x)) = F(x) for U ~ Uniform(0,1)",
                "Because F⁻¹ and F cancel each other",
                "This is only true for symmetric distributions"
              ),
              choiceValues = list("a","b","c","d")
            ),
            uiOutput("q5fb")
          )
        )
      )
    )
  )
)

# ============================================================
#  SERVER
# ============================================================
server <- function(input, output, session) {

  # ── current distribution ──────────────────────────────────
  cur_d <- reactive({
    DISTS[[as.integer(input$dist_idx)]]
  })

  # ── dynamic parameter sliders ─────────────────────────────
  output$param_sliders <- renderUI({
    d <- cur_d()
    lapply(d$params, function(pm) {
      tagList(
        tags$label(pm$label),
        sliderInput(paste0("p_", pm$id), NULL,
          min=pm$min, max=pm$max, value=pm$val, step=pm$step)
      )
    })
  })

  # ── parameter vector ─────────────────────────────────────
  pv <- reactive({
    d <- cur_d()
    sapply(d$params, function(pm) {
      v <- input[[paste0("p_", pm$id)]]
      if (is.null(v)) pm$val else v
    })
  })

  # ── fix support for parameterised dists ──────────────────
  get_support <- reactive({
    d  <- cur_d()
    p  <- pv()
    lo <- d$support[1]
    hi <- d$support[2]
    if (!is.finite(lo)) lo <- d$F_inv(0.001, p)
    if (!is.finite(hi)) hi <- d$F_inv(0.999, p)
    c(lo, hi)
  })

  # ── run ITM ──────────────────────────────────────────────
  samp_rv <- eventReactive(input$go_btn, {
    d  <- cur_d()
    p  <- pv()
    n  <- input$n_samp
    set.seed(input$seed_val)
    u  <- runif(n)
    x  <- tryCatch(d$F_inv(u, p), error=function(e) rep(NA_real_, n))
    x  <- x[is.finite(x)]
    list(u=u[is.finite(d$F_inv(u, p))], x=x)
  })

  # ── sidebar stats ────────────────────────────────────────
  output$stats_ui <- renderUI({
    req(samp_rv())
    x <- samp_rv()$x
    d <- cur_d(); p <- pv()
    sp <- get_support()
    div(class="stats-grid",
      div(class="stat-cell",
        tags$span(class="sv", format(length(x), big.mark=",")),
        tags$span(class="sl", "n samples")
      ),
      div(class="stat-cell",
        tags$span(class="sv", round(mean(x), 4)),
        tags$span(class="sl", "sample mean")
      ),
      div(class="stat-cell",
        tags$span(class="sv", round(sd(x), 4)),
        tags$span(class="sl", "sample SD")
      ),
      div(class="stat-cell",
        tags$span(class="sv", round(median(x), 4)),
        tags$span(class="sl", "median")
      )
    )
  })

  # ── Tab 2: formulas ──────────────────────────────────────
  output$deriv_ui <- renderUI({
    HTML(cur_d()$deriv_html)
  })

  output$formulas_ui <- renderUI({
    d <- cur_d()
    tagList(
      tags$span(class="fml", d$cdf_expr),
      tags$span(class="fml", d$finv_expr)
    )
  })

  output$cdf_plot <- renderPlot({
    d  <- cur_d(); p <- pv(); sp <- get_support()
    xs <- seq(sp[1], sp[2], length.out=400)
    Fv <- d$F(xs, p)
    df <- data.frame(x=xs, F=Fv)
    ggplot(df, aes(x, F)) +
      geom_line(colour="#6366f1", linewidth=1.3) +
      geom_hline(yintercept=c(0,1), colour="#1d2030", linewidth=.7) +
      labs(x="x", y="F(x)", title="Cumulative Distribution Function") +
      dark_theme()
  })

  output$quantile_plot <- renderPlot({
    d  <- cur_d(); p <- pv()
    us <- seq(0.005, 0.995, length.out=400)
    qv <- d$F_inv(us, p)
    qv[!is.finite(qv)] <- NA
    df <- data.frame(u=us, q=qv)
    ggplot(df, aes(u, q)) +
      geom_line(colour="#22c55e", linewidth=1.3) +
      labs(x="u", y="F⁻¹(u)", title="Quantile (Inverse CDF) Function") +
      dark_theme()
  })

  # ── Tab 3: visualisation plots ────────────────────────────
  output$hist_plot <- renderPlot({
    req(samp_rv())
    x  <- samp_rv()$x
    d  <- cur_d(); p <- pv(); sp <- get_support()
    x_trim <- x[x >= sp[1] & x <= sp[2]]
    xs <- seq(sp[1], sp[2], length.out=400)
    fv <- d$f(xs, p)
    df_f <- data.frame(x=xs, y=fv)
    ggplot(data.frame(x=x_trim), aes(x)) +
      geom_histogram(aes(y=after_stat(density)), bins=50,
                     fill="#4f46e5", alpha=.7, colour=NA) +
      geom_line(data=df_f, aes(x,y), colour="#f87171",
                linewidth=1.3) +
      labs(x="x", y="Density",
           title="Sample (blue) vs f(x) (red)") +
      dark_theme()
  })

  output$ecdf_plot <- renderPlot({
    req(samp_rv())
    x  <- samp_rv()$x
    d  <- cur_d(); p <- pv(); sp <- get_support()
    x_trim <- sort(x[x >= sp[1] & x <= sp[2]])
    ecdf_y <- seq_along(x_trim) / length(x_trim)
    xs <- seq(sp[1], sp[2], length.out=400)
    Fv <- d$F(xs, p)
    df_e <- data.frame(x=x_trim, y=ecdf_y)
    df_t <- data.frame(x=xs, y=Fv)
    ggplot() +
      geom_step(data=df_e, aes(x,y, colour="Empirical"), linewidth=.8) +
      geom_line(data=df_t, aes(x,y, colour="Theoretical"), linewidth=1.2) +
      scale_colour_manual(values=c("Empirical"="#818cf8","Theoretical"="#f87171")) +
      labs(x="x", y="F(x)", colour=NULL,
           title="Empirical CDF vs Theoretical CDF") +
      dark_theme()
  })

  output$transform_plot <- renderPlot({
    req(samp_rv())
    s  <- samp_rv()
    n  <- min(500, length(s$x))
    u_s <- s$u[1:n]; x_s <- s$x[1:n]
    df <- data.frame(u=u_s, x=x_s)
    ggplot(df, aes(u, x)) +
      geom_point(colour="#818cf8", alpha=.5, size=1.2) +
      geom_smooth(colour="#f87171", linewidth=1, se=FALSE, method="loess", formula=y~x) +
      labs(x="U ~ Uniform(0,1)", y="X = F⁻¹(U)",
           title="Mapping: U → X = F⁻¹(U)  (first 500 pts)") +
      dark_theme()
  })

  output$qq_plot <- renderPlot({
    req(samp_rv())
    x  <- samp_rv()$x
    d  <- cur_d(); p <- pv()
    n  <- min(2000, length(x))
    xs <- sort(x[1:n])
    ps <- ppoints(n)
    th <- d$F_inv(ps, p)
    th[!is.finite(th)] <- NA
    df <- data.frame(th=th, sm=xs)
    ggplot(df, aes(th, sm)) +
      geom_point(colour="#818cf8", alpha=.4, size=.9) +
      geom_abline(colour="#f87171", linewidth=1) +
      labs(x="Theoretical Quantiles", y="Sample Quantiles",
           title="Q-Q Plot: points should lie on the red line") +
      dark_theme()
  })

  # ── Tab 4: interactive CDF ────────────────────────────────
  output$u_readout <- renderUI({
    d <- cur_d(); p <- pv()
    u <- input$u_val
    x_val <- tryCatch(d$F_inv(u, p), error=function(e) NA)
    div(class="stats-grid",
      div(class="stat-cell",
        tags$span(class="sv", round(u, 3)),
        tags$span(class="sl", "u")
      ),
      div(class="stat-cell",
        tags$span(class="sv", round(x_val, 4)),
        tags$span(class="sl", "F⁻¹(u) = x")
      )
    )
  })

  output$cdf_annotated <- renderPlot({
    d  <- cur_d(); p <- pv(); sp <- get_support()
    u  <- input$u_val
    x_val <- tryCatch(d$F_inv(u, p), error=function(e) NA_real_)

    xs <- seq(sp[1], sp[2], length.out=500)
    Fv <- d$F(xs, p)
    df <- data.frame(x=xs, F=Fv)

    g <- ggplot(df, aes(x, F)) +
      geom_line(colour="#6366f1", linewidth=1.4)

    if (is.finite(x_val)) {
      # horizontal line from y-axis to the curve
      g <- g +
        geom_segment(aes(x=sp[1], xend=x_val, y=u, yend=u),
                     colour="#f59e0b", linewidth=.9, linetype="dashed") +
        # vertical line from curve down to x-axis
        geom_segment(aes(x=x_val, xend=x_val, y=0, yend=u),
                     colour="#22c55e", linewidth=.9, linetype="dashed") +
        # point on the curve
        geom_point(aes(x=x_val, y=u), colour="white", size=3) +
        # u label on y-axis
        annotate("text", x=sp[1], y=u+0.04,
                 label=paste0("u=", round(u,2)),
                 colour="#f59e0b", size=3.5, hjust=0, fontface="bold") +
        # x label on x-axis
        annotate("text", x=x_val, y=-0.05,
                 label=paste0("F⁻¹(u)=", round(x_val,3)),
                 colour="#22c55e", size=3.5, hjust=0.5, fontface="bold")
    }

    g + labs(x="x", y="F(x)",
             title="Dashed lines show how to read F⁻¹(u) from the CDF") +
      dark_theme()
  })

  # ── Tab 5: code ──────────────────────────────────────────
  output$ccode_out <- renderText({ cur_d()$c_code })
  output$scode_out <- renderText({ cur_d()$sample_code })

  # ── Tab 6: Quiz ──────────────────────────────────────────
  output$q1fb <- renderUI({
    req(input$q1)
    if (input$q1 == "a")
      div(class="fb-ok",  "✅ Correct! This is the Probability Integral Transform theorem.")
    else
      div(class="fb-no",  "❌ If U ~ Uniform(0,1), then X = F⁻¹(U) has CDF F. The direction matters!")
  })
  output$q2fb <- renderUI({
    req(input$q2)
    if (input$q2 == "b")
      div(class="fb-ok",  "✅ Correct! F(x) = 1-e^(-λx), so solving F(x)=u gives x = -ln(1-u)/λ.")
    else
      div(class="fb-no",  "❌ Set 1-e^(-λx) = u → e^(-λx) = 1-u → x = -ln(1-u)/λ.")
  })
  output$q3fb <- renderUI({
    req(input$q3)
    if (input$q3 == "b")
      div(class="fb-ok",  "✅ Correct! Walk up the CDF until F(k) ≥ u. In R: qXXX(u, ...).")
    else
      div(class="fb-no",  "❌ For discrete distributions, find the smallest k with F(k) ≥ u.")
  })
  output$q4fb <- renderUI({
    req(input$q4)
    if (input$q4 == "b")
      div(class="fb-ok",  "✅ Correct! Every U produces exactly one accepted X — no rejections wasted.")
    else
      div(class="fb-no",  "❌ The main advantage is 100% efficiency: every U → accepted sample, no waste.")
  })
  output$q5fb <- renderUI({
    req(input$q5)
    if (input$q5 == "b")
      div(class="fb-ok",  "✅ Correct! P(F⁻¹(U) ≤ x) = P(U ≤ F(x)) = F(x), since U ~ Uniform(0,1).")
    else
      div(class="fb-no",  "❌ P(F⁻¹(U) ≤ x) = P(U ≤ F(x)) = F(x) because F is non-decreasing and U is Uniform.")
  })
}

shinyApp(ui, server)
