library(shiny)
library(ggplot2)

# ============================================================
#  DISTRIBUTION DEFINITIONS
#  Each entry: f, support, g_type, c_formula, derivation HTML,
#              c_code, sample_code
# ============================================================
DISTS <- list(

  list(
    label    = "f(x) = 20x(1-x)³   [Beta(2,5)]",
    f        = function(x) 20 * x * (1 - x)^3,
    support  = c(0, 1),
    g_type   = "unif01",
    c_formula = "135/64 ≈ 2.109",
    deriv_html = "
      <b>Target:</b> f(x) = 20x(1-x)³ &nbsp;&nbsp; <b>Proposal:</b> g(x) = Uniform(0,1) → g(x) = 1<br><br>
      <b>Step 1.</b> Form the ratio:<br>
      &nbsp;&nbsp; f(x)/g(x) = 20x(1-x)³<br><br>
      <b>Step 2.</b> Differentiate and set to zero:<br>
      &nbsp;&nbsp; d/dx [20x(1-x)³] = 20[(1-x)³ - 3x(1-x)²] = 0<br>
      &nbsp;&nbsp; 20(1-x)²[(1-x) - 3x] = 0 → x* = 1/4<br><br>
      <b>Step 3.</b> Evaluate at x* = 1/4:<br>
      &nbsp;&nbsp; c = 20 × (1/4) × (3/4)³ = 20 × (27/256) = <b>135/64 ≈ 2.109</b><br><br>
      <b>Acceptance condition:</b><br>
      &nbsp;&nbsp; U₂ ≤ f(U₁) / (c·g(U₁)) = (64/135) × 20U₁(1-U₁)³<br>
      &nbsp;&nbsp; Simplifies to: U₂ ≤ (256/27) × U₁(1-U₁)³",
    c_code = '# Define f(x) — target density
f <- function(x) 20 * x * (1 - x)^3

# g(x) = Uniform(0,1), so g(x) = 1 everywhere
# Ratio f(x)/g(x) = f(x)

# Find c = max of f(x)/g(x) using optimize()
result <- optimize(f, interval = c(0, 1), maximum = TRUE)
c_val  <- result$objective   # 2.109375  (= 135/64)
x_star <- result$maximum     # 0.25      (= 1/4)

cat("x* =", round(x_star, 6), "\n")  # critical point
cat("c  =", round(c_val,  6), "\n")  # envelope constant',

    sample_code = '# Acceptance-Rejection sampler
set.seed(361)
n <- 10000   # desired sample size
k <- 0       # accepted count
i <- 0       # total iterations
y <- numeric(n)

while (k < n) {
  U1 <- runif(1)   # candidate from g(x) = Uniform(0,1)
  U2 <- runif(1)   # for acceptance test
  i  <- i + 1

  if (U2 <= f(U1) / c_val) {   # accept if below boundary
    k    <- k + 1
    y[k] <- U1
  }
}

cat("Total iterations:", i, "\n")
cat("Acceptance rate :", round(n / i, 4), "\n")  # ≈ 1/c'
  ),

  list(
    label    = "f(x) = x²/9   [0 < x < 3]",
    f        = function(x) x^2 / 9,
    support  = c(0, 3),
    g_type   = "unif03",
    c_formula = "3",
    deriv_html = "
      <b>Target:</b> f(x) = x²/9 &nbsp;&nbsp; <b>Proposal:</b> g(x) = Uniform(0,3) → g(x) = 1/3<br><br>
      <b>Step 1.</b> Form the ratio:<br>
      &nbsp;&nbsp; f(x)/g(x) = (x²/9) / (1/3) = x²/3<br><br>
      <b>Step 2.</b> x²/3 is <i>increasing</i> on [0,3], so maximum is at the right boundary:<br>
      &nbsp;&nbsp; c = 3²/3 = 9/3 = <b>3</b><br><br>
      <b>Acceptance condition:</b><br>
      &nbsp;&nbsp; U ≤ f(Y) / (c·g(Y)) = (Y²/9) / (3 × 1/3) = Y²/9",
    c_code = '# Define f and g
f <- function(x) x^2 / 9
g <- function(x) 1/3   # Uniform(0,3) density

# Ratio f(x)/g(x) = x^2/3, increasing → max at x=3
result <- optimize(function(x) f(x)/g(x),
                   interval = c(0, 3), maximum = TRUE)
c_val  <- result$objective   # 3
x_star <- result$maximum     # 3

cat("c =", c_val, "\n")',

    sample_code = 'set.seed(361)
n <- 10000;  acc <- numeric(n)
k <- 0;  total <- 0

while (k < n) {
  Y <- runif(1, 0, 3)   # sample from g = Uniform(0,3)
  U <- runif(1)
  total <- total + 1

  if (U <= f(Y) / (c_val * g(Y))) {
    k      <- k + 1
    acc[k] <- Y
  }
}

cat("Rejection rate:", round(1 - n/total, 4), "\n")'
  ),

  list(
    label    = "f(x) = 3x²   [0 ≤ x ≤ 1]",
    f        = function(x) 3 * x^2,
    support  = c(0, 1),
    g_type   = "unif01",
    c_formula = "3",
    deriv_html = "
      <b>Target:</b> f(x) = 3x² &nbsp;&nbsp; <b>Proposal:</b> g(x) = Uniform(0,1) → g(x) = 1<br><br>
      <b>Step 1.</b> Form the ratio:<br>
      &nbsp;&nbsp; f(x)/g(x) = 3x²<br><br>
      <b>Step 2.</b> 3x² is increasing on [0,1] → maximum at x = 1:<br>
      &nbsp;&nbsp; c = 3 × 1² = <b>3</b><br><br>
      <b>Acceptance condition:</b><br>
      &nbsp;&nbsp; U₂ ≤ f(U₁) / (c·g(U₁)) = 3U₁² / 3 = U₁²<br>
      &nbsp;&nbsp; i.e., accept if U₂ ≤ U₁²",
    c_code = '# f(x) = 3x^2 on [0,1], g(x) = Uniform(0,1)
f <- function(x) 3 * x^2

# f(x)/g(x) = 3x^2, maximized at x=1
result <- optimize(f, interval = c(0, 1), maximum = TRUE)
c_val  <- result$objective   # = 3

cat("c =", c_val, "\n")',

    sample_code = 'set.seed(1234)
n <- 10000;  k <- 0;  i <- 0;  y <- numeric(n)

while (k < n) {
  u <- runif(1)   # acceptance uniform
  x <- runif(1)   # candidate from Uniform(0,1)
  i <- i + 1

  if (u < x^2) {  # simplified: f/(c*g) = 3x^2/3 = x^2
    k    <- k + 1
    y[k] <- x
  }
}

cat("Acceptance rate:", round(n/i, 4), "\n")  # ≈ 1/3'
  ),

  list(
    label    = "f(x) = (2/π)√(1-x²)   [Semicircle]",
    f        = function(x) (2/pi) * sqrt(pmax(1 - x^2, 0)),
    support  = c(-1, 1),
    g_type   = "unif11",
    c_formula = "4/π ≈ 1.273",
    deriv_html = "
      <b>Target:</b> f(x) = (2/π)√(1-x²) &nbsp;&nbsp; <b>Proposal:</b> g(x) = Uniform(-1,1) → g(x) = 1/2<br><br>
      <b>Step 1.</b> Form the ratio:<br>
      &nbsp;&nbsp; f(x)/g(x) = (2/π)√(1-x²) / (1/2) = (4/π)√(1-x²)<br><br>
      <b>Step 2.</b> Differentiate:<br>
      &nbsp;&nbsp; d/dx [(4/π)√(1-x²)] = -4x / (π√(1-x²)) = 0 → x* = 0<br><br>
      <b>Step 3.</b> Evaluate at x* = 0:<br>
      &nbsp;&nbsp; c = (4/π)√(1-0) = <b>4/π ≈ 1.273</b><br><br>
      <b>Acceptance condition:</b><br>
      &nbsp;&nbsp; U₂ ≤ f(U₁) / (c·g(U₁)) = [(2/π)√(1-U₁²)] / [(4/π)(1/2)] = √(1-U₁²)",
    c_code = '# f(x) = (2/pi)*sqrt(1-x^2) on [-1,1]
# g(x) = Uniform(-1,1), density = 1/2
f <- function(x) (2/pi) * sqrt(1 - x^2)
g <- function(x) 1/2

# Ratio = (4/pi)*sqrt(1-x^2), max at x=0
result <- optimize(function(x) f(x)/g(x),
                   interval = c(-1, 1), maximum = TRUE)
c_val  <- result$objective   # = 4/pi ≈ 1.2732
x_star <- result$maximum     # ≈ 0

cat("c  =", round(c_val, 6), "\n")   # 4/pi',

    sample_code = 'set.seed(361)
n <- 10000;  accepted <- numeric(n)
k <- 0;  total <- 0

while (k < n) {
  U1 <- runif(1, -1, 1)   # from g = Uniform(-1,1)
  U2 <- runif(1)
  total <- total + 1

  # f/(c*g) simplifies to sqrt(1 - U1^2)
  if (U2 <= sqrt(1 - U1^2)) {
    k          <- k + 1
    accepted[k] <- U1
  }
}

cat("Acceptance rate:", round(n/total, 4), "\n")  # ≈ pi/4'
  ),

  list(
    label    = "f(x) = dbeta(x, α, β)   [Beta general]",
    f        = function(x) dbeta(x, 3, 2),   # default shown; server overrides
    support  = c(0, 1),
    g_type   = "unif01",
    c_formula = "computed via optimize()",
    deriv_html = "
      <b>Target:</b> f(x) = Beta(α, β) density &nbsp;&nbsp; <b>Proposal:</b> g(x) = Uniform(0,1)<br><br>
      <b>Step 1.</b> The Beta density is:<br>
      &nbsp;&nbsp; f(x) = Γ(α+β)/[Γ(α)Γ(β)] · x^(α-1)(1-x)^(β-1)<br><br>
      <b>Step 2.</b> Set derivative to zero → mode at:<br>
      &nbsp;&nbsp; x* = (α-1)/(α+β-2) &nbsp; (for α,β > 1)<br>
      &nbsp;&nbsp; Use x* = 0 if α ≤ 1, x* = 1 if β ≤ 1<br><br>
      <b>Step 3.</b> c = f(x*) / g(x*) = f(x*) &nbsp; (since g = 1)<br><br>
      In R, <code>optimize()</code> finds this automatically.",
    c_code = '# Beta(alpha, beta) with g(x) = Uniform(0,1)
alpha <- 3;  beta_val <- 2
f <- function(x) dbeta(x, alpha, beta_val)

result <- optimize(f, interval = c(1e-6, 1 - 1e-6),
                   maximum = TRUE)
c_val  <- result$objective   # peak of Beta density
x_star <- result$maximum     # mode

cat("Mode x* =", round(x_star, 6), "\n")
cat("c       =", round(c_val,  6), "\n")',

    sample_code = 'set.seed(361)
n <- 1000;  accepted <- numeric(n)
k <- 0;  total <- 0

while (k < n) {
  X <- runif(1)          # from Uniform(0,1)
  U <- runif(1)
  total <- total + 1

  if (U < f(X) / c_val) {
    k          <- k + 1
    accepted[k] <- X
  }
}

cat("Acceptance rate:", round(n/total, 4), "\n")'
  ),

  list(
    label    = "f(x) ∝ exp(κ cos x)   [Von Mises]",
    f        = function(x) exp(1 * cos(x)),   # default k=1; server overrides
    support  = c(-pi, pi),
    g_type   = "unifpi",
    c_formula = "2π·exp(κ)",
    deriv_html = "
      <b>Target:</b> f(x) ∝ exp(κ cos x), x ∈ (-π, π] &nbsp;&nbsp;
      <b>Proposal:</b> g(x) = Uniform(-π, π) → g(x) = 1/(2π)<br><br>
      <b>Step 1.</b> Form the ratio:<br>
      &nbsp;&nbsp; f(x)/g(x) = exp(κ cos x) × 2π<br><br>
      <b>Step 2.</b> cos(x) is maximized at x = 0, where cos(0) = 1:<br>
      &nbsp;&nbsp; c = 2π × exp(κ × 1) = <b>2π·exp(κ)</b><br><br>
      <b>Step 3.</b> Acceptance condition simplifies:<br>
      &nbsp;&nbsp; U ≤ exp(κ cos x) / exp(κ) = exp(κ(cos x - 1))<br>
      &nbsp;&nbsp; (since exp(κ(cos x - 1)) ≤ 1 always, because cos x ≤ 1)",
    c_code = '# Von Mises: f(x) = exp(kappa * cos(x)), x in (-pi, pi)
# g(x) = Uniform(-pi, pi), density = 1/(2*pi)
kappa <- 1.0

# Maximum of f(x)/g(x) at x=0 (where cos=1)
c_val <- 2 * pi * exp(kappa)
cat("c =", round(c_val, 6), "\n")

# Verify numerically
result <- optimize(function(x) exp(kappa*cos(x)) * 2*pi,
                   interval = c(-pi, pi), maximum = TRUE)
cat("Numeric check:", round(result$objective, 6), "\n")',

    sample_code = 'set.seed(361)
n <- 10000;  X <- numeric(n)
k <- 0;  total <- 0

while (k < n) {
  x0 <- runif(1, -pi, pi)   # from Uniform(-pi, pi)
  u  <- runif(1)
  total <- total + 1

  # Accept condition: u <= exp(kappa*(cos(x0) - 1))
  if (u <= exp(kappa * (cos(x0) - 1))) {
    k    <- k + 1
    X[k] <- x0
  }
}

cat("Acceptance rate:", round(n/total, 4), "\n")'
  )
)

# proposal density and sampler by type
g_density <- function(type) {
  switch(type,
    unif01 = function(x) rep(1,       length(x)),
    unif03 = function(x) rep(1/3,     length(x)),
    unif11 = function(x) rep(1/2,     length(x)),
    unifpi = function(x) rep(1/(2*pi),length(x))
  )
}
g_sampler <- function(type, supp) {
  switch(type,
    unif01 = function(m) runif(m, 0,   1),
    unif03 = function(m) runif(m, 0,   3),
    unif11 = function(m) runif(m, -1,  1),
    unifpi = function(m) runif(m, -pi, pi)
  )
}

# ============================================================
#  CORE AR SAMPLER  (vectorised, no loop, fast)
# ============================================================
do_ar <- function(f, g_r, g_d, c_val, n_want, seed) {
  set.seed(seed)
  # generate enough candidates in one shot
  batch <- max(n_want * ceiling(c_val + 1) * 3L, 50000L)
  cands  <- g_r(batch)
  us     <- runif(batch)
  ratio  <- f(cands) / (c_val * g_d(cands))
  ratio  <- pmin(ratio, 1)            # clamp — never > 1 with valid c
  accept <- us <= ratio

  accepted <- cands[accept]
  rejected <- cands[!accept]

  # cumulative acceptance rate for convergence plot
  cum_r <- cumsum(as.integer(accept)) / seq_along(accept)
  thin  <- round(seq(1, batch, length.out = 500))

  list(
    accepted = head(accepted, n_want),
    rejected = head(rejected, 3000),
    n_acc    = min(n_want, length(accepted)),
    n_tot    = batch,
    rate     = mean(accept),
    cum_r    = cum_r[thin],
    cum_idx  = thin,
    c        = c_val
  )
}

# ============================================================
#  UI
# ============================================================
ui <- fluidPage(

  tags$head(tags$style(HTML('
    @import url("https://fonts.googleapis.com/css2?family=Crimson+Pro:wght@400;600;700&family=JetBrains+Mono:wght@400;500&family=DM+Sans:wght@400;500;600&display=swap");

    * { box-sizing: border-box; }
    body {
      background: #0f1117;
      color: #e8eaf0;
      font-family: "DM Sans", sans-serif;
      font-size: 14px;
    }

    /* ── Header ── */
    .hdr {
      background: linear-gradient(135deg, #1a1d2e 0%, #0f1117 100%);
      border-bottom: 1px solid #2a2d3e;
      padding: 18px 28px 14px;
      margin-bottom: 20px;
    }
    .hdr h1 {
      font-family: "Crimson Pro", serif;
      font-size: 26px; font-weight: 700;
      color: #e8eaf0; margin: 0 0 4px;
      letter-spacing: -0.3px;
    }
    .hdr p { margin: 0; color: #7a7f99; font-size: 13px; }
    .badge {
      display: inline-block;
      background: #1e2235; border: 1px solid #2e3250;
      color: #8b9cf4; border-radius: 4px;
      padding: 2px 8px; font-size: 11px; font-weight: 600;
      margin-right: 6px; margin-top: 6px;
    }

    /* ── Sidebar ── */
    .sidebar {
      background: #13151f;
      border: 1px solid #1e2235;
      border-radius: 8px;
      padding: 16px;
    }
    .sidebar label { color: #a0a5c0; font-size: 12px; font-weight: 600;
                     text-transform: uppercase; letter-spacing: .5px; }
    .sidebar select, .sidebar input[type=number] {
      background: #0f1117 !important;
      border: 1px solid #2a2d3e !important;
      color: #e8eaf0 !important;
      border-radius: 5px; padding: 6px 10px;
      font-size: 13px; width: 100%;
    }
    .sidebar hr { border-color: #1e2235; margin: 14px 0; }
    .sidebar h5 {
      font-family: "Crimson Pro", serif;
      font-size: 15px; color: #c8cce8;
      margin: 0 0 12px; font-weight: 600;
    }

    /* run button */
    .run-btn {
      background: #3d4aad !important;
      border: none !important; color: white !important;
      font-weight: 600 !important; letter-spacing: .4px !important;
      border-radius: 6px !important; width: 100% !important;
      padding: 9px 0 !important; font-size: 13px !important;
      transition: background .2s !important;
      cursor: pointer !important;
    }
    .run-btn:hover { background: #4e5cce !important; }

    /* ── Stat pills ── */
    .stats-row {
      display: flex; flex-wrap: wrap; gap: 8px; margin-top: 14px;
    }
    .stat-pill {
      background: #1a1d2e;
      border: 1px solid #2a2d3e;
      border-radius: 6px;
      padding: 8px 12px;
      min-width: 90px;
      text-align: center;
    }
    .stat-pill .sv { font-size: 17px; font-weight: 700; color: #8b9cf4; }
    .stat-pill .sl { font-size: 10px; color: #6a6f8a;
                     text-transform: uppercase; letter-spacing: .4px; margin-top: 2px; }

    /* ── Tabs ── */
    .nav-tabs {
      border-bottom: 1px solid #2a2d3e !important;
      margin-bottom: 16px;
    }
    .nav-tabs > li > a {
      background: transparent !important;
      border: none !important;
      color: #6a6f8a !important;
      font-weight: 600; font-size: 13px;
      padding: 8px 16px !important;
      border-radius: 0 !important;
      border-bottom: 2px solid transparent !important;
    }
    .nav-tabs > li.active > a {
      color: #8b9cf4 !important;
      border-bottom: 2px solid #8b9cf4 !important;
      background: transparent !important;
    }
    .nav-tabs > li > a:hover { color: #c8cce8 !important; }

    /* ── Cards ── */
    .card {
      background: #13151f;
      border: 1px solid #1e2235;
      border-radius: 8px;
      padding: 16px;
      margin-bottom: 16px;
    }
    .card h4 {
      font-family: "Crimson Pro", serif;
      font-size: 15px; font-weight: 600;
      color: #c8cce8; margin: 0 0 12px;
      padding-bottom: 8px;
      border-bottom: 1px solid #1e2235;
    }

    /* ── Derivation box ── */
    .deriv-box {
      background: #0f1117;
      border-left: 3px solid #8b9cf4;
      border-radius: 0 6px 6px 0;
      padding: 14px 16px;
      font-size: 13.5px;
      line-height: 1.9;
      color: #c8cce8;
    }

    /* ── Code box ── */
    .code-box {
      background: #0a0c14;
      border: 1px solid #1e2235;
      border-radius: 6px;
      padding: 14px 16px;
      font-family: "JetBrains Mono", monospace;
      font-size: 12px;
      line-height: 1.75;
      color: #a8b4d0;
      white-space: pre;
      overflow-x: auto;
    }

    /* ── Algo steps ── */
    .step {
      display: flex; align-items: flex-start;
      gap: 12px; margin-bottom: 14px;
    }
    .step-num {
      background: #3d4aad;
      color: white; border-radius: 50%;
      width: 26px; height: 26px; min-width: 26px;
      display: flex; align-items: center; justify-content: center;
      font-size: 12px; font-weight: 700;
    }
    .step-body { flex: 1; }
    .step-body b { color: #8b9cf4; }

    /* formula highlight */
    .fml {
      background: #1a1d2e;
      border: 1px solid #2e3250;
      border-radius: 5px;
      padding: 8px 14px;
      font-family: "JetBrains Mono", monospace;
      font-size: 12.5px;
      color: #c4c9f0;
      display: block;
      margin: 8px 0;
    }

    /* alert boxes */
    .alert-info {
      background: #1a1d2e !important;
      border: 1px solid #2e3250 !important;
      border-left: 3px solid #8b9cf4 !important;
      color: #c8cce8 !important;
      border-radius: 6px !important;
      padding: 10px 14px !important;
      font-size: 13px !important;
      margin-bottom: 12px !important;
    }
    .alert-success {
      background: #0e1e18 !important;
      border: 1px solid #1a3a2a !important;
      border-left: 3px solid #2ecc71 !important;
      color: #a0e8c0 !important;
      border-radius: 6px !important;
      padding: 10px 14px !important;
      font-size: 13px !important;
    }
    .alert-warn {
      background: #1e180a !important;
      border: 1px solid #3a2e0a !important;
      border-left: 3px solid #f39c12 !important;
      color: #e8c87a !important;
      border-radius: 6px !important;
      padding: 10px 14px !important;
      font-size: 13px !important;
    }

    /* quiz feedback */
    .fb-correct { color: #2ecc71; font-weight: 600; margin-top: 6px; }
    .fb-wrong   { color: #e74c3c; font-weight: 600; margin-top: 6px; }

    /* scrollbar */
    ::-webkit-scrollbar { width: 5px; height: 5px; }
    ::-webkit-scrollbar-track { background: #0f1117; }
    ::-webkit-scrollbar-thumb { background: #2a2d3e; border-radius: 3px; }

    /* ggplot bg override */
    .shiny-plot-output { border-radius: 6px; overflow: hidden; }
  '))),

  # ── HEADER ────────────────────────────────────────────────
  div(class = "hdr",
    h1("Acceptance-Rejection Sampling"),
    p("Interactive teaching tool — algorithm, derivations, code, visualisations"),
    span(class="badge", "📐 Derive c"),
    span(class="badge", "💻 R Code"),
    span(class="badge", "📊 Visualise"),
    span(class="badge", "🔬 Compare c"),
    span(class="badge", "🎓 Quiz")
  ),

  fluidRow(

    # ── LEFT SIDEBAR ──────────────────────────────────────────
    column(3,
      div(class = "sidebar",

        h5("Distribution"),
        selectInput("dist_idx", label = NULL,
          choices = setNames(as.character(1:6),
            sapply(DISTS, `[[`, "label")),
          selected = "1"
        ),

        # conditional sliders for Beta and Von Mises
        conditionalPanel("input.dist_idx == '5'",
          tags$label("Shape α"),
          sliderInput("ba", NULL, 0.5, 8, 3, 0.5),
          tags$label("Shape β"),
          sliderInput("bb", NULL, 0.5, 8, 2, 0.5)
        ),
        conditionalPanel("input.dist_idx == '6'",
          tags$label("Concentration κ"),
          sliderInput("kappa", NULL, 0.1, 5, 1, 0.1)
        ),

        tags$hr(),
        h5("Sampling"),
        tags$label("Sample size n"),
        sliderInput("n_samp", NULL, 200, 15000, 5000, 200),
        tags$label("Random seed"),
        numericInput("seed", NULL, 361, 1, 99999, 1),
        tags$label("Envelope constant c"),
        checkboxInput("use_opt_c", "Use optimal c  (recommended)", TRUE),
        conditionalPanel("!input.use_opt_c",
          sliderInput("c_manual", NULL, 1, 10, 2, 0.01)
        ),
        tags$br(),
        actionButton("go", "▶  Run Sampling", class = "run-btn"),

        tags$hr(),
        h5("Results"),
        uiOutput("stats_out")
      )
    ),

    # ── MAIN PANEL ────────────────────────────────────────────
    column(9,
      tabsetPanel(id = "tabs",

        # TAB 1 — HOW IT WORKS
        tabPanel("📖 Algorithm",
          tags$br(),
          div(class = "alert-info",
            "The acceptance-rejection method generates samples from a
             target f(x) using a simpler proposal g(x).
             No CDF inversion needed — only pointwise evaluation of f(x)."
          ),
          fluidRow(
            column(6,
              div(class = "card",
                h4("The 4-Step Algorithm"),
                div(class="step",
                  div(class="step-num","1"),
                  div(class="step-body",
                    tags$b("Choose proposal g(x) and constant c"),
                    tags$br(),
                    "Find g(x) easy to simulate from and a constant c such that",
                    tags$span(class="fml", "f(x) ≤ c · g(x)   for all x")
                  )
                ),
                div(class="step",
                  div(class="step-num","2"),
                  div(class="step-body",
                    tags$b("Draw candidate Y ~ g(x)")
                  )
                ),
                div(class="step",
                  div(class="step-num","3"),
                  div(class="step-body",
                    tags$b("Draw U ~ Uniform(0,1)")
                  )
                ),
                div(class="step",
                  div(class="step-num","4"),
                  div(class="step-body",
                    tags$b("Accept or Reject"),
                    tags$span(class="fml", "Accept Y  if  U ≤ f(Y) / [c · g(Y)]"),
                    "Otherwise discard Y and return to Step 2."
                  )
                )
              )
            ),
            column(6,
              div(class = "card",
                h4("Finding the Optimal c"),
                tags$span(class="fml", "c = sup  f(x) / g(x)"),
                tags$p("Geometric interpretation: c·g(x) is an envelope that
                        sits above f(x) everywhere. The ratio f(x)/[c·g(x)]
                        is always ≤ 1, making it a valid probability."),
                div(class="alert-warn",
                  "⚠️ If c is too small, f(x) > c·g(x) somewhere → wrong samples!
                   If c is too large → many rejections but correct samples."
                ),
                div(class = "card", style="margin-top:10px;",
                  h4("Efficiency"),
                  tags$span(class="fml", "P(accept) = 1/c"),
                  tags$span(class="fml", "N ~ Geometric(1/c)"),
                  tags$span(class="fml", "E[iterations per sample] = c"),
                  tags$p("A smaller c = higher efficiency = fewer wasted candidates.")
                )
              )
            )
          )
        ),

        # TAB 2 — FINDING c
        tabPanel("📐 Derive c",
          tags$br(),
          fluidRow(
            column(6,
              div(class = "card",
                h4("Step-by-Step Derivation"),
                div(class = "deriv-box", uiOutput("deriv_ui"))
              ),
              div(class = "card",
                h4("Optimal c Value"),
                div(class = "alert-success", uiOutput("c_value_ui"))
              )
            ),
            column(6,
              div(class = "card",
                h4("Ratio f(x)/g(x) — optimal c is the maximum"),
                plotOutput("ratio_plot", height = "320px")
              )
            )
          )
        ),

        # TAB 3 — VISUALISE
        tabPanel("📊 Visualise",
          tags$br(),
          div(class = "alert-info",
            "Click  ▶ Run Sampling  in the sidebar first."
          ),
          fluidRow(
            column(6,
              div(class="card",
                h4("Histogram of Accepted Samples vs f(x)"),
                plotOutput("hist_plot", height = "290px")
              )
            ),
            column(6,
              div(class="card",
                h4("Accepted vs Rejected Points"),
                plotOutput("scatter_plot", height = "290px")
              )
            )
          ),
          fluidRow(
            column(6,
              div(class="card",
                h4("Envelope: c·g(x) must cover f(x)"),
                plotOutput("envelope_plot", height = "250px")
              )
            ),
            column(6,
              div(class="card",
                h4("Cumulative Acceptance Rate → 1/c"),
                plotOutput("conv_plot", height = "250px")
              )
            )
          )
        ),

        # TAB 4 — CODE
        tabPanel("💻 R Code",
          tags$br(),
          div(class="card",
            h4("Step 1 — Define f, g and compute c"),
            div(class = "code-box", textOutput("c_code_out"))
          ),
          div(class="card",
            h4("Step 2 — The sampling loop"),
            div(class = "code-box", textOutput("samp_code_out"))
          ),
          div(class="card",
            h4("Step 3 — Validate with a histogram"),
            div(class = "code-box",
'# Validate: histogram vs theoretical density
hist(y, probability = TRUE, breaks = 40,
     col = "steelblue", border = "white",
     main = "Sample vs True Density", xlab = "x")

x_seq <- seq(support_lo, support_hi, length.out = 300)
lines(x_seq, f(x_seq), col = "red", lwd = 2)

cat("Sample mean:", round(mean(y), 4), "\\n")
cat("Sample SD  :", round(sd(y),   4), "\\n")')
          )
        ),

        # TAB 5 — COMPARE c
        tabPanel("🔬 Effect of c",
          tags$br(),
          div(class="alert-info",
            "Compare what happens with different values of c.
             Too small = invalid (histogram wrong). Optimal = correct and efficient.
             Too large = correct but wasteful."
          ),
          fluidRow(
            column(3,
              div(class="sidebar",
                h5("Three c values"),
                tags$label("c₁  (too small — INVALID)"),
                numericInput("c1", NULL, value = 1.2, min=0.1, step=0.1),
                tags$label("c₂  (optimal)"),
                numericInput("c2", NULL, value = 2.1, min=0.1, step=0.1),
                tags$label("c₃  (too large — wasteful)"),
                numericInput("c3", NULL, value = 5.0, min=0.1, step=0.1),
                tags$br(),
                actionButton("go_compare", "▶  Compare", class="run-btn")
              )
            ),
            column(9,
              div(class="card",
                h4("Histograms for each c"),
                plotOutput("compare_hist", height = "280px")
              ),
              div(class="card",
                h4("Envelopes for each c"),
                plotOutput("compare_env", height = "240px")
              )
            )
          )
        ),

        # TAB 6 — QUIZ
        tabPanel("🎓 Quiz",
          tags$br(),
          div(class="card",
            h4("Q1 — Why must f(x) ≤ c·g(x) for all x?"),
            radioButtons("q1", NULL, selected=character(0), choiceNames=list(
              "So that f(x)/(c·g(x)) stays ≤ 1 everywhere (valid probability)",
              "To make the sampler faster",
              "c must be an integer"
            ), choiceValues=list("a","b","c")),
            uiOutput("q1fb")
          ),
          div(class="card",
            h4("Q2 — If c = 4, what is the expected number of iterations to get one accepted sample?"),
            radioButtons("q2", NULL, selected=character(0),
              choices=c("1","2","4","8"), inline=TRUE),
            uiOutput("q2fb")
          ),
          div(class="card",
            h4("Q3 — What distribution does the number of iterations N follow?"),
            radioButtons("q3", NULL, selected=character(0),
              choices=c("Normal","Poisson","Geometric","Uniform"), inline=TRUE),
            uiOutput("q3fb")
          ),
          div(class="card",
            h4("Q4 — How is the optimal c computed?"),
            radioButtons("q4", NULL, selected=character(0), choiceNames=list(
              "c = integral of f(x)/g(x)",
              "c = sup f(x)/g(x)  — the supremum of the ratio",
              "c = mean of f(x)",
              "c = 1 always works"
            ), choiceValues=list("a","b","c","d")),
            uiOutput("q4fb")
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

  # ── active distribution (reactive) ──────────────────────────
  cur_dist <- reactive({
    idx <- as.integer(input$dist_idx)
    d   <- DISTS[[idx]]
    # override f for parameterised dists
    if (idx == 5) {
      a <- input$ba; b <- input$bb
      d$f <- function(x) dbeta(x, a, b)
    }
    if (idx == 6) {
      k <- input$kappa
      d$f <- function(x) exp(k * cos(x))
    }
    d
  })

  # ── optimal c ────────────────────────────────────────────────
  opt_c <- reactive({
    d  <- cur_dist()
    gd <- g_density(d$g_type)
    xs <- seq(d$support[1] + 1e-5, d$support[2] - 1e-5, length.out = 5000)
    fv <- d$f(xs)
    gv <- gd(xs)
    max(fv / gv, na.rm = TRUE) * 1.005
  })

  # sync c2 in compare tab when distribution changes
  observeEvent(input$dist_idx, {
    oc <- round(opt_c(), 3)
    updateNumericInput(session, "c1", value = round(oc * 0.55, 3))
    updateNumericInput(session, "c2", value = oc)
    updateNumericInput(session, "c3", value = round(oc * 2.5,  3))
  })

  # ── run sampling ─────────────────────────────────────────────
  result <- eventReactive(input$go, {
    d     <- cur_dist()
    gd    <- g_density(d$g_type)
    gr    <- g_sampler(d$g_type, d$support)
    c_val <- if (input$use_opt_c) opt_c() else input$c_manual
    do_ar(d$f, gr, gd, c_val, input$n_samp, input$seed)
  })

  # ── sidebar stats ────────────────────────────────────────────
  output$stats_out <- renderUI({
    req(result())
    r <- result()
    div(class = "stats-row",
      div(class="stat-pill",
        div(class="sv", round(r$c, 3)),
        div(class="sl", "c used")
      ),
      div(class="stat-pill",
        div(class="sv", paste0(round(r$rate*100, 1), "%")),
        div(class="sl", "acc. rate")
      ),
      div(class="stat-pill",
        div(class="sv", paste0(round(100/r$c, 1), "%")),
        div(class="sl", "efficiency")
      ),
      div(class="stat-pill",
        div(class="sv", format(r$n_acc, big.mark=",")),
        div(class="sl", "accepted")
      ),
      div(class="stat-pill",
        div(class="sv", round(mean(r$accepted), 3)),
        div(class="sl", "mean")
      ),
      div(class="stat-pill",
        div(class="sv", round(sd(r$accepted), 3)),
        div(class="sl", "SD")
      )
    )
  })

  # ── Tab 2: derivation ────────────────────────────────────────
  output$deriv_ui <- renderUI({
    HTML(cur_dist()$deriv_html)
  })

  output$c_value_ui <- renderUI({
    d <- cur_dist()
    HTML(paste0(
      "<b>Formula: </b>", d$c_formula, "<br>",
      "<b>Computed: </b>", round(opt_c(), 6),
      " &nbsp;→&nbsp; Acceptance rate = ",
      round(100 / opt_c(), 2), "%"
    ))
  })

  output$ratio_plot <- renderPlot({
    d   <- cur_dist()
    gd  <- g_density(d$g_type)
    c_v <- opt_c()
    xs  <- seq(d$support[1]+1e-4, d$support[2]-1e-4, length.out=400)
    rv  <- d$f(xs) / gd(xs)
    df  <- data.frame(x=xs, ratio=rv)

    ggplot(df, aes(x, ratio)) +
      geom_ribbon(aes(ymin=0, ymax=ratio), fill="#3d4aad", alpha=.3) +
      geom_line(colour="#8b9cf4", linewidth=1.2) +
      geom_hline(yintercept=c_v, colour="#e74c3c", linewidth=1,
                 linetype="dashed") +
      annotate("text", x = mean(d$support),
               y = c_v * 1.06,
               label = paste0("c = ", round(c_v, 4)),
               colour="#e74c3c", size=3.8, fontface="bold") +
      labs(x="x", y="f(x) / g(x)") +
      theme_minimal(base_size=12) +
      theme(plot.background  = element_rect(fill="#13151f", colour=NA),
            panel.background = element_rect(fill="#13151f", colour=NA),
            panel.grid       = element_line(colour="#1e2235"),
            axis.text        = element_text(colour="#7a7f99"),
            axis.title       = element_text(colour="#a0a5c0"))
  })

  # ── Tab 3: plots ─────────────────────────────────────────────
  dark_theme <- function() {
    theme_minimal(base_size=12) +
      theme(plot.background  = element_rect(fill="#13151f", colour=NA),
            panel.background = element_rect(fill="#13151f", colour=NA),
            panel.grid       = element_line(colour="#1e2235"),
            axis.text        = element_text(colour="#7a7f99"),
            axis.title       = element_text(colour="#a0a5c0"),
            legend.background= element_rect(fill="#13151f", colour=NA),
            legend.text      = element_text(colour="#a0a5c0"),
            plot.title       = element_text(colour="#c8cce8", size=12))
  }

  output$hist_plot <- renderPlot({
    req(result())
    r   <- result()
    d   <- cur_dist()
    xs  <- seq(d$support[1], d$support[2], length.out=400)
    dfd <- data.frame(x=xs, y=d$f(xs))

    ggplot(data.frame(x=r$accepted), aes(x)) +
      geom_histogram(aes(y=after_stat(density)), bins=45,
                     fill="#3d4aad", alpha=.7, colour=NA) +
      geom_line(data=dfd, aes(x,y), colour="#e74c3c", linewidth=1.3) +
      labs(x="x", y="Density",
           title="Blue bars = sample,  Red line = f(x)") +
      dark_theme()
  })

  output$scatter_plot <- renderPlot({
    req(result())
    r   <- result()
    d   <- cur_dist()
    gd  <- g_density(d$g_type)
    c_v <- r$c
    na  <- min(length(r$accepted), 2000)
    nr  <- min(length(r$rejected), 1200)

    set.seed(1)
    ay <- runif(na, 0, d$f(r$accepted[1:na]) / (c_v * gd(r$accepted[1:na])))
    ry <- runif(nr, d$f(r$rejected[1:nr]) / (c_v * gd(r$rejected[1:nr])), 1)

    df <- data.frame(
      x = c(r$accepted[1:na], r$rejected[1:nr]),
      y = c(ay, ry),
      s = c(rep("Accepted", na), rep("Rejected", nr))
    )
    xs <- seq(d$support[1]+1e-4, d$support[2]-1e-4, length.out=400)
    bnd <- data.frame(x=xs, y=d$f(xs)/(c_v*gd(xs)))

    ggplot(df, aes(x, y, colour=s)) +
      geom_point(alpha=.25, size=.7) +
      geom_line(data=bnd, aes(x,y), colour="white",
                linewidth=.8, linetype="dashed", inherit.aes=FALSE) +
      scale_colour_manual(values=c("Accepted"="#2ecc71","Rejected"="#e74c3c")) +
      labs(x="x", y="U",
           title="Dashed line = acceptance boundary f/(c·g)") +
      dark_theme()
  })

  output$envelope_plot <- renderPlot({
    req(result())
    r   <- result()
    d   <- cur_dist()
    gd  <- g_density(d$g_type)
    xs  <- seq(d$support[1]+1e-4, d$support[2]-1e-4, length.out=400)
    df  <- rbind(
      data.frame(x=xs, y=d$f(xs),            type="f(x) target"),
      data.frame(x=xs, y=r$c * gd(xs),       type="c·g(x) envelope")
    )
    ggplot(df, aes(x, y, colour=type, linetype=type)) +
      geom_line(linewidth=1.2) +
      scale_colour_manual(values=c("f(x) target"="#8b9cf4",
                                   "c·g(x) envelope"="#f39c12")) +
      scale_linetype_manual(values=c("f(x) target"="solid",
                                     "c·g(x) envelope"="dashed")) +
      labs(x="x", y="Density",
           title="c·g(x) must always sit above f(x)") +
      dark_theme()
  })

  output$conv_plot <- renderPlot({
    req(result())
    r  <- result()
    df <- data.frame(iter=r$cum_idx, rate=r$cum_r)
    target <- 1 / r$c

    ggplot(df, aes(iter, rate)) +
      geom_line(colour="#8b9cf4", linewidth=1) +
      geom_hline(yintercept=target, colour="#e74c3c",
                 linetype="dashed", linewidth=1) +
      annotate("text", x=max(df$iter)*0.6,
               y=target+0.03,
               label=paste0("1/c = ", round(target,4)),
               colour="#e74c3c", size=3.5, fontface="bold") +
      scale_y_continuous(labels=scales::percent_format(1), limits=c(0,1)) +
      labs(x="Iteration", y="Cumulative acceptance rate") +
      dark_theme()
  })

  # ── Tab 4: code ──────────────────────────────────────────────
  output$c_code_out    <- renderText({ cur_dist()$c_code })
  output$samp_code_out <- renderText({ cur_dist()$sample_code })

  # ── Tab 5: compare c ─────────────────────────────────────────
  compare_res <- eventReactive(input$go_compare, {
    d  <- cur_dist()
    gd <- g_density(d$g_type)
    gr <- g_sampler(d$g_type, d$support)
    cs <- c(input$c1, input$c2, input$c3)
    lapply(seq_along(cs), function(i) {
      do_ar(d$f, gr, gd, cs[i], n_want=2000, seed=42+i)
    })
  })

  output$compare_hist <- renderPlot({
    req(compare_res())
    res <- compare_res()
    d   <- cur_dist()
    xs  <- seq(d$support[1], d$support[2], length.out=400)
    dfd <- data.frame(x=xs, y=d$f(xs))
    cs  <- c(input$c1, input$c2, input$c3)
    cols <- c("#e74c3c","#2ecc71","#f39c12")
    labs <- c(paste0("c=",cs[1]," (too small)"),
              paste0("c=",cs[2]," (optimal)"),
              paste0("c=",cs[3]," (too large)"))

    plots <- lapply(1:3, function(i) {
      r   <- res[[i]]
      acc <- r$accepted
      if (length(acc) < 5) return(NULL)
      ggplot(data.frame(x=acc), aes(x)) +
        geom_histogram(aes(y=after_stat(density)), bins=35,
                       fill=cols[i], alpha=.6, colour=NA) +
        geom_line(data=dfd, aes(x,y), colour="white", linewidth=1) +
        labs(title=paste0(labs[i],
               "\nacc. rate: ", round(r$rate*100,1), "%"),
             x="x", y="Density") +
        dark_theme() +
        theme(plot.title=element_text(size=10, colour=cols[i]))
    })

    # combine side by side using base par
    # Use patchwork if installed, else cowplot, else just plot[2]
    tryCatch({
      library(patchwork, quietly=TRUE)
      wrap_plots(Filter(Negate(is.null), plots), nrow=1)
    }, error=function(e) {
      # fallback: just show middle (optimal) plot
      if (!is.null(plots[[2]])) plots[[2]] else plots[[1]]
    })
  })

  output$compare_env <- renderPlot({
    req(compare_res())
    d   <- cur_dist()
    gd  <- g_density(d$g_type)
    xs  <- seq(d$support[1]+1e-4, d$support[2]-1e-4, length.out=400)
    cs  <- c(input$c1, input$c2, input$c3)
    pal <- c("#e74c3c","#2ecc71","#f39c12")

    df_f   <- data.frame(x=xs, y=d$f(xs), grp="f(x)")
    df_env <- do.call(rbind, lapply(seq_along(cs), function(i)
      data.frame(x=xs, y=cs[i]*gd(xs),
                 grp=paste0("c=",cs[i]))
    ))

    ggplot() +
      geom_line(data=df_f,   aes(x,y), colour="white",
                linewidth=1.3, key_glyph="path") +
      geom_line(data=df_env, aes(x,y,colour=grp),
                linewidth=1, linetype="dashed") +
      scale_colour_manual(
        values = setNames(pal, paste0("c=",cs))
      ) +
      labs(x="x", y="Density", colour=NULL,
           title="White = f(x),  dashed = c·g(x) for each c") +
      dark_theme()
  })

  # ── Tab 6: quiz ──────────────────────────────────────────────
  output$q1fb <- renderUI({
    req(input$q1)
    if (input$q1 == "a")
      div(class="fb-correct",
        "✅ Correct! f/(c·g) must be ≤ 1 everywhere to be a valid probability.")
    else
      div(class="fb-wrong",
        "❌ The key reason: if f(x)/(c·g(x)) > 1 somewhere, the acceptance condition P > 1, which is impossible.")
  })

  output$q2fb <- renderUI({
    req(input$q2)
    if (input$q2 == "4")
      div(class="fb-correct", "✅ Correct! E[N] = c = 4.")
    else
      div(class="fb-wrong", "❌ E[N] = c. With c = 4, on average 4 iterations per accepted sample.")
  })

  output$q3fb <- renderUI({
    req(input$q3)
    if (input$q3 == "Geometric")
      div(class="fb-correct",
        "✅ Correct! Each trial independently accepts with probability 1/c, so N ~ Geometric(1/c).")
    else
      div(class="fb-wrong",
        "❌ Each trial is an independent Bernoulli(1/c), so N ~ Geometric(1/c).")
  })

  output$q4fb <- renderUI({
    req(input$q4)
    if (input$q4 == "b")
      div(class="fb-correct",
        "✅ Correct! c = sup f(x)/g(x). In R: optimize(function(x) f(x)/g(x), interval, maximum=TRUE)$objective")
    else
      div(class="fb-wrong",
        "❌ c = sup_x f(x)/g(x) — the supremum (maximum) of the ratio over the support.")
  })
}

shinyApp(ui, server)
