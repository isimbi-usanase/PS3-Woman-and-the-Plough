
# =====================================================================
# Extension (e): Heterogeneity of the plough effect
# Alesina, Giuliano & Nunn (2013)
# =====================================================================

# ---- 1. Packages and data --------------------------------------------

library(haven)
library(dplyr)
library(fixest)

d <- read_dta("data/crosscountry_dataset.dta") |>
  zap_labels() |>
  mutate(
    continent = factor(continent),
    ln_income_c = ln_income - mean(ln_income, na.rm = TRUE)
  )
# ---- 2. Control variables --------------------------------------------

setFixest_fml(
  ..hist = ~ agricultural_suitability + tropical_climate +
    large_animals + political_hierarchies + economic_complexity,
  ..inc = ~ ln_income + ln_income_squared
)

# ---- 3. Plough x Communism -------------------------------------------

# Without continent fixed effects
m_comm <- feols(
  c(flfp2000, female_ownership, women_politics) ~
    plow * communist_dummy + ..hist + ..inc,
  data = d, vcov = "hetero"
)

# With continent fixed effects
m_comm_fe <- feols(
  c(flfp2000, female_ownership, women_politics) ~
    plow * communist_dummy + ..hist + ..inc | continent,
  data = d, vcov = "hetero"
)

# Marginal plough effect in communist countries
lincom <- function(m) {
  b <- coef(m)
  V <- vcov(m)
  a <- "plow"
  i <- "plow:communist_dummy"
  
  est <- b[a] + b[i]
  se <- sqrt(V[a, a] + V[i, i] + 2 * V[a, i])
  
  c(
    estimate = unname(est),
    se = unname(se),
    p_value = unname(2 * pnorm(-abs(est / se))),
    CI_lower = unname(est - 1.96 * se),
    CI_upper = unname(est + 1.96 * se)
  )
}

# Marginal effects with and without continent fixed effects
round(t(sapply(as.list(m_comm), lincom)), 3)
round(t(sapply(as.list(m_comm_fe), lincom)), 3)

# ---- 4. Plough x Income ----------------------------------------------

# Without continent fixed effects
m_inc <- feols(
  c(flfp2000, female_ownership, women_politics) ~
    plow * ln_income_c + I(ln_income_c^2) + ..hist,
  data = d, vcov = "hetero"
)

# With continent fixed effects
m_inc_fe <- feols(
  c(flfp2000, female_ownership, women_politics) ~
    plow * ln_income_c + I(ln_income_c^2) + ..hist | continent,
  data = d, vcov = "hetero"
)

# ---- 5. Regression tables --------------------------------------------

labels <- c(
  plow = "Traditional plough use",
  communist_dummy = "Communist history",
  "plow:communist_dummy" = "Plough x Communist",
  ln_income_c = "Log income (centred)",
  "plow:ln_income_c" = "Plough x Log income",
  flfp2000 = "FLFP 2000",
  female_ownership = "Female firm ownership",
  women_politics = "Women in parliament"
)

# Table A: Heterogeneity by communist history
etable(
  m_comm, m_comm_fe,
  keep = c("%^plow$", "%^communist_dummy$", 
           "%^plow:communist_dummy$"),
  dict = labels,
  digits = 3,
  se.below = TRUE,
  fitstat = ~ n + r2
)

# Table B: Heterogeneity by economic development
etable(
  m_inc, m_inc_fe,
  keep = c("%^plow$", "%^ln_income_c$", 
           "%^plow:ln_income_c$"),
  dict = labels,
  digits = 3,
  se.below = TRUE,
  fitstat = ~ n + r2
)
