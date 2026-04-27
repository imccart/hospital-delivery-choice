# Estimate Choice Model ---------------------------------------------------

all.predictions <- list()
all.coefficients <- list()
models <- list()

for (market in markets) {
  result <- estimate_choice_model(
    market = market,
    var1 = var1,
    var2 = var2,
    pfx.vars = pfx.vars,
    pfx.inc = pfx.inc,
    data = choice.reg
  )
  
  all.predictions[[market]] <- result$predictions
  all.coefficients[[market]] <- result$coefficients
  models[[paste0("market_", market)]] <- result$model
  
}

final.boot <- bootstrap_choice_model(markets, var1, var2, pfx.vars, pfx.inc, choice.reg,
                                     n_bootstrap = n_boot, bin_spec = bin_spec)