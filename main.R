library(smoof)
library(ecr)
library(ggplot2)
library(kableExtra)
library(ParamHelpers) # Nowa zależność

set.seed(42068)

# 1. Pobieranie prawdziwych dziedzin ---------------------------------------------------------
get_domain <- function(fn) {
  ps <- getParamSet(fn)
  list(
    lower = sapply(ps$pars, function(p) p$lower),
    upper = sapply(ps$pars, function(p) p$upper)
  )
}

# 2. Poprawiona funkcja generująca punkty ----------------------------------------------------
getRandomPoint <- function(dimensions, domain) {
  sapply(1:dimensions, function(i) {
    runif(1, domain$lower[i], domain$upper[i])
  })
}

# 3. Uniwersalna funkcja PRS -----------------------------------------------------------------
performPRS <- function(n_eval, fn, domain) {
  dimensions <- length(domain$lower)
  points <- matrix(nrow = n_eval, ncol = dimensions)
  for(i in 1:n_eval) {
    points[i,] <- getRandomPoint(dimensions, domain)
  }
  min(apply(points, 1, fn))
}

# 4. Uniwersalna funkcja GA ------------------------------------------------------------------
performGA <- function(n_runs, n_eval, fn, domain) {
  dimensions <- length(domain$lower)
  res <- replicate(n_runs, ecr(
    fitness.fun = fn,
    n.dim = dimensions,
    lower = domain$lower,
    upper = domain$upper,
    minimize = TRUE,
    representation = "float",
    mu = 50L,
    lambda = 25L,
    terminators = list(stopOnEvals(n_eval)),
    mutator = setup(mutGauss, lower = domain$lower, upper = domain$upper)
  )$best.y)
  res
}

# 5. Przeprowadzenie eksperymentu -----------------------------------------------------------
run_experiment <- function(fn_name, dimensions, n_runs = 100) {
  # Inicjalizacja funkcji
  fn <- switch(fn_name,
               "Alpine01" = makeAlpine01Function(dimensions),
               "Schwefel" = makeSchwefelFunction(dimensions))
  
  domain <- get_domain(fn)
  true_min <- ifelse(fn_name == "Alpine01", 0, -418.9829 * dimensions)
  
  # PRS
  prs_results <- replicate(n_runs, performPRS(1000, fn, domain))
  
  # GA
  ga_results <- performGA(n_runs, 1000, fn, domain)
  
  list(
    prs = prs_results,
    ga = ga_results,
    stats = data.frame(
      Function = fn_name,
      Dimensions = dimensions,
      PRS_mean = mean(prs_results),
      GA_mean = mean(ga_results),
      True_min = true_min
    )
  )
}

# 6. Uruchomienie wszystkich eksperymentów --------------------------------------------------
results <- list(
  run_experiment("Alpine01", 2),
  run_experiment("Alpine01", 10),
  run_experiment("Alpine01", 20),
  run_experiment("Schwefel", 2),
  run_experiment("Schwefel", 10),
  run_experiment("Schwefel", 20)
)

# 7. Wizualizacja ---------------------------------------------------------------------------
# Funkcje do tworzenia wykresów
visualize_histogram <- function(data, true_min) {
  ggplot(data, aes(x = value, fill = algorithm)) +
    geom_histogram(position = "identity", alpha = 0.7, bins = 30) +
    geom_vline(xintercept = true_min, color = "red", linetype = "dashed") +
    facet_wrap(~algorithm, ncol = 1) +
    labs(title = paste("Histogram wyników dla", unique(data$function_dim)),
         x = "Wartość funkcji", y = "Częstotliwość") +
    theme_bw()
}

visualize_violin <- function(data, true_min) {
  ggplot(data, aes(x = algorithm, y = value, fill = algorithm)) +
    geom_violin(trim = FALSE, show.legend = FALSE) +
    geom_boxplot(width = 0.1, fill = "white", outlier.size = 1) +
    geom_hline(yintercept = true_min, color = "red", linetype = "dashed") +
    labs(title = paste("Wykres skrzypcowy dla", unique(data$function_dim)),
         x = "Algorytm", y = "Wartość funkcji") +
    theme_bw() +
    theme(legend.position = "none")
}

# Generowanie wszystkich wykresów
for(res in results) {
  fn_dim <- paste0(res$stats$Function, " ", res$stats$Dimensions, "D")
  df <- data.frame(
    function_dim = fn_dim,
    algorithm = rep(c("PRS", "GA"), each = length(res$prs)),
    value = c(res$prs, res$ga)
  )
  print(visualize_histogram(df, res$stats$True_min))
  print(visualize_violin(df, res$stats$True_min))
}

# 8. Prezentacja wyników --------------------------------------------------------------------
stats_df <- do.call(rbind, lapply(results, function(x) x$stats))
stats_df %>%
  mutate(
    PRS_mean = round(PRS_mean, 3),
    GA_mean = round(GA_mean, 3),
    Difference = round(abs(PRS_mean - GA_mean), 3),
    True_min = round(True_min, 3)
  ) %>%
  kable(align = "c", caption = "Porównanie wyników") %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  add_header_above(c(" " = 2, "Średnie" = 2, " " = 2))