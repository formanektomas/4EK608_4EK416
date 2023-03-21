# dickey fuller distribution ----------------------------------------------
library(tibble)
library(purrr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(dyn)

set.seed(125)

# Function returning random walk ------------------------------------------

generate_series <- function(y0 = 1,
                            a = 1,
                            N = 100) {
  error <- rnorm(N, 0, 1)
  y <- c()
  y[1] <- y0

  for (i in 2:N) {
    y[i] <- a * y[i - 1] + error[i]
  }

  return(tibble(t = 1:N, y))
}

# experiment into function for convenience --------------------------------

runExperiment <- function(replications, N) {
  experiment <- tibble(
    id = 1:replications,
    data = map(1:replications, ~ generate_series(N = N))
  ) %>%
    mutate(model = map(.x = data, ~ dyn$lm(c(NA, diff(
      .x$y, 1
    )) ~ lag(.x$y, 1)))) %>%
    mutate(summaries = map(model, broom::tidy))

  return(experiment)
}

# Running the experiment --------------------------------------------------

N <- 100 # Random walk length
replications <- 1e4

df <- runExperiment(replications, N = 100)

head(df)

# for replications = 1 000, the nested tibble (df) contains:
# 1 000 different random walks of length 100 in 'data' column
# 1 000 dyn objects with estimated model for DF test in 'model' column
# 1 000 model summaries incl. estimates, t-stats., etc... in 'summaries' column

head(df)

df %>%
  unnest(data)

df %>%
  unnest(summaries)

# RW plot -----------------------------------------------------------------

df %>%
  unnest(data) %>%
  select(id, t, y) %>%
  group_by(id) %>%
  ggplot(aes(x = t, y, color = factor(id))) +
  geom_line() +
  theme(legend.position = "none") # otherwise you'll see only the legend...

# Simulated Dickey-fuller distribution ------------------------------------

df %>%
  unnest(summaries) %>%
  filter(!term == "(Intercept)") %>%
  ggplot(aes(statistic)) +
  geom_density(color = "red2") +
  labs(
    title = "Simulated Dickey Fuller distribution",
    subtitle = paste(replications, "replications")
  ) +
  theme_light()

# Sample quantiles --------------------------------------------------------

df %>%
  unnest(summaries) %>%
  filter(!term == "(Intercept)") %>%
  group_by(term) %>%
  summarize(
    lower = quantile(probs = 0.05, statistic),
    upper = quantile(probs = 0.95, statistic)
  )
