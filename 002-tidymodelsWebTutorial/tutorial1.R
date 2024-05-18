library(tidyverse)
library(parsnip)
library(readr)
library(broom.mixed)

urchins <- read_csv("https://tidymodels.org/start/models/urchins.csv") %>% 
        setNames(
                c(
                        "food_regime",
                        "initial_volume",
                        "width"
                )
        ) %>% 
        mutate(
                food_regime = factor(food_regime, levels = c("Initial", "Low", "High"))
        )

urchins

urchins %>% 
        ggplot(aes(x = initial_volume, y = width, group = food_regime, col = food_regime)) + 
        geom_point() + 
        geom_smooth(method = lm, se = FALSE) +
        scale_color_viridis_d(option = "plasma", end = .7)

lm_mod <- linear_reg()

lm_fit <- lm_mod %>% 
        fit(
                width ~ initial_volume + food_regime,
                data = urchins
        )

lm_fit

tidy(lm_fit)

new_points <- expand.grid(
        initial_volume = 20,
        food_regime = c("Initial", "Low", "High")
)

mean_pred <- predict(
        lm_fit,
        new_data = new_points
)

mean_pred

conf_int_pred <- predict(
        lm_fit,
        new_data = new_points, 
        type = "conf_int"
)

conf_int_pred

prior_dist <- rstanarm::student_t(df = 1)

set.seed(123)

bayes_mod <- linear_reg() %>% 
        set_engine(
                "stan",
                prior_intercept = prior_dist,
                prior = prior_dist
        ) 

bayes_fit <- bayes_mod %>% 
        fit(
                width ~ initial_volume * food_regime,
                data = urchins
        )

print(bayes_fit, digits = 5)

tidy(bayes_fit, conf.int = TRUE)
