library(stringr)
library(DALEXtra)

explainer <- explain_tidymodels(rf_fit,
                                data = select(datos_train, -edad_phijo),
                                y = datos_train$edad_phijo)

## Años educacion
pdp_time <- model_profile(explainer,
                          variables="anio_educ")

as_tibble(pdp_time$agr_profiles) %>%
  mutate(`_label_` = str_remove(`_label_`, "workflow_")) %>%
  ggplot(aes(`_x_`, `_yhat_`, color = `_label_`)) +
  geom_line(size = 1.2, alpha = 0.8) +
  labs(
       x = "Años de educación",
       y = "Edad a la que tuve el primer hijo",
       color = NULL,
       title = "Partial dependence plot",
       subtitle = "Prediciones de Random Forest"
      )

## Riqueza
pdp_time_riqueza <- model_profile(explainer,
                          variables="ind_riqueza")

as_tibble(pdp_time_riqueza$agr_profiles) %>%
  mutate(`_label_` = str_remove(`_label_`, "workflow_")) %>%
  ggplot(aes(`_x_`, `_yhat_`, color = `_label_`)) +
  geom_line(size = 1.2, alpha = 0.8) +
  labs(
    x = "Indice de riqueza",
    y = "Edad a la que tuve el primer hijo",
    color = NULL,
    title = "Partial dependence plot",
    subtitle = "Prediciones de Random Forest"
  )

## Parejas
pdp_time_parejas <- model_profile(explainer,
                                  variables="n_parejas")

as_tibble(pdp_time_parejas$agr_profiles) %>%
  mutate(`_label_` = str_remove(`_label_`, "workflow_")) %>%
  ggplot(aes(`_x_`, `_yhat_`, color = `_label_`)) +
  geom_line(size = 1.2, alpha = 0.8) +
  labs(
    x = "Número de parejas",
    y = "Edad a la que tuve el primer hijo",
    color = NULL,
    title = "Partial dependence plot",
    subtitle = "Prediciones de Random Forest"
  )
