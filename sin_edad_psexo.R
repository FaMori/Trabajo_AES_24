## Saco edad_psexo y sin tuneo

datos_split <- initial_split(datos_phijo, 
                             strata = pais,
                             prop = 0.85) 

datos_train <- training(datos_split)
datos_test <- testing(datos_split)

datos_recipe <- recipe(edad_phijo ~ pais + urbano + anio_educ + leer + ind_riqueza + con_antic + n_parejas, data = datos_train) %>%
  step_normalize(all_numeric_predictors())

## BAGGING
bagging_spec <- rand_forest(mtry =.cols(), min_n=130,  trees=1000) %>%
  set_engine('ranger', importance = "permutation", num.threads=6) %>%
  set_mode('regression')

bagging_workflow <- workflow() %>%
  add_model(bagging_spec) %>%
  add_recipe(datos_recipe)

bagging_fit <- fit(bagging_workflow, data = datos_train)

vip(bagging_fit) + theme_minimal()

## RF
rf_spec <- rand_forest(mtry =5, min_n=130,  trees=2000) %>%
set_engine('ranger', importance = "permutation", num.threads=6) %>%
  set_mode('regression')

rf_workflow <- workflow() %>%
  add_model(rf_spec) %>%
  add_recipe(datos_recipe)

rf_fit <- fit(rf_workflow, data = datos_train)

vip(rf_fit) + theme_minimal()
