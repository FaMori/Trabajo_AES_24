## RF para empezar

data <- datos_phijo %>% select(-'id',-'ponderador',-'anio',-'edad',-'psu',-'estrato',-'zona',-'edad_psexo')

datos_split <- initial_split(data, 
                             strata = pais,
                             prop = 0.85) 

datos_train <- training(datos_split)
datos_test <- testing(datos_split)

datos_recipe <- recipe(edad_phijo ~ ., data = datos_train)

## TEST SIN TUNEO
rf_workflow <- workflow() %>%
  add_model(rf_spec) %>%
  add_recipe(datos_recipe)

rf_fit <- rf_workflow %>% update_model(
  rand_forest(mtry = 12, min_n=500,  trees=500) %>%
    set_engine('ranger',importance='permutation',num.threads=6) %>% set_mode('regression')
) %>% fit(data = datos_train)

vip(rf_fit) + theme_minimal() + labs(y='Importancia', x='Variables')

## TUNEO
rf_spec <- rand_forest(mtry =tune(), min_n=tune(),  trees=tune()) %>%
  set_engine('ranger', num.threads=6) %>%
  set_mode('regression')

rf_workflow <- workflow() %>%
  add_model(rf_spec) %>%
  add_recipe(datos_recipe)

rf_grid <- expand.grid(mtry = c(2,4,6,8,10,12,14,16),
                       min_n=c(50,100,150,200,250,300,350,400,450,500, 1000,1500,2000,2500,5000),
                       trees=c(10,50,100,250,500,1000))

rf_cv <- vfold_cv(datos_train, v = 10)

rf_tune <- tune_grid(rf_workflow, resamples = rf_cv, grid = rf_grid)

rf_best <- rf_tune %>% select_best(metric='rmse')

```{r rf_vip}
#| label: fig-rf_vip
#| fig-cap: "Gráfico barras, importancia de las variables en el modelo de RF"
#| fig-align: center
#| fig-height: 5
#| fig-width: 8

vip(rf_fit) + theme_minimal() + labs(y='Importancia', x='Variables')
```

