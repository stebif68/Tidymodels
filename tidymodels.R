### Esempio Tidymodels
### S.B. 25.01.2023

options(scipen = 999)


## libraries to be used 
library(ISLR2)
library(tidymodels)
tidymodels_prefer(quiet=F)

## how does initial_split work?
?initial_split()

## set seed for reproducibility
set.seed(3)

## step 1 - sampling - no strata

auto_split <- initial_split(
  Auto,
  prop = 0.5,
  # stratification by outcome variable
  #strata = mpg
)


## check distribution and quantiles 
Auto %>% 
  mutate(Q=ntile(mpg,4))%>% 
  group_by(Q) %>% 
  mutate(Q1=ifelse(Q<4,max(mpg),min(mpg)))%>% 
  ggplot(.,aes(mpg))+
  geom_line(stat = "density")+
  geom_vline(aes(xintercept=Q1),
            color="blue", 
            linetype="dashed", 
            size=.2)



## new sampling using strata
set.seed(3)
auto_split <- initial_split(
  Auto,
  prop = 0.5,
  # stratification by outcome variable
  strata = mpg
)


## create training data

auto_training <- auto_split %>%
  training()



## create testing data
auto_test <- auto_split %>%
  testing()



## count record in training
nrow(auto_training)



## define model, package & type of response

lm_spec <- linear_reg() %>%
  set_engine(engine = "lm") %>%
  set_mode(mode = "regression")

## fit the model after defining the formula

m <- fit(
  lm_spec, # parsnip model spec
  mpg ~ horsepower, # formula
  auto_training # data frame
)


## visualize output
m


## fancy output
tidy(m) %>% 
  knitr::kable(.,digits = 2, caption = 'Parametri Stimati')


## using fitting model to get prediction (in test dataset)

lm_pred <- m %>%
  predict(new_data = auto_test)


## look at first 6 results
head(lm_pred) %>% 
  knitr::kable(caption = 'Predizioni',digits=.1)


## add prediction to original data

auto_test_res <- auto_test %>%
  select(mpg, horsepower) %>%
  bind_cols(lm_pred)

head(auto_test_res) %>% 
  knitr::kable(caption='Stima e Variabile Originale',digits=c(0,0,1))



## calculate root mean square error

auto_test_res %>%
  rmse(truth = mpg, estimate = .pred)


## calculate r-square
auto_test_res %>%
  rsq(truth = mpg, estimate = .pred)

## Plot predicted vs observed

auto_test_res %>%
  ggplot(aes(x = mpg, y = .pred)) +
  geom_point(alpha = .5) +
  geom_abline(color = "blue", linetype = 2) +
  coord_obs_pred() +
  labs(x = "MPG", y = "Predicted MPG")

## what does coord_obs_pred do?
?coord_obs_pred

## Resampling (k fold cross-validation)
set.seed(123)

folds <- vfold_cv(Auto, v = 10,strata = mpg)

## fit model to samples
fit_resamples(lm_spec,mpg ~ horsepower,resamples=folds)-> res

res %>% 
  collect_metrics() %>% 
  knitr::kable(caption='Statistiche Cross-Validation')
  



## repeated CV
## be careful it takes a little bit longer


set.seed(123)

resall<-NULL
for (i in seq(1:5)){
  
  folds <- vfold_cv(Auto, v = 10,
                    strata = mpg, 
                    repeats =i)
  fit_resamples(lm_spec,
                mpg ~ horsepower,
                resamples=folds)-> res
  res %>% 
  collect_metrics()-> res1
  bind_rows(resall,res1)-> resall
}

## Plot standard error of the mean error by number of replicates
resall %>% 
  filter(.metric=='rmse') %>% 
  mutate(Repliche=n/10) %>% 
  select(Repliche,std_err) %>% 
  ggplot(aes(Repliche,std_err))+
  geom_line()

