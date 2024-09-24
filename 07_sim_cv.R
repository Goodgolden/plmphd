library(here, quietly = TRUE)
library(tidyverse, quietly = TRUE)
devtools::load_all()

seed <- 10
set.seed(seed)

## simulate data -------------------------------------------------------------
Nid <- rpois(20000, 20)
Ntime <- map(Nid, ~rnorm(.x, 0.8, 0.3) %>%
               abs() %>%
               as.data.frame())

data0 <- cbind(id = 1:20000,
               group = rep(c("training", "testing"), by = 10000),
               gap = Ntime,
               sex = rep(c("male", "female"), each = 10000)) %>%
  as.data.frame() %>%
  mutate(hmax = c(rnorm(10000, 175, 10),
                  rnorm(10000, 160, 11)),
         hd = c(rnorm(10000, 20, 4),
                rnorm(10000, 20, 6.5)),
         rate = runif(20000, 5.8, 11),
         s1 = c(abs(rnorm(10000, 0.85, 0.1)),
                abs(rnorm(10000, 0.75, 0.1))),
         theta = c(runif(10000, 11, 17),
                   runif(10000, 10, 15))) %>%
  mutate(s0 = s1 / rate,
         h3 = hmax - 2 * hd / (exp(s0 *(3 - theta)) + exp(s1 * (3 - theta))),
         h10 = hmax - 2 * hd/ (exp(s0 *(10 - theta)) + exp(s1 * (10 - theta))),
         h20 = hmax - 2 * hd / (exp(s0 *(20 - theta)) + exp(s1 * (20 - theta))),
         sex = unlist(sex),
         group = unlist(group),
         id = as.character(unlist(id))) %>%
  filter(h3 > 80,
         h3 < 106,
         hd > 15,
         h10 > 100) %>%
  group_by(group, sex) %>%
  slice_sample(n = 250L) %>%
  ungroup()
simulation <- data0 %>%
  unnest(gap) %>%
  dplyr::select(gap = 3, everything()) %>%
  group_by(id) %>%
  mutate(time = lag(cumsum(gap), default = 0) + 3) %>%
  ungroup() %>%
  filter(time <= 20) %>%
  mutate(resid = rnorm(n(), 0, 1),
         ht = hmax - 2 * hd /
           (exp(s0 *(time - theta)) + exp(s1 * (time - theta))) + resid) %>%
  as.data.frame() %>%
  mutate(time1 = as.numeric(time)) %>%
  mutate(time = round(time1, 1)) %>%
  dplyr::select(ht, id, sex, time, group)

remove(data0)
remove(Nid)
remove(Ntime)

## people like me method -----------------------------------------------------
bsk = c(3, 6, 9, 12)

train <- dplyr::filter(simulation, group == "training")

train1 <- train %>%
  group_by(id) %>%
  summarise(ht0 = min(ht))

train2 <- full_join(train, train1, by = "id") %>%
  mutate(sid = "S") %>%
  unite(id, sid, id, sep = "")

test <- dplyr::filter(simulation, group == "testing")

test1 <- test %>%
  group_by(id) %>%
  summarise(ht0 = min(ht))

test2 <- full_join(test, test1, by = "id") %>%
  mutate(sid = "S") %>%
  unite(id, sid, id, sep = "")

id_test <- unique(test2$id)

lf <- "ht ~ sex + sex:ht0 +
               + ht0"

gf <- "ht ~ cs(time, df = 3)"
gs <- "~ cs(time, df = 1)"

plm_s_n30 <- map(15:1, ~try(plm_single(train_data = train2,
                                   test_data = test2,
                                   outcome_var = "ht",
                                   time_var = "time",
                                   id_var = "id",
                                   tmin = 3,
                                   tmax = 17,
                                   brokenstick_knots = bsk,
                                   anchor_time = .,
                                   linear_formula = lf,
                                   gamlss_formula = gf,
                                   gamlss_sigma = gs,
                                   match_number = 30,
                                   match_plot = FALSE,
                                   predict_plot = FALSE)) %>%
                   suppressMessages(),
                 .progress = TRUE)
save(plm_s_n30,
     file = paste0("results/simcv_", seed,
                   "_single_time_n30_time1_15_",
                   Sys.Date(),
                   ".rdata"))


plm_s_t11 <- map(1:50, ~plm_single(train_data = train2,
                                   test_data = test2,
                                   outcome_var = "ht",
                                   time_var = "time",
                                   id_var = "id",
                                   tmin = 3,
                                   tmax = 17,
                                   brokenstick_knots = bsk,
                                   anchor_time = 11,
                                   linear_formula = lf,
                                   gamlss_formula = gf,
                                   gamlss_sigma = gs,
                                   match_number = .,
                                   match_plot = FALSE,
                                   predict_plot = FALSE) %>%
                   suppressMessages(),
                 .progress = TRUE)



save(plm_s_t11,
     file = paste0("results/simcv_", seed,
                   "_single_time_n1:50_time11_",
                   Sys.Date(),
                   ".rdata"))
