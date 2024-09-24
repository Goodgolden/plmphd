library(here, quietly = TRUE)
library(tidyverse, quietly = TRUE)
devtools::load_all()

load("~/Desktop/plmphd/R/train_test.rda")
train1 <- train %>%
  group_by(id) %>%
  summarise(ht0 = min(ht),
            age0 = min(age))

train2 <- full_join(train, train1, by = "id") %>%
  mutate(sid = "S") %>%
  unite(id, sid, id, sep = "")

test1 <- test %>%
  group_by(id) %>%
  summarise(ht0 = min(ht),
            age0 = min(age))

test2 <- full_join(test, test1, by = "id") %>%
  mutate(sid = "S") %>%
  unite(id, sid, id, sep = "")

bsk <- c(3, 6, 9, 12)
at <- 10
mn <- 15

lf <- "ht ~ sex + sex:ht0 +
              genotype + genotype:ht0 +
              ethnic + ethnic:ht0 + ht0"

gf <- "ht ~ cs(time, df = 3)"
gs <- "~ cs(time, df = 1)"

plm_s_n30 <- map(1:15, ~plm_single(train_data = train2,
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
                                    predict_plot = FALSE) %>%
                   suppressMessages(),
                 .progress = TRUE)
save(plm_s_n30,
     file = paste0("results/plm_single_time_n30_time1_15_",
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
     file = paste0("results/plm_single_time_n1:50_time11_",
                   Sys.Date(),
                   ".rdata"))
