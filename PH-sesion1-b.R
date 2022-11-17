library(readr)
stack_overflow <- read_csv("survey_results_public.csv",
col_types = cols(Respondent = col_integer(),
Age = col_integer(), Age1stCode = col_integer()))


library(dplyr)
library(ggplot2)

library(rsample)
library(purrr)
library(tictoc)
glimpse(stack_overflow)

mean_comp_samp <- mean(stack_overflow$ConvertedComp, na.rm = TRUE)

mean_comp_samp <- stack_overflow %>%
  summarize(mean_compensation = mean(ConvertedComp,na.rm=TRUE)) %>%
  pull(mean_compensation)

tic()
print("version replicate")
so_boot_distn <- replicate(
  n = 5000,
  expr = {
    # Step 1. Resample
    stack_overflow %>% 
      slice_sample(prop = 1, replace = TRUE) %>%
      # Step 2. Calculate point estimate
      summarize(mean_compensation = mean(ConvertedComp,na.rm = TRUE)) %>%
      pull(mean_compensation)
  }
)


tibble(resample_mean = so_boot_distn) %>%
  ggplot(aes(resample_mean)) +
  geom_histogram(binwidth = 1000)
toc()
lobstr::obj_size(so_boot_distn)

tic()
print("version rsample")
so_boot_distn2 <- bootstraps(stack_overflow,times = 5000)
so_boot_distn3 <- map_dbl(
  so_boot_distn2$splits,
  function(x) {
    dat <- as.data.frame(x)$ConvertedComp
    mean(dat,na.rm = TRUE)
  }
)
hist(so_boot_distn3)
toc()
lobstr::obj_size(so_boot_distn2)
