library(readr)
stack_overflow <- read_csv("survey_results_public.csv",
col_types = cols(Respondent = col_integer(),
Age = col_integer(), Age1stCode = col_integer()))


library(dplyr)
library(ggplot2)
library(forcats)


glimpse(stack_overflow)
stack_overflow <- stack_overflow %>% mutate(across(where(is_character),as_factor))

mean_comp_samp <- mean(stack_overflow$ConvertedComp, na.rm = TRUE)

mean_comp_samp <- stack_overflow %>%
  summarize(mean_compensation = mean(ConvertedComp,na.rm=TRUE)) %>%
  pull(mean_compensation)

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


std_error <- sd(so_boot_distn)

mean_comp_samp

mean_comp_hyp <- 110000

std_error

z_score <- (mean_comp_samp - mean_comp_hyp) /std_error

prop_child_samp <- stack_overflow %>%
  summarize(point_estimate = mean(Age1stCode == "child",na.rm = T)) %>%
  pull(point_estimate)

prop_child_hyp <- 0.35
std_error <- 0.0096028
z_score <- (prop_child_samp -  prop_child_hyp)/str_error

p_value <- pnorm(z_score,lower.tail = FALSE)


##############3
library(fst)
late_shipments <- read_fst(path = "/Users/sergionava/Documents/Estadística y Diseño Experimental_20221110_1646/Modulo 6. Pruebas de Hipótesis en R/Base de Datos Entrega fst/late_shipments.fst",
         as.data.table = T)

late_shipments <- read_fst(path = "/Users/sergionava/Documents/Estadística y Diseño Experimental_20221110_1646/Modulo 6. Pruebas de Hipótesis en R/Base de Datos Entrega fst/late_shipments.fst",
                           as.data.table = T)