# 0 set up ----
library(tidyverse)
library(here)
library(truncnorm)
library(scales)

# 1 read in files ----
# read cleaned round 2 data only
nee_results <- read.csv(here('data_raw', 'Round2CleanedData.csv'), header = TRUE)
# read expert labels
expertLabels <- read.csv(here('data_raw', 'expert_labels.csv'), header = TRUE)
# read in strategy label and grouping variables for strategies
strategy_lookup <- read.csv(here('data_raw', 'strategy_lookup.csv'), header = TRUE)

# some of the filtering steps below are legacy from a previous raw data input file
# which do not apply to the cleaned data file 
# but code is kept as a reminder of what was removed
nee_results_r2 <- nee_results |> 
  filter(lo != hi) |> 
  filter(!(lo == 0 & hi == 100)) |> 
  filter(!is.na(lo)) |> 
  filter(!is.na(hi)) |> 
  filter(which_phase == 2) |> 
  dplyr::select(-timestamp, -comments_lo, -comments_hi) 

# 2 produce table 1 ----
# parameters by group and the number of expert opinions
# later we update table1 with distribution parameters for each group see table2 code later
strategy_expert_counts <- nee_results_r2 |> 
  group_by(strategy, strategyGroup, strategyLabel, strategyType) |> 
  summarise(experts_main = n()) |> 
   dplyr::select(strategyGroup,
                strategyType,
                strategy,
                strategyLabel,
                experts_main) 

table1 <- strategy_expert_counts |> 
  ungroup() |> 
  dplyr::select(strategyGroup,
                strategyType,
                strategyLabel,
                experts_main) |> 
  arrange(strategyGroup,
          strategyLabel)


# 3 aggregate results ----
# derive mean and sd form each experts' 80% CI 
nee_distParam_r2 <- nee_results_r2 |> 
  mutate(mu = (hi + lo) / 2) |> 
  mutate(sigma = (hi - mu) / qnorm(0.9, 0, 1)) |> 
  arrange(strategy, lo, hi) |> 
  group_by(strategy) |>   
  mutate(expert_id = sequence(n())) |> 
  dplyr::select(strategy, which_phase, lo, hi, mu, sigma, expert_id, expertLabel)


# 4 main analysis ----
# (round 2 results only) 

# create dataframe to hold bootstrapped values and empirical cdf
parameter_bootstrap_values <-
  data.frame(activity_subset = character(),
             value = numeric())


empirical_parameter_cdfs <-
  data.frame(activity_subset = character(),
             percentile = numeric(),
             value = numeric())

set.seed(2208)

# loop over each activity subset
for (i in (1:nrow(strategy_expert_counts))) 
{
  activity_subset <- strategy_expert_counts[[i, 3]]
  experts <- strategy_expert_counts[[i, 5]]
  
  sampled_values <- data.frame(activity_subset = character(),
                               value = numeric())
  
  # loop over each expert who offered parameters for this activity subset
  for (j in (1:experts))
    # draw random values from the truncated normal distribution 
    # min 0, max 100, with mean and sd from expert
    # 100000 values for each parameter
  {
    a <- nee_distParam_r2 |> 
      filter(strategy == activity_subset) |> 
      filter(expert_id == j)
    
    mu <- pull(a, mu)
    
    sigma <- pull(a, sigma)
  
    sampled_values <- 
      sampled_values |> 
      bind_rows(data.frame(activity_subset = activity_subset,
                 value = rtruncnorm(round(100000 / experts), 
                    a = 0, b = 100, 
                    mean = mu, sd = sigma)))
    
  }
  
      parameter_bootstrap_values <-
      parameter_bootstrap_values |> 
      bind_rows(sampled_values)
  
  # calculate the percentile values from these lists
  for (k in (0:100)) 
  {
    empirical_parameter_cdfs<-
      empirical_parameter_cdfs |>  
      bind_rows(data.frame(strategy = strategy_expert_counts[[i, 3]],
                           strategyLabel = strategy_expert_counts[[i, 4]],
                           strategyGroup = strategy_expert_counts[[i, 1]],
                           strategyType = strategy_expert_counts[[i, 2]],
                           percentile = k/100,
                           value = quantile(sampled_values$value, k/100)))
    
  }
  
}


# 5 produce table 2 ----
# mean, 10% and 90% percentiles 

table2 <- empirical_parameter_cdfs |> 
  filter(percentile == 0.1 | percentile == 0.9) |> 
  mutate(percentile = 100 * percentile) |> 
  pivot_wider(names_from = 'percentile', 
              values_from = 'value',
              names_prefix = 'percentile') |> 
  left_join(parameter_bootstrap_values |> 
              rename(strategy = activity_subset) |> 
              group_by(strategy) |> 
              summarise(mean = mean(value),sd=sd(value)),
            join_by(strategy)) |> 
  dplyr::select(strategyGroup, strategyType, strategyLabel, 
                mean, sd, percentile10, percentile90)

# join table 1 (which has N of experts count) and table2
table1 <- merge(table1,table2, by=c("strategyGroup","strategyType","strategyLabel"))

write.csv(table1, file = here('table_outputs', 'table1.csv'), row.names = FALSE)



# 6 plot the results ----

# 6.1 from main analysis ----
plot_groups_main <- strategy_lookup |> 
  inner_join(nee_distParam_r2,
             join_by(strategy)) |> 
  dplyr::select(strategyType, strategyGroup) |> 
  unique()


for (i in (1:nrow(plot_groups_main))) 
  
  {

# plot the parameter pdfs
  parameter_bootstrap_values |> 
    left_join(strategy_lookup, 
              join_by(activity_subset == strategy)) |> 
    filter(strategyGroup == plot_groups_main[[i, 2]]) |> 
    filter(strategyType == plot_groups_main[[i, 1]]) |> 
    ggplot() +
    geom_hline(aes(yintercept = 0),
               colour = 'grey') +
    geom_freqpoly(aes(x = value),
                  bins = 50)  +
    facet_wrap(~strategyLabel,
               labeller = label_wrap_gen(width = 40)) +
    scale_x_continuous(limits = c(0, 100)) +
    labs(title = paste(plot_groups_main[[i, 2]], 
                       plot_groups_main[[i, 1]], sep = ' : '),
         subtitle = 'empirical parameter pdfs',
         y = 'probability density',
         x = '% reduction') +
    theme(axis.text.y = element_blank())

  ggsave(filename = here('chart_outputs', 
                       paste('pdfs-Main-', 
                             plot_groups_main[[i, 2]], '-', 
                             plot_groups_main[[i, 1]], 
                        '.jpg',
                        sep = '')),
       width = 27, height = 20,
       units = 'cm',
       device = 'jpg')

  # plot the parameter cdfs
  empirical_parameter_cdfs |> 
    filter(strategyGroup == plot_groups_main[[i, 2]]) |> 
    filter(strategyType == plot_groups_main[[i, 1]]) |> 
    ggplot() +
    geom_hline(aes(yintercept = 0),
               colour = 'grey') +
    geom_hline(aes(yintercept = 1),
               colour = 'grey') +
    geom_line(aes(x = value,
                  y = percentile)) +
    facet_wrap(~strategyLabel,
               labeller = label_wrap_gen(width = 40)) +
    scale_y_continuous(label = percent_format()) +
    labs(title = paste(plot_groups_main[[i, 2]], 
                       plot_groups_main[[i, 1]], sep = ' : '),
         subtitle = 'empirical parameter cdfs',
         y = 'percentile',
         x = '% reduction')

  ggsave(filename = here('chart_outputs', 
                         paste('cdfs-Main ', 
                               plot_groups_main[[i, 2]], '-', 
                               plot_groups_main[[i, 1]], 
                               '.jpg',
                               sep = '')),
                       width = 27, height = 20,
                       units = 'cm',
                       device = 'jpg')

}


# 7 visualisation of expert views and aggregation ----

rangeChartData <- nee_distParam_r2 |> 
  dplyr::select(strategy, hi, lo,
                expertLabel) |> 
  mutate(rangeType = 'expert') |> 
  bind_rows(empirical_parameter_cdfs |> 
              filter(percentile == 0.1 | percentile == 0.9) |> 
              mutate(percentile = 100 * percentile) |> 
              pivot_wider(names_from = 'percentile', 
                          values_from = 'value',
                          names_prefix = 'percentile') |> 
              dplyr::select(strategy,
                            lo = percentile10, 
                            hi = percentile90) |> 
              mutate(rangeType = 'aggregate',
                     expertLabel = 'agg')) |> 
    left_join(strategy_lookup,
            join_by(strategy))




for (i in (1:nrow(plot_groups_main))) 
  
{
  
  # plot the parameter pdfs
  rangeChartData |> 
    filter(strategyGroup == plot_groups_main[[i, 2]]) |> 
    filter(strategyType == plot_groups_main[[i, 1]]) |> 
    ggplot() +
    geom_segment(aes(x = lo, xend = hi, 
                     y = expertLabel, yend = expertLabel,
                     colour = rangeType)) +
    facet_wrap(~strategyLabel, 
               scales = 'free_y',
               labeller = label_wrap_gen(width = 40)) +
    scale_x_continuous(limits = c(0, 100)) +
    theme(legend.title = element_blank(),
          legend.position = 'bottom') +
    labs(title = paste(plot_groups_main[[i, 2]], 
                       plot_groups_main[[i, 1]], sep = ' : '),
         subtitle = 'expert views (80% confidence intervals)',
         y = 'expert',
         x = '% reduction')
                   

  
  ggsave(filename = here('chart_outputs', 
                         paste('ranges-main-', 
                               plot_groups_main[[i, 2]], '-', 
                               plot_groups_main[[i, 1]], 
                               '.jpg',
                               sep = '')),
         width = 27, height = 20,
         units = 'cm',
         device = 'jpg')


}

# 8 output ecdfs to rds and csv files ----

write.csv(empirical_parameter_cdfs, file = here('table_outputs', 'empirical_parameter_cdfs.csv'),
          row.names = FALSE)

saveRDS(empirical_parameter_cdfs, file = here('table_outputs', 'empirical_parameter_cdfs.RDS'))
