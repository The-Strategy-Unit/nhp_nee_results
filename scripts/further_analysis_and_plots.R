# 0 set up ----
library(tidyverse)
library(here)
library(gridExtra)
library(reshape2)
library(RColorBrewer)

options(pillar.sigfig=4)
# This file produces summary statistics and three plots that are included in the report and paper 
# The plots are called
# Paper_Fig1_All.jpg
# Paper_Fig2_Boxplot.jpg
# Paper_Fig2_Boxplot.jpg

# 1 read results file ----
df <- as_tibble(read.csv(here('table_outputs', 'table1.csv'), header = TRUE))
df <- rename(df, p10=percentile10, p90=percentile90)
df$range <- df$p90-df$p10

# 2 summary statistics  ----
quantile(df$p10, seq(0,1,0.1))
quantile(df$p90, seq(0,1,0.1))
quantile(df$mean, seq(0,1,0.1))
quantile(df$range, seq(0,1,0.1))

# 2.1 summary statistics by groups  ----

df |> group_by (strategyLabel) |> 
  summarise(x=mean(mean), n=n()) |>
  arrange(x)


df |> group_by (strategyGroup, strategyType) |> 
      summarise(grp_mean=mean(mean), avg_range=mean(range), n=n()) |>
      arrange(grp_mean)

df |> group_by (strategyType) |> 
  summarise(x=mean(mean), y=mean(range), n=n()) |>
  arrange(x)

df |>
  group_by(strategyGroup) |>
  summarize(min_mean = min(mean), max_mean=max(mean), n=n())

# 3.1 Paper_Fig1_All.jpg  ----
df$strategyGroup <- as_factor(df$strategyGroup)

df$mycolors <- df$strategyGroup

df$mycolors <- "black"
df$mycolors [df$strategyGroup == levels(as_factor(df$strategyGroup))[1]] =brewer.pal(8,"Dark2")[1]
df$mycolors [df$strategyGroup == levels(as_factor(df$strategyGroup))[2]] =brewer.pal(8,"Dark2")[2]
df$mycolors [df$strategyGroup == levels(as_factor(df$strategyGroup))[3]] =brewer.pal(8,"Dark2")[3]
df$mycolors [df$strategyGroup == levels(as_factor(df$strategyGroup))[4]] =brewer.pal(8,"Dark2")[4]
df$mycolors [df$strategyGroup == levels(as_factor(df$strategyGroup))[5]] =brewer.pal(8,"Dark2")[5]
df$mycolors [df$strategyGroup == levels(as_factor(df$strategyGroup))[6]] =brewer.pal(8,"Dark2")[6]
df$mycolors [df$strategyGroup == levels(as_factor(df$strategyGroup))[7]] =brewer.pal(8,"Dark2")[7]
df$mycolors [df$strategyGroup == levels(as_factor(df$strategyGroup))[8]] =brewer.pal(8,"Dark2")[8]


df |>
  mutate(strategyLabel = fct_reorder(strategyLabel, -mean)) |>
  ggplot() +
  geom_segment(aes(x = p10, xend = p90,y = strategyLabel, yend = strategyLabel, color=strategyGroup))+
  geom_point(aes(x = mean, y = strategyLabel, color=strategyGroup)) +
  geom_vline(xintercept = 50, col='grey') +
  facet_grid (~strategyType) + 
  ylab("Hospital Activity") + xlab("Aggregate P10 to P90 forecasts") +
  scale_colour_brewer(palette = "Dark2") +
  theme(axis.text.y = element_text(color =  df |> arrange(desc(mean)) |> pull(mycolors))) +
  theme(legend.position="none") 

ggsave(filename = here('chart_outputs', 'Paper_Fig1_All.jpg'),
       width = 40, height = 20,
       units = 'cm',
       device = 'jpg')

# 3.2 Paper_Fig2_Boxplot.jpg  ----

ggplot(df, aes(x=reorder(strategyGroup, mean, FUN=median) , y=mean)) +
  geom_boxplot() + 
  coord_flip() + 
  xlab("Hospital Activity Group") + ylab("Mean of aggregate forecasts") # 700 x 900 on clipboard for paper

ggsave(filename = here('chart_outputs', 'Paper_Fig2_Boxplot.jpg'),
       width = 40, height = 20,
       units = 'cm',
       device = 'jpg')

# 3.3 Paper_Fig2_Boxplot.jpg  ----

ggplot(df, aes(x=reorder(strategyType, mean, FUN=median) , y=mean)) +
  geom_boxplot() + 
  coord_flip() + 
  xlab("Type of mitigation") + ylab("Mean aggregate forecasts") # 700 x 900 on clipboard for paper


ggsave(filename = here('chart_outputs', 'Paper_Fig3_Boxplot.jpg'),
       width = 40, height = 20,
       units = 'cm',
       device = 'jpg')
