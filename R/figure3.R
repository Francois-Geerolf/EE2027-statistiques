rm(list = ls())
setwd(here::here())
library(tidyverse)
library(eurostat)
library(scales)


# Download monthly HICP data
raw <- get_eurostat("namq_10_a10",
                            time_format = "date",
                            filters     = list(geo = c("FR", "DE", "IT", "ES", "EA21"),
                                               nace_r2 = "C",
                                               s_adj = "SCA",
                                               unit = "PD20_EUR"))


# Nominal wages --------

figure3 <- raw |>
  filter(!is.na(values)) |>
  rename(date = time) |>
  group_by(geo) |>
  filter(date >= as.Date("1996-01-01")) |>
  arrange(date) |>
  mutate(values = 100*values/values[1],
         geo = factor(geo, levels = c("FR", "DE", "IT", "ES", "EA21"),
                      labels = c("France", "Allemagne", "Italie", "Espagne", "Zone Euro")))  |>
  ungroup() |>
  select(geo, date, values)

write_csv2(figure3, file = "csv/figure3.csv")


ggplot(data = figure3) + geom_line(aes(x = date, y = values, linetype = geo)) + 
  theme_minimal() + xlab("") + ylab("") +
  scale_x_date(breaks = seq(1996, 2100, 5) %>% paste0("-01-01") %>% as.Date,
               labels = date_format("%Y")) +
  scale_y_log10(breaks = seq(90, 200, 5)) + 
  scale_color_identity() + 
  theme(legend.position = c(0.2, 0.8),
        legend.title = element_blank())


ggsave("pdf/figure3.pdf", width = 5, height = 3, device = cairo_pdf)
ggsave("png/figure3.png", width = 5, height = 3, bg = "white")



