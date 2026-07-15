rm(list = ls())
setwd(here::here())
library(tidyverse)
library(eurostat)
library(scales)

raw <- get_eurostat("prc_hicp_minr",
                    time_format = "date",
                    filters     = list(geo = c("FR", "DE", "IT", "ES"),
                                       unit = "I25",
                                       coicop18 = "CP0711"))

figure1 <- raw |>
  rename(date = time) |>
  group_by(geo) |>
  filter(date >= as.Date("1996-01-01")) |>
  arrange(date) |>
  mutate(values = 100*values/values[1],
         geo = factor(geo, levels = c("FR", "DE", "IT", "ES"),
                      labels = c("France", "Allemagne", "Italie", "Espagne")))  |>
  ungroup() |>
  select(geo, date, values)

write_csv2(figure1, file = "csv/figure1.csv")


ggplot(data = figure1) + geom_line(aes(x = date, y = values, linetype = geo)) + 
  theme_minimal() + xlab("") + ylab("") +
  scale_x_date(breaks = seq(1996, 2100, 2) %>% paste0("-01-01") %>% as.Date,
               labels = date_format("%Y")) +
  scale_y_log10(breaks = seq(90, 200, 5)) + 
  scale_color_identity() + 
  theme(legend.position = c(0.3, 0.8),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1))


ggsave("pdf/figure1.pdf", width = 5, height = 3, device = cairo_pdf)
ggsave("png/figure1.png", width = 5, height = 3, bg = "white")




