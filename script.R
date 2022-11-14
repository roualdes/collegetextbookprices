library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(forcats)
library(ggplot2)


## Data taken from
## https://download.bls.gov/pub/time.series/cu/cu.data.0.Current
## script will download, but not save the data to file
## Glossary
## https://www.bls.gov/cex/csxgloss.htm

## CUUR0000SAH => Housing
## CUUR0000SAM => Medical care
## CUUR0000SERG02 => Recreational books
## CUUR0000SSEA011 => College textbooks
## CUUR0000SA0LE => CPI (everything but energy)

wdths <- fwf_widths(c(17, 4, 3, 12, 10)+1,
                    col_names = c("series_id", "year", "period", "value", "footnotes"))
df <- read_fwf("https://download.bls.gov/pub/time.series/cu/cu.data.0.Current",
               col_positions = wdths,
               skip = 1)

df2 <- df %>%
    filter(series_id %in% c("CUUR0000SAH", "CUUR0000SAM", "CUUR0000SERG02", "CUUR0000SSEA011", "CUUR0000SA0LE")) %>%
    mutate(month = month(period(month = str_match(period, "[\\d]{2}"), units = "month"))) %>%
    filter(month < 13) %>%
    mutate(Date = ymd(paste(year, month, "01", sep="-")),
           Series = fct_recode(series_id,
                               "Housing" = "CUUR0000SAH",
                               "Medical care" = "CUUR0000SAM",
                               "Recreational books" = "CUUR0000SERG02",
                               "College textbooks" = "CUUR0000SSEA011",
                               "CPI (no energy)" = "CUUR0000SA0LE"))


df2 %>%
    filter(Date >= min(df2 %>% filter(series_id == "CUUR0000SSEA011") %>% pull(Date))) %>%
    group_by(Series) %>%
    mutate(pchange = 100 * (value / value[1] - 1)) %>%
    ggplot(aes(Date, pchange, color = Series)) +
    geom_line(size = 1.5) +
    labs(y = "Percent change from 2002") +
    theme_minimal() +
    theme(text = element_text(size=18))

ggsave("textbookprices.png", width = 12, height = 8)
