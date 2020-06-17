library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(ggplot2)


## Download
## https://download.bls.gov/pub/time.series/cu/
## cu.data.0.Current
## https://www.bls.gov/cex/csxgloss.htm

## CUSR0000SAH => Housing
## CUSR0000SAM2 => Medical care
## CUUR0000SERG02 => Recreational books
## CUUR0000SSEA011 => College textbooks
## CUURD000SA0LE => CPI (everything but energy)


wdths <- fwf_widths(c(17, 4, 3, 12, 10)+1,
                    col_names = c("series_id", "year", "period", "value", "footnotes"))
df <- read_fwf("https://download.bls.gov/pub/time.series/cu/cu.data.0.Current", col_positions = wdths, skip = 1)


df2 <- df %>%
    filter(str_detect(series_id, "^CUUR0000SSEA011$|^CUUR0000SERG02$|^CUSR0000SAM$|^CUSR0000SAH$|^CUURD000SA0LE$")) %>%
    mutate(month = month(period(month = str_match(period, "[\\d]{2}"), units = "month"))) %>%
    filter(month < 13) %>%
    mutate(Date = ymd(paste(year, month, "01", sep="-")),
           Series = factor(series_id,
                           labels = c("Housing", "Medical care",
                                      "Recreational books", "College textbooks",
                                      "CPI (no energy)")))


df2 %>%
    filter(Date >= min(df2 %>% filter(series_id == "CUUR0000SSEA011") %>% pull(Date))) %>%
    group_by(Series) %>%
    mutate(pchange = 100 * (value / value[1] - 1)) %>%
    ggplot(aes(Date, pchange, color = Series)) +
    geom_line(size = 1.5) +
    labs(y = "Percent change from 2002") +
    theme_minimal() +
    theme(text = element_text(size=18))

ggsave("textbookprices.pdf")
