library(readxl)
library(magrittr)
library(dplyr)
source('dcf_calculator.R')
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, na.rm = na.rm, ...)
  H <- 1 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

ranking_data <- read_excel('Ranking con mas data.xlsx')
names(ranking_data)

ranking_data <- ranking_data %>%
  
  filter(!is.na(`GICS Sector`)) %>%
  mutate(dcf_growth_rate =`cagr eps 10 years` / 100) %>%
  mutate( trail_12m_net_inc =trail_12m_net_inc * 1000000) %>% 
  group_by(`GICS Sector`) %>% 
  arrange(`Ranking`)


# mutate( dcf_growth_rate =ifelse(`cagr eps 10 years` > .20,.20,`cagr eps 10 years`)) %>%
# mutate( dcf_growth_rate =ifelse(dcf_growth_rate < .05,.05,dcf_growth_rate))


ranking_data$MarginOfSafety <- NA
for(iLoop in 1: NROW(ranking_data)){
  curr_mos_row <- ranking_data[iLoop,,drop = FALSE]
  ranking_data[iLoop,'MarginOfSafety'] <- 
    dcf_calc(price =  curr_mos_row$`Market Cap`,
             eps = curr_mos_row$trail_12m_net_inc ,
             dcf.discount.rate = .10,
             growth.rate = curr_mos_row$dcf_growth_rate,
             years.growth = 10,
             stable.growth = .04,
             years.stable = 10)
}

ranking_data$MarginOfSafety2 <- NA
for(iLoop in 1: NROW(ranking_data)){
  curr_mos_row <- ranking_data[iLoop,,drop = FALSE]
  ranking_data[iLoop,'MarginOfSafety2'] <- 
    dcf_calc(price =  curr_mos_row$`Market Cap`,
             eps = curr_mos_row$trail_12m_net_inc ,
             dcf.discount.rate = .10,
             growth.rate = curr_mos_row$dcf_growth_rate,
             years.growth = 5,
             stable.growth = .04,
             years.stable = 15)
}


ranking_data$MarginOfSafety3 <- NA
for(iLoop in 1: NROW(ranking_data)){
  curr_mos_row <- ranking_data[iLoop,,drop = FALSE]
  ranking_data[iLoop,'MarginOfSafety3'] <- 
    dcf_calc(price =  curr_mos_row$`Market Cap`,
             eps = curr_mos_row$trail_12m_net_inc ,
             dcf.discount.rate = .10,
             growth.rate = curr_mos_row$dcf_growth_rate / 2,
             years.growth = 10,
             stable.growth = .04,
             years.stable = 10)
}



ranking_data <- ranking_data %>%
  filter(!(is.na(MarginOfSafety))) %>%
  mutate(pe = `Market Cap`  / trail_12m_net_inc)



ranking_data <- ranking_data %>%
  mutate(pe = remove_outliers(pe, probs = c(0.05,.4))) %>%
  mutate(MarginOfSafety = remove_outliers(MarginOfSafety, probs = c(0.30,.95))) %>%
  filter(!(is.na(pe)),!(is.na(MarginOfSafety))) %>%
  filter(pe >0) %>%
  group_by(`GICS Ind Name`) %>%
  arrange(Ranking)



ggplot(ranking_data, aes(x = MarginOfSafety * 100, y = pe,
                         size = `Market Cap`, color = dcf_growth_rate)) +
  geom_point(alpha = 0.5) +
  geom_vline(xintercept = 0) +geom_hline(yintercept = 15) +
  facet_wrap(~`GICS Sector`
  ) + theme_minimal() +
  theme(legend.position = "none")

write.csv(ranking_data,'best-companies.csv', row.names = FALSE)

