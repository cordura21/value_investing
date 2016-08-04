library(readxl)
library(magrittr)
library(dplyr)

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}
ranking_data <- read_excel('Ranking con mas data.xlsx')
names(ranking_data)

ranking_data_2 <- ranking_data %>%

  filter(!is.na(`GICS Sector`))

library(ggplot2)

plots <- list()
plots[[1]] <- ggplot(ranking_data_2,aes(
  y = remove_outliers(`Promedio ROE 20 Anos`,probs = c(.05,.99)),
  x = remove_outliers(`Desvio ROE 20 Anos`,probs = c(.05,.95)),size = `Market Cap`)) +
  geom_point( alpha = 0.5)+ geom_smooth() +
  facet_wrap(~`GICS Sector`,ncol = 3) + theme_minimal()+theme(legend.position = "none")
plots[[1]]



plots[[2]] <- ggplot(ranking_data_2 %>% filter(`GICS Sector` == 'Information Technology'),aes(
  y = remove_outliers(`Promedio ROE 20 Anos`,probs = c(.05,.99)),
  x = remove_outliers(`Desvio ROE 20 Anos`,probs = c(.05,.95)),size = `Market Cap` / 1000000,
  color = `GICS Ind Name`)) +
  geom_point( ) + geom_smooth()+ theme_minimal()
plots[[2]]

library(formattable)

ranking_data_2 %>% group_by(`GICS Sector`) %>% 
  summarise(Companies = n(), Rank = median(`Ranking`)) %>% 
arrange(Rank) %>%formattable() 

ranking_data_3 <- 
ranking_data_2 %>% 
  group_by(`GICS Sector`) %>% 
  arrange(`Ranking`) %>% 
  filter(`Ranking` < quantile(`Ranking`, .05)) %>%

  mutate( trail_12m_net_inc =trail_12m_net_inc * 1000000) %>%
  
  mutate( dcf_growth_rate =ifelse(`cagr eps 10 years` > 20,20,`cagr eps 10 years`)) %>%
  mutate( dcf_growth_rate =ifelse(dcf_growth_rate < 5,5,dcf_growth_rate)) %>%
  mutate( dcf_growth_rate =dcf_growth_rate / 100) 
 
ranking_data_3$MarginOfSafety <- NA
for(iLoop in 1: NROW(ranking_data_3)){
  curr_mos_row <- ranking_data_3[iLoop,,drop = FALSE]
  ranking_data_3[iLoop,'MarginOfSafety'] <- 
   dcf_calc(price =  curr_mos_row$`Market Cap`,
                    eps = curr_mos_row$trail_12m_net_inc ,
                    dcf.discount.rate = .10,
                    growth.rate = curr_mos_row$dcf_growth_rate,
                    years.growth = 10,
                    stable.growth = .04,
                    years.stable = 10)
}

ranking_data_3$MarginOfSafety2 <- NA
for(iLoop in 1: NROW(ranking_data_3)){
  curr_mos_row <- ranking_data_3[iLoop,,drop = FALSE]
  ranking_data_3[iLoop,'MarginOfSafety2'] <- 
    dcf_calc(price =  curr_mos_row$`Market Cap`,
             eps = curr_mos_row$trail_12m_net_inc ,
             dcf.discount.rate = .10,
             growth.rate = curr_mos_row$dcf_growth_rate,
             years.growth = 5,
             stable.growth = .04,
             years.stable = 15)
}


ranking_data_3$MarginOfSafety3 <- NA
for(iLoop in 1: NROW(ranking_data_3)){
  curr_mos_row <- ranking_data_3[iLoop,,drop = FALSE]
  ranking_data_3[iLoop,'MarginOfSafety3'] <- 
    dcf_calc(price =  curr_mos_row$`Market Cap`,
             eps = curr_mos_row$trail_12m_net_inc ,
             dcf.discount.rate = .10,
             growth.rate = curr_mos_row$dcf_growth_rate / 2,
             years.growth = 10,
             stable.growth = .04,
             years.stable = 10)
}
 write.csv(ranking_data_3,'best-companies.csv', row.names = FALSE)
