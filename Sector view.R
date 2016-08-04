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


ranking_data_2 %>% 
  group_by(`GICS Sector`) %>% 
  arrange(`Ranking`) %>% 
  filter(`Ranking` < quantile(`Ranking`, .05)) %>%
  write.csv(.,'best-companies.csv', row.names = FALSE)
