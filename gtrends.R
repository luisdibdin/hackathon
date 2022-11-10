library(gtrendsR)
library(ggplot2)
library(magrittr)

data1 <-gtrends(keyword = c("Extinction Rebellion",
                           "Just Stop Oil",
                           "fridays for future",
                           "Greenpeace UK",
                           "Animal Rebellion"),
                         geo = "GB",
                         time = "today+5-y",
                         onlyInterest = T)

data2 <-gtrends(keyword = c("Friends of the Earth",
                           "Ocean Rebellion",
                           "Insulate Britain",
                           "Climate Strike",
                           "Stop cambo"),
               geo = "GB",
               time = "today+5-y",
               onlyInterest = T)

data3 <-gtrends(keyword = c("Stay grounded",
                           "UK Student Climate Network"),
               geo = "GB",
               time = "today+5-y",
               onlyInterest = T)

data <- list(data1, data2, data3) 

data <- purrr::map(data, ~ {.x[[1]]$hits <- as.numeric(ifelse(.x[[1]]$hits == "<1",
                                      1,
                                      .x[[1]]$hits))
.x}
            ) %>%
  purrr::reduce(dplyr::bind_rows)



interest_over_time <- data$interest_over_time
write.csv(interest_over_time, file =  "gtrends_interest_over_time.csv", row.names = F)


a <- ggplot(interest_over_time)+
  geom_line(aes(x = date, y = as.numeric(hits), colour = keyword))

b <-  plotly::ggplotly(a)

