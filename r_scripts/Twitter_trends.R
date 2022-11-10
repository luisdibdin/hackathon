library(ggplot2)
library(magrittr)
twitter_data <- read.csv("tidy_twitter_data.csv")
twitter_data[,2:36] <- as.data.frame(sapply(twitter_data[,2:36], as.numeric))
                                  
twitter_data <- tidyr::pivot_longer(twitter_data, 2:36, names_to = "Date")

twitter_data$Date <- lubridate::my(twitter_data$Date)

trends <- ggplot(twitter_data) +
  geom_line(aes(x = Date, y = log10(value), colour = `ï..Twitter.Handle`))

plotly::ggplotly(trends)
