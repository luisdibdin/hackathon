
library(tidyverse)
library(plotly)
library(readxl)
library(readODS)

setwd("D:/Joel/Docs/Hackathon/")

beis_spreadsheet <- read.ods("BEIS_Data_Tables.ods")


### Concerns

concerns <- beis_spreadsheet[[2]] %>% 
  select(A, B, H)

unique(concerns$A)



### Policies

policies <- beis_spreadsheet[[14]] %>% 
  select(A, B, H)

unique(policies$A)

table61 <- policies[c(5:10),] %>% 
  select(support = B, perc = H)


### Generate all tables as list

table_seq <- seq(1, 71, by = 14)

tables <- list()
iter <- 0

for (i in table_seq){
  
  iter <- iter + 1
  
  start <- i+4
  end <- i+9
  
  this_table <- policies[c(start:end),] %>% 
    select(support = B, perc = H) 
  
  this_table <- this_table %>% 
    mutate(support = factor(support, levels = unique(this_table$support)))
  
  tables[[iter]] <- this_table
  
}

table1 <- tables[[1]]


#mtcars$manuf <- sapply(strsplit(rownames(mtcars), " "), "[[", 1)

df <- table1
# df <- df %>% group_by(support)
# df <- df %>% summarize(count = n())
# fig <- df %>% plot_ly(labels = ~support, values = ~perc)
# fig <- fig %>% add_pie(hole = 0.6)
# fig <- fig %>% layout(title = "Donut charts using Plotly",  showlegend = F,
#                       xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
#                       yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

fig





# titles <- c("ban on the sale of new petrol, diesel and hybrid cars by 2035",
#             "Scrap incentives that encourage people to fly more",
#             "A 'frequent flier levy' ",
#             "Advertising bans and restrictions on high emissions products or sectors",
#             "citizens steering group to monitor government's progress against its targets",
#             "abelling food and drink products to show the amount of emissions")

category <- c("end to fuel car sales by 2035",
                "end to flight incentives",
                "frequent flier levy",
                "high emisssion product advertising bans",
                "citizens steering group for target monitoring",
                "emissions labelling on food/drink")


support <- c()


### Get % that support policies overall
for(i in tables){
  
  perc_support <- sum(as.numeric(i[c(1, 2), 2])) ## take from top two rows of each columns
  perc_support <- round(100*perc_support, digits = 0)
  
  support <- c(support, perc_support)
}


statement <- c()

for(i in 1:length(tables)){
  
  this_statement <- paste0(support[i], "% support\n", category[i])
  statement <- c(statement, this_statement)
  
}

statement

my_colours <- c(RColorBrewer::brewer.pal(5, "Blues"), "#888888")

row <- c(0,0,0,
         1,1,1)
column <- c(0,1,2,
            0,1,2)

fig <- plot_ly(marker = list(colors = my_colours))

for(i in c(1:6)){
  
  fig <- fig %>% add_pie(data = tables[[i]], labels = ~support, values = ~perc, 
                         name = category[i],
                         textinfo = "none",
                         hoverinfo = "text",
                         text = ~paste(
                           '</br>', category[i], 
                           '</br>', support, ": ", round(100*as.numeric(perc), 1), "%"),
                         domain = list(row = row[i], column = column[i]), hole = 0.6,
                         direction = "clockwise",
                         sort = FALSE) #%>%
    #layout(annotations = list(text = statement[i]))
}

fig <- fig %>% layout(title = "Public Agreement to Climate Policies", showlegend = T,
                      grid=list(rows=2, columns=3),
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      legend = list(orientation = "h",
                                    xanchor = "center",
                                    x = 0.5))


fig


### TODO


### Add titles to centre circles (or appreviations)
### use RAG colour scheme
### Add caption on source/date of survey data
## comment code


### https://plotly.com/r/subplots/ <- has infor on titling subplots

## Use red amber green colouring


### add to gitub 

### repeat for other indicators???

#marker = list(colours = my_colours)



#htmlwidgets::saveWidget(as_widget(fig), "policies_support.html")

htmlwidgets::saveWidget(as_widget(fig), "policies_support2.html")
