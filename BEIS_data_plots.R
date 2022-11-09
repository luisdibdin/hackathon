
library(tidyverse)
library(plotly)
library(readxl)
library(readODS)

setwd("D:/Joel/Docs/Hackathon/")

beis_spreadsheet <- read.ods("BEIS_Data_Tables.ods")




### Policies ######## 

policies <- beis_spreadsheet[[14]] %>% 
  select(A, B, H)

unique(policies$A) ## What are the different tables?

## checking format of tables
table61 <- policies[c(5:10),] %>% 
  select(support = B, perc = H)

#################### Format data ##########################################

### Generate all tables as list

## Each table is indexed in the same way, every 14 lines - so loop through to get these
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


### Create policy labels

category <- c("end to fuel car sales by 2035",
                "end to flight incentives",
                "frequent flier levy",
                "high emisssion product advertising bans",
                "citizens steering group for target monitoring",
                "emissions labelling on food/drink")

### Add category to each table to allow easier labelling in plotly

for(i in 1:6){
  
  tables[[i]] <- tables[[i]] %>% 
    mutate(cat = category[i]) ### add var for category
  
}


#### Create headline statement of support

support <- c()

### Get % that support policies overall
for(i in tables){
  
  perc_support <- sum(as.numeric(i[c(1, 2), 2])) ## take from top two rows of each columns
  perc_support <- round(100*perc_support, digits = 0)
  
  support <- c(support, perc_support)
}

## Make into a statement

statement <- c()

for(i in 1:length(tables)){
  
  this_statement <- paste0(support[i], "% support ", category[i])
  statement <- c(statement, this_statement)
  
}

statement

statement2 <- str_wrap(statement, width = 15)

### Use red, amber, green colours
#my_colours <- c(RColorBrewer::brewer.pal(5, "Blues"), "#888888")
my_colours <- c("#2dc937", "#99c140", "#e7b416", "#db7b2b", "#cc3232")#, "#888888") ## use traffic light colour system
my_colours <- c(scales::muted(my_colours, l = 70, c = 90), "#888888") ## muting colours slightly


######################## Plot ###############################################

# Set grid coordinaetes for plots 
row <- c(0,0,0,
         1,1,1)
column <- c(0,1,2,
            0,1,2)

## Initiate plot_ly figure and set colours 

fig <- plot_ly(marker = list(colors = my_colours))

## set centres of each subplot
x_centres <- rep(c(2/13, 1/2, 11/13), 2) ## This was done through trial and error, may need to adjust in markdown
y_centres <- c(rep(5/6, 3), rep(1/6, 3))


### Create each subplot
for(i in c(1:6)){
  
  fig <- fig %>% add_pie(data = tables[[i]], labels = ~support, values = ~perc, 
                         name = category[i],
                         textinfo = "none",
                         hoverinfo = "text",
                         text = ~ paste0(
                           '</br>', cat, 
                           '</br>', support, ": ", round(100*as.numeric(perc), 1), "%"),
                         domain = list(row = row[i], column = column[i]), hole = 0.6,
                         direction = "clockwise",
                         sort = FALSE) %>%
    add_annotations(x = x_centres[i],
                    y = y_centres[i],
                    text = statement2[i],
                    xref = "paper",
                    yref = "paper",
                    xanchor = "center",
                    yanchor = "center",
                    showarrow = FALSE)
    #layout(annotations = list(text = statement[i]))
}

## Format
fig <- fig %>% layout(title = "Public Support of Climate Policies", showlegend = T,
                      grid=list(rows=2, columns=3),
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      legend = list(orientation = "h",
                                    xanchor = "center",
                                    x = 0.5))



## Final figure
fig

## Save as html widget
htmlwidgets::saveWidget(as_widget(fig), "policies_support3.html")


### https://plotly.com/r/subplots/ <- has infor on titling subplots


### repeat for other indicators???


