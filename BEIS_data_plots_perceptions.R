
beis_spreadsheet <- read.ods("BEIS_Data_Tables.ods")


### actions ######## 

actions <- beis_spreadsheet[[10]] %>% 
  select(A, B, H)

unique(actions$A) ## What are the different tables?

## checking format of tables
table61 <- actions[c(5:11),] %>% 
  select(support = B, perc = H)

#################### Format data ##########################################

### Generate all tables as list

## Each table is indexed in the same way, every 15 lines - so loop through to get these (Do top 4 seperately as in different format)
table_seq <- seq(1, 46, by = 15) 

tables <- list()
iter <- 0

for (i in table_seq){
  
  iter <- iter + 1
  
  start <- i+4
  end <- i+10
  
  this_table <- actions[c(start:end),] %>% 
    select(support = B, perc = H) 
  
  this_table <- this_table %>% 
    mutate(support = factor(support, levels = unique(this_table$support)))
  
  tables[[iter]] <- this_table
  
}

table1 <- tables[[4]]


### Create policy labels

category <- c("use a green energy supplier",
              "use a smart meter",
              "eat mainly plant-based meals",
              "minimise food waste")

### Add category to each table to allow easier labelling in plotly

for(i in 1:4){
  
  tables[[i]] <- tables[[i]] %>% 
    mutate(cat = category[i]) ### add var for category
  
}


#### Create headline statement of support

support <- c()

### Get % that support actions overall
for(i in tables){
  
  perc_support <- sum(as.numeric(i[c(1:3), 2])) ## take from top two rows of each columns
  perc_support <- round(100*perc_support, digits = 0)
  
  support <- c(support, perc_support)
}

## Make into a statement

statement <- c()

for(i in 1:length(tables)){
  
  this_statement <- paste0(support[i], "% at least 'somewhat likely' to ", category[i])
  statement <- c(statement, this_statement)
  
}

statement

statement2 <- str_wrap(statement, width = 15)

### Use red, amber, green colours
#my_colours <- c(RColorBrewer::brewer.pal(5, "Blues"), "#888888")
my_colours <- c("#2dc937", "#99c140", "#e7b416", "#db7b2b", "#cc3232")#, "#888888") ## use traffic light colour system
my_colours <- c("#238b45", scales::muted(my_colours, l = 70, c = 90), "#888888") ## muting colours slightly


######################## Plot ###############################################

# Set grid coordinaetes for plots 
row <- c(0,0,
         1,1)
column <- c(0,1,
            0,1)



## Initiate plot_ly figure and set colours 

fig <- plot_ly(marker = list(colors = my_colours))

## set centres of each subplot
x_centres <- rep(c(2.1/9, 6.9/9), 2) ## This was done through trial and error, may need to adjust in markdown
y_centres <- c(rep(5/6, 2), rep(1/6, 2))


### Create each subplot
for(i in c(1:4)){
  
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
fig <- fig %>% layout(title = "Liklihood of making changes in the next 6 months", showlegend = T,
                      grid=list(rows=2, columns=2),
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      legend = list(orientation = "h",
                                    xanchor = "center",
                                    x = 0.5))



## Final figure
fig



htmlwidgets::saveWidget(as_widget(fig), "beis_actions_6_months.html")











#### Repeating for actions now


## Each table is indexed in the same way, every 15 lines - so loop through to get these (Do top 4 seperately as in different format)
table_seq <- seq(1, 15, by = 14) 

tables <- list()
iter <- 0

for (i in table_seq){
  
  iter <- iter + 1
  
  start <- i+64
  end <- i+69
  
  this_table <- actions[c(start:end),] %>% 
    select(support = B, perc = H) 
  
  this_table <- this_table %>% 
    mutate(support = factor(support, levels = unique(this_table$support)))
  
  tables[[iter]] <- this_table
  
}

# tables[[1]]
# tables[[2]]


### Create policy labels

category <- c("use less heating",
              "reduce thermostat temperature")

### Add category to each table to allow easier labelling in plotly

for(i in 1:2){
  
  tables[[i]] <- tables[[i]] %>% 
    mutate(cat = category[i]) ### add var for category
  
}


#### Create headline statement of support

support <- c()

### Get % that support actions overall
for(i in tables){
  
  perc_support <- sum(as.numeric(i[c(1, 2), 2])) ## take from top two rows of each columns
  perc_support <- round(100*perc_support, digits = 0)
  
  support <- c(support, perc_support)
}

## Make into a statement

statement <- c()

for(i in 1:length(tables)){
  
  this_statement <- paste0(support[i], "% at least 'somewhat likely' to ", category[i])
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
row <- c(0,0)
column <- c(0,1)






## Initiate plot_ly figure and set colours 

fig <- plot_ly(marker = list(colors = my_colours))

## set centres of each subplot
x_centres <- rep(c(2.1/9, 6.9/9)) ## This was done through trial and error, may need to adjust in markdown
#y_centres <- c(rep(5/6, 2), rep(1/6, 2))


### Create each subplot
for(i in c(1:2)){
  
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
                    y = 0.5,
                    text = statement2[i],
                    xref = "paper",
                    yref = "paper",
                    xanchor = "center",
                    yanchor = "center",
                    showarrow = FALSE)
  #layout(annotations = list(text = statement[i]))
}

## Format
fig <- fig %>% layout(title = "Liklihood of making changes compared to last winter", showlegend = T,
                      grid=list(rows=1, columns=2),
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      legend = list(orientation = "h",
                                    xanchor = "center",
                                    x = 0.5))



## Final figure
fig



htmlwidgets::saveWidget(as_widget(fig), "beis_actions_now.html")






####### Finally for "next time doing this" ####################################

table_seq <- seq(1, 31, by = 15) 

tables <- list()
iter <- 0

for (i in table_seq){
  
  iter <- iter + 1
  
  start <- i+92
  end <- i+98
  
  this_table <- actions[c(start:end),] %>% 
    select(support = B, perc = H) 
  
  this_table <- this_table %>% 
    mutate(support = factor(support, levels = unique(this_table$support)))
  
  tables[[iter]] <- this_table
  
}

tables[[3]]


### Create policy labels

category <- c("buy an electric car (instead of petrol/diesel)",
              "avoid travelling by plane",
              "switch to low-carbon heating technology")

### Add category to each table to allow easier labelling in plotly

for(i in 1:3){
  
  tables[[i]] <- tables[[i]] %>% 
    mutate(cat = category[i]) ### add var for category
  
}


#### Create headline statement of support

support <- c()

### Get % that support actions overall
for(i in tables){
  
  perc_support <- sum(as.numeric(i[c(1:3), 2])) ## take from top two rows of each columns
  perc_support <- round(100*perc_support, digits = 0)
  
  support <- c(support, perc_support)
}

## Make into a statement

statement <- c()

for(i in 1:length(tables)){
  
  this_statement <- paste0(support[i], "% at least 'somewhat likely' to ", category[i])
  statement <- c(statement, this_statement)
  
}

statement

statement2 <- str_wrap(statement, width = 15)

### Use red, amber, green colours
#my_colours <- c(RColorBrewer::brewer.pal(5, "Blues"), "#888888")
my_colours <- c("#2dc937", "#99c140", "#e7b416", "#db7b2b", "#cc3232")#, "#888888") ## use traffic light colour system
my_colours <- c("#238b45", scales::muted(my_colours, l = 70, c = 90), "#888888") ## muting colours slightly


######################## Plot ###############################################

# Set grid coordinaetes for plots 
row <- c(0,0, 0)
column <- c(0,1,2)



## Initiate plot_ly figure and set colours 

fig <- plot_ly(marker = list(colors = my_colours))

## set centres of each subplot
x_centres <- c(2/13, 1/2, 11/13) ## This was done through trial and error, may need to adjust in markdown
y_centres <- 0.5


### Create each subplot
for(i in c(1:3)){
  
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
fig <- fig %>% layout(title = "Liklihood of making changes next time", showlegend = T,
                      grid=list(rows=1, columns=3),
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      legend = list(orientation = "h",
                                    xanchor = "center",
                                    x = 0.5))



## Final figure
fig



htmlwidgets::saveWidget(as_widget(fig), "beis_actions_now.html")





