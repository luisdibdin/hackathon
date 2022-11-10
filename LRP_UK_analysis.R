
### Analysis of LRP for UK

library(tidyverse)

LRP_UK <- read.csv("D:/Joel/Docs/Hackathon/hackathon/LRP_UK_clean.csv")


#### Overall figures of concerns

LRP_UK <- LRP_UK %>% 
  mutate(
    threat_percieved = case_when(Q3 == 1 ~ "Very serious",
                                 Q3 == 2 ~ "Somewhat serious",
                                 Q3 == 3 ~ "Not a threat at all",
                                 Q3 == 98 ~ "Don't know")
  )


threat_props <- LRP_UK %>% 
  filter(!is.na(threat_percieved)) %>% 
  group_by(Year) %>% 
  count(threat_percieved) %>% 
  mutate(perc = 100*n/sum(n))


################################


### Logistic regression looking at predictors of percieveing climate change as a risk to country

## Set level orders/reference categories

LRP_UK <- LRP_UK %>% 
  mutate(AgeGroup = factor(AgeGroup, levels = c("15_29", "30_49", "50_64", "65_94")),
         education_level = factor(education_level, levels = c("Low", "Medium", "High")),
         Income_categories = factor(Income_categories, levels = c("Poorest 20%",
                                                                  "Second 20%",
                                                                  "Third 20%",
                                                                  "Fourth 20%",
                                                                  "Richest 20%")),
         Year = as.character(Year),
         Year = factor(Year, levels = c("2019", "2021")),
         Gender = as_factor(Gender),
         Urbanicity = as_factor(Urbanicity),
         HouseholdSize = factor(HouseholdSize, levels = c("1_2", "3_4", "5+")),
         ChildrenInHousehold = factor(ChildrenInHousehold, levels = c("No", "Yes")))


###


## firstly to check how to use age is it linear???

age_props <- LRP_UK %>% 
  group_by(Age) %>% 
  count(climate_threat) %>% 
  mutate(prop = n/sum(n)) %>% 
  filter(climate_threat == 1)


ggplot(age_props, aes(x = Age, y = prop)) +
  geom_col() ## Not clear and not much power - use categories. If more time would like into spline



colnames(LRP_UK)


my_model <- "climate_threat ~ AgeGroup + Gender + education_level + Income_categories + Urbanicity + HouseholdSize + ChildrenInHousehold + Year"


threat.mod <- glm(my_model,
                  data = LRP_UK,
                  family = "binomial")

summary(threat.mod)


### Now get ORs and Plot - Plotly forest plot???

threat_ORS <- exp(cbind(coef(threat.mod), confint.default(threat.mod)))

threat_ORS

threat_ORS_fin <- data.frame(
  level = rownames(threat_ORS),
  OR = threat_ORS[,1],
  ll = threat_ORS[,2],
  ul = threat_ORS[,3],
  p = coef(summary(threat.mod))[, 4]
)


threat_ORS_fin


vars <- c("AgeGroup", "Gender", "education_level", "Income_categories", 
          "Urbanicity", "HouseholdSize", "ChildrenInHousehold", "Year")

refs <- c("AgeGroup15_29", "GenderFemale", "education_levelLow",
          "Income_categoriesPoorest 20%", "UrbanicityRural",
          "HouseholdSize1_2", "ChildrenInHouseholdsNo", "Year2019")

reference_categories <- data.frame(
  level = c("AgeGroup15_29", "GenderFemale", "education_levelLow",
            "Income_categoriesPoorest 20%", "UrbanicityRural",
            "HouseholdSize1_2", "ChildrenInHouseholdNo", "Year2019"),
  OR = 1
)

threat_plot_data <- bind_rows(reference_categories, threat_ORS_fin) %>% 
  mutate(variable = str_extract(level, paste(vars, collapse = "|")),
         lev = str_remove_all(level, paste(vars, collapse = "|"))) %>% 
  filter(!is.na(variable))

level_ord <- c()

for(i in vars){
  
  lev <- levels(LRP_UK[, i])
  level_ord <- c(level_ord, lev)
}

level_ord

threat_plot_data <- threat_plot_data %>% 
  mutate(lev = factor(lev, levels = level_ord)) %>% 
  arrange(lev)






ggplot(threat_plot_data, aes(x = OR, y = lev, colour = variable)) +
  geom_point(size = 2) +
  scale_x_log10() +
  scale_color_brewer(palette = "Set2") +
  geom_errorbar(aes(xmin = ll, xmax = ul), width = 0.5) +
  geom_vline(xintercept = 1, colour = "black", linetype = "dashed") +
  labs(title = "How Climate Change Threat Perception Varies by Demographic in the UK") +
  theme_classic() +
  theme(axis.line.x = element_blank(), panel.grid.major.x = element_line(colour = "grey90", linetype = "dashed"), axis.ticks.x = element_blank(),
        legend.position = "none", strip.text.y = element_text(size = 7, angle = 0)) +
  ylab(NULL) +
  xlab("Odds Ratio") +
  facet_grid(rows = vars(variable), scales = "free", space = "free")





