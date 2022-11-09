library(magrittr)

ACLED_UK_clean <- read.csv("ACLED_UK_clean.csv", row.names = 1)

#Convert dataframe into spatial object
ACLED_sf <- sf::st_as_sf(ACLED_UK_clean,
                         coords = c("longitude", "latitude"),
                         crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

Climate_protests <- ACLED_sf %>%
  dplyr::filter(.data$climate == "Y") %>%
  dplyr::select(.data$data_id,
                .data$action_group,
                .data$date,
                .data$sub_event_type,
                .data$notes)

distinct_protests <- Climate_protests %>%
  dplyr::group_by(data_id)%>%
  dplyr::slice(1)%>%
  dplyr::ungroup()


plot(Climate_protests)





