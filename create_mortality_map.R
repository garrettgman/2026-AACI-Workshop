# Georgia County Mortality Rate Choropleth Map Function
# This function creates an interactive map showing average annual mortality rates
# per 100,000 population for Georgia counties

library(tidyverse)
library(leaflet)
library(sf)
library(tigris)

options(tigris_use_cache = TRUE)


create_mortality_map <- function(mortality_data, 
                                  population_data, 
                                  state_fips = "13",
                                  palette = "YlOrRd") {
  
  county_year_totals <- mortality_data |>
    group_by(County, Year) |>
    summarise(n = sum(n, na.rm = TRUE), .groups = "drop")
  
  county_avg_deaths <- county_year_totals |>
    group_by(County) |>
    summarise(avg_deaths = mean(n, na.rm = TRUE), .groups = "drop")

  county_avg_rate <- county_avg_deaths |>
    left_join(population_data, by = "County") |>
    mutate(Rate = (avg_deaths / Population) * 100000)
  
  counties_geo <- counties(state = state_fips, cb = TRUE, year = 2021, progress_bar = FALSE) |>
    st_transform(4326) |>  # Transform to WGS84 (EPSG:4326) to avoid datum warning
    mutate(County = paste(NAME, "County"))
  
  map_data <- counties_geo |>
    left_join(county_avg_rate, by = "County") |>
    mutate(Rate = replace_na(Rate, 0))
  
  pal <- colorNumeric(
    palette = palette,
    domain = map_data$Rate,
    na.color = "#808080"
  )

  leaflet(map_data) |>
    addTiles() |>
    addPolygons(
      fillColor = ~pal(Rate),
      fillOpacity = 0.7,
      color = "#FFFFFF",
      weight = 1,
      smoothFactor = 0.5,
      highlightOptions = highlightOptions(
        weight = 2,
        color = "#666",
        fillOpacity = 0.9,
        bringToFront = TRUE
      ),
      label = ~paste0(County, ": ", round(Rate, 1), " per 100,000"),
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"
      )
    ) |>
    addLegend(
      pal = pal,
      values = ~Rate,
      opacity = 0.7,
      title = "Mortality Rate<br>per 100,000",
      position = "bottomright"
    )
}
