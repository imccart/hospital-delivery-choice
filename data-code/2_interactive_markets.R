## Interactive map --------------------------------------------------------

# Convert the cluster boundary polygons to line geometries
cluster_boundaries_lines <- st_cast(cluster.boundaries, "LINESTRING")

# Create an interactive plotly map
interactive_map <- plot_ly() %>%
  # Add the base map of census tracts (as polygons)
  add_sf(data = merged.dat, color = I("grey"), sizes = I(0.5)) %>%
  # Add cluster boundaries as lines explicitly setting the trace type
  add_trace(data = cluster_boundaries_lines, type = 'scatter', mode = 'lines',
            line = list(color = 'black', width = 1.5),
            hoverinfo = 'text',
            text = ~paste('Total Patients: ', total_patients,
                          '<br>Total Tracts: ', total_tracts))

# Customize layout
interactive_map <- interactive_map %>%
  layout(title = "Interactive Map of Census Tracts and Market Clusters",
         hovermode = "closest")

# Print the interactive map
interactive_map
