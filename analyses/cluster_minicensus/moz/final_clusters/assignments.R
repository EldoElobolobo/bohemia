set.seed(123)
library(sp)
library(bohemia)
library(tidyverse)
library(yaml)
library(databrew)
library(leaflet)
library(kableExtra)
library(ggplot2)
theme_set(theme_simple())

# Read in objects created in monte_carlo.R
# (simulation number 9 is the final one)
cluster_level <- read_csv('cluster_level.csv')
hamlet_level <- read_csv('hamlet_level.csv')
hh_level <- read_csv('hh_level.csv')
status_level <- read_csv('status_level.csv')
load('image.RData')
load('../data.RData')

# Get the subset spatial objects
if(!'final_spatial_data.RData' %in% dir()){
  set.seed(123)
  master_poly <- master_poly[master_poly@data$sim == 9 &
                               master_poly@data$iter_buffer_distance == 400 &
                               master_poly@data$iter_n_children == 20,]
  master_buf <- master_buf[master_buf@data$sim == 9 &
                             master_buf@data$iter_buffer_distance == 400 &
                             master_buf@data$iter_n_children == 20,]
  master_hull <- master_hull[master_hull@data$sim == 9 &
                               master_hull@data$iter_buffer_distance == 400 &
                               master_hull@data$iter_n_children == 20,]
  save(master_poly,
       master_buf,
       master_hull,
       file = 'final_spatial_data.RData')
} else {
  load('final_spatial_data.RData')
}

# Create arms (just once)
if(!'arm_assignments.csv' %in% dir()){
  set.seed(123)
  arm_assignments <- tibble(cluster = cluster_level$cluster)
  assig_helper <- rep(1:3, each = 53)
  assig_helper <- sample(assig_helper, length(assig_helper))
  arm_assignments$assignment <- assig_helper
  write_csv(arm_assignments, 'arm_assignments.csv')
} else {
  arm_assignments <- read_csv('arm_assignments.csv')
}

# Use the ento deliverables document to generate sentinel clusters
if(!'ento_sentinel_clusters.csv' %in% dir()){
  set.seed(123)
  out_list <- list()
  for(i in 1:3){
    these <- sample(arm_assignments$cluster[arm_assignments$assignment == i], size = 5)
    out <- tibble(arm = i,
                  cluster = these,
                  type = 'Sentinel cluster')
    out_list[[i]] <- out
  }
  ento_sentinel_clusters <- bind_rows(out_list) %>%
    arrange(arm, cluster)
  write_csv(ento_sentinel_clusters, 'ento_sentinel_clusters.csv')
} else {
  ento_sentinel_clusters <- read_csv('ento_sentinel_clusters.csv')
}
# Show the sentinenl clusters
xx <- spTransform(master_hull, proj4string(bohemia::mop2))
yy <- spTransform(master_buf, proj4string(bohemia::mop2))


l <- leaflet() %>%
  addProviderTiles(providers$Stamen.Toner)

cols <- c('red', 'blue', 'green')
for(i in 1:3){
  this <- ento_sentinel_clusters %>%
    filter(arm == i)
  z <- xx[xx$cluster %in% this$cluster,]
  zz <- yy[yy$cluster %in% this$cluster,]
  l <- l %>%
    addPolygons(data = z,
                color = cols[i],
                popup = paste0('Cluster: ', z@data$cluster, '. Assignment arm: ', i)) %>%
    addPolylines(data = zz,
                color = cols[i])
}
l
library(htmltools)
save_html(html = l, file = '~/Desktop/sentinelclustersmoz.html')
