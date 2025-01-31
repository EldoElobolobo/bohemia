source('global.R')

# # Clustering rules:
# humans: >= 35 children (<5 in moz, 5-15 in tza)
# distance: >= 1km around core (ie, 2km between cores)
# number of ttm groups: 3 (human-ivm, all-ivm, control)
# number of clusters per group: >48 per group
# Preferences:
# - no splitting of villages (assuming to mean "hamlets")
# - experiment by different species


# Define the country
the_country <- 'Mozambique'

# Define whether we want to interpolate for missing animals
# and/or missing people
interpolate_animals <- TRUE
interpolate_humans <- TRUE

# Define the percentage of people which are children
p_children <- 30

# Define the human sufficiency rule
human_sufficiency_rule <- 'n_children >= 35'
animal_sufficiency_rule <- 'n_animals >= 35'

# Define the distance between each buffer

# Get the locations
left <- geocodes %>% filter(Country == the_country) %>%
  filter(!duplicated(code)) %>%
  dplyr::select(code, lng, lat)
# Get the animal info
right <- animal %>% filter(Country == the_country) %>%
  filter(!duplicated(hamlet_code)) %>%
  mutate(code = hamlet_code) %>%
  dplyr::select(code,
                contains('n_'))
# Join locations and animal info
joined <- left_join(left, right)
# Get the number of residents info
right <- recon_data %>% filter(Country == the_country) %>%
  filter(!duplicated(hamlet_code)) %>%
  mutate(code = hamlet_code) %>%
  dplyr::select(code,
                n_households = number_hh)
# Join all info
df <- left_join(joined, right)
message(nrow(df), ' locations. Removing those without geocoding reduces to:')
df <- df %>% filter(!is.na(lng), !is.na(lat))
message(nrow(df), ' locations.')

# Recode categorical variables so as to get approximate numbers
recodify <- function(x){
  x <- as.character(x)
  x <- ifelse(x == '0', 0,
              ifelse(x == '1 to 5', 3,
                     ifelse(x == '6 to 19', 12,
                            ifelse(x == '20 or more', 30, NA))))
  return(x)
}
n_names <- c('n_cattle', 'n_goats', 'n_pigs')
for(j in 1:length(n_names)){
  this_column <- n_names[j]
  df[,this_column] <- recodify(as.character(unlist(df[,this_column])))
}

# Carry out interpolations
if(interpolate_humans){
  missing_humans <- which(is.na(df$n_households))
  message('Going to inerpolate for ', length(missing_humans), ' hamlets with missing number of households')
  if(length(missing_humans) > 0){
    for(i in missing_humans){
      this_row <- missing_humans[i]
      df$n_households[i] <- sample(df$n_households[!is.na(df$n_households)],
                                   1)
    }
  }
}
if(interpolate_animals){
  animal_vars <- c('n_cattle', 'n_goats', 'n_pigs')
  for(j in 1:length(animal_vars)){
    animal_var <- animal_vars[j]
    animal_name <- gsub('n_', '', animal_var)
    missing_animals <- which(is.na(unlist(df[,animal_var])))
    message('Going to inerpolate for ', length(missing_animals), ' hamlets with missing ', animal_name)
    if(length(missing_animals) > 0){
      for(i in missing_animals){
        this_row <- missing_animals[i]
        good_animals <- as.numeric(unlist(df[,animal_var]))
        good_animals <- good_animals[!is.na(good_animals)]
        df[i, animal_var] <- sample(good_animals, 1)
      }
    }
  }
}
# Remove any data with missing animals or humans
df <- df %>%
  filter(!is.na(n_households),
         !is.na(n_cattle),
         !is.na(n_pigs),
         !is.na(n_goats))

# Define a n_children variable
df$n_children <- df$n_households * (0.01 * p_children)
# Define an animal variable
df$n_animals <- df$n_cattle + df$n_goats + df$n_pigs

# Create a space for indicating whether the hamlet
# has already been assigned to a cluster or not
df$assigned <- FALSE
df$cluster <- 0
# Spatial section ######################################
library(dplyr)
library(sp)
library(geosphere)

# Create a spatial version of df
df$id <- 1:nrow(df)
df_sp <- df
coordinates(df_sp) <- ~lng+lat
proj4string(df_sp) <- CRS("+init=epsg:4326") # define as lat/lng
zone <- 36
new_proj <- CRS(paste0("+proj=utm +zone=", 
                       zone, 
                       " +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
df_sp <- spTransform(df_sp, new_proj)

# Get a distance matrix between all points
distance_matrix <- rgeos::gDistance(spgeom1 = df_sp, byid = TRUE)

# define the sufficiency rule
sufficiency_rule <- paste0(animal_sufficiency_rule, ' & ',
                           human_sufficiency_rule)
suffiency_text <- paste0(
  "this_cluster <- this_cluster %>% summarise(n_households = sum(n_households), n_cattle = sum(n_cattle), n_pigs = sum(n_pigs), n_goats = sum(n_goats), n_animals = sum(n_animals), n_children = sum(n_children)) %>%
  dplyr::mutate(is_sufficient = ", sufficiency_rule, ")"
)

# Loop around and build clusters
cluster_list <- list()
done_ids  <- c()
done <- FALSE
cluster_counter <- 1

# Pre-assign (before creating clusters) the treatment groups
# (so as to not have the necessity of buffers between clusters of identical
# treatment groups)
assignment_vector <- c(rep(1, 48), rep(2, 48), rep(3, 48))
assignment_vector <- sample(assignment_vector, length(assignment_vector))
# Add to it in case we have more than 3*48 clusters
part2 <- sample(1:3, size = 1000 - length(assignment_vector), replace = TRUE)
assignment_vector <- c(assignment_vector, part2)

while(!done){
  message(paste0('Working on cluster ', cluster_counter))
  # If there have been no assignments yet, find a start point
  if(length(which(df$assigned)) < 1){
    median_distances <- as.numeric(apply(distance_matrix, 1, median, na.rm = TRUE))
    start_here <- which.min(median_distances)[1]
    this_id <- df$id[start_here]
    df$assigned[start_here] <- TRUE
    df$cluster[start_here] <- cluster_counter
    done_ids <- c(done_ids, this_id)
  }
  # Use whatever the "next_id" is to start building a new cluster
  this_cluster <- this_hamlet <-  df %>% filter(id == this_id)
  # See if this hamlet is sufficient by itself
  eval(parse(text = suffiency_text))
  is_sufficient <- this_cluster$is_sufficient
  # if not sufficient, get nearest
  hamlet_counter <- 1
  message('...1 hamlet')
  if(is.na(this_id)){
    done <- TRUE
  }
  while(!is_sufficient & !done){
    hamlet_counter <- hamlet_counter + 1
    message('...', hamlet_counter, ' hamlets')
    this_index <- which(df$id == this_id)
    # Get the nearest hamlet
    distances_from_index <- as.numeric(distance_matrix[this_index,])
    # but make sure not to include any hamlets which are already done
    ok_to_use <- which(!df$assigned)
    ids <- df$id[ok_to_use]
    distances_from_index <- distances_from_index[ok_to_use]
    nearest_index <- which.min(distances_from_index)[1]
    close_id <- ids[nearest_index]
    message('---Assigning hamlet with id: ', close_id)
    df$assigned[df$id == close_id] <- TRUE
    df$cluster[df$id == close_id] <- cluster_counter
    done_ids <- c(done_ids, close_id)
    this_hamlet <- df %>% filter(id == close_id)
    this_cluster <- bind_rows(this_hamlet %>% dplyr::select(n_households,
                                                            n_cattle,
                                                            n_pigs,
                                                            n_goats,
                                                            n_animals,
                                                            n_children),
                              this_cluster %>% dplyr::select(-is_sufficient))
    eval(parse(text = suffiency_text))
    is_sufficient <- this_cluster$is_sufficient
    # Define the next_id
    if(!is_sufficient){
      ok_to_use <- which(!df$assigned)
      ids <- df$id[ok_to_use]
      next_near_index <- which(df$id == close_id)
      distances_from_index <- as.numeric(distance_matrix[next_near_index,])
      distances_from_index <- distances_from_index[ok_to_use]
      nearest_index <- which.min(distances_from_index)[1]
      close_id <- ids[nearest_index]
      this_id <- close_id
      if(length(which(df$assigned)) == nrow(df)){
        done <- TRUE
      }
      # message('---Assigning hamlet with id: ', close_id)
      # df$assigned[df$id == close_id] <- TRUE
      # df$cluster[df$id == close_id] <- TRUE
      # done_ids <- c(done_ids, close_id)
    }
  }
  # Finished one cluster, start the next one
  cluster_counter <-cluster_counter + 1
  ok_to_use <- which(!df$assigned)
  ids <- df$id[ok_to_use]
  distances_from_index <- as.numeric(distance_matrix[this_index,])
  
  this_group <- assignment_vector[cluster_counter]
  next_group <- assignment_vector[cluster_counter + 1]
  if(length(next_group) == 0){
    is_same <- FALSE
  } else {
    is_same <- this_group == next_group
  }
  if(!is_same){
    not_ok <- which(distances_from_index < 2000)
    not_ok_ids <- ids[not_ok]
    df$assigned[df$id %in% not_ok_ids] <- TRUE
    done_ids <- c(done_ids, not_ok_ids)
    ok_to_use <- ok_to_use[!ok_to_use %in% not_ok]
  }
  
  if(length(ok_to_use) > 0){
    
    distances_from_index <- distances_from_index[ok_to_use]
    nearest_index <- which.min(distances_from_index)[1]
    close_id <- ids[nearest_index]
    this_id <- close_id
    
    message('---Assigning hamlet with id: ', close_id)
    df$assigned[df$id == close_id] <- TRUE
    df$cluster[df$id == close_id] <- cluster_counter
    done_ids <- c(done_ids, close_id)
  } else {
    done <- TRUE
  }
  
  if(length(which(df$assigned)) == nrow(df)){
    done <- TRUE
  }
  message(length(which(df$assigned)), ' hamlets done')
  message(cluster_counter)
}

# Bring in assignment numbers
assignment_df <- tibble(cluster = 1:1000,
                        assignment_group = assignment_vector)
hamlet_df <- left_join(df, assignment_df)
cluster_df <- hamlet_df %>%
  group_by(cluster) %>%
  summarise(n_hamlets = length(unique(id)),
            assignment_group = dplyr::first(assignment_group),
            hamlet_codes = paste0(code, collapse = ', '),
            n_households = sum(n_households), 
            n_cattle = sum(n_cattle), 
            n_pigs = sum(n_pigs), 
            n_goats = sum(n_goats), 
            n_animals = sum(n_animals), 
            n_children = sum(n_children))
# # Plot
# pd <- hamlet_df
# pd <- pd[sample(1:nrow(pd), nrow(pd), replace = FALSE),]
# pd$cluster <- factor(pd$cluster, levels = unique(pd$cluster))
# ggplot(data = pd,
#        aes(x = lng,
#            y = lat,
#            color = factor(assignment_group))) +
#   geom_point() +
#   theme(legend.position = 'none')
# 
# ##############################
# 
# # Make polygons
# # Add a perimeter around each cluster
# hamlet_df_sp <- hamlet_df
# coordinates(hamlet_df_sp) <- ~lng+lat
# proj4string(hamlet_df_sp) <- proj4string(ruf2)
# # proj4string(hamlet_df_sp) <- CRS("+init=epsg:4326") # define as lat/lng
# # zone <- 36
# # new_proj <- CRS(paste0("+proj=utm +zone=", 
# #                        zone, 
# #                        " +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
# # hamlet_df_sp <- spTransform(hamlet_df_sp, new_proj)
# hamlet_df_sp@data$id <- hamlet_df_sp@data$cluster
# hamlet_df_sp@data$lng <- df$lng
# hamlet_df_sp@data$lat <- df$lat
# v <- voronoi(shp = hamlet_df_sp,
#              poly = ruf2)
# v_fort <- fortify(v, region = 'id') %>% mutate(cluster = as.numeric(id))
# v_fort <- left_join(v_fort, cluster_df)
# 
# ggplot() +
#   geom_polygon(data = v_fort,
#                aes(x = long,
#                    y = lat,
#                    group = group,
#                    fill = factor(assignment_group)),
#                color = 'white', size = 0.1) +
#   theme(legend.position = 'none')
# 
# 
# ggplot(data = hamlet_df,
#        aes(x = lng,
#            y = lat,
#            color = factor(assignment_group))) +
#   geom_point()
# 
# buffers <- rgeos::gBuffer(df_sp, byid = TRUE, width = 1000)
# # Get in lat/lng
# buffers_ll <- spTransform(buffers, proj4string(ruf2))
# 

pd
