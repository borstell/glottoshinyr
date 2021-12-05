### R script for plotting Pacific-centered language maps from Glottolog data

# Load libraries
library(maps)
library(mapproj)
library(tidyverse)
library(randomcoloR)
library(ggnewscale)

    
# Set longitude cut-off point
cut_longitude <- -25

# Load a world map (excluding Antarctica)
rawmap <- fortify(maps::map(fill=TRUE, plot=FALSE)) %>% 
  filter(region != "Antarctica")

# Adjust regions that fall west of cut longitude (-25); move USA and Greenland manually
pacific_map <- rawmap %>%
  group_by(region) %>% 
  mutate(west = if_else(max(long)<(cut_longitude), TRUE, FALSE)) %>% 
  mutate(west = if_else(region %in% c("USA","Greenland"), TRUE, west)) %>% 
  mutate(long = if_else(west,long+(360),long))

# Adjust lake data similarly
lakes <- map_data("lakes") %>%
  mutate(long = if_else(long<(cut_longitude),long+(360),long))


# Read Glottolog and join data
glottolog <- read_csv("https://raw.githubusercontent.com/glottolog/glottolog-cldf/master/cldf/values.csv", na = c("","<NA>")) %>% 
  rename(Glottocode = "Language_ID") %>% 
  select(2:4) %>% 
  pivot_wider(names_from = "Parameter_ID", values_from = "Value") %>% 
  select(-(4:7)) 

languages <- read_csv("https://raw.githubusercontent.com/glottolog/glottolog-cldf/master/cldf/languages.csv", na = c("","<NA>")) %>% 
  select(-ID) %>% 
  mutate(Family_ID = if_else(is.na(Family_ID), Glottocode, Family_ID))


# Create a family data frame to join for family affiliation
families <- languages %>% 
  select(Name, Glottocode) %>% 
  rename(Family = "Name") %>% 
  rename(Family_ID = "Glottocode")

# ... join all data ...
glottolog <- full_join(glottolog,languages, by="Glottocode") %>% 
  filter(!is.na(Family_ID)) %>% 
  left_join(families, by="Family_ID")

# ... and adjust Glottolog data to Pacific-centered map
df <- glottolog %>% 
  mutate(Longitude = if_else(Longitude<cut_longitude,Longitude+(360),Longitude)) %>% 
  filter(Longitude != "")


# Read custom features and join to data frame to plot
features <- read_csv("/path/to/your/data.csv")

df <- df %>% 
  inner_join(features, by="Glottocode")
  #left_join(features, by="Name")
  #left_join(features, by="ISO639P3code")


# Set color palette based on what variable to map
pal <- distinctColorPalette(length(unique(df$Feature)))
#pal <- distinctColorPalette(length(unique(df$Family)))
#pal <- distinctColorPalette(length(unique(df$Name)))


# Set (a)esthetic constants for plotting
custom_alpha <- 1
custom_pointsize <- 2
custom_pointshape <- 21
custom_continentfill <- "grey70"
custom_fontsize <- 15
custom_fontfamily <- "Arial"


# Plot map with ggplot2
ggplot() + 
  geom_polygon(data=pacific_map,aes(x = long-(180+cut_longitude), y = lat, group = group), fill = custom_continentfill) +
  geom_polygon(data=lakes, aes(x = long-(180+cut_longitude), y = lat, group = group, fill = "white")) +
  scale_fill_identity() +
  new_scale_fill() +
  
  # Choose how you want to plot the geom_points (uncomment your choice)
  geom_point(data=df, aes(x=Longitude-(180+cut_longitude), y=Latitude, fill=Feature), shape=custom_pointshape, size=custom_pointsize, alpha=custom_alpha) +
  #geom_point(data=df, aes(x=Longitude-(180+cut_longitude), y=Latitude, fill=Name), shape=custom_pointshape, size=custom_pointsize, alpha=custom_alpha) +
  #geom_point(data=df, aes(x=Longitude-(180+cut_longitude), y=Latitude, fill=Family), shape=custom_pointshape, size=custom_pointsize, alpha=custom_alpha) +
  #geom_point(data=df, aes(x=Longitude-(180+cut_longitude), y=Latitude), shape=custom_pointshape, fill="skyblue", color="grey10", size=custom_pointsize, alpha=custom_alpha) +
  #geom_point(data=df, aes(x=Longitude-(180+cut_longitude), y=Latitude), shape=custom_pointshape, fill="grey10", color="white", size=custom_pointsize, alpha=custom_alpha) +
  #geom_point(data=df, aes(x=Longitude-(180+cut_longitude), y=Latitude, fill=Family), shape=custom_pointshape, size=custom_pointsize, alpha=custom_alpha) +
  
  # Choose map projection as desired (uncomment your choice)
  coord_map(projection="vandergrinten", xlim=c(-180,180)) +
  #coord_quickmap(xlim = c(-180,180)) +
  #coord_map(projection="gilbert", xlim=c(-180,180)) +
  
  # Uncomment line below if you are using a manually set single color (i.e. no feature)
  scale_fill_manual(values=pal) +
  
  labs(caption="Data: glottolog.org; Libraries: {ggnewscale,mapproj,maps,randomcoloR,shiny,tidyverse}") +
  theme_void(base_size=custom_fontsize, base_family = custom_fontfamily) + 
  theme(panel.border = element_rect(color = NA, fill=NA, size=.5), plot.margin = unit(c(5,5,5,5), "pt"),
        legend.position = "bottom",
        plot.caption = element_text(color="grey80",size=rel(.5)),
        panel.background = element_rect(color="transparent", fill="white"),
        plot.background = element_rect(color="transparent", fill="white")) 


# Save plotted map
# You can specify file extension, width and height (and in which units ("in","cm","mm") and dpi)
ggsave(filename = "/path/to/your/map.png", width=8, height=6, units="in", dpi=900)
