### Shiny app for plotting Pacific-centered maps from Glottolog data

# Load libraries
library(shiny)
library(maps)
library(mapproj)
library(tidyverse)
library(randomcoloR)
library(ggnewscale)

# Define UI
ui <- fluidPage(
  
  # Title 
  titlePanel("GlottoshinyR: Glottolog Language Map"),
  
  # Sidebar layout
  sidebarLayout(
    
    # Sidebar panel
    sidebarPanel(
      radioButtons("level",
                   label="Data level by",
                   choices = list("language",
                                  "family",
                                  "macroarea"),
                   inline = FALSE),
      conditionalPanel(
        condition = "input.level == 'family'",
        textInput("families",
                  label="Enter families (comma-separated)")
      ),
      conditionalPanel(
        condition = "input.level == 'macroarea'",
        textInput("macroareas",
                  label="Enter macroareas (comma-separated)")
      ),
      conditionalPanel(
        condition = "input.level == 'language'",
        radioButtons("format",
                     label="Input format",
                     choices = list("Name (Glottolog)",
                                    "Glottocode (Glottolog)",
                                    "ISO 639-3"),
                     inline = FALSE),
        textInput("lgs",
                  label="Enter languages by name, Glottocode or ISO 639-3 (comma-separated)"),
        selectInput("type",
                    label="Color categorization",
                    choices = list("Glottolog: Family",
                                   "Glottolog: Language",
                                   "Custom: Features",
                                   "None: bright",
                                   "None: dark"),
                    selected = "Glottolog: Family")
      ),
      conditionalPanel(
        condition = "input.level != 'language'",
        selectInput("type2",
                    label="Color categorization",
                    choices = list("Glottolog",
                                   "None: bright",
                                   "None: dark"),
                    selected = "Glottolog")
      ),
      conditionalPanel(
        condition = "input.type == 'Custom: Features' & input.level=='language'",
        textInput("features",
                  label="Enter features (comma-separated)")
      ),
      radioButtons("legend",
                   label="Show legend",
                   choices = list("bottom",
                                  "right",
                                  "none"),
                   inline = TRUE),
      selectInput("projection",
                  label = "Projection",
                  choices = list("Gilbert",
                                 "Mercator",
                                 "Van der Grinten"),
                  selected = "Van der Grinten"),
      sliderInput("size", 
                  label = "Point size",
                  min = 0, max = 15,
                  value = 2),
      sliderInput("alpha",
                  label = "Point alpha",
                  min = 0, max = 1,
                  value = 1),
      sliderInput("fill",
                  label = "Land mass fill",
                  min = 0, max = 100,
                  value = 75),
      conditionalPanel(
        condition = "input.legend != 'none'",
        sliderInput("textsize",
                    label="Text size",
                    min = 0, max = 30,
                    value = 15)
      ),
      radioButtons("font",
                   label="Text font",
                   choices = list("Arial",
                                  "Times New Roman"),
                   inline=TRUE)
    ),
    # Main panel
    mainPanel(
      
      # Output: Map
      plotOutput(outputId = "map"),
      downloadButton(outputId = "savemap", label = "Download map"),
      HTML("<br><br>This application was created by <a href='https://borstell.github.io'>Carl Börstell</a>
           as a simple tool for creating Pacific-centered language maps from <a href='https://glottolog.org'>Glottolog</a> data.
           <br>You can plot individual languages with the possibility to categorize them according to
           top-level language family or some feature based on your own input data, or plot entire language families or macroareas.
           <br>
           <br>Glottolog and the R libraries used to create this mapping tool are automatically included
           in a text caption at the bottom of the output plot, but make sure to cite these source accordingly
           should you use the map for a paper or presentation or similar: 
           <a href='https://glottolog.org/meta/cite'>Glottolog<a>,
           <a href='https://cran.r-project.org/web/packages/ggnewscale/index.html'>ggnewscale</a>,
           <a href='https://cran.r-project.org/web/packages/mapproj/index.html'>mapproj</a>,
           <a href='https://cran.r-project.org/web/packages/maps/index.html'>maps</a>,
           <a href='https://cran.r-project.org/web/packages/randomcoloR/index.html'>randomcoloR</a>,
           <a href='https://www.tidyverse.org'>tidyverse</a>.
           <br>
           <br>For more advanced Glottolog mapping with R, see e.g. the <a href='https://github.com/SietzeN/glottospace'>glottospace</a> package.
           <br>
           <br>To cite reports in publications, please use:
           <br>
           <br>Börstell, Carl. 2021. GlottoshinyR. https://linguistr.shinyapps.io/glottoshinyr/
           <br>
<br>BibTeX entry for reference:
<br>
<br><code>
@Manual{GlottoshinyR,<br>
    title = {{GlottoshinyR}},<br>
    author = {Carl Börstell},<br>
    note = {Shiny application},<br>
    year = {2021},<br>
    url = {https://linguistr.shinyapps.io/glottoshinyr/},<br>
}</code>")
      
    )
  )
)

# Server
server <- function(input, output) {
  
  
  # Render map
  output$map <- renderPlot({
    
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
    
    # Create a family data frame to join for family affiliation and join all data
    families <- languages %>% 
      select(Name, Glottocode) %>% 
      rename(Family = "Name") %>% 
      rename(Family_ID = "Glottocode")
    
    glottolog <- full_join(glottolog,languages, by="Glottocode") %>% 
      filter(!is.na(Family_ID)) %>% 
      left_join(families, by="Family_ID")
    
    # Store input data
    langs <- trimws(as.vector(str_split(input$lgs,",")[[1]]))
    families <- trimws(as.vector(str_split(input$families,",")[[1]]))
    macroareas <- trimws(as.vector(str_split(input$macroareas,",")[[1]]))
    
    # Define map projection
    if(input$projection=="Mercator"){
      proj <- coord_quickmap(xlim = c(min(pacific_map$long),max(pacific_map$long)-360))
    }
    if(input$projection=="Van der Grinten"){
      proj <- coord_map(projection="vandergrinten", xlim=c(-180,180))
    }
    if(input$projection=="Gilbert"){
      proj <- coord_map(projection="gilbert", xlim=c(-180,180))
    }
    
    # Adjust Glottolog data to Pacific-centered map
    df <- glottolog %>% 
      mutate(Longitude = if_else(Longitude<cut_longitude,Longitude+(360),Longitude)) %>% 
      filter(Longitude != "")
    
    # Filter data frame based on input
    if(input$level=="family"){
      df <- df %>% 
        filter(Family %in% families)
    } else{
      if(input$level=="macroarea"){
        df <- df %>% 
          filter(Macroarea %in% macroareas)
      } else{
        if(input$format=="ISO 639-3"){
          df <- df %>% 
            filter(ISO639P3code %in% langs) 
        } else{
          if(input$format=="Glottocode (Glottolog)"){
            df <- df %>% 
              filter(Glottocode %in% langs) 
          } else{
            df <- df %>% 
              filter(Name %in% langs) 
          }
        }
      }
    }
    
    # Join input features to plot
    if(input$type=="Custom: Features"){
      feats <- as.vector(str_split(input$features,",")[[1]])
      features <- data.frame(id=langs,
                             Feature=feats)
      if(input$format=="ISO 639-3"){
        df <- df %>% 
          left_join(features, by=c("ISO639P3code" = "id"))
      } else{
        if(input$format=="Glottocode (Glottolog)"){
          df <- df %>% 
            left_join(features, by=c("Glottocode" = "id"))
        } else{
          df <- df %>% 
            left_join(features, by=c("Name" = "id"))
        } 
      }
      points <- geom_point(data=df, aes(x=Longitude-(180+cut_longitude), y=Latitude, fill=Feature), shape=21, size=input$size, alpha=input$alpha)
    }
    
    # Set parameters for data points
    if(input$level=="language" & input$type!="Custom: Features"){
      if(input$type=="Glottolog: Language"){
        points <- geom_point(data=df, aes(x=Longitude-(180+cut_longitude), y=Latitude, fill=Name), shape=21, size=input$size, alpha=input$alpha)
      }
      if(input$type=="Glottolog: Family"){
        points <- geom_point(data=df, aes(x=Longitude-(180+cut_longitude), y=Latitude, fill=Family), shape=21, size=input$size, alpha=input$alpha)
      }
      if(input$type=="None: bright"){
        points <- geom_point(data=df, aes(x=Longitude-(180+cut_longitude), y=Latitude), shape=21, fill="skyblue", color="grey10", size=input$size, alpha=input$alpha)
      } 
      if(input$type=="None: dark"){
        points <- geom_point(data=df, aes(x=Longitude-(180+cut_longitude), y=Latitude), shape=21, fill="grey10", color="white", size=input$size, alpha=input$alpha)
      } 
    }
    if(input$level!="language"){
      if(input$type2=="None: bright"){
        points <- geom_point(data=df, aes(x=Longitude-(180+cut_longitude), y=Latitude), shape=21, fill="skyblue", color="grey10", size=input$size, alpha=input$alpha)
      } 
      if(input$type2=="None: dark"){
        points <- geom_point(data=df, aes(x=Longitude-(180+cut_longitude), y=Latitude), shape=21, fill="grey10", color="white", size=input$size, alpha=input$alpha)
      } 
      if(input$type2=="Glottolog"){
        points <- geom_point(data=df, aes(x=Longitude-(180+cut_longitude), y=Latitude, fill=Family), shape=21, size=input$size, alpha=input$alpha)
      }
    }
    
    # Set color palette
    if(nrow(df)<1){
      pal <- distinctColorPalette(40)
    }else{
      if(input$type=="Glottolog: Family" | input$type2=="Glottolog"){
        pal <- distinctColorPalette(length(unique(df$Family)))
      }
      if(input$type=="Glottolog: Language"){
        pal <- distinctColorPalette(length(unique(df$Name)))
      }
      if(input$type=="Custom: Features"){
        pal <- distinctColorPalette(length(unique(df$Feature)))
      }
    }
    
    # Plot map with ggplot2
    ggplot() + 
      geom_polygon(data=pacific_map,aes(x = long-(180+cut_longitude), y = lat, group = group), fill = paste0("grey",input$fill)) +
      geom_polygon(data=lakes, aes(x = long-(180+cut_longitude), y = lat, group = group, fill = "white")) +
      scale_fill_identity() +
      new_scale_fill() +
      points +
      proj +
      scale_fill_manual(values=pal) +
      labs(caption="Data: glottolog.org; Libraries: {ggnewscale,mapproj,maps,randomcoloR,shiny,tidyverse}") +
      theme_void(base_size=input$textsize, base_family = input$font) + 
      theme(panel.border = element_rect(color = NA, fill=NA, size=.5), plot.margin = unit(c(5,5,5,5), "pt"),
            legend.position = input$legend,
            plot.caption = element_text(color="grey80",size=rel(.5)),
            panel.background = element_rect(color="transparent", fill="white"),
            plot.background = element_rect(color="transparent", fill="white")) 
  })
  
  # Save output plot
  output$savemap = downloadHandler(
    filename = function() {paste0("Glottomap_",Sys.Date(),".png")},
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 6,
                       res = 900, units = "in")
      }
      ggsave(file, device = device)
    })
}

# Run Shiny app
shinyApp(ui = ui, server = server)
