library(shiny)
library(DT)
library(readr)
library(tidyr)
library(sf)
library(ggplot2)
library(wordcloud)
library(dplyr)
library(plotly)
library(shinyjs)
library(rapportools)
library(ggrepel)

characters = read_csv("data/characters.csv")
episodes = read_csv("data/episodes.csv")
scenes = read_csv("data/scenes.csv")
appearances = read_csv("data/appearances.csv")
populations=read_csv("data/populations.csv")



ui <- fluidPage(
  titlePanel("Shiny - Interactive Visualization GOT"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      selectInput(inputId="color1",label="Choose Color",choices = c("Red"="Red","Blue"="Blue","Green"="Green"),
                  selected = "Blue",multiple = F),
      
      
      selectInput(inputId="channel1",label="Selectionner un personnage",choices = as.vector(characters[1]),
                  selected = "Jon Snow",multiple = F),
      
      
      selectInput(inputId="channel2",label="Selectionner un deuxième personnage (default :None)",choices = as.vector(rbind(characters[1],"None")),
                  selected = "None",multiple = F),
      
      sliderInput(inputId = "nbrpersonne",
                  label = "Nombre maximal des personnes :",
                  min = 15,
                  max = 50,
                  value = 5),
      
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      tabsetPanel(type='tabs',tabPanel(
      
      titlePanel(h5("Répartition spatiale")),
      h2("1-Répartition spatiale des scènes de la personne selectionnée",style='background-color:coral;
                     padding-left: 15px'),
      plotOutput(outputId = "distPlot",height = "600px",
                 width = "100%")),
      
      tabPanel(titlePanel(h5("Temps d'apparition")),h2("2-Temps d'apparition par épisode",style='background-color:coral;
                     padding-left: 15px'),
      plotlyOutput(outputId = "distPlot1")),
      tabPanel(titlePanel(h5("Wordcloud")),
      h2("3-Wordcloud des personnes proche du personnage selectionné",style='background-color:coral;
                     padding-left: 15px'),
      plotOutput(outputId = "distPlot2",height = "500px",
                 width = "100%")),
      tabPanel(titlePanel(h5("Famille")),
               h2("4-Famille de la personne (en cas d'appartenance à une famille)",style='background-color:coral;
                     padding-left: 15px'),
               plotOutput(outputId = "distPlot3",height = "500px",
                          width = "100%"))
    )
  )
  
))

server <- function(input, output, session) {
  
  
  
  
  
  output$distPlot <- renderPlot({
    
    locations=st_read("./data/GoTRelease/Locations.shp",crs=4326)
    lakes=st_read("./data/GoTRelease/Lakes.shp",crs=4326)
    conts=st_read("./data/GoTRelease/Continents.shp",crs=4326)
    land=st_read("./data/GoTRelease/Land.shp",crs=4326)
    wall=st_read("./data/GoTRelease/Wall.shp",crs=4326)
    islands=st_read("./data/GoTRelease/Islands.shp",crs=4326)
    kingdoms=st_read("./data/GoTRelease/Political.shp",crs=4326)
    landscapes=st_read("./data/GoTRelease/Landscape.shp",crs=4326)
    roads=st_read("./data/GoTRelease/Roads.shp",crs=4326)
    rivers=st_read("./data/GoTRelease/Rivers.shp",crs=4326)
    
    if(input$color1=="Red"){
      sColor = "#ff3300"
    }else if(input$color1=="Blue"){
      sColor = "#3399ff"
    }else if(input$color1=="Green"){
      sColor = "#66ff33"
    }
    
    
    
    
    if(input$channel2=="None"){
    
    
    
      scenes_locations=st_read("./data/GoTRelease/ScenesLocations.shp",crs=4326)
      loc_time=appearances %>% filter(name==input$channel1) %>% left_join(scenes) %>% group_by(location) %>% summarize(duration=sum(duration,na.rm=TRUE))
      loc_time_js = scenes_locations %>% left_join(loc_time)%>% drop_na(duration)
      
      loc_scene=appearances %>% filter(name==input$channel1) %>% left_join(scenes) %>% group_by(location) %>%count(location, sort = TRUE)
      loc_scene_js = scenes_locations %>% left_join(loc_scene)%>% drop_na(n)
      
      
      
      X=st_coordinates(loc_time_js$geometry)[,1]
      Y=st_coordinates(loc_time_js$geometry)[,2]
      
      loc_scene_js_f=paste(loc_scene_js$n, " scene (s)", sep="")
      ll=paste(loc_time_js$location,loc_scene_js_f)
      
      
      colforest="#c0d7c2"
      colriver="#7ec9dc"
      colriver="#d7eef4"
      colland="ivory"
      borderland = "ivory3"
      ggplot()+geom_sf(data=land,fill=colland,col=borderland,size=0.1)+
        geom_sf(data=islands,fill=colland,col="ivory3")+
        geom_sf(data=landscapes %>% filter(type=="forest"),fill=colforest,col=colforest,alpha=0.7)+
        geom_sf(data=rivers,col=colriver)+
        geom_sf(data=lakes,col=colriver,fill=colriver)+
        geom_sf(data=wall,col="black",size=1)+
        geom_sf(data=loc_time_js,aes(size=duration/60),color=sColor)+theme_minimal()+
        geom_text_repel(data = loc_time_js,
                        aes(x=X,y=Y ,label = ll),
                        color = 'black',
                        size  = 4,
                        box.padding = 0.7, point.padding = 0.5)+
        
        
        scale_size_area("Durées (min) :",max_size = 15,breaks=c(30,60,120,240))+
        theme(panel.background = element_rect(fill = colriver,color=NA),legend.position = "bottom")
      
    
    }
    else{
      
      
      
      scenes_locations=st_read("./data/GoTRelease/ScenesLocations.shp",crs=4326)
      
      
      loc_time1=appearances %>% filter(name == input$channel1) %>% left_join(appearances,by=c("sceneId"="sceneId"))
      loc_time2=loc_time1 %>% filter(name.y == input$channel2)%>%left_join(scenes)
      loc_time_js1=scenes_locations %>% left_join(loc_time2)%>% drop_na(duration)%>%group_by(location) %>% summarize(duration=sum(duration,na.rm=TRUE))
      
      
      if(is.empty(loc_time_js1$duration)==FALSE){
        
        loc_scene1=appearances %>% filter(name == input$channel1) %>% left_join(appearances,by=c("sceneId"="sceneId"))
        loc_scene2=loc_scene1 %>% filter(name.y==input$channel2) %>% left_join(scenes)%>%drop_na(duration)
        loc_scene_js1 = scenes_locations %>% left_join(loc_scene2)%>%group_by(location) %>%drop_na(name.y)%>%count(location, sort = TRUE)
        
        X1=st_coordinates(loc_time_js1$geometry)[,1]
        Y1=st_coordinates(loc_time_js1$geometry)[,2]
        
        loc_scene_js_f1=paste(loc_scene_js1$n, " scene (s)", sep="")
        ll1=paste(loc_time_js1$location,loc_scene_js_f1)
        
        
        
      
      
      
        colforest="#c0d7c2"
        colriver="#7ec9dc"
        colriver="#d7eef4"
        colland="ivory"
        borderland = "ivory3"
        ggplot()+geom_sf(data=land,fill=colland,col=borderland,size=0.1)+
          geom_sf(data=islands,fill=colland,col="ivory3")+
          geom_sf(data=landscapes %>% filter(type=="forest"),fill=colforest,col=colforest,alpha=0.7)+
          geom_sf(data=rivers,col=colriver)+
          geom_sf(data=lakes,col=colriver,fill=colriver)+
          geom_sf(data=wall,col="black",size=1)+
          geom_sf(data=loc_time_js1,aes(size=duration/60),color=sColor)+theme_minimal()+coord_sf(expand = 0,ndiscr = 0)+
          geom_text_repel(data = loc_time_js1,
                          aes(x=X1,y=Y1 ,label = ll1),
                          color = 'black',
                          size  = 3,
                          box.padding = 0.7, point.padding = 0.5)+
          scale_size_area("Durées (min) :",max_size = 15,breaks=c(30,60,120,240))+
          theme(panel.background = element_rect(fill = colriver,color=NA),legend.position = "bottom")
      }
      else{
        colforest="#c0d7c2"
        colriver="#7ec9dc"
        colriver="#d7eef4"
        colland="ivory"
        borderland = "ivory3"
        ggplot()+geom_sf(data=land,fill=colland,col=borderland,size=0.1)+
          geom_sf(data=islands,fill=colland,col="ivory3")+
          geom_sf(data=landscapes %>% filter(type=="forest"),fill=colforest,col=colforest,alpha=0.7)+
          geom_sf(data=rivers,col=colriver)+
          geom_sf(data=lakes,col=colriver,fill=colriver)+
          geom_sf(data=wall,col="black",size=1)+
          theme_minimal()+coord_sf(expand = 0,ndiscr = 0)+
          theme(panel.background = element_rect(fill = colriver,color=NA))
          
        
      }
      
    }
    
      
  })
  
  output$distPlot1 <- renderPlotly({
    
    if(input$channel2=="None"){
      
      jstime = appearances %>% filter(name==input$channel1) %>% 
        left_join(scenes) %>% 
        group_by(episodeId) %>%
        summarise(time=sum(duration))
      jstime_1=jstime %>%left_join(episodes)%>%group_by(seasonNum)
      Seasons=factor(jstime_1$seasonNum)
      ggplot(jstime_1) + 
        geom_point(aes(x=episodeId,y=time,colour = Seasons)) +
        theme_bw()+
        xlab("épisode")+ylab("temps")+
        ggtitle(paste("Temps de présence par épisode de", input$channel1))
      
    }
    else{
      js_time1=appearances %>% filter(name == input$channel1) %>% left_join(appearances,by=c("sceneId"="sceneId"))
      js_time2=js_time1 %>% filter(name.y == input$channel2)%>%left_join(scenes)%>%group_by(episodeId) %>% summarise(time=sum(duration))
      jstime_1=js_time2 %>%left_join(episodes)%>%group_by(seasonNum)
      Seasons=factor(jstime_1$seasonNum)
      ggplot(jstime_1) + 
        geom_point(aes(x=episodeId,y=time,colour = Seasons)) +
        theme_bw()+
        xlab("épisode")+ylab("temps")+
        ggtitle(paste("Temps de présence par épisode de", input$channel1,"avec",input$channel2))
    }
    
    
  })
  
  output$distPlot2 <- renderPlot({
    
    js_data=appearances %>% filter(name == input$channel1)
    
    Withperson=js_data %>% left_join(appearances,by=c("sceneId"="sceneId")) %>% 
      filter(name.x!=name.y) %>% 
      group_by(name.x,name.y) %>%
      count(name.y, sort = TRUE)
      
    
    wordcloud(words = Withperson$name.y, freq = Withperson$n,scale=c(3,0.25),min.freq = 1,
              max.words=input$nbrpersonne,random.order=FALSE, rot.per=0.2,
              colors=brewer.pal(8, "Dark2"))
    
    
  })
  
  output$distPlot3 <- renderPlot({
    
    locations=st_read("./data/GoTRelease/Locations.shp",crs=4326)
    lakes=st_read("./data/GoTRelease/Lakes.shp",crs=4326)
    conts=st_read("./data/GoTRelease/Continents.shp",crs=4326)
    land=st_read("./data/GoTRelease/Land.shp",crs=4326)
    wall=st_read("./data/GoTRelease/Wall.shp",crs=4326)
    islands=st_read("./data/GoTRelease/Islands.shp",crs=4326)
    kingdoms=st_read("./data/GoTRelease/Political.shp",crs=4326)
    landscapes=st_read("./data/GoTRelease/Landscape.shp",crs=4326)
    roads=st_read("./data/GoTRelease/Roads.shp",crs=4326)
    rivers=st_read("./data/GoTRelease/Rivers.shp",crs=4326)
    scenes_locations=st_read("./data/GoTRelease/ScenesLocations.shp",crs=4326)
    
    if(input$color1=="Red"){
      sColor = "#ff3300"
    }else if(input$color1=="Blue"){
      sColor = "#3399ff"
    }else if(input$color1=="Green"){
      sColor = "#66ff33"
    }
    
    js_family=characters %>% filter(name == input$channel1)
    famille_name=js_family$house[1]
    poli=st_read("./data/GoTRelease/Political.shp",crs=4326)
    
    
    location_family_row=poli%>% filter(ClaimedBy == famille_name)
    location_family_name=location_family_row$name
    location_family_geom=scenes_locations%>% filter(location==location_family_name)
    size_location=populations%>% filter(name==location_family_name)
    Population=size_location$population
    

    
    X3=st_coordinates(location_family_geom$geometry)[,1]
    Y3=st_coordinates(location_family_geom$geometry)[,2]
    
    colforest="#c0d7c2"
    colriver="#7ec9dc"
    colriver="#d7eef4"
    colland="ivory"
    borderland = "ivory3"
    ggplot()+geom_sf(data=land,fill=colland,col=borderland,size=0.1)+
      geom_sf(data=islands,fill=colland,col="ivory3")+
      geom_sf(data=landscapes %>% filter(type=="forest"),fill=colforest,col=colforest,alpha=0.7)+
      geom_sf(data=rivers,col=colriver)+
      geom_sf(data=lakes,col=colriver,fill=colriver)+
      geom_sf(data=wall,col="black",size=1)+
      geom_sf(data=location_family_geom,aes(size=Population),color=sColor)+theme_minimal()+
      geom_text_repel(data = location_family_geom,
                      aes(x=X3,y=Y3 ,label = location_family_geom$location),size=4,
                      color = 'black',
                      box.padding = 0.7, point.padding = 0.5,guide=FALSE)+
      theme(panel.background = element_rect(fill = colriver,color=NA))
      
    
    
    
    
  })
  
 
}

shinyApp(ui, server)
