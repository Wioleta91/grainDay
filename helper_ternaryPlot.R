#Load the Libraries


library(ggtern)
library(plyr)

#Load the Data
data(USDA)



#Put tile labels at the midpoint of each tile.
USDA.LAB <- ddply(USDA,"Label",function(df){
  apply(df[,1:3],2,mean)
})

#Tweak
USDA.LAB$Angle = sapply(as.character(USDA.LAB$Label),function(x){
  switch(x,"Loamy Sand"=-35,0)
})


#Construct the plot.
plot_Tern <-ggtern(data=USDA,aes(Sand,Clay,Silt,color=Label,fill=Label)) +
            geom_polygon(alpha=0.40,size=0.5,color="black") +
            geom_mask() +
            geom_text(data=USDA.LAB,aes(label=Label,angle=Angle),color="black",size=3.5) +
            theme_rgbw() +
            theme_showsecondary() +
            theme_showarrows() +
            weight_percent() +
            guides(fill='none') +
            theme_legend_position("topleft") +
            labs(title = "USDA Textural Classification Chart",
                 fill = "Textural Class",
                 color = "Textural Class")


plot_Tern


mock_data <- read.csv("grainDay/data_sample.csv")
colnames(mock_data)[c(5:7)] <- c("Sand", "Clay", "Silt")

plot_Tern +
  geom_point(
    data = mock_data,
    mapping = aes(
      Sand,
      Clay,
      Silt,
      color = Location
    ),
    size = 3,
    inherit.aes = FALSE
  )



ternPlot_clean <- ggtern(data = mock_data, aes(x = Sand, y = Clay, z = Silt, color = Location)) +
                  geom_point() +
                  theme_showsecondary() +
                  theme_showarrows() +
                  theme_rgbw() +
                  labs(title = "USDA Textural Classification Chart",
                       fill = "Sample Location",
                       color = "Textural Class")










