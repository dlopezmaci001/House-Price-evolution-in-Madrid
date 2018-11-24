# Libraries

library(ggplot2)
library(gganimate)
library(dplyr)


# Load data

house_data <- read.csv2("YOUR DIR"/barrios.csv",sep=",")
str(house_data)

# Transform the variables to the corresponding format

house_data$Quarter <- as.Date(house_data$Quarter, format = "%d-%m-%y")

house_data$Index <- as.numeric(house_data$Index)

house_data$Area <- as.character(house_data$Area)

#We create a subset to plot and see how the final graph will look

house_data_2001 <- house_data %>% filter(Quarter == "2001-01-01")

 # Plot

ggplot(house_data_2001,
       aes(x=Area,y=Index, label= Area, color = Area))+
  ggtitle("Evolución del precio de la Vivienda por M2")+
  xlab("Distrito") +
  ylab("Precio de la vivienda por M2") +
  geom_point(stat='identity',size=10)+
  geom_segment(aes( y=100,
                    x= Area,
                    xend=Area,
                    yend=Index)
  ) +
  geom_text(color="black",size=3) +
  coord_flip()+
  theme(legend.position = "none")

# Code for animation

ggplot(house_data,
       aes(x=Area,y=Index, label=Area, color=Area)) + 
  ggtitle("Evolución del precio de la Vivienda por M2")+
  xlab("Precio de la vivienda por M2") +
  ylab("Distrito") +
  geom_point(stat='identity', size=18) +
  geom_segment(aes(
    y=200,
    x=Area,
    yend=Index,
    xend=Area)
  ) + 
  geom_text(color="black",size=3)+
  coord_flip() +
  theme(legend.position = "none") +
  labs(title="Evolución del precio de la Vivienda por M2",subtitle= 'Trimestre: {frame_time}',x='Distrito',y="Precio M2")+
  transition_time(Quarter) +
  ease_aes("linear")

anim_save("precio_vivienda.gif")

# Change in price since 2007 to 2018

# Extract values for prices in 2018

house_data_2018 <- house_data %>% filter(Quarter > "2017-12-31")

# values for 2007

house_data_2007 <- house_data %>% filter(Quarter > "2006-12-31" & Quarter < "2007-10-01")

# Calculate the percentage change

perc_change <- as.data.frame((((house_data_2018$Index-house_data_2007$Index)/house_data_2007$Index)*100))

variation <- cbind(perc_change,house_data_2018$Quarter,house_data_2007$Area)

colnames(variation) <- c("perc_change","Quarter","Area")

# We create a subset to prepare the graph

variation_2018 <- variation %>% filter(Quarter == "2018-01-01")

# Plot 

ggplot(variation_2018,
       aes(x=Area,y=perc_change, label= Area, color = Area))+
  ggtitle("Variación en el precio por M2 por distrito (Comparativa 2018 vs. 2007)")+
  xlab("Distrito") +
  ylab("Precio de la vivienda por M2") +
  geom_point(stat='identity',size=10)+
  geom_segment(aes( y=0,
                    x= Area,
                    xend=Area,
                    yend=perc_change)
  ) +
  geom_text(color="black",size=3) +
  coord_flip()+
  theme(legend.position = "none")

# Code for animation

ggplot(variation,
       aes(x=Area,y=perc_change, label=Area, color=Area)) + 
  ggtitle("Variación en el precio por M2 por distrito (2018 vs. 2007)")+
  xlab("Distrito") +
  ylab("Precio de la vivienda por M2") +
  geom_point(stat='identity', size=18) +
  geom_segment(aes(
    y=0,
    x=Area,
    yend=perc_change,
    xend=Area)
  ) + 
  geom_text(color="black",size=3)+
  coord_flip() +
  theme(legend.position = "none") +
  labs(title="Variación en el precio por M2 por distrito (Comparativa 2018 vs. 2007)",subtitle= 'Trimestre: {frame_time}',x='Distrito',y="Precio M2")+
  transition_time(Quarter) +
  ease_aes("linear")

anim_save("precio_vivienda_perc_change.gif")
