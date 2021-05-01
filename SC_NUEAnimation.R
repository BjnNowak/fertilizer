# Clear space 
#############
rm(list=ls())
gc()

# Load packages 
###############
library(tidyverse)
library(plyr)
library(gganimate)
library(showtext)
library(ggrepel)

# Import data
#############
# Read in with tidytuesdayR package 
# install.packages("tidytuesdayR")
tuesdata <- tidytuesdayR::tt_load('2020-09-01')
# Select the yield/fertilizer table
ferti <- tuesdata$cereal_crop_yield_vs_fertilizer_application%>%
  filter(complete.cases(.))
colnames(ferti)<-c("Country","Code","Year",'Cereal_Yield',"Nitrogen")

# Data preparation
##################
# Select countries of interest
ferti_countries <- filter(
  ferti,
  ferti$Country=="France"|
  ferti$Country=="Germany"|
  ferti$Country=="Ukraine"|
  ferti$Country=="United States"|
  ferti$Country=="Brazil"|
  ferti$Country=="Argentina"|
  ferti$Country=="Australia"|
  ferti$Country=="Finland"|
  ferti$Country=="Canada"|
  ferti$Country=="China"|
  ferti$Country=="India"|
  #ferti$Country=="Japan"|
  #ferti$Country=="Nepal"|
  ferti$Country=="Senegal"|
  ferti$Country=="Kenya"|
  ferti$Country=="Pakistan"|
  ferti$Country=="Italy"|
  ferti$Country=="Norway"|
  ferti$Country=="Cote d'Ivoire"
)

ferti_nocountries <- ferti%>% # Table without country name (for the animation)
  select(-Country)

resume_countries<-ddply( # Calculate mean and sd per country for the period
  ferti_countries,
  .(Country),
  summarize,
  mean_N = mean(Nitrogen),
  sd_N = sd(Nitrogen),
  mean_Y = mean(Cereal_Yield),
  sd_Y = sd(Cereal_Yield),
  eff=mean_N/mean_Y
)%>%mutate(pal = case_when( # Create color palette based on N use efficiency
  eff < 10  ~ "royalblue",
  (eff >= 10)&(eff < 20)  ~ "lightblue4",
  (eff >= 20)&(eff < 30)  ~ "lightgoldenrod4",
  (eff >= 30)&(eff < 40)  ~ "indianred4",
  TRUE ~ "red1"
))%>%  mutate(li = case_when( # Create linetype per country
  Country=="China"|
  Country=="United States"|
  Country=="India"|
  Country=="Argentina"|
  Country=="Ukraine"|
  Country=="Brazil"|
  Country=="Canada"
    ~ "solid",
  TRUE ~ "blank"
))%>%  mutate(la = case_when( # Create linetype per country
  Country=="Canada"~ "Higher yield,\nlower efficiency",
  Country=="Brazil"~ "Higher yield,\nsame efficiency",
  Country=="United States"~"Increase in yield\nand efficiency",
  TRUE ~ ""
))
  
ferti_countries_pal<-merge(ferti_countries,resume_countries,by="Country") # Add palette to country individual values


# Prepare the plot
##################
# Load fonts
font_add_google(name = "Roboto", family = "Roboto")
showtext_auto()
font_add_google(name = "Rambla", family = "Rambla")
showtext_auto()

# Select font and some colors
ft<-"Roboto"
ft2<-"Rambla"
ax<-"#514242"
ax2<-"grey80"
back<-"white"


pl<-ggplot(data=resume_countries,aes(x=mean_N,y=mean_Y,color=Country))+
  annotate(geom="segment",x=0,xend=250,y=3,yend=3,color="black",size=0.1,alpha=0.2)+
  annotate(geom="segment",x=0,xend=250,y=6,yend=6,color="black",size=0.1,alpha=0.2)+
  annotate(geom="segment",x=50,xend=50,y=0,yend=9,color="black",size=0.1,alpha=0.2)+
  annotate(geom="segment",x=150,xend=150,y=1,yend=9,color="black",size=0.1,alpha=0.2)+
  geom_point(data=filter(ferti_nocountries,Nitrogen<255),aes(y=Cereal_Yield,x=Nitrogen),color="dimgrey",alpha=0.2)+
  scale_fill_gradient(low="white",high="black")+
  scale_y_continuous(breaks=seq(3,6,3),limits=c(0,9))+
  scale_x_continuous(breaks=seq(50,150,100),limits=c(0,265))+
  annotate(geom="text",x=265,y=9,color=ax,size=3,label="Data source: FAO Stats & OurWorldInData, from TidyTuesday",family=ft,hjust=0,vjust=1,angle=270)+
  annotate(geom="text",x=0,y=6.5,color="royalblue",size=4,label="High\nefficiency",family=ft,hjust=0)+
  annotate(geom="text",x=250,y=3.5,color="red1",size=4,label="Low\nefficiency",family=ft,hjust=1)+
  annotate(geom="segment",x=0,xend=90,y=0,yend=9,linetype="twodash",color=ax,size=0.1)+
  annotate(geom="segment",x=0,xend=20*9,y=0,yend=9,linetype="twodash",color=ax,size=0.1)+
  annotate(geom="segment",x=0,xend=250,y=0,yend=250/30,linetype="twodash",color=ax,size=0.1)+
  annotate(geom="segment",x=0,xend=250,y=0,yend=250/40,linetype="twodash",color=ax,size=0.1)+
  annotate(geom="text",x=75,y=8,angle=67,color=ax,size=3.5,label=expression(paste("10kgN.t"^"-1")))+
  annotate(geom="text",x=154,y=8,angle=52,color=ax,size=3.5,label=expression(paste("20kgN.t"^"-1")))+
  annotate(geom="text",x=232,y=8,angle=41,color=ax,size=3.5,label=expression(paste("30kgN.t"^"-1")))+
  annotate(geom="text",x=231,y=6,angle=34,color=ax,size=3.5,label=expression(paste("40kgN.t"^"-1")))+
  geom_point(
    data=ferti_countries_pal,
    aes(y=Cereal_Yield,x=Nitrogen,fill=Year),
    size=5,alpha=1,pch=21)+
  geom_path(
    data=ferti_countries_pal,
    aes(x=Nitrogen,y=Cereal_Yield,linetype=Country),
    arrow = arrow(type = "open", angle = 30, length = unit(0.1, "inches")),
    size=1.5
  )+ 
  scale_linetype_manual(
    values=resume_countries$li)+
  geom_label(
    data=filter(ferti_countries_pal,Year==2002|Year==2017),
    aes(y=Cereal_Yield,x=Nitrogen,label=Year),family=ft2
  )+
   geom_text(data=resume_countries,aes(
    label=paste(Country,"needed",scales::number(eff,acc=1,big.mark = ""),"kgN\nto produce one ton of cereal"),
    color=Country),
    x=250,y=0,vjust=0,hjust=1,family=ft2,size=5
  ) +
  #geom_linerange(aes(ymin = mean_Y-sd_Y,ymax = mean_Y+sd_Y),size=2)+
  #geom_linerange(aes(xmin = mean_N-sd_N,xmax = mean_N+sd_N),size=2)+
  geom_label_repel(
    aes(x=mean_N,y=mean_Y,label=Country),family=ft2,
    fontface="bold",size=6,
    nudge_x=15,nudge_y =-1,hjust=0, min.segment.length = Inf)+
  geom_text(
    aes(x=mean_N+18,y=mean_Y-1.8,label=la),family=ft2,
    fontface="bold",size=4,hjust=0)+
  scale_colour_manual(values = resume_countries$pal)+
  labs(
    title="How much fertilizer for one ton of cereal?",
    x=expression(paste("Nitrogen fertilizer inputs (kgN.ha"^"-1",")")),
    y=expression(paste("Cereal yield (t.ha"^"-1",")")),
    subtitle="Dots show annual values per country, from white to black from\n2002 to 2017, and connected by a line for most evident trends"
  )+
  theme(
    text= element_text(family = ft),
    plot.caption= element_text(family = ft,size=4,angle=90),
    plot.subtitle = element_text(face="plain",lineheight=1.2),
    plot.title=element_text(face="bold",lineheight=1.2),
    panel.background = element_rect(fill=back),
    plot.background = element_rect(fill=back,color=back),
    legend.position="none",
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5,size=12,color=ax ),
    axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=0.5,size=12,color=ax),
    axis.title.x = element_text(vjust = 0,size=12,face="plain",color=ax, margin = margin(t = 10, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(vjust = 0,size=12,face="plain",color=ax, margin = margin(t = 0, r = 10, b = 0, l = 0)),
    axis.ticks = element_blank(),
  ) +
  transition_states(Country,
                    transition_length = 0,
                    state_length = 5)+ 
  ease_aes('cubic-in-out')

frames_per_country<-5 #Increase for slower animation
animation<-animate(
  pl,
  nframes=length(resume_countries$Country)*frames_per_country,
  start_pause = 0,
  end_pause = 0,
  renderer=magick_renderer(loop = TRUE)
)

animation
anim_save("Animation_NUE.gif", animation)

