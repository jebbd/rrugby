title<-data%>%glue::glue_data("{Home} vs {Away} at {Stadium} on {lubridate::day(Date)} {lubridate::month(Date,abbr=TRUE,label=TRUE)} {lubridate::year(Date)}")
tidyr::unnest(data,player_stats)%>%
mutate(Strip=case_when(
       `Home/Away` == "Home" ~ Home_strip,
       `Home/Away` == "Away" ~ Away_strip
  ))%>%
  mutate(brightness=(colorspace::coords(as(colorspace::hex2RGB(Strip),"polarLUV"))%>%as.data.frame())$L)%>%
  relocate(Strip,brightness)%>%
  mutate(x=case_when(
    Number == 1 ~ 4,
    Number == 2 ~ 5,
    Number == 3 ~ 6,
    Number == 4 ~ 4.5,
    Number == 5 ~ 5.5,
    Number == 6 ~ 3.75,
    Number == 7 ~ 6.25,
    Number == 8 ~ 5,
    Number == 9 ~ 4.5,
    Number == 10 ~ 6,
    Number == 11 ~ 3.5,
    Number == 12 ~ 7,
    Number == 13 ~ 8,
    Number == 14 ~ 9,
    Number == 15 ~ 5,
    Number == 16 ~ 10.5,
    Number == 17 ~ 10.5,
    Number == 18 ~ 10.5,
    Number == 19 ~ 10.5,
    Number == 20 ~ 10.5,
    Number == 21 ~ 10.5,
    Number == 22 ~ 10.5
  ))%>%mutate(y=case_when(
    Number == 1 ~ 1.5,
    Number == 2 ~ 1.5,
    Number == 3 ~ 1.5,
    Number == 4 ~ 3.25,
    Number == 5 ~ 3.25,
    Number == 6 ~ 4.25,
    Number == 7 ~ 4.25,
    Number == 8 ~ 5,
    Number == 9 ~ 7.25,
    Number == 10 ~ 8.5,
    Number == 11 ~ 9.5,
    Number == 12 ~ 9.25,
    Number == 13 ~ 9.5,
    Number == 14 ~ 10,
    Number == 15 ~ 12,
    Number == 16 ~ 1.5,
    Number == 17 ~ 3.25,
    Number == 18 ~ 5,
    Number == 19 ~ 6.75,
    Number == 20 ~ 8.5,
    Number == 21 ~ 10.25,
    Number == 22 ~ 12
  ))%>%relocate(x,y)%>%
  mutate(y=case_when(
    `Home/Away`== "Away" ~ y*-1,
    TRUE ~ y
  ))%>%
  mutate(glued_stats=glue::glue(
    "<b>{Player}</b>
     Try Scored: {Try_Scored}
     Metres Made: {Ball_Carry_Meters}
     Ball Carries: {Ball_Carry}
     Line Breaks: {Line_Break}
     Defenders Beaten: {Beat_Defender}
     Passes: {Passes}
     Offloads: {Offload}
     Tackles Completed: {Tackle_Made}
     Tackles Missed: {Tackle_Missed}
     Turnovers Won: {Total_Turnovers_Gained}
     Turnovers Conceded: {Total_Turnovers_Conceded}
     Penalties Conceded: {Penalties_Conceded}
     Yellow Card: {Yellow_Card}
     Red Card: {Red_Card}
     "
  ))->stat_pitch

  stat_pitch%>%
  ggplot(aes(x=x,y=y,fill=Strip,label=Number))+
  geom_rect(fill="chartreuse4",colour=NA,ymax=14,ymin=-14,xmin=0,xmax=9.75)+
  geom_segment(lwd=1,linetype=5,y=12/50*10,yend=12/50*10,x=0,xend=9.75,colour="white")+
  geom_segment(lwd=1,linetype=1,y=12/50*32,yend=12/50*32,x=0,xend=9.75,colour="white")+
  geom_segment(lwd=1,linetype=5,y=12/50*50,yend=12/50*50,x=0,xend=9.75,colour="white")+
  geom_segment(lwd=1,linetype=1,y=0,yend=0,x=0,xend=9.75,colour="white")+
  geom_segment(lwd=1,linetype=5,y=-12/50*10,yend=-12/50*10,x=0,xend=9.75,colour="white")+
  geom_segment(lwd=1,linetype=1,y=-12/50*32,yend=-12/50*32,x=0,xend=9.75,colour="white")+
  geom_segment(lwd=1,linetype=5,y=-12/50*50,yend=-12/50*50,x=0,xend=9.75,colour="white")+
  geom_point(size=10,shape=21,colour="black",aes(text=glued_stats))+
  geom_text(aes(colour=brightness<45))+
  xlim(c(2.5,11))+ylim(c(-12,12))+scale_fill_identity()+
  scale_colour_manual(values = c("black","white"))+
  labs(x="",y="",title=title)+
  theme(legend.position = "none",
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank())->pitch_plot

plotly::ggplotly(pitch_plot,tooltip="text")





