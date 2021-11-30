#packages
library(ggplot2)
library(reshape)
           #CREATE FUNCTIONS 


#### creates a summary of a specific pitcher. Only pitcherid or pitcher name is required 
pitcher_sum <- function(baseball_data,PitcherId,BatterSide,Date,Semester){
  
  #subset the data by name or id 
  all_pitches<-baseball_data[which(baseball_data$PitcherId ==PitcherId),]
  if(nrow(all_pitches)==0){
    all_pitches<-baseball_data[which(baseball_data$Pitcher ==PitcherId),]
  }
  
  
  #if extra arguments are provices then subset the data further 
  if(!missing(BatterSide)) {  if(BatterSide!="Both"){ all_pitches<- all_pitches[which(all_pitches$BatterSide ==BatterSide),]  } }
  if(!missing(Date)) {all_pitches<- all_pitches[which(all_pitches$Date ==Date),]  }
  if(!missing(Semester)) {all_pitches<- all_pitches[which(all_pitches$Semester ==Semester),]  }
  
  
  #create columns
 
  
  total<-  tapply(all_pitches$PitcherId,all_pitches$TaggedPitchType, length)
  speed<-  tapply(all_pitches$RelSpeed,all_pitches$TaggedPitchType, function(x) mean(x,na.rm = TRUE))
  spin<-   tapply(all_pitches$SpinRate ,all_pitches$TaggedPitchType, function(x) mean(x,na.rm = TRUE))
  

  return(na.omit(data.frame(total,speed,spin)))
  
}


#### creates a summary of a specific batter. Only batterid or batter name is required 
batter_sum <- function(baseball_data,BatterId,PitcherThrows,Date,Semester){
  
  #subset data by either batter id or batter name 
  all_pitches<-baseball_data[which(baseball_data$BatterId ==BatterId),]
  if(nrow(all_pitches)==0){
    all_pitches<-baseball_data[which(baseball_data$Batter ==BatterId),]
  }
  
  
  #if extra arguments are provided then subset the data further 
  if(!missing(PitcherThrows)) {  if(PitcherThrows!="Both"){ all_pitches<- all_pitches[which(all_pitches$PitcherThrows ==PitcherThrows),]  } }
  if(!missing(Date)) {all_pitches<- all_pitches[which(all_pitches$Date ==Date),]  }
  if(!missing(Semester)) {all_pitches<- all_pitches[which(all_pitches$Semester ==Semester),]  }
  
  
  #create columns
  total<-  tapply(all_pitches$BatterId,all_pitches$TaggedPitchType, length)
  exit_velo<-  tapply(all_pitches$ExitSpeed,all_pitches$TaggedPitchType,function(x){median(x,na.rm = TRUE)})
  
  
  is_contact<-function(x){  
  if( x=='InPLay'  | x=='FoulBall'){1}
  else if(x =='StrikeSwinging'){0}
 
   else{NA}
  }

  is_groundball<-function(x){  
    if( x=='GroundBall'  ){1}
    else if(x !='Undefined'){0}
    else{NA}
  }  
  
  all_pitches$contact<- sapply(all_pitches$PitchCall,is_contact)
  all_pitches$groundball<- sapply(all_pitches$HitType,is_groundball)
  
  swing_strike_perc<-round(1-  tapply(all_pitches$contact ,all_pitches$TaggedPitchType,function(x){mean(x,na.rm = TRUE)}),2)*100
  groundball_perc<-round(tapply(all_pitches$groundball,all_pitches$TaggedPitchType,function(x){mean(x,na.rm = TRUE)}),2)*100
  
  return(na.omit(data.frame(total,exit_velo,swing_strike_perc,groundball_perc)))
  
}



##### creates a spray chart for a specifc hitter. only batterId or batter name is requried 
spray_chart <- function(baseball_data,BatterId,PitcherThrows){
  
  
  #subset data by either batter id or batter name 
  all_pitches<-baseball_data[which(baseball_data$BatterId ==BatterId),]
  if(nrow(all_pitches)==0){
    all_pitches<-baseball_data[which(baseball_data$Batter ==BatterId),]
  }
  
  #if extra arguments are provided then subset the data further 
  if(!missing(PitcherThrows) ) {if( PitcherThrows!= "Both") {all_pitches<- all_pitches[which(all_pitches$PitcherThrows ==PitcherThrows),]  }}
  

  #removes foul hits and adds a new column that makes plotting easier using trig 
   all_pitches<-all_pitches[which(abs(all_pitches$Bearing)<=45),]
   all_pitches$y_Distance<-all_pitches$Distance * cos((all_pitches$Bearing *pi) /180) 
   all_pitches$X_Distance<-all_pitches$Distance * sin((all_pitches$Bearing *pi) /180) 
  
   #pythag to create bases 
   bases<-data.frame(x=c(0,90/sqrt(2),0,-90/sqrt(2),0),
                     y=c(0,90/sqrt(2),2*90/sqrt(2),90/sqrt(2),0 ))
   
  p<-ggplot(all_pitches,aes(X_Distance,y_Distance))+
    geom_point(aes(colour=PlayResult,shape=HitType,size=rel(2)))+
    coord_equal()+
    geom_path(aes(x=x,y=y),data=bases)+
    geom_segment(x=0,xend=300,y=0,yend=300)+
    geom_segment(x=0,xend=-300,y=0,yend=300)+
    ggtitle(BatterId)+
    theme_classic(base_size = 15)+
    theme( plot.title = element_text(hjust = 0.5),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())+ 
   guides(colour = guide_legend(override.aes = list(size=8)))+
  guides(shape = guide_legend(override.aes = list(size=8)))+
    scale_size(guide = 'none')
  
  
 
  
  return(p)
  
  
}

###build a lauch angle histogram
LA_plot <- function(baseball_data,BatterId,PitcherThrows,Semester){
  #subset data by either batter id or batter name 
  all_pitches<-baseball_data[which(baseball_data$BatterId ==BatterId),]
  if(nrow(all_pitches)==0){
    all_pitches<-baseball_data[which(baseball_data$Batter ==BatterId),]
  }
  #if extra arguments are provided then subset the data further 
  if(!missing(PitcherThrows) ) {if( PitcherThrows!= "Both") {all_pitches<- all_pitches[which(all_pitches$PitcherThrows ==PitcherThrows),]  }}
  if(!missing(Semester)) {all_pitches<- all_pitches[which(all_pitches$Semester ==Semester),]  }
  

  p = ggplot(all_pitches,aes(x=ifelse(Angle >= 0, abs(Angle-90), abs(Angle+90))
                               ,y=ExitSpeed,color=HitType,size=rel(2)))+
    geom_point()+
    ylim(0,120)+
    scale_x_continuous(limits=c(0,360), breaks=seq(0,359,45),labels =c(90,45,0,-45,-90,'','','')  )+
    coord_polar()+
    theme_bw(base_size = 15)+
    ggtitle(BatterId)+
    geom_text(aes( 0 ,   y = 30,   label = "30"), color = 'black',  size = 5,  nudge_y = .005)+
    geom_text(aes( 0 ,   y = 60,   label = "60"), color = 'black',  size = 5,  nudge_y = .005)+
    geom_text(aes( 0 ,   y = 90,   label = "90"), color = 'black',  size = 5,  nudge_y = .005)+
    geom_text(aes( 0 ,   y = 120,   label = "120"), color = 'black',  size = 5,  nudge_y = .005)+
    
     theme(plot.title = element_text(hjust = 0.5),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.y=element_blank()
           
           )+

  guides(color = guide_legend(override.aes = list(size=8)))+ 
    
    scale_size(guide = 'none')

  
  return(p)
  
  
}

#builds a variety of different heatmaps 
player_heatmap<-function(baseball_data,PlayerId,pitcher=TRUE,measure='ExitSpeed',PitcherThrows,BatterSide,Semester,smooth=FALSE){
  #subset data by either player id or player name 
all_pitches=data.frame()

##if pitcher is set to true, search for this player as a pitcher. If false search for him as a batter  
  if(pitcher==TRUE){
    
    all_pitches<-baseball_data[which(baseball_data$PitcherId == PlayerId),]
    if(nrow(all_pitches)==0){
      all_pitches<-baseball_data[which(baseball_data$Pitcher ==PlayerId),]
    }
  
  }
  
  if(nrow(all_pitches)==0){
    all_pitches<-baseball_data[which(baseball_data$Batter ==PlayerId),]
  }
  if(nrow(all_pitches)==0){
    all_pitches<-baseball_data[which(baseball_data$BatterId ==PlayerId),]
  }
  

  
  
  
  #if extra arguments are provided then subset the data further
  if(!missing(PitcherThrows) ) {if( PitcherThrows!= "Both") {all_pitches<- all_pitches[which(all_pitches$PitcherThrows ==PitcherThrows),]  }}
  if(!missing(Semester)) {all_pitches<- all_pitches[which(all_pitches$Semester ==Semester),]  }
  if(!missing(BatterSide)) {  if(BatterSide!="Both"){ all_pitches<- all_pitches[which(all_pitches$BatterSide ==BatterSide),]  } }
 
  #sets up the zones data frame for later graphs
  zone_rates<-data.frame()
  step_x<-.6
  step_y<-.6
  zone_sequence_x<-seq(-1.8,1.8,step_x)
  zone_sequence_y<-seq(0.1,5,step_y)
  total_zone<-length(zone_sequence_x)*length(zone_sequence_y)
  zone_rates[1:total_zone,"zones"]<-as.vector(1:total_zone) 
  zone_rates$x<-0
  zone_rates$y<-0
  #defines the x and y limits for each zone
  cnt<-1
  for(i in zone_sequence_y){
    for(j in zone_sequence_x){
      
      zone_rates$x[cnt]<-j
      zone_rates$y[cnt]<-i
      
      cnt<-cnt+1;        
      
    }
    
  }
  #function to identify zone of a specific point
  find_zone<-function(x,y){
    cnt<-1
    call<-NA
    for(i in zone_sequence_y){
      for(j in zone_sequence_x){
        if(y<i & y >=i-step_y & x<j & x>=j-step_x ){
          call<-cnt
          return(call)
        }
        cnt<-cnt+1
      }
    }
    return(call)
  }

  #remove pitches with NA for location and apply find_zone function to all pitches  
  all_pitches<-all_pitches[which(!is.na(all_pitches$PlateLocHeight) & !is.na(all_pitches$PlateLocSide)   ),]
  all_pitches$zone<-mapply(find_zone,all_pitches$PlateLocSide,all_pitches$PlateLocHeight)
    
  #find the value per zone for the meaasure that was selected 
  if(measure=='ExitSpeed' ){
  zone_rates$value<-sapply(zone_rates$zones,function(x) round(mean(all_pitches$ExitSpeed[which(all_pitches$zone==x)],na.rm=TRUE) ))
  }
  else if (measure=='Frequency'){
    zone_rates$value<-sapply(zone_rates$zones,function(x) length(all_pitches$ExitSpeed[which(all_pitches$zone==x)]) )
  }

  else if (measure=='Iso'){
    #function that incorporates the iso forumula and finds it for a specific zone
    find_iso<-function(x){
      all_pitches<-all_pitches[which(all_pitches$zone==x),]  
      ab<-length(which(all_pitches$PlayResult %in% c('Single','Double','Triple','HomeRun','Out')  ))
      if(is.na(ab)){ 
         return(0)
      }  
    
      else {
        return(round(  (length(which(all_pitches$PlayResult=='Double'))+ 
                       (2*length(which(all_pitches$PlayResult=='Triple')))+ 
                       (3*length(which(all_pitches$PlayResult=='HomeRun'))))/ 
                        ab ,2))
      }
    }
    zone_rates$value<-sapply(zone_rates$zones,find_iso )
  }
  else if (measure=='Slug'){  
    find_slug<-function(x){
      all_pitches<-all_pitches[which(all_pitches$zone==x),]  
      ab<-length(which(all_pitches$PlayResult %in% c('Single','Double','Triple','HomeRun','Out')  ))
      if(is.na(ab)){ 
        return(0)
      }  
    
      else {
        return(round( (length(which(all_pitches$PlayResult=='Single'))+
                      (2*length(which(all_pitches$PlayResult=='Double'))) +
                      (3*length(which(all_pitches$PlayResult=='Triple')))+ 
                      (4*length(which(all_pitches$PlayResult=='HomeRun')))) /
                      ab ,2))
      }
    }
    zone_rates$value<-sapply(zone_rates$zones,find_slug )
  }
  else if (measure=='Miss'){  
    #function that determines if there was contact on a pitch
    is_contact<-function(x){  
    if( x=='InPLay'  | x=='FoulBall'){1}
    else if(x =='StrikeSwinging'){0}
    else{NA}
    }

  
    all_pitches$contact<- sapply(all_pitches$PitchCall,is_contact)
    zone_rates$value<-sapply(zone_rates$zones,function(x) 100-(round(mean(all_pitches$contact[which(all_pitches$zone==x)],na.rm=TRUE),2)*100))
   }
  
  
  
  
  if(smooth==TRUE){
      fill<-geom_raster(aes(fill=value), interpolate = TRUE)
      text<-geom_blank()
  }
  else{
      fill<-geom_tile(aes(fill=value))
      text<-geom_text(aes(label=value))
  }

  ##estimates of borders of the strikezone
  topKzone<- 3.5
  botKzone<-1.6
  inKzone <- -.95
  outKzone <- 0.95

  ###ploting info. fill and text are defined above and determine the different between smooth and not smooth 
  p<-ggplot(zone_rates,aes(x,y))+
    fill+
    text+
    coord_equal()+
    geom_segment(x=inKzone,xend=inKzone,y=botKzone,yend=topKzone)+
    geom_segment(x=outKzone,xend=outKzone,y=botKzone,yend=topKzone)+
    geom_segment(x=inKzone,xend=outKzone,y=botKzone,yend=botKzone)+
    geom_segment(x=inKzone,xend=outKzone,y=topKzone,yend=topKzone)+
    scale_fill_gradient(low = "green", high = "red")+
    ggtitle(paste(PlayerId,"(Pitchers View)",sep = " "))+
    theme(plot.title = element_text(hjust = 0.5),axis.title.x = element_blank(),axis.title.y = element_blank() )+
    guides(fill=guide_legend(title=as.character(measure) ))+
   theme(legend.text=element_text(size=25))
    return(p)
}  


pitcher_counts<-function(baseball_data,PitcherId,BatterSide,Semester){
  #subset data by either batter id or batter name 
  all_pitches<-baseball_data[which(baseball_data$PitcherId ==PitcherId),]
  if(nrow(all_pitches)==0){
    all_pitches<-baseball_data[which(baseball_data$Pitcher ==PitcherId),]
  }
  
  #if extra arguments are provided then subset the data further 
  if(!missing(BatterSide)) {  if(BatterSide!="Both"){ all_pitches<- all_pitches[which(all_pitches$BatterSide ==BatterSide),]  } }
  if(!missing(Semester)) {all_pitches<- all_pitches[which(all_pitches$Semester ==Semester),]  }
  
   all_pitches$count<-paste(all_pitches$Balls,all_pitches$Strikes,   sep='-')
  
   counts<- c('0-0','0-1','0-2','1-0','1-1','1-2','2-0','2-1','2-2','3-0','3-1','3-2')
   get_count_dist<- function(COUNT){
      pitches_in_count<-all_pitches[which(all_pitches$count==COUNT),]
      return(tapply(pitches_in_count$PitcherId,pitches_in_count$TaggedPitchType, function(x) length(x)/nrow(pitches_in_count)   ))
   }

   pitch_distribution<- reshape::melt(sapply(counts,get_count_dist))
   colnames(pitch_distribution)<-c('Pitch_Type','Count','Value')
   p<-  ggplot(pitch_distribution,aes(x='',y=Value,fill=Pitch_Type))+
     geom_bar(width = 1,stat="identity",position = position_fill())+
     coord_polar("y") + 
 #    coord_equal()+
     scale_fill_brewer(palette='Set2') +
     geom_text(aes(label=paste0(round(Value,2)*100,'%','')), position = position_fill(vjust = 0.5),size=5,fontface='bold')+
     theme_dark()+
     theme(axis.text.x=element_blank())+
     theme(plot.title = element_text(hjust = 0.5))+
     facet_wrap(~ Count) + 
    
      theme(
         axis.title.x = element_blank(),
         axis.title.y = element_blank(),
         strip.text = element_text(size=rel(2)),
         panel.border = element_blank(),
         panel.grid=element_blank(),
         axis.ticks = element_blank(),
         plot.background = element_rect(fill = "grey") 
          ) +
    guides(color = guide_legend(override.aes = list(size=8)))

        
   return(p) 
  
  
   
  
}



