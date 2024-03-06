library(tidyverse)
library(ggpubr)

theme_publish <- function(base_size = 12, base_family = "",
                          base_linewidth = 0.25, ...) {
  half_line <- base_size / 2
  small_rel <- 0.8
  small_size <- small_rel * base_size
  
  theme_bw(base_size = base_size, base_family = base_family, ...) %+replace%
    theme(
      rect = element_rect(fill = "transparent", colour = NA, color = NA,
                          linewidth = 0, linetype = 0),
      text = element_text(family = base_family, face = "plain",
                          colour = "black", size = base_size, hjust = 0.5,
                          vjust = 0.5, angle = 0, lineheight = 0.9,
                          margin = ggplot2::margin(), debug = F),
      
      axis.text = element_text(size = small_size),
      axis.text.x = element_text(margin = ggplot2::margin(t = small_size/4),
                                 vjust = 1),
      axis.text.y = element_text(margin = ggplot2::margin(r = small_size/4), 
                                 hjust = 1),
      axis.title.x = element_text(margin = ggplot2::margin(t = small_size,
                                                           b = small_size)),
      axis.title.y = element_text(angle = 90,
                                  margin = ggplot2::margin(r = small_size,
                                                           l = small_size/4)),
      axis.ticks = element_line(colour = "black", linewidth = base_linewidth),
      axis.ticks.length = unit(0.25, 'lines'),
      
      axis.line = element_line(colour = "black", linewidth = base_linewidth),
      axis.line.x = element_line(colour = "black", linewidth = base_linewidth), 
      axis.line.y = element_line(colour = "black", linewidth = base_linewidth), 
      
      legend.spacing = unit(base_size/4, "pt"),
      legend.key = element_blank(),
      legend.key.size = unit(1 * base_size, "pt"),
      legend.key.width = unit(1.5 * base_size, 'pt'),
      legend.text = element_text(size = rel(small_rel)),
      legend.title = element_text(size = rel(small_rel), face = 'bold'),
      legend.position = 'bottom',
      legend.box = 'horizontal',
      
      panel.spacing = unit(1, "lines"),
      panel.background = element_blank(),
      panel.border = element_blank(), 
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      
      strip.text = element_text(size = base_size),
      strip.background = element_rect(fill = NA, colour = "black",
                                      linewidth = 0.125),
      strip.text.x = element_text(face = 'bold', hjust = 0,
                                  margin = ggplot2::margin(b = small_size/2,
                                                           t = small_size/4)),
      strip.text.y = element_text(angle = -90, face = 'bold',
                                  margin = ggplot2::margin(l = small_size/2,
                                                           r = small_size/4)),
      
      plot.margin = unit(c(5,5,0,0), "pt"),
      plot.background = element_blank(),
      plot.title = element_text(face = "bold", size = 1.2 * base_size, 
                                margin = ggplot2::margin(b = half_line),
                                hjust = 0)
    )
}

df=read_csv()

df=df%>%
  mutate(TOP=mytriggs*creatureSum)
df1=df%>%
  filter(TOP>=100)



df2=df%>%
  filter(TOP<75)



df3=df%>%
  filter(TOP<233)%>%
  filter(TOP>=75)


ggplot(df,aes(x=TOP,y=meanhighscores))+
  geom_point(size=0.3)+
  geom_segment(x = 0, xend = 88.13, y = 0, yend = 1.76, color = "red")+
  geom_segment(x = 88.13, xend = 227.04, y = 1.76, yend = 99, color = "red")+
  geom_segment(x = 227.04, xend = Inf, y = 99, yend = 99, color = "red")+
  labs(y="Average Cards Milled")+
  labs(x="Triggers x Sum Creatures")+
  scale_x_continuous(limits = c(0,300))+
  theme_publish()

lm_fit1 <- lm(meanhighscores ~ 0+TOP ,data=df2)

beta1=as.numeric(coef(lm_fit1)[1])


x <- c(0:80)
f <- function(x) beta1*x

ggplot(df2,aes(x=TOP,y=meanhighscores))+
  geom_point(size=0.3)+
  stat_function(fun=f,colour="red")+
  scale_y_continuous(limits = c(0, 5))+
  annotate("text",x =6.1, y =4.8, label=paste("y=",round(beta1,digits=2),"(x)"))+
  annotate("text",x=5.8, y=4.5, parse=TRUE,label=paste("R^2 ==",round(summary(lm_fit1)$adj.r.squared,digits=3)))+
  labs(y="Average Cards Milled")+
  labs(x="Triggers x Sum Creatures")+
  theme_publish()


lm_fit2 <- lm(meanhighscores ~ TOP ,data=df3)
summary(lm_fit2)
intercept2=as.numeric(coef(lm_fit2)[1])
beta2=as.numeric(coef(lm_fit2)[2])

x <- c(80:233)
f <- function(x) beta2*x+intercept2
  
ggplot(df3,aes(x=TOP,y=meanhighscores))+
  geom_point(size=0.3)+
  stat_function(fun=f,colour="red")+
  annotate("text",x =88.5, y =99, label=paste("y=",round(beta2,digits=2),"(x)",round(intercept2,digits=2)))+
  annotate("text",x=84, y=92, parse=TRUE,label=paste("R^2 ==",round(summary(lm_fit2)$adj.r.squared,digits=3)))+
  labs(y="Average Cards Milled")+
  labs(x="Triggers x Sum Creatures")+
  theme_publish()


lm_fit3 <- lm(percentmill ~ I(log(TOP)),data=df1)

intercept3=as.numeric(coef(lm_fit3)[1])
beta3=as.numeric(coef(lm_fit3)[2])

x <- c(100:400)
f <- function(x) beta3*log(x)+intercept3
at50=exp((50-intercept3)/beta3)

ggplot(df1,aes(x=TOP,y=percentmill))+
  geom_point(size = 0.3)+
  labs(y="% Chance to Mill Opponets Out")+
  labs(x="Triggers x Sum Creatures")+
  stat_function(fun=f,colour="red")+
  geom_segment(x = -Inf, xend = at50, y = 50, yend = 50, color = "blue") +
  geom_segment(x = at50, xend = at50, y = -Inf, yend = 50, color = "blue")+
  scale_y_continuous(limits = c(0, 100))+
  annotate("text", x = 205, y = 0 , label = round(at50,digits=3))+
  annotate("text",x =143, y =97, label=paste("y=",round(beta3,digits=2),"ln(x)",round(intercept3,digits=2)))+
  annotate("text",x=122, y=91, parse=TRUE,label=paste("R^2 ==",round(summary(lm_fit3)$adj.r.squared,digits=3)))+
  theme_publish()
 

  
