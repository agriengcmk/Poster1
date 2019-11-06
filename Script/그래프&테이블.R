library(tidyverse)
library(knitr)



        

#데이터 불러오기
df_TMR1<- read_csv("Data/field exp_TMR pull only.csv") #하중-응력 그래프용
df_TMR<- read_csv("Data/field exp_TMR.csv")
df_COM<- read_csv("Data/field exp_COM.csv")

df_COM %>% select(Draw_Load2, Draw_Disp2)%>% filter(Draw_Load2==1000)


#응력 비교 테이블
df_TMR %>% filter(Draw_LOAD2==1071.47)%>% 
  select(Draw_STN01, Draw_STN03, Draw_STN05 ,Draw_STN07)*0.2

df_TMR %>% filter(Pin_LOAD2==1032.92)%>% 
  select(Pin_STN01, Pin_STN03,  Pin_STN05, Pin_STN07)*0.2


tribble(
  ~"실험체", ~ "S1 응력 (N/mm^2)", ~"S3 응력 (N/mm^2)", 
  ~"S5 응력 (N/mm^2)", ~"S7 응력 (N/mm^2)",
  "Exp_draw", -103.156, -109.616, 30.466, 44.062,
  "Exp_pin"  , -81.79, -74.4, 66.648, 65.652,
  "Exp_draw/Exp_pin", 1.26, 1.48, 0.46, 0.67)%>%
  kable(caption = "Table 3. 실험체별 응력 비교", align = c("c"))











# 횡강성 테이블
df_COM %>% mutate(num= rep(1:length(Draw_Disp2)))%>% 
  filter(num==51|num==101|num==151) %>%
  select(Draw_Disp2 ,Draw_Load2) %>% 
  mutate("Stiffness1 (N/mm)"=(Draw_Load2/Draw_Disp2))

df_COM %>% mutate(num= rep(1:length(Pin_Disp2)))%>% 
  filter(num==51|num==101|num==151) %>%
  select(Pin_Disp2 ,Pin_Load2) %>% 
  mutate("Stiffness1 (N/mm)"=(Pin_Load2/Pin_Disp2))

tribble(
  ~"목표변위 (mm)", ~"Exp_draw (N/mm)", ~"Exp_pin (N/mm)",
  "50.3", 8.84, 8.33,
  "100.0", 7.57, 7.26,
  "150.0", 6.98, 6.73) %>%
  kable(caption = "Table 3. 실험체별 횡강성", align = c("c"))









#하중-변위 그래프
g1<- ggplot(df_COM)+geom_line(aes(Draw_Disp2, Draw_Load2, col="Draw_Load2"), size=1.2)+
  geom_line(aes(Draw_Disp3, Draw_Load3, col="Draw_Load3"), size=1.2)+
  geom_segment(aes(x=0, xend=50.3, y=0, yend=445), color="red", size=1, linetype="dotted")+
  geom_segment(aes(x=0, xend=50.3, y=0, yend=440), color="blue", size=1, linetype="dotted")+
  geom_segment(aes(x=0, xend=150, y=0, yend=1047), color="red", size=1, linetype="dotted")+
  geom_segment(aes(x=0, xend=150, y=0, yend=1032), color="blue", size=1, linetype="dotted")+
  scale_colour_discrete(name="Load position", labels=c("Load 1", "Load 2"))+
  xlab("Displacement (mm)")+ylab("Load (N)")+
  ylim(0,1200)+
  theme_classic()+
  theme(
    axis.title = element_text(size=16),
    axis.text = element_text(size=14),
    legend.text=element_text(size=12),
    legend.title=element_text(size=14),
    legend.key.height = unit(0.8, "cm"),
    legend.key.width =  unit(0.8, "cm"),
    panel.background = element_blank(),
    legend.position = c(0.35, 0.85))


g2<- ggplot(df_COM)+geom_line(aes(Pin_Disp2, Pin_Load2, col="Pin_Load2"), size=1.2)+
  geom_line(aes(Pin_Disp3, Pin_Load3, col="Pin_Load3"), size=1.2)+
  geom_segment(aes(x=0, xend=50.2, y=0, yend=418), color="red", size=1, linetype="dotted")+
  geom_segment(aes(x=0, xend=50.1, y=0, yend=408), color="blue", size=1, linetype="dotted")+
  geom_segment(aes(x=0, xend=150, y=0, yend=1010.0), color="red", size=1, linetype="dotted")+
  geom_segment(aes(x=0, xend=150, y=0, yend=1077.0), color="blue", size=1, linetype="dotted")+
  scale_colour_discrete(name="Load position", labels=c("Load 1", "Load 2"))+
  xlab("Displacement (mm)")+ylab("Load (N)")+
  ylim(0,1200)+
  theme_classic()+
  theme(
    axis.title = element_text(size=16),
    axis.text = element_text(size=14),
    legend.text=element_text(size=12),
    legend.title=element_text(size=14),
    legend.key.height = unit(0.8, "cm"),
    legend.key.width =  unit(0.8, "cm"),
    panel.background = element_blank(),
    legend.position = "non"
  )


plot_grid(g1, g2, labels = c("A", "B"))







#하중-응력 그래프
df_TMR1<- mutate(df_TMR1, 
                time=rep(1:50, len=length(df_TMR1$Draw_LOAD2)),
                time1=rep(1:50, len=length(df_TMR1$Pin_LOAD2)))


df_TMR_Draw<- df_TMR1 %>% mutate(
              S1=(Draw_STN01)*0.2, S2=(Draw_STN02)*0.2,
              S3=(Draw_STN03)*0.2, S4=(Draw_STN04)*0.2,
              S5=(Draw_STN05)*0.2, S6=(Draw_STN06)*0.2,
              S7=(Draw_STN07)*0.2, S8=(Draw_STN08)*0.2)%>%
              filter(time==1) %>%
              select(Draw_LOAD2, S1:S8)%>%
              gather(type, value, -Draw_LOAD2) %>%
              ggplot(aes(value, Draw_LOAD2, col=type))+
              geom_path(size=1)+
              geom_point(size=1.5)+
              ylab("Load (N)")+xlab("Stress (MPa)")+ xlim(-120, 120)+
              theme_bw()+
              theme(axis.title = element_text(size=16),
                    axis.text = element_text(size=14),
                    strip.text = element_text(size=14),
                    legend.position = "top",
                    legend.text = element_text(size=14),
                    legend.title = element_blank())+
              guides(color=guide_legend(nrow=1))


df_TMR_Pin<- df_TMR1 %>% mutate(
              S1=(Pin_STN01)*0.2, S2=(Pin_STN02)*0.2,
              S3=(Pin_STN03)*0.2, S4=(Pin_STN04)*0.2,
              S5=(Pin_STN05)*0.2, S6=(Pin_STN06)*0.2,
              S7=(Pin_STN07)*0.2, S8=(Pin_STN08)*0.2)%>%
              filter(time1==1)%>%
              select(Pin_LOAD2, S1:S8) %>%
              gather(type, value, -Pin_LOAD2) %>%
              ggplot(aes(value, Pin_LOAD2, col=type))+
              geom_path(size=1)+
              geom_point(size=1.5)+
              ylab("Load (N)")+xlab("Stress (MPa)")+ xlim(-120, 120)+
              theme_bw()+
              theme(axis.title = element_text(size=16),
                    axis.text = element_text(size=14),
                    strip.text = element_text(size=14),
                    legend.position = "top",
                    legend.text = element_text(size=14),
                    legend.title = element_blank())+
              guides(color=guide_legend(nrow=1))








plot_grid(df_TMR_Draw, df_TMR_Pin, labels = c("A", "B"))





a<- c("실제 지반", "고정", "힌지", "스프링" )
Load1<- c(150, 46.23)
Load2<- c(150, 140.26)


