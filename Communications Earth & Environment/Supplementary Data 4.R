library(readr)
library(extrafont)
library(readxl)
library(ggplot2)
library(plotly)
library(tidyverse)
library(scales)
library(plyr)
library(RColorBrewer)



# type the file location of "Supplementary Data 1.xlsx"
setwd("C:/Users/")


mytheme <- theme_bw() + 
  theme(  plot.title = element_text(face = "bold", size = (15), hjust = 0.5)
          , text  = element_text(family = "sans")
          , axis.title = element_text(size = (10))
          , axis.text = element_text(size = (10))
          , legend.text = element_text(size = (10))
          , legend.position = "bottom"
          , legend.background = element_rect(linetype = "solid", color = "black")
          , strip.text = element_text(size = 10, color = "white", face = "bold")
          , strip.background = element_rect(color = "black", fill = "#447e59")
          , legend.title = element_blank()  
          , legend.spacing.y = unit(0, 'line')
          , legend.key.height=unit(0,"line")
  )

comColor <- c(
  'Grain' = '#f0e442' #yellow
  , 'Produce' =  '#009e73' # blueish green 
  , 'Meat & Poultry' = '#d55e00' #vermilion
  , 'Dairy' = '#56b4e9' # l blue 
  , 'Oil & Sugar' = "#cc79a7" # redish purple
  , 'Other'  = "#0072b2" # d blue
)#'#ff8000' # orange

  
colorStage <- c("On-farm" = "#8fbb55",
                "Manufacturing" = "#e5a940",
                "Distribution" = "#3ba2ad",
                "W&R" = "#5785b7",
                "Consumption" = "#cb4d3d")

colorDisEPA <- c( 
  
  "Food Donation" = '#0072b2' # d blue
  ,  "Animal Feed" = '#009e73' # green
  , "Industrial Uses" = '#f0e442' # yellow
  , "Compost/ Land App." = '#e69f00' # orange
  , 'Recycled (other)' = "#cc79a7" # red/purple
  , 'LWI' = "#d55e00" # vermilion 
  , "Unknown" = "#000000" #black
  # , "Water" = "#56b4e9" #l blue
)
# Figure 1  ----
sankeyNodes <- read_excel("manuscript Datafiles.xlsx", sheet = "Fig 1 Sankey nodes")
sankeyLinks <- read_excel("manuscript Datafiles.xlsx", sheet = "Fig 1 Sankey links")

plot_ly(
  type = "sankey",
  orientation = "h",

    node = list(
    label = sankeyNodes$nodeName, 
    x = sankeyNodes$pos_x,
    y = sankeyNodes$pos_y,
    color = sankeyNodes$nodeColor,
    
    
    pad = 10,
    thickness = 2,
    line = list(
      color = "black",
      width = 0.5
    )
  ),
  
  link = list(
    source = sankeyLinks$linkSource,
    target = sankeyLinks$linkTarget,
    value =  sankeyLinks$linkVal
    ,
    color = sankeyLinks$linkColor
  )
) 

# Figure 2 ----
commBar_untidy <- read_excel("manuscript Datafiles.xlsx", sheet = "Fig2_6_7")
commBar <- commBar_untidy %>% 
  filter(Stage != "Total") %>% 
  pivot_longer(!c(Stage, Stage2, flowType, flowType2, flowType3), names_to = "Commodity", values_to = "Value") %>% 
  group_by(flowType2, Commodity, Stage2, Stage)%>%
  dplyr::summarise(Value = sum(Value, na.rm = TRUE)) %>%
  ungroup

comLevels2 <- c('Grain', 'Dairy', 'Veg.', 'M&P', 'Fruit', 'Oil' , 'Sugar'
                , 'Eggs', 'Seafood', 'Nuts', 'Donated')

commBar$Commodity <- factor(commBar$Commodity, levels = comLevels2)
commBar$Stage <- factor(commBar$Stage, levels = c("On-farm","Manufacturing" ,"Distribution", "W&R", "Consumer", "Total"))
commBar$flowType2 <- factor(commBar$flowType2, levels = c("To Next Stage", "Water","Consumed","FLW"))

Fig2 <-
  commBar %>% filter(Commodity != "Donated") %>%
ggplot( 
  aes(x = factor(Stage2, levels = c("F","M" ,"D", "W/R", "C", "T"))
      , y = Value
      , fill = factor(flowType2, levels = c( "To Next Stage", "Water","Consumed","FLW"
      ))))+
  geom_bar(stat = "identity", colour = "black")+
  scale_fill_manual(values =  c( "#009e73", "#56b4e9","#f0e442","#d55e00"))+
  facet_wrap(factor(Commodity, levels = comLevels2)~., scales = "free_y", nrow = 2)+
  labs(x = "", y = "Quantity of Food (MMT)", fill = "")+
  guides(fill = guide_legend(nrow=1, byrow=TRUE)) +
  mytheme + theme(legend.position = "bottom",  axis.text.x = element_text(size = 8),  axis.text.y = element_text(size = 8))

ggsave("Fig2.pdf", device = "pdf", width = 18,  height = 8,  units = "cm")


# Figures 3 & 4 ----

foodFlow <- read_excel("manuscript Datafiles.xlsx",  sheet = "Fig3_4")
foodFlow <- filter(foodFlow, inTo != "Unharvested") %>% filter(inTo != "Unsold")

foodFlow <- foodFlow %>% rowwise() %>%
  mutate('Produce' = Fruit + Vegetables
         , 'Oil & Sugar' = Oil + Sugar
         , 'Other' = Seafood + Nuts + Eggs)

foodFlow <- foodFlow %>% select(-Oil, -Sugar, -Fruit, -Vegetables, -Seafood, -Nuts, -Eggs)

foodFlowGather <- foodFlow %>% pivot_longer(!c(Stage, Direction, flowName, inTo), names_to = "Commodity", values_to = "Value") %>% 
  filter(inTo != "Food Service" & inTo != "Household" & inTo != "Food Banks" & inTo != "Food Service Consumed"& inTo !=  "Household Consumed" & inTo != "Food Banks Consumed")

FLW <- filter(foodFlowGather, Direction == "FLW") %>% filter( Commodity != "Donated")
FLW$inTo <- factor(FLW$inTo, levels = c("On-farm", "Manufacturing", "Distribution", "W&R","Consumption" ,"Food Service", "Household"))

comLevels <- c('Grain', 'Produce', 'Meat & Poultry', 'Dairy', 'Oil & Sugar', 'Other', 'Donated')

Fig3 <-
  filter(FLW, Commodity != "Donated")%>%
  ggplot(aes(x = inTo, y = Value, fill = factor(Commodity, levels = comLevels)), color = "black")+
  geom_bar(stat = "identity", colour = "black")+
  scale_fill_manual(values = comColor)+
  labs(x = "", y = "Quantity of FLW (MMT)", fill = "")+
  guides(fill = guide_legend(nrow=1, byrow=TRUE)) +
  mytheme + theme(   legend.text = element_text(size = 9)
                     , axis.text.x = element_text(size = 10)
                     , axis.text.y = element_text(size = 10)
                     , axis.title = element_text(size = 10))

ggsave("Fig3.pdf", device = "pdf", width = 18,  height = 8,  units = "cm")


Fig4 <-
ggplot(FLW, aes(fill = inTo, y = Value, x = factor(Commodity, levels = comLevels)), color = "black")+
  geom_bar(stat = "identity", colour = "black")+
  scale_fill_manual(values = colorStage)+
  labs(x = "", y = "Quantity of FLW (MMT)", fill = "")+
  guides(fill = guide_legend(nrow=1, byrow=TRUE)) +
  mytheme  + theme(   legend.text = element_text(size = 9)
                      , axis.text.x = element_text(size = 10)
                      , axis.text.y = element_text(size = 10)
                      , axis.title = element_text(size = 10))
ggsave("Fig4.pdf", device = "pdf", width = 18,  height = 8,  units = "cm")


# Figure 5 ----

endWaste <- read_excel("manuscript Datafiles.xlsx", sheet = "Fig5")

epaDisposalLevels <- c("Unknown", "LWI","Recycled (other)" ,"Compost/ Land App.", "Industrial Uses", "Animal Feed", "Food Donation")
epaStageLevels = c("On-farm" ,  "Manufacturing", "Distribution","W&R", "Consumption")



endWasteEPA <-endWaste %>%
  filter(Disposal != "Water") %>%
  group_by(epaLevel, epaStage)%>%
  dplyr::summarize(value = sum(value, na.rm = TRUE)) %>%
  ungroup()




Fig5 <-
  endWasteEPA %>% 
ggplot() +
  geom_col(aes(fill = factor(epaLevel, levels = rev(epaDisposalLevels))
               , y = value, x = factor(epaStage, levels = epaStageLevels))
           , color= "black")+
  scale_fill_manual(values = colorDisEPA)+
  guides(fill = guide_legend(nrow=2, byrow=TRUE)) +
  scale_x_discrete(labels = function(x) str_wrap(x,width = 14))+
  labs(x = "", y = "Food Loss and Waste (MMT)") +
  mytheme + theme(legend.text = element_text(size = 10)
                  , axis.text.x = element_text(size = 11)
                  , axis.title.y = element_text(size = 11))

ggsave("Fig5.pdf", device = "pdf", width = 18,  height = 8,  units = "cm")


# Figures 6 & 7 ----
# Please run code for Figure 3 before these figures

uncertainty <- read_excel("manuscript Datafiles.xlsx", sheet = "Fig6_7")
 
Fig6 <-
ggplot( )+
  geom_bar(data = filter(filter(commBar, Commodity != "Donated"), flowType2 == "FLW" | flowType2 == "Water"),
           aes(x = factor(Stage2, levels = c("F","M" ,"D", "W/R", "C", "T")) , y = Value, 
               fill = factor(flowType2, levels = c( "Water","FLW")))
           , stat = "identity", colour = "black")+
  geom_point(data = pivot_longer(uncertainty, c(mean, upper, lower), names_to = "point", values_to = "Value"),
             aes(factor(Stage2, levels = c("F","M" ,"D", "W/R", "C", "T")), y = Value)
             , shape = "-", color = "black", size = 5) +
  geom_segment(data = uncertainty, aes(x = factor(Stage2, levels = c("F","M" ,"D", "W/R", "C", "T")), xend = factor(Stage2, levels = c("F","M" ,"D", "W/R", "C", "T"))
                                      ,  y = lower, yend = upper))+
  scale_fill_manual(values =  c("#56b4e9", "#d55e00"))+
  facet_wrap(factor(Commodity, levels = comLevels2)~., scales = "free_y", nrow = 2)+
  labs(x = "", y = "Quantity of Food (MMT)", fill = "")+
  guides(fill = guide_legend(nrow=1, byrow=TRUE)) +
  mytheme + theme(legend.position = "bottom",  axis.text.x = element_text(size = 8),  axis.text.y = element_text(size = 8))

ggsave("Fig6.pdf", device = "pdf", width = 18,  height = 8,  units = "cm")

uncertaintyPercent <-
filter(commBar, flowType2 == "FLW" | flowType2 == "Water") %>% pivot_wider(names_from = flowType2, values_from = Value) %>% left_join(uncertainty) %>%
  rowwise()%>%
  dplyr::mutate(
    TotalFLW = sum(c_across(c(FLW, Water)), na.rm = TRUE) ,
    meanPerc = (mean - TotalFLW) / TotalFLW
               , upperPerc = (upper - TotalFLW) / TotalFLW
               , lowerPerc = (lower - TotalFLW) / TotalFLW
               ) %>%
  filter(Commodity != "Donated")

Fig7 <-
  ggplot( )+
  geom_point(data =   pivot_longer(select(uncertaintyPercent, -FLW, -Water, -mean, -lower, -TotalFLW),
                                   c(meanPerc, upperPerc, lowerPerc), names_to = "bound", values_to = "Value"),
             aes(factor(Stage2, levels = c("F","M" ,"D", "W/R", "C", "T")), y = Value)
             , shape = "-",  size = 8) +
  geom_segment(data = uncertaintyPercent, aes(x = factor(Stage2, levels = c("F","M" ,"D", "W/R", "C", "T")), xend = factor(Stage2, levels = c("F","M" ,"D", "W/R", "C", "T"))
                                       ,  y = lowerPerc, yend = upperPerc), size = 1, color = "black")+
    geom_hline(yintercept = 0)+
  facet_wrap(factor(Commodity, levels = comLevels2)~., nrow = 2)+
  scale_y_continuous(labels = scales::percent_format(accuracy = .1))+
  labs(x = "", y = "Uncertainty")+
  mytheme + theme(legend.position = "bottom",  axis.text = element_text(size = 8))

ggsave("Fig7.pdf", device = "pdf", width = 18,  height = 8,  units = "cm")



