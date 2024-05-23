# Figure 1
#linear association between altitude and launch sites
library(ggplot2)
library(ggpmisc)
data_num<-read.csv(file="launching_Site_total.csv",check.names = F)
data_num
count <- sum(data_num$Y >= 81 & data_num$Y <= 90) #0 
count
count <- sum(data_num$Y >= 71 & data_num$Y < 81) #5
count <- sum(data_num$Y >= 61 & data_num$Y < 71) #15
count <- sum(data_num$Y >= 51 & data_num$Y < 61) #28
count <- sum(data_num$Y >= 41 & data_num$Y < 51) #27
count <- sum(data_num$Y >= 31 & data_num$Y < 41) #51
count <- sum(data_num$Y >= 21 & data_num$Y < 31) #26
count <- sum(data_num$Y >= 11 & data_num$Y < 21) #14
count <- sum(data_num$Y >= 0 & data_num$Y < 11)  #11
count <- sum(data_num$Y >= -11 & data_num$Y < 0) #9
count <- sum(data_num$Y >= -21 & data_num$Y < -11) #4
count <- sum(data_num$Y >= -31 & data_num$Y < -21) #9
count <- sum(data_num$Y >= -41 & data_num$Y < -31) #13
count <- sum(data_num$Y >= -51 & data_num$Y < -41) #2
count <- sum(data_num$Y >= -61 & data_num$Y < -51) #0
count <- sum(data_num$Y >= -71 & data_num$Y < -61) #7
count <- sum(data_num$Y >= -81 & data_num$Y < -70) #1
count <- sum(data_num$Y >= -90 & data_num$Y < -81) #0 
# cut into groups by 10-interval 
data_num$YL<-data.frame(breaks=cut(data_num$Y,breaks = 10) )
# sum
data_num2<-aggregate(data_num$Count, list(data_num$Y_level),sum)

colnames(data_num2)<-c("Level","Count")

data_num
# rename
data_num$Y_level<-factor(data_num$Y_level,                 # Relevel group factor
                         levels = c("L-1","L-2","L-3","L-4","L-5","L-6","L-7","L-8","L-9","L-10","L-11", 
                                    "L-12","L-13","L-14","L-15","L-16","L-17","L-18"))

# Fit polynomial regression line and add labels
library(ggrepel)
library(mgcv)
gam_y <- gam(Per ~ s(ID), data=data_num)
summary(gam_y)

ggplot(data_num, aes(ID,Per)) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x))

# give a name to a formula
formula <- y ~ poly(x, 3, raw = TRUE)
formula = y ~ s(x)
# using defaults
cout_biom<-ggplot(data_num, aes(ID,Per)) +
  #stat_poly_line(formula = formula,color="red", fill="#FDAF91FF") +
  expand_limits(x=c(0, 20))+
  #stat_poly_eq(use_label(c("R2", "F", "P"), sep = "*\"; \"*"),
  #formula = formula,size=6.5)+
  geom_smooth(method = "gam", formula = y ~ s(x),
              color="red", fill="#FDAF91FF")+
  geom_point(color= "black",size=2.3) +
  geom_text_repel(aes(label = scales::percent(Per*0.01, accuracy=0.1) ),color = "black",
                  size = 5.5) +
  theme_bw()+
  theme(legend.title=element_blank(),
        panel.border = element_rect(color = "black", 
                                    fill = NA, 
                                    size = 1.2),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(colour = "black", size = 20
                                   #,angle=30, vjust=.8, hjust=0.8
        ),
        axis.text.y = element_text(colour = "black", size = 20))+scale_x_reverse()+
  coord_flip()
cout_biom
ggsave(plot = cout_biom, width =5, height = 9, dpi = 600, filename = "count_lati_line.jpg")
# barplot
count_lati<-ggplot(data_num, aes(x=Y_level , y=Count,fill=Count)) + 
  scale_fill_gradient(low = "#42B540FF", high = "#F8766D")+
  theme(axis.title.x = element_text(colour = "black", size = 12),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(colour = "black", size = 12), 
        panel.background = element_blank(),
        plot.margin = unit(c(1,-1,1,0), "mm"))+
  scale_x_discrete(limits = rev)+
  geom_bar(stat="identity")+coord_flip()
count_lati
ggsave(plot = count_lati, width =4.5, height = 8, dpi = 300, filename = "count_lati_bar.jpg")
##

##########################
##########################
# Figure 2
data_pro<-read.csv(file="Luanching_pro.csv")
data_pro
data_pro$Word <- factor(data_pro$Word ,
                        levels = c("World_Heritage", "Protected_Land","Non_protected"))
data_pro$Costal<- factor(data_pro$Costal  ,
                         levels = c("Total","Inland", "Costal"))
data_pro$Percentage2<-data_pro$Percentage2*100

p<-ggplot(data_pro, aes(x=Word, y=Percentage2, fill=Word)) +
  facet_wrap(~Costal)+
  scale_fill_manual(values = c("#F8766D", "orange","#42B540FF"))+
  geom_bar(stat="identity")+
  geom_text(aes(y = Percentage2 + 2, label = round(Percentage2,2)), 
            position = position_dodge(width = 0.9),
            size =3.7, vjust = 0, hjust = 0.5)+
  expand_limits(y=c(0, 100))+
  theme(legend.background=element_blank(),
        legend.justification = "top",
        legend.position ="right",legend.key.size = unit(0.75, "cm"),
        legend.text=element_text(size=10),
        axis.text.y = element_text(colour = "black", size = 12), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        #legend.title=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.background =element_rect(fill="white"),
        strip.text.x  = element_text(size = 14),
        # Hide panel borders and remove grid lines
        panel.border = element_blank(),
        axis.line.x=element_line(linewidth=1),
        # Change axis line
        axis.line = element_line(colour = "black")
  )

p
ggsave(plot = p, width = 6.6, height = 3,dpi = 600, filename = "protect_inco2.jpg")

################################
# Figure 3
#Figure 3a
# red list species
library(FSA)
Data_species<-read.csv(file="Red_species.csv",check.names = F)
Data_species<-Data_species[rowSums(is.na(Data_species)) == 0,]
colnames(Data_species)
##density graph

Data_species
library(ggplot2)

Operating_site_heatmap<-ggplot(Data_species, aes(X, Y)) + 
  borders() + 
  xlim(c(-180, 180)) + ylim(c(-90, 90)) + 
  stat_density_2d(aes(fill = ..level..), geom="polygon",alpha=0.7) +
  geom_point(position="jitter", alpha=.3, colour="red") +
  gradient_fill("YlOrRd")+
  coord_cartesian(clip = "off") +
  theme_bw()+
  theme(axis.text.y   = element_text(size=14),
        axis.text.x   = element_text(size=14),
        axis.title.y  = element_text(size=14),
        axis.title.x  = element_text(size=14),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)
  )
Operating_site_heatmap
ggsave(plot = Operating_site_heatmap, width = 9, height = 5, dpi = 600, filename = "Operating_site_heatmap.jpg")
# Figure 3b
# modeling
Data_species$Combined_lg<-log(Data_species$Combined+1)
library(glmmTMB)
library(mgcv)
LinX <- gam(Combined_lg ~ s(Y), data=Data_species)
summary(LinX)
LinY <- gam(Combined_lg ~ s(X), data=Data_species)
summary(LinY)

dat_X <- ggpredict(LinX, terms = c("X"))
plot(dat_X)
dat<-data.frame(dat_X)


combined_x<-ggplot(data=dat, aes(x = x)) +
  geom_ribbon(aes(ymin = conf.low , ymax = conf.high),fill="#FFAA00",  alpha = 0.3) +
  geom_line(aes(y = predicted), color ="#FF1C00")+
  theme_bw()+
  theme(axis.text.y   = element_text(size=14),
        axis.text.x   = element_text(size=14),
        axis.title.y  = element_text(size=14),
        axis.title.x  = element_text(size=14),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)
  )+
  labs(x="Altitude", 
       y = "Predicted Threaten \n Richness (± 95%CI) "
  )
combined_x
#save figure
ggsave(plot = combined_x, width = 10, height = 2, dpi = 600, filename = "combined_x.jpg")
#Figure 3c
dat_Y <- ggpredict(LinY , terms = c("Y"))
plot(dat_Y)
dat_Y<-data.frame(dat_Y)
combined_Y<-ggplot(data=dat_Y, aes(x = x)) +
  geom_ribbon(aes(ymin = conf.low , ymax = conf.high),fill="#FFAA00",  alpha = 0.3) +
  geom_line(aes(y = predicted), color ="#FF1C00")+
  theme_bw()+
  theme(axis.text.y   = element_text(size=14),
        axis.text.x   = element_text(size=14),
        axis.title.y  = element_text(size=14),
        axis.title.x  = element_text(size=14),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)
  )+
  labs(x="Altitude", 
       y = "Predicted Threaten \n Richness (± 95%CI) "
  )+
  guides(
    y = "none",
    y.sec = "axis"
  )
combined_Y
#save figure
ggsave(plot = combined_Y, width = 8, height = 3.5, dpi = 600, filename = "combined_Y.jpg")

########################
# Figure 4
library(rcompanion)
#TSCF=1 removed it 
Data_species<-filter(Data_species,Biome!="TSCF")
Data_species3<-Data_species[rowSums(is.na(Data_species)) == 0,]
#Mammals
kruskal.test(Mammals ~ Biome,
             data = Data_species3)
PT = dunnTest(Mammals ~ Biome,
              data=Data_species3,
              method="bh")    
PT
PTD = PT$res
Tk<-cldList(comparison = PTD$Comparison,
            p.value    = PTD$P.adj,
            threshold  = 0.05)
Tk
#plot
Data_species3$Mammals_lg<-log(Data_species3$Mammals+1)
# boxplot
boxplot(Mammals_lg ~ Biome,
        data = Data_species3,col=rainbow(length(unique(Data_species3$Biome))),
        ylab="Mammals",
        xlab="Biome",ylim = c(0, 2.2))

dev.copy(png,'Mammals_plot.png',width=10,height=4,units="in",res=600)
dev.off()
#Reptiles
kruskal.test(Reptiles ~ Biome,
             data = Data_species3)
PT = dunnTest(Reptiles ~ Biome,
              data=Data_species3,
              method="bh")    # Can adjust p-values;
# See ?p.adjust for options

PT
PTD = PT$res
Tk<-cldList(comparison = PTD$Comparison,
            p.value    = PTD$P.adj,
            threshold  = 0.05)
Tk
#plot
Data_species3$Reptiles_lg<-log(Data_species3$Reptiles+1)
# boxplot
boxplot(Reptiles_lg ~ Biome,
        data = Data_species3,col=rainbow(length(unique(Data_species3$Biome))),
        ylab="Reptiles",
        xlab="Biome",ylim = c(0, 2.2))

dev.copy(png,'Reptiles_plot.png',width=10,height=4,units="in",res=600)
dev.off()
#Birds
kruskal.test(Birds ~ Biome,
             data = Data_species3)

PT = dunnTest(Birds ~ Biome,
              data=Data_species3,
              method="bh")  
PT
PTD = PT$res
Tk<-cldList(comparison = PTD$Comparison,
            p.value    = PTD$P.adj,
            threshold  = 0.05)
Tk
#plot
Data_species3$Birds_lg<-log(Data_species3$Birds+1)

# boxplot
boxplot(Birds_lg ~ Biome,
        data = Data_species3,col=rainbow(length(unique(Data_species3$Biome))),
        ylab="Birds",
        xlab="Biome",ylim = c(0, 2.5))

dev.copy(png,'Birds_plot.png',width=10,height=4,units="in",res=600)
dev.off()
#Amphibians
kruskal.test(Amphibians ~ Biome,
             data = Data_species3)
PT = dunnTest(Amphibians ~ Biome,
              data=Data_species3,
              method="bh")    
PT
PTD = PT$res
Tk<-cldList(comparison = PTD$Comparison,
            p.value    = PTD$P.adj,
            threshold  = 0.05)
Tk
#plot
Data_species3$Amphibians_lg<-log(Data_species3$Amphibians+1)

# boxplot
boxplot(Amphibians_lg ~ Biome,
        data = Data_species3,col=rainbow(length(unique(Data_species3$Biome))),
        ylab="Amphibians",
        xlab="Biome",ylim = c(0, 2.2))

dev.copy(png,'Amphibians_plot.png',width=10,height=4,units="in",res=600)
dev.off()
#Combined
kruskal.test(Combined ~ Biome,
             data = Data_species3)


PT = dunnTest(Combined ~ Biome,
              data=Data_species3,
              method="bh")  
PT
PTD = PT$res
library(rcompanion)
Tk<-cldList(comparison = PTD$Comparison,
            p.value    = PTD$P.adj,
            threshold  = 0.05)
Tk
#plot
Data_species3$Combined_lg<-log(Data_species3$Combined+1)

# boxplot
boxplot(Combined_lg ~ Biome,
        data = Data_species3,col=rainbow(length(unique(Data_species3$Biome))),
        ylab="Combined",
        xlab="Biome",ylim = c(0, 2.5))

dev.copy(png,'Combined_plot.png',width=10,height=4,units="in",res=600)
dev.off()
######################################
#NMDS
# remove NA rows
Data_species<-read.csv(file="Red_species.csv",check.names = F)
Data_species<-Data_species[rowSums(is.na(Data_species)) == 0,]
Data_species<-na.omit(Data_species)
colnames(Data_species)
Data_species2<-Data_species[,15:ncol(Data_species)]
library(ecole)
gene_table_m<-scale(Data_species2)
gene_table_m<-as.matrix(Data_species2)
gene_table_m_dis<-vegdist(gene_table_m, method='bray')
gene_table_m_dis<-bray0(gene_table_m)
# permanova
samples_table2<-as.data.frame(as.matrix(gene_table_m_dis))
# set seed
set.seed(42)
Data_species$Coastal
permanova<-adonis2(gene_table_m_dis ~ Coastal*Y+Coastal*X+Biome, 
                                    data=Data_species,by="terms")
permanova
# calculating NMDS
nmds_red = metaMDS(gene_table_m_dis,k=2,try=50)
scores(nmds_red$points) 
summary(nmds_red)

data.scores <- as.data.frame(scores(nmds_red$points)) 

Data_species$Coastal
dim(Data_species)
dim(data.scores_genes)
# combine the data
data.scores<-cbind(data.scores,Data_species)
data.scores[,1:10]
# name
#envfit analysis
variable<-Data_species[,c("Mammals","Reptiles","Birds","Amphibians")]
variable<- scale(variable, center = FALSE, scale = apply(variable, 2, sd, na.rm = TRUE))
variable<- as.data.frame(variable)
bac.phy.fit <- envfit(nmds_red, variable, permutations = 999,na.rm = TRUE)
#creast a dataframe
phy.scores.bac <- as.data.frame(scores(bac.phy.fit, display = "vectors"))

# beak
breaks <- seq(-180, 180, by = 60)
breaks
data.scores$X_b<-cut(data.scores$X,breaks = c(-180, -120, -60,0 , 60,  120,180))
data.scores$Y_b<-cut(data.scores$Y,breaks = c(-90, -60, -30,0 , 30,60,90))
# order
data.scores$X_b<- factor(data.scores$X_b ,
                               levels = c( "(120,180]","(60,120]","(0,60]","(-60,0]",
                                           "(-120,-60]","(-180,-120]" ))
data.scores$Y_b<- factor(data.scores$Y_b ,
                               levels = c( "(60,90]","(30,60]","(0,30]","(-30,0]",
                                           "(-60,-30]","(-90,-60]" ))
head(data.scores)


NMDS_red_NA <- ggplot(data.scores, aes(x=MDS1, y=MDS2,color=Y_b))+ #sets up the plot
  geom_point(aes(MDS1, MDS2,shape=X_b #colour = factor(Biome), #size=4,
                 #size =as.numeric(data.scores_genes$Time2)
  ),size=4)+ #adds site points to plot, shape determined by Landuse, colour determined by Management
  coord_fixed()+
  #scale_shape_manual(values = c(1,19))+
  theme_classic()+ #scale_color_manual(values = c("#00BA38","#F8766D","#B79F00","#619CFF","#00BFC4"))+
  theme(panel.background = element_rect(fill = NA, colour = "black", size = 1, linetype = "solid"))+
  #labs(colour = "Time", shape = "Location")+ # add legend labels for Management and Landuse
  theme(legend.position = "right", legend.text = element_text(size = 14), legend.title = element_text(size = 18), axis.text = element_text(size = 14),axis.title = element_text(size = 16)) # add legend at right of plot

NMDS_red_NA2<-NMDS_red_NA+scale_color_manual(values = c("#D3D93E", "#FFAA00","#C84248","#FF1C00", "#000080", "#5F7FC7"))+
  geom_segment(aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), 
               data =phy.scores.bac, size =1, alpha = 0.5, colour = "grey30")+
  geom_text(data = phy.scores.bac, aes(x = NMDS1*1.2, y = NMDS2*1.2), colour = "grey30", 
            fontface = "bold", label = row.names(phy.scores.bac))

M<-NMDS_red_NA2+
  annotate("text", x = -1.7, y = 0.7, size=6.5,label = paste0("Stress: ", format(nmds_genes$stress, digits = 4)), hjust = 0) +
  annotate("text", x = -0.6, y = 0.7,size=6.5, label="italic(k)==2 ", parse=TRUE)+
  coord_fixed(ratio = 1.8)+
  #geom_path(data=hull_gene,aes(MDS1, MDS2))
  expand_limits(y=c(-0.8, 0.8))+expand_limits(x=c(-1.8, 1.2))+
  #theme(legend.title=element_blank(),
  #legend.background=element_blank(),legend.box = "horizontal",legend.position =c(0.7,0.9),legend.key.size = unit(0.5, "cm"),
  #legend.text=element_text(size=10.5))+
  
  theme(axis.text.y = element_text(colour = "black", size = 14), 
        axis.text.x = element_text(colour = "black", size = 14), 
        axis.title.y = element_text(size = 18), 
        axis.title.x = element_text(size = 18, colour = "black"),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.0))

M

ggsave(plot = M, width = 9, height =7, dpi = 500, filename = "NMDS_redlist.jpg",bg = "transparent")

################################
#NMDS1  coast 
data.scores
#
kruskal.test(MDS1 ~ Coastal,
             data = data.scores)
# boxplot
data.scores$Coastal<- factor(data.scores$Coastal,
                                   levels = c( "Inner","Coastal" ))
tiff(file="NMDS1_coastal.tiff",
     width=4, height=7, units="in", res=600)
boxplot(MDS1 ~ Coastal,
        data = data.scores,col=rainbow(length(unique(data.scores$Coastal))),
        ylab="NMDS1",
        xlab="Costal",ylim = c(-1.7, 1.4))

dev.off()
#NMDS1  coast 
#Combined
data.scores
data.scores1<-filter(data.scores,Biome!="TSCF")

kruskal.test(MDS1 ~ Biome,
             data = data.scores1)

PT = dunnTest(MDS1 ~ Biome,
              data=data.scores1,
              method="bh")   
PT
PTD = PT$res
library(rcompanion)
Tk<-cldList(comparison = PTD$Comparison,
            p.value    = PTD$P.adj,
            threshold  = 0.05)
Tk
# boxplot
data.scores1<-filter(data.scores,Biome!="TSCF")
tiff(file="NMDS1_biomes.tiff",
     width=10, height=4, units="in", res=600)
boxplot(MDS1 ~ Biome,
        data = data.scores1,col=rainbow(length(unique(data.scores1$Biome))),
        ylab="NMDS1",
        xlab="Biome",ylim = c(-1.7, 1.3))

dev.off()















