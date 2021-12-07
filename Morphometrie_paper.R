#####     MORPHOMETRICS THALPOMENA      ###
###########################################


#####FIGURE FOR SUPPLEMENTARY#######



    setwd("E:\\offene_Manuskripte\\Revision_Thalpomena\\NicheRover")

    Thalpi<-read.csv("E:\\offene_Manuskripte\\Revision_Thalpomena\\NicheRover\\Rover_all.csv")


###   Boxplot of the features per Cluster
    
        library(ggplot2)
        library(gridExtra)  
        library(broman)
        library("ggpubr")
        library(extrafont)
        library(cowplot)
    # Basic box plot
    p <- ggplot(Thalpi, aes(x=Thalpi$Cluster, y=Thalpi$ElytrenLänge)) + 
      geom_boxplot()
    p
    
    
    
    # One box per feature
    p1 <- ggplot(Thalpi, aes(x=Cluster, y=ElytrenLänge, fill=Cluster)) + 
      geom_boxplot() +
      ggtitle("Elytra length") +
      xlab("") + ylab("size") +
      scale_fill_manual(values=c("forestgreen", "violetred4","royalblue4","red3")) +
      theme(text=element_text(size=10, family="Arial"))

  
    p2 <- ggplot(Thalpi, aes(x=Cluster, y=ElytrenBREITE, fill=Cluster)) + 
      geom_boxplot()+
      ggtitle("Elytra width") +
      xlab("") + ylab("")+
      scale_fill_manual(values=c("forestgreen", "violetred4","royalblue4","red3"))+
      theme(text=element_text(size=10, family="Arial"))
  
    p3 <- ggplot(Thalpi, aes(x=Cluster, y=Thalpi$FlügelLÄNGE, fill=Cluster)) + 
      geom_boxplot()+
      ggtitle("Wing length") +
      xlab("") + ylab("")+
      scale_fill_manual(values=c("forestgreen", "violetred4","royalblue4","red3"))+
      theme(text=element_text(size=10, family="Arial")) 
   
     p4 <- ggplot(Thalpi, aes(x=Cluster, y=Thalpi$FlügelBREITE, fill=Cluster)) + 
      geom_boxplot()+
      ggtitle("Wing width") +
      xlab("") + ylab("")+
      scale_fill_manual(values=c("forestgreen", "violetred4","royalblue4","red3"))+
      theme(text=element_text(size=10, family="Arial")) 
     
     p5 <- ggplot(Thalpi, aes(x=Cluster, y=Thalpi$PronotumHalsLÄNGE, fill=Cluster)) + 
       geom_boxplot()+
       ggtitle("Prothorax length") +
       xlab("") + ylab("")+
       scale_fill_manual(values=c("forestgreen", "violetred4","royalblue4","red3"))+
       theme(text=element_text(size=10, family="Arial")) 
     
     p6 <- ggplot(Thalpi, aes(x=Cluster, y=Thalpi$PronotumLÄNGE, fill=Cluster)) + 
       geom_boxplot()+
       ggtitle("Pronotum length") +
       xlab("") + ylab("")+
       scale_fill_manual(values=c("forestgreen", "violetred4","royalblue4","red3"))+
       theme(text=element_text(size=10, family="Arial"))
     
     p7 <- ggplot(Thalpi, aes(x=Cluster, y=Thalpi$PronotumBREITE, fill=Cluster)) + 
       geom_boxplot()+
       ggtitle("Pronotum width") +
       xlab("") + ylab("")+
       scale_fill_manual(values=c("forestgreen", "violetred4","royalblue4","red3"))+
       theme(text=element_text(size=10, family="Arial"))
     
     p8 <- ggplot(Thalpi, aes(x=Cluster, y=Thalpi$KopfBREITE, fill=Cluster)) + 
       geom_boxplot()+
       ggtitle("Head width") +
       xlab("") + ylab("")+
       scale_fill_manual(values=c("forestgreen", "violetred4","royalblue4","red3"))+
       theme(text=element_text(size=10, family="Arial"))
     
     p9 <- ggplot(Thalpi, aes(x=Cluster, y=Thalpi$KopfHÖHE, fill=Cluster)) + 
       geom_boxplot()+
       ggtitle("Head hight") +
       xlab("") + ylab("")+
       scale_fill_manual(values=c("forestgreen", "violetred4","royalblue4","red3"))+
       theme(text=element_text(size=10, family="Arial"))
     
     p10 <- ggplot(Thalpi, aes(x=Cluster, y=Thalpi$Augenabstand, fill=Cluster)) + 
       geom_boxplot()+
       ggtitle("Interocular width") +
       xlab("") + ylab("")+
       scale_fill_manual(values=c("forestgreen", "violetred4","royalblue4","red3"))+
       theme(text=element_text(size=10, family="Arial"))
     
     p11 <- ggplot(Thalpi, aes(x=Cluster, y=Thalpi$FemurLÄNGE, fill=Cluster)) + 
       geom_boxplot()+
       ggtitle("Femur length") +
       xlab("") + ylab("")+
       scale_fill_manual(values=c("forestgreen", "violetred4","royalblue4","red3"))+
       theme(text=element_text(size=10, family="Arial"))
     
     p12 <- ggplot(Thalpi, aes(x=Cluster, y=Thalpi$FemurBREITE, fill=Cluster)) + 
       geom_boxplot()+
       ggtitle("Femur width") +
       xlab("") + ylab("")+
       scale_fill_manual(values=c("forestgreen", "violetred4","royalblue4","red3"))+
       theme(text=element_text(size=10, family="Arial"))
     
     p13 <- ggplot(Thalpi, aes(x=Cluster, y=Thalpi$TibiaLÄNGE, fill=Cluster)) + 
       geom_boxplot()+
       ggtitle("Tibia length") +
       xlab("") + ylab("")+
       scale_fill_manual(values=c("forestgreen", "violetred4","royalblue4","red3"))+
       theme(text=element_text(size=10, family="Arial"))
 
     

    #####     Canonical Variates Analysis
    
    library(tidyverse)
    library(cowplot)
    library(broom)
    library(magrittr)
    library(MASS)
    
    
    Rover <-read.csv("E:\\offene_Manuskripte\\Revision_Thalpomena\\NicheRover\\Rover_all.csv")
    
    
   Rover.lda <- lda(Cluster ~ ., data = Rover)
    Rover.lda    

    Rover.lda$scaling    
    
    Rover.sub <- 
      Rover %>% 
      dplyr::select(-Cluster) %>% # drop Species column 
      as.matrix  # cast to matrix for calculations
    
    # calculate CV scores
    CVA.scores <- Rover.sub %*% Rover.lda$scaling
    
    # create data frame with scores
    Rover.CV <- data.frame(CVA.scores)
    Rover.CV$Cluster <- Rover$Cluster

   p14<- Rover.cva.plot <-
      ggplot(Rover.CV, aes(x = LD1, y = LD2)) + 
      geom_point(aes(color=Cluster), alpha=1) + 
      labs(x = "CV1", y = "CV2") +
      coord_fixed(ratio=1)+
      ggtitle("CVA") +
      scale_color_manual(values = c("forestgreen", "violetred4","royalblue4","red3"))# keep the unit scaling of the plot fixed at 1
    
    Rover.cva.plot    

    
    p15<- ggplot(Rover.CV, aes(x = LD1)) +
      geom_density(aes(color=Cluster)) +
      labs(x = "CV1") +
      ggtitle("Density plot of CV1") +
      scale_color_manual(values = c("forestgreen", "violetred4","royalblue4","red3"))# keep the unit scaling of the plot fixed at 1
    
p15
    
      
    ### Estimating confidence regions for group means in CVA
    
    legend <- get_legend(p1)
    p1 <- p1 + theme(legend.position="none")
    legend <- get_legend(p2)
    p2 <- p2 + theme(legend.position="none")
    legend <- get_legend(p3)
    p3 <- p3 + theme(legend.position="none")
    legend <- get_legend(p4)
    p4 <- p4 + theme(legend.position="none")
    legend <- get_legend(p5)
    p5 <- p5 + theme(legend.position="none")
    legend <- get_legend(p6)
    p6 <- p6 + theme(legend.position="none")
    legend <- get_legend(p7)
    p7 <- p7 + theme(legend.position="none")
    legend <- get_legend(p8)
    p8 <- p8 + theme(legend.position="none")
    legend <- get_legend(p9)
    p9 <- p9 + theme(legend.position="none")
    legend <- get_legend(p10)
    p10 <- p10 + theme(legend.position="none")
    legend <- get_legend(p11)
    p11 <- p11 + theme(legend.position="none")
    legend <- get_legend(p12)
    p12 <- p12 + theme(legend.position="none")
    legend1 <- get_legend(p13)
    p13 <- p13 + theme(legend.position="none")
    legend <- get_legend(p14)
    p14 <- p14 + theme(legend.position="none")
    legend <- get_legend(p15)
    p15 <- p15 + theme(legend.position="none")
    
    plot_grid(p1, p2, p3, p4, p5, p6,p7,p8,p9,p10,p11,p12,p13,p14,p15, labels=c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)", "(g)", "(h)", "(i)", "(j)", "(k)", "(l)", "(m)", "(n)", "(o)"), ncol = 3, nrow = 5)
    
    
    
    
    
 ##############################################################
    ###########################################################
    
    
    ###   FIGURE FOR PAPER  ####
    
    p16<- Rover.cva.plot <-
      ggplot(Rover.CV, aes(x = LD1, y = LD2)) + 
      geom_point(aes(color=Cluster), alpha=1, size=2) + 
      labs(x = "CV1", y = "CV2") +
      ggtitle("CVA NA species") +
      scale_color_manual(values = c("forestgreen", "violetred4","royalblue4","red3")) + # keep the unit scaling of the plot fixed at 1
      theme_bw()
   p16 
  
    
    p17<- ggplot(Rover.CV, aes(x = LD1)) +
      geom_density(aes(color=Cluster), size=1.2) +
      labs(x = "CV1", y="Density") +
      ggtitle("Density plot of CV1 NA species") +
      scale_color_manual(values = c("forestgreen", "violetred4","royalblue4","red3")) + # keep the unit scaling of the plot fixed at 
      theme_bw()
    
    p17
    
    
    genetic<-read.csv("E:\\offene_Manuskripte\\Revision_Thalpomena\\NicheRover\\Rover_test_OB.csv", header=TRUE)
    
    
    genetic.lda <- lda(Cluster ~ ., data = genetic)
    genetic.lda    
    
    genetic.lda$scaling    
    
    genetic.sub <- 
      genetic %>% 
      dplyr::select(-Cluster) %>% # drop Species column 
      as.matrix  # cast to matrix for calculations
    
    # calculate CV scores
    CVA.scores <- genetic.sub %*% genetic.lda$scaling
    
    # create data frame with scores
    genetic.CV <- data.frame(CVA.scores)
    genetic.CV$Cluster <- genetic$Cluster
    
    p18<- genetic.cva.plot <-
      ggplot(genetic.CV, aes(x = LD1, y = LD2)) + 
      geom_point(aes(color=Cluster), alpha=1, size=2) + 
      labs(x = "CV1", y = "CV2") +
      ggtitle("CVA genetic data") +
      scale_color_manual(values = c("forestgreen", "violetred4","royalblue4","red3")) +
      theme_bw()
    
    p18   
    
    
    p19<- ggplot(genetic.CV, aes(x = LD1)) +
      geom_density(aes(color=Cluster), size=1.2) +
      labs(x = "CV1", y="Density") +
      ggtitle("Density plot of CV1 genetic data") +
      scale_color_manual(values = c("forestgreen", "violetred4","royalblue4","red3")) + # keep the unit scaling of the plot fixed at 1
      theme_bw()
    p19
    
##### Thalpomena schulthessi
    
    schulthessi<-read.csv("E:\\offene_Manuskripte\\Revision_Thalpomena\\NicheRover\\schulthessi1.csv", header=TRUE)
    
    
    schulthessi.lda <- lda(Cluster ~ ., data = schulthessi)
    schulthessi.lda    
    
    schulthessi.lda$scaling    
    
    schulthessi.sub <- 
      schulthessi %>% 
      dplyr::select(-Cluster) %>% # drop Species column 
      as.matrix  # cast to matrix for calculations
    
    # calculate CV scores
    CVA.scores <- schulthessi.sub %*% schulthessi.lda$scaling
    
    # create data frame with scores
    schulthessi.CV <- data.frame(CVA.scores)
    schulthessi.CV$Cluster <- schulthessi$Cluster
    
    p20<- schulthessi.cva.plot <-
      ggplot(schulthessi.CV, aes(x = LD1, y = LD2)) + 
      geom_point(aes(color=Cluster), alpha=1, size=2) + 
      labs(x = "CV1", y = "CV2") +
      ggtitle("CVA all species") +
      scale_color_manual(values = c("red3" ,"forestgreen","violetred4","black","royalblue4")) +
      theme_bw()    +
      theme(legend.text = element_text(face = "italic")) 
  
    p20   
    
    
    p21<- ggplot(schulthessi.CV, aes(x = LD1)) +
      geom_density(aes(color=Cluster), size=1.2) +
      labs(x = "CV1", y="Density") +
      ggtitle("Density plot of CV1 all species") +
      scale_color_manual(values = c("red3" ,"forestgreen","violetred4","black","royalblue4")) + # keep the unit scaling of the plot fixed at 1
      theme_bw()
    p21
    
### all species current taxonomy
    
    taxonomy<-read.csv("E:\\offene_Manuskripte\\Revision_Thalpomena\\NicheRover\\Thalpi_species_Past.csv", header=TRUE)
    
    
    taxonomy.lda <- lda(Art ~ ., data = taxonomy)
    taxonomy.lda    
    
    taxonomy.lda$scaling    
    
    taxonomy.sub <- 
      taxonomy %>% 
      dplyr::select(-Art) %>% # drop Species column 
      as.matrix  # cast to matrix for calculations
    
    # calculate CV scores
    CVA.scores <- taxonomy.sub %*% taxonomy.lda$scaling
    
    # create data frame with scores
    taxonomy.CV <- data.frame(CVA.scores)
    taxonomy.CV$Art <- taxonomy$Art
   
   # install.packages("scico")
    library(scico)
  #  scico_palette_show()
    
    
    p22<- taxonomy.cva.plot <-
      ggplot(taxonomy.CV, aes(x = LD1, y = LD2)) + 
      geom_point(aes(color=Art), alpha=1, size=2) +
      labs(x = "CV1", y = "CV2") +
      ggtitle("CVA taxonomic classification") +
      theme_bw() 
    
    
    p22   
### legends for the figure ###extra
    
    p23<- p22 + 
      theme(legend.position="bottom", legend.box = "horizontal") + 
      scale_color_discrete(name = "taxonomic grouping (g)") +
      theme(legend.text = element_text(face = "italic"))
           p23    
     
           
      ####
    legend <- get_legend(p18)
    p18 <- p18 + theme(legend.position="none")
    legend <- get_legend(p19)
    p19 <- p19 + theme(legend.position="none")
    legend <- get_legend(p16)
    p16 <- p16 + theme(legend.position="none")
    legend <- get_legend(p17)
    p17 <- p17 + theme(legend.position="none")
    legend <- get_legend(p20)
    p20 <- p20 + theme(legend.position="none")
    legend <- get_legend(p21)
    p21 <- p21 + theme(legend.position="none")
    legend <- get_legend(p22)
    p22 <- p22 + theme(legend.position="none")

    plot_grid(p18, p19,p16, p17, p20, p21, p22,  labels=c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)", "(g)", ""), ncol = 2, nrow = 4)
    
    
    
#######################################################################
    ##################################################################
    
   