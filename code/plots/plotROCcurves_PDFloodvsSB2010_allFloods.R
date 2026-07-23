#plot ROC curves for all floods

rm(list=ls())

library(ggplot2)
library(patchwork)
library(cowplot)

setwd("/Users/f007f8t/probDnsclRealData_truncatedT")
# dir<- commandArgs(trailingOnly=TRUE)
# setwd(dir)

if(!dir.exists("plots")) dir.create("plots")

nFloods=4
nMethods=2

#load ROC data from PDFlood
load("data/ROCdata_allFloods")

ROCdata_allFloods$TPR<- as.numeric(ROCdata_allFloods$TPR)
ROCdata_allFloods$TNR<- as.numeric(ROCdata_allFloods$TNR)
ROCdata_allFloods$FPR<- as.numeric(ROCdata_allFloods$FPR)
ROCdata_allFloods$FNR<- as.numeric(ROCdata_allFloods$FNR)
ROCdata_allFloods$threshold<- rep(seq(0.001,1,by=0.001),nFloods)

#load ROC data from SB2010
load("data/ROC_SB2010_allFloods")

ROC_SB2010_allFloods$TPR<- as.numeric(ROC_SB2010_allFloods$TPR)
ROC_SB2010_allFloods$TNR<- as.numeric(ROC_SB2010_allFloods$TNR)
ROC_SB2010_allFloods$FPR<- as.numeric(ROC_SB2010_allFloods$FPR)
ROC_SB2010_allFloods$FNR<- as.numeric(ROC_SB2010_allFloods$FNR)
ROC_SB2010_allFloods$threshold<- rep(seq(0.001,1,by=0.001),nFloods)


combined_data<- data.frame("flood" = c(ROCdata_allFloods$flood,ROC_SB2010_allFloods$flood),
                           "TPR"=as.numeric(c(ROCdata_allFloods$TPR,ROC_SB2010_allFloods$TPR)),
                           "FPR"=as.numeric(c(ROCdata_allFloods$FPR,ROC_SB2010_allFloods$FPR)),
                           "TNR"=as.numeric(c(ROCdata_allFloods$TNR,ROC_SB2010_allFloods$TNR)),
                           "FNR"=as.numeric(c(ROCdata_allFloods$FNR,ROC_SB2010_allFloods$FNR)),
                           "threshold"= as.numeric(c(ROCdata_allFloods$threshold,ROC_SB2010_allFloods$threshold)),
                           "approach"=c(rep("PDFlood",nrow(ROCdata_allFloods)),
                                        rep("SB2010",nrow(ROC_SB2010_allFloods))))

################################################################################
######################## plots just for PDFlood ################################
################################################################################
filename<- paste0("plots/ROC_curve_PDFlood_allFloods.jpeg")
jpeg(file = filename,width = 600,height=500)
print(ggplot(data=ROCdata_allFloods,aes(x=FPR,y=TPR,color=flood))+
        #geom_point(color="red",size=3)+
        geom_line(linewidth=2)+
        geom_abline(intercept=0,slope=1,lwd = 2) +
        ylab("True Positive Rate")+ xlab("False Positive Rate")+
        xlim(0,1) + ylim(0,1) +
        theme_bw() + 
        theme(plot.title = element_text(size=24), 
              axis.title = element_text(size=24),
              axis.text = element_text(size = 20),
              legend.text= element_text(size=24),
              legend.title= element_text(size=24)))
dev.off()

#plot threshold VS TPR

filename<- paste0("plots/threshold_vs_TPR_PDFlood_allFloods.jpeg")
jpeg(file = filename,width = 600,height=500)
print(ggplot(data=ROCdata_allFloods,aes(x=threshold,y=TPR,color=flood))+
        #geom_point(color="red",size=3)+
        geom_line(linewidth=2)+
        ylab("True Positive Rate")+ xlab("Threshold")+
        xlim(0,1) + ylim(0,1) +
        theme_bw() + 
        theme(plot.title = element_text(size=24), 
              axis.title = element_text(size=24),
              axis.text = element_text(size = 20),
              legend.text= element_text(size=24),
              legend.title= element_text(size=24)))
dev.off()

#plot threshold VS TNR

filename<- paste0("plots/threshold_vs_TNR_PDFlood_allFloods.jpeg")
jpeg(file = filename,width = 600,height=500)
print(ggplot(data=ROCdata_allFloods,aes(x=threshold,y=TNR,color=flood))+
        #geom_point(color="red",size=3)+
        geom_line(linewidth=2)+
        ylab("True Negative Rate")+ xlab("Threshold")+
        xlim(0,1) + ylim(0,1) +
        theme_bw() + 
        theme(plot.title = element_text(size=24), 
              axis.title = element_text(size=24),
              axis.text = element_text(size = 20),
              legend.text= element_text(size=24),
              legend.title= element_text(size=24)))
dev.off()

################################################################################
######################### plots just for SB2010 ################################
################################################################################

filename<- paste0("plots/ROC_curve_SB2010_allFloods.jpeg")
jpeg(file = filename,width = 600,height=500)
print(ggplot(data=ROC_SB2010_allFloods,aes(x=FPR,y=TPR,color=flood))+
        #geom_point(color="red",size=3)+
        geom_line(linewidth=2)+
        geom_abline(intercept=0, slope=1, lwd = 2) +
        ylab("True Positive Rate")+ xlab("False Positive Rate")+
        xlim(0,1) + ylim(0,1) +
        theme_bw() + 
        theme(plot.title = element_text(size=24), 
              axis.title = element_text(size=24),
              axis.text = element_text(size = 20),
              legend.text= element_text(size=24),
              legend.title= element_text(size=24)))
dev.off()

#plot threshold VS TPR

filename<- paste0("plots/threshold_vs_TPR_SB2010_allFloods.jpeg")
jpeg(file = filename,width = 600,height=500)
print(ggplot(data=ROC_SB2010_allFloods,aes(x=threshold,y=TPR,color=flood))+
        #geom_point(color="red",size=3)+
        geom_line(linewidth=2)+
        ylab("True Positive Rate")+ xlab("Threshold")+
        xlim(0,1) + ylim(0,1) +
        theme_bw() + 
        theme(plot.title = element_text(size=24), 
              axis.title = element_text(size=24),
              axis.text = element_text(size = 20),
              legend.text= element_text(size=24),
              legend.title= element_text(size=24)))
dev.off()

#plot threshold VS TNR

filename<- paste0("plots/threshold_vs_TNR_SB2010_allFloods.jpeg")
jpeg(file = filename,width = 600,height=500)
print(ggplot(data=ROC_SB2010_allFloods,aes(x=threshold,y=TNR,color=flood))+
        #geom_point(color="red",size=3)+
        geom_line(linewidth=2)+
        ylab("True Negative Rate")+ xlab("Threshold")+
        xlim(0,1) + ylim(0,1) +
        theme_bw() + 
        theme(plot.title = element_text(size=24), 
              axis.title = element_text(size=24),
              axis.text = element_text(size = 20),
              legend.text= element_text(size=24),
              legend.title= element_text(size=24)))
dev.off()


################################################################################

#plot threshold VS TPR

filename<- paste0("plots/threshold_vs_TPR_PDFloodvsSB2010_allFloods.jpeg")
jpeg(file = filename,width = 600,height=500)
print(ggplot(data=combined_data,aes(x=threshold,y=TPR,color=flood,linetype = approach))+
        #geom_point(color="red",size=3)+
        geom_line(linewidth=2)+
        ylab("True Positive Rate")+ xlab("Threshold")+
        xlim(0,1) + ylim(0,1) +
        theme_bw() + 
        theme(plot.title = element_text(size=24), 
              axis.title = element_text(size=24),
              axis.text = element_text(size = 20),
              legend.text= element_text(size=24),
              legend.title= element_text(size=24)))
dev.off()

#plot threshold VS TNR

filename<- paste0("plots/threshold_vs_TNR_PDFloodvsSB2010_allFloods.jpeg")
jpeg(file = filename,width = 600,height=500)
print(ggplot(data=combined_data,aes(x=threshold,y=TNR,color=flood,linetype = approach))+
        #geom_point(color="red",size=3)+
        geom_line(linewidth=2)+
        ylab("True Negative Rate")+ xlab("Threshold")+
        xlim(0,1) + ylim(0,1) +
        theme_bw() + 
        theme(plot.title = element_text(size=24), 
              axis.title = element_text(size=24),
              axis.text = element_text(size = 20),
              legend.text= element_text(size=24),
              legend.title= element_text(size=24)))
dev.off()

plot1<- ggplot(data=combined_data,aes(x=threshold,y=TPR,color=flood,linetype = approach))+
  #geom_point(color="red",size=3)+
  geom_line(linewidth=2)+
  ylab("True Positive Rate")+ xlab("Threshold")+
  xlim(0,1) + ylim(0,1) +
  theme_bw() + 
  theme(plot.title = element_text(size=24), 
        axis.title = element_text(size=24),
        axis.text = element_text(size = 20),
        legend.text= element_text(size=24),
        legend.title= element_text(size=24))

plot2<- ggplot(data=combined_data,aes(x=threshold,y=TNR,color=flood,linetype = approach))+
  #geom_point(color="red",size=3)+
  geom_line(linewidth=2)+
  ylab("True Negative Rate")+ xlab("Threshold")+
  xlim(0,1) + ylim(0,1) +
  theme_bw() + 
  theme(plot.title = element_text(size=24), 
        axis.title = element_text(size=24),
        axis.text = element_text(size = 20),
        legend.text= element_text(size=24),
        legend.title= element_text(size=24))

plot1_no_legend <- plot1 + theme(legend.position = "none")
plot2_no_legend <- plot2 + theme(legend.position = "none")

legend1 <- cowplot::get_legend(plot1)

filename<- "plots/threshold_vs_TPRandTNR_PDFlood_vs_SB2010.jpeg"

jpeg(file = filename,width = 1420,height=620)
(plot1_no_legend | plot2_no_legend) + legend1 + 
    plot_layout(widths = c(1, 1, 0.2))

dev.off()

