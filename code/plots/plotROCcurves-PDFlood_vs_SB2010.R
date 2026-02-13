#plot our ROC curve against a competing probabilistic approach

rm(list=ls())
library(ggplot2)

# setwd("/Volumes/RothS/probDnsclRealData")
setwd("/Users/f007f8t/Documents/GitHub/probDnsclRealData")

if(!dir.exists("plots")) dir.create("plots")

#load ROC data from PDFlood
load("data/ROC_data")

#load ROC data from SB2010
load("data/ROC_data_SB2010")


combined_data<- data.frame("TPR"=c(ROC_data$TPR,ROC_data_SB2010$TPR),
                           "FPR"=c(ROC_data$FPR,ROC_data_SB2010$FPR),
                           "TNR"=c(ROC_data$TNR,ROC_data_SB2010$TNR),
                           "FNR"=c(ROC_data$FNR,ROC_data_SB2010$FNR),
                           "threshold"= rep(seq(0.001,1,by=0.001),2),
                           "Approach"=c(rep("PDFlood",nrow(ROC_data)),rep("SB2010",nrow(ROC_data_SB2010))))
#plot the ROC curve

filename<- paste0("plots/ROC_curve_PDFlood_vs_SB2010.jpeg")
jpeg(file = filename,width = 600,height=500)
print(ggplot(data=combined_data,aes(x=FPR,y=TPR,color=Approach,linetype=Approach))+
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

#plot the reverse ROC curve
filename<- paste0("plots/reverseROC_curve_PDFlood_vs_SB2010.jpeg")
jpeg(file = filename,width = 600,height=500)
print(ggplot(data=combined_data,aes(x=FNR,y=TNR,color=Approach,linetype=Approach))+
        #geom_point(color="red",size=3)+
        geom_line(linewidth=2)+
        geom_abline(intercept=0,slope=1,lwd = 2) +
        ylab("True Negative Rate")+ xlab("False Negative Rate")+
        xlim(0,1) + ylim(0,1) +
        theme_bw() + 
        theme(plot.title = element_text(size=24), 
              axis.title = element_text(size=24),
              axis.text = element_text(size = 20),
              legend.text= element_text(size=24),
              legend.title= element_text(size=24)))
dev.off()


#plot threshold VS TPR

filename<- paste0("plots/threshold_vs_TPR_PDFlood_vs_SB2010.jpeg")
jpeg(file = filename,width = 600,height=500)
print(ggplot(data=combined_data,aes(x=threshold,y=TPR,color=Approach))+
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

#plot threshold VS FPR

filename<- paste0("plots/threshold_vs_FPR_PDFlood_vs_SB2010.jpeg")
jpeg(file = filename,width = 600,height=500)
print(ggplot(data=combined_data,aes(x=threshold,y=FPR,color=Approach))+
        #geom_point(color="red",size=3)+
        geom_line(linewidth=2)+
        ylab("False Positive Rate")+ xlab("Threshold")+
        xlim(0,1) + ylim(0,1) +
        theme_bw() + 
        theme(plot.title = element_text(size=24), 
              axis.title = element_text(size=24),
              axis.text = element_text(size = 20),
              legend.text= element_text(size=24),
              legend.title= element_text(size=24)))
dev.off()

#plot threshold VS TNR

filename<- paste0("plots/threshold_vs_TNR_PDFlood_vs_SB2010.jpeg")
jpeg(file = filename,width = 600,height=500)
print(ggplot(data=combined_data,aes(x=threshold,y=TNR,color=Approach))+
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

#plot threshold VS FNR

filename<- paste0("plots/threshold_vs_FNR_PDFlood_vs_SB2010.jpeg")
jpeg(file = filename,width = 600,height=500)
print(ggplot(data=combined_data,aes(x=threshold,y=FNR,color=Approach))+
        #geom_point(color="red",size=3)+
        geom_line(linewidth=2)+
        ylab("False Negative Rate")+ xlab("Threshold")+
        xlim(0,1) + ylim(0,1) +
        theme_bw() + 
        theme(plot.title = element_text(size=24), 
              axis.title = element_text(size=24),
              axis.text = element_text(size = 20),
              legend.text= element_text(size=24),
              legend.title= element_text(size=24)))
dev.off()