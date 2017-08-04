source('utils/graphics_utils.R')

#read formatted csv file
data<- read.csv('aprh/forest-plots/PooledNO2.csv',header=FALSE,colClasses = 'character') 
csvtext <- .csv_to_text(data)

MLU1 <- .mean_lower_upper(data[1:19,]) 
MLU2 <- .mean_lower_upper(data[-c(1:19),]) 

is.summary <- is.na(MLU1[,'mean'])
title <- 'TEST'

png(file="aprh/forest-plots/PooledNO2.png",width=3600,height=3000,res=300)

forestplot(csvtext, new_page = T,
           is.summary = is.summary,
           legend_args = fpLegend(pos = list(x=.5, y=.95), 
                                  gp=gpar(col="#CCCCCC", fill="#F9F9F9")),
           legend = c("Wheeze", "Shortness of breath"),
           graph.pos=3,
           zero = 1,
           fn.ci_norm = c(fpDrawNormalCI, fpDrawCircleCI),
           txt_gp = fpTxtGp(label = list(gpar(cex=1.05),
                                         gpar(cex=1.05),
                                         gpar(cex=1.05),
                                         gpar(cex=1.05, col="blue")),
                            legend = gpar(cex =1.05),
                            xlab = gpar(cex = 1.15),
                            ticks = gpar(cex = 1.05),
                            title = gpar(cex = 1.15)),
           hrzl_lines=list("7" = gpar(lwd=98, lineend="butt", columns=c(2:5), col="#99999922"),
                           "13" = gpar(lwd=98, lineend="butt", columns=c(2:5), col="#99999922"),
                           "19" = gpar(lwd=98, lineend="butt", columns=c(2:5), col="#99999922")),
           boxsize = .15, # We set the box size to better visualize the type
           line.margin =.20 , # We need to add this to avoid crowding
           mean = cbind(MLU1[,'mean'], MLU2[,'mean']),
           lower = cbind(MLU1[,'lower'], MLU2[,'lower']),
           upper = cbind(MLU1[,'upper'], MLU2[,'upper']),
           #clip = c(.77,1.31),
           #xticks =seq(from=0.9,to=2.1,by=.1),  
           xticks =c(from=1,1.5,2),  
           col=fpColors(box=c("blue", "darkred")),
           xlab="OR (95% CI)"
)

dev.off()