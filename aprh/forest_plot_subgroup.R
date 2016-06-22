source('utils/graphics_utils.R')

#read formatted csv file
Lifelines_pm25_wheeze <- read.csv('lifelines_pm25_wheeze.csv',header=FALSE,colClasses = 'character') 

MLU <- .mean_lower_upper(Lifelines_pm25_wheeze) 

is.summary <- is.na(MLU[,'mean'])
title <- 'TEST'

forestplot(Lifelines_pm25_wheeze, new_page = T,
           is.summary = is.summary,
           legend_args = fpLegend(pos = list(x=.85, y=.95), 
                                  gp=gpar(col="#CCCCCC", fill="#F9F9F9")),
           legend = c("Lifelines", "Lifelines"),
           graph.pos=3,
           zero = 1,
           fn.ci_norm = c(fpDrawNormalCI, fpDrawCircleCI),
           txt_gp = fpTxtGp(label = gpar(cex=.75),xlab = gpar(cex = .9),ticks = gpar(cex = .6),title = gpar(cex = .9) ),
           boxsize = .15, # We set the box size to better visualize the type
           line.margin = .3, # We need to add this to avoid crowding
           mean = cbind(MLU[,'mean'], MLU[,'mean']),
           lower = cbind(MLU[,'lower'], MLU[,'lower']),
           upper = cbind(MLU[,'upper'], MLU[,'upper']),
           #clip = c(.77,1.31),
           xticks =seq(from=.5,to=1.5,by=.25),  
           col=fpColors(box=c("blue", "darkred")),
           xlab="Odd ratio",
           title = title
)
