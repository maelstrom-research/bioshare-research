###########################################
#     FOREST PLOT FOR UKBIOBANK
##########################################


#Attach all functions required for foresplot
source('utils/graphics_utils.R',echo=F,print.eval=F)

# LOAD DATAs IN EACH OPAL SERVER
source('aprh/aprh_data.R',echo=F,print.eval=F)

#RUN DUMMY STUDY HERE once so it applies to all subsequent steps
run.dummy.study('D') #run once 

#--- stacking coefficient regression for forestplot ----
expos.c <- c('PM25_ESCAPE','PM10_ESC','NO2_ESCAPE','PMcoarse_ESCAPE')
outcomes.c <- c(
  'SYM_WHEEZ', 
  'SYM_SBREATH_EXT'
)

model <- 'AGE_YRS+GENDER+PM_BMI_CATEGORIAL+EDU_HIGHEST_2+SMK_STATUS+SMK_PASSIVE_ALL+INCOME'


gstack.pm25 <- .glist.by('PM25_ESCAPE',outcome=outcomes.c,model=model,data='D',fam='binomial',by='outcome',ref='lifelines')
gstack.pmco <- .glist.by('PMcoarse_ESCAPE',outcome=outcomes.c,model=model,data='D',fam='binomial',by='outcome',ref ='lifelines')
gstack.no2 <- .glist.by('NO2_ESCAPE',outcome=outcomes.c,model=model,data='D',fam='binomial',by='outcome',ref ='lifelines')

#--- getting forestplot meta-data  ready---
meta.pm25 <- .stack.fp(gstack.pm25,fix='PM2.5')
meta.pmco <- .stack.fp(gstack.pmco,fix='PMcoarse')
meta.no2 <- .stack.fp(gstack.no2,fix='NO2')

#define statistics name ex: 'Odds ratio (95% CI)'
stat.name <- 'Odds ratio (95% CI)'

#only the first group or study need to define the stat otherwise it will display everywhere in the final plot
labeltxt.pm25 <- .label.fp(gstack.pm25,fix='PM2.5',statdesc=stat.name)  #<- "stat" is defined only here the fist group in the plot
labeltxt.pmco <- .label.fp(gstack.pmco,fix='PMcoarse')
labeltxt.no2 <- .label.fp(gstack.no2,fix='NO2')


is.summary <- c(T,rep(F,nrow(meta.pm25)-1))

####### combine pm2.5 and pmcoarse
meta.c <- rbind(meta.pm25,meta.pmco,meta.no2)
label.c<- rbind(labeltxt.pm25,labeltxt.pmco,labeltxt.no2)
is.summary.c <- c(is.summary,is.summary)


#--------------------plotting -------------

#--- multi
forestplot(label.c,meta.c,new_page = TRUE,is.summary=is.summary.c,zero =1,boxsize=.1,graph.pos=2,
           lwd.xaxis=2,
           txt_gp = fpTxtGp(label = gpar(cex=.70),xlab = gpar(cex = .8),ticks = gpar(cex = .5),title = gpar(cex = .9) ),
           line.margin = .01,
           ci.vertices.height = 0.02,
           lwd.zero = 2,
           col=fpColors(box="royalblue",line="darkblue", summary="royalblue"),
           xlab = 'Odds Ratio',
           title = 'Pooled',
           vertice=T,
           clip = c(.77,1.31),
           xticks =seq(from=.85,to=1.15,by=.05)   
)

