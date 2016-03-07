###########################################
#     FOREST PLOT FOR LIFELINES
##########################################

#Attach all functions required for foresplot
source('utils/graphics_utils.R',echo=F,print.eval=F)

# LOAD DATAs IN EACH OPAL SERVER
source('aprh/aprh_data.R',echo=F,print.eval=F)


#--- stacking coefficient regression for forestplot ----
expos.c <- c('PM25_ESCAPE','PM10_ESC','NO2_ESCAPE','PMcoarse_ESCAPE')
outcomes.c <- c(
  'SYM_BREATH_PRB',
  'SYM_WHEEZ', 
  'SYM_WHEEZ_NOCOLD',
  'SYM_SBREATH',
  #'SYM_SBREATH_WALK',
  'SYM_PHLEGM_UP',
  #'SYM_PHLEGM_UP_FREQ',
  'SYM_PHLEGM_DAY'
  #'SYM_PHLEGM_DAY_FREQ'
  #'SYM_SBREATH_EXT'
)


model <- 'AGE_YRS+GENDER+PM_BMI_CATEGORIAL+EDU_HIGHEST_2+SMK_STATUS+SMK_PASSIVE_ALL+INCOME'


gstack.pm25 <- .glist.by('PM25_ESCAPE',outcome=outcomes.c,model=model,data='D',fam='binomial',by='outcome',datasources=opals[1])
gstack.pmco <- .glist.by('PMcoarse_ESCAPE',outcome=outcomes.c,model=model,data='D',fam='binomial',by='outcome',datasources=opals[1])

#--- getting forestplot meta-data  ready---
meta.pm25 <- .stack.fp(gstack.pm25,fix='PM2.5')
meta.pmco <- .stack.fp(gstack.pmco,fix='PMcoarse')

labeltxt.pm25 <- .label.fp(gstack.pm25,fix='PM2.5')
labeltxt.pmco <- .label.fp(gstack.pmco,fix='PMcoarse')



is.summary <- c(T,rep(F,nrow(meta.pm25)-1))

####### combine pm2.5 and pmcoarse
meta2 <- rbind(meta.pm25,meta.pmco)
label2<- rbind(labeltxt.pm25,labeltxt.pmco)
is.summary2 <- c(is.summary,is.summary)


#--------------------plotting -------------

#--- multi
forestplot(label2,meta2,new_page = TRUE,is.summary=is.summary2,zero =1,boxsize=.1,graph.pos=2,
           lwd.xaxis=2,
           txt_gp = fpTxtGp(label = gpar(cex=.75),xlab = gpar(cex = .8),ticks = gpar(cex = .5),summary = gpar(cex = .8),title = gpar(cex = .9) ),
           line.margin = .01,
           ci.vertices.height = 0.02,
           lwd.zero = 2,
           col=fpColors(box="royalblue",line="darkblue", summary="royalblue"),
           xlab = 'Odds Ratio',
           title = 'LifeLines',
           vertice=T,
           clip = c(.77,1.31),
           xticks =seq(from=.8,to=1.25,by=.05)   
           )

