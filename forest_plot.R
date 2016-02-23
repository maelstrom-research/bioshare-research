source('graphics_utils.R',echo=F,print.eval=F)

#--- stacking coefficient regression for forestplot ----
expos.c <- c('PM25_ESCAPE','PM10_ESC','NO2_ESCAPE','PMcoarse_ESCAPE')
outcomes.c <- c(
  'SYM_WHEEZ', 
  'SYM_WHEEZ_NOCOLD',
  'SYM_SBREATH',
  #'SYM_SBREATH_WALK',
  'SYM_BREATH_PRB',
  'SYM_PHLEGM_UP',
  'SYM_PHLEGM_UP_FREQ',
  'SYM_PHLEGM_DAY',
  'SYM_PHLEGM_DAY_FREQ'
  #'SYM_SBREATH_EXT'
)


model <- 'AGE_YRS+GENDER+PM_BMI_CATEGORIAL+EDU_HIGHEST_2+SMK_STATUS+SMK_PASSIVE_ALL+INCOME'


#by 
#####################
expo <- 'PM25_ESCAPE'
expo <- 'PM10_ESC'
expo <- 'NO2_ESCAPE'
expo <- 'PMcoarse_ESCAPE'

####################
outcome <- 'SYM_WHEEZ' 
#outcome <- 'SYM_WHEEZ_NOCOLD'
#outcome <- 'SYM_SBREATH'
outcome <- 'SYM_SBREATH_WALK'
#outcome <- 'SYM_BREATH_PRB'
#outcome <- 'SYM_PHLEGM_UP'
#outcome <- 'SYM_PHLEGM_UP_FREQ'
#outcome <- 'SYM_PHLEGM_DAY'
#outcome <- 'SYM_PHLEGM_DAY_FREQ'
outcome <- 'SYM_SBREATH_EXT'


gstack <- .glist.by(expo,outcome=outcomes.c,model=model,data='D',fam='binomial',by='outcome',datasources=opals[1])


#--- getting forestplot meta-data  ready---
meta.stack <- .stack.fp(gstack,label=expo)
labeltxt <- .label.fp(gstack,label=expo)
is.summary <- c(T,rep(F,nrow(meta.stack)-1))



#######mutiple
meta2 <- rbind(meta.stack,meta.stack)
label2<- rbind(labeltxt,labeltxt)
is.summary2 <- c(is.summary,is.summary)



#--------------------plotting -------------
#--- uni
forestplot(labeltxt,meta.stack,new_page = TRUE,is.summary=is.summary,zero =1,boxsize=.1,
           lwd.xaxis=2,
           txt_gp = fpTxtGp(label = gpar(cex=.60),xlab = gpar(cex = .8),ticks = gpar(cex = .5) ),
           ci.vertices.height = 0.02,
           lwd.zero = 2,
           col=fpColors(box="royalblue",line="darkblue", summary="royalblue"),
           xlab = 'Odds Ratio',title = 'Study Name(TODO)',
           vertice=T,
           clip = c(.95,1.21),
           xticks =seq(from=.9,to=1.15,by=.05))

#--- multi
forestplot(label2,meta2,new_page = TRUE,is.summary=is.summary2,zero =1,boxsize=.1,graph.pos=2,
           lwd.xaxis=2,
           txt_gp = fpTxtGp(label = gpar(cex=.60),xlab = gpar(cex = .8),ticks = gpar(cex = .5),summary = gpar(cex = .8) ),
           line.margin = .01,
           ci.vertices.height = 0.02,
           lwd.zero = 2,
           col=fpColors(box="royalblue",line="darkblue", summary="royalblue"),
           xlab = 'Odds Ratio',title = 'Study Name(TODO)',
           vertice=T,
           clip = c(.95,1.21),
           xticks =seq(from=.9,to=1.15,by=.05))

