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

gstack <- .glist.by(expos.c,outcome=outcome,model=model,data='D',fam='binomial',by='expo',datasources=opals[1])


#--- getting forestplot meta-data  ready---
meta.stack <- .stack.fp.info(gstack)
labeltxt <- .labelfptext(gstack)
is.summary <- c(T,rep(F,nrow(meta.stack)-1))



#--------------------plotting -------------
#---
forestplot(labeltxt,meta.stack,new_page = TRUE,is.summary=is.summary,zero =1,boxsize=.05,graph.pos=2,
           txt_gp = fpTxtGp(label = gpar(cex=.70)),ci.vertices.height = 0.02,lwd.zero = 5,
           col=fpColors(box="royalblue",line="darkblue", summary="royalblue"), clip =c(.95,1.1),
           xlab = 'OR log scale',title = 'TEST',vertice=T,xticks = c(.95,1,1.05,1.1))


forestplot(ztext,zm,new_page = TRUE,is.summary=is.summary,zero =1,boxsize=.05,graph.pos=2,
           txt_gp = fpTxtGp(label = gpar(cex=.70)),ci.vertices.height = 0.02,
           col=fpColors(box="royalblue",line="darkblue", summary="royalblue"),
           xlab = 'OR log scale',title = 'TEST',vertice=T,xticks = c(.95,1,1.05,1.1))

forestplot(labeltxt,mean = cbind(dbl.meta[1],dbl.meta[4]),lower = cbind(dbl.meta[2],dbl.meta[5]),upper =cbind(dbl.meta[3],dbl.meta[6]),
           new_page = TRUE,is.summary=is.summary,zero =1,boxsize=.05,graph.pos=2,line.margin = .1,
           txt_gp = fpTxtGp(label = gpar(cex=.70)),legend = c('paul','raoul'),ci.vertices = T,ci.vertices.height = .02,
           col=fpColors(box=c("royalblue",'darkred'),line="darkblue", summary="royalblue"),
           xlab = 'OR log scale',lwd.zero = 3,fn.ci_norm = c(fpDrawNormalCI, fpDrawCircleCI) )