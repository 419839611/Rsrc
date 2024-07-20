library(survminer)
library(survival)

#survival_risk(tmp,'score_survival.pdf')
survival_risk = function(data,figurename) {
	fit = surv_fit(Surv(OS.month, OS) ~ group, data = data)
	res = ggsurvplot(fit,fun='pct',risk.table=T,pval = T,xlab='OS Time (months)',
		legend.labs = c("cluser1(N=130)", "cluster2(N=47)"))
	# res$table <- res$table + theme(axis.line=element_blank(),axis.ticks=element_blank(),axis.text.x=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank())
	# res$plot <- res$plot + annotate(geom='text',x=2.5,y=0.5,label=text)
	ggsave(file = figurename, print(res),width =7.8,onefile=F)

}

survival_risk_censor = function(data,figurename) {
	fit = surv_fit(Surv(OS.month, OS) ~ group, data = data)
	res = ggsurvplot(fit,fun='pct',risk.table=T,ncensor.plot=T,pval = T,xlab='OS Time (months)')
	# res$table <- res$table + theme(axis.line=element_blank(),axis.ticks=element_blank(),axis.text.x=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank())
	# res$plot <- res$plot + annotate(geom='text',x=2.5,y=0.5,label=text)
	ggsave(file = figurename, print(res),width =7.8)

}


library(survival)

plot_survival = function(data,group) {
	diff = survdiff(Surv(PFI.time,PFI) ~ risk,data=data)
	geneupper = subset(data,risk == "high")
	fitUp = survfit(Surv(PFI.time,PFI) ~ 1,data=geneupper,conf.type="log-log")
	genelower = subset(data,risk == "low")
	fitLow = survfit(Surv(PFI.time,PFI) ~ 1, data=genelower,conf.type="log-log")
	p = 1 - pchisq(diff$chisq, length(diff$n) - 1)
	plot(fitLow,conf.int=F,mark.time=T,col="blue",lwd=1.5,main=group,xlab='Time(days)',ylab='Survival rate',xlim=c(0,7100))
	lines(fitUp,conf.int=F,mark.time=T,col="red",lwd=1.5)
	text(x=6000,y=0.1,paste0('p=',round(p,4)),cex=2)
	legend("bottomleft",lty=c(1,1),c("high risk","low risk"),col=c("red","blue"))
}
pdf('KM_score.pdf')
plot_survival(train_result,'training')
dev.off()