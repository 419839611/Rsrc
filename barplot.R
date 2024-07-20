lasso$col = ifelse(lasso$coefficient > 0,'orange','deepskyblue4')
lasso = lasso[order(lasso$coefficient),]
pdf('coef.pdf')
barplot(lasso[,1],col=lasso[,2],horiz=T,xlim=c(-0.4,0.1),xlab='coefficient')
axis(2, at=seq(0.8,11,1.2),labels=rownames(lasso),las=2,tick=F,pos=-0.35)
dev.off()