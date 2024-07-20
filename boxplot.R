library (plyr)

# immune_df = apply(tmp,1,function(x,clustername=cluster$cluster){data.frame(gsea=x,cluster=clustername)})
# test = ldply (immune_df, data.frame)
# p_asterisk = ifelse(immune_p>=0.05,'ns',ifelse(immune_p>=0.01,'*',ifelse(immune_p>=0.001,'**','***')))
# eg:plot_2group('immune_infiltration.pdf',test,'Immune Infiltration',p_asterisk,8)
plot_2group = function(fig,data,y,p_asterisk,figheight) {
	xlab = unique(data$cell)
	ma = max(data$gsea)
	mi = min(data$gsea)
	par(mar=c(8,4.1,0.5,2.1))
	# par(mai=c(3.5,0.82,0.1,0.42))
	pdf(fig,width=15,height=figheight)
	boxplot(gsea ~ cluster+cell,data=data,col=c('skyblue','tomato'),ylim=c(mi,ma+0.1),ylab=y,xlab='',axes = F)
	axis(2, las=1)
	axis(1, at = seq(1.5,length(xlab)*2,2),labels=xlab,las=2)
	legend('topright',col=c('skyblue','tomato'),legend=unique(test$cluster),pch=c(15,15),box.lwd=0,ncol=2)
	for (i in 1:length(p_asterisk)) {text(x=i*2-0.5,y=ma+0.05,p_asterisk[i])}	
	dev.off()
}


library(ggplot2)
library(ggpubr)
pf = ggboxplot(test, x="cell", y="gsea", color = "cluster",fill='cluster',palette=c('skyblue','tomato'),ylab='Immune Infiltration',xlab='') +
	rotate_x_text(angle = 45) +
	ggtitle('Gemcitabine') + 
	stat_compare_means(aes(group=cluster),label = "p.signif", method = "wilcox.test")
ggsave('immune_infiltration.pdf', plot = pf,width=15,height = 8)

kat_cluster = data.frame(score=score$score,KAT_KDAC.cluster=paste0('KAT_KDAC.cluster',clusters$KAT_KDAC.cluster))
pf = ggboxplot(kat_cluster, x = "KAT_KDAC.cluster", y = "score",color = "KAT_KDAC.cluster",palette = c('skyblue','tomato'),xlab='',add = "jitter")+
	stat_compare_means(comparisons = list(c('KAT_KDAC.cluster1','KAT_KDAC.cluster2')),label = "p.format", method = "wilcox.test") # Add pairwise comparisons p-value
	#stat_compare_means(label.y = 50)     # Add global p-value
ggsave('katcluster_score.pdf', plot = pf)