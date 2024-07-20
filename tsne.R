library(Rtsne)

#expression is a dataframe, rows are samples and columns are genes
#colors represent different sample groups, skyblue tomato yellowgreen gray72
plot_tsne = function(expression,figname,colors) {
	pdf(figname)
	tsne = Rtsne(expression, perplexity=10)
	plot(tsne$Y, col =colors ,pch = 19, cex = 1,xlab='Tsn1',ylab='Tsn2')
	legend('topright',pch=19,col=unique(colors),legend=c('cluster1(N=)','cluster2(N=)'))
	dev.off()
}
plot_tsne('tsne2.pdf',as.matrix(hyp),colors)
#function is error: Input X is not a matrix
tsne = Rtsne(hyp, perplexity=10)
pdf('tsne2.pdf')
plot(tsne$Y, col =colors ,pch = 19, cex = 1,xlab='Tsn1',ylab='Tsn2')
legend('topright',pch=19,col=unique(colors),legend=c('cluster1(N=130)','cluster2(N=47)'))
dev.off()