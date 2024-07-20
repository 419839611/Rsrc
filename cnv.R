library(maftools)

gistic = readGistic(gisticAllLesionsFile = 'all_lesions.conf_95.txt',
	gisticAmpGenesFile = 'amp_genes.conf_95.txt', 
	gisticDelGenesFile = 'del_genes.conf_95.txt', 
	gisticScoresFile = 'scores.gistic')
pdf('low_cnv.pdf')
gisticChromPlot(gistic = gistic, ref.build = 'hg38')
dev.off()
