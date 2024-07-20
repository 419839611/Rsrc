library(circlize)

bed = bed[order(bed$chrom,bed$start),]
plus = c('KAT2B','KAT6A','KAT6B','HDAC5')
subtract = c('KAT2A','EP300')
bed[plus,2:3] = bed[plus,2:3] + 5000000
bed[subtract,2:3] = bed[subtract,2:3] - 5000000
cytoband = read.cytoband()
cytoband$chr.len
#bed: chr, start, end, gene
plot_circos = function(bed,figname) {
	rownames(bed) = bed$gene
	# bed['HNRNPA2B1',2:3] = c(50229355,50240613)
	# bed['METTL3',2:3] = c(46966081,46979657)
	pdf(figname)
	circos.par("start.degree" = 90)
	circos.initializeWithIdeogram()
	circos.genomicTrack(ylim=c(0,10),bg.border=NA)
	for (i in 1:nrow(bed)) {
	    circos.genomicText(bed[i,2:3],bed[i,],y=4,labels.column=4,sector.index=bed[i,1],track.index=3,cex=0.5,facing= "clockwise")
	  }
	circos.clear()
	dev.off()
}