library(forestplot)

#coxf: column=pvalue,hr,hr_low,hr_high
#eg: plot_forestplot = plot_forestplot(immune_os,'immune_os.pdf')
plot_forestplot = function(coxf, figname) {
	coxf = coxf[order(coxf[,2],decreasing=T),]
	coxf = round(coxf, 3)
	conf = paste0(coxf[, 2], '(', coxf[, 3], '-', coxf[, 4], ')')
	mytext = cbind(c('Signature', '', rownames(coxf)),
	              c('pvalue', '', coxf[, 1]),
	              c('HR(CI)', '', conf))
	pdf(figname, height = 5, width = 8)
	forestplot(labeltext = mytext, hrzl_lines = list("2" = gpar(lwd = 1, col = "black")),
	         mean = c(NA, NA, coxf[, 2]), lower = c(NA, NA, coxf[, 3]), upper = c(NA, NA, coxf[, 4]),
	         boxsize = 0.2, graph.pos = 2, col = fpColors(lines = "#990000", box = "#660000", zero = "darkblue"),
	         lwd.ci = 2, ci.vertices.height = 0.1, ci.vertices = TRUE, zero = 1, lwd.zero = 2, grid = T, lwd.xaxis = 1, 
	         colgap = unit(0.5, "cm"), txt_gp = fpTxtGp(ticks = gpar(cex = 0.8)), new_page = FALSE)
	dev.off()
}