library(ggalluvial)

#clusters: column=KAT_KDAC.cluster gene.cluster score survival
plot_sankey = function(clusters,figname) {
	long = to_lodes_form(clusters,key = "clusters",axes = 1:ncol(clusters))
	figure = ggplot(data = long,
		aes(x = clusters,stratum = stratum, alluvium = alluvium,label = stratum)) +
		geom_alluvium(aes(fill = stratum)) +
		geom_stratum() + 
		geom_text(stat = "stratum") +
		labs(x = '') +
		theme_minimal() +
		theme(legend.position = 'none',axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12))
		# theme(panel.grid = element_blank(), legend.title = element_blank(), legend.position = 'none',
	 #          panel.background = element_blank(), axis.text.y = element_blank(), 
	 #          axis.ticks = element_blank(), axis.text.x = element_text(size = 12))
		# theme_minimal()
	ggsave(filename = figname, plot = figure)
}