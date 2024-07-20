library(ConsensusClusterPlus)

#kat_exp is a matrix: row=gene,column=sample
cluster_km = ConsensusClusterPlus(kat_exp,maxK=6,reps=1000, pItem=0.8,clusterAlg="km",
	title="/home/kaela/projects/F200914002/3.KAT_cluster",distance="euclidean",plot='pdf')

cluster_hc = ConsensusClusterPlus(kat_exp,maxK=6,reps=1000, pItem=0.8,clusterAlg="hc",
	title="/home/kaela/projects/F200914002/3.KAT_cluster",distance="pearson",plot='pdf',innerLinkage="complete")

cluster_pam = ConsensusClusterPlus(kat_exp,maxK=6,reps=1000, pItem=0.8,clusterAlg="pam",
	title="/home/kaela/projects/F200914002/3.KAT_cluster",plot='pdf')
for (i in 2:4) {
	cluster = cluster_km[[i]]$consensusClass
	test = data.frame(sample=names(cluster),cluser=cluster)
	write.table(test,paste0(as.character(i),'cluster.txt'),sep='\t',quote=F,row.names=F,col.names=T)
}