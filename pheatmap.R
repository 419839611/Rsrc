library(pheatmap)

cluster = read.delim('../3.KAT_cluster/2_km.txt',header=T,row.names=1)
cluster1=grep('KAT_KDAC.cluster1',cluster$cluster)
cluster2=grep('KAT_KDAC.cluster2',cluster$cluster)
data = data[,c(cluster1,cluster2)]
heatmap = pheatmap(data,width=30,cluster_cols=F,cluster_rows=T,show_colnames=F,annotation_col=cluster,scale='row',filename=figname)

library(ComplexHeatmap)

pdf('score_heatmap.pdf',height=2.5)
ht = Heatmap(matrix(data[,1],ncol=365),
        top_annotation=columnAnnotation(`risk score`=data[,2],grade=clinical$neoplasm_histologic_grade,weight=clinical$weight_group,stage=clinical$pathologic_stage,`pathologic stage`=clinical$pathologic_T),
        name='TF risk score',cluster_rows =F,cluster_columns =F,show_column_names =F,show_row_names =F)
draw(ht,heatmap_legend_side = "left",annotation_legend_side = "bottom")
dev.off()