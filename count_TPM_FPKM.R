#exp gene_len基因一一对应
#exp: row=gene, column=sample
count2tpm = function(exp,gene_len,filename){
        exp = exp / gene_len
        total = colSums(exp)
        exp_tpm = t(t(exp) / total)
        exp_tpm = exp_tpm * 10^6
        write.table(exp_tpm,filename,sep='\t',quote=F,row.names=T,col.names=T)
        return(exp_tpm)
}
tpm = count2tpm(count,gene_length$Length,'GSE148506_tpm.txt')



countToTpm <- function(counts, effLen)
{
    rate <- log(counts) - log(effLen)
    denom <- log(sum(exp(rate)))
    exp(rate - denom + log(1e6))
}

countToFpkm <- function(counts, effLen)
{
    N <- sum(counts)
    exp( log(counts) + log(1e9) - log(effLen) - log(N) )
}

fpkmToTpm <- function(fpkm)
{
    exp(log(fpkm) - log(sum(fpkm)) + log(1e6))
}

################################################################################
# An example
################################################################################
# count convert
cnts <- c(4250, 3300, 200, 1750, 50, 0)
lens <- c(900, 1020, 2000, 770, 3000, 1777)
countDf <- data.frame(count = cnts, length = lens)

## assume a mean(FLD) = 203.7
#countDf$effLength <- countDf$length - 203.7 + 1
countDf$effLength=countDf$length
countDf$tpm <- with(countDf, countToTpm(count, effLength))
countDf$fpkm <- with(countDf, countToFpkm(count, effLength))

#fpkmToTpm
expMatrix<-read.table("fpkm_expr.txt",header = T,row.names = 1)
tpms <- apply(expMatrix,2,fpkmToTpm)
tpms[1:3,]
#最后可以根据TPM的特征进行检查，看每列加和是否一致
colSums(tpms)


a = read.delim('gencode.v42lift37.annotation.20221122.CDS.bed',header=F)
a$l = a[,3] - a[,2]
b = aggregate(l~V5,a,sum)
c = aggregate(l~V4,a,sum)
write.table(b,'CDS_len.txt',sep ="\t",quote =F,col.names =F,row.names =F)

library("GenomicFeatures")

txdb <- makeTxDbFromGFF("/GCI/home/huisun/common_data/annotation/gencode.v42lift37.annotation.gtf",format="gtf")
exons_gene <- exonsBy(txdb, by = "gene")
exons_gene_len <- lapply(exons_gene,function(x){sum(width(reduce(x)))})
df <- data.frame(matrix(unlist(exons_gene_len), nrow=length(exons_gene_len), byrow=TRUE),row.names=names(exons_gene_len))
write.table(df,file ="/share_storage/pipeline/NGSToolkit6/database_NGS/STAR_database/gencodeV42/gene_exons_len.txt",sep ="\t",quote =F,col.names =F,row.names =T)
