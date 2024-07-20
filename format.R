library (plyr)

#data: row=gene,column=sample
mtrx2group = function(data,group){
	data = apply(data,1,function(x,clustername=group){data.frame(dependent_variables=x,group=clustername)})
	group_df = ldply (data, data.frame)
	colnames(group_df)[1] = 'independent_variables'
	return(group_df)
}
