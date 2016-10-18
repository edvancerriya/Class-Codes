x=sample(c("a","b","c","d"),50,replace = T)
id=paste0("a",1:50)
d=data.frame(id=id,x=x)
d$x=as.character(x)

make_dummy=function(df,var_name){
  t=table(d[,var_name])
  exclude=names(t)[t==min(t)][1]
  values=unique(d[,var_name])
  values=values[values!=exclude]
  for(value in values){
    df[,paste0(var_name,".",value)]=as.numeric(df[,var_name]==value)
  }
  l= names(df)==var_name
  df=df[,!l]
}
d=make_dummy(d,"x")
