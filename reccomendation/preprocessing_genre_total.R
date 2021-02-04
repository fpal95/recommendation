t = read.csv(file="C:/tmdb/movies_metadata.csv", header=TRUE)

nr_g = nrow(t)
v = vector()
n = vector()
g = vector()

#####################step1-split the dataset#####################
#split column 'genres' and remove some columns
for(i in 1:nr_g){
  t$category[i] = strsplit(as.character(t[i,4]),split=",")
  l = length(t$category[[i]])/2
  for(j in 1:l){
    a = 2*j-1
    v = c(v,a)
  }
  t$category[[i]] = t$category[[i]][-v] 
}

t = t[-c(25:45)]

#substring only genres
for(i in 1:nr_g){
  l = length(t$category[[i]])
  if(l==1){name = substr(t$category[[i]][l],11,nchar(t$category[[i]][l])-3)
  n = c(n,name)}
  else{for(j in 1:(l-1)){name = substr(t$category[[i]][j],11,nchar(t$category[[i]][j])-2)
  n = c(n,name)}
    name = substr(t$category[[i]][l],11,nchar(t$category[[i]][l])-3)
    n = c(n, name)
  }
} 

#type of genres
cate = levels(as.factor(n)) #vector

#####################step2-split the genres#####################
n_g = length(cate)

#make genres list for next step
for(i in 1:nr_g){
  l = length(t$category[[i]])
  if(l==1){t$cate[[i]] = unlist(strsplit(t$category[[i]][l],split=":"))[2]
  t$cate[[i]][l] = substr(t$cate[[i]],3,nchar(t$cate[[i]])-3)}
  else{for(j in 1:(l-1)){
    t$cate[[i]][j] = unlist(strsplit(t$category[[i]][j],split=":"))[2]
    t$cate[[i]][j] = substr(t$cate[[i]][j],3,nchar(t$cate[[i]][j])-2)}
  }
  t$cate[[i]][l] = unlist(strsplit(t$category[[i]][l],split=":"))[2]
  t$cate[[i]][l] = substr(t$cate[[i]][l],3,nchar(t$cate[[i]][l])-3)
}

#binary type of genres
#해당 장르에 속하면 1, 아니면 0
for(i in 1:nr_g){
  k = length(t$cate[[i]])
  for(j in 1:n_g){
    indi = vector()
    for(m in 1:k){
      if(t$cate[[i]][m]==cate[j]) indi[m]=1 else indi[m]=0
    }
    indicate=sum(indi)
    g=c(g,indicate)
  }
}

#make matrix using binary dataset above
genre1 = matrix(g,nr_g,n_g,byrow =TRUE)
colnames(genre1) = c("Action","Adventure","Animation","Comedy","Crime",
                     "Documentary","Drama","Family","Fantasy","Foreign",
                     "History","Horror","Music","Mystery","Romance","SF",
                     "Thriller","TV Movie","War","Western")
genre1 = cbind(genre1, id=t["id"])