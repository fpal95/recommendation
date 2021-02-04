PATH = "c:/tmdb/"
data_dir = read.csv(file=PATH+"credits_all.csv", header=TRUE)

crew = data_dir[]
crew = crew[crew$crew != "[]",]
crew = crew[!is.na(crew$id), ]

nr_crew = nrow(crew)

director = list()
for(i in 1:nr_crew){director[i] = strsplit(as.character(crew$crew[i]),split=",")}

#감독 정보 없으면 NA
index =  vector()
mat_direc = matrix(nrow=nr_crew,ncol=3)
for(i in 1:nr_crew){
  if(identical(grep(" 'job': 'Director'",director[[i]]),integer(0))){
    mat_direc[i,1] = crew$id[i]
    mat_direc[i,2] = NA
    mat_direc[i,3] =NA
  }else{
    index[i] = grep(" 'job': 'Director'",director[[i]])
    #small$director[[i]] = unlist(strsplit(small$director[[i]][index[i]+1],split=":"))[2]
    #small$director[[i]] = substr(small$director[[i]],3,nchar(small$director[[i]])-2)
    mat_direc[i,1] = crew$id[i]
    mat_direc[i,2] = unlist(strsplit(director[[i]][index[i]-1],split=":"))[2]
    mat_direc[i,2] = substr(mat_direc[i,2],2,nchar(director[[i]]))
    mat_direc[i,3] = unlist(strsplit(director[[i]][index[i]+1],split=":"))[2]
    mat_direc[i,3] = substr(mat_direc[i,3],3,nchar(mat_direc[i,3])-1)
  }
}

colnames(mat_direc)=c("id","direc_id","director")

#감독 정보 없는 행 삭제
df_direc = as.data.frame(mat_direc)
df_direc = df_direc[!is.na(df_direc$direc_id),]

#감독 파악(총 42541명의 감독)
uniq_direc = unique(df_direc)