##배우 추출하기
PATH = "c:/tmdb/"
cast = read.csv(file=PATH + "credits_all.csv", header=TRUE)

nr_cast = nrow(cast)

#주연과 조연의 순서대로 배우 정렬되어 있음
#상위에 기재되어 있는 배우가 영화의 중요 인물이라는 가정하에
#6명의 배우만 포함
for(i in 1:nr_cast){cast$actor[i] = strsplit(as.character(cast[i,1]),split=",")
cast$actor[[i]] = cast$actor[[i]][1:48]}

del = vector()
for(j in 1:6){
  a = 8*j-7
  b = 8*j-5
  c = 8*j-1
  d = 8*j
  del = c(del,a,b,c,d)}
for(i in 1:nr_cast){cast$actor[[i]] = cast$actor[[i]][-del]}

actor1 = matrix(nrow=nr_cast,ncol=24)
for(i in 1:6){
  for(j in 1:nr_cast){
    a = 4*i-3
    b = 4*i-2
    c = 4*i-1
    d = 4*i
    actor1[j,a] = unlist(cast$actor[[j]])[a]
    actor1[j,b] = unlist(cast$actor[[j]])[b]
    actor1[j,c] = unlist(cast$actor[[j]])[c]
    actor1[j,d] = unlist(cast$actor[[j]])[d]
    
    actor1[j,a] = unlist(strsplit(actor1[j,a],split=":"))[2]
    actor1[j,b] = unlist(strsplit(actor1[j,b],split=":"))[2]
    actor1[j,c] = unlist(strsplit(actor1[j,c],split=":"))[2]
    actor1[j,d] = unlist(strsplit(actor1[j,d],split=":"))[2]
    
    actor1[j,a] = substr(actor1[j,a],3,nchar(actor1[j,a])-1)
    actor1[j,b] = trimws(actor1[j,b])
    actor1[j,c] = trimws(actor1[j,c])
    actor1[j,d] = substr(actor1[j,d],3,nchar(actor1[j,d])-1)
  }
}

#각 영화의 고유 id와 6명의 배우를 병합.
actor1 = cbind(actor1, id=cast["id"])

#t = read.csv(file=PATH+"movies_metadata.csv", header=TRUE)

act = merge(actor1,t,'id',all=FALSE)
act = subset(act,select=-c(adult,belongs_to_collection,genres,homepage,budget,video,original_language,
                           original_title,imdb_id,overview,popularity,poster_path,production_companies,
                           production_countries,release_date,revenue,runtime,spoken_languages,status,tagline,
                           vote_average,vote_count))
colnames(act)=c("id","character_1","gender_1","acterid_1","actername_1",
                "character_2","gender_2","acterid_2","actername_2",
                "character_3","gender_3","acterid_3","actername_3",
                "character_4","gender_4","acterid_4","actername_4",
                "character_5","gender_5","acterid_5","actername_5",
                "character_6","gender_6","acterid_6","actername_6", "title")

#결측치 행 제거
act_nomiss = na.omit(act)

install.packages("dplyr")
library("dplyr")

act = act[complete.cases(act),]
s_1 = summarise(group_by(act,actername_1), mean=mean(vote_average))
s_2 = summarise(group_by(act,actername_2), mean=mean(vote_average))
s_3 = summarise(group_by(act,actername_3), mean=mean(vote_average))
s_4 = summarise(group_by(act,actername_4), mean=mean(vote_average))
s_5 = summarise(group_by(act,actername_5), mean=mean(vote_average))
s_6 = summarise(group_by(act,actername_6), mean=mean(vote_average))