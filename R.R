text.v <- scan("data/plainText/melville.txt",what='character',sep='\n')
text.v[1]

start.v <- which(text.v=='CHAPTER 1. Loomings.')

end.v <- which(text.v=='orphan.')
novel.lines.v <- text.v[start.v:end.v]

start.metadata.v <- text.v[1:start.v-1]
end.metadata.v <- text.v[(end.v+1):length(text.v)]

metadata.v <- c(start.metadata.v,end.metadata.v)


novel.lines.v <- text.v[start.v:end.v]

novel.v <- paste(novel.lines.v,collapse = " ")
head(novel.lower.v)


novel.lower.v <- tolower(novel.v)


mody.words.1 <- strsplit(novel.lower.v, "\\W")
moby.word.v <- unlist(mody.words.1)

not.blanks.v <- which(moby.words!="")

moby.word.v <- moby.word.v[not.blanks.v]

# beginning the analysis

length(moby.word.v[which(moby.word.v=="whale")])


# put a count of the occurerences of whale into whale.hits.v

whale.hits.v <- length(moby.word.v[which(moby.word.v=="whale")])
total.words.v <- length(moby.word.v)
whale.hits.v/total.words.v


length(unique(moby.word.v))


moby.freqs.t <- table(moby.word.v)

sorted.moby.freqs.t <- sort(moby.freqs.t,decreasing = TRUE)

# Practivce 

# 2.1
plot(sorted.moby.freqs.t[1:10])


# accesing the comparing word frequency data
sorted.moby.freqs.t['he']


moby.word.v[4:6]

sorted.moby.freqs.t[1]


# recycling 

sorted.moby.rel.freqs.t <- 100*(sorted.moby.freqs.t/sum(sorted.moby.freqs.t))



# TOken distribution analysis


n.timve.v <- seq(1:length(moby.word.v))

whales.v <- which(moby.word.v=="whale")


w.count.v <- rep(NA,length(n.timve.v))

w.count.v[whales.v] <- 1


plot(w.count.v,main = "Dispersion Plot of whale in Moby Dick",xlab = "novel time",ylab  ="whale",ylim=c(0,5),yaxt='n',type='h')


ahabs.v <- which(moby.word.v=='ahab')
a.count.v <- rep(NA,length(n.timve.v))
a.count.v[ahabs.v] <- 1
plot(a.count.v,main = "Dispersion Plot of whale in Moby Dick",xlab = "novel time",ylab  ="whale",ylim=c(0,5),yaxt='n',type='h' )


# Searching with grep.
rm(list=ls())

text.v <- scan("data/plainText/melville.txt",what='character',sep='\n')
text.v[1]

start.v <- which(text.v=='CHAPTER 1. Loomings.')

end.v <- which(text.v=='orphan.')
novel.lines.v <- text.v[start.v:end.v]
chap.positions.v <- grep("^CHAPTER \\d",novel.lines.v)


novel.lines.v[chap.positions.v]

novel.lines.v <- c(novel.lines.v,"END")

last.position.v <- length(novel.lines.v)
chap.positions.v <- c(chap.positions.v,last.position.v)

# loops 
chap.positions.v[2]


for( i in 1:length(chap.positions.v)){
  print(chap.positions.v[i])
}


for( i in 1:length(chap.positions.v)){
  print(paste("Chapter ",i, " begins at positions",
              chap.positions.v[i]), sep='')
}


chapter.raws.1 <- list()
chapter.freqs.1 <- list()


for (i in 1:length(chap.positions.v)){
  if(i!=length(chap.positions.v)){
    chapter.title <- novel.lines.v[chap.positions.v[i]]
    start <- chap.positions.v[i]+1
    end   <- chap.positions.v[i+1]-1
    chapter.lines.v <- novel.lines.v[start:end]
    chapter.words.v <- tolower(paste(chapter.lines.v, collapse = " "))
    chapter.words.l <- strsplit(chapter.words.v, "\\W")
    chapter.word.v  <- unlist(chapter.words.l)
    chapter.word.v  <- chapter.word.v[which(chapter.word.v!="")]
    chapter.freqs.t <- table(chapter.word.v)
    chapter.raws.1[[chapter.title]] <- chapter.freqs.t
    chapter.freqs.t.rel <- 100*(chapter.freqs.t/sum(chapter.freqs.t))
    chapter.freqs.1[[chapter.title]] <- chapter.freqs.t.rel
    }
}


novel.lines.v[chap.positions.v[i]]


## Accessing and Processing List Items

chapter.freqs.1
chapter.raws.1


chapter.freqs.1[[1]]["whale"]


whale.l <- lapply(chapter.freqs.1, '[', 'whale')


whales.m <- do.call(rbind,whale.l)


ahab.l <- lapply(chapter.freqs.1, '[', 'ahab')
ahabs <- do.call(rbind,ahab.l)

# cbind

class(whales.m[,1])


whales.v <- whales.m[,1]
ahabs.v <- ahabs[,1]

whales.ahabs.m <- cbind(whales.v,ahabs.v)
dim(whales.ahabs.m)
colnames(whales.ahabs.m) <- c("whale","ahab")
barplot(whales.ahabs.m,beside=T,col='green')


# Chapter 5 Correlation
whale.l <- lapply(chapter.freqs.1, '[', 'whale')

whales.ahabs.m[which(is.na(whales.ahabs.m))] <- 0

# identify the position of NA values in the matrix
the.na.positions <- which(is.na(whales.ahabs.m))
# setthe values held ion thje found positions to zero
whales.ahabs.m[the.na.positions] <- 0

cor(whales.ahabs.m)

mycor <- cor(whales.ahabs.m[,"whale"],whales.ahabs.m[,"ahab"])

# 5.4 Testing correlation with Randomizations
# make life easier and convert to a data frame
cor.data.df <- as.data.frame(whales.ahabs.m)

cor(cor.data.df)

sample(cor.data.df$whale)


cor(sample(cor.data.df$whale),cor.data.df$ahab)



mycors.v <- NULL
for(i in 1:10000){
  mycors.v <- c(mycors.v,cor(sample(cor.data.df$whale),cor.data.df$ahab))
}
h <- hist(mycors.v,breaks = 100,col="red",
          xlab = "Corellation Coefficient",
          main = "Histogram of Random Corellation Coefficients\n with Normal Curve",
          plot=TRUE)

xfit <- seq(min(mycors.v),max(mycors.v),length=1000)
yfit <- dnorm(xfit,mean=mean(mycors.v),sd=sd(mycors.v))
yfit <- yfit*diff(h$mids[1:2])*length(mycors.v)
lines(xfit,yfit,col="black",lwd=2)

# Chapter 6 Mesoanalysis
