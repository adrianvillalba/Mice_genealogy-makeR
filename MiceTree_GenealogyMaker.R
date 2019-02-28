#First I need to load the libraries i will need
library(kinship2)
library(pedigree)
library(dplyr) 
library(utils)

#Then I open, read and extract the requested information:
colony <- read.csv(file.choose(), sep=";", stringsAsFactors=FALSE)
id <- colony$Animal
id1 <- colony$Father
id2 <- colony$Mother
oldped <- data.frame(id, id1, id2)
ped = oldped[!apply(is.na(oldped) | oldped == "", 1, all), ]

#Pedigree representation
pedAll <- pedigree(
  id=id, 
  dadid=id1, 
  momid=id2,
  sex=colony$Animal_Gender)
plot(pedAll, cex=.2, mar=c(6,1,6,1))

#Computing the number of offspring for each individual
branches <- data.frame(id, id1, id2)
ord <- orderPed(branches)
df <-data.frame(ord)
branches$order=df
pedf <- branches[with(branches, order(ord)), ]

#Computing the  Generation Number for each individual in a data frame
Generation <- kindepth(pedf$id,pedf$id1, pedf$id2)
GenerationN <- data.frame(Generation)
rownames(GenerationN) = pedf$id

#Computing the Inbreeding factor for each individual
Inbreed_Factor <- calcInbreeding(pedf)
Inbreed <- data.frame(Inbreed_Factor)
rownames(Inbreed) = pedf$id
print(Inbreed)

to_be_exported <- cbind(Inbreed,Generation)
write.csv2(to_be_exported, file = "Data_Colony.csv")
pdf(file="Colony_Tree.pdf",width=50,height=20)
plot(pedAll)
dev.off()