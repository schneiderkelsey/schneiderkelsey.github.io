pesticide <- read.table("data_in/pesticide201314.csv",sep=",",header=T,fill=T)
pesticide2015 <- read.table("data_in/pesticide2015.csv",sep=",",header=T,fill=T)


deadbees <- read.table("data_in/deadbees.csv",sep=",",header=T,fill=T)

deadbees <- deadbees[,1:14]
deadbees$julian <- deadbees$julian+31+28+31+25

n <- dim(deadbees)
deadbees$order <- c(1:n[1])

# 05-25-18
test <- tapply(deadbees$deadperday,deadbees$hive,max)
test2 <- unique(deadbees$hive)
test3 <- data.frame(hive=test2[-75],maxdeadperday=test[-1])
test3$hive[41:45] <- test3$hive[c(43,44,41,42)]
test3$hive[45:50] <- test3$hive[c(47,46,48,49,50,46)]
test3$hive[46] <- "2015HR5"

test3$hive[58:59] <- test3$hive[c(59,58)]
test3$hive[63:65] <- test3$hive[c(65,63,64)]

# 05-25-18 2015FSR8-27 wrong order

test4 <-  merge(deadbees,test3,by.x=3,by.y=1,all.x=T)

deadbees <- test4
deadbees$mortality2 <- deadbees$deadperday/deadbees$maxdeadperday

test5 <- order(deadbees$order)
deadbees <- deadbees[test5,]

# 05-24-18 2014 CH31, CH32 shifted later than 2013
# 05-24-18 2013 CH35 shifted later than 2013 CH31, CH32

# 05-24-18 colony type package-nuc 2014 CH33*, CH34*,FSR23*,FSR24* have low mortality
# 05-24-18 colony type package 2013 WB15, WB16* has low mortality

par(mfrow=c(4,1),mai=c(.5,1,0,0))
#plot(deadbees$julian[deadbees$hive=="2013CH31"],deadbees$deadperday[deadbees$hive=="2013CH31"],type="b",xlim=c(110,180),ylim=c(0,90))
#points(deadbees$julian[deadbees$hive=="2014CH31"],deadbees$deadperday[deadbees$hive=="2014CH31"],type="b",col="red")
#points(deadbees$julian[deadbees$hive=="2013CH32"],deadbees$deadperday[deadbees$hive=="2013CH32"],type="b",col="red")
#points(deadbees$julian[deadbees$hive=="2013CH35"],deadbees$deadperday[deadbees$hive=="2013CH35"],type="b",col="blue")
#points(deadbees$julian[deadbees$hive=="2013CH36"],deadbees$deadperday[deadbees$hive=="2013CH36"],type="b",col="green")

#abline(v=31+28+31+30+5)
#abline(v=31+28+31+30+16)

#plot(deadbees$julian[deadbees$hive=="2013CH31"],deadbees$mortality[deadbees$hive=="2013CH31"],type="b",xlim=c(110,180),ylim=c(-2,3))
#points(deadbees$julian[deadbees$hive=="2014CH31"],deadbees$mortality[deadbees$hive=="2014CH31"],type="b",col="red")
#points(deadbees$julian[deadbees$hive=="2013CH32"],deadbees$mortality[deadbees$hive=="2013CH32"],type="b",col="red")
#points(deadbees$julian[deadbees$hive=="2013CH35"],deadbees$mortality[deadbees$hive=="2013CH35"],type="b",col="blue")
#points(deadbees$julian[deadbees$hive=="2013CH36"],deadbees$mortality[deadbees$hive=="2013CH36"],type="b",col="green")

plot(deadbees$julian[deadbees$hive=="2013CH31"],deadbees$mortality2[deadbees$hive=="2013CH31"],type="b",xlim=c(110,180),ylim=c(0,1))
#points(deadbees$julian[deadbees$hive=="2014CH31"],deadbees$mortality2[deadbees$hive=="2014CH31"],type="b",col="red")
points(deadbees$julian[deadbees$hive=="2013CH32"],deadbees$mortality2[deadbees$hive=="2013CH32"],type="b",col="red")
points(deadbees$julian[deadbees$hive=="2013CH35"],deadbees$mortality2[deadbees$hive=="2013CH35"],type="b",col="blue")
points(deadbees$julian[deadbees$hive=="2013CH36"],deadbees$mortality2[deadbees$hive=="2013CH36"],type="b",col="green")

abline(v=31+28+31+30+5)
abline(v=31+28+31+30+16)

plot(pesticide$julian[pesticide$year==2013&pesticide$Site=="CH"],pesticide$clothianidin[pesticide$year==2013&pesticide$Site=="CH"],xlim=c(110,180),type="p",pch=16)

abline(v=31+28+31+30+5)
abline(v=31+28+31+30+16)

plot(pesticide$julian[pesticide$year==2013&pesticide$Site=="CH"],pesticide$imidacloprid[pesticide$year==2013&pesticide$Site=="CH"],xlim=c(110,180),type="p",pch=16)

abline(v=31+28+31+30+5)
abline(v=31+28+31+30+16)

plot(pesticide$julian[pesticide$year==2013&pesticide$Site=="CH"],pesticide$thiamethoxam[pesticide$year==2013&pesticide$Site=="CH"],xlim=c(110,180),type="p",pch=16)

abline(v=31+28+31+30+5)
abline(v=31+28+31+30+16)

par(mfrow=c(4,1),mai=c(.5,1,0,0))
#plot(deadbees$julian[deadbees$hive=="2014CH31"],deadbees$deadperday[deadbees$hive=="2014CH31"],type="b",xlim=c(110,180),ylim=c(0,150))
#points(deadbees$julian[deadbees$hive=="2014CH31"],deadbees$deadperday[deadbees$hive=="2014CH31"],type="b",col="red")
#points(deadbees$julian[deadbees$hive=="2014CH32"],deadbees$deadperday[deadbees$hive=="2014CH32"],type="b",col="red")
#points(deadbees$julian[deadbees$hive=="2014CH33"],deadbees$deadperday[deadbees$hive=="2014CH33"],type="b",col="brown")
#points(deadbees$julian[deadbees$hive=="2014CH34"],deadbees$deadperday[deadbees$hive=="2014CH34"],type="b",col="orange")

#abline(v=31+28+31+30+5)
#abline(v=31+28+31+30+10)
#abline(v=31+28+31+30+16)
#plot(deadbees$julian[deadbees$hive=="2014CH31"],deadbees$mortality[deadbees$hive=="2014CH31"],type="b",xlim=c(110,180),ylim=c(-2,4))
#points(deadbees$julian[deadbees$hive=="2014CH31"],deadbees$mortality[deadbees$hive=="2014CH31"],type="b",col="red")
#points(deadbees$julian[deadbees$hive=="2014CH32"],deadbees$mortality[deadbees$hive=="2014CH32"],type="b",col="red")
#points(deadbees$julian[deadbees$hive=="2014CH33"],deadbees$mortality[deadbees$hive=="2014CH33"],type="b",col="brown")
#points(deadbees$julian[deadbees$hive=="2014CH34"],deadbees$mortality[deadbees$hive=="2014CH34"],type="b",col="orange")

#abline(v=31+28+31+30+5)
#abline(v=31+28+31+30+10)
#abline(v=31+28+31+30+16)
plot(deadbees$julian[deadbees$hive=="2014CH31"],deadbees$mortality2[deadbees$hive=="2014CH31"],type="b",xlim=c(110,180),ylim=c(0,1))
#points(deadbees$julian[deadbees$hive=="2014CH31"],deadbees$mortality2[deadbees$hive=="2014CH31"],type="b",col="red")
points(deadbees$julian[deadbees$hive=="2014CH32"],deadbees$mortality2[deadbees$hive=="2014CH32"],type="b",col="red")
points(deadbees$julian[deadbees$hive=="2014CH33"],deadbees$mortality2[deadbees$hive=="2014CH33"],type="b",col="brown")
points(deadbees$julian[deadbees$hive=="2014CH34"],deadbees$mortality2[deadbees$hive=="2014CH34"],type="b",col="orange")

abline(v=31+28+31+30+5)
abline(v=31+28+31+30+10)
abline(v=31+28+31+30+16)

plot(pesticide$julian[pesticide$year==2014&pesticide$Site=="CH"],pesticide$clothianidin[pesticide$year==2014&pesticide$Site=="CH"],xlim=c(110,180),type="p",pch=16)

abline(v=31+28+31+30+5)
abline(v=31+28+31+30+10)
abline(v=31+28+31+30+16)

plot(pesticide$julian[pesticide$year==2014&pesticide$Site=="CH"],pesticide$imidacloprid[pesticide$year==2014&pesticide$Site=="CH"],xlim=c(110,180),type="p",pch=16)

abline(v=31+28+31+30+5)
abline(v=31+28+31+30+10)
abline(v=31+28+31+30+16)

plot(pesticide$julian[pesticide$year==2014&pesticide$Site=="CH"],pesticide$thiamethoxam[pesticide$year==2014&pesticide$Site=="CH"],xlim=c(110,180),type="p",pch=16)

abline(v=31+28+31+30+5)
abline(v=31+28+31+30+10)
abline(v=31+28+31+30+16)

# 04-25-18 mortality index overinflates 2013FSR25, 2013FSR26, and understates 2013FSR21, 2013FSR22

par(mfrow=c(4,1),mai=c(.5,1,0,0))

#plot(deadbees$julian[deadbees$hive=="2013FSR21"],deadbees$deadperday[deadbees$hive=="2013FSR21"],type="b",xlim=c(110,180),ylim=c(0,400))
#points(deadbees$julian[deadbees$hive=="2014FSR21"],deadbees$deadperday[deadbees$hive=="2014FSR21"],type="b",col="red")
#points(deadbees$julian[deadbees$hive=="2013FSR22"],deadbees$deadperday[deadbees$hive=="2013FSR22"],type="b",col="red")
#points(deadbees$julian[deadbees$hive=="2013FSR25"],deadbees$deadperday[deadbees$hive=="2013FSR25"],type="b",col="blue")
#points(deadbees$julian[deadbees$hive=="2013FSR26"],deadbees$deadperday[deadbees$hive=="2013FSR26"],type="b",col="green")

#abline(v=31+28+31+30+5)
#abline(v=31+28+31+30+16)

#plot(deadbees$julian[deadbees$hive=="2013FSR21"],deadbees$mortality[deadbees$hive=="2013FSR21"],type="b",xlim=c(110,180),ylim=c(-2,4))
#points(deadbees$julian[deadbees$hive=="2014FSR21"],deadbees$mortality[deadbees$hive=="2014FSR21"],type="b",col="red")
#points(deadbees$julian[deadbees$hive=="2013FSR22"],deadbees$mortality[deadbees$hive=="2013FSR22"],type="b",col="red")
#points(deadbees$julian[deadbees$hive=="2013FSR25"],deadbees$mortality[deadbees$hive=="2013FSR25"],type="b",col="blue")
#points(deadbees$julian[deadbees$hive=="2013FSR26"],deadbees$mortality[deadbees$hive=="2013FSR26"],type="b",col="green")

#abline(v=31+28+31+30+5)
#abline(v=31+28+31+30+16)

plot(deadbees$julian[deadbees$hive=="2013FSR21"],deadbees$mortality2[deadbees$hive=="2013FSR21"],type="b",xlim=c(110,180),ylim=c(0,1))
#points(deadbees$julian[deadbees$hive=="2014FSR21"],deadbees$mortality2[deadbees$hive=="2014FSR21"],type="b",col="red")
points(deadbees$julian[deadbees$hive=="2013FSR22"],deadbees$mortality2[deadbees$hive=="2013FSR22"],type="b",col="red")
points(deadbees$julian[deadbees$hive=="2013FSR25"],deadbees$mortality2[deadbees$hive=="2013FSR25"],type="b",col="blue")
points(deadbees$julian[deadbees$hive=="2013FSR26"],deadbees$mortality2[deadbees$hive=="2013FSR26"],type="b",col="green")

abline(v=31+28+31+30+5)
abline(v=31+28+31+30+16)

plot(pesticide$julian[pesticide$year==2013&pesticide$Site=="FSR"],pesticide$clothianidin[pesticide$year==2013&pesticide$Site=="FSR"],xlim=c(110,180),type="p",pch=16)

abline(v=31+28+31+30+5)
abline(v=31+28+31+30+16)

plot(pesticide$julian[pesticide$year==2013&pesticide$Site=="FSR"],pesticide$imidacloprid[pesticide$year==2013&pesticide$Site=="FSR"],xlim=c(110,180),type="p",pch=16)

abline(v=31+28+31+30+5)
abline(v=31+28+31+30+16)

plot(pesticide$julian[pesticide$year==2013&pesticide$Site=="FSR"],pesticide$thiamethoxam[pesticide$year==2013&pesticide$Site=="FSR"],xlim=c(110,180),type="p",pch=16)

abline(v=31+28+31+30+5)
abline(v=31+28+31+30+16)

par(mfrow=c(4,1),mai=c(.5,1,0,0))

#plot(deadbees$julian[deadbees$hive=="2013WB11"],deadbees$deadperday[deadbees$hive=="2013WB11"],type="b",xlim=c(110,180),ylim=c(0,60))
#points(deadbees$julian[deadbees$hive=="2014WB11"],deadbees$deadperday[deadbees$hive=="2014WB11"],type="b",col="red")
#points(deadbees$julian[deadbees$hive=="2013WB12"],deadbees$deadperday[deadbees$hive=="2013WB12"],type="b",col="red")
#points(deadbees$julian[deadbees$hive=="2013WB15"],deadbees$deadperday[deadbees$hive=="2013WB15"],type="b",col="blue")
#points(deadbees$julian[deadbees$hive=="2013WB16"],deadbees$deadperday[deadbees$hive=="2013WB16"],type="b",col="green")

#abline(v=31+28+31+30+5)
#abline(v=31+28+31+30+16)

#plot(deadbees$julian[deadbees$hive=="2013WB11"],deadbees$mortality[deadbees$hive=="2013WB11"],type="b",xlim=c(110,180),ylim=c(-2,4))
#points(deadbees$julian[deadbees$hive=="2014WB11"],deadbees$mortality[deadbees$hive=="2014WB11"],type="b",col="red")
#points(deadbees$julian[deadbees$hive=="2013WB12"],deadbees$mortality[deadbees$hive=="2013WB12"],type="b",col="red")
#points(deadbees$julian[deadbees$hive=="2013WB15"],deadbees$mortality[deadbees$hive=="2013WB15"],type="b",col="blue")
#points(deadbees$julian[deadbees$hive=="2013WB16"],deadbees$mortality[deadbees$hive=="2013WB16"],type="b",col="green")

#abline(v=31+28+31+30+5)
#abline(v=31+28+31+30+16)

plot(deadbees$julian[deadbees$hive=="2013WB11"],deadbees$mortality2[deadbees$hive=="2013WB11"],type="b",xlim=c(110,180),ylim=c(0,1))
#points(deadbees$julian[deadbees$hive=="2014WB11"],deadbees$mortality2[deadbees$hive=="2014WB11"],type="b",col="red")
points(deadbees$julian[deadbees$hive=="2013WB12"],deadbees$mortality2[deadbees$hive=="2013WB12"],type="b",col="red")
points(deadbees$julian[deadbees$hive=="2013WB15"],deadbees$mortality2[deadbees$hive=="2013WB15"],type="b",col="blue")
points(deadbees$julian[deadbees$hive=="2013WB16"],deadbees$mortality2[deadbees$hive=="2013WB16"],type="b",col="green")

abline(v=31+28+31+30+5)
abline(v=31+28+31+30+16)

plot(pesticide$julian[pesticide$year==2013&pesticide$Site=="WB"],pesticide$clothianidin[pesticide$year==2013&pesticide$Site=="WB"],xlim=c(110,180),type="p",pch=16)

abline(v=31+28+31+30+5)
abline(v=31+28+31+30+16)

plot(pesticide$julian[pesticide$year==2013&pesticide$Site=="WB"],pesticide$imidacloprid[pesticide$year==2013&pesticide$Site=="WB"],xlim=c(110,180),type="p",pch=16)

abline(v=31+28+31+30+5)
abline(v=31+28+31+30+16)

plot(pesticide$julian[pesticide$year==2013&pesticide$Site=="WB"],pesticide$thiamethoxam[pesticide$year==2013&pesticide$Site=="WB"],xlim=c(110,180),type="p",pch=16)

abline(v=31+28+31+30+5)
abline(v=31+28+31+30+16)

par(mfrow=c(4,1),mai=c(.5,1,0,0))
#plot(deadbees$julian[deadbees$hive=="2014FSR21"],deadbees$deadperday[deadbees$hive=="2014FSR21"],type="b",xlim=c(110,180),ylim=c(0,220))
#points(deadbees$julian[deadbees$hive=="2014FSR21"],deadbees$deadperday[deadbees$hive=="2014FSR21"],type="b",col="red")
#points(deadbees$julian[deadbees$hive=="2014FSR22"],deadbees$deadperday[deadbees$hive=="2014FSR22"],type="b",col="red")
#points(deadbees$julian[deadbees$hive=="2014FSR23"],deadbees$deadperday[deadbees$hive=="2014FSR23"],type="b",col="brown")
#points(deadbees$julian[deadbees$hive=="2014FSR24"],deadbees$deadperday[deadbees$hive=="2014FSR24"],type="b",col="orange")

#abline(v=31+28+31+30+5)
#abline(v=31+28+31+30+10)
#abline(v=31+28+31+30+16)

#plot(deadbees$julian[deadbees$hive=="2014FSR21"],deadbees$mortality[deadbees$hive=="2014FSR21"],type="b",xlim=c(110,180))
#points(deadbees$julian[deadbees$hive=="2014FSR21"],deadbees$mortality[deadbees$hive=="2014FSR21"],type="b",col="red")
#points(deadbees$julian[deadbees$hive=="2014FSR22"],deadbees$mortality[deadbees$hive=="2014FSR22"],type="b",col="red")
#points(deadbees$julian[deadbees$hive=="2014FSR23"],deadbees$mortality[deadbees$hive=="2014FSR23"],type="b",col="brown")
#points(deadbees$julian[deadbees$hive=="2014FSR24"],deadbees$mortality[deadbees$hive=="2014FSR24"],type="b",col="orange")

#abline(v=31+28+31+30+5)
#abline(v=31+28+31+30+10)
#abline(v=31+28+31+30+16)

plot(deadbees$julian[deadbees$hive=="2014FSR21"],deadbees$mortality2[deadbees$hive=="2014FSR21"],type="b",xlim=c(110,180),ylim=c(0,1))
#points(deadbees$julian[deadbees$hive=="2014FSR21"],deadbees$mortality2[deadbees$hive=="2014FSR21"],type="b",col="red")
points(deadbees$julian[deadbees$hive=="2014FSR22"],deadbees$mortality2[deadbees$hive=="2014FSR22"],type="b",col="red")
points(deadbees$julian[deadbees$hive=="2014FSR23"],deadbees$mortality2[deadbees$hive=="2014FSR23"],type="b",col="brown")
points(deadbees$julian[deadbees$hive=="2014FSR24"],deadbees$mortality2[deadbees$hive=="2014FSR24"],type="b",col="orange")

abline(v=31+28+31+30+5)
abline(v=31+28+31+30+10)
abline(v=31+28+31+30+16)

plot(pesticide$julian[pesticide$year==2014&pesticide$Site=="FSR"],pesticide$clothianidin[pesticide$year==2014&pesticide$Site=="FSR"],xlim=c(110,180),type="p",pch=16)

abline(v=31+28+31+30+5)
abline(v=31+28+31+30+10)
abline(v=31+28+31+30+16)

plot(pesticide$julian[pesticide$year==2014&pesticide$Site=="FSR"],pesticide$imidacloprid[pesticide$year==2014&pesticide$Site=="FSR"],xlim=c(110,180),type="p",pch=16)

abline(v=31+28+31+30+5)
abline(v=31+28+31+30+10)
abline(v=31+28+31+30+16)

plot(pesticide$julian[pesticide$year==2014&pesticide$Site=="FSR"],pesticide$thiamethoxam[pesticide$year==2014&pesticide$Site=="FSR"],xlim=c(110,180),type="p",pch=16)

abline(v=31+28+31+30+5)
abline(v=31+28+31+30+10)
abline(v=31+28+31+30+16)

# 05-25-18 2014EP61 is way overinflated by mortality index

par(mfrow=c(3,1),mai=c(.5,1,0,0))
plot(deadbees$julian[deadbees$hive=="2014EP61"],deadbees$deadperday[deadbees$hive=="2014EP61"],type="b",xlim=c(110,180),ylim=c(0,220))
#points(deadbees$julian[deadbees$hive=="2014EP61"],deadbees$deadperday[deadbees$hive=="2014EP61"],type="b",col="red")
points(deadbees$julian[deadbees$hive=="2014EP62"],deadbees$deadperday[deadbees$hive=="2014EP62"],type="b",col="red")
points(deadbees$julian[deadbees$hive=="2014EP63"],deadbees$deadperday[deadbees$hive=="2014EP63"],type="b",col="blue")
points(deadbees$julian[deadbees$hive=="2014EP64"],deadbees$deadperday[deadbees$hive=="2014EP64"],type="b",col="green")

abline(v=31+28+31+30+5)
abline(v=31+28+31+30+10)
abline(v=31+28+31+30+16)

plot(deadbees$julian[deadbees$hive=="2014EP61"],deadbees$mortality[deadbees$hive=="2014EP61"],type="b",xlim=c(110,180))
#points(deadbees$julian[deadbees$hive=="2014EP61"],deadbees$mortality[deadbees$hive=="2014EP61"],type="b",col="red")
points(deadbees$julian[deadbees$hive=="2014EP62"],deadbees$mortality[deadbees$hive=="2014EP62"],type="b",col="red")
points(deadbees$julian[deadbees$hive=="2014EP63"],deadbees$mortality[deadbees$hive=="2014EP63"],type="b",col="blue")
points(deadbees$julian[deadbees$hive=="2014EP64"],deadbees$mortality[deadbees$hive=="2014EP64"],type="b",col="green")

abline(v=31+28+31+30+5)
abline(v=31+28+31+30+10)
abline(v=31+28+31+30+16)

plot(deadbees$julian[deadbees$hive=="2014EP61"],deadbees$mortality2[deadbees$hive=="2014EP61"],type="b",xlim=c(110,180),ylim=c(0,1))
#points(deadbees$julian[deadbees$hive=="2014EP61"],deadbees$mortality2[deadbees$hive=="2014EP61"],type="b",col="red")
points(deadbees$julian[deadbees$hive=="2014EP62"],deadbees$mortality2[deadbees$hive=="2014EP62"],type="b",col="red")
points(deadbees$julian[deadbees$hive=="2014EP63"],deadbees$mortality2[deadbees$hive=="2014EP63"],type="b",col="blue")
points(deadbees$julian[deadbees$hive=="2014EP64"],deadbees$mortality2[deadbees$hive=="2014EP64"],type="b",col="green")

abline(v=31+28+31+30+5)
abline(v=31+28+31+30+10)
abline(v=31+28+31+30+16)

# 05-25-18 2014MB51 is overinflated by mortality index

par(mfrow=c(4,1),mai=c(.5,1,0,0))
#plot(deadbees$julian[deadbees$hive=="2014MB51"],deadbees$deadperday[deadbees$hive=="2014MB51"],type="b",xlim=c(110,180),ylim=c(0,220))
#points(deadbees$julian[deadbees$hive=="2014MB51"],deadbees$deadperday[deadbees$hive=="2014MB51"],type="b",col="red")
#points(deadbees$julian[deadbees$hive=="2014MB52"],deadbees$deadperday[deadbees$hive=="2014MB52"],type="b",col="red")
#points(deadbees$julian[deadbees$hive=="2014MB53"],deadbees$deadperday[deadbees$hive=="2014MB53"],type="b",col="blue")
#points(deadbees$julian[deadbees$hive=="2014MB54"],deadbees$deadperday[deadbees$hive=="2014MB54"],type="b",col="green")

#abline(v=31+28+31+30+5)
#abline(v=31+28+31+30+10)
#abline(v=31+28+31+30+16)

#plot(deadbees$julian[deadbees$hive=="2014MB51"],deadbees$mortality[deadbees$hive=="2014MB51"],type="b",xlim=c(110,180))
#points(deadbees$julian[deadbees$hive=="2014MB51"],deadbees$mortality[deadbees$hive=="2014MB51"],type="b",col="red")
#points(deadbees$julian[deadbees$hive=="2014MB52"],deadbees$mortality[deadbees$hive=="2014MB52"],type="b",col="red")
#points(deadbees$julian[deadbees$hive=="2014MB53"],deadbees$mortality[deadbees$hive=="2014MB53"],type="b",col="blue")
#points(deadbees$julian[deadbees$hive=="2014MB54"],deadbees$mortality[deadbees$hive=="2014MB54"],type="b",col="green")

#abline(v=31+28+31+30+5)
#abline(v=31+28+31+30+10)
#abline(v=31+28+31+30+16)

plot(deadbees$julian[deadbees$hive=="2014MB51"],deadbees$mortality2[deadbees$hive=="2014MB51"],type="b",xlim=c(110,180),ylim=c(0,1))
#points(deadbees$julian[deadbees$hive=="2014MB51"],deadbees$mortality2[deadbees$hive=="2014MB51"],type="b",col="red")
points(deadbees$julian[deadbees$hive=="2014MB52"],deadbees$mortality2[deadbees$hive=="2014MB52"],type="b",col="red")
points(deadbees$julian[deadbees$hive=="2014MB53"],deadbees$mortality2[deadbees$hive=="2014MB53"],type="b",col="blue")
points(deadbees$julian[deadbees$hive=="2014MB54"],deadbees$mortality2[deadbees$hive=="2014MB54"],type="b",col="green")

abline(v=31+28+31+30+5)
abline(v=31+28+31+30+10)
abline(v=31+28+31+30+16)

plot(pesticide$julian[pesticide$year==2014&pesticide$Site=="MB"],pesticide$clothianidin[pesticide$year==2014&pesticide$Site=="MB"],xlim=c(110,180),type="p",pch=16)

abline(v=31+28+31+30+5)
abline(v=31+28+31+30+10)
abline(v=31+28+31+30+16)

plot(pesticide$julian[pesticide$year==2014&pesticide$Site=="MB"],pesticide$imidacloprid[pesticide$year==2014&pesticide$Site=="MB"],xlim=c(110,180),type="p",pch=16)

abline(v=31+28+31+30+5)
abline(v=31+28+31+30+10)
abline(v=31+28+31+30+16)

plot(pesticide$julian[pesticide$year==2014&pesticide$Site=="MB"],pesticide$thiamethoxam[pesticide$year==2014&pesticide$Site=="MB"],xlim=c(110,180),type="p",pch=16)

abline(v=31+28+31+30+5)
abline(v=31+28+31+30+10)
abline(v=31+28+31+30+16)

par(mfrow=c(3,1),mai=c(.5,1,0,0))
plot(deadbees$julian[deadbees$hive=="2014TD71"],deadbees$deadperday[deadbees$hive=="2014TD71"],type="b",xlim=c(110,180),ylim=c(0,220))
#points(deadbees$julian[deadbees$hive=="2014TD71"],deadbees$deadperday[deadbees$hive=="2014TD71"],type="b",col="red")
points(deadbees$julian[deadbees$hive=="2014TD72"],deadbees$deadperday[deadbees$hive=="2014TD72"],type="b",col="red")
points(deadbees$julian[deadbees$hive=="2014TD73"],deadbees$deadperday[deadbees$hive=="2014TD73"],type="b",col="blue")
points(deadbees$julian[deadbees$hive=="2014TD74"],deadbees$deadperday[deadbees$hive=="2014TD74"],type="b",col="green")

abline(v=31+28+31+30+5)
abline(v=31+28+31+30+10)
abline(v=31+28+31+30+16)

plot(deadbees$julian[deadbees$hive=="2014TD71"],deadbees$mortality[deadbees$hive=="2014TD71"],type="b",xlim=c(110,180),ylim=c(-1,2.5))
#points(deadbees$julian[deadbees$hive=="2014TD71"],deadbees$mortality[deadbees$hive=="2014TD71"],type="b",col="red")
points(deadbees$julian[deadbees$hive=="2014TD72"],deadbees$mortality[deadbees$hive=="2014TD72"],type="b",col="red")
points(deadbees$julian[deadbees$hive=="2014TD73"],deadbees$mortality[deadbees$hive=="2014TD73"],type="b",col="blue")
points(deadbees$julian[deadbees$hive=="2014TD74"],deadbees$mortality[deadbees$hive=="2014TD74"],type="b",col="green")

abline(v=31+28+31+30+5)
abline(v=31+28+31+30+10)
abline(v=31+28+31+30+16)

plot(deadbees$julian[deadbees$hive=="2014TD71"],deadbees$mortality2[deadbees$hive=="2014TD71"],type="b",xlim=c(110,180),ylim=c(0,1))
#points(deadbees$julian[deadbees$hive=="2014TD71"],deadbees$mortality2[deadbees$hive=="2014TD71"],type="b",col="red")
points(deadbees$julian[deadbees$hive=="2014TD72"],deadbees$mortality2[deadbees$hive=="2014TD72"],type="b",col="red")
points(deadbees$julian[deadbees$hive=="2014TD73"],deadbees$mortality2[deadbees$hive=="2014TD73"],type="b",col="blue")
points(deadbees$julian[deadbees$hive=="2014TD74"],deadbees$mortality2[deadbees$hive=="2014TD74"],type="b",col="green")

abline(v=31+28+31+30+5)
abline(v=31+28+31+30+10)
abline(v=31+28+31+30+16)

par(mfrow=c(4,1),mai=c(.5,1,0,0))

#plot(deadbees$julian[deadbees$hive=="2014WB11"],deadbees$deadperday[deadbees$hive=="2014WB11"],type="b",xlim=c(110,180),ylim=c(0,160))
#points(deadbees$julian[deadbees$hive=="2014WB11"],deadbees$deadperday[deadbees$hive=="2014WB11"],type="b",col="red")
#points(deadbees$julian[deadbees$hive=="2014WB12"],deadbees$deadperday[deadbees$hive=="2014WB12"],type="b",col="red")
#points(deadbees$julian[deadbees$hive=="2014WB13"],deadbees$deadperday[deadbees$hive=="2014WB13"],type="b",col="blue")
#points(deadbees$julian[deadbees$hive=="2014WB14"],deadbees$deadperday[deadbees$hive=="2014WB14"],type="b",col="green")

#abline(v=31+28+31+30+5)
#abline(v=31+28+31+30+10)
#abline(v=31+28+31+30+16)

#plot(deadbees$julian[deadbees$hive=="2014WB11"],deadbees$mortality[deadbees$hive=="2014WB11"],type="b",xlim=c(110,180),ylim=c(-2,4))
#points(deadbees$julian[deadbees$hive=="2014WB11"],deadbees$mortality[deadbees$hive=="2014WB11"],type="b",col="red")
#points(deadbees$julian[deadbees$hive=="2014WB12"],deadbees$mortality[deadbees$hive=="2014WB12"],type="b",col="red")
#points(deadbees$julian[deadbees$hive=="2014WB13"],deadbees$mortality[deadbees$hive=="2014WB13"],type="b",col="blue")
#points(deadbees$julian[deadbees$hive=="2014WB14"],deadbees$mortality[deadbees$hive=="2014WB14"],type="b",col="green")

#abline(v=31+28+31+30+5)
#abline(v=31+28+31+30+10)
#abline(v=31+28+31+30+16)

plot(deadbees$julian[deadbees$hive=="2014WB11"],deadbees$mortality2[deadbees$hive=="2014WB11"],type="b",xlim=c(110,180),ylim=c(0,1))
#points(deadbees$julian[deadbees$hive=="2014WB11"],deadbees$mortality2[deadbees$hive=="2014WB11"],type="b",col="red")
points(deadbees$julian[deadbees$hive=="2014WB12"],deadbees$mortality2[deadbees$hive=="2014WB12"],type="b",col="red")
points(deadbees$julian[deadbees$hive=="2014WB13"],deadbees$mortality2[deadbees$hive=="2014WB13"],type="b",col="blue")
points(deadbees$julian[deadbees$hive=="2014WB14"],deadbees$mortality2[deadbees$hive=="2014WB14"],type="b",col="green")

abline(v=31+28+31+30+5)
abline(v=31+28+31+30+10)
abline(v=31+28+31+30+16)

plot(pesticide$julian[pesticide$year==2014&pesticide$Site=="WB"],pesticide$clothianidin[pesticide$year==2014&pesticide$Site=="WB"],xlim=c(110,180),type="p",pch=16)

abline(v=31+28+31+30+5)
abline(v=31+28+31+30+10)
abline(v=31+28+31+30+16)

plot(pesticide$julian[pesticide$year==2014&pesticide$Site=="WB"],pesticide$imidacloprid[pesticide$year==2014&pesticide$Site=="WB"],xlim=c(110,180),type="p",pch=16)

abline(v=31+28+31+30+5)
abline(v=31+28+31+30+10)
abline(v=31+28+31+30+16)

plot(pesticide$julian[pesticide$year==2014&pesticide$Site=="WB"],pesticide$thiamethoxam[pesticide$year==2014&pesticide$Site=="WB"],xlim=c(110,180),type="p",pch=16)

abline(v=31+28+31+30+5)
abline(v=31+28+31+30+10)
abline(v=31+28+31+30+16)

par(mfrow=c(8,1),mai=c(.5,1,0,0))

#plot(deadbees$julian[deadbees$hive=="2015BG59"],deadbees$deadperday[deadbees$hive=="2015BG59"],type="b",xlim=c(110,180),ylim=c(0,160))
#points(deadbees$julian[deadbees$hive=="2015BG59"],deadbees$deadperday[deadbees$hive=="2015BG59"],type="b",col="red")
#points(deadbees$julian[deadbees$hive=="2015BG60"],deadbees$deadperday[deadbees$hive=="2015BG60"],type="b",col="red")

#abline(v=31+28+31+30+2)
#abline(v=31+28+31+30+8)

#plot(deadbees$julian[deadbees$hive=="2015BG59"],deadbees$mortality[deadbees$hive=="2015BG59"],type="b",xlim=c(110,180),ylim=c(-2,4))
#points(deadbees$julian[deadbees$hive=="2015BG59"],deadbees$mortality[deadbees$hive=="2015BG59"],type="b",col="red")
#points(deadbees$julian[deadbees$hive=="2015BG60"],deadbees$mortality[deadbees$hive=="2015BG60"],type="b",col="red")

#abline(v=31+28+31+30+2)
#abline(v=31+28+31+30+8)

plot(deadbees$julian[deadbees$hive=="2015BG59"],deadbees$mortality2[deadbees$hive=="2015BG59"],type="b",xlim=c(110,180),ylim=c(0,1))
#points(deadbees$julian[deadbees$hive=="2015BG59"],deadbees$mortality2[deadbees$hive=="2015BG59"],type="b",col="red")
points(deadbees$julian[deadbees$hive=="2015BG60"],deadbees$mortality2[deadbees$hive=="2015BG60"],type="b",col="red")

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(pesticide2015$julian[pesticide2015$site=="BG"],pesticide2015$clothianidin[pesticide2015$site=="BG"],xlim=c(110,180),type="p",pch=16,col="red")

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(pesticide2015$julian[pesticide2015$site=="BG"],pesticide2015$thiamethoxam[pesticide2015$site=="BG"],xlim=c(110,180),type="p",pch=16,col="red")

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(pesticide2015$julian[pesticide2015$site=="BG"],pesticide2015$nitenpyram[pesticide2015$site=="BG"],xlim=c(110,180),type="p",pch=16,col="red")

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(pesticide2015$julian[pesticide2015$site=="BG"],pesticide2015$imidacloprid[pesticide2015$site=="BG"],xlim=c(110,180),type="p",pch=16,col="red")

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(pesticide2015$julian[pesticide2015$site=="BG"],pesticide2015$dinotefuran[pesticide2015$site=="BG"],xlim=c(110,180),type="p",pch=16)

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)



plot(pesticide2015$julian[pesticide2015$site=="BG"],pesticide2015$acetamiprid[pesticide2015$site=="BG"],xlim=c(110,180),type="p",pch=16,col="red")

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(pesticide2015$julian[pesticide2015$site=="BG"],pesticide2015$thiacloprid[pesticide2015$site=="BG"],xlim=c(110,180),type="p",pch=16,col="red")

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

par(mfrow=c(8,1),mai=c(.5,1,0,0))

#plot(deadbees$julian[deadbees$hive=="2015DS31"],deadbees$deadperday[deadbees$hive=="2015DS31"],type="b",xlim=c(110,180),ylim=c(0,160))
#points(deadbees$julian[deadbees$hive=="2015DS31"],deadbees$deadperday[deadbees$hive=="2015DS31"],type="b",col="red")
#points(deadbees$julian[deadbees$hive=="2015DS33"],deadbees$deadperday[deadbees$hive=="2015DS33"],type="b",col="red")

#abline(v=31+28+31+30+2)
#abline(v=31+28+31+30+8)

#plot(deadbees$julian[deadbees$hive=="2015DS31"],deadbees$mortality[deadbees$hive=="2015DS31"],type="b",xlim=c(110,180),ylim=c(-2,4))
#points(deadbees$julian[deadbees$hive=="2015DS31"],deadbees$mortality[deadbees$hive=="2015DS31"],type="b",col="red")
#points(deadbees$julian[deadbees$hive=="2015DS33"],deadbees$mortality[deadbees$hive=="2015DS33"],type="b",col="red")

#abline(v=31+28+31+30+2)
#abline(v=31+28+31+30+8)

plot(deadbees$julian[deadbees$hive=="2015DS31"],deadbees$mortality2[deadbees$hive=="2015DS31"],type="b",xlim=c(110,180),ylim=c(0,1))
#points(deadbees$julian[deadbees$hive=="2015DS31"],deadbees$mortality2[deadbees$hive=="2015DS31"],type="b",col="red")
points(deadbees$julian[deadbees$hive=="2015DS33"],deadbees$mortality2[deadbees$hive=="2015DS33"],type="b",col="red")

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(pesticide2015$julian[pesticide2015$site=="DS"],pesticide2015$clothianidin[pesticide2015$site=="DS"],xlim=c(110,180),type="p",pch=16,col="red")

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(pesticide2015$julian[pesticide2015$site=="DS"],pesticide2015$thiamethoxam[pesticide2015$site=="DS"],xlim=c(110,180),type="p",pch=16,col="red")

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(pesticide2015$julian[pesticide2015$site=="DS"],pesticide2015$nitenpyram[pesticide2015$site=="DS"],xlim=c(110,180),type="p",pch=16,col="red")

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(pesticide2015$julian[pesticide2015$site=="DS"],pesticide2015$imidacloprid[pesticide2015$site=="DS"],xlim=c(110,180),type="p",pch=16)

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(pesticide2015$julian[pesticide2015$site=="DS"],pesticide2015$dinotefuran[pesticide2015$site=="DS"],xlim=c(110,180),type="p",pch=16)

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)



plot(pesticide2015$julian[pesticide2015$site=="DS"],pesticide2015$acetamiprid[pesticide2015$site=="DS"],xlim=c(110,180),type="p",pch=16)

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(pesticide2015$julian[pesticide2015$site=="DS"],pesticide2015$thiacloprid[pesticide2015$site=="DS"],xlim=c(110,180),type="p",pch=16)

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

# 05-25-18 2015FSR9, FSR8 overstated

par(mfrow=c(8,1),mai=c(.5,1,0,0))

#plot(deadbees$julian[deadbees$hive=="2015FSR8"],deadbees$deadperday[deadbees$hive=="2015FSR8"],type="b",xlim=c(110,180),ylim=c(0,250))
#points(deadbees$julian[deadbees$hive=="2015FSR8"],deadbees$deadperday[deadbees$hive=="2015FSR8"],type="b",col="red")
#points(deadbees$julian[deadbees$hive=="2015FSR9"],deadbees$deadperday[deadbees$hive=="2015FSR9"],type="b",col="red")
#points(deadbees$julian[deadbees$hive=="2015FSR21"],deadbees$deadperday[deadbees$hive=="2015FSR21"],type="b",col="blue")
#points(deadbees$julian[deadbees$hive=="2015FSR27"],deadbees$deadperday[deadbees$hive=="2015FSR27"],type="b",col="green")

#abline(v=31+28+31+30+2)
#abline(v=31+28+31+30+8)

#plot(deadbees$julian[deadbees$hive=="2015FSR8"],deadbees$mortality[deadbees$hive=="2015FSR8"],type="b",xlim=c(110,180),ylim=c(-2,5))
#points(deadbees$julian[deadbees$hive=="2015FSR8"],deadbees$mortality[deadbees$hive=="2015FSR8"],type="b",col="red")
#points(deadbees$julian[deadbees$hive=="2015FSR9"],deadbees$mortality[deadbees$hive=="2015FSR9"],type="b",col="red")
#points(deadbees$julian[deadbees$hive=="2015FSR21"],deadbees$mortality[deadbees$hive=="2015FSR21"],type="b",col="blue")
#points(deadbees$julian[deadbees$hive=="2015FSR27"],deadbees$mortality[deadbees$hive=="2015FSR27"],type="b",col="green")

#abline(v=31+28+31+30+2)
#abline(v=31+28+31+30+8)

plot(deadbees$julian[deadbees$hive=="2015FSR8"],deadbees$mortality2[deadbees$hive=="2015FSR8"],type="b",xlim=c(110,180),ylim=c(0,1))
#points(deadbees$julian[deadbees$hive=="2015FSR8"],deadbees$mortality2[deadbees$hive=="2015FSR8"],type="b",col="red")
points(deadbees$julian[deadbees$hive=="2015FSR9"],deadbees$mortality2[deadbees$hive=="2015FSR9"],type="b",col="red")
points(deadbees$julian[deadbees$hive=="2015FSR21"],deadbees$mortality2[deadbees$hive=="2015FSR21"],type="b",col="blue")
points(deadbees$julian[deadbees$hive=="2015FSR27"],deadbees$mortality2[deadbees$hive=="2015FSR27"],type="b",col="green")

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(pesticide2015$julian[pesticide2015$site=="FSR"],pesticide2015$clothianidin[pesticide2015$site=="FSR"],xlim=c(110,180),type="p",pch=16,col="red")

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(pesticide2015$julian[pesticide2015$site=="FSR"],pesticide2015$thiamethoxam[pesticide2015$site=="FSR"],xlim=c(110,180),type="p",pch=16,col="red")

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(pesticide2015$julian[pesticide2015$site=="FSR"],pesticide2015$imidacloprid[pesticide2015$site=="FSR"],xlim=c(110,180),type="p",pch=16)

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(pesticide2015$julian[pesticide2015$site=="FSR"],pesticide2015$dinotefuran[pesticide2015$site=="FSR"],xlim=c(110,180),type="p",pch=16)

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(pesticide2015$julian[pesticide2015$site=="FSR"],pesticide2015$nitenpyram[pesticide2015$site=="FSR"],xlim=c(110,180),type="p",pch=16)

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(pesticide2015$julian[pesticide2015$site=="FSR"],pesticide2015$acetamiprid[pesticide2015$site=="FSR"],xlim=c(110,180),type="p",pch=16)

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(pesticide2015$julian[pesticide2015$site=="FSR"],pesticide2015$thiacloprid[pesticide2015$site=="FSR"],xlim=c(110,180),type="p",pch=16)

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

# 05-25-18 2015HR5, 2015HR6, 2015HR20  overstated

par(mfrow=c(8,1),mai=c(.5,1,0,0))

#plot(deadbees$julian[deadbees$hive=="2015HR5"],deadbees$deadperday[deadbees$hive=="2015HR5"],type="b",xlim=c(110,180),ylim=c(0,250))
#points(deadbees$julian[deadbees$hive=="2015HR5"],deadbees$deadperday[deadbees$hive=="2015HR5"],type="b",col="red")
#points(deadbees$julian[deadbees$hive=="2015HR6"],deadbees$deadperday[deadbees$hive=="2015HR6"],type="b",col="red")
#points(deadbees$julian[deadbees$hive=="2015HR20"],deadbees$deadperday[deadbees$hive=="2015HR20"],type="b",col="blue")
#points(deadbees$julian[deadbees$hive=="2015HR51"],deadbees$deadperday[deadbees$hive=="2015HR51"],type="b",col="green")
#points(deadbees$julian[deadbees$hive=="2015HR53"],deadbees$deadperday[deadbees$hive=="2015HR53"],type="b",col="brown")
#points(deadbees$julian[deadbees$hive=="2015HR54"],deadbees$deadperday[deadbees$hive=="2015HR54"],type="b",col="orange")

#abline(v=31+28+31+30+2)
#abline(v=31+28+31+30+8)

#plot(deadbees$julian[deadbees$hive=="2015HR5"],deadbees$mortality[deadbees$hive=="2015HR5"],type="b",xlim=c(110,180),ylim=c(-2,6))
#points(deadbees$julian[deadbees$hive=="2015HR5"],deadbees$mortality[deadbees$hive=="2015HR5"],type="b",col="red")
#points(deadbees$julian[deadbees$hive=="2015HR6"],deadbees$mortality[deadbees$hive=="2015HR6"],type="b",col="red")
#points(deadbees$julian[deadbees$hive=="2015HR20"],deadbees$mortality[deadbees$hive=="2015HR20"],type="b",col="blue")
#points(deadbees$julian[deadbees$hive=="2015HR51"],deadbees$mortality[deadbees$hive=="2015HR51"],type="b",col="green")
#points(deadbees$julian[deadbees$hive=="2015HR53"],deadbees$mortality[deadbees$hive=="2015HR53"],type="b",col="brown")
#points(deadbees$julian[deadbees$hive=="2015HR54"],deadbees$mortality[deadbees$hive=="2015HR54"],type="b",col="orange")

#abline(v=31+28+31+30+2)
#abline(v=31+28+31+30+8)

plot(deadbees$julian[deadbees$hive=="2015HR5"],deadbees$mortality2[deadbees$hive=="2015HR5"],type="b",xlim=c(110,180),ylim=c(0,1))
#points(deadbees$julian[deadbees$hive=="2015HR5"],deadbees$mortality2[deadbees$hive=="2015HR5"],type="b",col="red")
points(deadbees$julian[deadbees$hive=="2015HR6"],deadbees$mortality2[deadbees$hive=="2015HR6"],type="b",col="red")
points(deadbees$julian[deadbees$hive=="2015HR20"],deadbees$mortality2[deadbees$hive=="2015HR20"],type="b",col="blue")
points(deadbees$julian[deadbees$hive=="2015HR51"],deadbees$mortality2[deadbees$hive=="2015HR51"],type="b",col="green")
points(deadbees$julian[deadbees$hive=="2015HR53"],deadbees$mortality2[deadbees$hive=="2015HR53"],type="b",col="brown")
points(deadbees$julian[deadbees$hive=="2015HR54"],deadbees$mortality2[deadbees$hive=="2015HR54"],type="b",col="orange")

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(pesticide2015$julian[pesticide2015$site=="HR"],pesticide2015$clothianidin[pesticide2015$site=="HR"],xlim=c(110,180),type="p",pch=16,col="red")

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(pesticide2015$julian[pesticide2015$site=="HR"],pesticide2015$thiamethoxam[pesticide2015$site=="HR"],xlim=c(110,180),type="p",pch=16,col="red")

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(pesticide2015$julian[pesticide2015$site=="HR"],pesticide2015$imidacloprid[pesticide2015$site=="HR"],xlim=c(110,180),type="p",pch=16)

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(pesticide2015$julian[pesticide2015$site=="HR"],pesticide2015$dinotefuran[pesticide2015$site=="HR"],xlim=c(110,180),type="p",pch=16)

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(pesticide2015$julian[pesticide2015$site=="HR"],pesticide2015$nitenpyram[pesticide2015$site=="HR"],xlim=c(110,180),type="p",pch=16)

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(pesticide2015$julian[pesticide2015$site=="HR"],pesticide2015$acetamiprid[pesticide2015$site=="HR"],xlim=c(110,180),type="p",pch=16)

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(pesticide2015$julian[pesticide2015$site=="HR"],pesticide2015$thiacloprid[pesticide2015$site=="HR"],xlim=c(110,180),type="p",pch=16)

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

par(mfrow=c(8,1),mai=c(.5,1,0,0))

#plot(deadbees$julian[deadbees$hive=="2015IB66"],deadbees$deadperday[deadbees$hive=="2015IB66"],type="b",xlim=c(110,180),ylim=c(0,160))
#points(deadbees$julian[deadbees$hive=="2015IB66"],deadbees$deadperday[deadbees$hive=="2015IB66"],type="b",col="red")
#points(deadbees$julian[deadbees$hive=="2015IB67"],deadbees$deadperday[deadbees$hive=="2015IB67"],type="b",col="red")

#abline(v=31+28+31+30+2)
#abline(v=31+28+31+30+8)

#plot(deadbees$julian[deadbees$hive=="2015IB66"],deadbees$mortality[deadbees$hive=="2015IB66"],type="b",xlim=c(110,180),ylim=c(-2,4))
#points(deadbees$julian[deadbees$hive=="2015IB66"],deadbees$mortality[deadbees$hive=="2015IB66"],type="b",col="red")
#points(deadbees$julian[deadbees$hive=="2015IB67"],deadbees$mortality[deadbees$hive=="2015IB67"],type="b",col="red")

#abline(v=31+28+31+30+2)
#abline(v=31+28+31+30+8)

plot(deadbees$julian[deadbees$hive=="2015IB66"],deadbees$mortality2[deadbees$hive=="2015IB66"],type="b",xlim=c(110,180),ylim=c(0,1))
#points(deadbees$julian[deadbees$hive=="2015IB66"],deadbees$mortality2[deadbees$hive=="2015IB66"],type="b",col="red")
points(deadbees$julian[deadbees$hive=="2015IB67"],deadbees$mortality2[deadbees$hive=="2015IB67"],type="b",col="red")

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(pesticide2015$julian[pesticide2015$site=="IB"],pesticide2015$clothianidin[pesticide2015$site=="IB"],xlim=c(110,180),type="p",pch=16,col="red")

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(pesticide2015$julian[pesticide2015$site=="IB"],pesticide2015$thiamethoxam[pesticide2015$site=="IB"],xlim=c(110,180),type="p",pch=16,col="red")

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(pesticide2015$julian[pesticide2015$site=="IB"],pesticide2015$imidacloprid[pesticide2015$site=="IB"],xlim=c(110,180),type="p",pch=16)

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(pesticide2015$julian[pesticide2015$site=="IB"],pesticide2015$dinotefuran[pesticide2015$site=="IB"],xlim=c(110,180),type="p",pch=16)

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(pesticide2015$julian[pesticide2015$site=="IB"],pesticide2015$nitenpyram[pesticide2015$site=="IB"],xlim=c(110,180),type="p",pch=16,col="red")

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(pesticide2015$julian[pesticide2015$site=="IB"],pesticide2015$acetamiprid[pesticide2015$site=="IB"],xlim=c(110,180),type="p",pch=16,col="red")

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(pesticide2015$julian[pesticide2015$site=="IB"],pesticide2015$thiacloprid[pesticide2015$site=="IB"],xlim=c(110,180),type="p",pch=16)

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)


# end 2015IB ----------------------------------------------------------------------------

# 05-25-18 2015MB11 overstated ----------------------------------------------------

par(mfrow=c(3,1),mai=c(.5,1,0,0))

plot(deadbees$julian[deadbees$hive=="2015MB10"],deadbees$deadperday[deadbees$hive=="2015MB10"],type="b",xlim=c(110,180),ylim=c(0,160))
#points(deadbees$julian[deadbees$hive=="2015MB10"],deadbees$deadperday[deadbees$hive=="2015MB10"],type="b",col="red")
points(deadbees$julian[deadbees$hive=="2015MB11"],deadbees$deadperday[deadbees$hive=="2015MB11"],type="b",col="red")
points(deadbees$julian[deadbees$hive=="2015MB23"],deadbees$deadperday[deadbees$hive=="2015MB23"],type="b",col="blue")
points(deadbees$julian[deadbees$hive=="2015MB36"],deadbees$deadperday[deadbees$hive=="2015MB36"],type="b",col="green")

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(deadbees$julian[deadbees$hive=="2015MB10"],deadbees$mortality[deadbees$hive=="2015MB10"],type="b",xlim=c(110,180),ylim=c(-2,4))
#points(deadbees$julian[deadbees$hive=="2015MB10"],deadbees$mortality[deadbees$hive=="2015MB10"],type="b",col="red")
points(deadbees$julian[deadbees$hive=="2015MB11"],deadbees$mortality[deadbees$hive=="2015MB11"],type="b",col="red")
points(deadbees$julian[deadbees$hive=="2015MB23"],deadbees$mortality[deadbees$hive=="2015MB23"],type="b",col="blue")
points(deadbees$julian[deadbees$hive=="2015MB36"],deadbees$mortality[deadbees$hive=="2015MB36"],type="b",col="green")

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(deadbees$julian[deadbees$hive=="2015MB10"],deadbees$mortality2[deadbees$hive=="2015MB10"],type="b",xlim=c(110,180),ylim=c(0,1))
#points(deadbees$julian[deadbees$hive=="2015MB10"],deadbees$mortality2[deadbees$hive=="2015MB10"],type="b",col="red")
points(deadbees$julian[deadbees$hive=="2015MB11"],deadbees$mortality2[deadbees$hive=="2015MB11"],type="b",col="red")
points(deadbees$julian[deadbees$hive=="2015MB23"],deadbees$mortality2[deadbees$hive=="2015MB23"],type="b",col="blue")
points(deadbees$julian[deadbees$hive=="2015MB36"],deadbees$mortality2[deadbees$hive=="2015MB36"],type="b",col="green")

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

# 05-25-18 2015MM39 overstated

par(mfrow=c(8,1),mai=c(.5,1,0,0))

#plot(deadbees$julian[deadbees$hive=="2015MM1"],deadbees$deadperday[deadbees$hive=="2015MM1"],type="b",xlim=c(110,180),ylim=c(0,160))
#points(deadbees$julian[deadbees$hive=="2015MM1"],deadbees$deadperday[deadbees$hive=="2015MM1"],type="b",col="red")
#points(deadbees$julian[deadbees$hive=="2015MM2"],deadbees$deadperday[deadbees$hive=="2015MM2"],type="b",col="red")
#points(deadbees$julian[deadbees$hive=="2015MM16"],deadbees$deadperday[deadbees$hive=="2015MM16"],type="b",col="blue")
#points(deadbees$julian[deadbees$hive=="2015MM37"],deadbees$deadperday[deadbees$hive=="2015MM37"],type="b",col="green")
#points(deadbees$julian[deadbees$hive=="2015MM39"],deadbees$deadperday[deadbees$hive=="2015MM39"],type="b",col="brown")
#points(deadbees$julian[deadbees$hive=="2015MM40"],deadbees$deadperday[deadbees$hive=="2015MM40"],type="b",col="orange")

#abline(v=31+28+31+30+2)
#abline(v=31+28+31+30+8)

#plot(deadbees$julian[deadbees$hive=="2015MM1"],deadbees$mortality[deadbees$hive=="2015MM1"],type="b",xlim=c(110,180),ylim=c(-2,4))
#points(deadbees$julian[deadbees$hive=="2015MM1"],deadbees$mortality[deadbees$hive=="2015MM1"],type="b",col="red")
#points(deadbees$julian[deadbees$hive=="2015MM2"],deadbees$mortality[deadbees$hive=="2015MM2"],type="b",col="red")
#points(deadbees$julian[deadbees$hive=="2015MM16"],deadbees$mortality[deadbees$hive=="2015MM16"],type="b",col="blue")
#points(deadbees$julian[deadbees$hive=="2015MM37"],deadbees$mortality[deadbees$hive=="2015MM37"],type="b",col="green")
#points(deadbees$julian[deadbees$hive=="2015MM39"],deadbees$mortality[deadbees$hive=="2015MM39"],type="b",col="brown")
#points(deadbees$julian[deadbees$hive=="2015MM40"],deadbees$mortality[deadbees$hive=="2015MM40"],type="b",col="orange")

#abline(v=31+28+31+30+2)
#abline(v=31+28+31+30+8)

plot(deadbees$julian[deadbees$hive=="2015MM1"],deadbees$mortality2[deadbees$hive=="2015MM1"],type="b",xlim=c(110,180),ylim=c(0,1))
#points(deadbees$julian[deadbees$hive=="2015MM1"],deadbees$mortality2[deadbees$hive=="2015MM1"],type="b",col="red")
points(deadbees$julian[deadbees$hive=="2015MM2"],deadbees$mortality2[deadbees$hive=="2015MM2"],type="b",col="red")
points(deadbees$julian[deadbees$hive=="2015MM16"],deadbees$mortality2[deadbees$hive=="2015MM16"],type="b",col="blue")
points(deadbees$julian[deadbees$hive=="2015MM37"],deadbees$mortality2[deadbees$hive=="2015MM37"],type="b",col="green")
points(deadbees$julian[deadbees$hive=="2015MM39"],deadbees$mortality2[deadbees$hive=="2015MM39"],type="b",col="brown")
points(deadbees$julian[deadbees$hive=="2015MM40"],deadbees$mortality2[deadbees$hive=="2015MM40"],type="b",col="orange")

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(pesticide2015$julian[pesticide2015$site=="MM"],pesticide2015$clothianidin[pesticide2015$site=="MM"],xlim=c(110,180),type="p",pch=16,col="red")

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(pesticide2015$julian[pesticide2015$site=="MM"],pesticide2015$thiamethoxam[pesticide2015$site=="MM"],xlim=c(110,180),type="p",pch=16,col="red")

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(pesticide2015$julian[pesticide2015$site=="MM"],pesticide2015$imidacloprid[pesticide2015$site=="MM"],xlim=c(110,180),type="p",pch=16)

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(pesticide2015$julian[pesticide2015$site=="MM"],pesticide2015$dinotefuran[pesticide2015$site=="MM"],xlim=c(110,180),type="p",pch=16)

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(pesticide2015$julian[pesticide2015$site=="MM"],pesticide2015$nitenpyram[pesticide2015$site=="MM"],xlim=c(110,180),type="p",pch=16)

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(pesticide2015$julian[pesticide2015$site=="MM"],pesticide2015$acetamiprid[pesticide2015$site=="MM"],xlim=c(110,180),type="p",pch=16)

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(pesticide2015$julian[pesticide2015$site=="MM"],pesticide2015$thiacloprid[pesticide2015$site=="MM"],xlim=c(110,180),type="p",pch=16)

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

par(mfrow=c(8,1),mai=c(.5,1,0,0))

#plot(deadbees$julian[deadbees$hive=="2015SC3"],deadbees$deadperday[deadbees$hive=="2015SC3"],type="b",xlim=c(110,180),ylim=c(0,160))
#points(deadbees$julian[deadbees$hive=="2015SC3"],deadbees$deadperday[deadbees$hive=="2015SC3"],type="b",col="red")
#points(deadbees$julian[deadbees$hive=="2015SC4"],deadbees$deadperday[deadbees$hive=="2015SC4"],type="b",col="red")
#points(deadbees$julian[deadbees$hive=="2015SC17"],deadbees$deadperday[deadbees$hive=="2015SC17"],type="b",col="blue")
#points(deadbees$julian[deadbees$hive=="2015SC45"],deadbees$deadperday[deadbees$hive=="2015SC45"],type="b",col="green")
#points(deadbees$julian[deadbees$hive=="2015SC48"],deadbees$deadperday[deadbees$hive=="2015SC48"],type="b",col="brown")
#points(deadbees$julian[deadbees$hive=="2015SC49"],deadbees$deadperday[deadbees$hive=="2015SC49"],type="b",col="orange")

#abline(v=31+28+31+30+2)
#abline(v=31+28+31+30+8)

#plot(deadbees$julian[deadbees$hive=="2015SC3"],deadbees$mortality[deadbees$hive=="2015SC3"],type="b",xlim=c(110,180),ylim=c(-2,4))
#points(deadbees$julian[deadbees$hive=="2015SC1"],deadbees$mortality[deadbees$hive=="2015SC1"],type="b",col="red")
#points(deadbees$julian[deadbees$hive=="2015SC4"],deadbees$mortality[deadbees$hive=="2015SC4"],type="b",col="red")
#points(deadbees$julian[deadbees$hive=="2015SC17"],deadbees$mortality[deadbees$hive=="2015SC17"],type="b",col="blue")
#points(deadbees$julian[deadbees$hive=="2015SC45"],deadbees$mortality[deadbees$hive=="2015SC45"],type="b",col="green")
#points(deadbees$julian[deadbees$hive=="2015SC48"],deadbees$mortality[deadbees$hive=="2015SC48"],type="b",col="brown")
#points(deadbees$julian[deadbees$hive=="2015SC49"],deadbees$mortality[deadbees$hive=="2015SC49"],type="b",col="orange")

#abline(v=31+28+31+30+2)
#abline(v=31+28+31+30+8)

plot(deadbees$julian[deadbees$hive=="2015SC3"],deadbees$mortality2[deadbees$hive=="2015SC3"],type="b",xlim=c(110,180),ylim=c(0,1))
#points(deadbees$julian[deadbees$hive=="2015SC1"],deadbees$mortality2[deadbees$hive=="2015SC1"],type="b",col="red")
points(deadbees$julian[deadbees$hive=="2015SC4"],deadbees$mortality2[deadbees$hive=="2015SC4"],type="b",col="red")
points(deadbees$julian[deadbees$hive=="2015SC17"],deadbees$mortality2[deadbees$hive=="2015SC17"],type="b",col="blue")
points(deadbees$julian[deadbees$hive=="2015SC45"],deadbees$mortality2[deadbees$hive=="2015SC45"],type="b",col="green")
points(deadbees$julian[deadbees$hive=="2015SC48"],deadbees$mortality2[deadbees$hive=="2015SC48"],type="b",col="brown")
points(deadbees$julian[deadbees$hive=="2015SC49"],deadbees$mortality2[deadbees$hive=="2015SC49"],type="b",col="orange")

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(pesticide2015$julian[pesticide2015$site=="SC"],pesticide2015$clothianidin[pesticide2015$site=="SC"],xlim=c(110,180),type="p",pch=16,col="red")

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(pesticide2015$julian[pesticide2015$site=="SC"],pesticide2015$thiamethoxam[pesticide2015$site=="SC"],xlim=c(110,180),type="p",pch=16,col="red")

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(pesticide2015$julian[pesticide2015$site=="SC"],pesticide2015$imidacloprid[pesticide2015$site=="SC"],xlim=c(110,180),type="p",pch=16,col="red")

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(pesticide2015$julian[pesticide2015$site=="SC"],pesticide2015$dinotefuran[pesticide2015$site=="SC"],xlim=c(110,180),type="p",pch=16)

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(pesticide2015$julian[pesticide2015$site=="SC"],pesticide2015$nitenpyram[pesticide2015$site=="SC"],xlim=c(110,180),type="p",pch=16)

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(pesticide2015$julian[pesticide2015$site=="SC"],pesticide2015$acetamiprid[pesticide2015$site=="SC"],xlim=c(110,180),type="p",pch=16)

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(pesticide2015$julian[pesticide2015$site=="SC"],pesticide2015$thiacloprid[pesticide2015$site=="SC"],xlim=c(110,180),type="p",pch=16)

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

# 05-25-18 2015TV63 overstated

par(mfrow=c(8,1),mai=c(.5,1,0,0))

#plot(deadbees$julian[deadbees$hive=="2015TV62"],deadbees$deadperday[deadbees$hive=="2015TV62"],type="b",xlim=c(110,180),ylim=c(0,460))
#points(deadbees$julian[deadbees$hive=="2015TV62"],deadbees$deadperday[deadbees$hive=="2015TV62"],type="b",col="red")
#points(deadbees$julian[deadbees$hive=="2015TV63"],deadbees$deadperday[deadbees$hive=="2015TV63"],type="b",col="red")

#abline(v=31+28+31+30+2)
#abline(v=31+28+31+30+8)

#plot(deadbees$julian[deadbees$hive=="2015TV62"],deadbees$mortality[deadbees$hive=="2015TV62"],type="b",xlim=c(110,180),ylim=c(-2,4))
#points(deadbees$julian[deadbees$hive=="2015TV62"],deadbees$mortality[deadbees$hive=="2015TV62"],type="b",col="red")
#points(deadbees$julian[deadbees$hive=="2015TV63"],deadbees$mortality[deadbees$hive=="2015TV63"],type="b",col="red")

#abline(v=31+28+31+30+2)
#abline(v=31+28+31+30+8)

plot(deadbees$julian[deadbees$hive=="2015TV62"],deadbees$mortality2[deadbees$hive=="2015TV62"],type="b",xlim=c(110,180),ylim=c(0,1))
#points(deadbees$julian[deadbees$hive=="2015TV62"],deadbees$mortality2[deadbees$hive=="2015TV62"],type="b",col="red")
points(deadbees$julian[deadbees$hive=="2015TV63"],deadbees$mortality2[deadbees$hive=="2015TV63"],type="b",col="red")

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(pesticide2015$julian[pesticide2015$site=="TV"],pesticide2015$clothianidin[pesticide2015$site=="TV"],xlim=c(110,180),type="p",pch=16,col="red")

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(pesticide2015$julian[pesticide2015$site=="TV"],pesticide2015$thiamethoxam[pesticide2015$site=="TV"],xlim=c(110,180),type="p",pch=16,col="red")

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(pesticide2015$julian[pesticide2015$site=="TV"],pesticide2015$imidacloprid[pesticide2015$site=="TV"],xlim=c(110,180),type="p",pch=16)

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(pesticide2015$julian[pesticide2015$site=="TV"],pesticide2015$dinotefuran[pesticide2015$site=="TV"],xlim=c(110,180),type="p",pch=16)

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(pesticide2015$julian[pesticide2015$site=="TV"],pesticide2015$nitenpyram[pesticide2015$site=="TV"],xlim=c(110,180),type="p",pch=16)

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(pesticide2015$julian[pesticide2015$site=="TV"],pesticide2015$acetamiprid[pesticide2015$site=="TV"],xlim=c(110,180),type="p",pch=16)

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(pesticide2015$julian[pesticide2015$site=="TV"],pesticide2015$thiacloprid[pesticide2015$site=="TV"],xlim=c(110,180),type="p",pch=16)

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

par(mfrow=c(8,1),mai=c(.5,1,0,0))

#plot(deadbees$julian[deadbees$hive=="2015WB26"],deadbees$deadperday[deadbees$hive=="2015WB26"],type="b",xlim=c(110,180),ylim=c(0,160))
#points(deadbees$julian[deadbees$hive=="2015WB26"],deadbees$deadperday[deadbees$hive=="2015WB26"],type="b",col="red")
#points(deadbees$julian[deadbees$hive=="2015WB12"],deadbees$deadperday[deadbees$hive=="2015WB12"],type="b",col="red")
#points(deadbees$julian[deadbees$hive=="2015WB13"],deadbees$deadperday[deadbees$hive=="2015WB13"],type="b",col="blue")
#points(deadbees$julian[deadbees$hive=="2015WB28"],deadbees$deadperday[deadbees$hive=="2015WB28"],type="b",col="green")

#abline(v=31+28+31+30+2)
#abline(v=31+28+31+30+8)

#plot(deadbees$julian[deadbees$hive=="2015WB26"],deadbees$mortality[deadbees$hive=="2015WB26"],type="b",xlim=c(110,180),ylim=c(-2,4))
#points(deadbees$julian[deadbees$hive=="2015WB26"],deadbees$mortality[deadbees$hive=="2015WB26"],type="b",col="red")
#points(deadbees$julian[deadbees$hive=="2015WB12"],deadbees$mortality[deadbees$hive=="2015WB12"],type="b",col="red")
#points(deadbees$julian[deadbees$hive=="2015WB13"],deadbees$mortality[deadbees$hive=="2015WB13"],type="b",col="blue")
#points(deadbees$julian[deadbees$hive=="2015WB28"],deadbees$mortality[deadbees$hive=="2015WB28"],type="b",col="green")

#abline(v=31+28+31+30+2)
#abline(v=31+28+31+30+8)

plot(deadbees$julian[deadbees$hive=="2015WB26"],deadbees$mortality2[deadbees$hive=="2015WB26"],type="b",xlim=c(110,180),ylim=c(0,1))
#points(deadbees$julian[deadbees$hive=="2015WB26"],deadbees$mortality2[deadbees$hive=="2015WB26"],type="b",col="red")
points(deadbees$julian[deadbees$hive=="2015WB12"],deadbees$mortality2[deadbees$hive=="2015WB12"],type="b",col="red")
points(deadbees$julian[deadbees$hive=="2015WB13"],deadbees$mortality2[deadbees$hive=="2015WB13"],type="b",col="blue")
points(deadbees$julian[deadbees$hive=="2015WB28"],deadbees$mortality2[deadbees$hive=="2015WB28"],type="b",col="green")

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(pesticide2015$julian[pesticide2015$site=="WB"],pesticide2015$clothianidin[pesticide2015$site=="WB"],xlim=c(110,180),type="p",pch=16,col="red")

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(pesticide2015$julian[pesticide2015$site=="WB"],pesticide2015$thiamethoxam[pesticide2015$site=="WB"],xlim=c(110,180),type="p",pch=16,col="red")

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(pesticide2015$julian[pesticide2015$site=="WB"],pesticide2015$imidacloprid[pesticide2015$site=="WB"],xlim=c(110,180),type="p",pch=16)

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(pesticide2015$julian[pesticide2015$site=="WB"],pesticide2015$dinotefuran[pesticide2015$site=="WB"],xlim=c(110,180),type="p",pch=16)

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(pesticide2015$julian[pesticide2015$site=="WB"],pesticide2015$nitenpyram[pesticide2015$site=="WB"],xlim=c(110,180),type="p",pch=16)

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(pesticide2015$julian[pesticide2015$site=="WB"],pesticide2015$acetamiprid[pesticide2015$site=="WB"],xlim=c(110,180),type="p",pch=16)

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(pesticide2015$julian[pesticide2015$site=="WB"],pesticide2015$thiacloprid[pesticide2015$site=="WB"],xlim=c(110,180),type="p",pch=16)

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

# END FIG -----------------------------

# 05-29-18 2015BG clothianidin cor best w mortlag3 -----------------------------

test <- deadbees[deadbees$hive=="2015BG60",]
test2 <- pesticide2015[pesticide2015$site=="BG",]
test3 <- merge(test,test2,by.x=8,by.y=6,all.x=T)

n <- dim(test3)
test3$mortlag1 <- c(test3$mortality2[-1],NA)
test3$mortlag2 <- c(test3$mortality2[-c(1:2)],rep(NA,2))
test3$mortlag3 <- c(test3$mortality2[-c(1:3)],rep(NA,3))
test3$mortlag4 <- c(test3$mortality2[-c(1:4)],rep(NA,4))
test3$mortlag5 <- c(test3$mortality2[-c(1:5)],rep(NA,5))

test4 <- test3

test4$dinotefuran[is.na(test4$dinotefuran)] <- 0
test4$nitenpyram[is.na(test4$nitenpyram)] <- 0
test4$thiamethoxam[is.na(test4$thiamethoxam)] <- 0
test4$clothianidin[is.na(test4$clothianidin)] <- 0
test4$imidacloprid[is.na(test4$imidacloprid)] <- 0
test4$acetamiprid[is.na(test4$acetamiprid)] <- 0
test4$thiacloprid[is.na(test4$thiacloprid)] <- 0

test5 <- cor(test3[,c(11:14,17,26:37)],use="pairwise.complete.obs")

test6 <- cor(test4[,c(11:14,17,26:37)],use="pairwise.complete.obs")
test6[c(6:12),c(5,13:16)]

# 05-29-18 BG59 mortlag1 cor best w clothianidin, thiamethoxam, thiacloprid, imidacloprid, mortlag3 cor best w nitenpyram
# 05-29-18 BG60 mortlag3 cor best w clothianidin, dinotefuran but cor poor

# combine 2 colonies
# 05-30-18 

             mortality2     mortlag1    mortlag2    mortlag3    mortlag4
dinotefuran  -0.2255791 -0.255352349 -0.31903751 -0.24035940  0.44847200
nitenpyram    0.1784615  0.146968936  0.05031284  0.43927831  0.13945056
thiamethoxam  0.2809125  0.164831022 -0.12694435  0.15697632 -0.05679981
clothianidin  0.3369837  0.234606383  0.10955827  0.17100596 -0.15485237
imidacloprid  0.2559086  0.008111221 -0.32611657  0.04814048 -0.03095654
acetamiprid   0.2073655  0.008383867 -0.31835627  0.06248770  0.14493056
thiacloprid   0.1761068  0.393344592 -0.00352315  0.41822793 -0.10306374

testa <- deadbees[deadbees$hive=="2015BG59",]
testb <- deadbees[deadbees$hive=="2015BG60",]

test2 <- pesticide2015[pesticide2015$site=="BG",]
n <- dim(test2)
test2$clothianidinlag1 <- c(NA,test2$clothianidin[-n[1]])
test2$thiamethoxamlag1 <- c(NA,test2$thiamethoxam[-n[1]])
test2$thiaclopridlag1 <- c(NA,test2$thiacloprid[-n[1]])

test3a <- merge(testa,test2,by.x=8,by.y=6,all.x=T)
test3b <- merge(testb,test2,by.x=8,by.y=6,all.x=T)

n <- dim(test3a)
test3a$mortlag1 <- c(test3a$mortality2[-1],NA)
test3a$mortlag2 <- c(test3a$mortality2[-c(1:2)],rep(NA,2))
test3a$mortlag3 <- c(test3a$mortality2[-c(1:3)],rep(NA,3))
test3a$mortlag4 <- c(test3a$mortality2[-c(1:4)],rep(NA,4))
test3a$mortlag5 <- c(test3a$mortality2[-c(1:5)],rep(NA,5))
test3a$mort_lag1 <- c(NA,test3a$mortality2[-n[1]])
test3a$mort_lag2 <- c(NA,NA,test3a$mortality2[-c(n[1]-1,n[1])])

n <- dim(test3b)
test3b$mortlag1 <- c(test3b$mortality2[-1],NA)
test3b$mortlag2 <- c(test3b$mortality2[-c(1:2)],rep(NA,2))
test3b$mortlag3 <- c(test3b$mortality2[-c(1:3)],rep(NA,3))
test3b$mortlag4 <- c(test3b$mortality2[-c(1:4)],rep(NA,4))
test3b$mortlag5 <- c(test3b$mortality2[-c(1:5)],rep(NA,5))
test3b$mort_lag1 <- c(NA,test3b$mortality2[-n[1]])
test3b$mort_lag2 <- c(NA,NA,test3b$mortality2[-c(n[1]-1,n[1])])

test3 <- rbind(test3a,test3b)

test4 <- test3

test4$dinotefuran[is.na(test4$dinotefuran)] <- 0
test4$nitenpyram[is.na(test4$nitenpyram)] <- 0
test4$thiamethoxam[is.na(test4$thiamethoxam)] <- 0
test4$clothianidin[is.na(test4$clothianidin)] <- 0
test4$imidacloprid[is.na(test4$imidacloprid)] <- 0
test4$acetamiprid[is.na(test4$acetamiprid)] <- 0
test4$thiacloprid[is.na(test4$thiacloprid)] <- 0

test5 <- cor(test3[,c(11:14,17,26:37)],use="pairwise.complete.obs")

test6 <- cor(test4[,c(11:14,17,26:37)],use="pairwise.complete.obs")

test6[c(6:12),c(5,13:16)]

plot(test4$clothianidin,test4$mortlag3,pch=16)

par(mfrow=c(3,1),mai=c(.5,1,0,0))
plot(test4$julian,test4$mortality2,type="b",xlim=c(110,180))

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(test4$julian,test4$mortlag3,type="b",xlim=c(110,180))
abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(test4$julian,test4$clothianidin,type="b",xlim=c(110,180))

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

test4$clothianidinspline <- test4$clothianidin
test4$clothianidinspline[test4$clothianidin<9] <- 0
test4$clothianidinsplinelag1 <- test4$clothianidinlag1
test4$clothianidinsplinelag1[test4$clothianidinlag1<9] <- 0

test4$i122 <- rep(0,n[1])
test4$i122[test4$julian==122] <- 1
test4$i125 <- rep(0,n[1])
test4$i125[test4$julian==125] <- 1
test4$i128 <- rep(0,n[1])
test4$i128[test4$julian==128] <- 1

test4$i145p <- rep(0,n[1])
test4$i145p[test4$julian>145] <- 1

test4$i140p <- rep(0,n[1])
test4$i140p[test4$julian>140] <- 1

test4$ibg59 <- rep(0,n[1])
test4$ibg59[test4$hive=="2015BG59"] <- 1

test4$clobg59 <- test4$clothianidin*test4$ibg59

test4$thiabg59 <- test4$thiamethoxam*test4$ibg59

test.lm <- lm(mortality2 ~ clobg59+i122+i145p,data=test4,na.action="na.exclude")
summary(test.lm)

test.gls <- gls(mortality2 ~ clobg59+i122+i145p, 
correlation = corARMA(p =1, q = 0),data=test4,na.action=na.omit,method="ML")
summary(test.gls)

#test4$res <- test.lm$res
n <- dim(test4)
test4$resid <- test4$mortality2-(test.lm$coef[1]+test.lm$coef[2]*test4$clobg59+test.lm$coef[3]*test4$i122+test.lm$coef[4]*test4$i145p)
test4$residlag1 <- c(NA,test4$resid[-n[1]])
test4$residlag1[is.na(test4$mort_lag1)] <- NA

plot(test4$julian,test4$resid)

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  0.34150    0.06404   5.332 6.74e-05 ***
clobg59      0.02868    0.01017   2.821  0.01229 *  
i122         0.57695    0.15531   3.715  0.00188 ** 
i145p       -0.23105    0.12081  -1.913  0.07388 .  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.2049 on 16 degrees of freedom
Multiple R-squared:  0.6671,    Adjusted R-squared:  0.6047 
F-statistic: 10.69 on 3 and 16 DF,  p-value: 0.0004222

# 12-13-18

Generalized least squares fit by maximum likelihood
  Model: mortality2 ~ clobg59 + i122 + i145p 
  Data: test4 
         AIC      BIC   logLik
  -0.9146858 5.059708 6.457343

Correlation Structure: AR(1)
 Formula: ~1 
 Parameter estimate(s):
       Phi 
-0.3469155 

Coefficients:
                 Value  Std.Error   t-value p-value
(Intercept)  0.3091425 0.04964052  6.227624  0.0000
clobg59      0.0350739 0.00776675  4.515905  0.0004
i122         0.6610634 0.14739630  4.484939  0.0004
i145p       -0.2058728 0.10034507 -2.051649  0.0569

Residual standard error: 0.1862071 
Degrees of freedom: 20 total; 16 residual

test.lm <- lm(mortality2 ~ thiamethoxam+i122+residlag1,data=test4,na.action="na.exclude")
summary(test.lm)

#test4$res <- test.lm$res
n <- dim(test4)
test4$resid <- test4$mortality2-(test.lm$coef[1]+test.lm$coef[2]*test4$thiamethoxam+test.lm$coef[3]*test4$i122)
test4$residlag1 <- c(NA,test4$resid[-n[1]])
test4$residlag1[is.na(test4$mort_lag1)] <- NA

plot(test4$julian,test4$resid)


Coefficients:
             Estimate Std. Error t value Pr(>|t|)   
(Intercept)  0.253743   0.086135   2.946  0.01063 * 
thiamethoxam 0.015088   0.008878   1.699  0.11134   
i122         0.639561   0.204763   3.123  0.00748 **
residlag1    0.246292   0.274204   0.898  0.38426   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.263 on 14 degrees of freedom
  (2 observations deleted due to missingness)
Multiple R-squared:  0.5159,    Adjusted R-squared:  0.4121 
F-statistic: 4.972 on 3 and 14 DF,  p-value: 0.01486

# end 2015BG ---------------------------------------

# 05-30-18 2015DS  -----------------------------

2015DS31  2015DS33

test <- deadbees[deadbees$hive=="2015DS33",]
test2 <- pesticide2015[pesticide2015$site=="DS",]
test3 <- merge(test,test2,by.x=8,by.y=6,all.x=T)

n <- dim(test3)
test3$mortlag1 <- c(test3$mortality2[-1],NA)
test3$mortlag2 <- c(test3$mortality2[-c(1:2)],rep(NA,2))
test3$mortlag3 <- c(test3$mortality2[-c(1:3)],rep(NA,3))
test3$mortlag4 <- c(test3$mortality2[-c(1:4)],rep(NA,4))
test3$mortlag5 <- c(test3$mortality2[-c(1:5)],rep(NA,5))

test4 <- test3

test4$dinotefuran[is.na(test4$dinotefuran)] <- 0
test4$nitenpyram[is.na(test4$nitenpyram)] <- 0
test4$thiamethoxam[is.na(test4$thiamethoxam)] <- 0
test4$clothianidin[is.na(test4$clothianidin)] <- 0
test4$imidacloprid[is.na(test4$imidacloprid)] <- 0
test4$acetamiprid[is.na(test4$acetamiprid)] <- 0
test4$thiacloprid[is.na(test4$thiacloprid)] <- 0

test5 <- cor(test3[,c(11:14,17,26:37)],use="pairwise.complete.obs")

test6 <- cor(test4[,c(11:14,17,26:37)],use="pairwise.complete.obs")

test6[c(6:12),c(5,13:16)]

# 05-30-18 DS31

             mortality2    mortlag1    mortlag2     mortlag3   mortlag4
dinotefuran  -0.3598834 -0.06850144 -0.03465540  0.002436284  0.7212590
nitenpyram    0.4704829  0.70019297 -0.07397855 -0.351546013  0.3889929
thiamethoxam  0.5866009  0.82305100  0.08382867  0.116820155  0.3769224
clothianidin  0.6609825  0.34053658 -0.03507827 -0.282822145 -0.2212970
imidacloprid -0.2296493 -0.02925183 -0.28912316 -0.734552445  0.1014236
acetamiprid  -0.2267233 -0.09789964 -0.28117292 -0.611709205  0.1296606
thiacloprid  -0.3802428 -0.08931353 -0.20022509 -0.663589574  0.3722505

# 05-30-18 DS33 

             mortality2   mortlag1    mortlag2    mortlag3  mortlag4
dinotefuran  -0.4158511 -0.3295674 -0.15509187 -0.19396539 0.9504082
nitenpyram    0.3227521  0.6412560  0.23420749  0.12771933 0.6926236
thiamethoxam  0.1120703  0.3783225  0.09836142  0.48423083 0.8185934
clothianidin  0.2892381  0.3412564  0.69511765  0.38776593 0.1155899
imidacloprid -0.2166678 -0.1010062  0.47054355 -0.36450608 0.1695520
acetamiprid  -0.2899770 -0.3142756  0.25483123  0.02418776 0.2972513
thiacloprid  -0.3781470 -0.2832608  0.30631475 -0.48419851 0.3716670

# combine 2 colonies

             mortality2    mortlag1    mortlag2    mortlag3   mortlag4
dinotefuran  -0.3877386 -0.20094215 -0.09602370 -0.12011226 0.74995483
nitenpyram    0.3949859  0.66800138  0.08439684 -0.01853439 0.52163013
thiamethoxam  0.3453778  0.59442669  0.09057795  0.33414425 0.60140210
clothianidin  0.4717302  0.33981154  0.33901153  0.16258164 0.03487292
imidacloprid -0.2227539 -0.06561996  0.10214527 -0.43191584 0.12882359
acetamiprid  -0.2584546 -0.20753706 -0.00446700 -0.15558696 0.21706948
thiacloprid  -0.3786533 -0.18758138  0.06073092 -0.48665761 0.30972122

testa <- deadbees[deadbees$hive=="2015DS31",]
testb <- deadbees[deadbees$hive=="2015DS33",]

test2 <- pesticide2015[pesticide2015$site=="DS",]
n <- dim(test2)
test2$clothianidinlag1 <- c(NA,test2$clothianidin[-n[1]])
test2$thiamethoxamlag1 <- c(NA,test2$thiamethoxam[-n[1]])
test2$nitenpyramlag1 <- c(NA,test2$nitenpyram[-n[1]])


test3a <- merge(testa,test2,by.x=8,by.y=6,all.x=T)
test3b <- merge(testb,test2,by.x=8,by.y=6,all.x=T)

n <- dim(test3a)
test3a$mortlag1 <- c(test3a$mortality2[-1],NA)
test3a$mortlag2 <- c(test3a$mortality2[-c(1:2)],rep(NA,2))
test3a$mortlag3 <- c(test3a$mortality2[-c(1:3)],rep(NA,3))
test3a$mortlag4 <- c(test3a$mortality2[-c(1:4)],rep(NA,4))
test3a$mortlag5 <- c(test3a$mortality2[-c(1:5)],rep(NA,5))
test3a$mort_lag1 <- c(NA,test3a$mortality2[-n[1]])
test3a$mort_lag2 <- c(NA,NA,test3a$mortality2[-c(n[1]-1,n[1])])

n <- dim(test3b)
test3b$mortlag1 <- c(test3b$mortality2[-1],NA)
test3b$mortlag2 <- c(test3b$mortality2[-c(1:2)],rep(NA,2))
test3b$mortlag3 <- c(test3b$mortality2[-c(1:3)],rep(NA,3))
test3b$mortlag4 <- c(test3b$mortality2[-c(1:4)],rep(NA,4))
test3b$mortlag5 <- c(test3b$mortality2[-c(1:5)],rep(NA,5))
test3b$mort_lag1 <- c(NA,test3b$mortality2[-n[1]])
test3b$mort_lag2 <- c(NA,NA,test3b$mortality2[-c(n[1]-1,n[1])])

test3 <- rbind(test3a,test3b)

test4 <- test3

test4$dinotefuran[is.na(test4$dinotefuran)] <- 0
test4$nitenpyram[is.na(test4$nitenpyram)] <- 0
test4$thiamethoxam[is.na(test4$thiamethoxam)] <- 0
test4$clothianidin[is.na(test4$clothianidin)] <- 0
test4$imidacloprid[is.na(test4$imidacloprid)] <- 0
test4$acetamiprid[is.na(test4$acetamiprid)] <- 0
test4$thiacloprid[is.na(test4$thiacloprid)] <- 0

test5 <- cor(test3[,c(11:14,17,26:37)],use="pairwise.complete.obs")

test6 <- cor(test4[,c(11:14,17,26:37)],use="pairwise.complete.obs")

test6[c(6:12),c(5,13:16)]

plot(test4$clothianidin,test4$mortlag3,pch=16)

par(mfrow=c(3,1),mai=c(.5,1,0,0))
plot(test4$julian,test4$mortality2,type="b",xlim=c(110,180))

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(test4$julian,test4$mortlag3,type="b",xlim=c(110,180))
abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(test4$julian,test4$clothianidin,type="b",xlim=c(110,180))

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

test4$clothianidinspline <- test4$clothianidin
test4$clothianidinspline[test4$clothianidin<9] <- 0
test4$clothianidinsplinelag1 <- test4$clothianidinlag1
test4$clothianidinsplinelag1[test4$clothianidinlag1<9] <- 0

test4$clothianidinlag1[test4$julian==154] <- 7

test4$i131 <- rep(0,n[1])
test4$i131[test4$julian==131] <- 1
test4$i122 <- rep(0,n[1])
test4$i122[test4$julian==122] <- 1
test4$i125 <- rep(0,n[1])
test4$i125[test4$julian==125] <- 1
test4$i128 <- rep(0,n[1])
test4$i128[test4$julian==128] <- 1
test4$i134 <- rep(0,n[1])
test4$i134[test4$julian==134] <- 1

test4$ids31 <- rep(0,n[1])
test4$ids31[test4$hive=="2015DS31"] <- 1

test4$clods31 <- test4$clothianidin*test4$ids31

test4$thiads31 <- test4$thiamethoxam*test4$ids31

test4$nitds31lag1 <- test4$nitenpyramlag1*test4$ids31
test4$thiads31lag1 <- test4$thiamethoxamlag1*test4$ids31

test.lm <- lm(mortality2 ~ thiamethoxamlag1,data=test4,na.action="na.exclude")
summary(test.lm)

#test4$res <- test.lm$res
n <- dim(test4)
test4$resid <- test4$mortality2-(test.lm$coef[1]+test.lm$coef[2]*test4$thiamethoxamlag1)
test4$residlag1 <- c(NA,test4$resid[-n[1]])
test4$residlag1[is.na(test4$mort_lag1)] <- NA

plot(test4$julian,test4$resid)

# 12-13-18

test.gls <- gls(mortality2 ~ thiamethoxamlag1, 
correlation = corARMA(p =1, q = 0),data=test4,na.action=na.omit,method="ML")
summary(test.gls)

Coefficients:
                 Estimate Std. Error t value Pr(>|t|)    
(Intercept)       0.04662    0.06623   0.704 0.497546    
thiamethoxamlag1  0.13960    0.02354   5.931 0.000145 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.1447 on 10 degrees of freedom
  (8 observations deleted due to missingness)
Multiple R-squared:  0.7786,    Adjusted R-squared:  0.7565 
F-statistic: 35.17 on 1 and 10 DF,  p-value: 0.0001449

# end 2015DS ---------------------------------------

# 05-30-18 2015FSR  -----------------------------

2015FSR8  2015FSR9 
[43] 2015FSR21 2015FSR27

test <- deadbees[deadbees$hive=="2015FSR8",]
test2 <- pesticide2015[pesticide2015$site=="FSR",]
test3 <- merge(test,test2,by.x=8,by.y=6,all.x=T)

n <- dim(test3)
test3$mortlag1 <- c(test3$mortality2[-1],NA)
test3$mortlag2 <- c(test3$mortality2[-c(1:2)],rep(NA,2))
test3$mortlag3 <- c(test3$mortality2[-c(1:3)],rep(NA,3))
test3$mortlag4 <- c(test3$mortality2[-c(1:4)],rep(NA,4))
test3$mortlag5 <- c(test3$mortality2[-c(1:5)],rep(NA,5))

test4 <- test3

test4$dinotefuran[is.na(test4$dinotefuran)] <- 0
test4$nitenpyram[is.na(test4$nitenpyram)] <- 0
test4$thiamethoxam[is.na(test4$thiamethoxam)] <- 0
test4$clothianidin[is.na(test4$clothianidin)] <- 0
test4$imidacloprid[is.na(test4$imidacloprid)] <- 0
test4$acetamiprid[is.na(test4$acetamiprid)] <- 0
test4$thiacloprid[is.na(test4$thiacloprid)] <- 0

test5 <- cor(test3[,c(11:14,17,26:37)],use="pairwise.complete.obs")

test6 <- cor(test4[,c(11:14,17,26:37)],use="pairwise.complete.obs")

test6[c(6:12),c(5,13:16)]

# 05-30-18 combine 4 colonies

              mortality2    mortlag1    mortlag2    mortlag3    mortlag4
dinotefuran  -0.22682037 -0.11111920 -0.19528672 -0.55497827 -0.36847618
nitenpyram   -0.11379737 -0.11282803 -0.07249396  0.09255343  0.36847618
thiamethoxam  0.74318014  0.56848674 -0.11054931  0.12628952 -0.01901676
clothianidin  0.79672618  0.58901788 -0.07452673  0.11706404  0.03229584
imidacloprid -0.08922260 -0.24195714 -0.22834362  0.50287560 -0.34750103
acetamiprid   0.27314209 -0.04827995 -0.25593428 -0.22509755 -0.03591521
thiacloprid  -0.09150625 -0.44388990 -0.41390822  0.14874277 -0.05426745

testa <- deadbees[deadbees$hive=="2015FSR8",]
testb <- deadbees[deadbees$hive=="2015FSR9",]
testc <- deadbees[deadbees$hive=="2015FSR21",]
testd <- deadbees[deadbees$hive=="2015FSR27",]

test2 <- pesticide2015[pesticide2015$site=="FSR",]
n <- dim(test2)
test2$clothianidinlag1 <- c(NA,test2$clothianidin[-n[1]])
test2$thiamethoxamlag1 <- c(NA,test2$thiamethoxam[-n[1]])

test3a <- merge(testa,test2,by.x=8,by.y=6,all.x=T)
test3b <- merge(testb,test2,by.x=8,by.y=6,all.x=T)
test3c <- merge(testc,test2,by.x=8,by.y=6,all.x=T)
test3d <- merge(testd,test2,by.x=8,by.y=6,all.x=T)

n <- dim(test3a)
test3a$mortlag1 <- c(test3a$mortality2[-1],NA)
test3a$mortlag2 <- c(test3a$mortality2[-c(1:2)],rep(NA,2))
test3a$mortlag3 <- c(test3a$mortality2[-c(1:3)],rep(NA,3))
test3a$mortlag4 <- c(test3a$mortality2[-c(1:4)],rep(NA,4))
test3a$mortlag5 <- c(test3a$mortality2[-c(1:5)],rep(NA,5))
test3a$mort_lag1 <- c(NA,test3a$mortality2[-n[1]])
test3a$mort_lag2 <- c(NA,NA,test3a$mortality2[-c(n[1]-1,n[1])])

n <- dim(test3b)
test3b$mortlag1 <- c(test3b$mortality2[-1],NA)
test3b$mortlag2 <- c(test3b$mortality2[-c(1:2)],rep(NA,2))
test3b$mortlag3 <- c(test3b$mortality2[-c(1:3)],rep(NA,3))
test3b$mortlag4 <- c(test3b$mortality2[-c(1:4)],rep(NA,4))
test3b$mortlag5 <- c(test3b$mortality2[-c(1:5)],rep(NA,5))
test3b$mort_lag1 <- c(NA,test3b$mortality2[-n[1]])
test3b$mort_lag2 <- c(NA,NA,test3b$mortality2[-c(n[1]-1,n[1])])

n <- dim(test3c)
test3c$mortlag1 <- c(test3c$mortality2[-1],NA)
test3c$mortlag2 <- c(test3c$mortality2[-c(1:2)],rep(NA,2))
test3c$mortlag3 <- c(test3c$mortality2[-c(1:3)],rep(NA,3))
test3c$mortlag4 <- c(test3c$mortality2[-c(1:4)],rep(NA,4))
test3c$mortlag5 <- c(test3c$mortality2[-c(1:5)],rep(NA,5))
test3c$mort_lag1 <- c(NA,test3c$mortality2[-n[1]])
test3c$mort_lag2 <- c(NA,NA,test3c$mortality2[-c(n[1]-1,n[1])])

n <- dim(test3d)
test3d$mortlag1 <- c(test3d$mortality2[-1],NA)
test3d$mortlag2 <- c(test3d$mortality2[-c(1:2)],rep(NA,2))
test3d$mortlag3 <- c(test3d$mortality2[-c(1:3)],rep(NA,3))
test3d$mortlag4 <- c(test3d$mortality2[-c(1:4)],rep(NA,4))
test3d$mortlag5 <- c(test3d$mortality2[-c(1:5)],rep(NA,5))
test3d$mort_lag1 <- c(NA,test3d$mortality2[-n[1]])
test3d$mort_lag2 <- c(NA,NA,test3d$mortality2[-c(n[1]-1,n[1])])

test3 <- rbind(test3a,test3b,test3c,test3d)

test4 <- test3

test4$dinotefuran[is.na(test4$dinotefuran)] <- 0
test4$nitenpyram[is.na(test4$nitenpyram)] <- 0
test4$thiamethoxam[is.na(test4$thiamethoxam)] <- 0
test4$clothianidin[is.na(test4$clothianidin)] <- 0
test4$imidacloprid[is.na(test4$imidacloprid)] <- 0
test4$acetamiprid[is.na(test4$acetamiprid)] <- 0
test4$thiacloprid[is.na(test4$thiacloprid)] <- 0

test5 <- cor(test3[,c(11:14,17,26:37)],use="pairwise.complete.obs")

test6 <- cor(test4[,c(11:14,17,26:37)],use="pairwise.complete.obs")

test6[c(6:12),c(5,13:16)]

plot(test4$clothianidin,test4$mortlag3,pch=16)

par(mfrow=c(3,1),mai=c(.5,1,0,0))
plot(test4$julian,test4$mortality2,type="b",xlim=c(110,180))

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(test4$julian,test4$mortlag3,type="b",xlim=c(110,180))
abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(test4$julian,test4$clothianidin,type="b",xlim=c(110,180))

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

test4$clothianidinspline <- test4$clothianidin
test4$clothianidinspline[test4$julian<146] <- 0
test4$clothianidinsplinelag1 <- test4$clothianidinlag1
test4$clothianidinsplinelag1[test4$clothianidinlag1<9] <- 0
test4$thiamethoxamlag1[is.na(test4$thiamethoxamlag1)&test4$julian>140] <- 0 
test4$thiamethoxamlag1[is.na(test4$thiamethoxamlag1)&test4$julian<120] <- 7 

n <- dim(test4)
test4$i122 <- rep(0,n[1])
test4$i122[test4$julian==122] <- 1
test4$i125 <- rep(0,n[1])
test4$i125[test4$julian==125] <- 1
test4$i128 <- rep(0,n[1])
test4$i128[test4$julian==128] <- 1
test4$i143 <- rep(0,n[1])
test4$i143[test4$julian==143] <- 1

test4$i145p <- rep(0,n[1])
test4$i145p[test4$julian>145] <- 1

testa <- deadbees[deadbees$hive=="2015FSR8",]
testb <- deadbees[deadbees$hive=="2015FSR9",]
testc <- deadbees[deadbees$hive=="2015FSR21",]
testd <- deadbees[deadbees$hive=="2015FSR27",]

test4$ifsr8 <- rep(0,n[1])
test4$ifsr8[test4$hive=="2015FSR8"] <- 1
test4$clofsr8 <- test4$clothianidin*test4$ifsr8
#test4$clofsr8lag3 <- test4$clothianidinlag3*test4$ifsr8
test4$thiafsr8 <- test4$thiamethoxam*test4$ifsr8

test4$ifsr9 <- rep(0,n[1])
test4$ifsr9[test4$hive=="2015FSR9"] <- 1
test4$clofsr9 <- test4$clothianidin*test4$ifsr9
#test4$clofsr9lag3 <- test4$clothianidinlag3*test4$ifsr9
test4$thiafsr9 <- test4$thiamethoxam*test4$ifsr9

test4$ifsr21 <- rep(0,n[1])
test4$ifsr21[test4$hive=="2015FSR21"] <- 1
test4$clofsr21 <- test4$clothianidin*test4$ifsr21
#test4$clofsr21lag3 <- test4$clothianidinlag3*test4$ifsr21
test4$thiafsr21 <- test4$thiamethoxam*test4$ifsr21

test4$ifsr27 <- rep(0,n[1])
test4$ifsr27[test4$hive=="2015FSR27"] <- 1
test4$clofsr27 <- test4$clothianidin*test4$ifsr27
#test4$clofsr27lag3 <- test4$clothianidinlag3*test4$ifsr27
test4$thiafsr27 <- test4$thiamethoxam*test4$ifsr27

test4$clothia <- test4$clothianidin+test4$thiamethoxam

test.lm <- lm(mortality2 ~ clothianidin+i143+thiamethoxamlag1,data=test4,na.action="na.exclude")
summary(test.lm)

test4$resid <- test4$mortality2-(test.lm$coef[1]+test.lm$coef[2]*test4$clothianidin+test.lm$coef[3]*test4$i143+test.lm$coef[4]*test4$thiamethoxamlag1)
test4$residlag1 <- c(NA,test4$resid[-n[1]])
test4$residlag1[is.na(test4$mort_lag1)] <- NA

plot(test4$julian,test4$resid,type="p")

Coefficients:
                 Estimate Std. Error t value Pr(>|t|)    
(Intercept)      0.027742   0.023411   1.185 0.244732    
clothianidin     0.007580   0.002085   3.636 0.000963 ***
i143             0.282420   0.056844   4.968 2.18e-05 ***
thiamethoxamlag1 0.011420   0.002185   5.227 1.03e-05 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.1044 on 32 degrees of freedom
  (4 observations deleted due to missingness)
Multiple R-squared:  0.8725,    Adjusted R-squared:  0.8606 
F-statistic: 73.02 on 3 and 32 DF,  p-value: 2.102e-14


test.lm <- lm(mortality2 ~ thiamethoxam+i125+i143,data=test4,na.action="na.exclude")
summary(test.lm)

test4$resid <- test4$mortality2-(test.lm$coef[1]+test.lm$coef[2]*test4$thiamethoxam+test.lm$coef[3]*test4$i125+test.lm$coef[4]*test4$i143)
test4$residlag1 <- c(NA,test4$resid[-n[1]])
test4$residlag1[is.na(test4$mort_lag1)] <- NA

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept)  0.089580   0.027047   3.312  0.00212 ** 
thiamethoxam 0.014615   0.001582   9.239 4.93e-11 ***
i125         0.553200   0.073484   7.528 6.74e-09 ***
i143         0.220110   0.072333   3.043  0.00436 ** 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.1347 on 36 degrees of freedom
Multiple R-squared:  0.8373,    Adjusted R-squared:  0.8237 
F-statistic: 61.76 on 3 and 36 DF,  p-value: 2.867e-14

# end 2015MM ---------------------------------------

# 05-29-18 2015HR clothianidin cor best w mortlag3 -----------------------------

test <- deadbees[deadbees$hive=="2015HR54",]
test2 <- pesticide2015[pesticide2015$site=="HR",]
test3 <- merge(test,test2,by.x=8,by.y=6,all.x=T)

n <- dim(test3)
test3$mortlag1 <- c(test3$mortality2[-1],NA)
test3$mortlag2 <- c(test3$mortality2[-c(1:2)],rep(NA,2))
test3$mortlag3 <- c(test3$mortality2[-c(1:3)],rep(NA,3))
test3$mortlag4 <- c(test3$mortality2[-c(1:4)],rep(NA,4))
test3$mortlag5 <- c(test3$mortality2[-c(1:5)],rep(NA,5))

test4 <- test3

test4$dinotefuran[is.na(test4$dinotefuran)] <- 0
test4$nitenpyram[is.na(test4$nitenpyram)] <- 0
test4$thiamethoxam[is.na(test4$thiamethoxam)] <- 0
test4$clothianidin[is.na(test4$clothianidin)] <- 0
test4$imidacloprid[is.na(test4$imidacloprid)] <- 0
test4$acetamiprid[is.na(test4$acetamiprid)] <- 0
test4$thiacloprid[is.na(test4$thiacloprid)] <- 0

test5 <- cor(test3[,c(11:14,17,26:37)],use="pairwise.complete.obs")

test6 <- cor(test4[,c(11:14,17,26:37)],use="pairwise.complete.obs")

# 05-29-18 HR5 mortality cor best w thiamethoxam, clothianidin, mortlag3 cor best w dinotefuran, thiacloprid
# 05-29-18 HR6 mortality cor best w thiamethoxam, clothianidin, mortlag3 cor best w dinotefuran, thiacloprid
# 05-29-18 HR20 mortality cor best w thiamethoxam, clothianidin, mortlag3 cor best w dinotefuran, thiacloprid
# 05-29-18 HR51 mortality cor best w thiamethoxam, clothianidin, mortlag3 cor best w dinotefuran, thiacloprid
# 05-29-18 HR53 mortality cor best w thiamethoxam, clothianidin, mortlag3 cor best w dinotefuran, thiacloprid
# 05-29-18 HR54 mortality cor best w thiamethoxam, clothianidin, mortlag3 cor best w dinotefuran, thiacloprid

# combine 6 colonies

              mortality2   mortlag1     mortlag2   mortlag3    mortlag4
dinotefuran  -0.07958075 -0.2180142 -0.009446363  0.9274781  0.53803065
nitenpyram   -0.18916093 -0.1501936  0.359032721 -0.1325503  0.14566504
thiamethoxam  0.79194026  0.1020324 -0.282421060 -0.3003975 -0.32321849
clothianidin  0.75567767  0.3948861  0.065368819 -0.3535106 -0.26854124
imidacloprid -0.12483447 -0.3286358 -0.481213132 -0.6355649 -0.53667769
acetamiprid  -0.12507520 -0.2200961 -0.169860381 -0.1547566 -0.03587439
thiacloprid  -0.33605062 -0.4457669 -0.293222823  0.6922738  0.46837114

testa <- deadbees[deadbees$hive=="2015HR5",]
testb <- deadbees[deadbees$hive=="2015HR6",]
testc <- deadbees[deadbees$hive=="2015HR20",]
testd <- deadbees[deadbees$hive=="2015HR51",]
teste <- deadbees[deadbees$hive=="2015HR53",]
testf <- deadbees[deadbees$hive=="2015HR54",]

test2 <- pesticide2015[pesticide2015$site=="HR",]
n <- dim(test2)
test2$clothianidinlag1 <- c(NA,test2$clothianidin[-n[1]])
test2$thiamethoxamlag1 <- c(NA,test2$thiamethoxam[-n[1]])

test3a <- merge(testa,test2,by.x=8,by.y=6,all.x=T)
test3b <- merge(testb,test2,by.x=8,by.y=6,all.x=T)
test3c <- merge(testc,test2,by.x=8,by.y=6,all.x=T)
test3d <- merge(testd,test2,by.x=8,by.y=6,all.x=T)
test3e <- merge(teste,test2,by.x=8,by.y=6,all.x=T)
test3f <- merge(testf,test2,by.x=8,by.y=6,all.x=T)

n <- dim(test3a)
test3a$mortlag1 <- c(test3a$mortality2[-1],NA)
test3a$mortlag2 <- c(test3a$mortality2[-c(1:2)],rep(NA,2))
test3a$mortlag3 <- c(test3a$mortality2[-c(1:3)],rep(NA,3))
test3a$mortlag4 <- c(test3a$mortality2[-c(1:4)],rep(NA,4))
test3a$mortlag5 <- c(test3a$mortality2[-c(1:5)],rep(NA,5))
test3a$mort_lag1 <- c(NA,test3a$mortality2[-n[1]])
test3a$mort_lag2 <- c(NA,NA,test3a$mortality2[-c(n[1]-1,n[1])])

n <- dim(test3b)
test3b$mortlag1 <- c(test3b$mortality2[-1],NA)
test3b$mortlag2 <- c(test3b$mortality2[-c(1:2)],rep(NA,2))
test3b$mortlag3 <- c(test3b$mortality2[-c(1:3)],rep(NA,3))
test3b$mortlag4 <- c(test3b$mortality2[-c(1:4)],rep(NA,4))
test3b$mortlag5 <- c(test3b$mortality2[-c(1:5)],rep(NA,5))
test3b$mort_lag1 <- c(NA,test3b$mortality2[-n[1]])
test3b$mort_lag2 <- c(NA,NA,test3b$mortality2[-c(n[1]-1,n[1])])

n <- dim(test3c)
test3c$mortlag1 <- c(test3c$mortality2[-1],NA)
test3c$mortlag2 <- c(test3c$mortality2[-c(1:2)],rep(NA,2))
test3c$mortlag3 <- c(test3c$mortality2[-c(1:3)],rep(NA,3))
test3c$mortlag4 <- c(test3c$mortality2[-c(1:4)],rep(NA,4))
test3c$mortlag5 <- c(test3c$mortality2[-c(1:5)],rep(NA,5))
test3c$mort_lag1 <- c(NA,test3c$mortality2[-n[1]])
test3c$mort_lag2 <- c(NA,NA,test3c$mortality2[-c(n[1]-1,n[1])])

n <- dim(test3d)
test3d$mortlag1 <- c(test3d$mortality2[-1],NA)
test3d$mortlag2 <- c(test3d$mortality2[-c(1:2)],rep(NA,2))
test3d$mortlag3 <- c(test3d$mortality2[-c(1:3)],rep(NA,3))
test3d$mortlag4 <- c(test3d$mortality2[-c(1:4)],rep(NA,4))
test3d$mortlag5 <- c(test3d$mortality2[-c(1:5)],rep(NA,5))
test3d$mort_lag1 <- c(NA,test3d$mortality2[-n[1]])
test3d$mort_lag2 <- c(NA,NA,test3d$mortality2[-c(n[1]-1,n[1])])

n <- dim(test3e)
test3e$mortlag1 <- c(test3e$mortality2[-1],NA)
test3e$mortlag2 <- c(test3e$mortality2[-c(1:2)],rep(NA,2))
test3e$mortlag3 <- c(test3e$mortality2[-c(1:3)],rep(NA,3))
test3e$mortlag4 <- c(test3e$mortality2[-c(1:4)],rep(NA,4))
test3e$mortlag5 <- c(test3e$mortality2[-c(1:5)],rep(NA,5))
test3e$mort_lag1 <- c(NA,test3e$mortality2[-n[1]])
test3e$mort_lag2 <- c(NA,NA,test3e$mortality2[-c(n[1]-1,n[1])])

n <- dim(test3f)
test3f$mortlag1 <- c(test3f$mortality2[-1],NA)
test3f$mortlag2 <- c(test3f$mortality2[-c(1:2)],rep(NA,2))
test3f$mortlag3 <- c(test3f$mortality2[-c(1:3)],rep(NA,3))
test3f$mortlag4 <- c(test3f$mortality2[-c(1:4)],rep(NA,4))
test3f$mortlag5 <- c(test3f$mortality2[-c(1:5)],rep(NA,5))
test3f$mort_lag1 <- c(NA,test3f$mortality2[-n[1]])
test3f$mort_lag2 <- c(NA,NA,test3f$mortality2[-c(n[1]-1,n[1])])

test3 <- rbind(test3a,test3b,test3c,test3d,test3e,test3f)

test4 <- test3

test4$dinotefuran[is.na(test4$dinotefuran)] <- 0
test4$nitenpyram[is.na(test4$nitenpyram)] <- 0
test4$thiamethoxam[is.na(test4$thiamethoxam)] <- 0
test4$clothianidin[is.na(test4$clothianidin)] <- 0
test4$imidacloprid[is.na(test4$imidacloprid)] <- 0
test4$acetamiprid[is.na(test4$acetamiprid)] <- 0
test4$thiacloprid[is.na(test4$thiacloprid)] <- 0

test5 <- cor(test3[,c(11:14,17,26:37)],use="pairwise.complete.obs")

test6 <- cor(test4[,c(11:14,17,26:37)],use="pairwise.complete.obs")

test6[c(6:12),c(5,13:16)]

plot(test4$clothianidin,test4$mortlag3,pch=16)

par(mfrow=c(3,1),mai=c(.5,1,0,0))
plot(test4$julian,test4$mortality2,type="b",xlim=c(110,180))

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(test4$julian,test4$mortlag3,type="b",xlim=c(110,180))
abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(test4$julian,test4$clothianidin,type="b",xlim=c(110,180))

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

n <- dim(test4)
test4$clothianidinspline <- test4$clothianidin
test4$clothianidinspline[test4$clothianidin<9] <- 0
test4$clothianidinsplinelag1 <- test4$clothianidinlag1
test4$clothianidinsplinelag1[test4$clothianidinlag1<9] <- 0

test4$i147 <- rep(0,n[1])
test4$i147[test4$julian==147] <- 1
test4$i131 <- rep(0,n[1])
test4$i131[test4$julian==131] <- 1
test4$i134 <- rep(0,n[1])
test4$i134[test4$julian==134] <- 1
test4$i119 <- rep(0,n[1])
test4$i119[test4$julian==119] <- 1
test4$i139 <- rep(0,n[1])
test4$i139[test4$julian==139] <- 1

test4$i141p <- rep(0,n[1])
test4$i141p[test4$julian>141] <- 1

testa <- deadbees[deadbees$hive=="2015HR5",]
testb <- deadbees[deadbees$hive=="2015HR6",]
testc <- deadbees[deadbees$hive=="2015HR20",]
testd <- deadbees[deadbees$hive=="2015HR51",]
teste <- deadbees[deadbees$hive=="2015HR53",]
testf <- deadbees[deadbees$hive=="2015HR54",]

test4$ihr5 <- rep(0,n[1])
test4$ihr5[test4$hive=="2015HR5"] <- 1
test4$clohr5 <- test4$clothianidin*test4$ihr5
#test4$clohr5lag3 <- test4$clothianidinlag3*test4$ihr5
test4$thiahr5 <- test4$thiamethoxam*test4$ihr5

test4$ihr6 <- rep(0,n[1])
test4$ihr6[test4$hive=="2015HR6"] <- 1
test4$clohr6 <- test4$clothianidin*test4$ihr6
#test4$clohr6lag3 <- test4$clothianidinlag3*test4$ihr6
test4$thiahr6 <- test4$thiamethoxam*test4$ihr6

test4$ihr20 <- rep(0,n[1])
test4$ihr20[test4$hive=="2015HR20"] <- 1
test4$clohr20 <- test4$clothianidin*test4$ihr20
#test4$clohr20lag3 <- test4$clothianidinlag3*test4$ihr20
test4$thiahr20 <- test4$thiamethoxam*test4$ihr20

test4$ihr51 <- rep(0,n[1])
test4$ihr51[test4$hive=="2015HR51"] <- 1
test4$clohr51 <- test4$clothianidin*test4$ihr51
#test4$clohr51lag3 <- test4$clothianidinlag3*test4$ihr51
test4$thiahr51 <- test4$thiamethoxam*test4$ihr51

test4$ihr53 <- rep(0,n[1])
test4$ihr53[test4$hive=="2015HR53"] <- 1
test4$clohr53 <- test4$clothianidin*test4$ihr53
#test4$clohr53lag3 <- test4$clothianidinlag3*test4$ihr53
test4$thiahr53 <- test4$thiamethoxam*test4$ihr53

test.lm <- lm(mortality2 ~ thiamethoxam+ihr5+ihr6+ihr20+residlag1,data=test4,na.action="na.exclude")
summary(test.lm)

#test4$res <- test.lm$res

test4$resid <- test4$mortality2-(test.lm$coef[1]+test.lm$coef[2]*test4$thiamethoxam+test.lm$coef[3]*test4$ihr5+test.lm$coef[4]*test4$ihr6+test.lm$coef[5]*test4$ihr20)
test4$residlag1 <- c(NA,test4$resid[-n[1]])
test4$residlag1[is.na(test4$mort_lag1)] <- NA

plot(test4$julian,test4$resid)

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)   0.239813   0.034500   6.951 2.50e-08 ***
thiamethoxam  0.052602   0.003996  13.164 6.24e-16 ***
ihr5         -0.193198   0.053559  -3.607 0.000868 ***
ihr6         -0.156053   0.053558  -2.914 0.005886 ** 
ihr20        -0.137517   0.053255  -2.582 0.013686 *  
residlag1     0.211888   0.117110   1.809 0.078117 .  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.1299 on 39 degrees of freedom
  (6 observations deleted due to missingness)
Multiple R-squared:  0.8482,    Adjusted R-squared:  0.8287 
F-statistic: 43.58 on 5 and 39 DF,  p-value: 6.099e-15


# end 2015HR ---------------------------------------

# 05-29-18 2015IB clothianidin cor best w mortlag3 -----------------------------

test <- deadbees[deadbees$hive=="2015IB67",]
test2 <- pesticide2015[pesticide2015$site=="IB",]
test3 <- merge(test,test2,by.x=8,by.y=6,all.x=T)

n <- dim(test3)
test3$mortlag1 <- c(test3$mortality2[-1],NA)
test3$mortlag2 <- c(test3$mortality2[-c(1:2)],rep(NA,2))
test3$mortlag3 <- c(test3$mortality2[-c(1:3)],rep(NA,3))
test3$mortlag4 <- c(test3$mortality2[-c(1:4)],rep(NA,4))
test3$mortlag5 <- c(test3$mortality2[-c(1:5)],rep(NA,5))

test4 <- test3

test4$dinotefuran[is.na(test4$dinotefuran)] <- 0
test4$nitenpyram[is.na(test4$nitenpyram)] <- 0
test4$thiamethoxam[is.na(test4$thiamethoxam)] <- 0
test4$clothianidin[is.na(test4$clothianidin)] <- 0
test4$imidacloprid[is.na(test4$imidacloprid)] <- 0
test4$acetamiprid[is.na(test4$acetamiprid)] <- 0
test4$thiacloprid[is.na(test4$thiacloprid)] <- 0

test5 <- cor(test3[,c(11:14,17,26:37)],use="pairwise.complete.obs")

test6 <- cor(test4[,c(11:14,17,26:37)],use="pairwise.complete.obs")

# 05-29-18 IB66 mortality cor best w clothianidin, mortlag3 cor best w dinotefuran, mortlag1 cor best w acetamiprid, mortlag4 cor best w imidacloprid
# 05-29-18 IB67 mortality cor best w clothianidin, mortlag1 cor best w dinotefuran, thiacloprid, mortlag4 cor best w thiamethoxam

# combine 2 colonies

              mortality2   mortlag1    mortlag2   mortlag3    mortlag4
dinotefuran  -0.06574499  0.3109139  0.33998346  0.3437931 -0.29674757
nitenpyram    0.08527394  0.3151274 -0.02468888 -0.1965144  0.04208517
thiamethoxam -0.07936636 -0.2035259 -0.09424445 -0.3772252  0.19419425
clothianidin  0.59260277  0.3160216 -0.01496245 -0.2505736 -0.36442488
imidacloprid -0.06343592  0.1712351  0.06817459  0.3199137  0.39059452
acetamiprid   0.12173105  0.3340523  0.13623097  0.1526934  0.26430681
thiacloprid  -0.06720977  0.2363831 -0.19927154 -0.1432244 -0.18096310

testa <- deadbees[deadbees$hive=="2015IB66",]
testb <- deadbees[deadbees$hive=="2015IB67",]

test2 <- pesticide2015[pesticide2015$site=="IB",]
n <- dim(test2)
test2$clothianidinlag1 <- c(NA,test2$clothianidin[-n[1]])
test2$thiamethoxamlag1 <- c(NA,test2$thiamethoxam[-n[1]])

test3a <- merge(testa,test2,by.x=8,by.y=6,all.x=T)
test3b <- merge(testb,test2,by.x=8,by.y=6,all.x=T)

n <- dim(test3a)
test3a$mortlag1 <- c(test3a$mortality2[-1],NA)
test3a$mortlag2 <- c(test3a$mortality2[-c(1:2)],rep(NA,2))
test3a$mortlag3 <- c(test3a$mortality2[-c(1:3)],rep(NA,3))
test3a$mortlag4 <- c(test3a$mortality2[-c(1:4)],rep(NA,4))
test3a$mortlag5 <- c(test3a$mortality2[-c(1:5)],rep(NA,5))
test3a$mort_lag1 <- c(NA,test3a$mortality2[-n[1]])
test3a$mort_lag2 <- c(NA,NA,test3a$mortality2[-c(n[1]-1,n[1])])

n <- dim(test3b)
test3b$mortlag1 <- c(test3b$mortality2[-1],NA)
test3b$mortlag2 <- c(test3b$mortality2[-c(1:2)],rep(NA,2))
test3b$mortlag3 <- c(test3b$mortality2[-c(1:3)],rep(NA,3))
test3b$mortlag4 <- c(test3b$mortality2[-c(1:4)],rep(NA,4))
test3b$mortlag5 <- c(test3b$mortality2[-c(1:5)],rep(NA,5))
test3b$mort_lag1 <- c(NA,test3b$mortality2[-n[1]])
test3b$mort_lag2 <- c(NA,NA,test3b$mortality2[-c(n[1]-1,n[1])])

test3 <- rbind(test3a,test3b)

test4 <- test3

test4$dinotefuran[is.na(test4$dinotefuran)] <- 0
test4$nitenpyram[is.na(test4$nitenpyram)] <- 0
test4$thiamethoxam[is.na(test4$thiamethoxam)] <- 0
test4$clothianidin[is.na(test4$clothianidin)] <- 0
test4$imidacloprid[is.na(test4$imidacloprid)] <- 0
test4$acetamiprid[is.na(test4$acetamiprid)] <- 0
test4$thiacloprid[is.na(test4$thiacloprid)] <- 0

test5 <- cor(test3[,c(11:14,17,26:37)],use="pairwise.complete.obs")

test6 <- cor(test4[,c(11:14,17,26:37)],use="pairwise.complete.obs")

test6[c(6:12),c(5,13:16)]

plot(test4$clothianidin,test4$mortlag3,pch=16)

par(mfrow=c(3,1),mai=c(.5,1,0,0))
plot(test4$julian,test4$mortality2,type="b",xlim=c(110,180))

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(test4$julian,test4$mortlag3,type="b",xlim=c(110,180))
abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(test4$julian,test4$clothianidin,type="b",xlim=c(110,180))

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

test4$clothianidinspline <- test4$clothianidin
test4$clothianidinspline[test4$clothianidin<9] <- 0
test4$clothianidinsplinelag1 <- test4$clothianidinlag1
test4$clothianidinsplinelag1[test4$clothianidinlag1<9] <- 0

test4$i147 <- rep(0,n[1])
test4$i147[test4$julian==147] <- 1
test4$i131 <- rep(0,n[1])
test4$i131[test4$julian==131] <- 1
test4$i128 <- rep(0,n[1])
test4$i128[test4$julian==128] <- 1

test4$i142p <- rep(0,n[1])
test4$i142p[test4$julian>142] <- 1

test4$i137p <- rep(0,n[1])
test4$i137p[test4$julian>137] <- 1

test4$iib66 <- rep(0,n[1])
test4$iib66[test4$hive=="2015IB66"] <- 1

test4$cloib66 <- test4$clothianidin*test4$iib66

test4$thiaib66 <- test4$thiamethoxam*test4$iib66

test.lm <- lm(mortality2 ~ clothianidin+i142p,data=test4,na.action="na.exclude")
summary(test.lm)

#test4$res <- test.lm$res
n <- dim(test4)
test4$resid <- test4$mortality2-(test.lm$coef[1]+test.lm$coef[2]*test4$clothianidin+test.lm$coef[3]*test4$i142p)
test4$residlag1 <- c(NA,test4$resid[-n[1]])
test4$residlag1[is.na(test4$mort_lag1)] <- NA

plot(test4$julian,test4$resid)

Coefficients:
             Estimate Std. Error t value Pr(>|t|)   
(Intercept)   0.03197    0.10575   0.302  0.76605   
clothianidin  0.06316    0.01598   3.953  0.00103 **
i142p         0.29086    0.13993   2.079  0.05310 . 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.2245 on 17 degrees of freedom
Multiple R-squared:  0.4827,    Adjusted R-squared:  0.4218 
F-statistic:  7.93 on 2 and 17 DF,  p-value: 0.00369


# end 2015IB ---------------------------------------

# 05-30-18 2015MB  -----------------------------

test <- deadbees[deadbees$hive=="2015MB36",]
test2 <- pesticide2015[pesticide2015$site=="MC",]
test3 <- merge(test,test2,by.x=8,by.y=6,all.x=T)

n <- dim(test3)
test3$mortlag1 <- c(test3$mortality2[-1],NA)
test3$mortlag2 <- c(test3$mortality2[-c(1:2)],rep(NA,2))
test3$mortlag3 <- c(test3$mortality2[-c(1:3)],rep(NA,3))
test3$mortlag4 <- c(test3$mortality2[-c(1:4)],rep(NA,4))
test3$mortlag5 <- c(test3$mortality2[-c(1:5)],rep(NA,5))

test4 <- test3

test4$dinotefuran[is.na(test4$dinotefuran)] <- 0
test4$nitenpyram[is.na(test4$nitenpyram)] <- 0
test4$thiamethoxam[is.na(test4$thiamethoxam)] <- 0
test4$clothianidin[is.na(test4$clothianidin)] <- 0
test4$imidacloprid[is.na(test4$imidacloprid)] <- 0
test4$acetamiprid[is.na(test4$acetamiprid)] <- 0
test4$thiacloprid[is.na(test4$thiacloprid)] <- 0

test5 <- cor(test3[,c(11:14,17,26:37)],use="pairwise.complete.obs")

test6 <- cor(test4[,c(11:14,17,26:37)],use="pairwise.complete.obs")

# 05-30-18 MB10 clothianidin cor w mortality, mortlag1, mortlag3, acetamiprid, thiamethoxam cor w mortality, nitenpyram cor w mortality-mortlag3,
# 05-30-18 MB10 cont. thiacloprid, imidacloprid cor w mortlag3, 
# 05-30-18 MB11 clothianidin cor w mortality, mortlag1, mortlag3, thiamethoxam cor w mortality, mortlag1, nitenpyram cor w mortality-mortlag3,
# 05-30-18 MB11 cont. acetamiprid cor w mortality, thiacloprid cor w mortlag2-mortlag3
# 05-30-18 MB23 clothianidin cor w mortality, mortlag3 (0.66), thiamethoxam cor w mortlag1, nitenpyram cor w mortlag1-mortlag3,
# 05-30-18 MB23 cont. acetamiprid cor w mortality (0.54), thiacloprid cor w mortlag2, imidacloprid cor w mortlag3, 
# 05-30-18 MB36 dinotefuran cor w mortlag1 (0.61), nitenpyram cor w mortality, clothianidin cor w mortality (0.58),  
# 05-30-18 MB36 cont. imidacloprid cor w mortality(0.73), mortlag3 (0.68), thiacloprid cor w mortality,

# combine 4 colonies
# 05-30-18 thiamethoxam cor w mortality (0.28), clothianidin cor w mortality (0.41),mortlag1 (0.51), imidacloprid cor w mortlag3 (0.36),
acetamiprid cor w mortality (0.43), 

              mortality2    mortlag1   mortlag2    mortlag3    mortlag4
dinotefuran  -0.07817937 -0.02882681 -0.2336726          NA          NA
nitenpyram    0.31222182  0.24576932  0.2466899  0.04523457  0.22679205
thiamethoxam  0.28124801  0.22942236 -0.2642141  0.11523216 -0.21414011
clothianidin  0.40562385  0.50795197 -0.3308377  0.35161535 -0.09753599
imidacloprid  0.19236950  0.12263717 -0.5791562  0.35727619 -0.14185651
acetamiprid   0.42569983 -0.21218781  0.2702506 -0.11684237 -0.07783240
thiacloprid   0.08234082  0.15873931  0.2489447  0.06624278  0.30562878

testa <- deadbees[deadbees$hive=="2015MB10",]
testb <- deadbees[deadbees$hive=="2015MB11",]
testc <- deadbees[deadbees$hive=="2015MB23",]
testd <- deadbees[deadbees$hive=="2015MB36",]

test2 <- pesticide2015[pesticide2015$site=="MC",]
n <- dim(test2)
test2$clothianidinlag1 <- c(NA,test2$clothianidin[-n[1]])
test2$thiamethoxamlag1 <- c(NA,test2$thiamethoxam[-n[1]])

test3a <- merge(testa,test2,by.x=8,by.y=6,all.x=T)
test3b <- merge(testb,test2,by.x=8,by.y=6,all.x=T)
test3c <- merge(testc,test2,by.x=8,by.y=6,all.x=T)
test3d <- merge(testd,test2,by.x=8,by.y=6,all.x=T)

n <- dim(test3a)
test3a$mortlag1 <- c(test3a$mortality2[-1],NA)
test3a$mortlag2 <- c(test3a$mortality2[-c(1:2)],rep(NA,2))
test3a$mortlag3 <- c(test3a$mortality2[-c(1:3)],rep(NA,3))
test3a$mortlag4 <- c(test3a$mortality2[-c(1:4)],rep(NA,4))
test3a$mortlag5 <- c(test3a$mortality2[-c(1:5)],rep(NA,5))
test3a$mort_lag1 <- c(NA,test3a$mortality2[-n[1]])
test3a$mort_lag2 <- c(NA,NA,test3a$mortality2[-c(n[1]-1,n[1])])

n <- dim(test3b)
test3b$mortlag1 <- c(test3b$mortality2[-1],NA)
test3b$mortlag2 <- c(test3b$mortality2[-c(1:2)],rep(NA,2))
test3b$mortlag3 <- c(test3b$mortality2[-c(1:3)],rep(NA,3))
test3b$mortlag4 <- c(test3b$mortality2[-c(1:4)],rep(NA,4))
test3b$mortlag5 <- c(test3b$mortality2[-c(1:5)],rep(NA,5))
test3b$mort_lag1 <- c(NA,test3b$mortality2[-n[1]])
test3b$mort_lag2 <- c(NA,NA,test3b$mortality2[-c(n[1]-1,n[1])])

n <- dim(test3c)
test3c$mortlag1 <- c(test3c$mortality2[-1],NA)
test3c$mortlag2 <- c(test3c$mortality2[-c(1:2)],rep(NA,2))
test3c$mortlag3 <- c(test3c$mortality2[-c(1:3)],rep(NA,3))
test3c$mortlag4 <- c(test3c$mortality2[-c(1:4)],rep(NA,4))
test3c$mortlag5 <- c(test3c$mortality2[-c(1:5)],rep(NA,5))
test3c$mort_lag1 <- c(NA,test3c$mortality2[-n[1]])
test3c$mort_lag2 <- c(NA,NA,test3c$mortality2[-c(n[1]-1,n[1])])

n <- dim(test3d)
test3d$mortlag1 <- c(test3d$mortality2[-1],NA)
test3d$mortlag2 <- c(test3d$mortality2[-c(1:2)],rep(NA,2))
test3d$mortlag3 <- c(test3d$mortality2[-c(1:3)],rep(NA,3))
test3d$mortlag4 <- c(test3d$mortality2[-c(1:4)],rep(NA,4))
test3d$mortlag5 <- c(test3d$mortality2[-c(1:5)],rep(NA,5))
test3d$mort_lag1 <- c(NA,test3d$mortality2[-n[1]])
test3d$mort_lag2 <- c(NA,NA,test3d$mortality2[-c(n[1]-1,n[1])])

test3 <- rbind(test3a,test3b,test3c,test3d)

test4 <- test3

test4$dinotefuran[is.na(test4$dinotefuran)] <- 0
test4$nitenpyram[is.na(test4$nitenpyram)] <- 0
test4$thiamethoxam[is.na(test4$thiamethoxam)] <- 0
test4$clothianidin[is.na(test4$clothianidin)] <- 0
test4$imidacloprid[is.na(test4$imidacloprid)] <- 0
test4$acetamiprid[is.na(test4$acetamiprid)] <- 0
test4$thiacloprid[is.na(test4$thiacloprid)] <- 0

test5 <- cor(test3[,c(11:14,17,26:37)],use="pairwise.complete.obs")

test6 <- cor(test4[,c(11:14,17,26:37)],use="pairwise.complete.obs")

test6[c(6:12),c(5,13:16)]

plot(test4$clothianidin,test4$mortlag3,pch=16)

par(mfrow=c(3,1),mai=c(.5,1,0,0))
plot(test4$julian,test4$mortality2,type="b",xlim=c(110,180))

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(test4$julian,test4$mortlag3,type="b",xlim=c(110,180))
abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(test4$julian,test4$clothianidin,type="b",xlim=c(110,180))

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

test4$clothianidinspline <- test4$clothianidin
test4$clothianidinspline[test4$clothianidin<9] <- 0
test4$clothianidinsplinelag1 <- test4$clothianidinlag1
test4$clothianidinsplinelag1[test4$clothianidinlag1<9] <- 0

test4$clothianidinlag1[test4$julian==154] <- 7

test4$i131 <- rep(0,n[1])
test4$i131[test4$julian==131] <- 1
test4$i125 <- rep(0,n[1])
test4$i125[test4$julian==125] <- 1
test4$i128 <- rep(0,n[1])
test4$i128[test4$julian==128] <- 1

test4$imb10 <- rep(0,n[1])
test4$imb10[test4$hive=="2015MB10"] <- 1

test4$imb11 <- rep(0,n[1])
test4$imb11[test4$hive=="2015MB11"] <- 1

test4$imb23 <- rep(0,n[1])
test4$imb23[test4$hive=="2015MB23"] <- 1

test4$clomb10 <- test4$clothianidin*test4$imb10
test4$clomb11 <- test4$clothianidin*test4$imb11
test4$clomb23 <- test4$clothianidin*test4$imb23

test.lm <- lm(mortality2 ~ clothianidin+clothianidinlag1+i131,data=test4,na.action="na.exclude")
summary(test.lm)

#test4$res <- test.lm$res
n <- dim(test4)
test4$resid <- test4$mortality2-(test.lm$coef[1]+test.lm$coef[2]*test4$clothianidin+test.lm$coef[3]*test4$clothianidinlag1+
test.lm$coef[4]*test4$i131)
test4$residlag1 <- c(NA,test4$resid[-n[1]])
test4$residlag1[is.na(test4$mort_lag1)] <- NA

plot(test4$julian,test4$resid)

Coefficients:
                 Estimate Std. Error t value Pr(>|t|)   
(Intercept)      0.029385   0.078977   0.372  0.71310   
clothianidin     0.038838   0.013473   2.883  0.00819 **
clothianidinlag1 0.025947   0.009761   2.658  0.01376 * 
i131             0.252314   0.113555   2.222  0.03597 * 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.1988 on 24 degrees of freedom
  (12 observations deleted due to missingness)
Multiple R-squared:  0.5953,    Adjusted R-squared:  0.5448 
F-statistic: 11.77 on 3 and 24 DF,  p-value: 6.147e-05


# end 2015MB ---------------------------------------

# 05-30-18 2015MM  -----------------------------

test <- deadbees[deadbees$hive=="2015MM1",]
test2 <- pesticide2015[pesticide2015$site=="MM",]
test3 <- merge(test,test2,by.x=8,by.y=6,all.x=T)

n <- dim(test3)
test3$mortlag1 <- c(test3$mortality2[-1],NA)
test3$mortlag2 <- c(test3$mortality2[-c(1:2)],rep(NA,2))
test3$mortlag3 <- c(test3$mortality2[-c(1:3)],rep(NA,3))
test3$mortlag4 <- c(test3$mortality2[-c(1:4)],rep(NA,4))
test3$mortlag5 <- c(test3$mortality2[-c(1:5)],rep(NA,5))

test4 <- test3

test4$dinotefuran[is.na(test4$dinotefuran)] <- 0
test4$nitenpyram[is.na(test4$nitenpyram)] <- 0
test4$thiamethoxam[is.na(test4$thiamethoxam)] <- 0
test4$clothianidin[is.na(test4$clothianidin)] <- 0
test4$imidacloprid[is.na(test4$imidacloprid)] <- 0
test4$acetamiprid[is.na(test4$acetamiprid)] <- 0
test4$thiacloprid[is.na(test4$thiacloprid)] <- 0

test5 <- cor(test3[,c(11:14,17,26:37)],use="pairwise.complete.obs")

test6 <- cor(test4[,c(11:14,17,26:37)],use="pairwise.complete.obs")

test6[c(6:12),c(5,13:16)]

# 05-30-18 combine 6 colonies

              mortality2    mortlag1   mortlag2    mortlag3    mortlag4
dinotefuran           NA          NA         NA          NA          NA
nitenpyram    0.45913982  0.33828438  0.1671270  0.40991867  0.31835624
thiamethoxam  0.42339874  0.50899488  0.1829649  0.07714630 -0.31966458
clothianidin  0.71523372  0.60725694  0.2330106 -0.08403629 -0.01115847
imidacloprid  0.19856544 -0.15922016 -0.5600181 -0.52585898 -0.33968257
acetamiprid   0.02789276 -0.04246891  0.1289316  0.66740616  0.56409515
thiacloprid  -0.30303512 -0.49199066 -0.3463435 -0.08727760 -0.18154765

testa <- deadbees[deadbees$hive=="2015MM1",]
testb <- deadbees[deadbees$hive=="2015MM2",]
testc <- deadbees[deadbees$hive=="2015MM16",]
testd <- deadbees[deadbees$hive=="2015MM37",]
teste <- deadbees[deadbees$hive=="2015MM39",]
testf <- deadbees[deadbees$hive=="2015MM40",]

test2 <- pesticide2015[pesticide2015$site=="MM",]
n <- dim(test2)
test2$clothianidinlag1 <- c(NA,test2$clothianidin[-n[1]])
test2$thiamethoxamlag1 <- c(NA,test2$thiamethoxam[-n[1]])

test3a <- merge(testa,test2,by.x=8,by.y=6,all.x=T)
test3b <- merge(testb,test2,by.x=8,by.y=6,all.x=T)
test3c <- merge(testc,test2,by.x=8,by.y=6,all.x=T)
test3d <- merge(testd,test2,by.x=8,by.y=6,all.x=T)
test3e <- merge(teste,test2,by.x=8,by.y=6,all.x=T)
test3f <- merge(testf,test2,by.x=8,by.y=6,all.x=T)

n <- dim(test3a)
test3a$mortlag1 <- c(test3a$mortality2[-1],NA)
test3a$mortlag2 <- c(test3a$mortality2[-c(1:2)],rep(NA,2))
test3a$mortlag3 <- c(test3a$mortality2[-c(1:3)],rep(NA,3))
test3a$mortlag4 <- c(test3a$mortality2[-c(1:4)],rep(NA,4))
test3a$mortlag5 <- c(test3a$mortality2[-c(1:5)],rep(NA,5))
test3a$mort_lag1 <- c(NA,test3a$mortality2[-n[1]])
test3a$mort_lag2 <- c(NA,NA,test3a$mortality2[-c(n[1]-1,n[1])])

n <- dim(test3b)
test3b$mortlag1 <- c(test3b$mortality2[-1],NA)
test3b$mortlag2 <- c(test3b$mortality2[-c(1:2)],rep(NA,2))
test3b$mortlag3 <- c(test3b$mortality2[-c(1:3)],rep(NA,3))
test3b$mortlag4 <- c(test3b$mortality2[-c(1:4)],rep(NA,4))
test3b$mortlag5 <- c(test3b$mortality2[-c(1:5)],rep(NA,5))
test3b$mort_lag1 <- c(NA,test3b$mortality2[-n[1]])
test3b$mort_lag2 <- c(NA,NA,test3b$mortality2[-c(n[1]-1,n[1])])

n <- dim(test3c)
test3c$mortlag1 <- c(test3c$mortality2[-1],NA)
test3c$mortlag2 <- c(test3c$mortality2[-c(1:2)],rep(NA,2))
test3c$mortlag3 <- c(test3c$mortality2[-c(1:3)],rep(NA,3))
test3c$mortlag4 <- c(test3c$mortality2[-c(1:4)],rep(NA,4))
test3c$mortlag5 <- c(test3c$mortality2[-c(1:5)],rep(NA,5))
test3c$mort_lag1 <- c(NA,test3c$mortality2[-n[1]])
test3c$mort_lag2 <- c(NA,NA,test3c$mortality2[-c(n[1]-1,n[1])])

n <- dim(test3d)
test3d$mortlag1 <- c(test3d$mortality2[-1],NA)
test3d$mortlag2 <- c(test3d$mortality2[-c(1:2)],rep(NA,2))
test3d$mortlag3 <- c(test3d$mortality2[-c(1:3)],rep(NA,3))
test3d$mortlag4 <- c(test3d$mortality2[-c(1:4)],rep(NA,4))
test3d$mortlag5 <- c(test3d$mortality2[-c(1:5)],rep(NA,5))
test3d$mort_lag1 <- c(NA,test3d$mortality2[-n[1]])
test3d$mort_lag2 <- c(NA,NA,test3d$mortality2[-c(n[1]-1,n[1])])

n <- dim(test3e)
test3e$mortlag1 <- c(test3e$mortality2[-1],NA)
test3e$mortlag2 <- c(test3e$mortality2[-c(1:2)],rep(NA,2))
test3e$mortlag3 <- c(test3e$mortality2[-c(1:3)],rep(NA,3))
test3e$mortlag4 <- c(test3e$mortality2[-c(1:4)],rep(NA,4))
test3e$mortlag5 <- c(test3e$mortality2[-c(1:5)],rep(NA,5))
test3e$mort_lag1 <- c(NA,test3e$mortality2[-n[1]])
test3e$mort_lag2 <- c(NA,NA,test3e$mortality2[-c(n[1]-1,n[1])])

n <- dim(test3f)
test3f$mortlag1 <- c(test3f$mortality2[-1],NA)
test3f$mortlag2 <- c(test3f$mortality2[-c(1:2)],rep(NA,2))
test3f$mortlag3 <- c(test3f$mortality2[-c(1:3)],rep(NA,3))
test3f$mortlag4 <- c(test3f$mortality2[-c(1:4)],rep(NA,4))
test3f$mortlag5 <- c(test3f$mortality2[-c(1:5)],rep(NA,5))
test3f$mort_lag1 <- c(NA,test3f$mortality2[-n[1]])
test3f$mort_lag2 <- c(NA,NA,test3f$mortality2[-c(n[1]-1,n[1])])

test3 <- rbind(test3a,test3b,test3c,test3d,test3e,test3f)

test4 <- test3

test4$dinotefuran[is.na(test4$dinotefuran)] <- 0
test4$nitenpyram[is.na(test4$nitenpyram)] <- 0
test4$thiamethoxam[is.na(test4$thiamethoxam)] <- 0
test4$clothianidin[is.na(test4$clothianidin)] <- 0
test4$imidacloprid[is.na(test4$imidacloprid)] <- 0
test4$acetamiprid[is.na(test4$acetamiprid)] <- 0
test4$thiacloprid[is.na(test4$thiacloprid)] <- 0

test5 <- cor(test3[,c(11:14,17,26:37)],use="pairwise.complete.obs")

test6 <- cor(test4[,c(11:14,17,26:37)],use="pairwise.complete.obs")

test6[c(6:12),c(5,13:16)]

plot(test4$clothianidin,test4$mortlag3,pch=16)

par(mfrow=c(3,1),mai=c(.5,1,0,0))
plot(test4$julian,test4$mortality2,type="b",xlim=c(110,180))

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(test4$julian,test4$mortlag3,type="b",xlim=c(110,180))
abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(test4$julian,test4$clothianidin,type="b",xlim=c(110,180))

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(test4$clothianidin,test4$mortality2,pch=16)
identify(test4$clothianidin,test4$mortality2)

plot(test4$clothianidin,test4$mortlag1,pch=16)

plot(test4$clothianidin,test4$mortality,pch=16)
cor(test4$clothianidin,test4$mortality,use="pairwise.complete.obs")

test4$clothianidinspline <- test4$clothianidin
test4$clothianidinspline[test4$clothianidin<9] <- 0
test4$clothianidinsplinelag1 <- test4$clothianidinlag1
test4$clothianidinsplinelag1[test4$clothianidinlag1<9] <- 0
test4$clothianidinlag1[is.na(test4$clothianidinlag1)] <- 2

test4$i119 <- rep(0,n[1])
test4$i119[test4$julian==119] <- 1

test4$i131 <- rep(0,n[1])
test4$i131[test4$julian==131] <- 1

testa <- deadbees[deadbees$hive=="2015MM1",]
testb <- deadbees[deadbees$hive=="2015MM2",]
testc <- deadbees[deadbees$hive=="2015MM16",]
testd <- deadbees[deadbees$hive=="2015MM37",]
teste <- deadbees[deadbees$hive=="2015MM39",]
testf <- deadbees[deadbees$hive=="2015MM40",]

test4$imm1 <- rep(0,n[1])
test4$imm1[test4$hive=="2015MM1"] <- 1
test4$clomm1 <- test4$clothianidin*test4$imm1
test4$clomm1lag1 <- test4$clothianidinlag1*test4$imm1
test4$thiamm1 <- test4$thiamethoxam*test4$imm1

test4$imm2 <- rep(0,n[1])
test4$imm2[test4$hive=="2015MM2"] <- 1
test4$clomm2 <- test4$clothianidin*test4$imm2
test4$clomm2lag1 <- test4$clothianidinlag1*test4$imm2
test4$thiamm2 <- test4$thiamethoxam*test4$imm2

test4$imm16 <- rep(0,n[1])
test4$imm16[test4$hive=="2015MM16"] <- 1
test4$clomm16 <- test4$clothianidin*test4$imm16
test4$clomm16lag1 <- test4$clothianidinlag1*test4$imm16
test4$thiamm16 <- test4$thiamethoxam*test4$imm16

test4$imm37 <- rep(0,n[1])
test4$imm37[test4$hive=="2015MM37"] <- 1
test4$clomm37 <- test4$clothianidin*test4$imm37
test4$clomm37lag1 <- test4$clothianidinlag1*test4$imm37
test4$thiamm37 <- test4$thiamethoxam*test4$imm37

test4$imm39 <- rep(0,n[1])
test4$imm39[test4$hive=="2015MM39"] <- 1
test4$clomm39 <- test4$clothianidin*test4$imm39
test4$clomm39lag1 <- test4$clothianidinlag1*test4$imm39
test4$thiamm39 <- test4$thiamethoxam*test4$imm39

test.lm <- lm(mortality2 ~ clothianidin+clothianidinlag1+i131+clomm39+imm16+imm39,data=test4,na.action="na.exclude")
summary(test.lm)

#test4$res <- test.lm$res
n <- dim(test4)
test4$resid <- test4$mortality2-(test.lm$coef[1]+test.lm$coef[2]*test4$clothianidin+test.lm$coef[3]*test4$clothianidinlag1+
test.lm$coef[4]*test4$i131+test.lm$coef[5]*test4$clomm39+test.lm$coef[6]*test4$imm16+test.lm$coef[7]*test4$imm39)
test4$residlag1 <- c(NA,test4$resid[-n[1]])
test4$residlag1[is.na(test4$mort_lag1)] <- NA

plot(test4$julian,test4$resid)

Coefficients:
                  Estimate Std. Error t value Pr(>|t|)    
(Intercept)       0.304529   0.033926   8.976 3.19e-12 ***
clothianidin      0.025429   0.003707   6.859 7.60e-09 ***
clothianidinlag1  0.012263   0.003643   3.366  0.00142 ** 
i131              0.158961   0.070685   2.249  0.02870 *  
clomm39           0.012187   0.008112   1.502  0.13894    
imm16            -0.165359   0.055827  -2.962  0.00457 ** 
imm39            -0.348983   0.071657  -4.870 1.05e-05 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.1579 on 53 degrees of freedom
Multiple R-squared:  0.7565,    Adjusted R-squared:  0.7289 
F-statistic: 27.44 on 6 and 53 DF,  p-value: 1.27e-14

# end 2015MM ---------------------------------------

# 05-30-18 2015SC  -----------------------------

test <- deadbees[deadbees$hive=="2015SC49",]
test2 <- pesticide2015[pesticide2015$site=="SC",]
test3 <- merge(test,test2,by.x=8,by.y=6,all.x=T)

n <- dim(test3)
test3$mortlag1 <- c(test3$mortality2[-1],NA)
test3$mortlag2 <- c(test3$mortality2[-c(1:2)],rep(NA,2))
test3$mortlag3 <- c(test3$mortality2[-c(1:3)],rep(NA,3))
test3$mortlag4 <- c(test3$mortality2[-c(1:4)],rep(NA,4))
test3$mortlag5 <- c(test3$mortality2[-c(1:5)],rep(NA,5))

test4 <- test3

test4$dinotefuran[is.na(test4$dinotefuran)] <- 0
test4$nitenpyram[is.na(test4$nitenpyram)] <- 0
test4$thiamethoxam[is.na(test4$thiamethoxam)] <- 0
test4$clothianidin[is.na(test4$clothianidin)] <- 0
test4$imidacloprid[is.na(test4$imidacloprid)] <- 0
test4$acetamiprid[is.na(test4$acetamiprid)] <- 0
test4$thiacloprid[is.na(test4$thiacloprid)] <- 0

test5 <- cor(test3[,c(11:14,17,26:37)],use="pairwise.complete.obs")

test6 <- cor(test4[,c(11:14,17,26:37)],use="pairwise.complete.obs")

test6[c(6:12),c(5,13:16)]

# 05-30-18 SC3

              mortality2   mortlag1   mortlag2    mortlag3   mortlag4
dinotefuran  -0.09811520 -0.3658589 -0.4487880 -0.43123479         NA
nitenpyram    0.07915573  0.1348264 -0.3321465  0.03986418  0.5272913
thiamethoxam  0.05424759  0.1256977 -0.6133831  0.04672387 -0.3106028
clothianidin -0.06218992  0.3338156  0.7515710 -0.13439296  0.9042442
imidacloprid -0.16556562 -0.3117515 -0.2069655 -0.14369561  0.8612429
acetamiprid  -0.07356265 -0.2828770 -0.3434997 -0.17954249  0.8917645
thiacloprid  -0.07876055 -0.3873326 -0.3726708  0.64073958  0.5322278

# 05-30-18 SC4 

             mortality2    mortlag1    mortlag2    mortlag3   mortlag4
dinotefuran  0.75063984 -0.35774392 -0.46509367 -0.47943375         NA
nitenpyram   0.46186335  0.51665647 -0.88681444  0.02924045  0.4717977
thiamethoxam 0.79799122 -0.03978187 -0.59785260  0.62096214 -0.3032439
clothianidin 0.05373711 -0.05272863  0.07946201  0.20003819  0.1912925
imidacloprid 0.72475395 -0.35169967 -0.51338633  0.27539265  0.2039941
acetamiprid  0.78736454 -0.31506727 -0.44965037  0.09916800  0.2221612
thiacloprid  0.70643369 -0.39591317 -0.46430859 -0.21583544  0.4827510

# 05-30-18 SC17

             mortality2   mortlag1   mortlag2   mortlag3   mortlag4
dinotefuran   0.1697303 0.07052107 -0.5592640 -0.5562117         NA
nitenpyram    0.3242395 0.51222446 -0.3699119 -0.3654591  0.5890736
thiamethoxam  0.4323985 0.30274863 -0.6241744 -0.4439508  0.1495348
clothianidin -0.1766109 0.25127013  0.6047212  0.3176380 -0.4280656
imidacloprid  0.1550746 0.04955815 -0.3806701  0.4063605 -0.1829429
acetamiprid   0.1142881 0.15355795 -0.4847137  0.4001396 -0.4024056
thiacloprid   0.2080181 0.09019471 -0.5012688  0.2327354  0.3017734

# 05-30-18 SC45

              mortality2   mortlag1   mortlag2    mortlag3    mortlag4
dinotefuran  -0.04891557 -0.4482783 -0.4433582 -0.44274222          NA
nitenpyram    0.40839582  0.1260122 -0.4618485  0.09561215  0.57692835
thiamethoxam  0.16355506 -0.3735775 -0.6259904 -0.11101188 -0.14473642
clothianidin  0.76078771  0.4081062  0.5345435  0.05007944 -0.09261584
imidacloprid  0.16379560 -0.3379849 -0.2575529  0.19984345 -0.12205277
acetamiprid   0.08826530 -0.3366165 -0.3634510  0.07512311 -0.10860685
thiacloprid   0.01579328 -0.3514428 -0.3524174  0.48539196  0.76375964

# 05-30-18 SC48

             mortality2   mortlag1   mortlag2    mortlag3   mortlag4
dinotefuran   0.7021940 -0.3488666 -0.1505278 -0.08878031         NA
nitenpyram    0.6508316  0.2418406 -0.7050523  0.20168169  0.1236638
thiamethoxam  0.7773717 -0.4663499 -0.3634809  0.65095580 -0.1026421
clothianidin  0.1267408  0.3706302 -0.3216032 -0.02592806 -0.2588155
imidacloprid  0.6781424 -0.1795411 -0.2718982  0.18451629 -0.3515835
acetamiprid   0.7543614 -0.2765808 -0.1772075 -0.07255415 -0.2576562
thiacloprid   0.7305507 -0.3276319 -0.1492682 -0.60088370  0.1978420

# 05-30-18 SC49

             mortality2   mortlag1    mortlag2     mortlag3    mortlag4
dinotefuran   0.3277090 -0.3364771  0.02023092  0.060935391          NA
nitenpyram    0.3064376  0.2199843 -0.42032888  0.263458230 -0.22090043
thiamethoxam  0.3621467 -0.3168614 -0.38154604  0.119220304 -0.30863185
clothianidin  0.6348006  0.1390919  0.48505596 -0.237778357 -0.34157224
imidacloprid  0.4464326 -0.2719569  0.15230660  0.085953010 -0.37233761
acetamiprid   0.4421179 -0.3171897  0.10195506 -0.183064372 -0.26778217
thiacloprid   0.3447712 -0.2250164  0.06938516 -0.009949115 -0.06267232

# combine 6 colonies

             mortality2   mortlag1   mortlag2    mortlag3    mortlag4
dinotefuran   0.2868252 -0.2890196 -0.3206306 -0.29782478          NA
nitenpyram    0.3632144  0.2697750 -0.5084752  0.03837995  0.28422311
thiamethoxam  0.4118249 -0.1394557 -0.5041534  0.15003483 -0.14787662
clothianidin  0.2357717  0.2410471  0.3174577  0.02595548 -0.03512073
imidacloprid  0.3240723 -0.2234599 -0.2369049  0.15412421 -0.02603782
acetamiprid   0.3401888 -0.2224820 -0.2708231  0.01924560 -0.01730621
thiacloprid   0.3084147 -0.2558694 -0.2781922  0.06509307  0.29538389

testa <- deadbees[deadbees$hive=="2015SC3",]
testb <- deadbees[deadbees$hive=="2015SC4",]
testc <- deadbees[deadbees$hive=="2015SC17",]
testd <- deadbees[deadbees$hive=="2015SC45",]
teste <- deadbees[deadbees$hive=="2015SC48",]
testf <- deadbees[deadbees$hive=="2015SC49",]

test2 <- pesticide2015[pesticide2015$site=="SC",]
n <- dim(test2)
test2$clothianidinlag1 <- c(NA,test2$clothianidin[-n[1]])
test2$thiamethoxamlag1 <- c(NA,test2$thiamethoxam[-n[1]])

test3a <- merge(testa,test2,by.x=8,by.y=6,all.x=T)
test3b <- merge(testb,test2,by.x=8,by.y=6,all.x=T)
test3c <- merge(testc,test2,by.x=8,by.y=6,all.x=T)
test3d <- merge(testd,test2,by.x=8,by.y=6,all.x=T)
test3e <- merge(teste,test2,by.x=8,by.y=6,all.x=T)
test3f <- merge(testf,test2,by.x=8,by.y=6,all.x=T)

n <- dim(test3a)
test3a$mortlag1 <- c(test3a$mortality2[-1],NA)
test3a$mortlag2 <- c(test3a$mortality2[-c(1:2)],rep(NA,2))
test3a$mortlag3 <- c(test3a$mortality2[-c(1:3)],rep(NA,3))
test3a$mortlag4 <- c(test3a$mortality2[-c(1:4)],rep(NA,4))
test3a$mortlag5 <- c(test3a$mortality2[-c(1:5)],rep(NA,5))
test3a$mort_lag1 <- c(NA,test3a$mortality2[-n[1]])
test3a$mort_lag2 <- c(NA,NA,test3a$mortality2[-c(n[1]-1,n[1])])

n <- dim(test3b)
test3b$mortlag1 <- c(test3b$mortality2[-1],NA)
test3b$mortlag2 <- c(test3b$mortality2[-c(1:2)],rep(NA,2))
test3b$mortlag3 <- c(test3b$mortality2[-c(1:3)],rep(NA,3))
test3b$mortlag4 <- c(test3b$mortality2[-c(1:4)],rep(NA,4))
test3b$mortlag5 <- c(test3b$mortality2[-c(1:5)],rep(NA,5))
test3b$mort_lag1 <- c(NA,test3b$mortality2[-n[1]])
test3b$mort_lag2 <- c(NA,NA,test3b$mortality2[-c(n[1]-1,n[1])])

n <- dim(test3c)
test3c$mortlag1 <- c(test3c$mortality2[-1],NA)
test3c$mortlag2 <- c(test3c$mortality2[-c(1:2)],rep(NA,2))
test3c$mortlag3 <- c(test3c$mortality2[-c(1:3)],rep(NA,3))
test3c$mortlag4 <- c(test3c$mortality2[-c(1:4)],rep(NA,4))
test3c$mortlag5 <- c(test3c$mortality2[-c(1:5)],rep(NA,5))
test3c$mort_lag1 <- c(NA,test3c$mortality2[-n[1]])
test3c$mort_lag2 <- c(NA,NA,test3c$mortality2[-c(n[1]-1,n[1])])

n <- dim(test3d)
test3d$mortlag1 <- c(test3d$mortality2[-1],NA)
test3d$mortlag2 <- c(test3d$mortality2[-c(1:2)],rep(NA,2))
test3d$mortlag3 <- c(test3d$mortality2[-c(1:3)],rep(NA,3))
test3d$mortlag4 <- c(test3d$mortality2[-c(1:4)],rep(NA,4))
test3d$mortlag5 <- c(test3d$mortality2[-c(1:5)],rep(NA,5))
test3d$mort_lag1 <- c(NA,test3d$mortality2[-n[1]])
test3d$mort_lag2 <- c(NA,NA,test3d$mortality2[-c(n[1]-1,n[1])])

n <- dim(test3e)
test3e$mortlag1 <- c(test3e$mortality2[-1],NA)
test3e$mortlag2 <- c(test3e$mortality2[-c(1:2)],rep(NA,2))
test3e$mortlag3 <- c(test3e$mortality2[-c(1:3)],rep(NA,3))
test3e$mortlag4 <- c(test3e$mortality2[-c(1:4)],rep(NA,4))
test3e$mortlag5 <- c(test3e$mortality2[-c(1:5)],rep(NA,5))
test3e$mort_lag1 <- c(NA,test3e$mortality2[-n[1]])
test3e$mort_lag2 <- c(NA,NA,test3e$mortality2[-c(n[1]-1,n[1])])

n <- dim(test3f)
test3f$mortlag1 <- c(test3f$mortality2[-1],NA)
test3f$mortlag2 <- c(test3f$mortality2[-c(1:2)],rep(NA,2))
test3f$mortlag3 <- c(test3f$mortality2[-c(1:3)],rep(NA,3))
test3f$mortlag4 <- c(test3f$mortality2[-c(1:4)],rep(NA,4))
test3f$mortlag5 <- c(test3f$mortality2[-c(1:5)],rep(NA,5))
test3f$mort_lag1 <- c(NA,test3f$mortality2[-n[1]])
test3f$mort_lag2 <- c(NA,NA,test3f$mortality2[-c(n[1]-1,n[1])])

test3 <- rbind(test3a,test3b,test3c,test3d,test3e,test3f)

test4 <- test3

test4$dinotefuran[is.na(test4$dinotefuran)] <- 0
test4$nitenpyram[is.na(test4$nitenpyram)] <- 0
test4$thiamethoxam[is.na(test4$thiamethoxam)] <- 0
test4$clothianidin[is.na(test4$clothianidin)] <- 0
test4$imidacloprid[is.na(test4$imidacloprid)] <- 0
test4$acetamiprid[is.na(test4$acetamiprid)] <- 0
test4$thiacloprid[is.na(test4$thiacloprid)] <- 0


test5 <- cor(test3[,c(11:14,17,26:37)],use="pairwise.complete.obs")

test6 <- cor(test4[,c(11:14,17,26:37)],use="pairwise.complete.obs")

test6[c(6:12),c(5,13:16)]

plot(test4$clothianidin,test4$mortlag3,pch=16)

par(mfrow=c(3,1),mai=c(.5,1,0,0))
plot(test4$julian,test4$mortality2,type="b",xlim=c(110,180))

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(test4$julian,test4$mortlag3,type="b",xlim=c(110,180))
abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(test4$julian,test4$clothianidin,type="b",xlim=c(110,180))

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

test4$clothianidinspline <- test4$clothianidin
test4$clothianidinspline[test4$clothianidin<9] <- 0
test4$clothianidinsplinelag1 <- test4$clothianidinlag1
test4$clothianidinsplinelag1[test4$clothianidinlag1<9] <- 0

test4$clothianidinlag1[test4$julian==154] <- 7

test4$i131 <- rep(0,n[1])
test4$i131[test4$julian==131] <- 1
test4$i125 <- rep(0,n[1])
test4$i125[test4$julian==125] <- 1
test4$i128 <- rep(0,n[1])
test4$i128[test4$julian==128] <- 1

testa <- deadbees[deadbees$hive=="2015SC3",]
testb <- deadbees[deadbees$hive=="2015SC4",]
testc <- deadbees[deadbees$hive=="2015SC17",]
testd <- deadbees[deadbees$hive=="2015SC45",]
teste <- deadbees[deadbees$hive=="2015SC48",]
testf <- deadbees[deadbees$hive=="2015SC49",]

test4$isc3 <- rep(0,n[1])
test4$isc3[test4$hive=="2015SC3"] <- 1

test4$isc4 <- rep(0,n[1])
test4$isc4[test4$hive=="2015SC4"] <- 1

test4$isc17 <- rep(0,n[1])
test4$isc17[test4$hive=="2015SC17"] <- 1

test4$isc45 <- rep(0,n[1])
test4$isc45[test4$hive=="2015SC45"] <- 1

test4$isc48 <- rep(0,n[1])
test4$isc48[test4$hive=="2015SC48"] <- 1

test4$isc49 <- rep(0,n[1])
test4$isc49[test4$hive=="2015SC49"] <- 1

test4$closc3 <- test4$clothianidin*test4$isc3
test4$closc4 <- test4$clothianidin*test4$isc4
test4$closc17 <- test4$clothianidin*test4$isc17
test4$closc45 <- test4$clothianidin*test4$isc45
test4$closc48 <- test4$clothianidin*test4$isc48

test4$thiasc3 <- test4$thiamethoxam*test4$isc3
test4$thiasc4 <- test4$thiamethoxam*test4$isc4
test4$thiasc17 <- test4$thiamethoxam*test4$isc17
test4$thiasc45 <- test4$thiamethoxam*test4$isc45
test4$thiasc48 <- test4$thiamethoxam*test4$isc48

test.lm <- lm(mortality2 ~ thiamethoxam+clothianidin+i128+closc3+closc4+closc17+closc48,data=test4,na.action="na.exclude")
summary(test.lm)

#test4$res <- test.lm$res
n <- dim(test4)
test4$resid <- test4$mortality2-(test.lm$coef[1]+test.lm$coef[2]*test4$thiamethoxam+test.lm$coef[3]*test4$clothianidin+test.lm$coef[4]*test4$i128+test.lm$coef[5]*test4$closc3+test.lm$coef[6]*test4$closc4+test.lm$coef[7]*test4$closc17+test.lm$coef[8]*test4$closc48)
test4$residlag1 <- c(NA,test4$resid[-n[1]])
test4$residlag1[is.na(test4$mort_lag1)] <- NA

plot(test4$julian,test4$resid)

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)   0.269186   0.038615   6.971 5.51e-09 ***
thiamethoxam  0.032189   0.006739   4.776 1.50e-05 ***
clothianidin  0.014710   0.003017   4.875 1.06e-05 ***
i128          0.357380   0.091640   3.900 0.000278 ***
closc3       -0.014709   0.005001  -2.942 0.004867 ** 
closc4       -0.011826   0.005001  -2.365 0.021793 *  
closc17      -0.012166   0.005001  -2.433 0.018451 *  
closc48      -0.013335   0.005001  -2.667 0.010186 *  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.212 on 52 degrees of freedom
Multiple R-squared:  0.5173,    Adjusted R-squared:  0.4523 
F-statistic:  7.96 on 7 and 52 DF,  p-value: 1.529e-06

# end 2015SC ---------------------------------------

# 05-30-18 2015TV clothianidin cor best w mortlag3 -----------------------------


test <- deadbees[deadbees$hive=="2015TV62",]
test2 <- pesticide2015[pesticide2015$site=="TV",]
test3 <- merge(test,test2,by.x=8,by.y=6,all.x=T)

n <- dim(test3)
test3$mortlag1 <- c(test3$mortality2[-1],NA)
test3$mortlag2 <- c(test3$mortality2[-c(1:2)],rep(NA,2))
test3$mortlag3 <- c(test3$mortality2[-c(1:3)],rep(NA,3))
test3$mortlag4 <- c(test3$mortality2[-c(1:4)],rep(NA,4))
test3$mortlag5 <- c(test3$mortality2[-c(1:5)],rep(NA,5))

test4 <- test3

test4$dinotefuran[is.na(test4$dinotefuran)] <- 0
test4$nitenpyram[is.na(test4$nitenpyram)] <- 0
test4$thiamethoxam[is.na(test4$thiamethoxam)] <- 0
test4$clothianidin[is.na(test4$clothianidin)] <- 0
test4$imidacloprid[is.na(test4$imidacloprid)] <- 0
test4$acetamiprid[is.na(test4$acetamiprid)] <- 0
test4$thiacloprid[is.na(test4$thiacloprid)] <- 0

test5 <- cor(test3[,c(11:14,17,26:37)],use="pairwise.complete.obs")

test6 <- cor(test4[,c(11:14,17,26:37)],use="pairwise.complete.obs")

# combine 2 colonies
# 05-30-18 

             mortality2    mortlag1     mortlag2     mortlag3
dinotefuran  -0.1693415 -0.25273254 -0.122692165 -0.177732071
nitenpyram    0.3623308 -0.01216854  0.125338398  0.508555641
thiamethoxam  0.8379878  0.29713317  0.002520443  0.057629246
clothianidin  0.7653590  0.56997342 -0.011111693 -0.007917215
imidacloprid -0.2102048 -0.15101095 -0.479883966 -0.288911954
acetamiprid  -0.1564206 -0.03716898 -0.153716264  0.080810919
thiacloprid  -0.2900676 -0.22395680 -0.433827861 -0.458453265
                 mortlag4
dinotefuran   0.009896327
nitenpyram    0.286549869
thiamethoxam -0.034949674
clothianidin -0.161596981
imidacloprid -0.317339564
acetamiprid   0.142388481
thiacloprid  -0.099571444


testa <- deadbees[deadbees$hive=="2015TV62",]
testb <- deadbees[deadbees$hive=="2015TV63",]

test2 <- pesticide2015[pesticide2015$site=="TV",]

n <- dim(test2)
test2$clothianidinlag1 <- c(NA,test2$clothianidin[-n[1]])
test2$thiamethoxamlag1 <- c(NA,test2$thiamethoxam[-n[1]])

test3a <- merge(testa,test2,by.x=8,by.y=6,all.x=T)
test3b <- merge(testb,test2,by.x=8,by.y=6,all.x=T)

n <- dim(test3a)
test3a$mortlag1 <- c(test3a$mortality2[-1],NA)
test3a$mortlag2 <- c(test3a$mortality2[-c(1:2)],rep(NA,2))
test3a$mortlag3 <- c(test3a$mortality2[-c(1:3)],rep(NA,3))
test3a$mortlag4 <- c(test3a$mortality2[-c(1:4)],rep(NA,4))
test3a$mortlag5 <- c(test3a$mortality2[-c(1:5)],rep(NA,5))
test3a$mort_lag1 <- c(NA,test3a$mortality2[-n[1]])
test3a$mort_lag2 <- c(NA,NA,test3a$mortality2[-c(n[1]-1,n[1])])

n <- dim(test3b)
test3b$mortlag1 <- c(test3b$mortality2[-1],NA)
test3b$mortlag2 <- c(test3b$mortality2[-c(1:2)],rep(NA,2))
test3b$mortlag3 <- c(test3b$mortality2[-c(1:3)],rep(NA,3))
test3b$mortlag4 <- c(test3b$mortality2[-c(1:4)],rep(NA,4))
test3b$mortlag5 <- c(test3b$mortality2[-c(1:5)],rep(NA,5))
test3b$mort_lag1 <- c(NA,test3b$mortality2[-n[1]])
test3b$mort_lag2 <- c(NA,NA,test3b$mortality2[-c(n[1]-1,n[1])])

test3 <- rbind(test3a,test3b)

test4 <- test3

test4$dinotefuran[is.na(test4$dinotefuran)] <- 0
test4$nitenpyram[is.na(test4$nitenpyram)] <- 0
test4$thiamethoxam[is.na(test4$thiamethoxam)] <- 0
test4$clothianidin[is.na(test4$clothianidin)] <- 0
test4$imidacloprid[is.na(test4$imidacloprid)] <- 0
test4$acetamiprid[is.na(test4$acetamiprid)] <- 0
test4$thiacloprid[is.na(test4$thiacloprid)] <- 0

test5 <- cor(test3[,c(11:14,17,26:37)],use="pairwise.complete.obs")

test6 <- cor(test4[,c(11:14,17,26:37)],use="pairwise.complete.obs")

test6[c(6:12),c(5,13:16)]

plot(test4$clothianidin,test4$mortlag3,pch=16)

par(mfrow=c(3,1),mai=c(.5,1,0,0))
plot(test4$julian,test4$mortality2,type="b",xlim=c(110,180))

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(test4$julian,test4$mortlag3,type="b",xlim=c(110,180))
abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(test4$julian,test4$clothianidin,type="b",xlim=c(110,180))

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

test4$clothianidinspline <- test4$clothianidin
test4$clothianidinspline[test4$clothianidin<9] <- 0
test4$clothianidinsplinelag1 <- test4$clothianidinlag1
test4$clothianidinsplinelag1[test4$clothianidinlag1<9] <- 0
test4$clothianidinlag1[is.na(test4$clothianidinlag1)] <- 1

test4$i147 <- rep(0,n[1])
test4$i147[test4$julian==147] <- 1
test4$i131 <- rep(0,n[1])
test4$i131[test4$julian==131] <- 1
test4$i134 <- rep(0,n[1])
test4$i134[test4$julian==134] <- 1
test4$i125 <- rep(0,n[1])
test4$i125[test4$julian==125] <- 1

test4$i145p <- rep(0,n[1])
test4$i145p[test4$julian>145] <- 1

test4$itv62 <- rep(0,n[1])
test4$itv62[test4$hive=="2015TV62"] <- 1
test4$clotv62 <- test4$clothianidin*test4$itv62
test4$clotv62lag1 <- test4$clothianidinlag1*test4$itv62
test4$thiatv62 <- test4$thiamethoxam*test4$itv62

test.lm <- lm(mortality2 ~ clothianidin+clothianidinlag1+itv62+thiamethoxam,data=test4,na.action="na.exclude")
summary(test.lm)

#test4$res <- test.lm$res
n <- dim(test4)
test4$resid <- test4$mortality2-(test.lm$coef[1]+test.lm$coef[2]*test4$clothianidin+test.lm$coef[3]*test4$clothianidinlag1+test.lm$coef[4]*test4$itv62+test.lm$coef[5]*test4$thiamethoxam)
test4$residlag1 <- c(NA,test4$resid[-n[1]])
test4$residlag1[is.na(test4$mort_lag1)] <- NA

plot(test4$julian,test4$resid)
 
Coefficients:
                 Estimate Std. Error t value Pr(>|t|)    
(Intercept)      0.054325   0.022458   2.419   0.0287 *  
clothianidin     0.003769   0.001948   1.935   0.0721 .  
clothianidinlag1 0.008936   0.001164   7.674 1.43e-06 ***
itv62            0.160679   0.026403   6.086 2.09e-05 ***
thiamethoxam     0.035004   0.005019   6.974 4.47e-06 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.05904 on 15 degrees of freedom
Multiple R-squared:  0.9622,    Adjusted R-squared:  0.9521 
F-statistic:  95.5 on 4 and 15 DF,  p-value: 1.755e-10

test.lm <- lm(mortality2 ~ thiamethoxam+thiamethoxamlag1+residlag1,data=test4,na.action="na.exclude")
summary(test.lm)

#test4$res <- test.lm$res
n <- dim(test4)
test4$resid <- test4$mortality2-(test.lm$coef[1]+test.lm$coef[2]*test4$thiamethoxam+test.lm$coef[3]*test4$thiamethoxamlag1)
test4$residlag1 <- c(NA,test4$resid[-n[1]])
test4$residlag1[is.na(test4$mort_lag1)] <- NA

Coefficients:
                 Estimate Std. Error t value Pr(>|t|)    
(Intercept)      0.106836   0.048271   2.213  0.05128 .  
thiamethoxam     0.051690   0.005920   8.731 5.43e-06 ***
thiamethoxamlag1 0.022786   0.005948   3.831  0.00331 ** 
residlag1        0.634792   0.262675   2.417  0.03627 *  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.1135 on 10 degrees of freedom
  (6 observations deleted due to missingness)
Multiple R-squared:  0.8916,    Adjusted R-squared:  0.8591 
F-statistic: 27.41 on 3 and 10 DF,  p-value: 3.866e-05

# end 2015TV ---------------------------------------

# 05-29-18 2015WB clothianidin cor best w mortlag3 -----------------------------

#test <- deadbees[deadbees$hive=="2015WB26",]
test <- deadbees[deadbees$hive=="2015WB12",]
test2 <- pesticide2015[pesticide2015$site=="WB",]
test3 <- merge(test,test2,by.x=8,by.y=6,all.x=T)

n <- dim(test3)
test3$mortlag1 <- c(test3$mortality2[-1],NA)
test3$mortlag2 <- c(test3$mortality2[-c(1:2)],rep(NA,2))
test3$mortlag3 <- c(test3$mortality2[-c(1:3)],rep(NA,3))
test3$mortlag4 <- c(test3$mortality2[-c(1:4)],rep(NA,4))
test3$mortlag5 <- c(test3$mortality2[-c(1:5)],rep(NA,5))

test4 <- test3

test4$dinotefuran[is.na(test4$dinotefuran)] <- 0
test4$nitenpyram[is.na(test4$nitenpyram)] <- 0
test4$thiamethoxam[is.na(test4$thiamethoxam)] <- 0
test4$clothianidin[is.na(test4$clothianidin)] <- 0
test4$imidacloprid[is.na(test4$imidacloprid)] <- 0
test4$acetamiprid[is.na(test4$acetamiprid)] <- 0
test4$thiacloprid[is.na(test4$thiacloprid)] <- 0

test5 <- cor(test3[,c(11:14,17,26:37)],use="pairwise.complete.obs")

test6 <- cor(test4[,c(11:14,17,26:37)],use="pairwise.complete.obs")

# 05-29-18 WB26 mortlag3 cor best w clothianidin, thiamethoxam
# 05-29-18 WB28 mortlag3 cor best w clothianidin, imidacloprid
# 05-29-18 WB13 mortlag1 cor best w clothianidin, mortlag1 & mortlag3 cor b w imidacloprid, mortlag3 cor b w thiamethoxam
# 05-29-18 WB12 mortlag3 cor best w imidacloprid, clothianidin, mortlag4 cor best w thiamethoxam

test12 <- test6
test13 <- test6
test25 <- test6
test26 <- test6

# combine 4 colonies
# 05-29-18 mortlag3 cor best w clothianidin, imidacloprid, acetamiprid, thiacloprid

              mortality2    mortlag1    mortlag2     mortlag3   mortlag4
dinotefuran  -0.22313954  0.08485606  0.14781080  0.303853271  0.3778926
nitenpyram    0.20064344 -0.33664234 -0.19577730 -0.053303387 -0.2285532
thiamethoxam  0.24038598  0.28663042  0.31826617 -0.004271709  0.1436058
clothianidin  0.21190558  0.36048932  0.23463586  0.481046402 -0.1261302
imidacloprid -0.08229315  0.20923217  0.03656322  0.443889455  0.0188007
acetamiprid  -0.19981002  0.15903689  0.24608011  0.396339277  0.2337338
thiacloprid  -0.13058844  0.22453451  0.14851072  0.503051962  0.1772413

testa <- deadbees[deadbees$hive=="2015WB12",]
testb <- deadbees[deadbees$hive=="2015WB13",]
testc <- deadbees[deadbees$hive=="2015WB28",]
testd <- deadbees[deadbees$hive=="2015WB26",]

test2 <- pesticide2015[pesticide2015$site=="WB",]
n <- dim(test2)
test2$clothianidinlag1 <- c(NA,test2$clothianidin[-n[1]])
test2$clothianidinlag2 <- c(rep(NA,2),test2$clothianidin[-c(n[1]-1,n[1])])
test2$clothianidinlag3 <- c(rep(NA,3),test2$clothianidin[-c(n[1]-2,n[1]-1,n[1])])
test2$thiamethoxamlag1 <- c(NA,test2$thiamethoxam[-n[1]])
test2$thiamethoxamlag2 <- c(rep(NA,2),test2$thiamethoxam[-c(n[1]-1,n[1])])
test2$thiamethoxamlag3 <- c(rep(NA,3),test2$thiamethoxam[-c(n[1]-2,n[1]-1,n[1])])
test2$thiaclopridlag1 <- c(NA,test2$thiacloprid[-n[1]])
test2$thiaclopridlag2 <- c(rep(NA,2),test2$thiacloprid[-c(n[1]-1,n[1])])
test2$thiaclopridlag3 <- c(rep(NA,3),test2$thiacloprid[-c(n[1]-2,n[1]-1,n[1])])

test3a <- merge(testa,test2,by.x=8,by.y=6,all.x=T)
test3b <- merge(testb,test2,by.x=8,by.y=6,all.x=T)
test3c <- merge(testc,test2,by.x=8,by.y=6,all.x=T)
test3d <- merge(testd,test2,by.x=8,by.y=6,all.x=T)
test3e <- merge(teste,test2,by.x=8,by.y=6,all.x=T)
test3f <- merge(testf,test2,by.x=8,by.y=6,all.x=T)

n <- dim(test3a)
test3a$mortlag1 <- c(test3a$mortality2[-1],NA)
test3a$mortlag2 <- c(test3a$mortality2[-c(1:2)],rep(NA,2))
test3a$mortlag3 <- c(test3a$mortality2[-c(1:3)],rep(NA,3))
test3a$mortlag4 <- c(test3a$mortality2[-c(1:4)],rep(NA,4))
test3a$mortlag5 <- c(test3a$mortality2[-c(1:5)],rep(NA,5))
test3a$mort_lag1 <- c(NA,test3a$mortality2[-n[1]])
test3a$mort_lag2 <- c(NA,NA,test3a$mortality2[-c(n[1]-1,n[1])])

n <- dim(test3b)
test3b$mortlag1 <- c(test3b$mortality2[-1],NA)
test3b$mortlag2 <- c(test3b$mortality2[-c(1:2)],rep(NA,2))
test3b$mortlag3 <- c(test3b$mortality2[-c(1:3)],rep(NA,3))
test3b$mortlag4 <- c(test3b$mortality2[-c(1:4)],rep(NA,4))
test3b$mortlag5 <- c(test3b$mortality2[-c(1:5)],rep(NA,5))
test3b$mort_lag1 <- c(NA,test3b$mortality2[-n[1]])
test3b$mort_lag2 <- c(NA,NA,test3b$mortality2[-c(n[1]-1,n[1])])

n <- dim(test3c)
test3c$mortlag1 <- c(test3c$mortality2[-1],NA)
test3c$mortlag2 <- c(test3c$mortality2[-c(1:2)],rep(NA,2))
test3c$mortlag3 <- c(test3c$mortality2[-c(1:3)],rep(NA,3))
test3c$mortlag4 <- c(test3c$mortality2[-c(1:4)],rep(NA,4))
test3c$mortlag5 <- c(test3c$mortality2[-c(1:5)],rep(NA,5))
test3c$mort_lag1 <- c(NA,test3c$mortality2[-n[1]])
test3c$mort_lag2 <- c(NA,NA,test3c$mortality2[-c(n[1]-1,n[1])])

n <- dim(test3d)
test3d$mortlag1 <- c(test3d$mortality2[-1],NA)
test3d$mortlag2 <- c(test3d$mortality2[-c(1:2)],rep(NA,2))
test3d$mortlag3 <- c(test3d$mortality2[-c(1:3)],rep(NA,3))
test3d$mortlag4 <- c(test3d$mortality2[-c(1:4)],rep(NA,4))
test3d$mortlag5 <- c(test3d$mortality2[-c(1:5)],rep(NA,5))
test3d$mort_lag1 <- c(NA,test3d$mortality2[-n[1]])
test3d$mort_lag2 <- c(NA,NA,test3d$mortality2[-c(n[1]-1,n[1])])

test3 <- rbind(test3a,test3b,test3c,test3d)

test4 <- test3

test4$dinotefuran[is.na(test4$dinotefuran)] <- 0
test4$nitenpyram[is.na(test4$nitenpyram)] <- 0
test4$thiamethoxam[is.na(test4$thiamethoxam)] <- 0
test4$clothianidin[is.na(test4$clothianidin)] <- 0
test4$imidacloprid[is.na(test4$imidacloprid)] <- 0
test4$acetamiprid[is.na(test4$acetamiprid)] <- 0
test4$thiacloprid[is.na(test4$thiacloprid)] <- 0

test5 <- cor(test3[,c(11:14,17,26:37)],use="pairwise.complete.obs")

test6 <- cor(test4[,c(11:14,17,26:37)],use="pairwise.complete.obs")

test6[c(6:12),c(5,13:16)]

plot(test4$clothianidin,test4$mortlag3,pch=16)

par(mfrow=c(3,1),mai=c(.5,1,0,0))
plot(test4$julian,test4$mortality2,type="b",xlim=c(110,180))

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(test4$julian,test4$mortlag3,type="b",xlim=c(110,180))
abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

plot(test4$julian,test4$clothianidin,type="b",xlim=c(110,180))

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)

test4$clothianidinspline <- test4$clothianidin
test4$clothianidinspline[test4$clothianidin<9] <- 0
test4$clothianidinsplinelag1 <- test4$clothianidinlag1
test4$clothianidinsplinelag1[test4$clothianidinlag1<9] <- 0
test4$clothianidinlag3[is.na(test4$clothianidinlag3)&test4$julian>147] <- 0
test4$clothianidinlag3[is.na(test4$clothianidinlag3)&test4$julian<147] <- 6

test4$i147 <- rep(0,n[1])
test4$i147[test4$julian==147] <- 1
test4$i131 <- rep(0,n[1])
test4$i131[test4$julian==131] <- 1
test4$i128 <- rep(0,n[1])
test4$i128[test4$julian==128] <- 1

test4$i120 <- rep(0,n[1])
test4$i120[test4$julian==120] <- 1

test4$i122 <- rep(0,n[1])
test4$i122[test4$julian==122] <- 1

test4$i125 <- rep(0,n[1])
test4$i125[test4$julian==125] <- 1

test4$i145p <- rep(0,n[1])
test4$i145p[test4$julian>145] <- 1

test4$i141p <- rep(0,n[1])
test4$i141p[test4$julian>141] <- 1

test4$iwb12 <- rep(0,n[1])
test4$iwb12[test4$hive=="2015WB12"] <- 1

test4$clowb12 <- test4$clothianidin*test4$iwb12
test4$clowb12lag3 <- test4$clothianidinlag3*test4$iwb12

test4$thiawb12 <- test4$thiamethoxam*test4$iwb12

test4$iwb13 <- rep(0,n[1])
test4$iwb13[test4$hive=="2015WB13"] <- 1

test4$clowb13 <- test4$clothianidin*test4$iwb13
test4$clowb13lag3 <- test4$clothianidinlag3*test4$iwb13

test4$thiawb13 <- test4$thiamethoxam*test4$iwb13

test4$iwb28 <- rep(0,n[1])
test4$iwb28[test4$hive=="2015WB28"] <- 1

test4$clowb28 <- test4$clothianidin*test4$iwb28
test4$clowb28lag3 <- test4$clothianidinlag3*test4$iwb28

test4$thiawb28 <- test4$thiamethoxam*test4$iwb28

test.lm <- lm(mortality2 ~ clothianidinlag3+thiamethoxam+iwb13+i120,data=test4,na.action="na.exclude")
summary(test.lm)

#test4$res <- test.lm$res
n <- dim(test4)
test4$resid <- test4$mortality2-(test.lm$coef[1]+test.lm$coef[2]*test4$clothianidinlag3+test.lm$coef[3]*test4$thiamethoxam+
test.lm$coef[4]*test4$iwb13+test.lm$coef[5]*test4$i120)
test4$residlag1 <- c(NA,test4$resid[-n[1]])
test4$residlag1[is.na(test4$mort_lag1)] <- NA

plot(test4$julian,test4$resid)

Coefficients:
                  Estimate Std. Error t value Pr(>|t|)   
(Intercept)       0.216082   0.076121   2.839  0.00749 **
clothianidinlag3  0.022502   0.007296   3.084  0.00397 **
thiamethoxam      0.013060   0.006510   2.006  0.05263 . 
iwb13             0.230111   0.084329   2.729  0.00988 **
i120             -0.255301   0.122666  -2.081  0.04479 * 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.2309 on 35 degrees of freedom
Multiple R-squared:  0.4238,    Adjusted R-squared:  0.3579 
F-statistic: 6.435 on 4 and 35 DF,  p-value: 0.0005438


# end 2015WB ---------------------------------------
