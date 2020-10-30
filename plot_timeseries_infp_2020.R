# import flux.table from txt file
flux.table <- read.csv(file="/work/mittity/CSSAGRI/plot_pdf/flux.table.0401_30.dat", heade=TRUE, row.names=NULL,sep=",")
#subset to high quility fluxes 
flux.table <- subset(flux.table, ((flux.table$flag.stat==TRUE)&(flux.table$flag.itc==TRUE)&(flux.table$flag.fpt==TRUE)))
# limited the maximun footprint <= 100
flux.table$lev1.xmax[flux.table$lev1.xmax>=200] <- 200
#load despike function
source(file="/work/mittity/CSSAGRI/fun_despike.R")
flux.table$lev1.n2o <- fun_despike(x=flux.table$lev1.n2o, nstd=2.0)

# calculate the accumulative N-flux
# replace na to zero
flux.table[is.na(flux.table)]<-0
flux.table$acc.n.fluxes <-  flux.table$lev1.hno3*14+flux.table$lev1.hono*14+ flux.table$lev1.n2o*28+
                            +flux.table$lev1.no.gd*14*2+flux.table$lev1.nox.gd*14*2
#despike 
flux.table$acc.n.fluxes <- fun_despike(flux.table$acc.n.fluxes,nstd=3.5)
# do the loop for the accumulative sume
flux.table$acc.n.fluxes <- cumsum(flux.table$acc.n.fluxes)



flux.table$date <- format(as.Date(flux.table$date.time),format="%m-%d")
#pdf("flux.time.series.20190806-20191114.QCQA.pdf",width=18,height=12,pointsize=18)
par( mai=c(0.6,0.6,0.3,0.3),cex.main=2.0)
layout( matrix(c(1,2,3,4,5,6,7,8),4,2,byrow=FALSE) )
#soil ferterlization 
dd<-levels(as.factor(flux.table$date))
x <- c(which(dd=="02-15")[1],
       which(dd=="02-16")[1],
       which(dd=="03-05")[1],
       which(dd=="03-06")[1],
       which(dd=="03-20")[1],
       which(dd=="03-21")[1],
       which(dd=="04-21")[1],
       which(dd=="04-22")[1])
#soil tillage
xx <- c(which(dd=="02-15")[1],
        which(dd=="02-16")[1],   
        which(dd=="02-17")[1],
        which(dd=="02-18")[1])

#water input
xxx <- c(which(dd==""[1],))

y <- c(-1,2)

plot(x=as.factor(flux.table$date), y=flux.table$lev1.hno3*14,
                  main=expression( paste( "(a) HN",O[3],", ng/(",m^{2}*s,")",sep="" )),
	          outline=FALSE, col="lightblue", ylim=c(-0.005,0.4))
rect(x[1],y[1],x[2],y[2],col= 'brown',density=10)
rect(x[3],y[1],x[4],y[2],col= 'brown',density=10) 
rect(x[5],y[1],x[6],y[2],col= 'brown',density=10) 
rect(x[7],y[1],x[8],y[2],col= 'brown',density=10) 
rect(xx[1],y[1],xx[2],y[2],col= 'gray',density=10)
rect(xx[3],y[1],xx[4],y[2],col= 'green',density=10)




plot(x=as.factor(flux.table$date), y=flux.table$lev1.hono*14,
                  main=expression( paste( "(b) HONO, ng/(",m^{2}*s,")",sep="" )),
	          ylim=c(-1E-3,1.5), outline=FALSE,col="brown")
rect(x[1],y[1],x[2],y[2],col= 'brown',density=10)
rect(x[3],y[1],x[4],y[2],col= 'brown',density=10) 
rect(x[5],y[1],x[6],y[2],col= 'brown',density=10) 
rect(x[7],y[1],x[8],y[2],col= 'brown',density=10) 
rect(xx[1],y[1],xx[2],y[2],col= 'gray',density=10)
rect(xx[3],y[1],xx[4],y[2],col= 'green',density=10)

y <- c(-50,750)

plot(x=as.factor(flux.table$date), y=flux.table$lev1.n2o*28,
                  main=expression( paste("(c)", N[2],"O, ng/(",m^{2}*s,")",sep="" )),
	          ylim=c(-5E0,750), outline=FALSE,col="gray" )
rect(x[1],y[1],x[2],y[2],col= 'brown',density=10)
rect(x[3],y[1],x[4],y[2],col= 'brown',density=10) 
rect(x[5],y[1],x[6],y[2],col= 'brown',density=10) 
rect(x[7],y[1],x[8],y[2],col= 'brown',density=10) 
rect(xx[1],y[1],xx[2],y[2],col= 'gray',density=10)
rect(xx[3],y[1],xx[4],y[2],col= 'green',density=10)


plot(x=as.factor(flux.table$date), y=flux.table$lev1.le.1,
                 main=expression( paste("(d)", H[2],"O"," flux W/(",m^{2},")",sep="" )),
	         ylim=c(-1E1,400),outline=FALSE,col="lightgray")
rect(x[1],y[1],x[2],y[2],col= 'brown',density=10)
rect(x[3],y[1],x[4],y[2],col= 'brown',density=10) 
rect(x[5],y[1],x[6],y[2],col= 'brown',density=10) 
rect(x[7],y[1],x[8],y[2],col= 'brown',density=10) 
rect(xx[1],y[1],xx[2],y[2],col= 'gray',density=10)
rect(xx[3],y[1],xx[4],y[2],col= 'green',density=10)

#dev.off()

ld.go<-TRUE

if(ld.go){
#dev.new()
#par(mfrow=c(3,2),mai=c(0.75,0.75,0.2,0.2) )

y<-c(-2,8)
plot(x=as.factor(flux.table$date), y=flux.table$lev1.no.gd*14*2,
	         main=expression( paste( "(e) NO, ng/(",m^{2}*s,")",sep="" )),
	         outline=FALSE,col="lightblue",ylim=c(-1,7))
rect(x[1],y[1],x[2],y[2],col= 'brown',density=10)
rect(x[3],y[1],x[4],y[2],col= 'brown',density=10) 
rect(x[5],y[1],x[6],y[2],col= 'brown',density=10) 
rect(x[7],y[1],x[8],y[2],col= 'brown',density=10) 
rect(xx[1],y[1],xx[2],y[2],col= 'gray',density=10)
rect(xx[3],y[1],xx[4],y[2],col= 'green',density=10)



plot(x=as.factor(flux.table$date), y=flux.table$lev1.nox.gd*14*2,
	         main=expression( paste( "(f) NOx, ng/(",m^{2}*s,")",sep="" )),
	         outline=FALSE,col="blue",ylim=c(-1,7))
rect(x[1],y[1],x[2],y[2],col= 'brown',density=10)
rect(x[3],y[1],x[4],y[2],col= 'brown',density=10) 
rect(x[5],y[1],x[6],y[2],col= 'brown',density=10) 
rect(x[7],y[1],x[8],y[2],col= 'brown',density=10) 
rect(xx[1],y[1],xx[2],y[2],col= 'gray',density=10)
rect(xx[3],y[1],xx[4],y[2],col= 'green',density=10)

#replace by accunulative N-fluxes
#plot(x=as.factor(flux.table$date), y=flux.table$lev1.nh3*14,
#                 main=expression( paste( "N",H[3],", N-flux ng/(",m^{2}*s,")",sep="" )),
#	         outline=FALSE,col="forestgreen",)
y<-c(-1,12)
plot(x=as.factor(flux.table$date), y=flux.table$acc.n.fluxes*1800*(10^-12)*12000,
                  main="(g) Acc Total N-efflux , (Kg/ha)", outline=FALSE,col="forestgreen" )
rect(x[1],y[1],x[2],y[2],col= 'brown',density=10)
rect(x[3],y[1],x[4],y[2],col= 'green',density=10) 
rect(x[5],y[1],x[6],y[2],col= 'brown',density=10) 
rect(x[7],y[1],x[8],y[2],col= 'brown',density=10) 
rect(x[9],y[1],x[10],y[2],col= 'brown',density=10)
rect(x[11],y[1],x[12],y[2],col= 'gray',density=10)
rect(x[13],y[1],x[14],y[2],col= 'yellow',density=10)
rect(xx[1],y[1],xx[2],y[2],col= 'gray',density=10)
rect(xx[3],y[1],xx[4],y[2],col= 'green',density=10)
rect(xx[5],y[1],xx[6],y[2],col= 'gray',density=10)

y<-c(-2,2)
plot(x=as.factor(flux.table$date), y=flux.table$lev1.co2.1,
                 main=expression( paste( "(h) C",O[2]," flux mg/(",m^{2}*s,")",sep="" )),
	         ylim=c(-2E0,1E0),outline=FALSE,col="green")
rect(x[1],y[1],x[2],y[2],col= 'brown',density=10)
rect(x[3],y[1],x[4],y[2],col= 'brown',density=10) 
rect(x[5],y[1],x[6],y[2],col= 'brown',density=10) 
rect(x[7],y[1],x[8],y[2],col= 'brown',density=10) 
rect(xx[1],y[1],xx[2],y[2],col= 'gray',density=10)
rect(xx[3],y[1],xx[4],y[2],col= 'green',density=10)


}

#dev.off()

pdf("flux.diurnal.mean.20190806-20191222_INFP.QCQA.pdf",width=18,height=12)
#plot diurnal 
par( mai=c(0.5,0.5,0.2,0.2))
layout( matrix(c(1,2,3,4,5,6,7,8),4,2,byrow=FALSE) )

flux.table$hh <- as.factor(substr(flux.table$date.time,start=12,stop=13))

plot(x=flux.table$hh, y=flux.table$lev1.hno3,
                  main=expression( paste( "HN",O[3]," flux nmol/(",m^{2}*s,")",sep="" )),
	          outline=FALSE, col="lightblue", ylim=c(-0.005,0.015))

plot(x=flux.table$hh, y=flux.table$lev1.hono,
                  main=expression( paste( "HONO flux nmol/(",m^{2}*s,")",sep="" )),
	          ylim=c(-1E-3,0.06), outline=FALSE,col="brown")

plot(x=flux.table$hh, y=flux.table$lev1.n2o,
                  main=expression( paste( N[2],"O flux nmol/(",m^{2}*s,")",sep="" )),
	          ylim=c(-5E0,20), outline=FALSE,col="gray" )


plot(x=flux.table$hh, y=flux.table$lev1.le.1,
                 main=expression( paste( H[2],"O"," flux W/(",m^{2},")",sep="" )),
	         ylim=c(-1E1,5E2),outline=FALSE,col="lightgray")

plot(x=flux.table$hh, y=flux.table$lev1.no.gd,
	         main=expression( paste( "NO flux nmol/(",m^{2}*s,")",sep="" )),
	         outline=FALSE,col="lightblue")

plot(x=flux.table$hh, y=flux.table$lev1.nox.gd,
	         main=expression( paste( "NOx flux nmol/(",m^{2}*s,")",sep="" )),
	         outline=FALSE,col="blue")


plot(x=flux.table$hh, y=flux.table$lev1.nh3,
                 main=expression( paste( "N",H[3]," flux nmol/(",m^{2}*s,")",sep="" )),
	         outline=FALSE,col="forestgreen",)

plot(x=flux.table$hh, y=flux.table$lev1.co2.1,
                 main=expression( paste( "C",O[2]," flux mg/(",m^{2}*s,")",sep="" )),
	         ylim=c(-1E0,1E0),outline=FALSE,col="green")


dev.off()


