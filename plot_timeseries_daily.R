
# import flux.table from txt file
flux.table <- read.table(file="/work/mittity/CSSAGRI/plot_pdf/flux.table.20190707-20190810.dat ", header=TRUE, row.names=NULL,sep=",")
#subset to high quility fluxes 
flux.table <- subset(flux.table, ((flux.table$flag.fpt==TRUE)|(flux.table$flag.stat==TRUE)))
# limited the maximun footprint <= 100
flux.table$lev1.xmax[flux.table$lev1.xmax>=200] <- 200

flux.table$date <- format(as.Date(flux.table$date.time),format="%Y-%m-%d")

pdf("flux.diurnal.mean.20190707-20190810.QCQA.pdf",width=18,height=12)
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


