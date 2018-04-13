
rmspfsc=
  function (time=5 ,claim=5, fmu = .1, fsigma = 2, fnu = 1,
            sumsev=100, smu = 50, ssigma = 3, snu = 2,
            family = list("NO","NO"),round=2 ,size=8,
            padlength=4, padwidth=2, ...)
  {
    if (any(time < 0))
      stop(paste("time must be >=0", "\n", ""))
    if (any(claim < 0))
      stop(paste("claim must be >=0", "\n", ""))
    if (any(sumsev < 0))
      stop(paste("sum severity of claims must be greater than 0 ", "\n", ""))
    if (any(time==0 && claim>0))
      stop(paste("claim should be 0 in time 0 ", "\n", ""))
    if (any(sumsev==0 && claim>0))
      stop(paste("sumsev must be greater than 0 for claim>0 ", "\n", ""))
    if (any(claim==0 && sumsev>0))
      stop(paste("sumsev must be 0 for claim=0 ", "\n", ""))

    nlirms.frequency.family=c("PGA","PIGA", "PGIG")
    nlirms.severity.family=c( "EGA","EIGA", "EGIG")

    y1=c(1:time)
    y1=rep(y1,claim+1)
    y1=sort(y1)
    y1=c(0,y1)
    y2=0:claim
    y2=rep(y2,time)
    y2=c(0,y2)

    frequency.family <- nlirms.family(family[[1]])$family[[1]]
    enc.f <- paste("enc.", nlirms.family(family[[1]])$family[[1]], sep = "")

    switch(length(nlirms.family(family[[1]])$parameters), {
      enc <- if (frequency.family %in% nlirms.frequency.family) {
        eval(call(enc.f, time = y1, claim=y2, mu = fmu))
      } else stop(paste("first family should be Discrete", "\n", ""))
    }, {
      enc <- if (frequency.family %in% nlirms.frequency.family)
        eval(call(enc.f, time = y1, claim=y2, mu = fmu, sigma=fsigma))
      else stop(paste("first family should be Discrete", "\n", ""))
    }, {
      enc <- if (frequency.family %in% nlirms.frequency.family)
        eval(call(enc.f, time = y1, claim=y2, mu = fmu, sigma=fsigma, nu=fnu))
      else stop(paste("first family should be Discrete", "\n", ""))
    })

    severity.family <- nlirms.family(family[[2]])$family[[1]]
    esc.f <- paste("esc.", nlirms.family(family[[2]])$family[[1]], sep = "")

    switch(length(nlirms.family(family[[2]])$parameters), {
      esc<- if (severity.family %in% nlirms.severity.family) {
        eval(call(esc.f, sumsev=sumsev, claim=y2, mu = smu))
      } else stop(paste("second family should be Continues", "\n", ""))
    }, {
      esc<- if (severity.family %in% nlirms.severity.family)
        eval(call(esc.f, sumsev=sumsev, claim=y2, mu = smu, sigma=ssigma))
      else stop(paste("second family should be Continues", "\n", ""))
    }, {
      esc<- if (severity.family %in% nlirms.severity.family)
        eval(call(esc.f, sumsev=sumsev, claim=y2, mu = smu, sigma=ssigma, nu=snu))
      else stop(paste("second family should be Continues", "\n", ""))
    })

    rms<-enc*esc
    rms=round(rms,round)

    x=rep("-",claim)
    x=c(rms[1],x,rms[2:length(rms)])

    if(time==0 && claim==0 ) {x=matrix(fmu*smu,1)}
    else {x=matrix(x,time+1,byrow=T)}

    ttheme<-ttheme_default(base_size = size, padding = unit(c(padlength, padwidth), "mm"))
    table <- tableGrob(x,theme=ttheme,cols=c(paste("claim=",0:claim)), rows=c(paste("time=",0:time)))
    title <- textGrob(paste("Rate Making System based on Posteriori Frequency and Severity Component
",frequency.family,"-",severity.family," Model" ,sep = ""),
                      gp=gpar(fontsize=10))
    padding <- unit(5,"mm")
    table <- gtable_add_rows(table, heights = grobHeight(title) + padding,pos = 0)
    table <- gtable_add_grob(table, title, 1, 1, 1,clip = "off" ,ncol(table))
    grid.draw(table)

    colnames(x)=c(paste("claim=",0:claim))
    rownames(x)=c(paste("time=",0:time))
    print(x)
  }


