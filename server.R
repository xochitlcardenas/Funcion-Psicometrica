
server <- function(input, output) {
  
  
  ########################### Curva y distribucion normal
  output$curvedistnorm <- renderPlot({ 
    x1 <- c(2,3,4,5,6)
    y1 <- c(0.02,0.16, 0.5, 0.84, 0.98) 
    
    options(repr.plot.width = 12, repr.plot.height = 10)
    layout(matrix(1:2,ncol=2))
    
    curve(pnorm(x,4,1),xlim=c(0,8), ylim = c(0,1),col="blue",lwd=2, 
          xlab="Intensidad del Estímulo",ylab="Proporción de respuestas [Yes]", axes= F)
    points(x1,y1, lwd = "1")
    axis(1,pos=0,padj=0.5)
    axis(2,pos=0,las=2)
    fabline <- function (level){
      if(input$stimulus1=="2"){abline(a=NULL,b=NULL, h =0.02, lwd=1, lty=2 )}
      else if(input$stimulus1=="3"){abline(a=NULL,b=NULL, h =0.16 , lwd=1, lty=2)}
      else if(input$stimulus1=="4"){abline(a=NULL,b=NULL, h =0.5 , lwd=1, lty=2)}
      else if(input$stimulus1=="5"){abline(a=NULL,b=NULL, h =0.84, lwd=1, lty=2 )}
      else {abline(a=NULL,b=NULL, h =0.98, lwd=1, lty=2 )}
    }
    abline(a=NULL,b=NULL, h= fabline(input$stimulus1) , v = input$stimulus1, lwd=1, lty=2)
    #abline(a=NULL,b=NULL, h= 0.5, v = input$stimulus1, lwd=1, lty=2)
    
    regionX=seq(0.02,input$stimulus1,0.01)            # Intervalo a sombrear
    xP <- c(0.,regionX,input$stimulus1)             # Base de los polígonos que crean el efecto "sombra"
    yP <- c(0,dnorm(regionX,4,1),0)
    curve(dnorm(x,4,1),col="blue",lwd=4,yaxs="i",xlim = c(0,8), ylim = c(0,0.4),
          xlab="Intensidad del Estímulo en el umbral momentario",ylab="Curva Normal Ordinal", axes=F)
    axis(1,pos=0,padj=0.5)
    axis(2,pos=0,las=2)
    polygon(xP,yP,col="#BFD1D7")
    
  })
  
  
  
  
  #####################################Estimación de curva parametrica 
  
  p <- c(0, log(1), 0.01, 0)
  names(p) <- c('alpha', 'logbeta', 'lambda', 'gamma')
  
  # generate the sampling scheme
  xr <- reactive({	
    # obtain the quantiles corresponding to range of expected answer probabilities
    xRange <- qpsy(input$xRange, p)
    seq(xRange[1], xRange[2], length=input$nPoints) # stimulus intensities used in the computations
  }) 
  
  # generate the data
  getData <- reactive({
    # take dependance of the action button
    input$runAgain
    
    llply(1:input$nBoot, function(i) data.gen(xr(), p, input$nObs))
  })
  
  # fit the curve
  modelFit <- reactive({
    laply(getData(), function(df) getParam(fitPsy(df, opts=input), opts=input), .drop=FALSE)
  })
  
  # get the threshold
  computeTheta <- reactive({
    aaply(modelFit(), 1, function(v) qpsy(input$criterion, v), .drop=FALSE)
  })
  
  # generate the curves for plotting
  bootPred <- reactive({
    # TODO adjust all these functions
    results <- modelFit()
    
    # generate a vector of stimulus intensities
    x <- seq(qnorm(0.01, p[1], exp(p[2])), qnorm(0.99, p[1], exp(p[2])), length=101)
    # generate an array of predicted probabilities, given x and the bootstrapped parameter estimates
    bootSamples <- aaply(results, 1, function(v) ppsy(x,v), .drop=FALSE, .parallel=FALSE)
    dimnames(bootSamples) <- list(iteration=1:input$nBoot, x=x)
    #reshape2::melt(bootSamples)
    t(bootSamples)
    
  })
  
  
  
  
  ############ generate the plot
  output$curvepsy <- renderPlot({
    
    aa <- bootPred()
    x <- as.numeric(dimnames(aa)[[1]])
    
    theta <- qpsy(input$criterion, p)
    samp <- xr()
    
    
    # define the plotting parameters
    # ------------------------------
    opar <- par(
      bty = 'n',
      mgp=c(2, 0.75, 0))
    colLines <- 'grey50' 	# color for line annotations
    colTxt <- 'grey30'		# color for text annotations
    colTheta <- 'red'
    alpha <- min(0.5, 20/input$nBoot)	# alpha, as a function of the number of bootSamples
    
    # define the layout
    layout(matrix(1:2, ncol=1, nrow=2), heights=c(2, 1))
    # layout.show(2)
    
    # Primer plot
    # ----------
    par(mar=c(0, 3, 0, 0))
    # plot the simulated lines
    matplot(x, aa, type='l', 
            lty=1, col=rgb(0,0,0,alpha), 
            xlim = range(x),
            ylim = c(0,1.1), 
            main=NULL, xlab='', ylab='desempeño', 
            axes=FALSE) 
    # add the generative function
    curve(ppsy(x, p), col=colTheta, lwd=ifelse(input$nBoot>10, 2, 1), add=TRUE)	
    # add the threshold
    abline(h = input$criterion, col=colLines, lty=3)
    segments(theta, input$criterion, theta, -2, col=colTheta, lty=2)
    # add annotations
    text(x = max(x), y = input$criterion, labels=c('criterio\numbral'), adj=c(1, 1.2), col=colTxt)
    text(x = theta, y = 0, labels=c('umbral real'), adj=c(-0.1, 0.5), col=colTheta)
    
    if (input$nBoot==1) {
      # show the real points
      dd <- getData()[[1]]
      points(dd[,'x'], dd[,'nYes']/(dd[,'nYes']+dd[, 'nNo']), pch = 19 )
      
    } else {
      # add the sampling scheme
      points(samp, y=rep(1.075, length(samp)), pch=16, col=colLines)
      segments(min(samp), 1.075, max(samp), 1.075, col=colLines)
      text(min(samp), 1.075, labels='esquema de sampleo', adj=c(0.01, -0.7), col=colTxt)	
    }
    
    # add left axis
    axis(2)
    
    # add legend
    legend('topleft', inset=c(0.025, 0),
           legend=c('original', 'ajustado'),
           col=c(colTheta, rgb(0,0,0,alpha)) , lty=1,
           title='Funciones psicométricas', 
           bty='n')
    
    # segundo plot
    # -----------
    thetaHat <- computeTheta()
    thetaHist <- hist(thetaHat, breaks=pretty(x, n = 100), plot=FALSE)
    if (input$nBoot > 1) {
      thetaDens <- density(thetaHat)
      yRange <-  c(0, max(c(thetaHist$density, thetaDens$y)))
    } else {
      yRange <- c(0, max(thetaHist$density))
    }
    
    
    par(mar=c(3, 3, 0, 0))
    # plot the histogram
    plot(thetaHist, freq=FALSE, 
         col='grey80', border='grey70',
         main=NULL, 	xlab='Intensidad del estímulo (unidad arbitraria)', ylab=NULL, 
         xlim=range(x), ylim=1.1*yRange, 
         axes=FALSE)
    # add the density (if there are more than one generated data set)
    if (input$nBoot > 1)
    {
      lines(thetaDens$x, thetaDens$y, col=colLines)
      # add annotations
      text(min(thetaDens$x), 0, label='distribucion\nde la estimación del umbral', col=colTxt, adj=c(1.05, -0.2))
    } else {
      text(min(thetaHat), 0, label='umbral estimado', col=colTxt, adj=c(1.1, -0.3))
    }
    # add the threshold
    abline(v=theta, col=colTheta, lty=2)
    # add bottom axis
    axis(1)
    
    par(opar)
  })
  
  ##########################################Familia de Funciones Plots  
  
  output$curvestypes <- renderPlot ({        #render permite generar codigo de gráfica  
    pointsxa <- c(-3,-2,-1,0,1,2,3)
    pointsy<- c(0,0.025,0.205,0.490,0.890,0.980,1)
    pointsyb <- c(0.015,0.670,0.900,0.935,0.975,0.985,0.975)
    pointsxb <- c(0.1,1.75,3.4,5.05,6.7,8.35,10)
    
    Intensity <- seq(-3, 3, len = 100)
    Intensity2 <- seq(0.1,10,len = 200)
    
    par(cex.axis=1.5,fg='#000000',col.axis='#000000')
    options(repr.plot.width = 12, repr.plot.height = 10)
    
    if (input$family=='Gaussianas'){
      plot(NULL,xlab="Nivel del Estímulo",ylab = "P[Sí]", xlim=c(-3,3), ylim=c(0,1)) #plot sólo gráfica el cuadro, limites de ejes y etiquetas de ejes
      if(input$typeofcurve1 == "g"){
        lines(Intensity, pnorm(Intensity), type = "l", 
              lwd = 2,col='blue')
      }else if(input$typeofcurve1 == "l"){
        lines(Intensity,plogis(Intensity, scale = sqrt(3)/pi),   #lines gráfica las curvas
              lty = 2, lwd = 3, col='green')
      }
      else if(input$typeofcurve1 == "c"){
        lines(Intensity, pcauchy(Intensity, scale = 5/8), lty = 3,
              lwd = 3, col='red')
      }
      else{
        lines(Intensity, pnorm(Intensity), type = "l", 
              lwd = 2,col='blue')
        lines(Intensity,plogis(Intensity, scale = sqrt(3)/pi),
              lty = 2, lwd = 3, col='green')
        lines(Intensity, pcauchy(Intensity, scale = 5/8), lty = 3,
              lwd = 3, col='red')
      }
      #Los ciclos if permiten correr instrucciones bajo ciertas condiciones, en este caso, permite visualizar una curva 
      #a partir de la eleccion en el cuadro de la izquierda.
      habline <- function (level){
        if(input$stimulus2a=="-3"){abline(a=NULL,b=NULL, h =0, lwd=1, lty=2 )}
        else if(input$stimulus2a=="-2"){abline(a=NULL,b=NULL, h =0.025 , lwd=1, lty=2)}
        else if(input$stimulus2a=="-1"){abline(a=NULL,b=NULL, h =0.205 , lwd=1, lty=2)}
        else if(input$stimulus2a=="0"){abline(a=NULL,b=NULL, h =0.490, lwd=1, lty=2 )}
        else if(input$stimulus2a=="1"){abline(a=NULL,b=NULL, h =0.890, lwd=1, lty=2 )}
        else if(input$stimulus2a=="2"){abline(a=NULL,b=NULL, h =0.980 , lwd=1, lty=2)}
        else {abline(a=NULL,b=NULL, h =1, lwd=1, lty=2 )}
      }
      abline(a=NULL,b=NULL, h= habline(input$stimulus2a) , v = input$stimulus2a, lwd=1, lty=2)
      
      points(pointsxa, pointsy, lwd=2,cex=1.5,pch=1) #points gráfica los puntos de los datos u observaciones
      
    }
    else{
      plot(NULL,xlab = "Intensity", ylab = "P[Intensity]", log="x", xlim=c(0.1,10),ylim=c(0.001,1)) #plot sólo gráfica el cuadro, limites de ejes y etiquetas de ejes
      if(input$typeofcurve2 == "nl"){
        lines(Intensity2, plnorm(Intensity2), type = "l", 
              lwd = 2,col='blue')
      }else if(input$typeofcurve2 == "w"){
        lines(Intensity2,pweibull(Intensity2, shape = 1, scale = 1.5),   #lines gráfica las curvas
              lty = 2, lwd = 3, col='green')
      }
      else{
        lines(Intensity2, plnorm(Intensity2), type = "l", 
              lwd = 2,col='blue')
        lines(Intensity2,pweibull(Intensity2, shape = 1, scale = 1.5),   #lines gráfica las curvas
              lty = 2, lwd = 3, col='green')
      }
      #Los ciclos if permiten correr instrucciones bajo ciertas condiciones, en este caso, permite visualizar una curva 
      #a partir de la eleccion en el cuadro de la izquierda.
      habline <- function (level){
        if(input$stimulus2b=="0"){abline(a=NULL,b=NULL, h =0, lwd=1, lty=2 )}
        else if(input$stimulus2b=="1"){abline(a=NULL,b=NULL, h =0.48 , lwd=1, lty=2)}
        else if(input$stimulus2b=="2"){abline(a=NULL,b=NULL, h =0.75, lwd=1, lty=2)}
        else if(input$stimulus2b=="3"){abline(a=NULL,b=NULL, h =0.86, lwd=1, lty=2 )}
        else if(input$stimulus2b=="4"){abline(a=NULL,b=NULL, h =0.92, lwd=1, lty=2 )}
        else if(input$stimulus2b=="5"){abline(a=NULL,b=NULL, h =0.935 , lwd=1, lty=2)}
        else if(input$stimulus2b=="6"){abline(a=NULL,b=NULL, h =0.95 , lwd=1, lty=2)}
        else if(input$stimulus2b=="7"){abline(a=NULL,b=NULL, h =0.96 , lwd=1, lty=2)}
        else if(input$stimulus2b=="8"){abline(a=NULL,b=NULL, h =0.97, lwd=1, lty=2 )}
        else if(input$stimulus2b=="9"){abline(a=NULL,b=NULL, h =0.98, lwd=1, lty=2 )}
        else {abline(a=NULL,b=NULL, h =1, lwd=1, lty=2 )}
      }
      abline(a=NULL,b=NULL, h= habline(input$stimulus2b),v = input$stimulus2b, lwd=1, lty=2)
      points(pointsxb, pointsyb, lwd=2,cex=1.5,pch=1)
    }
    
  }) #cierra output de curvestypes
  
  ############################ Media - SD  
  output$media <- renderPlot ({ 
    
    options(repr.plot.width = 12, repr.plot.height = 10)
    layout(matrix(1:2,ncol=2))
    
    
    curve(pnorm(x,mean=input$mean,input$desviation),xlim=c(0,16), ylim = c(0,1),col="blue",lwd=2, 
          xlab="Intensidad del Estímulo",ylab="Proporción de respuestas [Yes]", axes= F)
    axis(1,pos=0,padj=0.5)
    axis(2,pos=0,las=2)
    abline(a=NULL,b=NULL, h= 0.5, v = input$mean, lwd=1, lty=2)
    
    curve(dnorm(x,input$mean,input$desviation),col="blue",lwd=2,yaxs="i",xlim = c(0,16), ylim = c(0,0.4),
          xlab="Intensidad del Estímulo en el umbral momentario",ylab="Curva Normal Ordinal", axes=F)
    axis(1,pos=0,padj=0.5)
    axis(2,pos=0,las=2)
    abline(a=NULL,b=NULL, h= NULL, v = input$mean, lwd=1, lty=2)
  })
  
  
} #cierra server
