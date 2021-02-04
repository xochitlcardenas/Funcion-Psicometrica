#
libs <- c('shiny', 'plyr', 'reshape2')
lapply(libs, require, character.only = TRUE)


ppsy <- function(x, p) p[4] + (1- p[3] - p[4]) * pnorm(x, p[1], exp(p[2]))

# function to convert probabilities from the psychometric function 
probaTrans <- function(prob, lambda, gamma) (prob-gamma) / (1-gamma-lambda)

# Quantile function (inverse of the psychometric function)
qpsy <- function(prob, p) qnorm(probaTrans(prob, p[3], p[4]), p[1], exp(p[2]))

# random generation from the psychometric function 
rpsy <- function(x, p, nObs)
{
    prob <- ppsy(x, p)
    rbinom(prob, nObs, prob)
}

# Functions to generate the data and fit the psychometric function
# ----------------------------------------------------------------

# a function to generate a data set 
data.gen <- function(x, p, nObs) 
{
    nYes <- rpsy(x, p, nObs) # simulated number of correct responses
    nNo <- nObs-nYes	# simulated number of incorrect responses
    cbind(x, nYes, nNo)
}

# define the likelihood function
likelihood <- function(p, df, opts) {	
    psi <- ppsy(df[, 1], c(p, 0, 0))
    -sum(df[,2]*log(psi) + df[,3]*log(1-psi))
}

# A function to fit the psychometric function 
fitPsy <- function(df, opts) 
{optim(c(1, log(3)), likelihood, df=df, opts=opts)
}

# a function to extract the parameters of a psychometric function
getParam <- function(obj, opts) {
    out <- c(obj$par, 0, 0)
    names(out) <- c('alpha', 'log(beta)', 'lambda', 'gamma')
    return(out)
}





##ui

ui <- fluidPage(
    shinyUI(
        navbarPage("Curva Psicométrica",
                   tabPanel("Psicofísica",
                            tags$h3("Psicofísica y curva psicométrica"),
                            tags$p("La psicofísica es el estudio de las relaciones entre la sensación (dominio psicológico) y los estímulos (dominio físico)."),
                            br(),
                            tags$p("El estudio se centra en tres tipos de capacidades: detectar estímulos, discriminarlos y estimar su valor. Los primeros se relacionan con el umbral diferencial y el umbral absoluto.",   
                                   "El umbral absoluto es la intensidad mínima de un estimulo para que este genere una sensación. El umbral diferencial es la intensidad mínima para notar una diferencia en el estímulo",
                                   ""),
                            br(),
                            tags$p(em("Puedes encontrar el código de esta app en:")),
                            tags$a (href="//github.com/xochitlcardenas/Funcion-Psicometrica-ShinyApp.git", "Github repository")
                            
                   ),#cierra tabPanel "Psicofísica"
                   
                   tabPanel("Curva Psicométrica",
                            #tags$p(),
                            tabsetPanel(
                                tabPanel(
                                    title = "Curva Psicometrica - Distribución Normal",
                                    tags$h4("Curva Psicometrica - Distribución Normal"), 
                                    tags$p("Fechner asumía que los umbrales sensoriales satisfacen 2 propiedades matemáticas: el umbral es entendido como ",
                                           "una variable aleatoria y dicha variable está asociada a una distribución normal. La base de estos supuestos se debe ", 
                                           "a que el umbral de un participante tiende a variar momento a momento, es decir, en algunas ocasiones un estímulo de 5 unidades ",
                                           "o más será percibido, pero en algunas otras el mismo estímulo no será percibido."),
                                    tags$p("Si uno grafica la frecuencia de cada uno de los umbrales momentáneos a lo largo de un experimento se dará cuenta que la distribución ",
                                           "de umbrales momentáneos es justamente una distribución normal. Este hecho apoya a que la forma de la función que relaciona a la intensidad ",
                                           "de un estímulo con la proporción de veces en que es percibido (o de notar una diferencia apenas perceptible en el caso del umbral diferencial) ",
                                           "sea una ogiva, pues dicha forma es justamente la frecuencia acumulada de una distribución normal."),
                                    sidebarLayout(
                                        sidebarPanel(
                                            sliderInput("stimulus1", "Elige el valor del estímulo", min = 2, max = 6, value = 3)
                                        ),
                                        mainPanel(
                                            plotOutput("curvedistnorm")
                                        )
                                    )
                                ),
                                
                                tabPanel(          #pestaña 1
                                    title = "Estimación de Curva Psicométrica",   
                                    tags$h4("Estimación de Curva Psicométrica"),
                                    tags$p("La función psicométrica relaciona la intensidad de un estímulo externo y las respuestas de en una tarea de clasificación ",
                                           "El objetivo al medir una función psicométrica es estimar un umbral."),
                                    tags$p("Aquí se puede observar el umbral para un conjunto de datos simulados."),
                                    sidebarLayout(
                                        sidebarPanel( 
                                            h4('Parámetros experimentales'),
                                            sliderInput("nPoints", "Número de puntos - datos", min = 3, max = 15, value = 9),
                                            sliderInput("nObs", "Número de observaciones - datos", min = 10, max = 200, value = 50,step=5),
                                            sliderInput("xRange", "Rango de rendimiento", min =0.01, max=0.99,value = c(0.25, 0.85),step=0.01),
                                            sliderInput("criterion", "Umbral", min = 0.1, max=0.9, value = 0.5, step = 0.05),
                                            sliderInput("nBoot", "Número de sets de datos generados", min=1,max=500,value=1),
                                            actionButton('runAgain', 'Generar nuevo sampleo de datos')
                                        ),
                                        mainPanel(
                                            plotOutput("curvepsy")
                                        )
                                    )
                                ), #cierra "Curvas Psicométricas"
                                tabPanel(         #pestaña 2
                                    title = "FCP",
                                    tags$h4("Familia de Curvas Psicométricas"),
                                    tags$p("Para una tarea de clasificación o detección, se necesita un resumen del rendimiento capturado por una pequeña cantidad de parámetros. Para esto existen diferentes funciones psicométricas ",
                                           "que buscan ajustarse al conjunto de datos obtenidos."),
                                    tags$p("Aquí se pueden visualizar diferentes funciones de dos tipos distintos de familias, las cuales se buscan ajustar a un conjunto de datos simulados"),
                                    sidebarLayout( #divide en 2 partes la pantalla
                                        sidebarPanel(
                                            selectInput("family", "Elige la familia de curvas que quieres visualizar", c("Gaussianas", "Logaritmicas" )),
                                            conditionalPanel( condition = "input.family=='Gaussianas'",
                                                              radioButtons("typeofcurve1","Elige la curva que quieres visualizar",c("Gaussiana"="g", "Logística"="l", "Cauchy"="c", "Todas"="t")),
                                                              sliderInput("stimulus2a", "Elige un valor del estímulo", min = -3, max = 3, value = 0)
                                            ),
                                            conditionalPanel( condition = "input.family=='Logaritmicas'",
                                                              radioButtons("typeofcurve2","Elige la curva que quieres visualizar",c("Normal Logaritmica"="nl", "Weibull"="w", "Todas"="t2")),
                                                              sliderInput("stimulus2b", "Elige un valor del estímulo", min = 0, max = 10, value = 5)
                                            )
                                        ), #cierra sidebarPanel
                                        mainPanel(
                                            plotOutput("curvestypes")  
                                        ) #cierra mainPanel
                                    ) #cierra siderbayLayout
                                ), #cierra "FCP"
                                tabPanel(         #pestaña 3
                                    title = "Media - SD",
                                    tags$h4("Media y Desviación Estándar"),
                                    tags$p("De acuerdo con la teoría clásica del umbral, la media y la variabilidad de la distribución de los umbrales momentáneos se puede determinar a través ",
                                           "de la función psicométrica, pues al incrementar la media de la distribución, la función psicométrica sufre un desplazamiento hacia la derecha, mientras ",
                                           "que al disminuir la media el desplazamiento será hacia la izquierda. Por otra parte, la desviación estándar de la distribución es medida por medio de la inclinación ",
                                           "de la función psicométrica, donde una desviación estándar pequeña lleva a una inclinación pronunciada por parte de la función psicométrica, mientras que una desviación ",
                                           "estándar grande lleva una función psicométrica más aplanada."),
                                    sidebarPanel(
                                        sliderInput("mean", "Elige un valor de mu", min =0, max = 16,  value =8),
                                        sliderInput("desviation", "Elige un valor de sigma", min =0, max = 2,  value =1, step = 0.1)
                                    ),
                                    mainPanel(
                                        plotOutput("media")
                                    )
                                ) #cierra "Media - SD"
                                
                            ) #cierra tabsetPanel     
                   )#cierra tabPanel "Curva Psicométríca"
                   
        )#cierra navbarPage
        
    ) #cierra shinyUI
) #cierra fluidPage



##server

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

# Run the app 
shinyApp(ui = ui, server = server)
