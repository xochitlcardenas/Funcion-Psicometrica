
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

