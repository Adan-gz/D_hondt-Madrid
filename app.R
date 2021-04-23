
# Esta APP Shiny ha sido creada para calcular el porcentaje de escaños que recibiría
# cada partido, de los 6 principales que se presentan, en las próximas elecciones
# autonómicas a la Comunidad de Madrid del 4 de Mayo.

library(shiny)
library(tidyverse)
#library(htmltools)
#library(bslib) # los theme no me funcionan

# Primero creo el algoritmo para calcular los asientos por medio de D'hondt

d_hondt <- function(participacion, blanco = NULL, nulo = NULL, PP, PSOE, MM, Vox, Podemos, Ciudadanos){
   
    library(tidyverse)
    
    poblacion <- 5112658 # poblacion total
    
    votantes <- (participacion + blanco) 
        # en Resultados Aproximados es asume que %participación = %voto válido; mientras que en el Exacto
        # se sumaría el porcentaje de Blanco
    
    asiento <- 1:136 # creo un dataframe con 136 filas, en las columnas los partidos para dividir por cada escaño en cada celda
    
    data <-  data.frame(asiento) %>% 
        mutate( # los valores en la función tienen que introducirse como tanto por 1 para facilitar aqui los calculos
                PP = ifelse(PP < 0.05, 0, PP), PSOE = ifelse(PSOE < 0.05, 0, PSOE), 
                MM = ifelse(MM < 0.05, 0, MM), Vox = ifelse(Vox < 0.05, 0, Vox),
                Podemos = ifelse(Podemos < 0.05, 0, Podemos), 
                Ciudadanos = ifelse(Ciudadanos < 0.05, 0, Ciudadanos) ) 
    # barrera legal: si no alcanzan el 5, su valor es 0, y así quedan fuera del cálculo posterior
   
     suma <- data %>% summarise(PP + PSOE + MM + Vox + Podemos + Ciudadanos + blanco) %>% pull()
    
     # le introduzco condiciones para que la fórmula no se ejecute si se introducen mal los parámetros
     
    if (participacion > 1) {
        return("Error") # la participacion no puede ser superior al 100%
   
    } else if ( participacion < 0   ){ # ni menor al 0% (estas dos condiciones son pensando en el Panel Aproximados)
        return("Error")
        
    } else if ( participacion + blanco + nulo > 1   ){ #esta suma tampoco puede mayor 1
        return("Error")
        
    } else if ( participacion + blanco + nulo < 0   ){ #ni menor de 0
        return("Error")
        # estas dos condiciones, donde si se tiene en cuenta la participacion real, son pensando en el Panel Exactos
        
    } else if ( suma > 1   ){ #la suma de 6 partidos mas blanco no puede superar el 100%
        return("Error")
        
    } else {
        # este sería el algoritmo D'hondt
        
       data %>% mutate( # así paso de porcentaje a números absolutos y divido este valor por los 136 escaños uno a uno
                    partido_PP = (PP * poblacion * votantes)/ asiento, # Ejemplo: 0.35 * 5M * 0.7 equivale a más de 1M de votantes
                    partido_PSOE  = (PSOE * poblacion * votantes)/ asiento,
                    "partido_Más Madrid" = (MM * poblacion * votantes)/ asiento, 
                    partido_Vox = (Vox * poblacion * votantes)/ asiento,
                    "partido_Unidas Podemos" = (Podemos * poblacion * votantes)/ asiento,
                    partido_Ciudadanos = (Ciudadanos * poblacion * votantes)/asiento )  %>% 
            select(-asiento) %>% # quito la variable escaños
            pivot_longer(partido_PP:partido_Ciudadanos,  # pivoto la tabla para tener una columna con los nombres de los partidos y
                         # otra con el resultado de las divisiones
                         names_to = "Partido", values_to = "division") %>% 
            arrange(desc(division)) %>% # las ordeno de mayor a menor
            head(136) %>% # me quedo con las 136 primeras
            count(Partido) %>%  # contabilizo el número de veces que aparece cada partido, lo que equivale a número de escaños
            transmute(Partido = gsub(pattern =  "partido_", replacement =  "", x = Partido),
                      Asientos = n) #quito esa parte del nombre de la variable Partido, y renombro
        # así obtengo un data.frame con los escaños, tras haber introducido en la fórmula porcentajes
      
    }
    
}

colores <- c("PSOE" =  "#dc0612", "PP" = "#007cc9", "Unidas Podemos" = "#732a66", 
             "Ciudadanos" = "#fa4e00", "Vox" = "#59c232", "Más Madrid" = "#0fddc4") # esto es para los gráficos

ui <- fluidPage(
    #theme = bs_theme(bg = "black", fg = "white", primary = "purple"), no me funciona
    
    titlePanel("4-M: Elecciones a la Asamblea de Madrid"),
    h4("Tú ", strong("decides"), " la ", strong("participación")," y el ", strong("voto."), strong("D'hondt"), " los ", strong("escaños.")),
    
 tabsetPanel(
        # Primer panel
     tabPanel("Información",
              sidebarLayout(
                  sidebarPanel(
                     h4(strong("¿Quieres saber qué color tendrá la Asamblea madrileña?")),
                     h5("En esta aplicación podrás decidir qué participación habrá, el porcentaje de voto que se lleva cada partido y, si quieres, el voto blanco y nulo. 
                        Automáticamente se calculará el número de escaños real de cada partido, y la suma de los bloques. "),
                     h5("La primera pestaña es para quien solo quiera curiosear. La segunda es para quien quiera resultados exactos. Aunque las diferencias son mínimas."),
                     h5("La aplicación está diseñada para que introduzcas porcentajes, no números absolutos de voto. Puedes ver más detalles en la Metodología."),
                     h5("¡Espero que te sea útil!"),
                     br(),
                     br(),
                     img(src = "imagen.png", heigh = 50, width = 300) # imagen de madrid
                  ),
                  
                  mainPanel(
                      br(),
                      br(),
                    h5(strong("Pero antes, ¿qué necesitas saber?")),
                    h5("· Lo primero: ", strong("D'hondt NO es una ley"), ", es una fórmula matemática. Simplemente es una forma concreta de repartir los escaños según los votos de los partidos."),
                    h5("· Se reparten ", strong("136 escaños")," y la circunscripción es la Comunidad entera."),
                    h5("· La ",strong("participación")," incluye el voto a partidos, el voto en blanco y el voto nulo."),
                    h5("· Pero a la hora de calcular los escaños, se tiene en cuenta el total de ", strong("votos válidos")," y aquí queda fuera el nulo."),
                    h5("· Finalmente, solo los partidos que saquen al menos un 5% de voto válido pueden conseguir escaños. Lo que llamamos ", strong("barrera legal.")),
                    br(),
                    h5("¡Ahora es tu turno!"),
                    br(),
                    br()
                  )
                  
              )),
      # Segundo Panel
     tabPanel("Resultados Aproximados",
              
              sidebarLayout(
                  sidebarPanel(
                      h6(" 1. Escoge el", strong("nivel de participación"), " y el",strong("porcentaje de voto"), "que conseguirá cada partido. Puedes usar los decimales que quieras."),
                      h6(" 2. No olvides que hay ", strong("barrera legal")," del 5%. Si a uno le das menos, no recibirá escaño."),
                      h6(" 3. Ten en cuenta que la suma de los partidos no puede ser superior al 100%.")
                      ),
              
                  mainPanel(
                      br(),
                      flowLayout(            
                          textInput("part1","Participación", "70,00"), 
                              # la fórmula de D'hondt requiere que estén en 0.05 , pero lo pongo como texto para que sea más cómodo a las personas,
                          # luego en la parte de server lo transformo a numerico para poder operar
                          
                          tableOutput("suma1")), # con esto saco una tabla que sume el total de voto a los partidos

                      flowLayout( # para que la gente ponga el voto a los partidos
                          textInput("PP1","PP",value =  "23"),
                          textInput("PSOE1","PSOE", value =  "27"),
                          textInput("MM1","Más Madrid", value =  "15"),
                          textInput("Vox1","Vox", value =  "10"),
                          textInput("UP1","Unidas Podemos", value =  "6"),
                          textInput("Cs1","Ciudadanos", value =  "15") 
                          
                           )
                      )),
              flowLayout(
                
               tableOutput("tabla1"), # tabla de escaños por bloques ideologicos
               plotOutput("plot1", width = "550px", height = "300px") ), # grafico de escaños
              br(),
               h6("*Nota: Respecto a participación, en este caso se asume que voto blanco y voto nulo son iguales a 0. 
                  Y respecto al total de votos, si la suma de partidos es 100%, se asumiría que los madrileños sólo votaron a estos
                  partidos y que no hubieron votos en blanco ni a candidaturas pequeñas. Si quieres poder ajustar manualmente el voto en blanco y nulo, puedes hacerlo en la pestaña de Resultados Exactos."),
              br(),
              br()
              
              
              
     ),
     
      # Tercer Panel
     tabPanel("Resultados Exactos",
               
              sidebarLayout(
                  sidebarPanel(
                      h6(" 1. Escoge el", strong("nivel de participación,"), " y el",strong("porcentaje de voto"), " a ", strong("partidos,")," en", strong(" blanco")," y ", strong("nulo.")),
                      h6(" 2. Recuerda que la ", strong("participación"), " es igual a: ", strong("participación (sin VB,VN) + voto en blanco + voto nulo.")),
                      h6(" 3. Pero en el caso del ", strong("voto válido"), " es solo ", strong("participación (sin VB,VN) + voto en blanco.")),
                      h6(" 4. Ten en cuenta que la ", strong("suma de % de partidos y voto en blanco no"), " puede ", strong("superar")," el ", strong("100%.")),
                      h6(" 5. No olvides que la ", strong("barrera legal"), " está en el 5%.")
                      ),
                  

                  mainPanel(
                      br(),
                      flowLayout(            
                          textInput("part","Participación sin VB/VN", value =  "65,5"),
                          textInput("blanco","Voto en blanco (VB)", value =  "0,5"),
                          textInput("nulo","Voto Nulo (VN)", value = "0,1"),
                          tableOutput("participacion"), # tabla del total real de participacion
                          tableOutput("valido"), # de voto valido
                          tableOutput("suma")), # suma de los partidos
                      
                      
                      flowLayout(
                          textInput("PP","PP",value =  "23"),
                          textInput("PSOE","PSOE", value =  "27"),
                          textInput("MM","Más Madrid", value =  "15"),
                          textInput("Vox","Vox", value =  "10"),
                          textInput("UP","Unidas Podemos", value =  "6"),
                          textInput("Cs","Ciudadanos", value =  "18,5") 
                          
                           ) 
                      ) ),
              
              flowLayout(
                  tableOutput("tabla"), # bloque de escaños
                  plotOutput("plot", width = "550px", height = "300px"), # grafico de escaños
                  br(),
                  br(),
                  br()
                   
                   )
    
     ),
     
     tabPanel("Metodología",
              sidebarLayout(
                  sidebarPanel(strong("¿Cómo se calculan los escaños?")),
                  mainPanel(
                      br(),
                      h6("En la mayor parte de los simuladores de la fórmula D’hondt te piden que
                        introduzcas el número exacto de votos (por ejemplo, 1.508.497). Esto es porque esta fórmula 
                        necesita los valores en términos absolutos, y no en porcentaje. Sin embargo, creo que a la mayoría 
                        de nosotros nos es más fácil pensar en términos de %, que en millones o cientos de miles de voto. 
                        Es raro que nos preguntemos si Ciudadanos se llevará 350.789 votos o 340.823. La preocupación de muchos más 
                        bien es si alcanzará el 5%. Es por este motivo, por una cuestión de comodidad, que he diseñado la aplicación
                        para introducir porcentajes. Pero, ¿cómo funciona?"),
                      h6("Se parte de que un 100% de participación el 4 de mayo implicaría que habrían ido a votar 5.112.658 de personas 
                        (INE). En el caso de Resultados Aproximados es bastante intuitivo. Como dejamos fuera el voto blanco y nulo 
                        (asumimos fueron un 0%), un 70% de participación significa que acudirán a las urnas más de 3,5 millones. Como
                        para nosotros todos estos votos serán válidos, entran en el cálculo de la fórmula D’hondt. "),
                      h6("¿Cuántos votos absolutos tendría cada partido? Si la participación es del 70% y hay 3,5 M de votos, un partido 
                        que haya conseguido un 35% este voto válido sumaría unos 1,25 millones de votos. Es decir, es el resultado de multiplicar 
                        1.508.497 ", em("x"), " participación ", em("x"), " voto al partido A. Haciendo esto para los otros 5 partidos (si es que todos consiguieron un 5% 
                        del voto válido) ya tendríamos los votos en valores absolutos. Para pasar a escaños se programa una fórmula que siga la lógica 
                        de la fórmula D’hondt."),
                      h6("En el caso de Resultados Exactos solo se produce un paso previo. He dividido entre Participación sin voto blanco ni nulo 
                        para hacerlo intuitivo y sencillo. De esta forma, la participación real es la suma de éste porcentaje, más el voto blanco y el
                        nulo. Mientras que en el voto válido, tras sumar el blanco se resta el nulo. Y es el porcentaje de voto válido el que se multiplica por 
                        1.508.497 y por voto al partido. Así obtenemos los valores absolutos de voto a cada partido. Lo siguiente ya es aplicar la fórmula D'hondt
                         que se había programado. Si quieres saber cómo distribuye este método los escaños ", 
                         a(href="https://es.wikipedia.org/wiki/Sistema_D%27Hondt", 
                                                                                                                "Wikipedia")," nunca defrauda",icon("smile-wink"),".")
                
                         )
              )),
     tabPanel("Autor y Revista ideol",
              sidebarLayout(
                  sidebarPanel(br(),
                               strong("Autor"),
                               br(),
                               br(),
                               br(),
                               br(),
                               strong("Revista ideol"),
                               br(),
                               br(),
                               br()),
                  mainPanel(
                      br(),
                      h6("Soy Romén Adán, politólogo por la U. Pompeu Fabra y estudiante de Máster en Análisis Político y Electoral en la Carlos III.
                         Es la primera app que desarrollo, así ni es lo más visual ni cómoda que habrás visto. Pero espero que te haya sido útil y te haya picado
                         un poco la curiosidad por seguir la campaña. Si quieres indagar en el código de R para crearla puedes verlo en mi github. 
                         Y si tienes consejos para mejorarlo son más que bienvenidos, ¡la app está en fase beta!."),
                     br(),
                     h6(strong("Ideol")," es una revista que hemos creado entre varios compañeros y compañeras de diferentes disciplinas, mayoritariamente del mundo de las
                        Ciencias Sociales. Esperamos aportar una mirada crítica al análisis de actualidad política y social, partiendo del conocimiento teórico de nuestras
                        ramas de estudio, y tratando de sustentar nuestros argumentos en datos. No te olvides de seguirnos en nuestras",
                        a(href="https://twitter.com/revista_ideol?ref_src=twsrc%5Egoogle%7Ctwcamp%5Eserp%7Ctwgr%5Eauthor", 
                          "redes") ,"y leernos en ", a(href="https://medium.com/revista-ideol</a>!</p>", 
                                                       "Medium"), " . Cómo para perdérselo."),
                
                     br(),
                     img(src = "ideol.png", heigh = 170, width = 550),
                     br()
                  ),
                  
              )
         
     )
     )
    
 )
    

server <- function(input, output, session) {
    
    # Segunda Panel
 
    output$suma1  <- renderTable( # tabla de votos totales
        
        data.frame(PP = input$PP1, PSOE = input$PSOE1, MM = input$MM1,
                   Vox = input$Vox1, UP = input$UP1, Cs = input$Cs1) %>% 
            
            mutate(PP = as.numeric(gsub(",",".", as.character(PP)))/100,
                   PSOE = as.numeric(gsub(",",".", as.character(PSOE)))/100,
                   MM = as.numeric(gsub(",",".", as.character(MM)))/100,
                   Vox = as.numeric(gsub(",",".", as.character(Vox)))/100,
                   UP = as.numeric(gsub(",",".", as.character(UP)))/100,
                   Cs = as.numeric(gsub(",",".", as.character(Cs)))/100,
                  # blanco = as.numeric(gsub(",",".", as.character(blanco)))/100,
                   
                   PP = ifelse(PP < 0.05, 0, PP), PSOE = ifelse(PSOE < 0.05, 0, PSOE),
                   MM = ifelse(MM < 0.05, 0, MM), Vox = ifelse(Vox < 0.05, 0, Vox),
                   UP = ifelse(UP < 0.05, 0, UP), Cs = ifelse(Cs < 0.05, 0, Cs)) %>% 
          
              summarise("*Total % voto partidos:" = (PP+PSOE+MM+Vox+UP+Cs)*100)
                 
    )
    
    data1 <- reactive({  # aplico fórmula D'hondt
        
        part1 <- as.numeric(gsub(",",".", as.character(input$part1)))/100
      #  blanco <- as.numeric(gsub(",",".", as.character(input$blanco)))/100
        PP1 <- as.numeric(gsub(",",".", as.character(input$PP1)))/100
        PSOE1 <- as.numeric(gsub(",",".", as.character(input$PSOE1)))/100
        MM1 <- as.numeric(gsub(",",".", as.character(input$MM1)))/100
        Vox1 <- as.numeric(gsub(",",".", as.character(input$Vox1)))/100
        UP1 <- as.numeric(gsub(",",".", as.character(input$UP1)))/100
        Cs1 <- as.numeric(gsub(",",".", as.character(input$Cs1)))/100
        
        d_hondt(participacion = part1, blanco = 0, nulo = 0, # asumimos que son 0
                PP = PP1,
                PSOE = PSOE1, MM = MM1,
                Vox = Vox1, Podemos = UP1,
                Ciudadanos = Cs1
        )
    })
    
    
    
    output$tabla1 <- renderTable( # tabla de escaños por bloque
   
    if (data1() == "Error"){
        "*"}
      else{
        
        data1() %>% 
            mutate(Bloque = case_when(
                Partido %in% c("PP","Vox","Ciudadanos") ~  "Derecha",
                Partido %in% c("PSOE","Unidas Podemos","Más Madrid") ~ "Izquierda")) %>% 
            filter(!is.na(Bloque)) %>% 
            group_by(Bloque) %>% 
            summarise(Escaños = sum(Asientos) )
      }
        
    )
    
    output$plot1 <- renderPlot({ # gráfico
        
     if (data1() == "Error"){ # advertencia si está introduciendo mal los valores
         
         data.frame(x = 1, y = 3, label = "No olvides que:\n1. La participación no puede ser mayor de 100 ni menor de 0.\n 2.La suma del % de voto a los partidos + % voto blanco no puede ser mayor de 100.") %>% 
             ggplot(aes(x = x, y = y, label = label))+
             geom_label()+theme_void()
         
     }
        else{
        data1() %>% 
            ggplot(aes( reorder(Partido, -Asientos), Asientos))+
            theme_minimal()+
            geom_col(aes(fill = Partido))+
            geom_text(aes(x = Partido, y = Asientos-2.5,
                           label = Asientos), size = 4)+
            scale_fill_manual(values = colores)+
            labs( title = "Escaños que obtendría cada partido:",
                  subtitle = "Mayoría absoluta: 69 diputados",
                  x = "", y = "")+
            theme(axis.text.x = element_text(face = "bold"),
                  axis.text.y = element_blank(),
                  legend.position = "none",
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank())
        
        }
    }, res = 96)
    
    
    # Tercer Panel: Resultados Exactos
    
    output$participacion <- renderTable(
        data.frame(part = input$part, blanco = input$blanco,
                   nulo = input$nulo) %>% 
            mutate(part = as.numeric(gsub(",",".", as.character(part)))/100,
                   blanco = as.numeric(gsub(",",".", as.character(blanco)))/100,
                   nulo = as.numeric(gsub(",",".", as.character(nulo)))/100) %>% 
            summarise("*Participación total" = (part+blanco+nulo)*100)
    )
    
    output$valido <- renderTable(
        data.frame(part = input$part, blanco = input$blanco,
                   nulo = input$nulo) %>% 
            mutate(part = as.numeric(gsub(",",".", as.character(part)))/100,
                   blanco = as.numeric(gsub(",",".", as.character(blanco)))/100,
                   nulo = as.numeric(gsub(",",".", as.character(nulo)))/100) %>% 
            summarise("*Voto válido" = (part+blanco)*100)
    )
    
    
    
    output$suma  <- renderTable(
        
        data.frame(PP = input$PP, PSOE = input$PSOE, MM = input$MM,
                   Vox = input$Vox, UP = input$UP, Cs = input$Cs,
                   blanco = input$blanco) %>% 
            
            mutate(PP = as.numeric(gsub(",",".", as.character(PP)))/100,
                   PSOE = as.numeric(gsub(",",".", as.character(PSOE)))/100,
                   MM = as.numeric(gsub(",",".", as.character(MM)))/100,
                   Vox = as.numeric(gsub(",",".", as.character(Vox)))/100,
                   UP = as.numeric(gsub(",",".", as.character(UP)))/100,
                   Cs = as.numeric(gsub(",",".", as.character(Cs)))/100,
                   blanco = as.numeric(gsub(",",".", as.character(blanco)))/100,
                   
                   PP = ifelse(PP < 0.05, 0, PP), PSOE = ifelse(PSOE < 0.05, 0, PSOE),
                   MM = ifelse(MM < 0.05, 0, MM), Vox = ifelse(Vox < 0.05, 0, Vox),
                   UP = ifelse(UP < 0.05, 0, UP), Cs = ifelse(Cs < 0.05, 0, Cs)) %>% 
            
            summarise("*Total % partidos + blanco:" = (PP+PSOE+MM+Vox+UP+Cs+blanco)*100)
        
    )
    
    data <- reactive({ # calculo d'hondt para este panel
        
        part <- as.numeric(gsub(",",".", as.character(input$part)))/100
        blanco <- as.numeric(gsub(",",".", as.character(input$blanco)))/100
        nulo <- as.numeric(gsub(",",".", as.character(input$nulo)))/100
        PP <- as.numeric(gsub(",",".", as.character(input$PP)))/100
        PSOE <- as.numeric(gsub(",",".", as.character(input$PSOE)))/100
        MM <- as.numeric(gsub(",",".", as.character(input$MM)))/100
        Vox <- as.numeric(gsub(",",".", as.character(input$Vox)))/100
        UP <- as.numeric(gsub(",",".", as.character(input$UP)))/100
        Cs <- as.numeric(gsub(",",".", as.character(input$Cs)))/100
        
        d_hondt(participacion = part, blanco = blanco, nulo = nulo,
                PP = PP,
                PSOE = PSOE, MM = MM,
                Vox = Vox, Podemos = UP,
                Ciudadanos = Cs
        )
    })
    
    
    
    output$tabla <- renderTable( #tabla de bloques
        
        if (data() == "Error"){
            "*"}
        else{
            
            data() %>% 
                mutate(Bloque = case_when(
                    Partido %in% c("PP","Vox","Ciudadanos") ~  "Derecha",
                    Partido %in% c("PSOE","Unidas Podemos","Más Madrid") ~ "Izquierda")) %>% 
                filter(!is.na(Bloque)) %>% 
                group_by(Bloque) %>% 
                summarise(Escaños = sum(Asientos) )
        }
        
    )
    
    output$plot <- renderPlot({ 
        
        if (data() == "Error"){ # advierto de errores
            
            data.frame(x = 1, y = 3, label = "No olvides que:\n1. La participación no puede ser mayor de 100 ni menor de 0.\n 2.La suma del % de voto a los partidos + % voto blanco no puede ser mayor de 100.") %>% 
                ggplot(aes(x = x, y = y, label = label))+
                geom_label()+theme_void()
            
        }
        else{
            data() %>% 
                ggplot(aes( reorder(Partido, -Asientos), Asientos))+
                theme_minimal()+
                geom_col(aes(fill = Partido))+
                geom_text(aes(x = Partido, y = Asientos-2.5,
                              label = Asientos), size = 4)+
                scale_fill_manual(values = colores)+
                labs( title = "Escaños que obtendría cada partido:",
                      subtitle = "Mayoría absoluta: 69 diputados",
                      x = "", y = "")+
                theme(axis.text.x = element_text(face = "bold"),
                      axis.text.y = element_blank(),
                      legend.position = "none",
                      panel.grid.major.x = element_blank(),
                      panel.grid.minor.x = element_blank())
            
        }
    }, res = 96)
    
    
   
}

shinyApp(ui, server)


