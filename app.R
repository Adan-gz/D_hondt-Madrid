
# Esta sencilla APP Shiny ha sido creada para calcular el porcentaje de escaños que recibiría
# cada partido, de los 6 principales que se presentan, en las próximas elecciones
# autonómicas a la Comunidad de Madrid del 4 de Mayo.

library(shiny)
library(tidyverse)


colores <- c("PSOE" =  "#dc0612", "PP" = "#007cc9", "Unidas Podemos" = "#732a66", 
             "Ciudadanos" = "#fa4e00", "Vox" = "#59c232", "Más Madrid" = "#0fddc4",
             "Otro" = "gray20") # esto es para los gráficos

poblacion <- 5112658 


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  titlePanel("4-M: Elecciones a la Asamblea de Madrid"),
  h4("Tú ", strong("decides"), " la ", strong("participación")," y el ", strong("voto."), strong("D'hondt"), " los ", strong("escaños.")),
  
  tabsetPanel(
    
    
    # Primer panel
    tabPanel("Información",
             sidebarLayout(
               sidebarPanel(
                 h4(strong("¿Quieres saber qué color tendrá la Asamblea madrileña?")),
                 h5("En esta aplicación puedes probar todas las combinaciones de resultados posibles que quieras,
                     y verás automáticamente cuántos escaños se llevaría cada partido, y qué bloque alcanzaría la mayoría absoluta."),
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
    tabPanel("Votos y Escaños",
  
  sidebarLayout(
    sidebarPanel(
      h5(strong("Selecciona el número de votos:")),
      
      sliderInput("PP3","PP",value =  719852, min = 0 ,max = 5112658, ticks = FALSE),
      sliderInput("PSOE3","PSOE", value =  884218, min = 0 ,max = 5112658, ticks = FALSE),
      sliderInput("MM3","Más Madrid", value =  475672, min = 0 ,max = 5112658, ticks = FALSE),
      sliderInput("Vox3","Vox", value =  287667, min = 0 ,max = 5112658, ticks = FALSE),
      sliderInput("UP3","Unidas Podemos", value =  181231, min = 0, max = 5112658, ticks = FALSE),
      sliderInput("Cs3","Ciudadanos", value =  629940, min = 0 ,max = 5112658, ticks = FALSE),
      sliderInput("num_blanco","Voto en blanco", value =  25563, min = 0, max = 5112658, ticks = FALSE),
      sliderInput("num_otro","Voto a otros partidos", value = 70555, min = 0,max = 5112658, ticks = FALSE),
      sliderInput("num_nulo","Voto Nulo", value = 13527, min = 0,max = 5112658, ticks = FALSE)
    ),
    
    
    mainPanel(
      br(),
      h6(" En este panel se incluye el proceder común de las calculadoras D'hondt. En la barrera de la izquierda puedes añadir
                     el número de votos a cada candidatura, votos en blanco y nulo. Recuerda que el número máximo de votantes es de 5M (final de cada barra),
                     por lo que si la suma de todos los votos supera el máximo devolverá un error. "),
      
      flowLayout(            
        tableOutput("num_voto_max"),
        tableOutput("participacion3"), # tabla del total real de participacion
        
        tableOutput("tabla3") # bloque de escaños
        
      ) ,
      
      strong("Porcentaje de voto a los partidos:"),
      tableOutput("tabla_porcentaje"),
      plotOutput("plot3", width = "550px", height = "300px") ,
      br(),
      br(),
      h6("Una curiosidad: ¿te quedó claro qué voto importa a la hora de repartir los escaños? Fíjate que si aumentas el Voto Nulo
                 todo lo que quieras, aumentará constantemente la participación (siempre que no superes el 100%), pero no cambiarán los escaños. Esto
                 es porque el voto nulo no se tiene en cuenta en el reparto de escaños. Sin embargo, si aumentas o disminuyes el voto en blanco, no solo sube o cae la
                 participación, sino que también puede cambiar el reparto de escaños. ¿Por qué? Porque los escaños se reparten a partir del voto válido,
                 que es la suma de los votos a todas las candidaturas más el voto en blanco. Es decir, si hay 100 votos y un partido tiene 5, supera la barrera 
                 legal del 5% y tiene derecho a escaño. Pero si el total de votos a aumenta a 110, ahora el partido tiene menos de un 5%, por lo que se queda fuera del reparto. Si lo quieres visualizar,
                prueba a dar a un partido pocos votos,
                 pero los suficientes para que consiga escaños. Ahora aumenta el voto en blanco y verás cómo los pierde.")
      
    ) )
),

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
                        br() ),
           mainPanel(
             br(),
             h6("Soy Romén Adán, politólogo por la U. Pompeu Fabra y estudiante de Máster en Análisis Político y Electoral en la Carlos III.
                Si quieres indagar en el código de R para crear la app puedes verlo en mi ",
                a(href="https://github.com/Adan-gz/D_hondt-Madrid", 
                  "github"),". Y si tienes consejos para mejorarla son más que bienvenidos."),
             br(),
             h6(strong("Ideol")," es una revista que hemos creado entre varios compañeros y compañeras de diferentes disciplinas, mayoritariamente del mundo de las
                        Ciencias Sociales. Esperamos aportar una mirada crítica al análisis de actualidad política y social, partiendo del conocimiento teórico de nuestras
                        ramas de estudio. Y, como no, tratando de sustentar nuestros argumentos en datos. No te olvides de seguirnos en nuestras",
                a(href="https://twitter.com/revista_ideol?ref_src=twsrc%5Egoogle%7Ctwcamp%5Eserp%7Ctwgr%5Eauthor", 
                  "redes") ,"y leernos en ", a(href="https://medium.com/revista-ideol", 
                                               "Medium"), "."),
             
             br(),
             img(src = "ideol.png", heigh = 170, width = 550),
             br()
           )
           
         )
         
)))



server <- function(input, output) {
  ### PANEL 1: NÚMEROS ABSOLUTOS
  output$num_voto_max <- renderTable(
    data.frame(x = "Número máximo de votos", y = "5.112.658") %>% 
      summarise("Número máximo de votos" = "5.112.658")
  )
  
  
  
  output$participacion3 <- renderTable(
    data.frame( blanco = input$num_blanco, 
                nulo = input$num_nulo, 
                otro = input$num_otro,
                PP = input$PP3, PSOE = input$PSOE3, MM = input$MM3,
                UP = input$UP3, Cs = input$Cs3, Vox = input$Vox3 ) %>% 
      
      summarise("Participación" = ((PP+PSOE+MM+UP+Cs+Vox+blanco+nulo+otro ) / 5112658 )*100   )
  )
  
  
  data3 <- reactive({ # calculo d'hondt para este panel
    
    data.frame(asiento = 1:136, PP = input$PP3, PSOE = input$PSOE3, MM = input$MM3, Vox = input$Vox3,
               UP = input$UP3, Cs = input$Cs3,  blanco = input$num_blanco, otro = input$num_otro) %>% 
      
      mutate( voto_valido = PP+PSOE+MM+UP+Cs+Vox+blanco+otro ,
              PP = ifelse( PP /voto_valido >= 0.05 , PP , 0 ) ,
              
              PSOE = ifelse( PSOE /voto_valido >= 0.05 , PSOE , 0 ),
              MM = ifelse( MM /voto_valido >= 0.05 , MM , 0 ),
              Vox = ifelse( Vox /voto_valido >= 0.05 , Vox , 0),
              UP = ifelse( UP /voto_valido >= 0.05 , UP , 0 ),
              Cs = ifelse( Cs /voto_valido >= 0.05 , Cs , 0) ,
              otro = ifelse( otro /voto_valido >= 0.05 , otro , 0)) %>% 
      
      transmute(PP = PP / asiento, PSOE = PSOE / asiento, "Más Madrid" = MM / asiento,
                Vox = Vox / asiento, "Unidas Podemos" = UP / asiento, "Ciudadanos" = Cs / asiento,
                "Otro" = otro / asiento) %>% 
      select(PP:Otro) %>% 
      pivot_longer(1:7) %>% 
      arrange(desc(value)) %>% 
      head(136) %>% 
      count(name) %>% 
      select(Partido = name , Asientos = n)
  })
  
  
  
  output$tabla3 <- renderTable( #tabla de bloques
    
    if ( (input$num_blanco + input$num_otro + input$PP3 + input$PSOE3 + input$MM3 +
          input$UP3 + input$Cs3 + input$Vox3 + input$num_nulo )/5112658 > 1   ){
      
      return("La participación no puede ser más del 100%")
      
    } else {
      
      data3() %>% 
        mutate(Bloque = case_when(
          Partido %in% c("PP","Vox","Ciudadanos") ~  "Derecha",
          Partido %in% c("PSOE","Unidas Podemos","Más Madrid") ~ "Izquierda",
          Partido == "Otro" ~ "Otro") ) %>% 
        filter(!is.na(Bloque)) %>% 
        group_by(Bloque) %>% 
        summarise("Escaños" = sum(Asientos) )
      
    }
    
    
  )
  
  
  output$tabla_porcentaje <- renderTable({
    
    valido <- input$PP3+input$PSOE3+input$MM3+input$Vox3+input$UP3+input$Cs3+input$num_blanco+input$num_otro
    
    data.frame("PP" = input$PP3, "Ciudadanos" = input$Cs3, "Vox" = input$Vox3,
               "PSOE" = input$PSOE3,  "Unidas Podemos" = input$UP3, "Más Madrid" = input$MM3, 
               "Blanco" = input$num_blanco, "Otro" = input$num_otro ) %>% 
      pivot_longer(1:8, names_to = "Partidos") %>% 
      mutate( value =  round( (value / valido)*100,2) ) %>%
      
      #filter( Partidos %in% c("PP","PSOE","Más.Madrid","Unidas.Podemos","Vox","Ciudadanos","blanco","input.num_otro") ) %>% 
      
      mutate( value = paste(value, " %")
      ) %>% select(Partidos, "Voto" = 2) %>% 
      pivot_wider(names_from = "Partidos", values_from = "Voto")
      
  })
  
  
  output$plot3 <- renderPlot({ 
    
    if ( (input$num_blanco + input$PP3 + input$PSOE3 + input$MM3 +
          input$UP3 + input$Cs3 + input$Vox3 + input$num_nulo + input$num_otro )/5112658 > 1   ){
      
      data.frame(x = 1, y = 3) %>% 
        ggplot(aes(x,y))+
        geom_label(aes(label = "La participación no puede ser más del 100%"))+
        theme_void()
      
    } else {
      
      data3() %>% 
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

# Run the application 
shinyApp(ui = ui, server = server)
