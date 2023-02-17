rm(list = ls())
library(shiny)
library(shinythemes)


ui <- navbarPage(
  "Teorema do Limite Central", theme = shinytheme("cosmo"), id = "tabset",
  
  conditionalPanel(condition = "input.tabset > '0'",
                   sidebarPanel( 
                     selectInput(inputId = "dist", 
                                 label = "Distibuição", 
                                 choices = c("Normal", "Uniforme", "Poisson","Binomial", "Exponencial", "Gamma"),
                                 width = "220px"),
                     
                     conditionalPanel(condition = "input.dist == 'Normal' ", 
                                      div(style="display:inline-block", numericInput("mu","Média", value=0, width = "100px")),
                                      div(style="display:inline-block", numericInput("var","Desvio Padrão", value=1, width = "100px"))
                     ),
                     
                     conditionalPanel(condition = "input.dist == 'Uniforme' ", 
                                      div(style="display:inline-block", numericInput("a","min", value=0, width = "100px")),
                                      div(style="display:inline-block", numericInput("b","max", value=1, width = "100px"))
                     ),
                     
                     conditionalPanel(condition = "input.dist == 'Exponencial' ",  
                                      div(style="display:inline-block", numericInput("lambda","Lambda", value=1, width = "100px")),
                     ),
                     
                     conditionalPanel(condition = "input.dist == 'Poisson' ", 
                                      div(style="display:inline-block", numericInput("lambdap","Lambda", value=5, width = "100px")),
                                      
                     ),
                     
                     conditionalPanel(condition = "input.dist == 'Gamma' ", 
                                      div(style="display:inline-block", numericInput("alfa","Alfa", value=5, width = "100px")),
                                      div(style="display:inline-block", numericInput("beta","Beta", value=2, width = "100px"))
                     ),
                     
                     conditionalPanel(condition = "input.dist == 'Binomial' ",
                                      div(style="display:inline-block", numericInput("nzinho","n", value=10, width = "100px")),
                                      div(style="display:inline-block", numericInput("p","prob", value=0.6, width = "100px"))
                     ),
                     
                     numericInput("seed", "Escolha uma semente:", min = 1, value = 2022, width = "150px"),
                     numericInput("m","Número de Repetições", min=5, max=10000, value=10, width = "160px"),
                     
                     
                     conditionalPanel(condition = "input.tabset < '4'",
                     sliderInput("N", "Tamanho da amostra", min=1, max=100,value=5, width = "300")),
                     
                     conditionalPanel(condition = "input.tabset == '4'",
                                div(style="display:inline-block", numericInput("N1", "Amostra 1", min=1, max=100,value=5)),
                                div(style="display:inline-block", numericInput("N2", "Amostra 2", min=1, max=100,value=30)),
                                div(style="display:inline-block", numericInput("N3", "Amostra 3", min=1, max=100,value=100))
                     )
                     
                     
                  ,width = 3)),
  
  
  
             tabPanel("Início", value = 0, fluid = TRUE,
             h1("Resumo"),
             h4("Neste trabalho foi feito a ilustração do Teorema Central do Limite utilizando-se de simulações
                            de Monte Carlo considerando diferentes tamanhos de amostras e diferentes distribuições de
                            probabilidade.",align = "justify"),
             strong("Palavras-chave: Geração de números pseudo-aleatorios; convergência em distribuição;
                              Simulação de Monte Carlo; Teorema Central do Limite.",align = "justify"),
             
             h1("Introdução"),
             h4("Um resultado visto em teoria das probabilidades, é que uma sequência de variáveis
                              aleatórias converge em distribuição, se e somente se, a sequência de suas funções
                              características convergir pontualmente para a função característica do limite. Este resultado é
                              extremamente importante para a prova do teorema central do limite (MAGALHÃES, 2006).
                              ",align = "justify"),
             h4("O TCL (teorema central do limite) estabelece que a distribuição da soma ou da média,
                              de variáveis aleatórias independentes e identicamente distribuídas (iid) se aproximará de uma
                              distribuição normal a medida que o tamanho de amostra aumenta.",align = "justify"),
             h4("Por conta desse resultado a distribuição normal é tão famosa, sendo bastante utilizada
                            em diversos testes estatísticos. Além disso, o TCL é extremamente importante para a
                            inferência estatística, tornando possível a estimação da média e o desvio padrão populacional
                            a partir de uma amostra extraída dessa população.",align = "justify"),
             h4("O trabalho trata-se de um experimento prático a fim de ilustrar o TCL, durante o
                              desenvolvimento foi utilizado o método de simulação de Monte Carlo, uma técnica
                              probabilística que visa prever possíveis resultados de um evento incerto.",align = "justify"),
             
             h1("Metodologia e Resultados"),
             h4("O código para a demonstração do teorema central do limite e desenvolvimento do
                              projeto foi escrito em linguagem R, e com o auxílio da biblioteca shiny para a organização e
                              exposição dos resultados do experimento. Através da biblioteca shiny foi criada uma interface
                              para facilitar a escolha da distribuição e diferentes tamanhos de amostras pelo usuário.
                              ",align = "justify"),
             h4("Os dados da população foram obtidos através da geração de números pseudoaleatórios e de acordo com a distribuição de probabilidade especificada. As amostras extraídas
                              da população e a quantidade de iterações a serem feitas através das simulações de Monte
                              Carlo podem definidas pelo usuário livremente.",align = "justify"),
             h4("Das distribuições de probabilidades selecionadas, optou-se pelas distribuições,
                              discretas e contínuas, mais conhecidas, sendo elas; Binominal, Poisson, Exponencial,
                              Uniforme, Gamma e Normal.",align = "justify"),
             h4("Partindo de uma semente pré definida (2022) para fins de reprodução do experimento,
                              em todas as simulações efetuadas foram realizadas a geração de 1000 números pseudoaleatórios de cada distribuição. Foram extraídas amostras de tamanho 5, 30 e 100 que tiveram
                              suas médias calculadas e armazenadas. Esse procedimento foi realizado 1000 vezes. Nota-se
                              que com o aumento do tamanho da amostra fica cada vez mais claro a aproximação para a
                              Distribuição Normal.",align = "justify"),

             h4("Para as distribuições discretas foram utilizados os parâmetros n = 10 e p = 0.6 na
                              distribuição binomial e λ = 5 para a distribuição poisson. Já para as distribuições contínuas,
                              foi utilizada a uniforme [0,1], a distribuição normal padrão, a exponencial com parâmetro λ =
                              1 e para a distribuição Gamma α = 5 e β = 1.",align = "justify"),
             
             h1("Considerações Finais"),
             h4("Com as simulações realizadas nesse estudo é possível visualizar de forma mais prática
                              a teoria por trás do teorema central do limite. Através de uma interface responsiva, criada com
                              o intuito de facilitar a testagem do teorema para diferentes funções de distribuição,
                              considerando diferentes parâmetros e tamanhos de amostra, o programa retorna em tempo real
                              a visualização das propriedades descritas no teorema para o usuário. Ainda como resultado, é
                              possível observar a convergência da média amostral para a populacional.",align = "justify")
  ),
  
  tabPanel("Distribuição da Média", value = 1, fluid = TRUE,
           mainPanel(plotOutput("hist"),
                     plotOutput("histmedia"),
                     
                    
           )
  ),
  
  
  tabPanel("Distribuição da Variância", value = 2, fluid = TRUE,
           mainPanel(plotOutput("hist1"),
             plotOutput("histvar")
           )
  ),
  
  tabPanel("Comparações", value = 3, fluid = TRUE, 
           mainPanel(plotOutput("histcompar"),
                     plotOutput("histcompar1"),
                     plotOutput("histcompar2"),
                     plotOutput("histcompar3")
           )
  ),
  
  
  
)


server <- function(input, output, session) {
  x <- reactive({
    set.seed(input$seed)
    switch (input$dist, 
            "Normal"=rnorm(input$m, input$mu, input$var),
            "Uniforme" = runif(input$m, input$a, input$b ),
            "Poisson" = rpois(input$m, input$lambdap),
            "Exponencial" = rexp(input$m, input$lambda),
            "Gamma" = rgamma(input$m, input$alfa, input$beta),
            "Binomial" = rbinom(input$nzinho, input$m, input$p))
  })
  
  nome <- reactive({
    switch (input$dist, 
            "Normal" = "Distribuição Normal",
            "Uniforme" = "Distribuição Uniforme",
            "Poisson" = "Distribuição Poisson",
            "Exponencial" = "Distribuição Exponencial",
            "Gamma" = "Distribuição Gamma",
            "Binomial" = "Distribuição Binomial")
  })
  
  
  ymean <- reactive({ 
    for (i in 1:input$m) {
      if (i==1) {
        ymean <- c(mean(sample(x(), size = input$N, replace = TRUE ))) 
      } else {
        ymean <- c(ymean,mean(sample(x(), size = input$N, replace = TRUE )))
      }}
    ymean
  })
  
  
  yvar <- reactive({ 
    for (i in 1:input$m) {
      if (i==1) {
        yvar <- c(var(sample(x(), size = input$N, replace = TRUE ))) 
      } else {
        yvar <- c(yvar,var(sample(x(), size = input$N, replace = TRUE )))
      }}
    yvar
  })
  
  
  compar1 <- reactive({
    for (i in 1:input$m) {
      if (i==1) {
        compar1 <- c(mean(sample(x(), size = input$N1, replace = TRUE ))) 
      } else {
        compar1 <- c(compar1,mean(sample(x(), size = input$N1, replace = TRUE )))
      }}
    compar1
    
  })
  
  compar2 <- reactive({
    for (i in 1:input$m) {
      if (i==1) {
        compar2 <- c(mean(sample(x(), size = input$N2, replace = TRUE ))) 
      } else {
        compar2 <- c(compar2,mean(sample(x(), size = input$N2, replace = TRUE )))
      }}
    compar2
    
  })
  
  compar3 <- reactive({
    for (i in 1:input$m) {
      if (i==1) {
        compar3 <- c(mean(sample(x(), size = input$N3, replace = TRUE ))) 
      } else {
        compar3 <- c(compar3,mean(sample(x(), size = input$N3, replace = TRUE )))
      }}
    compar3
    
  })
  
  
  
  output$hist <- renderPlot({
    hist(x() , main=nome(), col='lightblue', xlab='Valor de x', ylab='Densidade', freq = FALSE)
  })
  
  output$histmedia <- renderPlot({
    hist(ymean(), main = "Distribuição da Média Amostral", col='lightpink',  xlab='Valor de x', ylab='Densidade', freq=FALSE)
    curve(dnorm(x,mean=mean(ymean()),sd=sd(ymean())), add=TRUE,col="red", lwd=3)
  })
  
  output$hist1 <- renderPlot({
    hist(x() , main=nome(), col='lightblue', xlab='Valor de x', ylab='Densidade', freq = FALSE)
  })
  
  output$histvar <- renderPlot({
    hist(yvar(), main = "Distribuição da Variância Amostral", col='lightpink',  xlab='Valor de x', ylab='Densidade', freq=FALSE)
    curve(dnorm(x,mean=mean(yvar()),sd=sd(yvar())), add=TRUE,col="red", lwd=3)
  })
  
  output$histcompar <- renderPlot({
    hist(x() , main=nome(), col='lightblue', xlab='Valor de x', ylab='Densidade', freq = FALSE)
  })
  
  output$histcompar1 <- renderPlot({
    hist(compar1(), main = "Distribuição da Média Amostral 1", col='lightpink',  xlab='Valor de x', ylab='Densidade', freq=FALSE)
    curve(dnorm(x,mean=mean(compar1()),sd=sd(compar1())), add=TRUE,col="red", lwd=3)  
  })
  
  output$histcompar2 <- renderPlot({   
    hist(compar2(), main = "Distribuição da Média Amostral 2", col='lightpink',  xlab='Valor de x', ylab='Densidade', freq=FALSE)
    curve(dnorm(x,mean=mean(compar2()),sd=sd(compar2())), add=TRUE,col="red", lwd=3)   
  })
  
  output$histcompar3 <- renderPlot({ 
    hist(compar3(), main = "Distribuição da Média Amostral 3", col='lightpink',  xlab='Valor de x', ylab='Densidade', freq=FALSE)
    curve(dnorm(x,mean=mean(compar3()),sd=sd(compar3())), add=TRUE,col="red", lwd=3)
  })
    

  
}


shinyApp(ui = ui, server = server)
