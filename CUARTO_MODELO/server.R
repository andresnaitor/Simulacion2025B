library(shiny)
#Método multiplicativo

mm<- function(x0, a, m){
  return(((a*x0)%%m))
  
}

mm_sim<- function(x0, a, m, nsim){
  vec<- numeric(nsim+1)
  vec[1]<- x0
  for(k in 1:nsim){
    vec[k+1]<- mm(vec[k], a, m)
  }
  vec<- (vec[-1])/m
  return(vec)
}

#Método Mixto
mi<- function(x0, a, m, c){
  return(((a*x0+c)%%m))
  
}
mi_sim<- function(x0, a, m, c, nsim){
  vec<- numeric(nsim+1)
  vec[1]<- x0
  for(k in 1:nsim){
    vec[k+1]<- mi(vec[k], a, m, c)
  }
  vec<- (vec[-1])/m
  return(vec)
}

#Método de los cuadrados medios
mcm<- function(x0, k){
  return(floor((x0^2-floor((x0^2)/(10^(2*k-k/2)))*10^(2*k-k/2))/(10^(k/2))))
  
}

mcm_sim<- function(x0,k,nsim){
  vec<- numeric(nsim+1)
  vec[1]<- x0
  for(j in 1:nsim){
    vec[j+1]<- mcm(vec[j],k)
  }
  vec<- vec[-1]/(10^k)
  return(vec)
}


#Método de lehmer
ml<- function(x0, n, c){
  return((x0*c-floor((x0*c)/(10^n))*10^n)-floor((x0*c)/(10^n)))
}

ml_sim<- function(x0, n, c, nsim){
  vec<- numeric(nsim+1)
  vec[1]<- x0
  for(j in 1:nsim){
    vec[j+1]<- ml(vec[j],n,c)
  }
  vec<- vec[-1]/(10^n)
  return(vec)
}


#Método de la transformación inversa
t_inversa <- function(probs, vals, nval){
  sprobs <- cumsum(probs)
  res <- numeric(nval)
  for (j in 1:nval){
    u <- runif(1)
    res[j] <- vals[which(u <= sprobs)[1]]
  }
  return(res)
}


# Generación de  variables aleatorias geométricas
x_geometrica <- function(p, nval){
  res <- numeric(nval)
  for(j in 1:nval){
    res[j] <- floor(log(runif(1)) / log(1 - p)) + 1
  }
  return(res)
}


# Generación de variables aleatorias Poisson

x_poisson <- function(lambda,nval){
  res<- numeric(nval)
  for(j in 1:nval){
    i  <- X <- 0
    Fx <- p <- exp(-lambda)
    U  <- runif(1)
    while(U>Fx){
      X<-i
      p<-(lambda*p)/(i+1)
      Fx<-Fx+p
      i<-i+1
    }
    res[j]<-i
  }
  return(res)
}


# Variables aleatorias binomiales

x_binomial <- function(n,p,nval){
  
  res<-numeric(nval)
  for(j in 1:nval){
    c <- p/(1-p)
    i <- 0
    Fx <- pr <- (1-p)^n
    U <- runif(1)
    while(U>Fx){
      pr <- c*((n-i)/(i+1))*pr
      Fx <- Fx + pr
      i <- i+1
    }
    res[j]<-i
  }
  return(res)
}




# Define server logic required to draw a histogram
function(input, output, session) {
  
  
  fx <- reactive({
    expresion <- input$func
    # Construye una función a partir del texto
    function(x){
      eval(parse(text=expresion))
    }
  })
  
  aleatorios<- eventReactive(input$simular,{
    if(input$metodo== "Congruencial Multiplicativo"){
      res<- mm_sim(x0= input$x0, a =input$a, m=input$m, nsim=input$num)
    } else if(input$metodo== "Congruencial Mixto"){
      res<- mi_sim(x0= input$x0, a =input$a, c= input$c, m=input$m, nsim=input$num)
    } else if(input$metodo== "Cuadrados Medios"){
      res<- mcm_sim(x0= input$x0, k= nchar(input$x0), nsim=input$num)
    } else{
      res<- ml_sim(x0= input$x0, n =4, c= 2, nsim=input$num)
    }
    res
  })
  
  output$code <- renderPrint({
    aleatorios()
  })
  
  
  output$grafico <- renderPlot({
    hist(aleatorios(),breaks=10,col="orange",main="Números aleatorios")
  })
  
  output$resumen <- renderPrint({
    summary(aleatorios())
  })
  
  output$graficofun <- renderPlot({
    xvals <- seq(input$linf,input$lsup,lenght.out=100)
    yvals <- fx()(xvals)
    
    plot(xvals,yvals,type="l",col="red",
         main="Función ingresado por el usuario")
  })
  
  output$code01 <- renderPrint({
    mean(aleatorios())
  })
  
  output$code02 <- renderPrint({
    mean(aleatorios())
  })
  
  output$integral <- renderPrint({
    u <- aleatorios()   
    f <- fx()           
    tipo <- input$tipo_integral
    
    if (tipo == "Finito") {
      # Integral en [linf, lsup]
      hy <- (input$lsup - input$linf) * f(input$linf + (input$lsup - input$linf) * u)
      
    } else if (tipo == "[0, ∞)") {
      # Integral en [0, ∞)
      x <- 1 / u
      hy <- f(x) / (u^2)
      
    } else if (tipo == "(-∞, ∞)") {
      # Integral en (-∞, ∞)
      x <- tan(pi * (u - 0.5))
      hy <- f(x) * (pi / cos(pi * (u - 0.5))^2)
      
    } else if (tipo == "[a, ∞)") {
      # Integral en [a, ∞), usando linf como a
      a <- input$linf
      x <- a + u / (1 - u)
      hy <- f(x) / (1 - u)^2
    }
    
    mean(hy)   
  })
  
  
  # Función transformada inversa
  t_inversa <- function(probs, vals, nval){
    sprobs <- cumsum(probs)
    res <- numeric(nval)
    for (j in 1:nval){
      u <- runif(1)
      res[j] <- vals[which(u <= sprobs)[1]]
    }
    return(res)
  }
  
  # Función geométrica
  x_geometrica <- function(p, nval){
    res <- numeric(nval)
    for(j in 1:nval){
      res[j] <- floor(log(runif(1)) / log(1 - p)) + 1
    }
    return(res)
  }
  
  # Simulación transformada inversa
  observeEvent(input$sim_discretas, {
    vals <- as.numeric(unlist(strsplit(input$vals, ",")))
    probs <- as.numeric(unlist(strsplit(input$probs, ",")))
    
    if (abs(sum(probs) - 1) > 1e-6) {
      output$discretas_out <- renderPrint({"Error: las probabilidades deben sumar 1"})
      output$discretas_plot <- renderPlot(NULL)
      return(NULL)
    }
    
    muestras <- t_inversa(probs, vals, input$nval)
    
    output$discretas_out <- renderPrint({muestras})
    output$discretas_plot <- renderPlot({
      barplot(table(muestras), col="skyblue", main="Frecuencias simuladas (Transformada Inversa)")
    })
  })
  
  # Simulación geométrica manual (botón)
  observeEvent(input$sim_geom, {
    muestras <- x_geometrica(p = input$p_geom, nval = input$n_geom)
    
    output$geom_out <- renderPrint({muestras})
    output$geom_plot <- renderPlot({
      barplot(table(muestras), col="lightgreen", main="Frecuencias simuladas (Geométrica)")
    })
  })
  
  # Poisson
  observeEvent(input$sim_poisson, {
    muestras <- x_poisson(lambda = input$lambda, nval = input$n_poisson)
    
    output$poisson_out <- renderPrint({muestras})
    output$poisson_plot <- renderPlot({
      barplot(table(muestras), col="orange", main="Frecuencias simuladas (Poisson)")
    })
  })
  
  # Binomial
  observeEvent(input$sim_binom, {
    muestras <- x_binomial(n = input$n_binom, p = input$p_binom, nval = input$nval_binom)
    
    output$binom_out <- renderPrint({muestras})
    output$binom_plot <- renderPlot({
      barplot(table(muestras), col="purple", main="Frecuencias simuladas (Binomial)")
    })
  })
  
  
  
  
  
  
  
  
}