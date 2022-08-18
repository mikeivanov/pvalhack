library(shiny)
library(tibble)
library(stringr)
library(ggplot2)

ui <- fluidPage(
    # Application title
    titlePanel("P-value Hacking"),

    sidebarLayout(
        sidebarPanel(
            sliderInput("mu",
                        "True mean:",
                        min = 0,
                        max = 0.5,
                        step = 0.01,
                        value = 0.1),
            sliderInput("n",
                        "Sample size:",
                        min = 10,
                        max = 200,
                        step = 1,
                        value = 20),
            selectInput("pval",
                        "Desired p-value",
                        choices = c(0.001, 0.01, 0.05, 0.1),
                        selected = 0.05),
            HTML("<hr>
                  <p>See also</p>
                  <ul>
                    <li><a href='https://github.com/mikeivanov/pvalhack/'>Source code</a></li>
                    <li><a href='https://mikeivanov.com/posts/2022-08-17-pvalhack/'>A brief explanation</a></li>
                    <li><a href='https://arxiv.org/abs/1603.07532'>The original paper</a></li>
                  </ul>")
        ),
        # Show a plot of the generated distribution
        mainPanel(
            h3("Probability distribution of a one-tailed p-value generated
                by Monte Carlo"),
            plotOutput("distPlot", height = 300),
            h4(textOutput("dist")),
            fluidRow(
              column(5, tableOutput("stats")),
              column(7, h4("Summary"),
                        textOutput("message")))
        )
    )
)

m <- 10^4

server <- function(input, output) {
    samples <- reactive({
        n <- input$n
        replicate(m, {
            ran <- rnorm(n, input$mu, 1)
            z <- mean(ran) / (sd(ran) / sqrt(n))
            1 - pt(z, n)
        })
    })

    observed <- reactive({
        n <- input$n
        replicate(m, {
            ran <- rnorm(n, input$mu, 1)
            z <- mean(ran) / (sd(ran) / sqrt(n))
            1 - pt(z, n)
        })
    })

    stats <- reactive({
        p <- observed()
        vol <- sum(p < as.numeric(input$pval)) / length(p)
        list(mean = mean(p),
             median = median(p),
             volume = vol,
             reps = round(1 / vol))
    })

    output$distPlot <- renderPlot({
        p <- observed()
        s <- stats()
        pval <- as.numeric(input$pval)

        ggplot(NULL, aes(x = p)) +
            geom_density(fill = "lightblue") +
            theme_bw() +
            annotate(geom = "text",
                     label = c(sprintf(" %0.3f", pval), " mean", " median"),
                     color = c("red", "black", "black"),
                     x = c(pval, s$mean, s$median),
                     y = 0,
                     angle = 90,
                     vjust = 1.3, hjust = "left") +
            geom_vline(color = c("red", "black", "black"),
                       linetype = c("solid", "solid", "dashed"),
                       xintercept = c(pval, s$mean, s$median)) +
            labs(x = "P-value",
                 y = "Probability Density")
    })

    output$stats <- renderTable({
        s <- stats()
        pval <- input$pval
        t <- tribble(
            ~Statistic, ~Value,
            "True p-value (mean)", sprintf("%0.3f", s$mean),
            "Median p-value", sprintf("%0.3f", s$median),
            str_interp("P(p-value < ${pval})"), sprintf("%0.3f", s$volume),
            "Exected # of tries", sprintf("%d", s$reps)
        )
        t
    })

    output$dist <- renderText({
        str_interp("X ~ N(m=${input$mu}, 1), H0: m=0, H1: m>0")
    })

    output$message <- renderText({
        s <- stats()
        str_interp("A spurious p-value <= ${input$pval}
                    is expected to be observed at least once after
                    ${s$rep} repetition(s) of the experiment.")
    })
}

# Run the application
shinyApp(ui = ui, server = server)
