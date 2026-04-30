#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinyjs)
library(bslib)

source("rsa.R")

listener = listener0
speaker = speaker1

#params = read_json("params.json")
params = read_json("optimal_params.json")


# Define UI for application that draws a histogram
ui <- page_fillable(
    title = "Reference monsters",

    useShinyjs(),

    # Sidebar with a slider input for number of bins 
    layout_columns(
        wellPanel(
        card(
            layout_columns(
             checkboxGroupInput(
                "prod_datasets",
                "Production event types:",
                c(
                  "Familiar" = "familiar",
                  "Surprising" = "surprising",
                  "Post-surprising" = "postsurprising",
                  "Neutral" = "neutral"
                ),
                select = c("familiar", "surprising")
              ),
             layout_columns(
             checkboxGroupInput( 
                "perc_datasets", 
                "Perception noise levels:", 
                c( 
                  "Low noise" = "low_noise", 
                  "High noise" = "high_noise", 
                  "No noise" = "clean"
                )#,
                #select = "low_noise"
              ),
             actionButton("reset", "Reset to defaults"),
             col_widths = c(10, 9)
            )
            )),
        layout_columns(
        layout_columns(
            layout_columns(
            card(
                card_header("Action priors"),
                sliderInput("jump_over_prior", "Jump over:", min = 0, max = 1.0, ticks = FALSE, value = params$jump_over_prior),
                sliderInput("wave_prior", "Wave:", min = 0, max = 1.0, ticks = FALSE, value = params$wave_prior),
                sliderInput("attack_prior", "Attack:", min = 0, max = 1.0, ticks = FALSE, value = params$attack_prior),
                sliderInput("throw_rock_prior", "Throw a rock:", min = 0, max = 1.0, ticks = FALSE, value = params$throw_rock_prior)
            ),
            card(
                card_header("Utterance cost"),
                sliderInput("np_cost", "Noun phrase cost:", min = 0, max = 1, step = 0.01, ticks = FALSE, value = params$np_cost),
                sliderInput("pro_cost", "Pronoun cost:", min = 0, max = 1, step = 0.01, ticks = FALSE, value = params$pro_cost)
            ),
            col_widths = c(10, 10)),
            layout_columns(
            card(
                card_header("Training priors"),
                sliderInput("train_prior", "Familiar vs surprising:", min = 0, max = 1.0, ticks = FALSE, value = params$train_prior),
                sliderInput("revision_prior", "Post-surprising:", min = 0, max = 1.0, ticks = FALSE, value = params$revision_prior)
            ),
            card(
                card_header("Antecedent position"),
                sliderInput("salience_prior", "Salience prior:", min = 0, max = 1.0, ticks = FALSE, value = params$salience_prior),
                sliderInput("agent_bias", "Prior for patient only:", min = 0, max = 1.0, ticks = FALSE, value = params$agent_bias),
                sliderInput("subject_bias", "Subject bias:", min = 0, max = 1.0, ticks = FALSE, value = params$subject_bias)
            ),
            card(
                card_header("Speaker rationality"),
                sliderInput("alpha", "Alpha:", min = 0, max = 10, step = 0.1, ticks = FALSE, value = params$alpha)
            ),
            col_widths = c(10, 10, 10)
            )),
            layout_columns(
            layout_columns(
                tooltip(
                    card(
                        card_header("Speaker assumed noise"),
                        sliderInput("error_zero", "Zeros heard as pronoun:", min = 0, max = 1, ticks = FALSE, value = 1 - params$certain_zero),
                        sliderInput("error_it", "Pronouns heard as zero:", min = 0, max = 1, ticks = FALSE, value = 1 - params$certain_it)
                    ),
                    "Test message",
                    id = "error_tooltip"
                ),
                tooltip(
                    card(
                        card_header("No noise condition"),
                        sliderInput("error_clean", "Zeros heard as pronoun:", min = 0, max = 1, ticks = FALSE, value = 1 - params$certain_clean),
                        sliderInput("error_clean_it", "Pronouns heard as zero:", min = 0, max = 1, ticks = FALSE, value = 1 - params$certain_clean_it)
                    ),
                    "Test message",
                    id = "error_clean_tooltip"
                ),
            col_widths = c(10, 10)
            ),
            layout_columns(
                tooltip(
                    card(
                        card_header("Low noise condition"),
                        sliderInput("error_low", "Zeros heard as pronoun:", min = 0, max = 1, ticks = FALSE, value = 1 - params$certain_low),
                        sliderInput("error_low_it", "Pronouns heard as zero:", min = 0, max = 1, ticks = FALSE, value = 1 - params$certain_low_it)
                    ),
                    "Test message",
                    id = "error_low_tooltip"
                ),
                tooltip(
                    card(
                        card_header("High noise condition"),
                        sliderInput("error_high", "Zeros heard as pronoun:", min = 0, max = 1, ticks = FALSE, value = 1 - params$certain_high),
                        sliderInput("error_high_it", "Pronouns heard as zero:", min = 0, max = 1, ticks = FALSE, value = 1 - params$certain_high_it)
                    ),
                    "Test message",
                    id = "error_high_tooltip"
                ),
            col_widths = c(10, 10)
            )
            ),
            col_widths = c(12, 12)
            ),
            style = "overflow-y:scroll; max-height: 100%"
        ),

        # Show a plot of the generated distribution
        card(
            layout_columns(
                wellPanel(
                    span(textOutput("model_title"), style= "text-align: center; font-size: large"),
                    plotOutput("plot_model_familiar"),
                    plotOutput("plot_model_surprising"),
                    plotOutput("plot_model_postsurprising"),
                    plotOutput("plot_model_neutral"),
                    plotOutput("plot_model_low_noise"),
                    plotOutput("plot_model_high_noise"),
                    plotOutput("plot_model_clean"),
                    style = "background-color: white"
               ),
               wellPanel(
                    span(textOutput("data_title"), style= "text-align: center; font-size: large"),
                    plotOutput("plot_data_familiar"),
                    plotOutput("plot_data_surprising"),
                    plotOutput("plot_data_postsurprising"),
                    plotOutput("plot_data_neutral"),
                    plotOutput("plot_data_low_noise"),
                    plotOutput("plot_data_high_noise"),
                    plotOutput("plot_data_clean"),
                    style = "background-color: white"
                )
            )
        ),
        col_widths = c(4, 8)
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    output$model_title = renderText({"Model predictions"})
    output$data_title = renderText({"Observed data"})
    
    observeEvent(input$reset,
    {
        updateSliderInput(session, "alpha", value = alpha)
        updateSliderInput(session, "jump_over_prior", value = action_priors$jump_over)
        updateSliderInput(session, "wave_prior", value = action_priors$wave)
        updateSliderInput(session, "attack_prior", value = action_priors$attack)
        updateSliderInput(session, "throw_rock_prior", value = action_priors$throw_rock)
        updateSliderInput(session, "salience_prior", value = salience_prior)
        updateSliderInput(session, "agent_bias", value = agent_bias)
        updateSliderInput(session, "subject_bias", value = subject_bias)
        updateSliderInput(session, "train_prior", value = train_prior)
        updateSliderInput(session, "revision_prior", value = revision_prior)
        updateSliderInput(session, "np_cost", value = np_cost)
        updateSliderInput(session, "pro_cost", value = pro_cost)
        updateSliderInput(session, "error_zero", value = error_zero)
        updateSliderInput(session, "error_it", value = error_it)
        updateSliderInput(session, "error_low", value = error_low_zero)
        updateSliderInput(session, "error_low_it", value = error_low_it)
        updateSliderInput(session, "error_high", value = error_high_zero)
        updateSliderInput(session, "error_high_it", value = error_high_it)
        updateSliderInput(session, "error_clean", value = error_clean_zero)
        updateSliderInput(session, "error_clean_it", value = error_clean_it)
    })
    
    prod_legend = reactive({input$prod_datasets[1] %||% "none"})
    perc_legend = reactive({input$perc_datasets[1] %||% "none"})

    observeEvent(input$prod_datasets,
    {
        if ("familiar" %in% input$prod_datasets){
            shinyjs::show("plot_model_familiar")
            shinyjs::show("plot_data_familiar")
        }
        else
        {
            shinyjs::hide("plot_model_familiar")
            shinyjs::hide("plot_data_familiar")
        }

        if ("surprising" %in% input$prod_datasets){
            shinyjs::show("plot_model_surprising")
            shinyjs::show("plot_data_surprising")
        }
        else
        {
            shinyjs::hide("plot_model_surprising")
            shinyjs::hide("plot_data_surprising")
        }

        if ("postsurprising" %in% input$prod_datasets){
            shinyjs::show("plot_model_postsurprising")
            shinyjs::show("plot_data_postsurprising")
        }
        else
        {
            shinyjs::hide("plot_model_postsurprising")
            shinyjs::hide("plot_data_postsurprising")
        }

        if ("neutral" %in% input$prod_datasets){
            shinyjs::show("plot_model_neutral")
            shinyjs::show("plot_data_neutral")
        }
        else
        {
            shinyjs::hide("plot_model_neutral")
            shinyjs::hide("plot_data_neutral")
        }
    }, ignoreNULL=FALSE)
    
    observeEvent(input$perc_datasets,
    {
        if ("low_noise" %in% input$perc_datasets){
            shinyjs::show("plot_model_low_noise")
            shinyjs::show("plot_data_low_noise")
        }
        else
        {
            shinyjs::hide("plot_model_low_noise")
            shinyjs::hide("plot_data_low_noise")
        }

        if ("high_noise" %in% input$perc_datasets){
            shinyjs::show("plot_model_high_noise")
            shinyjs::show("plot_data_high_noise")
        }
        else
        {
            shinyjs::hide("plot_model_high_noise")
            shinyjs::hide("plot_data_high_noise")
        }

        if ("clean" %in% input$perc_datasets){
            shinyjs::show("plot_model_clean")
            shinyjs::show("plot_data_clean")
        }
        else
        {
            shinyjs::hide("plot_model_clean")
            shinyjs::hide("plot_data_clean")
        }
    }, ignoreNULL=FALSE)
    
    params <- reactive({list(
        event_type = "neutral",
        states = states,
        utterances = utterances,
        meaning = meaning,
        alpha = input$alpha,
        salience_prior = input$salience_prior,
        subject_bias = input$subject_bias,
        agent_bias = input$agent_bias,
        train_prior = input$train_prior,
        revision_prior = input$revision_prior,
        certain_zero = 1 - input$error_zero,
        certain_it = 1 - input$error_it,
        np_cost = input$np_cost,
        pro_cost = input$pro_cost,
        zero_cost = 0,
        patient_color = 'yellow'
    )})
    
    local_action_priors = reactive({list(
        jump_over = input$jump_over_prior,
        wave =  input$wave_prior,
        attack =  input$attack_prior,
        throw_rock = input$throw_rock_prior
    )})
    
    observe({
        local_params <- params()
        local_params$event_type <- "familiar"
        
        error_zero = round(1 - doubleConfusionProb("zero", "zero", local_params), 2)
        error_it = round(1 - doubleConfusionProb("pro", "pro", local_params), 2)

        update_tooltip("error_tooltip", paste("Intended zeros interpreted as pronoun:", error_zero, " Intended pronouns interpreted as zero:", error_it))

        local_params = params()
        local_params$certain_zero = 1 - input$error_low
        local_params$certain_it = 1 - input$error_low_it

        error_zero = round(1 - reverseConfusionProb("zero", "zero", local_params), 2)
        error_it = round(1 - reverseConfusionProb("pro", "pro", local_params), 2)

        update_tooltip("error_low_tooltip", paste("Heard zeros interpreted as pronoun:", error_zero, " Heard pronouns interpreted as zero:", error_it))

        local_params = params()
        local_params$certain_zero = 1 - input$error_high
        local_params$certain_it = 1 - input$error_high_it

        error_zero = round(1 - reverseConfusionProb("zero", "zero", local_params), 2)
        error_it = round(1 - reverseConfusionProb("pro", "pro", local_params), 2)

        update_tooltip("error_high_tooltip", paste("Heard zeros interpreted as pronoun:", error_zero, " Heard pronouns interpreted as zero:", error_it))

        local_params = params()
        local_params$certain_zero = 1 - input$error_clean
        local_params$certain_it = 1 - input$error_clean_it

        error_zero = round(1 - reverseConfusionProb("zero", "zero", local_params), 2)
        error_it = round(1 - reverseConfusionProb("pro", "pro", local_params), 2)

        update_tooltip("error_clean_tooltip", paste("Heard zeros interpreted as pronoun:", error_zero, " Heard pronouns interpreted as zero:", error_it))
    })

    output$plot_model_familiar <- renderPlot({
        if ('familiar' %in% input$prod_datasets) {
            local_params <- params()
            local_params$event_type <- "familiar"
            speaker_dist = speakerDist(speaker, local_action_priors(), local_params)
            drawSpeakerDist(speaker_dist, show_legend = prod_legend() == "familiar", y_label = "Familiar events")
        }
    })
    
    output$plot_data_familiar <- renderPlot({
        if ('familiar' %in% input$prod_datasets) {
            drawSpeakerGold("prod_data_training_familiar.csv", show_legend = prod_legend() == "familiar", y_label = "Familiar events")
        }
    })
    
    output$plot_model_surprising <- renderPlot({
        if ('surprising' %in% input$prod_datasets) {
            local_params <- params()
            local_params$event_type <- "surprising"
            speaker_dist = speakerDist(speaker, local_action_priors(), local_params)
            drawSpeakerDist(speaker_dist, show_legend = prod_legend() == "surprising", y_label = "Surprising events")
        }
    })
    
    output$plot_data_surprising <- renderPlot({
        if ('surprising' %in% input$prod_datasets) {
            drawSpeakerGold("prod_data_training_surprising.csv", show_legend = prod_legend() == "surprising", y_label = "Surprising events")
        }
    })

    output$plot_model_postsurprising <- renderPlot({
        if ('postsurprising' %in% input$prod_datasets) {
            local_params <- params()
            local_params$event_type <- "postsurprising"
            speaker_dist = speakerDist(speaker, local_action_priors(), local_params)
            drawSpeakerDist(speaker_dist, show_legend = prod_legend() == "postsurprising", y_label = "Post-surprising events")
        }
    })
    
    output$plot_data_postsurprising <- renderPlot({
        if ('postsurprising' %in% input$prod_datasets) {
            drawSpeakerGold("prod_data_training_postsurprising.csv", show_legend = prod_legend() == "postsurprising", y_label = "Post-surprising events")
        }
    })

    output$plot_model_neutral <- renderPlot({
        if ('neutral' %in% input$prod_datasets) {
            local_params <- params()
            local_params$event_type <- "neutral"
            speaker_dist = speakerDist(speaker, local_action_priors(), local_params)
            drawSpeakerDist(speaker_dist, show_legend = prod_legend() == "neutral", y_label = "Neutral events")
        }
    })
    
    output$plot_data_neutral <- renderPlot({
        if ('neutral' %in% input$prod_datasets) {
            drawSpeakerGold("prod_data_no_training_rest.csv", show_legend = prod_legend() == "neutral", y_label = "Neutral events")
        }
    })
    
    output$plot_model_low_noise <- renderPlot({
        local_params = params()
        local_params$certain_zero = 1 - input$error_low
        local_params$certain_it = 1 - input$error_low_it
        
        if ('low_noise' %in% input$perc_datasets) {
            listener_dist = listenerDist(listener, local_action_priors(), local_params)
            drawListenerDist(listener_dist, show_legend = perc_legend() == "low_noise", y_label = "Low noise")
        }
    })
    
    output$plot_data_low_noise <- renderPlot({
        if ('low_noise' %in% input$perc_datasets) {
            drawListenerGold("perc_data_low_noise.csv", show_legend = perc_legend() == "low_noise", y_label = "Low noise")
        }
    })

    output$plot_model_high_noise <- renderPlot({
        if ('high_noise' %in% input$perc_datasets) {
            local_params = params()
            local_params$certain_zero = 1 - input$error_high
            local_params$certain_it = 1 - input$error_high_it
            listener_dist = listenerDist(listener, local_action_priors(), local_params)
            drawListenerDist(listener_dist, show_legend = perc_legend() == "high_noise", y_label = "High noise")
        }
    })
    
    output$plot_data_high_noise <- renderPlot({
        if ('high_noise' %in% input$perc_datasets) {
            drawListenerGold("perc_data_high_noise.csv", show_legend = perc_legend() == "high_noise", y_label = "High noise")
        }
    })
    
    output$plot_model_clean <- renderPlot({
        if ('clean' %in% input$perc_datasets) {
            local_params = params()
            local_params$certain_zero = 1 - input$error_clean
            local_params$certain_it = 1 - input$error_clean_it
            listener_dist = listenerDist(listener, local_action_priors(), local_params)
            drawListenerDist(listener_dist, show_legend = perc_legend() == "clean", y_label = "No noise")
        }
    })
    
    output$plot_data_clean <- renderPlot({
        if ('clean' %in% input$perc_datasets) {
            drawListenerGold("perc_data_clean.csv", show_legend = perc_legend() == "clean", y_label = "No noise")
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
