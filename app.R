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

default_params = read_json("params_manual_salience.json")
#default_params = read_json("params_manual_replacement.json")
#default_params = read_json("params_manual_patient_only.json")
#default_params = read_json("params_manual_np_bias.json")
#default_params = read_json("params.json")

#default_params = read_json("optimal_params.json")
#default_params$replacement_prior = 0.5


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
                card_header("Action priors:"),
                sliderInput("jump_over_prior", "Jump over:", min = 0, max = 1.0, ticks = FALSE, value = default_params$jump_over_prior),
                sliderInput("wave_prior", "Wave:", min = 0, max = 1.0, ticks = FALSE, value = default_params$wave_prior),
                sliderInput("attack_prior", "Attack:", min = 0, max = 1.0, ticks = FALSE, value = default_params$attack_prior),
                sliderInput("throw_rock_prior", "Throw a rock:", min = 0, max = 1.0, ticks = FALSE, value = default_params$throw_rock_prior),
                #sliderInput("overall_prior", "Overall:", min = 0, max = 1.0, ticks = FALSE, value = 1 - default_params$salience_prior),
                #actionButton("adjust_action_priors", "Re-adjust")
            ),
            card(
                card_header("Speaker rationality:"),
                sliderInput("alpha", "Alpha:", min = 0, max = 10, step = 0.1, ticks = FALSE, value = default_params$alpha)
            ),
            col_widths = c(10, 10)),
            layout_columns(
            #card(
            #    card_header("Antecedent position:"),
            #     selectizeInput('preset', label = NULL, multiple = FALSE, choices = c("Best regular priors" = "normal", "Best flat patient" = "flat"), select = "normal"),
            #     checkboxInput("flat_patient", "Flat prior for patient:", FALSE),
            #     sliderInput("replacement_prior", NULL, min = 0, max = 1.0, ticks = FALSE, value = default_params$replacement_prior),
                #  selectizeInput(
                #      'preset',
                #      label = NULL,
                #      multiple = FALSE,
                #      choices = c(
                #       "Salience prior" = "salience",
                #       "Prior for patient only" = "patient",
                #       "Symmetric NP bias" = "symmetric",
                #       "Asymmetric NP bias" = "asymmetric"
                #     ),
                #     select = "patient"
                # ),
                # sliderInput("salience_prior", "Salience prior:", min = 0, max = 1.0, ticks = FALSE, value = default_params$salience_prior),
                # sliderInput("agent_bias", "Prior for patient only:", min = 0, max = 1.0, ticks = FALSE, value = default_params$agent_bias),
                # sliderInput("np_bias", "Agent NP bias:", min = 0, max = 1.0, ticks = FALSE, value = default_params$np_bias),
                # sliderInput("np_bias2", "Patient NP bias:", min = 0, max = 1.0, ticks = FALSE, value = default_params$np_bias2)
            #),
            card(
                card_header("Monster strength:"),
                sliderInput("train_prior", "Trained prior:", min = 0, max = 1.0, ticks = FALSE, value = default_params$train_prior),
                sliderInput("revision_prior", "Revised prior:", min = 0, max = 1.0, ticks = FALSE, value = default_params$revision_prior)
            ),
            card(
                card_header("Utterance cost:"),
                sliderInput("np_cost", "Noun phrase cost:", min = 0, max = 1, step = 0.01, ticks = FALSE, value = default_params$np_cost),
                sliderInput("pro_cost", "Pronoun cost:", min = 0, max = 1, step = 0.01, ticks = FALSE, value = default_params$pro_cost)
            ),
            col_widths = c(10, 10, 10)
            )),
            layout_columns(
            layout_columns(
                tooltip(
                    card(
                        card_header("Speaker assumed noise:"),
                        sliderInput("error_zero", "Zeros heard as pronoun:", min = 0, max = 1, ticks = FALSE, value = 1 - default_params$certain_zero),
                        sliderInput("error_it", "Pronouns heard as zero:", min = 0, max = 1, ticks = FALSE, value = 1 - default_params$certain_it)
                    ),
                    "Test message",
                    id = "error_tooltip"
                ),
                tooltip(
                    card(
                        card_header("No noise condition:"),
                        sliderInput("error_clean", "Zeros heard as pronoun:", min = 0, max = 1, ticks = FALSE, value = 1 - default_params$certain_clean),
                        sliderInput("error_clean_it", "Pronouns heard as zero:", min = 0, max = 1, ticks = FALSE, value = 1 - default_params$certain_clean_it)
                    ),
                    "Test message",
                    id = "error_clean_tooltip"
                ),
            col_widths = c(10, 10)
            ),
            layout_columns(
                tooltip(
                    card(
                        card_header("Low noise condition:"),
                        sliderInput("error_low", "Zeros heard as pronoun:", min = 0, max = 1, ticks = FALSE, value = 1 - default_params$certain_low),
                        sliderInput("error_low_it", "Pronouns heard as zero:", min = 0, max = 1, ticks = FALSE, value = 1 - default_params$certain_low_it)
                    ),
                    "Test message",
                    id = "error_low_tooltip"
                ),
                tooltip(
                    card(
                        card_header("High noise condition:"),
                        sliderInput("error_high", "Zeros heard as pronoun:", min = 0, max = 1, ticks = FALSE, value = 1 - default_params$certain_high),
                        sliderInput("error_high_it", "Pronouns heard as zero:", min = 0, max = 1, ticks = FALSE, value = 1 - default_params$certain_high_it)
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

recalculatePrior <- function(action_prior, overall_prior)
{
    pos_p <- action_prior * overall_prior
    neg_p <- (1 - action_prior) * (1 - overall_prior)
    
    return (pos_p / (pos_p + neg_p))
}

loadParams <- function(file, session)
{
    default_params = read_json(file)

    updateSliderInput(session, "alpha", value = default_params$alpha)
    updateSliderInput(session, "jump_over_prior", value = default_params$jump_over_prior)
    updateSliderInput(session, "wave_prior", value = default_params$wave_prior)
    updateSliderInput(session, "attack_prior", value = default_params$attack_prior)
    updateSliderInput(session, "throw_rock_prior", value = default_params$throw_rock_prior)
    #updateSliderInput(session, "overall_prior", value = 1 - default_params$salience_prior)
    #updateCheckboxInput(session, "flat_patient", value = default_params$flat_patient)
    #updateSliderInput(session, "replacement_prior", value = default_params$replacement_prior)
    #updateSliderInput(session, "agent_bias", value = default_params$agent_bias)
    #updateSliderInput(session, "np_bias", value = default_params$np_bias)
    #updateSliderInput(session, "np_bias2", value = default_params$np_bias2)
    updateSliderInput(session, "train_prior", value = default_params$train_prior)
    updateSliderInput(session, "revision_prior", value = default_params$revision_prior)
    updateSliderInput(session, "np_cost", value = default_params$np_cost)
    updateSliderInput(session, "pro_cost", value = default_params$pro_cost)
    updateSliderInput(session, "error_zero", value = 1 - default_params$certain_zero)
    updateSliderInput(session, "error_it", value = 1 - default_params$certain_it)
    updateSliderInput(session, "error_low", value = 1 - default_params$certain_low)
    updateSliderInput(session, "error_low_it", value = 1 - default_params$certain_low_it)
    updateSliderInput(session, "error_high", value = 1 - default_params$certain_high)
    updateSliderInput(session, "error_high_it", value = 1 - default_params$certain_high_it)
    updateSliderInput(session, "error_clean", value = 1 - default_params$certain_clean)
    updateSliderInput(session, "error_clean_it", value = 1 - default_params$certain_clean_it)
    #updateSelectInput(session, "preset", select = "patient")
}

# Define server logic required to draw a histogram
server <- function(input, output, session)
{
    
    output$model_title = renderText({"Model predictions"})
    output$data_title = renderText({"Observed data"})
    
    observeEvent(input$preset,
    {
        if (input$preset == "normal")
            loadParams("params_manual_salience.json", session)
        else
            loadParams("params_manual_replacement.json", session)
    })
    
    observeEvent(input$reset,
    {
        #if (input$preset == "normal")
            loadParams("params_manual_salience.json", session)
        #else
        #    loadParams("params_manual_replacement.json", session)
    })
    
    # observeEvent(input$preset,
    # {
    #     if (input$preset == "salience")
    #     {
    #         updateSliderInput(session, "salience_prior", value = default_params$agent_bias)
    #         updateSliderInput(session, "agent_bias", value = 0.5)
    #         updateSliderInput(session, "np_bias", value = 0.5)
    #         updateSliderInput(session, "np_bias2", value = 0.5)
    #     }
    #     else if (input$preset == "patient")
    #     {
    #         updateSliderInput(session, "salience_prior", value = 0.5)
    #         updateSliderInput(session, "agent_bias", value = default_params$agent_bias)
    #         updateSliderInput(session, "np_bias", value = 0.5)
    #         updateSliderInput(session, "np_bias2", value = 0.5)
    #     }
    #     else if (input$preset == "symmetric")
    #     {
    #         value = 0.63
    #         updateSliderInput(session, "salience_prior", value = 0.5)
    #         updateSliderInput(session, "agent_bias", value = 0.5)
    #         updateSliderInput(session, "np_bias", value = 1 - value)
    #         updateSliderInput(session, "np_bias2", value = value)
    #     }
    #     else if (input$preset == "asymmetric")
    #     {
    #         updateSliderInput(session, "salience_prior", value = 0.5)
    #         updateSliderInput(session, "agent_bias", value = 0.5)
    #         updateSliderInput(session, "np_bias", value = 0.5)
    #         updateSliderInput(session, "np_bias2", value = 0.66)
    #     }
    # })
    
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
    
    observeEvent(input$adjust_action_priors,
    {
        updateSliderInput(session, "jump_over_prior", value = recalculatePrior(input$jump_over_prior, input$overall_prior))
        updateSliderInput(session, "wave_prior", value = recalculatePrior(input$wave_prior, input$overall_prior))
        updateSliderInput(session, "attack_prior", value = recalculatePrior(input$attack_prior, input$overall_prior))
        updateSliderInput(session, "throw_rock_prior", value = recalculatePrior(input$throw_rock_prior, input$overall_prior))
        updateSliderInput(session, "replacement_prior", value = recalculatePrior(input$replacement_prior, input$overall_prior))
        updateSliderInput(session, "overall_prior", value = 0.5)
        
    })
    
    params <- reactive({list(
        event_type = "neutral",
        states = states,
        utterances = utterances,
        meaning = meaning,
        alpha = input$alpha,
        salience_prior = 0.5,#1 - input$overall_prior,
        flat_patient = FALSE,#input$flat_patient,
        replacement_prior = 0.5,#input$replacement_prior,
        np_bias = 0.5,
        np_bias2 = 0.5,
        agent_bias = 0.5,
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
