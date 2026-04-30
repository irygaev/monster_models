library(tidyverse)
library(ggplot2)
library(jsonlite)

states = c('agent_falls', 'patient_falls')
utterances = c('the_agent', 'the_patient', 'pro', 'zero')
meaning = list(the_agent = c('agent_falls'), the_patient = c('patient_falls'), pro = c('agent_falls', 'patient_falls'), zero =  c('agent_falls'))

actions <- c('jump_over', 'wave', 'attack', 'throw_rock')

alpha = 0
alpha_prod = alpha

jump_over_prior = 0.32
wave_prior = 0.38
attack_prior = 0.48
throw_rock_prior = 0.68
#overall_prior = 0.41

salience_prior = 0.5
agent_bias = 0.5

train_prior = 0.5
revision_prior = 0.5

certain_zero = 0.6
certain_it = 0.9
certain_surp = certain_zero
certain_surp_it = certain_it

certain_clean = 0.95
certain_clean_it = 0.83
certain_low = 0.92
certain_low_it = 0.62
certain_high = 0.88
certain_high_it = 0.58

np_cost = 0.5
pro_cost = 0
zero_cost = 0

alpha = 0.3454907013046489
alpha_prod = alpha
salience_prior = 0.5
train_prior = 0.7750847338962296
certain_zero = 0.9
certain_it = 0.6
np_cost = 0.11091083174636054
pro_cost = 3.7015134051278626


jump_over_prior = 0.0030764878857947233
wave_prior = 0.009152993692588953
attack_prior = 0.10768234489906259
throw_rock_prior = 0.29197699338965194


action_priors = list(jump_over = jump_over_prior, wave = wave_prior, attack = attack_prior, throw_rock = throw_rock_prior)

params_familiar <- list(
    states = states,
    utterances = utterances,
    meaning = meaning,
    alpha = alpha_prod,
    action_prior = 0.5,
    salience_prior = salience_prior,
    agent_bias = agent_bias,
    train_prior = train_prior,
    revision_prior = revision_prior,
    event_type = "familiar",
    certain_zero = certain_zero,
    certain_it = certain_it,
    np_cost = np_cost,
    pro_cost = pro_cost,
    zero_cost = zero_cost,
    patient_color = 'yellow'
)

params_surprising <- list(
    states = states,
    utterances = utterances,
    meaning = meaning,
    alpha = alpha_prod,
    action_prior = 0.5,
    salience_prior = salience_prior,
    agent_bias = agent_bias,
    train_prior = train_prior,
    revision_prior = revision_prior,
    event_type = "surprising",
    certain_zero = certain_surp,
    certain_it = certain_surp_it,
    np_cost = np_cost,
    pro_cost = pro_cost,
    zero_cost = zero_cost,
    patient_color = 'yellow'
)

params_low_noise <- list(
    states = states,
    utterances = utterances,
    meaning = meaning,
    alpha = alpha,
    action_prior = 0.5,
    salience_prior = salience_prior,
    agent_bias = agent_bias,
    train_prior = 0.5,
    revision_prior = revision_prior,
    event_type = "neutral",
    certain_zero = certain_low,
    certain_it = certain_low_it,
    np_cost = np_cost,
    pro_cost = pro_cost,
    zero_cost = zero_cost,
    patient_color = 'yellow'
)

params_high_noise <- list(
    states = states,
    utterances = utterances,
    meaning = meaning,
    alpha = alpha,
    action_prior = 0.5,
    salience_prior = salience_prior,
    agent_bias = agent_bias,
    train_prior = 0.5,
    revision_prior = revision_prior,
    event_type = "neutral",
    certain_zero = certain_high,
    certain_it = certain_high_it,
    np_cost = np_cost,
    pro_cost = pro_cost,
    zero_cost = zero_cost,
    patient_color = 'yellow'
)

params <- list(
    states = states,
    utterances = utterances,
    meaning = meaning,
    alpha = 1,
    action_prior = 0.5,
    salience_prior = 0.5,
    agent_bias = agent_bias,
    subject_bias = 0.5,
    train_prior = 0.6,
    revision_prior = 0.4,
    event_type = "neutral",
    certain_zero = 0.95,
    certain_it = 0.6,
    np_cost = 0.2,
    pro_cost = 0.4,
    zero_cost = 0,
    patient_color = 'yellow'
)

isTrue <- function(utterance, state, params) {
    if (state %in% params$meaning[[utterance]]) return(1)
    else return(0)
}

statePrior <- function(state, params) {
    p1 <- params$action_prior
    p2 <- 1 - params$salience_prior
    
    if (params$event_type == 'postsurprising'){
        p3 <- ifelse(params$patient_color == 'blue', params$revision_prior, ifelse(params$patient_color == 'red', 1 - params$revision_prior, 0.5))
    }
    else{
        p3 <- ifelse(params$patient_color == 'blue', params$train_prior, ifelse(params$patient_color == 'red', 1 - params$train_prior, 0.5))
    }

    p <- p1 * p2 * p3 / (p1 * p2 * p3 + (1 - p1) * (1 - p2) * (1 - p3)) # patient falling probability

    if (state == 'patient_falls') return(p)
    else return(1 - p)
}

confusionProb <- function(i_utt, p_utt, params){
    certain_zero <- params$certain_zero #/ (params$certain_zero + 1 - params$certain_it)
    certain_it <- params$certain_it #/ (params$certain_it + 1 - params$certain_zero)
    
    if (i_utt == 'pro') {
        return(ifelse(p_utt == 'pro', certain_it, ifelse(p_utt == 'zero', 1 - certain_it, 0)))
    }
    else if (i_utt == 'zero') {
        return(ifelse(p_utt == 'zero', certain_zero, ifelse(p_utt == 'pro', 1 - certain_zero, 0)))
    }
    else if (i_utt == p_utt) {
        return(1)
    }
    else {
        return(0)
    }
}

reverseConfusionProb <- function(p_utt, i_utt, params) {
    dist = list()
    
    for (utterance in params$utterances) {
        prior = utterancePrior(utterance, "agent_falls", params)
        likelihood = confusionProb(utterance, p_utt, params)
        
        dist[[utterance]] = prior * likelihood
    }
    
    s <- sum(unlist(dist))
    
    for (utterance in params$utterances){
        dist[[utterance]] <- ifelse(s == 0, 1 / length(params$utterances), dist[[utterance]] / s)
    }
        
    return(dist[[i_utt]])
}

doubleConfusionProb <- function(speaker_i_utt, listener_i_utt, params) {
    dist = list()
    
    for (i_utt in params$utterances) {
        dist[[i_utt]] = 0
        
        for (p_utt in params$utterances) {
            prior = confusionProb(speaker_i_utt, p_utt, params)
            likelihood = reverseConfusionProb(p_utt, i_utt, params)
            
            dist[[i_utt]] = dist[[i_utt]] + prior * likelihood
        }
    }
    
    s <- sum(unlist(dist))
    
    for (utterance in params$utterances){
        dist[[utterance]] <- ifelse(s == 0, 1 / length(params$utterances), dist[[utterance]] / s)
    }
        
    return(dist[[listener_i_utt]])
}

#params$certain_it
#params$certain_zero
#doubleConfusionProb("pro", "the_agent", params)

cost <- function(utterance, params){
    if (utterance == 'pro') return(params$pro_cost)
    else if (utterance == 'zero') return(params$zero_cost)
    else return(params$np_cost)
}

utterancePriors <- function(state, params){
    probs <- sapply(params$utterances, function(utterance) exp(-params$alpha * cost(utterance, params)), USE.NAMES = FALSE)
    
    obj_bias <- c(params$subject_bias, params$subject_bias, 1 - params$subject_bias, 1 - params$subject_bias)
    subj_bias <- 1 - obj_bias

    if(state == "agent_falls") probs <- probs * subj_bias
    else                       probs <- probs * obj_bias
    
    dist = list()
    
    for (i in seq(1, length(params$utterances))){
        dist[[params$utterances[i]]] <- probs[i]
    }
    
    return(dist)
}

#params$subject_bias = 0.5

#utterancePriors("agent_falls", params)

utterancePrior <- function(utterance, state, params){
    return(utterancePriors(state, params)[[utterance]])
}

listener0 <- function(utterance, params){
    dist = list()
    
    for (state in params$states){
        state_prior <- statePrior(state, params)
        
        likelihood <- sum(sapply(params$utterances, function(i_utt) utterancePrior(i_utt, state, params) * confusionProb(i_utt, utterance, params) * isTrue(i_utt, state, params)))
        
        dist[[state]] <- state_prior * likelihood
    }
    
    s <- sum(unlist(dist))
    
    for (state in params$states){
        dist[[state]] <- ifelse(s == 0, 1 / length(params$states), dist[[state]] / s)
    }
        
    return(dist)
}

literalListener <- listener0

params$certain_it = 0
params$certain_zero = 1

listener0("pro", params)


listenerN <- function(n, utterance, params) {
    if (n == 0) {
        return(listener0(utterance, params))
    }
    else {
        dist = list()
        
        for (state in params$states){
            state_prior <- statePrior(state, params)
            
            likelihood <- sum(sapply(params$utterances, function(i_utt) utterancePrior(i_utt, state, params) * confusionProb(i_utt, utterance, params) * speakerN(n - 1, state, params)[[i_utt]]))
            
            dist[[state]] <- state_prior * likelihood
        }
        
        s <- sum(unlist(dist))
        
        for (state in params$states){
            dist[[state]] <- ifelse(s == 0, 1 / length(params$states), dist[[state]] / s)
        }
        
        return(dist)
    }
}

listener1 <- function(utterance, params) {
    return (listenerN(1, utterance, params))
}

inversePragmaticListener = listener1

listener2 <- function(utterance, params) {
    return (listenerN(2, utterance, params))
}

pragmaticListener = listener2

listenerN(0, "pro", append(params_familiar, list(alpha = 10)))

speaker0 <- function(state, params){
    dist = list()
    
    for (utterance in params$utterances){
        utterance_prior <- utterancePrior(utterance, state, params)
        
        #likelihood <- sum(sapply(params$utterances, function(p_utt) confusionProb(utterance, p_utt, params) * isTrue(p_utt, state, params)))
        likelihood <- isTrue(utterance, state, params)

        dist[[utterance]] <- utterance_prior * likelihood
    }
    
    s <- sum(unlist(dist))
    
    for (utterance in params$utterances){
        dist[[utterance]] <- ifelse(s == 0, 1 / length(params$utterances), dist[[utterance]] / s)
    }
    
    return(dist)
}

speakerN <- function(n, state, params){
    if (n == 0) {
        return(speaker0(state, params))
    }
    else
    {
        dist = list()
        
        listener_params = params
        listener_params$salience_prior = ifelse(state == "agent_falls", params$salience_prior, ifelse(params$salience_prior == 0.5, params$agent_bias, params$salience_prior))
        
        listener_params$patient_color =
            ifelse(params$event_type == "familiar", ifelse(state == 'patient_falls', 'blue', 'red'),
            ifelse(params$event_type == "surprising", ifelse(state == 'patient_falls', 'red', 'blue'),
            ifelse(params$event_type == "postsurprising", ifelse(state == 'patient_falls', 'blue', 'red'),
            'yellow')))
        
        for (utterance in params$utterances){
            
            utterance_prior <- utterancePrior(utterance, state, params)
            
            sum_likelihood <- 0
            
            for (p_utt in params$utterances){
                confusion_prob = confusionProb(utterance, p_utt, params)
                #confusion_prob = ifelse(utterance == p_utt, 1, 0)

                log_likelihood = log(listenerN(n - 1, p_utt, listener_params)[[state]])
                
                sum_likelihood <- sum_likelihood + ifelse(confusion_prob == 0, 0, confusion_prob * log_likelihood)
            }
            
            utility <- exp(ifelse(params$alpha == 0, 0, params$alpha * sum_likelihood))
            
            p <- utterance_prior * utility
    
            dist[[utterance]] <- p
        }
    
        s <- sum(unlist(dist))
        
        for (utterance in params$utterances){
            dist[[utterance]] <- ifelse(s == 0, 1 / length(params$utterances), dist[[utterance]] / s)
        }
        
        return(dist)
    }
}

speaker1 <- function(state, params) {
    return (speakerN(1, state, params))
}


#speaker1("patient_falls", test_params)

#literalListener("pro", test_params)


#params_familiar$action_prior = 0.29197699338965194
#speaker1("patient_falls", params_familiar)

pragmaticSpeaker <- speaker1

speaker2 <- function(state, params) {
    return (speakerN(2, state, params))
}

inversePragmaticSpeaker <- speaker2

#speaker0("patient_falls", params_familiar)


listenerDist <- function(listener, action_priors, params) {
    df = data.frame(action=character(), utterance=character(), state=character(), p=numeric())
    
    for (action in names(action_priors)) {
        params$action_prior <- action_priors[[action]]
        
        for (utterance in params$utterances) {
            dist <- listener(utterance, params)
            
            for (state in names(dist)){
                p <- dist[[state]]
                df <- rbind(df, list(action = action, utterance = utterance, state = state, p = p))
            }
        }
    }
    
    return(df)
}

speakerDist <- function(speaker, action_priors, params) {
    df = data.frame(action=character(), state=character(), utterance=character(), p=numeric())
    
    for (action in names(action_priors)) {
        params$action_prior <- action_priors[[action]]

        for (state in params$states) {
            dist <- speaker(state, params)

            for (utterance in names(dist)){
                p <- dist[[utterance]]
                df <- rbind(df, list(action = action, state = state, utterance = utterance, p = p))
            }
        }
    }
    
    return(df)
}

drawListenerDist <- function(dist, show_legend = FALSE, y_label = NULL) {
    dist %>%
        mutate(action = factor(action, levels = c("jump_over", "wave", "attack", "throw_rock"))) %>%
        filter(utterance %in% c("pro", "zero")) %>%
        group_by(utterance, action, state) %>%
        summarize(n = sum(p), groups = 'keep') %>%
        mutate(percent = n / sum(n)) %>%
        ggplot(aes(x=factor(action, labels = c("jump over", "wave", "attack", "throw rock")) , y=percent, fill=factor(state, labels = c("Agent", "Patient")))) +
        geom_bar(stat="identity", color="black") + 
        facet_grid(~factor(utterance, levels = c("zero", "pro"), labels = c("Zero anaphor", "Pronoun"))) +
        geom_text(aes(label = ifelse(percent < 0.06, "", sprintf("%0.2f", percent))), vjust = 1.2, size = 5, colour = "black", position = "stack") +
        labs(x = "", y=y_label, fill="Interpretation:") +
        #scale_fill_grey(start = 0.8, end = 0.5) +
        scale_fill_manual("Interpretation:", values = c("Agent" = "#FEFE62", "Patient" = "#0C7BDC")) +
        theme_bw() +
        theme(text = element_text(size = 14), legend.position = ifelse(show_legend, 'top', 'none'))
}

#listener_dist = listenerDist(listener0, action_priors, params_low_noise)
#listener0("pro", params2)
#drawListenerDist(listener_dist, TRUE, "Test")

speaker1("patient_falls", params)

drawSpeakerDist <- function(dist, show_legend = FALSE, y_label = NULL) {
    dist %>%
        mutate(action = factor(action, levels = c("jump_over", "wave", "attack", "throw_rock"))) %>%
        mutate(utterance2 = ifelse(utterance %in% c("the_agent", "the_patient"), "np", utterance)) %>%
        mutate(utterance2 = factor(utterance2, levels = c("zero", "pro", "np"), labels = c("Zero anaphor", "Pronoun", "Noun phrase"))) %>%
        group_by(action, state, utterance2) %>%
        summarize(n = sum(p), groups = 'keep') %>%
        mutate(percent = n / sum(n)) %>%
        ggplot(aes(x=factor(action, labels = c("jump over", "wave", "attack", "throw rock")), y=percent, fill=utterance2)) +
        geom_bar(stat="identity", color="black") + 
        facet_grid(~factor(state, labels=c("Agent antecedent", "Patient antecedent"))) +
        geom_text(aes(label = ifelse(percent < 0.06, "", sprintf("%0.2f", percent))), vjust = 1.2, size = 5, colour = "black", position = "stack") +
        #labs(x = "Actions", y="Production rate", fill="Utterance") +
        labs(x="", y=y_label, fill="Utterance:") +
        scale_fill_manual("Utterance:", values = c("Noun phrase" = "#0C7BDC", "Pronoun" = "#E1BE6A", "Zero anaphor" = "#FEFE62")) +
        #scale_fill_grey(start = 0.9, end = 0.5) +
        #scale_x_discrete(labels = c("familiar", "suprising", "post-surpr.")) +
        theme_bw() +
        theme(text = element_text(size = 14), legend.position = ifelse(show_legend, 'top', 'none'))
}

#speaker_dist = speakerDist(speaker1, action_priors, params_familiar)
#drawSpeakerDist(speaker_dist)

readSpeakerGold <- function(gold_csv) {
    
    speaker_gold = read.csv(gold_csv)
    
    speaker_gold <- speaker_gold %>%
        mutate(action = factor(action, levels=actions)) %>%
        mutate(state = factor(state, levels=states)) %>%
        mutate(utterance = factor(utterance, levels=c("zero", "pro", "np"))) %>%
        filter(action %in% actions)
    
    speaker_gold <- speaker_gold %>%
        group_by(action, state, utterance) %>%
        summarize(count = sum(count), groups = 'keep') %>%
        mutate(percent = count * 100 / sum(count))
    
    return (speaker_gold)
}

drawSpeakerGold <- function(gold_csv, show_legend = FALSE, y_label = NULL) {
    
    speaker_gold = readSpeakerGold(gold_csv)
    
    speaker_gold %>%
        group_by(action, state, utterance) %>%
        summarize(n = sum(count), groups = 'keep') %>%
        mutate(percent = n / sum(n)) %>%
        ggplot(aes(x=factor(action, labels = c("jump over", "wave", "attack", "throw rock")), y=percent, fill=factor(utterance, levels = c("zero", "pro", "np"), labels = c("Zero anaphor", "Pronoun", "Noun phrase")))) +
        geom_bar(stat="identity", color="black") + 
        facet_grid(~factor(state, labels=c("Agent antecedent", "Patient antecedent"))) +
        geom_text(aes(label = ifelse(percent < 0.06, "", sprintf("%0.2f", percent))), vjust = 1.2, size = 5, colour = "black", position = "stack") +
        #labs(x = "Actions", y="Production rate", fill="Utterance") +
        labs(x="", y=y_label, fill="Utterance:") +
        #scale_fill_grey(start = 0.9, end = 0.5) +
        scale_fill_manual("Utterance:", values = c("Noun phrase" = "#0C7BDC", "Pronoun" = "#E1BE6A", "Zero anaphor" = "#FEFE62")) +
        theme_bw() +
        theme(text = element_text(size = 14), legend.position = ifelse(show_legend, 'top', 'none'))
}

readListenerGold <- function(gold_csv) {
    
    listener_gold = read.csv(gold_csv)

    listener_gold <- listener_gold %>%
        mutate(action = factor(action, levels=actions)) %>%
        mutate(utterance = factor(utterance, levels=utterances)) %>%
        mutate(state = factor(state, levels=states)) %>%
        filter(action %in% actions)
    
    listener_gold <- listener_gold %>%
        group_by(action, utterance, state) %>%
        summarize(count = sum(count)) %>%
        mutate(percent = count * 100 / sum(count))
    
    return (listener_gold)
}

drawListenerGold <- function(gold_csv, show_legend = FALSE, y_label = NULL) {
    
    listener_gold = readListenerGold(gold_csv)
    
    listener_gold %>%
        group_by(action, utterance, state) %>%
        summarize(n = sum(count)) %>%
        mutate(percent = n / sum(n)) %>%
        ggplot(aes(x=factor(action, labels=c("jump over", "wave", "attack", "throw rock")), y=percent, fill=factor(state, labels=c("Agent", "Patient")))) +
        geom_bar(stat="identity", color="black") + 
        facet_grid(~factor(utterance, levels=c("zero", "pro"), labels = c("Zero anaphor", "Pronoun"))) +
        geom_text(aes(label = ifelse(percent < 0.06, "", sprintf("%0.2f", percent))), vjust = 1.2, size = 5, colour = "black", position = "stack") +
        labs(x = "", y=y_label, fill="Interpretation:") +
        #scale_fill_grey(start = 0.8, end = 0.5) +
        scale_fill_manual("Interpretation:", values = c("Agent" = "#FEFE62", "Patient" = "#0C7BDC")) +
        theme_bw() +
        theme(text = element_text(size = 14), legend.position = ifelse(show_legend, 'top', 'none'))
    #ggsave("fig-perception.pdf", width = 4, height = 3.7, units = "in")
}


