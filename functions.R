library(tidyverse)
library(ggplot2)
library(jsonlite)

readSpeakerData <- function(sample_filename, part) {
    
    json <- read_json(sample_filename)
    
    speaker_data = data.frame(action=character(), state=character(), utterance=character(), count=numeric())
    
    size <- length(json) * length(actions) * length(states) * length(utterances)
    action_offset = (part - 1) * length(actions)
    
    action_list = character(size)
    state_list = character(size)
    utterance_list = character(size)
    count_list = numeric(size)
    
    index = 0
    
    for (i in 1:length(json)) {
        sample <- json[[i]]$speaker
        
        for (j in 1:length(actions)) {
            action <- actions[j]
            
            for (k in 1:length(states)) {
                state <- states[k]
                
                for (l in 1:length(utterances))  {
                    utterance <- utterances[l]
                    
                    index <- index + 1
                    
                    action_list[index] = action
                    state_list[index] = state
                    utterance_list[index] = utterance
                    count_list[index] = strtoi(sample[[action_offset + j]][[k]][[l+1]]) + ifelse(l > 1, 0, strtoi(sample[[j]][[k]][[1]]))
                }
                
                offset = index - length(utterances) + 1
                s = sum(count_list[offset:index])
                
                for (l in 0:(length(utterances) - 1))  {
                    count_list[offset + l] <- count_list[offset + l] * 100 / s
                }
            }
        }
    }
    
    rows = list(action = action_list, state = state_list, utterance = utterance_list, count = count_list)
    speaker_data = rbind(speaker_data, rows)
    
    #speaker_data$count = as.numeric(speaker_data$count)
    
    speaker_data <- speaker_data %>%
        mutate(action = factor(action, levels=actions)) %>%
        mutate(state = factor(state, levels=states)) %>%
        mutate(utterance = factor(utterance, levels=rev(utterances)))
    
    return (speaker_data)
}

readSpeakerGold <- function(gold_csv) {
    
    speaker_gold = read.csv(gold_csv)
    
    speaker_gold <- speaker_gold %>%
        mutate(action = factor(action, levels=actions)) %>%
        mutate(state = factor(state, levels=states)) %>%
        mutate(utterance = factor(utterance, levels=utterances)) %>%
        filter(action %in% actions)
    
    speaker_gold <- speaker_gold %>%
        group_by(action, state, utterance) %>%
        summarize(count = sum(count)) %>%
        mutate(percent = count * 100 / sum(count))
    
    return (speaker_gold)
}

drawSpeaker <- function(speaker_data, gold_csv) {
    
    speaker_gold = readSpeakerGold(gold_csv)
    
    speaker_data %>%
        ggplot(aes(x=count)) +
        #geom_histogram() +
        geom_density() +
        geom_point(data = speaker_gold, aes(x = percent, y = 0, color = "red")) +
        facet_grid(action ~ state + utterance) +
        ylim(0, 0.2)
    
    #    speaker_data %>%
    #        ggplot(aes(x=count)) +
    #        #geom_histogram() +
    #        geom_density() +
    #        geom_point(data = speaker_gold, aes(x = percent, y = 0, color = "red")) +
    #        facet_grid(action ~ state + factor(utterance, levels=c("np", "pro", "zero"))) +
    #        guides(color="none") +
    #        labs(x = "Production rate, %", y="Probability density") +
    #        ylim(0, 0.2) +
    #        theme_bw() +
    #        theme(text = element_text(size = 14))
    #    #ggsave("fig-production-model.png", width = 8, height = 3, units = "in")
}

drawSpeakerDist <- function(dist) {
    dist %>%
        group_by(action, state, utterance) %>%
        summarize(n = sum(count)) %>%
        mutate(percent = n / sum(n)) %>%
        ggplot(aes(x=action, y=percent, fill=utterance)) +
        geom_bar(stat="identity", color="black") + 
        facet_grid(~state) +
        geom_text(aes(label = ifelse(percent < 0.04, "", sprintf("%0.2f", percent))), vjust = 1.1, size = 5, colour = "black", position = "stack") +
        labs(x = "State", y="Percentage", fill="Utterance") +
        scale_fill_grey(start = 0.9, end = 0.5) +
        #scale_x_discrete(labels = c("familiar", "suprising", "post-surpr.")) +
        theme_bw() +
        theme(text = element_text(size = 14))
}


drawSpeakerGold <- function(gold_csv) {
    
    speaker_gold = readSpeakerGold(gold_csv)
    
    speaker_gold %>%
        group_by(action, state, utterance) %>%
        summarize(n = sum(count)) %>%
        mutate(percent = n / sum(n)) %>%
        ggplot(aes(x=action, y=percent, fill=factor(utterance, levels = c("zero", "pro", "np")))) +
        geom_bar(stat="identity", color="black") + 
        facet_grid(~state) +
        geom_text(aes(label = ifelse(percent < 0.04, "", sprintf("%0.2f", percent))), vjust = 1.1, size = 5, colour = "black", position = "stack") +
        labs(x = "State", y="Percentage", fill="Utterance") +
        scale_fill_grey(start = 0.9, end = 0.5) +
        theme_bw() +
        theme(text = element_text(size = 14))
}


readListenerData <- function(sample_filename, part) {
    
    json <- read_json(sample_filename)
    
    listener_data = data.frame(action=character(), utterance=character(), state=character(), count=numeric())
    
    size <- length(json) * length(actions) * (length(utterances) - 1) * length(states)
    action_offset = (part - 1) * length(actions)
    
    action_list = character(size)
    state_list = character(size)
    utterance_list = character(size)
    count_list = numeric(size)
    
    index = 0
    
    for (i in 1:length(json)) {
        sample <- json[[i]]$listener
        
        for (j in 1:length(actions)) {
            action <- actions[j]
            
            for (k in 1:(length(utterances)-1)) {
                utterance <- utterances[k + 1]
                
                for (l in 1:length(states))  {
                    
                    state <- states[l]
                    
                    index <- index + 1
                    
                    action_list[index] = action
                    utterance_list[index] = utterance
                    state_list[index] = state
                    count_list[index] = strtoi(sample[[action_offset + j]][[k]][[l]])
                }
                
                offset = index - length(states) + 1
                s = sum(count_list[offset:index])
                
                for (l in 0:(length(states) - 1))  {
                    count_list[offset + l] <- count_list[offset + l] * 100 / s
                }
                
                #s = sum(tail(count_list, length(states)))
                #offset = length(count_list) - length(states)
                
                #for (l in 1:length(states))  {
                #    count_list[offset + l] <- count_list[offset + l] * 100 / s
                #}
            }
        }
    }
    
    rows = list(action = action_list, utterance = utterance_list, state = state_list, count = count_list)
    listener_data = rbind(listener_data, rows)
    
    listener_data <- listener_data %>%
        mutate(action = factor(action, levels=actions)) %>%
        mutate(utterance = factor(utterance, levels=utterances)) %>%
        mutate(state = factor(state, levels=states))
    
    return (listener_data)
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

drawListener <- function(listener_data, gold_csv) {
    
    listener_gold = readListenerGold(gold_csv)
    
    listener_data %>%
        ggplot(aes(x=count)) +
        #geom_histogram() +
        geom_density() +
        geom_point(data = listener_gold, aes(x = percent, y = 0, color = "red")) +
        facet_grid(action ~ utterance + state) +
        #xlim(0, 100) +
        ylim(0, 0.2)
    
    #    listener_data %>%
    #        ggplot(aes(x=count)) +
    #        #geom_histogram() +
    #        geom_density() +
    #        geom_point(data = listener_gold, aes(x = percent, y = 0, color = "red")) +
    #        facet_grid(factor(utterance, levels = c("zero", "pro"), labels = c('zero anaphor', 'pronoun')) ~ factor(state, labels=c("patient falls", "agent falls"))) +
    #        guides(color="none") +
    #        labs(x = "Perception rate in %", y="Probability density") +
    #        #ylim(0, 0.12) +
    #        theme_bw() +
    #        theme(text = element_text(size = 14))
    #    ggsave("fig-perception-model.png", width = 8, height = 3, units = "in")
}

drawListenerGold <- function(gold_csv) {
    
    listener_gold = readListenerGold(gold_csv)
    
    listener_gold %>%
        group_by(action, utterance, state) %>%
        summarize(n = sum(count)) %>%
        mutate(percent = n / sum(n)) %>%
        ggplot(aes(x=factor(utterance, levels=c("zero", "pro")), y=percent, fill=factor(state, labels=c("agent falls", "patient falls")))) +
        geom_bar(stat="identity", color="black") + 
        facet_grid(~action) +
        geom_text(aes(label = sprintf("%0.2f", percent)), vjust = 1.1, size = 5, colour = "black", position = "stack") +
        labs(x = "Utterance", y="Percentage", fill="State") +
        scale_fill_grey(start = 0.8, end = 0.5) +
        theme_bw() +
        theme(text = element_text(size = 14))
    #ggsave("fig-perception.pdf", width = 4, height = 3.7, units = "in")
}

