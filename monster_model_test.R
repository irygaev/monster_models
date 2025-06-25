library(tidyverse)
library(ggplot2)
library(jsonlite)

#actions <- c('jump_over', 'wave', 'attack', 'throw_rock')
actions <- c('overall')
states <- c('agent_falls', 'patient_falls')
utterances <- c('np', 'pro', 'zero')

json <- read_json("sample.json")

speaker_data = data.frame(action=character(), state=character(), utterance=character(), count=integer())

action_list = c()
state_list = c()
utterance_list = c()
count_list = c()

for (i in 1:length(json)) {
    sample <- json[[i]]$PP_speaker
    
    for (j in 1:length(actions)) {
        action <- actions[j]

        for (k in 1:length(states)) {
            state <- states[k]
            
            for (l in 1:length(utterances))  {
                utterance <- utterances[l]
                
                action_list = append(action_list, action)
                state_list = append(state_list, state)
                utterance_list = append(utterance_list, utterance)
                count_list = append(count_list, strtoi(sample[[j]][[k]][[l]]))
            }
        }
    }
}

rows = list(action = action_list, state = state_list, utterance = utterance_list, count = count_list)
speaker_data = rbind(speaker_data, rows)

speaker_data$count = as.numeric(speaker_data$count)

speaker_data <- speaker_data %>%
    mutate(action = factor(action, levels=actions)) %>%
    mutate(state = factor(state, levels=states)) %>%
    mutate(utterance = factor(utterance, levels=utterances))

speaker_gold = read.csv("speaker_gold2.csv")
    
speaker_gold <- speaker_gold %>%
    mutate(action = factor(action, levels=actions)) %>%
    mutate(state = factor(state, levels=states)) %>%
    mutate(utterance = factor(utterance, levels=utterances)) %>%
    filter(action %in% actions)

speaker_gold %>%
    group_by(action, state, utterance) %>%
    summarize(n = sum(count)) %>%
    mutate(percent = n / sum(n)) %>%
    ggplot(aes(x=factor(state, labels=c("agent falls", "patient falls")), y=percent, fill=factor(utterance, levels = c("zero", "pro", "np")))) +
    geom_bar(stat="identity", color="black") + 
    #facet_grid(~action) +
    geom_text(aes(label = ifelse(state == "patient_falls" & utterance == "zero", "", sprintf("%0.2f", percent))), vjust = 1.1, size = 5, colour = "black", position = "stack") +
    labs(x = "State", y="Percentage", fill="Utterance") +
    scale_fill_grey(start = 0.9, end = 0.5) +
    #scale_x_discrete(labels = c("familiar", "suprising", "post-surpr.")) +
    theme_bw() +
    theme(text = element_text(size = 14))
ggsave("fig-production.pdf", width = 4, height = 3.7, units = "in")

speaker_data %>%
    ggplot(aes(x=count)) +
    #geom_histogram() +
    geom_density() +
    geom_point(data = speaker_gold, aes(x = count, y = 0, color = "red")) +
    facet_grid(factor(state, labels=c("agent falls", "patient falls")) ~ factor(utterance, levels=c("zero", "pro", "np"))) +
    guides(color="none") +
    labs(x = "Counts", y="Probability density") +
    ylim(0, 0.12) +
    theme_bw() +
    theme(text = element_text(size = 14))
ggsave("fig-production-model.pdf", width = 8, height = 3, units = "in")



json <- read_json("sample.json")

listener_data = data.frame(action=character(), utterance=character(), state=character(), count=integer())

action_list = c()
utterance_list = c()
state_list = c()
count_list = c()

for (i in 1:length(json)) {
    sample <- json[[i]]$PP_listener
    
    for (j in 1:length(actions)) {
        action <- actions[j]
        
        for (k in 1:(length(utterances)-1)) {
            utterance <- utterances[k + 1]
            
            for (l in 1:length(states))  {
                state <- states[l]
                
                action_list = append(action_list, action)
                utterance_list = append(utterance_list, utterance)
                state_list = append(state_list, state)
                count_list = append(count_list, strtoi(sample[[j]][[k]][[l]]))
            }
        }
    }
}

rows = list(action = action_list, utterance = utterance_list, state = state_list, count = count_list)
listener_data = rbind(listener_data, rows)


listener_data$count = as.numeric(listener_data$count)

listener_data <- listener_data %>%
    mutate(action = factor(action, levels=actions)) %>%
    mutate(utterance = factor(utterance, levels=utterances)) %>%
    mutate(state = factor(state, levels=states))

listener_gold = read.csv("listener_gold2.csv")

listener_gold <- listener_gold %>%
    mutate(action = factor(action, levels=actions)) %>%
    mutate(utterance = factor(utterance, levels=utterances)) %>%
    mutate(state = factor(state, levels=states)) %>%
    filter(action %in% actions)

listener_gold %>%
    group_by(action, utterance, state) %>%
    summarize(n = sum(count)) %>%
    mutate(percent = n / sum(n)) %>%
    ggplot(aes(x=factor(utterance, levels=c("zero", "pro")), y=percent, fill=factor(state, labels=c("agent falls", "patient falls")))) +
    geom_bar(stat="identity", color="black") + 
    #facet_grid(~action) +
    geom_text(aes(label = sprintf("%0.2f", percent)), vjust = 1.1, size = 5, colour = "black", position = "stack") +
    labs(x = "Utterance", y="Percentage", fill="State") +
    scale_fill_grey(start = 0.8, end = 0.5) +
    #scale_x_discrete(labels = c("familiar", "suprising", "post-surpr.")) +
    theme_bw() +
    theme(text = element_text(size = 14))
ggsave("fig-perception.pdf", width = 4, height = 3.7, units = "in")

listener_data %>%
    ggplot(aes(x=count)) +
    #geom_histogram() +
    geom_density() +
    geom_point(data = listener_gold, aes(x = count, y = 0, color = "red")) +
    facet_grid(factor(utterance, levels = c("zero", "pro")) ~ factor(state, labels=c("agent falls", "patient falls"))) +
    guides(color="none") +
    labs(x = "Counts", y="Probability density") +
    ylim(0, 0.06) +
    theme_bw() +
    theme(text = element_text(size = 14))
ggsave("fig-perception-model.pdf", width = 8, height = 3, units = "in")

