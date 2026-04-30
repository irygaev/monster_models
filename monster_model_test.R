source("functions.R")

actions <- c('jump_over', 'wave', 'attack', 'throw_rock')
#actions <- c('throw_rock')
#actions <- c('overall')
states <- c('agent_falls', 'patient_falls')
utterances <- c('np', 'pro', 'zero')

filename = "sample1.json"


speaker_data1 = readSpeakerData(filename, 1)
drawSpeakerDist(speaker_data1)
drawSpeaker(speaker_data1, "prod_data_training_familiar.csv")

speaker_data2 = readSpeakerData(filename, 2)
drawSpeakerDist(speaker_data2)
drawSpeaker(speaker_data2, "prod_data_training_surprising.csv")

speaker_data3 = readSpeakerData(filename, 3)
drawSpeakerDist(speaker_data3)
drawSpeaker(speaker_data3, "prod_data_training_postsurprising.csv")

speaker_data4 = readSpeakerData(filename, 4)
drawSpeakerDist(speaker_data4)
drawSpeaker(speaker_data4, "prod_data_no_training_rest.csv")


listener_data1 = readListenerData(filename, 1)
drawListener(listener_data1, "perc_data_clean.csv")

listener_data2 = readListenerData(filename, 2)
drawListener(listener_data2, "perc_data_low_noise.csv")

listener_data3 = readListenerData(filename, 2)
drawListener(listener_data3, "perc_data_high_noise.csv")





drawSpeakerGold("prod_data_training.csv")
drawSpeakerGold("prod_data_training_familiar.csv")
drawSpeakerGold("prod_data_training_surprising.csv")
drawSpeakerGold("prod_data_training_postsurprising.csv")
drawSpeakerGold("prod_data_no_training.csv")
drawSpeakerGold("prod_data_no_training_first.csv")
drawSpeakerGold("prod_data_no_training_rest.csv")

drawListenerGold("perc_data_clean.csv")
drawListenerGold("perc_data_low_noise.csv")
drawListenerGold("perc_data_high_noise.csv")



