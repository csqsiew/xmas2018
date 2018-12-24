# tune for silver bells  

library(audio)
library(tidyverse)

notes <- c(C = 12, D = 14, E = 4, F = 5, g = 6, G = 7, A = 9, B = 11, x = -100) # g = F sharp, x = blank  

pitch <- paste(
    "C A G E C A G E",
    "E5 D C A A A",
    "D C B G g F G F F E x",
    "C A G E C A G E",
    "E5 D C A A A",
    "D C B G g F G D C x",
    "E F G x A B C x", #chorus
    "B B C D C B C G x",
    "E F G x A B C x",
    "B B C D C B C"
)

duration <- c(
    0.5, 0.5, 1, 1, 0.5, 0.5, 1, 1, 
    0.5, 0.5, 1, 1, 1, 2,
    0.5, 0.5, 1, 1, 1, 1, 1, 1, 1, 2.5, 0.5,
    0.5, 0.5, 1, 1, 0.5, 0.5, 1, 1, 
    0.5, 0.5, 1, 1, 1, 2, 
    0.5, 0.5, 1, 1, 1, 1, 1, 1, 2.5, 0.5,
    0.5, 0.5, 2, 0.25, 0.5, 0.5, 2, 0.25, # chorus
    1, 1, 1, 2, 0.5, 0.5, 1,2, 0.5,
    0.5, 0.5, 2, 0.25, 0.5, 0.5, 2, 0.25,
    1, 1, 1, 1, 1, 1, 2.5
)

silver <- data_frame(pitch = strsplit(pitch, " ")[[1]],
                   duration = duration)

silver <- silver %>%
    mutate(octave = substring(pitch, nchar(pitch)) %>%
    {suppressWarnings(as.numeric(.))} %>%
        ifelse(is.na(.), 4, .),
    note = notes[substr(pitch, 1, 1)],
    note = note + 
        grepl("#", pitch) -
        grepl("b", pitch) + 
        octave * 12 +
        12 * (note < 0),
    freq = 2 ^ ((note - 60) / 12) * 800) 

tempo <- 120

sample_rate <- 44100

make_sine <- function(freq, duration) {
    wave <- sin(seq(0, duration / tempo * 60, 1 / sample_rate) *
                    freq * 2 * pi)
    fade <- seq(0, 1, 50 / sample_rate)
    wave * c(fade, rep(1, length(wave) - 2 * length(fade)), rev(fade))
}

silver_wave <- mapply(make_sine, silver$freq, silver$duration) %>%
    do.call("c", .)

save.wave(silver_wave, 'silver.wav')
