# tune for we wish you a merry christmas 

notes <- c(C = 0, D = 2, E = 4, F = 6, G = 7, A = 9, B = 11) # F note = F sharp 

pitch <- paste(
  "D G G A G F E C",
  "E A A B A G F D",
  "D B B C5 B A G E",
  "D D E A F G",
  "D G G G F",
  "F G F E D",
  "A B A G D5 D", # high D note
  "D D E A F G"
)

duration <- c(
  1, 1, 0.5, 0.5, 0.5, 0.5, 1, 1, 
  1, 1, 0.5, 0.5, 0.5, 0.5, 1, 1, 
  1, 1, 0.5, 0.5, 0.5, 0.5, 1, 1, 
  0.5, 0.5, 1, 1, 1, 2,
  1, 1, 1, 1, 2,
  1, 1, 1, 1, 2,
  1, 1, 1, 1, 1, 1,
  0.5, 0.5, 1, 1, 1, 2
)

wish <- data_frame(pitch = strsplit(pitch, " ")[[1]],
                   duration = duration)

wish <- wish %>%
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

tempo <- 150

sample_rate <- 44100

make_sine <- function(freq, duration) {
  wave <- sin(seq(0, duration / tempo * 60, 1 / sample_rate) *
                freq * 2 * pi)
  fade <- seq(0, 1, 50 / sample_rate)
  wave * c(fade, rep(1, length(wave) - 2 * length(fade)), rev(fade))
}

wish_wave <- mapply(make_sine, wish$freq, wish$duration) %>%
  do.call("c", .)

save.wave(wish_wave, 'wish.wav')
