library(tidyverse)
library(tidyr)
full_data <-read_csv('MET2J_filtered_instruments.csv')

filtered_full_data <- full_data |>
  na.omit() |>
  filter(`Date of Birth`>= 1900, `Date of Birth` <= 2000 ) |>
  mutate(Decade = case_when(
    startsWith(as.character(`Date of Birth`), "190") ~ "1900s",
    startsWith(as.character(`Date of Birth`), "191") ~ "1910s",
    startsWith(as.character(`Date of Birth`), "192") ~ "1920s",
    startsWith(as.character(`Date of Birth`), "193") ~ "1930s",
    startsWith(as.character(`Date of Birth`), "194") ~ "1940s",
    startsWith(as.character(`Date of Birth`), "195") ~ "1950s",
    startsWith(as.character(`Date of Birth`), "196") ~ "1960s",
    startsWith(as.character(`Date of Birth`), "197") ~ "1970s",
    startsWith(as.character(`Date of Birth`), "198") ~ "1980s",
    startsWith(as.character(`Date of Birth`), "199") ~ "1990s",
  TRUE ~ as.character(`Date of Birth`)  )) |>
  mutate(Instrument = recode(Instrument,"Singing"= "Voice", "Microphone" = "Voice", 
                                                    "Lead vocalist" = "Voice", "Human voice" = "Voice", 
                                                    "Vocal music" = "Voice", "Soprano" = "Voice", 
                                                    "Rapping" = "Voice", "Singer-songwriter"="Voice",
                                                    "Tenor" = "Voice", "Scat singing" = "Voice",
                                                    "Vocal percussion" = "Voice",  
                                                    "Vocoder"="Voice", "Beatboxing"="Voice")) |>
  mutate(Instrument = recode(Instrument,"pianist" = "Piano", "Steinway & Sons" = "Piano", 
                            "Rhodes piano" = "Piano", "Baldwin Piano Company" = "Piano", 
                            "List of Korg products" = "Piano", "Keyboard instrument" = "Piano", 
                            "Fairlight CMI" = "Piano")) |>
  mutate(Instrument = recode(Instrument, "Guitarist" = "Guitar", "PRS guitars" = "Guitar", 
                             "Ibanez" = "Guitar", "Acoustic guitar" = "Guitar", 
                             "ESP Guitars" = "Guitar", "Yamaha electric guitar models" = "Guitar", 
                             "Pedal steel guitar" = "Guitar", "Gibson Les Paul" = "Guitar", 
                             "Takamine guitars" = "Guitar", "Fender Stratocaster" = "Guitar", 
                             "Gibson ES-150" = "Guitar", "Dobro" = "Guitar", "C. F. Martin & Company" = "Guitar",
                             "Avalon Guitars" = "Guitar", "Bentar" = "Guitar", "Fender Jazzmaster" = "Guitar", 
                             "Steel-string acoustic guitar" = "Guitar", "Dean Guitars" = "Guitar", 
                             "Fernandes Guitars" = "Guitar", "Gretsch" = "Guitar", "Fender Telecaster" = "Guitar", 
                             "B.C. Rich, Harmony Company" = "Guitar", "Rhythm guitar" = "Guitar", "Hagström" = "Guitar", 
                             "Rickenbacker" = "Guitar", "Fender Telecaster" = "Guitar", "Twelve-string guitar" = "Guitar", 
                             "Yamaha Corporation" = "Guitar",  "Slide guitar" = "Guitar", "Ovation Guitar Company" = "Guitar", 
                             "First Act" = "Guitar", "Lap steel guitar" = "Guitar", "Electric guitar" = "Guitar", 
                             "Steinberger" = "Guitar", "Gibson ES-335" = "Guitar", "Gibson Ripper" = "Guitar", 
                             "Jackson Guitars" = "Guitar", "Gibson Flying V" = "Guitar", "Fender Jaguar" = "Guitar", 
                             "Classical guitar" = "Guitar")) |>
  mutate(Instrument = recode(Instrument, "Music Man (company)" = "Bass guitar", 
                             "Fender Jazz Bass" = "Bass guitar", "Slapping (music)" = "Bass guitar", 
                             "Fender Precision Bass" = "Bass guitar", "Double bass" = "Bass guitar", 
                             "Fender Mustang Bass" = "Bass guitar", "Wal (bass)" = "Bass guitar")) |>
  mutate(Instrument = recode(Instrument, "Electric violin" = "Violin")) |>
  mutate(Instrument = recode(Instrument, "Multi-instrumentalist" = "Orchestra")) |>
  mutate(Instrument = recode(Instrument, "Hohner" = "Harmonica")) |>
  mutate(Instrument = recode(Instrument, "Programming (music)" = "Music software",  
                             "Deejay (Jamaican)" = "Music software", "Reason (software)" = "Music software", 
                             "Vox (musical equipment)" = "Music software", "Logic Pro" = "Music software",  
                             "Sampler (musical instrument)" = "Music software", 
                             "DJ mixer" = "Music software", "Turntablism" = "Music software", 
                             "Ampeg" = "Music software", "FL Studio" = "Music software",  
                             "Skoog" = "Music software", "Sampler (musical instrument)" = "Music software", 
                             "Electronic musical instrument" = "Music software", 
                             "Digital audio workstation" = "Music software", "Programmer" = "Music software", 
                             "Game Boy music" = "Music software")) |>
  mutate(Instrument = recode(Instrument, "Drum" = "Drums" , "Bass drum" = "Drums", 
                               "Drum machine" = "Drums", "Sabian" = "Drums", 
                               "Drum Workshop" = "Drums", "Drum" = "Drums", 
                               "Gretsch Drums" = "Drums", "Percussion instrument" = "Drums", 
                               "Noble & Cooley" = "Drums", "Tom-tom drum" = "Drums", 
                               "Mapex Drums" = "Drums", "Drum kit" = "Drums")) |>
  mutate(Instrument = recode(Instrument, "Types of trombone" = "Trombone", "C.G. Conn" = "Trombone")) |>
  group_by(Decade) |>
  select(-`Date of Birth`)|>
  write_csv("Cleaned_Instruments.csv")
 
#pivoted_instruments <- filtered_full_data |>
  #pivot_wider(names_from = Instrument, values_from = Country)

print(filtered_full_data, n=20)


