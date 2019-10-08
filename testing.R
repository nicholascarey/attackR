

# Humpback morphometrics --------------------------------------------------

attacker_length <- 1050
# Humpback - Jaw length
Ljaw <- 10^(1.205*log10(attacker_length) - 0.880)
# Diameter
attacker_diameter <- ((attacker_length*0.75)/pi)
## location of max_girth
max_girth <- 0.42 * attacker_length

## Set body segments
## These are proportions of the total length to the
## rostrum from max girth forward
body_segments <- c(0.42,
                   0.35,
                   0.30,
                   0.25,
                   0.20,
                   0.15,
                   0.10,
                   0.05,
                   0.00)
## distance from rostrum of each body segement - convert to cm
segment_dist <- body_segments * attacker_length * 100

## Horizontal profile
## Girth as proportion of max girth at each body segment
horiz_prop <- c(1.0000,
                0.9310,
                0.8800,
                0.7770,
                0.7210,
                0.6760,
                0.5580,
                0.3440,
                0.0000)

## Horizontal diameter at each segment
horiz_diam <- horiz_prop * attacker_diameter

## Vertical profile
## Girth as proportion of max girth at each body segment
vert_prop <- c(1.0000,
               0.9040,
               0.8470,
               0.8030,
               0.7080,
               0.5950,
               0.4430,
               0.3290,
               0.0000)

## Vertical diameter at each segment
vert_diam <- vert_prop * attacker_diameter

## Maximum diamater between vert and horiz
segment_diam <- pmax(horiz_diam, vert_diam)




# Blue Whale Morphometrics ------------------------------------------------

# Blue whale - Jaw length, diameter, location of max_girth
Ljaw <- 10.^(1.36694*log10(attacker_length) - 1.21986)
attacker_diameter <- ((attacker_length*0.434)/pi)*100
max_girth <- 0.20 * attacker_length * 100

body_segments <- c(0.20, 0.18, 0.16, 0.14, 0.12, 0.10, 0.08, 0.06, 0.04, 0.02, 0.00)
segment_dist <- body_segments * attacker_length * 100

horiz_prop <- c(1.00, 0.86, 0.85, 0.85, 0.82, 0.81, 0.75, 0.63, 0.51, 0.29, 0.00)
horiz_diam <- horiz_prop * attacker_diameter

vert_prop <- c(1.00, 1.00, 0.99, 0.96, 0.90, 0.83, 0.70, 0.60, 0.53, 0.41, 0.00)
vert_diam <- vert_prop * attacker_diameter

segment_diam <- pmax(horiz_diam, vert_diam)



# Sea Lion Morphometrics --------------------------------------------------

  # California sealion - of specific size
  # Later we will do do this programmatically as with the whales

  attacker_length <- 1.67
  attacker_diameter <- 55 # Only for this specimen of 1.67m L. Will try and incorporate a formula later

  max_girth <- 0.40 * sealion_length * 100

  body_segments <- c(0.40, 0.35, 0.30, 0.25, 0.20, 0.15, 0.10, 0.05, 0.00)
  segment_dist <- body_segments * attacker_length * 100

  horiz_prop <- c(1.00, 0.93, 0.83, 0.69, 0.56, 0.49, 0.44, 0.34, 0.00)
  horiz_diam <- horiz_prop * attacker_diameter

  vert_prop <- c(1.00, 0.94, 0.88, 0.77, 0.66, 0.61, 0.47, 0.29, 0.00)
  vert_diam <- vert_prop * attacker_diameter

  segment_diam <- pmax(horiz_diam, vert_diam)






  speed = 500
  speed = af_speeds_mont.rd

  model_length = NULL
  model_length = 180

  frequency = 60
  body_length = 1050
  body_width_v = 250.669
  body_width_h = 250.669
  profile_v <- c(0.000, 0.344, 0.558, 0.676, 0.721, 0.777, 0.880, 0.931, 0.950, 0.900, 0.800,
                 0.700, 0.600, 0.500, 0.400, 0.300, 0.250, 0.200, 0.150, 0.100, 0.000)
  profile_h <- c(0.000, 0.329, 0.443, 0.595, 0.708, 0.803, 0.847, 0.904, 0.950, 0.900, 0.800,
                 0.700, 0.600, 0.500, 0.400, 0.300, 0.250, 0.200, 0.150, 0.100, 0.000)
  simple_output = TRUE
  plot = TRUE
  plot_from = 9
  plot_to = NULL
  dAdt_range = c(0.5, 1.5)
  alpha_range = c(0.5, 1)


tmp <- attack_model(speed = af_speeds_mont.rd,
             model_length = NULL,
             frequency = 60,
             body_length = 1050,
             body_width_v = 250.669,
             body_width_h = 250.669,
             max_girth_loc_v = 0.42,
             max_girth_loc_h = 0.42,
             profile_v <- c(0.000, 0.344, 0.558, 0.676, 0.721, 0.777, 0.880, 0.931, 0.980, 0.900, 0.800,
                            0.700, 0.600, 0.500, 0.400, 0.300, 0.250, 0.200, 0.150, 0.100, 0.000),
             profile_h <- c(0.000, 0.329, 0.443, 0.595, 0.708, 0.803, 0.847, 0.904, 0.973, 0.900, 0.800,
                            0.700, 0.600, 0.500, 0.400, 0.300, 0.250, 0.200, 0.150, 0.100, 0.000),
             select_width = "mid",
             simple_output = TRUE,
             plot = TRUE,
             plot_from = 8,
             plot_to = 12,
             dAdt_range = c(0.5, 1.5),
             alpha_range = c(0.2, 0.3))


#rm(list=ls())

speed = 500
model_length = 180
frequency = 60
body_length = 1050
body_width_v = 250.669
body_width_h = 250.669
max_girth_loc_v = 0.42
max_girth_loc_h = 0.42
profile_v = c(0.000, 0.344, 0.558, 0.676, 0.721, 0.777, 0.880, 0.931, 0.950, 0.900, 0.800, 0.700, 0.600, 0.500, 0.400, 0.300, 0.250, 0.200, 0.150, 0.100, 0.000)
profile_h = c(0.000, 0.329, 0.443, 0.595, 0.708, 0.803, 0.847, 0.904, 0.950, 0.900, 0.800, 0.700, 0.600, 0.500, 0.400, 0.300, 0.250, 0.200, 0.150, 0.100, 0.000)
select_width = "mean"
simple_output = TRUE
plot = TRUE
plot_from = 0
plot_to = NULL
dAdt_range = c(0.5, 1.5)
alpha_range = NULL


profile = profile_v
max_girth_loc = max_girth_loc_v
body_length = body_length
body_width = body_width_v

tmp2 <- diam_on_screen(tmp$final_model$alpha, 20)

head(tmp2)
tail(tmp2)



