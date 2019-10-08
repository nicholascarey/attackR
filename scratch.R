## HUMPBACK
attacker_length <- 10.5
## length of jaw
Ljaw <- 10^(1.205*log10(body_length) - 0.880)
body_width_v <- ((body_length*0.75)/pi)
body_width_h <- ((body_length*0.75)/pi)
max_girth_loc_v <- 0.42
max_girth_loc_h <- 0.42
profile_v <- c(0.000, 0.344, 0.558, 0.676, 0.721, 0.777, 0.880, 0.931, 0.980, 0.900, 0.800,
               0.700, 0.600, 0.500, 0.400, 0.300, 0.250, 0.200, 0.150, 0.100, 0.000)
profile_h <- c(0.000, 0.329, 0.443, 0.595, 0.708, 0.803, 0.847, 0.904, 0.973, 0.900, 0.800,
               0.700, 0.600, 0.500, 0.400, 0.300, 0.250, 0.200, 0.150, 0.100, 0.000)



## af
species = "humpback"
x = af_mont_speeds.rd
frequency = 60
fish_at = 651
a_mouth_open = 613
b_max_gape_start = 651
c_max_gape_end = 663
d_mouth_closed = 709
## socal
species = "humpback"
x = af_xs_socal.rd
frequency = 60
model_length = 651
a_mouth_open = 613
b_max_gape_start = 651
c_max_gape_end = 663
d_mouth_closed = 709

select_width = "mid"
simple_output = TRUE
plot = TRUE
plot_from = NULL
plot_to = NULL
cALT = NULL
dAdt_range = NULL




# Species specific parameters ---------------------------------------------

  jaw_length <- (10^(1.205*log10(body_length/100) - 0.880))*100
  lower_jaw_angle <- 50
  upper_jaw_angle <- 30
  max_girth_loc_v <- 0.42
  max_girth_loc_h <- 0.42


  bl_blue <- 2280
  jaw_length <- 10^(1.36694*log10(bl_blue/100) - 1.21986)*100
  body_width_v <- ((bl_blue*0.434)/pi)*100
  body_width_h <- ((bl_blue*0.434)/pi)*100
  lower_jaw_angle <- 50
  upper_jaw_angle <- 30
  max_girth_loc_v <- 0.20
  max_girth_loc_h <- 0.20








## HUMPBACK
body_length <- 1050
## length of jaw
#Ljaw <- 10^(1.205*log10(body_length) - 0.880)
Ljaw <- (10^(1.205*log10(body_length/100) - 0.880))*100

body_width_v <- ((body_length*0.75)/pi)
body_width_h <- ((body_length*0.75)/pi)
max_girth_loc_v <- 0.42
max_girth_loc_h <- 0.42
profile_v <- c(0.000, 0.344, 0.558, 0.676, 0.721, 0.777, 0.880, 0.931, 0.980, 0.900, 0.800,
               0.700, 0.600, 0.500, 0.400, 0.300, 0.250, 0.200, 0.150, 0.100, 0.000)
profile_h <- c(0.000, 0.329, 0.443, 0.595, 0.708, 0.803, 0.847, 0.904, 0.973, 0.900, 0.800,
               0.700, 0.600, 0.500, 0.400, 0.300, 0.250, 0.200, 0.150, 0.100, 0.000)


## af
species = "humpback"
speed = af_speeds_mont.rd
frequency = 60
model_length = NULL
a_mouth_open = 622
b_max_gape_start = 688
c_max_gape_end = 718
d_mouth_closed = 802
## af NO MO
species = "humpback"
speed = af_speeds_mont.rd
frequency = 60
model_length = NULL
a_mouth_open = NULL
b_max_gape_start = NULL
c_max_gape_end = NULL
d_mouth_closed = NULL
## af
species = "humpback"
speed = af_speeds_mont.rd
frequency = 60
model_length = 651
a_mouth_open = 613
b_max_gape_start = 651
c_max_gape_end = 663
d_mouth_closed = 709
## socal
species = "humpback"
speed = af_speeds_socal.rd
frequency = 60
model_length = 651
a_mouth_open = 613
b_max_gape_start = 651
c_max_gape_end = 663
d_mouth_closed = 709

select_width = "mid"
simple_output = TRUE
plot = TRUE
plot_from = NULL
plot_to = NULL
alpha_range = NULL
dAdt_range = NULL


blength <- 22.8
blength <- 2280
## BLUE
Ljaw <- 10^(1.36694*log10(blength) - 1.21986)
Ljaw <- 10^(1.36694*log10(blength/100) - 1.21986)*100
attacker_diameter <- ((attacker_length*0.434)/pi)*100
max_girth <- 0.20
body_segments <- c(0.20, 0.18, 0.16, 0.14, 0.12, 0.10, 0.08, 0.06, 0.04, 0.02, 0.00)
horiz_prop <- c(1.00, 0.86, 0.85, 0.85, 0.82, 0.81, 0.75, 0.63, 0.51, 0.29, 0.00)
vert_prop <- c(1.00, 1.00, 0.99, 0.96, 0.90, 0.83, 0.70, 0.60, 0.53, 0.41, 0.00)


## Jaw angle for whales
lower_jaw_angle <- 50 # degrees from observations
upper_jaw_angle <- 30 # degrees from observations


## SEALION
attacker_length <- 1.67
attacker_diameter <- 55 # Only for this specimen of 1.67m L. Will try and incorporate a formula later
max_girth <- 0.40
body_segments <- c(0.40, 0.35, 0.30, 0.25, 0.20, 0.15, 0.10, 0.05, 0.00)
horiz_prop <- c(1.00, 0.93, 0.83, 0.69, 0.56, 0.49, 0.44, 0.34, 0.00)
vert_prop <- c(1.00, 0.94, 0.88, 0.77, 0.66, 0.61, 0.47, 0.29, 0.00)



speed = af_speeds_mont.rd
a_mouth_open = 622
b_max_gape_start = 688
c_max_gape_end = 718
d_mouth_closed = 802





# NEW Mouth Opening -------------------------------------------------------





plot(output$final_model$alpha, ylim = c(0, 3.14))
plot(alpha_total_jaw, ylim = c(0, 3.14))
plot(pmax(alpha_total_jaw, output$final_model$alpha), ylim = c(0, 3.14))

plot(diff(pmax(alpha_total_jaw, output$final_model$alpha))*60)


tail(alpha_total_jaw)
tail(output$final_model$alpha)



## Centre profile

profile <- bw_prof_h
profile <- bw_prof_v
profile <- interpolate_widths(bw_prof_v, NULL, 1053, 441)
profile <- profile_h

profile_centered <- data.frame(x = 1:length(profile), neg = -1 * ((profile/max(profile))/2), pos = ((profile/max(profile))/2))

plot.new()
plot(1, type="n", xlab="", ylab="", xlim = c(0,nrow(profile_centered)), ylim = c(-0.5,0.5),
     axes = FALSE)

for(i in 1:nrow(profile_centered)){
rect(profile_centered[i,1]-0.5, profile_centered[i,2], profile_centered[i,1]+0.5, profile_centered[i,3])
}











humpback_length <- 1050

attack_model_whale(
  species = "humpback",
  speed = af_speeds_mont.rd,
  model_length = NULL,
  frequency = 60,
  body_length = humpback_length,
  body_width_v = ((humpback_length*0.75)/pi),
  body_width_h = ((humpback_length*0.75)/pi),
  profile_v <- c(0.000, 0.344, 0.558, 0.676, 0.721, 0.777, 0.880, 0.931, 0.980, 0.900, 0.800,
                 0.700, 0.600, 0.500, 0.400, 0.300, 0.250, 0.200, 0.150, 0.100, 0.000),
  profile_h <- c(0.000, 0.329, 0.443, 0.595, 0.708, 0.803, 0.847, 0.904, 0.973, 0.900, 0.800,
                 0.700, 0.600, 0.500, 0.400, 0.300, 0.250, 0.200, 0.150, 0.100, 0.000),
  max_girth_loc_v = 0.42,
  max_girth_loc_h = 0.42,
  width_filter = "mid",
  a_mouth_open = 622,
  b_max_gape_start = 688,
  c_max_gape_end = 718,
  d_mouth_closed = 802,
  simple_output = TRUE,
  plot = TRUE,
  plot_from = 8,
  plot_to = 12,
  alpha_range = NULL,
  dAdt_range = c(1,2))




attack_model_whale(
  species = "humpback",
  speed = af_speeds_socal.rd,
  model_length = 651,
  frequency = 60,
  body_length = humpback_length,
  body_width_v = ((humpback_length*0.75)/pi),
  body_width_h = ((humpback_length*0.75)/pi),
  profile_v <- c(0.000, 0.344, 0.558, 0.676, 0.721, 0.777, 0.880, 0.931, 0.980, 0.900, 0.800,
                 0.700, 0.600, 0.500, 0.400, 0.300, 0.250, 0.200, 0.150, 0.100, 0.000),
  profile_h <- c(0.000, 0.329, 0.443, 0.595, 0.708, 0.803, 0.847, 0.904, 0.973, 0.900, 0.800,
                 0.700, 0.600, 0.500, 0.400, 0.300, 0.250, 0.200, 0.150, 0.100, 0.000),
  max_girth_loc_v = 0.42,
  max_girth_loc_h = 0.42,
  width_filter = "mid",
  a_mouth_open = 613,
  b_max_gape_start = 651,
  c_max_gape_end = 663,
  d_mouth_closed = 709,
  simple_output = TRUE,
  plot = TRUE,
  plot_from = 8,
  plot_to = 12,
  alpha_range = c(2.5,2.8),
  dAdt_range = c(0.91,6))


