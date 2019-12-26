# `attackR`
An `R` package for modelling the visual aspects of an attack by a predator on a prey.

### Publication
This code was used in the following publication in PNAS to determine how much of a prey's visual field is taken up by an attacking humpback whale. It allows for various speeds and timings of the whale opening its mouth, which vastly increases the apparent visual angle of the whale. While it was applied specifically to humpback whales, given some simple morphological parameters it can also be applied to other filter-feeding whales which engulf large volumes of water, such as blue whales. More importantly, it can be used to model how **any** attacking predator appears to a prey.   

**David E. Cade, Nicholas Carey, Paolo Domenici, Jean Potvin, Jeremy A. Goldbogen, 2019**. Predator-informed looming stimulus experiments reveal how large filter feeding whales capture highly maneuverable forage fish. *Proceedings of the National Academy of Sciences*. https://doi.org/10.1073/pnas.1911099116


![text](images/fig1)

### Main functions

#### `attack_model`
The primary function in the package is `attack_model`. The function uses parameters such as the predator size, speed, and shape to determine the maximum perceived width at any stage if the attack, and how large this appears in the prey's visual field. From the prey's perspective it calculates the visual angle of the attacker (**alpha**, or **{α}**) in radians, and the rate of change of this angle (**{dα/dt}** in radians/s), as well as distance and time to capture.

The function only requires a relatively small number of inputs:

 *\code{frequency}* in *\code{Hz}*. This is the resolution of the model (equivalent to frame rate if creating a looming animation)

*\code{speed}* A single, constant speed value or a vector of variable speeds can be entered. 

*\code{model_length}* determines the ultimate length of the model, and from how far away the predator starts its attack.


Morphology: In order to correctly calculate the widest apparent part of the predator, and thus **{α}**, the function requires several morphometric inputs:

*\code{body_length}* is simply the total length from nose to tail (or equivalent). 

*\code{body_width_v}* and *\code{body_width_h}* are the **maximum** body widths in the vertical (i.e. dorsal:ventral) and horizontal (lateral) planes of the body. 


The function calculates the widest apparent width of any part of the predator's body from the prey's perspective at each instance along the model. This is *usually* the maximum width of the predator, but it also depends on the predator's shape. At closer distances, more anterior parts of the predator will appear to be wider, and so result in a higher **{α}** value. For this reason, predator shape profiles can be entered. 
 
*\code{profile_v}* and *\code{profile_h}* are vectors of widths of the predator's body as a proportion of the maximum width from nose to tail. Therefore, they should generally start and end on zero (though they don't have to), and all values must be between zero and 1. They must be regularly spaced, e.g. every 10\% along the body. The longer (i.e. higher resolution) these profiles are, the better their representation of the morphology of the animal. Actual values of width along the body are calculated to the resolution of the entered unit of *\code{body_length}* by linearly interpolating between each proportional width. For example, if the predator *\code{body_length}* is 1000cm, a width value is calculated at every
 cm along the body through interpolation between each proportional width.
 
Only one profile is required. This is be useful if your predator is always wider in one plane than the other, or generally the same width in both planes. 




@section Filtering of maximum apparent width between body planes: Apparent
 widths are calculated for both body planes, if two are entered. Which width
 at each body segment (e.g. horizontal or vertical) used to calculate **{α}**
 is determined via the *\code{width_filter}* operator. This can be the
 midpoint value between them (*\code{"mid"}*, the default), maximum value
 (*\code{"max"}*), or minimum value (*\code{"min"}*). You can also choose to
 use only the vertical or horizontal profile widths exclusively
 (*\code{"v"}*, *\code{"h"}*). You can also choose to use the predator's
 maximum width in either plane (*\code{max_width_v}*, *\code{max_width_h}*)
 to calculate **{α}**, in which case all other segments of the body are
 ignored, and only **{α}** of the maximum width is determined.

 *\code{max_width_loc_v}* and *\code{max_width_loc_h}* are the locations of
 the maximum widths of the predator occur along the body as a proportion of
 the total body length going from the anterior. They are only necessary if
 they are not specified as one of the proportional widths in
 *\code{profile_v}* and *\code{profile_h}*, that is none of these have the
 value of exactly 1. They can also occur at an intermediate section of the
 body, not included as part of the body profiles.




@section Output: By default *\code{simple_output = TRUE}* the output is a data
 frame with every row representing a single instance (*\code{frame}*) of the
 model at the set *\code{frequency}*. The data frame contains columns for
 frame, speed, time, time reversed, distance of the nose of the predator,
 **{α}**, and **{dα/dt}**. If *\code{simple_output = FALSE}* the output is a
 *\code{list()}* object given class *\code{attack_model}* containing the
 above data frame, plus inputs, subset regions (see next section), and data
 detailing how each alpha was calculated (interpolated profiles, locations of
 maximum apparent width at each frame, etc.). This option greatly increases
 the time the function takes to run.

@section Subset regions: *\code{alpha_range}* and *\code{dadt_range}* allow
 regions bounded by values of **{α}** and **{dα/dt}** to be subset. Both
 inputs are a vector of two numeric values indicating the lower and upper
 range of the desired **{α}** and **{dα/dt}** region. The function identifies
 the closest matching, first instance of these values in the *\code{$alpha}*
 and *\code{dadt}* columns of the model data frame. If *\code{simple_output =
 FALSE}*, these can be found as their own elements in the output
 *\code{list()}* object. If *\code{plot = TRUE}* these are also plotted as
 blue and green shaded regions, respectively.

@section Plotting: If *\code{plot = TRUE}* a plot is produced showing *speed*,
 **{α}** and **{dα/dt}**. The X range of the plot can set with
 *\code{plot_from}* and *\code{plot_to}*. Optional *\code{alpha_range}* and
 *\code{dadt_range}* can also be plotted (see above).

@section General: Models end when the nose (most anterior part in the
 profiles) of the predator reaches the prey (last row of the data.frame,
 where time and distance equal zero).

 At some point, the maximum girth of the predator will not make up the widest
 apparent visual angle of the predator, but more anterior segments will
 appear to the prey to be wider, and have a higher **{α}**. After
 interpolation of body profiles, filtering of apparent widths (see above),
 and identification of maximum apparent width at each iteration of the model,
 these final widths and their relative distances from the observing prey are
 used to calculate the viewing angle, **{α}**, and used to calculate the rate
 of change in **{α}**, **{dα/dt}** in radians/s.
