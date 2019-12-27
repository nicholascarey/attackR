# `attackR`
An `R` package for modelling the visual aspects of an attack by a predator on a prey.

### Summary
This package can be used to model how an attacking predator appears to a prey, given its size, speed, and shape. It was used in the following publication in PNAS to determine how much of a prey's visual field is taken up by an attacking humpback whale. It allows for varying speed and sizes of the whale, and there is specific functionality to determine how apparent size is affected by the timing of the mouth opening during a lunge, though this can be ignored for other predators.

**David E. Cade, Nicholas Carey, Paolo Domenici, Jean Potvin, Jeremy A. Goldbogen, 2019**. Predator-informed looming stimulus experiments reveal how large filter feeding whales capture highly maneuverable forage fish. *Proceedings of the National Academy of Sciences*. https://doi.org/10.1073/pnas.1911099116

![](docs/images/fig1.png?raw=true "Fig1")

While the code was applied specifically to humpback whales for this study, given some simple morphological parameters it can be used to model how *any* attacking predator appears to a prey. It can also be applied to other filter-feeding whales which engulf large volumes of water, such as blue whales, given some inputs regarding mouth opening timings. The package takes full account of the predator's three-dimensional shape when determining its perceived size in the prey's visual field. This is because at close distances, the maximum girth of the predator will not necessarily make up the widest apparent visual angle. Instead  more anterior segments will appear to the prey to be wider, and have a higher apparent size. 

From the prey's perspective the package calculates the visual angle of the attacker (**α**) in radians, and the rate of change of this angle (**dα/dt** in radians/s), as well as distance and time to capture.

See the help documentation within the package for more information. 

Stay tuned for a vignette showing how to use the package to model attacks by both whales and other predators, and how to turn these into looming animations. 

