#Colour Reporting paper

##Authors: Daniel Noble, Tom White, Kate Umbers, James O'Hanlon, Rhiannon Dalrymple, Dan Zurek

This repository contains data collected from the literature to assess to quality of reporting in signal-based colour studies of animal and plant taxa. Our literature search includes studies collected in 2013 from research journals likley to do colour signal research.

To compile ms, knit to ms.md then in terminal run:

pandoc ms.md -o ms.pdf --bibliography references.bib --csl ref_style.csl -H ms.sty