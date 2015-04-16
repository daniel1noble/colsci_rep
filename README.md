# Reproducible research in the study of biological coloration

Thomas E. White^1,7^, Rhiannon L. Dalrymple^2,7^, Daniel W. A. Noble^1,2,7^, James C. O’Hanlon ^1,7^, Daniel B. Zurek^3,7^, Kate D. L. Umbers^4,5,6,7^

^1^ Department of Biological Sciences, Macquarie University, North Ryde, NSW, Australia, 2109 ^2^ Evolution & Ecology Research Centre, School of Biological, Earth and Environmental Sciences, University of New South Wales, Kensington, NSW, Australia, 2022 ^3^ Department of Biological Sciences, University of Pittsburgh, Pittsburgh, PA, USA, 15260 ^4^ School of Biological Sciences, University of Wollongong, Wollongong, NSW, Austrlaia 2252 ^5^ Centre for Evolutionary Biology, University of Western Australia, Crawley, WA, Australia 6008 ^6^ School of Science and Health, University of Western Sydney Hawkesbury, NSW, Australia 2751

### Introduction
This repository contains data collected from the literature to assess to quality of reporting in signal-based colour studies of animal and plant taxa. Our literature search includes studies collected in 2013 from research journals likley to do colour signal research. We also detail important methodological deatils that ideally should be reported in studies that quantify colour using both spectrophotometry and camera equipment along with important analytical parameters used for visual modelling. 

### Executing code and compiling the manuscript

To compile ms, knit to ms.md then in terminal run:

pandoc ms.md -o ms.pdf --bibliography references.bib --csl ref_style.csl -H ms.sty