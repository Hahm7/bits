---
title: Necessity is the mother of NVIDIA
author: ''
date: '2025-04-25'
slug: []
categories: []
tags: []
hero: /images/nvidia_4.jpg
excerpt: ~
---

Strategic missteps, failed products, and a financial crisis. This was NVIDIA in 1996. Founded by Chris Malachowsky, Curtis Priem, and Jensen Huang in 1993 to create graphics chips for gaming, the company faced collapse just three years later, with only \$3 million left while burning through $330,000 a month. The competition was fierce, with many companies persuing the same goals in a market that demanded significant time and money to operate. How did they get past this to become the juggernauts they now are? Because their limitations sparked the most creative solution.


### 3D Graphics

The objects we see in 3D graphics are made up of polygons, which are like the tiles in a mosaic. Then there are primitives which are the basic tile shapes available to work with and hardware manufacturers like NVIDIA choose the type of primitive their chips will support while developers choose which type to create their 3D models with. The two most common types of primitives at that time were triangles and quadrilaterals. Because triangles have three points and sides, they always lie flat like a piece of paper. This makes them easy for computers to process, but it can also make 3D objects appear less realistic.




![](/images/final_fantasy.png "The stiff, blocky characters of Final Fantasy VII (1997) on the PS1.")

Quadrilaterals’ four points and sides enable smoother surfaces with the potential for better visual quality, but these additional points and sides make them trickier to work with, as the shape can become warped or twisted if one point is slightly out of place.

![](/images/triangles.gif "Triangle sides remain flat when points move.") ![](/images/quads.gif "Quadrilateral sides can exist in different planes which can create twisted shapes when points move.")

As the industry was transitioning from 2D to 3D graphics, there were no definitive standards set but there was a growing preference for the simplicity and reliability of triangles. NVIDIA’s first product, the NV1 chip, supported both triangle and quadrilateral rendering but it was optimised for the latter believing they would produce better looking graphics using quadratic shapes. There was also a deal with Sega who used a similar approach for their flagship console at the time, the Saturn. Aligning design with Sega’s needs could make it possible for computers equipped with NVIDIA’s chip to play some Saturn games. The NV1 chip was also positioned to handle other things a gamer might need like audio and controller support which was innovative for its time. While NVIDIA’s technical choices made sense in theory, the practical realities of the rapidly evolving graphics market revealed critical flaws with their approach.

### Growing Pains

To integrate multiple features into a chip, it was necessary to save on memory and NVIDIA used forward texture mapping for this. Think of texture mapping like applying wallpaper, where forward texture mapping is similar to rolling the wallpaper directly on to the wall and sticking it down as you go. For 3D graphics, it saves memory because it’s quick and simple but the pattern might look distorted in corners or curved areas. This method is in contrast to inverse texture mapping where you measure the wall first and cut the wallpaper precisely to fit each section. It requires more computing power but produces results without distortions. By cutting corners on graphics, the NV1 succeeded in saving memory, but it did not matter in the end since packing too many features into one chip increased the <a href="https://segaretro.org/NV1" target="_blank">costs</a>. The chip ended up becoming less attractive compared to cheaper alternatives that focused solely on the <a href="https://en.wikipedia.org/wiki/NV1" target="_blank">graphics</a>.

Beyond the cost issues, NVIDIA's design choices created additional problems. Optimising for quadrilaterals made it difficult for developers to create graphics for the NV1 as most 3D graphic tools used triangle based <a href="https://vintage3d.org/nv1.php" target="_blank">rendering</a>. The knockout blow came with Microsoft’s DirectX API. PCs rely on a variety of components, including graphics chips, and each combination of components requires unique instructions, which was a nightmare scenario for developers who had to code for every one of these combinations or restrict compatibility. DirectX became the universal translator where you just give it instructions and it will communicate with every hardware combination for you. The API quickly became the standard as Microsoft dominated the PC market with their Windows operating system. It damaged NVIDIA, as DirectX's triangle based rendering and inverse texture mapping initially made the NV1 incompatible. Though DirectX drivers were later developed for the NV1 and its successor the NV2, both remained slower than other chips due to their quadrilateral based rendering.

To create their third product, the RIVA128 chip, NVIDIA adopted triangle based rendering and inverse texture mapping for better DirectX compatibility on top of other improvements to <a href="https://86box.net/2025/02/25/riva128-part-1.html" target="_blank">performance</a>. However, making a great product with features highly desired by the market would not have been enough. NVIDIA needed to make this chip within what seemed like an impossible timeframe.

### Emulation

Developing a graphic chip takes about 2 years but the failure of their first two products meant NVIDIA had less than a year to make a new one. It was this intense time pressure that pushed the company to adopt emulation, a decision that proved to be a major competitive advantage. This involved testing and refining the chip design virtually without the need to build multiple physical prototypes, bypassing the lengthy back-and-forth with manufacturers. For NVIDIA’s engineers, emulation was a mind numbing process akin to watching paint dry while competitors who were not under the same time pressures dismissed it as a complex <a href="https://www.cgw.com/Press-Center/Web-Exclusives/2011/Inside-Nvidias-Emulation-Lab.aspx" target="_blank">approach</a>. Despite the risks, emulation helped bring the RIVA128 to market before the company went out of business and the product became a commercial <a href="https://www.computer.org/publications/tech-news/chasing-pixels/famous-graphics-chips-nvidias-riva128" target="_blank">success</a>. NVIDIA gained a competitive edge through this shortened development cycle but equally significant was their new workflow that made use of virtualised hardware. This fostered a culture of rapid experimentation and as emulation became a more pleasant process, NVIDIA’s engineers could refine designs with more complex features and discover innovations with unprecedented <a href="https://www.cascade.app/studies/how-nvidia-dominated-the-graphics-processing-space-with-its-strategy" target="_blank">efficiency</a>. They could proceed with near certainty that their products would be both innovative and functional, while competitors remained in limbo awaiting physical prototypes.

### Setbacks Into Strengths

NVIDIA’s origin demonstrates how constraints can fuel innovation rather than hinder it. Although early design choices resulted in failed products, they provided valuable lessons that revealed the true dynamics of the market. However, with their finances depleted, the traditional development method was no longer an option to apply what they had learned to their next product. NVIDIA turned to emulation not because they saw it as a revolutionary approach, but because survival demanded it. This desperate necessity became their defining advantage. 

NVIDIA's journey reminds us that sometimes our significant breakthroughs don't come when we have abundant resources and endless options, but when limitations force us to reimagine the impossible and take risks more comfortable competitors avoid. Their transformation from struggling startup to industry titan wasn't despite those early constraints. It was because of them.
