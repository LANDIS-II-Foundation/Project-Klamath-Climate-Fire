# Project-Klamath-Climate

This repository includes inputs to LANDIS-II for the Klamath project, funded by NSF.

# Modelling Details

This research utilized the following extensions: Net Ecosystem Carbon and Nitrogen (NECN) Succession (v4.2.1), Biomass Harvest (v3.2), Dynamic Fuels and Fire (v2.2.1), Dynamic Biomass Fuels (v4.0), Biomass Output (v2.2), Cohort Statistics Output (v2.2), and Biomass Reclassification Output (v2.2). Original model parameterization for the fire and succession extensions were performed by Serra-Diaz et al. (2018).

There were six management scenarios developed at two workshops held in Yreka, CA: 1) Business-as-usual (BAU), 2) Climate change adaptation (CCA), 3) Let-it-burn (LIB), 4) Privatization (PRIV), 5) Eco-restoration (Rx), and 6) Strategic fuels treatments (SFT). The BAU strategy reflects generalized average management activities for the years 2010-2014 for this landscape. Activities on USFS lands were based on data derived from the USFS FACTs database, and for BLM lands, data available through the BLM Medford office. Activities on private lands were estimated from county level timber receipts. CCA focused on adapting to climate change by a variety of means, including promoting oak/pine habitat, using fuels treatments, protecting legacy trees, and allowing fire-use fires. The LIB strategy essentially stops management activity on federal lands, where there would be no harvesting or fire suppression. The PRIV scenario imagined a hypothetical scenario where federal lands holdings were transferred to the public and that the forests were intensively managed for timber. Rx, as a strategy, uses prescribed fire and fire-use fire as the main forms of restoration. SFT uses roads and ridges fuel treatments, combined with fire-use fire, to change the forest.

LANDIS-II utilizes climate data from the USGS (https://cida.usgs.gov/gdp/), and there were four climate projections from three different emissions pathways used in future projections: (1) ACCESS GCM from RCP 8.5 climate change scenario (hotter and drier than historical climate), (2) CanESM2 from RCP 8.5 (hotter and wetter), (3) CNRM from RCP 4.5 (warmer and wetter), and (4) MIROC5 from RCP 2.6 (warmer and drier). As a baseline, a contemporary climate (1950-2010) was also used to generate data projections.

# Model Inputs

13 tree species, and 7 shrub types were modeled. Specifics can be found in Species.txt.
Slope and upaspect rasters derived from 30m NED.
Initial communities derived from GNN data available from LEMMA (https://lemma.forestry.oregonstate.edu/data).

# Model Outputs

As currently configured, the outputs include:

* ANPP and NEE estimates at 10-year intervals
* Fire outputs: fuel types, time since last fire, severity, percent conifer/fir
* Harvest outputs: amount and prescription
* Annual water budget at 10-year intervals
* Biomass by species at 10-year intervals
* Forest cover type
* Age distributions by species at 20-year intervals

# Other Details

Simulations were run on a 7.29 ha grid (270 m cell side) over 85 years (2015-2100).
Model input rasters were projected in Albers Equal Area, but model outputs have no projection information.
Average simulation replicate run time is ~28 hours. Each run requires 1 cpu core, 8 GB of memory, and 3 GB of storage.
To capture stochasticity of disturbance, each simulation and climate projection was replicated 10 times.

# Point of Contact

Point of contact: Charles Maxwell, cjmaxwe3@ncsu.edu, Department of Forestry and Environmental Resources, North Carolina State University

# Use Limitations

There are no planned updates for this time. The data and product accuracy may vary due to compilation from various sources. This information may be updated, corrected, or otherwise modified without notification.
