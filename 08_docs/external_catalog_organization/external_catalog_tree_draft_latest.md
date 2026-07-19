# BRIM External Layers organization draft

Generated: 2026-06-10 10:52:22

This draft combines current External catalog rows with random-adds audit candidates when the EL_001 ranked-plan CSV is available.
Edit `external_group`, `external_group_order`, `external_subgroup`, and `external_subgroup_order` in `00_config/external_service_catalog.csv` to reorganize the live External Layers panel.

## Water / groundwater / water quality (44)

### Groundwater levels (8)
- [current] DWR Groundwater Depth Seasonal Contours
- [current] DWR Recently Measured Groundwater Levels
- [current] DWR Seasonal Groundwater Depth Contours
- [current] DWR Seasonal Groundwater Depth Points
- [current] DWR Seasonal Groundwater Elevation Contours
- [current] DWR Seasonal Groundwater Elevation Points
- [current] DWR Seasonal Groundwater Level Change Contours
- [current] DWR Seasonal Groundwater Level Change Points


### Groundwater / SGMA (5)
- [current] DWR Adjudicated Groundwater Areas
- [current] DWR Critically Overdrafted Basins
- [current] DWR Groundwater Sustainability Agencies
- [current] DWR Groundwater Sustainability Plan Areas
- [current] DWR SGMA 2019 Basin Prioritization


### Delta / Bay-Delta (3)
- [current] DWR Delta Primary/Secondary Zones
- [current] DWR Legal Delta Boundary
- [current] DWR Suisun Marsh Boundary


### Groundwater basins (1)
- [current] DWR Bulletin 118 Groundwater Basins


### Groundwater monitoring (2)
- [current] DWR Groundwater Stations — Enterprise Water Management
- [current] DWR Groundwater Stations — inspectable features


### Water quality / monitoring (2)
- [current] DWR Continuous Monitoring Stations — Hydstra Period
- [current] DWR Discrete Grab Water Quality Stations


### Groundwater models / C2VSim (8)
- [current] DWR C2VSimFG Boundary
- [current] DWR C2VSimFG Elements
- [current] DWR C2VSimFG Groundwater Head Observations
- [current] DWR C2VSimFG Nodes
- [current] DWR C2VSimFG Stream Observations
- [current] DWR C2VSimFG Stream Reaches
- [current] DWR C2VSimFG Subregions
- [current] DWR C2VSimFG Subsidence Observations


### Wells / well completion reports (2)
- [current] DWR Well Completion Reports Index
- [current] DWR Well Completion Reports Index — inspectable features


### Water rights / PODs (1)
- [current] SWRCB Points of Diversion


### Water quality / impaired waters (2)
- [current] SWRCB Impaired Waters / Integrated Report
- [candidate] 303(d) / 305(b) impaired waters / TMDL context — `IMPAIRED_WATERS`


### Groundwater quality / drinking water (1)
- [current] Water Boards 2026 Aquifer Risk Map — Water Quality Risk


### Water service areas (1)
- [current] Water Boards Drought Water Service Areas


### Streamflow / observed gages (1)
- [current] Live Stream Gages / Flow


### Streamflow / modeled flow (1)
- [current] NWM Streamflow Analysis (cfs)


### GDE / groundwater ecology (1)
- [candidate] California NCCAG / groundwater-dependent ecosystems / iGDE — `CA_GDE_NCCAG`


### Soil moisture (5)
- [candidate] CPC NLDAS Noah soil moisture percentiles — `CPC_NLDAS_SOIL`
- [candidate] Crop-CASMA soil moisture anomaly — `CROP_CASMA`
- [candidate] NASA SPoRT-LIS soil moisture percentiles — `SPORT_LIS`
- [candidate] NOAA National Water Model Land Analysis near-surface soil moisture saturation — `NWM_SOIL_MOISTURE`
- [candidate] SMAP / GRACE broad soil moisture products — `SMAP_GRACE`

## Water operations / flood / coastal (11)

### Flood management (3)
- [current] DWR Local Maintenance Areas — Flood Protection
- [current] DWR SPFC Planning Area
- [current] DWR Systemwide Planning Area


### Flood management / levees (1)
- [current] DWR Historic Levee Breaks


### Flood hazards (1)
- [current] FEMA NFHL Flood Hazard Zones


### Sea level rise / coastal flooding (6)
- [current] CNRA CSMW Potential Sea-Level-Rise Impacts
- [current] NOAA Potential Marsh Distribution — 6.5 ft
- [current] NOAA Sea Level Rise Inundation — 1.5 ft
- [current] NOAA Sea Level Rise Inundation — 2 ft
- [current] NOAA Sea Level Rise Inundation — 3 ft
- [current] NOAA Sea Level Rise Inundation — 6 ft

## Weather / snow / drought (22)

### Drought / water shortage vulnerability (3)
- [current] DWR Water Shortage Social Vulnerability — Block Groups
- [current] DWR Water Shortage Vulnerability — Sections
- [current] DWR Water Shortage Vulnerability — Small Water Systems


### Weather operations / WFOs (1)
- [current] NWS WFOs


### Climate monitoring / drought (1)
- [current] U.S. Drought Monitor (current)


### Climate outlooks / CPC (4)
- [current] CPC 6-10 Day Precipitation Outlook
- [current] CPC 6-10 Day Temperature Outlook
- [current] CPC 8-14 Day Precipitation Outlook
- [current] CPC 8-14 Day Temperature Outlook


### Climate outlook (2)
- [candidate] CPC monthly / seasonal outlooks — `CPC_MONTH_SEASON`
- [candidate] CPC Week 3–4 outlooks — `CPC_WEEK34`


### Drought / vegetation (6)
- [candidate] CAL FIRE / FRAP vegetation, fuels, burn severity — `FRAP_VEG_FIRE`
- [candidate] NASA GIBS MODIS/VIIRS NDVI-style imagery — `NASA_GIBS_NDVI`
- [candidate] NOAA STAR NDVI / greenness products — `NOAA_STAR_NDVI`
- [candidate] NOAA STAR VHI / VCI / TCI vegetation health — `NOAA_STAR_VHI`
- [candidate] QuickDRI — `QUICKDRI`
- [candidate] VegDRI — `VEGDRI`


### Hazards / week-2 outlooks (1)
- [candidate] CPC Day 8–14 hazards / rapid-onset drought / week-2 hazards — `CPC_HAZARDS`


### Snow analysis (1)
- [candidate] NOHRSC snow analysis — snow depth and SWE — `NOHRSC_SNOW`


### Wind (2)
- [candidate] Better wind — NOAA surface observations / MADIS wind barbs — `BETTER_WIND_OBS`
- [candidate] NDFD / gridded forecast wind — `NDFD_WIND`


### Winter weather (1)
- [candidate] WPC Winter Storm Severity Index — `WPC_WSSI`

## Fire / hazards / emergency (8)

### Fire / cameras (2)
- [current] ALERTCalifornia Camera Viewsheds
- [current] ALERTCalifornia Cameras


### Fire / burn scars (2)
- [current] CAL FIRE Fire Perimeters — All
- [current] CAL FIRE Recent Large Fire Perimeters


### Fire / active incidents (1)
- [current] NIFC Current Wildfire Perimeters


### Convective outlooks (1)
- [candidate] SPC convective outlooks — `SPC_CONVECTIVE`


### Fire weather outlooks (2)
- [candidate] SPC fire weather outlooks — `SPC_FIREWX`
- [candidate] SPC mesoscale discussions — `SPC_MESO_DISC`

## Ecology / habitat / species (20)

### ACE / terrestrial biodiversity (1)
- [current] CDFW ACE Statewide Terrestrial Biodiversity Summary


### ACE / species biodiversity (1)
- [current] CDFW ACE Species Biodiversity


### ACE / rare species (1)
- [current] CDFW ACE Terrestrial Rare Species Richness


### ACE / irreplaceability (1)
- [current] CDFW ACE Terrestrial Irreplaceability Summary


### ACE / aquatic biodiversity (3)
- [current] CDFW ACE Aquatic Amphibian Irreplaceability
- [current] CDFW ACE Aquatic Native Amphibian Richness
- [current] CDFW ACE Aquatic Native Species Richness Summary


### ACE / wetlands and riparian (3)
- [current] CDFW ACE Estuary and Tidal Habitat
- [current] CDFW ACE Freshwater Wetlands by Watershed
- [current] CDFW ACE Riparian Habitat by Watershed


### ACE / vegetation and habitat (2)
- [current] CDFW ACE Oak Shrublands
- [current] CDFW ACE Oak Woodlands


### ACE / species habitat (1)
- [current] CDFW ACE Monarch Overwintering Areas


### ACE / climate vulnerability (1)
- [current] CDFW ACE Terrestrial Climate Vulnerable Species


### ACE / game species (1)
- [current] CDFW ACE Terrestrial Native Game Species


### ACE / birds (1)
- [current] CDFW ACE Terrestrial Rare Bird Richness


### Monitoring / riparian / aquatic (1)
- [current] BLM AIM Lotic Indicators


### Monitoring / species indicators (1)
- [current] BLM AIM Terrestrial Species Indicators


### Threatened / endangered species (1)
- [current] USFWS Final Critical Habitat


### Wetlands / riparian (1)
- [current] USFWS National Wetlands Inventory

## BLM / land status / monitoring (6)

### Monitoring / terrestrial (2)
- [current] BLM AIM LMF Hub
- [current] BLM AIM TerrADat


### Federal lands / land status (1)
- [current] BLM CA Land Status / Surface Management Agency


### Cadastral / PLSS (2)
- [current] BLM-CA PLSS Aliquots / Sections
- [current] BLM National PLSS / CadNSDI


### BLM recreation / access (1)
- [candidate] BLM CA recreation sites / recreation areas / OHV — `BLM_RECREATION`

## Mining / energy / infrastructure (13)

### Water infrastructure / regulation (1)
- [current] DWR FERC Project Boundaries


### Geology / infrastructure (1)
- [current] DWR Fault Crossings — State Water Project


### Conveyance / infrastructure (1)
- [current] DWR Local Canals and Aqueducts


### Water operations / CalSim (1)
- [current] DWR CalSim II Inflow Data Locations


### Energy / transmission / planning (4)
- [candidate] BLM CA renewable energy projects — `BLM_RENEWABLE_PROJECTS`
- [candidate] DRECP / renewable energy zones / conservation designations — `DRECP`
- [candidate] Electric substations — `SUBSTATIONS`
- [candidate] Electric transmission lines — `TRANSMISSION_LINES`


### Mining / abandoned mines (3)
- [candidate] Active mines / SMARA / DOC mine status — `ACTIVE_MINES_SMARA`
- [candidate] CA mapped abandoned mine features — `AML_CA_FEATURES`
- [candidate] Principal Areas of Mine Pollution / mine pollution areas — `AML_PAMP`


### Oil / gas / geothermal (1)
- [candidate] CalGEM oil/gas/geothermal wells, fields, leases, notices, permits — `CALGEM_WELLS`


### Subsidence / infrastructure risk (1)
- [candidate] DWR / TRE Altamira InSAR land subsidence — `INSAR_SUBSIDENCE`

## Transportation / access (2)

### Highways (1)
- [candidate] Major highways / National Highway System — `HIGHWAYS_NHS`


### Railroads (1)
- [candidate] Railroads — `RAILROADS`

## Administrative / boundaries / planning (8)

### Hydrologic boundaries (1)
- [current] DWR Hydrologic Regions


### Planning / water management (3)
- [current] DWR IRWM Regions
- [current] DWR Proposition 1 Funding Areas
- [current] DWR Water Plan Planning Areas


### Water agencies / districts (2)
- [current] DWR Water Districts
- [current] DWR Water Districts — inspectable features


### Administrative / RWQCB (1)
- [current] RWQCB Boundaries — Water Boards


### Tribal lands / BIA (1)
- [current] BIA Tribal Lands / AIAN-LAR

## Explore / experimental (2)

### Space weather links (2)
- [candidate] SWPC aurora 30-minute forecast — `SWPC_AURORA`
- [candidate] SWPC space-weather alerts, watches, warnings, K-index — `SWPC_SPACE`

