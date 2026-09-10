#!/usr/bin/env node
'use strict';

// No-install model and small DOM harness. This is not mounted-browser evidence.
// The existing suite supplies its public projection and preserves its full assertions.
const { createModel, resources: fixtureResources, registry, relationships, extractFunction } =
  require('./test_guide_resource_explorer.js');
const fs = require('fs');
const path = require('path');
const assert = require('assert');
const crypto = require('crypto');
const rootPath = path.resolve(__dirname, '..');
const source = fs.readFileSync(path.join(rootPath, '03_functions/js/leaflet_brim_guide.js'), 'utf8');
const css = fs.readFileSync(path.join(rootPath, '03_functions/css/leaflet_brim_guide.css'), 'utf8');
const bundle = process.env.BRIM_PROVIDER_TEST_BUNDLE
  ? JSON.parse(fs.readFileSync(process.env.BRIM_PROVIDER_TEST_BUNDLE, 'utf8'))
  : { resources: fixtureResources };
const before = JSON.stringify({ bundle, registry, relationships });
const resources = bundle.resources;
const model = createModel(resources);
const policy = model.providerPolicy();
assert.deepStrictEqual(policy.groups, [
  { id: 'federal', label: 'Federal' }, { id: 'state', label: 'State' },
  { id: 'types', label: 'Provider types' }
]);
// Independent approved roster: never generated from the implementation or catalog counts.
const expectedRoster = [
  ['blm', 'BLM', 'federal'], ['epa', 'EPA', 'federal'], ['fema', 'FEMA', 'federal'],
  ['nasa', 'NASA', 'federal'], ['noaa', 'NOAA', 'federal'], ['usace', 'USACE', 'federal'],
  ['usbr', 'USBR', 'federal'], ['usda', 'USDA', 'federal'], ['usgs', 'USGS', 'federal'],
  ['dwr', 'DWR', 'state'], ['waterboards', 'Water Boards', 'state'],
  ['county','County','types'],['nonprofits','Nonprofits / NGOs','types'],
  ['other','Other','types'],['private','Private','types'],['regional_local','Regional/local','types']
];
const expectedLabels = Object.fromEntries(expectedRoster.map(([id, label]) => [id, label]));
assert.deepStrictEqual(policy.families.map(f => [f.id, f.label, f.group]), expectedRoster);
assert.deepStrictEqual(policy.provider_roles, ['display_provider', 'publisher', 'maintainer', 'partner']);
const allFamilies = policy.families.concat(policy.legacy_families);
assert.equal(new Set(allFamilies.map(f => f.id)).size, 29);
assert.deepStrictEqual(policy.groups.map(g => policy.families.filter(f => f.group === g.id).length), [9, 2, 5]);
assert.deepStrictEqual(policy.legacy_families.map(f => f.id).sort(),
  ['aso','calfire','caloes','cdfa','climateengine','cvfpb','cw3e','nifc','nsidc','pivotal','prism','synoptic','windy']);
const initial = model.createState();
const initialIds = model.results(initial).map(r => r.id);
assert.equal(initialIds.length, 228);
assert.equal(new Set(initialIds).size, 228);
assert.deepStrictEqual(new Set(initialIds), new Set(resources.map(r => r.id)));
assert.deepStrictEqual(model.facetCounts(initial).presets,
  { in_brim_map: 28, beyond_the_map: 200, all_resources: 228 });
assert.deepStrictEqual(initial.providers, []);
assert(!('providerGroupsOpen' in initial));
assert(!('narrow' in initial));
assert.deepStrictEqual(model.createState({ providerGroupsOpen: ['other'], narrow: true }), initial);
const expectedCounts = { blm: 3, usbr: 14, epa: 4, fema: 2, nasa: 19, nifc: 1,
  noaa: 24, usace: 4, usda: 10, usgs: 15, calfire: 1, caloes: 1, cdfa: 1,
  cvfpb: 1, dwr: 14, waterboards: 5, aso: 1, cw3e: 3, climateengine: 1,
  nsidc: 3, pivotal: 2, prism: 1, synoptic: 3, windy: 1, county:13, nonprofits:5, other:60, private:11, regional_local:27 };
assert.deepStrictEqual(model.facetCounts(initial).providers,
  Object.fromEntries(Object.entries(expectedCounts).map(([id, count]) => ['family:' + id, count])));

// Independent full ID sets from the approved sealed proposal plus exact L082 overlay.
const expectedProviderSets = {
  "blm": [
    "resource_blm_california",
    "resource_blm_california_wildfire_dashboard_public",
    "resource_blm_maps_and_geospatial_data"
  ],
  "epa": [
    "resource_epa_cyanweb",
    "resource_epa_epa_echo_platform",
    "resource_epa_epa_hows_my_waterway_platform",
    "resource_epa_epa_waters_geoviewer_viewer"
  ],
  "fema": [
    "resource_fema_fema_flood_map_service_center_platform",
    "resource_geospatial_and_remote_sensing_data_provi_fema_national_flood_hazard_layer_viewer_viewer"
  ],
  "nasa": [
    "resource_nasa_asf_displacement_portal",
    "resource_nasa_cyfi_explorer",
    "resource_nasa_ecostress_data_resources",
    "resource_nasa_firms_global_fire_map",
    "resource_nasa_grace_analysis_tool",
    "resource_nasa_grace_data",
    "resource_nasa_grace_groundwater_soil_moisture",
    "resource_nasa_grace_tellus",
    "resource_nasa_nasa_grace_interactive_browsers_and_data_access_collection",
    "resource_nasa_nasa_veda_earth_information_dashboard_platform",
    "resource_nasa_nldas_drought_monitor",
    "resource_nasa_opera_products",
    "resource_nasa_smap_data",
    "resource_nasa_smap_l3_enhanced_soil_moisture",
    "resource_nasa_smap_mission",
    "resource_nasa_stream_water_quality_tool",
    "resource_nasa_swot_hydrology_resources",
    "resource_nasa_worldview",
    "resource_usda_crop_casma"
  ],
  "noaa": [
    "resource_cira_slider",
    "resource_climate_and_drought_data_providers_drought_gov_california_dashboard",
    "resource_ncsmmn_network_map",
    "resource_ncsmmn_soil_moisture_portal",
    "resource_nidis_grace_groundwater_soil_moisture",
    "resource_nidis_soil_moisture_dashboard",
    "resource_nidis_soil_moisture_resources",
    "resource_noaa_cnrfc",
    "resource_noaa_coastwatch_data_portal",
    "resource_noaa_coastwatch_erddap",
    "resource_noaa_cpc_forecasts_outlooks",
    "resource_noaa_cpc_monthly_global_soil_moisture_product",
    "resource_noaa_cpc_soil_moisture",
    "resource_noaa_goes_image_viewer",
    "resource_noaa_noaa_coastwatch_data_access_tools_collection",
    "resource_noaa_noaa_sea_level_rise_viewer_viewer",
    "resource_noaa_nwps",
    "resource_noaa_nwps_api_service",
    "resource_noaa_nws_graphical_forecasts",
    "resource_noaa_smops",
    "resource_noaa_smops_maps",
    "resource_noaa_vegetation_health",
    "resource_noaa_wpc_excessive_rainfall_outlook",
    "resource_noaa_wpc_qpf"
  ],
  "usace": [
    "resource_usace_cwms_data_api",
    "resource_usace_los_angeles_district_water_management_platform",
    "resource_usace_sacramento_district_water_control_data_system",
    "resource_usace_usace_water_management_data_platform"
  ],
  "usbr": [
    "resource_usbr",
    "resource_usbr_central_valley_operations_office_platform",
    "resource_usbr_central_valley_project_water_supply_program",
    "resource_usbr_colorado_river_basin_hub",
    "resource_usbr_cvp_long_term_operations_program",
    "resource_usbr_klamath_project_water_operations_platform",
    "resource_usbr_lower_colorado_river_operations",
    "resource_usbr_reclamation_agrimet_platform",
    "resource_usbr_reclamation_hydromet_platform",
    "resource_usbr_reclamation_information_sharing_environment_platform",
    "resource_usbr_reclamation_watersmart_program",
    "resource_usbr_stanislaus_watershed_team",
    "resource_usbr_truckee_river_operating_agreement_platform",
    "resource_usbr_upper_colorado_basin_water_operations"
  ],
  "usda": [
    "resource_nrcs_nrcs_snotel_historic_data_other",
    "resource_nrcs_nrcs_snotel_update_report_selector_product",
    "resource_nrcs_nwcc",
    "resource_nrcs_scan",
    "resource_nrcs_snow_survey_water_supply_forecasting",
    "resource_nrcs_soil_data_access_platform",
    "resource_usda_crop_casma",
    "resource_usda_nass_usda_crop_casma_metadata_and_documentation_documentation",
    "resource_usda_usda_ag_data_commons_isnobal_other",
    "resource_usda_vegscape"
  ],
  "usgs": [
    "resource_california_natural_flows",
    "resource_usgs_bcmv8",
    "resource_usgs_groundwater_watch",
    "resource_usgs_national_hydrography_products",
    "resource_usgs_quickdri",
    "resource_usgs_streamstats",
    "resource_usgs_usgs_3d_elevation_program_program",
    "resource_usgs_usgs_california_river_basin_schematics_collection",
    "resource_usgs_usgs_site_inventory_service_other",
    "resource_usgs_usgs_the_national_map_downloader_other",
    "resource_usgs_usgs_water_services_apis_service",
    "resource_usgs_vegdri",
    "resource_usgs_water_dashboard",
    "resource_usgs_water_data_nation",
    "resource_usgs_water_quality_portal"
  ],
  "dwr": [
    "resource_dwr_bulletin118_sgma_2019",
    "resource_dwr_california_groundwater_live",
    "resource_dwr_california_water_watch",
    "resource_dwr_casgem",
    "resource_dwr_cdec",
    "resource_dwr_cdec_station_metadata_other",
    "resource_dwr_cdec_station_search_other",
    "resource_dwr_cimis",
    "resource_dwr_dwr_land_use_viewer_viewer",
    "resource_dwr_groundwater_sustainability_agencies",
    "resource_dwr_sgma_data_viewer_viewer",
    "resource_dwr_sgma_water_year_type_dataset",
    "resource_dwr_snowtrax_platform",
    "resource_dwr_water_data_library"
  ],
  "waterboards": [
    "resource_swrcb_california_environmental_data_exchange_network_other",
    "resource_swrcb_impaired_waters_and_tmdls_program",
    "resource_swrcb_safer_dashboard_dashboard",
    "resource_swrcb_state_water_board_division_of_water_rights_program",
    "resource_water_quality_and_ecosystem_data_provide_safe_to_swim_map_viewer"
  ],
  "county": [
    "resource_lacpw_los_angeles_county_precipitation_data_platform",
    "resource_marin_county_flood_control_marin_county_rainfall_and_creek_data_dashboards_collection",
    "resource_napa_county_flood_control_napa_valley_rainfall_and_stream_monitoring_map_viewer",
    "resource_ocpw_orange_county_hydrology_data_portal_platform",
    "resource_ocpw_orange_county_monitoring_and_data_collection",
    "resource_san_joaquin_county_san_joaquin_county_hydrometeorological_data_map_viewer",
    "resource_santa_barbara_county_public_works_santa_barbara_county_hydrology_section_collection",
    "resource_santa_barbara_county_public_works_santa_barbara_county_real_time_hydrology_platform",
    "resource_santa_cruz_county_flood_control_santa_cruz_county_hydrologic_monitoring_map_viewer",
    "resource_sonoma_water_russian_river_operating_conditions",
    "resource_sonoma_water_sonoma_county_sonoma_county_alert2_dashboards_collection",
    "resource_sonoma_water_sonoma_water_current_water_supply_levels_dashboard",
    "resource_sonoma_water_sonoma_water_reservoir_projections_dashboard"
  ],
  "nonprofits": [
    "resource_agricultural_water_and_evapotranspiratio_openet_data_explorer_viewer",
    "resource_agricultural_water_and_evapotranspiratio_openet_platform",
    "resource_california_natural_flows",
    "resource_ccvfca_california_central_valley_flood_control_association_membership_collection",
    "resource_cocorahs_cocorahs_other"
  ],
  "private": [
    "resource_aso_airborne_snow_observatories",
    "resource_brightband_operational_weatherbench",
    "resource_google_deepmind_weather_lab",
    "resource_meteologix_meteologix_model_charts_platform",
    "resource_pivotal_weather_pivotal_weather_model_maps_platform",
    "resource_pivotal_weather_pivotal_weather_soundings_product",
    "resource_sce_big_creek_project",
    "resource_sce_flow_and_reservoir_portal",
    "resource_synoptic_data_mesowest_california_surface_weather_map_viewer",
    "resource_synoptic_data_synoptic_data_viewer_viewer",
    "resource_synoptic_data_synoptic_weather_api_platform"
  ],
  "regional_local": [
    "resource_bafpaa_bay_area_flood_protection_agencies_association_platform",
    "resource_city_of_san_diego_public_utilities_depar_city_of_san_diego_reservoir_water_levels_other",
    "resource_city_of_stockton_city_of_stockton_reclamation_district_directory_collection",
    "resource_ebmud_east_bay_mud_daily_water_supply_report_product",
    "resource_ebmud_east_bay_mud_water_supply_reports_dashboard",
    "resource_eid_eid_project_184_portal_platform",
    "resource_eid_el_dorado_irrigation_district_project_184_water_data_platform",
    "resource_klrdd_knights_landing_ridge_drainage_district_other",
    "resource_mid_modesto_irrigation_district_weather_other",
    "resource_mwa_mojave_basin_area_watermaster_platform",
    "resource_nid_nevada_irrigation_district_river_and_reservoir_data_platform",
    "resource_nid_nevada_irrigation_district_telemetry_map_platform",
    "resource_pcwa_placer_county_water_agency_american_river_flows_other",
    "resource_rd_17_reclamation_district_17_mossdale_other",
    "resource_rd_2039_reclamation_district_2039_jones_tract_other",
    "resource_rd_830_reclamation_district_830_other",
    "resource_scvwd_valley_water_surface_water_data_portal_platform",
    "resource_scwa_solano_county_flood_monitoring_map_viewer",
    "resource_sdcwa_san_diego_county_water_authority_surface_water_other",
    "resource_sfpuc_san_francisco_public_utilities_commission_platform",
    "resource_smud_upper_american_river_project_conditions",
    "resource_srwsld_sacramento_river_west_side_levee_district_other",
    "resource_tid_turlock_irrigation_district_wiski_web_platform",
    "resource_wrd_water_replenishment_district_watermaster_platform",
    "resource_wwd_westlands_water_management_plan_platform",
    "resource_ywa_yuba_river_conditions_other",
    "resource_zone_7_zone_7_water_agency_platform"
  ],
  "other": [
    "resource_cal_oes_california_flood_preparedness_program",
    "resource_calfire_fire_perimeters",
    "resource_california_and_western_snow_research_org_global_cryosphere_watch_snow_dataset_inventory_collection",
    "resource_california_environmental_flows_framework",
    "resource_california_local_water_and_flood_agencie_middle_fork_american_river_conditions_platform",
    "resource_california_water_rights_and_watermaster_california_voluntary_agreements_platform",
    "resource_cdfa_california_department_of_food_and_agriculture_frep_program",
    "resource_chino_basin_watermaster_chino_basin_watermaster_platform",
    "resource_climate_and_drought_data_providers_climate_toolbox_platform",
    "resource_climate_and_drought_data_providers_gridmet_dataset",
    "resource_climate_and_drought_data_providers_terraclimate_dataset",
    "resource_climate_engine",
    "resource_clms_copernicus_daily_soil_water_index_europe_1_km_dataset",
    "resource_clms_copernicus_daily_surface_soil_moisture_europe_1_km_dataset",
    "resource_contra_costa_county_flood_control_and_wa_contra_costa_county_rainmap_viewer",
    "resource_cssl_uc_berkeley_central_sierra_snow_lab_other",
    "resource_cvfpb_central_valley_flood_protection_board_platform",
    "resource_cw3e_cw3e_california_weather_and_water_portal_viewer",
    "resource_cw3e_cw3e_micro_rain_radar_snow_levels_dashboard",
    "resource_cw3e_cw3e_west_wrf_accumulated_snow_product",
    "resource_doi",
    "resource_esa_esa_climate_change_initiative_soil_moisture_program",
    "resource_flood_and_levee_programs_california_coastal_commission_sea_level_rise_program",
    "resource_geolibre",
    "resource_geospatial_and_remote_sensing_data_provi_copernicus_browser_viewer",
    "resource_geospatial_and_remote_sensing_data_provi_opentopography_platform",
    "resource_h_saf_eumetsat_h_saf_soil_moisture_products_collection",
    "resource_inciweb_incident_information",
    "resource_ismn",
    "resource_itrc_cal_poly_irrigation_training_and_research_center_platform",
    "resource_krwa_kings_river_water_association_platform",
    "resource_main_san_gabriel_basin_watermaster_main_san_gabriel_basin_watermaster_platform",
    "resource_mcwra_monterey_county_water_resources_agency_other",
    "resource_nifc_public_fire_information",
    "resource_nifc_wfigs_current",
    "resource_northern_california_fire_coordination",
    "resource_nsidc_nsidc_airborne_snow_observatory_data_other",
    "resource_nsidc_nsidc_snow_today_platform",
    "resource_polarwx_tropical",
    "resource_prism_normals",
    "resource_rcfcwcd_riverside_county_rainfall_map_viewer",
    "resource_sacramento_water_forum_sacramento_water_forum_platform",
    "resource_sdcfcd_san_diego_county_flood_control_data_collection",
    "resource_six_basins_watermaster_six_basins_watermaster_platform",
    "resource_sjrrp_friant_releases_and_allocations",
    "resource_southern_california_fire_coordination",
    "resource_troa_truckee_river_daily_watermaster_report_product",
    "resource_troa_truckee_river_operating_agreement_platform",
    "resource_trrp_flows_and_releases",
    "resource_tu_wien_soil_moisture_viewer",
    "resource_ulara_upper_los_angeles_river_area_watermaster_platform",
    "resource_uswfs_public_information",
    "resource_ventura_county_watershed_protection_dist_ventura_county_flood_warning_system_platform",
    "resource_water_quality_and_ecosystem_data_provide_delta_science_program_program",
    "resource_water_quality_and_ecosystem_data_provide_interagency_ecological_program_program",
    "resource_weather_and_mesonet_services_ncar_real_time_weather_data_platform",
    "resource_weather_and_mesonet_services_tropical_tidbits_forecast_models_platform",
    "resource_weather_and_mesonet_services_western_regional_climate_center_platform",
    "resource_windy_windy_other",
    "resource_wwa_western_water_assessment_swe_fusion_other"
  ]
};
for (const [family, expected] of Object.entries(expectedProviderSets)) {
  const state = model.createState({providers:['family:'+family]});
  const actual = model.results(state).map(r=>r.id);
  assert.equal(new Set(actual).size,actual.length);
  assert.deepStrictEqual([...actual].sort(),expected, family+' exact membership');
}
const offeredNonOther = new Set(Object.entries(expectedProviderSets).filter(([id])=>id!=='other').flatMap(([,ids])=>ids));
assert.deepStrictEqual(resources.filter(r=>!offeredNonOther.has(r.id)).map(r=>r.id).sort(),expectedProviderSets.other);
for (const input of [{query:'water'},{query:'zzzzzzzzzz'},{preset:'in_brim_map'},
  {subject:'Fire & Burn Areas'},{resourceType:'dashboard'}]) {
  const unfiltered = model.results(model.createState(input)).map(r=>r.id);
  const narrowed = model.results(model.createState({...input,providers:['family:other']})).map(r=>r.id);
  assert.deepStrictEqual([...narrowed].sort(),unfiltered.filter(id=>expectedProviderSets.other.includes(id)).sort());
}
for (const family of Object.keys(expectedProviderSets)) {
  const state=model.createState({providers:['family:'+family,'family:other']});
  assert.deepStrictEqual(model.results(state).map(r=>r.id).sort(),[...new Set([...expectedProviderSets[family],...expectedProviderSets.other])].sort());
}
for(const query of ['nonprofit','NGO']) assert.deepStrictEqual(
 model.results(model.createState({query})).map(r=>r.id).sort(),expectedProviderSets.nonprofits);
for(const [query,id] of [['Southern California Edison','resource_sce_flow_and_reservoir_portal'],
 ['Sacramento Municipal Utility District','resource_smud_upper_american_river_project_conditions'],
 ['BLM California Wildfire Dashboard','resource_blm_california_wildfire_dashboard_public'],
 ['USGS Water Services APIs','resource_usgs_usgs_water_services_apis_service']]) {
 assert(model.results(model.createState({query})).some(r=>r.id===id),query);
}
// One shared query clear retains provider, view, subject and focus ownership.
const queryClearState=model.createState({query:'water',providers:['family:other'],preset:'beyond_the_map',subject:'Surface Water'});
const queryCleared=model.setQuery(queryClearState,'');
assert.deepStrictEqual(queryCleared.providers,queryClearState.providers);
assert.equal(queryCleared.preset,queryClearState.preset);assert.equal(queryCleared.subject,queryClearState.subject);
assert.equal(queryCleared.focusKey,'resource-search');
const joint={...resources[0],id:'joint_noaa_ngo',provider:'Joint program',providers:[
 {name:'National Oceanic and Atmospheric Administration',role:'publisher'},
 {name:'The Nature Conservancy',role:'partner'}],representedProducts:[]};
const jointModel=createModel([joint]);
assert.deepStrictEqual(jointModel.providerMembership(joint),['family:noaa','family:nonprofits']);
assert.equal(jointModel.results(jointModel.createState({providers:['family:noaa','family:nonprofits']})).length,1);
const geographyOnly={...resources[0],id:'geography_only',provider:'Unclassified organization',providers:[],
 geographicScope:{scopeType:'county',scopeLabel:'County',names:['Los Angeles County']},representedProducts:[]};
assert.deepStrictEqual(createModel([geographyOnly]).providerMembership(geographyOnly),['family:other']);
assert(css.includes('#brim-guide-root .brim-guide__search { cursor: text; }'));
assert(css.includes('::-webkit-search-cancel-button { cursor: pointer; }'));

const classification = registry.resources.map(r => ({ id: r.id,
  public: r.publication_state === 'published',
  families: model.providerMembership({ id: r.id, title: r.title,
    provider: r.providers.find(p => p.role === 'display_provider').name,
    providers: r.providers, canonicalUrl: r.canonical_url }) }));
assert.equal(classification.length, 233);
assert.equal(classification.filter(r => r.public && r.families.length).length, 228);
assert.equal(classification.filter(r => r.public && !r.families.length).length, 0);
assert.equal(classification.filter(r => r.public).reduce((n, r) => n + r.families.length, 0),
  Object.values(expectedCounts).reduce((a,b)=>a+b,0));
assert.equal(new Set(resources.map(r => r.provider)).size, 122);
assert(classification.filter(r => !r.public).every(r => !initialIds.includes(r.id)));
// Exercise every explicit spelling through the single exposed mapping; no fuzzy fallback.
for (const family of allFamilies) for (const name of family.exact_provider_names) {
  for (const role of policy.provider_roles) {
    assert(model.providerMembership({ id: 'fixture', provider: 'Uncurated source',
      providers: [{ role, name: '  ' + name.toUpperCase().replace(/ /g, '  ') + '  ' }]
    }).includes('family:' + family.id), `${family.id}: ${name} / ${role}`);
  }
  assert(!model.providerMembership({ id: 'fixture', provider: 'Uncurated source',
    providers: [{ role: 'data_owner', name }] }).includes('family:' + family.id));
  assert(!model.providerMembership({ id: 'fixture', provider: 'Prefix ' + name,
    providers: [] }).includes('family:' + family.id));
}
assert.deepStrictEqual(model.providerMembership({ id: 'fixture', provider: 'Uncurated source',
  providers: [], title: 'NASA NOAA USGS', searchText: 'NASA',
  canonicalUrl: 'https://www.nasa.gov', representedProducts: [{ title: 'NASA' }] }), ['family:other']);
assert.deepStrictEqual(model.providerMembership({ id: 'fixture', provider: 'NIFC / WFIGS' }), ['family:nifc','family:other']);
assert(!model.providerMembership({ id: 'fixture', provider: 'U.S. Department of Agriculture' }).includes('family:usbr'));
assert(!model.providerMembership({ id: 'fixture', provider: 'USDA-NRCS' }).includes('family:usda'));
assert.equal(policy.exact_resource_rules.length, 5);
for (const rule of policy.exact_resource_rules) {
  const r = resources.find(r => r.id === rule.resource_id);
  assert(r);
  assert(rule.families.every(id => model.providerMembership(r).includes('family:' + id)));
  for (const field of ['provider', 'title', 'canonicalUrl']) {
    assert.throws(() => createModel([{ ...r, [field]: r[field] + ' changed' }]), /guard mismatch/);
  }
  assert.deepStrictEqual(model.providerMembership({ ...r, id: 'similar-but-not-reviewed' }), ['family:other']);
}
for (const id of ['resource_noaa_cnrfc', 'resource_noaa_nws_graphical_forecasts',
  'resource_noaa_wpc_excessive_rainfall_outlook']) {
  assert(model.results(model.createState({ providers: ['family:noaa'] })).some(r => r.id === id));
}
const gsas = resources.find(r => /All Groundwater Sustainability Agencies/i.test(r.title));
assert(gsas && model.providerMembership(gsas).includes('family:dwr'));
for (const pair of [['family:nasa', 'family:nsidc'], ['family:nasa', 'family:usda']]) {
  const joint = resources.filter(r => pair.every(f => model.providerMembership(r).includes(f)));
  assert.equal(joint.length, 1);
  const results = model.results(model.createState({ providers: pair }));
  assert.equal(results.filter(r => r.id === joint[0].id).length, 1);
  const union = new Set(pair.flatMap(f => model.results(model.createState({ providers: [f] })).map(r => r.id)));
  assert.deepStrictEqual(new Set(results.map(r => r.id)), union);
}
// Independent count oracle: exact Set intersections for all dimensions, ignoring providers.
for (const input of [ {}, { query: 'NASA' }, { query: 'water', preset: 'in_brim_map' },
  { subject: 'Climate & Drought', informationType: 'Forecast / Outlook' },
  { resourceType: 'organization_homepage' }, { productContextId: 'ops_cdec_reservoir_storage' } ]) {
  const state = model.createState({ ...input, providers: ['family:noaa', 'family:nasa'] });
  const otherMatches = model.results(model.clearProviders(state));
  for (const option of model.providerOptions(state)) {
    const ids = new Set(otherMatches.filter(r => model.providerMembership(r).includes(option.value)).map(r => r.id));
    assert.equal(option.count, ids.size, JSON.stringify(input) + option.value);
  }
  const expected = otherMatches.filter(r => state.providers.some(f => model.providerMembership(r).includes(f)));
  assert.deepStrictEqual(model.results(state).map(r => r.id), expected.map(r => r.id));
}
for (const group of policy.groups) for (const isProviderType of [false,true]) {
  const options = model.providerOptions(initial).filter(o => o.group === group.id && o.isProviderType === isProviderType);
  assert.deepStrictEqual(options.map(o => o.label), options.map(o => o.label).sort((a,b) => a.toLowerCase().localeCompare(b.toLowerCase())));
}
assert.deepStrictEqual(model.providerOptions(initial).filter(o=>o.isProviderType).map(o=>o.label),
 ['County','Nonprofits / NGOs','Other','Private','Regional/local']);
assert.deepStrictEqual(model.providerOptions(model.createState({ providerQuery: 'impossible' })), model.providerOptions(initial));
const zero = model.createState({ query: 'zzzzzzzzzz', providers: ['family:noaa'], subject: 'Climate & Drought' });
assert.equal(model.results(zero).length, 0);
assert.equal(model.providerOptions(zero).length, 16);
assert(model.providerOptions(zero).every(o => o.count === 0));
assert.equal(model.chips(zero).filter(c => c.key === 'provider').length, 1);
const cleared = model.clearProviders(zero);
assert.equal(cleared.query, zero.query); assert.equal(cleared.subject, zero.subject);
assert.deepStrictEqual(cleared.providers, []);
assert.equal(cleared.focusKey, 'resource-provider-family%3Ablm');
assert.deepStrictEqual(model.removeChip(zero, 'provider', 'family:noaa').providers, []);
for (const nameQuery of ['Westlands', 'Chino Basin']) {
  const r = resources.find(r => r.provider.includes(nameQuery)); assert(r);
  const legacy = model.createState({ providers: [r.provider], query: '' });
  assert(model.results(legacy).every(item => item.provider === r.provider));
  assert(model.results(legacy).some(item => item.id === r.id));
  assert(model.chips(legacy)[0].label.startsWith('Exact provider: '));
  assert(!model.providerOptions(legacy).some(o => o.label.includes(nameQuery)));
}
const stale = model.createState({ query: 'water', providers: ['family:noaa', 'Unknown old provider', 'family:retired'] });
assert.equal(model.chips(stale).filter(c => c.label.startsWith('Unavailable provider: ')).length, 2);
assert.equal(model.removeChip(stale, 'provider', 'Unknown old provider').query, 'water');
assert(model.removeChip(stale, 'provider', 'Unknown old provider').providers.includes('family:noaa'));
assert.equal(model.results(model.createState({ providers: ['Unknown old provider'] })).length, 0);
let state = model.createState({ providers: ['family:noaa'], query: 'water',
  productContextId: 'ops_cdec_reservoir_storage', resultsScrollTop: 300, facetScrollTop: 120,
  returnResultsScrollTop: 300 });
assert.deepStrictEqual(model.providerGroups(state), policy.groups);
const selected = model.selectResource(state, 'resource_noaa_cnrfc');
const saved = model.snapshot(selected);
const restored = model.restore(saved);
assert.deepStrictEqual(restored, selected);
assert(!('providerGroupsOpen' in model.escape(restored).state));
assert.equal(model.escape(restored).state.productContextId, state.productContextId);
assert.equal(model.escape(restored).state.resultsScrollTop, 300);
assert.equal(model.setQuery(state, 'forecast').facetScrollTop, 120);
assert(!('providerGroupsOpen' in model.setQuery(state, 'forecast')));
assert.equal(model.reset(state).query, '');
assert.deepStrictEqual(model.reset(state).providers, []);
assert.equal(model.reset(state).productContextId, '');
assert.equal(model.reset(state).facetScrollTop, 0);
assert(!('providerGroupsOpen' in model.reset(state)));
// Exact A5E parity in the working evidence area, or unchanged Git model for portable runs.
let parityCases;
if (process.env.BRIM_PROVIDER_PARITY_BASELINE) {
  parityCases = JSON.parse(fs.readFileSync(process.env.BRIM_PROVIDER_PARITY_BASELINE)).cases;
} else {
  const { execFileSync } = require('child_process');
  const baseline = execFileSync('git', ['--no-optional-locks', '-C', rootPath, 'show',
    '3cdfc886da1aadcd447bb8c2b52d6d185fda0a97:03_functions/js/leaflet_brim_guide.js'],
  { encoding: 'utf8', env: { ...process.env, GIT_OPTIONAL_LOCKS: '0' } });
  assert.equal(crypto.createHash('sha256').update(baseline).digest('hex'),
    'c893c8a750842400203215e24a2b9a7be82e93828fa0a41530e36af1685dfcc5');
  const oldModel = new Function('return (' + extractFunction(baseline, 'ptCreateResourceExplorerModel') + ')')()(resources);
  parityCases = ['all_resources', 'in_brim_map', 'beyond_the_map'].flatMap(preset =>
    ['', 'Westlands', 'Chino Basin', 'GSA', 'BLM', 'ACEC', 'Federal Wilderness', 'CadNSDI',
      'CAL FIRE', 'NIFC', 'WFIGS', 'CW3E', 'Pivotal', 'Windy', 'Success Dam', 'SCSC1', 'NASA', 'NOAA'].flatMap(query =>
      ['', 'ops_cdec_reservoir_storage'].map(productContextId => {
        const input = { preset, query, productContextId };
        return { input, ids: oldModel.results(oldModel.createState(input)).map(r => r.id) };
      })));
}
assert.equal(parityCases.length, 108);
for (const { input, ids } of parityCases) {
  assert.deepStrictEqual(model.results(model.createState(input)).map(r => r.id), ids, JSON.stringify(input));
}

// A synthetic catalog may grow or shrink without changing any checkbox choice or order.
const expectedOptions = expectedRoster.map(([id,label,group]) => ({ value: 'family:'+id, label, group, isProviderType: ['county','nonprofits','other','private','regional_local'].includes(id) }));
for (const count of [0,1,35]) {
  const synthetic = allFamilies.flatMap(f => Array.from({ length: count }, (_,i) => ({
    ...resources[0], id: 'synthetic_'+f.id+'_'+i, title: f.id+' '+i,
    provider: f.exact_provider_names[0], providers: [], representedProducts: []
  })));
  const sm = createModel(synthetic);
  for (const input of [{}, { query: 'zzzzzzzzzz', providers: ['family:blm'] },
    { subject: 'nonexistent' }, { preset: 'beyond_the_map' }, { providers: ['family:nifc'] }]) {
    const options = sm.providerOptions(sm.createState(input));
    assert.deepStrictEqual(options.map(({count,...option}) => option), expectedOptions);
    assert(options.some(o=>o.value==='family:blm'));
    assert(!options.some(o=>o.value==='family:nifc'));
    if (input.query) assert(options.every(o=>o.count===0));
  }
  assert.equal(sm.providerOptions(sm.createState()).find(o=>o.value==='family:blm').count,count);
}
const optionSource = extractFunction(source,'providerOptions');
assert(!/\.filter\(|\.sort\(|\.slice\(/.test(optionSource), 'Roster must not be filtered, promoted or sorted from counts');
assert(!/providerGroupsOpen|toggleProviderGroup|providerMinCount|minimumProviderCount|providerEligibility/.test(source));
const nifc = resources.find(r=>r.provider==='NIFC / WFIGS'); assert(nifc);
for (const query of ['NIFC','WFIGS']) assert(model.results(model.createState({query})).some(r=>r.id===nifc.id));
const oldNifc = model.createState({providers:['family:nifc']});
assert.deepStrictEqual(model.results(oldNifc).map(r=>r.id),[nifc.id]);
assert(model.chips(oldNifc)[0].label.includes('NIFC'));
assert(!model.chips(oldNifc)[0].label.includes('BLM'));
assert.deepStrictEqual(model.removeChip(oldNifc,'provider','family:nifc').providers,[]);
for (const family of policy.legacy_families) {
  const old = model.createState({providers:['family:'+family.id]});
  assert.equal(model.chips(old)[0].label,family.label);
  assert.deepStrictEqual(model.removeChip(old,'provider','family:'+family.id).providers,[]);
  assert(!model.providerOptions(old).some(o=>o.value==='family:'+family.id));
}

// Current canonical links must agree with the public BLM projection, including source_reference.
const blm = resources.find(r=>r.id==='resource_blm_california'); assert(blm);
const canonicalBlm = relationships.products.filter(p=>p.resource_links.some(l=>l.resource_id===blm.id));
assert.equal(blm.representedProducts.length,25);
assert.deepStrictEqual(new Set(blm.representedProducts.map(p=>p.productId)),new Set(canonicalBlm.map(p=>p.product_id)));
const requiredBlm = ['acec','federal_wilderness','wilderness_study_areas','EXT103','EXT104','tool_blm_sma_context'];
for (const id of requiredBlm) assert(blm.representedProducts.some(p=>p.productId===id));
assert.deepStrictEqual(model.results(model.createState({providers:['family:blm']})).map(r=>r.id).sort(),expectedProviderSets.blm);
for (const link of blm.representedProducts) {
  const canonical = canonicalBlm.find(p=>p.product_id===link.productId);
  assert.equal(link.relationshipRole,canonical.resource_links.find(l=>l.resource_id===blm.id).relationship_role);
  if (bundle.products) {
    const product = bundle.products.find(p=>p.id===link.productId); assert(product);
    assert.equal(product.relatedResources.find(r=>r.id===blm.id).relationshipRole,link.relationshipRole);
  }
}
for (const id of ['resource_usgs_bcmv8','resource_prism_normals','resource_dwr_bulletin118_sgma_2019']) {
  const r = resources.find(r=>r.id===id); assert(r, id);
  assert(!model.providerMembership(r).includes('family:blm'), 'Shared Products do not assign BLM ownership');
}

// Minimal DOM: real renderers and handlers; layout and native activation are simulated.
const document = { activeElement: null };
class Element {
  constructor(tag, className = '', text = '') {
    this.tagName = tag.toUpperCase(); this.className = className; this.textContent = text;
    this.children = []; this.attributes = {}; this.parentNode = null; this.hidden = false;
    this.disabled = false; this.scrollTop = 0;
    this.classList = { toggle: (name,on) => { const c=new Set(this.className.split(' '));
      if(on)c.add(name);else c.delete(name);this.className=[...c].join(' '); } };
  }
  appendChild(child) { child.parentNode = this; this.children.push(child); return child; }
  setAttribute(k,v) { this.attributes[k] = String(v); }
  getAttribute(k) { return this.attributes[k] ?? null; }
  hasAttribute(k) { return k in this.attributes; }
  get offsetParent() { return this.hidden || (this.parentNode && this.parentNode.offsetParent === null) ? null : {}; }
  focus(options) { assert(this.offsetParent !== null && !this.disabled, 'Focus target is hidden/disabled');
    this.focusOptions=options; document.activeElement = this; }
  contains(other) { return this === other || this.children.some(c => c.contains(other)); }
  closest() { return this.hasAttribute('data-guide-action') ? this : this.parentNode && this.parentNode.closest(); }
  all() { return this.children.flatMap(c => [c, ...c.all()]); }
  querySelectorAll(selector) {
    if (selector === '[data-guide-focus-key]') return this.all().filter(e => e.hasAttribute('data-guide-focus-key'));
    if (selector.startsWith('button:not')) return this.all().filter(e => !e.disabled && ['BUTTON','INPUT','SELECT'].includes(e.tagName));
    throw new Error('Unhandled DOM selector: ' + selector);
  }
}
const node = (tag, cls, text) => new Element(tag, cls, text);
const button = (cls,text,action,label) => {
  const e = node('button',cls,text); e.type='button';
  if (action) e.setAttribute('data-guide-action',action);
  if (label) e.setAttribute('aria-label',label);
  return e;
};
const helpers = {node, button, resourceExplorerModel:model,
  asArray:x=>Array.isArray(x)?x:x?[x]:[], resourcesById:Object.fromEntries(resources.map(r=>[r.id,r]))};
for (const name of ['resourceFocusKey','resourceCountText','appendResourceFacetLabel','sortedCountKeys',
  'resourceFacetChoices','resourceFacetSelect','renderResourceProviders','renderResourceFacets',
  'resourceRepresentation','deliveryLabel','relationshipRoleLabel','appendResourceDetailRow','renderResourceDetail']) {
  helpers[name]=new Function(...Object.keys(helpers),'return ('+extractFunction(source,name)+')')(...Object.values(helpers));
}
const focusKey = helpers.resourceFocusKey;
const hasClass = (e,name)=>e.className.split(' ').includes(name);
for (const width of [1440,1280,390]) {
  const ui = { view:'resource-explorer', resourceExplorer:model.createState({pane:'facets',facetsOpen:true}) };
  const root = node('div'); const search=node('input');search.type='search';focusKey(search,'resource-search');
  const filterToggle=button('','Filters','resource-facets-toggle');focusKey(filterToggle,'resource-facets-toggle');
  const render = () => { root.children=[];root.appendChild(search);root.appendChild(filterToggle);
    root.appendChild(helpers.renderResourceFacets(ui.resourceExplorer,model.facetCounts(ui.resourceExplorer))); };
  const findAction = a=>root.all().find(e=>e.getAttribute('data-guide-action')===a);
  const findClass = c=>root.all().find(e=>hasClass(e,c));
  const checkbox = id=>root.all().find(e=>e.getAttribute('data-resource-provider')==='family:'+id);
  const revealed=[];
  const focus = (key,reveal)=>{ const e=root.all().find(e=>e.getAttribute('data-guide-focus-key')===key);
    assert(e,'Missing focus target '+key);e.focus({preventScroll:!reveal}); if(reveal)revealed.push(key); };
  const click = new Function('root','state','resourceExplorerModel','render','focusResourceTarget',
    'return ('+extractFunction(source,'handleRootClick')+')')(root,ui,model,render,focus);
  const change = new Function('state','resourceExplorerModel','render','focusResourceTarget',
    'return ('+extractFunction(source,'handleResourceChange')+')')(ui,model,render,focus);
  const keydown = new Function('root','state','document','resourceExplorerModel','render','focusResourceTarget',
    'return ('+extractFunction(source,'handleRootKeydown')+')')(root,ui,document,model,render,focus);
  const activate=(e,key)=>{let prevented=false;keydown({target:e,key,preventDefault(){prevented=true;}});
    assert(!prevented,'Native activation intercepted');if(e.tagName==='BUTTON')click({target:e});
    else {assert.equal(e.type,'checkbox');e.checked=!e.checked;change({target:e});}};
  render();
  const providers=findClass('brim-guide__resource-provider');
  assert.equal(providers.children[0].children[0].textContent,'Selected providers');
  assert.equal(providers.children[1].textContent,'Filter by agency or type; search above for any provider.');
  assert.deepStrictEqual(providers.all().filter(e=>e.tagName==='LEGEND').map(e=>e.textContent),['Federal','State','Provider types']);
  const fieldsets=providers.all().filter(e=>e.tagName==='FIELDSET');
  assert.equal(fieldsets.length,3);
  assert.deepStrictEqual(fieldsets.map(e=>e.getAttribute('data-provider-group')),['federal','state','types']);
  assert(fieldsets.every(e=>e.parentNode===findClass('brim-guide__resource-provider-groups')));
  for (const section of fieldsets) {
    const group=section.getAttribute('data-provider-group');
    assert.deepStrictEqual(section.all().filter(e=>e.type==='checkbox').map(e=>e.getAttribute('data-resource-provider')),
      expectedRoster.filter(([, ,g])=>g===group).map(([id])=>'family:'+id));
  }
  assert.equal(providers.all().filter(e=>e.type==='checkbox').length,16);
  const otherDescription=providers.all().find(e=>e.id===checkbox('other').getAttribute('aria-describedby'));
  assert(otherDescription && hasClass(otherDescription,'brim-guide__search-label'));
  assert.equal(otherDescription.textContent,'Other contains public Resources outside all offered agency and provider-type groups, before other filters.');
  assert(!providers.all().some(e=>['SELECT','DETAILS','SUMMARY'].includes(e.tagName)||e.hasAttribute('aria-expanded')));
  const allIds=root.all().filter(e=>e.id).map(e=>e.id);assert.equal(new Set(allIds).size,allIds.length);
  for(const cb of providers.all().filter(e=>e.type==='checkbox')) {
    assert.equal(cb.parentNode.tagName,'LABEL');assert.equal(cb.parentNode.getAttribute('for'),cb.id);
    assert.deepStrictEqual(cb.parentNode.children.map(e=>e.tagName),['INPUT','SPAN','SMALL']);
    assert(cb.getAttribute('aria-label').includes('Resource'));assert(!cb.disabled && cb.offsetParent!==null);
  }
  activate(checkbox('noaa'),' ');assert.equal(document.activeElement,checkbox('noaa'));
  assert.equal(model.chips(ui.resourceExplorer)[0].label,'NOAA');
  ui.resourceExplorer=model.setQuery(ui.resourceExplorer,'zzzzzzzzzz');render();
  assert(checkbox('noaa').checked&&!checkbox('noaa').disabled);
  click({target:findAction('resource-provider-clear')});assert.equal(document.activeElement,checkbox('blm'));
  assert.equal(ui.resourceExplorer.query,'zzzzzzzzzz');
  ui.resourceExplorer=model.createState({pane:'facets',facetsOpen:true,providers:['family:blm']});render();
  const body=findClass('brim-guide__resource-filter-body'),footer=findClass('brim-guide__resource-filter-actions');
  assert.equal(body.parentNode,footer.parentNode);assert(!body.contains(footer));
  assert.equal(footer.parentNode.children.at(-1),footer);assert(footer.contains(findAction('resource-reset')));
  assert.equal(findAction('resource-more-filters').getAttribute('aria-controls'),'brim-guide-resource-more-panel');
  activate(findAction('resource-more-filters'),'Enter');
  const select=root.all().find(e=>e.tagName==='SELECT');assert(select);
  assert.equal(document.activeElement,select);assert.equal(revealed.at(-1),'resource-more-resourceType');
  assert(findClass('brim-guide__resource-filter-body').contains(select));
  assert.equal(findClass('brim-guide__resource-more-panel').hidden,false);
  assert.equal(findClass('brim-guide__resource-filter-body').children.at(-1),findClass('brim-guide__resource-more-panel'));
  assert.deepStrictEqual(findClass('brim-guide__resource-facet-choices').all().filter(e=>e.tagName==='H3').map(e=>e.textContent),['Subject','Information Type']);
  const expandedOrder=root.all().filter(e=>['SELECT','BUTTON'].includes(e.tagName));
  assert(expandedOrder.indexOf(select)<expandedOrder.indexOf(findAction('resource-more-filters')));
  assert(expandedOrder.indexOf(findAction('resource-more-filters'))<expandedOrder.indexOf(findAction('resource-reset')));
  const types=Object.keys(model.facetCounts(ui.resourceExplorer).resourceTypes);assert(types.includes('organization_homepage'));
  select.value='organization_homepage';change({target:select});
  assert.equal(ui.resourceExplorer.resourceType,'organization_homepage');
  assert.deepStrictEqual(model.results(ui.resourceExplorer).map(r=>r.id),[blm.id]);
  assert(ui.resourceExplorer.moreFiltersOpen);
  activate(findAction('resource-more-filters'),' ');assert(!ui.resourceExplorer.moreFiltersOpen);
  assert.equal(document.activeElement,findAction('resource-more-filters'));
  assert.equal(ui.resourceExplorer.resourceType,'organization_homepage');
  // Simulate native sequential Tab between real rendered controls; verify the root trap's ends.
  activate(findAction('resource-more-filters'),'Enter');
  const focusable=root.querySelectorAll('button:not([disabled])').filter(e=>e.offsetParent!==null);
  for(let i=0;i<focusable.length;i++) {
    focusable[i].focus();let prevented=false;
    keydown({target:focusable[i],key:'Tab',shiftKey:false,preventDefault(){prevented=true;}});
    if(i===focusable.length-1){assert(prevented);assert.equal(document.activeElement,search);}
    else {assert(!prevented);focusable[i+1].focus();}
  }
  assert.equal(focusable.at(-1),findAction('resource-reset'));
  search.focus();let wrapped=false;keydown({target:search,key:'Tab',shiftKey:true,preventDefault(){wrapped=true;}});
  assert(wrapped);assert.equal(document.activeElement,findAction('resource-reset'));
  click({target:findAction('resource-reset')});assert.equal(document.activeElement,search);
  assert.deepStrictEqual(ui.resourceExplorer.providers,[]);assert.equal(ui.resourceExplorer.resourceType,'');
}
// Desktop and narrow facet panes use the filter body; other narrow/intermediate panes retain main.
// Mock scroll values exercise ownership and restoration, not CSS geometry.
for (const width of [1440,1280,900,390]) {
  const ui={resourceExplorer:model.createState()};
  const body={scrollTop:121},results={scrollTop:242},detail={scrollTop:363};
  const pane={value:'facets'};
  const explorer={getAttribute:()=>pane.value,querySelector:selector=>({
    '.brim-guide__resource-filter-body':body,'.brim-guide__resource-results-scroll':results,
    '.brim-guide__resource-detail':detail
  })[selector]};
  const main={scrollTop:484,querySelector:()=>explorer};
  const win={matchMedia:query=>({matches:query==='(min-width: 1101px)'?width>=1101:width<=700})};
  const capture=new Function('main','state','window','return ('+extractFunction(source,'captureResourceScrollPositions')+')')(main,ui,win);
  const restore=new Function('main','state','window','return ('+extractFunction(source,'restoreResourceScrollPositions')+')')(main,ui,win);
  const bodyOwnsFacets=width>=1101||width<=700;
  capture();assert.equal(ui.resourceExplorer.facetScrollTop,bodyOwnsFacets?121:484);
  if(width>=1101){assert.equal(ui.resourceExplorer.resultsScrollTop,242);assert.equal(ui.resourceExplorer.detailScrollTop,363);}
  body.scrollTop=results.scrollTop=detail.scrollTop=main.scrollTop=0;restore();
  assert.equal(bodyOwnsFacets?body.scrollTop:main.scrollTop,bodyOwnsFacets?121:484);
  if(bodyOwnsFacets)assert.equal(main.scrollTop,0);
  if(width<1101){
    pane.value='results';main.scrollTop=55;capture();assert.equal(ui.resourceExplorer.resultsScrollTop,55);
    main.scrollTop=0;restore();assert.equal(main.scrollTop,55);
    pane.value='detail';main.scrollTop=77;capture();assert.equal(ui.resourceExplorer.detailScrollTop,77);
    main.scrollTop=0;restore();assert.equal(main.scrollTop,77);
    assert.equal(ui.resourceExplorer.facetScrollTop,bodyOwnsFacets?121:484);
  }
}
// Explicit reveal uses native focus scrolling then captures it before scheduled restoration.
{
  const target=node('select');focusKey(target,'resource-more-resourceType');
  const root=node('div');root.appendChild(target);const events=[];
  const win={setTimeout:fn=>fn()};
  const focus=new Function('window','root','state','searchInput','captureResourceScrollPositions',
    'restoreResourceScrollAfterFocus','focusMain','return ('+extractFunction(source,'focusResourceTarget')+')')(
    win,root,{view:'resource-explorer'},node('input'),()=>events.push('capture'),()=>events.push('restore'),()=>{});
  focus('resource-more-resourceType',true);assert.equal(target.focusOptions.preventScroll,false);
  assert.deepStrictEqual(events,['capture','restore']);events.length=0;
  focus('resource-more-resourceType');assert.equal(target.focusOptions.preventScroll,true);assert.deepStrictEqual(events,['restore']);
}
// Run the actual detail renderer and shared Product handler for every existing BLM relationship.
const blmDetail=helpers.renderResourceDetail(blm);
const productButtons=blmDetail.all().filter(e=>e.getAttribute('data-guide-action')==='record');
assert.deepStrictEqual(productButtons.map(e=>e.getAttribute('data-guide-record')),blm.representedProducts.map(p=>p.productId));
const products=bundle.products || blm.representedProducts.map(p=>({kind:'Product',id:p.productId,title:p.title}));
const recordsById=Object.fromEntries(products.map(p=>[p.id,p]));
for(const target of productButtons) {
  const ui={view:'resource-explorer',resourceExplorer:model.selectResource(model.createState({providers:['family:blm']}),blm.id)};
  const saved=model.snapshot(ui.resourceExplorer);const history=[];
  const open=new Function('recordsById','state','pushHistory','render','focusMain',
    'return ('+extractFunction(source,'openRecord')+')')(recordsById,ui,()=>history.push(model.snapshot(ui.resourceExplorer)),()=>{},()=>{});
  const click=new Function('root','openRecord','return ('+extractFunction(source,'handleRootClick')+')')(blmDetail,open);
  click({target});assert.equal(ui.view,'detail');assert.equal(ui.selectedId,target.getAttribute('data-guide-record'));
  assert.deepStrictEqual(model.restore(history.pop()),saved);
}
assert(blmDetail.all().some(e=>e.textContent==='Bureau of Land Management'));
assert.equal(JSON.stringify(model.detail(model.selectResource(model.createState({providers:['family:nifc']}),nifc.id)).accessPoints),JSON.stringify(nifc.accessPoints));
assert(!source.includes('resource-provider-search') && !source.includes('handleResourceInput'));
assert(!source.includes("action === 'resource-provider-group'"));
assert(!css.includes('.brim-guide__resource-provider-toggle'));
assert(css.includes('.brim-guide input:focus-visible'));
for(const match of css.matchAll(/\.brim-guide__resource-provider-options\s*\{([^}]+)\}/g)) {
  assert(!/overflow|max-height/.test(match[1]),'Nested provider scroll returned');
  assert(!/space-between|\b1fr\b/.test(match[1]),'Provider controls must not stretch across equal-width cells');
}
const optionRule=css.match(/\.brim-guide__resource-provider-option\s*\{([^}]+)\}/)[1];
assert(optionRule.includes('display: inline-flex')&&optionRule.includes('gap: 3px'));
assert(!/space-between|margin[^;]*auto/.test(optionRule));
assert(css.includes('container: brim-providers / inline-size'));
assert(css.includes('grid-template-columns: repeat(3, max-content)'));
assert(css.includes('grid-template-columns: repeat(5, max-content)'));
assert(css.includes('@container brim-providers (max-width: 25em)'));
assert(css.includes('flex-basis: 100%'),'State/type groups must stack when space or enlarged text requires it');
assert(css.includes('min-height: max(24px, 1.5rem)'));
assert(css.includes('font-size: 0.8125rem; font-weight: 400'));
assert(css.includes('scroll-padding-block: 0.75rem'));
assert(css.includes('.brim-guide__resource-filter-body {\n    flex: 1 1 auto; min-height: 0; overflow-x: hidden; overflow-y: auto'));
assert(css.includes('.brim-guide__resource-filter-actions {\n    flex: 0 0 auto;'));
const actionRules=[...css.matchAll(/\.brim-guide__resource-filter-actions\s*\{([^}]+)\}/g)];
assert(actionRules.every(m=>!/position:\s*(fixed|absolute|sticky)/.test(m[1])));
assert(source.includes("existing.querySelector('.brim-guide__resource-filter-body')"));
assert(source.includes('candidates[index].focus({ preventScroll: !reveal })'));
assert(source.includes('if (reveal) captureResourceScrollPositions()'));
assert(css.slice(css.lastIndexOf('@media (max-width: 700px)')).includes('[data-resource-pane="facets"] .brim-guide__resource-presets {\n    position: static;'));
const narrowRules=css.slice(css.lastIndexOf('@media (max-width: 700px)'));
assert(narrowRules.includes('.brim-guide__main:has(.brim-guide__resource-explorer[data-resource-pane="facets"]) {\n    display: grid; grid-template-rows: minmax(0, 1fr); overflow: hidden;'));
assert(narrowRules.includes('[data-resource-pane="facets"] .brim-guide__resource-filter-body {\n    flex: 1 1 auto; min-height: 0; overflow-x: hidden; overflow-y: auto'));
assert(narrowRules.includes('[data-resource-pane="facets"] .brim-guide__resource-filter-actions {\n    flex: 0 0 auto;'));
assert.equal(JSON.stringify({bundle,registry,relationships}),before,'Input bundle/attribution/URLs/relationships mutated');
console.log('SELECTED_PROVIDERS=11_AGENCIES_PLUS_5_TYPES; GROUPS=3_FEDERAL_STATE_TYPES; BLM_PRESENT; NIFC_SEARCHABLE_LEGACY_REMOVABLE');
console.log('STATIC_ROSTER_0_1_MANY_AND_FILTERED_ZERO=PASS; COUNT_ELIGIBILITY=ABSENT');
console.log('PROVIDER_MAPPING_COUNTS_OR_AND_COMPATIBILITY=PASS; BLM_25_LINKS_AND_PRODUCT_NAVIGATION=PASS');
console.log('NO_PROVIDER_SEARCH_ORDER_PARITY=108_PASS');
console.log('MORE_FILTERS_STRUCTURE_NATIVE_ACTIVATION_FOCUS_AND_RESET=PASS');
console.log('DOM_STRUCTURE_AND_SCROLL_OWNERSHIP=PASS_1440_1280_390; GEOMETRY_NOT_TESTED; NATIVE_ACTIVATION_SIMULATED; MOUNTED_BROWSER=NOT_RUN');
console.log('INPUT_BUNDLE_AND_ATTRIBUTION_IMMUTABLE=PASS');
