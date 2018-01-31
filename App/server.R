server <- function(input, output, session){
  
  output$user <- renderText({
    session$user
  })
  
  country <- callModule(sidebar,'sb') ## Calling server module to get Country selection
  dt_city <- callModule(direct_city_filterSERVER, 'dt_city')
  callModule(top_metrics_cm,'cm',country) ## Calling Current Month ValueBoxes server Module
  callModule(top_metrics_pm,'pm',country) ## Calling Previous Month ValueBoxes server Module
  
  ## Traffic
  callModule(users_channelSERVER,'trf_1a',country) ## Calling Chart trf_1a server Module
  callModule(users_sourceSERVER,'trf_1b',country) ## Calling Chart trf_1b server Module
  callModule(users_segmentdeviceSERVER,'trf_1c',country) ## Calling Chart trf_1c server Module
  callModule(users_segmenttierSERVER,'trf_1d',country) ## Calling Chart trf_1d server Module
  callModule(users_tierSERVER,'trf_1f',country) ## Calling Chart trf_1f server Module
  callModule(users_dailySERVER, 'trf_2a', country)
  callModule(users_cumulative_dailySERVER, 'trf_2b', country)
  
  callModule(users_direct_monthlySERVER, 'trf_3a', country, dt_city)
  callModule(users_direct_perc_monthlySERVER, 'trf_3b', country, dt_city)
  callModule(users_direct_listing_profileSERVER, 'trf_3c', country, dt_city)
  callModule(users_direct_growthSERVER, 'trf_3d', country, dt_city)
  
  ## Transactions
  callModule(trans_mode,'tra_1a',country) ## Calling Chart tra_1a server Module
  callModule(trans_monetizeable_unique,'tra_1b',country) ## Calling Chart tra_1b server Module
  callModule(trans_source,'tra_1c',country) ## Calling Chart tra_1c server Module
  # callModule(trans_segment,'tra_1d',country) ## Calling Chart tra_1d server Module
  callModule(trans_tier,'tra_1g',country) ## Calling Chart tra_1g server Module
  
  callModule(trans_daily,'tra_2a',country) ## Calling Chart tra_2a server Module
  callModule(trans_unique_dailySERVER,'tra_2b', country) ## Calling Chart tra_2b server Module
  callModule(trans_unique_7d_movingSERVER,'tra_2c', country) ## Calling Chart tra_2c server Module
  callModule(trans_unique_30d_movingSERVER,'tra_2d', country) ## Calling Chart tra_2d server Module
  
  callModule(trans_abs_country,'tra_3a',country) ## Calling Chart tra_3a server Module

  ## Conversion
  # callModule(conversion_nonunique,'cnv_1a',country) ## Calling Chart cnv_1a server Module
  callModule(conversion_unique,'cnv_1b', country) ## Calling Chart cnv_1b server Module
  callModule(conversion_tierSERVER,'cnv_1c', country) ## Calling Chart cnv_1c server Module
  callModule(conversion_unique_sourceSERVER,'cnv_1d', country) ## Calling Chart cnv_1d server Module
  callModule(conversion_dailySERVER, 'cnv_2a', country) ## Calling Chart cnv_2a server Module
  
  ## CPT
  callModule(cpt_weekly_summarySERVER, 'cpt_0a', country)
  callModule(cpt_weekly_summary_mrrSERVER, 'cpt_0b', country)
  callModule(cpt_monthly_expansionSERVER, 'cpt_0c', country)
  
  callModule(unique_transactions_enterpriseSERVER, 'cpt_1a', country)
  callModule(transactions_monetisedSERVER, 'cpt_1b', country)
  callModule(transactions_monetised_funnelsSERVER, 'cpt_1c', country)
  
  callModule(mrr_reach_cptSERVER, 'cpt_2a', country)
  callModule(ecpt_reach_cptSERVER, 'cpt_2b', country)

  callModule(cpt_practices_liveSERVER, 'cpt_3a', country)
  callModule(fqs_cpt_live_docsSERVER, 'cpt_3b', country)
  callModule(fqs_cpt_recos20SERVER, 'cpt_3c', country)
  callModule(fqs_cpt_reviews10SERVER, 'cpt_3d', country)
  callModule(fqs_cpt_bookSERVER, 'cpt_3e', country)
  
  callModule(cpt_practice_churn_90dSERVER, 'cpt_4a', country)
  callModule(cpt_practice_churn_30dSERVER, 'cpt_4b', country)
  callModule(cpt_practice_acquisitionSERVER, 'cpt_4c', country)
  
  ## GMV/ VPE
  callModule(gmv_buSERVER, 'gmv_1a')
  callModule(vpe_buSERVER, 'gmv_1b')
  
  ## Liquidity
  callModule(liquiditySERVER, 'liq_1a')
}