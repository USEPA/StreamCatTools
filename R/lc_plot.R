#' Plot National Nutrient Inventory data for lakes
#'
#' @description
#' Function to plot time series of nitrogen and phosphorus budgets for a given lake
#' COMID. This function allows a user to return a time series of major inputs, 
#' outputs, and derived metrics of nitrogen and phosphorus. Plot is returned as an 
#' object
#' 
#' @author 
#' Selia Markley
#'
#' @param comid Identifier of lake COMID user wants to plot NNI data for. Must be a character string
#' with the COMID digit.
#' Syntax: com=<COMID>
#' 
#' @param include.nue Include time series of nitrogen use efficiency in the returned plot. 
#' The default value is false. 
#' Values: true|false 
#'
#' @return
#' Return plot as an object.
#' @export
#'
#' @examples
#' \dontrun{
#' p <- lc_plotnni(comid='23794487')
#' p <- lc_plotnni(comid='23794487', include.nue=TRUE)
#' }

lc_plotnni <- function(comid, include.nue = FALSE){
  
  # Get StreamCat data
  nni <- lc_get_data(metric = 'n_dep_1990,n_ff_1990,n_uf_1990,n_lw_1990,n_hw_1990,n_ags_1990,n_cf_1990,n_cr_1990,p_cr_1990,p_lw_1990,p_hw_1990,p_uf_1990,p_ff_1990,p_ags_1990,n_dep_1991,n_ff_1991,n_uf_1991,n_lw_1991,n_hw_1991,n_ags_1991,n_cf_1991,n_cr_1991,p_cr_1991,p_lw_1991,p_hw_1991,p_uf_1991,p_ff_1991,p_ags_1991,n_dep_1992,n_ff_1992,n_uf_1992,n_lw_1992,n_hw_1992,n_ags_1992,n_cf_1992,n_cr_1992,p_cr_1992,p_lw_1992,p_hw_1992,p_uf_1992,p_ff_1992,p_ags_1992,n_dep_1993,n_ff_1993,n_uf_1993,n_lw_1993,n_hw_1993,n_ags_1993,n_cf_1993,n_cr_1993,p_cr_1993,p_lw_1993,p_hw_1993,p_uf_1993,p_ff_1993,p_ags_1993,n_dep_1994,n_ff_1994,n_uf_1994,n_lw_1994,n_hw_1994,n_ags_1994,n_cf_1994,n_cr_1994,p_cr_1994,p_lw_1994,p_hw_1994,p_uf_1994,p_ff_1994,p_ags_1994,n_dep_1995,n_ff_1995,n_uf_1995,n_lw_1995,n_hw_1995,n_ags_1995,n_cf_1995,n_cr_1995,p_cr_1995,p_lw_1995,p_hw_1995,p_uf_1995,p_ff_1995,p_ags_1995,n_dep_1996,n_ff_1996,n_uf_1996,n_lw_1996,n_hw_1996,n_ags_1996,n_cf_1996,n_cr_1996,p_cr_1996,p_lw_1996,p_hw_1996,p_uf_1996,p_ff_1996,p_ags_1996,n_dep_1997,n_ff_1997,n_uf_1997,n_lw_1997,n_hw_1997,n_ags_1997,n_cf_1997,n_cr_1997,p_cr_1997,p_lw_1997,p_hw_1997,p_uf_1997,p_ff_1997,p_ags_1997,n_dep_1998,n_ff_1998,n_uf_1998,n_lw_1998,n_hw_1998,n_ags_1998,n_cf_1998,n_cr_1998,p_cr_1998,p_lw_1998,p_hw_1998,p_uf_1998,p_ff_1998,p_ags_1998,n_dep_1999,n_ff_1999,n_uf_1999,n_lw_1999,n_hw_1999,n_ags_1999,n_cf_1999,n_cr_1999,p_cr_1999,p_lw_1999,p_hw_1999,p_uf_1999,p_ff_1999,p_ags_1999,n_dep_2000,n_ff_2000,n_uf_2000,n_lw_2000,n_hw_2000,n_ags_2000,n_cf_2000,n_cr_2000,p_cr_2000,p_lw_2000,p_hw_2000,p_uf_2000,p_ff_2000,p_ags_2000,n_dep_2001,n_ff_2001,n_uf_2001,n_lw_2001,n_hw_2001,n_ags_2001,n_cf_2001,n_cr_2001,p_cr_2001,p_lw_2001,p_hw_2001,p_uf_2001,p_ff_2001,p_ags_2001,n_dep_2002,n_ff_2002,n_uf_2002,n_lw_2002,n_hw_2002,n_ags_2002,n_cf_2002,n_cr_2002,p_cr_2002,p_lw_2002,p_hw_2002,p_uf_2002,p_ff_2002,p_ags_2002,n_dep_2003,n_ff_2003,n_uf_2003,n_lw_2003,n_hw_2003,n_ags_2003,n_cf_2003,n_cr_2003,p_cr_2003,p_lw_2003,p_hw_2003,p_uf_2003,p_ff_2003,p_ags_2003,n_dep_2004,n_ff_2004,n_uf_2004,n_lw_2004,n_hw_2004,n_ags_2004,n_cf_2004,n_cr_2004,p_cr_2004,p_lw_2004,p_hw_2004,p_uf_2004,p_ff_2004,p_ags_2004,n_dep_2005,n_ff_2005,n_uf_2005,n_lw_2005,n_hw_2005,n_ags_2005,n_cf_2005,n_cr_2005,p_cr_2005,p_lw_2005,p_hw_2005,p_uf_2005,p_ff_2005,p_ags_2005,n_dep_2006,n_ff_2006,n_uf_2006,n_lw_2006,n_hw_2006,n_ags_2006,n_cf_2006,n_cr_2006,p_cr_2006,p_lw_2006,p_hw_2006,p_uf_2006,p_ff_2006,p_ags_2006,n_dep_2007,n_ff_2007,n_uf_2007,n_lw_2007,n_hw_2007,n_ags_2007,n_cf_2007,n_cr_2007,p_cr_2007,p_lw_2007,p_hw_2007,p_uf_2007,p_ff_2007,p_ags_2007,n_dep_2008,n_ff_2008,n_uf_2008,n_lw_2008,n_hw_2008,n_ags_2008,n_cf_2008,n_cr_2008,p_cr_2008,p_lw_2008,p_hw_2008,p_uf_2008,p_ff_2008,p_ags_2008,n_dep_2009,n_ff_2009,n_uf_2009,n_lw_2009,n_hw_2009,n_ags_2009,n_cf_2009,n_cr_2009,p_cr_2009,p_lw_2009,p_hw_2009,p_uf_2009,p_ff_2009,p_ags_2009,n_dep_2010,n_ff_2010,n_uf_2010,n_lw_2010,n_hw_2010,n_ags_2010,n_cf_2010,n_cr_2010,p_cr_2010,p_lw_2010,p_hw_2010,p_uf_2010,p_ff_2010,p_ags_2010,n_dep_2011,n_ff_2011,n_uf_2011,n_lw_2011,n_hw_2011,n_ags_2011,n_cf_2011,n_cr_2011,p_cr_2011,p_lw_2011,p_hw_2011,p_uf_2011,p_ff_2011,p_ags_2011,n_dep_2012,n_ff_2012,n_uf_2012,n_lw_2012,n_hw_2012,n_ags_2012,n_cf_2012,n_cr_2012,p_cr_2012,p_lw_2012,p_hw_2012,p_uf_2012,p_ff_2012,p_ags_2012,n_dep_2013,n_ff_2013,n_uf_2013,n_lw_2013,n_hw_2013,n_ags_2013,n_cf_2013,n_cr_2013,p_cr_2013,p_lw_2013,p_hw_2013,p_uf_2013,p_ff_2013,p_ags_2013,n_dep_2014,n_ff_2014,n_uf_2014,n_lw_2014,n_hw_2014,n_ags_2014,n_cf_2014,n_cr_2014,p_cr_2014,p_lw_2014,p_hw_2014,p_uf_2014,p_ff_2014,p_ags_2014,n_dep_2015,n_ff_2015,n_uf_2015,n_lw_2015,n_hw_2015,n_ags_2015,n_cf_2015,n_cr_2015,p_cr_2015,p_lw_2015,p_hw_2015,p_uf_2015,p_ff_2015,p_ags_2015,n_dep_2016,n_ff_2016,n_uf_2016,n_lw_2016,n_hw_2016,n_ags_2016,n_cf_2016,n_cr_2016,p_cr_2016,p_lw_2016,p_hw_2016,p_uf_2016,p_ff_2016,p_ags_2016,n_dep_2017,n_ff_2017,n_uf_2017,n_lw_2017,n_hw_2017,n_ags_2017,n_cf_2017,n_cr_2017,p_cr_2017,p_lw_2017,p_hw_2017,p_uf_2017,p_ff_2017,p_ags_2017',
                     aoi='ws',
                     comid = comid,
                     showAreaSqKm = FALSE,
                     showPctFull = FALSE)
  
  # Declare NULL metrics created w/in function
  year <- NULL
  value <- NULL
  ags <- NULL
  cr <- NULL
  totag <- NULL
  metric <- NULL
  estimated <- NULL
  
  # Create N inputs df
  nin <- nni[, grepl("^(n)", names(nni)) & !grepl("(cr)", names(nni)) & !grepl("(ags)", names(nni))]
  
  names(nin) <- sapply(names(nin), function(col){
    substr(col, 3, nchar(col) -2)
  })
  
  nin <- nin |>
    tidyr::pivot_longer(
      cols = tidyselect::everything(),
      names_to = c("metric", "year"),
      names_sep = "_",
      values_to = "value"
    ) |>
    dplyr::mutate(year = as.integer(year)) |>
    dplyr::mutate(value = value / 1000000)
  
  #Create P inputs df
  
  pin <- nni[, grepl("^(p)", names(nni)) & !grepl("(cr)", names(nni)) & !grepl("(ags)", names(nni))]
  
  names(pin) <- sapply(names(pin), function(col){
    substr(col, 3, nchar(col) -2)
  })
  
  pin <- pin |>
    tidyr::pivot_longer(
      cols = tidyselect::everything(),
      names_to = c("metric", "year"),
      names_sep = "_",
      values_to = "value"
    ) |>
    dplyr::mutate(year = as.integer(year)) |>
    dplyr::mutate(value = value / 1000000)
  
  #Create N dfs for lines (cr, agsur, nue)
  
  nlines <- nni[, grepl("^n_cr|^n_ags", names(nni))]
  
  names(nlines) <- sapply(names(nlines), function(col){
    substr(col, 3, nchar(col) -2)
  })
  
  nlines <- nlines |>
    tidyr::pivot_longer(
      cols = tidyselect::everything(),
      names_to = c("metric","year"),
      names_sep = "_",
      values_to = "value"
    ) |>
    dplyr::mutate(year = as.integer(year)) |>
    dplyr::mutate(value = value / 1000000) |>
    tidyr::pivot_wider(
      names_from = 'metric', 
      values_from = 'value'
    ) |>
    dplyr::mutate(totag = ags + cr) |>
    dplyr::mutate(nue = (cr / totag) * 100) |>
    tidyr::pivot_longer(
      cols = !year, 
      names_to="metric", 
      values_to="value")
  
  ncrag <- nlines |>
    dplyr::filter(metric %in% c('ags', 'cr'))
  
  nue <- nlines |>
    dplyr::filter(metric == 'nue') |>
    tidyr::pivot_wider(names_from = 'metric',
                values_from = 'value')
  
  #Create P dfs for lines (cr, agsur, pue)
  
  plines <- nni[, grepl("^p_cr|^p_ags", names(nni))]
  
  names(plines) <- sapply(names(plines), function(col){
    substr(col, 3, nchar(col) -2)
  })
  
  
  plines <- plines |>
    tidyr::pivot_longer(
      cols = tidyselect::everything(),
      names_to = c("metric","year"),
      names_sep = "_",
      values_to = "value"
    ) |>
    dplyr::mutate(year = as.integer(year)) |>
    dplyr::mutate(value = value / 1000000) |>
    tidyr::pivot_wider(
      names_from = 'metric', 
      values_from = 'value'
    ) |>
    dplyr::mutate(totag = ags + cr) |>
    dplyr::mutate(nue = (cr / totag) * 100) |>
    tidyr::pivot_longer(
      cols = !year, 
      names_to="metric", 
      values_to="value")
  
  pcrag <- plines |>
    dplyr::filter(metric %in% c('ags', 'cr'))
  
  pue <- plines |>
    dplyr::filter(metric == 'nue') |>
    tidyr::pivot_wider(names_from = 'metric',
                values_from = 'value')
  
  pdf <- dplyr::bind_rows(plines, pin)
  
  ndf <- dplyr::bind_rows(nlines, nin)
  
  #create estimate column
  knownfertyrs <- c(1987,1988,1989,1990,1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,
                    2005,2006,2007,2008,2009,2010,2011,2012,2017)
  nwsin <- nin |>
    dplyr::mutate(estimated=dplyr::case_when(
      metric == "dep" ~ FALSE,
      metric == "hw" ~ FALSE,
      metric == "cf" & year %in% c(1987,1992,1997,2002,2007,2012, 2017) ~ FALSE,
      metric == "ff" & year %in% knownfertyrs ~ FALSE,
      metric == "uf" & year %in% knownfertyrs ~ FALSE,
      metric == "lw" & year %in% c(1987,1992,1997,2002,2007,2012,2017) ~ FALSE,
      TRUE ~ TRUE
    )) 
  
  pwsin <- pin |>
    dplyr::filter(metric != 'cr') |>
    dplyr::mutate(estimated=dplyr::case_when(
      metric == "hw" ~ FALSE,
      metric == "lw" & year %in% c(1987,1992,1997,2002,2007,2012,2017) ~ FALSE,
      metric == "ff" & year %in% knownfertyrs ~ FALSE,
      metric == "uf" & year %in% knownfertyrs ~ FALSE,
      TRUE ~ TRUE
    ))
  
  #get ready for plot
  colorsn <- c('ff' = '#A3CC51', 'lw'='#B26F2C','hw'='#E51932','uf'='black', 'dep'='#6db6ff', 'cf'='#FFD700')
  colorsp <- c('ff' = '#A3CC51', 'lw'='#B26F2C','hw'='#E51932','uf'='black')
  
  nwsin$metric <- factor(nwsin$metric, levels = c('uf','hw','dep','lw','cf','ff'))
  pwsin$metric <- factor(pwsin$metric, levels = c('uf','hw','lw','ff'))
  
  
  #create titles with higher level include.nue param
  if (include.nue == TRUE){
    nbartitle <- 'b)'
    pbartitle <- 'd)'
    nuetitle <- 'a)'
    puetitle <- 'c)'
  } else{
    nbartitle <- 'a)'
    pbartitle <- 'b)'
    nuetitle <- ' '
    puetitle <- ' '
  }
  
  #create N bar plot
  nbar <- ggplot() + 
    ggpattern::geom_bar_pattern(data = nwsin, 
                     aes(x=year,y=value, fill=metric, 
                         pattern=factor(estimated, levels=c(TRUE,FALSE),
                                        labels=c('Estimated','Non-Estimated'))),
                     pattern = ifelse(nwsin$estimated, 'stripe','none'),
                     pattern_color='white',
                     pattern_density=0.05, 
                     pattern_fill = 'white', 
                     pattern_alpha = 0.5, 
                     pattern_spacing=0.025,
                     stat='identity', position='stack', 
                     pattern_size=0.05) +
    labs(title = 'Nitrogen (million kg)',
         y = "Budget",
         x = " ") +
    scale_fill_manual(values=colorsn,
                      labels = c('ff' = 'Farm Fertilizer',
                                 'uf' = 'Urban Fertilizer',
                                 'cf' = 'Crop N-Fixation',
                                 'lw' = 'Livestock Manure',
                                 'hw' = 'Human Waste',
                                 'dep' = 'Total Deposition')) + 
    scale_pattern_manual(name='Estimate Status',
                         values=c('Estimated'='stripe','Non-estimated'='none')) + 
    geom_line(data=ncrag, 
              aes(x=year,y=value, linetype=metric), 
              linewidth=1.25, color="black") + 
    scale_linetype_manual(values = 
                            c("ags"="solid", "cr"="dotted"),
                          labels = c('ags' = 'Agricultural Surplus',
                                     'cr' = 'Crop Removal')) +
    guides(fill=
             guide_legend(order=1, override.aes = list(pattern='none')), 
           pattern=
             guide_legend(order=2, override.aes = list(fill='grey')), 
           linetype=
             guide_legend(title=NULL)) + 
    scale_x_continuous(breaks=seq(1987,2017,by=5)) + 
    scale_color_manual(values=c("Agricultural Surplus"="black"), 
                       guide = 'none') + 
    theme_bw() + 
    theme(plot.title = element_text(size=9, face="bold"), 
          axis.title.y = element_text(size=9), 
          legend.background = element_rect(fill="white", colour = "black"), 
          legend.title = element_blank())
  
  #create p bar plot
  pbar <- ggplot() + 
    ggpattern::geom_bar_pattern(data = pwsin, 
                     aes(x=year,y=value, fill=metric, 
                         pattern=factor(estimated, levels=c(TRUE,FALSE),
                                        labels=c('Estimated','Non-Estimated'))), 
                     pattern = ifelse(pwsin$estimated, 'stripe','none'),
                     pattern_color='white',
                     pattern_density=0.05, 
                     pattern_fill = 'white', 
                     pattern_alpha = 0.5, 
                     pattern_spacing=0.025,
                     stat='identity', position='stack', pattern_size=0.05) +
    labs(title = 'Phosphorus (million kg)',
         y = "Budget",
         x = " ") +
    scale_fill_manual(values=colorsp) + 
    scale_pattern_manual(name='Estimate Status',
                         values=c('Estimated'='stripe',
                                  'Non-estimated'='none')) + 
    geom_line(data=pcrag, 
              aes(x=year,y=value, 
                  linetype=metric), 
              linewidth=1.25, color="black") + 
    scale_linetype_manual(values = 
                            c("ags"="solid", "cr"="dotted")) +
    guides(fill=
             guide_legend(order=1, override.aes = list(pattern='none')), 
           pattern=
             guide_legend(order=2, override.aes = list(fill='grey')), 
           linetype=
             guide_legend(title=NULL)) + 
    guides(fill="none", pattern = "none", linetype="none") +
    scale_x_continuous(breaks=seq(1987,2017,by=5)) + 
    scale_color_manual(values=c("Agricultural Surplus"="black"), 
                       guide = 'none') + 
    theme_bw() + 
    theme(plot.title = element_text(size=9, face="bold"), 
          axis.title.y = element_text(size=9), 
          legend.background = element_rect(fill="white", colour = "black"), 
          legend.title = element_blank())
  
  #create nue line plots
  nue <- ggplot() + 
    geom_line(data=nue, aes(x=year,y=nue), linewidth=1.25, color='seagreen')+
    theme_bw() + 
    scale_x_continuous(breaks=seq(1987,2017,by=5)) + 
    labs(title = 'Nitrogen Use Efficiency',
         y = "%", 
         x=" ") + 
    theme(plot.title = element_text(size=9, face="bold"), 
          axis.title.x = element_text(size=9), 
          axis.title.y = element_text(size=9)) 
  
  pue <- ggplot() + 
    geom_line(data=pue, aes(x=year,y=nue, lty='Nutrient Use Efficiency'), linewidth=1.25, color="seagreen") + 
    theme_bw() + 
    scale_x_continuous(breaks=seq(1987,2017,by=5)) + 
    labs(title = 'Phosphorus Use Efficiency',
         y = "%", 
         x="Year") + 
    theme(plot.title = element_text(size=9, face="bold"), 
          axis.title.x = element_text(size=9), 
          axis.title.y = element_text(size=9, hjust=0.5),
          legend.background = element_rect(fill="white", colour = "black"), 
          legend.title = element_blank())  +
    guides(fill="none", pattern = "none", linetype="none")
  
  #export final figure
  inputs <- patchwork::wrap_plots(nbar, pbar, ncol=1, guides="collect")
  nue <- patchwork::wrap_plots(nue, pue, ncol=1, guides="collect")
  
  if (include.nue == TRUE){
    timenni <- patchwork::wrap_plots(nue, inputs, ncol=2) 
  }
  else {
    timenni <- inputs 
  }
  
  return(timenni)
  
}
