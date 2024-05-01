
library(dplyr)
library(ubms)
library(MCMCvis)
library(ggplot2)

options(digits = 3)

#setwd("C:/Users/saraw/Documents/Panthera/GKE")
source("./R_Scripts/3_Data_Wrangling_Tidying/Detection_History/Create_Det_Hist_SaraW/make_int_fun.R")

# Example raw input data of target species; object is called raw_lion_dat
load("./Data/Example_Data/FromSara_MakeDetectionHistory/raw_lion_dat.Rdata") 

# Convert to one detection/non-detection value per station per day 
lion_occ <- raw_lion_dat %>%
    group_by(Station, Date) %>%
	mutate(Obs = max(Obs, na.rm = TRUE)) %>%
    mutate(Eff = sum(Eff, na.rm = TRUE)) %>%
    slice(1) %>%
	as.data.frame() 



# Different sampling occasion lengths to try out (in days)
samp_occ_l <- c(1, 3, 5, 7, 10, 14, 30)


# Loop through each sampling occasion length 
samp_occ_l_summ <- data.frame()
for(j in 1:length(samp_occ_l)){
     
    #  Sampling occasion specs
    survey_period <- "day" 
    number_of_periods <- samp_occ_l[j]  # how many days you want each survey to be
    print(paste(samp_occ_l[j], "day sampling occasion"))
    
    eff_int <- lion_occ %>%
    	select(Station, Date, Eff) %>%
    	as_tibble()
    eff_tmp1 <- make_interval(eff_int, Date, survey_period, number_of_periods)
    eff_tmp2 <- eff_tmp1 %>%
    	group_by(Station) %>%
    	mutate(survey_interval = interval - min(interval) + 1) %>%
    	ungroup()
    eff_tmp3 <- eff_tmp2 %>%
    	select(Station, Eff, survey_interval) %>%
    	group_by(survey_interval, Station) %>%
    	mutate(Camdays = sum(Eff, na.rm = TRUE)) %>%
    	slice(1) %>%
    	as.data.frame() %>%
        select(-Eff) 
    wide_eff <- eff_tmp3 %>%
    	arrange(survey_interval, Station) %>%
    	pivot_wider(id_cols = c(Station),
    		values_from = Camdays, names_from = survey_interval) %>%
    	as.data.frame()
    wide_eff[is.na(wide_eff)] <- 0	
    
    
    det_int <- lion_occ %>%
    	select(Station, Date, Obs) %>% 
    	as_tibble()
    det_tmp1 <- make_interval(det_int, Date, survey_period, number_of_periods) 
    det_tmp2 <- det_tmp1 %>%
    	group_by(Station) %>%
    	mutate(survey_interval = interval - min(interval) + 1) %>%
    	ungroup()
    det_tmp3 <- det_tmp2 %>%
    	select(Station, Obs, survey_interval) %>%  
    	group_by(survey_interval, Station) %>% 
    	mutate(Obs = max(Obs, na.rm = TRUE)) %>%
    	slice(1) %>%
    	as.data.frame() 	
    wide_det <- det_tmp3 %>%
    	arrange(survey_interval, Station) %>%
	    pivot_wider(id_cols = c(Station),
		values_from = Obs, names_from = survey_interval) %>%
	    as.data.frame()
    wide_det[wide_det == "-Inf"] <- NA

      
    #Double check that stations stayed in same order (all should be TRUE)
    table(wide_eff$Station == wide_det$Station)
    
    
    #  Make objects into format needed for 'ubms'
    effort <- wide_eff %>%
    	dplyr::select(-Station) %>% 
    	as.matrix()
    y <- wide_det %>% 
    	dplyr::select(-Station) %>% 
    	as.matrix()

    model_dat <- unmarkedFrameOccu(y = y, 
    	obsCovs = list(effort = effort)) 
    head(model_dat)

    
    # Fit basic model using stan_occu(); you could use unmarked here, but 'ubms'
    #  is the package you will need to run a "stacked" model where each row is
    #  a station-year 
    
    # Note that the example data I gave you is not "stacked" (each row is just a
    #  station, but if you did have station-year stacked data, you would include
    # (1|Station) as a random effect: stan_occu(~effort ~1 + (1|Station)
    # The trade off is that stan_occu takes longer
    fit <- stan_occu(~scale(effort) ~1, 
        data = model_dat, chains = 3, iter = 1000, cores = 3)
    
    # Pull out pieces of info to save from summary of model output
    fit_summ <- MCMCsummary(fit@stanfit)[1:3,]
    tmp <- data.frame(
        samp_occ_days = samp_occ_l[j],
        mean_occ = fit_summ[1,1], 
        lci_occ = fit_summ[1,3],
        uci_occ = fit_summ[1,5],
        mean_det = fit_summ[2,1], 
        lci_det = fit_summ[2,3],
        uci_det = fit_summ[2,5],
        mean_eff = fit_summ[3,1], 
        lci_eff = fit_summ[3,3],
        uci_eff = fit_summ[3,5])
  
    samp_occ_l_summ <- samp_occ_l_summ %>% rbind(tmp)
    
    rm(fit, tmp)
    }

samp_occ_l_summ

# Save summarized output of all sample occasion lengths
#save(samp_occ_l_summ, file = "fit_varying_samp_occ_l.Rdata")


# Visualize
ggplot(data = samp_occ_l_summ, 
        aes(y = mean_det, x = as.factor(samp_occ_days))) +
    geom_point(size = 2.5, shape = 19) +
    geom_errorbar(aes(ymin = lci_det, ymax = uci_det), 
        linewidth = 0.65, width = 0.15) +
    scale_y_continuous(limits = c(-4, 3)) +
    theme_bw() +
    xlab("Sampling occasion length (days)") +
    ylab("Detection probability (logit scale)") +
    theme(text = element_text(size = 14))
    
ggplot(data = samp_occ_l_summ, 
        aes(y = mean_eff, x = as.factor(samp_occ_days))) +
    geom_point(size = 2.5, shape = 19) +
    geom_errorbar(aes(ymin = lci_eff, ymax = uci_eff), 
        linewidth = 0.65, width = 0.15) +
    scale_y_continuous(limits = c(-4, 4)) +
    theme_bw() +
    xlab("Sampling occasion length (days)") +
    ylab("Influence of effort on detection probability") +
    theme(text = element_text(size = 14))


ggplot(data = samp_occ_l_summ, 
        aes(y = mean_occ, x = as.factor(samp_occ_days))) +
    geom_point(size = 2.5, shape = 19) +
    geom_errorbar(aes(ymin = lci_occ, ymax = uci_occ), 
        linewidth = 0.65, width = 0.15) +
    scale_y_continuous(limits = c(-4, 4)) +
    theme_bw() +
    xlab("Sampling occasion length (days)") +
    ylab("Occurrence probability (logit scale)") +
    theme(text = element_text(size = 14))

