# A helper function that predicts growth curve to a desired domain
# Note that the levels of the stratification variables (dummys) must be provided
# Curvetype: "cur", "past", "cum"
# Arguments
# growf: Used defined growth function. If NULL, default formulation will be used.
getGrowthCurve <- function(
		tmax = 300,
		region = NULL,
		fert = NULL,
		L1 = NULL,
		NFI = "notspecified",
		type = c("Net", "Gross")[1], 
		mods = NULL,
		curvetype = "cur",
		regions = NULL,    
		ferts = NULL,
		L1s = NULL,
        NFIs = "notspecified",
   		growf = NULL,
		age_name = NULL, # character name for age used in the mod fitting
		fert_name = "fert", # character name for fert used in the mod fitting
		L1_name = "L1", # character name for L1 used in the mod fitting
		NFI_name = "NFI", # character name for L1 used in the mod fitting
		grreg_name = "region" # character name for growth used in the mod fit
) {
	### Example arguments
	# tmax = 300,
	# region = "Central-South",
	# fert = "average",
	# L1 = "Alpine coniferous",
	# NFI = "NNFI-1418/NNFI-1923",
	# type = c("Net", "Gross")[1], 
	# mods = NULL,
	# curvetype = "cur",
	# regions = c("Central-South","East","Finnmark","North","West"),    
	# ferts = c("average","good","poor"),
	# L1s= c("Alpine coniferous","boreal","Broadleaved forest",                                             
	# 		"Hemiboreal, nemoral coniferous and mixed broadleaved-coniferous",
	# 		"Mire and swamp forest", "Plantations and self-sown exotic forest"),
	# NFIs= c("NNFI-1115/NNFI-1418","NNFI-1418/NNFI-1923")
	###

	if (is.null(age_name)) {
		stop(paste0("Please set argument 'age_name' to define age" , 
		"variable used in model fitting"))
	}

	if (is.null(mods)) {
		stop("Please provide growth model.")
	}

	if (is.null(region) | is.null(fert) | is.null(L1)) {
		stop("Please provide target domain specifications. NFI optional.")
	}

	if (is.null(regions) | is.null(ferts) | is.null(L1s)) {
		stop(paste0("Please provide all domain specifications. ", 
					"NFI is optional. Needed for prediction."))
	}

	if (!(curvetype %in% c("cur", "past", "cum"))) {
		stop("Error! Please specify a valid curvetype: cur, past or cum.")    
	}
	# predict.gls -funktiossa bugi, ja siksi newdatassa pitää esiintyä
	# luokitteluasteikollisten muuttujoen kaikkia tasoja.
	# siksi dataan pitää lisätä ne alkuun ja pudotetaan niiden ennusteeet lopuksi pois
	maxlev <- max(length(regions), length(L1s), length(NFIs), length(ferts))
	preddat <- data.frame(
		region = factor(c(rep(regions, length = maxlev), rep(region, tmax)), levels = regions),
		fert = factor(c(rep(ferts, length = maxlev), rep(fert, tmax)), levels = ferts),
		L1 = factor(c(rep(L1s, length = maxlev), rep(L1, tmax)), levels = L1s),
		NFI = factor(c(rep(NFIs, length = maxlev), rep(NFI, tmax)), levels = NFIs),
		age_ub = c(rep(1, maxlev), 1:tmax)
	) %>%
	  rename(!!age_name := age_ub, 
	  !!fert_name := fert, 
	  !!L1_name := L1, 
	  !!grreg_name := region,
	  !!NFI_name := NFI
	  )
	
	if (!(any(type == c("Net", "Gross")))) stop("Argument 'type' must be ether 'Net' or 'Gross'")
	if (!(any(region == regions))) stop("Maakuntaa ei loydy")
	if (!(any(fert == ferts))) stop("Kasvupaikkaa on oltava 'Reheva', 'Keskihyva', 'Karuhko' tai 'Karu'")
	if (!(any(L1 == L1s))) stop("'suo' on oltava 'Kangas' tai 'Turvemaa'")
	if (type == "Net") sel <- 1 else sel <- 2
	
	# Do checkups for the global environment
	# Functions must be present in the global environment
	# Ensure that there are no existing functions with same names
	if (!exists("growthfun", envir = .GlobalEnv)) {
		growthfun_bup <- NULL
	} else { # if already exists, backups
		cat("growthfun exists in the .GlobalEnv. Omitting, taking back-ups.", fill = TRUE)
		growthfun_bup <- growthfun
		rm(growthfun, envir = .GlobalEnv)
	}
	
	if (!exists("growthfun0", envir = .GlobalEnv)) {
		growthfun0_bup <- NULL
	} else { # if already exists, backups
		cat("growthfun0 exists in the .GlobalEnv. Omitting, taking back-ups.", fill = TRUE)
		growthfun0_bup <- growthfun0
		rm(growthfun0, envir = .GlobalEnv)
	}
	
	if (!exists("growthfun05", envir = .GlobalEnv)) {
		growthfun05_bup <- NULL
	} else { # if already exists, backups
		cat("growthfun05 exists in the .GlobalEnv. Omitting, taking back-ups.", fill = TRUE)
		growthfun05_bup <- growthfun05
		rm(growthfun05, envir = .GlobalEnv)
	}
	
	if (!exists("growthFunCum", envir = .GlobalEnv)) {
		growthFunCum_bup <- NULL
	} else { # if already exists, backups
		cat("growthFunCum exists in the .GlobalEnv. Omitting, taking back-ups.", fill = TRUE)
		growthFunCum_bup <- growthFunCum
		rm(growthFunCum, envir = .GlobalEnv)
	}
	
	# Prepare functions by curve types 
	if (curvetype == "cur") {
		# Growth of the current year
		growthfun0<-function(input) {
			age<-input[1]
			N<-input[2]
			theta1<-input[3]
			theta2<-input[4]
			ir<-input[5]
			GPP<-theta1*(1-exp(-N/10000*pi*((1:age)*ir)^2))
			R<-0
			increment<-GPP[1]
			if (age>1) {
				for (i in 2:age) {
					R<-R+theta2*increment
					increment<-GPP[i]-R
				}
			}
			increment
		}
		assign("growthfun0", growthfun0, envir = .GlobalEnv)
		
		# Specifify growth function for the prediction stage
        # Allow user to define if desired
        if (is.null(growf)) { # Use default if not given
            growthfun <- function(age, N, ltheta1, theta2, ir, slope1, slope2) {
                theta1 <- exp(ltheta1)
                apply(cbind(age, N, theta1, theta2, ir), 1, growthfun0)
            }
        } else {
            #cat("Using user-defined growth function", fill = TRUE)
            growthfun <- growf
        }
		
	} else if (curvetype == "past") {
		# Growth of the last 5 years
		growthfun05 <- function(input, T = 5) {
			age <- input[1]
			N <- input[2]
			theta1 <- input[3]
			theta2 <- input[4]
			ir <- input[5]
			GPP <- theta1 * (1 - exp(-N / 10000 * pi * ((1:age) * ir)^2))
			R <- 0
			increment <- rep(NA, length(GPP))
			increment[1] <- GPP[1]
			if (age > 1) {
				for (i in 2:age) {
					R <- R + theta2 * increment[i - 1]
					increment[i] <- GPP[i] - R
				}
			}
			sum(increment[max(1, age - T + 1):age]) / T
		}
		assign("growthfun05", growthfun05, envir = .GlobalEnv)
        # Allow user to define if desired
        if (is.null(growf)) { # Use default if not given
            growthfun <- function(age, N, ltheta1, theta2, ir, slope1, slope2) {
                theta1 <- exp(ltheta1)
                slope1 * pmax(0, 6 - age) + # edellisen sukupolven puut
                    slope2 * pmax(0, 50 - age) + # jättöpuut
                    apply(cbind(age, N, theta1, theta2, ir), 1, growthfun05)
            }
        } else {
            #cat("Using user-defined growth function ", fill = TRUE)
            growthfun <- growf
        }
	} else if (curvetype == "cum") {
		# A cumulative version of the growth function. This is used in the prediction
		growthFunCum <- function(input) {
			age <- input[1]
			lambda <- input[2]
			theta1 <- input[3]
			theta2 <- input[4]
			ir <- input[5]
			GPP <- theta1 * (1 - exp(-lambda / 10000 * pi * ((1:age) * ir)^2))
			R <- 0
			increment <- rep(NA, length(GPP))
			increment[1] <- GPP[1]
			if (age > 1) {
				for (i in 2:age) {
					R <- R + theta2 * increment[i - 1]
					increment[i] <- GPP[i] - R
				}
			}
			if (age > 0) sum(increment) else 0
		}
		assign("growthFunCum", growthFunCum, envir = .GlobalEnv)
		# Specifify growth function for the prediction stage
        # Allow user to define if desired
        if (is.null(growf)) { # Use default if not given
            growthfun <- function(age, N, ltheta1, theta2, ir, slope1, slope2) {
                theta1 <- exp(ltheta1)
                theta2 <- exp(ltheta2)
                apply(cbind(age, N, theta1, theta2, ir), 1, growthFunCum)
            }
        } else {
            growthfun <- growf
        }
	} else {
		stop("Invalid curvetype.")
	}
	
	assign("growthfun", growthfun, envir = .GlobalEnv)
	
	pred <- predict(mods[[sel]], newdata = preddat)[-(1:maxlev)]
	attr(pred, "MAI") <- cumsum(pred) / (1:tmax)
	
	# Check if glob env was modified, back-modify if modified
	if (!is.null(growthfun_bup)) {
		assign("growthfun", growthfun_bup, envir = .GlobalEnv)
	} else { # remove if it was assigned
		rm(growthfun, envir = .GlobalEnv)
	}
	
	# Check if glob env was modified, back-modify if modified
	if (curvetype == "cur" & !is.null(growthfun0_bup)) {
		assign("growthfun0", growthfun0_bup, envir = .GlobalEnv)
	} else if (curvetype == "cur" & 
			is.null(growthfun0_bup)) { # remove if it was assigned
		rm(growthfun0, envir = .GlobalEnv)
	} else {
		TRUE
	}
	
	# Check if glob env was modified, back-modify if modified
	if (!is.null(growthfun05_bup)) {
		assign("growthfun05", growthfun05_bup, envir = .GlobalEnv)
	} else if (curvetype == "past" & 
			is.null(growthfun05_bup)) { # remove if it was assigned
		rm(growthfun05, envir = .GlobalEnv)
	} else {
		TRUE
	}
	
	# Check if glob env was modified, back-modify if modified
	if (!is.null(growthFunCum_bup)) {
		assign("growthFunCum", growthFunCum_bup, envir = .GlobalEnv)
	} else if (curvetype == "cum" & 
			is.null(growthFunCum_bup)) { # remove if it was assigned
		rm(growthFunCum, envir = .GlobalEnv)
	} else {
		TRUE
	}
	# Returns
	return(pred)
}

# domain_stra: A list of all possible stratification variables that define domains, eg. geographical areas, and fertility
#                    The names of list elements must match with categories used in the training data of the models!
#                    Alternatively, set stratification variables NULL to 
#                    automatically fetch variables from the model object. 
#                    For example, domains_stra = list(maakunta = "auto", kluok = c("Fertile" "Medium"))
#                     Be careful this because of the leveling of several stratifiers or intercept! Typically one level is missing.

getThinningCurve <- function(
		tmax_class = 300,
		region = NULL,
		fert = NULL,
		L1 = NULL,
		NFI = NULL,
		netgrowthmod = growthNorway$Net,
		volmodel = volNorway,
		retrees = TRUE,
		domain_stra = list(region = NULL,
				fert = NULL,
				L1 = NULL,
				NFI = NULL),
		volf = NULL,
		growf = NULL,
		age_name = "age", # character name for age used in the mod fitting
		fert_name = "fert", # character name for fert used in the mod fitting
		L1_name = "L1", # character name for L1 used in the mod fitting
		NFI_name = "NFI", # character name for L1 used in the mod fitting
		grreg_name = "region" # character name for growth used in the mod fit
) {
	# Example args
	# tmax_class = 300,
	# region = "Central-South",
	# fert = "average",
	# L1 = "Alpine coniferous",
	# NFI = "NNFI-1418/NNFI-1923",
	# netgrowthmod = growthNorway$Net,
	# volmodel = volNorway,
	# retrees = TRUE,
	# domain_stra = list(region = c("Central-South","East","Finnmark","North","West"),
	# 		fert = c("average","good","poor"),
	# 		L1 = c("Alpine coniferous","boreal","Broadleaved forest",                                             
	# 				"Hemiboreal, nemoral coniferous and mixed broadleaved-coniferous",
	# 				"Mire and swamp forest", "Plantations and self-sown exotic forest"),
	# 		NFI = c("NNFI-1115/NNFI-1418","NNFI-1418/NNFI-1923")),
	# volf = NULL,
	# growf = NULL
	if (is.null(netgrowthmod)) {
		stop("Please provide growth model.")
	}

	if (is.null(region) | is.null(fert) | is.null(L1) | is.null(NFI)) {
		stop("Please provide target domain specifications.")
	}

	if (is.null(domain_stra$region) | is.null(domain_stra$fert) | 
		is.null(domain_stra$L1) | is.null(domain_stra$NFI)) {
		stop("Please provide all domain specifications. Needed for prediction.")
	}
	# stratification variables
	strat_vs <- names(domain_stra)
	
	# Function to split strings based on the pattern, using base R
	# This is used to catch domains from the model object
	split_strings <- function(strings, pattern, element) {
		result <- lapply(strings, function(str) {
					split_result <- strsplit(str, pattern)[[1]] # strsplit returns a list
					split_result <- unlist(split_result)[element]
					return(split_result)
				})
		return(unlist(result))
	}   
	
	dom_stra_up <- list() # Collector
	for (i in seq_along(strat_vs)) {
		# Check if automatic selection requested, use user-defined otherwise
		if (domain_stra[[strat_vs[i]]][1] != "auto") {
			add_vec <- domain_stra[[strat_vs[i]]] # user input
			dom_stra_up[[i]] <- add_vec # add to the list
#            cat(paste0(length(add_vec), " ", strat_vs[i], 
#            " levels/categories found from the user-defined vector."), 
#            fill = TRUE)
			next
		}
		# If automatic search was requested
		m_coef <- netgrowthmod |> coef() |> names()
		string_oi_bool <- m_coef |> grepl(pattern = strat_vs[i])
		m_coef <- m_coef[string_oi_bool]
		sel <- split_strings(m_coef, strat_vs[i], 2) |> unique()
		add_vec <- sel
		dom_stra_up[[i]] <- add_vec # add to the list
#        cat(paste0(length(add_vec), " ",strat_vs[i], 
#            " levels/categories found from the model object."), fill = TRUE)
	}
	names(dom_stra_up) <- strat_vs
	domain_stra <- dom_stra_up # Replace
	
	# predict.gls -funktiossa bugi, ja siksi newdatassa pitää esiintyä
	# luokitteluasteikollisten muuttujoen kaikkia tasoja.
	# siksi dataan pitää lisätä ne alkuun ja pudotetaan niiden ennusteeet lopuksi pois
	#  note: This adds just dummy rows, because the predic function requires all levels
	# The first length(maakunta) rows will be omitted after the predict call
	maxlev<-max(sapply(domain_stra,length))
	preddat <- data.frame(
		region = factor(c(rep(domain_stra$region, length = maxlev), 
		  rep(region, tmax_class)), levels = domain_stra$region),
		fert = factor(c(rep(domain_stra$fert, length = maxlev), 
		  rep(fert, tmax_class)), levels = domain_stra$fert),
		L1 = factor(c(rep(domain_stra$L1, length = maxlev), 
		  rep(L1, tmax_class)), levels = domain_stra$L1),
		NFI = factor(c(rep(domain_stra$NFI, length = maxlev), 
		  rep(NFI, tmax_class)), levels = domain_stra$NFI),
		age_ub = c(rep(1, maxlev), 1:tmax_class)
	) %>%
	  rename(!!age_name := age_ub, 
	  !!fert_name := fert, 
	  !!L1_name := L1, 
	  !!grreg_name := region,
	  !!NFI_name := NFI
	  )

	if (!(any(region == domain_stra$region))) stop("Maakuntaa ei loydy")
	
	if (!(any(fert == domain_stra$fert))) stop("Kasvupaikkaa on oltava 'Reheva', 'Keskihyva', 'Karuhko' tai 'Karu'")
	
	if (!(any(L1 == domain_stra$L1))) stop("'suo' on olatava 'Kangas' tai 'Turvemaa'")

	if (!(any(NFI == domain_stra$NFI))) stop("'suo' on olatava 'Kangas' tai 'Turvemaa'")
	
	# Do checkups for the global environment
	# volfun  must be present in the global environment while prediction
	if (!exists("volfun", envir = .GlobalEnv)) {
		volfun_bup <- NULL
	} else { # if already exists, backups
		cat("volfun exists in the .GlobalEnv. Omitting, taking back-ups.", 
				fill = TRUE)
		volfun_bup <- volfun
		rm(volfun, envir = .GlobalEnv)
	}
	
	# Do the same for 
	if (!exists("growthFunCum", envir = .GlobalEnv)) {
		growthFunCum_bup <- NULL
	} else { # if already exists, backups
		cat("growthFunCum exists in the .GlobalEnv. Omitting, taking back-ups.", 
				fill = TRUE)
		growthFunCum_bup <- growthFunCum
		rm(growthFunCum, envir = .GlobalEnv)
	}    
	
	# Do the same for growthfun
	if (!exists("growthfun", envir = .GlobalEnv)) {
		growthfun_bup <- NULL
	} else { # if already exists, backups
		cat("growthfun exists in the .GlobalEnv. Omitting, taking back-ups.", 
				fill = TRUE)
		growthfun_bup <- growthfun
		rm(growthfun, envir = .GlobalEnv)
	}    
	
	# A cumulative version of the growth function. This is used in the prediction
	growthFunCum <- function(input) {
		age <- input[1]
		lambda <- input[2]
		theta1 <- input[3]
		theta2 <- input[4]
		ir <- input[5]
		GPP <- theta1 * (1 - exp(-lambda / 10000 * pi * ((1:age) * ir)^2))
		R <- 0
		increment <- rep(NA, length(GPP))
		increment[1] <- GPP[1]
		if (age > 1) {
			for (i in 2:age) {
				R <- R + theta2 * increment[i - 1]
				increment[i] <- GPP[i] - R
			}
		}
		if (age > 0) sum(increment) else 0
	}
	if (retrees) {
        # This is a cumulative version of the growth function
        # Note: the growth of retention trees for ten years.
        if (is.null(volf)) {
            volfun <- function(age, lambda, ltheta1, theta2, ir, slope) {
                theta1 <- exp(ltheta1)
                slope * pmax(0, 10 - age) + # jatto oletetaa koska cumsum
                apply(cbind(age, lambda, theta1, theta2, ir), 1, growthFunCum)
            }
        } else {
            volfun <- volf
        }
	} else {
        # This is a cumulative version of the growth function
        # Do not consider the growth of retention trees
        if (is.null(volf)) {
            volfun <- function(age, lambda, ltheta1, theta2, ir, slope) { 
                theta1 <- exp(ltheta1)
            #	slope*pmax(0,10-age)++ # jättöpuut oletetaa
                apply(cbind(age, lambda, theta1, theta2, ir), 1, growthFunCum)
            }
        } else {
            volfun <- volf
        }
	}
	
    # Specifify growth function for the prediction, growth curve based volume
	if (is.null(growf)) {
		growthfun <- function(age,N,ltheta1,theta2,ir,slope1,slope2) {
                        theta1 <- exp(ltheta1)
						#theta2 <- exp(theta2)
                        apply(cbind(age, N, theta1, theta2, ir), 1, growthFunCum)
                        }    
	} else {
		growthfun <- growf
	}

	assign("volfun", volfun, envir = .GlobalEnv)
	assign("growthFunCum", growthFunCum, envir = .GlobalEnv)
	assign("growthfun", growthfun, envir = .GlobalEnv)
	
	# Carry out predictions with the cumulative growth function
	# Predict volume by age class, based on the net growth curve
	cumnetgrowth <- predict(netgrowthmod, 
			newdata = preddat)[-(1:maxlev)]
	
	pred_cur <- predict(volmodel, newdata = preddat)[-(1:maxlev)]
	thin_recipe <- diff(cumnetgrowth - pred_cur) # Cum thin
	
	thin_recipe_p <- thin_recipe / 
			cumnetgrowth[-length(cumnetgrowth)] # the proportion of volume removed in a year
	# in terms of cumnetgrowth at age0
	# match length with age, 
	# add zero-thinning for age class zero, assuming no thinnings for zero age
	# Note: age classes start from zero
	# Repeat for last class 
	thin_recipe <- c(0, thin_recipe, thin_recipe[length(thin_recipe)]) 
	thin_recipe_p <- c(0, thin_recipe_p, thin_recipe_p[length(thin_recipe_p)])
	
	# Check if glob env was modified, back-modify if modified
	if (!is.null(volfun_bup)) {
		assign("volfun", volfun_bup, envir = .GlobalEnv)
	} else { # remove if it was assigned
		rm(volfun, envir = .GlobalEnv)
	}
	
	if (!is.null(growthFunCum_bup)) {
		assign("growthFunCum", growthFunCum_bup, envir = .GlobalEnv)
	} else { # remove if it was assigned
		rm(growthFunCum, envir = .GlobalEnv)
	}    
	
	if (!is.null(growthfun_bup)) {
		assign("growthfun", growthfun_bup, envir = .GlobalEnv)
	} else { # remove if it was assigned
		rm(growthfun, envir = .GlobalEnv)
	}        
	
	# Outputs
	return(list(predvol_curve = cumnetgrowth, 
					predvol_volmod = pred_cur, 
					diff_a = thin_recipe,
					diff_p = thin_recipe_p))
}

# Smoothing function; weighted moving average on a numeric vector
tas <- function(x, lambda = 5, weights = rep(1 / lambda, lambda)) {
    n <- length(x)
    dif <- floor(lambda / 2)
    xmat <- matrix(0, nrow = n + 2 * dif, ncol = lambda)
    for (i in 1:lambda) {
        xmat[(1:n) + i - 1, i] <- x
    }
    xtas <- apply(xmat, 1, function(x) sum(weights * x))
    #     xtas[dif+1]<-xtas[dif+1]+sum(xtas[1:dif])
    xtas[(dif + 1):(2 * dif)] <- xtas[(dif + 1):(2 * dif)] + xtas[dif:1]
    xtas[(n + 1):(n + dif)] <- xtas[(n + 1):(n + dif)] + xtas[-(1:(dif + n))][dif:1]
    xtas <- xtas[(1:length(x)) + dif]
}
