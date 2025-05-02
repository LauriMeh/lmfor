# Input is a vector with elemets age, N, theta1, theta2 and ir. 
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

# Input is a vector with elemets age, N, theta1, theta2 and ir.
# Growth of the last 5 years 
# This is used in the model fitting phase
growthfun05<-function(input, T=5) {
           age<-input[1]
           N<-input[2]
           theta1<-input[3]
           theta2<-input[4]
           ir<-input[5]
           GPP<-theta1*(1-exp(-N/10000*pi*((1:age)*ir)^2))
           R<-0
           increment<-rep(NA,length(GPP))
           increment[1]<-GPP[1]
           if (age>1) {
              for (i in 2:age) {
                  R<-R+theta2*increment[i-1]
                  increment[i]<-GPP[i]-R
                  }
              }
           sum(increment[max(1,age-T+1):age])/T
}
           
growthpast5<-function(age,N,ltheta1,theta2,ir,slope1,slope2) {
             theta1<-exp(ltheta1)
             slope1*pmax(0,6-age)+ # edellisen sukupolven puut
             slope2*pmax(0,50-age)+ # jättöpuut
             apply(cbind(age,N,theta1,theta2,ir),1,growthfun05)
}   

# A function used to fit the growth curve
growthfun <- function(age, N, ltheta1, theta2, ir, slope1, slope2) {
               theta1 <- exp(ltheta1)
               apply(cbind(age, N, theta1, theta2, ir), 1, growthfun0)
}

# A cumulative version of the growth function, used in the fitting phase
# Takes into account the retention trees.
volfun<-function(age,lambda,ltheta1,theta2,ir,slope) { # funktio on sama kuin ennen
	theta1<-exp(ltheta1)
	slope*pmax(0,10-age)+ # jättöpuut oletetaa
	apply(cbind(age,lambda,theta1,theta2,ir),1,growthFunCum)
}

volfun0<-function(age,lambda,ltheta1,theta2,ir,slope) { 
	theta1<-exp(ltheta1)
#	slope*pmax(0,10-age)++ # jättöpuut oletetaa
	apply(cbind(age,lambda,theta1,theta2,ir),1,growthFunCum)
}

# A cumulative version of the growth function. This is used in the prediction
growthFunCum<-function(input) {
	age<-input[1]
	lambda<-input[2]
	theta1<-input[3]
	theta2<-input[4]
	ir<-input[5]
	GPP<-theta1*(1-exp(-lambda/10000*pi*((1:age)*ir)^2))
	R<-0
	increment<-rep(NA,length(GPP))
	increment[1]<-GPP[1]
	if (age>1) {
		for (i in 2:age) {
			R<-R+theta2*increment[i-1]
			increment[i]<-GPP[i]-R
		}
	}
	if (age>0) sum(increment) else 0
}

# A helper function that predicts growth curve to a desired domain
# Note that the levels of the stratification variables (dummys) must be provided
getGrowthCurve <- function(
    tmax = 300,
    maakunta = "Pohjois-Karjala",
    kasvupaikka = "Reheva",
    suo = "Kangas",
    type = c("Netto", "Brutto")[1], 
    mods = models,
    # maakunnat = c(
    #     "Uusimaa", "Varsinais-Suomi", "Satakunta", "Kanta-Hame",
    #     "Pirkanmaa", "Paijat-Hame", "Kymenlaakso", "Etela-Karjala",
    #     "Etela-Savo", "Pohjois-Savo", "Pohjois-Karjala", "Keski-Suomi",
    #     "Etela-Pohjanmaa", "Pohjanmaa", "Keski-Pohjanmaa", "Pohjois-Pohjanmaa",
    #     "Kainuu", "Lappi"
    # ),
    maakunnat = c("Uusimaa","Southwest Finland","Satakunta","Kanta-Hame",
				"Pirkanmaa","Paijat-Hame","Kymenlaakso","South Karelia",
				"South Savo","North Savo","North Karelia","Central Finland",
				"South Osthrobotnia","Ostrobothnia","Central Ostrobothnia","North Ostrobothnia",
				"Kainuu","Lapland"),    
    #kasvupaikat = c("Reheva", "Keskihyva", "Karuhko", "Karu"),
    kasvupaikat =c("Fertile","Medium","Unfertile","Very unfertile"),
    #suot = c("Kangas", "Turvemaa"),
    suot = c("Mineral soil", "Peatland")) {
    # predict.gls -funktiossa bugi, ja siksi newdatassa pitää esiintyä
    # luokitteluasteikollisten muuttujoen kaikkia tasoja.
    # siksi dataan pitää lisätä ne alkuun ja pudotetaan niiden ennusteeet lopuksi pois
    preddat <- data.frame(
        maakunta = factor(c(maakunnat, rep(maakunta, tmax)), levels = maakunnat),
        kluok = factor(c(rep(kasvupaikat, length = length(maakunnat)), rep(kasvupaikka, tmax)), levels = kasvupaikat),
        suo = factor(c(rep(suot, length = length(maakunnat)), rep(suo, tmax)), levels = suot),
        ika = c(rep(1, length(maakunnat)), 1:tmax)
    )
    if (!(any(type == c("Netto", "Brutto")))) stop("Argument 'type' must be ether 'Netto' or 'Brutto'")
    if (!(any(maakunta == maakunnat))) stop("Maakuntaa ei loydy")
    if (!(any(kasvupaikka == kasvupaikat))) stop("Kasvupaikkaa on oltava 'Reheva', 'Keskihyva', 'Karuhko' tai 'Karu'")
    if (!(any(suo == suot))) stop("'suo' on olatava 'Kangas' tai 'Turvemaa'")
    if (type == "Netto") sel <- 1 else sel <- 2
    #                growthfun<-attributes(mods)$growthfun
    #                growthfun0<-attributes(mods)$growthfun0
    pred <- predict(mods[[sel]], newdata = preddat)[-(1:length(maakunnat))]
    attr(pred, "MAI") <- cumsum(pred) / (1:tmax)
    pred
}

# domain_stra: A list of all possible stratification variables that define domains, eg. geographical areas, and fertility
#                    The names of list elements must match with categories used in the training data of the models!
#                    Alternatively, set stratification variables NULL to 
#                    automatically fetch variables from the model object. 
#                    For example, domains_stra = list(maakunta = "auto", kluok = c("Fertile" "Medium"))
#                     Be careful this because of the leveling of several stratifiers or intercept! Typically one level is missing.

getThinningCurve <- function(tmax_class = 300,
                        maakunta = "North Karelia",
                        kluok = "Fertile",
                        suo = "Mineral soil",
                        netgrowthmod = models$Net,
                        volmodel = modnlsVol0,
                        domain_stra = list(maakunta = c("auto"),
                                          kluok = c("Fertile", "Medium", 
                                                    "Unfertile", "Very unfertile"),
                                          suo = c("Mineral soil", "Peatland"))) {
    add_f_globen_g <- FALSE #dev tests; to be removed
    add_f_globen_v <- FALSE #dev tests; to be removed
    # TODO: target variables as a list!
    # # DBG
    # domain_stra <- list(
    #     maakunta = c(
    #         "Uusimaa", "Southwest Finland", "Satakunta", "Kanta-Hame",
    #         "Pirkanmaa", "Paijat-Hame", "Kymenlaakso", "South Karelia",
    #         "South Savo", "North Savo", "North Karelia", "Central Finland",
    #         "South Osthrobotnia", "Ostrobothnia",
    #         "Central Ostrobothnia", "North Ostrobothnia",
    #         "Kainuu", "Lapland"
    #     ),
    #     kluok = c("Fertile", "Medium", "Unfertile", "Very unfertile"),
    #     suo = c("Mineral soil", "Peatland")
    # )
    # stratification variables
    strat_vs <- names(domain_stra)

    # Function to split strings based on the pattern, using base R
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
            cat(paste0(length(add_vec), " ", strat_vs[i], 
            " levels/categories found from the user-defined vector."), 
            fill = TRUE)
            next
        }
        # If automatic search was requested
        m_coef <- netgrowthmod |> coef() |> names()
        string_oi_bool <- m_coef |> grepl(pattern = strat_vs[i])
        m_coef <- m_coef[string_oi_bool]
        sel <- split_strings(m_coef, strat_vs[i], 2) |> unique()
        add_vec <- sel
        dom_stra_up[[i]] <- add_vec # add to the list
        cat(paste0(length(add_vec), " ",strat_vs[i], 
            " levels/categories found from the model object."), fill = TRUE)
    }
    names(dom_stra_up) <- strat_vs
    domain_stra <- dom_stra_up # Replace

    # predict.gls -funktiossa bugi, ja siksi newdatassa pitää esiintyä
    # luokitteluasteikollisten muuttujoen kaikkia tasoja.
    # siksi dataan pitää lisätä ne alkuun ja pudotetaan niiden ennusteeet lopuksi pois
    #  note: This adds just dummy rows, because the predic function requires all levels
    # The first length(maakunta) rows will be omitted after the predict call
    # TODO: Toimii talla datalla, mutta tassa tulee ongelmia jos 
    # maakunta-vektori on lyhempi kuin muut 
    # Taman voisi tehda niin, etta ensiksi muodostetaan taydellinen grid
    # ja lopuksi filtteroida mieluisat ennusteet
    preddat <- data.frame(
        maakunta = factor(c(domain_stra$maakunta, rep(maakunta, tmax_class)), levels = domain_stra$maakunta),
        kluok = factor(c(rep(domain_stra$kluok, length = length(domain_stra$maakunta)), 
        rep(kluok, tmax_class)), levels = domain_stra$kluok),
        suo = factor(c(rep(domain_stra$suo, length = length(domain_stra$maakunta)), 
        rep(suo, tmax_class)), levels = domain_stra$suo),
        ika = c(rep(1, length(domain_stra$maakunta)), 1:tmax_class)
    )

    if (!(any(maakunta == domain_stra$maakunta))) stop("Maakuntaa ei loydy")
    
    if (!(any(kluok == domain_stra$kluok))) stop("Kasvupaikkaa on oltava 'Reheva', 'Keskihyva', 'Karuhko' tai 'Karu'")
   
    if (!(any(suo == domain_stra$suo))) stop("'suo' on olatava 'Kangas' tai 'Turvemaa'")
    
    # # Used by the predict function
    # growthFunCum <- function(input) {
    #     age<-input[1]
    #     lambda<-input[2]
    #     theta1<-input[3]
    #     theta2<-input[4]
    #     ir<-input[5]
    #     GPP<-theta1*(1-exp(-lambda/10000*pi*((1:age)*ir)^2))
    #     R<-0
    #     increment<-rep(NA,length(GPP))
    #     increment[1]<-GPP[1]
    #     if (age>1) {
    #         for (i in 2:age) {
    #             R<-R+theta2*increment[i-1]
    #             increment[i]<-GPP[i]-R
    #         }
    #     }
    #     if (age>0) sum(increment) else 0
    # }

    # Function used in the model fitting, growthfun0 to be replaced

    # if (!exists("growthfun", envir = .GlobalEnv)) {
    #     growthfun <<- function(age, N, ltheta1, theta2, ir, slope1, slope2) {
    #                   theta1 <- exp(ltheta1)
    #                   apply(cbind(age, N, theta1, theta2, ir), 1, growthfun0)
    #     }
    #     add_f_globen_g <- TRUE
    # } else {
    #     stop(paste0("Stopping: growthfun found in .GlobalEnv. ", 
    #                 " Is this the right one?"))
    # }


    # Used by the predict function
    volfun0 <- function(age, lambda, ltheta1, theta2, ir, slope) { 
        theta1 <- exp(ltheta1)
    #	slope*pmax(0,10-age)++ # jättöpuut oletetaa
        apply(cbind(age, lambda, theta1, theta2, ir), 1, growthFunCum)
    }
    
    # Use cumulative in the pred function
    #growthfun0 <- growthFunCum  # Not working?
    growthfun <- function(age, N, ltheta1, theta2, ir, slope1, slope2) {
                theta1 <- exp(ltheta1)
                apply(cbind(age, N, theta1, theta2, ir), 1, growthFunCum)
    }

    # Carry out predictions with the cumulative growth function
    # Predict volume by age class, based on the net growth curve
    cumnetgrowth <- predict(netgrowthmod, 
                            newdata = preddat)[-(1:length(domain_stra$maakunta))]
    # Predict using volume models, current volume
    # The volfun must be in the global environ for the predict function
    # if (!exists("volfun", envir = .GlobalEnv)) {
    #     volfun <<- volfun0
    #     add_f_globen_v <- TRUE
    # } else {
    #     stop(paste0("Stopping: volfun found in .GlobalEnv. ", 
    #                 " Is this the right one? Please remove it."))
    # }
    
    pred_cur <- predict(modnlsVol0, newdata = preddat)[-(1:length(domain_stra$maakunta))]
    thin_recipe <- diff(cumnetgrowth - pred_cur) # Cum thin

    thin_recipe_p <- thin_recipe / cumnetgrowth[-1] # the proportion of volume given by curve

    thin_recipe <- c(0, thin_recipe) # match length with age
    thin_recipe_p <- c(0, thin_recipe_p)
    
    if (add_f_globen_g) {
        rm("growthfun", envir = .GlobalEnv)
    }
    if (add_f_globen_v) {
        rm("volfun", envir = .GlobalEnv)
    }
    
    # Outputs
    return(list(predvol_curve = cumnetgrowth, 
                predvol_volmod = pred_cur, 
                diff_a = thin_recipe,
                diff_p = thin_recipe_p))
}
