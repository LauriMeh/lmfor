# A helper function that predicts growth curve to a desired domain
# Note that the levels of the stratification variables (dummys) must be provided
# Curvetype: "cur", "past", "cum"
getGrowthCurve <- function(
    tmax = 300,
    maakunta = "Pohjois-Karjala",
    kasvupaikka = "Reheva",
    suo = "Kangas",
    type = c("Netto", "Brutto")[1], 
    mods = models,
    curvetype = "cur",
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
    
    if (!(curvetype %in% c("cur", "past", "cum"))) {
        stop("Error! Please specify a valid curvetype: cur, past or cum.")    
    }
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
        growthfun <- function(age,N,ltheta1,theta2,ir,slope1,slope2) {
                        theta1<-exp(ltheta1)
                        apply(cbind(age,N,theta1,theta2,ir),1,growthfun0)
                        }
    } else if (curvetype == "past") {
        # Growth of the last 5 years 
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
        assign("growthfun05", growthfun05, envir = .GlobalEnv)
        # Specifify growth function for the prediction stage
        growthfun <- function(age,N,ltheta1,theta2,ir,slope1,slope2) {
                        theta1<-exp(ltheta1)
                        slope1*pmax(0,6-age)+ # edellisen sukupolven puut
                        slope2*pmax(0,50-age)+ # jättöpuut
                        apply(cbind(age,N,theta1,theta2,ir),1,growthfun05)
                        }
    } else if (curvetype == "cum") {
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
        assign("growthFunCum", growthFunCum, envir = .GlobalEnv)
        # Specifify growth function for the prediction stage
        growthfun <- function(age,N,ltheta1,theta2,ir,slope1,slope2) {
                        theta1<-exp(ltheta1)
                        apply(cbind(age,N,theta1,theta2,ir),1,growthFunCum)
                        }
    } else {
        stop("Invalid curvetype.")
    }

    assign("growthfun", growthfun, envir = .GlobalEnv)

    pred <- predict(mods[[sel]], newdata = preddat)[-(1:length(maakunnat))]
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

getThinningCurve <- function(tmax_class = 300,
                        maakunta = "North Karelia",
                        kluok = "Fertile",
                        suo = "Mineral soil",
                        netgrowthmod = models$Net,
                        volmodel = modnlsVol0,
                        retrees = TRUE,
                        domain_stra = list(maakunta = c("auto"),
                                          kluok = c("Fertile", "Medium", 
                                                    "Unfertile", "Very unfertile"),
                                          suo = c("Mineral soil", "Peatland"))) {
    # # DBG with the Finnish data
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
        ika = c(rep(1, length(domain_stra$maakunta)), 1:tmax_class) # Requires re-thinking
    )

    if (!(any(maakunta == domain_stra$maakunta))) stop("Maakuntaa ei loydy")
    
    if (!(any(kluok == domain_stra$kluok))) stop("Kasvupaikkaa on oltava 'Reheva', 'Keskihyva', 'Karuhko' tai 'Karu'")
   
    if (!(any(suo == domain_stra$suo))) stop("'suo' on olatava 'Kangas' tai 'Turvemaa'")
    
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
        volfun <- function(age, lambda, ltheta1, theta2, ir, slope) {
            theta1 <- exp(ltheta1)
            slope*pmax(0,10-age)+ # jatto oletetaa koska cumsum
            apply(cbind(age, lambda, theta1, theta2, ir), 1, growthFunCum)
        }
    } else {
        # This is a cumulative version of the growth function
        # Do not consider the growth of retention trees
        volfun <- function(age, lambda, ltheta1, theta2, ir, slope) { 
            theta1 <- exp(ltheta1)
        #	slope*pmax(0,10-age)++ # jättöpuut oletetaa
            apply(cbind(age, lambda, theta1, theta2, ir), 1, growthFunCum)
        }
    }

    # Specifify growth function for the prediction, growth curve based volume
    growthfun <- function(age,N,ltheta1,theta2,ir,slope1,slope2) {
                    theta1<-exp(ltheta1)
                    apply(cbind(age,N,theta1,theta2,ir),1,growthFunCum)
                    }    

    assign("volfun", volfun, envir = .GlobalEnv)
    assign("growthFunCum", growthFunCum, envir = .GlobalEnv)
    assign("growthfun", growthfun, envir = .GlobalEnv)

    # Carry out predictions with the cumulative growth function
    # Predict volume by age class, based on the net growth curve
    cumnetgrowth <- predict(netgrowthmod, 
                        newdata = preddat)[-(1:length(domain_stra$maakunta))]

    pred_cur <- predict(volmodel, newdata = preddat)[-(1:length(domain_stra$maakunta))]
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
