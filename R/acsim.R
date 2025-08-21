 # Author: 03191657
###############################################################################
# 
# Age class simulator of Lauri Mehtätalo
# 
# copied from file vmisimul/simfunc4.R
# That file includes also a possibility for the video animation
# 
# Author: Lauri Mehtatalo
# Simuloidaan VMI-tulosten perusteella hakkuita
# vuoden ik�luokat, hakkkum��r� tavoitteen mukainen. 
# p��tehakkuut kohdeneetaan satunnisesti hakkuuik�isiin metsiin 
# osa p��tehakkuista yl�harvennuksia
# Argumnentit: age, area growth: vejtprit joissa on l�ht�aineiston ik�luokat (vuotta), 
#              ik�luokkien pinta-alat (hehtaaria) ja ko. ik�luokan mets�n kasvu (m3/ha/v)
# ProtectionLimit: skalaari, mik� on vanhin ik�luokka joka voidan p��tehakata
# 
# CutCriterion ja target: Kriteeri jonka perusteella hakkuut tehd��n  
# Seuraavat on mahdolisia
# CutCriterion        target
# ProportionOfGrowth  Poistuman osuus kokonaiskasvusta (vaihteluv�li 0-2, 1=hakataan t�sm�llen kasvun verran)
# HarvestVolume       poistuman tilavuus (milj m3/v, vaihteluv�li ES reheviss� metsiss� 0-50)
# AbsoluteSink        kasvun poistuman erotus (milj m3/v) vaihteluv�li 0-20
# ClearcutArea        p��tehakkuuala (ha/v) vaihteluv�li 0-200000
# RotationLength      p��tehakkuuik� (v), vaihteluv�li 0-200
# 
# title               kuvan otiskko
# simLength           simuloinnin pituus on simlength*protectionLimit
# startYear           mink� vuoden ik�jakauma annettiin tiedostossa area
# productSink         mik� osuus hakkuukertym�st� p��tyy tuotenieluun (0-1)
# thinningRule        kuinka usein harvennetaan ja mik� osuus tilavuudesta poistuu (k�yt� oletuksia)
## thinningLimit       vanhin met� jota voidaan harventaa (k�yt� oletusarvoa) POISTETTU
# ymax                tulostettavan kuvan y-akselin maksimiarvot pinta-alalle ja kasvulle
# xmax                kuvan x-akselin maksimi
# addTexts            tulostetanko kuvaan tekstit
# above               mik� osuus p��tehakkuista tehd��n yl�harvennuksina, k�yt� arvoa 0
# shift               kuinka paljon mets� nuorenee yl�harvennuksessa, k�yt� oletusta
# phi                 p��tehakkuiden ajoituksen jakaumaa kuvaavan eksponenttijakauman rate-parametri
# areasuo             suojeltujen metsien ik�jakauma. k�yt� oletusta (esi suojelualueita mukana)
# makeplot            tehd��nk� kuva
# cutSuo, mortality   suojelumetsiin liittyvi� parametreja.               

###############################################################################
LongTermSim <- function(age = NULL, 
                        area = NULL, 
                        growth = NULL, 
                        growthG = NA, 
                        protectionLimit = 120, #thinning=0.005,
                        cutCriterion = NULL, 
                        target = NULL, 
                        simLength = 2, 
                        startyear = 2020,
                        productSink = 0.05, 
                        thinningRule = c(0.019, 100), 
                        cutSuo = FALSE, 
                        mortality = 0.01,
                        phi = 1, 
                        above = 0, 
                        shift = 30, 
                        areasuo = NA) {


# productSink: which proportion of harvested volume contributes to net increase in product pool.
# i.e., ratio product sink / cuttings
# thinning rule: vector of lenggth two: thinning interval, and thinning removal (% of volume). 
# Vuotuinen harvennuspoistuma / ik�luokka 
#  pdf(file)
#  win.print(width=12,height=7)
#  dev.new(width=10,height=7)
  if (cutCriterion == "RotationLength" | cutCriterion == "ClearcutArea") {
    if (phi != 1) {
      cat(paste0("Warning: exp with phi = 1 required with", 
                 "cutCriterions 'ClearcutArea' or 'RotationLength'. Forcing.)", 
                 fill = TRUE))
      #exp<-TRUE # gaussian option rmvd 4/2025
      phi<-1
    }   
  }

  # Check arguments, do some logical checs
  if (is.null(cutCriterion) | (length(cutCriterion) == 1 & 
                               !(cutCriterion %in% c("RotationLength", 
                                "ClearcutArea", "HarvestVolume", 
                                "ProportionOfGrowth", "AbsoluteSink")))) {
    stop(paste0("Please specify desired cutting criterion: cutCriterion. ", 
              "For available alternatives, please refer to the documentation."))
  }

  if (is.null(age) | is.null(area) | is.null(growth)) {
    stop("Error: Invalid parameters. Please define age, area and growth.")
  }

  if (is.null(target)) {
    stop("Please specify desired harvesting level: target")
  }

  if (!is.numeric(simLength) || simLength <= 0) {
    stop("Please set simLength as an integer > 0.")
  }

  if (!is.numeric(protectionLimit) | any(!is.numeric(growth)) | 
    (any(!is.numeric(growthG)) & any(!is.na(growthG))) | any(!is.numeric(age))) {
      stop(paste0("Invalid argument detected. Please use numeric values for ", 
        "age, growth, growthG and protectionLimit"))
  }

  if (length(age) < simLength) { # TODO: extend vectors automatically?
    cat(paste0("Warning: age vector is shorter", 
      "than the number of sim. years!"), fill = TRUE)
  }
  nrep <- simLength # Modified 020425
  nclass <- length(age)

  # kvek: hakkuiden piiriss� olevien metsien kasvu
  # suojelui�n ylitt�vien talousmetsien kasvu (area, ik�>protectionlimit)
  # suojeltujen metsien kasvu (areasuo)
  kvek <- kvek2 <- kvek3 <- hvek <- mvek <- rep(0, nrep) # Initialize net growth
  kvekG <- kvek2G <- kvek3G <- rep(0, nrep) # Initialize gross growth
  ikajak <- matrix(0, ncol = nrep + 1, 
                  nrow = nclass + nrep) # init age distrib matrix, mineral
                  # is the "+ nrep" just for extra rows?
  ikajaksuo <- matrix(0, ncol = nrep + 1, # init age distrib matrix, mire
                  nrow = nclass + nrep)

  if (is.na(areasuo[1])) areasuo<-rep(0, length(area))
  if (cutSuo) {
              area<-area+areasuo
              areasuo<-rep(0,length(area))
              } 
  
  ikajak[1:nclass,1]<-area # set current state
  ikajaksuo[1:nclass,1]<-areasuo # set current state
  
  # Cuttings
  # Initilaize cuttin instruction
  k <- length(age)
  cuttingInstruction <- rep(0, k)
  # Determine thinning rules
  if (length(thinningRule) == length(age)) {
    cat("User-defined thinning rule activated.", fill = TRUE)
    cuttingInstruction[1:length(cuttingInstruction)] <- thinningRule 
  } else if (length(thinningRule) == 2) {
    #thinning <- 1-(1-thinningRule[2])^(1/thinningRule[1])
    thin_limit <- thinningRule[2] # assume 100 year limit for thinnings
    thinning <- thinningRule[1]# Lauris original, 1.9% of class volume 1 - (1 - 0.25)^(1 / 15) 
    cuttingInstruction[1:(thin_limit - 1)] <- thinning 
  } else {
    stop("Invalid thinningRule. Please give a vector of removals", "\n",
          "by age class or a volume curve and cumulative growth curve.")
  }

  # Initialize vectors
  V <- (1 - cuttingInstruction) * growth    # initialize, used only for year 1
  Vs <- (1 - mortality) * growth
  removals <- cuttingInstruction * growth # initialize, use only for year 1
  mortalityVek <- rep(0, k)
  mortalityVek[1:100] <- mortality
  removalss <- mortalityVek * growth # initialize, use only for year 1
  for (i in 2:k) {
      # volume (per ha) of age class i: volume of class i-1 + growth - removals
      V[i]<-(1-cuttingInstruction[i])*(V[i-1]+growth[i]) 
      removals[i]<-cuttingInstruction[i]*(V[i-1]+growth[i])
      Vs[i]<-(1-mortalityVek[i])*(Vs[i-1]+growth[i]) # suojelumetsan tilavuus iassa x
      removalss[i]<-mortalityVek[i]*(Vs[i-1]+growth[i])
  }
    
#  oldestClass<-protectionLimit
  overcut<-FALSE
  oldestClass<-max((1:length(area))[area>0])
#  cat(length(target),", ",nrep,"\n")
  if (length(target) == 1 | length(target) == nrep) {
     target <- rep(target, nrep)
	} else {
    stop("The length of target should equal to 1 or number of simulation years")
  }
  for (i in 1:nrep) { 
    # Growth of productive (kvek), old (kvek2) and productive forest
#    cat(i,"/",nrep,"\n")
    kvek[i]<-sum(area[1:oldestClass]*growth[1:oldestClass])/1e6
    kvek2[i]<-sum(area[-(1:oldestClass)]*growth[-(1:oldestClass)])/1e6
    kvek3[i]<-sum(areasuo*growth)/1e6 # suojelumetsien kasvu
    if (!all(is.na(growthG))) {
       kvekG[i]<-sum(area[1:oldestClass]*growthG[1:oldestClass])/1e6
       kvek2G[i]<-sum(area[-(1:oldestClass)]*growthG[-(1:oldestClass)])/1e6
       kvek3G[i]<-sum(areasuo*growthG)/1e6 # suojelumetsien kasvu
       }
    # compute thinning removals ..    
    hvek[i]<-sum(area*removals)/1e6 # Thinning removals
    mvek[i]<-sum(areasuo*removalss)/1e6 # mortality in protected forests
    # .. and find how much more m3 needs to be cut in final fellings
    # to meet the cutting criteria 
    # hakkuutavoite0 = removals from thinnings and clearcuts
    if (cutCriterion=="AbsoluteSink") {
        hakkuutavoite0<-max(kvek[i]+kvek2[i]+kvek3[i]-target[i],hvek[i])
        } else if (cutCriterion=="ProportionOfGrowth") {
        hakkuutavoite0<-(kvek[i]+kvek2[i]+kvek3[i])*target[i]       
        } else if (cutCriterion=="RotationLength") {
        hakkuutavoite0<-sum(area[-(1:(target[i]-1))]*V[-(1:(target[i]-1))])/1e6+hvek[i] 
        } else if (cutCriterion=="ClearcutArea") {
        tmp<-oldestClass
        end<-FALSE
        hakkuutavoite0<-hvek[i]
        hakkuutarve<-target[i]
        while (hakkuutarve>0&tmp>0) { 
              hakkuutavoite0<-hakkuutavoite0+pmin(hakkuutarve,area[tmp])*V[tmp]/1e6
              hakkuutarve<-hakkuutarve-area[tmp]
              tmp<-tmp-1
              }
        } else if (cutCriterion=="HarvestVolume") {
          hakkuutavoite0 <- max(target[i], hvek[i]) # Hakataan aina ainakin harvennukset, tämä pitäisi implemetopida myös muihin kriteereihin
        } else {
          stop("Invalid cutCriterion.")
        }
        
        # Todelinen 
        hakkuutavoite<-min(hakkuutavoite0,sum(area[3:oldestClass]*V[3:oldestClass])/1e6-hvek[i]) 
        if (hakkuutavoite<hakkuutavoite0) overcut<-TRUE 
    # The target volume for fellings has now  been set, 
    # so perform the cuttings now.
    # Remove so much volume from the oldest classes 
    # that the target is met. 
    
    jak<-sort(dexp(0:(oldestClass-1),phi))
    
    Vshift<-c(rep(0,shift),V)[1:oldestClass]
    ero<-function(alpha) {
         avovek<-pmin(alpha*jak,1)
         if (min(avovek)==1) return(NA)
         (1-above)*sum(avovek*area[1:oldestClass]*V[1:oldestClass])/1e6+ # avohakkuupoistuma +
         above*sum(pmin(alpha*jak,1)*area[1:oldestClass]*(V[1:oldestClass]-Vshift[1:oldestClass]))/1e6-  # yl�harvennuspoistuma +
         (hakkuutavoite-hvek[i])                                          # - tavoite
         }
         
    # jos met�sss� on puuta v�hint��n tavoitteen verran niin hakataan vanhimpia luokkia.  
       ub<-1
#       laskuri<-0
       while (ero(ub)<0) {
             ub<-2*ub 
#             print(laskuri<-laskuri+1)
             }
       alpha<-updown(0,ub,ero) #p��tehakataan pmin(alpha*jak,1) mottia kustakin ik�luokasta
       paatehakkuut<-(1-above)*pmin(alpha*jak,1)*area[1:oldestClass]
       ylaharv<-above*pmin(alpha*jak,1)*area[1:oldestClass]
       # poistetaan hakatut alat hakatuista ik�luokista 
#       area[1:oldestClass]<-area[1:oldestClass]-paatehakkuut-ylaharv+c(ylaharv[-(1:shift)],rep(0,shift)) # ikajakauma p��tehakkuiden j�lkeen
       area[1:oldestClass]<-area[1:oldestClass]-paatehakkuut-ylaharv+c(ylaharv[-(1:min(oldestClass,shift))],rep(0,min(oldestClass,shift))) # ikajakauma p��tehakkuiden j�lkeen
#       paatehakkuuala<-sum(paatehakkuut)+sum(ylaharv[1:shift]) # p��tehakattu pinta-ala
       paatehakkuuala<-sum(paatehakkuut)+sum(ylaharv[1:min(oldestClass,shift)]) # p��tehakattu pinta-ala
       hvek[i]<-hvek[i]+# hakkuukertym� alaharvennuksista 
                sum(paatehakkuut*V[1:oldestClass])/1e6+ # ja p��tehakkuista
                sum(ylaharv*(V[1:oldestClass]-Vshift[1:oldestClass]))/1e6 # ja yl�harvennuksista
       # growing
    #  print(i)
       area<-c(paatehakkuuala,area)
       areasuo<-c(0,areasuo)
       areasuo[-(1:(protectionLimit+1))]<-areasuo[-(1:(protectionLimit+1))]+area[-(1:(protectionLimit+1))]
       area[-(1:(protectionLimit+1))]<-0
       removals<-c(removals,0)
       removalss<-c(removalss,0)
       age<-c(age,max(age)+1)
       #oldestClass<-oldestClass+1
       oldestClass<-max((1:length(area))[area>0]) 
       k<-k+1
       growth<-c(growth,growth[length(growth)]) # uuden luokan kasvu on sama kuin vanhimman
       if (!all(is.na(growthG))) growthG<-c(growthG,growthG[length(growthG)]) # uuden luokan kasvu on sama kuin vanhimman
       V<-c(V,V[k-1]+growth[k])
       Vs<-c(Vs,Vs[k-1]+growth[k])
       ikajak[1:(nclass+i),i+1]<-area
       ikajaksuo[1:(nclass+i),i+1]<-areasuo
       }
    
    sink1<--hvek+kvek        # pystypuuston nielu, talousmets�
    sink2<-kvek2+kvek3       # pystypuuston nielu, suojelumets�
    sink3<-sink1+sink2       # pystypuuston kokonaisnielu
    sinkp1<-productSink*hvek # tuotenielu, talusmets�
    sinkp2<-productSink*0    # tuotenielu, suojelumets�
    sinkp3<-sinkp1+sinkp2    # tuotenielu yht 

    # kumulatiiviset puustonielut
    sink1c<-cumsum(sink1) 
    sink2c<-cumsum(sink2)
    sink3c<-cumsum(sink3)
    
    # kumulatiiviset kokonaisielut
    sinktot1c<-cumsum(sinkp1+sink1) 
    sinktot2c<-cumsum(sinkp2+sink2)
    sinktot3c<-cumsum(sinkp3+sink3) 
    
#    dev.off() 
  # Return # TODO
  return(list(ikajakTalous=ikajak,ikajakSuojellut=ikajaksuo,
       netgrowths=data.frame(nuoret=kvek,vanhat=kvek2,suojellut=kvek3),
       grossgrowths=data.frame(nuoret=kvekG,vanhat=kvek2G,suojellut=kvek3G),
       hakkuumaara=hvek,
       nielut=data.frame(puustoTalous=sink1,puustoSuojelu=sink2,puustoYht=sink3,
                         tuoteTalous=sinkp1,tuoteSuojelu=sinkp2,tuoteYht=sinkp3),
       nielutCum=data.frame(puustoTalous=sink1c,puustoSuojelu=sink2c,puustoYht=sink3c,
                            tuoteTalosu=sinktot1c,tuoteSuojelu=sinktot2c,tuoteYht=sinktot3c),
       growth=sum(kvek[1:nrep]),
	     growthCurve<-growth,
       removals=sum(hvek[1:nrep]),
       volume=V,
       volumeSuo=Vs,
       kasvuTal=kvek,
       kasvuSuo=kvek2+kvek3,
       mortSuo=mvek,
       varTal=sink1c,
       varSuo=sink2c,
       nieTal=sink1,
       nieSuo=sink2,
       overcut=overcut))
  }

           



