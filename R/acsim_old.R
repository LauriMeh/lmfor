# TODO: Add comment
# 
# Author: 03191657
###############################################################################
# 
# Age class simulator of Lauri Mehtätalo
# 
# copied from file vmisimul/simfunc4.R
# That file includes also a possibility for the video animation
# 
# Author: Lauri Meht�talo
# Simuloidaan VMI-tulosten perusteella hakkuita
# vuoden ik�luokat, hakkkum��r� tavoitteen mukainen. 
# p��tehakkuut kohdeneetaan satunnisesti hakkuuik�isiin metsiin 
# N(meanph,sdph)jakaumasta 
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
# thinningLimit       vanhin met� jota voidaan harventaa (k�yt� oletusarvoa)
# ymax                tulostettavan kuvan y-akselin maksimiarvot pinta-alalle ja kasvulle
# xmax                kuvan x-akselin maksimi
# addTexts            tulostetanko kuvaan tekstit
# meanph, sdph        p��tehakkuun keski-ik� ja hajonta jos exp=FALSE
# above               mik� osuus p��tehakkuista tehd��n yl�harvennuksina, k�yt� arvoa 0
# shift               kuinka paljon mets� nuorenee yl�harvennuksessa, k�yt� oletusta
# phi                 p��tehakkuiden ajoituksen jakaumaa kuvaavan eksponenttijakauman rate-parametri
# exp                 k�ytet��nk� p��tehakkuiden ajoitksen jakaumana eksponenttijakaumaa (TRUE) vai 
#                     normaalijakaumaa (FALSE). k�yt� oletusta. 
# areasuo             suojeltujen metsien ik�jakauma. k�yt� oletusta (esi suojelualueita mukana)
# makeplot            tehd��nk� kuva
# cutSuo, nortality   suojelumetsiin liittyvi� parametreja.               
 
###############################################################################
LongTermSimOld<-function(age,area,growth,protectionLimit=120,#thinning=0.005,
                      cutCriterion,target,title,simLength=2,startyear=2020,
                      productSink=0.05,thinningRule=c(25,0.20),thinninglimit=90,
                      ymax=c(160000,10),xmax=NA,addTexts=FALSE,meanph=90,sdph=10,
                      above=0.3,shift=30,phi=1/10,exp=TRUE,areasuo=NA,
                      makeplot=FALSE,cutSuo=FALSE,mortality=0.01) {
#                                    file="simul.pdf"
#                                     
# 
# productSink: which proportion of harvested volume contributes to net increase in product pool.
# i.e., ratio product sink / cuttings
# thinning rule: vector of lenggth two: thinning interval, and thinning removal (% of volume). 
# Vuotuinen harvennuspoistuma / ik�luokka 
#  pdf(file)
#  win.print(width=12,height=7)
#  dev.new(width=10,height=7)
  if (cutCriterion=="RotationLength"|cutCriterion=="CleacutArea") {
     exp<-TRUE
     phi<-1
     }
  thinning<-1-(1-thinningRule[2])^(1/thinningRule[1])
  
  nrep<-simLength*protectionLimit
  nclass<-length(age)
  # kvek: hakkuiden piiriss� olevien metsien kasvu
  # suojelui�n ylitt�vien talousmetsien kasvu (area, ik�>protectionlimit)
  # suojeltujen metsien kasvu (areasuo)
  kvek<-kvek2<-kvek3<-hvek<-mvek<-rep(0,nrep)
  ikajak<-matrix(0,ncol=nrep+1,nrow=nclass+nrep)
  ikajaksuo<-matrix(0,ncol=nrep+1,nrow=nclass+nrep)

  if (is.na(areasuo[1])) areasuo<-rep(0,length(area))
  if (cutSuo) {
              area<-area+areasuo
              areasuo<-rep(0,length(area))
              } 
  
  ikajak[1:nclass,1]<-area
  ikajaksuo[1:nclass,1]<-areasuo
  
  k<-length(age)
  cuttingInstruction<-rep(0,k)
  cuttingInstruction[1:(thinninglimit-1)]<-thinning # Which proportion is removed in thinnings each year 
  V<-(1-cuttingInstruction)*growth    # initialize, used only for year 1
  Vs<-(1-mortality)*growth
  removals<-cuttingInstruction*growth # initialize, use only for year 1
  mortalityVek<-rep(0,k)
  mortalityVek[1:100]<-mortality
  removalss<-mortalityVek*growth # initialize, use only for year 1
  for (i in 2:k) {
      # volume (per ha) of age class i: volume of class i-1 + growth - removals
      V[i]<-(1-cuttingInstruction[i])*(V[i-1]+growth[i]) 
      removals[i]<-cuttingInstruction[i]*(V[i-1]+growth[i])
      Vs[i]<-(1-mortalityVek[i])*(Vs[i-1]+growth[i]) # suojelumets�n tilavuus i�ss� x
      removalss[i]<-mortalityVek[i]*(Vs[i-1]+growth[i])
      }
    
  oldestClass<-protectionLimit
  for (i in 1:nrep) { # this loop produces a new graph in each run
    # Growth of productive (kvek), old (kvek2) and productive forest
    kvek[i]<-sum(area[1:oldestClass]*growth[1:oldestClass])/1e6
    kvek2[i]<-sum(area[-(1:oldestClass)]*growth[-(1:oldestClass)])/1e6
    kvek3[i]<-sum(areasuo*growth)/1e6 # suojelumetsien kasvu
    # compute thinning removals ..    
    hvek[i]<-sum(area*removals)/1e6
    mvek[i]<-sum(areasuo*removalss)/1e6 # kuolleisuus suojelumetsiss�
    # .. and find how much more m3 needs to be cut in final fellings
    # to meet the cutting criteria 
    # hakkuutavoite0 = removals from thinnings and clearcuts
    if (cutCriterion=="AbsoluteSink") {
        hakkuutavoite0<-max(kvek[i]+kvek2[i]+kvek3[i]-target,hvek[i])
        } else if (cutCriterion=="ProportionOfGrowth") {
        hakkuutavoite0<-(kvek[i]+kvek2[i]+kvek3[i])*target       
        } else if (cutCriterion=="RotationLength") {
        hakkuutavoite0<-sum(area[-(1:(target-1))]*V[-(1:(target-1))])/1e6+hvek[i] 
        } else if (cutCriterion=="ClearcutArea") {
        tmp<-oldestClass
        end<-FALSE
        hakkuutavoite0<-hvek[i]
        hakkuutarve<-target
        while (hakkuutarve>0&tmp>0) { 
              hakkuutavoite0<-hakkuutavoite0+pmin(hakkuutarve,area[tmp])*V[tmp]/1e6
              hakkuutarve<-hakkuutarve-area[tmp]
              tmp<-tmp-1
              }
        } else if (cutCriterion=="HarvestVolume") {
        hakkuutavoite0 <- target
        }
        
        # Todelinen 
        hakkuutavoite<-min(hakkuutavoite0,sum(area[3:oldestClass]*V[3:oldestClass])/1e6-hvek[i])  
    # The target volume for fellings has now  been set, 
    # so perform the cuttings now.
    # Remove so much volume from the oldest classes 
    # that the target is met. 
    
    if (exp) {
       jak<-sort(dexp(0:(oldestClass-1),phi))
       } else {
       jak<-dnorm(age[1:oldestClass],mean=meanph,sd=sdph)
       }
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
       area[1:oldestClass]<-area[1:oldestClass]-paatehakkuut-ylaharv+c(ylaharv[-(1:shift)],rep(0,shift)) # ikajakauma p��tehakkuiden j�lkeen
       paatehakkuuala<-sum(paatehakkuut)+sum(ylaharv[1:shift]) # p��tehakattu pinta-ala
       hvek[i]<-hvek[i]+# hakkuukertym� alaharvennuksista 
                sum(paatehakkuut*V[1:oldestClass])/1e6+ # ja p��tehakkuista
                sum(ylaharv*(V[1:oldestClass]-Vshift[1:oldestClass]))/1e6 # ja yl�harvennuksista
       # growing
    #  print(i)
       area<-c(paatehakkuuala,area)
       areasuo<-c(0,areasuo)
       removals<-c(removals,0)
       removalss<-c(removalss,0)
       age<-c(age,max(age)+1)
       oldestClass<-oldestClass+1
       k<-k+1
       growth<-c(growth,growth[length(growth)]) # uuden luokan kasvu on sama kuin vanhimman
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
    sinkp3<-sinkp1+sinkp2    # tuotenilum yht 

    # kumulatiiviset puustonielut
    sink1c<-cumsum(sink1) 
    sink2c<-cumsum(sink2)
    sink3c<-cumsum(sink3)
    
    # kumulatiiviset kokonaisielut
    sinktot1c<-cumsum(sinkp1+sink1) 
    sinktot2c<-cumsum(sinkp2+sink2)
    sinktot3c<-cumsum(sinkp3+sink3) 
    
    if (makeplot) {
    par(mai=par()$mai[c(1,2,3,2)])
    if (is.na(xmax)) xmax<-simLength*protectionLimit 
    gfactor<-ymax[1]/ymax[2]
#  if (is.na(growth)) growth<-growthfun(age)

  # Kuvien piirto
    for (k in 1:5) {
      plot(age,(ikajak+ikajaksuo)[,1],type="n",lwd=20,lend=1,col="green",ylim=c(0,max(ymax[1],gfactor*ymax[2])),xlim=c(0,xmax),
           main=paste(title,"Year",startyear),ylab="area, ha",xlab="age class, yr")
  for (j in 1:ymax[2]) {
      abline(h=(j-1)*gfactor,col="gray")
      mtext((j-1),side=4,at=gfactor*(j-1),cex=1)
      }
  mtext("Growth, m3/ha",side=4,at=ymax[1]/2,cex=1,line=1.3) 
  points(age,(ikajak+ikajaksuo)[,1],type="h",lwd=1,lend=1,col="#336600")
  points(age,(ikajaksuo)[,1],type="h",lwd=1,lend=1,col="red")
  lines(age,growth*gfactor)
  } # k in 1:5
    
  for (i in 1:nrep) {
    plot(age,(ikajak+ikajaksuo)[,i+1],type="n",lwd=20,lend=1,col="green",ylim=c(0,ymax[1]),xlim=c(0,xmax),
         main=paste(title,"Year",startyear+i),
         ylab="area, ha",xlab="age class, yr")
#    xxyy<-par()$usr
#    ranx<-diff(xxyy[1:2])
#    rany<-diff(xxyy[3:4])
#    minx<-xxyy[1]
#    miny<-xxyy[3]
    for (j in 1:ymax[2]) {
        abline(h=(j-1)*gfactor,col="gray")
        mtext((j-1),side=4,at=gfactor*(j-1),cex=1)
        }
    mtext("Growth, m3/ha",side=4,at=ymax[1]/2,cex=1,line=1.3) 
    points(age,(ikajak+ikajaksuo)[,i+1],type="h",lwd=1,lend=1,col="#336600")
    points(age,ikajaksuo[,i+1],type="h",lwd=1,lend=1,col="red")
    lines(age,growth*gfactor)

    if (addTexts) {
    text(c(0.75,0.85,0.95)*xmax,rep(1,3)*ymax[1],c("Commercial","Protected","Total"),pos=2)
    text(c(0.65,0.75,0.85,0.95)*xmax,rep(0.95,4)*ymax[1],c("Growth, Mm3/v",round(kvek[i],2),round((kvek2+kvek3)[i],2),round((kvek+kvek2+kvek3)[i],2)),pos=2)
    text(c(0.65,0.75,0.85,0.95)*xmax,rep(0.9,4)*ymax[1],c("Removals, Mm3/v",round(hvek[i],2),0,round(hvek[i],2)),pos=2)
    text(c(0.65,0.75,0.85,0.95)*xmax,rep(0.85,4)*ymax[1],
         c("Carbon sink of standing trees, Mm3/v",round(sink1[i],2),round(sink2[i],2),round(sink3[i],2)),pos=2,
         col=c(1,1,1,1))
    text(c(0.65,0.75,0.85,0.95)*xmax,rep(0.8,4)*ymax[1],
         c("Carbon sink of wood products, Mm3/v",round(sinkp1[i],2),round(sinkp2[i],2),round(sinkp3[i],2)),pos=2,
         col=c(1,1,1,1))
    text(c(0.65,0.75,0.85,0.95)*xmax,rep(0.75,4)*ymax[1],col=c(1,2,1,1),c("Cumulative sink of standing trees, Mm3",round(sink1c[i],2),round(sink2c[i],2),round(sink3c[i],2)),pos=2)
    text(c(0.65,0.75,0.85,0.95)*xmax,rep(0.70,4)*ymax[1],col=c(1,2,1,1),c("Cumulative total sink, Mm3",round(sinktot1c[i],2),round(sinktot2c[i],2),round(sinktot3c[i],2)),pos=2)    
    }
  }
  }
#    dev.off()  
  list(ikajakTalous=ikajak,ikajakSuojellut=ikajaksuo,
       kasvut=data.frame(nuoret=kvek,vanhat=kvek2,suojellut=kvek3),
       hakkuumaara=hvek,
       nielut=data.frame(puustoTalous=sink1,puustoSuojelu=sink2,puustoYht=sink3,
                         tuoteTalous=sinkp1,tuoteSuojelu=sinkp2,tuoteYht=sinkp3),
       nielutCum=data.frame(puustoTalous=sink1c,puustoSuojelu=sink2c,puustoYht=sink3c,
                            tuoteTalosu=sinktot1c,tuoteSuojelu=sinktot2c,tuoteYht=sinktot3c),
       growth=sum(kvek[1:nrep]),
       removals=sum(hvek[1:nrep]),
       volume=V,
       volumeSuo=Vs,
       kasvuTal=kvek,
       kasvuSuo=kvek2+kvek3,
       mortSuo=mvek,
       varTal=sink1c,
       varSuo=sink2c,
       nieTal=sink1,
       nieSuo=sink2)
  }

           



