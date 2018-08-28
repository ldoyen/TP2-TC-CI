# Put custom tests in this file.

# Uncommenting the following line of code will disable
# auto-detection of new variables and thus prevent swirl from
# executing every command twice, which can slow things down.

# AUTO_DETECT_NEWVAR <- FALSE

# However, this means that you should detect user-created
# variables when appropriate. The answer test, creates_new_var()
# can be used for for the purpose, but it also re-evaluates the
# expression which the user entered, so care must be taken.

# Get the swirl state
getState <- function(){
  # Whenever swirl is running, its callback is at the top of its call stack.
  # Swirl's state, named e, is stored in the environment of the callback.
  environment(sys.function(1))$e
}

# Retrieve the log from swirl's state
getLog <- function(){
  getState()$log
}

Hist.EAP <- function(x,breaks,add=FALSE,xlim,ylim,rect=TRUE,col,lty=par("lty"),lwd=par("lwd"),when=TRUE,numeroter=FALSE,fuzzy=1e-7,...) {
  x[which(x!=min(x))]<-x[which(x!=min(x))]-fuzzy
  intervalle<-pretty(x,breaks)
  centers<-(intervalle[2:(length(intervalle))]+intervalle[1:(length(intervalle)-1)])/2
  rang<-c(intervalle[1],intervalle[length(intervalle)])
  pas<-intervalle[2]-intervalle[1]
  pas2<-pas/2.0
  cuts <- cut(x,br=intervalle,include.lowest=TRUE)
  eff <- as.vector(table(cuts))
  params <- names(list(...))
  par("usr")->usr
  if(!add && missing(xlim)) xlim <- rang
  tot <- sum(eff)
  high <- 1/tot/pas
  high2 <- high/2.0
  if(!add && missing(ylim)) ylim <- c(0,high*max(eff))
  if(!add) {
    plot(0,0,type="n",xlim=xlim,ylim=ylim,...)
  }
  x.bricks <- rep(centers,eff)
  ## les hauteurs des briques !!!
  y.bricks <- unlist(sapply(eff[eff>0],function(i) seq(1,i)))*high
  if(missing(col)) col <- cm.colors(tot)
  else if(length(col)<tot) {
    lcol <- length(col)
    col <- rep(col,lcol*(tot%/%lcol)+1)[1:tot]
  }
  ## les briques
  if(rect && any(when)) {
    order(order(cuts))->init.order
    bricks.x <- x.bricks[init.order]
    bricks.y <- y.bricks[init.order]
    rect(bricks.x[when]-pas2,bricks.y[when]-high,bricks.x[when]+pas2,bricks.y[when],col=col[when])
    if(numeroter) text(x.bricks,y.bricks-high/2,1:length(x.bricks))
  }
  ## le contour
  freq <- eff*high
  br.contour <- c(centers[1]-pas2,centers+pas2)
  x.contour <- rep(br.contour,rep(2,length(br.contour)))
  y.contour <- c(0,rep(freq,rep(2,length(freq))),0)
  lines(x.contour,y.contour,lwd=lwd,lty=lty)
  abline(h=0)
  if(add)
    assign(".histo.EAP",list(x=x,pas=pas,x.bricks=x.bricks,y.bricks=y.bricks,centers=centers,counts=eff,col=col,x.contour=x.contour,y.contour=y.contour),envir=.randomEnv)
  else
    assign(".histo.EAP",list(x=x,pas=pas,x.bricks=x.bricks,y.bricks=y.bricks,centers=centers,counts=eff,col=col,xlim=xlim,ylim=ylim,x.contour=x.contour,y.contour=y.contour),envir=.randomEnv)

  cumeff<-c(0,cumsum(eff))
  index<-sort(x,index.return=TRUE)$ix
  num_brique_min<-1:length(x)
  num_brique_max<-1:length(x)
  for(i in 1:length(x)){
    j<-max(which(cumeff<which(index==i)))
    num_brique_min[i]<-cumeff[j]+1
    num_brique_max[i]<-cumeff[j+1]
  }

  return(list(
    centers=centers,
    pas=pas,
    eff=eff,
    i_brique_min=num_brique_min,
    i_brique_max=num_brique_max
  ))
}


Choix_sujet_etudiant<-function(num_etud,nb_sujet=5){
  #return(floor((num_etud-floor(num_etud/100)*100)/20))
  set.seed(num_etud)
  return(sample(1:nb_sujet,1))
}

genere_data<-function(vs){
  data<-replicate(vs$m2, mean(rbinom(vs$n,1,vs$p0)))
  iddata<-sample(1:vs$m1,6,replace=FALSE)


  data[iddata[1]]<-max((floor(vs$pinf*vs$n)-1)/vs$n,0)
  data[iddata[2]]<-max((floor(vs$pinf*vs$n)-4)/vs$n,0)
  data[iddata[3]]<-(ceiling(vs$pinf*vs$n)+1)/vs$n
  data[iddata[4]]<-(floor(vs$psup*vs$n)-1)/vs$n
  data[iddata[5]]<-min((ceiling(vs$psup*vs$n)+1)/vs$n,1)
  data[iddata[6]]<-min((ceiling(vs$psup*vs$n)+4)/vs$n,1)
  return(data)
}

num_etud<-function(){
  ###Les differents sujets
  variable_sujet<-list(
	m1=c(17,18,19,21,22),
	m2=c(110,120,130,140,150),
	n=c(125,200,250,400,500),
	p0=c(0.2,0.22,0.24,0.26,0.28),
	pinf=c(0.18,0.2,0.22,0.24,0.26),
	psup=c(0.22,0.24,0.26,0.28,0.3),
	nom_data=c("pAest","pB_est","pC_Est","pDEst","p_X_est"),
	pageWebsujet=c("https://toltex.u-ga.fr/VAM/TP2/sujet.html","https://toltex.u-ga.fr/VAM/TP2/sujet_tp.html","https://toltex.u-ga.fr/VAM/TP2/sujet__tp.html","https://toltex.u-ga.fr/VAM/TP2/sujet_tp2.html","https://toltex.u-ga.fr/VAM/TP2/sujet__tp2.html"),
	Y=c("Y^A","Y^B","Y^C","Y^D","Y^X"),
	y=c("y^A","y^B","y^C","y^D","y^X"),
	qalea=c(FALSE,TRUE,TRUE,FALSE,FALSE))
  ####

  e <- get("e", parent.frame())
  num_etud <- as.integer(e$val)
  res<-TRUE
  if (is.na(num_etud)|(num_etud<0)){
    res<-FALSE
    } else {
    confirmation<-paste("Vous confirmez que votre num\xE9ro d'\xE9tudiant est", num_etud, "?",sep=" ")
    Encoding(confirmation)<- "latin1"
    message(confirmation)
    res<-readline("Tapez 1, si c'est bien le cas, sinon appuyez sur n'importe quelle autre touche. Puis validez.")==1
    if (res){
      e$num_etud<-num_etud
      e$num_sujet <- Choix_sujet_etudiant(num_etud,length(variable_sujet$m1))
      set.seed(num_etud)
      vs<-variable_sujet
      for (i in 1:length(vs)){
	       vs[[i]]<-vs[[i]][e$num_sujet]
      }
      e$vs<-vs
      assign(vs$nom_data,genere_data(vs),.GlobalEnv)
      e$log$mon_skip<-e$log$skipped
    }
  }
  return(res)
}

submit_log <- function(){
  e <- get("e", parent.frame())

  res<-FALSE
  selection <- getState()$val
if(selection %in% 1:5){
  res<-TRUE

  nom_etud <- readline("Quelle est votre nom de famille ? ")
  demande_prenom<-"Quelle est votre pr\xE9nom ? "
  Encoding(demande_prenom) <- "latin1"
  prenom_etud <- readline(demande_prenom)

  # Please edit the link below
  pre_fill_link1 <- "https://docs.google.com/forms/d/e/1FAIpQLSeWzSmQyQa5YE-MUL_0DxzD5RShhaKbWBS63Bu0AmdbxwmI2w/viewform?usp=pp_url&entry.1536247898="
  pre_fill_link2 <- "https://docs.google.com/forms/d/e/1FAIpQLSfgztQT4bQTcgAuTlpMtVD5vQfAcLz5TWXqdS-D24Ctog4TFg/viewform?usp=pp_url&entry.1449157816="
  pre_fill_link3 <- "https://docs.google.com/forms/d/e/1FAIpQLSc-MLNgzzLzS6znCGlIMnSpBwbfqsbmJYGItyOxL0ucInW3YQ/viewform?usp=pp_url&entry.947620631="
  pre_fill_link4 <- "https://docs.google.com/forms/d/e/1FAIpQLSdHFMGd0kZ0K3n3wWX85Ka1FMonKLm1dF409NbjgIL0U0kMKA/viewform?usp=pp_url&entry.1829019151="
  pre_fill_link5 <- "https://docs.google.com/forms/d/e/1FAIpQLSdXGObsIGsQlhgQ4UwxknYANU2EAlm8cbakMVxpNFD9kmsmgg/viewform?usp=pp_url&entry.958732492="

  pre_fill_link <- switch(selection,
    pre_fill_link1,
    pre_fill_link2,
    pre_fill_link3,
    pre_fill_link4,
    pre_fill_link5
  )

  # Do not edit the code below
  if(!grepl("=$", pre_fill_link)){
    pre_fill_link <- paste0(pre_fill_link, "=")
  }

  p <- function(x, p, f, l = length(x)){if(l < p){x <- c(x, rep(f, p - l))};x}

  temp <- tempfile()
  log_ <- getLog()
  nrow_ <- max(unlist(lapply(log_, length)))
  e$log$skipped[1:length(e$log$mon_skip)]<-e$log$mon_skip
  log_tbl <- data.frame( p(log_$question_number, nrow_, NA),
                         p(log_$correct, nrow_, NA),
                         p(log_$attempt, nrow_, NA),
                         p(log_$skipped, nrow_, NA),
                         p(log_$datetime, nrow_, NA),
                        stringsAsFactors = FALSE)
  names(log_tbl) <- c(e$num_etud, nom_etud, prenom_etud,log_$lesson_name,e$num_sujet)
  write.csv(log_tbl, file = temp, row.names = FALSE)
  encoded_log <- base64encode(temp)
  browseURL(paste0(pre_fill_link, encoded_log))

    e <- get("e", parent.frame())
    if(selection %in% c(1,2,3)) e$adresse_email<-"laurent.doyen@iut2.univ-grenoble-alpes.fr" else e$adresse_email<-"marie-jose.martinez@iut2.univ-grenoble-alpes.fr"
    e$sujet_email<-paste0("**TP2-TC-CI**"," G",selection,", ",log_$lesson_name,", ", nom_etud,collapse="")
    e$corp_email<-encoded_log
}
  return(res)
}

email_log<-function(){
    e <- get("e", parent.frame())
    res<-FALSE
    if(e$val=="Oui"){
      email(e$adresse_email,e$sujet_email,e$corp_email)
      res<-TRUE
    }
    return(res)
}

#answear test to known if the value of the answear is between b_inf and b_sup
test_between <- function(b_inf,b_sup){
  n<-length(b_inf)
  res<-TRUE
  e <- get("e", parent.frame())
  e<-e$val
  for(i in 1:n){
    res<-res&(e[i] >= b_inf[i])&(e[i] <= b_sup[i])
  }
  return(res)
}

ouvrir_sujet_TP<-function(){
  e <- get("e", parent.frame())
  selection <- getState()$val
  res<-FALSE
  if(selection == "Oui"){
    browseURL(e$vs$pageWebsujet)
    res<-TRUE
  }
  return(res)
}

test_passer<-function(){
  e <- get("e", parent.frame())
  res<-(e$expr=="passer()")
  if(length(e$log$mon_skip)>0) e$log$skipped[1:length(e$log$mon_skip)]<-e$log$mon_skip
  e$log$mon_skip<-e$log$skipped
  e$log$mon_skip[length(e$log$mon_skip)+1]<-res
  return(res)
}

briques<-function(){
  e <- get("e", parent.frame())
  selection <- getState()$val
  res<-FALSE
  if(selection == "Oui"){
    caracHist<-hist(get(e$vs$nom_data)[1:e$vs$m1],plot=FALSE)
    Hist.EAP(get(e$vs$nom_data)[1:e$vs$m1],col="white",breaks=length(caracHist$breaks)-1,bty="n",main=paste0('Histogram of ',e$vs$nom_data,"[1:",e$vs$m1,"]",collapse=""),xlab=paste0(e$vs$nom_data,"[1:",e$vs$m1,"]",collapse=""),ylab="Density")
    res<-TRUE
  }
  return(res)
}

num_briques<-function(){
  e <- get("e", parent.frame())
  selection <- getState()$val
  res<-FALSE
  if(selection == "Oui"){
    caracHist<-hist(get(e$vs$nom_data)[1:e$vs$m1],plot=FALSE)
    info_hist<-Hist.EAP(get(e$vs$nom_data)[1:e$vs$m1],col="white",numeroter=TRUE,breaks=length(caracHist$breaks)-1,bty="n",main=paste0('Histogram of ',e$vs$nom_data,"[1:",e$vs$m1,"]",collapse=""),xlab=paste0(e$vs$nom_data,"[1:",e$vs$m1,"]",collapse=""),ylab="Density")
    e$info_hist<-info_hist
    res<-TRUE
  }
  return(res)
}

assoc_briques<-function(){
  e <- get("e", parent.frame())
  if(grepl("passer()",paste0(e$val,collapse = ""))){
    res<-TRUE
    if(length(e$log$mon_skip)>0) e$log$skipped[1:length(e$log$mon_skip)]<-e$log$mon_skip
    e$log$mon_skip<-e$log$skipped
    e$log$mon_skip[length(e$log$mon_skip)+1]<-TRUE
  } else {
    if(class(try(eval(parse(text=paste0("c(",paste0(e$val,collapse=","),")"))),silent=TRUE))=="try-error"){
      res<-FALSE
      pb<-"Vous devez indiquer les num\xE9ro des briques en les s\xE9parant par des virgules !"
      Encoding(pb) <- "latin1"
      message(pb)
    } else {
      reponse<-eval(parse(text=paste0("c(",paste0(e$val,collapse=","),")")))
      res<-sum(sort(reponse)==(1:(length(reponse))))==(e$vs$m1)
      if(res){
        for(i in 1:(e$vs$m1)) {res<-res&((reponse[i])>=(e$info_hist$i_brique_min[i]))&((reponse[i])<=(e$info_hist$i_brique_max[i]))}
      } else {
        pb<-"Chaque num\xE9ro de brique ne peut apparaitre q'une seule fois !"
        Encoding(pb) <- "latin1"
        message(pb)
      }

    }
  }
  return(res)
}

inf_briques<-function(){
  e <- get("e", parent.frame())
  if(grepl("passer()",paste0(e$val,collapse = ""))){
    res<-TRUE
    if(length(e$log$mon_skip)>0) e$log$skipped[1:length(e$log$mon_skip)]<-e$log$mon_skip
    e$log$mon_skip<-e$log$skipped
    e$log$mon_skip[length(e$log$mon_skip)+1]<-TRUE
  } else {
    if(class(try(eval(parse(text=paste0("c(",paste0(e$val,collapse=","),")"))),silent=TRUE))=="try-error"){
      res<-FALSE
      pb<-"Vous devez indiquer les num\xE9ro des briques en les s\xE9parant par des virgules !"
      Encoding(pb) <- "latin1"
      message(pb)
    } else {
      reponse<-eval(parse(text=paste0("c(",paste0(e$val,collapse=","),")")))
      res<-length(unique(reponse))==(length(reponse))
      if(res){
        ok<-(get(e$vs$nom_data)[1:e$vs$m1])<=(e$vs$psup)
        res<-res&(length(reponse) == sum(ok))& prod(reponse<=max(e$info_hist$i_brique_max[which(ok)]))
      } else {
        pb<-"Chaque num\xE9ro de brique ne peut apparaitre q'une seule fois !"
        Encoding(pb) <- "latin1"
        message(pb)
      }

    }
  }
  return(res)
}

aire_brique<-function(){
    e <- get("e", parent.frame())
    return(prod(e$val==(1/e$vs$m1)))
}

aireinf_brique<-function(){
    e <- get("e", parent.frame())
    return(prod(e$val==( sum((get(e$vs$nom_data)[1:e$vs$m1])<=(e$vs$psup)) /e$vs$m1)))
}

calc_freq<-function(){
    e <- get("e", parent.frame())
    return(prod(e$val==( mean(get(e$vs$nom_data)>e$vs$pinf & get(e$vs$nom_data)<= e$vs$psup) )))
}
