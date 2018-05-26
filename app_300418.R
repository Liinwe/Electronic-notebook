################# CHARGEMENT DES LIBRAIRIES

library(shiny)
library(shinythemes)
library(DT)
library(dplyr)
library(shinyBS)
library(shinyTime)
library(RPostgreSQL)
library(shinyalert)

################ CHARGEMENT DE LA BASE DE DONNEES 

con<- dbConnect(PostgreSQL(), host="pggeodb-preprod.nancy.inra.fr", dbname="", user="", password="")



############### LISTE DE CHOIX

# liste de choix pour les selectInput, ces listes sont updatés si l'utilisateur le souhaite en utilisant l'option autre dans la selection
bleGravChoices = list("superficielle", "légère","profonde", 
     "fracture", "fracture _consolidée",
     " plaie_fermée", " pelade")

bleTraitChoices = list("allumisol", "serflex_allumisol",
     "points", "euthanasie", "rien")

blelocalisationChoices = list(" 1" = 1, "2" = 2, "3" = 3,"Autre localisation")




################## FORMULAIRE CARACTERISTIQUES DE L'ANIMAL



contentcaractanimal = fluidPage(
  #titlePanel("Caract. de l'animal"),
  
  fluidRow(
    column(2, numericInput(inputId = ("nSabot"), value = " ",label = h4("N° Sabot"), min=1, max=28)),
    column(2, numericInput(inputId = "pSabotPlein", value = " ",label = h4("Poids Sabot Plein"),min=0,max=65 )),
    column(2, numericInput(inputId = "pSabotVide", value = " ",label = h4("Poids Sabot Vide"),min=0,max=50 )),
    column(2, h4("Poids Animal"),textOutput("value")),
    column(12,hr()),
    column(2, checkboxInput(inputId = "estNouvelAnimal", value = T,label = h4("Nouvel Animal"))),
  
    
    column(2,conditionalPanel( condition = "input.estNouvelAnimal == 1",
             numericInput(inputId = "nAnimal", value = " ",label = h4("N° Animal"),min=0 ))),
    
    column(2,conditionalPanel(condition = "input.estNouvelAnimal == 0",
             selectizeInput("nAnimal2",h4("N° Animal"), choices = dbGetQuery(con,"select (ani_etiq) from public.t_animal_ani"),options=list(placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }')), selected = NULL))),
    
    column(2, selectizeInput("idSite", h4("Site"), 
                             choices = dbGetQuery(con,"select (sit_nom_court) from public.tr_site_capture_sit"),options=list(placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }')), selected = NULL)), 
    column(12),
   
    column(2, timeInput("heureDebut",h4("Heure début"),seconds = FALSE)),
    column(2, selectInput("idRFID", h4("Rfid"), 
             choices = list("1" = 1, "2" = 2,"3" = 3), selected = 1)),
    column(2, selectInput("idTagOrG", h4("Tag Oreille Gauche"),
            choices = list(" 1" = 1, "2" = 2,"3" = 3), selected = 1) ),
    column(2, selectInput("idTagOrD", h4("Tag Oreille Droite"), 
            choices = list("Site 1" = 1, "Site 2" = 2,"Site 3" = 3), selected = 1)),
    column(12,hr()),
    column(2, dateInput('dateCapture',label=h4("Date"),value = Sys.Date())),
    column(2, radioButtons("sexe",h4("Sexe"),choiceNames = list("M","F"),choiceValues = list(0,1))),
    column(1,useShinyalert())
          ),
  
  hr(),
  
  fluidRow(
    column(2, numericInput("cirCou", value=" ", h4("Circonférence cou"),min=0)),
    column(2, numericInput("lPattArriere", value=" ", h4("Longueur de la patte arrière"),min=0)),
    column(2, numericInput("tglucose", value="", h4("Taux de Glucose sanguin"), min=0))
          ),
  
  
  conditionalPanel(
      condition = "input.sexe == 0",
 
      fluidRow(
        column(2, numericInput("lBoisGauche", value=" ", h4("Longueur bois gauche"),min=0)),
        column(2, numericInput("lBoisDroit", value=" ", h4("Longueur bois droit"),min=0)),
        column(2, selectInput("etatBois", h4("État bois"), choices = list("Velours", "tombés", "durs"), selected = 1))
              )
                 )
)



################## FORMULAIRE BLESSURES

contentblessures = fluidPage( 
 # titlePanel("Blessures"),
  fluidRow(
    column(3,  
           selectInput("blelocalisation_sel", h4("Localisation"), 
                       choices = blelocalisationChoices, selected = 1),
           bsModal("nouvelleLocalization_modal", "Entrer la localisation","", size = "large",wellPanel(
             textInput("nouvelle_localisation_txt",""),
             actionButton("ok_button", "OK"),
             actionButton("quit_button", "Quitter")
           )),
           textInput("blelocalisation_txt","")
    ),
    column(3, selectInput("bleGrav_sel", h4("Gravité"), choices = bleGravChoices, selected = "superficielle"),
              textInput("bleGrav_txt","") ),
    column(3, selectInput("bleTrait_sel", h4("Traitement"), choices = bleTraitChoices, selected = 1),
              textInput("bleTrait_txt","")),
    column(3, actionButton("ajoutBle","Ajouter une blessure"))
  ),
  
  hr(),
  
  fluidRow(
    sidebarLayout(
      mainPanel(
      dataTableOutput("tableblessure")),
      sidebarPanel(actionButton("sup_Bles", "Supprimer blessure"))
                )
          ))



################## FORMULAIRE PRELEVEMENTS

contentprelevement = fluidPage()



################## FORMULAIRE COLLIER

contentcollier = fluidPage(
  #titlePanel("Caracteristique du collier"),
  fluidRow(
    #titlePanel("Pose de collier"),
    column(3, checkboxInput(inputId = "new_collier", value = F,label = h4("Nouveau collier"))),
    column(3, actionButton("ajoutColl","Confirmer la nouvelle pose"))
))


################## FORMULAIRE COMPORTEMENT TABLE

contenttable = fluidPage(
  #titlePanel("Comportement sur table"),
  
  fluidRow(
  
  column(2,timeInput("time_table", h4("Heure:"),seconds = FALSE),
           actionButton("to_current_time_table", "Afficher l'heure")),  
  column(3,numericInput("rectTemp", value=" ", h4("Température rectale"),step = 1)),  
  column(3,numericInput("ExtTemp", value=" ", h4("Température extérieure"),step = 1)),
  column(12,hr()),
  column(2,radioButtons("lutte",h4("Lutte"),choiceNames = list("Oui","Non"),choiceValues = list(T,F), selected = character(0))),
  column(2,radioButtons("halete",h4("Halete"),choiceNames = list("Oui","Non"),choiceValues = list(T,F), selected =character(0))),
  column(2,radioButtons("cribague",h4("Cri Bague"), choices  = list(NA,"0", "1-2", ">2"))),
  column(2,radioButtons("criautre", h4("Cri Autre"), choices = list("0", "1-2", ">2"), selected = F)),
  column(12,hr()),
  column(2,selectizeInput("Notation_euro_table", h4("Notation Eurodeer"), 
                          choices = dbGetQuery(con,"select (ect_comportement) from lu_tables.tr_eurodeer_comp_table_ect"),options=list(placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }')), selected = NULL)) 
))


################## FORMULAIRE HISTORIQUE :

contenthistorique = fluidPage(
  #titlePanel("Historique"),
    fluidRow(
      
      tabPanel("Caractéristiques de l'animal",
            checkboxInput("recap","recapture ?", 1),
            
            conditionalPanel(
                condition = "input.recap == 1",
                fluidRow(column(width= 2, selectInput(inputId = "ani_etiq", label = h4("N°Animal"),
                                choices = dbGetQuery(con,"Select ani_etiq from public.t_animal_ani order by ani_etiq")), selected = NULL, offset= 0.5))),
            
            conditionalPanel(
              condition = "input.recap == 0",
              fluidRow(column(width= 2, numericInput("new_ani_etiq",value="" ,h4("N°Animal"))))),
            
      tabPanel("Historique de capture", DT::dataTableOutput("historique"))
    )))



################## FORMULAIRE CHECKLIST 1 :

contentcheck1 =  fluidPage(fluidRow(
  titlePanel("Checklist - Caractéristiques"),
  tabPanel("Cheklist 1", DT::dataTableOutput("tablechecklist1")), 
  column(12,useShinyalert(),
         actionButton("checklist_1", "Checklist",icon('eye'),width='25%')),

#  titlePanel("Checklist - Prelevements"),
  
column(12,hr()),

  #conditionalPanel(
  #  condition = "input.new_collier == 1",
  #  fluidRow(titlePanel("Checklist - Collier"))) ,
  
  
  titlePanel("Checklist - Table"),
  tabPanel("Checklist Table",DT::dataTableOutput("tablechecklist_table")), 
  column(12,useShinyalert(),
         actionButton("checklist_tab", "Checklist",icon('eye'),width='25%'))
))




################## FORMULAIRE COMPORTEMENT AU LACHER :


###submitButton(format(Sys.time(), "%X"))
#timeInput("time2", "Heure lâcher:", value = Sys.time(),seconds = FALSE))

contentlacher = fluidPage(
 # titlePanel("Comportement au lâcher"),
  fluidRow(
    column(2,timeInput("time", h4("Heure de lâcher:"),seconds = FALSE),
             actionButton("to_current_time", "Afficher l'heure")),
    
    column(2, timeInput("time2", h4("Heure de 2nd lâcher:"),seconds = FALSE),
              actionButton("to_current_time2", "Afficher l'heure")),
    
    column(1,numericInput("nbre_stops",value=NULL, h4("Nombre de stops"),min=0)),
    column(1,numericInput("nbre_personnes", value=NULL, h4("Nbre de personnes"),min=1)),
    
    column(12,hr()),
    
    column(1,radioButtons("vitesse",h4("Vitesse"),choiceNames = list("Pas","Course"),choiceValues = list(0,1), selected = F)),
    column(1,radioButtons("allure",h4("Allure"),choiceNames = list("Réfléchi","Bolide"),choiceValues = list(0,1), selected = F)),
    column(1,radioButtons("cabriole_saut",h4("Cabriole"), choiceNames = list("Oui","Non"), choiceValues = list(1,0), selected = F)),
    column(1,radioButtons("gratte_collier", h4("Gratte collier"), choiceNames = list("Oui","Non"), choiceValues = list(1,0), selected = F)),
    column(1,radioButtons("tombe", h4("Tombe"), choiceNames = list("Oui","Non"), choiceValues = list(1,0), selected = F)),
    column(1,radioButtons("cri",h4("Cri"),choiceNames = list("Oui","Non"),choiceValues = list(1,0), selected = F)),
    column(1,radioButtons("titube",h4("Titube"),choiceNames = list("Oui","Non"),choiceValues = list(1,0), selected = F)),
    column(1,radioButtons("couche",h4("Couché"), choiceNames = list("Oui","Non"), choiceValues = list(1,0), selected = character(0))),
    
    column(12,hr()),
    
    column(2,selectizeInput("visibilite", h4("Visibilité fuite"), 
      choices = list("0-10","11-50","51-100",">100","Nuit"), options=list(placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }')), selected = NULL)),
    
    column(2,selectizeInput("habitat", h4("Habitat lâcher"), 
      choices = dbGetQuery(con,"select distinct (t_capture_cpt.cpt_lache_habitat_lache) from cmpt.t_capture_cpt"), options=list(placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }')), selected = NULL)),
    
    column(2, selectizeInput("habitat_perte", h4("Habitat perte de vue"), 
      choices = dbGetQuery(con,"select distinct (t_capture_cpt.cpt_lache_habitat_pertevue) from cmpt.t_capture_cpt"),options=list(placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }')), selected = NULL)),
    
    column(2,selectizeInput("Notation_euro", h4("Notation Eurodeer"), 
      choices = dbGetQuery(con,"select (ecl_comportement_lache) from lu_tables.tr_eurodeer_comp_lache_ecl"),options=list(placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }')), selected = NULL))
    
  ))
  


################## FORMULAIRE CHECKLIST 2 :


contentcheck2 = fluidPage(fluidRow(
  tabPanel("Checklist 2", DT::dataTableOutput("tablechecklist2")), 
  
  column(12,useShinyalert(),
         actionButton("checklist_2", "Checklist",icon('eye'),width='25%'))))


################## FORMULAIRE COMPORTEMENT CAPTURE :


contentcapture = fluidPage(
  
  #titlePanel("Comportement Capture"),
  fluidRow(
    
    column(2,dateInput('date',label=h4("Date"),value = Sys.Date())),
    column(2,selectizeInput("nSabot",label = h4("N° Sabot"), choices = dbGetQuery(con,"select distinct cap_num_sabot FROM public.t_capture_cap"),options=list(placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }')), selected = NULL)),
    column(2,timeInput("cpt_heure_debut_filet",h4("Heure arrivée filet"),seconds = FALSE)),
    
    #column(3, 
    #selectInput("N° sabot", h4("N°sabot"), choices = list("Velours", "tombés", "durs"), selected = 1),
    #numericInput(inputId = "nSabot", value = " ",label = h4("N° Sabot"), min=1, max=28)
    #), # faut trouver un moyen de récuperer dans la table les différents N°sabot enregistrer dans la rubrique caractéristique de l'animal
    
    
    column(12,hr()),
    column(2,timeInput("cpt_temps_filet", h4("Temps passé filet"),seconds = FALSE)),
    column(2,textInput("nom_capteur_txt",label=h4("Nom des capteurs",""))),
    column(2,selectInput("Nbre_pers_experimentes",h4("Nombre de capteurs expérimentés"),choices = list("1"=1,"2"=2,"3"=3,"4"=4,"5"=5),selected = F)),
    
    column(12,hr()),
    
    column(1,radioButtons("cpt_filet_vitesse",h4("Vitesse"),choiceNames = list("Pas","Course"),choiceValues = list(0,1), selected = F)),
    column(1,radioButtons("cpt_filet_allure",h4("Allure"),choiceNames = list("Réfléchi","Bolide"),choiceValues = list(0,1), selected = F)),
    column(1,radioButtons("cpt_filet_lutte", h4("Lutte"), choiceNames = list("Oui","Non"), choiceValues = list(1,0), selected = character(0))),
    column(1,radioButtons("cpt_filet_halete",h4("Halete"), choiceNames = list("Oui","Non"), choiceValues = list(1,0), selected = character(0))),
    column(1,radioButtons("cpt_filet_cri",h4("Cri"),choiceNames = list("Oui","Non"),choiceValues = list(1,0), selected =c("None selected" = ""))),
    column(12,hr()),
    column(2,textInput("Remarques",label=h4("Remarques",""))),
    column(1),
    column(4,useShinyalert(),
           actionButton("checklist_capture", "Checklist",icon('eye')))
    
    
  ))

                
################## FORMULAIRE COMPORTEMENT SABOT :


contentsabot = fluidPage(
 # titlePanel("Comportement sabot"), 
  fluidRow(
    
    #Heure de mise en sabot
    column(3, timeInput("cpt_heure_mise_sabot", h4("Heure de mise en sabot:"),seconds = FALSE)),
    
    #Fin de surveillance
    column(3,timeInput("cpt_heure_fin_surv", h4("Fin de surveillance"),seconds = FALSE)),
    
    column(12,hr()),
    
    #Acepromazine
    column(2,selectizeInput("cpt_dose_acepromazine",h4("Acepromazine"), choices = dbGetQuery(con,"select distinct cpt_dose_acepromazine from cmpt.t_capture_cpt order by cpt_dose_acepromazine"),options = (list(create = TRUE,placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }'))), selected = NULL)),
    
    #Sur le dos
    column(1,radioButtons("cpt_sabot_retournement",h4("Sur le dos"),choiceNames = list("Oui","Non"),choiceValues = list(1,0), selected =c("None selected" = ""))),
    
    #Couché
    column(1, radioButtons("cpt_sabot_couche",h4("Couché"),choiceNames = list("Oui","Non"),choiceValues = list(1,0), selected =c("None selected" = ""))),
    
    #Agité
    column(1, radioButtons("cpt_sabot_agitation",h4("Agité"),choiceNames = list("Oui","Non"),choiceValues = list(1,0), selected =c("None selected" = ""))),
    
    column(12,hr()),
    
    #Observateur
    column(3,textInput("Observateur",label=h4("Observateurs",""))),
    
    #Remarque
    column(3,textInput("Remarques",label=h4("Remarque",""))),
    
    column(12,hr()),
    
    column(4,useShinyalert(),
           actionButton("checklist_sabot", "Checklist",icon('eye')))
    
    
  )
)


######## ORGANISATION DES RUBRIQUES

caractanimal = tabPanel("Caract. de l'animal",contentcaractanimal)
blessures = tabPanel("Blessures",contentblessures)
prelevement= tabPanel("Prélèvements",contentprelevement)
caractcollier = tabPanel("Caract. du collier",contentcollier)
comportable = tabPanel("Comportement table",contenttable)
historique = tabPanel("Historique captures",contenthistorique)
checklist1 = tabPanel("checklist 1",contentcheck1)
comporlacher = tabPanel("Comportement lâcher",contentlacher)
checklist2 = tabPanel("checklist 2",contentcheck2)
comporcapture = tabPanel("Comportement capture",contentcapture)
comporsabot = tabPanel("Comportement sabot",contentsabot)



################## UI :
##Lumen or cerulean or sandstone

ui <- shinyUI(navbarPage("Formulaires",
   #theme=shinytheme("sandstone"),
   # Application title
   # titlePanel("Carnet Electronique"),
        #tabsetPanel(
          tabPanel ("Animal", caractanimal),
          tabPanel ("Blessures", blessures),
          tabPanel ("Prelevement", prelevement),
          tabPanel  ("Collier",caractcollier),
          tabPanel  ("Table",comportable),
          tabPanel  ("historique",historique),
          tabPanel  ("checklist 1",checklist1),
          tabPanel ( "Lâcher",comporlacher),
          tabPanel  ("Checklist 2",checklist2),
          tabPanel  ("Capture",comporcapture),
          tabPanel( "Sabot",comporsabot)
          #tabPanel("Summary", verbatimTextOutput("summary")),
          #tabPanel("Table", tableOutput("table"))
        )
)


################################################################################################ SERVER :


server <- function(input, output,session) {
   
   output$value = renderText({input$pSabotPlein-input$pSabotVide})

## Caracteristiques : 
   
  # observeEvent (input$nSabot, {
   #  if ((input$nSabot)>28) {
    #   shinyalert("STOP!", "Est-ce un nouveau numéro de sabot ?", type = "warning",confirmButtonText="Oui", showCancelButton=T,cancelButtonText="Non", callbackR = modalcallbacknSabot)
     #  }})
   
   
   
   
   
   
## Blessures : 
   
   blessure = data.frame()
   row.names(blessure) = NULL
   
   output$tableblessure = DT::renderDT(expr = blessure,server = F)
   observe({
     # if(length(input$sexe)>1) {
     #   updateCheckboxGroupInput(session,"sexe", selected= tail(input$sexe,1))
     # }
   })
   
   sup_Ligne = observeEvent(input$sup_Bles, {
       if (!is.null(input$tableblessure_rows_selected)) {
         blessure <<- blessure[-as.numeric(input$tableblessure_rows_selected),]
         output$tableblessure = DT::renderDT(blessure,server = F)
       }
     }
   )
   observeEvent (input$blelocalisation_sel, {
     if (!is.null(input$blelocalisation_sel)) {
       if( input$blelocalisation_sel == "Autre localisation"){
         toggleModal(session,"nouvelleLocalization_modal","open")
         # showModal(modalDialog(
         #   title = "Entrer la localisation",
         #   textInput("nouvelle_localisation_txt",""),
         #   
         #   easyClose = TRUE
         # 
         # ))
       }
     }
   })
   
   observeEvent(input$ajoutBle, {
          loca = ""
          grav = ""
          trait = ""
          if (input$blelocalisation_txt != "") {
            loca = input$blelocalisation_txt
            x = input$blelocalisation_sel
            #blelocalisationChoices[[length(blelocalisationChoices)-1]] =
            blelocalisationChoices <<- cbind(blelocalisationChoices,input$blelocalisation_txt)
            updateSelectInput(session,"blelocalisation_sel",
                              choices = blelocalisationChoices,
                              selected = input$blelocalisation_txt
            )
          } else {
            loca = input$blelocalisation_sel
          }
          if (input$bleGrav_txt != "") {
            grav = input$bleGrav_txt
          } else {
            grav = input$bleGrav_sel
          }
          if (input$bleTrait_txt != "") {
            trait = input$bleTrait_txt
          } else {
            trait = input$bleTrait_sel
          }
          
          blessure <<- rbind(blessure,data.frame("Localisation" = c(loca), "Gravité" =c(grav), "Traitement" = c(trait)))
          output$tableblessure = DT::renderDT(blessure,server = F)

   }
   ) 
   
######## PARTIE TABLE
   
   
observeEvent(input$to_current_time_table, {
  updateTimeInput(session, "time_table", value = Sys.time())
   })
   
######### Partie historique :
   
     
     output$historique <- DT::renderDataTable({
       outp <- dbGetQuery(con,paste0("select t.ani_etiq as ani, t.ani_sexe as s, t.cap_date as date, t.cap_poids as poids, t.cap_lpa as lpa, t.cap_age_classe as age, t.sit_nom_court as site, 
                                     t.teq_nom_court as teq, t.eqa_date_debut as debut, t.eqa_date_fin as fin, t.cap_annee_suivi as an, round(t.temps_suivi/30.43) as mois,  count(t.cpos_id) as locs, t.eqt_id_usuel as equip, t.mar_libelle as marque, t.mod_libelle as modele, array_to_string( array_agg( distinct eqc_sen_id), ', ') as capteurs from (SELECT eqc_sen_id, cpos_id, ani_etiq, ani_sexe, cap_date, cap_poids, cap_lpa, cap_age_classe, sit_nom_court, 
                                     teq_nom_court, cap_annee_suivi, eqa_date_debut, eqa_date_fin, eqa_date_fin - eqa_date_debut as temps_suivi, eqt_id_usuel, mar_libelle, mod_libelle
                                     FROM public.v_aniposi_gpsgsm, public.t_equipement_conf_eqc ) as t where t.ani_etiq = '",input$ani_etiq,"' group by t.ani_etiq, t.ani_sexe, t.cap_date, t.cap_poids, t.cap_lpa, t.cap_age_classe, t.sit_nom_court, 
                                     t.teq_nom_court, t.cap_annee_suivi, t.eqa_date_debut, t.eqa_date_fin, t.temps_suivi, t.eqt_id_usuel, t.mar_libelle, t.mod_libelle order by cap_date"))
    
       ret <- DT::datatable(outp)
       return(ret)
     })
     
     
######### PARTIE CHECKLIST 1
     
     checklist1 = data.frame()
     row.names(checklist1) = NULL
     output$tablechecklist1 = DT::renderDT(expr = checklist1,server = F)
     
     observeEvent(input$checklist_1, { 
       #cat(file=stderr(), "test", input$nSabot, "\n")
       
       if (!is.na(input$nSabot)) {
         checklist1 <<- data.frame("nSabot" = input$nSabot)}
       else {checklist1 <<- data.frame("nSabot"= c("NULL"))}
       
       if (!is.na(input$nAnimal)) {
         checklist1 <<- cbind(checklist1,data.frame("nAnimal" = input$nAnimal))}
       else {checklist1 <<- cbind(checklist1,data.frame("nAnimal"= c("NULL")))}
       
       if ((input$idSite)!="") {
         checklist1 <<- cbind(checklist1,data.frame("idSite" = input$idSite))}
       else {checklist1 <<- cbind(checklist1,data.frame("idSite"= c("NULL")))}
       
       if ((input$idRFID)!="") {
         checklist1 <<- cbind(checklist1,data.frame("idRFID" = input$idRFID))}
       else {checklist1 <<- cbind(checklist1,data.frame("idRFID"= c("NULL")))}
       
       if ((input$idTagOrG)!="") {
         checklist1 <<- cbind(checklist1,data.frame("Tag_gauche" = input$idTagOrG))}
       else {checklist1 <<- cbind(checklist1,data.frame("Tag_gauche"= c("NULL")))}
       
       if ((input$idTagOrD)!="") {
         checklist1 <<- cbind(checklist1,data.frame("Tag_droit" = input$idTagOrD))}
       else {checklist1 <<- cbind(checklist1,data.frame("Tag_droit"= c("NULL")))}
       
       if (!is.na(input$lPattArriere)) {
         checklist1 <<- cbind(checklist1,data.frame("lPattArriere" = input$lPattArriere))}
       else {checklist1 <<- cbind(checklist1,data.frame("lPattArriere"= c("NULL")))}
       
       if ((input$sexe)!="") {
         checklist1 <<- cbind(checklist1,data.frame("sexe" = input$sexe))}
       else {checklist1 <<- cbind(checklist1,data.frame("sexe"= c("NULL")))}
       
       if (!is.na(input$lBoisGauche)& (input$sexe==0)) {
         checklist1 <<- cbind(checklist1,data.frame("lBoisGauche" = input$lBoisGauche))}
       else if (is.na(input$lBoisGauche)& (input$sexe==0)) {checklist1 <<- cbind(checklist1,data.frame("lBoisGauche"= c("NULL")))}
       
       if (!is.na(input$lBoisDroit) & (input$sexe==0)) {
         checklist1 <<- cbind(checklist1,data.frame("lBoisDroit" = input$lBoisDroit))}
       else if (is.na(input$lBoisDroit)& (input$sexe==0)) {checklist1 <<- cbind(checklist1,data.frame("lBoisDroit"= c("NULL")))}
       
       if (((input$etatBois)!="") &(input$sexe==0)){
         checklist1 <<- cbind(checklist1,data.frame("etatBois" = input$etatBois))}
       else if (((input$etatBois)!="")& (input$sexe==0)) {checklist1 <<- cbind(checklist1,data.frame("etatBois"= c("NULL")))}
       
       if (!is.na(input$tglucose)) {
         checklist1 <<- cbind(checklist1,data.frame("Glucose" = input$tglucose))}
       else {checklist1 <<- cbind(checklist1,data.frame("Glucose"= c("NULL")))}
       
       if (!is.na(input$cirCou)) {
         checklist1 <<- cbind(checklist1,data.frame("cirCou" = input$cirCou))}
       else {checklist1 <<- cbind(checklist1,data.frame("cirCou"= c("NULL")))}
       
       output$tablechecklist1 = DT::renderDT(checklist1,server = F) 
       
     })
 
     
    # CHECKLIST TABLE
        
        checklist_table = data.frame()
        row.names(checklist_table) = NULL
        output$tablechecklist_table = DT::renderDT(expr = checklist_table,server = F)
        
        observeEvent(input$checklist_tab, { 
          #cat(file=stderr(), "test", input$nSabot, "\n")
          
          if (!is.na(input$ExtTemp)) {
            checklist_table <<- data.frame("ExtTemp" = input$ExtTemp)}
          else {checklist_table <<- data.frame("ExtTemp"= c("NULL"))}
          
          if (!is.na(input$rectTemp)) {
            checklist_table <<- data.frame("rectTemp" = input$rectTemp)}
          else {checklist_table <<- data.frame("rectTemp"= c("NULL"))}
          
          if (!is.null(input$lutte)) {
            checklist_table <<- cbind(checklist_table,data.frame("lutte" = input$lutte))}
          else {checklist_table <<- cbind(checklist_table,data.frame("lutte"= c("NULL")))}
          
          if (!is.null(input$halete)) {
            checklist_table <<- cbind(checklist_table,data.frame("halete" = input$halete))}
          else {checklist_table <<- cbind(checklist_table,data.frame("halete"= c("NULL")))}
          
          if (!is.null(input$cribague)) {
            checklist_table <<- cbind(checklist_table,data.frame("cribague" = input$cribague))}
          else {checklist_table <<- cbind(checklist_table,data.frame("cribague"= c("NULL")))}
          
          if (!is.null(input$criautre)) {
            checklist_table <<- cbind(checklist_table,data.frame("criautre" = input$criautre))}
          else {checklist_table <<- cbind(checklist_table,data.frame("criautre"= c("NULL")))}
     
          if ((input$Notation_euro_table)!="") {
            checklist_table <<- cbind(checklist_table,data.frame("Notation_euro_table" = input$Notation_euro_table))}
          else {checklist_table <<- cbind(checklist_table,data.frame("Notation_euro_table"= c("NULL")))}
          
          output$tablechecklist_table = DT::renderDT(checklist_table,server = F) 
          
        })
          
          
          
          
####### Partie comportement lacher :
     
     observeEvent(input$to_current_time, {
       updateTimeInput(session, "time", value = Sys.time())
     })
     
     observeEvent(input$to_current_time2, {
       updateTimeInput(session, "time2", value = Sys.time())
     })
   
     observeEvent(input$checklist_2, { 
      # cat(file=stderr(), "visi", input$titube, "\n")
       
       if (is.null(input$vitesse) | is.null(input$titube) | is.null(input$couche) | is.null(input$cabriole_saut)
           | is.null(input$cri) | is.null(input$allure) | is.null(input$gratte_collier) | is.null(input$tombe)
           | (input$habitat)=="" | (input$Notation_euro)=="" | (input$habitat_perte)=="" | is.na(input$nbre_stops) | (input$visibilite)=="" | is.na(input$nbre_personnes))
       {shinyalert("STOP!", "Données manquantes", type = "warning",confirmButtonText="Valider quand même", showCancelButton=T,cancelButtonText="Annuler", callbackR = modalCallback2)} 
       
       else 
       {shinyalert("Nice!", "Parfait", type = "success",showCancelButton=T, callbackR = modalCallback2)} 
     })



######### CHECKLIST 2


checklist2 = data.frame()
row.names(checklist2) = NULL
output$tablechecklist2 = DT::renderDT(expr = checklist2,server = F)


observeEvent(input$checklist_2, { 

  if (!is.null(input$vitesse))  {
    checklist2 <<- data.frame("Vitesse" = input$vitesse)}
  else {checklist2 <<- data.frame("Vitesse"= c("NULL"))}
  
  if (!is.null(input$titube)) {
    checklist2 <<- cbind(checklist2,data.frame("titube" = input$titube))}
  else {checklist2 <<- cbind(checklist2,data.frame("titube"= c("NULL")))}
 
   if (!is.null(input$couche)) {
    checklist2 <<- cbind(checklist2,data.frame("couche" = input$couche))}
  else {checklist2 <<- cbind(checklist2,data.frame("couche"= c("NULL")))}
  
  if (!is.null(input$cabriole_saut)) {
    checklist2 <<- cbind(checklist2,data.frame("cabriole_saut" = input$cabriole_saut))}
  else {checklist2 <<- cbind(checklist2,data.frame("cabriole_saut"= c("NULL")))}
  
  if (!is.null(input$cri)) {
 checklist2 <<- cbind(checklist2,data.frame("cri" = input$cri))}
  else {checklist2 <<- cbind(checklist2,data.frame("cri"= c("NULL")))}
  
  if (!is.null(input$allure)) {
    checklist2 <<- cbind(checklist2,data.frame("allure" = input$allure))}
  else {checklist2 <<- cbind(checklist2,data.frame("allure"= c("NULL")))}
  
  if (!is.null(input$gratte_collier)) {
    checklist2 <<- cbind(checklist2,data.frame("Gratte_Collier" = input$gratte_collier))}
  else {checklist2 <<- cbind(checklist2,data.frame("Gratte_Collier"= c("NULL")))}
  
  if (!is.null(input$tombe)) {
    checklist2 <<- cbind(checklist2,data.frame("tombe" = input$tombe))}
  else {checklist2 <<- cbind(checklist2,data.frame("tombe"= c("NULL")))}
  
  if ((input$habitat)!="") {
    checklist2 <<- cbind(checklist2,data.frame("habitat" = input$habitat))}
  else {checklist2 <<- cbind(checklist2,data.frame("habitat"= c("NULL")))}
  
  if ((input$Notation_euro)!="") {
    checklist2 <<- cbind(checklist2,data.frame("Eurodeer" = input$Notation_euro))}
  else {checklist2 <<- cbind(checklist2,data.frame("Eurodeer"= c("NULL")))}
  
  if ((input$habitat_perte)!="") {
    checklist2 <<- cbind(checklist2,data.frame("Habitat" = input$habitat_perte))}
  else {checklist2 <<- cbind(checklist2,data.frame("Habitat"= c("NULL")))}
  
  if (!is.na(input$nbre_stops)) {
    checklist2 <<- cbind(checklist2,data.frame("Stops" = input$nbre_stops))}
  else {checklist2 <<- cbind(checklist2,data.frame("Stops"= c("NULL")))}
  
  if ((input$visibilite)!="") {
    checklist2 <<- cbind(checklist2,data.frame("Visibilite" = input$visibilite))}
  else {checklist2 <<- cbind(checklist2,data.frame("Visibilite"= c("NULL")))}
  
  if (!is.na(input$nbre_personnes)) {
    checklist2 <<- cbind(checklist2,data.frame("Nbre_personnes" = input$nbre_personnes))}
  else {checklist2 <<- cbind(checklist2,data.frame("Nbre_personnes"= c("NULL")))}
  
  
  output$tablechecklist2 = DT::renderDT(checklist2,server = F) 

  })

######## CAPTURE

#checklist_capt = data.frame()
#row.names(checklist_capt) = NULL
#output$tablechecklist_capt = DT::renderDT(expr = checklist_capt,server = F)

observeEvent(input$checklist_capture, { 

cpt_capt=0
  
  if ((input$nSabot)=="")  
    {shinyalert("STOP!", "Numéro Sabot manquant", type = "warning",confirmButtonText="Valider quand même", showCancelButton=T,cancelButtonText="Annuler")} 
  else
    {cpt_capt=cpt_capt+1} 
  
  if ((input$nom_capteur_txt)=="") 
    {shinyalert("STOP!", "Nom Capteur manquant", type = "warning",confirmButtonText="Valider quand même", showCancelButton=T,cancelButtonText="Annuler")} 
  else {cpt_capt=cpt_capt+1}  

  #if ((input$Nbre_pers_experimentes)==1) 
  #  {shinyalert("STOP!", "Nbre personnes experimentées = 1", type = "warning",confirmButtonText="Valeur exacte", showCancelButton=T,cancelButtonText="Annuler")} 
 # else {cpt_capt=cpt_capt+1} 

  if (is.null(input$cpt_filet_vitesse)) 
    {shinyalert("STOP!", "Donnée vitesse au filet manquante", type = "warning",confirmButtonText="Valider quand même", showCancelButton=T,cancelButtonText="Annuler")} 
  else {cpt_capt=cpt_capt+1} 

if (is.null(input$cpt_filet_allure)) 
{shinyalert("STOP!", "Donnée allure au filet manquante", type = "warning",confirmButtonText="Valider quand même", showCancelButton=T,cancelButtonText="Annuler")} 
else {cpt_capt=cpt_capt+1} 

if (is.null(input$cpt_filet_lutte)) 
{shinyalert("STOP!", "Donnée lutte au filet manquante", type = "warning",confirmButtonText="Valider quand même", showCancelButton=T,cancelButtonText="Annuler")} 
else {cpt_capt=cpt_capt+1} 

if (is.null(input$cpt_filet_halete)) 
{shinyalert("STOP!", "Donnée halete au filet manquante", type = "warning",confirmButtonText="Valider quand même", showCancelButton=T,cancelButtonText="Annuler")} 
else {cpt_capt=cpt_capt+1} 

if (is.null(input$cpt_filet_cri)) 
{shinyalert("STOP!", "Donnée cri au filet manquante", type = "warning",confirmButtonText="Valider quand même", showCancelButton=T,cancelButtonText="Annuler")} 
else {cpt_capt=cpt_capt+1} 

if (cpt_capt==7)
{shinyalert("PARFAIT!", "Toutes les données rentrées", type="success",confirmButtonText="Valider", showCancelButton=T,cancelButtonText="Annuler")} 


})


####### SABOT


observeEvent(input$checklist_sabot, { 
  
  cpt_sabot=0
  
  if ((input$cpt_dose_acepromazine)=="")  
  {shinyalert("STOP!", "Dose acepromazine manquante", type = "warning",confirmButtonText="Valider quand même", showCancelButton=T,cancelButtonText="Annuler")} 
  else {cpt_sabot=cpt_sabot+1} 
  
  if ((input$Observateur)=="") 
  {shinyalert("STOP!", "Observateur manquant", type = "warning",confirmButtonText="Valider quand même", showCancelButton=T,cancelButtonText="Annuler")} 
  else {cpt_sabot=cpt_sabot+1}  
  
  if (is.null(input$cpt_sabot_retournement)) 
  {shinyalert("STOP!", "Donnée retournement sabot manquante", type = "warning",confirmButtonText="Valider quand même", showCancelButton=T,cancelButtonText="Annuler")} 
  else {cpt_sabot=cpt_sabot+1} 
  
  if (is.null(input$cpt_sabot_couche)) 
  {shinyalert("STOP!", "Donnée couché sabot manquante", type = "warning",confirmButtonText="Valider quand même", showCancelButton=T,cancelButtonText="Annuler")} 
  else {cpt_sabot=cpt_sabot+1} 
  
  if (is.null(input$cpt_sabot_agitation)) 
  {shinyalert("STOP!", "Donnée agitation sabot manquante", type = "warning",confirmButtonText="Valider quand même", showCancelButton=T,cancelButtonText="Annuler")} 
  else {cpt_sabot=cpt_sabot+1} 
  
  if (cpt_sabot==5)
  {shinyalert("PARFAIT!", "Toutes les données rentrées", type="success",confirmButtonText="Valider", showCancelButton=T,cancelButtonText="Annuler")} 
  
  
})




######## AJOUTER VALEURS DE LA CHECKLIST DANS BASE DE DONNEES

# pour obtenir le cpt_id suivant

max_value=dbGetQuery(con,paste0('SELECT cpt_id FROM cmpt.t_capture_cpt order by cpt_id desc limit 1'))
max_value=as.integer((max_value[1,1])+1)


modalCallback2 <- function(value) {
  if (value == TRUE) {
    dbSendQuery(con,sprintf("INSERT INTO cmpt.t_capture_cpt (cpt_id,cpt_ani_etiq, cpt_date,cpt_annee_suivi, cpt_lache_visibilite, cpt_cap_id)
VALUES (%s,100,'1961-06-16',1111,'exemple',000)",max_value))
  }}

}

################## LANCEMENT DE L'APPLICATION :

#dbDisconnect(con)

shinyApp(ui = ui, server = server)

