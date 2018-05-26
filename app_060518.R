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

bleTraitChoices =  dbGetQuery(con,"select distinct blt_traitement from lu_tables.tr_blessure_traitement_blt")
#("allumisol", "serflex_allumisol","points", "euthanasie", "rien")

blelocalisationChoices = dbGetQuery(con,"select distinct (bll_localisation) from lu_tables.tr_blessure_loc_bll")




################## FORMULAIRE CARACTERISTIQUES DE L'ANIMAL



contentcaractanimal = fluidPage(
  #titlePanel("Caract. de l'animal"),

  fluidRow(
    uiOutput("out_sabot"),
    column(2, numericInput(inputId = "numSabot", value =0,label = h4("N Sabot"),min=0,max=28 )),
    column(2, numericInput(inputId = "pSabotPlein", value = " ",label = h4("Poids Sabot Plein"),min=0,max=65 )),
    column(2, numericInput(inputId = "pSabotVide", value = " ",label = h4("Poids Sabot Vide"),min=0,max=50 )),
    column(2, h4("Poids Animal"),textOutput("value")),
    column(12,hr()),
    column(2, checkboxInput(inputId = "estNouvelAnimal", value = T,label = h4("Nouvel Animal"))),
  
    
    column(2,conditionalPanel( condition = "input.estNouvelAnimal == 1",
             numericInput(inputId = "nAnimal", value = " ",label = h4("N° Animal"),min=0 ))),
    
    column(2,conditionalPanel(condition = "input.estNouvelAnimal == 1", selectizeInput("idSite", h4("Site"),choices = dbGetQuery(con,"select sit_nom_court from public.tr_site_capture_sit"),
             options=list(placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }')), selected = NULL))),
    
    column(2,conditionalPanel(condition = "input.estNouvelAnimal == 0",
              selectizeInput("nAnimal2",h4("N° Animal"), choices = dbGetQuery(con,"select ani_etiq from public.t_animal_ani"),
                  options=list(placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }')), selected = NULL))),
   
     column(2,conditionalPanel(condition = "input.estNouvelAnimal == 0", h4("Site"), textOutput("out_nAnimal2"))),
    
    column(12),
    column(2,timeInput("time_caract", h4("Heure de lâcher:"),seconds = FALSE),
           actionButton("to_current_time_caract", "Afficher l'heure")),

    # column(2, selectizeInput("idRFID", h4("RFID"),
    #           choices = dbGetQuery(con,"select (rfi_cap_id) from public.tr_site_capture_sit where ani_etiq='",input.nAnimal2,"'"),options=list(placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }')), selected = NULL)),

    # column(2,conditionalPanel(condition = "input.estNouvelAnimal == 1", h4("Tag Gauche"), textOutput("out_idTagOrG"))),
    # column(2,conditionalPanel(condition = "input.estNouvelAnimal == 1", h4("Tag Gauche"), textOutput("out_idTagOrG"))),
    
    
    column(2, numericInput("idTagOrG", h4("Tag Oreille Gauche"),value="0")),
    column(2, numericInput("idTagOrD", h4("Tag Oreille Droite"),value="0")),
    
    
    column(2,conditionalPanel(condition = "input.estNouvelAnimal == 0", h4("Tag Gauche"), textOutput("out_idTagOrG"))),
    column(2,conditionalPanel(condition = "input.estNouvelAnimal == 0", h4("Tag Droite"), textOutput("out_idTagOrD"))),
    
    
    column(12,hr()),
    column(2, dateInput('date_caract',label=h4("Date"),value = Sys.Date())),
    column(2, radioButtons("sexe",h4("Sexe"),choiceNames = list("M","F"), choiceValues = list("M","F")))
          ),
  
  hr(),
  
  fluidRow(
    column(2, numericInput("cirCou", value='0', h4("Circonférence cou"),min=0, max=(dbGetQuery(con,"select max(cap_circou) from t_capture_cap")))),
    uiOutput("out_cirCou"),
    column(2, numericInput("lPattArriere", value='0', h4("Longueur patte arrière"),min=0, max=(dbGetQuery(con,"select max(cap_lpa) from t_capture_cap")))),
    uiOutput("out_lPattArriere"),
    column(2, numericInput("tglucose", value="", h4("Taux de Glucose sanguin"), min=0))
          ),
  
  conditionalPanel(
      condition = "input.sexe == 'M'",
 
      fluidRow(
        column(2, numericInput("lBoisGauche", value='0', h4("Longueur bois gauche"),min=0, max=(dbGetQuery(con,"select max(nca_valeur) from public.tj_mesureenum_capture_nca")))),
        column(2, numericInput("lBoisDroit", value='0', h4("Longueur bois droit"),min=0, max=(dbGetQuery(con,"select max(nca_valeur) from public.tj_mesureenum_capture_nca")))),
        #uiOutput("out_lBoisGauche"), 
        #uiOutput("out_lBoisDroit"),
        column(2, selectizeInput("etatBois", h4("État bois"), choices = list("Velours", "tombés", "durs"), options = list(create = TRUE)))
              )
                 )
)



################## FORMULAIRE BLESSURES

contentblessures = fluidPage( 
 # titlePanel("Blessures"),
  fluidRow(
    
    column(3, selectizeInput("blelocalisation_sel", h4("Localisation"), 
                             choices = blelocalisationChoices,options=list(placeholder='Choisir une valeur :',create = TRUE, onInitialize = I('function() { this.setValue(""); }')), selected = NULL) 
           #bsModal("nouvelleLocalization_modal", "Entrer la localisation","", size = "large",wellPanel(
            # textInput("nouvelle_localisation_txt",""),
             #actionButton("ok_button", "OK"),
             #actionButton("quit_button", "Quitter")
           #))
           #textInput("blelocalisation_txt","")
    ),
    
    column(3, selectInput("bleGrav_sel", h4("Gravité"), choices = bleGravChoices, selected = "superficielle")),
              #textInput("bleGrav_txt","") ),
    column(3, selectizeInput("bleTrait_sel", h4("Traitement"), choices = bleTraitChoices,options = list(placeholder='Choisir une valeur :',create = TRUE, onInitialize = I('function() { this.setValue(""); }')), selected = NULL)),
              #textInput("bleTrait_txt","")),
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

contentprelevement = fluidPage(
  
  fluidRow(
    column(2,selectizeInput("diarrhee", h4("Diarrhée ?"),choices = list(TRUE,FALSE),options=list(placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }')), selected = NULL)),
    column(2,selectizeInput("tiques", h4("Nombre Tiques"), choices = c(1:30,'>30'), options=list(placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }')), selected = NULL))
    ),
  
  fluidRow(
    
    column(2, selectizeInput("type_prelev", h4("Type de prélèvement"), 
                            choices = dbGetQuery(con,"select distinct (sat_type) from lu_tables.tr_samples_types_sat"),options=list(placeholder='Choisir une valeur :',create = TRUE, onInitialize = I('function() { this.setValue(""); }')), selected = NULL)), 
    column(2, selectizeInput("local_prelev", h4("Localisation"), 
                            choices = dbGetQuery(con,"select distinct (sal_localisation) from lu_tables.tr_samples_loc_sal"),options=list(placeholder='Choisir une valeur :',create = TRUE, onInitialize = I('function() { this.setValue(""); }')), selected = NULL)), 
    column(2, selectizeInput("cont_prelev", h4("Contenant"), 
                            choices = dbGetQuery(con,"select distinct (sac_conditionnement) from lu_tables.tr_samples_contenant_sac"),options=list(placeholder='Choisir une valeur :',create = TRUE, onInitialize = I('function() { this.setValue(""); }')), selected = NULL)), 
    column(2, selectizeInput("solv_prelev", h4("Solvant"), 
                            choices = dbGetQuery(con,"select distinct (sas_solvant) from lu_tables.tr_samples_solvant_sas"),options=list(placeholder='Choisir une valeur :',create = TRUE, onInitialize = I('function() { this.setValue(""); }')), selected = NULL)), 
    column(2, selectizeInput("nbre_echant", h4("Nombre d'échantillons"), 
                            choices =list( 1,2,3,4,5) ,selected = NULL)),
    column(3, actionButton("ajout_prelev","Ajouter un prelevement"))
  ),
  
 # if (input.type_prelev=="sang" && input.local_prelev=="jugulaire" && input.cont_prelev=="tube rouge" && input.solv_prelev=="sec") {} 
  
  
  hr(),
  
  fluidRow(
    sidebarLayout(
      mainPanel(
        dataTableOutput("tableprelevement")),
      sidebarPanel(actionButton("sup_prelev", "Supprimer prelevement"))
    ))
)
  
  
  



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
      choices = dbGetQuery(con,"select distinct (t_capture_cpt.cpt_lache_habitat_lache) from cmpt.t_capture_cpt"), options=list(placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }'),create = TRUE), selected = NULL)),
    
    column(2, selectizeInput("habitat_perte", h4("Habitat perte de vue"), 
      choices = dbGetQuery(con,"select distinct (t_capture_cpt.cpt_lache_habitat_pertevue) from cmpt.t_capture_cpt"),options=list(placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }'),create = TRUE), selected = NULL)),
    
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
    
    column(2,dateInput('date_capture',label=h4("Date"),value ='2017-01-01')),
    column(2,selectizeInput("numSabot_capture",label = h4("N° Sabot"), choices = dbGetQuery(con,"select distinct cap_num_sabot FROM public.t_capture_cap"),options=list(placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }')), selected = NULL)),
    column(2,timeInput("cpt_heure_debut_filet",h4("Heure arrivée filet"),seconds = FALSE)),
    
    #column(3, 
    #selectInput("N° sabot", h4("N°sabot"), choices = list("Velours", "tombés", "durs"), selected = 1),
    #numericInput(inputId = "numSabot", value = " ",label = h4("N° Sabot"), min=1, max=28)
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
          tabPanel ("Prélèvement", prelevement),
          tabPanel  ("Collier",caractcollier),
          tabPanel  ("Table",comportable),
          tabPanel  ("historique",historique),
          tabPanel  ("Checklist 1",checklist1),
          tabPanel ( "Lâcher",comporlacher),
          tabPanel  ("Checklist 2",checklist2),
          tabPanel  ("Capture",comporcapture),
          tabPanel( "Sabot",comporsabot)
          #tabPanel("Summary", verbatimTextOutput("summary")),
          #tabPanel("Table", tableOutput("table"))
        )
)




################################################################################################ 
################################################################################################ SERVER :
################################################################################################ 


server <- function(input, output,session) {
   
   output$value = renderText({input$pSabotPlein-input$pSabotVide})

## Caracteristiques :
   
   output$bla <- renderUI ({
     print("ba")
     print(input$date_capture)
           })
   
   output$out_sabot <- renderUI({
        if (input$numSabot>28) {
        shinyalert("STOP!", "Est-ce un nouveau numéro de sabot ?", type = "warning",confirmButtonText="Oui", showCancelButton=T,cancelButtonText="Non",html=TRUE )
   } })
 
   # Pour vérifier que le numéro de l'animal n'existe pas déjà :
   
   # output$out_nAnimal <- renderUI({
   #   if (input$nAnimal){}
   # })
 
    output$out_nAnimal2 <- renderText({

      if ((input$nAnimal2)!="") {
      str = paste("select sit_nom_court from public.tr_site_capture_sit where (sit_id in (select cap_sit_id from public.t_capture_cap where cap_ani_id =", input$nAnimal2, "))", sep = "")
      resres = dbGetQuery(con,str)
      idSite2 <<- resres[1, 1]
      idSite2}
    })
    
    output$out_idTagOrG <- renderText({
      if ((input$nAnimal2)!="") {
      str = paste("select cap_tag_gauche from public.t_capture_cap where cap_ani_id =", input$nAnimal2, sep = "")
      resres = dbGetQuery(con,str)
      idTagOrG <<- resres[1, 1]
      idTagOrG}
    })
    
    output$out_idTagOrD <- renderText({
      if ((input$nAnimal2)!="") {
        str = paste("select cap_tag_droit from public.t_capture_cap where cap_ani_id =", input$nAnimal2, sep = "")
        resres = dbGetQuery(con,str)
        idTagOrD <<- resres[1, 1]
        idTagOrD}
    })

    output$out_cirCou <- renderUI({
      if (input$cirCou > dbGetQuery(con,"select max(cap_circou) from t_capture_cap")) {
      shinyalert("STOP!", "Circonférence elevée", type = "warning",confirmButtonText="Oui", showCancelButton=T,cancelButtonText="Non",html=TRUE )
    }})
    
    output$out_lPattArriere <- renderUI({
      if (input$lPattArriere > dbGetQuery(con,"select max(cap_lpa) from t_capture_cap")) {
        shinyalert("STOP!", "Longueur patte elevée", type = "warning",confirmButtonText="Oui", showCancelButton=T,cancelButtonText="Non",html=TRUE )
      }})
    

    output$out_lBoisGauche <- renderUI({
      if (input$lBoisGauche > dbGetQuery(con,"select max(nca_valeur) from public.tj_mesureenum_capture_nca")) {
        shinyalert("STOP!", "Longueur bois gauche elevée", type = "warning",confirmButtonText="Valider", showCancelButton=T,cancelButtonText="Annuler",html=TRUE )
      }})

    output$out_lBoisDroit <- renderUI({
      if (input$lBoisDroit > dbGetQuery(con,"select max(nca_valeur) from public.tj_mesureenum_capture_nca")) {
        shinyalert("STOP!", "Longueur bois droit elevée", type = "warning",confirmButtonText="Valider", showCancelButton=T,cancelButtonText="Annuler",html=TRUE )
      }})
    
    observeEvent(input$to_current_time_caract, {
      updateTimeInput(session, "time_caract", value = Sys.time())
    })

   
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
          # loca = ""
          # grav = ""
          # trait = ""
          # if (input$blelocalisation_txt != "") {
          #   loca = input$blelocalisation_txt
          #   x = input$blelocalisation_sel
          #   #blelocalisationChoices[[length(blelocalisationChoices)-1]] =
          #   blelocalisationChoices <<- cbind(blelocalisationChoices,input$blelocalisation_txt)
          #   updateSelectInput(session,"blelocalisation_sel",
          #                     choices = blelocalisationChoices,
          #                     selected = input$blelocalisation_txt
          #   )
          # } else {
          #   loca = input$blelocalisation_sel
          # }
          # if (input$bleGrav_txt != "") {
          #   grav = input$bleGrav_txt
          # } else {
          #   grav = input$bleGrav_sel
          # }
          # if (input$bleTrait_txt != "") {
          #   trait = input$bleTrait_txt
          # } else {
          #   trait = input$bleTrait_sel
          # }
          
          blessure <<- rbind(blessure,data.frame("Localisation" = c(input$blelocalisation_sel), "Gravité" =c(input$bleGrav_sel), "Traitement" = c(input$bleTrait_sel)))
          output$tableblessure = DT::renderDT(blessure,server = F)

   }
   ) 
   
   
######## PARTIE PRELEVEMENTS
   
   prelevement = data.frame()
   row.names(prelevement) = NULL
   
   output$tableprelevement = DT::renderDT(expr = prelevement,server = F)
   
   sup_Ligne_prelev = observeEvent(input$sup_prelev, {
     if (!is.null(input$tableprelevement_rows_selected)) {
       prelevement <<- prelevement[-as.numeric(input$tableprelevement_rows_selected),]
       output$tableprelevement = DT::renderDT(prelevement,server = F)
     }
   })
   
   # observeEvent (input$type_prelev, {
   #   if (!is.null(input$type_prelev)) {
   #     if( input$type_prelev == "Autre localisation"){
   #       toggleModal(session,"nouvelleLocalization_modal","open")}
   #  }}) 
   
   observeEvent(input$ajout_prelev, {
     prelevement <<- rbind(prelevement,data.frame("Type" = c(input$type_prelev), "Localisation" =c(input$local_prelev), "Contenant" = c(input$cont_prelev),"Solvant" = c(input$solv_prelev),"Nombre d'échantillons" = c(input$nbre_echant)))
     output$tableprelevement = DT::renderDT(prelevement,server = F)
   })
  
   
######## PARTIE TABLE
   
   
observeEvent(input$to_current_time_table, {
  updateTimeInput(session, "time_table", value = Sys.time())
   })
   
######### Partie historique :
   
     # 
     # output$historique <- DT::renderDataTable({
     #   outp <- dbGetQuery(con,paste0("select t.ani_etiq as ani, t.ani_sexe as s, t.cap_date as date, t.cap_poids as poids, t.cap_lpa as lpa, t.cap_age_classe as age, t.sit_nom_court as site, 
     #                                 t.teq_nom_court as teq, t.eqa_date_debut as debut, t.eqa_date_fin as fin, t.cap_annee_suivi as an, round(t.temps_suivi/30.43) as mois,  count(t.cpos_id) as locs, t.eqt_id_usuel as equip, t.mar_libelle as marque, t.mod_libelle as modele, array_to_string( array_agg( distinct eqc_sen_id), ', ') as capteurs from (SELECT eqc_sen_id, cpos_id, ani_etiq, ani_sexe, cap_date, cap_poids, cap_lpa, cap_age_classe, sit_nom_court, 
     #                                 teq_nom_court, cap_annee_suivi, eqa_date_debut, eqa_date_fin, eqa_date_fin - eqa_date_debut as temps_suivi, eqt_id_usuel, mar_libelle, mod_libelle
     #                                 FROM public.v_aniposi_gpsgsm, public.t_equipement_conf_eqc ) as t where t.ani_etiq = '",input$ani_etiq,"' group by t.ani_etiq, t.ani_sexe, t.cap_date, t.cap_poids, t.cap_lpa, t.cap_age_classe, t.sit_nom_court, 
     #                                 t.teq_nom_court, t.cap_annee_suivi, t.eqa_date_debut, t.eqa_date_fin, t.temps_suivi, t.eqt_id_usuel, t.mar_libelle, t.mod_libelle order by cap_date"))
     # 
     #   ret <- DT::datatable(outp)
     #   return(ret)
     # })
     # 
     
######### PARTIE CHECKLIST 1
     
     checklist1 = data.frame()
     row.names(checklist1) = NULL
     output$tablechecklist1 = DT::renderDT(expr = checklist1,server = F)
     
     observeEvent(input$checklist_1, { 
       #cat(file=stderr(), "test", input$numSabot, "\n")
       
       if ((input$numSabot)!=0) {
         checklist1 <<- data.frame("Numéro Sabot" = input$numSabot)}
       else {checklist1 <<- data.frame("Numéro Sabot"= c("NULL"))}

       if (!is.na(input$nAnimal) & (input$estNouvelAnimal == 1)) {
         checklist1 <<- cbind(checklist1,data.frame("N°Animal" = input$nAnimal))}
       else if (is.na(input$nAnimal) & (input$estNouvelAnimal == 1)) {checklist1 <<- cbind(checklist1,data.frame("N°Animal"= c("NULL")))}
       
       if ((input$estNouvelAnimal == 0) & !is.na(input$nAnimal2)) {
         checklist1 <<- cbind(checklist1,data.frame("N°Animal" = input$nAnimal2))}
       else if ((input$estNouvelAnimal == 0) & is.na(input$nAnimal2)){checklist1 <<- cbind(checklist1,data.frame("n°Animal"= c("NULL")))}

       if ((input$estNouvelAnimal == 0) & (idSite2)!="") {
         checklist1 <<- cbind(checklist1,data.frame("Site" = idSite2))}
       else if ((idSite2=="") & (input$estNouvelAnimal==0)){checklist1 <<- cbind(checklist1,data.frame("Site"= c("NULL")))}
       
       if ((input$estNouvelAnimal == 1) & (input$idSite)!="") {
         checklist1 <<- cbind(checklist1,data.frame("Site" = input$idSite))}
       else if ((input$idSite)=="" & (input$estNouvelAnimal==1)){checklist1 <<- cbind(checklist1,data.frame("Site"= c("NULL")))}
       
       # if ((input$idRFID)!="") {
       #   checklist1 <<- cbind(checklist1,data.frame("idRFID" = input$idRFID))}
       # else {checklist1 <<- cbind(checklist1,data.frame("idRFID"= c("NULL")))}
       
       if ((input$idTagOrG)!="0" & input$estNouvelAnimal == 1) {
         checklist1 <<- cbind(checklist1,data.frame("Tag Gauche" = input$idTagOrG))}
       else if ((input$idTagOrG)=="0" & input$estNouvelAnimal == 1){checklist1 <<- cbind(checklist1,data.frame("Tag Gauche"= c("NULL")))}
       
       if ((input$idTagOrD)!="0" & input$estNouvelAnimal == 1) {
         checklist1 <<- cbind(checklist1,data.frame("Tag Droit" = input$idTagOrD))}
       else if ((input$idTagOrD=="0") & input$estNouvelAnimal == 1){checklist1 <<- cbind(checklist1,data.frame("Tag Droit"= c("NULL")))}
       
       if (((idTagOrG!="")|(!is.na(idTagOrG))|!is.null(idTagOrG)) & input$estNouvelAnimal == 0) {
         checklist1 <<- cbind(checklist1,data.frame("Tag Gauche" = idTagOrG))}
       else if ((idTagOrG=='') & input$estNouvelAnimal == 0) {checklist1 <<- cbind(checklist1,data.frame("Tag Gauche"= c("NULL")))}
       
       if (((idTagOrD!="")|(!is.na(idTagOrD))|!is.null(idTagOrD)) & input$estNouvelAnimal == 0) {
         checklist1 <<- cbind(checklist1,data.frame("Tag Droit" = idTagOrD))}
       else if (idTagOrD=="" & input$estNouvelAnimal == 0) {checklist1 <<- cbind(checklist1,data.frame("Tag Droit"= c("NULL")))}
       
       if (!is.na(input$lPattArriere)) {
         checklist1 <<- cbind(checklist1,data.frame("Longueur patte" = input$lPattArriere))}
       else {checklist1 <<- cbind(checklist1,data.frame("Longueur patte"= c("NULL")))}
       
       if ((input$sexe)!="") {
         checklist1 <<- cbind(checklist1,data.frame("Sexe" = input$sexe))}
       else {checklist1 <<- cbind(checklist1,data.frame("Sexe"= c("NULL")))}
       
       if ((input$lBoisGauche!=0) & (input$sexe=='M')) {
         checklist1 <<- cbind(checklist1,data.frame("Longueur bois G" = input$lBoisGauche))}
       else if ((input$lBoisGauche==0)& (input$sexe=='M')) {checklist1 <<- cbind(checklist1,data.frame("Longueur bois G"= c("NULL")))}
       
       if ((input$lBoisDroit!=0) & (input$sexe=='M')) {
         checklist1 <<- cbind(checklist1,data.frame("Longueur bois D" = input$lBoisDroit))}
       else if ((input$lBoisDroit==0) & (input$sexe=='M')) {checklist1 <<- cbind(checklist1,data.frame("Longueur bois D"= c("NULL")))}
       
       if (((input$etatBois)!="") &(input$sexe=='M')){
         checklist1 <<- cbind(checklist1,data.frame("Etat bois" = input$etatBois))}
       else if (((input$etatBois)=="")& (input$sexe=='M')) {checklist1 <<- cbind(checklist1,data.frame("Etat bois"= c("NULL")))}
       
       if (!is.na(input$tglucose)) {
         checklist1 <<- cbind(checklist1,data.frame("Glucose" = input$tglucose))}
       else {checklist1 <<- cbind(checklist1,data.frame("Glucose"= c("NULL")))}
       
       if ((input$cirCou)!=0) {
         checklist1 <<- cbind(checklist1,data.frame("Cou" = input$cirCou))}
       else {checklist1 <<- cbind(checklist1,data.frame("Cou"= c("NULL")))}
       
       output$tablechecklist1 = DT::renderDT(checklist1,server = F) 
       
     })
 
     
    # CHECKLIST TABLE
        
        checklist_table = data.frame()
        row.names(checklist_table) = NULL
        output$tablechecklist_table = DT::renderDT(expr = checklist_table,server = F)
        
        observeEvent(input$checklist_tab, { 
          #cat(file=stderr(), "test", input$numSabot, "\n")
          
          if (!is.na(input$ExtTemp)) {
            checklist_table <<- data.frame("T° Ext" = input$ExtTemp)}
          else {checklist_table <<- data.frame("T° Ext"= c("NULL"))}
          
          if (!is.na(input$rectTemp)) {
            checklist_table <<- cbind(checklist_table,data.frame("T° rectale" = input$rectTemp))}
          else {checklist_table <<- cbind(checklist_table,data.frame("T° rectale"= c("NULL")))}
          
          if (!is.null(input$lutte)) {
            checklist_table <<- cbind(checklist_table,data.frame("Lutte" = input$lutte))}
          else {checklist_table <<- cbind(checklist_table,data.frame("Lutte"= c("NULL")))}
          
          if (!is.null(input$halete)) {
            checklist_table <<- cbind(checklist_table,data.frame("Halete" = input$halete))}
          else {checklist_table <<- cbind(checklist_table,data.frame("Halete"= c("NULL")))}
          
          if (!is.null(input$cribague)) {
            checklist_table <<- cbind(checklist_table,data.frame("Cri bague" = input$cribague))}
          else {checklist_table <<- cbind(checklist_table,data.frame("Cri bague"= c("NULL")))}
          
          if (!is.null(input$criautre)) {
            checklist_table <<- cbind(checklist_table,data.frame("Cri autre" = input$criautre))}
          else {checklist_table <<- cbind(checklist_table,data.frame("Cri autre"= c("NULL")))}
     
          if ((input$Notation_euro_table)!="") {
            checklist_table <<- cbind(checklist_table,data.frame("Eurodeer" = input$Notation_euro_table))}
          else {checklist_table <<- cbind(checklist_table,data.frame("Eurodeer"= c("NULL")))}
          
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
    checklist2 <<- cbind(checklist2,data.frame("Titube" = input$titube))}
  else {checklist2 <<- cbind(checklist2,data.frame("Titube"= c("NULL")))}
 
   if (!is.null(input$couche)) {
    checklist2 <<- cbind(checklist2,data.frame("Couché" = input$couche))}
  else {checklist2 <<- cbind(checklist2,data.frame("Couché"= c("NULL")))}
  
  if (!is.null(input$cabriole_saut)) {
    checklist2 <<- cbind(checklist2,data.frame("cabriole saut" = input$cabriole_saut))}
  else {checklist2 <<- cbind(checklist2,data.frame("cabriole saut"= c("NULL")))}
  
  if (!is.null(input$cri)) {
 checklist2 <<- cbind(checklist2,data.frame("Cri" = input$cri))}
  else {checklist2 <<- cbind(checklist2,data.frame("Cri"= c("NULL")))}
  
  if (!is.null(input$allure)) {
    checklist2 <<- cbind(checklist2,data.frame("Allure" = input$allure))}
  else {checklist2 <<- cbind(checklist2,data.frame("Allure"= c("NULL")))}
  
  if (!is.null(input$gratte_collier)) {
    checklist2 <<- cbind(checklist2,data.frame("Gratte_Collier" = input$gratte_collier))}
  else {checklist2 <<- cbind(checklist2,data.frame("Gratte_Collier"= c("NULL")))}
  
  if (!is.null(input$tombe)) {
    checklist2 <<- cbind(checklist2,data.frame("Tombe" = input$tombe))}
  else {checklist2 <<- cbind(checklist2,data.frame("Tombe"= c("NULL")))}
  
  if ((input$habitat)!="") {
    checklist2 <<- cbind(checklist2,data.frame("Habitat" = input$habitat))}
  else {checklist2 <<- cbind(checklist2,data.frame("Habitat"= c("NULL")))}
  
  if ((input$Notation_euro)!="") {
    checklist2 <<- cbind(checklist2,data.frame("Eurodeer" = input$Notation_euro))}
  else {checklist2 <<- cbind(checklist2,data.frame("Eurodeer"= c("NULL")))}
  
  if ((input$habitat_perte)!="") {
    checklist2 <<- cbind(checklist2,data.frame("Habitat perte" = input$habitat_perte))}
  else {checklist2 <<- cbind(checklist2,data.frame("Habitat perte"= c("NULL")))}
  
  if (!is.na(input$nbre_stops)) {
    checklist2 <<- cbind(checklist2,data.frame("Stops" = input$nbre_stops))}
  else {checklist2 <<- cbind(checklist2,data.frame("Stops"= c("NULL")))}
  
  if ((input$visibilite)!="") {
    checklist2 <<- cbind(checklist2,data.frame("Visibilité" = input$visibilite))}
  else {checklist2 <<- cbind(checklist2,data.frame("Visibilité"= c("NULL")))}
  
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
  
  if ((input$numSabot_capture)=="")  
    {shinyalert("STOP!", "Numéro Sabot manquant", type = "warning",confirmButtonText="Valider quand même", showCancelButton=T,cancelButtonText="Annuler")} 
  else
  {cpt_capt=cpt_capt+1} 


if ((as.character(input$date_capture))!='2017-01-01') {
 # date_capture <<- as.character(input$date_capture)
  cpt_capt=cpt_capt+1
}
else {shinyalert("STOP!", "Date manquante", type = "warning",confirmButtonText="Valider quand même", showCancelButton=T,cancelButtonText="Annuler")} 


  
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

if (cpt_capt==8)
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

idSite2=''
idTagOrG=''
idTagOrD=''

}


################## LANCEMENT DE L'APPLICATION :

#dbDisconnect(con)

shinyApp(ui = ui, server = server)

