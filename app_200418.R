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

blelocalisationChoices = list(" 1" = 1, "2" = 2,
                              "3" = 3,"Autre localisation")


################## FORMULAIRE CARACTERISTIQUES DE L'ANIMAL

contentcaractanimal = fluidPage(
  #titlePanel("Caract. de l'animal"),
  
  fluidRow(
    column(2, numericInput(inputId = "nSabot", value = " ",label = h4("N° Sabot"), min=1, max=28)),
    column(2, numericInput(inputId = "pSabotPlein", value = " ",label = h4("Poids Sabot Plein"),min=0,max=70 )),
    column(2, numericInput(inputId = "pSabotVide", value = " ",label = h4("Poids Sabot Vide"),min=0,max=60 )),
    column(2, h4("Poids Animal"),textOutput("value")),
    column(12,hr()),
    column(2, checkboxInput(inputId = "estNouvelAnimal", value = T,label = h4("Nouvel Animal"))),
    column(2, numericInput(inputId = "nAnimal", value = " ",label = h4("N° Animal"),min=0 )),
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
    column(2, radioButtons("sexe",h4("Sexe"),choiceNames = list("M","F"),choiceValues = list(0,1)))
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
    titlePanel("Pose de collier"),
    column(3, checkboxInput(inputId = "Nouveau collier", value = F,label = h4("Pose") )),
    column(3, actionButton("ajoutBle","Confirmer la nouvelle pose"))
))


################## FORMULAIRE COMPORTEMENT TABLE

contenttable = fluidPage(
  #titlePanel("Comportement sur table"),
  
  fluidRow(
    
  column(3,numericInput("ExtTemp", value=" ", h4("Température extérieur"),step = 1)),
  column(12,hr()),
  column(2,radioButtons("lutte",h4("Lutte"),choiceNames = list("Oui","Non"),choiceValues = list(T,F), selected =c("None selected" = ""))),
  column(2,radioButtons("halete",h4("Halete"),choiceNames = list("Oui","Non"),choiceValues = list(T,F), selected = character(0))),
  column(2,radioButtons("cribague",h4("Cri Bague"), choiceNames = list("NA","0", "1-2", ">2"), choiceValues = list(0,1,2,3), selected = F)),
  column(2,radioButtons("criautre", h4("Cri Autre"), choiceNames = list("0", "1-2", ">2"), choiceValues = list(0,1,2), selected = F)),
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
            fluidRow(
               column(width= 2,selectInput(inputId = "ani_etiq", label = h4("N°Animal"),choices = dbGetQuery(con,"Select ani_etiq from public.t_animal_ani order by ani_etiq")), selected = NULL, offset= 0.5)
                     ) 
              ),
      tabPanel("Historique de capture", DT::dataTableOutput("historique"))
    ))



################## FORMULAIRE CHECKLIST 1 :

contentcheck1 = fluidPage()


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
    column(1,numericInput("nbre_personnes", value=" ", h4("Nbre de personnes"),min=1)),
    
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
      choices = dbGetQuery(con,"select (ecl_comportement_lache) from lu_tables.tr_eurodeer_comp_lache_ecl"),options=list(placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }')), selected = NULL)),
    
    column(12,hr()),
    
    column(12,useShinyalert(),
              actionButton("test", "Checklist2",icon('eye'),width='25%'))
    
  ))
  


################## FORMULAIRE CHECKLIST 2 :


contentcheck2 = fluidPage()


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
    column(2,selectInput("Nbre_pers_experimentes",h4("Nombre de capteurs expérimentés"),choices = list("1"=1,"2"=2,"3"=3,"4"=4,"5"=5),selected = 1)),
    
    column(12,hr()),
    
    column(1,radioButtons("cpt_filet_vitesse",h4("Vitesse"),choiceNames = list("Pas","Course"),choiceValues = list(0,1), selected = F)),
    column(1,radioButtons("cpt_filet_allure",h4("Allure"),choiceNames = list("Réfléchi","Bolide"),choiceValues = list(0,1), selected = F)),
    column(1,radioButtons("cpt_filet_lutte", h4("Lutte"), choiceNames = list("Oui","Non"), choiceValues = list(1,0), selected = character(0))),
    column(1,radioButtons("cpt_filet_halete",h4("Halete"), choiceNames = list("Oui","Non"), choiceValues = list(1,0), selected = character(0))),
    column(1,radioButtons("cpt_filet_cri",h4("Cri"),choiceNames = list("Oui","Non"),choiceValues = list(1,0), selected =c("None selected" = ""))),
    column(12,hr()),
    column(2,textInput("Remarques",label=h4("Remarques","")))
    
    
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
    column(3,textInput("Remarques",label=h4("Remarque","")))
    
    
  )
)


######## ORGANISATION DES RUBRIQUES

caractanimal = tabPanel("Caract. de l'animal",contentcaractanimal)
blessures = tabPanel("Blessures",contentblessures)
prelevement= tabPanel("Prélèvements",contentprelevement)
caractcollier = tabPanel("Caract. du collier",contentcollier)
comportable = tabPanel("Comportement table",contenttable)
historique = tabPanel("Historique captures",contenthistorique)
#checklist1 = tabPanel("checklist 1",contentcheck1)
comporlacher = tabPanel("Comportement lâcher",contentlacher)
#checklist2 = tabPanel("checklist 2",contentcheck2)
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
          #checklist1,
          tabPanel ( "Lâcher",comporlacher),
          #checklist2,
          tabPanel  ("Capture",comporcapture),
          tabPanel( "Sabot",comporsabot)
          #tabPanel("Summary", verbatimTextOutput("summary")),
          #tabPanel("Table", tableOutput("table"))
        )
)


################## SERVER :

server <- function(input, output,session) {
   
   output$value = renderText({input$pSabotPlein-input$pSabotVide})
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
   
   ## Partie historique :
   
     
     output$historique <- DT::renderDataTable({
       outp <- dbGetQuery(con,paste0("select t.ani_etiq as ani, t.ani_sexe as s, t.cap_date as date, t.cap_poids as poids, t.cap_lpa as lpa, t.cap_age_classe as age, t.sit_nom_court as site, 
                                     t.teq_nom_court as teq, t.eqa_date_debut as debut, t.eqa_date_fin as fin, t.cap_annee_suivi as an, round(t.temps_suivi/30.43) as mois,  count(t.cpos_id) as locs, t.eqt_id_usuel as equip, t.mar_libelle as marque, t.mod_libelle as modele, array_to_string( array_agg( distinct eqc_sen_id), ', ') as capteurs from (SELECT eqc_sen_id, cpos_id, ani_etiq, ani_sexe, cap_date, cap_poids, cap_lpa, cap_age_classe, sit_nom_court, 
                                     teq_nom_court, cap_annee_suivi, eqa_date_debut, eqa_date_fin, eqa_date_fin - eqa_date_debut as temps_suivi, eqt_id_usuel, mar_libelle, mod_libelle
                                     FROM public.v_aniposi_gpsgsm, public.t_equipement_conf_eqc ) as t where t.ani_etiq = '",input$ani_etiq,"' group by t.ani_etiq, t.ani_sexe, t.cap_date, t.cap_poids, t.cap_lpa, t.cap_age_classe, t.sit_nom_court, 
                                     t.teq_nom_court, t.cap_annee_suivi, t.eqa_date_debut, t.eqa_date_fin, t.temps_suivi, t.eqt_id_usuel, t.mar_libelle, t.mod_libelle order by cap_date"))
      
        
  # output$historique <- DT::renderDataTable({
    # outp <- dbGetQuery(con,paste0("select t.eqc_sen_id as capteur from (SELECT eqc_sen_id FROM public.t_equipement_conf_eqc) as t where t.ani_etiq = '",input$ani_etiq,"' group by t.eqc_sen_id "))
   
       ret <- DT::datatable(outp)
       return(ret)
     })
     
   
   ## Partie comportement lacher :
     
     observeEvent(input$to_current_time, {
       updateTimeInput(session, "time", value = Sys.time())
     })
     
     observeEvent(input$to_current_time2, {
       updateTimeInput(session, "time2", value = Sys.time())
     })
   
     observeEvent(input$test, { 
       #cat(file=stderr(), "visi", input$visibilite, "\n")
       
       if (is.null(input$vitesse) | is.null(input$titube) | is.null(input$couche) | is.null(input$cabriole_saut)
           | is.null(input$cri) | is.null(input$allure) | is.null(input$gratte_collier) | is.null(input$tombe)
           | (input$habitat)=="" | (input$Notation_euro)=="" | (input$habitat_perte)=="" | is.na(input$nbre_stops) | (input$visibilite)=="")
       {shinyalert("STOP!", "Données manquantes", type = "warning",confirmButtonText="Valider quand même", showCancelButton=T,cancelButtonText="Annuler")} 
       
       else if (!is.null(input$vitesse) | !is.null(input$titube) | !is.null(input$couche) | !is.null(input$cabriole_saut)
                | !is.null(input$cri) | !is.null(input$allure) | !is.null(input$gratte_collier) | !is.null(input$tombe)
                | (input$habitat)!="" | (input$Notation_euro)!="" | (input$habitat_perte)!="" | !is.na(input$nbre_stops) | (input$visibilite)!="")
       {shinyalert("Nice!", "Parfait", type = "success",showCancelButton=T)} 
     })
}



################## LANCEMENT DE L'APPLICATION :

#dbDisconnect(con)

shinyApp(ui = ui, server = server)

