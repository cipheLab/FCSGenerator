library("shiny")
library("shinyjs")

ui <- fluidPage(theme = "generateFCS.css",
  useShinyjs(),
  div(id="AppTitle",
  titlePanel("FCS Generator")),
  
  sidebarLayout( 
    
    sidebarPanel(div(id="setBoxes"),
                 id="left_menu",
                 width = 3),
    
    mainPanel
    (
      navbarPage("",
        navbarMenu("Generate a file",
            
            #==================================================================
            #==================================================================
            #GENERATE POPULATIONS AUTOMATICALLY-------------------------------------
           
            tabPanel("Generate Populations Automatically",
              div(id="generatePanel",
                h3("Generate Populations Automatically"),
                div(id="generatePanelValues",
                    numericInput("pop_sizeInputAut",
                                 h5("Number of events"),
                                 value = 1000),
                    numericInput("dimInputAut",
                                 h5("Number of parameters"),
                                 value = 3),
                    numericInput("nmbClusterInput",
                                 h5("Number of populations"),
                                 value = 1),
                    
                    div(
                      h4("Rare Populations Parameters"),
                      numericInput("rareClusterInput",
                                   h5("Number of rare populations"),
                                   value = 0),
                      numericInput("min_rare",
                                          h6("Min frequence"),
                                          value = 0,
                                          min = 0,
                                          max = 100),
                      numericInput("max_rare",
                                          h6("Max frequence"),
                                          value = 10,
                                          min = 0,
                                          max = 10),
                      id="minPopPanel")),
                
                div(id="generatePanelFiles",
                    numericInput("nmb_filesInputAut",
                                 h5("Number of control files"),
                                 value = 1),
                    h4("Variable parameters: "),
                    div(id="parametersCheckboxList",
                        class = "parametersCheckbox_div")),
                
                div(id="generateDiv2",
                    actionButton("generateFCSAutomatically",
                                 h4("Generate the files"),
                                 width="90%",
                                 style="background-color:gray;color:white")),
                width = 4
              )),
            
            
            #==================================================================
            #==================================================================
            #GENERATE POPULATIONS MANUALLY-------------------------------------
            
            tabPanel("Generate Populations Manually",
              div(id="popModif",
                  h3("Generate Populations Manually"),
                  div(id="man_commandsDiv",
                      numericInput("pop_sizeInputMan",
                                   h5("Number of events"),
                                   value = 1000),
                      numericInput("dimInputMan",
                                   h5("Number of parameters"),
                                   value = 3)),
                  div(id="man_commandsDivFiles",
                      numericInput("nmb_filesInputMan",
                                   h5("Number of control files"),
                                   value = 1),
                      h4("Variable parameters: "),
                      div(id="parametersCheckboxListMan",
                          class = "parametersCheckbox_div")),
                  div(class="popFreqDiv",
                          column(2,id="popCol",
                                 div(id="pop_0",style="height:15vh",
                                     h4("Population Frequencies: ",style="padding: 4rem 0.5rem"))),
                          column(3,id="minCol",
                                 div(id="min_0",style="height:15vh",
                                     numericInput("min_00",
                                                  h3("Min frequence"),
                                                  value = 0,
                                                  min = 0,
                                                  max = 100))),
                          column(3,id="maxCol",
                                 div(id="max_0",style="height:15vh",
                                     numericInput("max_00",
                                                  h3("Max frequence"),
                                                  value = 100,
                                                  min = 0,
                                                  max = 100))),
                          column(1,id="delCol",
                                 div(id="delPop_0",style="height:15vh"))),
                  div(id="Commands",
                      div(id="addPopDiv",
                          actionButton("addPopDens", display="margin-top='50%'",
                                       h6("Add a population density"),
                                       width = "95%",
                                       style="background-color:gray;color:white")),
                      div(id="generateDiv",
                          actionButton("generateFCSManually",
                                       h6("Generate the files"),
                                       width="95%",
                                       style="background-color:gray;color:white")))
                  
              ))
        ),
        #==================================================================
        #==================================================================
        #MUTANTS MENU------------------------------------------------------
        tabPanel("Mutants",
                 h2("Create mutant files"),
                 div(id="set_used_id_div",
                     selectInput("set_used_id",
                                 "Set of control Files To Use",
                                 choices = list(" " = 0),
                                 selected = 0)),
                 actionButton("set_used_refresh",
                              h5("Refresh"),
                              class = "set_used_load_button"),
                 actionButton("set_used_load",
                              h5("Load"),
                              class = "set_used_load_button"),
                 div(id="setPopList",
                     h3("Populations: "),
                     class = "setPopList_div"),
                 div(id="pop_red_generate_div",
                     actionButton("pop_red_generate_button",
                                  h4("Generate Mutants"),
                                  width="80%",
                                  style="background-color:gray;color:yellow"))),
        #==================================================================
        #==================================================================
        #VIEW PANEL--------------------------------------------------------
        tabPanel("View Panel",
                 imageOutput("testPlot", height = "90vh"),
                 value="viewPan"),
        id="nav"
      ),
      id="mainPanel",
      width = 9
    ),
    fluid = TRUE
  )
)
