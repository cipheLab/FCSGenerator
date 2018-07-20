library("shiny")
library("shinyjs")
library("ggcyto")
library("flowCore")

setsList <- list()
setsList.ID <- 0
clust.codeList <- list()
setSelected <- 0 

delPopButtonsUsed <- vector()
popId <- 0

saveAllButtonPrint <- FALSE

library("flowCore")

server <- function(input, output, session)
{
  useShinyjs()
  ##VARIABLES A REINITIALISER##
  setsList <- list()
  clust.codeList <- list()
  setsList.ID <- 0
  setSelected <- 0

  delPopButtonsUsed <- vector()
  popId <- 0

  saveAllButtonPrint <- FALSE
  ##=========================##
  shinyjs::disable("pop_red_generate_button")
  shinyjs::disable("set_used_load")


    observeEvent(input$generateFCSAutomatically,
    {
        shinyjs::disable("generateFCSAutomatically")
        progress <- Progress$new()
        progress$set(message="WRITING FCS")

        varPar <- c()
        for (i in 1:input$dimInputAut)
        {
            if (input[[paste0("dim_change_box_",i)]])
            {
                varPar[length(varPar)+1] <- i
            }
        }

        setsList.ID <<- setsList.ID + 1
        temp <- advanced.generateFCS(nmb.events = as.integer(input$pop_sizeInputAut),
                                     nmb.dim = as.integer(input$dimInputAut),
                                     nmb.clust = as.integer(input$nmbClusterInput),
                                     nmb.min.pop = as.integer(input$rareClusterInput),
                                     generate.min.pop = as.logical(input$rareClusterInput),
                                     min.pop.size.density = input$min_rare,
                                     max.pop.size.density = input$max_rare,
                                     variable.param = varPar,
                                     nmb.files = as.integer(input$nmb_filesInputAut))

        setsList <<- c(setsList, list(list(temp[[1]],list(NULL))))
        clust.codeList <<- c(clust.codeList, list(temp[[2]]))

        progress$close()

        progress <- Progress$new()
        progress$set(message="BUFFERING DATA")
        lapply(c(1:setsList.ID), function(i)
        {
          if(!is.null(unlist(setsList[[i]])))
          {
            if(is.null(input[[paste("box_ctrl_",i,"_",1,"_text", sep="")]]))
            {
              insertUI(selector = "#setBoxes",
                       where = "beforeEnd",
                       ui = div(id=paste0("set_",i),
                                downloadButton(paste("set_",i,"_dl", sep=""),
                                               h6("Save"),
                                               class = "setBoxButton"),
                                actionButton(paste("set_",i,"_del", sep=""),
                                             h6("Remove"),
                                             class = "setBoxButton"),
                                actionButton(paste("set_",i,"_hide", sep=""),
                                             h6("Hide/Show"),
                                             class = "setBoxButton"),
                                h4(paste0("Set ",i)),
                                div(id=paste0("set_container_",i),
                                    div(id=paste0("set_",i,"_ctrl_container"),
                                        downloadButton(paste("set_ctrl_",i,"_dl", sep=""),
                                                       h6("Save"),
                                                       class = "setCtrlBoxButton"),
                                        actionButton(paste("set_ctrl_",i,"_del", sep=""),
                                                     h6("Remove"),
                                                     class = "setCtrlBoxButton"),
                                        actionButton(paste("set_ctrl_",i,"_hide", sep=""),
                                                     h6("Hide/Show"),
                                                     class = "setCtrlBoxButton"),
                                        h5("Control Files"),
                                        div(id=paste0("set_",i,"_ctrl"),
                                            class="setBoxControl"),
                                        class="setBoxControlContainer")),
                                class="setBox"))

                observeEvent(input[[paste("set_",i,"_hide",sep="")]],
                 {
                     toggle(paste0("set_container_",i))
                 })

                observeEvent(input[[paste("set_",i,"_del",sep="")]],
                {
                     removeUI(selector = paste("#set_",i,sep=""))
                     setsList[[i]] <<- list(NULL,NULL)
                     if(saveAllButtonPrint)
                     {
                         removeButton <- TRUE
                         temp <- sum(sapply(1:length(setsList),function(k)
                         {
                             a <- length(unlist(setsList[[k]][[1]]))
                             b <- length(unlist(setsList[[k]][[2]]))
                             return( ((a + b) > 0)*1 )
                         }))
                         if(temp > 0)
                         {
                             removeButton <- FALSE
                         }
                         if(removeButton)
                         {
                             removeUI(selector = "#save_all_div")
                             saveAllButtonPrint <<- FALSE
                         }
                     }


                     setsAvailable <- list()
                     j <- 0
                     if(setsList.ID > 0)
                     {
                         lapply(1:setsList.ID, function(i)
                         {
                             if(!is.null(unlist(setsList[[i]])))
                             {
                                 if(length(unlist(setsList[[i]][[1]])) > 0)
                                 {
                                     setsAvailable <<- c(setsAvailable,list(i))
                                     names(setsAvailable)[[length(setsAvailable)]] <<- paste0("Set ", i)
                                     j <<- i
                                 }
                             }
                         })
                     }
                     removeUI(selector = "#pop_red_div")
                     shinyjs::disable("pop_red_generate_button")
                     if(j > 0)
                     {
                         shinyjs::enable("set_used_load")
                         updateSelectInput(session, "set_used_id", "Set of control Files To Use", choices = setsAvailable, selected = j)
                     }
                     else
                     {
                         shinyjs::disable("set_used_load")
                         updateSelectInput(session, "set_used_id", "Set of control Files To Use", choices = list(" " = 0), selected = j)
                     }
                })

                observeEvent(input[[paste("set_ctrl_",i,"_hide",sep="")]],
                {
                    toggle(paste0("set_",i,"_ctrl"))
                })

                observeEvent(input[[paste("set_ctrl_",i,"_del",sep="")]],
                {
                     removeUI(selector = paste("#set_",i,"_ctrl_container",sep=""))
                     setsList[[i]][[1]] <<- list(NULL)
                     if(saveAllButtonPrint)
                     {
                         removeButton <- TRUE
                         temp <- sum(sapply(1:length(setsList),function(k)
                         {
                             return(((length(unlist(setsList[[k]][[1]])) + length(unlist(setsList[[k]][[2]]))) > 0)*1)
                         }))
                         if(temp > 0)
                         {
                             removeButton <- FALSE
                         }
                         if(removeButton)
                         {
                             removeUI(selector = "#save_all_div")
                             saveAllButtonPrint <<- FALSE
                         }
                         if( (length(unlist(setsList[[i]][[1]])) + length(unlist(setsList[[i]][[2]])))  == 0)
                         {
                             removeUI(selector = paste("#set_",i,sep=""))
                         }
                     }

                     setsAvailable <- list()
                     j <- 0
                     if(setsList.ID > 0)
                     {
                         lapply(1:setsList.ID, function(i)
                         {
                             if(!is.null(unlist(setsList[[i]])))
                             {
                                 if(length(unlist(setsList[[i]][[1]])) > 0)
                                 {
                                     setsAvailable <<- c(setsAvailable,list(i))
                                     names(setsAvailable)[[length(setsAvailable)]] <<- paste0("Set ", i)
                                     j <<- i
                                 }
                             }
                         })
                     }

                     removeUI(selector = "#pop_red_div")
                     shinyjs::disable("pop_red_generate_button")
                     if(j > 0)
                     {
                         shinyjs::enable("set_used_load")
                         updateSelectInput(session, "set_used_id", "Set of control Files To Use", choices = setsAvailable, selected = j)
                     }
                     else
                     {
                         shinyjs::disable("set_used_load")
                         updateSelectInput(session, "set_used_id", "Set of control Files To Use", choices = list(" " = 0), selected = j)
                     }
                })
                output[[paste("set_ctrl_",i,"_dl",sep="")]] <- downloadHandler(#---------------------------------------------------------------
                    filename = function()
                    {
                        paste("output","zip",sep=".")
                    },
                    content= function(file)
                    {
                        fnames <- c()
                        tmpdir <- tempdir()
                        setwd(tmpdir)
                        dir.create("ctrl")
                        if(length(setsList[[i]][[1]]) > 0)
                        {
                            lapply(c(1:length(setsList[[i]][[1]])), function(l)
                            {
                                if(!is.null(unlist(setsList[[i]][[1]][[l]])))
                                {
                                    fnames <<- c(fnames,paste("ctrl/",input[[paste("box_ctrl_",i,"_",l,"_text",sep="")]],".fcs",sep=""))
                                    fcs.temp <- setsList[[i]][[1]][[l]]
                                    if(!is.null(input[["rangeInput"]]))
                                    {
                                        if(!input[["rangeInput"]])
                                        {
                                            fcs.temp <- advanced.transform.values(fcs.temp)
                                        }
                                    }
                                    write.FCS(fcs.temp,fnames[length(fnames)], delimiter = "#")
                                }
                            })
                        }
                        zip(zipfile=file,files=fnames)
                    }
                )
                output[[paste("set_",i,"_dl",sep="")]] <- downloadHandler(#---------------------------------------------------------------
                    filename = function()
                    {
                        paste("output","zip",sep=".")
                    },
                    content= function(file)
                    {
                        fnames <- c()
                        tmpdir <- tempdir()
                        setwd(tmpdir)
                        dir.create("ctrl")
                        if(length(setsList[[i]][[1]]) > 0)
                        {
                            lapply(c(1:length(setsList[[i]][[1]])), function(l)
                            {
                                if(!is.null(unlist(setsList[[i]][[1]][[l]])))
                                {
                                    fnames <<- c(fnames,paste("ctrl/",input[[paste("box_ctrl_",i,"_",l,"_text",sep="")]],".fcs",sep=""))
                                    fcs.temp <- setsList[[i]][[1]][[l]]
                                    if(!is.null(input[["rangeInput"]]))
                                    {
                                        if(!input[["rangeInput"]])
                                        {
                                            fcs.temp <- advanced.transform.values(fcs.temp)
                                        }
                                    }
                                    write.FCS(fcs.temp,fnames[length(fnames)], delimiter = "#")
                                }
                            })
                        }
                        dir.create("mutant")
                        if(length(setsList[[i]][[2]]) > 0)
                        {
                            lapply(c(1:length(setsList[[i]][[2]])), function(l)
                            {
                                if(!is.null(unlist(setsList[[i]][[2]][[l]])))
                                {
                                    fnames <<- c(fnames,paste("mutant/",input[[paste("box_mut_",i,"_",l,"_text",sep="")]],".fcs",sep=""))
                                    fcs.temp <- setsList[[i]][[2]][[l]]
                                    if(!is.null(input[["rangeInput"]]))
                                    {
                                        if(!input[["rangeInput"]])
                                        {
                                            fcs.temp <- advanced.transform.values(fcs.temp)
                                        }
                                    }
                                    write.FCS(fcs.temp,fnames[length(fnames)], delimiter = "#")
                                }
                            })
                        }
                        zip(zipfile=file,files=fnames)
                    }
                )


              lapply(1:length(setsList[[i]][[1]]),function(j)#---------------------------------------------------------------
              {
                  t <- setsList[[i]][[1]][[j]]
                  insertUI(selector = paste0("#set_",i,"_ctrl"),
                           where = "beforeEnd",
                           ui = div(id=paste("box_ctrl_",i,"_",j, sep=""),
                                    textInput(paste("box_ctrl_",i,"_",j,"_text", sep=""),
                                              "",
                                              width="100%",
                                              value=paste("CTRL_events-",nrow(t@exprs),
                                                          "__pop-",max(t@exprs[,ncol(t@exprs)]),
                                                          "__dim-",input$dimInputAut,
                                                          "__file-",i,"_",j,
                                                          "__",gsub(" ","__",gsub(":","_",Sys.time())),
                                                          sep="")),
                                    downloadButton(paste("box_ctrl_",i,"_",j,"_dl", sep=""),
                                                   h6("Save"),
                                                   class = "fileBoxButton"),
                                    actionButton(paste("box_ctrl_",i,"_",j,"_del", sep=""),
                                                 h6("Remove"),
                                                 class = "fileBoxButton"),
                                    actionButton(paste("box_ctrl_",i,"_",j,"_print", sep=""),
                                                 h6("Show parameters"),
                                                 class = "fileBoxButton"),
                                    class="controlFileBox"
                                    )
                          )
                  observeEvent(input[[paste("box_ctrl_",i,"_",j,"_del",sep="")]],
                   {
                       removeUI(selector = paste("#box_ctrl_",i,"_",j,sep=""))
                       setsList[[i]][[1]][[j]] <<- list(NULL)
                       if(saveAllButtonPrint)
                       {
                           removeButton <- TRUE
                           temp <- sum(sapply(1:length(setsList),function(k)
                           {
                               return(((length(unlist(setsList[[k]][[1]])) + length(unlist(setsList[[k]][[2]]))) > 0)*1)
                           }))
                           if(temp > 0)
                           {
                               removeButton <- FALSE
                           }
                           if(removeButton)
                           {
                               removeUI(selector = "#save_all_div")
                               saveAllButtonPrint <<- FALSE
                           }
                       }
                       if(length(unlist(setsList[[i]][[1]])) == 0)
                       {
                           removeUI(selector = paste("#set_",i,"_ctrl_container",sep=""))
                       }
                       if((length(unlist(setsList[[i]][[1]])) + length(unlist(setsList[[i]][[2]]))) == 0)
                       {
                           removeUI(selector = paste("#set_",i,sep=""))
                       }


                       setsAvailable <- list()
                       j <- 0
                       if(setsList.ID > 0)
                       {
                           lapply(1:setsList.ID, function(i)
                           {
                               if(!is.null(unlist(setsList[[i]])))
                               {
                                   if(length(unlist(setsList[[i]][[1]])) > 0)
                                   {
                                       setsAvailable <<- c(setsAvailable,list(i))
                                       names(setsAvailable)[[length(setsAvailable)]] <<- paste0("Set ", i)
                                       j <<- i
                                   }
                               }
                           })
                       }

                       removeUI(selector = "#pop_red_div")
                       shinyjs::disable("pop_red_generate_button")
                       if(j > 0)
                       {
                           shinyjs::enable("set_used_load")
                           updateSelectInput(session, "set_used_id", "Set of control Files To Use", choices = setsAvailable, selected = j)
                       }
                       else
                       {
                           shinyjs::disable("set_used_load")
                           updateSelectInput(session, "set_used_id", "Set of control Files To Use", choices = list(" " = 0), selected = j)
                       }
                   })
                  observeEvent(input[[paste("box_ctrl_",i,"_",j,"_print",sep="")]],
                   {
                       updateNavbarPage(session, "nav", selected = "viewPan")
                       t <- setsList[[i]][[1]][[j]]
                       output$testPlot <- renderPlot(
                           {
                               l <- ncol(t@exprs) -1
                               n <- as.integer(sqrt(l))
                               if(l>(n^2))
                               {
                                   par(mfrow=c(n+1,n+1))
                               }
                               else
                               {
                                   par(mfrow=c(n,n))
                               }
                               lapply(1:l, function(pl)
                               {
                                   hist(t@exprs[,pl], breaks = 500, main=colnames(t)[pl])
                               })
                           })
                   })
                  output[[paste("box_ctrl_",i,"_",j,"_dl",sep="")]] <- downloadHandler(#---------------------------------------------------------------
                      filename = function()
                      {
                          paste(input[[paste("box_ctrl_",i,"_",j,"_text",sep="")]],".fcs",sep="")
                      },
                      content= function(file)
                      {
                          fcs.temp <- setsList[[i]][[1]][[j]]
                          if(!is.null(input[["rangeInput"]]))
                          {
                              if(!input[["rangeInput"]])
                              {
                                  fcs.temp <- advanced.transform.values(fcs.temp)
                              }
                          }
                          write.FCS(fcs.temp,file, delimiter="#")
                      }
                  )
              })
            }
          }
        })
        progress$close()

        if(!saveAllButtonPrint)
        {
            insertUI(selector = "#setBoxes",
                     where = "beforeBegin",
                     ui = div(id="save_all_div",
                              checkboxInput("rangeInput",
                                            "Transform Values",
                                            value = TRUE),
                              downloadButton("save_all",
                                             h6("Save All"))))

            output[["save_all"]] <- downloadHandler(
                filename = function()
                {
                    paste("output","zip",sep=".")
                },
                content= function(file)
                {
                    fnames <- c()
                    tmpdir <- tempdir()
                    setwd(tmpdir)
                    if(setsList.ID > 0)
                    {
                        for(k in c(1:setsList.ID))
                        {
                            if(length(setsList[[k]]) == 2)
                            {
                                if(length(setsList[[k]][[1]]) > 0)
                                {
                                    dir.create("ctrl")
                                    for(l in c(1:length(setsList[[k]][[1]])))
                                    {
                                        print(l)
                                        if(!is.null(unlist(setsList[[k]][[1]][[l]])))
                                        {
                                            fnames <- c(fnames,paste("ctrl/",input[[paste("box_ctrl_",k,"_",l,"_text",sep="")]],".fcs",sep=""))
                                            fcs.temp <- setsList[[k]][[1]][[l]]
                                            if(!is.null(input[["rangeInput"]]))
                                            {
                                                if(!input[["rangeInput"]])
                                                {
                                                    fcs.temp <- advanced.transform.values(fcs.temp)
                                                }
                                            }
                                            write.FCS(fcs.temp,fnames[length(fnames)], delimiter = "#")
                                        }
                                    }
                                }

                                if(length(setsList[[k]][[2]]) > 0)
                                {
                                    dir.create("mutant")
                                    for(l in c(1:length(setsList[[k]][[2]])))
                                    {
                                        if(!is.null(unlist(setsList[[k]][[2]][[l]])))
                                        {
                                            fnames <- c(fnames,paste("mutant/",input[[paste("box_mut_",k,"_",l,"_text",sep="")]],".fcs",sep=""))
                                            fcs.temp <- setsList[[k]][[2]][[l]]
                                            if(!is.null(input[["rangeInput"]]))
                                            {
                                                if(!input[["rangeInput"]])
                                                {
                                                    fcs.temp <- advanced.transform.values(fcs.temp)
                                                }
                                            }
                                            write.FCS(fcs.temp,fnames[length(fnames)], delimiter = "#")
                                        }
                                    }
                                }
                            }
                        }
                    }
                    print(fnames)
                    zip(zipfile=file,files=fnames)
                },
                contentType = "application/zip"
            )

            saveAllButtonPrint <<- TRUE
        }


        setsAvailable <- list()
        j <- 0
        if(setsList.ID > 0)
        {
            lapply(1:setsList.ID, function(i)
            {
                if(!is.null(unlist(setsList[[i]])))
                {
                    if(length(unlist(setsList[[i]][[1]])) > 0)
                    {
                        setsAvailable <<- c(setsAvailable,list(i))
                        names(setsAvailable)[[length(setsAvailable)]] <<- paste0("Set ", i)
                        j <<- i
                    }
                }
            })
        }

        removeUI(selector = "#pop_red_div")
        shinyjs::disable("pop_red_generate_button")
        if(j > 0)
        {
            shinyjs::enable("set_used_load")
            updateSelectInput(session, "set_used_id", "Set of control Files To Use", choices = setsAvailable, selected = j)
        }
        else
        {
            shinyjs::disable("set_used_load")
            updateSelectInput(session, "set_used_id", "Set of control Files To Use", choices = list(" " = 0), selected = j)
        }
        
        delay(800,
            shinyjs::enable("generateFCSAutomatically")
        )
        

    })

    observeEvent(input$dimInputAut,
    {
         val = 1;
         if(!is.na(input$dimInputAut))
         {
             val = input$dimInputAut;
         }
         lapply(1:val, function(i)
         {
             observeEvent(input$dimInputAut,
             {
                  removeUI(selector = paste0("#dim_change_box_",i),
                           immediate = TRUE)
                  removeUI(selector = paste0("#dim_change_",i),
                           immediate = TRUE)
             })

             insertUI(selector = "#parametersCheckboxList",
                      where = "beforeEnd",
                      ui = div(id=paste0("dim_change_",i),
                            checkboxInput(paste0("dim_change_box_",i),
                                            h6(paste0("Parameter ", i)),
                                            value = 0)))
         })
     })




    observeEvent(input$addPopDens,
    {
           shinyjs::disable("addPopDens")
           updateNumericInput(session, "nmbClusterInput", value=max(length(delPopButtonsUsed[delPopButtonsUsed==TRUE]) + 1, input$nmbClusterInput))
           popId <<- popId + 1

           insertUI(selector = "#popCol",
                    where = "beforeEnd",
                    ui = div(id=paste("pop_",popId, sep=""),
                             style="height:15vh;margin-top:50px",
                             h4("Population: Frequencies",style="padding: 4rem 0.5rem")))

           insertUI(selector = "#minCol",
                    where = "beforeEnd",
                    ui =  div(id=paste("min_",popId, sep=""),
                              style="height:15vh;margin-top:50px",
                              numericInput(paste("min_",popId,popId, sep=""),
                                           h3("Min Frequency"),
                                           value = 0,
                                           min = 0,
                                           max = 100)))

           insertUI(selector = "#maxCol",
                    where = "beforeEnd",
                    ui = div(id=paste("max_",popId, sep=""),
                             style="height:15vh;margin-top:50px",
                             numericInput(paste("max_",popId,popId, sep=""),
                                          h3("Max Frequency"),
                                          value = 100,
                                          min = 0,
                                          max = 100)))

           insertUI(selector = "#delCol",
                    where = "beforeEnd",
                    ui = div(id=paste("delPop_",popId, sep=""),
                             style="height:15vh;margin-top:50px",
                             actionButton(paste("delPop_",popId,popId, sep=""),
                                          h6("Remove"),
                                          style="margin: 6.8rem 0.5rem")))




           delPopButtonsUsed <<- c(delPopButtonsUsed,TRUE)
           if (length(delPopButtonsUsed)>0)
           {
             lapply(c(1:length(delPopButtonsUsed)), function(i)
             {
               if(delPopButtonsUsed[i])
               {
                 observeEvent(input[[paste("delPop_",i,i,sep="")]],
                  {
                    removeUI(selector = paste("#min_",i, sep=""))
                    removeUI(selector = paste("#max_",i, sep=""))
                    removeUI(selector = paste("#delPop_",i, sep=""))
                    removeUI(selector = paste("#pop_",i, sep=""))
                    delPopButtonsUsed[i] <<- FALSE
                  })
               }
             })
           }

           j <- 1

           shinyjs::enable("addPopDens")
    })

    observeEvent(input$generateFCSManually,
    {
        shinyjs::disable("generateFCSManually")
        progress <- Progress$new()
        progress$set(message="WRITING FCS")

        varPar <- c()
        for (i in 1:input$dimInputMan)
        {
            if (input[[paste0("dim_change_box_man",i)]])
            {
                varPar[length(varPar)+1] <- i
            }
        }

        setsList.ID <<- setsList.ID + 1

        if (length(delPopButtonsUsed) > 0)
        {
           freq <- Filter(Negate(is.null),
                          c(list(c(input$min_00, input$max_00)),
                            lapply(c(1:length(delPopButtonsUsed)), function(i)
                            {
                                if(!is.null(delPopButtonsUsed[i]))
                                {
                                    if(delPopButtonsUsed[i])
                                    {
                                        return(c(input[[paste("min_",i,i,sep="")]], input[[paste("max_",i,i,sep="")]]))
                                    }
                                }
                            })))
        }
        else
        {
           freq <- list(c(input$min_00, input$max_00))
        }


        temp <- advanced.generateFCS(nmb.events = as.integer(input$pop_sizeInputMan),
                                     nmb.dim = as.integer(input$dimInputMan),
                                     freq.pop = freq,
                                     nmb.files = as.integer(input$nmb_filesInputMan),
                                     variable.param = varPar)

        setsList <<- c(setsList, list(list(temp[[1]],list(NULL))))
        clust.codeList <<- c(clust.codeList, list(temp[[2]]))

        progress$close()

        progress <- Progress$new()
        progress$set(message="BUFFERING DATA")
        lapply(c(1:setsList.ID), function(i)
        {
            if(!is.null(unlist(setsList[[i]])))
            {
                if(is.null(input[[paste("box_ctrl_",i,"_",1,"_text", sep="")]]))
                {
                    insertUI(selector = "#setBoxes",
                             where = "beforeEnd",
                             ui = div(id=paste0("set_",i),
                                      downloadButton(paste("set_",i,"_dl", sep=""),
                                                     h6("Save"),
                                                     class = "setBoxButton"),
                                      actionButton(paste("set_",i,"_del", sep=""),
                                                   h6("Remove"),
                                                   class = "setBoxButton"),
                                      actionButton(paste("set_",i,"_hide", sep=""),
                                                   h6("Hide/Show"),
                                                   class = "setBoxButton"),
                                      h4(paste0("Set ",i)),
                                      div(id=paste0("set_container_",i),
                                          div(id=paste0("set_",i,"_ctrl_container"),
                                              downloadButton(paste("set_ctrl_",i,"_dl", sep=""),
                                                             h6("Save"),
                                                             class = "setCtrlBoxButton"),
                                              actionButton(paste("set_ctrl_",i,"_del", sep=""),
                                                           h6("Remove"),
                                                           class = "setCtrlBoxButton"),
                                              actionButton(paste("set_ctrl_",i,"_hide", sep=""),
                                                           h6("Hide/Show"),
                                                           class = "setCtrlBoxButton"),
                                              h5("Control Files"),
                                              div(id=paste0("set_",i,"_ctrl"),
                                                  class="setBoxControl"),
                                              class="setBoxControlContainer")),
                                      class="setBox"))

                    observeEvent(input[[paste("set_",i,"_hide",sep="")]],
                     {
                         toggle(paste0("set_container_",i))
                     })

                    observeEvent(input[[paste("set_",i,"_del",sep="")]],
                     {
                         removeUI(selector = paste("#set_",i,sep=""))
                         setsList[[i]] <<- list(NULL,NULL)
                         if(saveAllButtonPrint)
                         {
                             removeButton <- TRUE
                             temp <- sum(sapply(1:length(setsList),function(k)
                             {
                                 return(((length(unlist(setsList[[k]][[1]])) + length(unlist(setsList[[k]][[2]]))) > 0)*1)
                             }))
                             if(temp > 0)
                             {
                                 removeButton <- FALSE
                             }
                             if(removeButton)
                             {
                                 removeUI(selector = "#save_all_div")
                                 saveAllButtonPrint <<- FALSE
                             }
                         }
                         if( (length(unlist(setsList[[i]][[1]])) + length(unlist(setsList[[i]][[2]])))  == 0)
                         {
                             removeUI(selector = paste("#set_",i,sep=""))
                         }

                         setsAvailable <- list()
                         j <- 0
                         if(setsList.ID > 0)
                         {
                             lapply(1:setsList.ID, function(i)
                             {
                                 if(!is.null(unlist(setsList[[i]])))
                                 {
                                     if(length(unlist(setsList[[i]][[1]])) > 0)
                                     {
                                         setsAvailable <<- c(setsAvailable,list(i))
                                         names(setsAvailable)[[length(setsAvailable)]] <<- paste0("Set ", i)
                                         j <<- i
                                     }
                                 }
                             })
                         }

                         removeUI(selector = "#pop_red_div")
                         shinyjs::disable("pop_red_generate_button")
                         if(j > 0)
                         {
                             shinyjs::enable("set_used_load")
                             updateSelectInput(session, "set_used_id", "Set of control Files To Use", choices = setsAvailable, selected = j)
                         }
                         else
                         {
                             shinyjs::disable("set_used_load")
                             updateSelectInput(session, "set_used_id", "Set of control Files To Use", choices = list(" " = 0), selected = j)
                         }
                     })

                    observeEvent(input[[paste("set_ctrl_",i,"_hide",sep="")]],
                     {
                         toggle(paste0("set_",i,"_ctrl"))
                     })

                    observeEvent(input[[paste("set_ctrl_",i,"_del",sep="")]],
                     {
                         removeUI(selector = paste("#set_",i,"_ctrl_container",sep=""))
                         setsList[[i]][[1]] <<- list(NULL)
                         if(saveAllButtonPrint)
                         {
                             removeButton <- TRUE
                             temp <- sum(sapply(1:length(setsList),function(k)
                             {
                                 return(((length(unlist(setsList[[k]][[1]])) + length(unlist(setsList[[k]][[2]]))) > 0)*1)
                             }))
                             if(temp > 0)
                             {
                                 removeButton <- FALSE
                             }
                             if(removeButton)
                             {
                                 removeUI(selector = "#save_all_div")
                                 saveAllButtonPrint <<- FALSE
                             }
                         }
                         if( (length(unlist(setsList[[i]][[1]])) + length(unlist(setsList[[i]][[2]])))  == 0)
                         {
                             removeUI(selector = paste("#set_",i,sep=""))
                         }

                         setsAvailable <- list()
                         j <- 0
                         if(setsList.ID > 0)
                         {
                             lapply(1:setsList.ID, function(i)
                             {
                                 if(!is.null(unlist(setsList[[i]])))
                                 {
                                     if(length(unlist(setsList[[i]][[1]])) > 0)
                                     {
                                         setsAvailable <<- c(setsAvailable,list(i))
                                         names(setsAvailable)[[length(setsAvailable)]] <<- paste0("Set ", i)
                                         j <<- i
                                     }
                                 }
                             })
                         }

                         removeUI(selector = "#pop_red_div")
                         shinyjs::disable("pop_red_generate_button")
                         if(j > 0)
                         {
                             shinyjs::enable("set_used_load")
                             updateSelectInput(session, "set_used_id", "Set of control Files To Use", choices = setsAvailable, selected = j)
                         }
                         else
                         {
                             shinyjs::disable("set_used_load")
                             updateSelectInput(session, "set_used_id", "Set of control Files To Use", choices = list(" " = 0), selected = j)
                         }
                     })
                    output[[paste("set_ctrl_",i,"_dl",sep="")]] <- downloadHandler(#---------------------------------------------------------------
                           filename = function()
                           {
                               paste("output","zip",sep=".")
                           },
                           content= function(file)
                           {
                               fnames <- c()
                               tmpdir <- tempdir()
                               setwd(tmpdir)
                               dir.create("ctrl")
                               if(length(setsList[[i]][[1]]) > 0)
                               {
                                   lapply(c(1:length(setsList[[i]][[1]])), function(l)
                                   {
                                       if(!is.null(unlist(setsList[[i]][[1]][[l]])))
                                       {
                                           fnames <<- c(fnames,paste("ctrl/",input[[paste("box_ctrl_",i,"_",l,"_text",sep="")]],".fcs",sep=""))
                                           fcs.temp <- setsList[[i]][[1]][[l]]
                                           if(!is.null(input[["rangeInput"]]))
                                           {
                                               if(!input[["rangeInput"]])
                                               {
                                                   fcs.temp <- advanced.transform.values(fcs.temp)
                                               }
                                           }
                                           write.FCS(fcs.temp,fnames[length(fnames)], delimiter = "#")
                                       }
                                   })
                               }
                               zip(zipfile=file,files=fnames)
                           }
                    )
                    output[[paste("set_",i,"_dl",sep="")]] <- downloadHandler(#---------------------------------------------------------------
                          filename = function()
                          {
                              paste("output","zip",sep=".")
                          },
                          content= function(file)
                          {
                              fnames <- c()
                              tmpdir <- tempdir()
                              setwd(tmpdir)
                              dir.create("ctrl")
                              if(length(setsList[[i]][[1]]) > 0)
                              {
                                  lapply(c(1:length(setsList[[i]][[1]])), function(l)
                                  {
                                      if(!is.null(unlist(setsList[[i]][[1]][[l]])))
                                      {
                                          fnames <<- c(fnames,paste("ctrl/",input[[paste("box_ctrl_",i,"_",l,"_text",sep="")]],".fcs",sep=""))
                                          fcs.temp <- setsList[[i]][[1]][[l]]
                                          if(!is.null(input[["rangeInput"]]))
                                          {
                                              if(!input[["rangeInput"]])
                                              {
                                                  fcs.temp <- advanced.transform.values(fcs.temp)
                                              }
                                          }
                                          write.FCS(fcs.temp,fnames[length(fnames)], delimiter = "#")
                                      }
                                  })
                              }
                              dir.create("mutant")
                              if(length(setsList[[i]][[2]]) > 0)
                              {
                                  lapply(c(1:length(setsList[[i]][[2]])), function(l)
                                  {
                                      if(!is.null(unlist(setsList[[i]][[2]][[l]])))
                                      {
                                          fnames <<- c(fnames,paste("mutant/",input[[paste("box_mut_",i,"_",l,"_text",sep="")]],".fcs",sep=""))
                                          fcs.temp <- setsList[[i]][[1]][[l]]
                                          if(!is.null(input[["rangeInput"]]))
                                          {
                                              if(!input[["rangeInput"]])
                                              {
                                                  fcs.temp <- advanced.transform.values(fcs.temp)
                                              }
                                          }
                                          write.FCS(fcs.temp,fnames[length(fnames)], delimiter = "#")
                                      }
                                  })
                              }
                              zip(zipfile=file,files=fnames)
                          }
                    )


                    lapply(1:length(setsList[[i]][[1]]),function(j)#---------------------------------------------------------------
                    {
                           t <- setsList[[i]][[1]][[j]]
                           insertUI(selector = paste0("#set_",i,"_ctrl"),
                                    where = "beforeEnd",
                                    ui = div(id=paste("box_ctrl_",i,"_",j, sep=""),
                                             textInput(paste("box_ctrl_",i,"_",j,"_text", sep=""),
                                                       "",
                                                       width="100%",
                                                       value=paste("CTRL_events-",nrow(t@exprs),
                                                                   "__pop-",max(t@exprs[,ncol(t@exprs)]),
                                                                   "__dim-",input$dimInputAut,
                                                                   "__file-",i,"_",j,
                                                                   "__",gsub(" ","__",gsub(":","_",Sys.time())),
                                                                   sep="")),
                                             downloadButton(paste("box_ctrl_",i,"_",j,"_dl", sep=""),
                                                            h6("Save"),
                                                            class = "fileBoxButton"),
                                             actionButton(paste("box_ctrl_",i,"_",j,"_del", sep=""),
                                                          h6("Remove"),
                                                          class = "fileBoxButton"),
                                             actionButton(paste("box_ctrl_",i,"_",j,"_print", sep=""),
                                                          h6("Show parameters"),
                                                          class = "fileBoxButton"),
                                             class="controlFileBox"
                                    )
                           )
                           observeEvent(input[[paste("box_ctrl_",i,"_",j,"_del",sep="")]],
                            {
                                removeUI(selector = paste("#box_ctrl_",i,"_",j,sep=""))
                                setsList[[i]][[1]][[j]] <<- list(NULL)
                                if(saveAllButtonPrint)
                                {
                                    removeButton <- TRUE
                                    temp <- sum(sapply(1:length(setsList),function(k)
                                    {
                                        return(((length(unlist(setsList[[k]][[1]])) + length(unlist(setsList[[k]][[2]]))) > 0)*1)
                                    }))
                                    if(temp > 0)
                                    {
                                        removeButton <- FALSE
                                    }
                                    if(removeButton)
                                    {
                                        removeUI(selector = "#save_all_div")
                                        saveAllButtonPrint <<- FALSE
                                    }
                                }
                                if(length(unlist(setsList[[i]][[1]])) == 0)
                                {
                                       removeUI(selector = paste("#set_",i,"_ctrl_container",sep=""))
                                }
                                if((length(unlist(setsList[[i]][[1]])) + length(unlist(setsList[[i]][[2]]))) == 0)
                                {
                                    removeUI(selector = paste("#set_",i,sep=""))
                                }



                                setsAvailable <- list()
                                j <- 0
                                if(setsList.ID > 0)
                                {
                                    lapply(1:setsList.ID, function(i)
                                    {
                                        if(!is.null(unlist(setsList[[i]])))
                                        {
                                            if(length(unlist(setsList[[i]][[1]])) > 0)
                                            {
                                                setsAvailable <<- c(setsAvailable,list(i))
                                                names(setsAvailable)[[length(setsAvailable)]] <<- paste0("Set ", i)
                                                j <<- i
                                            }
                                        }
                                    })
                                }

                                removeUI(selector = "#pop_red_div")
                                shinyjs::disable("pop_red_generate_button")
                                if(j > 0)
                                {
                                    shinyjs::enable("set_used_load")
                                    updateSelectInput(session, "set_used_id", "Set of control Files To Use", choices = setsAvailable, selected = j)
                                }
                                else
                                {
                                    shinyjs::disable("set_used_load")
                                    updateSelectInput(session, "set_used_id", "Set of control Files To Use", choices = list(" " = 0), selected = j)
                                }
                            })
                           observeEvent(input[[paste("box_ctrl_",i,"_",j,"_print",sep="")]],
                            {
                                updateNavbarPage(session, "nav", selected = "viewPan")
                                t <- setsList[[i]][[1]][[j]]
                                output$testPlot <- renderPlot(
                                    {
                                        l <- ncol(t@exprs) -1
                                        n <- as.integer(sqrt(l))
                                        if(l>(n^2))
                                        {
                                            par(mfrow=c(n+1,n+1))
                                        }
                                        else
                                        {
                                            par(mfrow=c(n,n))
                                        }
                                        lapply(1:l, function(pl)
                                        {
                                            hist(t@exprs[,pl], breaks = 500, main=colnames(t)[pl])
                                        })
                                    })
                            })
                           output[[paste("box_ctrl_",i,"_",j,"_dl",sep="")]] <- downloadHandler(#---------------------------------------------------------------
                                    filename = function()
                                    {
                                        paste(input[[paste("box_ctrl_",i,"_",j,"_text",sep="")]],".fcs",sep="")
                                    },
                                    content= function(file)
                                    {
                                        fcs.temp <- setsList[[i]][[1]][[j]]
                                        if(!is.null(input[["rangeInput"]]))
                                        {
                                            if(!input[["rangeInput"]])
                                            {
                                                fcs.temp <- advanced.transform.values(fcs.temp)
                                            }
                                        }
                                        write.FCS(fcs.temp,file, delimiter="#")
                                    }
                           )
                    })
                }
            }
        })
        progress$close()

        if(!saveAllButtonPrint)
        {
            insertUI(selector = "#setBoxes",
                     where = "beforeBegin",
                     ui = div(id="save_all_div",
                              checkboxInput("rangeInput",
                                            "Transform Values",
                                            value = TRUE),
                              downloadButton("save_all",
                                             h6("Save All"))))

            output[["save_all"]] <- downloadHandler(
                filename = function()
                {
                    paste("output","zip",sep=".")
                },
                content= function(file)
                {
                    fnames <- c()
                    tmpdir <- tempdir()
                    setwd(tmpdir)
                    if(setsList.ID > 0)
                    {
                        lapply(c(1:setsList.ID), function(k)
                        {
                            if(length(setsList[[k]]) == 2)
                            {
                                if(length(setsList[[k]][[1]]) > 0)
                                {
                                    dir.create("ctrl")
                                    lapply(c(1:length(setsList[[k]][[1]])), function(l)
                                    {
                                        if(!is.null(unlist(setsList[[k]][[1]][[l]])))
                                        {
                                            fnames <<- c(fnames,paste("ctrl/",input[[paste("box_ctrl",k,"_",l,"_text",sep="")]],".fcs",sep=""))
                                            fcs.temp <- setsList[[k]][[1]][[l]]
                                            if(!is.null(input[["rangeInput"]]))
                                            {
                                                if(!input[["rangeInput"]])
                                                {
                                                    fcs.temp <- advanced.transform.values(fcs.temp)
                                                }
                                            }
                                            write.FCS(fcs.temp,fnames[length(fnames)], delimiter = "#")
                                        }
                                    })
                                }

                                if(length(setsList[[k]][[2]]) > 0)
                                {
                                    dir.create("mutant")
                                    lapply(c(1:length(setsList[[k]][[2]])), function(l)
                                    {
                                        if(!is.null(unlist(setsList[[k]][[2]][[l]])))
                                        {
                                            fnames <<- c(fnames,paste("mutant/",input[[paste("box_mut_",k,"_",l,"_text",sep="")]],".fcs",sep=""))
                                            fcs.temp <- setsList[[k]][[2]][[l]]
                                            if(!is.null(input[["rangeInput"]]))
                                            {
                                                if(!input[["rangeInput"]])
                                                {
                                                    fcs.temp <- advanced.transform.values(fcs.temp)
                                                }
                                            }
                                            write.FCS(fcs.temp,fnames[length(fnames)], delimiter = "#")
                                        }
                                    })
                                }
                            }
                        })
                    }
                    zip(zipfile=file,files=fnames)
                },
                contentType = "application/zip"
            )

            saveAllButtonPrint <<- TRUE
        }



        setsAvailable <- list()
        j <- 0
        if(setsList.ID > 0)
        {
            lapply(1:setsList.ID, function(i)
            {
                if(!is.null(unlist(setsList[[i]])))
                {
                    if(length(unlist(setsList[[i]][[1]])) > 0)
                    {
                        setsAvailable <<- c(setsAvailable,list(i))
                        names(setsAvailable)[[length(setsAvailable)]] <<- paste0("Set ", i)
                        j <<- i
                    }
                }
            })
        }

        removeUI(selector = "#pop_red_div")
        shinyjs::disable("pop_red_generate_button")
        if(j > 0)
        {
            shinyjs::enable("set_used_load")
            updateSelectInput(session, "set_used_id", "Set of control Files To Use", choices = setsAvailable, selected = j)
        }
        else
        {
            shinyjs::disable("set_used_load")
            updateSelectInput(session, "set_used_id", "Set of control Files To Use", choices = list(" " = 0), selected = j)
        }
        delay(800,
            shinyjs::enable("generateFCSManually")
        )

   })

    observeEvent(input$dimInputMan,
    {
         val = 1;
         if(!is.na(input$dimInputMan))
         {
             val = input$dimInputMan;
         }
         lapply(1:val, function(i)
         {
             observeEvent(input$dimInputMan,
              {
                  removeUI(selector = paste0("#dim_change_box_man",i),
                           immediate = TRUE)
                  removeUI(selector = paste0("#dim_change_man",i),
                           immediate = TRUE)
              })

             insertUI(selector = "#parametersCheckboxListMan",
                      where = "beforeEnd",
                      ui = div(id=paste0("dim_change_man",i),
                               checkboxInput(paste0("dim_change_box_man",i),
                                             h6(paste0("Parameter ", i)),
                                             value = 0)))
         })
    })



    observeEvent(input$set_used_load,
    {
        removeUI(selector = "#pop_red_div")
        setSelected <<- 1
        if(!is.na(input$set_used_id))
        {
            setSelected <<- as.integer(input$set_used_id)
        }
        if(length(unlist(setsList[[setSelected]][[1]])) > 0)
        {
            t <- NULL
            while(is.null(t))
            {
                lapply(1:length(setsList[[setSelected]][[1]]), function(j)
                {
                    if (!is.null(unlist(setsList[[setSelected]][[1]][[j]])))
                    {
                        t <<- setsList[[setSelected]][[1]][[j]]
                    }
                })
            }
            nmb.pop <- as.integer(max(t@exprs[,ncol(t@exprs)]))
            insertUI(selector = "#setPopList",
                     where = "beforeEnd",
                     ui = div(id="pop_red_div"))
            lapply(1:nmb.pop, function(p)
            {
                cl_col <- ncol(t@exprs)
                nmb.events.clust = length(t@exprs[t@exprs[,cl_col]==p,cl_col])
                nmb.events.tot = nrow(t@exprs)

                insertUI(selector = "#pop_red_div",
                         where = "beforeEnd",
                         ui = div(id=paste0("pop_red_",p),
                                  h4(paste0(nmb.events.clust, " events (",100*nmb.events.clust/nmb.events.tot,"%)" ))))
                k <- 1
                lapply(1:length(setsList[[setSelected]][[1]]), function(j)
                {
                    if(!is.null(unlist(setsList[[setSelected]][[1]][[j]])))
                    {
                        insertUI(selector = paste0("#pop_red_",p),
                                 where = "beforeEnd",
                                 ui = div(id=paste0("pop_red_",p,"_file_",j),
                                          numericInput(paste0("pop_red_",p,"_fileInput_",j),
                                                       h6(paste0("Reduction in file ", k)),
                                                       value = 0),
                                          class="pop_red_file_div"))
                              k <<- k+1
                          }
                })

            })
            delay(800,
                shinyjs::enable("pop_red_generate_button")
            )
        }
    })

    observeEvent(input$set_used_refresh,
    {
        shinyjs::disable("pop_red_generate_button")
        removeUI(selector = "#pop_red_div")
        setsAvailable <- list()
        j <- 0
        if(setsList.ID > 0)
        {
            lapply(1:setsList.ID, function(i)
            {
                if(!is.null(unlist(setsList[[i]])))
                {
                    if(length(unlist(setsList[[i]][[1]])) > 0)
                    {
                        setsAvailable <<- c(setsAvailable,list(i))
                        names(setsAvailable)[[length(setsAvailable)]] <<- paste0("Set ", i)
                        j <<- i
                    }
                }
            })
        }

        removeUI(selector = "#pop_red_div")
        shinyjs::disable("pop_red_generate_button")
        if(j > 0)
        {
            shinyjs::enable("set_used_load")
            updateSelectInput(session, "set_used_id", "Set of control Files To Use", choices = setsAvailable, selected = j)
        }
        else
        {
            shinyjs::disable("set_used_load")
            updateSelectInput(session, "set_used_id", "Set of control Files To Use", choices = list(" " = 0), selected = j)
        }
    })

    observeEvent(input$pop_red_generate_button,
    {
         shinyjs::disable("pop_red_generate_button")
         removeUI(selector = "#pop_red_div")
         i <- setSelected
         if(!is.null(unlist(setsList[[setSelected]])))
         {
             setsList[[setSelected]][[2]] <<- list()
             t <- NULL
             while(is.null(t))
             {
                 lapply(1:length(setsList[[setSelected]][[1]]), function(j)
                 {
                     if (!is.null(unlist(setsList[[setSelected]][[1]][[j]])))
                     {
                         t <<- setsList[[setSelected]][[1]][[j]]
                     }
                 })
             }
             nmb.pop <- as.integer(max(t@exprs[,ncol(t@exprs)]))
             file.id <- c()
             lapply(1:length(setsList[[setSelected]][[1]]), function(j)
             {
                 if (!is.null(unlist(setsList[[setSelected]][[1]][[j]])))
                 {
                     file.id <<- c(file.id,j)
                     clust.to.red <- c()
                     red.perc <- c()
                     lapply(1:nmb.pop, function(p)
                     {
                         if(!is.na(input[[paste0("pop_red_",p,"_fileInput_",j)]]))
                         {
                             if (input[[paste0("pop_red_",p,"_fileInput_",j)]] > 0)
                             {
                                 clust.to.red <<- c(clust.to.red,p)
                                 red.perc <<- c(red.perc,input[[paste0("pop_red_",p,"_file_",j)]])
                             }
                         }
                     })
                     fcsMut <- advanced.create.mutation.file(setsList[[setSelected]][[1]][[j]],
                                                             clusters.to.reduce = clust.to.red,
                                                             reduction.percentage = red.perc,
                                                             clust_code = clust.codeList[[setSelected]])
                     setsList[[setSelected]][[2]] <<- c(setsList[[setSelected]][[2]], fcsMut)
                     removeUI(selector = paste("#box_mut_",setSelected,"_",j,sep=""))
                 }
             })
             removeUI(selector = paste0("#set_",i,"_mut_container"))
             insertUI(selector = paste0("#set_container_",i),
                      where = "beforeEnd",
                      ui = div(id=paste0("set_",i,"_mut_container"),
                               downloadButton(paste("set_mut_",i,"_dl", sep=""),
                                              h6("Save"),
                                              class = "setMutBoxButton"),
                               actionButton(paste("set_mut_",i,"_del", sep=""),
                                            h6("Remove"),
                                            class = "setMutBoxButton"),
                               actionButton(paste("set_mut_",i,"_hide", sep=""),
                                            h6("Hide/Show"),
                                            class = "setMutBoxButton"),
                               h5("Mutant Files"),
                               div(id=paste0("set_",i,"_mut"),
                                   class="setBoxMutant"),
                               class="setBoxMutantContainer"))

             observeEvent(input[[paste("set_mut_",i,"_hide",sep="")]],
              {
                  toggle(paste0("set_",i,"_mut"))
              })

             observeEvent(input[[paste("set_mut_",i,"_del",sep="")]],
              {
                  removeUI(selector = paste("#set_",i,"_mut_container",sep=""))
                  setsList[[i]][[2]] <<- list(NULL)
                  if(saveAllButtonPrint)
                  {
                      removeButton <- TRUE
                      temp <- sum(sapply(1:length(setsList),function(k)
                      {
                          return(((length(unlist(setsList[[k]][[1]])) + length(unlist(setsList[[k]][[2]]))) > 0)*1)
                      }))
                      if(temp > 0)
                      {
                          removeButton <- FALSE
                      }
                      if(removeButton)
                      {
                          removeUI(selector = "#save_all_div")
                          saveAllButtonPrint <<- FALSE
                      }
                  }
                  if(length(unlist(setsList[[i]][[2]])) == 0)
                  {
                      removeUI(selector = paste("#set_",i,"_mut_container",sep=""))
                  }
                  if( (length(unlist(setsList[[i]][[1]])) + length(unlist(setsList[[i]][[2]])))  == 0)
                  {
                      removeUI(selector = paste("#set_",i,sep=""))
                  }


                  setsAvailable <- list()
                  j <- 0
                  if(setsList.ID > 0)
                  {
                      lapply(1:setsList.ID, function(i)
                      {
                          if(!is.null(unlist(setsList[[i]])))
                          {
                              if(length(unlist(setsList[[i]][[1]])) > 0)
                              {
                                  setsAvailable <<- c(setsAvailable,list(i))
                                  names(setsAvailable)[[length(setsAvailable)]] <<- paste0("Set ", i)
                                  j <<- i
                              }
                          }
                      })
                  }

                  removeUI(selector = "#pop_red_div")
                  shinyjs::disable("pop_red_generate_button")
                  if(j > 0)
                  {
                      shinyjs::enable("set_used_load")
                      updateSelectInput(session, "set_used_id", "Set of control Files To Use", choices = setsAvailable, selected = j)
                  }
                  else
                  {
                      shinyjs::disable("set_used_load")
                      updateSelectInput(session, "set_used_id", "Set of control Files To Use", choices = list(" " = 0), selected = j)
                  }
              })
             output[[paste("set_mut_",i,"_dl",sep="")]] <- downloadHandler(#---------------------------------------------------------------
                   filename = function()
                   {
                       paste("output","zip",sep=".")
                   },
                   content= function(file)
                   {
                       fnames <- c()
                       tmpdir <- tempdir()
                       setwd(tmpdir)
                       dir.create("mutant")
                       if(length(setsList[[i]][[2]]) > 0)
                       {
                           lapply(c(1:length(setsList[[i]][[2]])), function(l)
                           {
                               if(!is.null(unlist(setsList[[i]][[2]][[l]])))
                               {
                                   fnames <<- c(fnames,paste("mutant/",input[[paste("box_mut_",i,"_",l,"_text",sep="")]],".fcs",sep=""))
                                   fcs.temp <- setsList[[i]][[2]][[l]]
                                   if(!is.null(input[["rangeInput"]]))
                                   {
                                       if(!input[["rangeInput"]])
                                       {
                                           fcs.temp <- advanced.transform.values(fcs.temp)
                                       }
                                   }
                                   write.FCS(fcs.temp,fnames[length(fnames)], delimiter = "#")
                               }
                           })
                       }
                       zip(zipfile=file,files=fnames)
                   }
             )
             lapply(1:length(setsList[[setSelected]][[2]]),function(j)#---------------------------------------------------------------
             {
                i <- setSelected
                t <- setsList[[i]][[2]][[j]]
                insertUI(selector = paste0("#set_",i,"_mut"),
                         where = "beforeEnd",
                         ui = div(id=paste("box_mut_",i,"_",j, sep=""),
                                  textInput(paste("box_mut_",i,"_",j,"_text", sep=""),
                                            "",
                                            width="100%",
                                            value=paste("MUT_events-",nrow(t@exprs),
                                                        "__pop-",max(t@exprs[,ncol(t@exprs)]),
                                                        "__dim-",input$dimInputAut,
                                                        "__file-",i,"_",file.id[j],
                                                        "__",gsub(" ","__",gsub(":","_",Sys.time())),
                                                        sep="")),
                                  downloadButton(paste("box_mut_",i,"_",j,"_dl", sep=""),
                                                 h6("Save"),
                                                 class = "fileBoxButton"),
                                  actionButton(paste("box_mut_",i,"_",j,"_del", sep=""),
                                               h6("Remove"),
                                               class = "fileBoxButton"),
                                  actionButton(paste("box_mut_",i,"_",j,"_print", sep=""),
                                               h6("Show parameters"),
                                               class = "fileBoxButton"),
                                  class="mutantFileBox"
                         )
                )
                observeEvent(input[[paste("box_mut_",i,"_",j,"_del",sep="")]],
                 {
                     removeUI(selector = paste("#box_mut_",i,"_",j,sep=""))
                     setsList[[i]][[2]][[j]] <<- list(NULL)
                     if(saveAllButtonPrint)
                     {
                         removeButton <- TRUE
                         temp <- sum(sapply(1:length(setsList),function(k)
                         {
                             return(((length(unlist(setsList[[k]][[1]])) + length(unlist(setsList[[k]][[2]]))) > 0)*1)
                         }))
                         if(temp > 0)
                         {
                             removeButton <- FALSE
                         }
                         if(removeButton)
                         {
                             removeUI(selector = "#save_all_div")
                             saveAllButtonPrint <<- FALSE
                         }
                     }

                     if(length(unlist(setsList[[i]][[2]])) == 0)
                     {
                         removeUI(selector = paste("#set_",i,"_mut_container",sep=""))
                     }
                     if((length(unlist(setsList[[i]][[1]])) + length(unlist(setsList[[i]][[2]]))) == 0)
                     {
                         removeUI(selector = paste("#set_",i,sep=""))
                     }



                     setsAvailable <- list()
                     j <- 0
                     if(setsList.ID > 0)
                     {
                         lapply(1:setsList.ID, function(i)
                         {
                             if(!is.null(unlist(setsList[[i]])))
                             {
                                 if(length(unlist(setsList[[i]][[1]])) > 0)
                                 {
                                     setsAvailable <<- c(setsAvailable,list(i))
                                     names(setsAvailable)[[length(setsAvailable)]] <<- paste0("Set ", i)
                                     j <<- i
                                 }
                             }
                         })
                     }

                     removeUI(selector = "#pop_red_div")
                     shinyjs::disable("pop_red_generate_button")
                     if(j > 0)
                     {
                         shinyjs::enable("set_used_load")
                         updateSelectInput(session, "set_used_id", "Set of control Files To Use", choices = setsAvailable, selected = j)
                     }
                     else
                     {
                         shinyjs::disable("set_used_load")
                         updateSelectInput(session, "set_used_id", "Set of control Files To Use", choices = list(" " = 0), selected = j)
                     }
                 })
                observeEvent(input[[paste("box_mut_",i,"_",j,"_print",sep="")]],
                 {
                     updateNavbarPage(session, "nav", selected = "viewPan")
                     t <- setsList[[i]][[2]][[j]]
                     output$testPlot <- renderPlot(
                         {
                             l <- ncol(t@exprs) -1
                             n <- as.integer(sqrt(l))
                             if(l>(n^2))
                             {
                                 par(mfrow=c(n+1,n+1))
                             }
                             else
                             {
                                 par(mfrow=c(n,n))
                             }
                             lapply(1:l, function(pl)
                             {
                                 hist(t@exprs[,pl], breaks = 500, main=colnames(t)[pl])
                             })
                         })
                 })
                output[[paste("box_mut_",i,"_",j,"_dl",sep="")]] <- downloadHandler(#---------------------------------------------------------------
                        filename = function()
                        {
                            paste(input[[paste("box_mut_",i,"_",j,"_text",sep="")]],".fcs",sep="")
                        },
                        content= function(file)
                        {
                            fcs.temp <- setsList[[i]][[2]][[j]]
                            if(!is.null(input[["rangeInput"]]))
                            {
                                if(!input[["rangeInput"]])
                                {
                                    fcs.temp <- advanced.transform.values(fcs.temp)
                                }
                            }
                            write.FCS(fcs.temp,file, delimiter="#")
                        }
                )
             })
         }
     })

}
