library(flowCore)
library(microbenchmark)
library(gtools)
library(Biobase)


advanced.generateFCS <- function(nmb.events = 10000, nmb.clust = 0, freq.pop = NULL, nmb.dim = 4,nmb.files = 1,
                               generate.min.pop = FALSE, min.pop.size.density = 0, max.pop.size.density = 5, nmb.min.pop = 1, savePath = NULL,
                               load.patterns.path = NULL, save.patterns.path=NULL, save.clusters = FALSE, variable.param = c(),
                               nmb.clust.to.change = 0, reduction.percentage.per.file = list())
{
    #Generation d'une matrice de reference pour tous les fichiers---------------------------------------------------
    generated.matrix <- matrix(nrow=nmb.events,ncol=nmb.dim+2)



    #Creation des noms de parametres + ajout parametre cluster--------------------------------
    colnames(generated.matrix) <- c(rep("NA",times = nmb.dim),"time","cluster")
    for (current_dim in 1:nmb.dim)
    {
        colnames(generated.matrix)[current_dim] <- paste("PARAM_",current_dim, sep="")
    }


    #Correction du nombre de clusters s'il n'y a pas assez de parametres pour les differencier (patterns confondus)----------------------
    #Generation de la liste "clust_events" a partir de "temp", contenant les indices du premier event de chaque cluster-----------
    clust_events <- list()
    temp <- vector()
    if (nmb.clust > length(freq.pop))#Generation automatique des clusters...
    {
        if (nmb.clust > 2^nmb.dim)
        {
            nmb.clust <- 2^nmb.dim
        }
        if(generate.min.pop)
        {
            temp2 <- sapply(c(1:nmb.min.pop),function(i)
            {
                return(as.integer(runif(n = 1,min = min.pop.size.density, max = max.pop.size.density) * nmb.events / 100))
            })
            main.pop <- nmb.events-sum(temp2)
            alpha.param <- 2*(main.pop * 0.15) / (nmb.clust-nmb.min.pop) / (nmb.clust-nmb.min.pop+1)
            temp <- c(temp2, sapply(1:(nmb.clust-nmb.min.pop),function(i)
            {
                a <- as.integer(alpha.param*i + 0.85*main.pop/(nmb.clust-nmb.min.pop))
                return(a)
            }))
        }
        else
        {
            main.pop <- nmb.events
            alpha.param <- 2*(main.pop * 0.15) / nmb.clust / (nmb.clust+1)
            temp <- sapply(1:(nmb.clust),function(i)
            {
                a <- as.integer(alpha.param*i + 0.85*main.pop/nmb.clust)
                return(a)
            })
        }
    }
    else if (nmb.clust <= length(freq.pop)) #...ou generation manuelle en fonction d'une liste de frequences
    {
        nmb.clust <- length(freq.pop)
        if (nmb.clust > 2^nmb.dim)
        {
            nmb.clust <- 2^nmb.dim
        }
        minF <- sum(sapply(c(1:length(freq.pop)), function(i)
        {
            return(freq.pop[[i]][1])
        }))
        maxF <- sum(sapply(c(1:length(freq.pop)), function(i)
        {
            return(freq.pop[[i]][2])
        }))

        temp <- c(sapply(c(1:length(freq.pop)), function(i)
        {
            return(freq.pop[[i]][1])
        }))

        if (minF > 100)
        {
            print("TOTAL DENSITY > 1 : DENSITIES MODIFIED ")
            threshold_down <- minF - 100
            for (i in c(1:nmb.clust))
            {
                t <- min(freq.pop[[i]][1], threshold_down)
                freq.pop[[i]][1] <- freq.pop[[i]][1] - t
                threshold_down <- threshold_down - t
                if (threshold_down <= 0)
                {
                    i = nmb.clust
                }
            }
            minF <- sum(sapply(c(1:length(freq.pop)), function(i)
            {
                return(freq.pop[[i]][1])
            }))
        }

        if (maxF < 100)
        {
            print("NO SOLUTION FOUND : DENSITIES MODIFIED ")
            threshold_up <- 100 - maxF
            for (i in c(1:nmb.clust))
            {
                t <- min(100-freq.pop[[i]][2], threshold_up)
                freq.pop[[i]][2] <- freq.pop[[i]][2] + t
                threshold_up <- threshold_up - t
                if (threshold_up <= 0)
                {
                    i = nmb.clust
                }
            }
            maxF <- sum(sapply(c(1:length(freq.pop)), function(i)
            {
                return(freq.pop[[i]][2])
            }))
        }



        if(minF <= 100 && maxF >= 100)
        {
            rep <- minF
            delta_rep <- 100 - minF
            while (as.integer(100*(rep - 100)) < 0)
            {
                temp <- c(sapply(c(1:length(freq.pop)), function(i)
                {
                    return(temp[i] + min(delta_rep/length(freq.pop),freq.pop[[i]][2]-temp[i]))
                }))
                rep <- sum(temp)
                delta_rep <- 100 - rep
            }
        }
        temp2 <- sapply(c(1:length(freq.pop)),function(i){
            return(as.integer(temp[i] * nmb.events / 100))
        })
        temp <- temp2
    }
    clust_events <- lapply(1:nmb.clust,function(i)#Utilisation de "temp" pour creer un index complet de tous les events. Chaque element de la liste est un cluster
    {
        if (i<nmb.clust)
        {
            if(i==1)
            {
                a <- 1:temp[1]
            }
            else
            {
                b <- sum(temp[c(1:i-1)])
                a <- (b+1):(b+temp[i])
            }
        }
        else
        {
            b <- sum(temp[c(1:i-1)])
            a <- (b+1):nmb.events
        }
        return(a)
    })







    #Generation de la liste des "clust_events" pour chaque fichier-------------------------------
    if (length(reduction.percentage.per.file) < (nmb.clust.to.change))
    {
        n = nmb.clust.to.change - length(reduction.percentage.per.file)
        emptyPerc <- rep(0,nmb.files)
        print(reduction.percentage.per.file)
        reduction.percentage.per.file <- c(reduction.percentage.per.file, rep(emptyPerc,n))
    }

    if( nmb.files > 1)
    {
        list.clust_events <- c(list(clust_events),sapply(1:(nmb.files-1), function(n.fl)
        {
            cl.e <- clust_events
            if(nmb.clust.to.change > 0)
            {
                cl.to.change <- sample(1:nmb.clust, nmb.clust.to.change, replace = FALSE)
                for (k in 1:nmb.clust.to.change)
                {
                    size.clust <- length(cl.e[[cl.to.change[k]]])
                    n = as.integer(size.clust * reduction.percentage.per.file[[cl.to.change[k]]][n.fl] / 100)
                    ev.to.repart <- cl.e[[cl.to.change[k]]][(size.clust-n+1):size.clust]
                    cl.e[[cl.to.change[k]]] <- cl.e[[cl.to.change[k]]][1:(size.clust-n)]
                    n.rep <- max(0,as.integer(n/(nmb.clust - nmb.clust.to.change)))
                    j <- 1
                    for (i in 1:nmb.clust)
                    {
                        if(j == nmb.clust.to.change)
                        {
                            if(!(i %in% cl.to.change))
                            {
                                cl.e[[i]] <- c(cl.e[[i]], ev.to.repart[ ((j-1)*n.rep + 1) : length(ev.to.repart)])
                            }
                        }
                        else
                        {
                            if(!(i %in% cl.to.change))
                            {
                                cl.e[[i]] <- c(cl.e[[i]], ev.to.repart[ ((j-1)*n.rep + 1) : (j * n.rep)])
                                j <- j+1
                            }
                        }
                    }
                }
            }

            return(cl.e)
        }, simplify = FALSE))
    }
    else
    {
        list.clust_events <- list(clust_events)
    }




    #Generation de tous les patterns possibles (fonction de "nmb.dim") ou lecture d'un fichier, et stockage dans "clust_code"----------------
    clust_code <- list()
    if(is.null(load.patterns.path))#Generation des patterns
    {
        for (l in c(1:nmb.clust))
        {
            change = TRUE
            while (change)
            {
                change = FALSE
                clust_code[[l]] <- sapply(c(1:nmb.dim), function(i)
                {
                    return(as.integer(sample(1:2,1)))
                })
                if(l>1)
                {
                    for (m in c(1:(l-1)))
                    {
                        if(prod(clust_code[[m]]==clust_code[[l]]))
                        {
                            change = TRUE
                            m = l
                        }
                    }
                }
            }
        }
        clust_code <- as.list(sample(clust_code, nmb.clust, replace = FALSE))#extraction de nmb.clust patterns
    }
    else#Lecture des patterns
    {
        mat <- read.csv(paste(load.patterns.path,".csv",sep=""))
        clust_code <- as.list(lapply(1:nrow(mat), function(i)
        {
              return(as.vector(mat[i,]))
        }))
        clust_code <- clust_code[1:nmb.clust]
    }


    #Sauvegarde des patterns (si option specifiee)---------------------------------
    if(!is.null(save.patterns.path))
    {
        mat <- matrix(unlist(clust_code, use.names = FALSE), nrow=nmb.dim)
        write.csv(mat, file=paste(save.patterns.path,".csv",sep=""))
    }




    #Generation des valeurs pour chaque event et regroupement par clusters, puis stockage dans "events_values"--------------------------------
    xmean <- list()
    sd <- list()
    for (current_cluster in 1:nmb.clust)
    {
        x <- 4.5
        xmean[[current_cluster]] <- lapply(1:nmb.dim,function(i)
        {
            t <- clust_code[[current_cluster]][i]
            m <- (t==1) * runif(1,x/12,x/5.6) +
                (t==2) * runif(1, 5*x/12, 11*x/12)
            return(m)
        })
        sd[[current_cluster]]<- lapply(1:nmb.dim,function(i)
        {
            t <- clust_code[[current_cluster]][i]
            s <- (t==1) * runif(1,x/7.7,x/5.8) +
                (t==2) * runif(1,x/6.6,x/4.2)

            return(s)
        })
        events_values <- lapply(1:nmb.dim,function(i)
        {
            v <- rnorm(length(clust_events[[current_cluster]]),mean = xmean[[current_cluster]][[i]],sd = sd[[current_cluster]][[i]])
            val <- sapply(c(1:length(v)),function(x)
            {
                tx <- v[[x]]
                while((tx < 0) || (tx > 4.5))
                {
                    tx <- rnorm(1,mean = xmean[[current_cluster]][[i]],sd = sd[[current_cluster]][[i]])
                }
                return(tx)
            })
            return(val)
        })

        for (k in 1:length(clust_events[[current_cluster]]))
        {
            generated.matrix[clust_events[[current_cluster]][k], ]  <- c(sapply(1:(nmb.dim+2), function(i)
            {
                r <- 0
                if(i <= nmb.dim)
                {
                    r <- events_values[[i]][k]
                }
                else 
                {
                    r <- as.integer(current_cluster)
                }
                return (r)
            }, simplify="array"))
        }
    }



    #Generation de tous les fichiers fcs---------------------------------------
    list.fcs.files <- list()
    for (i in 1:nmb.files)
    {
        #Modificaiton de chaque matrice----------------------------------------------
        temp.matrix <- generated.matrix
        for (cl in 1:nmb.clust)
        {
            if(nmb.clust.to.change > 0)
            {
                if (length(list.clust_events[[i]][[cl]]) > length(clust_events[[cl]]))
                {
                    for (d in 1:(nmb.dim+2))
                    {
                        if(d <= nmb.dim)
                        {
                            for (e in list.clust_events[[i]][[cl]][ (length(clust_events[[cl]]) + 1) : length(list.clust_events[[i]][[cl]]) ])
                            {
								tx <- rnorm(1, mean = xmean[[cl]][[d]], sd = sd[[cl]][[d]])
								while((tx < 0) || (tx > 4.5))
								{
									tx <- rnorm(1,mean = xmean[[cl]][[d]],sd = sd[[cl]][[d]])
								}
                                temp.matrix[e,d] <- tx
                            }
                        }
                        else if (d == nmb.dim+2)
                        {
                            for (e in list.clust_events[[i]][[cl]][ (length(clust_events[[cl]]) + 1) : length(list.clust_events[[i]][[cl]]) ])
                            {
                                temp.matrix[e,d]<- cl
                            }
                        }
                    }
                }
            }
            for (p in variable.param)
            {
                x <- 4.5
                xm <- (clust_code[[cl]][p]==1) * runif(1,x/12,x/5.6) +
                    (clust_code[[cl]][p]==2) * runif(1, 5*x/12, 11*x/12)

                xsd <- (clust_code[[cl]][p]==1) * runif(1,x/7.7,x/5.8) +
                    (clust_code[[cl]][p]==2) * runif(1,x/6.6,x/4.2)
					
                temp.matrix[list.clust_events[[i]][[cl]],p] <- sapply(list.clust_events[[i]][[cl]], function(i)
                {
					tx <- rnorm(1, mean = xm, sd = xsd)
					while((tx < 0) || (tx > 4.5))
					{
						tx <- rnorm(1,mean = xm,sd = xsd)
					}
                    return(tx)
                })
            }
        }


        #Ecriture des metadata
		
		fcs <- flowFrame(temp.matrix)
		descR <- description(fcs)
		lapply(c(1:dim(temp.matrix)[2]),function(x)
		{
			descR[[paste0("$P",x,"R")]] <<- 262144
		})
		
		nmb.grp <- min(nmb.events,1000)
		descR[["TIMESTEP"]] <- 1/nmb.grp
		lapply(1:nmb.events, function(e)
		{
			temp.matrix[e,(nmb.dim+1)] <<- descR[["TIMESTEP"]] *  as.integer(e / (nmb.events/nmb.grp))
			
		})
		fcs <- flowFrame(temp.matrix, description = descR)

        if(!is.null(savePath))
        {
            write.FCS(fcs, paste(savePath,i,".fcs"))
        }

        list.fcs.files[[i]] <- fcs
    }

    return(list(list.fcs.files, clust_code))
}


advanced.create.mutation.file <- function(ctrl.fcs.file, clusters.to.reduce = c(), reduction.percentage = c(), clust_code = list())
{
    temp.matrix <- ctrl.fcs.file@exprs
    nmb.dim <- ncol(temp.matrix) -2
    nmb.clust <- max(temp.matrix[,nmb.dim+2])
    nmb.clust.to.change <- length(clusters.to.reduce)

    clust_events <- lapply(1:nmb.clust, function(i)
    {
        return(c(match(i,temp.matrix[,nmb.dim+1])))
    })

    cl.e <- lapply(1:nmb.clust, function(i)
    {
        return(which(temp.matrix[,nmb.dim+1] %in% c(i)))
    })

    if(length(reduction.percentage) < length(clusters.to.reduce))
    {
        m <- length(clusters.to.reduce) - length(reduction.percentage)
        reduction.percentage <- c(reduction.percentage, rep(0,m))
    }

    #MODIFICATION DE LA LISTE DES EVENTS
    if(nmb.clust.to.change > 0)
    {
        cl.to.change <- clusters.to.reduce
        for (k in 1:nmb.clust.to.change)
        {
            size.clust <- length(cl.e[[cl.to.change[k]]])
            n = as.integer(size.clust * reduction.percentage[k] / 100)
            ev.to.repart <- cl.e[[cl.to.change[k]]][(size.clust-n+1):size.clust]
            print(paste("size : ",n))
            cl.e[[cl.to.change[k]]] <- cl.e[[cl.to.change[k]]][1:(size.clust-n)]
            n.rep <- max(0,as.integer(n/(nmb.clust - nmb.clust.to.change)))
            j <- 1
            for (i in 1:nmb.clust)
            {
                if(j == nmb.clust.to.change)
                {
                    if(!(i %in% cl.to.change))
                    {
                        cl.e[[i]] <- c(cl.e[[i]], ev.to.repart[ ((j-1)*n.rep + 1) : length(ev.to.repart)])
                    }
                }
                else
                {
                    if(!(i %in% cl.to.change))
                    {
                        cl.e[[i]] <- c(cl.e[[i]], ev.to.repart[ ((j-1)*n.rep + 1) : (j * n.rep)])
                        j <- j+1
                    }
                }
            }
        }
    }

    for (cl in 1:nmb.clust)
    {
        if(nmb.clust.to.change > 0)
        {
            if (length(cl.e[[cl]]) > length(clust_events[[cl]]))
            {
                for (d in 1:(nmb.dim+2))
                {
                    if(d <= nmb.dim)
                    {
                        for (e in cl.e[[cl]][ (length(clust_events[[cl]]) + 1) : length(cl.e[[cl]]) ])
                        {
                            x <- 4.5
                            xm <- (clust_code[[cl]][d]==1) * runif(1,x/12,x/5.6) +
                                (clust_code[[cl]][d]==2) * runif(1, 5*x/12, 11*x/12)

                            xsd <- (clust_code[[cl]][d]==1) * runif(1,x/7.7,x/5.8) +
                                (clust_code[[cl]][d]==2) * runif(1,x/6.6,x/4.2)

                            val <- rnorm(1, mean = xm, sd = xsd)
                            while( val > x || val < 0)
                            {
                                val <- rnorm(1, mean = xm, sd = xsd)
                            }
                            temp.matrix[e,d] <- val
                        }
                    }
                    else if (d == nmb.dim+2)
                    {
                        for (e in cl.e[[cl]][ (length(clust_events[[cl]]) + 1) : length(cl.e[[cl]]) ])
                        {
                            temp.matrix[e,d]<- cl
                        }
                    }
                }
            }
        }
    }

    #Ecriture des metadata-------------------------
	
	fcs <- flowFrame(temp.matrix)
    descR <- description(fcs)
    lapply(c(1:dim(temp.matrix)[2]),function(x)
    {
        descR[[paste0("$P",x,"R")]] <<- 262144
    })
	fcs <- flowFrame(temp.matrix, description = descR)

    return(fcs)
}


advanced.transform.values <- function(fcs.in)
{
    write.FCS(fcs.in,"tempazerty.fcs")
    fcs <- read.FCS("tempazerty.fcs")
    lgcl <- logicleTransform(t=262144)
    invLgcl <- inverseLogicleTransform(trans = lgcl)
    nmb.dim <- ncol(fcs.in@exprs)-1

    fcs <- flowCore::transform(fcs, transformList(colnames(fcs.in@exprs)[1:nmb.dim], invLgcl))
    fcs@parameters@data$maxRange <- sapply(1:(nmb.dim+2), function(i)
    {
        return(262144)
    })

    return (fcs)
}
