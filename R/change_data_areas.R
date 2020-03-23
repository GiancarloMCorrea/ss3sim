#' Merge data created by the OM to the EM where there are different number 
#' of areas between them. Then, sample functions take samples from 
#' this merged data.
#'
#' This prepares a \code{.dat} file to be used by an EM when more
#' multiple areas are implemented. 
#'
#' @author Giancarlo M. Correa
#' @param ini_dat \code{.dat} used by the OM.
#' @param transform_fleets List that assigns EM fleets to OM fleets
#' to be merged.
#' @param area_fleet_em Area assignement for each fleet in the EM.
#' @param fleet_index Fleet index (in numbers) in the OM.
#' @template dat_list
#' @seealso ss3sim_base, change_data
#' @family change functions
#' @return An invisible cleaned data list as an object.
#' @note This function does not write the result to file.
change_data_areas <- function(ini_dat, transform_fleets, area_fleet_em, fleet_index){

  data_2area <- ini_dat
  data_1area <- data_2area

  # new variables:
  nFleet <- as.vector(unlist(lapply(transform_fleets, function(x) x[1])))
  FleetFinder <- which(data_2area$fleetinfo$fleetname %in% nFleet)

  # start to replace values from OM to EM
  data_1area$Nareas <- max(area_fleet_em$area)
  data_1area$Nfleets <- nrow(area_fleet_em)

  data_1area$fleetinfo <- data_2area$fleetinfo[FleetFinder, ]
  data_1area$fleetinfo$fleetname <- area_fleet_em$fleetname
  data_1area$fleetinfo$area <- area_fleet_em$area

  data_1area$fleetnames <- area_fleet_em$fleetname
  data_1area$surveytiming <- data_1area$fleetinfo$surveytiming
  data_1area$units_of_catch <- data_1area$fleetinfo$units
  data_1area$areas <- data_1area$fleetinfo$area

  vecFleets <- 1:data_1area$Nfleets # vector of fleet indices for EM

  # CATCH ------------------------------------
  CatchList <- fleet_index[which(data_1area$fleetinfo$type == 1)]
  tmpList <- list()
  for(j in seq_along(CatchList)){

    tmpList[[j]] <- data_2area$catch[data_2area$catch$fleet == CatchList[[j]][1], ] # 1 because fishery fleet is always one
    tmpcatch2 <- rep(0, times = length(data_2area$catch$catch[data_2area$catch$fleet == CatchList[[j]][1]]))

    iNFleets <- length(CatchList[[j]])
    if(iNFleets > 1) {
      
      for(i in 2:iNFleets) {
        tmpcatch3 <- data_2area$catch$catch[data_2area$catch$fleet == CatchList[[j]][i]]
        tmpcatch2 <- tmpcatch2 + tmpcatch3
      }

    }

    tmpList[[j]]$catch <- (tmpList[[j]]$catch + tmpcatch2)
    tmpList[[j]]$fleet <- vecFleets[which(names(CatchList)[j] == data_1area$fleetinfo$fleetname)]

  }

  data_1area$catch <- do.call('rbind', tmpList)

  # CPUE-------------------------------------------------------
  data_1area$CPUEinfo <- data_2area$CPUEinfo[FleetFinder,]
  rownames(data_1area$CPUEinfo) <- area_fleet_em$fleetname
  data_1area$CPUEinfo$Fleet <- vecFleets

  CPUEList <- fleet_index[which(data_1area$fleetinfo$type == 3)]
  tmpList <- list()
  for(j in seq_along(CPUEList)){

    tmpList[[j]] <- data_2area$CPUE[data_2area$CPUE$index == CPUEList[[j]][1], ] # 1 because fishery fleet is always one
    tmpcatch2 <- rep(0, times = length(data_2area$CPUE$obs[data_2area$CPUE$index == CPUEList[[j]][1]]))

    iNFleets <- length(CPUEList[[j]])
    if(iNFleets > 1) {

      for(i in 2:iNFleets) {
        tmpcatch3 <- data_2area$CPUE$obs[data_2area$CPUE$index == CPUEList[[j]][i]]
        tmpcatch2 <- tmpcatch2 + tmpcatch3
      }

    }

    tmpList[[j]]$obs <- (tmpList[[j]]$obs + tmpcatch2)
    tmpList[[j]]$index <- vecFleets[which(names(CPUEList)[j] == data_1area$fleetinfo$fleetname)]

  }

  data_1area$CPUE <- do.call('rbind', tmpList)

  data_1area$len_info <- data_2area$len_info[FleetFinder,]
  rownames(data_1area$len_info) <- area_fleet_em$fleetname

  # CREATE NEW LEN COMPOSITION ----------------------------------
  weightDF <- data.frame(year = c(data_2area$catch$year , data_2area$CPUE$year),
                        fleet = c(data_2area$catch$fleet , data_2area$CPUE$index),
                        weight = c(data_2area$catch$catch, data_2area$CPUE$obs))
  weightDF$ind <- paste0(weightDF$year, '_', weightDF$fleet)

  tmplencom <- data_2area$lencomp
  tmplencom$ind <- paste0(tmplencom$Yr, '_', tmplencom$FltSvy)

  tmpList <- list()
  for(j in seq_along(fleet_index)){
  
    matchVec <- tmplencom$FltSvy %in% fleet_index[[j]]
    endMat <- NULL

    if(any(matchVec)){

      weightDFtmp <- weightDF[weightDF$fleet %in% fleet_index[[j]], ] # subset catch data for that fleet
      tmplencom2 <- tmplencom[matchVec, ] # subset lencomps for that fleet
      lenmatrix <- tmplencom2[ ,7:(ncol(tmplencom2)-1)] # matrix years by length bins

      endMat1 <- tmplencom2[tmplencom2$FltSvy %in% fleet_index[[j]][1] ,1:6] # used when data is merged
      endMat1_1 <- tmplencom2[tmplencom2$FltSvy %in% fleet_index[[j]] ,1:6] # used to identify gender

      weightVec <- weightDFtmp[match(tmplencom2$ind, weightDFtmp$ind), ]
      weightVec2 <- aggregate(weightVec$weight, list(year = weightVec$year), sum)

      # here start gender effect:
      Genders = unique(endMat1$Gender)

      for(k in seq_along(Genders)) {

        lenmatrix_2 = lenmatrix[which(endMat1_1$Gender == Genders[k]), ]
        mat1 <- sweep(as.matrix(lenmatrix_2), 1, weightVec$weight, FUN = '*')
        mat2 <- rowsum(mat1, group = endMat1_1$Yr, reorder = FALSE)

        if(any(is.na(apply(X = mat2, MARGIN = 1, FUN = max)))){
          delRows <- as.vector(which(is.na(apply(X = mat2, MARGIN = 1, FUN = max))))
          mat2 <- mat2[-delRows, ]
          endMat1 <- endMat1[-delRows, ]
        }

        mat3 <- sweep(as.matrix(mat2), 1, weightVec2$x, FUN = '/')
        endMat <- cbind(endMat1, mat3)

      }

      endMat$FltSvy <- vecFleets[which(names(fleet_index)[j] == data_1area$fleetinfo$fleetname)]

    }

    tmpList[[j]] <- endMat

  }

  data_1area$lencomp <- do.call('rbind', tmpList)

  # --------------------------------------------------------------

  data_1area$age_info <- data_2area$age_info[FleetFinder,]
  rownames(data_1area$age_info) <- area_fleet_em$fleetname


  # CREATE NEW AGE COMPOSITION ----------------------------------

  tmpagecom <- data_2area$agecomp
  tmpagecom$ind <- paste0(tmpagecom$Yr, '_', tmpagecom$FltSvy)

  tmpList <- list()
  for(j in seq_along(fleet_index)){

    matchVec <- tmpagecom$FltSvy %in% fleet_index[[j]]
    endMat <- NULL

    if(any(matchVec)){

      weightDFtmp <- weightDF[weightDF$fleet %in% fleet_index[[j]], ]
      tmpagecom2 <- tmpagecom[matchVec, ]
      agematrix <- tmpagecom2[ ,10:(ncol(tmpagecom2)-1)] # matrix years by length bins

      endMat1 <- tmpagecom2[tmpagecom2$FltSvy %in% fleet_index[[j]][1] ,1:9]
      endMat1_1 <- tmpagecom2[tmpagecom2$FltSvy %in% fleet_index[[j]] ,1:6] # used to identify gender

      weightVec <- weightDFtmp[match(tmpagecom2$ind, weightDFtmp$ind), ]
      weightVec2 <- aggregate(weightVec$weight, list(year = weightVec$year), sum)

      # here start gender effect:
      Genders = unique(endMat1$Gender)

      for(k in seq_along(Genders)) {

        agematrix_2 = agematrix[which(endMat1_1$Gender == Genders[k]), ]
        mat1 <- sweep(as.matrix(agematrix_2), 1, weightVec$weight, FUN = '*')
        mat2 <- rowsum(mat1, group = endMat1_1$Yr, reorder = FALSE)
        if(any(is.na(apply(X = mat2, MARGIN = 1, FUN = max)))){
          delRows <- as.vector(which(is.na(apply(X = mat2, MARGIN = 1, FUN = max))))
          mat2 <- mat2[-delRows, ]
          endMat1 <- endMat1[-delRows, ]
        }
        
        mat3 <- sweep(as.matrix(mat2), 1, weightVec2$x, FUN = '/')
        endMat <- cbind(endMat1, mat3)

      }

      endMat$FltSvy <- vecFleets[which(names(fleet_index)[j] == data_1area$fleetinfo$fleetname)]

    }

    tmpList[[j]] <- endMat

  }

  data_1area$agecomp <- do.call('rbind', tmpList)

  # --------------------------------------------------------------

  data_1area$Nfleet <- length(which(data_1area$fleetinfo$type == 1))
  data_1area$Nsurveys <- length(which(data_1area$fleetinfo$type == 3))
  data_1area$N_areas <- max(area_fleet_em$area)
  data_1area$N_cpue <- nrow(data_1area$CPUE)

  data_1area$fleetinfo1 <- data_2area$fleetinfo1[, FleetFinder]
  colnames(data_1area$fleetinfo1) <- area_fleet_em$fleetname
  tmpMat <- as.matrix(data_1area$fleetinfo1)
  tmpMat[2, ] <- area_fleet_em$area
  data_1area$fleetinfo1 <- as.data.frame(tmpMat)

  data_1area$fleetinfo2 <- data_2area$fleetinfo2[, FleetFinder]
  colnames(data_1area$fleetinfo2) <- area_fleet_em$fleetname

  data_1area$comp_tail_compression <- data_2area$comp_tail_compression[FleetFinder]
  data_1area$add_to_comp <- data_2area$add_to_comp[FleetFinder]
  data_1area$max_combined_lbin <- data_2area$max_combined_lbin[FleetFinder]
  data_1area$NCPUEObs <- data_2area$NCPUEObs[FleetFinder]
  data_1area$comp_tail_compression <- data_2area$comp_tail_compression[FleetFinder]

  return(data_1area)

}
