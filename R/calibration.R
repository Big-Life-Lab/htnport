library(ggplot2)

calibration <- function(prediction, outcome, package = "Steyerberg", ...)
{
  # input:
  #   outcome:       actual outcome
  #   prediction:    model predicted values, should be the same length as outcome.
  #   groupvec:         a vector of the same length as outcome, subgroup categories for strong calibration
  #   package:       can be "Steyerberg" or "Harrell"
  #   ...:      optional arguments for insider functions
  
  # output:          a named vector

  if (length(outcome) != length(prediction)) 
    stop("lengths of outcome and prediction do not agree")
  
  if (length(unique(outcome)) == 2)
  {
    # Logistic regression: based on rms::val.prob
    if (min(prediction, na.rm = TRUE) < 0 | max(prediction, na.rm = TRUE) > 1) 
      stop("Logistic regression: predicted probability should be between 0 and 1.")
    
    # remove probability 0 or 1
    outcome    <- outcome[prediction > 0 & prediction < 1]
    prediction <- prediction[prediction > 0 & prediction < 1]
    if (package == "Steyerberg")
    {
      stats <- CalibrationCurves::val.prob.ci.2(prediction, outcome, ...)
    } else
    {
      stats <- rms::val.prob(prediction, outcome, ...)
    }
  } else
  {
    stats <- calibrationCon(prediction, outcome, ...)
   }
  
  return(stats)
}

# ===========================

calibrationCon <- function(prediction, outcome, groupvec = NULL, g = 1, violin = FALSE, marginPlt = FALSE) {
  
  if (!is.null(groupvec))
  {
    if (length(outcome) != length(groupvec)) 
      stop("lengths of outcome and group do not agree")
    
    groupvec <- as.character(groupvec)
  }
  
  df <- data.frame(prediction = prediction, outcome = outcome)
  if (!is.null(groupvec))
    df <- df %>% mutate(group_fct = factor(groupvec))
  
  df <- df %>% mutate(er = abs(outcome - prediction))
  
  min <- min(min(outcome, na.rm = TRUE), min(prediction, na.rm = TRUE))
  max <- max(max(outcome, na.rm = TRUE), max(prediction, na.rm = TRUE))
  
  # calibration plot
  if (g == 1) # The scatter plot
  {
    p <- ggplot(df, aes(prediction, outcome))+geom_point(alpha = 0.5)+geom_smooth(color = "red")+
      xlab("Predicted value")+
      ylab("Observed value")+
      xlim(min,max)+
      ylim(min, max)+
      geom_segment(aes(x = min, y = min, xend = max, yend = max), linetype = "dotted", color = "black")+
      coord_fixed()
    
    if (!is.null(groupvec))
      p <- p + facet_wrap(~group_fct)
    
    if (marginPlt)
    {
      p <- p + theme(legend.position="bottom")
      p <- ggExtra::ggMarginal(p, type="histogram")
    }
    
    print(p)
  } else
  {
    df <- df %>% mutate(prediction_bin = cut(prediction, breaks=g))
    
    if (violin)
    {
      p <- ggplot(df,aes(x=prediction_bin,y=outcome)) + 
        geom_violin() + 
        geom_boxplot(width = 0.1)
      
      print(p)
    } else
    {
      data.bin <- plyr::ddply(df, "prediction_bin", function(DF) {
        data.frame(median=plyr::numcolwise(median)(DF), 
                   ci1 = plyr::numcolwise(quantile)(DF, probs = .25),
                   ci2 = plyr::numcolwise(quantile)(DF, probs = .75),
                   count=plyr::numcolwise(length)(DF))
      })
      
      p <- ggplot(data.bin,aes(x=median.prediction,y=median.outcome,size=count.prediction)) + 
        geom_point()+
        geom_errorbarh(aes(xmin=ci1.prediction, xmax=ci2.prediction), size = 0.5)+
        geom_errorbar(aes(ymin=ci1.outcome, ymax=ci2.outcome), size = 0.5)+
        geom_smooth(method = "lm", color = "red", size = 0.5)+
        xlab("Predicted value")+
        ylab("Observed value")+
        xlim(min,max)+
        ylim(min, max)+
        geom_segment(aes(x = min, y = min, xend = max, yend = max), linetype = "dotted", color = "black", size = 0.5)+
        coord_fixed()
      
      if (marginPlt)
      {
        p <- p + theme(legend.position="bottom")
        
        library(cowplot)
        
        xhist <- 
          axis_canvas(p, axis = "x") + 
          geom_histogram(data = df, aes(x = prediction), color = 'lightgray')
        yhist <-
          axis_canvas(p, axis = "y", coord_flip = TRUE) + 
          geom_histogram(data = df, aes(x = outcome), color = 'lightgray') +
          coord_flip()
        p <- p %>%
          insert_xaxis_grob(xhist, grid::unit(1, "in"), position = "top") %>%
          insert_yaxis_grob(yhist, grid::unit(1, "in"), position = "right") %>%
          ggdraw()
        #p <- ggExtra::ggMarginal(p, type="histogram")
      }
      
      print(p)
    }
   }
  
  # stats of the whole data
  lr <- lm(outcome ~ prediction, data = df)
  eavg <- mean(df$er, na.rm = TRUE)
  emax <- max(df$er, na.rm = TRUE)
  e90 <- unname(quantile(df$er, 0.9, na.rm = TRUE))
  
  stats <- c("Overall", lr$coefficients[1], lr$coefficients[2], emax, e90, eavg)
  names(stats) <- c("", "Intercept", "Slope", "Emax", "E90", "Eavg")
  
  # stats in each group
  if (!is.null(groupvec))
  {
    group_name <- levels(df$group_fct)
    
    for (i in 1:length(group_name))
    {
      temp <- df %>% filter(group_fct == group_name[i])
      lr <- lm(outcome ~ prediction, data = temp)
      eavg <- mean(temp$er, na.rm = TRUE)
      emax <- max(temp$er, na.rm = TRUE)
      e90 <- unname(quantile(temp$er, 0.9, na.rm = TRUE))
      stats <- rbind(stats,c(group_name[i], lr$coefficients[1], lr$coefficients[2], emax, e90, eavg))
    }
  }
   
  return(stats)
}



