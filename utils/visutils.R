# p_lin: Predict Linear
# Returns a 100 row (x,y) data frame, which describe a line.
p_lin <- function(dataset, responselabel, termlabel) {
  df = data.frame(response = dataset[[responselabel]],
                  term = as.numeric(dataset[[termlabel]]))
  min = min(df$term) * 100
  max = max(df$term) * 100
  fm <- lm(response ~ term, data = df)
  df_p <- data.frame(x = (min:max)/100)
  df_p$term <- (min:max)/100
  curve <- data.frame(x = (min:max)/100, y = predict(fm, df_p))
  return(curve)
}

p_lin_coef <- function(dataset, responselabel, termlabel) {
  df = data.frame(response = dataset[[responselabel]],
                  term = as.numeric(dataset[[termlabel]]))
  min = min(df$term) * 100
  max = max(df$term) * 100
  fm <- lm(response ~ term, data = df)
  coefficients = coef(fm)
  coeff_string <- paste(names(coefficients), coefficients, sep = ": ", collapse = ", ")
  return(coeff_string)
}


p_supsmu <- function(df, response, term, b=0.1) {
  curve = smooth.spline(df[[term]], df[[response]], spar=b)
  return(tibble(x = curve$x, y=curve$y))
}

n_clip <- function(x, a = 0, b = 1) {
  ifelse(x <= a,  a, ifelse(x >= b, b, x))
}

t_color <- function(x, levels, colors) {
  #debug
  #x = c(0.23, 0.45, 0.95, 0.01)
  #l = tibble(levels = c(0, 0.2,0.55,0.85,1.1),
  #           colors = c("g0","g1", "g2", "g3", "g4"))
  #levels = l$levels
  #colors = l$colors
    
  toMatch = sapply(x, function(x) min(levels[levels>=x]))
  
  match = match(toMatch, levels)
  c <- colors[match]
  
  return(c)
}

plot_scatter <- function(dataset, xlabel, ylabel, plabel, desc,miny,maxy,jit,xtitle = " ",ytitle = " ", symbol=NA) {
  df = data.frame(x = dataset[[xlabel]],
                  y = as.numeric(dataset[[ylabel]]),
                  p = dataset[[plabel]])
  
  # Simple SD
  #df = df %>% group_by(x) %>% summarise(mean = mean(y), sd=sd(y))
  
  if (is.na(symbol)) {
    df$sym = "1"
  } else {
    df$sym = dataset[[symbol]]
  }
  
  df = df %>% mutate(
    x_grp = as.character(x),
    x = as.numeric(as.factor(x)),
    x_jit = x #jitter(x, amount=0.1)
  )
  #browser()
  df = df %>% beeswarm_values(x_col = "x_jit", y_col = "y",xRange=2,yRange=maxy-miny,cex=jit)
  
  #save(df, file = 'data_hk_95.rda', compress=TRUE)
  
  #browser()
  # 95% Confidence intervals
  
  df_error = df %>%
    group_by(p,x) %>%
    summarise(y = mean(y)) %>%
    group_by(x) %>%
    summarise(mean.y = mean(y, na.rm = TRUE),
              sd.y = sd(y, na.rm = TRUE),
              n.y = n()) %>%
    mutate(se.y = sd.y / sqrt(n.y),
           lower.ci.y = mean.y - qt(1 - (0.05 / 2), n.y - 1) * se.y,
           upper.ci.y = mean.y + qt(1 - (0.05 / 2), n.y - 1) * se.y,
           x = as.numeric(as.factor(x)))
  
  fig_c = fig %>%
    add_trace(data=df,
              x=~x_jit, y=~y, color=I("rgba(0,0,0,0.2)"), symbol=~sym, type='scatter',mode='markers') %>%
    add_trace(data=df_error,
              x=~x, y=df_error$mean.y, color=I("black"), type='scatter',mode="markers",
              marker = list(color = I("black"), size = 7),
              error_y=~list(symmetric=FALSE, array=df_error$upper.ci.y - df_error$mean.y, arrayminus=df_error$mean.y-df_error$lower.ci.y, color = '#000000')) %>%
    #add_trace(data=df_error,
    #x=~x, y=(df_error$mean.y/2)-df_error$lower.ci.y, color=I("black"), type='scatter', mode = 'text', text = ~paste0(format(round(lower.ci.y,0), nsmall = 0),'  '), textposition = 'middle right',
    #textfont = list(color = I("black"), size = 15)) %>%
    layout(margin=list(l=5,r=0,t=55,b=0),title=list(font=list(size=15), xanchor="center", xref="paper",
                                                    text=desc), showlegend=F,
           xaxis=list(tickmode='array',tickvals=unique(df$x), ticktext=unique(df$x_grp),linewidth=1, range=c(0.45,max(df$x) + 0.45), title=xtitle, zeroline=F, tickfont=list(size=15)),
           yaxis=list(zeroline=T, linewidth=1, range=c(miny,maxy), title=ytitle, zeroline=F, tickfont=list(size=13), showticklabels=T))
  fig_c
  
  return(fig_c)
}
