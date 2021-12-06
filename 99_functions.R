# 99_functions.R
# functions for career disruption survey
# October 2021

# make version of cipher function that allows NAs
caesar_na = function(text){
  if(is.na(text) == TRUE){
    encrypted = ''
    return(encrypted)
  }
  encrypted = caesar(text, shift = 4, decrypt = FALSE)
  return(encrypted)
}

# to replace missing with zero
replace_zero = function(x){replace_na(x, '0')}

# date formating
my.date.format = function(x){y= format(x, "%d %b %Y"); return(y)} 

# function for rounding numbers with zeros kept
roundz = function(x, digits=0){
  dformat = paste('%.', digits, 'f', sep='')
  x = sprintf(dformat, round(x, digits))
  return(x)
}

# from https://rstudio-pubs-static.s3.amazonaws.com/408658_512da947714740b99253228f084a08a9.html
# Capitalize the first letter in a word string in R
CapStr <- function(y) {
  c <- strsplit(y, " ")[[1]]
  paste(toupper(substring(c, 1,1)), substring(c, 2),
        sep="", collapse=" ")
}


## Make a nice table (single variable version) ##
make_table = function(indata,  # survey data
                      inlabels,  # survey labels
                      add_nhmrc = FALSE, # add data from NHMRC to random sample
                      include.missing = TRUE, # include missing or not
                      ordered = FALSE, # use defined ordering for categories
                      label, # label to use
                      bar_type = 'percent', # plot numbers or percent
                      add_ci = FALSE, # give 95% confidence interval for random sample
                      ltype = 'label', # is the above label for the 'question' or the 'label'
                      digits = 0, # digits for continuous
                      wrap_labels = NULL, # add carriage returns to labels in plot
                      stack = TRUE, # stack style plot or side by side?
                      rel_widths = c(1.4, 1), # relative width for pyramid plots
                      sample_colours = sample_colours, # colours for random and non-random sample
                      type = 'categorical'){
  
  # select the variable and rename
  if(ltype == 'label'){
    var = filter(inlabels, labels == label) %>% pull(names)
  }
  if(ltype == 'question'){
    var = label
  }
  n = names(indata)
  names(indata)[n == var] = 'var'
  
  # remove missing or ...
  if(include.missing == FALSE){
    indata = filter(indata, !is.na(var))
  }
  # ... make missing into a category
  if(include.missing == TRUE){
    indata = mutate(indata,
                    var = ifelse(is.na(var), 'Missing', var))
  }
  
  # ordering
  if(ordered == TRUE){
    olabels = filter(ordering, question == label)
    any.missing = any(indata$var == 'Missing')
    if(include.missing == FALSE | any.missing == FALSE){
      olabels = filter(olabels, var != 'Missing')
    }
    indata = mutate(indata, 
                    var = factor(var, levels = olabels$var))
  }
  
  ### categorical ###
  if(type == 'categorical'){
    
    ### a) table
    counts = group_by(indata, sample, var) %>%
      tally() 
    # add total
    total = group_by(indata, var) %>%
      tally() %>%
      mutate(sample = 'Total')
    tab = bind_rows(counts, total) %>%
      group_by(sample) %>%
      mutate(
        var = as.character(var),
        var = ifelse(is.na(var)==TRUE, 'Missing', var), # missing response
        percent = prop.table(n),
        percent = roundz(percent*100),
        cell = paste(n, ' (', percent, ')', sep='')) %>%
      dplyr::select(-n, -percent) %>%
      pivot_wider(values_from = 'cell', names_from = 'sample') %>%
      mutate_if(is.character, replace_zero) # replace missing with zero
    # rename column header
    names(tab)[1] = 'Response' 
    #
    ftab = flextable(tab) %>%
      theme_box() %>%
      autofit()
    
    ### b) back-to-back bar plot
    # create numbers and percents
    for_plot = group_by(counts, sample) %>%
      mutate(p = prop.table(n)*100)
    if(bar_type == 'percent'){
      for_plot = mutate(for_plot, y=p)
      ylab = 'Percent'
    }
    if(bar_type == 'number'){
      for_plot = mutate(for_plot, y=n)
      ylab = 'Frequency'
    }
    # wrap labels
    if(is.null(wrap_labels) == FALSE){for_plot = mutate(for_plot, var = str_wrap(var, width = wrap_labels))}
    #
    plot_random = ggplot(data=filter(for_plot, sample=='Random'), aes(x=var, y=y, fill=var))+
      g.theme+
      scale_y_reverse()+ # reverse order
      theme(legend.position = 'none')+
      ylab(ylab)+
      xlab('')+
      coord_flip(clip = 'off')+
      facet_wrap(~sample)+
      theme(plot.margin = margin(0, 2, 0, 0, "mm") ) # t, r, b, l
    #
    plot_nonrandom = ggplot(data=filter(for_plot, sample!='Random'), aes(x=var, y=y, fill=var))+
      g.theme+
      scale_x_discrete(labels=NULL)+ # turn off labels
      theme(legend.position = 'none')+
      ylab(ylab)+
      xlab('')+
      coord_flip(clip = 'off')+
      facet_wrap(~sample)+
      theme(plot.margin = margin(0, 2, 0, -5, "mm") ) # t, r, b, l; reduce gaps with negative
    # version with numbers
    if(stack == FALSE){
      plot_random = plot_random + geom_bar(stat='identity', fill = sample_colours[1])
      plot_nonrandom = plot_nonrandom + geom_bar(stat='identity', fill = sample_colours[2])
    }
    if(stack == TRUE){
      plot_random = plot_random + 
        geom_bar(stat='identity') + 
        scale_fill_manual(NULL, values = cbPalette)
      plot_nonrandom = plot_nonrandom + geom_bar(stat='identity') + scale_fill_manual(NULL, values = cbPalette)
    }
    # add NHMRC results to random sample (optional)
    if(add_nhmrc == TRUE){
      to_add = filter(nhmrc, type == label)
      if(is.null(wrap_labels) == FALSE){to_add = mutate(to_add, var = str_wrap(var, width = wrap_labels))}
      plot_random = plot_random + geom_point(data=to_add, aes(x=var, y=p), pch=16, size=3, col='black')
    }
    #
    if(ordered == TRUE){
      olabels = rev(olabels$var)
      if(is.null(wrap_labels) == FALSE){olabels = str_wrap(olabels, width = wrap_labels)}
      plot_random = plot_random + 
        scale_x_discrete(limits = olabels)  # Missing at bottom
      plot_nonrandom = plot_nonrandom + 
        scale_x_discrete(limits = olabels, labels=NULL)  # Missing at bottom
    }
    
    # use consistent upper limit for axes
    y.max.1 = max(abs(ggplot_build(plot_random)$layout$panel_params[[1]]$x.range))
    y.max.2 = max(abs(ggplot_build(plot_nonrandom)$layout$panel_params[[1]]$x.range))
    y.max = max(y.max.1, y.max.2)
    # 
    if(bar_type == 'number'){
      plot_random = plot_random + scale_y_reverse(limits=c(y.max, 0))
      plot_nonrandom = plot_nonrandom + scale_y_continuous(limits=c(0, y.max))
    }
    if(bar_type == 'percent'){ # add % to axis labels
      plot_random = plot_random + scale_y_reverse(limits=c(y.max, 0), labels = scales::percent_format(scale=1))
      plot_nonrandom = plot_nonrandom + scale_y_continuous(limits=c(0, y.max),labels = scales::percent_format(scale=1))
    }
    
    plot = plot_grid(plot_random, plot_nonrandom, nrow=1, ncol=2, rel_widths = rel_widths)
    
  }

  ### continuous ###
  if(type == 'continuous'){
    
    ## a) table
    tab = group_by(indata, sample) %>%
      summarise(median = roundz(quantile(var, 0.5, na.rm=TRUE), digits),
                lower = roundz(quantile(var, 0.25, na.rm=TRUE), digits),
                upper = roundz(quantile(var, 0.75, na.rm=TRUE), digits)) 
    # add total
    total = summarise(indata, 
                median = roundz(quantile(var, 0.5, na.rm=TRUE), digits),
                lower = roundz(quantile(var, 0.25, na.rm=TRUE), digits),
                upper = roundz(quantile(var, 0.75, na.rm=TRUE), digits)) %>%
      mutate(sample = 'Total')
    # 
    tab = bind_rows(tab, total) %>%
      mutate(cell = paste(median, ' (', lower, ' to ', upper, ')', sep='')) %>%
      dplyr::select(sample, cell) %>%
      rename('Median (IQR)' = 'cell')
    names(tab)[2] = 'Median (IQR)' # rename column header
    ftab = flextable(tab) %>%
      theme_box() %>%
      autofit()
    
    ## b) box plot
    ylab = filter(inlabels, names==label) %>% pull(labels)
    plot = ggplot(data=indata, aes(x=factor(sample), y=var, fill=sample))+
      geom_boxplot()+
      scale_fill_manual(NULL, labels=c('Non-random','Random'), values=sample_colours)+
      xlab('')+
      ylab(ylab)+
      g.theme+
      theme(legend.position = 'none')+
      coord_flip()
    
    # null plots needed for saving
    plot_random = plot_nonrandom = NULL
  }
  
  ## confidence interval for random sample
  ftab_ci = NULL
  if(add_ci == TRUE){
    counts = filter(indata, 
                    var != 'Missing', # exclude missing for this
                    sample=='Random') %>%
      group_by(var) %>%
      tally() %>%
      mutate(p = n / sum(n))
    intervals = multinomialCI(counts$n, alpha=0.05)
    cis_raw = bind_cols(counts, as.data.frame(intervals))
    names(cis_raw)[1] = c('Response')
    names(cis_raw)[4:5] = c('lower','upper')
    ftab_ci = mutate(cis_raw,
                     p = roundz(100*p, digits), # turn into rounded percents
                     lower = roundz(100*lower, digits),
                     upper = roundz(100*upper, digits)) %>%
      flextable() %>%
      theme_box() %>%
      autofit()
  }
  
  # return
  return = list()
  return$table = ftab
  return$plot = plot
  return$plot_random = plot_random # also save individual plots
  return$plot_nonrandom = plot_nonrandom
  return$ci = ftab_ci
  return(return)
      
}


## export levels for table/graph ordering ##
export_levels = function(indata, inlabels, questions){
  for_export = NULL
  n = names(indata)
  for (q in questions){
    #
    temp_data = indata
    names(temp_data)[n == q] = 'var'
    #
    f = dplyr::select(temp_data, var) %>%
      unique() %>%
      mutate(question = q)
    for_export = bind_rows(for_export, f)
  }
  for_export = dplyr::select(for_export, question, var) %>%
    mutate(var = ifelse(is.na(var)==TRUE, 'Missing', var))
  return(for_export)
}


## Make a nice table (multiple variable version) ##
make_table_matrix = function(indata,  # survey data
                      inlabels,  # survey labels
                      legend_order, # for ordering results to match bars
                      bar_colours = c('dark red','grey','dark blue'),
                      include.missing = TRUE, # include missing or not
                      bar_type = 'percent', # plot numbers or percent
                      ordered = FALSE, # use defined ordering for response categories
                      ordered_row = 'Agree', # order rows by frequency of this response
                      remove = '', # text to remove from labels
                      wrap_labels = 40, # use carriage returns to shorten labels in the plot
                      start, # start of the variable name
                      expand_zero = FALSE # use expand on the x-axis to reduce white space
                      ){
  
  # select the variables and put in long format
  long = dplyr::select(indata, 'sample', starts_with(start)) %>%
    pivot_longer(cols = starts_with(start))

  # remove missing or ...
  if(include.missing == FALSE){
    long = filter(long, !is.na(value))
  }
  # ... make missing into a category
  if(include.missing == TRUE){
    long = mutate(long,
                  value = ifelse(is.na(value), 'Missing', value))
  }

  # get the question labels and remove repeated opening text
  qlabels = filter(inlabels, str_detect(names, pattern=start)) %>% 
    mutate(labels = str_remove(labels, pattern=remove)) %>%
    rename('name' = 'names')
    
  # ordering of responses
  if(ordered == TRUE){
    olabels = filter(ordering, question == start)
    any.missing = any(long$value == 'Missing')
    if(include.missing == FALSE | any.missing == FALSE){
      olabels = filter(olabels, var != 'Missing')
    }
    long = mutate(long, 
                  value = factor(value, levels = olabels$var))
  }
  
    ### a) table
    counts = group_by(long, sample, name, value) %>%
      tally() 
    tab = group_by(counts, sample, name) %>%
      mutate(
        percent = prop.table(n),
        percent = roundz(percent*100),
        cell = paste(n, ' (', percent, ')', sep='')) %>%
      dplyr::select(-n, -percent) %>%
      pivot_wider(values_from = 'cell', names_from = 'sample') %>%
      ungroup() %>%
      mutate_if(is.character, replace_zero) # replace missing with zero
    # order rows by frequency
    if(is.null(ordered_row) == FALSE){
      order = filter(long, value == ordered_row) %>%
        group_by(name) %>%
        tally() %>%
        arrange(-n) %>% # from high to low
        dplyr::select(-n) %>%
        mutate(rown = 1:n())
      counts = left_join(counts, order, by='name') %>%
        arrange(rown, value)
      tab = left_join(tab, order, by='name') %>%
        arrange(rown, value)
      # add ordering to plot data too
      long = left_join(long, order, by='name')
    }
    tab = left_join(tab, qlabels, by='name') %>% # add long labels
      dplyr::select(labels, everything(), -name) # re-order columns
    # rename column header
    names(tab)[1:2] = c('Question','Response')
    #
    ftab = dplyr::select(tab, -rown) %>%
      flextable() %>%
      theme_box() %>%
      merge_v(j=1) %>%
      autofit()
    
    # breaks in labels - wrap labels
    if(is.null(wrap_labels) == FALSE){
      qlabels = mutate(qlabels,
                       labels = str_wrap(labels, width = wrap_labels))
    }
    
    ### b) bar plot
    # create numbers and percents
    for_plot = group_by(counts, sample, name) %>%
      mutate(p = prop.table(n)*100)
    if(bar_type == 'percent'){
      for_plot = mutate(for_plot, y=p)
      ylab = 'Percent'
    }
    if(bar_type == 'number'){
      for_plot = mutate(for_plot, y=n)
      ylab = 'Frequency'
    }
    
    # for ordering facets
    olabels = c('Random','Non-random')
    for_plot = mutate(for_plot,
                      sampleo = ifelse(sample=='Random', 1, 2),
                      sampleo = factor(sampleo, levels=1:2, labels=olabels) )
    
    if(is.null(ordered_row)==TRUE){
      plot = ggplot(data=for_plot, aes(x=name, y=y, fill=value))+
        scale_x_discrete(labels=qlabels$labels, expand=c(0,0))
    }
    if(is.null(ordered_row)==FALSE){
      olabels = dplyr::select(tab, rown, Question) %>%
        unique()
      if(is.null(wrap_labels)==FALSE){
        olabels = mutate(olabels,
          Question = str_wrap(Question, width=wrap_labels))
      }
      plot = ggplot(data=for_plot, aes(x=rown, y=y, fill=value))+
        scale_x_continuous(breaks = 1:nrow(olabels), labels=olabels$Question, expand=c(0,0))
    }
    
    #
    plot = plot + geom_bar(position='stack', stat='identity')+
      scale_fill_manual(NULL, breaks = legend_order, values = bar_colours)+ # add breaks for ordering
      g.theme+
      scale_y_continuous(expand=c(0,0), labels = scales::percent_format(scale=1))+ # remove gap; use %
      theme(plot.margin = margin(0, 4, -5, 0, "mm"), # t, r, b, l; reduce space at bottom as there's no axis label
            axis.text.x = element_text(size=7), # smaller text due to cramped percents
            panel.spacing = unit(5, "mm"), # increase space between panels to avoid overlap in % labels
            legend.position = 'top',
            legend.box.spacing = unit(0, 'mm'), # reduce space between plot and legend
            legend.box.margin	= margin(t=0, r=0, b=0, l=0), # reduce space around legend
            legend.margin = margin(t=0, r=0, b=0, l=0, unit='mm'))+ # reduce space around legend
      ylab('')+
      xlab('')+
      coord_flip()+
      facet_wrap(~sampleo, scales='free_x')

  # return
  return = list()
  return$table = ftab
  return$plot = plot
  return(return)
  
}


## Make a list of comments
# add gender and years of experience to each comment
comments = function(indata,  # survey data
                    question, # label to use
                    plus_label = NULL) # additional label for comments
{
  
  # select the variable and rename
  n = names(indata)
  names(indata)[n == question] = 'var'
  # ditto for plus label
  if(is.null(plus_label) == FALSE){
    names(indata)[n == plus_label] = 'label'
  }
  
  non_missing = filter(indata, 
                       !is.na(var),
                       tolower(var) != 'no' # remove this text
                       ) %>%
    #select(sample, q9_2, var) %>% # q9_2 is gender
    arrange(sample, q9_2) # group together
  comments = NULL
  for (k in 1:nrow(non_missing)){
    if(is.null(plus_label)==TRUE){
      comment = paste('* (', non_missing$q9_2[k] , ', ', non_missing$q9_3[k], ') ', non_missing$var[k], '\n', sep='')
    }
    if(is.null(plus_label)==FALSE){
      comment = paste('* (', non_missing$q9_2[k] , ', ', non_missing$q9_3[k], ', ', non_missing$label[k], ') ', non_missing$var[k], '\n', sep='')
    }
    comments = c(comments, comment)
  }
  return(comments)
}
