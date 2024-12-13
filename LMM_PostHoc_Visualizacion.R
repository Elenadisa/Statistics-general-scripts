#! /usr/bin/env Rscript

#@description perform linear mixed model analysis and a post-hoc.
#@author Elena D. Díaz Santiago

###############################################################################
#                                 LOAD PACKAGES                               #
###############################################################################

library(ggplot2)
library(optparse)
library(dplyr)
library(lme4)
library(multcomp)
library(emmeans)

###############################################################################
#                                 OPTPARSE                                    #
###############################################################################
option_list <- list(
  make_option(c("-i", "--input"), type="character",
              help="Input Dataframe"),
  make_option(c("-m", "--main_variable"), type="character",
              help="variable to test"),
  make_option(c("-f", "--formula"), type="character",
              help="formula with fixed and random variables for LMM"),
  make_option(c("-p", "--posthoc_method"), type="character",
              help="posthoc method"),
  make_option(c("-P", "--posthoc_formula"), type="character",
              help="posthoc method"),
  make_option(c("-o", "--barplot"), type="character",
              help="barplot name"),
  make_option(c("-x", "--x_axis"), type="character",
              help="x axis variable"),
  make_option(c("-y", "--ylabel"), type="character",
              help="text for y axis lab"),
  make_option(c("-c", "--color_var"), type="character",
              help="variable to set the color")
)

opt <- parse_args(OptionParser(option_list=option_list))

###############################################################################
#                                 FUNCTIONS                                   #
###############################################################################

create_barplot <- function(df, x_var, col_var, y_lab){
  
  plt <- ggplot(df , aes(x = get(x_var), y = emmean, fill=get(col_var)))  + 
    geom_bar(colour="black", stat="identity", position=position_dodge()) +
    #barss with standard error
    geom_errorbar(aes(ymin=emmean, ymax=emmean+SE), linewidth = 1, width=0.2, position=position_dodge(.9)) +
    #black and white colors
    scale_fill_grey(start=0, end=1) +
    #eliminate backgroud grid
    theme_classic() +
    #specify axis appearance
    ylab(y_lab) +
    theme(axis.title=element_text(size=14,face="bold"),
          axis.text=element_text(size=12),
          legend.text=element_text(size=12),
          axis.title.x=element_blank(),
          legend.title=element_blank()) +
    #add posthoc sifnificance letters
    geom_text(mapping = aes(label = .group, y = upper.CL * 1, size=12), 
              position = position_dodge(width = 0.9), show.legend = F, fontface = "bold")
  
  return(plt)
}

###############################################################################
#                                 MAIN                                        #
###############################################################################
#load dataframe
df <- read.table(opt$input, header = TRUE, stringsAsFactors = FALSE, sep="\t")

#get LMM formula 
main_variable <- opt$main_variable
f <- formula(paste(main_variable , opt$formula))

#perform LMM analysis
model <- lme4::lmer(formula = f, data = df)

cat("LMM Analysis\n\n")
summary(model)

#PERFORM POST HOC

#obtain post hoc fomula
p <- formula(opt$posthoc_formula)

posthoc <- emmeans(model, p, adjust=opt$posthoc_method)
cat("\nPost-hoc\n\n")
summary(posthoc)

#obtain post hoc significance letters
df.lmm.posthoc <- data.frame(cld(posthoc$emmeans, Letters = LETTERS))

#create plot file
png(filename=opt$barplot, units="in", width=6, height=6, res=300)
create_barplot(df.lmm.posthoc, opt$x_axis, opt$color_var, opt$ylabel)
dev.off()

