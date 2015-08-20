# PISA_PAPER
# Repository includes Latex source code, image and pdf inline files, R scripts and data and     # related material
RDATA folder -> all saved data
RSCRIPTS folder -> all R scripts naming convention - Any output files (eg pdf) to follow input file name with suffic OUT eg STUDTAB3.R may generate output, call it STUDTAB3a.pdf for first output, STUDTAB3b.pdf for second otput and so on.

 DATA GENERATION names:  Use what we have eg PISA2012_2a. R etc
 PAPER related names : Four or five descriptive letters of Paper Section name eg INTR STUD TEACH
                       Three letter if it relates to FIG for Figure and TAB for Table FAB for                          both. Serial number in chronological order of making the file: Examples:
                       INTRFIG1.R or STUDTAB3.R
 FEATURE related names: Features can be descriptives, particular regressions eg. FRYER-LEVITT,
                        Oaxaca-Blinder, Initial Regression
                        PREL as suffix, 4 letter acronym for feature eg FRYR OXBL and series 1
                        plus letters a b c d etc. for specification attempts that will
                        eventually be moved to PAPER: Examples: Preliminary Descriptive Tables 
                        with mean comparison of students would be PRELDES1a; Another  file on 
                        students would be PRELDES1b, but one on Teachers would be PRELDES2a
LATEX folder -> Latex file names follow R script that generated their data eg INTRFIG1.tex and 
                        any generated pdf files; Non R related TEX files eg TikZ will say the 
                        application used Tikz1a.tex etc. 
                        
EXCEL folder -> for any EXCEL data files - just arbitrary names
WINSTEPS folder -> arbitrary names for now

                        
                    
