###############################################################################
###############################################################################
###############################################################################

## decision-making flowchart tab ----------------------------------------------

tab_flowchart <- tabPanel(
    
    title = "Decision-making flowchart",
    
    value = "tab_flowchart",
    
    h1("Decision-making flowchart"),

    p(HTML(
        "This flowchart serves as a practical guide for selecting appropriate statistical methods based on the ",
        "characteristics of your dataset. Starting from the type of data you have (nominal, ordinal, or metric), ",
        "the diagram helps you navigate through key decision points such as distribution assumptions, the number ",
        "of samples being compared, and whether those samples are dependent or independent."
    )),

    p(HTML("By following the decision paths, users can quickly identify which statistical test may be most ",
        "suitable for their analysis, ranging from parametric approaches (e.g., <em>t</em>-tests, ANOVA) to ",
        "nonparametric alternatives (e.g., Mann-Whitney, Kruskal-Wallis, Wilcoxon tests). The flowchart is ",
        "designed as an intuitive entry point, especially for those less experienced in statistics, providing ",
        "a visual decision-making aid before moving on to detailed analysis."
    )),

    tags$img(src = "images/flowchart.svg", class = "flowchart"),
)


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################
