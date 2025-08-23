###############################################################################
###############################################################################
###############################################################################

## AI wizard tab --------------------------------------------------------------

tab_ai_insight <- tabPanel(
    
    title = "AI insight",
    
    value = "tab_ai_insight",
    
    h1("AI insight (experimental)"),
    
    HTML(
      "<p><i><strong>Note:</strong> The analysis and insights provided below are based on a <strong>limited sample</strong> of the dataset, not the full data. 
      As such, some results may be indicative rather than definitive, and certain patterns or anomalies present in the complete dataset may not be captured here. 
      This summary is intended to offer a <strong>preliminary view</strong> of the data, highlight potential trends, and suggest possible directions for further exploration or more detailed statistical investigation.</i></p>
      
      <p><i>Users should interpret the findings with caution and consider them as <strong>guiding insights</strong> rather than absolute conclusions. 
      The purpose of this overview is to provide a starting point for understanding the structure, composition, and key characteristics of the dataset, 
      helping users to formulate hypotheses, plan additional analyses, or identify areas requiring deeper examination. 
      Remember that real-world data can contain inconsistencies, missing values, or outliers that may not be fully represented in a small sample, 
      so any decisions based solely on this preview should be corroborated with the complete data.</i></p>"
    ),
    
    withSpinner(htmlOutput("ai_response"), size = 2, type = 4, color = "#53b3eb")
)

## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################
