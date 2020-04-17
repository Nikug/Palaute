helpUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 6, offset = 3,
        tags$h1("Palaute"),
        tags$p("Plot, Analyze, Learn And Understand Topic Emotions"),
        br(),
        tags$h1("About"),
        tags$p("The source code for this project can be found in:"),
        tags$a("https://github.com/Nikug/course-analysis", href = "https://github.com/Nikug/course-analysis"),
        br(),
        br(),
        tags$p("This artefact is part of a master's thesis. The thesis can be found from:"),
        tags$a("A link to the thesis", href = "#"),
        br(),
        br(),
        tags$p("This software uses a combination of topic modeling and sentiment/emotion analysis in order to
               analyze text data and find patterns in it."),
        tags$p("Topic modeling is done using Structural topic model (STM) algorithm by Roberts et al. (2013).
               More information about the algorithm can be found in their article,
               the vignette for the R package and their website:"),
        tags$a("STM Journal article",
               href = "https://scholar.princeton.edu/files/bstewart/files/stmnips2013.pdf"),
        br(),
        tags$a("Structural topic model R package vignette",
               href = "https://cran.r-project.org/web/packages/stm/vignettes/stmVignette.pdf"),
        br(),
        tags$a("https://www.structuraltopicmodel.com/", href = "https://www.structuraltopicmodel.com/"),
        br(),
        br(),
        tags$p("Sentiment and emotion analysis is done using Syuzhet R package by Jockers (2015)
               using NRC emotion lexicon by Mohammad & Turney (2013):"),
        tags$a("https://github.com/mjockers/syuzhet", href = "https://github.com/mjockers/syuzhet"),
        br(),
        tags$a("NRC Journal article", href = "https://arxiv.org/pdf/1308.6242.pdf"),
        br(),
        tags$a("NRC website", href = "https://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm"),
        hr(),
        br(),
        tags$h1("How to use"),
        tags$p("The program has a following workflow:"),
        tags$li("Upload a .csv file"),
        tags$li("Remap the data"),
        tags$li("Start the analysis"),
        tags$li("Observe the results in summary view"),
        tags$li("Observe topic specific results in the details view"),
        br(),
        hr(),
        br(),
        tags$h1("Main"),
        tags$h3("Uploading a file"),
        tags$p("Files can be uploaded in the Main tab. The files need to be in comma-separated value (csv) format.",
               tags$strong("Only UTF-8 encoding is supported."), "Other file encodings might have missing/broken characters.",
               tags$li(tags$strong("CSV delimiter"), "can be used to select the delimiter character."),
               tags$li(tags$strong("Header"), "can be used to select whether the file contains a header line or not.")),
      br(),
      tags$h3("Remapping the data"),
      tags$p("This part allows the user to select the columns from the original data and how they will be used for the analysis.
             First, the structure of the original file is shown. Then there are dropdowns for selecting how the data is used
             to build a new data structure. Finally, there is a preview of the new file structure.",
             br(),
             "Each of the sections has a", tags$strong("Hide"), "button that collapses the section.
             This can be useful when dealing with data with a lot of columns. Other controls:",
             tags$li(tags$strong("Show rows"), "controls how many rows from the data are shown."),
             tags$li(tags$strong("Header length"), "controls how many characters are shown for each header."),
             tags$li(tags$strong("Build the mapped file"), "When this is selected, output data is automatically rebuild
                     whenever the data or column mappings change. When not selected, output data is not build
                     until this is selected again. When dealing with large datasets it can be useful to turn
                     this off until the mappings are completed, since each rebuild can take some time. This is
                     automatically turned off whenever a dataset of more than", DefaultRemap$automaticBuildLimit,
                     "rows is uploaded."),
             tags$li(tags$strong("Download as CSV"), "allows downloading the output data as a CSV file.")
             ),
      tags$br(),
      tags$p("The controls for mapping the columns are generated each time the input data changes. The software tries to guess
             what kind of data each column contains and creates controls divided to text and numeric respectively.
             By default all text columns that contain varying data are included as documents and two prevalence covariates
             are selected from the numeric columns.
             There are four possible use cases for each column:",
             tags$li(tags$strong("Document"), "These are the text documents that are used in the topic modeling algorithm"),
             tags$li(tags$strong("Prevalence covariate"), "This data is used as a covariate on the frequency on how often a topic
                     is discussed. There can be multiple prevalence covariates and they don't tend to slow down the algoritm"),
             tags$li(tags$strong("Content covariate"), "These affect how each topic is discussed.",
                     tags$strong("There can be at a maximum only one content covariate."),
                     "If multiple content covariates are selected, only the first one is used. Selecting a content covariate
                     significantly slows down the algorithm and they should be only used if you know what you are doing.
                     Calculating the amount of topics does not support content covariates, and they are silently not used
                     when the option to calculate the amount of topics is selected."),
             tags$li(tags$strong("Don't include"), "These columns are excluded from the output data.")
             ),
      br(),
      tags$h3("Analysis options"),
      tags$p("These settings control the strucutal topic model algorithm.
             By default there are pretty useful options selected. For more in-depth data analysis",
             tags$strong("Use default options"), "can be deselected to show more options."),
      br(),
      tags$p("Text preprocessing steps and sentiment analysis are dependant on the language of the data, so",
             tags$strong("Data language"), "should be set to the language of the data. Currenlty only Finnish and English
             are supported. Data sets should only contain a single language as multiple languages will confuse the topic
             modeling algorithm and sentiment analysis is done only on the lexicon of the selected language."
             ),
      br(),
      tags$p(tags$strong("Options preview"), "shows the selected options.",
             br(),
             tags$strong("Data preview"), "shows information about the data that will be analyzed."),
      br(),
      tags$p("Options:",
             tags$li(tags$strong("Use sampling"), "When selected, the data will be randomly sampled to only
                     include the number of documents selected from", tags$strong("Sample size"), "input."),
             tags$li(tags$strong("Topic count"), "The topic modeling algorithm will find this amount of topics from the data"),
             tags$li(tags$strong("Calculate the number of topics"), "When selecting this option, all the topic counts between",
                     tags$strong("Search start"), "and", tags$strong("Search end"), "will be tested and the mathematically
                     best topic count is selected. In addition to that, each model is ran", STM$runs, "times to select the
                     best initalization.", tags$strong("This will take a long time."), "The best model is selected as 
                     maximum mean of normalized values of model semantic coherence and exclusivity."),
             tags$li(tags$strong("maximum iterations"), "Selects the maximum amount of iterations the model is ran. 
                     It is possible for the model to converge before this limit is reached, but if the model does not
                     converge, it is stopped after this amount of iterations."),
             tags$li(tags$strong("Start analysis"), "Starts performing the analysis. A progress bar shows up in the bottom-right
                     corner of the window while analysis is ongoing.")
             ),
      br(),
      hr(),
      br(),
      tags$h3("Summary"),
      tags$p("Summary view contains information about the topic model and different graphs about the results."),
      tags$p(tags$strong("Topid distance map"), "and", tags$strong("Topic-document relation"), "graphs are interactive.
             Clicking on the center of the points shows additional information in a box under the graph."),
      br(),
      hr(),
      br(),
      tags$h3("Details"),
      tags$p("Details view contains information about individual topics. There are:"),
      tags$li("Topic keywords using four different methods."),
      tags$li("Example documents"),
      tags$li("Topic sentiment"),
      tags$li("Topic emotions"),
      br(),
      br(),
      tags$p("Every topic can be named by the user using the", tags$strong("Topic name"), "text box. This has no
             function other than it can make analysing the data easier when the interpreted topic names can be written down."),
      br(),
      tags$p("Each of these panels can be hidden using the filter. In addition to that the emotions can be ordered to descending
             order or alphabetical order using", tags$strong("Sort emotion analysis."), "The amount of shown keywords
             can be controlled using", tags$strong("Number of keywords."), "Same can be done for the top documents
             using", tags$strong("Number of top documents."), "Finally, each topic can be collapsed using the",
             tags$strong("Hide"), "button."),
      br(),
      tags$p("Sentiment and emotions are calculated on exclusive documents. This means that a highest correlating topic
             is selected for each document and the sentiment/emotion analysis is run solely on those documents.
             The top documents are the documents with the highest correlation to the topic. These are not necessarily
             the same documents that the sentiment/emotion analysis is performed on."),
      br(),
      hr(),
      br(),
      tags$h3("Help"),
      tags$p("This page contains a short help."),
      br(),
      hr(),
      br()
      )
    )
  )
}

helpUIFunction <- function(input, output, session) {
  
}