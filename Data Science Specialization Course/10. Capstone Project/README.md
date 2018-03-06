## Data Science Specialization Course : Capstone Project.
**Summary:**

The assignment tasked students to create a probabilistic English language model, capable of predicting words based on previous context. The model itself was trained on a corpus consisting of blog posts, online newspaper articles and twitter feeds. The basic model was smoothed by Kneser-Ney and pruned by Seymore-Rosenfeld methods. 

In addition, a simple Shiny application was built to showcase the model's capabilities. The end result is a 5-gram model, which recognizes over 40,000 words and 12,000,000 word combinations, is able to handle both unknown words and unknown combinations of words, and has a perplexity score of 253.

**Contents:**
* Full project write-up is available as a "Capstone_Project_Report.pdf"
* Raw R markdown version of the project is included as "Capstone_Project_Report.Rmd"
* Latex template and Biblatex bibliography for the R markdown version are "svm-latex-ms.tex" and "biblio.bib"
* R code for the language model is found in "raw-tidy.R"
* R code for the Shiny app if is found in "server.R" and "ui.R"
