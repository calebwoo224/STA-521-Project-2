# STA 521 Fall 2022 Project 2

#### By Caleb Woo and Eli Gnesin 

#### Updated December 6, 2022

This directory contains all the necessary files for recreating our 2nd project, on classifying cloud cover in Arctic images. The following files are contained in this folder:

1.  `project-2.rmd`: The R-markdown file used for the project. The first code chunk loads in all the necessary packages and sets global options that the code chunks should evaluate silently (without messages or warnings), should not appear in the PDF, and should produce their output. Every code chunk is set to evaluate except `hyperest-ntree-lr`, which can be set to evaluate for future use (evaluating this code chunk takes \~10 hours).
2.  `parameter_estimation.csv`: This CSV is the output file for the code chunk `hyperest-ntree-lr`. The Rmd file writes that chunk to this file, then immediately reads from this file in the next code chunk. If using new data, then `hyperest-ntree-lr` will be rerun, and will overwrite this CSV. This data is used for the parameter estimation plots in part 4(a).
3.  `proj2functions`: A primitative R package containing the three functions used in this project. Each function is also in this folder in an independent R file, and the contents of the functions are the same (but the package also includes documentation). The functions are:
    -   `block_split.R`: This function takes in training data and labels and splits each image into $k \times k$ blocks, which can then be shuffled for crossvalidation and separated into train, validation, and test subsets.
    -   `image_split.R`: This function takes in training data and labels and splits the dataset into $k$ total blocks, and returns the training, validation, and test subsets of the dataset separately.
    -   `CVmaster.R`: This function takes in training data, training labels, a generic classifier name, a number of folds $K$, and a split type, and returns the k-fold cross-validation error, average cross-validation error, and test error for the trained classifier on the data split using one of the above split functions. Other arguments include a probability threshold for binary classification, a random seed for reproducibility, a prediction type argument, a formula argument (to allow for some classifiers that do not take formulas), a loss function (only misclassification error is used), and other arguments that can be passed forward into the classifier for parameter tuning or optimization. The function also returns a ROC object on the test data, the predictions on each cross-validated fold and on the test set, and the true labels for the test set.
4.  `references.bib`: The list of references for this project (in this case, only the article).
5.  `yu2008.pdf`: The source article for the project.

To reproduce the paper from scratch:

1.   Open the Rmd file to check that it works.

2.  If you would like to use the functions directly in the folder (as opposed to the package), uncomment the three lines in the `setup-packages` chunk that source the three functions and comment out the `library(proj2functions)` line in that same code chunk.

3.   Set `eval=True` in the code chunk label for the `hyperest-ntree-lr` chunk to be able to rerun the parameter grid estimation code.

4.  Go through and set the desired seed in all of the "set.seed" functions as well as an argument in every CVmaster function (the default is 521, which was the seed we used to produce the paper).

5.  Knit the file! This will take about 20 minutes without the parameter grid estimation or 10-11 hours with the parameter grid estimation (which is why it does not evaluate by default). All of the code to recreate the PDF is within the Rmd file or one of the three functions called (either in the package or sourced from the R files), so knitting the document will produce a single self-contained PDF.
