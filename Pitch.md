Next Word Prediction
========================================================
author: Tsuyoshi Matsuura
date: November 18, 2019


***Project Objective:***

To develop a demonstration app that is capable of
predicting the 'Next Word' when it is provided with
some text.




Features of the 'Next Word' prediction app
========================================================

* The app generates 'Next Word's as the user types in text
* Upto five 'Next Word's are generated for each text
* The app has a profanity filter, i.e. it will ignore profanities input by the user
  and will not generate profanities itself

The language model for 'Next Word' prediction was implemented
using 'tidytext' which is very well integrated with the
'tidyverse' packages, in particular 'dplyr'.

For quick app deployment Shiny was used as the development
environment for the GUI.

Language Model description
========================================================

* The app is based on a quad-gram language model with Kneser-Ney smoothing
* The app was trained on 90% of the provided  English blogs, news and twitter texts
* To keep the size of the app manageable and the response time reasonable the 
 generated n-grams (n=1,2,3 & 4) were pruned
* The prediction accuracy is expected to be around 15% based on a test using
 the remaining 10% of the blogs, news and twitter data

Further development
========================================================

The current app was developed and built in less than one month's
time and requires more time and resources for further development, e.g.:

* Increase accuracy by using larger and more diverse training sets (including going beyond quad-grams)

* Development / implementation of more efficient storage and retrieval of n-grams

* Testing of the application in the field and collecting user feedback

## I am looking for early commitment of more resources!


Using the 'Next Word' app
========================================================
***Link to app:***

https://tsuyoshi-matsuura.shinyapps.io/NextWord/

***Instructions:*** <br>
 * After the app loads, enter text into the input box.
 * The app will predict upto five words that can serve as 'Next Word's to the input text
 * Continuing adding text will automatically update the predicted 'Next Word's 


## Enjoy using the app & Please provide feedback!!!!