Background
========================================================

Over the last few years, the popularity of text mining grows, as part of data processing snd spelling software. In this report, the author wishes to present his preliminary observations and plans for producing a shiny application that could presict a writer's nest word. 
 
In the last five years, the computer hardware industry developed more powerful computers, dealing with high memory and processing demands. 
 
In this new Shiny app, I wish to use this  advantage to develop a word predicter, which could be based on a larger corpus than ever before while doing the main calculations in advance. The Shiny interface will conduct only the last basic calculations based on a processed probability table, done offline. 


Methodology
========================================================

Most of the calculations were done before the application is even launched in a preparation file called "preparation app.R". In this file the system prepared a table of frequencies of couple, three-words or four-words as a prediction table, which made it possible for the application just to pick the right results:

* frequencies: only those with text frequency of 20 observations or above.
* Sample: 100% of the dataset of Twitter twits, blog posts and news correspondence.

More details about the sample and its structure can be found in my milestone report: <https://rpubs.com/mickeykislev/795174>


The uniqueness of the application
========================================================

We all write differently in different media. We do not use the same language when we write a tweet on Twitter or writing a post in a blog, and, of course, writing a news correspondence.

In this application a person can choose the prediction genre suitable to the media used by the user.

<img src="https://github.com/mickeykislev/Data-Science-Capstone/raw/img/example.png"; style="max-width:900px;float:center;">


How to use the app
========================================================

All you have to do is to write the beginning of the sentence and the application will show options to complete.

The link to the application is:
<https://mickeykislev.shinyapps.io/WordPredict4/>

<img src="https://github.com/mickeykislev/Data-Science-Capstone/raw/img/%E2%80%8F%E2%80%8Fhowto.PNG"; style="max-width:850px;float:center;">
