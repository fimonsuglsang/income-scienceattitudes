# income-scienceattitudes

Code for the article "Is science for the rich and powerful? Investigating the relation between income and trust in science across 145 countries"

To reproduce the analyses from the article, follow these steps:

1.	download the data from the webpage linked in the data folder, and rename these to have the same name as the link
2.	run scripts/1_clean.r to prepare data
3.	run scripts/2_regobjects to get subset regression objects
4.	(optional) run scripts/3_mixedeffects. Objects are also saved due to computation time.
5.	in-text figures are reproduced running scripts/4_main.r 
6.	(optional) run scripts/5_indi splits pr region.r, for the code for the data for the final figure in the  addnalyses.r
7.	the additional analyses in the suplemental materials are reproduces running scripts/6_addnalyses.r 

All scripts are anotated to indicate the steps taken. Contact me at simon.fuglsang@ps.au.dk in case of any questions.
