# Project: Words 4 Music

### [Project Description](doc/Project4_desc.md)

![image](http://cdn.newsapi.com.au/image/v1/f7131c018870330120dbe4b73bb7695c?width=650)

Term: Fall 2016

+ [Data link](https://courseworks2.columbia.edu/courses/11849/files/folder/Project_Files?preview=763391)-(**courseworks login required**)
+ [Data description](doc/readme.html)
+ Contributor's name:  Xuechun Sun (xs2254@columbia.edu)
+ Projec title:  Words for Music
+ Project summary: 
	+ Clustering lyrics words using Topic Modeling
		1. Generating words distribution for each topic.
		2. Generate the topic of each word, given that word in a song
	
	+ Extract Features through “/analysis” files
		1. Variables Process through “/analysis” files 
			1. bars_start (n1 dim vector): start time of each bar according to The Echo Nest
			2. beats_start (n1 dim vector): start time of each beat according to The Echo Nest
			3. sections_start (n2 dim vector): start time of each section according to The Echo Nest
			4. segments_loudness_max (n3 dim vector): max loudness during each segment
			5. segments_loudness_start (n3 dim vector): loudness at the beginning of each segment
			6. segments_pitches (12 * n3 dim vector): chroma features for each segment
			7. segments_timbre (12 * n3 dim vector): MFCC-like features for each segment
			8. tatums_start(n4 dim vector): start time of each tatum according to The Echo Nest
		2. For values in variables 1,2,3,8 in the previous slides, the number is actually increasing time points. Choose Summary statistics (quantiles), mean and SD for these variables for each song (7 dim vector for each variable)
		3. For values in variables 4,5,6,7 in the previous slides, the number is time points in each small time segment. Get sample points equidistant from raw dataset, in other words, choosing sample data successively from the original data, where the interval of each two sampling data are the same. (get 20 intervals and 21 sample points for each variable
)
		4. original Feature Matrix: have Number_of_song * 573 dim
		
		5. Use PCA analysis to reduce dimension to 225 dim (explain around 80% variance)
		
	+ Make prediction using Multinomial Regression ／ Random Forest
		1. Using Random Forest(lower error rate) to build model and get prediction matrix (100 * N_of_topic dim)
		2. get final rank by converting word count (rf_test_prediction_rf(100 * N_of_topic dim) %*% topic_dist(N_of_topic * 5000 dim)) to rank using formula: P(word|feature) = \sigma P(word|Topic) * P(Topic|feature)


	
Following [suggestions](http://nicercode.github.io/blog/2013-04-05-projects/) by [RICH FITZJOHN](http://nicercode.github.io/about/#Team) (@richfitz). This folder is orgarnized as follows.

```
proj/
├── lib/
├── data/
├── doc/
├── figs/
└── output/
```

Please see each subfolder for a README file.
