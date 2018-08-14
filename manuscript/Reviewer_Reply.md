#  Comments from the SME:

**General comment: I appreciate the authors' efforts to ensure transparency and repeatability of the analysis.**

Thank you. 

**l. 20: not all the models are process-based; revise?**

We added changed this to "statistical and processed based models".

**l. 41: could you delete "uncertainty in", or replace with "estimates of"? (Isn't it the timing itself that's important, not the uncertainty in the timing?)**

We deleted "uncertainty in".

**l. 81: fix "quanity".**

Fixed.

**l. 115: why these dates?**

These dates were used in accordance with Crimmins (2017), who used them prior, as budburst is unexpected to be first seen past DOY 172 and DOY 213 for flowers. 

**l. 127: phenophase (singular)**

Fixed.

**ll. 210-211. I don't understand this. Why do some seemingly strong relationships (e.g., "naive" panel of Fig. 2; also some panels of Fig. 3) yield negative R-squared values? This probably just reflects my ignorance... but it's likely that if I'm mystified, other readers will be, too! Please explain more clearly how the <0 R-squareds arise.**



**I found the discussion quite insightful, although there are some additional points that should be addressed and a few areas where clarity could be improved (see Reviewer 1's comments)**. 

Thank you. Please see the replies to Reviewer 1. 


**Also:**
**l. 289: what do you mean by "internal phenology"? internal physiological processes?**

Specifically we meant the date of endodormancy break, but had not used this term prior and did not want to confuse readers unfamiliar with it. We have changed this text to be more descriptive of the Chuine et al. 2016 study results. 

**l. 294. "warrant caution" -> "indicate that caution is warranted"**

Fixed.

**l. 305: "the" is repeated.**

Fixed.

**Fig. 4: the labels on the right-hand side ("Metric 1/2") are uninformative. Please use more intuitive labels, e.g. "LTER data/NPN data".**

We replaced the labels to reflect the data evaluated. We also adjusted the methods section slightly to hopefully make this more clear.

# Reviewer(s)' Comments to Author:

## Reviewer: 1

### Comments to the Author
**Comparison of large-scale citizen science data and long-term study data for phenology modeling**

**This is a very practical study and the evaluation of the approach is clear and pretty straightforward to follow. The authors might improve readability a bit by either adding subheads into the Methods & Results or repeatedly using the same terms to refer to the various steps (eg, cross-validation).**

After some discussion, we decided not to add subheadings. 

**The most critical thing that I see that’s lacking is that there is no consideration of how decisions made regarding which NPN data to include in the analysis might be impacting model performance. The authors allowed observations with up to 30d between the last reported “no” and the first reported “yes” and chose the midpoint. This could still lead to up to 15d error in the assumed date of “yes” and the true date of “yes”; this variance may in many cases be larger than the signal in the model. Though actually addressing this sensitivity is probably beyond the scope of the analysis, the potential and implications for this error should be discussed explicitly in the paper.**

Gerst et al. 2016 looked at this issue and found that differences in site level means among more restrictive datasets was normally distributed. Thus having a shorter cutoff period here would result equally in less overestimates *and* underestimates. Since the models here are optimzed for RMSE we feel the results would not be affected overall by having a cutoff shorter than 30 days.

Shawn's note:
After talking with Ethan I'll redo most of the analysis with a shorter cutoff date and see how things compare.
also report the distribution of "days between" to see if they're very far off between 30day cutoff and a shorter cutoff. 

**The authors lay out two goals for this effort – to determine “inferences about biological processes driving phenology” and to develop predictive phenology models. It may be just me…using the term “inference” in this way confused me.**



**Regarding the predictions that can result from these different models…this is of strong interest to many stakeholder realms. It would be fantastic to see a bit more attention given in the discussion to recommendations for implementation. The authors talk about the tradeoffs between more complex formulations and simpler models and – generally – reasons these differences may come about. It would be great to also see a bit of more practical discussion along the lines of, “if you can tolerate error of 7-14d, then model X may meet your needs, and it’s fine to build the models with either NPN or LTER data. However, if your needs are Y, then you’d be better off doing blah, blah…”… would this be possible?**

note:
This can essentially be done by looking at some of the supplement figures. Will point this out in the discussion.
shoot for 2-3 sentences describing some rules of thumb. with specific of error rates someone can be ok with.


**Thank you for your serious efforts to understand the USA-NPN data and to contribute to the collective understanding of this dataset’s potential as well as shortcomings. I’m so excited to see this sort of work happen! I’m happy to talk with you more regarding this manuscript as well as your other efforts, especially if any of these comments require clarification.**
Sincerely,
Theresa Crimmins, theresa@usanpn.org

### Smaller things

**Please refer to the Network as “USA National Phenology Network” and “USA-NPN” because there are lots of other phenology networks in other countries**

We have added this clarification in several places within the text to avoid any confusion. 

**In several places “data” is treated as singular… data are plural**

Fixed. 

**L67: the citizen science program run by the USA-NPN that yields the data housed in the National Phenology Database is Nature’s Notebook**
**Thank you for acknowledging the Nature’s Notebook contributors for the data!**

You're welcome. We're happy to credit all data providers, as this study would not have been possible without them. 

**Please see the USA-NPN data attribution policy for recommendations for how to properly cite the dataset: https://www.usanpn.org/terms#DataAttribution**



### Abstract
**be sure to indicate that the study is focused on the U.S.**

This is now made clear by specifying that it's the USA NPN. 

**L26-29: models performed best when applied to the same data with which they were built – you attribute this to scale…though other things could be at play here, such as local adaptation and species’ sensitivity to different forcing variables varying across the range.**



**L33: you mention that the NPN dataset offers many species… though this is a good point, it is actually irrelevant to the arguments that you are making in this paragraph, that is, if your comparison is focused on only a handful of species.  Oh, now I see you make this point several times in the paper…but I’m still struggling to see how it’s relevant. Is it that you could have built models with many more species in this study based on what NPN offers, but you were limited by what the LTER datasets had to offer?**

Reviewer 2 is correct in that our analysis was limited by species common to the NPN and four LTER datasets. Large scale phenology studies typically employ as many species as are available in a given dataset. Thus we point out the large species pool of the NPN dataset throughout as it is something other researchers should consider when designing their own analysis.

**L34-36: this concluding sentence could be made stronger if you kept the focus on the findings of the present paper – the strengths and limitations of the two datasets and their suitable applications.**

We felt that the strengths and limits of these datasets required more attention in the abstract than could be managed in the final sentence, so we discussed them in the prior sentences primarily and instead looked forward in the final sentence. 


### Introduction

**L77: Nature’s Notebook has 1,000s of volunteer participants (www.usanpn.org/data/dashboard)**

Fixed.

**L80: may also want to check out Feldman et al. (2018) – https://doi.org/10.1007/s00484-018-1540-4 for further support on cit sci observer skill**



### Methods

**I think it’s pretty critical to provide your sample sizes for each of the species/phenophases under evaluation for each of the datasets… when I read that you held out 20% of observations for evaluation, I was left wondering, was this 5 points or 50 or 500? You could include this info in Table 1**

We have added details of the sample sizes to the supplement table S1.

**What is the sampling frequency for the LTER sites? This is quite relevant because you chose to use 30d as your cut-off for NPN data - you’ll allow for up to 30d between the last reported “no” and the first reported “yes”. How does this compare to what’s possible in the LTER datasets? It could definitely have an impact on the sensitivity of your results.**


Shawns notes:
put the numbers below in the table or ref. and re-reference them when i talk about reviewer 1's other issue with NPN sampling frequency

sampling freq. from metadata / from data
harvard  3-7 days / 5.6 days
hubbard  7 days / 7.1 days
hjandrews  7 days / 7.6 days
jornada  monthly / 30.5 days

**After reading about the phenology datasets, I expected to read about the temperature data used. I see now that it’s nested within the “Modeling” section… the organization just surprised/confused me.**

The phenological data are the crux of the study so we felt that they deserved special consideration, while the components used to generate the models could be kept within the modeling section. We will gladly change this if the editor feels it is necessary. 

**L138-140: Provide some references for your GDD calculation – there are multiple ways to go about calculating GDDs**

The exact calculations are unique to each model and are provided along with references in Table 2. We have clarified this in the text. We also added references for the GDD and Fixed GDD model.

**L143: what is NCD?**

This is the number of chill days (mean daily temperature < 0 degrees C) since Jan. 1 in a given year. We have added the description to the text. 

**L145: please provide some references to back up your statement that these formulations are the most common**



**L156: I’m sure you extracted the daily temp at the lat/long of each observation location – maybe just make that a bit clearer**

We have clarified this. 

**I don’t think I saw “RMSEA – RMSEB” and “RMSEC-RMSED” referenced anywhere else in the paper**

We removed these specific equations and replaced them with a better description of this analysis. 

**L196: what does “scenario A” refer to here?**

This referred to the RMSE values derived from LTER Model predictions for held out LTER data. We rewrote this paragraph slightly and changed the labels on Figure 4 to make this analysis more clear. 

### Results

**L231-237: is it possible to quantify these comparisons in any way?**

Suggestion from Ethan:
do a paired t-test here to show distribution is different from 0. Make sure text still matches if a bunch are or are not significantly different than 0.

### Discussion

**L244: are you referring to the temporal depth and/or frequency of sampling when you say “intense”?**

We are referring to both of these as LTER sites typically have a long history of frequent and consistent sampling compared to USA-NPN sites. 

**263: “limitations in the sampling of the NPN dataset” – what sort of limitations? Small sample size, frequency of sampling, or something else?**

The frequency of sampling, in a given season and also between seasons, is the primary limit we consider here and we've made this clearer by stating as much. 

**L266-268: it would be helpful to add some references for local adaptation and differing sensitivity to phenological drivers across gradients**

We added two references here to highlight the potential variation in phenological requirements across different gradients. 

**L270-272: another option could be to develop local or regional models, especially if a national-scale prediction is not needed**

We have briefly included this possibility, as there may indeed be circumstances (population conservation) where the national prediction is unneeded.

**L273: define “model identifiability” and/or provide a reference? I think you are explaining it in the subsequent sentence – if so, just make this more explicit.**

We have made this clearer by switching to a colon instead of a period between the two sentences.

**L276: what do you mean by “these analyses”?**

Maybe change to "our analyses" ? 

**L293: not sure what you’re referring to when you say “this previous research”**

We were referencing the two studies cited in this paragraph (Chuine et al. 2016 and Basler 2016). We have changed this to "In combination with the aforementioned studies,..."

**L337: there are many recent studies that have brought together these various datsets; would be worth adding a couple of references**


Shawn's notes on some refs to add:
comparing phenocam + modis: https://doi.org/10.1038/s41598-018-23804-6
comparing modis phenology w/ NPN & Flux towers: 10.1016/j.ecolind.2017.02.024



## Reviewer: 2

### Comments to the Author
Summary:

**While I do think that this manuscript should be ultimately acceptable for publication at Ecology, there are a few major/minor comments and suggestions for which I would like the authors to consider.**

Major Comments and Suggestions:

**First, for NPN species with larger pools/spatial extents of observations, I wonder if would be useful to calibrate the models using subsets somehow stratified by climate and/or latitude. For example, train the models using only warm region (or southern) red maple and test on cold region maples (or vice versa). Perhaps this could further address the issue of model identifiability (as discussed in Lines  259-277). While the oft-mentioned Basler (2016) study certainly had nice spatiotemporal coverage, it did not have nearly the latitudinal/climate variability that NPN affords.**

We discussed this and ultimately decided that it is beyond the scope of the current paper. Although we agree that this approach has the potential to improve the paper, we are unsure of the extent to which this would address the issue of identifiability, which is itself noted as a potential issue rather than a definite one. We also feel that it would require too much space to adequately investigate and discuss, and that the current set of analyses sufficiently accomplish the goals we set out in the introduction. However, we do hope to pursue these confounding factors further in future efforts. 

**Second, given the length of time series of the LTER datasets, I think it would be useful to further examine the performance of models based on other criteria such as the amount of interannual variability explained (correlation or R-square) or bias during anomalously warm or cold years. Perhaps the latter might be outside of the overall realm of the paper, but I think it would be informative to know which models capture this type of information which matters in a global change context.**

Shawn's note:
make a R^2 figure for all species/phenophase for the supplement
on the stratified sampling. We thought of this, there's not enough data even for Acer rubrum. Write a sentence or 2 about this in the discussion and how it could be done someday when enought data is collected.

### Minor Comments and Suggestions:

**Line 281: Please verify what is T* (I assume base temperature)**


