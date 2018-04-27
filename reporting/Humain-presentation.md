Context & objectives
====================

Integration of the RMI + Pameseb AWS networks
---------------------------------------------

-   Agromet : providing hourly gridded (1km²) weather datasets for agricultural decision support system
-   Possibility of RMI stations integration for better spatialization ==&gt; interoperability ?

Assessing the agreement level between the 2 types of AWS
--------------------------------------------------------

-   previous [work](http://onlinelibrary.wiley.com/doi/10.1002/wea.2158/pdf) by Geoff Jenkins described a comparison method based on regression model correlation study.
-   as demonstrated by [Giavarina](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4470095/), correlation studies is not appropriate to study the differences.
-   The alternative [Bland-Altman analysis](https://www-users.york.ac.uk/~mb55/meas/ba.pdf) proposed by the authors is used in this study.

Data presentation
=================

Statistics :
------------

| id  | sid  | poste  |       sum|   mean|    min|   max|    sd|    var|   q25|   q50|    q75|
|:----|:-----|:-------|---------:|------:|------:|-----:|-----:|------:|-----:|-----:|------:|
| 31  | 61   | Humain |  176438.2|  10.18|  -9.80|  33.7|  7.14|  50.99|  4.80|  9.90|  15.20|
| 64  | 1000 | Humain |  175695.3|  10.14|  -9.62|  33.2|  6.97|  48.52|  4.94|  9.94|  15.02|

Time series
-----------

![Time series](Humain-presentation_files/figure-markdown_github/unnamed-chunk-2-1.png)

Density
-------

![Density](Humain-presentation_files/figure-markdown_github/unnamed-chunk-3-1.png)

Scatter
-------

![Scatter plot](Humain-presentation_files/figure-markdown_github/unnamed-chunk-4-1.png)

First insights
==============

Similarity
----------

Datasets seem in accordance ...

-   but to which extent ?
-   How can we quantify this degree of similarity ?
-   Even if a certain degree of similarity : Pameseb seems warmer

Temperature difference investigation
------------------------------------

What can be infered by studying the temperature differences rather than their correlation ?

Investigating the temperature difference
========================================

Bland-Altman analysis
---------------------

We can use a [Bland Altman analysis](https://pokyah.github.io/howto/assessing-the-agreement-between-2-quantitative-methods-of-measurements-understanding-the-Bland-Altman-analysis/) to measure the agreement level between the 2 stations.

It consists to take observations pair by pair, compute their means and difference an plot them.

Vizualizing Bland-Altman analyis
--------------------------------

![](Humain-presentation_files/figure-markdown_github/Plot%20of%20mean%20tsa%20vs.%20tsaPameseb%20-%20tsaIRM-1.png)

What the Bland-Altman plot tells us ?
-------------------------------------

-   Higher mean temperature shows and higher temperature difference.

-   The highest temperature differences are recorded between april and august.

-   Temperature differences up to 2.5°C

-   ==&gt; CRA-W AWS tends to significantly over-estimate the temperature compared to the RMI station

A plausible hypothesis to investigate
-------------------------------------

The 2 stations are actually different in their design :

-   RMI : Stevenson radiation screeen mechanically ventiled

-   Pameseb : custom screen without mechanical ventilation

-   ==&gt; We can assume that it exists an overheating of the Pameseb shelter compared to IRM, especially during periods of **High irradiance** and **low wind** for which the lack of ventilation system at Pameseb may significantly increase the measured temperature.

Visualising the 2 designs
-------------------------

![](./assets/shelters.png)

Questioning the potential effect of the ventilation
===================================================

Strategy
--------

-   Do we see the same pattern in temperature differences when considering situations of low irradiance and high-wind situations only ?
-   Filtering the dataset by keeping situations where : `vvt > vvt_mean && ens < 10`

Low irradiance and high wind BA Plot
------------------------------------

![](Humain-presentation_files/figure-markdown_github/unnamed-chunk-5-1.png)

What the Low irradiance and high wind BA plot tells us ?
--------------------------------------------------------

-   The differences are still present but are clearly **less important**. Some discrepancies due to outliers

-   Even with no irradiance, the rising trend is still present. How can this be ? According to RMI, the ventilation system pumps up air 50 cm below the sensor. While the atmosphere is stratified, the RMI shelter might be ventiled with colder air than the one it is supposed to measure...

-   ==&gt; So the rising trend might also be due to an artifical "over-cooling" rather than the combined effect of high irradiance + low wind only

Statistics of the Low irradiance and high wind
----------------------------------------------

| id  | sid  | poste  |       sum|  mean|   min|    max|    sd|    var|   q25|  q50|    q75|
|:----|:-----|:-------|---------:|-----:|-----:|------:|-----:|------:|-----:|----:|------:|
| 31  | 61   | Humain |  31017.60|  7.37|  -9.1|  25.70|  5.23|  27.33|  3.90|  7.2|  10.50|
| 64  | 1000 | Humain |  31512.05|  7.49|  -8.8|  25.33|  5.15|  26.52|  3.98|  7.3|  10.64|

Strangely, IRM is globally **warmer**

-   even if the temperature difference for high mean temperature is stil positive (`T°pameseb > T°IRM`) !
-   even it might be subject to artifical over-cooling when the air is stratified
-   ==&gt; Hypothesis : when the shelters stop to receive direct solar radiation, we might expect that the Pameseb one cools quicker due to its less important inertia ?

Considering the high irradiance and low wind situations
-------------------------------------------------------

We can have a quick comparative lookat the **low-wind and high irradiance** situtions only to better grasp the effect of ventilation

High Irradiance & Low wind Balnd-Altman
---------------------------------------

![](Humain-presentation_files/figure-markdown_github/unnamed-chunk-7-1.png)

What the High irradiance and low wind BA plot tells us ?
--------------------------------------------------------

-   The highest temperature discrepencies are
-   Learn outputs of BA stats

==&gt; **prior to the integration of the 2 networks in the spatialization process, a correction model needs to be applied to the temperature data produced by the CRA-W AWS ne2rk**.

Summarizing the examination of a ventilation mechanism
------------------------------------------------------

-   IRM temperature tends to be smaller when low temperature situations (&lt;5°C) whatever the wind and irradiance conditions ("over-cooling" by pumping colder stratified air ?)
-   Pameseb temperature tends to be higher when high temperature situations () only during low wind and high irradiance situations (lack of mechanical ventilation)
-   more more ! Pay attention to axis, etc !!! A lot to say

Solving the disagreement
========================

Strategy
--------

We can build a temperature correction model based on irradiance and wind speed conditions

Conclusions, perspectives and colofon
=====================================

Conclusions
-----------

-   prior to spatialization, a correction model needs to be applied before integrating the 2 networks

Perspectives
------------

-   Once finished, we plan to submit this work to the ["Weather" Journal of the RMETS](http://rmets.onlinelibrary.wiley.com/hub/journal/10.1002/(ISSN)1477-8696/aims-and-scope/read-full-aims-and-scope.html).
-   title idea : &gt; Quantitative intercomparison of 2 years hourly records of temperature and relative humidity measured by 2 different types of professional-grade automatic weather stations at Humain, Belgium

Colofon
-------

-   This report is still in major revision phase. **Consider it as a first draft version**.
-   This document was generated using R software with the [knitr library](https://deanattali.com/2015/03/24/knitrs-best-hidden-gem-spin/)
-   The interactive plots are rendered from ggplot by [plotly](https://plot.ly).
-   The source code of the analysis and this presentation is availbale on [github](https://github.com/pokyah/AWS-Humain-comparison)

<script>
  $(document).ready(function() {
    $items = $('div#TOC li');
    $items.each(function(idx) {
      num_ul = $(this).parentsUntil('#TOC').length;
      $(this).css({'text-indent': num_ul * 10, 'padding-left': 
          0});
    });
    
  });
</script>
