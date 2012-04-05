.. role:: my-note

****************
Internal Report
****************


:author: Paul Tremblay 
:organization: Zappos 
:date: 2011-11-29
:abstract: 

        This report sums changes to ZFC not found in the Amazon
        report.

========================
Changes in Productivity
========================

Overview
=========


ZFC experienced a decline in productivity in 2011.


.. figure:: charts/outbound_rate_2011.png
   :height: 2.6in
   :width: 5.73in
   :alt: Outbound Rate, 2011

   Rate for units processed for outbound. The sharp decline in the shaded
   area, at week 38, occurred during the cutover. _`outbound-graph`

Looking at last year helps us better understand the decline.

.. figure:: charts/outbound_rate_2010_2011.png
   :width: 550px
   :height: 250px
   :alt: Outbound Rate for 2010 and 2011

   Rates for outbound processes for 2010 and 2011.

There is a 8.32 unit difference, +- 2.58 error (95% confidence
interval). That translates to a -25% change, +- 7.9%. The graph shows
that the decline started around week 8, labelled “migration” on the
first chart. From weeks 6 to 11, Zappos moved all of its merchandise
from warehouse 1 to warehouse 2 in order to make warehouse 1 ready for
Amazon products. Not only did this migration require extra labor, but
ZFC processes non-footwear at a slower rate than footwear. The section
on static picking outlines the exact differences for static picking,
and also shows that static picking accounts for most of the decline
for the outbound process. If WH 2 had not received so much apparel,
then most likely the rate would have remained at 35 units/ man hour. 

The next big decline occurs in mid June, labelled “Testing start.”
Testing closed the warehouse at 1:00 AM in the morning, 5 hours before
the brief time the warehouse is usually closed between shifts. The
early closing time caused a backlog. Not only does a backlog by itself
mean goods are processed slower, but it also required ZFC to hire
temporary workers. Testing lasted until the week of cutover.


Other warehouse disruption also hurt the outbound ate. For example, in
early August ZFC tested SLAM shipping stations, and  closed down PANDA
lines in order to install the SLAM counterpart. With the PANDA line
down, and the SLAM not yet functioning, workers had to manually
prepare boxes for shipping. A week before cutover, ZFC tore out two
more PANDA stations, again temporarily replacing them with manual
stations. Graph shipping-graph_ shows the results on this disruption.
Note the decline in week 33, and again in week 37, a week before
cutover. Other processes show the sharp decline occurring about a week
later. 

Last, the trends for both years run more or less parallel to each other. The
sharp declines, starting at the end of September, results because of Zappos
business model. Peak season for Zappos starts at the end of September, when
ZFC receives a huge influx of goods, until the end of December, when Zappos
ships a large amount of product. In order to handle this extra volume, Zappos
hires more workers than it needs, making sure customer's orders never have to
wait and Zappos reputation and future profitability suffer. The hiring trend
was magnified in 2011, when ZFC hired even more worker than the previous year,
because the previous year Zappos had not hired enough help and experienced
backlogs. 



Productivity by Process for Outbound
====================================

As expected from the overall decline of outbound rates, the rates for
individual processes also declined. The following graphs show the rates before
and after the cutover.

.. figure:: charts/outbound_rate_2011_all.png
   :width: 5.73in
   :height: 5.21in
   :alt: Outbound Rate for Several Processes, 2010


Shipping
==========

Shipping can handle a much larger volume of goods than other stations, and
there are fewer shipping stations. Hence the rate is much higer. 


.. figure:: charts/shipping_rate_2011.png
   :height: 2.6in
   :width: 5.73in
   :alt: Shipping Rate, 2010
   :name: shipping-graph

   Units shipped per hour over weeks, with a trend line. The first
   decline occurred during week 31, at the start of August, when ZFC
   had to close down PANDA stations to install SLAM lines. The second
   decline, in the shaded area, occurred during the cutover. 

Here are the other processes, compared with the previous year, included for
completeness.

.. figure:: charts/picking_rate_2010_2011.png
   :height: 2.6in
   :width: 5.73in
   :alt: Picking, 2010, 2011


There is a 31.8 unit difference, +- 7.9 units, from 2010, or a
-34% change, +- 8.5%.  Static and carousel picking are
combined for convenience. Although carousel picking has a much higher
rate, its volume is low enough that it does not change the overall
picking rate.

.. figure:: charts/multis_rate_2010_2011.png
   :height: 2.6in
   :width: 5.73in
   :alt: Multis Rate for  2010 and 2011


There is a 10 unit difference in multis, +- 5.5 units. That translates
to a -13%  +- 8.5%.

.. figure:: charts/singles_rate_2010_2011.png
   :height: 2.6in
   :width: 5.73in
   :alt: Singles Rate for 2010 and 2011



There is a 10.3 unit difference, +- 6.8 units. This translates to a
-17% change, +- 9%.

.. figure:: charts/shipping_rate_2010_2011.png
   :height: 2.6in
   :width: 5.73in
   :alt: Shipping Rate for 2010 and 2011


There is no significant difference for shipping between the two years.

Static Picking
===============

Static picking accounts for most of the decline in rates for outbound,
pre cutover.

.. figure:: charts/static_picking_2010_2011.png
   :alt: Static Picking, 2010, 2011
   :width:  5in
   :height:  3.5in

.. figure:: charts/static_vs_other.png
   :alt: Static Picking Compared to Other Processes
   :width:  5in
   :height:  7in



Note how the rates for static picking mirror those for the outbound overall. 

From Justin Williams’ email


     I can definitely shed some light on reduction in efficiency last year in
     static picking.  From a direct rate perspective there were three main reasons
     for declining performance.  Firstly, we started the year with only footwear In
     W2.  Starting in late February we transferred all of the non-footwear from W1
     into W2.  This ended in early April.  This transition of inventory resulted in
     a change to our picking procedure and our picking teams had to be trained on
     how to pick to tote.  Also, Picking non-footwear to tote is less efficient
     than picking footwear to conveyor.  Secondly, as we gained inventory from the
     move and in preparation for peak, we had to use pick mods that didn't have
     conveyance yet.  This required pickers to travel several hundred feet and
     through gates to drop product onto takeaway conveyors.  Finally, as we
     received non-conveyable product from W1, we had to start up a new department,
     Singulate, to take full totes of Multis and individually induct product into
     our Multi sorter to Multi lanes.  I believe this department labor was shown
     under picking for several months until it was split out separately.  

______________________________
Footwear Vs Nonfootwear Rates
______________________________

The decline in footwear resulted largely because it takes longer to
process non-footwear than it does footwear. The following graph
illustrates this.

.. figure:: charts/foot_vs_non_foot_rate_2011.png
   :alt: Rates for Picking, 2011, Footwear and Non Footwear
   :width:  5in
   :height:  7in

We can estimate the affect of apparel on the overall static picking
rate by determining how much longer it takes to pick to tote. We can
achieve this estimate in two ways:

1. Estimate the difference from the figures that generated the above
   chart. Doing so we find that it takes 1.6 as long.

2. Use figures from tests done at ZFC. These tests show that it takes
   1.7 as long to pick non footwear to a tote.

These two figures can generate two more adjusted lines. Normally, the
rate is determined by the units/hours. For static picking, the rate
is:

.. math:: 

 (text(units-footwear) + text(units-nonfootwear))/(text(hours-footwear) +
 text(hours-nonfootwear))

We adjust the rate by taking out the extra time for the non-footwear
picking. If it takes 1.6 as long to pick to a tote, then we divide the
hours by the same amount. Doing so tells us the rate if all units
picked were shoes, instead of apparel and non conveyor items. The
equation for the lines then become:

.. math::

  (text(units-footwear) + text(units-nonfootwear))/(text(hours-footwear) +
  text(hours-nonfootwear)/1.6)


.. math::

 (text(units-footwear) + text(units-nonfootwear))/(text(hours-footwear) +
 text(hours-nonfootwear)/1.7)

We can generate a third line to test the accuracy of the other two.
Starting at week 8, we see a sharp decline in the static picking rate.
Nothing else explains this drop except the massive amount of apparel
moved to the warehouse. The drop showed that it took 1.3 times as long
to process all product. In order to adjust this line, we can multiply
the rate from week 8 by 1.3. 

If our adjustments are correct, we should see all three lines match
up. In fact, this is the case.


.. figure:: charts/static_picking_2010_2011_adj2.png
   :alt: Static Picking with Adjusted Lines, 2011
   :width:  5in
   :height:  7in

We cannot adjust the year past the cutover by determining the amount
of footwear and multiplying that by 1.6 or 1.7 because there is no
data for non footwear for this time period. The Amazon WMS does not
allow us to track footwear vs. non footwear. However, we can apply
the adjustment of the third method above, of multiplying the entire
rate by 1.3. Doing so yields a sensible adjustment:

.. figure:: charts/static_picking_2010_2011_adj4.png
   :alt: Static Picking with Adjusted Lines, 2011, 2012
   :width:  5in
   :height:  7in


Note how at the start of the year, static picking was at a rate of 62
units. If we adjust the rate, we end at around the same place. For the
first 3 weeks of 2012, the rates are 55, 55.5, and 55 units/man hour.
If we apply the same adjustments, the rates are approximately 71,
higher than last year.


The adjustment constant of 1.3 may be somewhat generous. If we take
the average of the all the adjustment lines from above, we come up
with a 1.2 adjustment line. In that case, the graph looks like this:

.. figure:: charts/static_picking_2010_2011_adj5.png
   :alt: Static Picking with Adjusted Lines, 2011, 2012
   :width:  5in
   :height:  7in

That puts ZFC at 65 units for the start of 2012 (after adjustment)
exactly the rate of the preceding year.



Productivity by Process for Inbound
====================================


.. figure:: charts/inbound_rate_2011.png
   :height: 2.6in
   :width: 5.73in
   :alt: Inbound Rate for 2011

   Units processed per hour over weeks, with a trend line, averaged
   across inbound processes, for the period before and after cutover.


.. figure:: charts/inbound_rate_2010_2011.png
   :height: 2.6in
   :width: 5.73in
   :alt: Inbound Rate for 2010 and 2011

There is a 9.1 unit change, +- 3.74 from 2010. That translates to -20%, +-
8.3%. 


Receive 
========


.. figure::  charts/receive_rate_2010_2011.png
   :height: 2.6in
   :width: 5.73in
   :alt: Receive Rates for 2010 and 2011.

There is a 25.03 unit difference, +- 9.50 from 2010. That translates
to -19% change, +- 7%. Unlike other processes, the rate diverges more
sharply after the cutover.  If we take weeks 1-38 as the first
interval, and 42-52 as the second interval, we find a 49.6 unit
difference, +- 9.91. That translates to -41% change, +- 8%. The
indirect labor accounts for much of this change.

.. figure::  charts/receive_rate_indirect_total_2010_2011.png
   :height: 2.6in
   :width: 5.73in
   :alt: Indirect to Total Hours, 2010, 2011. 


The Amazon WMS introduced inefficiency to the receive process in
requiring an extra step of converting the PO on the side of the box to
an Amazon PO. Whereas formerly a ZFC team member only had to scan the
side of the box before putting it on a conveyor belt, now he must
apply and scan a case sticker, and then associate the case sticker to
an Amazon PO by means of PO wrapper tool provided by Amazon. In some
cases this involves scanning a code on a separate sheets of paper.
These extra steps have increased the amount of indirect labor. 

In order to figure out the extra hours, we can look at the indirect
hours as a percentage of the direct hours. 

.. figure:: charts/receive_direct_indirect.png
   :height: 2.6in
   :width: 5.73in
   :alt: Indirect vs. Direct Hours, Receive, 2010, 2011.

We can adjust the lines in two ways:

1. Calculate what the indirect hours should have been based on earlier
   in the year. Earlier in the year the indirect labor amounted to
   .455 |multiply| the direct labor (with the error margin added in). 
   The total hours for after the cutover then becomes direct hours +
   .455 |multiply| indirect hours.

2. Calculate what the indirect hours should have been based on last
   year for the same time period. Last year the indirect labor
   amounted to .365 |multiply| of the direct labor (adjusted for
   error). The total hours for after the cutover then becomes direct hours +
   .365 |multiply| indirect hours.

The table below summarizes the results of these adjustments. 


.. class:: receive-hours
.. csv-table:: Receive hours, adjusted 
     :file: tables/receive_rate_adj.csv
     :header-rows: 1

The adjustments show that the indirect labor amounted to 12,381, or
19,222 extra hours. The second number is probably  more accurate, since
it is based on the previous year. Given that the peak season requires
a huge increase in direct labor, we should see the percentage of
indirect labor decline, and  hence have reason for thinking the
indirect labor amounts to only .365 of th direct. Using the second
figure, we can find the cost of the Amazon inefficiency:

19,222 |multiply| $16.10 = $309,474.00



Putaway
========


.. figure:: charts/putaway_rate_2010_2011.png
   :height: 2.6in
   :width: 5.73in
   :alt: Static and Carousel putaway for 2010 and 2011.


There is a -35.6 unit difference from 2011, +- 12.7 units. That
amounts to -24% difference +-15%. 



Returns
=========


.. figure:: charts/return_rate_2010_2011.png
   :height: 2.6in
   :width: 5.73in
   :alt: Return Rates for 2010 and 2011 

   There is a 7 unit difference, +- 1.97. That translates to -23%, +- 7%.

Like the receive process, the returns process shows a sharp divergence
from the previous year. The divergence starts at week 25.  The change
occurred for several reasons. In order to meet increasing demand, ZFC
increased the size of its warehouse. During the build, ZFC eliminated
the inbound line, the trash line, and the outbound line from the
return process. Instead of having return items directly conveyed on a
belt to the unpack station, workers had to put them on a palettes, and
then move the entire palette. Likewise, team members also had to
return items to storage by moving them on palettes.  Similarly, trash
was moved by a manual process. These changes required more manual
labor and hurt rates. Both these changes occurred at the beginning of
July, and can by the steep decline in graph returns-graph_. 

In addition, the returns department moved to its own building, one
half mile away. Instead of simply placing items on a conveyor belt to
transport them to storage, workers now had to put them on a palette,
and have the palette driven to warehouse 2. This extra step also
requires more labor, hurting efficiency rates. 


.. figure:: charts/return_rate_2011_2.png
   :height: 3.6in
   :width: 5.73in
   :alt: Return Rates for 2010 
   :name: returns-graph

   Units Processed per hour over weeks, for returns, in 2011.
   

The percentage of indirect labor also increased at the same time.

.. figure:: charts/return_percent_indirect.png
   :height: 3.6in
   :width: 5.73in
   :alt: Percentage of Indirect to Total Labor, Returns

This change occurred largely in part because returns categorized many
processes as indirect that formerly were categorized as direct.




Indirect vs. Direct Hours
==========================

Since early 2011, Zappos has tracked direct and indirect hours. ZFC counts a
direct hour as labor that directly contributes to a process. For example, when
an employee picks an item off a shelf and puts it on a conveyor belt, the time
counts towards direct labor. In contrast, the labor required to make sure the
scanning guns operate correctly count as indirect labor. 

Not surprisingly, the percentage of indirect to direct labor increased in
2011. ZFC expected this change because of the extra labor needed to move goods
to a new warehouse, as well as implement thorough testing of Amazon's
software. The chart below shows how as the percentage of indirect to direct
hours increased, the outbound rate decreased.

.. figure:: charts/indirect_out_vs_outbound_percent.png
   :height: 2.6in
   :width: 5.73in
   :alt: Indirect Labor Compared to Outbound Rate

   Indirect labor and outbound rate over time. Each line represents a
   percentage based on the first week of 2011. While indirect to direct labor
   increased to over 160% compared to the fist week, outbound rate decreased
   to under 60% compared to the first week.

The percentage of indirect to direct labor increased at about 3% a
month, starting at approximately 25% and peaking at approximately
55% right after the cutover, though without the spike, the percentage
would have reached a maximum of approximately 44%. Since the cutover,
the percentage has declined. This doesn't indicate so much that ZFC
decreased its indirect labor as that it increased its direct labor for
peak season.

At the same time, the increase in indirect labor does not explain the decrease
in throughput rate, as the following graph shows. 

.. figure:: charts/direct_vs_total_outbound2_arrows.png
   :height: 2.6in
   :width: 5.73in
   :alt: Direct vs. Total Rate

   Rate of all processes, with indirect hours, and with indirect hours
   removed.

The overall line shows the number of units processed per man hour. The
direct line shows the units processed per direct man hour, or the
total hours required minus the indirect hours required. Since rate is
determined by units divided by hours, the direct rate will always be
higher, since it reduces the size of the denominator. The arrows shows
the affect of indirect labor. If the indirect hours are zero, the two
lines meet. As the indirect labor increases, the arrows get longer,
and the overall rate declines.

For the sake of examining the affect of the indirect hours on the
overall rate, we want to look at the trends (or slope) of the lines
compared to each other. If indirect hours accounted for the decline in
rate, we should see an improvement in the direct trend once these
indirect hours are removed. Instead of sloping down, the trend for
direct rates should slope up, or, at least not slope down as sharply.
In fact, the graph shows that the rates with and without indirect
hours run parallel to each other. Put another way, indirect hours were
used effectively. 

We can also compare the indirect to total labor for years 2010 and
2011. Note that there is no difference.

.. figure:: charts/indirect_out_vs_total_2010_2011.png
   :alt: Percentage of Indirect to Total Labor
   :height: 3in

   Percentage of indirect to direct labor. There is only .7% difference
   between the two years.

Last, we can compare the direct rates of both years, and the total
rates of both years. Again, there is no difference

.. figure:: charts/direct_vs_total_2010_2011.png
   :alt: Throughput, 2010, 2011, with indirect removed.
   :width:  5in
   :height:  7in

The picture for inbound looks much the same, with a slight difference. 

.. figure:: charts/direct_vs_inbound_total_2010_2011.png
   :alt: Direct Labor vs. Total
   :width:  5in
   :height:  7in

Direct rate and total rate are relatively close to each other.
However, the throughput (overall) rate diverges from the direct rate,
indicating that indirect hours had an impact on the overall rate. In
fact, the percentage of indirect to total hours increase drastically
in 2011.


.. figure:: charts/indirect_inbound_2010_2011.png
   :alt: Percentage of Indirect to Total Labor
   :width:  5in
   :height:  7in


This change occurred in large part because ZFC recategorized the
indirect and direct labor. For example, returns counted as direct
labor the banding process, but later in the year, categorized this
process as indirect. 



Problem Solving
=================

Hours for problems increased for problem solving from 2010.

.. figure:: charts/problem_solve_2010_2011.png
   :alt: Total Hours for Problem Solving, 2010, 2011
   :width:  5in
   :height:  3.5in


With 95% significance, there is a 1.5% increase for weeks 38-52 from
2010 to 2011. With 95% significance, there is a 1.4% increase from
weeks 1-37 to weeks 38-52 for 2011. 

There is no statistical significant change for weeks 1-37 for years
2010 and 2011.

The total hours for 2011 are 2,335,137. Multiply this by .015 to get 
35027 hours. Multiply 35027 times $16.10 an hour to get a cost of 
$563,935.00

Note: if you take away the peak for weeks 38-41, there is still a .96%
increase from 2010 to 2011.  That is still a $360,918 increase


.. put this earlier


======
Costs
======

Outbound
=========

The cost of the cutover can be estimate  by examining the hours and
units as the year progresses and setting the first week in January as
our baseline. As ZFC increases the units it processes, it should
increase the hours in direct proportion. So if week 10 shows a 25%
increase, the hours should also increase to 25%. In fact, this is the
case for 2010.

.. figure:: charts/units_hours_2010.png
   :alt: Outbound Units and Hours, 2010
   :width:  5in
   :height:  7in

Note how the two lines practically lie on top of each other until week
38. At week 38, the line for hours starts to diverge, and continues
to diverge drastically, reaching a peak at week 50. The divergence
occurs because of the ZFC business model of increasing labor beyond
base need in order to ensure excellent customer service. 

Year 2011 looks slightly different.

.. figure:: charts/units_hours_2011.png
   :alt: Outbound Units and Hours, 2011
   :width:  5in
   :height:  7in

Again, the lines *nearly* lie on top of each other until week 38.
However, the slight space between the lines represents the affect of
the cutover. Starting in June, the hours increased greater than the units.
This divergence represents inefficiency, or cost. If the units
increased 25%, but the hours increased 30%, then ZFC used an extra 5%
in labor. We can calculate the actual number of hours by multiplying
the extra percent by the hours in January. 

In addition, we have to adjust for weeks 38 through 41. These weeks
clearly fall within the cutover, but also fall within the peak period.
Simply subtracting the two lines for this period will exaggerate the
cost, since, given the ZFC business model, hours are expected to be
higher. Instead, the previous year serves a baseline. For weeks 38, 39, 40,
and 41 in 2010, the hours were higher by 18%, 19%, 39%, and 47%. For
the same weeks in 2011, the differences were 241%, 431%, 375%, and
298%.  Subtracting the weeks in 2010 from 2011 yields a corrected
amount:

241% - 18% = 233%

431% - 19% = 412%

375% - 39% = 336%

298% - 47% = 251%

The following table summarizes the extra hours, for weeks 25 through
42, the period affected by the cutover.

.. class:: cutover-costs
.. csv-table:: Extra Hours, Outbound 
     :file: tables/extra_hours_outbound.csv
     :header-rows: 1

There were 86,583 extra hours. 86,583 |multiply| $16.10 amounts to $1,393,983.  

The following graph magnifies the critical area to show the extra
hours. The gray area represents the extra costs.

.. figure:: charts/outbound_costs.png
   :alt: Costs for Outbound
   :width:  5in
   :height:  7in


Inbound
=========

Costs can be calculated the same way for inbound, with a slight change.
Since returns suffered inefficiencies not related to the Amazon WMS,
these will be taken out of the calculations.

.. figure:: charts/units_hours_inbound_2010.png
   :alt: Inbound Units and Hours, 2010
   :width:  5in
   :height:  7in

2010 shows that hours did not always increase in direct proportion to
the increase in units. However, the average for the weeks before
cutover show that if anything, ZFC increased its efficiency (increased
processing units more than hours), so can can safely assume the same
for 2011, since any mistake will underestimate costs.

.. figure:: charts/units_hours_inbound_2011.png
   :alt: Inbound Units and Hours, 2011
   :width:  5in
   :height:  7in

The following table summarizes the extra hours, for weeks 25 through
42, the period affected by the cutover.

.. class:: cutover-costs
.. csv-table:: Extra Hours, Outbound 
     :file: tables/extra_hours_inbound.csv
     :header-rows: 1

The costs are found by the area under the curve. The extra hours are
65,900. 71,544 |multiply| $16.10 amounts to $1,060,989.00


The following graph magnifies the critical area to show the extra
hours. The gray area represents the extra costs.

.. figure:: charts/inbound_cost.png
   :alt: Inbound Units and Hours, 2010
   :width:  5in
   :height:  7in


The costs for both inbound and outbound is $2,454,972.00




Predicted Costs for 2012
========================

ZFC conducted a study to determine the effect of some of the
processes. The following graph and table summarize these estimates.

.. figure:: charts/total_costs.png
   :height: 2.6in
   :width: 5.73in
   :alt: Predicted Costs for Several Processes

   Predicted costs for several processes.

.. class:: projected-summary
.. csv-table:: Summary of Projected Costs, 2012 
     :file: tables/projected_summary_costs.csv
     :header-rows: 1

.. container:: caption

    Summary of predicted costs for 2012, for some processes.


_________
Returns
_________

Because ZFC does not know all the details of the FCSW return process, it cannot completely
predict the costs. However, it does know the indirect labor costs caused by two additional
steps. Under FCSW, unloading cartons from the trailer requires that the tracking number
be scanned. In addition, return items must be released to the conveyor, either directly on
in a tote, requiring a bar code be scanned.

The following two tables estimate the change in labor rate and the change in costs caused
by the transition to FCSW.

.. figure:\: num 12


.. figure:: charts/projected_rate_returns.png
   :height: 2.6in
   :width: 5.73in
   :alt: Predicted Rates for Returns

   Predicted rates for returns.





____________
Receiving
____________

As with returns, FC does not know all the details for the receiving process and cannot
completely predict changes. However, FC can predict impact for labor for the additional
step of retrieving a valid identification number to associate incoming cartons with an
Inbound Shipment Delivery (ISD). FCSW acquires this number in one of two ways:

1. FCWS scans the SSCC18 bar code or manual identification of the Amazon PO on
   the carton. 

2. If a vendor does not send an AISN (or does not include an SSCC18 on the
   carton); and if the vendor still uses legacy ZZZ-PO codes, the legacy
   ZZZ-PO must be entered or scanned using an additional tool that translates
   the original ZZZ-PO into the appropriate Amazon PO. This number is printed
   to a label, which is then applied to the carton. 

The following two tables estimate the change in labor rate and the change in costs caused
by the transition to FCSW.

.. figure:: charts/projected_rate_receive.png
   :height: 2.6in
   :width: 5.73in
   :alt: Predicted Receiving Rates 

   Predicted receiving rates per hour for Zappos vs. Zappos FCSW for 2012



________________
Static Picking
________________

There will be some efficiency gains in the static picking process due to an
increase in pick density, as the current FIFO (First In First Out)
requirement does not exist in FCSW. Based upon analysis that was performed in
the static area of quad 3 (in W2) that compared pick rates from the end of
February (when areas were less dense) with rates at the end of April (when
areas were more dense) along with reviewing the current pick rates in SDF6, FC
estimates FCSW will improve picking rates by 4%.

The following two tables estimate the change in labor rate and the change in costs caused
by the transition to FCSW.

.. figure:: charts/projected_rate_static_pick.png
   :height: 2.6in
   :width: 5.73in
   :alt: Projected Rate for Static Picking, Pre- and Post-Cutover.

   Predicted static picking rates per hour for Zappos vs. Zappos FCSW for 2012



______________________________
Receiving Exception Handling
______________________________

There will not be any efficiency gains from the exception process itself, but there will
be an overall cost savings due to a reduction in volume of goods moved through the
exception handling process. Currently, three types of exceptions go through exception
handling process:

FCSW physically handles only the last of these exceptions, catalog related information
defects, while treating the other two as RPIs to be routed to putaway and stored in
unsellable locations until auto-resolved by the appropriate buying teams.

Below is an illustration of this change in volume as well as the cost associated with this
change in the process.


.. figure:: charts/projected_exceptions_vol.png
   :height: 2.6in
   :width: 5.73in
   :alt: predicted Exceptions Handling Volume

   Predicted exception handling volume for Zappos vs. Zappos FCSW for 2012


_____________________________
Remaining Analysis Summary
_____________________________

Currently, ZFC does not have enough details about FCSW to effectively
determine process gains or losses for the following: static putaway; carousel
putaway; tote picking;carousel picking; singulate (induction); multi binning;
multi packing; and Panda / SLAM, truck load. The physical steps for these
processes appear similar as they exist today, but as ZFC better understands
these operations under FCSW, and confirms and finalizes procedures, it can
conduct the appropriate analysis.


.. class:: appendix

====================================
Tables of Projected Rates and Costs
====================================


.. class:: projected-costs
.. csv-table:: Projected Rates and Costs for Returns
     :file: tables/projected_rates_returns.csv
     :header-rows: 1

.. container:: caption

    Data for Projected Rates and Costs for Returns, 2012.

.. class:: projected-costs
.. csv-table:: Projected Rates and Costs for Receiving
     :file: tables/projected_rates_receive.csv
     :header-rows: 1

.. container:: caption

    Data for Projected Rates and Costs for Receiving, 2012.

.. class:: projected-costs
.. csv-table:: Projected Rates and Costs for Static Picking
     :file: tables/projected_rates_static_picking.csv
     :header-rows: 1

.. container:: caption

    Data for Projected Rates and Costs for Static Picking, 2012.

.. class:: projected-exceptions
.. csv-table:: Projected Volume for Exceptions Handling
     :file: tables/projected_exceptions_volume.csv
     :header-rows: 1

.. container:: caption

    Data for Projected Volume for Exceptions, 2012.


.. class:: appendix

====================================================
Tables of Rates of Productivity Pre and Post Cutover
====================================================


.. class:: long-metrics
.. csv-table:: Carousel Picking 2010-2011
     :file: tables/carousel_picking.csv
     :header-rows: 1

.. container:: caption

    Data for Carousel picking, 2010-2011.

.. class:: long-metrics
.. csv-table:: Static Picking 2010-2011
     :file: tables/static_picking.csv
     :header-rows: 1

.. container:: caption

    Data for Static picking, 2010-2011.

.. class:: long-metrics
.. csv-table:: Singulate 2010-2011
     :file: tables/singulate.csv
     :header-rows: 1

.. container:: caption

    Data for the singulation process, 2010-2011. The singulation process did
    not exist before week 24 of 2011.

.. class:: long-metrics
.. csv-table:: Multis 2010-2011
     :file: tables/multis.csv
     :header-rows: 1

.. container:: caption

    Data for the multis process, 2010-2011. 

.. class:: long-metrics
.. csv-table:: Singles 2010-2011
     :file: tables/singles.csv
     :header-rows: 1

.. container:: caption

    Data for the singles process, 2010-2011. 

.. class:: long-metrics
.. csv-table:: Shipping 2010-2011
     :file: tables/shipping.csv
     :header-rows: 1

.. container:: caption

    Data for the shipping process, 2010-2011. 

.. class:: long-metrics
.. csv-table:: Outbound 2010-2011
     :file: tables/outbound.csv
     :header-rows: 1

.. container:: caption

    Data for the outbound process, 2010-2011. 

.. class:: long-metrics
.. csv-table:: Receive 2010-2011
     :file: tables/receive.csv
     :header-rows: 1

.. container:: caption

    Data for the receive process, 2010-2011. 

.. class:: long-metrics
.. csv-table:: Carousel Putaway 2010-2011
     :file: tables/carousel_putaway.csv
     :header-rows: 1

.. container:: caption

    Data for the carousel putaway process, 2010-2011. 

.. class:: long-metrics
.. csv-table:: Static Putaway 2010-2011
     :file: tables/static_putaway.csv
     :header-rows: 1

.. container:: caption

    Data for the static putaway process, 2010-2011. 

.. class:: long-metrics
.. csv-table:: Returns 2010-2011
     :file: tables/returns.csv
     :header-rows: 1

.. container:: caption

    Data for the returns process, 2010-2011. 

.. class:: long-metrics
.. csv-table:: Inbound 2010-2011
     :file: tables/returns.csv
     :header-rows: 1

.. container:: caption

    Data for all inbound processes, 2010-2011. 


.. |multiply| unicode:: U+02715 
