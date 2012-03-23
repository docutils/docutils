.. role:: my-note

****************
Executive Report
****************

2nd draft
*********

:author: Paul Tremblay 
:organization: Zappos 
:date: 2011-11-29
:abstract: 

        As a result of the purchase in 2009 by Amazon, Zappos had to
        to switch its warehouse management to utilize Amazon’s
        inventory sharing system. This transition, known as the
        cutover, began in 2010, but intensive efforts started in June
        of 2011 and lasted  until September 23 , 2011, the actual date
        of the conversion. Zappos spent 6.5 million dollars in
        replacing equipment to work with Amazon's software and in
        extra labor to handle backlogs created by the change. Zappos
        underwent an intensive testing process to ensure Amazon's
        software would function as expected. In order to test that
        Amazon's software moved products correctly through its
        warehouse, Zappos Fulfillment Center (ZFC) often closed down
        its facility early in the morning and worked through the night
        with its hardware partner, Intelligrated. To test that
        Amazon's and Zappos’ databases would correctly share and
        update information, Zappos set up a mock warehouse known as
        the mini ZFC. The transition did not go smoothly and caused
        several problems. Zappos went over budget. Because Amazon did
        not understand the complexity of integrating Zappos unique
        License Plate Number (LPN) system, it did not develop tools on
        time. Without these tools, Zappos could not properly train its
        employees. It could also not conduct the tests as planned,
        forcing Zappos to switch the transition date from early August
        into late September, when Zappos received most of its
        products. The transition resulted in greater warehouse
        inefficiency and increased costs. ZFC lost flexibility in key
        areas and some of Amazon’s software does not work optimally
        with Zappos’ business model. Nonetheless, the conversion was
        mostly successful. Zappos fulfilled 99 percent of orders
        placed on the weekend of the massive migration, and continues
        shipping orders on time.



============
Introduction
============

Zappos Chief Executive Tony Hsieh described the changes as “Open heart
surgery on a moving train.” With the acquisition by Amazon in 2009,
Zappos switched its entire warehouse system in order to share
inventory with Amazon. Though Zappos had a highly effective management
system which could track hundreds of thousands of items from Nike
shoes to Cuisinart cookware, it had no way of handling merchandise
from other vendors. Amazon software would give them this ability,
granting the shoe retailer a larger marketplace. However, the
integration presented two major challenges. First, it had to ensure
that Zappos would retain the ability to process orders in its unique
way that resulted in incredibly fast shipping, making the company so
successful. Second, the  switch would have be seamless, with no
delay in delivery. The major retailer could not disappoint
customers who had come to rely on Zappos for its legendary service.
The movement of products could not be disrupted even during the
surgery of implementing a new system.


Zappos’ License Plate Number (LPN) System
===========================================

Every process in an automated warehouse that tracks or moves an item
relies on the scan of that item. For other Amazon warehouses, this
number is the so-called FCSKU code. In contrast, Zappos uses  a
License Plate Number, or LPN. Since Amazon tools (software interfaces)
were designed to read and process FCSKU numbers, not LPNs, Amazon had
to develop  new software for Zappos for many warehouse tasks. The 
depth and complexity of Amazon tools meant this process would take two
years. 

LPNs are bar codes unique to each product. Think of two boxes of black
Nike *Musique IV* shoes, size 10, identical in every aspect, down
the length of the shoe laces. They get the same UPC code. But Zappos
assigns them different LPNs, allowing the shoe retailer to track the
item from the time it comes into the warehouse, to when it is put away
on a shelf, to when it leaves in a UPS truck headed to a customer in
Anchorage Alaska 1 month later, and even when a year later, when that
customer decides to return the item to Zappos Fulfillment Center. 

Using LPNs offers several advantages over UPC codes, especially when problem
solving. If a box of shoes falls off a cart, A Zappos worker only needs to
place the item on an available shelf and scan the bar code beneath the shelf.
Under another warehouse system, a worker would have to check every bin to see
which misses that pair of shoes. If a pair of shoes comes through receiving
and there is doubt whether that item has gotten scanned, Zappos only needs to
scan the LPN on the box. Under another warehouse system, an employee would
have to do an inventory by counting the number of similar items scanned
already, then count the same items found already stowed in the warehouse. If
product does not get stored correctly, Zappos can identify the worker that put
those items away and coach him. Another warehouse system could not find the
source of the problem. In ZFC, if an item gets picked (pulled from a shelf),
the LPN would tell exactly who that item should be shipped to, and what
priority the item takes, so that if the lines went down, Zappos could still
direct that box to its right location. Other warehouse systems do not have
this flexibility. (Since the cutover, Zappos has lost this functionality as
well.) When an item gets misrouted, the LPN tells a Zappos team member where
to route the item. Another warehouse must run the item through the lines again
(as Zappos now does, having lost this functionality, as well). 

The advantages of LPNs also extends to customer service. If three
different customers return the same pair of Reebok shoes, Zappos knows
to check that particular product, to determine if the material was
scuffed or the shoelaces discolored.  Another warehouse system would
not know to check the box, perhaps thinking the problem lay with the
brand in general. If a pair of jeans comes back with a wallet in them,
or a box of shoes with marijuana (both real occurrences!), a simple
scan at ZFC tells reveals the customer. Under another warehouse
system, employees might have to do more investigation.

LPNS also offered an easier way to transfer goods between Zappos’
warehouse and outlet stores. Formerly, moving a box of shoes from ZFC
to the outlet stores simply involved scanning in the LPN, as did
shipping large volumes of goods to vendors. Zappos viewed such
transfers almost the same as moving it within the warehouse.  Having
lost this functionality, ZFC must now move goods through shipping, as
if Zappos were a manufacturer and the vendor an outlet store. This
puts a greater strain on the shipping process. It also hinders Zappos
ability to plan for peak, since  Zappos builds its warehouse around
the day it will do the highest volume, and it cannot account for
unexpected vendor returns or demands.

In addition, the LPN system can better handle items from two different
warehouses. Zappos, having warehouses located within ½ mile of each
other, always ships orders of multiple items in the same box, in
contrast to Amazon, which having warehouses all over the country, will
ship items from different warehouses. When Zappos needs to pull items
from different warehouses for the same order, it requires no more than
a worker scanning the LPN from each warehouse, and letting the system 
handle all other routing.

Last, using LPNs helps Zappos manage paying tariffs on certain goods.
As a Foreign Trade Zone entity, ZFC receives certain items directly
from ships, these items not having to pass through customs. ZFC must
then pay the tariff as it ships each item. If an item is shipped out
and the tariff paid, and the item is then returned, Zappos knows not
to pay the tariff again. Other warehouse systems would pay the tariff
twice (though these systems would know not to pay the tariff on the
last pair of shoes to offset paying double on the first). 

Overall, Zappos uses LPNs to provide better customer service. Since the
retailer  has an extremely low tolerance for delays in shipping items
to its customers, it demands a short “cycle time” (the amount of time
required to solve a problem). LPNs allowed Zappos to fulfill this
goal. However, because ZFC has lost some of the functionality of LPNs,
it needs more resources to solve problems. Whereas pre cutover,
Zappos had 2 people for problem solving for outbound, it now has a
whole team with a lead and a team manager.


=======
Testing
=======

Zappos naturally desired to conduct intensive tests to ensure that
when it went live, switching to Amazon's software, the system would
function as expected. Planning began as early as 2009, with Zappos team
members flying out to both Las Vegas, Zappos head quarters, and
Seattle, Amazon's headquarters. These initial meetings determined the
hardware requirements and also outlined general testing procedures.

On site testing began in June, 2011, and followed along two paths,
transactional and warehouse. The transactional phase made
sure Zappos could read and access Amazon's database. Warehouse
testing involved making sure products moved properly through the
warehouse.

Transactional Testing
=====================

Zappos set up what it referred to as the mini ZFC in order to test that
databases of Amazon and Zappos correctly communicated with each other.
A brief overview of the data sharing will make other parts of this
document clearer. 

Zappos and Amazon share information throughout the entire receiving
and shipping process. The sharing begins when Zappos places a
purchasing order, or PO, say, for 100 boxes of Nike shoes. The
information about the PO gets transmitted to the host, Amazon. When
the cartons arrive at the Zappos ZFC, an employee scans the UPC on the
side of the boxes. Amazon receives this information directly,
confirming the arrival of the merchandise. In turn, Amazon sends the
confirmation back to a database at Zappos, where the finance
department increases its assets. Similarly, when a box gets shipped
from the Zappos warehouse, Amazon receives the scan directly,
decreases its inventory by one, and tells the Zappos database to do the
same. The same updating occurs for inventory. If a box of shoes falls
behind the shelf and is hidden from a scan of a member of the Zappos
auditing team, Amazon gets a flag for a lost item, and communicates
this back to Zappos; when the box is found two months later, both
Amazon's and Zappos’ database get updated again.

To be clear, this description explains only part of the database
sharing. For example, it does not detail how the Zappos Customer
Loyalty Team (CLT) might need the tracking number for a concerned
customer. Amazon sends this information to Zappos Site Manager, a
system distinct from Amazon's, and on its own network. Amazon’s system
must have special code to interact with Site Manager.  Also, Amazon
software handles another task besides managing databases—it instructs
the hardware in ZFC how to route items.  This second task will be
discussed in the next section. Nonetheless, the central point remains:
before the cutover, Zappos received information about the arrival and
shipment of an items directly from the scans in its own warehouse.
After the cutover, Zappos would get this data from Amazon. The mini FC
was established to make sure the messaging between Amazon and Zappos
functioned correctly.

Zappos built its mini fulfillment center in a cage on June 24, 2011.
Rather than Versace coats and Roberto Cavalli shoes, the mini
warehouse's inventory consisted of LPN codes printed on cheap 8 ½ by
11 printing paper. This humble operation was later taken over by the
indefatigable Dr. Spankelstein, or Jason Hayes, who used the pieces of
paper as if goods, scanning them to see if Amazon got the message the
goods represented. At first, Jason tested simple processes, such as
the receiving and the shipment of merchandise.  Once these tests
proved successful, Jason tested for more complicated operations, the
exceptions that arose during course of a real day at ZFC. If Zappos
ordered 100 cartons of shoes, but 120 arrived, that should trigger an
overage flag, which tells Zappos to handle that inventory differently.
If a picker discovers damage to the shoes, those shoes should be
pulled, not shipped. If a customer cancels an order, Amazon and Zappos
must know not to ship it. If a customer returns an item, the database
must indicate whether or not he has gotten his refund. In each case,
Jason tested to see that Amazon got and sent the right message. For
example, Jason stood in front of his inventory of papers on a shelf
and intentionally scanned in the wrong number–representing the actual
occurrence when inventory did not match the database—and then verified
that the databases got updated correctly .

Because of problems experienced with the other phase of testing, that
in the warehouse, transactional testing in the mini ZFC started late.
It proceeded slowly. Amazon did not develop the tools on time. Because
of the late start, Zappos did not have enough time to thoroughly test
several processes. One part of the warehouse consists in handling BIG
orders, those orders so big that they can not fit in a single box. ZFC
could only test the messaging to this process twice. Likewise, it only
ran a few successful tests on gift cards before going live.

______________________
Problems with Training
______________________

Amazon's delay in developing tools hurt training for the cutover.
Effective training for a large facility such as Zappos, which employs
over 2,000 workers during its off season, needs four to five weeks.
Effective training also requires that the trainees have hands on
experience using the tools. Last, effective training mandates
consistency, in making sure the trainees' knowledge gets reinforced
first by the uniformity of the training material, and then in having
the actual of job experience match the training.

Amazon's delay in developing tools prevented Zappos from meeting any
of these criteria.  Because Zappos did not have physical tools,
trainees never had to opportunity to practice, to press actual buttons
or go back one screen on a RF gun. Instead, they learned solely by
hours of viewing screen shots presented through Power Point
presentations. Studies show that most people learn effectively by
doing rather than watching, but workers at Zappos never had this
opportunity.

Zappos training was further undermined by the screen shots not
arriving on time. Amazon had made a commitment to deliver these screen
shots by June 15, 2011. On June 13, 2011, a few days earlier, Zappos
expressed concern in an internal meeting that it had not received the
screen shots, but noted that as long as it received them in a few
days, it would “just allow time for makeup sessions.” On July 11,
2011, Zappos still had not gotten these screen shots and expressed
concern at meetings that “training was at risk.” By July 15 of the
same year, Zappos was forced to delay training implementation.
Delaying the implementation meant less time to train all employees.

Last, Zappos training was made less effective by the screen shots not
accurately showing how the tools worked. Because Amazon had not
developed the tools in time, and made changes to them late in to the
testing processes, it also changed the tools' interface, rendering the
screen shots inaccurate.

The inaccuracies between the tools and screens shots were many.

* For the Putaway tools, Amazon made so many updates, none of which showed up
  on the slides, that Zappos had to take down in the warehouse, and then
  dispose, 85 job aids (posters outlining how to use this tool).

* For the singles and multis tools, Amazon did not present any
  information on the light system (Put-To-Light) in its slides. In
  addition, the slides indicated that 2 scanning tools would be
  available per lane, when there was only 1; the slides told team
  members to scan the P-slip when in fact they do not scan it; and the
  team members were given inaccurate information on how to read the flashing
  Put-To-Light system.

* For receiving, the slides did not accurately present the web interface and
  the job aids had be replaced after the cutover.

* For unload, the inaccuracies in the slides forced Zappos to update
  all of the job aids and power point slides after the cutover.

* For returns, the changes in the slides were communicated so late
  that there was no time to retrain members who were trained earlier
  and who got improper instructions.

* OOPS, a trouble shooting tool, presented unique problems. Because so
  few distribution centers use this tool, no information existed for
  its use after cutover. The documentation that was available before
  the cutover indicated buttons and tabs that did not exist, and
  Zappos had so little confidence in the training materials that it
  provided no training before the cutover. After the cutover, ZFC
  re-created slides and job aids.

* For the singulate process, numerous changes occurred after the
  cutover, forcing Zappos to update its training and power point
  presentation; for example, the scanners did not correctly
  “interface”  forcing Zappos to change the training for this process.

* For the Tote Wrangle process, a discrepancy existed between the
  tote/ID and card ID issues. :my-note:`[not clear here]` So many
  changes were made to the process flows and modules after the
  cutover, that none of these flowcharts were valid after the cutover.

* Overall, the updates forced Zappos the change the videos for training on the
  Multis and Singles processes as well as to the videos for Return videos.
  Changes occurred so late that members trained earlier did not get the proper
  training.

Problems with Training for the Manual Drop
___________________________________________

In addition, Zappos could not get proper training for the manual drop part of
its system. “Dropping,” or waving, is warehouse jargon for the order of
processing orders. If Zappos receives ten thousand orders on a busy morning,
it does not blindly put these orders on the floor to get processed all at
once. Doing so would cause problems, amongst other things, overloading the
system and clogging certain stations like multis. Instead, a warehouse
maximizes its efficiency by judiciously releasing orders to the floor
gradually.

Sophisticated software handles waving most effectively, deciding what
to release based on complicated algorithms. Automatic warehouses, such
as those run by Amazon, use such software (usually SAP). In
contrast, a manual warehouse uses a worker, who looks at a computer
screen and releases items based on what he learned in special
training.

Not surprisingly, automatic waving results in more efficiency. An
automatic system maximizes the goods available to a warehouse. If a
warehouse can handle 3,000 singles orders and 1,000 multiple orders,
the computer assures that exactly that number remains available at any
time. This results in higher pick rates, because a picker that has a
large amount of goods to choose from walks less and picks more. An
automatic drop system means less workers having to man picking
stations, which means less training time.  Last, an automatic system
avoids the human mistake of releasing too many goods, overloading the
lines.

Before the cutover, Zappos used a hybrid warehouse. Computers diverted
items by reading codes on boxes, but humans manually waved items for
picking. Zappos had hoped to get an automatic waving system with the
cutover in order to offset what it perceived as inefficiencies (for
example, in the returns process). Unfortunately, three weeks before
the proposed cutover, Amazon announced it could not integrate
automatic waving after all. The problem  lay with the moving carousel
in ZFC, and more specifically, the proprietary software of Diamond
Phoenix that controlled the carousel. Amazon does not use a moving
carousel in any of its warehouses, so did not have any code to
interact with it. Though Amazon initially believed it could
incorporate Diamond Phoenix into its system, the integration turned
out too complicated. 

Without the automatic waving system, ZFC felt its managers needed special
training in learning how to wave goods in an Amazon warehouse. ZFC sent
managers to the Amazon warehouse Lex 1 to get such training. Unfortunately,
Lex 1 did not seem willing to share its knowledge, having the Zappos trainees
folding boxes instead of learning the system. Feeling the managers wasted
their time, ZFC had them return to Shepherdsville after one day. 

Migration
=========

Another aspect of transactional testing involved moving Zappos
inventory to the Amazon database. Zappos needed to develop both a
strategy for transferring the bulk of the six million items, known as
bulk migration, and a strategy for handling special orders that could
not be part of the bulk migration.

Since ZFC would have to shut down its warehouse during the bulk
migration, it needed to ascertain the length of the process, in part
determined by how many threads Zappos could use. (A thread is the
smallest unit of processing that can be scheduled by an operating
system; the more threads that a process has, the faster it will
complete.) A team in Las Vegas, where the actual migration would take
place, initially estimated the migration would require 14 hours. It
tested this claim by a simulation run on June 14, 2011. Zappos
still needed to know how many threads it could have available for the
process. However, as late as August 21, 2011, Amazon still could not
give Zappos an exact answer, stating it would base its decision on a
test the following Saturday.

While the warehouse was shutdown for bulk migration, Zappos needed to
fulfill special orders, those requiring overnight shipping, approximately 15%
of all shipments.  Different strategies  were discussed as early as June
27, 2011, with Zappos eventually settling on a Just in Time (JIT) strategy,
in which a special script, created in Las Vegas, would run in the background,
pulling and separating special orders from bulk migration. On July 29, 2011,
Zappos was a bit frustrated at having no resolution to this problem. 

Warehouse Testing
=================

Zappos also conducted intensive testing on the movement of goods though its
warehouse. A brief overview of how goods move through the warehouse will make
warehouse testing clearer.

__________________________________
Overview of Warehouse Processes
__________________________________

Zappos has 27 Control Panels that make sure the products move to their correct
destination. The flow of products can be quite complex. It starts when large
cartons of goods arrive in the back of semi trucks.  A member of the receiving
team scans the entire box into ZFC's system.  The boxes then move on a
conveyor belt to a receiving station, where an employee opens the carton,
places a LPN on each item, and then scans that LPN. From this point forward,
for the entire history of the product, the LPN will always be associated with
this particular item, just like a social security number is always associated
with a particular person. The employee places the items back on a conveyor
belt. As the items speed along the conveyor belt, they pass under a sorter,
which determines the pick module location, A, B, or C. For example, boots,
being large, cannot fit in the moving carousel, so will be routed to the
static racks. Next, a member of the Putaway team picks up the item, scans it
into his gun, finds an empty location, and then again scans both the LPN on
the box and the bar code on the shelf, assigning the box a location so it can
later be retrieved. From arrival to stowing, a box is scanned and tracked no
fewer than 4 times.

Shipping a good likewise requires routing. First, a computer sends a
message to the picker closest to the item indicating its location. The
picker pulls the box from the shelf, scans it, and places it on a
conveyor belt. Machine sorters read the LPN from the side of the box and
divert it to a shipping station, where a member of the shipping team
scans the LPN, places the product in a larger shipping carton, and
affixes a label to its side. The system sends the shipping carton to
the SLAM line. A box can ship by different means, by UPS overnight, by
UPS ground, or by Fed Ex, to mention a few. The SLAM determines the
shipping means by using its built-in scanner to read the label on the
side of the box.  After affixing a label to the top of the box, the
SLAM routes it to the correct truck. 

For orders that contain more than one item, the routing becomes  more
sophisticated.  Let's imagine that a customer, Jan Freeman, orders a
pair of Roberto Cavalli shoes and a Versace coat. Another customer,
Roger Dawson, orders a Timex watch and a pair of Nike shoes. As with a
single order, the computer finds the picker closest to the items.
Imagine it finds that a single picker is near both the Robert Cavalli
shoes for Jan as well as the Nike shoes for Roger. The picker
retrieves both boxes, and after scanning each LPN, starts them on a
conveyor belt.  Instead of diverting these items directly to shipping,
the sorter sends them to a station called multis, where each item
is put in a bin to await its mate. Imagine that the bin for
the dress shoes is at the end of lane 1, while the location for the
athletic shoes is at the end of lane 8.

Another picker finds the watch and the Versace coat. The watch is too
small, and the coat too soft, to travel along a conveyor belt, so both
items are conveyed in a tote. The tote travels to a station called
singulation, the station that separates and sends items to the correct
location. A member at the singulation team removes the coat and watch
from the tote, scans both, and puts each in its own tote. The system
determines that the coat should be sent to lane 1, and the watch to
lane 8.

When the coat arrives at the end of lane 1, a member of the
multis team scans the LPN. A light on the Put-To-Light system
flashes right below the bin with the Robert Cavalli shoes, directing
the worker to the right bin. Since all the items have arrived for
Jan's order, the light flashes red, telling the employee to push the
items to the back of the bin. On the other side, a member of the
packing team  packs both items in a shipping box and sends the box to
the SLAM line. The SLAM station handles the carton exactly as it does
for a carton for a single item, weighing it, reading the label on the
side, determining its mean of shipping, affixing a label to the top,
and sending it to the right truck.

This brief overview handles many types of  orders, though it does not explain
unusual situations, such as how the system handles overages, when a vendor
sends too many items. 

Note that unlike other Amazon warehouses, Zappos doe not require that goods
move in totes, or larger containers that can hold several items. So long as
the item measures more than six inches, it goes directly onto the conveyor
belt and gets routed to its destination. Only items moving in totes,
approximately 30% of all items, require singulation. All other products
travel directly to shipping or multis.

If the direction of goods gets controlled by a brain, Amazon software,
the actual movement takes place on equipment provided by
Intelligrated, a local company that partners with Zappos to provide
the hardware for the warehouse. A body must learn to communicate with
a brain. Intelligrated equipment must know how to take commands from
Amazon software. To this end, Intelligrated became the third party in
the testing team, joining Zappos and Amazon. Located locally,
:my-note:`C. doesn’t like this word choice` Intelligrated often sent
members to the Zappos Fulfillment Center in Shepherdsville.  

Clearly, the movements of goods through the Zappos Fulfillment Center
requires a sophisticated system, which had functioned  because Zappos
built it orderly, over time. The scans communicated with equipment
Zappos had slowly integrated, by means of computer code the company
had written and modified, as needed. The cutover would change the
system all at once. Zappos software would not get the messages about
incoming cartons; Amazon's software would. Zappos software would not
direct places to store shoes; Amazon's would. Zappos system would not
determine the closest picker to a pair of shoes; Amazon's system
would. Amazon's system, FCSW, would have to communicate with ZFC
hardware and give the right routing directions.

______________________
Initial Phases
______________________

Testing for the warehouse took several phases, from creating a plan,
to testing in a simulated atmosphere, to testing in the real system
itself, in which items were moved in and out of Amazon's databases. In
the first phases, Zappos would plan for the tests themselves,
establishing what needed to be tested, and how those tests took place.
It set the cutover date for August 22, 2011. It established targets
and informed managers of these targets. One goal included informing
Amazon of Zappos’ unique needs.  Another was to determine the bulk
migration strategy (as outlined in the last section). Other goals
included:

* Setting up a vendor strategy for returns.

* Developing a roll out strategy. Zappos would use the roll out
  strategy in case the transition to Amazon's system failed, switching
  back to the system already in place. Ultimately, ZFC determined it
  would probably not need the roll out strategy.

* Outlining a strategy for training, preparing OPS (operational
  management), and planning a schedule for purchasing hardware.
 

By July, the testing began in earnest. Instead of closing down its
warehouse from 4:30 AM to 6:30 AM, the normal time, ZFC closed down as
early as 12:00 midnight. In the early hours of the morning, committed
members of the i2 team probed the new system. This involved first
creating fake products to run through the warehouse, with team members
stuffing “reboxes,” old boxes ruined in shipping, with rolls of duct
tape to give them weight so they would not fall off the conveyor
belts.  To imitate larger totes, which contain such items as baby
shoes or jewelry, members used the totes used in actual shipping, but
filled them with papers on which LPNs had been printed. The mechanical
scanners seemed content enough to read the merchandise as high heels
and boots. The next phase involved Zappos switching from the Zappos
network to the Amazon network by means of running a VLAN script. At
the start, the script took quite a long time to run; towards the end
of testing, ZFC switched much more quickly between the two networks.
Once switched, members moved the bogus items through the ZFC, checking
to see that Amazon got and sent the right messages. Jason Davis from
Zappos would often have a conference call with Amazon and inform them
what items Zappos had placed on a conveyor belt. He would see if
Amazon got the message that an item had been scanned. He would radio
to Zappos and Intelligrated personal what Amazon told him, and in
turn, tell Amazon what happened in the warehouse. 

Zappos gradually increased the complexity of its testing. At first
it tried to get a single item through the warehouse, from picking, to
a singles station and finally through shipping. When it finally
achieved this goal some three weeks into testing, on June 20, 2011,
the team let out a whoop of jubilation. Likewise, it considered it a
victory when the SLAM line read a SPOO label from the side of the box.
At the same time, Zappos experienced many problems. It encountered a
backlog because of closing down the warehouse and internally expressed
the need to hire more help. Most of the processes did not function
properly. For example, when testing the SLAM line, it found that
Amazon assigned the wrong status to each item. Amazon was informed of
this problem and said it would get back to Zappos. Zappos felt it fell
behind schedule, in both getting the system working, and in getting
training material. 

___________________________
Example of a Testing Night
___________________________

On July 22, 2011, ZFC conducted all night testing with Amazon and its
hardware partner Intelligrated. A description of this test will give a
good example of the procedure and problems the testing team
encountered. ZFC made several preparation prior to testing. In order
to assure hardware could communicate with the host Amazon, it set up
the proper hardware with Cisco switches  It got the Avalanche software
downloaded to the RF Guns. (RF Guns, like a laptop, need an operating
system to function.) Unfortunately, Zappos made these preparations in
vain. Amazon communicated to Zappos that these tests could not go
forward until four days later, on July 26, because of problems with
the Amazon system. This meant that Zappos could only test conveyance
processes, those processes that guided the Zappos warehouse equipment.
It could not test the databases themselves.

Zappos realized another problem with the RF guns. Amazon required that
any scanning guns interacting with their system be equipped with WPA
(Wifi Protected Access) security. Zappos current model, the 9060, did
not have this ability, forcing the shoe retailer to acquire the 9090
models. The newer models had the potential of introducing another
incompatibility, as ZFC feared that they would not interact properly
with Diamond Phoenix, the software that operated the carousel, or
moving racks. A group of engineers in Shepherdsville felt a small
sense of exultation when it got the 9090s to work with the
Diamond Phoenix system. Later, ZFC had to make another decision
regarding the guns.  It needed over 250 of the new 9090s to operate
its warehouse, a purchase that required approximately $450,000 in
expenditures.  Not wanting to use this much capital, Zappos decided to
lease the guns instead.

For the night of the actual testing, Friday, July 22, a team
consisting of nine personnel from Intelligrated was present, as was a
team consisting of 4 members from Amazon. At 10:00 PM, ZFC turned off
the scanner and conducted some preliminary testing with Intelligrated.
At 12:00 AM, ZFC initiated a VLAN script, a small program that
switched the machines from communicating with Zappos to Amazon's
software. The script took a full 35 minutes to initiate, and then for
some unknown reason, did not connect control panels 1 and 2. The
testing teams placed the totes on the conveyors and checked to see
that the Amazon software directed them to the right place. At 1:00 AM
the tests halted because Zappos could not get the correct messages
from Amazon, and the testing could not resume again until 2:30 AM,
when it was determined that Amazon had blocked a port through
firewall.

Zappos resumed the test with singulation. The SICK scanner could not read the
label correctly, at least initially. When it did read the label correctly,
Amazon did not properly send a message correctly to ZFC to move the tote
along.  Intelligrated pointed out that it correctly released the totes after
nine seconds of no read, exactly according to the Amazon specification.
Intelligrated noted that it would take some time to resolve this problem.

At 4:00 AM, the testing then moved to rebin, where items that belong together
are put together in a bin. In testing, the teams discovered that the lights in
the Put-To-Light system, did not function properly. None-the-less, these tests
were mostly successful.

Meanwhile, a group of dedicated Intelligrated engineers had been
working on fixing the lack of communication in the singulation stage.
At 6:45 AM, believing the problem fixed, the team again ran the test,
this time with success, marking a small triumph after a night of mixed
results.

At 9:15 AM, the testing team held a meeting to discuss the results of
the night. Zappos expressed apprehension that the lights from the
warehouse would overpower the Put-To-Light system, though this turned
out not to be the case. Amazon suggested that Intelligrated use the
time from 4:00 AM until 7:00 AM to test hardware in the system. It
was discovered that the singulate process requires two types of totes,
inbound totes, and trays for inducted items. ZFC discussed the need to
options for re-labeling portions of current tote stock or procuring
additional totes. 

Amazon stated that the communication problems in the singulation
process between Intelligrated machines and Amazon software lay
entirely with Intelligrated. Members of the other teams felt this
assessment both inaccurate and not helpful. Zappos felt equally
frustrated when, two days later, Amazon repeated this statement in a
conference call.

_________________________________
General Problems with Testing
_________________________________

Intensive testing continued right up to the cutover, as in the dark of
the morning Zappos members put boxes on lines testing every process
and divert possible. By Friday, August 26, 2011, ZFC reported that
significant progress in testing. None-the-less, a new cutover date was
announced to September 15, since many processes did not function with
Amazon software. Zappos prepared for the next large testing period,
when Amazon would visit for several weeks, and over 50 people would be
present, 40 from the Zappos headquarters in Las Vegas, and 10 from the
Amazon team in Seattle. Zappos created a “war room,” with at least
five phone lines and 6-8 dedicated ports, set up on the mezzanine for
the Amazon team. By Monday, August 29, 2011, lack of functionality
forced ZFC changed its cutover date to September 18, 2011. This date
got changed again to September 23, 2011.

________________
Lack of Support
________________

Throughout  much of the testing period, Zappos did not get the
support it needed. Two examples illustrate this problem. In testing
the SLAM line, ZFC successfully processed a box and got a label
printed, but then could not get it diverted to the right truck,
instead getting a “900” error message. A call to Amazon  proved
fruitless, as Amazon’s routing team insisted its routing worked
exactly as expected. The routing team instructed ZFC to get
Intelligrated to solve the problem. However, Intelligrated could not
proceed, since the “900” message proved completely unintelligible,
giving no hints on what to do. The testing team proceeded blindly,
changing parameters that had nothing to do with the problem, and
getting the same result. After several night and many phone calls, ZFC
was connected to the shipping department, which informed ZFC
that it needed to configure the SLAM line for lanes and destinations.
Only after getting this advice could ZFC get this process to work.

The second illustration also occurred when testing SLAM. Right before
reaching SLAM, a box is packed with its contents and a p-slip. The
computers must then divert the box to the right SLAM station and print
a label. In testing, neither process occurred. Amazon advised ZFC to
focus just on the lack of a shipping label, and when that did not
solve the problem, to focus on the box not getting correctly
diverted. Numerous calls to Amazon did not yield any specifics beyond
these vague generalities. The testing team came to the warehouse
every morning at 1:00 AM, frustrated at not knowing how to proceed.
Only after five nights did Amazon  find the source of the
problem. In order for the scanner to read the label correctly, the
label needs to start with the letter “S,” instead of the “D” Zappos
had been using. With the letter changed, the box diverted correctly
and the label got printed.  (However, even now the labels present a
difficulty for ZFC.  In order to look up an item for trouble shooting,
a worker must remove the “S” from the label and substitute a “D.”)

The lack of clear communication resulted because of the uniqueness of
Zappos’ warehouses and the compartmentalization of Amazon. In contrast
to other Amazon warehouses, Zappos uses an encrypted label peeled off
the p-slip and applied to the side of the box, presenting a problem
Amazon’s technical team did not normally encounter. The routing team
handled the “900” message the best it could. As far as it knew,
routing worked correctly, exactly as expected, so if felt it could do
no more than convey this information to ZFC. It did not know the
problem and its solution lay with another department, shipping. The
other problems ZFC experienced followed a similar pattern. Amazon told
Zappos that its software would work out of the box, as is, but often
it didn’t. When the testing team asked Amazon for details, Amazon
responded that the software worked as expected.

________________________________________
Lack of Volume Testing Affects Cutover
________________________________________

On the first weekend in October of 2011, Zappos experienced what the
press called a rare “hiccup,” a “one-off” event. Customers had to wait
more than two days to get their orders. The unusualness of this event
made some commentators note that even Zappos was not “perfect”
(Reuters, Oct. 7, 2011). 

The delay in shipping happened during the transition of warehouse
management systems. Zappos had tried to plan for every contingency,
testing every divert and process before making the actual transition.
However, it did not test high volume. The largest amount of goods
tested in one night was 500 units. To give an idea of the smallness of
this number, consider that during peak, ZFC can process up to 10,000
units an hour. The tests in no way covered what ZFC would really have
to handle.

The lack of volume hid from the testing team a weakness that would
cause Intelligrated servers to crash. Most hardware vendors build
their equipment for warehouses that move their items in a tote, which
carries approximately 15 items. As the tote moves through the line, it
sends one message for all of these items. In contrast, Zappos moves
most of its items by themselves. 15 items send out their own messages,
15 times the amount of other warehouses. This massive volume
overwhelmed the unexpected Intelligrated servers, shutting down the
entire warehouse. Intelligrated solved the problem by shortening the
logs associated with this messaging, but Zappos reputation for flawless
service got tainted, if only for the short term.

The press noted that Zappos would “learn” from the experience. But it
is difficult to imagine the lesson. Zappos i2 shop master leader Scott
Zachow noted that he cannot envision a test for volume, when it took a
whole team several hours to construct 500 test boxes, and a complete
test would require at 20 times, if not 100 times as many. Superb
industrial engineer and sometimes roller derby girl Becka Embry has
suggested that other transitions would want to devise a test to
simulate the tens of thousands of messages sent and received between
host and server.


=======================
Changes in Productivity
=======================

ZFC experienced two types of changes in 2011. The first category
encompasses ZFC moving product in warehouse one to warehouse two in
order to accommodate Amazon inventory as well as ZFC expanding warehouse two
to accommodate Zappos growth. The second category encompasses the
change from the Zappos WMS to the Amazon WMS. While ZFC has analyzed
the affects of both types of changes, this report focuses on just the
second category, the affects of the Amazon WMS.

Some of the inefficiency resulted because ZFC had to learn the new
system. For example, apparel, being soft, cannot be placed directly on
a conveyor belt, but must move in a tote. For several weeks after the
cutover workers did not understand the process of picking to a tote
and placed the tote on the conveyor belt closing out the process on
their scanning guns. This error created totes with not destination,
forcing the problem solving team to determine their location. 

Other inefficiency resulted because of Amazon software. As mentioned
earlier, the switch to the Amazon WMS has created a greater burden for
problem solving, forcing ZFC to create complete teams that were not
needed before the cutover. The following graph quantifies the amount
of extra labor. 

.. figure:: charts/problem_solve_2010_2011.png
   :height: 2.6in
   :width: 5.73in
   :alt: Problem Solving, 2010, 2011
   :name: problem-solve-graph

   Percentage of hours spent problem solving compared to total hours.
   Problem solving increased 1.5% after the cutover.


The graph measures the amount of hours used to problem solves in terms
of a percentage of the total hours used for all processes. Prior to
the cutover in 2011, ZFC kept this percentage at the same
rate as the previous year. However, after the cutover, the percentage
increased at least 1.5%. (This figure is based on on a 95% confidence level.) 
This resulted in 35,027 extra hours, or $563,935.00 in extra labor
costs.

There was also a change in inefficiency for the receive process.  The
Amazon WMS requires the extra step of converting the PO on the side of
the box to an Amazon PO. Whereas formerly a ZFC team member only had
to scan the side of the box before putting it on a conveyor belt, now
he must apply and scan a case sticker, and then associate the case
sticker to an Amazon PO by means of PO wrapper tool provided by
Amazon. In some cases this involves scanning a code on a separate
sheets of paper. These extra steps have increased the amount of
indirect labor. The extra hours amounted to at least 19,222, or
$309,474. 

.. figure:: charts/receive_rate_adjusted.png
   :height: 2.6in
   :width: 5.73in
   :alt: Receive Rate with Adjustment, 2011

   Rates for the receiving process, 2011, with an adjustment line
   indicating where the rate would have been if the extra indirect
   hours were removed. The gray area between the curves represents the
   extra inefficiency. The first decline, labeled “migration,” occurred
   when Zappos moved product from warehouse 1 to warehouse 2. The
   second decline, labeled “cutover,” resulted because ZFC hired a
   large number of workers in anticipation of the transition to Amazon
   WMS. All processes saw the same two declines.
   _`receive-cost-graph`

Other processes suffered in a similar way to problem solving, but
because these processes also underwent changes at the same time as the
cutover, it is difficult to exactly quantify the costs. For example, a
pair of boots requires extra labor when going through the singulation
station. Because the boots won’t fit in a tote, a worker must write
out the LPN by hand on a piece of paper and place that paper in place
of the boot in the tote. When that tote arrives at multis, a runner
must walk to the singulate station to manually retrieve the
merchandise. Under the old system, a worker knew the destination of
the boots as soon as they were picked and could send them directly to
the multis station. Yet, even despite the obviousness of this
inefficiency, ZFC cannot determine the exact decline in rate because
the singulation station just began operating midway through 2011, and ZFC
cannot compare it to last year, or even make a pre- to post- cutover
comparison. 

Rates for the return process also suffered. On the one hand, this
decline resulted because of the disruption in building warehouse 2, in which
returns lost their outbound lines, as well as because of the move to a
new warehouse. On the other hand, rates declined because of Amazon’s
tools. Products sold through Zappos and then returned have to be
processed with ZFC’s tool, site manager. In contrast, products sold
since the cutover, through Amazon, and then returned, are processed by
Amazon software. The two types of returns force workers to switch
between two screens (an extra step that will disappear after a year).
The new screen also requires more time.  As members of the return team
process items at their station, they answer questions about the
quality of the item. Five of these questions don’t apply to the ZFC
returns process, such as if the product needs to be repackaged or if
has been returned with manuals, yet workers must answer them anyway.
The transition from question to question can sometimes move slow,
showing the user a spinning wheel. 

Login for returns also requires more time, since users must go through
a full 23 steps to login. Last, getting passwords from Amazon for new
users can take as long as three days.  During peak season, the returns
department can have one hundred new temporary workers walk through the
door in the morning, but these employees can’t perform their tasks
without a password. 


=====
Costs
=====


Zappos spent $6.5 million for the cutover. Keep in mind that some of
these costs benefited Zappos. For example, ZFC may have installed the
Put-To-Light system, regardless of the cutover. In addition, ZFC had
to increase the number of singulate stations to handle the increased
volume when moving inventory to warehouse 2 from warehouse 1, which
housed Amazon product. While these singulation stations increased
overall efficiency, ZFC was forced to make the changes without any
planning, increasing the cost of installation, and not allowing better
thought-out implementation.  

Expenses resulted from having to install a new network system to
access Amazon’s network and from having to purchase new hardware to
accommodate Amazon’s specifications, such as switching from PANDA to
SLAM shipping lanes; leasing or buying scanning guns that met Amazon’s
security requirements; and having to acquire new printing heads for
the Zebra printers that printed at 300 DPI because of the
incompatibility of the heads that printed at 200 DPI. Zappos did not
expect to have to purchase much of this hardware, so went over budget.

 

.. class:: overall-costs
.. csv-table:: Overall Costs
   :widths: 15, 15
   :file: tables/total_costs.csv
   :header-rows: 1


.. container:: caption

    Costs for cutover in dollars. Systems costs refers to those costs for installing an
    additional network in ZFC, such as wiring and IDF switches. IT refers to both the
    software and hardware that cost under $100,000 that make the warehouse work. Examples
    include RF guns and the software to operate them. Labor costs refer to the extra labor
    needed to clear backlogs. See the appendix for details.

.. class:: Glossary

========
Glossary
========

:CLT: Zappos Customer Loyalty Team, the team devoted to providing service to
      Zappos customers.

:Cutover: The transition from Zappos warehouse management to Amazon warehouse
          management.

:Diamond Phoenix: Company that provides software for ZFC’s moving carousel.

:Dropping: Releasing an item from a queue in a database to a floor so
           the units can be processed. Syngamous with picking.

:FCSW: Fulfillment Center Software. In the context of this document, Amazon’s
       software, as opposed to Zappos’s software.

:Intelligrated: Company that provided the hardware and support for ZFC.

:Job aids: Posters hung by work stations to guide Zappos team members
           on how to perform certain tasks.

:Licence Plate Number: A number assigned by Zappos to route and track its
                       products. Unlike other systems, an LPN is unique to each product, even
                       identical products.

:Multilation: A station in a Zappos warehouse that handles goods for multiple
              orders. 

:OOPS: A trouble shooting tool.

:P-slip: A packaging slip Zappos includes with each order. [Beef this
         up!]

:PANDA: Print and Apply label. The machines used by ZFC before switching to
        SLAM.

:Picking: Retrieving an item from the warehouse.

:Put-To-Light: A system of lights in the multis station that directs a
               worker to the right bin.

:SICK: Scanner used by Zappos warehouse.

:Singles: Items consisting of a single order, shipped and processed by
          themselves.

:Singulation: A station in Zappos warehouse that separates items in totes and
              routes them to their correct location.

:SLAM: Ship Label and Apply Manifest. The SLAM lines ae machines that
       apply labels and created manifests for items shipping from the Zappos
       warehouse. 

:SPOO: A SPOO label connects different boxes shipped from the same
       order.

:Tool: In the context of this document, any software (usually the interface of that
        software, such as a web interface) to facilitate a task. For example, a scanning
        tool means the software loaded on a gun that allows a worker to scan items, pick
        items, etc.

:Throughput rate: The rate found by dividing the total units shipped
                  divided by the number of man hours required to ship those items.

:Waving: See “picking.”

:ZFC: Zappos Fulfilment Center in Shepherdsville, KY. Syngamous with Zappos for
      the most part, except where specifically noted.



.. class:: appendix

=====
Costs
=====


.. class:: hardware-costs
.. csv-table:: System Costs
     :file: tables/sys_costs.csv
     :header-rows: 1
     :name: table-costs1


.. class:: hardware-costs
.. csv-table:: Hardware Costs
     :file: tables/hardware_costs.csv
     :header-rows: 1


.. class:: hardware-costs
.. csv-table:: Other Costs
     :file: tables/other_costs.csv
     :header-rows: 1


.. class:: problem-solving
.. csv-table:: Hours for Problem Solving
     :file: tables/problem_solve_hours_sum.csv
     :header-rows: 1


