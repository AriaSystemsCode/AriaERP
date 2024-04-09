set defa to r:\aria27\dbfs\02
use apvinvdt share in 0 orde  1
use bomcost share in 0 orde 1
sele 2
SET ORDER TO TAG Bomcstkt OF r:\aria27\dbfs\02\bomcost.cdx
use r:\aria26\dat2\bomcost share in 0 orde 1  alia  bom26
use r:\aria26\data2\bomcost share in 0 orde 1  alia  bom26
sele 3
SET ORDER TO TAG Bomcstkt OF r:\aria26\data2\bomcost.cdx
sele 2
brw
brow
sum nTotCst to B27  for eval(key()) = "1I411006"
? b27
sele 3
sum nTotCst to B26  for eval(key()) = "1411006"
?b26
?b27-b26
sele 1
set orde to 2
brow
sum nApApRAmnt  to a  for eval(key()) = "S411006" FOR cbomtype <> '5'
sum nApApRAmnt  to a  for eval(key()) = "S411006" and  cbomtype <> '5'
?a


*******


SELET BOMCOST 

SUM NTOTCST TO BMcST   FOR eval(key()) = CiTEM  +"I411006"  and cApInvNo = '612097501   '


select apinvtkt 
SUM nApAplAmnt  TO ap FOR eval(key()) = "S"+pono  and   cApInvNo = invno 
CLEA 
?BMCST
?AP 