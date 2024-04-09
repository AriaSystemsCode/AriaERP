************************************************************************
* Modifications :_
*
*B500790,1  MAN 06/21/95 Changing the numeric fields picture in the FRX
*B500790,1               to print the commas   
*
*E300266,6 Reham 08/08/95 In (APBANKES.FRX) call func. gfGetAdr() to print
*E300266,6                the right format.
*
************************************************************************

SET ORDER TO TAG BANKCODE  IN APBANKS
SET ORDER TO TAG BANKCHECK IN APCHECKS
SELECT APBANKS
SET RELATION TO CBNKCODE INTO APCHECKS ADDITIVE
SET SKIP TO APCHECKS

DO gfDispRe WITH EVAL('LCRPFORM'),'FOR '+lcRpExp

SET RELATION TO
