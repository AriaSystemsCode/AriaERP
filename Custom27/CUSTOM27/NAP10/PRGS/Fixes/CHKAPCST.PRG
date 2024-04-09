
*PoNo =
*' CostItem= 

SELECT APINVTKT 
SET ORDER TO 2     && cimtyp+ctktno+STR(lineno,6)+crsession 

SELECT BOMCOST
set order to 2   && cbomtype+cimtyp+ctktno+item+iclr+mfgcode+cwarecode+cdyelot+crsession+cisession

SUM NTOTACST   to BCost  FOR EVAL(KEY()) = CostItem+"I"+PoNo 

SELECT APINVTKT 
SUM nApApLAmnt to ACost  FOR EVAL(KEY()) = "S"+PoNo 

CLEA

? bCost
? aCost

 