lParameters lcInfFile
lcClassLibSet = SET('Classlib')
SET CLASSLIB TO COPYOBJ ADDI
oCopyObject = CREATEOBJECT('COPYOBJ',lcInfFile)
RELEASE oCopyObject
SET CLASSLIB TO &lcClassLibSet
