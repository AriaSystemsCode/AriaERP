***********************************************************************
*:  Program file : MAREREC.PRG
*:  Program desc.: Issue Material PO
*:         System: Aria 4XP
*:      Developer: Khalid Mohi El-Din Mohamed
*:           Date: 01/12/2005
*:      Reference:
*:************************************************************************
*: Passed Parameters  : lcInvType => "0002" for Styles
*:                      lcPType   => 'G' Issue Material PO
*:*************************************************************************
*: Modifications      :
*:*************************************************************************

*-- Issue Material PO
lcParameter = "'0002','G'"
oAriaApplication.DoProgram("AWRPOSTREC",lcParameter,.F.,'PO')
