***********************************************************************
*:  Program file : PORCVRT.PRG
*:  Program desc.: Issue Return Style PO
*:         System: Aria 4XP
*:      Developer: Khalid Mohi El-Din Mohamed
*:           Date: 09/11/2004
*:      Reference: 
*:************************************************************************
*: Passed Parameters  : lcInvType => "0001" for Styles
*:                      lcPType   => 'R' Issue Reurn Style PO
*:*************************************************************************
*: Modifications      :
*:*************************************************************************

*--Issue Return Style PO
lcParameter = "'0001','R'"
oAriaApplication.DoProgram("AWRPOSTREC",lcParameter,.F.,'PO')
