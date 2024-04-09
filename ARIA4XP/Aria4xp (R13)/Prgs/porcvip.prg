***********************************************************************
*:  Program file : PORCVIP.PRG
*:  Program desc.: Issue Inter-Location PO, Issue Iner-Location Shipment
*:         System: Aria 4XP
*:      Developer: Khalid Mohi El-Din Mohamed
*:           Date: 09/11/2004
*:      Reference: *N037578,1
*:************************************************************************
*: Passed Parameters  : lcInvType => "0001" for Styles
*:                      lcPType   => 'N' Issue Inter-Location PO
*:*************************************************************************
*: Modifications      :
*:*************************************************************************

*--Issue Inter Company P/O.
lcParameter = "'0001','N'"
oAriaApplication.DoProgram("AWRPOSTREC",lcParameter,.F.,'PO')
