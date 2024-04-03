***********************************************************************
*:  Program file : MFRCVCT.PRG
*:  Program desc.: Receive Cutting Ticket
*:         System: Aria 4XP
*:      Developer: Khalid Mohi El-Din Mohamed
*:           Date: 04/28/2008
*:      Reference:
*:************************************************************************
*: Passed Parameters  : lcInvType => "0001" for Styles
*:                      lcPType   => 'M' Receive Cutting Ticket
*:*************************************************************************
*: Modifications      :
*:*************************************************************************

*--Issue Return Style PO
*lcParameter = "'0001','M'"
*oAriaApplication.DoProgram("AWRPOSTREC",lcParameter,.F.,'MF')
DO (oAriaApplication.ApplicationHome+'POSTREC') WITH '0001','M'
