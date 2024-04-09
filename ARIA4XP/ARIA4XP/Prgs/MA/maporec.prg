***********************************************************************
*:  Program file : MAPOREC.PRG
*:  Program desc.: Receive Material PO
*:         System: Aria 4XP
*:      Developer: Khalid Mohi El-Din Mohamed
*:           Date: 09/11/2004
*:      Reference: *N037578,1
*:************************************************************************
*: Passed Parameters  : lcInvType => "0002" for Styles
*:                      lcPType   => 'P' Receive Material PO
*:*************************************************************************
*: Modifications      :
*:*************************************************************************

*-- Receive Material PO

*AMH Conf. changes [Start]
*lcParameter = "'0002','P'"
*oAriaApplication.DoProgram("AWRPOSTREC",lcParameter,.F.,'PO')
DO (oAriaApplication.ApplicationHome+'POSTREC.FXP') WITH '0002','P'
*AMH Conf. changes [End]
