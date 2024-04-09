***********************************************************************
*:  Program file : MAMNREC.prg
*:  Program desc.: Receive MMO
*:         System: Aria 4XP
*:      Developer: Mariam Mazhar(MMT)
*:           Date: 09/30/2012
*:      Reference:
*:************************************************************************
*: Passed Parameters  : lcInvType => "0002" for Materail
*:                      lcPType   => 'W' Receive MMO
*:*************************************************************************
*: Modifications      :
*: N037687,1 MMT 09/30/2012 Convert Receive Material Manufacturing Order to Aria 4.0 XP[T20110914.0019]
*:*************************************************************************

*--Issue Return Style PO
lcParameter = "'0002','W'"
oAriaApplication.DoProgram("AWRPOSTREC",lcParameter,.F.,'PO')
