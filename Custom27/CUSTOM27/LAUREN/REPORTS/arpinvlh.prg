***************************************************************
*: Program file  : ARPINVL.PRG
*: Program desc. : Custom Invoice Form For Lauren hansen
*: Module        : ACCOUNTS RECEIVABLES
*:         System: Aria Apparel System
*:      Developer: IHB
*:*************************************************************
*: Calls 		 : NONE
*:         Functions  : NONE
*:*************************************************************
*: Reference : This Program Is Due To C # 101506
*:*************************************************************

*-- Open required file
IF !USED('SalesRep')
  llRepFile = gfOpenFile(gcDataDir+'SalesRep',gcDataDir+'SalesRep','SH')
ENDIF
SELECT INVHDR
