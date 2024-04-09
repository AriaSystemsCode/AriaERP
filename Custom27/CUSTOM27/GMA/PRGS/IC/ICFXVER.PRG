*:************************************************************
*: Program file  : ICFIXVER.PRG
*: Program desc. : Fix data in SPCK_HDR,SPCK_LIN files to meet the version customization 
*:                 of GMA 
*: For screen    : Menu
*:        System : Aria Advantage Series.
*:        Module : INVENTORY CONTROL (IC).
*:     Developer : tarek Mohammed Ibrahim TMI
*:     Entry     : C101945,1
*:************************************************************
*: Passed Parameters  : None
*:************************************************************
*: Example : DO icPack
*:************************************************************
*: Modifications :
*:************************************************************


lnRespns = gfModalGen('QRM00000B00006','DIALOG',.F.,.F.,;
                      'This program will update version in Packs file, Are you sure to continue?')

IF lnRespns = 1
  *--Open needed files
  =gfOpenFile(gcDataDir+'SPCK_HDR','','SH')
  =gfOpenFile(gcDataDir+'SPCK_LIN','','SH')
  
  *--Update SPCK_HDR file
  SELECT SPCK_HDR
  REPLACE CPKVERSION WITH '   1' FOR EMPTY(CPKVERSION)
  
  *--Update SPCK_LIN file
  SELECT SPCK_LIN
  REPLACE CPKVERSION WITH '   1' FOR EMPTY(CPKVERSION)
  
  
  *--CLOSE Opened files
  USE IN  SPCK_HDR
  USE IN  SPCK_LIN
  
  =gfModalGen('INM00000B00000','DIALOG',.F.,.F.,;
                      "All packs are updated with version '1'" )
ENDIF                      