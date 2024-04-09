*:***************************************************************************
*: Program file  : ALPIKREV
*: Program desc. : OUTSTANDING PICK TICKET REPORT FOR (REV03) (C102463)
*: System        : Aria Advantage Series.
*: Module        : ALLOCATION (AL)
*: Developer     : Hend Ghanem (HBG)
*: Date          : 22/10/2001
*:***************************************************************************
*C129625,1 MMR 09/21/2005 Adding Option to Print Factored/Non Factored /Both.
*:***************************************************************************
STORE '' TO lcTitle
STORE .F. To llEndReprt 
lcStyHeadr = gfItemMask('HI')
lcTime = TIME()

IF lcRpSortBy = 'C'   && If sort by Complete Date.
  lcTitle = 'Sort by complete date'
ELSE
  IF lcRpSortBy = 'S'  && If Sort by Start Date. 
    lcTitle = 'Sort by start date'
  ELSE   && If Sort by Piktkt #.
    lcTitle = 'Sort by Pick ticket number'
  ENDIF
ENDIF    

IF llOGFltCh
  DO lfCreaTemp   && Create Temp file to collect data in
  DO lfCollect    && Collect data
ENDIF

SELECT (lcTempFile)
GO BOTTOM
REPLACE &lcTempFile..lEndRep WITH .T.

LOCATE
IF EOF(lcTempFile)
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN
ENDIF

DO gfDispRe WITH EVALUATE('lcRpName') 

*!*************************************************************
*! Name      : lfCreaTemp  
*: Developer : Hend Ghanem (HBG)
*: Date      : 22/10/2001
*! Purpose   : Create Temp file to collect data in
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Example   : =lfCreaTemp()
*!*************************************************************
FUNCTION lfCreaTemp  


*-- check If File is created or not
IF USED(lcTempFile) AND RECCOUNT(lcTempFile) > 0
  USE IN (lcTempFile)

ENDIF
*-- Create File
IF !USED(lcTempFile)
  
  lnI = 1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'PikTkt'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 6
  laTempStru[lnI,4] = 0
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'cWareCode'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 6
  laTempStru[lnI,4] = 0
    
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'Account'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 5
  laTempStru[lnI,4] = 0
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'Store'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 8
  laTempStru[lnI,4] = 0

  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'DistCent'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 8
  laTempStru[lnI,4] = 0
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'ShpToNam'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 30
  laTempStru[lnI,4] = 0
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'Date'
  laTempStru[lnI,2] = 'D'
  laTempStru[lnI,3] = 8
  laTempStru[lnI,4] = 0

  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'Order'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 6
  laTempStru[lnI,4] = 0
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'BolNum'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 6
  laTempStru[lnI,4] = 0

  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'Style'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 19
  laTempStru[lnI,4] = 0

  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'Start'
  laTempStru[lnI,2] = 'D'
  laTempStru[lnI,3] = 8
  laTempStru[lnI,4] = 0

  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'Complete'
  laTempStru[lnI,2] = 'D'
  laTempStru[lnI,3] = 8
  laTempStru[lnI,4] = 0

  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'TotPik'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 6
  laTempStru[lnI,4] = 0

  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'Age'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 6
  laTempStru[lnI,4] = 0

  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'lEndRep'
  laTempStru[lnI,2] = 'L'
  laTempStru[lnI,3] = 1
  laTempStru[lnI,4] = 0
*ash1
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'Printed'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 1
  laTempStru[lnI,4] = 0

  CREATE CURSOR (lcTempFile) FROM ARRAY laTempStru          
  ZAP
 
  IF lcRpSortBy = 'C'   && If sort by Complete Date.
    INDEX ON  cWareCode + DTOS(COMPLETE) + PIKTKT  TAG (lcTempFile) OF (gcWorkDir + lcTempFile+ ".CDX")
  ELSE
    IF lcRpSortBy = 'S'  && If Sort by Start Date. 
      INDEX ON  cWareCode + DTOS(START) + PIKTKT  TAG (lcTempFile) OF (gcWorkDir + lcTempFile+ ".CDX")
    ELSE   && If Sort by Piktkt #.
      INDEX ON  cWareCode + PIKTKT TAG (lcTempFile) OF (gcWorkDir + lcTempFile+ ".CDX")
    ENDIF
  ENDIF    
  
  *-- Hend 12/17/2001 Open files if not opened before [Begin]
  *=gfOpenFile(gcDataDir+'WareHous',gcDataDir+'WareHous','SH')
  *=gfOpenFile(gcDataDir+'Pack_hdr',gcDataDir+'Pack_hdr','SH')
  IF !USED("WareHous")
    =gfOpenFile(gcDataDir+'WareHous',gcDataDir+'WareHous','SH')
  ENDIF
  
  IF !USED('Pack_hdr')  
    =gfOpenFile(gcDataDir+'Pack_hdr',gcDataDir+'Pack_hdr','SH')
  ENDIF  
  *-- Hend 12/17/2001 Open files if not opened before [End  ]
  
ENDIF  

*!*************************************************************
*! Name      : lfCollect    
*: Developer : Hend Ghanem (HBG)
*: Date      : 22/10/2001
*! Purpose   : Create Temp file to collect data in
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Example   : =lfCollect()
*!*************************************************************
FUNCTION lfCollect    

SELECT ORDLINE
SET RELATION TO PIKTKT INTO PIKTKT ADDITIVE

ldDate = gdSysDate

LOCATE
*C129625,1 MMR 09/21/2005 Adding Option to Print Factored/Non Factored /Both.[Start]
SET RELATION TO 'O'+ORDER INTO ORDHDR ADDITIVE
DO CASE
 CASE lcPrntFactr='Y'
   lcRpExp=lcRpExp+ ".AND. !EMPTY(ORDHDR.CFACCODE)"
 CASE lcPrntFactr='N' 
   lcRpExp=lcRpExp+ ".AND. EMPTY(ORDHDR.CFACCODE)"
 OTHERWISE
   lcRpExp=lcRpExp  
ENDCASE   
*C129625,1 MMR.[End]
SCAN FOR &lcRpExp AND (!EMPTY(PIKTKT) AND PIKTKT <> '******')
  SCATTER MEMVAR MEMO
  m.Store = IIF(EMPTY(ORDLINE.STORE),ORDLINE.Account,ORDLINE.Store) 
  m.cWareCode = IIF(SEEK(PikTkt.cWareCode,'Warehous'),Warehous.cDesc,'')
  =SEEK(IIF(EMPTY(ORDLINE.STORE),'M'+ Account,'S'+ Account + Store),'CUSTOMER')
  m.DistCent  = IIF(EMPTY(ORDLINE.STORE),'',CUSTOMER.dist_ctr)
  m.ShpToNam  = CUSTOMER.StName
  
  m.Date      = PIKTKT.Date
  m.Age       = ldDate - PIKTKT.Date
  m.BolNum    = IIF(SEEK(Piktkt,'Pack_Hdr'),Pack_Hdr.bill_ladg,'')
  m.lEndRep   = .F.
  *ash1
  m.Printed   = IIF(PIKTKT.prtflag='P','Y','N')
  INSERT INTO (lcTempFile) FROM MEMVAR
ENDSCAN

SET RELATION TO  
*-- end of lpColect.

*!*************************************************************
*! Name      : lfsrPkt
*! Developer : Hend Ghanem (HBG)
*! Date      : 22/10/2001
*! Purpose   : Change account flag, in range browse screen.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example   : =lfsrPkt()
*!*************************************************************
*! Note      : S symbol is [S,Set] , R symbol is Reset
*!*************************************************************
FUNCTION lfsrPkt
PARAMETERS lcParm
SELECT PIKTKT
GO TOP
*-- End of lfsrPkt.


*!**************************************************************************
*! Name      : lfDummy
*! Developer : Hend Ghanem (HBG)
*! Date      : 22/10/2001
*! Purpose   : To check if report end or not
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Called from        : ALPIKREV.FRX
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example            : lfDummy()
*!**************************************************************************
*! Note               : 
*!**************************************************************************
FUNCTION lfDummy

PRIVATE lnCurAlias

lnCurAlias = SELECT(0)
llEndReprt =  &lcTempFile..lEndRep 

SELECT (lnCurAlias)
RETURN 0
