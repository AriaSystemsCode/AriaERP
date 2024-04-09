*!**************************************************************************
*: Name      : APPMAIN.PRG
*: Developer : TAREK MOHAMED IBRAHIM - TMI
*: Date      : 03/28/2004
*: Purpose   : Custom BAR Code Reciving by shipment for APP10
*: ENTRY #   : C121114,1
*:**************************************************************************
*: Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*:             lcFunPars -> Process function parameters, sent as a string.
*:**************************************************************************
PARAMETER lcEvntFun,lcFunPars

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLT(lcEvntFun)+'('+lcFunPars+')'

*--Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue

*:**************************************************************************
*:* Name        : lfIGNRPO
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/28/2004
*:* Purpose     : Ignore checking Po
*:***************************************************************************
*C121114,1
FUNCTION lfIGNRPO
PRIVATE lnSlct,lcAppVr
lnSlct = SELECT()

IF lcTranTyp = 'I'
  lcAppVr = '__'+SUBSTR(lcTmpRecv,3)
  CREATE CURSOR &lcAppVr (lDoTriger L , SHIPPED C(8))
  APPEND BLANK
  
  llDoTriger = EMPTY(lcCutTkt) .AND. !EMPTY(lcShip)
  REPLACE lDoTriger WITH llDoTriger ;
          SHIPPED   WITH gfTempName()
  
  IF llDoTriger
    *C121114,3  TMI [Start] 
    *lcCuttkt = '_'     
    IF !cbByCrtn
      lcCuttkt = '_'
    ENDIF
    *C121114,3  TMI [End  ] 
    SHOW GET lcCutTkt DISABLE
    SHOW GET ibtkt    DISABLE
  ENDIF

ENDIF

SELECT (lnSlct)
*-- End of lfIGNRPO.


*:**************************************************************************
*:* Name        : lfSvShp
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/28/2004
*:* Purpose     : Save the shipment in a temp file
*:***************************************************************************
*C121114,1
FUNCTION lfSvShp
PRIVATE lnSlct,lcAppVr,lcTmpFl,lcHoldPo
lnSlct = SELECT()

lcAppVr = '__'+SUBSTR(lcTmpRecv,3)
IF &lcAppVr..lDoTriger
  lcSvOrd = ORDER('POSLN')
  SET ORDER TO POSLNS IN POSLN

  lcTmpFl = &lcAppVr..SHIPPED
  SELECT STYLE,CSTYTYPE,PO,QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8, ;
                  000000 AS RCV1,000000 AS RCV2,000000 AS RCV3,000000 AS RCV4,;
                  000000 AS RCV5,000000 AS RCV6,000000 AS RCV7,000000 AS RCV8 ;                  
  FROM POSLN ;
  WHERE SHIPNO = lcShip ;
  INTO TABLE (gcWorkDir+lcTmpFl)
  INDEX ON STYLE+CSTYTYPE+PO TAG STYLEPO
  
  lcHoldPo = ' '
  SELECT &lcTmpFl
  LOCATE
  SCAN
    *- Use ord1,...,ord8 fields to compute recived qty's
    SELECT POSLN
    =SEEK(&lcTmpFl..STYLE,'POSLN')
    SCAN REST WHILE STYLE+CSTYTYPE+PO+STR(LINENO,6)+TRANCD = ;
                    &lcTmpFl..STYLE+&lcTmpFl..CSTYTYPE+&lcTmpFl..PO ;
              FOR TRANCD = '2'
      SELECT &lcTmpFl
      REPLACE RCV1 WITH RCV1 + POSLN.QTY1 ;
              RCV2 WITH RCV2 + POSLN.QTY2 ;
              RCV3 WITH RCV3 + POSLN.QTY3 ;
              RCV4 WITH RCV4 + POSLN.QTY4 ;
              RCV5 WITH RCV5 + POSLN.QTY5 ;
              RCV6 WITH RCV6 + POSLN.QTY6 ;
              RCV7 WITH RCV7 + POSLN.QTY7 ;
              RCV8 WITH RCV8 + POSLN.QTY8 
    ENDSCAN
    IF SEEK(&lcTmpFl..CSTYTYPE+&lcTmpFl..PO,'POSHDR') .AND. POSHDR.STATUS # 'O' 
      IF !POSHDR.PO $ lcHoldPo
        lcHoldPo = lcHoldPo + POSHDR.PO + ','
      ENDIF
    ENDIF

  ENDSCAN
  
  SET ORDER TO &lcSvOrd IN POSLN
  
  IF !EMPTY(lcHoldPo)
    lcHoldPo = ALLTRIM(SUBSTR(lcHoldPo,1,LEN(lcHoldPo)-1))
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'The style PO(s)# ' + lcHoldPo + IIF(','$lcHoldPO, ' are ',' is ')+' Hold, All included styles will be rejected.')
  ENDIF

ENDIF
SELECT (lnSlct)
*-- end of lfSvShp.


*:**************************************************************************
*:* Name        : lfUpdCtTkt
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/28/2004
*:* Purpose     : Update lcCutTkt variable with the current PO
*:***************************************************************************
*C121114,1
FUNCTION lfUpdCtTkt
PRIVATE lnSlct,lcAppVr,lcSty
lnSlct = SELECT()

lcAppVr = '__'+SUBSTR(lcTmpRecv,3)
IF &lcAppVr..lDoTriger
  lcSty = SUBSTR(&lcTmpCurs..ITEM,1,19)
  lcSzNo = SUBSTR(&lcTmpCurs..ITEM,20,1)
  lcSzNo = IIF(EMPTY(lcSzNo) , '1' , lcSzNo )
  
  lcCuttkt = '      '

  SELECT (&lcAppVr..SHIPPED)
  =SEEK(lcSty,&lcAppVr..SHIPPED)
  LOCATE REST WHILE STYLE+CSTYTYPE+PO = lcSty ;
              FOR QTY&lcSzNo > RCV&lcSzNo
  IF FOUND()
    REPLACE RCV&lcSzNo WITH RCV&lcSzNo + 1
    lcCuttkt = EVAL(EVAL(lcAppVr+'.SHIPPED')+'.PO')
  ELSE
    SET ORDER TO TAG STYLEPO DESC
    GO TOP
    IF SEEK(lcSty)
      REPLACE RCV&lcSzNo WITH RCV&lcSzNo + 1    
      lcCuttkt = EVAL(EVAL(lcAppVr+'.SHIPPED')+'.PO')
    ENDIF
    SET ORDER TO TAG STYLEPO ASCE
    GO TOP
  ENDIF  
ENDIF
SELECT (lnSlct)
*-- end of lfUpdCtTkt.

*:**************************************************************************
*:* Name        : lfDISBL
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/28/2004
*:* Purpose     : Disable lcCutTkt field in Add mode
*:***************************************************************************
*C121114,1
FUNCTION lfDISBL
PRIVATE lnSlct,lcAppVr
lnSlct = SELECT()

lcAppVr = '__'+SUBSTR(lcTmpRecv,3)
*C121114,3  TMI [Start] 
*IF &lcAppVr..lDoTriger
*C121114,3  TMI [End  ] 
  SHOW GET lcCutTkt DISABLE
  SHOW GET ibtkt    DISABLE
*C121114,3  TMI [Start] 
*ENDIF
*C121114,3  TMI [End  ] 

SELECT (lnSlct)

*-- end of lfDISBL.

*:**************************************************************************
*:* Name        : GtCtTkt
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/28/2004
*:* Purpose     : Get lcCutTkt variable from lcTmpRecv file
*:***************************************************************************
*C121114,1
FUNCTION lfGtCtTkt
PRIVATE lnSlct,lcAppVr
lnSlct = SELECT()

lcAppVr = '__'+SUBSTR(lcTmpRecv,3)
IF &lcAppVr..lDoTriger
  lcCutTkt = &lcTmpRecv..PO_CT
ENDIF

SELECT (lnSlct)
*-- end of lfGtCtTkt.

*:**************************************************************************
*:* Name        : lfUpdStr
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/28/2004
*:* Purpose     : Update the variable lcTmpStr used in Warning message box
*:***************************************************************************
*C121114,1
FUNCTION lfUpdStr
PRIVATE lnSlct,lcAppVr
lnSlct = SELECT()

lcAppVr = '__'+SUBSTR(lcTmpRecv,3)
IF &lcAppVr..lDoTriger
  lcTmpStr = ALLTRIM(&lcTmpRecv..Style) + "|" + "Shipment" + "|" + lcShip
ENDIF

SELECT (lnSlct)
*-- end of lfUpdStr.

*:**************************************************************************
*:* Name        : lfUPDTMPFL
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/31/2004
*:* Purpose     : Update the temp file 
*:***************************************************************************
*C121114,1
FUNCTION lfUPDTMPFL
PRIVATE lnSlct,lcAppVr
lnSlct = SELECT()

lcAppVr = '__'+SUBSTR(lcTmpRecv,3)
*C121114,3  TMI [Start] Always apply this funciton
*IF &lcAppVr..lDoTriger
  *C121114,3  TMI [End  ] 
  
  *C121114,3  TMI [Start] Restore lcImpFile name
*  lcAppVr = '__'+SUBSTR(lcTmpRecv,3)
*  lcImpFile = ALLTRIM(&lcAppVr..RCVTXT)
*  SHOW GET lcImpFile
  *C121114,3  TMI [End  ] 

  SELECT (lcTmpCurs)
  GO TOP
  IF !EOF()
    GOTO 1
    *C121114,3  TMI [Start] 
    *IF 'STYLESIZE#' $ CHRTRAN(UPPER(&lcTmpCurs..Item),'; ','')
    *  DELETE 
    LOCATE FOR 'STYLESIZE#' $ CHRTRAN(UPPER(&lcTmpCurs..Item),'; ','')
    IF FOUND()
      DELETE REST FOR 'STYLESIZE#' $ CHRTRAN(UPPER(&lcTmpCurs..Item),'; ','')
      *C121114,3  TMI [End  ] 
      *C121114,3  TMI [Start] Reject the case this text file contains upc's only
      IF rbMethod=1 .AND. rbScanBy=2
        =gfModalGen('INM00000B00000',.F.,.F.,.F.,'The selected file ' + lcImpFile + ;
                                           ' contains Styles only , You must select "STYLE" option.')
        lcCuttkt = ''               
        SHOW GET lcCuttkt                            
        ZAP
        RETURN .F.
      ENDIF
      *- if There is styles with no size , add size 1
      SCAN
        IF LEN(ALLTRIM(ITEM))<20
          REPLACE ITEM WITH PADR(ITEM,19)+'1'
        ENDIF
      ENDSCAN
    ELSE
      IF rbMethod=1 .AND. rbScanBy=1
        =gfModalGen('INM00000B00000',.F.,.F.,.F.,'The selected file ' + lcImpFile + ;
                                           ' contains UPCs only , Can not continue.')
        lcCuttkt = ''                                           
        SHOW GET lcCuttkt                            
        ZAP
        RETURN .F.
      ENDIF
      *C121114,3  TMI [End  ] 
    ENDIF
    GO TOP
    *SCAN 
    *  REPLACE ITEM WITH STRTRAN(ITEM,'-')
    *ENDSCA
  ENDIF
*C121114,3  TMI [Start] commnet this line
*ENDIF
*C121114,3  TMI [End  ] 
SELECT (lnSlct)
*-- end of lfUPDTMPFL.

*:**************************************************************************
*:* Name        : lfGTSZDESC
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/31/2004
*:* Purpose     : Get Size description
*:***************************************************************************
*C121114,1
FUNCTION lfGTSZDESC
PRIVATE lnSlct,lcAppVr
PRIVATE lcSzDesc

lnSlct = SELECT()

lcAppVr = '__'+SUBSTR(lcTmpRecv,3)
IF &lcAppVr..lDoTriger
  GO TOP IN STYLE
  GO TOP IN SCALE
  =SEEK(&lcTmpRecv..STYLE,'STYLE')
  =SEEK('S'+STYLE.SCALE,'SCALE')
  *C121114,3  TMI [Start] if no size supplied , let it be '1'
  *lcSzDesc = &lcTmpRecv..Size
  lcSzDesc = IIF(EMPTY(&lcTmpRecv..Size),'1',&lcTmpRecv..Size)
  *C121114,3  TMI [End  ] 
  REPLACE SzDesc WITH Scale.Sz&lcSzDesc
ENDIF  

SELECT (lnSlct)
*-- end of lfGTSZDESC.

*:**************************************************************************
*:* Name        : lfEXGTMSK
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/31/2004
*:* Purpose     : Exit the function lfGetMask in case of App10
*:***************************************************************************
*C121114,1 
FUNCTION lfEXGTMSK
PRIVATE lnSlct,lcAppVr
lnSlct = SELECT()

lcAppVr = '__'+SUBSTR(lcTmpRecv,3)
*C121114,3  TMI [Start] Always do this funtion for App10
*IF &lcAppVr..lDoTriger
  *C121114,3  TMI [End  ] 
  IF !llUPC .AND. llStyle .AND. !EMPTY(laSepartor[1,1])
    RETURN .F.
  ENDIF
  *C121114,3  TMI [Start] Commnet this line
*ENDIF  
*C121114,3  TMI [End  ] 
SELECT (lnSlct)
*-- end of lfEXGTMSK.

*:**************************************************************************
*:* Name        : lfCHKSTATS
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/01/2004
*:* Purpose     : Reject styles in Hold Po's 
*:***************************************************************************
*C121114,1  
FUNCTION lfCHKSTATS
PRIVATE lnSlct,lcAppVr
lnSlct = SELECT()
lcAppVr = '__'+SUBSTR(lcTmpRecv,3)
IF &lcAppVr..lDoTriger
  lcTmpFl = &lcAppVr..SHIPPED
  IF SEEK(&lcTmpFl..CSTYTYPE+&lcTmpFl..PO,'POSHDR')
    IF POSHDR.STATUS # 'O'
      llAccepted = .F.
    ENDIF
    IF EMPTY(&lcTmpRecv..SzDesc)
      llAccepted = .F.
    ENDIF    
  ENDIF
ENDIF
SELECT (lnSlct)

*-- end of lfCHKSTATS.

*:**************************************************************************
*:* Name        : lfRmvComa
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/11/2004
*:* Purpose     : Remove semi colon from the temp file befor processing file 
*:***************************************************************************
*C121114,3  
FUNCTION lfRmvComa
PRIVATE lnHndl,lnSize,lcRead

lcAppVr = '__'+SUBSTR(lcTmpRecv,3)

lnHndl = FOPEN(lcImpFile,2)
IF lnHndl>0
  lnSize = FSEEK(lnHndl,0,2)
  =FSEEK(lnHndl,0,0)
  lcRead = FREAD(lnHndl,lnSize)
  lcRead = STRTRAN(lcRead,';','')
  =FCLOSE(lnHndl)
  
  lcTmpTxt = gfTempName()
  lnOutput = FCREATE(gcWorkDir+lcTmpTxt+'.TXT')
  lnWritten = FWRITE(lnOutput,lcRead)
  =FCLOSE(lnOutput)
  
*  SELECT &lcAppVr
*  REPLACE RCVTXT WITH gcWorkDir+lcTmpTxt+'.TXT'
  
  lcImpFile = gcWorkDir+lcTmpTxt+'.TXT'
ENDIF

*-- end of lfRmvComa.

*:**************************************************************************
*:* Name        : lfSLMETHOD
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/11/2004
*:* Purpose     : Select method and scan by
*:***************************************************************************
*C121114,3  
FUNCTION lfSLMETHOD
STORE 1 TO rbScanBy,rbMethod 
*-- end of lfSLMETHOD.

*:**************************************************************************
*:* Name        : lfREFRESH
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/11/2004
*:* Purpose     : Refresh lcCuttkt variable
*:***************************************************************************
*C121114,3  
FUNCTION lfREFRESH
lcCuttkt = '      '
SHOW GET lcCuttkt 
*-- end of lfREFRESH.
*CP121868,1 BWA 04/28/2004 Trigger to be able to show the complete orders.[START]
*!*************************************************************
*! Name      : lfvOrdComp
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! SuperVisor: ASHRAF SHERIF
*! Date      : 04/22/2004
*! Purpose   : Validation function for the Order field.
*!*************************************************************
*! Called from  : APP10MAN.PRG
*! Program name : SOORCN.PRG
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lfvOrdComp
*!*************************************************************
FUNCTION lfvOrdComp
PRIVATE lcVar , lcObj , laTemp

lcVar = SYS(18)                && Varible to hold  the name of the memory variable used to create the current GET control
lcObj = EVALUATE(SYS(18))      && Varible to hold the current field value

lcObj = IIF(EMPTY(lcObj) .OR. '?' $ lcObj , lcObj , PADL(ALLTRIM(lcObj) , 6 , '0'))

IF !EMPTY(lcObj) AND ('?' $ lcObj OR !IIF(lcRpOrdTyp = 'A'  ,;
   SEEK('O'+lcObj , 'ORDHDR') OR SEEK('C'+lcObj , 'ORDHDR') ,;
   SEEK(lcRpOrdTyp+lcObj , 'ORDHDR')))

  SELECT ORDHDR
  DIMENSION laTemp[1]
  laTemp = ''      && Array to hold the Selected value

  lcBrFields = "CTYPE=IIF(cOrdType='C','Contract',IIF(cOrdType='O','Order',IIF(cOrdType='T','EDI Temp. Order','')))"+;
               ":R :H= 'Order Type' :20, "          +;
               "ORDER     :R :H= 'Order #' , "   +;
               "ACCOUNT   :R :H= 'Account' ,"    +;
               "STORE     :R :H= 'Store' ,"      +;
               "ENTERED   :R :H= 'Entered Date',"+;
               "SEASON    :R :H= 'Season' ,"     +;
               "cDIVISION :R :H= 'Division' ,"   +;
               "CTERMCODE :R :H= 'Terms' ,"      +;
               "SHIPVIA   :R :H= 'ShipVia' ,"    +;
               "STATUS    :R :H= 'Status ' ,"    +; 
               "OPEN      :R :H= 'Open Amt. ',"  +; 
               "BULK      :R :H= 'Bulk' "

  lcFile_Ttl = IIF(lcRpOrdTyp = 'A',"Orders/Contracts",IIF(lcRpOrdTyp='O',"Orders...",;
                   IIF(lcRpOrdTyp='T',"EDI Temporary Orders","Contracts...")))

  lcBrowCond = [FOR (IIF(lcRpOrdSap = 'A' , !(ORDHDR.STATUS $ "X") , ;
               ORDHDR.STATUS = lcRpOrdSap) AND ] + ;
               [ IIF(lcRpOrdTyp = "A",.T.,CORDTYPE = lcRpOrdTyp) AND] +;
               [ IIF(lcRpEDIFlt='O',!ORDHDR.LEDIORDER,IIF(lcRpEDIFlt='E' , ;
               ORDHDR.LEDIORDER,.T.)))]

  = gfBrows(lcBrowCond,'ORDER','laTemp')  
  IF !EMPTY(laTemp[1])
    lcObj = laTemp[1]
  ELSE
    lcObj = laOldVal
  ENDIF
ENDIF
&lcVar = lcObj

*--End of lfvOrdComp.
*!*************************************************************
*! Name      : lfSetOrd
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! SuperVisor: ASHRAF SHERIF
*! Date      : 04/22/2004
*! Purpose   : Rise change order flag, in range browse screen.
*!*************************************************************
*! Called from  : APP10MAN.PRG
*! Program name : SOORCN.PRG
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lfSetOrd
*!*************************************************************
FUNCTION lfSetOrd

DO CASE
  CASE lcParm = 'S'
    SELECT ORDHDR
    DO CASE
      CASE lcRpOrdTyp = "A"
         SET FILTER TO IIF(lcRpOrdSap= 'A', !(ORDHDR.STATUS $ "X")  ,;
           STATUS = lcRpOrdSap) AND IIF(lcRpEDIFlt='O',!ORDHDR.lEdiOrder, ; 
           IIF(lcRpEDIFlt='E',ORDHDR.lEdiOrder,.T.))
         LOCATE
         
      CASE lcRpOrdTyp = "O"
         SET FILTER TO (CORDTYPE + ORDER = "O") AND IIF(lcRpOrdSap= 'A', !(ORDHDR.STATUS $ "X") ,;
            STATUS = lcRpOrdSap) AND IIF(lcRpEDIFlt='O',!ORDHDR.lEdiOrder, ; 
            IIF(lcRpEDIFlt='E',ORDHDR.lEdiOrder,.T.))
            LOCATE FOR CORDTYPE+ORDER = "O"

      CASE lcRpOrdTyp = "C"
         SET FILTER TO (CORDTYPE + ORDER = "C") AND IIF(lcRpOrdSap= 'A', !(ORDHDR.STATUS $ "X") ,;
            STATUS = lcRpOrdSap) AND  IIF(lcRpEDIFlt='O',!ORDHDR.lEdiOrder, ; 
            IIF(lcRpEDIFlt='E',ORDHDR.lEdiOrder,.T.))
         LOCATE FOR CORDTYPE+ORDER = "C"

      CASE lcRpOrdTyp = "T" 
         SET FILTER TO (CORDTYPE + ORDER = "T") AND IIF(lcRpOrdSap= 'A', !(ORDHDR.STATUS $ "X") ,;
            STATUS = lcRpOrdSap) AND IIF(ORDHDR.cOrdType='T' .AND. ;
            ORDHDR.MON_FLG $ 'LG' ,.T.,OPEN >0) AND IIF(lcRpEDIFlt='O',!ORDHDR.lEdiOrder, ; 
            IIF(lcRpEDIFlt='E',ORDHDR.lEdiOrder,.T.))
         LOCATE FOR CORDTYPE+ORDER = "T"
    ENDCASE                  

  CASE lcParm = 'R'
    SELECT ORDHDR
    SET FILTER TO
    llClearSel = .F.
ENDCASE

*--End of lfSetOrd.
*CP121868,1 BWA 04/28/2004 Trigger to be able to show the complete orders.[START]