*!**************************************************************************
*! Name      : DAVMAIN.PRG
*! Developer : RANIA ABDEL RAZIK (RAE)
*! Date      : 12/12/2002
*! Purpose   : David Luke Ltd Custom Process Program .
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.
*!**************************************************************************
*MODIFICATIONS
*C128583,1 TMI 08/23/2005 Recive PO/Shipments via the Bar Code Temp receiving for Pan21
*!**************************************************************************
PARAMETER lcEvntFun,lcFunPars

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLTRIM(lcEvntFun)+'('+lcFunPars+')'

*-- Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue

*:**************************************************************************
*:* Name        : lfCHKPRIAG
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/13/2005
*:* Purpose     : get the variable name to Compare valid  prices for Banach based on setup with
*:***************************************************************************
FUNCTION lfCHKPRIAG
PRIVATE lcDateTyp,lcChkDt
lcDateTyp = gfGetMemVar('M_CHKPRIAG')
DO CASE
CASE lcDateTyp = 'E'   && Entered
  lcChkDt = 'laData(8)'

CASE lcDateTyp = 'S'   && Start
  lcChkDt = 'laData(9)'
  
CASE lcDateTyp = 'C'   && Complete
  lcChkDt = 'm.COMPLETE' 
  
ENDCASE

RETURN lcChkDt
*-- end of lfCHKPRIAG.

*:**************************************************************************
*:* Name        : lfPNPRICOM
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/14/2005
*:* Purpose     : Update special prices based on complete date in the quick 
*:*             : order entry screen not based on the complete date in the header folder
*:***************************************************************************
*:* Called from : QUICKORD.SPX screen (lfPriComDt function )
*:***************************************************************************
FUNCTION lfPNPRICOM
PRIVATE lnMaxInd , lnPrvAlias , lcCountNo , lnStyComm , lnGrssPric , lnI , lcSz , lcScl , lcClr ,lcDateTyp 
        

*- Not the complete date
lcDateTyp = gfGetMemVar('M_CHKPRIAG')
IF lcDateTyp $ 'SE'   && Entered,Start
  RETURN
ENDIF  

*- Complete date not changed
IF m.COMPLETE = lcOldValue
  RETURN
ENDIF  

lnPrvAlias = SELECT()

SELECT (lcTempCur)
SET FILTER TO 
GO TOP

=SEEK(M.Style,'STYLE')

lcScl = ' '
*C127807,3  TMI [Start] save the current color
lcClr = ' '
*C127807,3  TMI [End  ] 
STORE 0 TO lnStyComm , lnGrssPric

SELECT (lcTempCur)
SCAN FOR EMPTY(LASTLINE)
  
  *- Loop for the current 10 fields and check changing in scale
  FOR lnI = 1 TO 10  
    lcSz = ALLTRIM(STR(lnI))    

    IF !EMPTY(&lcTempCur..SCL&lcSz) 
      *C127807,3  TMI [Start] check if the color is changed
      *IF lcScl <> &lcTempCur..SCL&lcSz
      IF lcScl <> &lcTempCur..SCL&lcSz .OR. lcClr <> &lcTempCur..COLOR
        *C127807,3  TMI [End  ] 
      
        lcScl = &lcTempCur..SCL&lcSz
        lcPRICCODE = ''
        
        lcSeekSty = PADR(&lcTempCur..STYMAJOR+lcSepart+PADR(&lcTempCur..COLOR,lnClrLen)+lcClrSpr+lcScl,19)
        =SEEK(lcSeekSty,'STYLE')
        WAIT WINDOW NOWAIT 'Updating prices for ' + lcSeekSty
        
        IF SEEK(lfPRICCODE()+laData[33]+lcSeekSty,'CSTPRICE')        
          lnStyComm  = IIF(CSTPRICE.COMMDV=0,IIF(Style.Commission,laData[28],0),CSTPRICE.COMMDV)
          lnGrssPric = CSTPRICE.PRICEDV
        ELSE              
          IF llMulCurr .AND. laData[33]<>gcBaseCurr 
            IF SEEK(lcSeekSty+laData[33],'STYPRICE')
              lnStyComm = IIF(Style.Commission,laData[28],0)
              lnGrssPric= STYPRICE.PRICEA
            ENDIF
          ELSE     
            lnStyComm  = IIF(Style.Commission,laData[28],0)
            lnGrssPric = STYLE.PRICEA
          ENDIF        
        ENDIF
        
      ENDIF
    
      SELECT (lcTempCur)
      REPLACE GPric&lcSz WITH lnGrssPric ;
              Comm&lcSz  WITH lnStyComm
    ENDIF
               
  ENDFOR
                  
ENDSCAN

SELECT (lcTempCur)
SET FILTER TO (Val(cSequnc) = lnfldgrp)

WAIT CLEAR
SELECT (lnPrvAlias)

*-- end of lfPNPRICOM.

*:**************************************************************************
*:* Name        : lfCHKSPLPR
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/20/2005
*:* Purpose     : check that some styles are entered/start with special prices
*:***************************************************************************
*:* Called from : lfvEntered IN SOORD.PRG
*:***************************************************************************
FUNCTION lfCHKSPLPR
PRIVATE lcSlct,lnRecno,lnI,lcI,lcVar,lcStyle,llOk,lnResp,lcDtFld,llPriFnd
lcSlct = SELECT()
llOk = .T.

IF USED('CSTPRICE')
  lcVar = SYS(18)
  IF UPPER(lfCHKPRIAG()) <> lcVar
    RETURN
  ENDIF

  llPriFnd = .F.
  lnRecno = RECNO(lcOrdLine)
  lcDtFld = IIF(lcVar = 'LADATA(8)','ENTERED','START')
  ldSvDate = EVAL(lcVar)
  &lcVar = &lcOrdHdr..&lcDtFld
  SELECT &lcOrdLine  
  LOCATE
  SCAN
    *- Check if the style price is obtained from the file cstprice depending on a price code
    IF SEEK(lfPRICCODE()+laData[33]+&lcOrdLine..STYLE,'CSTPRICE') .AND. CSTPRICE.PRICEDV = &lcOrdLine..GROS_PRICE
      IF !BETWEEN(ldSvDate,CSTPRICH.DVLDPRFR,CSTPRICH.DVLDPRTO)      
        llPriFnd = .T.
        EXIT
      ENDIF
    ENDIF    
  ENDSCAN 
  IF llPriFnd
    lcStyle = &lcOrdLine..STYLE
    lcDtDesc = IIF(lcVar = 'LADATA(8)','Entered','Start')
    lnResp = gfModalGen('INM00000B00006',.F.,.F.,.F.,'The some styles are entered via the Quick order entry screen with a special price. Are you sure you want to change the &lcDtDesc date?')
    llOk = (lnResp = 1)
  ENDIF   
  IF BETWEEN(lnRecno,1,RECCOUNT(lcOrdLine))
    GOTO (lnRecno) IN &lcOrdLine
  ENDIF
  
  *- Restore saved date
  IF llOk
    &lcVar = ldSvDate
  ENDIF
ENDIF

SELECT (lcSlct)
RETURN llOk
*-- end of lfCHKSPLPR.

*:**************************************************************************
*:* Name        : lfPRICCODE
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/02/2003
*:* Purpose     : get Customer Price code 
*                 This is a typical copy of the funtion in Davmain since calling
*                 it from PANMAIN get us with an error
*:***************************************************************************
*C#200431,1
FUNCTION lfPRICCODE
*C127807,1  TMI [Start] get the field name to Compare valid  prices with ( for Banach based on setup - Entered, Start or Complete Dates)
IF ASCAN(laEvntTrig,'CHKPRIAG') <> 0
  lcChkDate = gfDoTriger('SOORD','CHKPRIAG')
ENDIF
*C127807,1  TMI [End  ] 
IF TYPE('lcPRICCODE')#'C' .OR. EMPTY(lcPRICCODE)
  PRIVATE lcSlct,lnRecno
  lcSlct = SELECT()
  SELECT CUSTOMER
  lnRecno = RECNO()
  =SEEK('M'+laData[2],'CUSTOMER')
  *C200551,1  TMI [Start] Pick the suitable price code with respect to the Entered date
  *lcPRICCODE = UPPER(CUSTOMER.PRICCODE)
  PRIVATE lnI,lcI,lcNoDtPCod
  IF !USED('CSTPRICH')
    =gfOpenFile(gcDataDir+'CSTPRICH','CSTPRICH','SH')
  ENDIF
  *-- Loop through all price codes
  *-   pick the first price code with suitable date,
  *-   if no one, pick the first one with no valid dates , otherwise return chr(255)
  lM_PRCCOD =gfGetMemvar('M_PRCCOD')
  *C123847,1  TMI [Start] case if other than DavLuck used davmain and the option M_PRCCOD is not defined for him
  lM_PRCCOD = IIF(TYPE('lM_PRCCOD')#'L',.T.,lM_PRCCOD)
  *C123847,1  TMI [End  ] 
  lcNoDtPCod = CHR(255)
  *ash123853
  PRIVATE lnClrPos,lnClrLen
  STORE 0  TO lnClrPos,lnClrLen
  =lfGetClrD()
  FOR lnI = 1 TO 15
    lcI = IIF(lnI = 1 , '' , PADL(lnI,2,'0') ) 
    *B119490,1  TMI [Start] locate price code for current style
    *IF !EMPTY(CUSTOMER.PRICCODE&lcI) .AND. SEEK(CUSTOMER.PRICCODE&lcI+laData[33],'CSTPRICH')
    *B124083,1  TMI [Start] Add the style field to the key of CSTPRICH file
    *IF !EMPTY(CUSTOMER.PRICCODE&lcI) .AND. SEEK(CUSTOMER.PRICCODE&lcI+laData[33],'CSTPRICH') ;
                                     .AND. SEEK(CUSTOMER.PRICCODE&lcI+laData[33]+STYLE.STYLE,'CSTPRICE')
    PRIVATE lcStyClr
    lcStyClr = IIF(lM_PRCCOD , '' , PADR(SUBSTR(STYLE.STYLE,1,lnClrPos+lnClrLen-1),19) )
    IF !EMPTY(CUSTOMER.PRICCODE&lcI) .AND. SEEK(CUSTOMER.PRICCODE&lcI+laData[33]+lcStyClr,'CSTPRICH') ;
                                     .AND. SEEK(CUSTOMER.PRICCODE&lcI+laData[33]+STYLE.STYLE,'CSTPRICE')
      *B124083,1  TMI [End  ] 
     
      *B119490,1  TMI [End  ]                                       
      IF EMPTY(CSTPRICH.DVLDPRTO)
        *- Get no valid date price code
        lcNoDtPCod = UPPER(CUSTOMER.PRICCODE&lcI)
      ELSE
        *C127807,1  TMI [Start] Compare valid  prices for Banach based on setup ( Entered, Start or Complete Dates)
        IF ASCAN(laEvntTrig,'CHKPRIAG') <> 0
          IF BETWEEN(&lcChkDate,CSTPRICH.DVLDPRFR,CSTPRICH.DVLDPRTO)
            EXIT
          ENDIF
        ELSE
          *C127807,1  TMI [End  ]         
          
          IF BETWEEN(laData[8],CSTPRICH.DVLDPRFR,CSTPRICH.DVLDPRTO)
            EXIT
          ENDIF
          
          *C127807,1  TMI [Start] 
        ENDIF
        *C127807,1  TMI [End  ] 
      ENDIF
    ENDIF    
  ENDFOR
  lcPRICCODE = IIF(lnI < 16 , UPPER(CUSTOMER.PRICCODE&lcI) , lcNoDtPCod )
  *C200551,1  TMI [End  ]   

  IF BETWEEN(lnRecno,1,RECCOUNT('CUSTOMER'))
    *C200551,1  TMI [Start] go to record in the correct alias
    *GOTO (lnRecno)
    GOTO (lnRecno) IN CUSTOMER  
    *C200551,1  TMI [End  ] 
  ENDIF
  SELECT (lcSlct)
ENDIF
RETURN lcPRICCODE
*-- end of lfPRICCODE.

*:**************************************************************************
*:* Name        : lfGetClrD                                       
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/20/2002
*:* Purpose     : Get Color Position and Color length
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfGetClrD()
*:***************************************************************************
*C#200431,1
FUNCTION lfGetClrD
DECLARE laItemSeg[1]
PRIVATE lnCount &&Tmi 07/15/2002
lcOldSelect=select()
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLen = LEN(laItemSeg[lnCount,3])
    lnClrPos = laItemSeg[lnCount,4]
    lcClrSpr = ALLT(laItemSeg[lnCount,6])
    EXIT
  ENDIF
ENDFOR

*C200452,1 TMI [Start] Get size position and size segment lenght 
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='S'
    lnSizeLen = LEN(laItemSeg[lnCount,3])
    lnSizePos = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR
*C200452,1 TMI [End  ] 

SELECT(lcOldSelect)
*--end function lfGetClrD

*:**************************************************************************
*:* Name        : lfSLMETHOD
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/21/2005
*:* Purpose     : Select the UPC method to import text files
*:***************************************************************************
*:* Called from : LPSHOW IN MFRCALL.PRG
*:***************************************************************************
*C128583,1
FUNCTION lfSLMETHOD
rbMethod = 1
*-- end of lfSLMETHOD.

*:**************************************************************************
*:* Name        : lfENBTNS
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/18/2005
*:* Purpose     : set "rbMethod" variable to 1 when the screen starts
*:***************************************************************************
*:* Called from : lpShow in MFRCALL.PRG
*:***************************************************************************
*C128583,1
FUNCTION lfENBTNS
rbMethod = 1
SHOW GET rbMethod DISABLE
SHOW GET rbScanBy DISABLE
SHOW GET cbByCrtn DISABLE
SHOW GET rbByLin  DISABLE
SHOW GET lcUPC    DISABLE
SHOW GET pbNewLn  DISABLE

SHOW GET pbGetFil  ENABLE
SHOW GET lcImpFile ENABLE

SHOW GET pbNewCt  DISABLE 
SHOW GET ibtkt    DISABLE
SHOW GET lcCuttkt DISABLE

*C128583,4  TMI [Start] Disable Shipno/PO#
SHOW GET lcShip   DISABLE
SHOW GET lcCuttkt DISABLE
SHOW GET ibtkt  DISABLE
SHOW GET ibShip DISABLE
*C128583,4  TMI [End  ] 

*- Remove the carton field from the browse 
lcRcvFld = "cMarker=IIF(RECNO()=lnMarker , '>',' '):H=' ':R:1:W=lfshplins() ,"+;
           "Ship:H='Ship #':R:8, PO_CT:H='P/O # ':R:8 ,"+;
           "Style:R:28 , SzDesc:R:9:H='Size' , cStyDesc:R:20:H='Description' , Dyelot:R , "+;
           "lcReject=IIF(lAccepted,'Accepted','Rejected'):H='Status':R:10 ,"+;
           "nQty:R:H='Qty.':6"

*-
IF WEXIST(lcRcAll3)
  SHOW GETS WINDOW (lcRcAll3) DISABLE ONLY
  *C128583,5  TMI [Start] 
  _CUROBJ = OBJNUM(pbGetFil)
  *C128583,5  TMI [End  ] 
ENDIF

*-- end of lfSlctFlBt.

*:**************************************************************************
*:* Name        : lfIMPANFL
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/18/2005
*:* Purpose     : Append data from the PAN21 file format and then update the 
*               : temp variable name lcImpFile with this imported file 
*:***************************************************************************
*:* Called from : lfvCollect IN MFRCALL.PRG
*:***************************************************************************
*C128583,1
FUNCTION lfIMPANFL

*- PAN21 SAMPLE FILE FOMRAT:
*- Po No         - 6 characters to match Aria PO No
*- Ship No       - 6 characters to match Aria Shipment No
*- Cstymajor     - 9 characters to match Aria Cstyle major
*- Style UPC     - 13 charcaters to match Aria Styleupc- upcnum1,upcnum2 and upcnum3
*- Qty Recvd     - 6 numeric - to match Aria Posln.qty
*- Date received - 99/99/9999 to be used as goods received date 

*- I will checked the opened file if it contains this format , if  no abort, if yes then convert it to the same 
*- format of the standard file, 
*- and since the user uses the UPC's then the new file will be UPCs only after checking that all 
*- data are valid 
*- Also rename the vaiable "lcImpFile" with the variable name "lcTmpImp" which is a file in the 
*- work folder to import from not from PAN21 file.

PRIVATE lcTmpImp,lcTmpLog,lnSlct,lnHndl,llOk,lnStyLen,lcStyUpcOrd,;
        lcErr,ldRcvDat

lnSlct = SELECT()
lcTmpImp = gfTempName()
lcTmpLog = 'Z_'+SUBSTR(lcRcAll0,3)
lcImported = 'I_'+SUBSTR(lcRcAll0,3)
llOk = .T.

*C128583,4  TMI [Start]   *** You have to select the file you will use to import your items. ***
IF EMPTY(lcImpFile)
  =gfModalGen("INM34143B00000" , "DIALOUG")
  _CUROBJ = OBJNUM(lcImpFile)  
  RETURN .F.
ENDIF
*C128583,4  TMI [End  ] 

lcStyUpcOrd = ORDER('STYLEUPC')
SET ORDER TO STYUPCN IN STYLEUPC

*- Create temp files
=lfCrtTmpFl()

SELECT CTKTRCVH
LOCATE FOR ALLTRIM(SUBSTR(lcImpFile,RAT('\',lcImpFile)+1)) $ CIMPFILE
IF FOUND()
  lcErr = 'The file '+lcImpFile+' was imported in the batch#'+CTKTRCVH.TMPRCVNUM +' ,can not reimport'
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,lcErr)
  RETURN .F.
ENDIF

IF llOk

  *- Create a temp index for the file lcTmpRecv
  SELECT &lcTmpRecv
  INDEX ON SHIP+PO_CT+STYLE TAG &lcTmpImp
  
  *- Append the Banach file to a temp cursor with the above format
  SELECT (lcTmpImp)
  APPEND FROM (lcImpFile) DELIMITED
  REPLACE ALL PO        WITH IIF(!EMPTY(PO),PADL(ALLTRIM(PO),6,'0'),'') ;
              SHIPNO    WITH ALLTRIM(SHIPNO) ;
              CSTYMAJOR WITH ALLTRIM(CSTYMAJOR)

  *- Scan the validity of data
  SELECT (lcTmpImp)
  *C128583,8  TMI [Start] use DELETED field INSTEAD OF DELETING the record
  *DELETE ALL FOR QTY = 0
  *GO TOP
  REPLACE DELETED WITH .T. FOR QTY = 0
  LOCATE FOR !DELETED
  *C128583,8  TMI [End  ] 
  
  llOk = !EOF()
ENDIF

*C128583,5  TMI [Start] Get the Ship#/PO# from the appended file 
SELECT (lcTmpImp)
GO TOP
*C128583,8  TMI [Start] go to first not deleted line
LOCATE FOR !DELETED
*C128583,8  TMI [End  ] 
lcShip = &lcTmpImp..SHIPNO
lcCuttkt = IIF(!EMPTY(lcShip),'_',&lcTmpImp..PO)
*C128583,5  TMI [End  ] 

*C128583,5  TMI [Start] comment these lines and no need for this check, we will receive all PO's
*-* *C128583,4  TMI [Start] Check the existence for more than one PO
*-* llRcvAll = .F.
*-* IF llOk .AND. EMPTY(lcShip) .AND. !EMPTY(lcCutTkt) 
*-*   DIMENSION laPO[1]
*-*   SELECT DISTINCT PO FROM &lcTmpImp WHERE !EMPTY(PO) AND !DELETED(lcTmpImp) INTO ARRAY laPO
*-*   IF ALEN(laPO) > 1
*-*     *- Buttons : \!\<Yes;\<No;\?\<Cancel
*-*     lnResp = gfModalGen('INM00000B00025',.F.,.F.,.F.,"The csv file contains more than one PO,"+;
*-*                                        " Do you want to create a batch for all contined PO's ?")
*-*     IF lnResp = 3
*-*       RETURN .F.
*-*     ENDIF       
*-*     llRcvAll = ( lnResp = 1 )
*-*   ENDIF
*-* ENDIF
*-* *C128583,4  TMI [End  ] 
llRcvAll = .T.
*C128583,5  TMI [End  ] 

IF llOk
  
  GO TOP IN (lcImported)
  ldRcvDat = &lcImported..RCVDATE 
  lnQtySum = 0

  SELECT (lcTmpImp) 
  *C128583,8  TMI [Start] no need to save delete status
  *-* *C128583,4  TMI [Start] save deleted setting
  *-* lcDel = SET('DELETED')
  *-* SET DELETED OFF
  *C128583,8  TMI [End  ] 
  GO TOP
  *C128583,4  TMI [End  ] 
  *C128583,8  TMI [Start] 
  *SCAN 
  SCAN FOR !DELETED
    *C128583,8  TMI [End  ] 
  
    *C128583,4  TMI [Start] if PO is not included in the received text file , continue ignoring this line
    IF !llRcvAll .AND. !EMPTY(lcCutTkt) .AND. !'_'$lcCutTkt .AND. !EMPTY(PO) .AND. PO <> lcCutTkt
      *C128583,8  TMI [Start] 
      *DELETE
      REPLACE DELETED WITH .T.
      *C128583,8  TMI [End  ] 
      LOOP
    ENDIF
    IF !EMPTY(lcShip) .AND. !EMPTY(SHIPNO) .AND. SHIPNO <> lcShip
      *C128583,8  TMI [Start] 
      *DELETE 
      REPLACE DELETED WITH .T.
      *C128583,8  TMI [End  ] 
      LOOP
    ENDIF
    *C128583,4  TMI [End  ] 
    
    IF EMPTY(PO) .AND. EMPTY(SHIPNO)
      lcErr = 'Both The PO# and the Shipment # are empty.'
      =lfAddError(RECNO(lcTmpImp),lcErr,.T.)
    ENDIF
    
    DO CASE
    CASE !EMPTY(STRTRAN(lcCutTkt,'_'))
      IF !EMPTY(PO) 
        IF !SEEK('P'+PO,'POSHDR'  )
          lcErr = 'The PO#: '+ALLTRIM(PO)+' not found in POSHDR file'
          =lfAddError(RECNO(lcTmpImp),lcErr,.T.)
          *C128583,4  TMI [Start] Prevent hold orders
        ELSE
          IF POSHDR.STATUS = 'H'
            lcErr = 'The PO#: '+ALLTRIM(PO)+' is Hold, can not continue'
            =lfAddError(RECNO(lcTmpImp),lcErr,.T.)
          ENDIF          
          *C128583,4  TMI [End  ] 
        ENDIF 
        IF !llRcvAll .AND. PO <> lcCutTkt
          lcErr = 'The PO#: '+ALLTRIM(PO)+' does not match the selected PO#'
          =lfAddError(RECNO(lcTmpImp),lcErr,.T.)
        ENDIF
        *C128583,4  TMI [Start] 
        *C128583,4  TMI [End  ] 
      ENDIF
    
    CASE !EMPTY(lcShip)    
      IF lcShip <> SHIPNO
        lcErr = 'Missing or invalid shipment #'
        =lfAddError(RECNO(lcTmpImp),lcErr,.T.)
      ENDIF
      IF !EMPTY(SHIPNO)
        IF !SEEK(SHIPNO,'SHPMTHDR')
          lcErr = 'The Shipment#: '+ALLTRIM(SHIPNO)+' is invalid'
          =lfAddError(RECNO(lcTmpImp),lcErr,.T.)
        ELSE
          *C128583,4  TMI [Start] Prevent hold shipments
          IF SHPMTHDR.STATUS = 'H'
            lcErr = 'The Shipment#: '+ALLTRIM(SHIPNO)+' is Hold, can not continue'
            =lfAddError(RECNO(lcTmpImp),lcErr,.T.)
          ENDIF          
          *C128583,4  TMI [End  ] 
          
          lcSvOrd = ORDER('POSLN')
          SET ORDER TO POSLNSH IN POSLN
          IF !EMPTY(PO) .AND. !SEEK(SHIPNO+'P'+PO,'POSLN')
            lcErr = 'The PO#: '+ALLTRIM(PO)+' is not assigned to the Shipment#: '+ALLTRIM(Shipno)
            =lfAddError(RECNO(lcTmpImp),lcErr,.T.)
          ENDIF
          SET ORDER TO &lcSvOrd IN POSLN
        ENDIF
      ENDIF    
    ENDCASE
    
    
    IF EMPTY(Date)
      lcErr = 'Empty date'
      =lfAddError(RECNO(lcTmpImp),lcErr,.T.)
    ELSE
      REPLACE RCVDATE WITH CTOD(stuff(stuff(date,3,0,'/'),6,0,'/'))
      ldRcvDat = RCVDATE
      IF EMPTY(RCVDATE)
        lcErr = 'Invalid date'
        =lfAddError(RECNO(lcTmpImp),lcErr,.T.)
      ELSE
        *C128583,4  TMI [Start] do not care if there is more than one receiving date
        *-* IF !EMPTY(ldRcvDat) .AND. ldRcvDat <> RCVDATE
        *-*   lcErr = 'Multiple receiving dates'
        *-*   INSERT INTO &lcTmpLog (NRECNO,CERROR,LERROR) VALUES (RECNO(lcTmpImp),lcErr,.F.)
        *-* ENDIF        
        *C128583,4  TMI [End  ] 
      ENDIF
    ENDIF
    
    IF !SEEK(CSTYMAJOR,'STYLE')
      lcErr = 'Style '+ALLTRIM(CSTYMAJOR)+' is not found in the style file'
      =lfAddError(RECNO(lcTmpImp),lcErr,.T.)
    ELSE
      IF STYLE.STATUS = 'X'
        lcErr = 'Style '+ALLTRIM(CSTYMAJOR)+' is cancelled'
        =lfAddError(RECNO(lcTmpImp),lcErr,.T.)
      ENDIF
    ENDIF
 
    IF EMPTY(StyleUPC)
      lcErr = 'Empty UPC'
      =lfAddError(RECNO(lcTmpImp),lcErr,.T.)
    ELSE   
      IF !SEEK(StyleUPC,'STYLEUPC')
        lcErr = 'UPC '+ALLTRIM(StyleUPC)+' not found in the StyleUpc file'
        =lfAddError(RECNO(lcTmpImp),lcErr,.T.)
      ELSE
        IF SUBSTR(STYLEUPC.STYLE,1,lnstylelen) <> &lcTmpImp..CSTYMAJOR
          lcErr = 'The UPC '+ALLTRIM(STYLEUPC)+' does not match the style '+ALLTRIM(&lcTmpImp..CSTYMAJOR)
          =lfAddError(RECNO(lcTmpImp),lcErr,.T.)
        ENDIF
      ENDIF
    ENDIF
    
    *- Update fields      
    =SEEK(CSTYMAJOR,'STYLE')
    REPLACE cStyDesc  WITH STYLE.DESC1 ;
            lCrStyDye WITH (Style.cDye_Flg = "Y") ;
            nPrice    WITH Style.nICost1
    =SEEK(STYLEUPC,'STYLEUPC')    .AND. ;
       SEEK(STYLEUPC.STYLE,'STYLE') .AND. ;
       SEEK('S'+STYLE.SCALE,'SCALE')
    lcSz = ALLTRIM(STYLEUPC.SIZE)    
    IF !EMPTY(lcSz)
      REPLACE STYLE  WITH STYLEUPC.STYLE ;
              SIZE   WITH lcSz           ;
              SZDESC WITH SCALE.SZ&lcSz   
      *C128583,7  TMI [Start] 
    ELSE
      lcErr = 'Size not specified '
      =lfAddError(RECNO(lcTmpImp),lcErr,.T.)
      *C128583,7  TMI [End  ]               
    ENDIF
    
    IF !EMPTY(SHIPNO)
      lcSvOrd = ORDER('POSLN')
      SET ORDER TO POSLNSH IN POSLN
      IF !EMPTY(PO)
        IF !SEEK(SHIPNO+'P'+PO+STYLE,'POSLN')
          lcErr = 'The style ' +ALLTRIM(STYLE)+ ' is not in the shipment# ' + SHIPNO + ' PO# ' + PO
          =lfAddError(RECNO(lcTmpImp),lcErr,.T.)
        ELSE
          REPLACE CWARECODE WITH POSLN.CWARECODE
        ENDIF        
      ELSE        
        *C128583,7  TMI [Start] missing PO#
        *-* =SEEK(SHIPNO,'POSLN')
        *-* SELECT POSLN
        *-* LOCATE WHILE SHIPNO = &lcTmpImp..SHIPNO FOR STYLE = &lcTmpImp..STYLE
        *-* IF !FOUND()
        *-*   lcErr = 'The style ' +ALLTRIM(&lcTmpImp..STYLE)+ ' is not in the Shipment# ' + &lcTmpImp..SHIPNO
        *-*   =lfAddError(RECNO(lcTmpImp),lcErr,.T.)
        *-* ELSE
        *-*   SELECT &lcTmpImp
        *-*   REPLACE PO        WITH POSLN.PO ;
        *-*           CWARECODE WITH POSLN.CWARECODE
        *-* ENDIF
          lcErr = 'Missing PO#'
          =lfAddError(RECNO(lcTmpImp),lcErr,.T.)
        *C128583,7  TMI [End  ] 
      ENDIF
      SET ORDER TO &lcSvOrd IN POSLN
    ELSE
      IF !SEEK('P'+PO+STYLE,'POSLN')
        lcErr = 'The style ' +ALLTRIM(STYLE)+ ' is not in the PO# ' + PO
        =lfAddError(RECNO(lcTmpImp),lcErr,.T.)
      ELSE
        REPLACE CWARECODE WITH POSLN.CWARECODE
      ENDIF        
    ENDIF      

    *- Accomulate check qty variable
    lnQtySum = lnQtySum + &lcTmpImp..QTY

    *- Check if this line is imported befor
    IF SEEK(SHIPNO+PO+STYLE,lcTmpRecv)
      lcErr = 'This line is imported befor in the same session.'
      =lfAddError(RECNO(lcTmpImp),lcErr,.T.)
    ENDIF

  ENDSCAN
  *C128583,8  TMI [Start] no need for this line 
  *-* *C128583,4  TMI [Start] restore deletion status
  *-* SET DELETED &lcDel
  *-* *C128583,4  TMI [End  ] 
  *C128583,8  TMI [End  ] 
  
  llOk = !SEEK(.T.,lcTmpLog) .AND. lnQtySum > 0
ELSE
  lcErr = 'No valid lines in the imported file'
  =lfAddError(0,lcErr,.T.)
ENDIF

IF llOk
  
  *=lfChkStru(lcTmpRecv)
  *C128583,3  TMI [Start] 
  SET ORDER TO POSLNSH IN POSLN
  *C128583,3  TMI [End  ] 

  SELECT (lcTmpImp)
  *C128583,8  TMI [Start] 
  *SCAN    
  GO TOP                            
  SCAN FOR !DELETED
    *C128583,8  TMI [End  ] 
    SELECT (lcTmpRecv)
    APPEND BLANK                      
    REPLACE cTranTyp  WITH lcTranTyp ;
            PO_CT     WITH &lcTmpImp..PO ;
            Ship      WITH &lcTmpImp..SHIPNO ;
            Carton    WITH IIF(lcTranTyp $ "MI" , cbByCrtn , .F.) ;
            cStatus   WITH "O" ;
            cCarton   WITH IIF(lcTranTyp $ "MI" , lcCarton , "") 
    REPLACE cWareCode WITH &lcTmpImp..cWareCode ;
            Style     WITH &lcTmpImp..STYLE     ;
            Size      WITH &lcTmpImp..SIZE      ;
            nQty      WITH &lcTmpImp..QTY       ;
            cStyDesc  WITH &lcTmpImp..cStyDesc  ;
            lCrStyDye WITH &lcTmpImp..lCrStyDye ;
            nPrice    WITH &lcTmpImp..nPrice    ;
            SzDesc    WITH &lcTmpImp..SzDesc    ;
            lFoundUPC WITH .T. ;
            lByUpc    WITH .T. ;
            lAccepted WITH .T. ;
            lAdjustPO WITH .F. 
    *C128583,3  TMI [Start] Update line # 
    IF SEEK(SHIP+'P'+PO_CT+STYLE,'POSLN')
      REPLACE Dyelot    WITH POSLN.Dyelot ;
              cWareCode WITH POSLN.cWareCode ;
              nLineNo   WITH POSLN.LineNo ;
              Reference WITH POSLN.Reference
    ENDIF
    *C128583,3  TMI [End  ] 
    
    =lfAddLns() 
    
  ENDSCAN
  
  *C128583,3  TMI [Start] 
  SET ORDER TO POSLN IN POSLN
  *C128583,3  TMI [End  ] 
  
ENDIF

llCollect = llOk

*- Show error log if any
IF !llOk .AND. ;
  gfModalGen('INM00000B00006',.F.,.F.,.F.,'Some errors detected,would you like to view the error log file') = 1
  PRIVATE lmStrRep,lnLogLnNo
  lnLogLnNo = 0
  lcWinTitl  = "Log Report"
  
  CREATE CURSOR TMPSTR (mStrRep M(120))
  SELECT TMPSTR 
  APPEND BLANK	
  lcErr = REPLICATE('=',68) + CHR(13) + ;
          'Data imported from file '+lcImpFile+' at Date:'+DTOC(DATE())+' Time:'+TIME() + CHR(13) + ;
          REPLICATE('=',68) + CHR(13) + ; 
          'Sn.-Line-          Description            ' + CHR(13) + ; 
          REPLICATE('=',68) 
  REPLACE mStrRep WITH lcErr
  
  SELECT (lcTmpLog)
  SET ORDER TO NRECNO IN &lcTmpLog
  GO TOP
  SCAN
    lmStrRep = CHR(13)
    IF nRecNo = 0 
      lmStrRep = lmStrRep + ALLTRIM(cError) 
    ELSE
      lnLogLnNo = lnLogLnNo + 1
      lmStrRep = lmStrRep + PADL(lnLogLnNo,3)+'-'+PADL(nRecNo,4)+'-'+ALLTRIM(cError)
    ENDIF                                     
    SELECT TMPSTR 
    REPLACE mStrRep WITH lmStrRep ADDITIVE
  ENDSCAN
  SELECT TMPSTR 
  DO (gcScrDir + 'SM\SMSTRREP.SPR')
  USE IN TMPSTR
ENDIF

*- Update the table of imported files
IF llOk
  INSERT INTO &lcImported (IMPFILE,RCVDATE) VALUES (lcImpFile,ldRcvDat)
ENDIF

*- Restore styleupc order
SET ORDER TO &lcStyUpcOrd IN STYLEUPC

USE IN &lcTmpLog
USE IN &lcTmpImp
ERASE (gcWorkDir+lcTmpImp+'.DBF')
ERASE (gcWorkDir+lcTmpImp+'.CDX')

*-- Blank the temp cursor that appended in the standard , to prevent adding unneeded lines
SELECT (lcTmpCurs)
ZAP

SELECT (lnSlct)
RETURN llOk
*-- end of lfIMPANFL.


*:**************************************************************************
*:* Name        : lfCrtTmpFl
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/22/2005
*:* Purpose     : Create needed temp files
*:***************************************************************************
*C128583,1
FUNCTION lfCrtTmpFl
PRIVATE laStruArr,lnI

lnI = 0

lnI = lnI + 1
DIMENSION laStruArr[lnI,4]
laStruArr[lnI,1] = 'PO'
laStruArr[lnI,2] = 'C'
laStruArr[lnI,3] = 6
laStruArr[lnI,4] = 0

lnI = lnI + 1
DIMENSION laStruArr[lnI,4]
laStruArr[lnI,1] = 'SHIPNO'
laStruArr[lnI,2] = 'C'
laStruArr[lnI,3] = 6
laStruArr[lnI,4] = 0

lnI = lnI + 1
DIMENSION laStruArr[lnI,4]
laStruArr[lnI,1] = 'CSTYMAJOR'
laStruArr[lnI,2] = 'C'
laStruArr[lnI,3] = lnstylelen
laStruArr[lnI,4] = 0

lnI = lnI + 1
DIMENSION laStruArr[lnI,4]
laStruArr[lnI,1] = 'StyleUPC'
laStruArr[lnI,2] = 'C'
laStruArr[lnI,3] = 13
laStruArr[lnI,4] = 0

lnI = lnI + 1
DIMENSION laStruArr[lnI,4]
laStruArr[lnI,1] = 'Qty'
laStruArr[lnI,2] = 'N'
laStruArr[lnI,3] = 6
laStruArr[lnI,4] = 0

lnI = lnI + 1
DIMENSION laStruArr[lnI,4]
laStruArr[lnI,1] = 'Date'
laStruArr[lnI,2] = 'C'
laStruArr[lnI,3] = 8
laStruArr[lnI,4] = 0

lnI = lnI + 1
DIMENSION laStruArr[lnI,4]
laStruArr[lnI,1] = 'RCVDATE'
laStruArr[lnI,2] = 'D'
laStruArr[lnI,3] = 8
laStruArr[lnI,4] = 0

lnI = lnI + 1
DIMENSION laStruArr[lnI,4]
laStruArr[lnI,1] = 'STYLE'
laStruArr[lnI,2] = 'C'
laStruArr[lnI,3] = 19
laStruArr[lnI,4] = 0

lnI = lnI + 1
DIMENSION laStruArr[lnI,4]
laStruArr[lnI,1] = 'SIZE'
laStruArr[lnI,2] = 'C'
laStruArr[lnI,3] = 1
laStruArr[lnI,4] = 0

lnI = lnI + 1
DIMENSION laStruArr[lnI,4]
laStruArr[lnI,1] = 'SzDesc'
laStruArr[lnI,2] = 'C'
laStruArr[lnI,3] = 5
laStruArr[lnI,4] = 0

lnI = lnI + 1
DIMENSION laStruArr[lnI,4]
laStruArr[lnI,1] = 'cStyDesc'
laStruArr[lnI,2] = 'C'
laStruArr[lnI,3] = 30
laStruArr[lnI,4] = 0

lnI = lnI + 1
DIMENSION laStruArr[lnI,4]
laStruArr[lnI,1] = 'lCrStyDye'
laStruArr[lnI,2] = 'L'
laStruArr[lnI,3] = 1
laStruArr[lnI,4] = 0

lnI = lnI + 1
DIMENSION laStruArr[lnI,4]
laStruArr[lnI,1] = 'nPrice'
laStruArr[lnI,2] = 'N'
laStruArr[lnI,3] = 13
laStruArr[lnI,4] = 3

lnI = lnI + 1
DIMENSION laStruArr[lnI,4]
laStruArr[lnI,1] = 'CWARECODE'
laStruArr[lnI,2] = 'C'
laStruArr[lnI,3] = 6
laStruArr[lnI,4] = 0

*C128583,8  TMI [Start] Add delete status field
lnI = lnI + 1
DIMENSION laStruArr[lnI,4]
laStruArr[lnI,1] = 'DELETED'
laStruArr[lnI,2] = 'L'
laStruArr[lnI,3] = 1
laStruArr[lnI,4] = 0
*C128583,8  TMI [End  ] 

CREATE TABLE (gcWorkDir+lcTmpImp) FROM ARRAY laStruArr
*C128583,4  TMI [Start] add index to "lcTmpImp" file 
INDEX ON SHIPNO+PO TAG SHIPNOPO
*C128583,4  TMI [End  ] 

*- Create a temp log file 
CREATE CURSOR (lcTmpLog) (NRECNO N(5),CERROR C(200),LERROR L)  
INDEX ON NRECNO TAG NRECNO
INDEX ON LERROR TAG LERROR

IF !USED(lcImported)
  CREATE TABLE (gcWorkDir+lcImported) (IMPFILE C(200),RCVDATE D)
ENDIF
*-- end of lfCrtTmpFl.

*:**************************************************************************
*:* Name        : lfAddError
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 09/01/2005
*:* Purpose     : Add error line to the error log file
*:***************************************************************************
FUNCTION lfAddError
PARAMETERS lnRecno,lcErr,llErr
*C128583,8  TMI [Start] 
*IF !DELETED(lcTmpImp)
IF !&lcTmpImp..DELETED
  *C128583,8  TMI [End  ] 
  INSERT INTO &lcTmpLog (NRECNO,CERROR,LERROR) VALUES (lnRecno,lcErr,llErr)
ENDIF
*-- end of lfAddError.

*!*************************************************************
*! Name      : lfAddLns
*! Developer : Abdou Elgendi [ABD]
*! Date      : 06/19/2000
*! Purpose   : Add Lines In temp File for recive lines.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfAddLns()
*!*************************************************************
*C128583,1
FUNCTION lfAddLns
PARAMETERS llSendUPC
Private lnAlias , lcSizes

IF TYPE('llSendUPC') = "U"
  llSendUPC = .F.
ENDIF
lnAlias = SELECT(0)
SELECT (lcTempRsln)
IF SEEK(&lcTmpRecv..Style)
  =SEEK('Total')
  REPLACE nRecTotqty WITH nRecTotqty + &lcTmpRecv..nQty         ,;
          nBalTotqty WITH nPoTotqty  - nRecTotqty
  SET SKIP OF PAD _INQUIRY OF _MSYSMENU .F.          
ENDIF
=SEEK(&lcTmpRecv..Style)
lcSizes = &lcTmpRecv..SIZE
REPLACE cStyDesc         WITH &lcTmpRecv..cStyDesc                        ,;
        nRecQty&lcSizes  WITH nRecQty&lcSizes + &lcTmpRecv..nQty          ,;
        nRecTotqty       WITH nRecTotqty      + &lcTmpRecv..nQty          ,;
        nBalaqty&lcSizes WITH nPOqty&lcSizes  - nRecQty&lcSizes,;
        nBalTotqty       WITH nPoTotqty       - nRecTotqty         
IF llSeltscrn
  =lfshplins()
ENDIF

SELECT(lnAlias)

*-- End Of lfAddLns.

*:**************************************************************************
*:* Name        : lfSHPIMPDR
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/21/2005
*:* Purpose     : Ship import dirctory
*:***************************************************************************
*:* Called from : sycconfg option grid
*:***************************************************************************
*C128583,1
FUNCTION lfGETDIR
PARAMETERS lcOrgDir
PRIVATE lcTmpName,lnHndl,lcTitl
IF !EMPTY(&lcOrgDir) 
  lcTmpName = gfTempName()
  &lcOrgDir = ALLTRIM(UPPER(&lcOrgDir))
  &lcOrgDir = &lcOrgDir + IIF(RIGHT(&lcOrgDir,1)='\','','\')
  lnHndl = FCREATE(&lcOrgDir.+lcTmpName+'.tmp')
  IF lnHndl < 0
    lcTitl = IIF('IMP'$ lcOrgDir , 'Import','Archive')
    &lcOrgDir = GETDIR('',lcTitl+' Shipment Directory')
  ENDIF
  =FCLOSE(lnHndl)
  ERASE (&lcOrgDir+lcTmpName+'.tmp')
ENDIF
*-- end of lfSHPIMPDR.

*:**************************************************************************
*:* Name        : lfVALIDIR
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/21/2005
*:* Purpose     : Check shipment import and archive directories are valid
*:***************************************************************************
*C128583,1
FUNCTION lfVALIDIR
PRIVATE llRet,lcSvPath,lcImp
llRet = .T.

lcImp = gfGetMemVar('M_SHIPIMP')
IF EMPTY(lcImp)
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'The shipment import directory is empty')
  llRet = .F.
ENDIF

IF EMPTY(gfGetMemVar('M_SHIPARC'))  
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'The shipment Archive directory is empty')
  llRet = .F.
ENDIF

*-- Get the text file hold the styles.
IF llRet
  lcSvPath = SET('DEFAULT')+SYS(2003)
  SET DEFAULT TO &lcImp
  lcImpFile = GETFILE('CSV', 'Select a CSV File', 'Select',1)
  *C128583,4  TMI [Start] 
  IF !FILE(lcImpFile)
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'File does not exist')
    lcImpFile = ''
  ENDIF
  *C128583,4  TMI [End  ] 
  SHOW GET lcImpFile
  SET DEFAULT TO &lcSvPath
ENDIF  

RETURN llRet
*-- end of lfVALIDIR.

*:**************************************************************************
*:* Name        : lfSHIPSLCT
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/21/2005
*:* Purpose     : if a shipment is selected then disable the PO field
*:***************************************************************************
*:* Called from : lfvShip IN mfrcall.prg 
*:***************************************************************************
*C128583,1
FUNCTION lfSHIPSLCT
PRIVATE lcStat
lcCuttkt = IIF(!EMPTY(lcShip),'_','')
lcStat = IIF(!EMPTY(lcShip),'DISABLE','ENABLE')
SHOW GET lcCuttkt &lcStat
SHOW GET ibtkt    &lcStat
*-- end of lfSHIPSLCT.

*:**************************************************************************
*:* Name        : lfAPPROVE
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/22/2005
*:* Purpose     : Approve the batch saved in the screen mfrcall
*:***************************************************************************
*:* Called from : lpsavscr in mfrcvtp.prg
*:***************************************************************************
*C128583,1
FUNCTION lfAPPROVE
llBatExist = .F.
RETURN .T.
*-- end of lfAPPROVE.

*:**************************************************************************
*:* Name        : lfSAVBATCH
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/22/2005
*:* Purpose     : Update the ctktrcvh.IMPFILE field
*:***************************************************************************
*C128583,1
FUNCTION lfSAVBATCH
PRIVATE lnSlct,lcImported,lnPos
lnSlct = SELECT()

lcImported = 'I_'+SUBSTR(lcRcAll0,3)
SELECT (lcImported)
GO TOP

lnPos = ASCAN(laUsrFields,'DRCVDATE')
IF lnPos>0
  lnPos = ASUBSCRIPT(laUsrFields,lnPos,1)
  laUsrFields[lnPos,6] = &lcImported..RCVDATE
ENDIF

lnPos = ASCAN(laUsrFields,'CIMPFILE')
IF lnPos>0
  lnPos = ASUBSCRIPT(laUsrFields,lnPos,1)
  laUsrFields[lnPos,6] = &lcImported..IMPFILE
ENDIF 

SELECT (lnSlct)
*-- end of lfSAVBATCH.

*:**************************************************************************
*:* Name        : lfDSCRPNCY
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/23/2005
*:* Purpose     : Create a descripance log file and update the shipment if needed
*:***************************************************************************
*:* Called from : lpSavScr IN MFRCVTP.PRG
*:***************************************************************************
FUNCTION lfDSCRPNCY
PRIVATE lnSlct,lcSvOrd,lcRcvOrd,lcOrd,lcTrancd,lcTmpCur,lcLine,llDiscrepance,;
        lnShipTot
lnSlct = SELECT()

llDiscrepance = .F.
lcShip = PADR(lcShip,6)

*C128583,5  TMI [Start] use the name TMPSTR 
*lcTmpCur = gfTempName()
lcTmpCur = 'TMPSTR'
*C128583,5  TMI [End  ] 
CREATE CURSOR &lcTmpCur (mStrRep M)
APPEND BLANK
REPLACE mStrRep WITH REPLICATE('=',68)+CHR(13)+;
                     'Discrepancies report'+CHR(13)+;
                     REPLICATE('=',68)+CHR(13)+;
                     'PO#    STYLE#              SZ1  SZ2  SZ3  SZ4  SZ5  SZ6  SZ7  SZ8'+CHR(13)

lcSvOrd = ORDER('POSLN')
lcRcvOrd = ORDER('CTKTRCVL')
SET ORDER TO CTKTRCVL IN CTKTRCVL
IF SEEK('I'+laData[1],'CTKTRCVL')
  lcTrancd = IIF( !EMPTY(lcShip) , '3','1')
  lnShipTot = 0
  SET ORDER TO POSLNSH IN POSLN
  SELECT CTKTRCVL
  SCAN REST WHILE CTYPE+TMPRCVNUM+CUTTKT+STYLE+DYELOT+CCARTON+STR(NLINENO,6)+STR(LINENO,6)+TRANCD = 'I'+laData[1]
    IF SEEK(lcShip+'P'+CUTTKT+STYLE,'POSLN')
      *-* llFound = .F.
      lcKey = lcShip+'P'+CUTTKT+STYLE
      *-* IF lcTrancd = '3'
      *-*   SELECT POSLN
      *-*   LOCATE REST WHILE SHIPNO+CSTYTYPE+PO+STYLE+STR(LINENO,6)+TRANCD = lcKey FOR TRANCD = '3'
      *-*   llFound = FOUND()
      *-* ENDIF
      DIMENSION laOpnQty[8]
      =lfGetOpn(TRANCD)
      lcLine = ''
      IF laOpnQty[1] <> CTKTRCVL.QTY1 .OR. ;
         laOpnQty[2] <> CTKTRCVL.QTY2 .OR. ;
         laOpnQty[3] <> CTKTRCVL.QTY3 .OR. ;
         laOpnQty[4] <> CTKTRCVL.QTY4 .OR. ;
         laOpnQty[5] <> CTKTRCVL.QTY5 .OR. ;
         laOpnQty[6] <> CTKTRCVL.QTY6 .OR. ;
         laOpnQty[7] <> CTKTRCVL.QTY7 .OR. ;
         laOpnQty[8] <> CTKTRCVL.QTY8 
        llDiscrepance = .T.
        =SEEK(STYLE,'STYLE')
        =SEEK('S'+STYLE.SCALE,'SCALE')
        lcLine = CTKTRCVL.CUTTKT+' '+CTKTRCVL.STYLE + ' ' + ;
                 SCALE.SZ1 + SCALE.SZ2 + SCALE.SZ3 + SCALE.SZ4 + ;
                 SCALE.SZ5 + SCALE.SZ6 + SCALE.SZ7 + SCALE.SZ8 + CHR(13)
        lcLine = lcLine +  SPACE(27) ;
               + IIF(laOpnQty[1] <> CTKTRCVL.QTY1 , PADR(CTKTRCVL.QTY1 - laOpnQty[1],5) , '     ' ) + ;
               + IIF(laOpnQty[2] <> CTKTRCVL.QTY2 , PADR(CTKTRCVL.QTY2 - laOpnQty[2],5) , '     ' ) + ;
               + IIF(laOpnQty[3] <> CTKTRCVL.QTY3 , PADR(CTKTRCVL.QTY3 - laOpnQty[3],5) , '     ' ) + ;
               + IIF(laOpnQty[4] <> CTKTRCVL.QTY4 , PADR(CTKTRCVL.QTY4 - laOpnQty[4],5) , '     ' ) + ;
               + IIF(laOpnQty[5] <> CTKTRCVL.QTY5 , PADR(CTKTRCVL.QTY5 - laOpnQty[5],5) , '     ' ) + ;
               + IIF(laOpnQty[6] <> CTKTRCVL.QTY6 , PADR(CTKTRCVL.QTY6 - laOpnQty[6],5) , '     ' ) + ;
               + IIF(laOpnQty[7] <> CTKTRCVL.QTY7 , PADR(CTKTRCVL.QTY7 - laOpnQty[7],5) , '     ' ) + ;
               + IIF(laOpnQty[8] <> CTKTRCVL.QTY8 , PADR(CTKTRCVL.QTY8 - laOpnQty[8],5) , '     ' ) + ;
               + CHR(13)
      ENDIF
      
      *- for ship case update the posln type 3 lines
      IF !EMPTY(lcLine)
        SELECT &lcTmpCur
        REPLACE MSTRREP WITH lcLine ADDITIVE
        *-* IF llFound
        IF POSLN.TRANCD='3'
          SELECT POSLN
          *C128583,5  TMI [Start] If the received qty in the import file is less than the qty in 
          * the shipment record then reduce the shipment qty – and if the received qty is greater
          * than shipment qty – then book the received qty into stock and increase the shipment record
          *REPLACE QTY1 WITH CTKTRCVL.QTY1 ;
                  QTY2 WITH CTKTRCVL.QTY2 ;
                  QTY3 WITH CTKTRCVL.QTY3 ;
                  QTY4 WITH CTKTRCVL.QTY4 ;
                  QTY5 WITH CTKTRCVL.QTY5 ;
                  QTY6 WITH CTKTRCVL.QTY6 ;
                  QTY7 WITH CTKTRCVL.QTY7 ;
                  QTY8 WITH CTKTRCVL.QTY8 ;
                  TOTQTY WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8                  
          REPLACE QTY1 WITH IIF(QTY1>CTKTRCVL.QTY1 , QTY1 , CTKTRCVL.QTY1) ;
                  QTY2 WITH IIF(QTY2>CTKTRCVL.QTY2 , QTY2 , CTKTRCVL.QTY2) ;
                  QTY3 WITH IIF(QTY3>CTKTRCVL.QTY3 , QTY3 , CTKTRCVL.QTY3) ;
                  QTY4 WITH IIF(QTY4>CTKTRCVL.QTY4 , QTY4 , CTKTRCVL.QTY4) ;
                  QTY5 WITH IIF(QTY5>CTKTRCVL.QTY5 , QTY5 , CTKTRCVL.QTY5) ;
                  QTY6 WITH IIF(QTY6>CTKTRCVL.QTY6 , QTY6 , CTKTRCVL.QTY6) ;
                  QTY7 WITH IIF(QTY7>CTKTRCVL.QTY7 , QTY7 , CTKTRCVL.QTY7) ;
                  QTY8 WITH IIF(QTY8>CTKTRCVL.QTY8 , QTY8 , CTKTRCVL.QTY8) ;
                  TOTQTY WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8                  
          *C128583,5  TMI [End  ] 

          *C128583,4  TMI [Start] Update style and stydye files with intransit qty's
          =SEEK(POSLN.STYLE,'STYLE')
          SELECT STYLE
          REPLACE INTRANS1 WITH INTRANS1 + ( CTKTRCVL.QTY1 - laOpnQty[1] ) ;
                  INTRANS2 WITH INTRANS2 + ( CTKTRCVL.QTY2 - laOpnQty[2] ) ;
                  INTRANS3 WITH INTRANS3 + ( CTKTRCVL.QTY3 - laOpnQty[3] ) ;
                  INTRANS4 WITH INTRANS4 + ( CTKTRCVL.QTY4 - laOpnQty[4] ) ;
                  INTRANS5 WITH INTRANS5 + ( CTKTRCVL.QTY5 - laOpnQty[5] ) ;
                  INTRANS6 WITH INTRANS6 + ( CTKTRCVL.QTY6 - laOpnQty[6] ) ;
                  INTRANS7 WITH INTRANS7 + ( CTKTRCVL.QTY7 - laOpnQty[7] ) ;
                  INTRANS8 WITH INTRANS8 + ( CTKTRCVL.QTY8 - laOpnQty[8] ) ;
                  TOTINTRN WITH INTRANS1+INTRANS2+INTRANS3+INTRANS4+INTRANS5+INTRANS6+INTRANS7+INTRANS8

          =SEEK(POSLN.STYLE+POSLN.CWARECODE+SPACE(10),'STYDYE')
          SELECT STYDYE
          REPLACE INTRANS1 WITH INTRANS1 + ( CTKTRCVL.QTY1 - laOpnQty[1] ) ;
                  INTRANS2 WITH INTRANS2 + ( CTKTRCVL.QTY2 - laOpnQty[2] ) ;
                  INTRANS3 WITH INTRANS3 + ( CTKTRCVL.QTY3 - laOpnQty[3] ) ;
                  INTRANS4 WITH INTRANS4 + ( CTKTRCVL.QTY4 - laOpnQty[4] ) ;
                  INTRANS5 WITH INTRANS5 + ( CTKTRCVL.QTY5 - laOpnQty[5] ) ;
                  INTRANS6 WITH INTRANS6 + ( CTKTRCVL.QTY6 - laOpnQty[6] ) ;
                  INTRANS7 WITH INTRANS7 + ( CTKTRCVL.QTY7 - laOpnQty[7] ) ;
                  INTRANS8 WITH INTRANS8 + ( CTKTRCVL.QTY8 - laOpnQty[8] ) ;
                  TOTINTRN WITH INTRANS1+INTRANS2+INTRANS3+INTRANS4+INTRANS5+INTRANS6+INTRANS7+INTRANS8
          *C128583,4  TMI [End  ]          
          
        ENDIF      
      ENDIF
      lnShipTot = lnShipTot + POSLN.TOTQTY
    ENDIF
  ENDSCAN
  IF !EMPTY(lcShip)
    =SEEK(lcShip,'SHPMTHDR')
    SELECT SHPMTHDR
    REPLACE TOTQTY WITH lnShipTot
    
    *- check lines included in the shipment and not cited in the temp reciving batch, update them to 0
    SELECT POSLN
    =SEEK(lcShip,'POSLN')
    SCAN REST WHILE SHIPNO = lcShip FOR TRANCD = '3'
      IF !SEEK('I'+laData[1]+POSLN.PO+POSLN.STYLE,'CTKTRCVL') 
        REPLACE QTY1   WITH 0 ;
                QTY2   WITH 0 ;
                QTY3   WITH 0 ;
                QTY4   WITH 0 ;
                QTY5   WITH 0 ;
                QTY6   WITH 0 ;
                QTY7   WITH 0 ;
                QTY8   WITH 0 ;
                TOTQTY WITH 0
      ENDIF
    ENDSCAN
  ENDIF

  IF llDiscrepance
    lcWinTitl  = "Log Report"
    SELECT &lcTmpCur
    DO (gcScrDir + 'SM\SMSTRREP.SPR')
    USE IN &lcTmpCur
  ELSE
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No discrepancies found.')
  ENDIF
ENDIF
SET ORDER TO &lcSvOrd IN POSLN
SET ORDER TO &lcRcvOrd IN CTKTRCVL
SELECT (lnSlct)
*-- end of lfDSCRPNCY.

*:**************************************************************************
*:* Name        : lfGetOpn
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 09/01/2005
*:* Purpose     : Get Open qty for the current line in the PO
*:***************************************************************************
FUNCTION lfGetOpn
PARAMETERS lcTrancd
PRIVATE lnSlct,lnRecno,lcKey,lcWareH,lcSvOrd
lnSlct = SELECT()
IF lcTrancd = '3'
  laOpnQty[1] = POSLN.QTY1
  laOpnQty[2] = POSLN.QTY2
  laOpnQty[3] = POSLN.QTY3
  laOpnQty[4] = POSLN.QTY4
  laOpnQty[5] = POSLN.QTY5
  laOpnQty[6] = POSLN.QTY6
  laOpnQty[7] = POSLN.QTY7
  laOpnQty[8] = POSLN.QTY8
  RETURN
ENDIF

SELECT POSLN
lnRecno = RECNO('POSLN')
lcSvOrd = ORDER('POSLN')

lcWareH = cWareCode
laOpnQty   = 0

SET ORDER TO POSLN IN POSLN
lcKey = CSTYTYPE+PO+STYLE+STR(LINENO,6)
=SEEK(lcKey,'POSLN')
SCAN REST WHILE CSTYTYPE+PO+STYLE+STR(LINENO,6)+TRANCD = lcKey ;
            FOR cWareCode = lcWareH .AND. TRANCD <> '3'
  FOR I=1 TO 8
    lcCnt=STR(I,1)
    laOpnQty[I]= IIF( TranCd='1' , laOpnQty[I]+Qty&lcCnt , MAX(laOpnQty[I]-Qty&lcCnt,0) )
  ENDFOR
ENDSCAN

IF BETWEEN(lnRecno,1,RECCOUNT('POSLN'))
  SELECT POSLN
  GOTO (lnRecno)
ENDIF
SET ORDER TO &lcSvOrd IN POSLN

SELECT (lnSlct)
*-- End of lfGetOpn.

*:**************************************************************************
*:* Name        : lfCOPYCSV
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/23/2005
*:* Purpose     : copy csv file from Import folder to archive folder for PAN21
*:***************************************************************************
*:* Called from : lpSavScr IN POSTREC.PRG
*:***************************************************************************
FUNCTION lfCOPYCSV
PRIVATE lnSlct,lcFile,lcImpDir,lcArcDir, lcFrFile , lcToFile
lnSlct = SELECT()

*- Do not run this trigger if the screen called with types other than batch
*C128583,6  TMI [Start] 
*IF lnType <> 3
*  RETURN
*ENDIF
IF laType[lnType,1] = 'Receive P/O Batch'
  *C128583,6  TMI [End  ] 
  SELECT CTKTRCVH
  IF SEEK('I'+lcBatch,'CTKTRCVH') .AND. !EMPTY(CTKTRCVH.CIMPFILE)
    lcImpDir = ALLTRIM(gfGetMemVar('M_SHIPIMP'))
    lcArcDir = ALLTRIM(gfGetMemVar('M_SHIPARC'))
    
    lcFrFile = ALLTRIM(CTKTRCVH.CIMPFILE)
    *C128583,8  TMI [Start] 
    *lcToFile = ALLTRIM(STRTRAN(CTKTRCVH.CIMPFILE,lcImpDir,lcArcDir))
    lcToFile = SUBSTR(lcFrFile,RAT('\',lcFrFile)+1)
    lcToFile = lcArcDir + lcToFile
    *C128583,8  TMI [End  ] 
    *C128583,5  TMI [Start] check file existance befor copying
    IF FILE(lcFrFile)
      *C128583,5  TMI [End  ] 
      COPY FILE (lcFrFile) TO (lcToFile)
      IF FILE(lcToFile)
        ERASE (lcFrFile)
      ENDIF
      *C128583,5  TMI [Start] 
    ENDIF
    *C128583,5  TMI [End  ] 
  ENDIF
  *C128583,6  TMI [Start] 
ENDIF  
*C128583,6  TMI [End  ]   
SELECT (lnSlct)
*-- end of lfCOPYCSV.

*:**************************************************************************
*:* Name        : lfTMPRCVDT
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/30/2005
*:* Purpose     : Recreate the lcTmpRecv temp file with the field DRCVDATE
*:***************************************************************************
*:* Called from : MFRCALL.PRG
*:***************************************************************************
FUNCTION lfTMPRCVDT
PRIVATE laStru,lnI

=AFIELDS(laStru)
IF ASCAN(laStru,'DRCVDATE')=0
  lnI = ALEN(laStru,1) + 1
  DIMENSION laStru[lnI,4]
  laStru[lnI,1] = 'DRCVDATE'
  laStru[lnI,2] = 'D'
  laStru[lnI,3] = 8
  laStru[lnI,4] = 0
  CREATE DBF (gcWorkDir+lcTmpRecv) FROM ARRAY laStru
ENDIF  
*-- end of lfTMPRCVDT.

*:**************************************************************************
*:* Name        : lfADRCVDAT
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/30/2005
*:* Purpose     : Add custom field for Receive Date for PANACH 
*:***************************************************************************
*:* Called from : lfCrtUnComp in POSTREC.PRG
*:***************************************************************************
FUNCTION lfADRCVDAT
PRIVATE lnI
IF ASCAN(laFStru,'DRCVDATE') = 0
  lnI = ALEN(laFStru,1) + 1
  DIMENSION laFStru[lnI,4]
  laFStru[lnI,1] = 'DRCVDATE'
  laFStru[lnI,2] = 'D'
  laFStru[lnI,3] = 8
  laFStru[lnI,4] = 0
ENDIF
IF ASCAN(laFStru,'RCVLNNO') = 0
  lnI = ALEN(laFStru,1) + 1
  DIMENSION laFStru[lnI,4]
  laFStru[lnI,1] = 'RCVLNNO'
  laFStru[lnI,2] = 'N'
  laFStru[lnI,3] = 6
  laFStru[lnI,4] = 0
ENDIF

*-- end of lfADRCVDAT.
*:**************************************************************************
*:* Name        : lfUPDRCVDT
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/23/2005
*:* Purpose     : Update the receiving date as sent in the PAN21 csv file
*:***************************************************************************
*:* Called from : lfvBatch IN POSTREC.PRG
*:***************************************************************************
*C128583,1
FUNCTION lfUPDRCVDT

IF !EMPTY(CTKTRCVH.DRCVDATE)
  ldRcvDate = CTKTRCVH.DRCVDATE 
ENDIF  

*-- end of lfUPDRCVDT.

*:**************************************************************************
*:* Name        : lfRECEIVDT
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/30/2005
*:* Purpose     : Update receive date comming from the imported file into the 
*:*             : batch, do not care with the receiving date on the screen
*:***************************************************************************
*:* Called from : lpSavScr in POUPDATE.PRG
*:***************************************************************************
*C128583
FUNCTION lfRECEIVDT
REPLACE DATE WITH DRCVDATE
*-- end of lfRECEIVDT.

*:**************************************************************************
*:* Name        : lfRCVDAT
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/30/2005
*:* Purpose     : lpSavScr IN MFRCALL.PRG
*:***************************************************************************
*C128583
FUNCTION lfRCVDAT
PRIVATE M.QTY1,M.QTY2,M.QTY3,M.QTY4,M.QTY5,M.QTY6,M.QTY7,M.QTY8,M.TOTQTY
SELECT (lcTmpLine)
IF EMPTY(DRCVDATE)
  REPLACE DRCVDATE WITH &lcTmpRecv..DRCVDATE ;
          RCVLNNO  WITH RECNO() ;
          LINENO   WITH RECNO()
ELSE
  IF DRCVDATE <> &lcTmpRecv..DRCVDATE
    SCATTER MEMVAR 
    APPEND BLANK
    STORE 0 TO M.QTY1,M.QTY2,M.QTY3,M.QTY4,M.QTY5,M.QTY6,M.QTY7,M.QTY8,M.TOTQTY
    GATHER MEMVAR
    REPLACE DRCVDATE WITH &lcTmpRecv..DRCVDATE ;
            RCVLNNO  WITH RECNO() ;
            LINENO   WITH RECNO()
  ENDIF
ENDIF
*-- end of lfRCVDAT.

*:**************************************************************************
*:* Name        : lfRCVLNNO   
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/30/2005
*:* Purpose     : Update the Lineno field from 'ctktrcvl.RCVLNNO' for PAN21
*:***************************************************************************
*:* Called from : lpsavscr in POSTREC.PRG
*:***************************************************************************
*C128583
FUNCTION lfRCVLNNO   
IF lnType = 3
  SELECT &lcTmpLine
  GO TOP
  REPLACE ALL LineNo WITH RCVLNNO ;
              TOTQTY WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8
  
ENDIF  
*-- end of lfRCVLNNO   .

*!**************************************************************************
*!* Name      : lfPrnt
*!* Developer : NNA - NADER NABIL ABD-ALMONAM
*!* Date      : 07/18/2005
*!* Purpose   : Print the 2nd Rep commission difference report.
*!**************************************************************************
*!* Example   : = lfPrnt()
*!**************************************************************************
FUNCTION lfvPrnt

IF pSetup(.T.)
  gcOutFile = gcWorkDir+gfTempName()+'.TXT'
  COPY MEMO TMPSTR.mStrRep TO &gcOutFile
  gcDevice = 'PRINTER'
  DO ENDREPORT
  gcDevice = 'SCREEN'
ENDIF

*-- End of lfvPrnt.

*:**************************************************************************
*:* Name        : lfZAPCURS
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 09/06/2005
*:* Purpose     : zap the temp file lcTmpCurs
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfZAPCURS()
*:***************************************************************************
FUNCTION lfZAPCURS

SELECT (lcTmpCurs)
ZAP

*-- end of lfZAPCURS.