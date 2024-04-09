*:****************************************************************
*: Program file  : SOPRODP.PRG
*: Program desc. : Production Plan (For Tip Top Computers.)
*: For screen    : SPPRODP.SPX
*: System        : Aria Apparel System - Version 2.7.
*: Module        : Sales Order (SO)
*: Developer     : ABDOU ELGENDI -  (ABD)
*: Date          : 01/05/2003
*: Tracking Job Number : C#200451,1
*:****************************************************************
*: Calls 
*:               : FUNCTIONS  :  lfoldvalue , lfvMajor , lfvNonMjr
*:               :            :  lfSegInfo  , lfvSize  , lpShow
*:               :            :  lfCreatTmp , lfCollect, lfvN5days
*:               :            :  lfvB5days  , lfGetWIP , lfGtSlsOrd
*:               :            :  lfvPland   , lfUpdProd, lpBtmScr
*:               -----------------------------------------------
*:               : PROCEDURE  : lpSavScr , lpTopScr , lpBtmScr
*:               :            : lpNxtScr , lpPrvScr
*:****************************************************************
*: Passed Parameters  : None.
*:****************************************************************
*:C#200451,1.
*:****************************************************************
*:Modifications  :
*:B607381,1 ABD 07/14/2003 The customer (Tony) just want to make the 
*:B607381,1 ABD            Fields of “Weekly Sales Forecast” Enabled 
*:B607381,1 ABD            And update the standard forecast file 
*:B607381,1 ABD            with these changes.
*:****************************************************************
*:
EXTERNAL ARRAY laData , laDefProc
DIMENSION laShowFig[10,5] , laSegInfo[1,9]
STORE ''  TO lcStyDesc , lcMjrTtl   , lcMjrPct , lcMjrPct   , lcStyMaj,;
             lcVendor  , lcOldValue , lcNMjrPt , lcNMjrTl   , lcSepart,;
             lcScFields, lcSize     , lcScale  , lcTempScal , lcCurSize,;
             lcTempLine 

STORE 0   TO lnstylewid , lncolorwid , lnXPos , lnGetXPos , lnSize , lnWeekNo ,;
             lnWeeks_No , lnLastWeek , lnAve_cost , laShowFig , lnCurStk  ,;
             lnOldValue
             
STORE .F. TO llbrowse

lcTempScal = gfTempName()
lcTempLine = gfTempName()

*-- This for loop to show the empty date at the headerfields.
FOR I = 1 To 5
  STORE {} TO laShowFig [1,I]
ENDFOR


ldDate      = CTOD('01/01/'+STR(YEAR(gdsysdate),4))
ldEndDate   = GOMONTH(LDdATE,12) - 1
No_Of_weeks = MAX(CEILING(((ldEndDate-DOW(ldEndDate)) - (ldDate-DOW(ldDate)+1))/7),1)
St_Day      = ldDate-DOW(ldDate)+1 

lnWeekNo = (MAX(CEILING((gdsysdate-St_Day)/7),1))
lcCizeSt = 'DISABLE'




*--ToolBar external proc.
laDefProc[1]  = .F.              && Disable the control panel prev proc.(lpTopScr)
laDefProc[2]  = .F.              && Disable the control panel prev proc.(lpBtmScr)
laDefProc[3]  = .F.              && Disable the control panel prev proc.(lpNxtScr)
laDefProc[4]  = .F.              && Disable the control panel prev proc.(lpPrvScr)

laDefProc[7]  = .F.              && Disable the control panel delete proc.(lpDelScr)
laDefProc[9]  = .F.              && Disable the control panel save proc.  (lpSavScr)

IF !gfSetup()
  RETURN
ENDIF

lcScFields ='Style'

*-- Function To Craete Temp File.
=lfCreatTmp()

IF !lfSegInfo() 
  *--Item structure not found, Cannot Proceed.
  =gfModalGen('QRM42080B42001','DIALOG','Item structure not found')
  glQuitting = .T.
  RETURN
ENDIF

FOR I=1 TO ALEN(laSegInfo,1)
  IF laSegInfo[I,3] ='C'
    llDspClrDs = .T.
  ENDIF
ENDFOR
lcProdMode = IIF(laScrMode[3] ,"ENABLE" ,"DISABLE")
laCtrStat[10] = 'DISABLE'

DO (gcScrDir+gcWinAppl+"\SOPROD.SPX")


*--Normal exit.
IF glQuitting

ENDIF
*-- End OF Code.
*:*************************************************************
*: Name      : lfOldValue
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 01/05/2003
*: Purpose   : Function to store old value of the current filed.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lf..()
*:*************************************************************
*:
FUNCTION lfoldvalue

lcOldValue = EVALUATE(SYS(18))
RETURN

*-- En OF lfoldvalue
*:*************************************************************
*: Name      : lfvMajor
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 01/05/2003
*: Purpose   : Valid function to validate style field.
*:*************************************************************
*: Calls     : 
*:             Procedures : 
*:             Functions  : gfStyBrw()
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lf..()
*:*************************************************************
*:
FUNCTION lfvMajor
PRIVATE lnAlias

lnAlias = SELECT (0)


llbrowse = llbrowse OR '?' $ lcMajor
IF llbrowse .OR. (!EMPTY(lcMajor) .AND. !SEEK(lcMajor,'STYLE'))
   IF !llbrowse .AND. !EMPTY(lcMajor) .AND. !SEEK(lcMajor,'STYLE')
     *-- give user message. to browse or reenter.
     IF gfModalGen("QRM00000B42014",.F.,.F.,.F.,'Style : '+lcMajor+'is not found in the data file' ) = 2
       lcMajor = ''
       _CUROBJ = OBJNUM(lcMajor)
       RETURN
     ENDIF
   ENDIF
   llbrowse = .F.
   lcMajor = gfStyBrw('M',"","",.F.)
ENDIF

SELECT (lnAlias)

*-- En OF lfvMajor.
*:*************************************************************
*: Name      : lfvNonMjr
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 01/05/2003
*: Purpose   : Valid function to validate non major part.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : lfGetInfo()
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lf..()
*:*************************************************************
*:
FUNCTION lfvNonMjr
PRIVATE lnAlias , lcOldOrder
lcOldOrder = ''

lnAlias = SELECT(0)
SELECT STYLE
lcOldOrder = ORDER() 
SET ORDER TO STYLE

llbrowse = llbrowse OR '?' $ lcNonMjr

lcNonMjr = IIF(llbrowse,'?',lcNonMjr)

IF EMPTY(lcNonMjr) 
  RETURN
ENDIF


IF llBrowse .OR. !MDOWN()
  llCanBrow = .T.
  IF SEEK(PADR(lcMajor,lnStyleWid)+lcSepart+lcNonMjr,'STYLE')
    llCanBrow = .F.
  ELSE  
    IF !SEEK(PADR(lcMajor,lnStyleWid),'STYLE')  
      lcMajor = ' '
      GO TOP
    ENDIF  
  ENDIF
  
  IF llCanBrow
    IF !EMPTY(lcMajor)
      lcNonMjr = gfStyBrw('N',lcMajor,lcNonMjr,.F.)
      IF !EMPTY(lcNonMjr) AND SUBSTR(lcNonMjr,1,1) = '*'
        =SEEK(lcMajor,'STYLE')
      ENDIF
    ELSE
     lcItemCd = gfStyBrw('I',lcMajor+lcSepart+lcNonMjr,'',.F.)
     lcMajor  = SUBSTR(lcItemCd ,1,lnstylewid)
     lcNonMjr = SUBSTR(lcItemCd ,lnstylewid+2,lnColorwid)
    ENDIF
  ENDIF
ENDIF

IF !EMPTY(lcNonMjr)
  lcStyDesc  = Style.DESC
  lcVendor   = Style.Vendor
  lnAve_cost = Style.Ave_cost
  lcScale    = STYLE.Scale
  SHOW GET lcMajor  DISABLE
  SHOW GET lcNonMjr DISABLE

  SHOW GET ibStyle DISABLE
  SHOW GET ibColor DISABLE
  SHOW GET lcStyDesc
  
  SHOW GET lcSize ENABLE
  SHOW GET ibCize ENABLE
ENDIF

=lfRefresh()

llBrowse = .F.

SELECT STYLE
SET ORDER TO lcOldOrder

SELECT(lnAlias)

RETURN

*-- End OF lfvNonMjr.
*:*************************************************************
*: Name      : lfSegInfo
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 01/05/2003
*: Purpose   : Biuld array for segment information.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : array elements as :
*:-1 MAJOR,NONMAJOR
*:-2 SEGMENT NUMBER
*:-3 SEGMENT TYPE 'FOTQZCDGS'
*:   F-> Free , O-> Other , T->Make , Q->Quality ,Z->Season
*:   C-> Color , D->Division ,G->Group ,S->Size
*:-4 SEGMENT START
*:-5 SEGMENT SIZE
*:-6 SEGMENT FILE
*:-7 SEGMENT FILE TAG
*:-8 SEGMENT FILE KEY
*:-9 SEGMENT FILE FIELD
*:*************************************************************
*: Example   : =lfSegInfo()
*:*************************************************************
*:
FUNCTION lfSegInfo

lnAlias=SELECT()
llStruOp=gfOpenFile(gcDataDir+'ICISTRU','Segno','SH')
IF !SEEK('U1','ICISTRU')
  IF USED('ICISTRU') AND llStruOp
    USE IN ICISTRU
  ENDIF
  RETURN .F.
ENDIF

DIME laMajSeg[1,1]
=gfItemMask(@laMajSeg)
lnMajSeg  = gfItemMask('SM')
lnNonSeg  = gfItemMask('SN')  
lcItemTl  = gfItemMask('HI')
lcMjrTtl  = gfItemMask('HM')
lcNMjrTl  = gfItemMask('HN')
lcMjrPct  = gfItemMask('PM')
lcNMjrPt  = gfItemMask('PN')
lcIMjrPt  = gfItemMask('PI')
lnstylewid=LEN(lcMjrPct)
lncolorwid=LEN(lcNMjrPt)
lcSepart  =SUBSTR(lcIMjrPt,lnstylewid+1,1)



STORE 25 TO lnXPos
STORE 25 TO lnGetXPos

lnXPos    = 15.3 + lnstylewid + 0.89
lnGetXPos = 15.3 + lnXPos + 0.89


FOR lnCnt=1 TO ALEN(laMajSeg,1)
  DIME laSegInfo[lnCnt,9]
  IF lnCnt<=lnMajSeg
     laSegInfo[lnCnt,1] = 'M' 
  ELSE
    IF lnCnt<=lnNonSeg+lnMajSeg
      laSegInfo[lnCnt,1] = 'N' 
    ELSE
      laSegInfo[lnCnt,1] = ' ' 
    ENDIF
  ENDIF
  laSegInfo[lnCnt,2]=STR(lnCnt,1)
  laSegInfo[lnCnt,3]=laMajSeg[lnCnt,1]
  laSegInfo[lnCnt,4]=laMajSeg[lnCnt,4]
  laSegInfo[lnCnt,5]=LEN(laMajSeg[lnCnt,3])
  laSegInfo[lnCnt,6]=IIF(laMajSeg[lnCnt,1]$'FOQT','ICSEGVAL',;
                     IIF(laMajSeg[lnCnt,1]='S','SCALE','CODES'))
  laSegInfo[lnCnt,7]=IIF(laMajSeg[lnCnt,1]$'FOQT','SEGVAL',;
                     IIF(laMajSeg[lnCnt,1]='S','SCALE','Idrltfname'))
  
  laSegInfo[lnCnt,8]=IIF(laMajSeg[lnCnt,1]$'FOQT',STR(lnCnt,1),;
                     IIF(laMajSeg[lnCnt,1]='S','S','NN'))
  IF laMajSeg[lnCnt,1]$'CZDGF'
    DO CASE
      CASE laSegInfo[lnCnt,3]='C'
        lcCdTyp = 'COLOR     '
      CASE laSegInfo[lnCnt,3]='Z'
        lcCdTyp = 'SEASON    '
      CASE laSegInfo[lnCnt,3]='D'
        lcCdTyp = 'CDIVISION '
      CASE laSegInfo[lnCnt,3]='G'
        lcCdTyp = 'CSTYGROUP '
      CASE laSegInfo[lnCnt,3]='F'
        lcCdTyp  =''
        lcFreeDc = laMajSeg[lnCnt,2]
    ENDCASE    
    laSegInfo[lnCnt,8] = laSegInfo[lnCnt,8]+lcCdTyp
  ENDIF
  laSegInfo[lnCnt,9] = IIF(laMajSeg[lnCnt,1]$'FOQT',"ciSegVal",;
                       IIF(laMajSeg[lnCnt,1]='S',"Scale","cCode_no"))
ENDFOR
SELECT(lnalias)
RETURN

*-- End Of lfSegInfo.
*:*************************************************************
*: Name      : lfvSize
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 01/05/2003
*: Purpose   : Valid function to validate style Size.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lf..()
*:*************************************************************
*:
FUNCTION lfvSize
PRIVATE lnAlias

lnAlias = SELECT(0)

*-- Check if the file create before that or it's first time.
IF USED(lcTempScal)
  SELECT (lcTempScal)
  DELE ALL
ELSE
  CREATE TABLE (gcWorkDir+lcTempScal);
   (Scale C(3)     , cscl_desc  C(10) , SZ C(5)  , nsize N(1),;
   llok_stat  L(1) , cadd_user  C(10) , cadd_time        C(11),;
   dadd_date  D(8) , clok_user  C(10) , dlok_date        D(08) ,;
   clok_time  C(8) , cowner     C(16) , cedit_user       C(10),;
   dedit_date D(8) , cedit_time C(11) )
   
  INDEX ON UPPER(SZ) TAG (lcTempScal)
ENDIF

SELECT SCALE
=SEEK('S'+lcScale)
FOR I = 1 TO CNT
  SCATTER MEMVAR MEMO
  lcSizes = STR(I,1)
  m.Sz    = M.Sz&lcSizes
  M.nsize = I
  INSERT INTO (lcTempScal) FROM MEMVAR
ENDFOR


llbrowse = llbrowse OR '?' $ lcSize

IF !llbrowse .AND. EMPTY(lcSize)
  RETURN
ENDIF


IF llBrowse .OR. !EMPTY(lcSize)
  IF llBrowse .OR. !SEEK(UPPER(lcSize),lcTempScal)
    lcPBrowTit = "Scale Sizes"         && Browse title.
    lcOldBrFld = lcBrFields
    DIMENSION latemp[2]                && array holr the return value from ariabrow
    laTemp = ''
    SELECT (lcTempScal)
    LOCATE
    lcBrFields = [Scale:H="Scale # ",cscl_desc:H="Scale Description ",SZ:H="Size"]
    llReturn = ARIABROW('',lcPBrowTitl,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','SZ,nsize','laTemp')
    lcBrFields = lcOldBrFld

    IF EMPTY(laTemp)
      lcSize  =''
      _CUROBJ = OBJNUM(lcSize)
      SELECT (lcTempScal)
      DELETE ALL
    ELSE
      lcSize = ALLTRIM(laTemp[1])
      lnsize = laTemp[2]
      lcCurSize = STR(lnsize,1)
    ENDIF
  ELSE
    lcCurSize  = STR(&lcTempScal..nSize,1)
  ENDIF
  lcSize = &lcTempScal..Sz
  
ENDIF

IF !EMPTY(lcSize)
  
  SHOW GET lcSize DISABLE
  SHOW GET ibCize DISABLE

  *-- go to view mode.
  laScrMode    = .F.
  laScrMode[2] = .T.
  SHOW GETS
  = lfCollect ()
ENDIF
llBrowse = .F.


SELECT(lnAlias)

*-- End OF lfvSize
*:*************************************************************
*: Name      : lpShow
*: Developer : Abdou Elgendy
*: Date      : 01/05/2003
*: Purpose   : Show function.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   :  =lpShow()
*:*************************************************************
*:
FUNCTION lpShow

SHOW GET pbPrvsScl ENABLE
SHOW GET pbNxtScl  ENABLE

*-- Control's
SHOW GET pbNotePad   DISABLE
SHOW GET pbAudTrail  DISABLE
SHOW GET pbDlt       DISABLE
SHOW GET pbBrws      DISABLE

IF lnWeeks_No = 1
  SHOW GET pbPrvsScl DISABLE
  SHOW GET pbNxtScl  ENABLE
ELSE
  SHOW GET pbPrvsScl ENABLE
  SHOW GET pbNxtScl  ENABLE
ENDIF


DO CASE
  ***--- S E L E C T   M O D E ---***
  CASE laScrMode[1]
    STORE '' TO lcMajor , lcNonMjr , lcStyDesc , lcSize,;
                lcVendor
    STORE 0 To laShowFig , lnAve_cost
    FOR I = 1 To 5
      STORE {} TO laShowFig [1,I]
    ENDFOR

    SHOW GET ibStyle   ENABLE
    SHOW GET ibColor   ENABLE
    SHOW GET pbPrvsScl DISABLE
    SHOW GET pbNxtScl  DISABLE

    SHOW GET pbPrvsScl DISABLE
    SHOW GET pbNxtScl  DISABLE
    
  ***--- V I E W   M O D E ---***
  CASE laScrMode[2]
    *-- recollect the data again.
    = lfCollect ()
  ***--- E D I T   M O D E ---***
  CASE laScrMode[3]
    SHOW GET ibStyle   DISABLE
    SHOW GET ibColor   DISABLE
    SHOW GET ibCize    DISABLE
    SHOW GET lcSize    DISABLE
    
    FOR I = 1 TO 5
     SHOW GET laShowFig[3,I] ENABLE
     
     *B607381,1 ABD - Enable Forcast Fields. [Begin]
     SHOW GET laShowFig[2,I] ENABLE
     *B607381,1 ABD - [End]
     
    ENDFOR
    

  ***--- A D D   M O D E ---***
  CASE laScrMode[4]

ENDCASE
=lfRefresh()



*-- En OF lpShow.
*:*************************************************************
*: Name      : lfCreatTmp
*: Developer : Abdou Elgendy
*: Date      : 01/05/2003
*: Purpose   : Function to Create Temp Files.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   :  =lfCreatTmp()
*:*************************************************************
*:
FUNCTION lfCreatTmp
PRIVATE lnAlias

lnAlias = SELECT(0)
SELECT Forcast

=AFIELDS(laFileStru)           
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru + 11, 4]
lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'nMyWeek'
laFileStru[lnFileStru ,2] = 'N'
laFileStru[lnFileStru ,3] = 2
laFileStru[lnFileStru ,4] = 0

lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'DMyDate'
laFileStru[lnFileStru ,2] = 'D'
laFileStru[lnFileStru ,3] = 8
laFileStru[lnFileStru ,4] = 0

lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'cweekNo'
laFileStru[lnFileStru ,2] = 'C'
laFileStru[lnFileStru ,3] = 2
laFileStru[lnFileStru ,4] = 0

lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'nTotWIP'
laFileStru[lnFileStru ,2] = 'N'
laFileStru[lnFileStru ,3] = 10
laFileStru[lnFileStru ,4] = 0


lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'nForPrd'
laFileStru[lnFileStru ,2] = 'N'
laFileStru[lnFileStru ,3] = 10
laFileStru[lnFileStru ,4] = 0

lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'nCurStk'
laFileStru[lnFileStru ,2] = 'N'
laFileStru[lnFileStru ,3] = 10
laFileStru[lnFileStru ,4] = 0

lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'nSalsOrder'
laFileStru[lnFileStru ,2] = 'N'
laFileStru[lnFileStru ,3] = 10
laFileStru[lnFileStru ,4] = 0

lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'nBalance'
laFileStru[lnFileStru ,2] = 'N'
laFileStru[lnFileStru ,3] = 10
laFileStru[lnFileStru ,4] = 0

lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'WeekStCovr'
laFileStru[lnFileStru ,2] = 'N'
laFileStru[lnFileStru ,3] = 10
laFileStru[lnFileStru ,4] = 0

lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'StkVal'
laFileStru[lnFileStru ,2] = 'N'
laFileStru[lnFileStru ,3] = 10
laFileStru[lnFileStru ,4] = 2

lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'Flage'
laFileStru[lnFileStru ,2] = 'C'
laFileStru[lnFileStru ,3] = 1
laFileStru[lnFileStru ,4] = 0

DECLARE laIndex[2,2]
laIndex[1,1] = [STR(nyear,4)+STR(nweek,2)+style]
laIndex[1,2] = lcTempLine
laIndex[2,1] = 'cweekNo'
laIndex[2,2] = 'lcTempLine'

=gfCrtTmp(lcTempLine,@laFileStru,@laIndex,lcTempLine)
SELECT(lnAlias)
*-- End OF lfCreatTmp
*:*************************************************************
*: Name      : lfCollect
*: Developer : Abdou Elgendy
*: Date      : 01/05/2003
*: Purpose   : Function to collect the data Temp Files.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   :  = lfCollect ()
*:*************************************************************
*:
FUNCTION lfCollect
PRIVATE lnAlias , ldsysdate , ln_weekNo,lnNextValue , lnCurStk
ln_weekNo = lnWeekNo
STORE gdsysdate TO ldsysdate , l2dsysdate
STORE 0 To lnRecNo , lnNextValue , lnCurStk
ln_weekNo = 1
lnAlias = SELECT(0)
SELECT (lcTempLine)
DELETE ALL


*-- collect data for 52 week.
M.style   = PADR(lcMajor,lnstylewid) +lcSepart+lcNonMjr
FOR I = 1 To 52
  APPEND BLANK

  *-- this variable for the currevnt screen.  
  ldsysdate = ldsysdate + IIF(I= 1,0,7)
  ldDate    = CTOD('01/01/'+STR(YEAR(ldsysdate),4))
  St_Day    = ldDate-DOW(ldDate)+1 
  lnWeek_No = (MAX(CEILING((ldsysdate-St_Day)/7),1))


  
  M.nMyWeek = I
  M.DMyDate = ldsysdate

  *-- this V\riables For the Forcast screen
  l2dsysdate = l2dsysdate + IIF(I= 1,0,7)
  ldDate2     = CTOD('01/01/'+STR(YEAR(l2dsysdate),4))
  St_Day2     = ldDate2-DOW(ldDate2)+1 
  lnWeek_No2  = (MAX(CEILING((l2dsysdate-St_Day2)/7),1))
  
  M.nweek   = lnWeek_No2
  M.nyear   = YEAR(l2dsysdate)
  
  *-- Number of week.
  lnRecNo  = lnRecNo + 1
  M.cweekNo = STR(ln_weekNo,2)
  IF lnRecNo = 5
    lnRecNo = 0
    ln_weekNo = ln_weekNo + 1
  ENDIF
  lnLastWeek = VAL(M.cweekNo)
  
  
  
  *-- get the Weekly sales Forecast.
  *-- style+STR(nyear,4)+STR(nweek,2)

  IF SEEK(M.Style+STR(M.nyear,4)+STR(M.nweek,2),'FORCAST')
    M.nforqty&lcCurSize = Forcast.nforqty&lcCurSize
    M.npqty&lcCurSize   = Forcast.npqty&lcCurSize
  ELSE
    STORE 0 To M.nforqty&lcCurSize , M.npqty&lcCurSize
  ENDIF
  
  GATHER MEMVAR MEMO
ENDFOR
SELECT (lcTempLine)
LOCATE

*- Fuction to get the WIP
= lfGetWIP()


*- Fuction to get the Outstanding sales order.
= lfGtSlsOrd()


*-- scan again to get needed data after we get the WIP.
SELECT (lcTempLine)
LOCATE
*-- This Function to Update the related filed for the production field.
= lfUpdProd ()

*-- show the first 5 record.
lnWeeks_No = 0
=lfvN5days()
SHOW GET pbPrvsScl DISABLE
SELECT(lnAlias)


*-- End OF lfCollect
*:*************************************************************
*: Name      : lfvN5days
*: Developer : Abdou Elgendy
*: Date      : 01/05/2003
*: Purpose   : Function to get the next five days for the screen.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   :  = lfvN5days ()
*:*************************************************************
*:
FUNCTION lfvN5days
PRIVATE lnAlias , lcOldOrdr , llSeek
llSeek = .T.

lnAlias = SELECT(0)
SELECT (lcTempLine)
lcOldOrdr = ORDER()
SET ORDER TO lcTempLine
=SEEK(STR(lnWeeks_No+1,2))

FOR I = 1 To 5
  laShowFig[1,I] = &lcTempLine..DMyDate
  laShowFig[2,I] = &lcTempLine..nForqty&lcCurSize
  laShowFig[3,I] = &lcTempLine..nPqty&lcCurSize
  laShowFig[4,I] = &lcTempLine..nTotWIP
  laShowFig[5,I] = &lcTempLine..nForPrd
  laShowFig[6,I] = &lcTempLine..nCurStk
  laShowFig[7,I] = &lcTempLine..nSalsOrder
  laShowFig[8,I] = &lcTempLine..nBalance
  laShowFig[9,I] = &lcTempLine..WeekStCovr
  laShowFig[10,I] = &lcTempLine..StkVal
  
  IF EOF()
    SHOW GET pbNxtScl DISABLE
  ELSE  
    SKIP
  ENDIF
  
ENDFOR

lnWeeks_No = lnWeeks_No + 1

IF lnWeeks_No = lnLastWeek
  SHOW GET pbPrvsScl ENABLE
  SHOW GET pbNxtScl  DISABLE
ELSE
  SHOW GET pbPrvsScl ENABLE
  SHOW GET pbNxtScl  ENABLE
ENDIF

SET ORDER TO &lcOldOrdr

FOR I = 1 TO 5
  SHOW GET laShowFig[3,I] 
  *B607381,1 ABD - Show get the Forcast Fields. [Begin]
  SHOW GET laShowFig[2,I]
  *B607381,1 ABD - [End]
  
ENDFOR


=lfRefresh()

SELECT (lnAlias)

*-- End OF lfvN5days.
*:*************************************************************
*: Name      : lfvB5days
*: Developer : Abdou Elgendy
*: Date      : 01/05/2003
*: Purpose   : Function to get the last five days for the screen.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   :  = lfvB5days ()
*:*************************************************************
*:
FUNCTION lfvB5days
PRIVATE lnAlias 

lnAlias = SELECT(0)


SELECT (lcTempLine)
SELECT (lcTempLine)
lcOldOrdr = ORDER()
SET ORDER TO lcTempLine

=SEEK(STR(lnWeeks_No-1,2))

FOR I = 1 To 5
  laShowFig[1,I] = &lcTempLine..DMyDate
  laShowFig[2,I] = &lcTempLine..nForqty&lcCurSize
  laShowFig[3,I] = &lcTempLine..nPqty&lcCurSize
  laShowFig[4,I] = &lcTempLine..nTotWIP
  laShowFig[5,I] = &lcTempLine..nForPrd
  laShowFig[6,I] = &lcTempLine..nCurStk
  laShowFig[7,I] = &lcTempLine..nSalsOrder
  laShowFig[8,I] = &lcTempLine..nBalance
  laShowFig[9,I] = &lcTempLine..WeekStCovr  
  laShowFig[10,I] = &lcTempLine..StkVal
  IF EOF()
    SHOW GET pbNxtScl DISABLE
  ELSE  
    SKIP
  ENDIF
  
ENDFOR

lnWeeks_No = lnWeeks_No - 1

IF lnWeeks_No = 1
  SHOW GET pbPrvsScl DISABLE
  SHOW GET pbNxtScl  ENABLE
ELSE
  SHOW GET pbPrvsScl ENABLE
  SHOW GET pbNxtScl  ENABLE
ENDIF


SET ORDER TO &lcOldOrdr

FOR I = 1 TO 5
  SHOW GET laShowFig[3,I] 
  
  *B607381,1 ABD - Show get the Forcast Fields. [Begin]
  SHOW GET laShowFig[2,I]
  *B607381,1 ABD - [End]

ENDFOR

=lfRefresh()
SELECT (lnAlias)

*-- End OF lfvB5days.
*:*************************************************************
*: Name      : lfGetWIP
*: Developer : Abdou Elgendy
*: Date      : 01/05/2003
*: Purpose   : Function to get the WIP from POFHDR.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   :  = lfGetWIP ()
*:*************************************************************
*:
FUNCTION lfGetWIP
PRIVATE lnAlias , lcStyle , ldsysdate , ldDate , St_Day , lnWeek_No , lnYear

lnAlias = SELECT(0)
lcStyle = &lcTempLine..Style

SELECT POSLN
SET ORDER TO Poslns
SET RELATION TO Posln.cstytype +  Posln.po INTO POSHDR

IF SEEK(lcStyle,'POSLN')
  SCAN REST WHILE style+cstytype+po+STR(lineno,6)+trancd = lcStyle;
    FOR POSHDR.STATUS $ 'OH' .AND. !(Posln.trancd = '3')
    
    *-- get the date and the wip will be depend on the first week.
    IF Poshdr.Complete >= gdSysDate
      ldsysdate = Poshdr.Complete
    ELSE
      ldsysdate = gdsysdate
    ENDIF

    ldDate    = CTOD('01/01/'+STR(YEAR(ldsysdate),4))
    St_Day    = ldDate-DOW(ldDate)+1 
    lnWeek_No = (MAX(CEILING((ldsysdate-St_Day)/7),1))
    lnYear    = YEAR(ldsysdate)
      
    *-- go to the selected week and update with the Qty.
    *-- Index On STR(nyear,4)+STR(nweek,2)+style
    IF SEEK(STR(lnYear,4)+STR(lnWeek_No,2)+lcStyle,lcTempLine)
      SELECT (lcTempLine)
      *-- Check on the type.
      IF Posln.Trancd = '1'
        REPLACE nTotWIP WITH nTotWIP + POSLN.qty&lcCurSize
      ELSE
        REPLACE nTotWIP WITH nTotWIP - POSLN.qty&lcCurSize
      ENDIF
      SELECT POSLN
    ENDIF
  ENDSCAN
ENDIF
SELECT POSLN
SET RELATION TO 

SELECT(lnAlias)
*-- End OF lfGetWIP.
*:*************************************************************
*: Name      : lfGtSlsOrd
*: Developer : Abdou Elgendy
*: Date      : 01/05/2003
*: Purpose   : Function to get the Outstanding sales order.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   :  = lfGtSlsOrd ()
*:*************************************************************
*:
FUNCTION lfGtSlsOrd
PRIVATE lnAlias , lcStyle , ldsysdate , ldDate , St_Day , lnWeek_No , lnYear

lnAlias = SELECT(0)
lcStyle = &lcTempLine..Style


SELECT ORDHDR
SET ORDER TO Ordhdr

SELECT ORDLINE
SET ORDER TO Ordlines
SET RELATION TO Ordline.cordtype +  Ordline.order INTO ORDHDR

IF SEEK(lcStyle,'Ordline')
  SCAN REST WHILE style+DTOS(complete)+cordtype+order+store+STR(lineno,6) = lcStyle;
    FOR ORDHDR.STATUS $ 'OH'
    
    *-- get the date and the wip will be depend on the first week.
    IF OrdHdr.Start >= gdSysDate
      ldsysdate = OrdHdr.Start
    ELSE
      ldsysdate = gdsysdate
    ENDIF

    ldDate    = CTOD('01/01/'+STR(YEAR(ldsysdate),4))
    St_Day    = ldDate-DOW(ldDate)+1 
    lnWeek_No = (MAX(CEILING((ldsysdate-St_Day)/7),1))
    lnYear    = YEAR(ldsysdate)
      
    *-- go to the selected week and update with the Qty.
    *-- Index On STR(nyear,4)+STR(nweek,2)+style
    IF SEEK(STR(lnYear,4)+STR(lnWeek_No,2)+lcStyle,lcTempLine)
      SELECT (lcTempLine)
      *-- Check on the type.
        REPLACE nSalsOrder WITH nSalsOrder + Ordline.qty&lcCurSize
      SELECT ORDLINE
    ENDIF
  ENDSCAN
ENDIF



SELECT ORDLINE
SET RELATION TO 

SELECT(lnAlias)

*-- End Of lfGtSlsOrd
*:*************************************************************
*: Name      : lfvPland
*: Developer : Abdou Elgendy
*: Date      : 01/05/2003
*: Purpose   : Function to Valid the Planned Production.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   :  = lfvPland ()
*:*************************************************************
*:
FUNCTION lfvPland
Parameters lnPlanQty
PRIVATE  ldDate , ldsysdate , St_Day , lnWeek_No , lnYear , lnAlias

lnAlias = SELECT(0)


*-- If statement to cheak if user inter Negative values.
IF laShowFig[3,lnPlanQty] < 0 
  *-- Message : 42000
  *-- Negative values are not allowed.
  *-- Button  : 40011
  *-- Ok
  = gfModalGen('TRM42000B40011','DIALOG')
  laShowFig[3,lnPlanQty] = lnOldValue
  _CUROBJ = _CUROBJ
  SELECT(lnAlias)
  RETURN
ENDIF


IF laShowFig[3,lnPlanQty] # lnOldValue
    
  ldsysdate = laShowFig[1,lnPlanQty]
  ldDate    = CTOD('01/01/'+STR(YEAR(ldsysdate),4))
  St_Day    = ldDate-DOW(ldDate)+1 
  lnWeek_No = (MAX(CEILING((ldsysdate-St_Day)/7),1))
  lnYear    = YEAR(ldsysdate)
  SELECT (lcTempLine)
  IF SEEK(STR(lnYear,4)+STR(lnWeek_No,2),lcTempLine)
  
    REPLACE npqty&lcCurSize WITH laShowFig[3,lnPlanQty],;
            Flage WITH 'N'

    *-- update the related field for Planned Production field.        
    SCATTER MEMVAR MEMO
    M.nForPrd = M.nForQty&lcCurSize - (M.npqty&lcCurSize + M.nTotWIP)
    M.nBalance = (M.npqty&lcCurSize + M.nTotWIP + M.nCurStk) - (M.nForQty&lcCurSize + M.nSalsOrder)
    GATHER MEMVAR MEMO
    lnRecNo = RECNO()
    
    
    *-- Call Function to Update the related filed for the production field.
    = lfUpdProd ()
    IF BETWEEN(lnRecNo,1,RECCOUNT())
      GOTO lnRecNo
    ENDIF

    *-- Update the field on the screen.
    lcOldOrdr = ORDER()
    SET ORDER TO lcTempLine
    =SEEK(STR(lnWeeks_No,2))
    FOR I = 1 TO 5
      laShowFig[5,I] = &lcTempLine..nForPrd
      laShowFig[6,I] = &lcTempLine..nCurStk
      laShowFig[8,I] = &lcTempLine..nBalance
      laShowFig[9,I] = &lcTempLine..WeekStCovr
      laShowFig[10,I] = &lcTempLine..StkVal
      
      IF !EOF()
        SKIP
      ENDIF
    ENDFOR
    SET ORDER TO &lcOldOrdr
  ENDIF
  
ENDIF

=lfRefresh()

SELECT(lnAlias)

*-- End of lfvPland.
*:*************************************************************
*: Name      : lpSavScr
*: Developer : Abdou Elgendy
*: Date      : 01/05/2003
*: Purpose   : Save/Update.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : Do ..
*:*************************************************************
*:
PROCEDURE lpSavScr
PRIVATE lnAlias

lnAlias = SELECT(0)

SELECT (lcTempLine)
SCAN FOR Flage = 'N'
  SCATTER MEMVAR MEMO
  *-- style+STR(nyear,4)+STR(nweek,2)
  SELECT FORCAST
  IF SEEK(M.Style+STR(M.nyear,4)+STR(M.nweek,2))
    REPLACE npqty&lcCurSize WITH M.npqty&lcCurSize,;
            nptotqty        WITH npqty1 + npqty2 + npqty3 + ;
               npqty4 + npqty5 + npqty6 + npqty7 + npqty8

   *B607381,1 ABD - Save the Forcast Fields. [Begin]
    REPLACE nForqty&lcCurSize WITH M.nForqty&lcCurSize,;
            nptotqty        WITH nForqty1 + nForqty2 + nForqty3 + ;
            nForqty4 + nForqty5 + nForqty6 + nForqty7 + nForqty8
   *B607381,1 ABD - [End]
  ELSE
    APPEN BLANK
    GATHER MEMVAR MEMO
  ENDIF
  SELECT (lcTempLine)
ENDSCAN

SELECT(lnAlias)

*-- End Of lpSavScr.
*:*************************************************************
*: Name      : lfUpdProd
*: Developer : Abdou Elgendy
*: Date      : 01/05/2003
*: Purpose   : Function to Update the related filed for the production field.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   :  = lfUpdProd ()
*:*************************************************************
*:
FUNCTION lfUpdProd
PRIVATE lnAlias , lnCurStk
lnCurStk = 0

lnAlias = SELECT (0)

*-- get the current stock.
SELECT STYLE
lcOldOrder = ORDER() 
SET ORDER TO STYLE
lnCurStk = IIF(SEEK(Style,'STYLE'),Style.Stk&lcCurSize,0)
SET ORDER TO &lcOldOrder


SELECT (lcTempLine)
SCAN
  SCATTER MEMVAR MEMO
  STORE 0 TO  M.nForPrd , M.nCurStk , M.nBalance , M.WeekStCovr , M.StkVal
  M.nForPrd = M.nForQty&lcCurSize - (M.npqty&lcCurSize + M.nTotWIP)
  IF M.nMyWeek = 1
    M.nCurStk   = lnCurStk
    lnNextValue = lnCurStk
  ELSE
    M.nCurStk   = lnNextValue + M.nForPrd 
    lnNextValue = M.nCurStk
  ENDIF
  
  M.nBalance = (M.npqty&lcCurSize + M.nTotWIP + M.nCurStk) - (M.nForQty&lcCurSize + M.nSalsOrder)
  
  IF M.nBalance # 0  .AND. M.nForQty&lcCurSize # 0
    M.WeekStCovr  = ROUND(M.nBalance / M.nForQty&lcCurSize,0)
  ENDIF
  
  m.StkVal = lnAve_cost *  M.nCurStk
  
  GATHER MEMVAR MEMO
ENDSCAN

SELECT (lnAlias)

*-- End OF lfUpdProd
*:*************************************************************
*: Name      : lpTopScr
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 01/05/2003
*: Purpose   : Top record in Navigation pb.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : Do ..
*:*************************************************************
*:
PROCEDURE lpTopScr
PRIVATE lnAlias

lcBaseFile = lcTempScal
SELECT (lcBaseFile)

SELECT (lcTempScal)
lcCurSize  = STR(&lcTempScal..nSize,1)
lcSize     = &lcTempScal..Sz


*-- End OF lpTopScr.
*:*************************************************************
*: Name      : lpBtmScr
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 01/05/2003
*: Purpose   : Bottom record in Navigation pb.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : Do ..
*:*************************************************************
*:
PROCEDURE lpBtmScr

lcBaseFile = lcTempScal
SELECT (lcBaseFile)

SELECT (lcTempScal)
lcCurSize  = STR(&lcTempScal..nSize,1)
lcSize     = &lcTempScal..Sz


*-- End OF lpBtmScr.
*:*************************************************************
*: Name      : lpNxtScr
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 01/05/2003
*: Purpose   : Next record in Navigation pb.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : Do ..
*:*************************************************************
*:
PROCEDURE lpNxtScr

lcBaseFile = lcTempScal
SELECT (lcBaseFile)

SELECT (lcTempScal)
lcCurSize  = STR(&lcTempScal..nSize,1)
lcSize     = &lcTempScal..Sz

*-- End OF lpNxtScr
*:*************************************************************
*: Name      : lpPrvScr
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 01/05/2003
*: Purpose   : Previus record in Navigation pb.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : Do ..
*:*************************************************************
*:
PROCEDURE lpPrvScr


lcBaseFile = lcTempScal
SELECT (lcBaseFile)

SELECT (lcTempScal)
lcCurSize  = STR(&lcTempScal..nSize,1)
lcSize     = &lcTempScal..Sz

*-- End OF lpPrvScr.
*B607381,1 ABD - [Begin]
*:*************************************************************
*: Name      : lfvForCast
*: Developer : Abdou Elgendy
*: Date      : 01/05/2003
*: Purpose   : Function to Valid the Planned Production.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   :  = lfvPland ()
*:*************************************************************
*:B607381,1 ABD - [Begin]
FUNCTION lfvForCast
Parameters lnForstQty
PRIVATE  ldDate , ldsysdate , St_Day , lnWeek_No , lnYear , lnAlias

lnAlias = SELECT(0)


*-- If statement to cheak if user inter Negative values.
IF laShowFig[2,lnForstQty] < 0 
  *-- Message : 42000
  *-- Negative values are not allowed.
  *-- Button  : 40011
  *-- Ok
  = gfModalGen('TRM42000B40011','DIALOG')
  laShowFig[2,lnForstQty] = lnOldValue
  _CUROBJ = _CUROBJ
  SELECT(lnAlias)
  RETURN
ENDIF


IF laShowFig[2,lnForstQty] # lnOldValue
    
  ldsysdate = laShowFig[1,lnForstQty]
  ldDate    = CTOD('01/01/'+STR(YEAR(ldsysdate),4))
  St_Day    = ldDate-DOW(ldDate)+1 
  lnWeek_No = (MAX(CEILING((ldsysdate-St_Day)/7),1))
  lnYear    = YEAR(ldsysdate)
  SELECT (lcTempLine)
  IF SEEK(STR(lnYear,4)+STR(lnWeek_No,2),lcTempLine)
  
    REPLACE nForqty&lcCurSize WITH laShowFig[2,lnForstQty],;
            Flage WITH 'N'

    *-- update the related field for Planned Production field.        
    SCATTER MEMVAR MEMO
    M.nForPrd = M.nForQty&lcCurSize - (M.npqty&lcCurSize + M.nTotWIP)
    M.nBalance = (M.npqty&lcCurSize + M.nTotWIP + M.nCurStk) - (M.nForQty&lcCurSize + M.nSalsOrder)
    GATHER MEMVAR MEMO
    lnRecNo = RECNO()

    *-- Call Function to Update the related filed for the production field.
    = lfUpdProd ()
    IF BETWEEN(lnRecNo,1,RECCOUNT())
      GOTO lnRecNo
    ENDIF

    *-- Update the field on the screen.
    lcOldOrdr = ORDER()
    SET ORDER TO lcTempLine
    =SEEK(STR(lnWeeks_No,2))
    FOR I = 1 TO 5
      laShowFig[5,I] = &lcTempLine..nForPrd
      laShowFig[6,I] = &lcTempLine..nCurStk
      laShowFig[8,I] = &lcTempLine..nBalance
      laShowFig[9,I] = &lcTempLine..WeekStCovr
      laShowFig[10,I] = &lcTempLine..StkVal
      
      IF !EOF()
        SKIP
      ENDIF
    ENDFOR
    SET ORDER TO &lcOldOrdr
  ENDIF
  
ENDIF

=lfRefresh()

SELECT(lnAlias)

*-- End of lfvForCast.
*B607381,1 ABD - [End]
*:*************************************************************