*:***************************************************************************
*: Program file  : POYEAR
*: Program desc. : Calculate yearly invoiced and purchased figures.
*: For screen    : POYEAR.SCX
*:        System : Aria Advantage Series.
*:        Module : Style Purchased Order (PO)
*:        Date   : 07/09/2003
*:     Developer : AHMED MAHER (AMH)
*:***************************************************************************
*: Calls : 
*:     Procedures : 
*:     Functions  : 
*:***************************************************************************
*: Example : DO POYEAR
*:***************************************************************************
*: Due to C200569
*:***************************************************************************
*:Modifications  :
*:C122961,1 NNA 06/20/2004 Make Some Changes as Follow :
*:C122961,1 NNA            1 - Add an Option headed "Calculate Closing Stock for Year End" to 
*:C122961,1 NNA                Calculate the Closing inventory For the Previous Year to use it as
*:C122961,1 NNA                Open Stock for the Current year
*:C122961,1 NNA            2 - Add an option headed "Update Forecast" to Create a Month 13 for the
*:C122961,1 NNA                Year Processed to get the Total Values for Both Sale and Purchase Forecast
*:C122961,1 NNA                And Save it in the new Forecast File
*:C122961,1 NNA            3 - Add New Fields to Yearinpo file (Nbkamt01-12) to have the amount of 
*:C122961,1 NNA                sales orders Booked Less Any Cancelled for that Size
*:C126297,1 NNA 05/12/2005 Make a change by adding a new option Called (Update this years forecast) to take the current
*:C126297,1 NNA            year(i.e 2005) invoice figures from the Yearinpo File to load them into the next year(i.e 2006)
*:C126297,1 NNA            in the forecast file then add a total record (month13) for style/color/size/month
*:****************************************************************

lcOkBmp  = gcBmpHome + "OK.BMP"
lcCanBmp = gcBmpHome + "CAN.BMP"
lnYear = YEAR(gdSysDate)

*C122961,1 NNA 06/20/2004 (Begin) Define new Variables
*STORE '0%' TO lcD1ThPrc,lcD2ThPrc,lcD3ThPrc
*-- lcD4ThPrc  -> For Sales order's Progress Bar Thermo.
*-- lcD5ThPrc  -> For Calculating Closing Stock's Progress Bar Thermo.
*-- lcD6ThPrc  -> For Update ForeCast's Progress Bar Thermo.
*-- llCalCstk  -> Variable to hold the Check Box value of Calculate Closing Stock
*-- llUpdFrcst -> Variable to hold the Check Box value of Update ForeCasts
*-- llMScale   -> Variable to hold if the customer use Extended size Scale or not
*-- lcTmpFrCst -> Variable to hold the Name of a new temp file that have the Forecast file's Data

STORE '0%' TO lcD1ThPrc , lcD2ThPrc , lcD3ThPrc , lcD4ThPrc , lcD5ThPrc , lcD6ThPrc
STORE .F. TO llCalCstk , llUpdFrcst , llMScale
STORE '' TO lcTmpFrCst
llMScale   = gfGetMemVar('M_USEEXSSC')
lcTmpFrCst = gfTempName()
*C122961,1 NNA (End)

*C126297,1 NNA 05/12/2005 (Begin) new variable to hold if user check to update this years forecast option
STORE .F. TO llUpFrcMth
*C126297,1 NNA (End)

*-- To get the Color position and length.
STORE 0 TO lnClrPos,lnClrLen
lnMjrWid = LEN(gfItemMask('PM'))
=lfGetColor()

*-- To get the include pattern No setup.
llUpdPatrn = gfGetMemVar('M_ADDPATNO')

IF !gfSetup()
  RETURN
ENDIF  

lcEscKey = ON('KEY','ESC')
ON KEY LABEL ESC CLEAR READ
DO (gcScrDir+gcWinAppl+'\POYEAR.SPX')
ON KEY LABEL ESC &lcEscKey.

glQuitting = .T.  && Rise quit flag because it's modal screen.
*-- end of program code.

*:*************************************************************
*: Name      : lfThrmo
*: Developer : AHMED MAHER (AMH)
*: Date      : 07/10/2003
*: Purpose   : Thermometers.
*:*************************************************************
FUNCTION lfThrmo
PARAMETERS lnCurrRc,lnMaxum,lcTrmNo

PRIVATE lcThrm,lcPrc
lcThrm = 'lcD' + lcTrmNo + 'Thrm'
lcPrc  = 'lcD' + lcTrmNo + 'ThPrc'

&lcThrm. = REPLICATE(CHR(219),CEILING((lnCurrRc*34)/lnMaxum))
&lcPrc.  = ALLTRIM(STR((lnCurrRc/lnMaxum)*100,3))+'%'
SHOW GET &lcThrm.
SHOW GET &lcPrc.

*-- End of lfThrmo.
*:*************************************************************
*: Name      : lfvOk
*: Developer : AHMED MAHER (AMH)
*: Date      : 07/10/2003
*: Purpose   : Valid function of the OK button
*:*************************************************************
FUNCTION lfvOk

IF lnYear < 1900
  =gfModalGen("TRM000000B00000","DIALOG",'','','This is not a valid year.')
  _CUROBJ = OBJNUM(lnYear)
  RETURN
ENDIF

IF SEEK(STR(lnYear,4,0),'YEARINPO')
  IF gfModalGen("QRM00000B34001","DIALOG",'','','This year already exist. '+;
                'Do you want to continue with overwrite the existed one?') = 2
    _CUROBJ = OBJNUM(lnYear)
    RETURN
  ELSE
    
    *C122961,1 NNA 06/20/2004 (Begin) IF user didn't Check 'Calculate Closing Stock for Year End'
    *C122961,1 NNA             I'll Warn him that the close Stock Will Be Zero 
    IF !llCalCstk .AND. gfModalGen('QRM32010B32001',.F.,.F.,.F.,'You did not check the' + ;
                        '[Calculate Closing Stock] Box, so Closing Stock will be Zero ' + ;
                        'Do you want to Proceed ?') = 2
      _CUROBJ = OBJNUM(lnYear)
      RETURN
    ELSE
    *C122961,1 NNA (End)

      SELECT YEARINPO
      DELETE REST WHILE STR(NYEAR,4,0) = STR(lnYear,4,0)

    *C122961,1 NNA (Begin)
    ENDIF
    *C122961,1 (End)
  ENDIF
ENDIF

PRIVATE lcDateSet,ldStart,ldEnd

lcDateSet = SET('DATE')
SET DATE TO MDY
ldStart = CTOD('01/01/'+STR(lnYear,4,0))
ldEnd   = CTOD('12/31/'+STR(lnYear,4,0))
SET DATE TO &lcDateSet.
lnInvMax = lfAddData("INV","INVDATE","1","~.INVOICE",      ".T.",          ".T.")
lnRetMax = lfAddData("RET","CRDATE" ,"2","~.CRMEMO" ,      ".T.",          ".F.")
lnPosMax = lfAddData("POS","ENTERED","3","~.CSTYTYPE+~.PO","TRANCD $ '15'","TRANCD='1'")
*C122961,1 NNA 06/20/2004 (Begin) Add the Sales order Booked data also
lnOrdMax = lfAddData("ORD","START"  ,"4","~.CORDTYPE+~.ORDER", ".T.",       ".T.")

IF llCalCstk		&& if the user Checked to [Calculate Closing Stock for Year End]
  IF !USED('STYINVJL')
    =gfOpenFile(gcDataDir+'STYINVJL',gcDataDir+'STYINVJL','SH')
  ENDIF
  =lfCalCstk()
ENDIF
IF llUpdFrcst		&& if the user Checked to [Update ForeCasts]
  =lfUpdFrcst()
ENDIF

*C126297,1 NNA 05/12/2005 (Begin) if the user Checked to [Update this years Forecast]
IF llUpFrcMth	
  =lfUpFrcMth()
ENDIF
*C126297,1 NNA (End)

*IF lnInvMax + lnRetMax + lnPosMax > 0
IF lnInvMax + lnRetMax + lnPosMax + lnOrdMax > 0
*C122961,1 NNA (End)

  =gfModalGen("INM000000B00000","DIALOG",'','','Calculate invoiced , purchased and Booked '+;
              'figures for year '+STR(lnYear,4,0)+' completed successfully.')
ELSE
  =gfModalGen("INM000000B00000","DIALOG",'','','There are no invoiced and purchased '+;
              'figures for year '+STR(lnYear,4,0)+'.')
ENDIF
=lfThrmo(0,1,'1')
=lfThrmo(0,1,'2')
=lfThrmo(0,1,'3')

*C122961,1 NNA 06/20/2004 (Begin) show The Thermo. For the Three new Progress Bar
=lfThrmo(0,1,'4')
=lfThrmo(0,1,'5')
=lfThrmo(0,1,'6')
*C122961,1 NNA (End)

*C126297,1 NNA 05/12/2005 (Begin) show The Thermo. For the new Progress Bar
=lfThrmo(0,1,'7')
*C126297,1 NNA (End)

_CUROBJ = OBJNUM(lnYear)
*-- End of lfvOk.

*!*************************************************************
*! Name      : lfGetColor
*! Developer : Ahmed Maher
*! Date      : 07/10/2003
*! Purpose   : Get the color information.
*!*************************************************************
*! Calls     : gfItemMask
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : lfGetColor()
*!*************************************************************
*
FUNCTION lfGetColor

*-- Array hold the segmants specifications of the style code structure.
DECLARE laStySeg[1,1]

*-- Count of the major part.
lnMjorCnt  = gfItemMask("SM")
  
*-- Fill an array with the segments strucure, & loop in it to 
*-- know if there a color segment in the style code strucure.
=gfItemMask(@laStySeg)
FOR lnCnt = lnMjorCnt + 1 TO ALEN(laStySeg,1)
  IF laStySeg[lnCnt , 1] = "C"
    *-- Flag to know if there is color in the style code strucure.
    llColorExt = .T.
    *-- Var. hold the start position of the color segment in the style code strucure.
    lnClrPos = laStySeg[lnCnt , 4]
    *-- Var. hold the color segment lenght in the style code strucure.
    lnClrLen = LEN(laStySeg[lnCnt , 3])
  ENDIF
ENDFOR
*-- end of lfGetColor.

*!*************************************************************
*! Name      : lfAddData
*! Developer : Ahmed Maher
*! Date      : 07/13/2003
*! Purpose   : Add the invoice , credit memo and PO data.
*!*************************************************************
*! Parameters: lcDataType "INV" = Invoice
*!                        "RET" = Credit memo
*!                        "POS" = PO
*!                        "SOS" = SO
*!             lcDateFld  the field used to check in date
*!             lcThrmo    "1" = Invoice
*!                        "2" = Credit memo
*!                        "3" = PO
*!                        "4" = SO
*!             lcWhileExp Expration to seek in the line file
*!             lcForExp   Expration to the for part of scan the line file
*!             lcSignCond Condation to use the positive sign.
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : lfAddData("INV","INVDATE","1","~.INVOICE",".T.",".T.")
*!*************************************************************
*
FUNCTION lfAddData
PARAMETERS lcDataType,lcDateFld,lcThrmo,lcWhileExp,lcForExp,lcSignCond

PRIVATE lnMax,lnCnt,lcColor,lnI,lcI,lcMonth,lnQty,lnJ,lcJ,lnSign,lcStyle,lcScale
PRIVATE lcHdrFile,lcLineFile,lcStatus,lcCond,lcWhileHdr,lcWhileLn,lcField,lcTotField

STORE 0 TO lnCnt
lcHdrFile  = lcDataType + 'HDR'
lcLineFile = lcDataType + IIF(lcDataType='POS','LN','LINE')

*C122961,1 NNA 06/20/2004 (Begin) if We Dealing with Style PO or Sales Order We Let the Cancelled 
*C122961,1 NNA             Orders,otherwise if this is invoice we let the voided invoices
*lcStatus   = IIF(lcDataType='POS','X','V')
lcStatus   = IIF(lcDataType='POS' .OR. lcDataType='ORD','X','V')
*C122961,1 NNA (End)

lcCond     = 'ALL FOR BETWEEN(' + lcDateFld + ',ldStart,ldEnd) .AND. STATUS # lcStatus' +;
             IIF(lcDataType='POS',' .AND. CSTYTYPE = "P"','')
lcWhileHdr = STRTRAN(lcWhileExp,'~',lcHdrFile)
lcWhileLn  = STRTRAN(lcWhileExp,'~',lcLineFile)

*C122961,1 NNA 06/20/2004 (Begin) Add the Book Fields of the sales order to the Criteria
*lcField    = 'N' + IIF(lcDataType='POS','POS','SHP') + 'MNTH'
*lcTotField = 'NTOT' + IIF(lcDataType='POS','POS','SHIP')
lcField     = 'N' + IIF(lcDataType='POS','POS',IIF(lcDataType='ORD','BKAMT','SHP')) +;
                     IIF(lcDataType<>'ORD' , 'MNTH' , '')
lcTotField  = 'NTOT' + IIF(lcDataType='POS','POS',IIF(lcDataType='ORD' , 'BOOK' , 'SHIP'))

*C122961,1 NNA (End)

SELECT (lcHdrFile)
COUNT &lcCond. TO lnMax

SCAN &lcCond.
  lnCnt = lnCnt + 1
  =lfThrmo(lnCnt,lnMax,lcThrmo)
  IF SEEK(EVALUATE(lcWhileHdr),lcLineFile)
    SELECT (lcLineFile)
    SCAN REST WHILE &lcWhileLn. = EVALUATE(lcWhileHdr) FOR &lcForExp.
      IF SEEK(STYLE,'STYLE') .AND. SEEK('S'+STYLE.SCALE,'SCALE')
        lcColor = SUBSTR(STYLE,lnClrPos,lnClrLen)
        lnSign  = IIF(EVALUATE(lcSignCond),1,-1)
        lcStyle = STYLE
        lcScale = STYLE.SCALE
        SELECT YEARINPO
        FOR lnI = 1 TO SCALE.CNT
          lcI = STR(lnI,1)

          *C122961,1 NNA 06/20/2004 (Begin) Add the Booked Fields to get it's Qty.
          *lnQty = EVALUATE(lcLineFile+'.QTY'+lcI)
          lnQty = EVALUATE(lcLineFile+IIF(lcDataType<>'ORD','.QTY','.BOOK')+lcI)          
          *C122961,1 NNA (End)
          
          IF lnQty > 0
            =lfAddLine()
            
            *C122961,1 NNA 06/20/2004 (Begin) Get the Cancelled Qty. to Decrease it from the booked Qty.
            IF lcDataType = 'ORD'
              =lfGetCancl(lcStyle ,&lcHdrFile..Order ,lcI)
            ENDIF
            *C122961,1 NNA (End)
            
            IF llUpdPatrn
              lcPattern  = PADR(STYLE.PATTERN,lnMjrWid)
              lcNewStyle = lcPattern + SUBSTR(lcStyle,lnMjrWid+1)
              IF lcDataType # 'POS' .AND. lcNewStyle # lcStyle .AND.;
                 SEEK(lcNewStyle,'STYLE') .AND. STYLE.SCALE = lcScale
                =lfAddLine()
              ENDIF
              =SEEK(lcStyle,'STYLE')
            ENDIF
          ENDIF
        ENDFOR
      ENDIF
    ENDSCAN
  ENDIF
ENDSCAN
RETURN lnMax
*-- end if lfAddData.

*!*************************************************************
*! Name      : lfAddLine
*! Developer : Ahmed Maher
*! Date      : 07/13/2003
*! Purpose   : Add line to YEARINPO file
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : lfAddLine()
*!*************************************************************
*
FUNCTION lfAddLine

IF !SEEK(STR(lnYear,4,0)+STYLE.CSTYMAJOR+lcColor+STYLE.SCALE+lcI)
  APPEND BLANK
  REPLACE NYEAR     WITH lnYear,;
          STYLE     WITH STYLE.CSTYMAJOR,;
          COLOR     WITH lcColor,;
          CSIZECODE WITH STYLE.SCALE+lcI
ENDIF

lcMonth = STRTRAN(STR(MONTH(EVALUATE(lcHdrFile+'.'+lcDateFld)),2,0),' ','0')
REPLACE (lcField+lcMonth) WITH EVALUATE(lcField+lcMonth) + (lnQty*lnSign)

*C122961,1 NNA 06/20/2004 (Begin) Add New Lines if We Get data for The Po Style Or Sales Order
*IF lcDataType # 'POS'
IF lcDataType # 'POS' .AND. lcDataType # 'ORD'
*C122961,1 NNA (End)

  FOR lnJ = VAL(lcMonth) TO 12
    lcJ = STRTRAN(STR(lnJ,2,0),' ','0')
    REPLACE ('NCUMSHIP'+lcJ) WITH EVALUATE('NCUMSHIP'+lcJ) + (lnQty * lnSign)
  ENDFOR
ENDIF

REPLACE (lcTotField) WITH EVALUATE(lcTotField) + (lnQty * lnSign)
*--end of lfAddLine.

*!*************************************************************
*! Name      : lfvCalCstk
*! Developer : NADER NABIL
*! Date      : 06/20/2004
*! Purpose   : Function to Unable or Disable the Calculate Stock Thermo.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : lfvCalCstk()
*!*************************************************************
*
FUNCTION lfvCalCstk
IF llCalCstk 
  SHOW GET lcD5Thrm  ENABLE
  SHOW GET lcD5ThPrc ENABLE
ELSE
  SHOW GET lcD5Thrm  DISABLE
  SHOW GET lcD5ThPrc DISABLE
ENDIF

*--End of lfvCalCstk.

*!*************************************************************
*! Name      : lfvUpdFrCst
*! Developer : NADER NABIL
*! Date      : 06/20/2004
*! Purpose   : Function to Unable or Disable the Forecast Thermo.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : lfvUpdFrCst()
*!*************************************************************
*
FUNCTION lfvUpdFrCst
IF llUpdFrcst
  SHOW GET lcD6Thrm  ENABLE
  SHOW GET lcD6ThPrc ENABLE
ELSE
  SHOW GET lcD6Thrm  DISABLE
  SHOW GET lcD6ThPrc DISABLE
ENDIF

*--End of lfvUpdFrCst.

*!*************************************************************
*! Name      : lfGetCancl
*! Developer : NADER NABIL (NNA)
*! Date      : 06/21/2004
*! Purpose   : Get the cancelled qty. from ordcanln.dbf
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : lfGetCancl()
*!*************************************************************
*
FUNCTION lfGetCancl
PARAMETERS lcSty,lnOrder , lnQtyNo

PRIVATE lcOldAlias , lnTotBook
STORE 0 TO lnTotBook
lcOldAlias = ALIAS()
SELECT ORDCANLN
IF SEEK('O' + lnOrder)
  SCAN REST WHILE cOrdType + Order = 'O' + lnOrder
    IF STYLE = lcSty
      SELECT YEARINPO

      IF SEEK(STR(lnYear,4,0)+STYLE.CSTYMAJOR+lcColor+STYLE.SCALE+lnQtyNo)      
        lcMonth = STRTRAN(STR(MONTH(EVALUATE(lcHdrFile+'.'+lcDateFld)),2,0),' ','0')
        REPLACE (lcField+lcMonth) WITH EVALUATE(lcField+lcMonth) - ORDCANLN.QTY&lnQtyNo
        FOR I = 1 TO 12
          N = STRTRAN(STR(I,2,0),' ','0')        
          lnTotBook = lnTotBook + NBKAMT&N
        ENDFOR
        REPLACE (lcTotField) WITH lnTotBook
      ENDIF
    ENDIF
  ENDSCAN
ELSE
  SELECT &lcOldAlias
  RETURN
ENDIF
SELECT &lcOldAlias


*--End of lfGetCancl.

*!*************************************************************
*! Name      : lfCalCstk
*! Developer : NADER NABIL (NNA)
*! Date      : 06/21/2004
*! Purpose   : Calculate the closing stock for year end
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : lfCalCstk()
*!*************************************************************
*
FUNCTION lfCalCstk
PRIVATE lnMax , lnCnt , lcSiz , lcStkNo , lnOldAls , lcColor
DECLARE laCloseStk[8]
STORE 0 TO lnCnt , lnMax , lnOldAls , laCloseStk
STORE '' TO lcSiz , lcStkNo , lcColor

lnOldAls = SELECT(0)
*lnSepMins = lfvGtSclSp()
SELECT STYLE
lnMax =RECCOUNT()
SCAN
  lnCnt = lnCnt + 1
  =lfThrmo(lnCnt,lnMax,'5')
  SELECT STYINVJL
    IF SEEK(Style.Style)
    SUM NSTK1,NSTK2,NSTK3,NSTK4,NSTK5,NSTK6,NSTK7,NSTK8 REST WHILE     ;
        style+cwarecode+csession+DTOS(dtrdate)+ctrcode+STR(lineno,6) = ;
        Style.Style FOR YEAR(DtrDate) < LNYEAR TO ARRAY laCloseStk
    ENDIF
  SELECT YEARINPO
    lcColor = SUBSTR(STYLE.Style,lnClrPos,lnClrLen)
    FOR X = 1 TO 8
      lci = STR(X,1)
      IF SEEK(STR(lnYear,4,0)+STYLE.CSTYMAJOR+lcColor+STYLE.SCALE+lcI)      
        REPLACE nCloseStk WITH laCloseStk[X]
      ELSE
        IF laCloseStk[X] > 0
          APPEND BLANK
          REPLACE NYEAR     WITH LNYEAR  , STYLE      WITH STYLE.CSTYMAJOR ,;
                  COLOR     WITH lcColor , cSizeCode  WITH STYLE.SCALE+lcI ,;
                  nCloseStk WITH laCloseStk[X]
        ENDIF
      ENDIF
    ENDFOR
  STORE 0 TO laCloseStk  
ENDSCAN
SELECT(lnOldAls)
RETURN

*--End of lfCalCstk.
*!*************************************************************
*! Name      : lfUpdFrcst
*! Developer : NADER NABIL (NNA)
*! Date      : 06/21/2004
*! Purpose   : update forecast File with Month 13 (total of the Processed Year)
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : lfUpdFrcst()
*!*************************************************************
*
FUNCTION lfUpdFrcst
PRIVATE lnMax , lnCnt , lcSiz
STORE 0 TO lnCnt , lnMax
IF SEEK(STR(lnYear,4),'FORECAST')
  SELECT FORECAST
  =AFIELDS(laFileStru)
  CREATE CURSOR (lcTmpFrCst) FROM ARRAY laFileStru
  INDEX ON style+STR(nyear,4) TAG (lcTmpFrCst) OF (lcTmpFrCst)
  SELECT FORECAST
  COUNT FOR Nmonth <> 13 TO lnMax

  = SEEK(STR(lnYear,4))
  SCAN REST WHILE STR(Nyear,4)+STR(Nmonth,2)+style = STR(lnYear,4) FOR Nmonth <>13
    lnCnt = lnCnt + 1
    =lfThrmo(lnCnt , lnMax ,'6')
    SCATTER MEMVAR
    m.Nmonth = 13
    SELECT (lcTmpFrCst)
    IF SEEK(m.Style + STR(m.Nyear,4))
      FOR I = 1 TO 8
        lcSiz = STR(I,1)      
        REPL NSalFrcst&lcSiz WITH NSalFrcst&lcSiz + m.NSalFrcst&lcSiz
        REPL NPurFrcst&lcSiz WITH NPurFrcst&lcSiz + m.NPurFrcst&lcSiz        
      ENDFOR
    ELSE
      INSERT INTO (lcTmpFrCst) FROM MEMVAR
    ENDIF
  ENDSCAN
  *-- append the Total Records from the Temp File to the forecast file
  SELECT (lcTmpFrCst)
  SCAN
    SCATTER MEMVAR
    IF SEEK(STR(nyear,4)+STR(nmonth,2)+style,'FORECAST')
      SELECT FORECAST
      GATHER MEMVAR
    ELSE
      INSERT INTO FORECAST FROM MEMVAR
    ENDIF
  ENDSCAN
  USE IN (lcTmpFrCst)
ELSE
  RETURN
ENDIF
*--End of lfUpdFrcst.
*!*************************************************************
*! Name      : lfvGtSclSp
*! Developer : NADER NABIL (NNA)
*! Date      : 06/21/2004
*! Purpose   : Function returns the length of Scale with Separator.
*!*************************************************************
*! Returns   : len of Scale 3 or 4 if separator exist.
*!*************************************************************
*! Example   : = lfvGtSclSp()
*!*************************************************************
FUNCTION lfvGtSclSp

IF llMScale
  llStruOp=gfOpenFile(gcDataDir+'ICISTRU','Segno','SH')
  SELECT ICISTRU
  LOCATE FOR cItemRecty='U' AND cISegType='S'
  IF FOUND()
    SKIP -1
    lnSepMins = IIF(!lSegEndMaj AND !EMPTY(cISegSepr),4,3)
  ELSE
    lnSepMins = 3
  ENDIF
  IF USED('ICISTRU') AND llStruOp
    USE IN ICISTRU
  ENDIF
ELSE
  lnSepMins = 0
ENDIF
RETURN (lnSepMins)

*--End of lfvGtSclSp.
*!*************************************************************
*! Name      : lfvUpFrcMth
*! Developer : NADER NABIL
*! Date      : 06/20/2004
*! Purpose   : Function to Unable or Disable the (Forecast this yoear) Thermo.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : lfvUpdFrCst()
*!*************************************************************
*
FUNCTION lfvUpFrcMth
IF llUpFrcMth
  SHOW GET lcD7Thrm  ENABLE
  SHOW GET lcD7ThPrc ENABLE
ELSE
  SHOW GET lcD7Thrm  DISABLE
  SHOW GET lcD7ThPrc DISABLE
ENDIF

*--End of lfvUpFrcMth.
*!*************************************************************
*! Name      : lfUpFrcMth
*! Developer : NADER NABIL (NNA)
*! Date      : 05/11/2005
*! Purpose   : update forecast File with the next
*!           : Year's Months Sales Records from the Yearinpo File
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : lfUpFrcMth()
*!*************************************************************
*!C126267,1
FUNCTION lfUpFrcMth
PRIVATE lnMax , lnCnt , lcSiz , lcColStyle , lcSiz , lcScal , lnSepMins
STORE '' TO lcColStyle , lcSiz , lcScal
STORE 0 TO lnCnt , lnMax , lnSepMins
lnSepMins = lfvGtSclSp()
SELECT FORECAST
=AFIELDS(laFileStru)
CREATE CURSOR (lcTmpFrCst) FROM ARRAY laFileStru
INDEX ON Style+STR(nYear,4)+STR(nMonth,2) TAG (lcTmpFrCst) OF (lcTmpFrCst)
*-- New Cursor to add a total record for every style/month
CREATE CURSOR AcumFile FROM ARRAY laFileStru
INDEX ON Style+STR(nYear,4)+STR(nMonth,2) TAG StyMnth OF AcumFile

IF SEEK(STR(lnYear,4,0),'YEARINPO')
  SELECT YEARINPO
  COUNT FOR Nyear = lnYear TO lnMax
  =SEEK(STR(lnYear,4,0))
  SCAN REST WHILE STR(nyear,4,0)+style+color+csizecode = STR(lnYear,4,0)
    lnCnt = lnCnt + 1
    =lfThrmo(lnCnt , lnMax ,'7')
    *-- Collect the Style Parts to Search with it in the forecast File
    lcScal  = SUBSTR(CSIZECODE,1,3)
    lcColStyle = SUBST(Style,1,lnMjrWid) + IIF(lnClrPos >(lnMjrWid+1) , '-' + COLOR ,COLOR)
    lcColStyle = lcColStyle + IIF(lnSepMins = 3 , lcScal + SPACE(1) , '-' + lcScal)
    lcSiz      = RIGHT(ALLTRIM(CSIZECODE),1)
    FOR MNTH = 1 TO 12
      lcMnth = PADL(ALLTRIM(STR(MNTH)),2,'0')
      IF YEARINPO.NSHPMNTH&lcMnth <> 0
        SELECT (lcTmpFrCst)
        IF SEEK(lcColStyle + STR((YEARINPO.nYear)+1,4)+STR(MNTH,2))
          REPL NSalFrcst&lcSiz WITH NSalFrcst&lcSiz + YEARINPO.NSHPMNTH&lcMnth
        ELSE
          INSERT INTO (lcTmpFrCst)(STYLE,NYEAR,NMONTH,NSALFRCST&lcSiz) VALUES (lcColStyle,(Yearinpo.nYear)+1,MNTH,;
          YEARINPO.NSHPMNTH&lcMnth)
        ENDIF
      ENDIF
    ENDFOR
  ENDSCAN
ENDIF
*-- append the Total Records from the Temp File to the forecast file
SELECT (lcTmpFrCst)
lnCnt = 0
lnMax = RECCOUNT()
SCAN
  lnCnt = lnCnt + 1
  =lfThrmo(lnCnt , lnMax ,'7')
  SCATTER MEMVAR
  IF SEEK(STR(nyear,4)+STR(nmonth,2)+style,'FORECAST')
    SELECT FORECAST
    GATHER MEMVAR
  ELSE
    INSERT INTO FORECAST FROM MEMVAR
  ENDIF
ENDSCAN
USE IN (lcTmpFrCst)
*--Acumulate the new forecast Year's figures into month13
SELECT FORECAST
lnCnt = 0
COUNT TO lnMax FOR NYEAR=lnYear+1 AND NMONTH<>13
IF SEEK(STR(lnYear+1,4))
  SCAN REST WHILE STR(Nyear,4)+STR(Nmonth,2)+style = STR(lnYear + 1,4) FOR Nmonth <>13
    lnCnt = lnCnt + 1
    =lfThrmo(lnCnt , lnMax ,'7')
    SCATTER MEMVAR
    m.Nmonth = 13
    SELECT AcumFile
    IF SEEK(m.Style + STR(m.Nyear,4))
      FOR I = 1 TO 8
        lcSiz = STR(I,1)      
        REPL NSalFrcst&lcSiz WITH NSalFrcst&lcSiz + m.NSalFrcst&lcSiz
        REPL NPurFrcst&lcSiz WITH NPurFrcst&lcSiz + m.NPurFrcst&lcSiz        
      ENDFOR
    ELSE
      INSERT INTO AcumFile FROM MEMVAR
    ENDIF
  ENDSCAN
  *-- append the Total Records from the Temp File to the forecast file
  SELECT AcumFile
  SCAN
    SCATTER MEMVAR
    IF SEEK(STR(nyear,4)+STR(AcumFile.nmonth,2)+AcumFile.style,'FORECAST')
      SELECT FORECAST
      GATHER MEMVAR
    ELSE
      INSERT INTO FORECAST FROM MEMVAR
    ENDIF
  ENDSCAN
  USE IN AcumFile
ENDIF

*--End of lfUpFrcMth.
