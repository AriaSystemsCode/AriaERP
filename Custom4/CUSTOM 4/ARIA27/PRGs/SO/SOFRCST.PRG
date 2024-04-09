*:*********************************************************************************************
*: Program file  : SOFRCST.PRG (Convert Forcast program for Ogio SOGI200.PRG into Standard)
*: Program desc. : Program to Add, or modify a record in the Forcast file
*: System        : Aria Advantage Series.
*: Module        : Sales Order (SO)
*: Developer     : Adel Mohammed El Gazzar (ADEL)
*: DATE          : 02/20/2002
*: REFER TO      : E301848
*:*********************************************************************************************
*: Calls         : gfModalGen , gfOpenFile , gfItemMask ,ARIABROW
*:    -----------------------------------------------------------------------------
*:    Functions  : lfvStyle , lfDispSize , lfvWeek , lfvYear , lfInit   , lpClose
*:               : lpClsScr , lpSavScr   , lfwQty  , lfvQty  , lpShow   , lpDelScr
*:               : lfwWeek  , lfwYear .
*:    ----------------------------------------------------------------------------
*:    Procedures : lpClose.
*:*********************************************************************************************
*: Passed Parameters  : None
*:*********************************************************************************************
*: Example : DO SOFRCST
*:*********************************************************************************************
*:Modifications
*E301900,1 TMI 04/28/2002 Update the Plan in style file from forcast.dbf file for a selected style
*:*********************************************************************************************
*:

*-- Initialize variables. 
lcStyDesc = SPACE(30)
EXTERNAL ARRAY laData,laKeyField , laWobjects
STORE .F. TO llNoShow,llBrowse
STORE .T. TO laScrMode[1]
STORE ' ' TO lcScFields
laDefProc[7]  = .F.     && Cancel/Uncancel procedure(lpDelScr)
laDefProc[9]  = .F.     && Save procedure(lpSavScr)
laDefProc[10] = .F.     && close procedure(lpClsScr)
llFromAd  = .F.
lcOldStyle = ''
*E301900,4 TMI [START]
lnOldTotQt = 0         &&  Define lnOldTotQt 
*E301900,4 TMI [END  ]

*E301900,1 TMI [START] Define and activate Option menu.
=lfActPad()
*E301900,1 TMI [END  ] Define and activate Option menu.

IF !gfSetup()
  RETURN
ENDIF

*-- Initialize some variables if first time we run the screen.
IF !WEXIST(gcBaseWind)
  *-- Initialize some variables.
  lcStyHdr   = gfItemMask("HI")
  lnStyLngth = LEN(lcStyHdr)       && Var. hold the length of the style.
  lnMjrWid   = LEN(gfItemMask('PM'))
  lcScFields = 'STYLE,NWEEK,NYEAR,NFORQTY1,NFORQTY2,NFORQTY3,NFORQTY4,NFORQTY5,NFORQTY6,'+;
               'NFORQTY7,NFORQTY8,NFORTOTQTY'
  SELECT FORCAST
  SCATTER FIELDS &lcScFields TO laData BLANK
ENDIF

SET ORDER TO STYLE IN STYLE
ldDate      = CTOD('01/01/'+STR(YEAR(gdsysdate),4))
ldEndDate   = GOMONTH(LDdATE,12) - 1
No_Of_weeks = MAX(CEILING(((ldEndDate-DOW(ldEndDate)) - (ldDate-DOW(ldDate)+1))/7),1)
St_Day      = ldDate-DOW(ldDate)+1 
last_day    = ldEndDate-dow(ldEndDate)
laData[2]   = (MAX(CEILING((gdsysdate-St_Day)/7),1))

*-- Display Week Start Date.
DO CASE
   CASE BETWEEN(gdsysdate,St_Day,last_day)
     laData[3] = YEAR(gdsysdate) 
     ldDispDate = ((laData[2]-1)*7) + St_Day
   CASE gdsysdate <  St_Day
     laData[3] = YEAR(gdsysdate) - 1
     ldDispDate = ((laData[2]-1)*7) + St_Day
   CASE gdsysdate > last_day
     laData[3] = YEAR(gdsysdate) + 1
     laData[2] = 1
     ldDispDate = last_day + 1     
ENDCASE
lnOldYear = laData[3]
lnOldWeek = laData[2]

*- Define variable in screen.
STORE 0 TO lnOldQty,laData[4],laData[5],laData[6],laData[7],laData[8],laData[9],laData[10],;
           laData[11],laData[12]
lcSize1 = 'Size1'
lcSize2 = 'Size2'
lcSize3 = 'Size3'
lcSize4 = 'Size4'
lcSize5 = 'Size5'
lcSize6 = 'Size6'
lcSize7 = 'Size7'
lcSize8 = 'Size8'
lnCount = 8

PUSH KEY
lcHdEsc1= ON('KEY','ESC')
ON KEY LABEL ESC 
ON KEY LABEL ESC DO lpClose

*-- Call the Screen.
DO (gcScrDir+gcWinAppl+"\SOFRCST.SPX")
POP KEY

*E301900,1 TMI [START] Release the Option menu.
RELEASE PAD _INQUIRY OF _MSYSMENU
*E301900,1 TMI [END  ] Release the Option menu.
*- End Of Program.

*!*************************************************************
*! Name      : lfvStyle
*: Developer : Adel Mohammed El Gazzar (ADEL)
*: DATE      : 02/20/2002
*! Purpose   : validate for the style in style file
*!*************************************************************
*! Calls     : None. 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*
FUNCTION lfvStyle

IF MDOWN() OR (!llBrowse AND EMPTY(SUBSTR(laData[1],1,lnMjrWid)) )
  RETURN
ENDIF

SELECT STYLE 
laData[1]=IIF(llBrowse ,'?',laData[1])
IF llBrowse OR (!EMPTY(laData[1]) AND !SEEK(laData[1],'STYLE'))
  lcHdEsc1= ON('KEY','ESC')
  ON KEY LABEL ESC 
  laData[1] = gfStyBrw('I','','',.F.)
  ON KEY LABEL ESC &lcHdEsc1
  llbrowse = .F.
  IF EMPTY(laData[1])
    _CUROBJ = OBJNUM(laData[1])
    RETURN
  ENDIF
ENDIF

IF !EMPTY(laData[1])
  lcStyDesc = Desc
  = lfDispSize ()
ENDIF

=lfRefresh('lwForcast1')
*- End Of lfvStyle.

*!*************************************************************
*! Name      : lfDispSize
*: Developer : Adel Mohammed El Gazzar (ADEL)
*: DATE      : 02/20/2002
*! Purpose   : Display the sizes of the style+color from the scale file
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None.
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*
FUNCTION lfDispSize

=SEEK(laData[1],'STYLE')
=SEEK('S'+STYLE .Scale,'Scale')
lnCount  = Scale.Cnt
STORE PADL(ALLTRIM(scale.sz1),5) TO lcsize1
STORE PADL(ALLTRIM(scale.sz2),5) TO lcsize2
STORE PADL(ALLTRIM(scale.sz3),5) TO lcsize3
STORE PADL(ALLTRIM(scale.sz4),5) TO lcsize4
STORE PADL(ALLTRIM(scale.sz5),5) TO lcsize5
STORE PADL(ALLTRIM(scale.sz6),5) TO lcsize6
STORE PADL(ALLTRIM(scale.sz7),5) TO lcsize7
STORE PADL(ALLTRIM(scale.sz8),5) TO lcsize8
IF lnCount = 0
  lnCount = 8
ENDIF
=lfRefresh()

*- End Of lfDispSize
*!*************************************************************
*! Name      : lfvWeek
*: Developer : Adel Mohammed El Gazzar (ADEL)
*: DATE      : 02/20/2002
*! Purpose   : Validate for the week number
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None.
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*
FUNCTION lfvWeek

IF laData[2] =< 0
  *- Text Message  : Week number cannot be less or equal to zero!.
  *- Message No    : 36014.
  *- Buttom Message: OK
  *- Buttom No.    : 00000
  =gfModalGen('TRM36014B00000','ALERT','Week number')
  laData[2]  = lnOldWeek
  _CUROBJ = OBJNUM(laData[2])
  RETURN
ENDIF

ldDate      = CTOD('01/01/'+STR(laData[3],4))
ldEndDate   = GOMONTH(LDdATE,12) - 1
No_Of_weeks = MAX(CEILING(((ldEndDate-DOW(ldEndDate)) - (ldDate-DOW(ldDate)+1))/7),1)
St_Day      = ldDate-DOW(ldDate)+1 

IF laData[2] > ROUND(No_Of_weeks,2)
  *- Text Message  : Weeks number cannot exceeds XX Weeks!.
  *- Message No    : 40171.
  *- Buttom Message: OK
  *- Buttom No.    : 00000
  =gfModalGen('TRM40171B00000','ALERT','Weeks number'+"|"+STR(No_Of_weeks,2)+' weeks!.')  
  laData[2]  = lnOldWeek
  _CUROBJ = OBJNUM(laData[2])
ENDIF

IF laData[2] = 1
  ldDispDate = St_Day
ELSE
  ldDispDate = ((laData[2]-1)*7) + St_Day
ENDIF  
SHOW GET ldDispDate DISABLE
=lfRefresh1()

*- End oflfvWeek
*!*************************************************************
*! Name      : lfvYear
*: Developer : Adel Mohammed El Gazzar (ADEL)
*: DATE      : 02/20/2002
*! Purpose   : Check if the entered week belongs to the year or not
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None.
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*
FUNCTION lfvYear

IF LASTKEY()<> 13 AND LASTKEY()<>9
  RETURN
ENDIF
IF LEN(ALLTRIM(STR(laData[3]))) < 4
  *- Text Message  : Invalid year.
  *- Message No    : 42138.
  *- Buttom Message: OK
  *- Buttom No.    : 00000
  =gfModalGen('TRM42138B00000','ALERT','year')
  _CUROBJ = OBJNUM(laData[3])
  laData[3] = lnOldYear
  RETURN
ENDIF

ldDate      = CTOD('01/01/'+STR(laData[3],4))
ldEndDate   = GOMONTH(LDdATE,12) - 1
No_Of_weeks = MAX(CEILING(((ldEndDate-DOW(ldEndDate)) - (ldDate-DOW(ldDate)+1))/7),1)
IF laData[2] > ROUND(No_Of_weeks,2)
  *- Text Message  : Invalid year.
  *- Message No    : 42138.
  *- Buttom Message: OK
  *- Buttom No.    : 00000
  = gfModalGen(.F.,.F.,.F.,.F.,'This year contains only ' +STR(No_Of_weeks,2)+ ' week.')
  _CUROBJ = OBJNUM(laData[2])
  RETURN
ENDIF

*- Enable and disable sizes fileds.
IF !EMPTY(laData[1])
  IF SEEK(laData[1]+STR(laData[3],4)+STR(laData[2],2),'FORCAST')
    laScrMode = .F.
    laScrMode[2] = .T.
  ELSE
    laScrMode = .F.
    laScrMode[4] = .T.
  ENDIF
ELSE
  RETURN  
ENDIF
*- Cheak if we have recored in this week - Year.
IF SEEK(laData[1]+STR(laData[3],4)+STR(laData[2],2),'FORCAST')
  =lfInit()
ENDIF
SHOW GETS
St_Day    = ldDate-DOW(ldDate)+1 
IF laData[2] = 1
  ldDispDate = St_Day
ELSE
  ldDispDate = ((laData[2]-1)*7) + St_Day
ENDIF
SHOW GET ldDispDate DISABLE
*- End Of lfvYear.

*!*************************************************************
*! Name      : lfInit
*: Developer : Adel Mohammed El Gazzar (ADEL)
*: DATE      : 02/20/2002
*! Purpose   : Initialize the varibles from an existing 
*! Purpose   : record or with defaults
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: llFromFile(.T., means initialize form file)
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*
FUNCTION lfInit

=SEEK(laData[1]+STR(laData[3],4)+STR(laData[2],2),'FORCAST')
laData[4]   = FORCAST.nForQty1
laData[5]   = FORCAST.nForQty2
laData[6]   = FORCAST.nForQty3
laData[7]   = FORCAST.nForQty4
laData[8]   = FORCAST.nForQty5
laData[9]   = FORCAST.nForQty6
laData[10]  = FORCAST.nForQty7
laData[11]  = FORCAST.nForQty8
laData[12]  = FORCAST.nForTotQty
lnNumber = 1
FOR lnNumber = 4 TO 11
  SHOW GET  laData[lnNumber] DISABLE
ENDFOR
=lfRefresh()
*- End Of lfInit.

*!*************************************************************
*! Name      : lfwQty
*: Developer : Adel Mohammed El Gazzar (ADEL)
*: DATE      : 02/20/2002
*! Purpose   : Keeps the old qantity in a variable
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: NUMBER(the amount number).
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*
FUNCTION lfwQty
PARAMETER NUMBER

lnOldQty =laData[NUMBER]

*- End Of lfwQty.
*!*************************************************************
*! Name      : lfvQty
*: Developer : Adel Mohammed El Gazzar (ADEL)
*: DATE      : 02/20/2002
*! Purpose   : Validate the entered quantity
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: NUMBER(the amount number).
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*
FUNCTION lfvQty
PARAMETER NUMBER

IF laData[NUMBER] < 0 
  *- Text Message  : Negative values are not allowed.
  *- Message No    : 42000
  *- Buttom Message: OK
  *- Buttom No.    : 00000
  = gfModalGen('TRM42000B00000','DIALOG')
  laData[NUMBER] = lnOldQty
  _CUROBJ = OBJNUM(laData[NUMBER])
  RETURN
ENDIF
llCUpdate = .T.
laData[12]=laData[12]-lnOldqty+laData[NUMBER]
=lfRefresh3()
*- End Of lfvQty.

*!*************************************************************
*! Name      : lfwWeek
*: Developer : Adel Mohammed El Gazzar (ADEL)
*: DATE      : 02/20/2002
*! Purpose   : get the old value of the week num. in a variable
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None.
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*
FUNCTION lfwWeek

lnOldWeek = laData[2]

*- End Of lfwWeek.
*!*************************************************************
*! Name      : lfwYear
*: Developer : Adel Mohammed El Gazzar (ADEL)
*: DATE      : 02/20/2002
*! Purpose   : Get the old value of the Year num. in a variable
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None.
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*
FUNCTION lfwYear

lnOldYear = laData[3]

*- End Of lfwYear.

*!*************************************************************
*! Name      : lpClose
*: Developer : Adel Mohammed El Gazzar (ADEL)
*: DATE      : 02/20/2002
*! Purpose   : Close screen
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None.
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*
PROCEDURE lpClose

IF laScrMode[3] OR laScrMode[4]
  IF gfModalGen('TRM38028B32000','DIALOG') = 1 
    *- Text Message  : Are you sure you want to cancel this session and lose all changes?
    *- Message No    : 38028
    *- Buttom Message: Yes - No 
    *- Buttom No.    : 32000
    llFromAd  = .F.
    IF !EOF('FORCAST')
      REPLACE FORCAST.llok_stat WITH .F.
    ENDIF
    IF laScrMode[3]
      =lfInit()
      laScrMode = .F.
      laScrMode[2] = .T.
    ELSE
      laScrMode = .F.
      laScrMode[1] = .T.
    ENDIF  
  ENDIF
  SHOW GETS
ELSE
  CLEAR READ  
ENDIF
RETURN


*!*************************************************************
*! Name      : lpShow
*: Developer : Adel Mohammed El Gazzar (ADEL)
*: DATE      : 02/20/2002
*! Purpose   : Show function
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lpShow()
*!*************************************************************
FUNCTION lpShow

DO CASE
  CASE laScrMode[1]
    SHOW GET ibStyle   ENABLE
    SHOW GET ibWeek    ENABLE
    SHOW GET laData[1] ENABLE
    SHOW GET laData[2] ENABLE
    SHOW GET laData[3] ENABLE
    lcStyDesc = SPACE(30)
    SHOW GET lcStyDesc
    IF !llFromAd
      ldDate      = CTOD('01/01/'+STR(YEAR(gdsysdate),4))
      ldEndDate   = GOMONTH(LDdATE,12) - 1
      No_Of_weeks = MAX(CEILING(((ldEndDate-DOW(ldEndDate)) - (ldDate-DOW(ldDate)+1))/7),1)
      St_Day      = ldDate-DOW(ldDate)+1 
      last_day    = ldEndDate-dow(ldEndDate)
      laData[2]   = (MAX(CEILING((gdsysdate-St_Day)/7),1))
      *-- Display Week Start Date.
      DO CASE
        CASE BETWEEN(gdsysdate,St_Day,last_day)
         laData[3] = YEAR(gdsysdate) 
         ldDispDate = ((laData[2]-1)*7) + St_Day
       CASE gdsysdate <  St_Day
         laData[3] = YEAR(gdsysdate) - 1
         ldDispDate = ((laData[2]-1)*7) + St_Day
       CASE gdsysdate > last_day
         laData[3] = YEAR(gdsysdate) + 1
         laData[2] = 1
         ldDispDate = last_day + 1     
      ENDCASE
    ELSE
      laData[2] = nWeek
      laData[3] = nYear
      laData[1] = IIF(EMPTY(STYLE),lcOldStyle,STYLE)
      *E301900,4 TMI [START] Restore Old select case if last totqty is zero
      IF lnOldTotQt = 0 
        laData[2] = lnOldWeek
        laData[3] = lnOldYear
      ELSE
      *E301900,4 TMI [END  ] Restore Old select case if last totqty is zero

        IF laData[2] = No_Of_weeks
          laData[2] = 1
          laData[3] = laData[3] + 1
          ldDate = CTOD('01/01/'+STR(laData[3],4))
          ldEndDate = GOMONTH(LDdATE,12) - 1
          No_Of_weeks = MAX(CEILING(((ldEndDate-DOW(ldEndDate)) - (ldDate-DOW(ldDate)+1))/7),1)
          St_Day    = ldDate-DOW(ldDate)+1 
          last_day  = ldEndDate-dow(ldEndDate)
        ELSE  
          laData[2] = laData[2] + 1
        ENDIF  
        IF laData[2] = 1
          ldDispDate = St_Day
        ELSE
          ldDispDate = ((laData[2])*7) + St_Day
        ENDIF  
      
      *E301900,4 TMI [START] Close the above If statemnet
      ENDIF
      *E301900,4 TMI [END  ] Close the above If statemnet

      SHOW GET laData[2] ENABLE
      SHOW GET laData[3] ENABLE      
    ENDIF
    
    lnOldYear = laData[3]
    lnOldWeek = laData[2]
    _CUROBJ = OBJNUM(laData[1])
  CASE laScrMode[2]
    SHOW GET ldDispDate DISABLE 
    SHOW GET ibStyle    DISABLE 
    SHOW GET ibWeek     DISABLE
    *- Function to Display the sizes of the style+color from the scale file.
    =lfDispSize()
    ldDate      = CTOD('01/01/'+STR(laData[3],4))    
    St_Day    = ldDate-DOW(ldDate)+1 
    IF laData[2] = 1
      ldDispDate = St_Day
    ELSE
      ldDispDate = ((laData[2]-1)*7) + St_Day
    ENDIF
  CASE laScrMode[3]
    SHOW GETS WINDOW lwForCast1  DISABLE ONLY
    SHOW GETS WINDOW lwForCast3  DISABLE ONLY
    FOR lnNumber = 1 TO Scale.Cnt
      SHOW GET  laData[3+lnNumber] ENABLE
    ENDFOR
    _CUROBJ = OBJNUM(laData[4])
    llFromAd  = .F.

  CASE laScrMode[4] 
    SHOW GETS WINDOW lwForCast1  DISABLE ONLY
    SHOW GETS WINDOW lwForCast3  DISABLE ONLY
    SHOW GET laData[1] DISABLE
    SHOW GET ibStyle   DISABLE  
    SHOW GET ibWeek    DISABLE
    SHOW GET laData[2] DISABLE
    SHOW GET laData[3] DISABLE      
    lnCount  = IIF(Scale.Cnt = 0,8,Scale.Cnt)
    FOR lnNumber = 1 TO lnCount
      SHOW GET  laData[3+lnNumber] ENABLE
    ENDFOR
    _CUROBJ = OBJNUM(laData[4])
ENDCASE 

*!*************************************************************
*! Name      : lpClsScr
*: Developer : Adel Mohammed El Gazzar (ADEL)
*: DATE      : 02/20/2002
*! Purpose   : Cancel new Order
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lpClsScr()
*!*************************************************************
FUNCTION lpClsScr

llFromAd  = .F.

SELECT FORCAST
IF laScrMode[3]
  SCATTER FIELDS &lcScFields TO laData
ENDIF


*!*************************************************************
*! Name      : lpSavScr
*: Developer : Adel Mohammed El Gazzar (ADEL)
*: DATE      : 02/20/2002
*! Purpose   : Save new or modified order
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lpSavScr()
*!*************************************************************
FUNCTION lpSavScr

lnAlias = SELECT(0)
SELECT FORCAST
IF laData[12] <> 0
  *- If modify an exist record. 
  IF laScrMode[4]
    APPEND BLANK
    SHOW GET laData[1]  ENABLE
    SHOW GET lcColor    ENABLE
    SHOW GET ibStyle    ENABLE
    SHOW GET ibWeek     ENABLE
    SHOW GET ibColor    ENABLE
    SHOW GET laData[2]  ENABLE
    SHOW GET laData[3]  ENABLE      
  ENDIF
  =RLOCK()
  GATHER FROM laData FIELDS &lcScFields
  UNLOCK
  llFromAd = laScrMode[4]
  lcOldStyle = STYLE
ELSE
  *E301900,4 TMI [START] if in add mode with no qty entered , exit
  IF laData[12] = 0
    =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'Total Quantity cannot be Zero.')
  ENDIF
  lnOldWeek  = ladata[2]
  lnOldYear  = ladata[3]
  *E301900,4 TMI [END  ] if in add mode with no qty entered , exit
  lcOldStyle = ladata[1]
  llFromAd = .T.
ENDIF
*E301900,4 TMI [START] Assign a value to lnOldTotQt 
lnOldTotQt = laData[12]
*E301900,4 TMI [END  ] Assign a value to lnOldTotQt 
SELECT(lnAlias)

*!*************************************************************
*! Name      : lpDelScr
*: Developer : Adel Mohammed El Gazzar (ADEL)
*: DATE      : 02/20/2002
*! Purpose   : Cancel/Uncancel order
*!*************************************************************
*! Calls     : gfModalGen,lfGetInfo
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lpDelScr()
*!*************************************************************
FUNCTION lpDelScr

lnAlias = SELECT()
SELECT FORCAST
=SEEK(laData[1]+STR(laData[3],4)+STR(laData[2],2),'FORCAST')  
DELETE
laScrMode    = .F.
laScrMode[1] = .T.
llFromAd  = .F.

SHOW GETS
SELECT(lnAlias)


*:**************************************************************************
*:* Name        : lfActPad
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/24/02
*:* Purpose     : Define Pad
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfActPad()
*:***************************************************************************
FUNCTION lfActPad

DEFINE PAD _INQUIRY OF _MSYSMENU PROMPT 'O\<ptions' KEY ALT+P , ' '
ON PAD _INQUIRY OF _msysmenu ACTIVATE POPUP _INQURYPOP

DEFINE POPUP _INQURYPOP MARGIN SHADOW
*DEFINE BAR 1  OF _INQURYPOP PROMPT '\<Update Plan' SKIP FOR laScrMode[1] .OR. laScrMode[4]
DEFINE BAR 1  OF _INQURYPOP PROMPT '\<Update Plan' SKIP FOR !laScrMode[2]

ON SELECTION POPUP _INQURYPOP DO lpvInquiry
*-- end of lfActPad.

*:**************************************************************************
*:* Name        : lpvInquiry  
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/24/02
*:* Purpose     : Inquery function to run option menu
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lpvInquiry()
*:* Refer to    : E301900,1
*:***************************************************************************
FUNCTION lpvInquiry
PRIVATE lcAlias,m.Plan1,m.Plan2,m.Plan3,m.Plan4,m.Plan5,m.Plan6,m.Plan7,m.Plan8,m.TOTPLAN

lcAlias = ALIAS()

DO CASE 
  CASE BAR() = 1
    IF !EMPTY(laData[1]) &&AND laData[3] = YEAR(gdSysDate)
      *E301900,4 TMI [START] Put a more descriptive message.
      *lcMsg = 'Are you sure to Update the Plane for style ' + laData[1] + ;
            ' for the Year ' + STR(laData[3],4) + ' from the forcasting data file ?'
      lcMsg = 'Are you sure you want to update the plan for style('+ laData[1] +;
              ') for the year '+ STR(laData[3],4) +' ?'
      *E301900,4 TMI [END  ] Put a more descriptive message.
      IF gfModalGen("TRM00000B00006","DIALOG",.F.,.F.,lcMsg) = 1
        =SEEK(laData[1]+STR(laData[3],4),'FORCAST')
        STORE 0 TO m.Plan1,m.Plan2,m.Plan3,m.Plan4,m.Plan5,m.Plan6,m.Plan7,m.Plan8,m.TOTPLAN
        SCAN REST WHILE STYLE+STR(NYEAR,4)+STR(NWEEK,2) = laData[1]+STR(laData[3],4)
          m.Plan1 = m.Plan1 + FORCAST.NFORQTY1  
          m.Plan2 = m.Plan2 + FORCAST.NFORQTY2  
          m.Plan3 = m.Plan3 + FORCAST.NFORQTY3  
          m.Plan4 = m.Plan4 + FORCAST.NFORQTY4  
          m.Plan5 = m.Plan5 + FORCAST.NFORQTY5  
          m.Plan6 = m.Plan6 + FORCAST.NFORQTY6  
          m.Plan7 = m.Plan7 + FORCAST.NFORQTY7  
          m.Plan8 = m.Plan8 + FORCAST.NFORQTY8  
          m.TOTPLAN = m.TOTPLAN + FORCAST.NFORTOTQTY
        ENDSCAN
        IF m.TOTPLAN > 0 AND SEEK(laData[1],'STYLE')
          SELECT Style
          REPLACE PLAN1 WITH m.Plan1,;
                  PLAN2 WITH m.Plan2,;
                  PLAN3 WITH m.Plan3,;
                  PLAN4 WITH m.Plan4,;
                  PLAN5 WITH m.Plan5,;
                  PLAN6 WITH m.Plan6,;
                  PLAN7 WITH m.Plan7,;
                  PLAN8 WITH m.Plan8,;
                  TOTPLAN WITH m.TOTPLAN
        ENDIF
      ENDIF
    ENDIF
  *--  
ENDCASE
=SEEK(laData[1]+STR(laData[3],4),'FORCAST')
SELECT (lcAlias)
*-- end of lpvInquiry.