*:***********************************************************************************
*: Program file  : SOGI200.PRG
*: Program desc. : Program to Add, or modify a record in the Forcast file
*: System        : Aria Advantage Series.
*: Module        : Sales Order (SO)
*: Developer     : ABDOU ELGENDI        - (ABD)
*: DATE          : 05/09/2000
*: REFER TO      : C101856.
*:***********************************************************************************
*: Calls         : gfModalGen , gfOpenFile , gfItemMask ,ARIABROW
*:    -----------------------------------------------------------------------------
*:    Functions  : lfvStyle , lfDispSize , lfvWeek , lfvYear , lfInit   , lfSave
*:               : lfvEdit  , lfBrowse   , lfwQty  , lfvQty  , lfvClose , lfvDelete
*:               : lfwWeek  , lfvSelect  , lfwYear .
*:    ----------------------------------------------------------------------------
*:    Procedures : lpClose.
*:***********************************************************************************
*: Passed Parameters  : None
*:***********************************************************************************
*: Example : DO SOOGI200
*:***********************************************************************************
*: This Program is due to C101856 ...
*:***********************************************************************************
*:Modifications
*:***********************************************************************************
*:

*-- Initialize variables. 
lcClose  = "\<Close"
lcCancel = "\<Cancel"
llBrowse = .F.
lcPrompt = lcClose
lcStyle = SPACE(19)
lcStyDesc = SPACE(30)

*-- End initialize variables. 

*-- Initialize some variables if first time we run the screen.
IF !WEXIST(gcBaseWind)
  *-- Initialize some variables.
  lcStyHdr   = gfItemMask("HI")
  lnStyLngth = LEN(lcStyHdr)       && Var. hold the length of the style.
  lnMjrWid   = LEN(gfItemMask('PM'))   
ENDIF

*- Open Need Files.
=gfOpenFile(gcDataDir+'CODES',gcDataDir+'CODES','SH')
=gfOpenFile(gcDataDir+'SCALE',gcDataDir+'SCALE','SH')
=gfOpenFile(gcDataDir+'STYLE',gcDataDir+'STYLE','SH')
=gfOpenFile(gcDataDir+'FORCAST',gcDataDir+'FORCAST','SH')

ldDate    = CTOD('01/01/'+STR(YEAR(DATE()),4))
ldEndDate = GOMONTH(LDdATE,12) - 1
No_Of_weeks = MAX(CEILING(((ldEndDate-DOW(ldEndDate)) - (ldDate-DOW(ldDate)+1))/7),1)
St_Day    = ldDate-DOW(ldDate)+1 
last_day  = ldEndDate-dow(ldEndDate)
lnWeek    = (MAX(CEILING((DATE()-St_Day)/7),1))

*-- Display Week Start Date.
DO CASE
   CASE BETWEEN(DATE(),St_Day,last_day)
     lnYear = YEAR(DATE()) 
     ldDispDate = ((lnWeek-1)*7) + St_Day
   CASE DATE() <  St_Day
     lnYear = YEAR(DATE()) - 1
     ldDispDate = ((lnWeek-1)*7) + St_Day
   CASE DATE() > last_day
     lnYear = YEAR(DATE()) + 1
     lnWeek = 1
     ldDispDate = last_day + 1     
ENDCASE

lnOldYear = lnYear
lnOldWeek = lnWeek
*- Define variable in screen.
STORE 0 TO lnOldQty,lnqty1,lnqty2,lnqty3,lnqty4,lnqty5,lnqty6,lnqty7,;
           lnqty8,lnTotal
lcSize1 = 'Size1'
lcSize2 = 'Size2'
lcSize3 = 'Size3'
lcSize4 = 'Size4'
lcSize5 = 'Size5'
lcSize6 = 'Size6'
lcSize7 = 'Size7'
lcSize8 = 'Size8'
lnCount = 8
llModify  = .F.

PUSH KEY
lcHdEsc1= ON('KEY','ESC')
ON KEY LABEL ESC 
ON KEY LABEL ESC DO lpClose
*-- Call the Screen.
DO (gcScrDir+gcWinAppl+"\SOGI200.SPX")
POP KEY

*- End Of Program.
*!*************************************************************
*! Name      : lfvStyle
*! Developer : ABDOU ELGENDI  - (ABD)
*! Date      : 08/14/2000
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

IF MDOWN() OR (!llBrowse AND EMPTY(SUBSTR(lcStyle,1,lnMjrWid)) )
  RETURN
ENDIF

SELECT STYLE 
lcStyle=IIF(llBrowse ,'?',lcStyle)
IF llBrowse OR (!EMPTY(lcStyle) AND !SEEK(lcStyle,'STYLE'))
  lcHdEsc1= ON('KEY','ESC')
  ON KEY LABEL ESC 
  lcStyle = gfStyBrw('I','','',.F.)
  ON KEY LABEL ESC &lcHdEsc1
  llbrowse = .F.
  IF EMPTY(lcStyle)
    _CUROBJ = OBJNUM(lcStyle)
    RETURN
  ENDIF
ENDIF

IF !EMPTY(lcStyle)
  lcStyDesc = Desc
  = lfDispSize ()
ENDIF

=lfRefresh('lwForcast1')

*- End Of lfvStyle.
*!*************************************************************
*! Name      : lfDispSize
*! Developer : ABDOU ELGENDI  - (ABD)
*! Date      : 08/14/2000
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

=SEEK(lcStyle,'STYLE')
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
*! Developer : ABDOU ELGENDI  - (ABD)
*! Date      : 08/14/2000
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

IF lnWeek =< 0
  *- Text Message  : Week number cannot be less or equal to zero!.
  *- Message No    : 36014.
  *- Buttom Message: OK
  *- Buttom No.    : 00000
  =gfModalGen('TRM36014B00000','ALERT','Week number')
  lnWeek  = lnOldWeek
  _CUROBJ = OBJNUM(lnWeek)
  RETURN
ENDIF

ldDate    = CTOD('01/01/'+STR(lnYear,4))
ldEndDate = GOMONTH(LDdATE,12) - 1
No_Of_weeks = MAX(CEILING(((ldEndDate-DOW(ldEndDate)) - (ldDate-DOW(ldDate)+1))/7),1)
St_Day    = ldDate-DOW(ldDate)+1 

IF lnWeek > ROUND(No_Of_weeks,2)
  *- Text Message  : Weeks number cannot exceeds XX Weeks!.
  *- Message No    : 40171.
  *- Buttom Message: OK
  *- Buttom No.    : 00000
  =gfModalGen('TRM40171B00000','ALERT','Weeks number'+"|"+STR(No_Of_weeks,2)+' weeks!.')  
  lnWeek  = lnOldWeek
  _CUROBJ = OBJNUM(lnWeek)
ENDIF

IF lnWeek = 1
  ldDispDate = St_Day
ELSE
  ldDispDate = ((lnWeek-1)*7) + St_Day
ENDIF  
SHOW GET ldDispDate DISABLE

=lfRefresh1()

*- End oflfvWeek
*!*************************************************************
*! Name      : lfvYear
*! Developer : ABDOU ELGENDI  - (ABD)
*! Date      : 08/14/2000
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

IF len(ALLTRIM(STR(lnYear))) < 4
  *- Text Message  : Invalid year.
  *- Message No    : 42138.
  *- Buttom Message: OK
  *- Buttom No.    : 00000
  =gfModalGen('TRM42138B00000','ALERT','year')
  _CUROBJ = OBJNUM(lnYear)
  lnYear = lnOldYear
  RETURN
ENDIF

ldDate      = CTOD('01/01/'+STR(lnYear,4))
ldEndDate   = GOMONTH(LDdATE,12) - 1
No_Of_weeks = MAX(CEILING(((ldEndDate-DOW(ldEndDate)) - (ldDate-DOW(ldDate)+1))/7),1)
IF lnWeek > ROUND(No_Of_weeks,2)
  *- Text Message  : Invalid year.
  *- Message No    : 42138.
  *- Buttom Message: OK
  *- Buttom No.    : 00000
  = gfModalGen(.f.,.f.,.f.,.f.,'This year contains only ' +STR(No_Of_weeks,2)+ ' week.')
  _CUROBJ = OBJNUM(lnWeek)
  RETURN
ENDIF

*- Enable and disable sizes fileds.
IF !EMPTY(lcStyle)
  SHOW GET lcStyle DISABLE
  SHOW GET ibStyle DISABLE  
  SHOW GET lnWeek  DISABLE
  SHOW GET lnYear  DISABLE      
  FOR lnNumber = 1 TO lnCount
    lcVariable = "lnqty"+STR(lnNumber,1)
    SHOW GET  &lcVariable ENABLE
  ENDFOR
ELSE
  RETURN  
ENDIF

*- Cheak if we have recored in this week - Year.
IF SEEK(lcStyle+STR(lnYear,4)+STR(lnWeek,2),'FORCAST')
  =lfInit()
  FOR lnNumber = 1 TO 8
    lcVariable = "lnqty"+STR(lnNumber,1)
    SHOW GET  &lcVariable DISABLE
  ENDFOR
  SHOW GET pbDelete  ENABLE      
  SHOW GET pbEdit    ENABLE
  SHOW GET pbSelect  ENABLE
  SHOW GET pbSave    DISABLE
  lcPrompt = lcClose
ELSE
  SHOW GET pbSave   ENABLE
  SHOW GET pbBrowse DISABLE
  lcPrompt = lcCancel
  _CUROBJ = OBJNUM(lnqty1)
ENDIF
SHOW GET pbClose,1 PROMPT lcPrompt

St_Day    = ldDate-DOW(ldDate)+1 
IF lnWeek = 1
  ldDispDate = St_Day
ELSE
  ldDispDate = ((lnWeek-1)*7) + St_Day
ENDIF
SHOW GET ldDispDate DISABLE

*- End Of lfvYear.
*!*************************************************************
*! Name      : lfInit
*! Developer : ABDOU ELGENDI  - (ABD)
*! Date      : 08/14/2000
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

=SEEK(lcStyle+STR(lnYear,4)+STR(lnWeek,2),'FORCAST')
lnQty1   = FORCAST.nForQty1
lnQty2   = FORCAST.nForQty2
lnQty3   = FORCAST.nForQty3
lnQty4   = FORCAST.nForQty4
lnQty5   = FORCAST.nForQty5
lnQty6   = FORCAST.nForQty6
lnQty7   = FORCAST.nForQty7
lnQty8   = FORCAST.nForQty8
lnTotal  = FORCAST.nForTotQty
lnNumber = 1
FOR lnNumber = 1 TO 8
  lcVariable = "lnqty"+STR(lnNumber,1)
  SHOW GET  &lcVariable DISABLE
ENDFOR
=lfRefresh()

*- End Of lfInit.
*!*************************************************************
*! Name      : lfSave
*! Developer : ABDOU ELGENDI  - (ABD)
*! Date      : 08/14/2000
*! Purpose   : Save a new record on the forcast file or 
*! Purpose   : the modification on an existing one
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None.
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*
FUNCTION lfSave

lnAlias = SELECT()
SELECT FORCAST

*- If modify an exist record. 
IF !llModify
  APPEND BLANK
  SHOW GET lcStyle   ENABLE
  SHOW GET lcColor   ENABLE
  SHOW GET ibStyle   ENABLE
  SHOW GET ibColor   ENABLE
  SHOW GET lnWeek    ENABLE
  SHOW GET lnYear    ENABLE      
ELSE 
  *- If new record.
  SHOW GET pbDelete  ENABLE      
  SHOW GET pbEdit    ENABLE
  SHOW GET pbSelect  ENABLE
ENDIF

REPLACE STYLE       WITH lcStyle,;
        nWeek       WITH lnWeek ,;
        nYear       WITH lnYear ,;        
        NForQTY1       WITH lnqty1 ,;
        NForQTY2       WITH lnqty2 ,;
        NForQTY3       WITH lnqty3 ,;
        NForQTY4       WITH lnqty4 ,;
        NForQTY5       WITH lnqty5 ,;
        NForQTY6       WITH lnqty6 ,;                           
        NForQTY7       WITH lnqty7 ,;                           
        NForQTY8       WITH lnqty8 ,;
        NForTotQty  WITH lnTotal

*- If modify an exist record. 
IF llModify        
  =lfInit()
ELSE
  IF lnWeek = No_Of_weeks
    lnWeek = 1
    lnYear = lnYear + 1
    ldDate = CTOD('01/01/'+STR(lnYear,4))
    ldEndDate = GOMONTH(LDdATE,12) - 1
    No_Of_weeks = MAX(CEILING(((ldEndDate-DOW(ldEndDate)) - (ldDate-DOW(ldDate)+1))/7),1)
    St_Day    = ldDate-DOW(ldDate)+1 
    last_day  = ldEndDate-dow(ldEndDate)
  ELSE  
    lnWeek = lnWeek + 1
  ENDIF  

  IF lnWeek = 1
    ldDispDate = St_Day
  ELSE
    ldDispDate = ((lnWeek-1)*7) + St_Day
  ENDIF  
  SHOW GET ldDispDate DISABLE

  lnOldWeek = lnWeek
  FOR lnNumber  = 1 TO 8
    lcVariable  = "lnqty"+STR(lnNumber,1)
    &lcVariable = 0
  ENDFOR
  lnTotal = 0
  SHOW GETS
  _CUROBJ = OBJNUM(lcStyle)  
ENDIF
FOR lnNumber = 1 TO 8
  lcVariable = "lnqty"+STR(lnNumber,1)
  SHOW GET  &lcVariable DISABLE
ENDFOR

SHOW GET pbSave    DISABLE
lcPrompt = lcClose
SHOW GET pbClose,1 PROMPT lcPrompt
SHOW GET pbBrowse  ENABLE
llModify  = .F.        
SELECT(lnAlias)

*- End Of lfSave.
*!*************************************************************
*! Name      : lfvEdit
*! Developer : ABDOU ELGENDI  - (ABD)
*! Date      : 08/14/2000
*! Purpose   : Permit the user to modify an existing 
*! Purpose   : Record in the forcast file
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None.
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*
FUNCTION lfvEdit

llModify  = .T.
FOR lnNumber = 1 TO lnCount
  lcVariable = "lnqty"+STR(lnNumber,1)
  SHOW GET  &lcVariable ENABLE
ENDFOR
SHOW GET pbEdit   DISABLE
SHOW GET pbSave   ENABLE
SHOW GET pbBrowse DISABLE
SHOW GET pbSelect DISABLE
SHOW GET pbDelete DISABLE
lcPrompt = lcCancel
SHOW GET pbClose,1 PROMPT lcPrompt
_CUROBJ = OBJNUM(lnqty1)

*- End Of lfvEdit.
*!*************************************************************
*! Name      : lfBrowse
*! Developer : ABDOU ELGENDI  - (ABD)
*! Date      : 08/14/2000
*! Purpose   : Browse the forcast file
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None.
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*
FUNCTION lfBrowse

lnAlias = SELECT()
SELECT FORCAST
GOTO TOP
DECLARE laData[3]
laData = ''
lcOld_ttl   = lcFile_ttl
lcPBrowTitl = "Forcast Quantities"   && Browse title.
lcTmpFld    = lcBrFields             && Save browse fields.

lcBrFields = [STYLE:H="Style",nYear:H="Year",Nweek:H="Week",]+;
             [NForqty1:H="Qty1",NForqty2:H="Qty2",NForqty3:H="Qty3",NForqty4:H="Qty4",]+;
             [NForqty5:H="Qty5",NForqty6:H="Qty6",NForqty7:H="Qty7",NForqty8:H="Qty8",NFORTOTQTY:H="TotQty"]
             
llSele = ARIABROW('',lcPBrowTitl,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,;
                     "","","STYLE,NWEEK,NYEAR","laData")

*-- Restore browse fields.
lcBrFields = lcTmpFld
lcFile_ttl = lcOld_ttl
SELECT (lnAlias)

IF llSele
 lcStyle =laData[1]
 *lcColor =laData[2]
 lnWeek  =laData[2]
 lnYear  =laData[3]   
 
 *- Function to Display the sizes of the style+color from the scale file.
 =lfDispSize()
 
 SHOW GET lcStyle   DISABLE
 SHOW GET lcColor   DISABLE
 SHOW GET ibStyle   DISABLE
 SHOW GET ibColor   DISABLE
 SHOW GET lnWeek    DISABLE
 SHOW GET lnYear    DISABLE
 SHOW GET pbEdit    ENABLE
 SHOW GET pbDelete  ENABLE
 SHOW GET pbSelect  ENABLE

 ldDate    = CTOD('01/01/'+STR(lnYear,4))
 ldEndDate = GOMONTH(LDdATE,12) - 1
 St_Day    = ldDate-DOW(ldDate)+1 
 IF lnWeek = 1
   ldDispDate = St_Day
 ELSE
   ldDispDate = ((lnWeek-1)*7) + St_Day
 ENDIF  
 SHOW GET ldDispDate DISABLE
 
 =lfInit()
 =lfRefresh()
ENDIF
SELECT(lnAlias)

*- End Of lfBrowse.
*!*************************************************************
*! Name      : lfwQty
*! Developer : ABDOU ELGENDI  - (ABD)
*! Date      : 08/14/2000
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

lnOldQty =lnqty&NUMBER

*- End Of lfwQty.
*!*************************************************************
*! Name      : lfvQty
*! Developer : ABDOU ELGENDI  - (ABD)
*! Date      : 08/14/2000
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

IF lnqty&NUMBER < 0 
  *- Text Message  : Negative values are not allowed.
  *- Message No    : 42000
  *- Buttom Message: OK
  *- Buttom No.    : 00000
  = gfModalGen('TRM42000B00000','DIALOG')
  lnqty&NUMBER = lnOldQty
  _CUROBJ = OBJNUM(lnqty&NUMBER)
  RETURN
ENDIF
lnTotal=lnTotal-lnOldqty+lnqty&NUMBER
=lfRefresh3()

*- End Of lfvQty.
*!*************************************************************
*! Name      : lfvClose
*! Developer : ABDOU ELGENDI  - (ABD)
*! Date      : 08/14/2000
*! Purpose   : Check if any changes happens ask the user 
*! Purpose   : If he wants to loose them or not
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None.
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*
FUNCTION lfvClose

IF lcPrompt = lcCancel
  *- Text Message  : Are you sure you want to cancel this session and lose all changes?
  *- Message No    : 38028
  *- Buttom Message: Yes - No 
  *- Buttom No.    : 32000
  IF gfModalGen('TRM38028B32000','DIALOG') = 1 
    IF llModify 
      =lfInit()
      SHOW GET lcStyle DISABLE
      SHOW GET ibStyle DISABLE
      SHOW GET lnWeek  DISABLE
      SHOW GET lnYear  DISABLE
      SHOW GET pbSelect ENABLE
      SHOW GET pbSave   DISABLE    
      SHOW GET pbBrowse ENABLE        
      SHOW GET pbEdit   ENABLE
      SHOW GET pbDelete   ENABLE      
    ELSE
      lcStyle = SPACE(19)
      ldDate    = CTOD('01/01/'+STR(YEAR(DATE()),4))
      ldEndDate = GOMONTH(LDdATE,12) - 1
      No_Of_weeks = MAX(CEILING(((ldEndDate-DOW(ldEndDate)) - (ldDate-DOW(ldDate)+1))/7),1)
      St_Day    = ldDate-DOW(ldDate)+1 
      last_day  = ldEndDate-dow(ldEndDate)
      lnWeek    = (MAX(CEILING((DATE()-St_Day)/7),1))
      DO CASE
        CASE BETWEEN(Date(),St_Day,last_day)
          lnYear = YEAR(Date()) 
          ldDispDate = ((lnWeek-1)*7) + St_Day
        CASE Date() <  St_Day
          lnYear = YEAR(Date()) - 1
          ldDispDate = ((lnWeek-1)*7) + St_Day
        CASE Date() > last_day
          lnYear = YEAR(Date()) + 1
          lnWeek = 1
          ldDispDate = St_Day
       ENDCASE
       SHOW GET ldDispDate DISABLE
      FOR lnNumber = 1 TO 8
        lcVariable = "lnqty"+STR(lnNumber,1)
        &lcVariable = 0
      ENDFOR
      lnTotal = 0
      =lfRefresh()
      SHOW GET lcStyle ENABLE
      SHOW GET ibStyle ENABLE
      SHOW GET lnWeek  ENABLE
      SHOW GET lnYear  ENABLE
      SHOW GET pbSelect DISABLE
      SHOW GET pbSave   DISABLE    
      SHOW GET pbBrowse ENABLE        
      SHOW GET pbEdit   DISABLE
      _CUROBJ = OBJNUM(lcStyle)
    ENDIF                
    FOR lnNumber = 1 TO 8
      lcVariable = "lnqty"+STR(lnNumber,1)
      SHOW GET  &lcVariable DISABLE
    ENDFOR
    lcPrompt = lcClose
    SHOW GET pbClose,1 PROMPT lcPrompt
  ENDIF                  
ELSE
  CLEAR READ
ENDIF
llModify = .F.

*- End Of lfvClose.
*!*************************************************************
*! Name      : lfvDelete
*! Developer : ABDOU ELGENDI  - (ABD)
*! Date      : 08/14/2000
*! Purpose   : Delete a record from the forcast file.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None.
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*
FUNCTION lfvDelete

*- Text Message  : Are you sure you want to delete ð? 
*- Message No    : 42124
*- Buttom Message: Yes - No 
*- Buttom No.    : 32000
IF gfModalGen('TRM42124B32000','DIALOG','this record') = 1
  lnAlias = SELECT()
  SELECT FORCAST
  =SEEK(lcStyle+STR(lnYear,4)+STR(lnWeek,2),'FORCAST')  
  DELETE
  lcStyle = SPACE(19)
  ldDate    = CTOD('01/01/'+STR(YEAR(DATE()),4))
  ldEndDate = GOMONTH(LDdATE,12) - 1
  No_Of_weeks = MAX(CEILING(((ldEndDate-DOW(ldEndDate)) - (ldDate-DOW(ldDate)+1))/7),1)
  St_Day    = ldDate-DOW(ldDate)+1 
  last_day  = ldEndDate-dow(ldEndDate)
  lnWeek    = (MAX(CEILING((DATE()-St_Day)/7),1))
  DO CASE
     CASE BETWEEN(Date(),St_Day,last_day)
       lnYear = YEAR(Date()) 
       ldDispDate = ((lnWeek-1)*7) + St_Day
     CASE Date() <  St_Day
       lnYear = YEAR(Date()) - 1
       ldDispDate = ((lnWeek-1)*7) + St_Day
     CASE Date() > last_day
       lnYear = YEAR(Date()) + 1
       lnWeek = 1
       ldDispDate = St_Day
  ENDCASE
  SHOW GET ldDispDate DISABLE
  
  FOR lnNumber  = 1 TO 8
    lcVariable  = "lnqty"+STR(lnNumber,1)
    &lcVariable = 0
    SHOW GET  &lcVariable DISABLE
  ENDFOR
  lnTotal = 0
  =lfRefresh()
  SHOW GET lcStyle ENABLE
  SHOW GET ibStyle ENABLE
  SHOW GET lnWeek  ENABLE
  SHOW GET lnYear  ENABLE
  SHOW GET pbDelete DISABLE
  SHOW GET pbSelect DISABLE
  SHOW GET pbSave   DISABLE    
  SHOW GET pbBrowse ENABLE        
  SHOW GET pbEdit   DISABLE
  lcPrompt = lcClose
  SHOW GET pbClose,1 PROMPT lcPrompt
  _CUROBJ = OBJNUM(lcStyle)
  SELECT(lnAlias)
ENDIF  

*- End Of lfvDelete.
*!*************************************************************
*! Name      : lfwWeek
*! Developer : ABDOU ELGENDI  - (ABD)
*! Date      : 08/14/2000
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

lnOldWeek = lnWeek

*- End Of lfwWeek.
*!*************************************************************
*! Name      : lfvSelect
*! Developer : ABDOU ELGENDI  - (ABD)
*! Date      : 08/14/2000
*! Purpose   : Permit the user to select new style+color
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Parameters: None.
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*
FUNCTION lfvSelect

lcStyle   = SPACE(19)
lcStyDesc = SPACE(30)
ldDate    = CTOD('01/01/'+STR(YEAR(DATE()),4))
ldEndDate = GOMONTH(LDdATE,12) - 1
No_Of_weeks = MAX(CEILING(((ldEndDate-DOW(ldEndDate)) - (ldDate-DOW(ldDate)+1))/7),1)
St_Day    = ldDate-DOW(ldDate)+1 
last_day  = ldEndDate-dow(ldEndDate)
lnWeek    = (MAX(CEILING((DATE()-St_Day)/7),1))
DO CASE
   CASE BETWEEN(Date(),St_Day,last_day)
     lnYear = YEAR(Date()) 
     ldDispDate = ((lnWeek-1)*7) + St_Day
   CASE Date() <  St_Day
     lnYear = YEAR(Date()) - 1
     ldDispDate = ((lnWeek-1)*7) + St_Day
   CASE Date() > last_day
     lnYear = YEAR(Date()) + 1
     lnWeek = 1
     ldDispDate = St_Day
ENDCASE

SHOW GET ldDispDate DISABLE
lnOldWeek = lnWeek
FOR lnNumber = 1 TO 8
  lcVariable = "lnqty"+STR(lnNumber,1)
  lcVarSize  = "lcsize"+STR(lnNumber,1)
  &lcVarSize  = 'SIZE'+STR(lnNumber,1)
  &lcVariable = 0
  SHOW GET  &lcVariable DISABLE
ENDFOR
lnTotal = 0
SHOW GET  lcStyle ENABLE
SHOW GET  ibStyle ENABLE
SHOW GET  lnWeek  ENABLE
SHOW GET  lnYear  ENABLE
SHOW GET  pbSelect DISABLE
SHOW GET  pbEdit   DISABLE
SHOW GET  pbSave   DISABLE
SHOW GET  pbBrowse ENABLE
SHOW GET  pbDelete DISABLE
_CUROBJ = OBJNUM(lcStyle)
=lfREFRESH()

*- End Of  lfvSelect.
*!*************************************************************
*! Name      : lfwYear
*! Developer : ABDOU ELGENDI  - (ABD)
*! Date      : 08/14/2000
*! Purpose   : get the old value of the Year num. in a variable
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None.
*!*************************************************************
*! Returns   : None.
*!*************************************************************
*
FUNCTION lfwYear

lnOldYear = lnYear

*- End Of lfwYear.
*!*************************************************************
*! Name      : lfClose
*! Developer : ABDOU ELGENDI  - (ABD)
*! Date      : 08/14/2000
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

= lfvClose ()

*- End Of lfClose
*!*************************************************************