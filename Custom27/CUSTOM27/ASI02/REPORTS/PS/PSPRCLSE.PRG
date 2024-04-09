*:****************************************************************************************
*: Program file  : PSPRCLSE.PRG
*: Program desc. : Performance By Class Or Season. [ FOR EAGLE'S EYE ]
*: System        : Aria Apparel System (A27).
*: Module        : Point Of Sale.  (PS)
*: Developer     : ABDOU ELGENDI - (ABD) Due to C#102204,1
*: Date          : 03/12/2001
*:****************************************************************************************
*: Calls 
*:               : FUNCTIONS  : lfCollect , lfCrTmpFls , lfwRepWhen , lfvEtrDate
*:               :            : lfOldValue, lfvClrRead .
*:               ---------------------------------------------------------------
*:               : PROCEDURE  : 
*:****************************************************************************************
*: Passed Parameters  : None
*:****************************************************************************************
*:C#102204,1.
*:****************************************************************************************
*:Modifications :
*:C#102204,4 ABD 03/22/2001 Fix some problem didn't cover by specis.
*:****************************************************************************************
*-- This report will print activity for styles that are 
*-- only sold at retail.

*-- Declaration variables.
*-- lcRpPrtCS  :- Variable Hold 'C' For Class 'S' For season
*-- ldRpFDay   :- Variable Hold the Selected date in the Option Grid.
*-- ldBegWeek  :- Variable Hold the Begnning of the week date.
*-- ldBegMonth :- Variable hold the Begnning of the month date.
*-- lnTotWtdSl :- Variable Hold the total of sum of week to date sales.
*-- lnTotWtdMD :- Variable Hold the total Week to Date Mardowns (sum of styles level Discount)
*--            :- Plus Additional discount if chosen in selection critria.
*-- lnTotMtdSl :- Variable Hold the total of sum of month to date sales.
*-- lnTotMtdMD :- Variable Hold the total of sum of Month to date sales.
*-- lcOldStyle :- Variable hold the old style.
*-- lnTotUntOH :- Variable hold the number of units on hand from the styles.
*-- lnTotPrcOH :- Variable hold the total proce for on hand.
*-- lnTtMtdQty :- Variable hold the total number of units sold month to date


STORE {} TO ldBegWeek , ldBegMonth
lcOldStyle = ''
*-abdou
STORE 0 TO lnTotAdMd ,lnTotdMd
*-abdou
*-- END Declaration variables.


*-- Get the beginning of the week.
ldBegWeek  = ldRpFDay - (DOW(ldRpFDay) - 1)
ldBegMonth = ldRpFDay - (DAY(ldRpFDay) - 1)
*-- if user change filter criteria then you must collect data again [Begin]
IF llOGFltCh
  *-- Function to Create the temp file.
  = lfCrTmpFls ()
  *-- Function to Collecte data in temp file.
  = lfCollect  ()
ENDIF

SELECT (lcTempFil)
GOTO TOP
DO gfDispRe WITH EVAL('lcRpForm')

RETURN
*-- Function Section.
*:****************************************************************
*: Name        : lfCollect
*: Developer   : ABDOU ELGENDI - (ABD)
*: Date        : 03/12/2001
*: Purpose     : Function to collect data
*:****************************************************************
*: Called from : Program.
*:****************************************************************
*: Calls       : None.
*:****************************************************************
*: Passed Parameters : None
*:****************************************************************
*: Return      : None
*:****************************************************************
*: Example     : = lfCollect()
*:****************************************************************
*
FUNCTION lfCollect
PRIVATE  lcPerForm 
STORE '' TO lcPerForm
lcPerForm = IIF(lcRpPrtCS = 'S' , '.Season' ,'.CstyGroup' )

SELECT POSTRAN
SET RELATION TO STYLE INTO STYLE ADDITIVE
SET RELATION TO Postran.tran INTO INVHDR ADDITIVE


*C#102204,4 ABD - Add return to scan expr. the we was get the sales only not sales
*C#102204,4 ABD - & return. [Begin]
*SCAN FOR &lcRpExp .AND. Month(ldRpFDay) = Month(Postran.Trandate)
SCAN FOR  Month(ldRpFDay) = Month(Postran.Trandate) .AND. ;
         Postran.Trantype $ '10' .AND. INVHDR.STATUS # 'V'
  *C#102204,4 ABD - [End]
  *-- Get the Week to date data.
  *C#102204,4 ABD - Cheak for selected group in the Option Grid [Begin]
  IF &lcRpExp
    *C#102204,4 ABD - [End]
    IF BETWEEN(Postran.Trandate,ldBegWeek,ldRpFDay)
      SELECT (lcTempFil)
      IF !SEEK(Style&lcPerForm)
        APPEND BLANK
        REPLACE Season WITH STYLE&lcPerForm
      ENDIF
    REPLACE nWTDSals WITH nWTDSals + Postran.nAmount
    *C#102204,4 ABD - Remark the next line. that we will collect for all season. [Begin]
    *-- Totat Week to data sales
    *lnTotWtdSl = lnTotWtdSl + Postran.nAmount
    *C#102204,4 ABD - [End]
    *-- Week to Date Mardowns (sum of styles level Discount)
    *-- Plus Additional discount if chosen in selection critria.
    IF Postran.disc_amt <> 0 .OR. Postran.discount <> 0
      *C#102204,4 ABD - Add the discound if type of transaction is sales accu. the amount
      *C#102204,4 ABD - Else sup. the ammount from the total discount. [Begin]
      
      STORE 0 To lnTotAdMd
      *REPLACE nWTDMardwn WITH nWTDMardwn + Postran.disc_amt + IIF(llRpPrtAd,Postran.discount,0)
       *-- lnTotdMd :- Hold the normal discount that come with style.
       
       lnTotdMd = nWTDMardwn + IIF(Postran.cType = 'S',Postran.disc_amt,-(Postran.disc_amt))

       *-- If inculoud the add. discount acc. it
       *-- lnTotAdMd  :- Hold add discount.
       lnTotAdMd = IIF(llRpPrtAd,IIF(Postran.cType = 'S',Postran.discount,-(Postran.discount)),0)
       lnTotdMd = lnTotdMd + lnTotAdMd
       
       REPLACE nWTDMardwn WITH lnTotdMd

      *-- Get slaes discount or sup. return discount.
      *lnTotWtdMD = lnTotWtdMD + Postran.disc_amt + IIF(llRpPrtAd,Postran.discount,0)
      IF Postran.cType = 'S'
        lnTotWtdMD = lnTotWtdMD + Postran.disc_amt + IIF(llRpPrtAd,Postran.discount,0)
      ELSE
        lnTotWtdMD = lnTotWtdMD - (Postran.disc_amt + IIF(llRpPrtAd,Postran.discount,0))
      ENDIF
      *C#102204,4 ABD - [End]
    ENDIF
    SELECT POSTRAN
    ENDIF
    
    *-- Get the Month to date data.
    IF BETWEEN(Postran.Trandate,ldBegMonth,ldRpFDay)
      SELECT (lcTempFil)
      IF !SEEK(Style&lcPerForm)
        APPEND BLANK
        REPLACE Season WITH STYLE&lcPerForm
      ENDIF
      REPLACE nMTDSals  WITH nMTDSals + Postran.nAmount
      
      *C#102204,4 ABD - Remark the next line. that we will collect for all season. [Begin]
      *-- Total amount of sold units month to date.
      *lnTotMtdSl = lnTotMtdSl + Postran.nAmount
      *C#102204,4 ABD - [End]
      
      *-- Total number of units sold Month to date.
      lnTtMtdQty = lnTtMtdQty + Postran.TotQty
      
      *-- Month to Date Mardowns (sum of styles level Discount)
      *-- Plus Additional discount if chosen in selection critria.
      IF Postran.disc_amt <> 0  .OR. Postran.discount <> 0
      
      *C#102204,4 ABD - Add the discound if type of transaction is sales accu. the amount
      *C#102204,4 ABD - Else sup. the ammount from the total discount. [Begin]
      
      STORE 0 To lnTotAdMd
      *REPLACE nMTDMardwn WITH nMTDMardwn + Postran.disc_amt + IIF(llRpPrtAd,Postran.discount,0)
      *-- lnTotdMd :- Hold the normal discount that come with style.
       
       lnTotdMd = nMTDMardwn + IIF(Postran.cType = 'S',Postran.disc_amt,-(Postran.disc_amt))

       *-- If inculoud the add. discount acc. it
       *-- lnTotAdMd  :- Hold add discount.
       lnTotAdMd = IIF(llRpPrtAd,IIF(Postran.cType = 'S',Postran.discount,-(Postran.discount)),0)
       lnTotdMd = lnTotdMd + lnTotAdMd
       
       REPLACE nMTDMardwn WITH lnTotdMd
      *C#102204,4 ABD - [End]


      *-- Get slaes discount or sup. return discount.
      *lnTotMtdMD = lnTotMtdMD + Postran.disc_amt + IIF(llRpPrtAd,Postran.discount,0)
      IF Postran.cType = 'S'
        lnTotMtdMD = lnTotMtdMD + Postran.disc_amt + IIF(llRpPrtAd,Postran.discount,0)
      ELSE
        lnTotMtdMD = lnTotMtdMD - (Postran.disc_amt + IIF(llRpPrtAd,Postran.discount,0))
      ENDIF
      *C#102204,4 ABD - [End]
      
      
      ENDIF

  
      *C#102204,4 ABD - Remark the next line. that we will collect for all season. [Begin]
      *IF lcOldStyle # Style.Style
      *  lcOldStyle = Style.Style
      *  REPLACE nUntOnHand WITH nUntOnHand + Style.Totstk ,;
      *          nInvOH     WITH nInvOH + (Style.Totstk * Style.nsugretpri)
      *          
      *  lnTotUntOH = lnTotUntOH + Style.Totstk
      *  lnTotPrcOH = lnTotPrcOH + (Style.Totstk * Style.nsugretpri)
      *ENDIF
      *C#102204,4 ABD - [End]
      SELECT POSTRAN
    ENDIF
  ENDIF

  *C#102204,4 ABD - We will collect for all season. [Begin]
  *-- We will scan on the postran again to get the total sales for
  *-- this week or month for all seaseon or style group.
  *-- Get the total sales for this week.
  IF BETWEEN(Postran.Trandate,ldBegWeek,ldRpFDay)
    lnTotWtdSl = lnTotWtdSl + Postran.nAmount
  ENDIF
  
  *-- Get the total sales for this Month.
  IF BETWEEN(Postran.Trandate,ldBegMonth,ldRpFDay)
    lnTotMtdSl = lnTotMtdSl + Postran.nAmount
  ENDIF
  *C#102204,4 ABD - [End]

ENDSCAN

*C#102204,4 ABD - We will collect for all season Or group. [Begin]
lcSeekExp = IIF(lcRpPrtCS = 'S' , 'Season' ,'CstyGroup' )
SELECT STYLE
SCAN 
  *-- Get the on hand for selected group or seaseon.
  IF SEEK(&lcSeekExp,lcTempFil)
    SELECT (lcTempFil)
    REPLACE nUntOnHand WITH nUntOnHand + Style.Totstk ,;
            nInvOH     WITH nInvOH + (Style.Totstk * Style.nsugretpri)
    SELECT STYLE
  ENDIF
  lnTotUntOH = lnTotUntOH + Style.Totstk
  lnTotPrcOH = lnTotPrcOH + (Style.Totstk * Style.nsugretpri)
ENDSCAN
*C#102204,4 ABD - [End]

*-- Remove the relation
SELECT POSTRAN
SET RELATION TO

*-- End Of lfCollect.
*:****************************************************************
*: Name        : lfCrTmpFls
*: Developer   : ABDOU ELGENDI - (ABD)
*: Date        : 03/12/2001
*: Purpose     : Function to Create temp files.
*:****************************************************************
*: Called from : Program.
*:****************************************************************
*: Calls       : None.
*:****************************************************************
*: Passed Parameters : None
*:****************************************************************
*: Return      : None
*:****************************************************************
*: Example     : = lfCrTmpFls ()
*:****************************************************************
*
FUNCTION lfCrTmpFls
PRIVATE lnOldAls
STORE 0 To lnTotWtdSl , lnTotWtdMD , lnTotMtdSl ,;
           lnTotMtdMD , lnTotUntOH , lnTotPrcOH ,;
           lnTtMtdQty , lnTotAdMd  , lnTotdMd

lnOldAls = SELECT(0)

DIMENSION laTrnTmp[7 , 4]

*-- Season or class
laTrnTmp [ 1 , 1 ] = 'Season'
laTrnTmp [ 1 , 2 ] = 'C'
laTrnTmp [ 1 , 3 ] = 6
laTrnTmp [ 1 , 4 ] = 0

*-- Sum of week to date sales of styles with above season or class code in dollars.
laTrnTmp [ 2 , 1 ] = 'nWTDSals'
laTrnTmp [ 2 , 2 ] = 'N'
laTrnTmp [ 2 , 3 ] = 8
laTrnTmp [ 2 , 4 ] = 2

*-- Week to Date Mardowns (sum of styles level Discount)
*-- Plus Additional discount if chosen in selection critria.
laTrnTmp [ 3 , 1 ] = 'nWTDMardwn'
laTrnTmp [ 3 , 2 ] = 'N'
laTrnTmp [ 3 , 3 ] = 8
laTrnTmp [ 3 , 4 ] = 2

*-- Sum Month to date sales of styles with above season code or class
laTrnTmp [ 4 , 1 ] = 'nMTDSals'
laTrnTmp [ 4 , 2 ] = 'N'
laTrnTmp [ 4 , 3 ] = 8
laTrnTmp [ 4 , 4 ] = 2

*-- Week to Date Mardowns (sum of styles level Discount)
*-- Plus Additional discount if chosen in selection critria.
laTrnTmp [ 5 , 1 ] = 'nMTDMardwn'
laTrnTmp [ 5 , 2 ] = 'N'
laTrnTmp [ 5 , 3 ] = 8
laTrnTmp [ 5 , 4 ] = 2

*-- Number of units on hand in styles.
laTrnTmp [ 6 , 1 ] = 'nUntOnHand'
laTrnTmp [ 6 , 2 ] = 'N'
laTrnTmp [ 6 , 3 ] = 8
laTrnTmp [ 6 , 4 ] = 0

*-- Units * Retail Price
laTrnTmp [ 7 , 1 ] = 'nInvOH'
laTrnTmp [ 7 , 2 ] = 'N'
laTrnTmp [ 7 , 3 ] = 8
laTrnTmp [ 7 , 4 ] = 0


CREATE DBF (gcWorkDir+lcTempFil) FROM ARRAY laTrnTmp
=gfOpenFile(gcWorkDir+lcTempFil,'','EX')

*-- Make Index on Style Group or Seaseon will be the same field.
INDEX ON Season TAG lcTempFil

SELECT(lnOldAls)
*-- End Of lfCrTmpFls.
*:*************************************************************
*: Name      : lfwRepWhen
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 03/12/2001
*: Purpose   : Option Grid When function
*:*************************************************************
*: Called from : Option Grid
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None.
*:*************************************************************
*: Return      : None.
*:*************************************************************
*: Example     : = lfwRepWhen()
*:*************************************************************
*
FUNCTION lfwRepWhen
PRIVATE lnAlias

lnAlias = SELECT (0)

*-- Make new index on postran on style field to help.
SELECT POSTRAN
INDEX ON STYLE TAG (lcPostRan) OF (gcWorkDir+lcPostRan)

SELECT (lnAlias)

*-- End OF lfwRepWhen.
*:****************************************************************
*: Name      : lfvEtrDate
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 03/12/2001
*: Purpose   : Cheak if date is empty.
*:****************************************************************
*: Called from : Option Grid.
*:****************************************************************
*: Calls       : None.
*:****************************************************************
*: Passed Parameters : None
*:****************************************************************
*: Return      : None
*:****************************************************************
*: Example     : = lfvEtrDate()
*:****************************************************************
*
FUNCTION lfvEtrDate

*-- If the entered date empty give user massage till him that.
*-- & don't leave date empty.

IF EMPTY(ldRpFDay)
  *-- Message No. : 04074
  *-- Message Text: Your date cannot be empty.
  *-- Button No.  : 00000
  *--             : Ok
  =gfModalGen('TRM04074B00000','DIALOG','Your date')
  ldRpFDay = lcOldValue 
ELSE
  IF  lcOldValue <> ldRpFDay
    llOGFltCh = .T.
  ENDIF
ENDIF

*-- End OF lfvEtrDate.
*:*************************************************************
*: Name      : lfOldValue
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 03/12/2001
*: Purpose   : When function to save old value.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns            : None.
*:****************************************************************
*: Example     : = lfOldvalue ()
*:****************************************************************
*
FUNCTION lfOldvalue

*-- Variable to hold the old value.
lcOldValue = EVALUATE(SYS(18))

*-- End Of lfoldvalue
*:*************************************************************
*: Name      : lfvClrRead
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 03/12/2001
*: Purpose   : Clear read the Option Grid.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns            : None.
*:****************************************************************
*: Example     : = lfvClrRead()
*:****************************************************************
*
FUNCTION lfvClrRead

*-- Clear read to  refresh the Option grid.
CLEAR READ

*-- End Of lfvClrRead
*:****************************************************************