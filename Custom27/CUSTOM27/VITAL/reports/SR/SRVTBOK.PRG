*:************************************************************************
*: Program file  : SRVTBOK.PRG
*: Program desc. : Custom Salesrep Booking Report for Vital (VIT10)                      
*: System        : Aria Advantage Series VER. 2.7
*: Module        : SR2 ( Sales Rep.)
*: Developer     : TMI - TAREK MOHAMED IBRAHIM
*: Date          : 04/27/2004
*: Entry #       : C119679,1 
*: Note  :   This report does not support multi currency , if this report needed to be multi currency 
*:       :   then this point needed to be worked again
*:************************************************************************
* Modifications
*:***************************************************************************
*B122973,1 TMI 05/24/2004 Enhance report speed , and fix other problems
*:***************************************************************************

*- Define Titles array
lcTime = TIME()

DIMENSION laMonth[12],laMnthOrd[12]
STORE ' ' TO m.Title1,m.Title2
laMonth = ' '
laMnthOrd = 0

*B122973,1  TMI [Start] 
DIME laStyStatus[3]
laStyStatus[1] = 'A'
laStyStatus[2] = 'H'
laStyStatus[3] = 'X'
*B122973,1  TMI [End  ] 

*- Get major lenght of the company
lnMajorLen = LEN(gfItemMask("PM"))

*B122973,1  TMI [Start] comment these lines
*-* *- Add expression for REP2 if REP1 exists in the lcRpExp
*-* lcNewExp = ' '
*-* IF "REP1" $ lcRpExp
*-*   =lfAddFxOr("ORDHDR.REP1","ORDHDR.REP2")
*-* ELSE
*-*   lcNewExp = lcRpExp
*-* ENDIF
*-* 
*-* *- Replace style.style with style.cstymajor
*-* lcNewExp = STRTRAN(lcNewExp,'STYLE.STYLE','STYLE.CSTYMAJOR')
*-* 
*-* *-Set needed relations
*-* =lfRelation()
*-* 
*-* *- Create needed temp file
*-* =lfTmpFls()
*-* 
*-* *- Collecting data
*-* IF !lfClct()
*-*   RETURN
*-* ENDIF  
*B122973,1  TMI [End  ] 

*B122973,1  TMI [Start] New way to collecting data

*-Get period data
*- Sort by options : Customers|Styles~C|S
IF lcRpSumDtl = 'S'
  lcFld1   = IIF(lcRpSortBy  = 'C' , 'ORDHDR.ACCOUNT' , 'SUBSTR(ORDLINE.STYLE,1,lnMajorLen)' )
ELSE
  lcFld1   = IIF(lcRpSortBy  = 'C' , 'ORDLINE.ACCOUNT' , 'SUBSTR(ORDLINE.STYLE,1,lnMajorLen)' )
  lcFld2   = IIF(lcRpSortBy != 'C' , 'ORDLINE.ACCOUNT' , 'SUBSTR(ORDLINE.STYLE,1,lnMajorLen)' )
ENDIF  
m.Title1 = IIF(lcRpSortBy  = 'C' , 'Account#' , 'Style#' )
m.Title2 = IIF(lcRpSortBy != 'C' , 'Account#' , 'Style#' )

*-Selct based on month fields  :  Entered Date|Start Date|Complete Date~E|S|C
lcSlcByFld = IIF(lcRpSlctBy = 'E' , 'ORDHDR.ENTERED' ,;
             IIF(lcRpSlctBy = 'S' , 'ORDHDR.START' , 'ORDHDR.COMPLETE' ))
lnPos = ASUBSCRIPT(laOgFxFlt , ASCAN(laOgFxFlt,lcSlcByFld) , 1 )
ldDtFr = CTOD(SUBSTR(laOgFxFlt[lnPos,6],1,10))
ldDtTo = CTOD(SUBSTR(laOgFxFlt[lnPos,6],12))
IF EMPTY(ldDtTo)
  *B122973,1  TMI [Start] Get description
  *lcDtDesc = LAOGFIELDH(ASCAN(LAOGFIELDN,lcSlcByFld))
  lcDtDesc = IIF('START' $ lcSlcByFld , 'Start Date', IIF('ENTERED' $ lcSlcByFld , 'Entered Date' , 'Complete Date'))
  *B122973,1  TMI [End  ] 
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'You must enter End period for &lcDtDesc.')
  RETURN .F.
ENDIF

*- Get report periods
* if the user does not entered start date , get 12 month ending with end date
IF EMPTY(ldDtFr)
  IF MONTH(ldDtTo) = 12
    ldDtFr = CTOD('01/01/'+STR(YEAR(ldDtTo),4))
  ELSE
    lcMnth = PADL(MONTH(ldDtTo)+1,2,'0')
    ldDtFr = CTOD( IIF( SET('DATE')='AMERICAN' , lcMnth+'/01/' , '01/'+lcMnth+'/' ) + STR(YEAR(ldDtTo)-1,4) )
  ENDIF
ENDIF

WAIT WINDOW NOWAIT 'Prepare monthes variables'

*- Fill title array
lnCurMnth = 0
lnOldMnth = 0
lnMnthCnt = 0
laMonth = ' '
FOR lnDay = 0 TO ldDtTo-ldDtFr
  IF MONTH(ldDtFr+lnDay) <> lnOldMnth
    lnCurMnth = MONTH(ldDtFr+lnDay)
    lnMnthCnt = lnMnthCnt + 1    
    laMonth[lnMnthCnt] = lfMnth(ldDtFr+lnDay) + '-' + RIGHT(STR(YEAR(ldDtFr+lnDay),4),2)    
    laMnthOrd[lnMnthCnt] = lnCurMnth
    lnOldMnth = lnCurMnth
  ENDIF
ENDFOR

*- Collecting data
IF llOGFltCh

  STORE '' TO lcAccExp, lcOrdExp, lcRepExp, lcStyExp, lcEntExp, lcSrtExp, lcCmpExp, lcGrpExp, ;
              lcSesExp, lcRltExp, lcCpoExp, lcPrtExp, lcStsExp, lcDivExp, lcTrmExp, lcShpExp, ;
              lcSpcExp
              
  lcEntSt = DTOS(CTOD(SUBSTR(laOGFxFlt[lnEntPos,6],1,;
                  ATC('|',laOGFxFlt[lnEntPos,6])-1)))
  lcEntEd = DTOS(CTOD(SUBSTR(laOGFxFlt[lnEntPos,6],;
                   ATC('|',laOGFxFlt[lnEntPos,6])+1)))
  llEntSides = EMPTY(ALLTRIM(lcEntSt+lcEntEd))

  lcStartSt = DTOS(CTOD(SUBSTR(laOGFxFlt[lnSrtPos,6],1,;
                  ATC('|',laOGFxFlt[lnSrtPos,6])-1)))
  lcStartEd = DTOS(CTOD(SUBSTR(laOGFxFlt[lnSrtPos,6],;
                   ATC('|',laOGFxFlt[lnSrtPos,6])+1)))
  llSrtSides = EMPTY(ALLTRIM(lcStartSt+lcStartEd))

  lcCompSt  = DTOS(CTOD(SUBSTR(laOGFxFlt[lnCmpPos,6],1,;
                   ATC('|',laOGFxFlt[lnCmpPos,6])-1)))
  lcCompEd  = DTOS(CTOD(SUBSTR(laOGFxFlt[lnCmpPos,6],;
                   ATC('|',laOGFxFlt[lnCmpPos,6])+1)))
  llCmpSides = EMPTY(ALLTRIM(lcCompSt + lcCompEd))
  
  IF !EMPTY(laOgFxFlt[lnGrpPos,6])
    lcGrpExp = "&laOgFxFlt[lnGrpPos,1]." + " $ laOgFxFlt[lnGrpPos,6]"
  ENDIF
    
  IF !EMPTY(laOgFxFlt[lnSesPos,6])
    lcSesExp = "&laOgFxFlt[lnSesPos,1]." + " $ laOgFxFlt[lnSesPos,6]"
  ENDIF
    
  IF !EMPTY(laOgFxFlt[lnRltPos,6])
    lcRltExp = "&laOgFxFlt[lnRltPos,1]." + " $ laOgFxFlt[lnRltPos,6]"
  ENDIF
    
  IF !EMPTY(laOgFxFlt[lnCpoPos,6])
    lcCpoExp = "&laOgFxFlt[lnCpoPos,1]." + " $ laOgFxFlt[lnCpoPos,6]"
  ENDIF
    
  IF !EMPTY(laOgFxFlt[lnPrtPos,6])
    lcPrtExp = "ALLT(&laOgFxFlt[lnPrtPos,1].)" + " $ laOgFxFlt[lnPrtPos,6]"
  ENDIF
    
  IF !EMPTY(laOgFxFlt[lnStsPos,6])
    lcStsExp = "&laOgFxFlt[lnStsPos,1]." + " $ laOgFxFlt[lnStsPos,6]"
  ENDIF
    
  IF !EMPTY(laOgFxFlt[lnDivPos,6])
    lcDivExp = "&laOgFxFlt[lnDivPos,1]." + " $ laOgFxFlt[lnDivPos,6]"
  ENDIF
    
  IF !EMPTY(laOgFxFlt[lnTrmPos,6])
    lcTrmExp = "&laOgFxFlt[lnTrmPos,1]." + " $ laOgFxFlt[lnTrmPos,6]"
  ENDIF
    
  IF !EMPTY(laOgFxFlt[lnShpPos,6])
    lcShpExp = "&laOgFxFlt[lnShpPos,1]." + " $ laOgFxFlt[lnShpPos,6]"
  ENDIF
    
  IF !EMPTY(laOgFxFlt[lnSpcPos,6])
    lcSpcExp = "&laOgFxFlt[lnSpcPos,1]." + " $ laOgFxFlt[lnSpcPos,6]"
  ENDIF

  IF !lfClct()
    RETURN
  ENDIF
ENDIF
WAIT CLEAR
*B122973,1  TMI [End  ]   

*- Show the report
SELECT &lcTmpClct
LOCATE
IF EOF()
  = gfModalGen("TRM00052B00000","DIALOG",'')  && 'There are no records to display' 
  RETURN
ELSE
  DO gfDispRe WITH EVAL('lcRpForm')
ENDIF  

*:**************************************************************************
*:* Name        : lfwRepWhen
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/26/2004
*:* Purpose     : When function for the report
*:***************************************************************************
*:* Called from : OG
*:***************************************************************************
*B122973,1  
FUNCTION lfwRepWhen

*- Get codes positions in 
lnAccPos = lfItmPos("ORDHDR.ACCOUNT")
lnOrdPos = lfItmPos("ORDHDR.ORDER")
lnRepPos = lfItmPos("ORDHDR.REP1")
lnStyPos = lfItmPos("STYLE.STYLE")
lnEntPos = lfItmPos("ORDHDR.ENTERED")
lnSrtPos = lfItmPos("ORDHDR.START")
lnCmpPos = lfItmPos("ORDHDR.COMPLETE")
lnGrpPos = lfItmPos("STYLE.CSTYGROUP")
lnSesPos = lfItmPos("STYLE.SEASON")
lnRltPos = lfItmPos("STYLE.ROYALTY")
lnCpoPos = lfItmPos("ORDHDR.CUSTPO")
lnPrtPos = lfItmPos("ORDHDR.PRIORITY")
lnStsPos = lfItmPos("ORDHDR.STATUS")
lnDivPos = lfItmPos("ORDHDR.CDIVISION")
lnTrmPos = lfItmPos("ORDHDR.CTERMCODE")
lnShpPos = lfItmPos("ORDHDR.SHIPVIA")
lnSpcPos = lfItmPos("ORDHDR.SPCINST")

*-- if it is the default case
IF lnOgSeting = 1
  =lfvSlcByMn()
ENDIF
*-- end of lfwRepWhen.

*!**************************************************************************
*! Name      : lfSeTOrdr
*! Developer : WAB - WALID A. WAHAB
*! Date      : 06/14/1999
*! Purpose   : Set Relation,Reset Relation, in range browse screen.
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Called from : Option Grid && Report DYE DEMOND REPORT
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =fSeTOrdr()
*!**************************************************************************
*! Note      : symbol is [S,Set- R,ReSet]
*!**************************************************************************
FUNCTION lfSeTOrdr 
PARAMETERS OpGrdParm
DO CASE
  CASE OpGrdParm = 'S'
    SELECT ORDHDR
    lcRelation = [IIF(EMPTY(Store) , 'M' + Account,'S' + Account + Store)]
    SET ORDER TO Customer IN Customer
    SET RELATION TO &lcRelation INTO CUSTOMER && To customer file.
    GO TOP
  CASE OpGrdParm = 'R'
    SELECT ORDHDR
    SET RELATION OFF INTO CUSTOMER && To customer file.
ENDCASE
*!**************************************************************************
*! Name      : lfSetAcc
*! Developer : WAB - WALID A. WAHAB
*! Date      : 06/14/1999
*! Purpose   : Set Relation,Reset Relation, in range browse screen.
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Called from : Option Grid  && Report DYE DEMOND REPORT
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfSetAcc()
*!**************************************************************************
*! Note      :  symbol is [S,Set- R,ReSet]
*!**************************************************************************
FUNCTION lfSetAcc
PARAMETERS OpGrdParm
DO CASE
  CASE OpGrdParm = 'S'  && Set code
    SELECT CUSTOMER
    SET ORDER TO TAG CUSTOMER
    GO TOP IN CUSTOMER
  CASE OpGrdParm = 'R'  && Reset code
    SELECT CUSTOMER
    SET ORDER TO 
ENDCASE
*!**************************************************************************
*! Name      : lfSeTSRep 
*! Developer : WAB - WALID A. WAHAB
*! Date      : 06/14/1999
*! Purpose   : Go top in the style IN RANGE
*!**************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfSeTSRep()
*!**************************************************************************
FUNCTION lfSeTSRep 
PARAMETERS OpGrdParm
DO CASE
  CASE OpGrdParm = 'S'
   SELECT SALESREP
   SET ORDER TO TAG  SALESREP
   GO TOP
  CASE OpGrdParm = 'R'
    SELECT SALESREP 
    SET ORDER TO 
ENDCASE

*!**************************************************************************
*! Name      : lfAddFxOr
*! Developer : WAB - WALID A. WAHAB
*! Date      : 06/10/1999
*! Purpose   : Create expretion Filter  
*!           : Devide laOgFlt into 2 array and add (.or.) inside 2nd array
*!**************************************************************************
*! Calls     : gfGenFlt()
*!**************************************************************************
*! Called from : srBokng.prg
*!**************************************************************************
*! Passed Parameters  : Exist Field / New Field
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfAddFxOr()
*!**************************************************************************
FUNCTION lfAddFxOr
PARAMETER lcExstFld,lcNewFld
PRIVATE lnFldPos,lnElmntNo,lnBrsPos,llNewRow,lnBrRow,lnBrCol

*-- We will extract the filter part concerning with REP1 and add REP2 to the filter expression
IF RAT('AND' , LEFT(lcRpExp,AT('ORDHDR.REP1',lcRpExp) )) # 0
  *-- Get the expression part before the rep1 expression
  lcTempV = SUBSTR(lcRpExp,RAT('AND' , LEFT(lcRpExp,AT('ORDHDR.REP1',lcRpExp) ))+4   )
ELSE
  lcTempV = lcRpExp
ENDIF
IF ATC(')AND',lcTempV) # 0 .OR. ATC(') AND',lcTempV) # 0
  *-- Get the exact REP1 expression
  lcTempV = LEFT(lcTempV,ATC('AND',lcTempV)-1 )
ENDIF
*-- Add an expression pretty much like REP1 but of REP2
lcExpr  = '(' + lcTempV + '.OR.' + STRTRAN(lcTempV,'ORDHDR.REP1','ORDHDR.REP2') + ') '
*-- Form the new expression
lcNewExp = STRTRAN(lcRpExp,lcTempV,lcExpr)

*:**************************************************************************
*:* Name        : lfvSlcByMn
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/27/2004
*:* Purpose     : Select by month based on 
*:***************************************************************************
FUNCTION lfvSlcByMn
PRIVATE lnPos,llChange,ldDtFr,ldDtTo,lnCurMnth,lnOldMnth,lnMnthCnt

*B122973,3  TMI [Start]* If type of date is changed and the date range is selected befor and the 
                       * period contains more than  12 monthes then empty the start date for the 
                       * selected date type
                       
lnPos = IIF(lcRpSlctBy = 'E' , lnEntPos , ;
        IIF(lcRpSlctBy = 'S' , lnSrtPos , ;
                               lnCmpPos ))
ldDtFr = CTOD(SUBSTR(laOgFxFlt[lnPos,6],1,10))
ldDtTo = CTOD(SUBSTR(laOgFxFlt[lnPos,6],12))
IF !EMPTY(ldDtFr)
  *- Check if period is more than 12 monthes 
  lnCurMnth = 0
  lnMnthCnt = 0
  FOR lnDay = 0 TO ldDtTo-ldDtFr
    IF MONTH(ldDtFr+lnDay) <> lnCurMnth
      lnCurMnth = MONTH(ldDtFr+lnDay)
      lnMnthCnt = lnMnthCnt + 1        
      IF lnMnthCnt > 12
        laOgFxFlt[lnPos,6] = '  /  /    |'+SUBSTR(laOgFxFlt[lnPos,6],12)
      ENDIF
    ENDIF
  ENDFOR
ENDIF  
*B122973,3  TMI [End  ] 

*-- end of lfvSlcByMn.

*:**************************************************************************
*:* Name        : lfsrSty
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/27/2004
*:* Purpose     : (S)et - (R)eset - (V)alid function for style browse
*:***************************************************************************
*:* Called from : OG
*:***************************************************************************
FUNCTION lfsrSty
PARAMETERS lcP
DO CASE
CASE lcP = 'S'
  SET ORDER TO CSTYLE IN STYLE
CASE lcP = 'R'
  SET ORDER TO STYLE IN STYLE
ENDCASE    
GO TOP IN STYLE

*-- end of lfsrSty.

*:**************************************************************************
*:* Name        : lfTmpFls
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/27/2004
*:* Purpose     : Create Temp files needed to collect data
*:***************************************************************************
*B122973,1  TMI [Start] rewrite this funciton 
*FUNCTION lfTmpFls
FUNCTION x_lfTmpFls
*B122973,1  TMI [End  ] 
PRIVATE laFStru,lnI
*- Create a temp file with 12 - monthes fields to collect data for styles
* The customer needs to sort either by customer of by style , so let the first two fields be with names
* fld1 and fld2, and width 19 , according to sort by selection you will save style or account in either fields
* and the key is fld1+fld2

lnI = 0

lnI = lnI + 1
DIMENSION laFStru[lnI,4]
laFStru[lnI,1] = 'FLD1'
laFStru[lnI,2] = 'C'
laFStru[lnI,3] = lnMajorLen
laFStru[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFStru[lnI,4]
laFStru[lnI,1] = 'FLD2'
laFStru[lnI,2] = 'C'
laFStru[lnI,3] = lnMajorLen
laFStru[lnI,4] = 0

*-Book qty fields distributed by monthes
lnI = lnI + 1
DIMENSION laFStru[lnI,4]
laFStru[lnI,1] = 'BOOK1'
laFStru[lnI,2] = 'N'
laFStru[lnI,3] = 8
laFStru[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFStru[lnI,4]
laFStru[lnI,1] = 'BOOK2'
laFStru[lnI,2] = 'N'
laFStru[lnI,3] = 8
laFStru[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFStru[lnI,4]
laFStru[lnI,1] = 'BOOK3'
laFStru[lnI,2] = 'N'
laFStru[lnI,3] = 8
laFStru[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFStru[lnI,4]
laFStru[lnI,1] = 'BOOK4'
laFStru[lnI,2] = 'N'
laFStru[lnI,3] = 8
laFStru[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFStru[lnI,4]
laFStru[lnI,1] = 'BOOK5'
laFStru[lnI,2] = 'N'
laFStru[lnI,3] = 8
laFStru[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFStru[lnI,4]
laFStru[lnI,1] = 'BOOK6'
laFStru[lnI,2] = 'N'
laFStru[lnI,3] = 8
laFStru[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFStru[lnI,4]
laFStru[lnI,1] = 'BOOK7'
laFStru[lnI,2] = 'N'
laFStru[lnI,3] = 8
laFStru[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFStru[lnI,4]
laFStru[lnI,1] = 'BOOK8'
laFStru[lnI,2] = 'N'
laFStru[lnI,3] = 8
laFStru[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFStru[lnI,4]
laFStru[lnI,1] = 'BOOK9'
laFStru[lnI,2] = 'N'
laFStru[lnI,3] = 8
laFStru[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFStru[lnI,4]
laFStru[lnI,1] = 'BOOK10'
laFStru[lnI,2] = 'N'
laFStru[lnI,3] = 8
laFStru[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFStru[lnI,4]
laFStru[lnI,1] = 'BOOK11'
laFStru[lnI,2] = 'N'
laFStru[lnI,3] = 8
laFStru[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFStru[lnI,4]
laFStru[lnI,1] = 'BOOK12'
laFStru[lnI,2] = 'N'
laFStru[lnI,3] = 8
laFStru[lnI,4] = 0

lnI = lnI + 1
DIMENSION laFStru[lnI,4]
laFStru[lnI,1] = 'TOTBOOK'
laFStru[lnI,2] = 'N'
laFStru[lnI,3] = 10
laFStru[lnI,4] = 0

*-Book amout fields distributed by monthes
lnI = lnI + 1
DIMENSION laFStru[lnI,4]
laFStru[lnI,1] = 'AMT1'
laFStru[lnI,2] = 'N'
laFStru[lnI,3] = 13
laFStru[lnI,4] = 2

lnI = lnI + 1
DIMENSION laFStru[lnI,4]
laFStru[lnI,1] = 'AMT2'
laFStru[lnI,2] = 'N'
laFStru[lnI,3] = 13
laFStru[lnI,4] = 2

lnI = lnI + 1
DIMENSION laFStru[lnI,4]
laFStru[lnI,1] = 'AMT3'
laFStru[lnI,2] = 'N'
laFStru[lnI,3] = 13
laFStru[lnI,4] = 2

lnI = lnI + 1
DIMENSION laFStru[lnI,4]
laFStru[lnI,1] = 'AMT4'
laFStru[lnI,2] = 'N'
laFStru[lnI,3] = 13
laFStru[lnI,4] = 2

lnI = lnI + 1
DIMENSION laFStru[lnI,4]
laFStru[lnI,1] = 'AMT5'
laFStru[lnI,2] = 'N'
laFStru[lnI,3] = 13
laFStru[lnI,4] = 2

lnI = lnI + 1
DIMENSION laFStru[lnI,4]
laFStru[lnI,1] = 'AMT6'
laFStru[lnI,2] = 'N'
laFStru[lnI,3] = 13
laFStru[lnI,4] = 2

lnI = lnI + 1
DIMENSION laFStru[lnI,4]
laFStru[lnI,1] = 'AMT7'
laFStru[lnI,2] = 'N'
laFStru[lnI,3] = 13
laFStru[lnI,4] = 2

lnI = lnI + 1
DIMENSION laFStru[lnI,4]
laFStru[lnI,1] = 'AMT8'
laFStru[lnI,2] = 'N'
laFStru[lnI,3] = 13
laFStru[lnI,4] = 2

lnI = lnI + 1
DIMENSION laFStru[lnI,4]
laFStru[lnI,1] = 'AMT9'
laFStru[lnI,2] = 'N'
laFStru[lnI,3] = 13
laFStru[lnI,4] = 2

lnI = lnI + 1
DIMENSION laFStru[lnI,4]
laFStru[lnI,1] = 'AMT10'
laFStru[lnI,2] = 'N'
laFStru[lnI,3] = 13
laFStru[lnI,4] = 2

lnI = lnI + 1
DIMENSION laFStru[lnI,4]
laFStru[lnI,1] = 'AMT11'
laFStru[lnI,2] = 'N'
laFStru[lnI,3] = 13
laFStru[lnI,4] = 2

lnI = lnI + 1
DIMENSION laFStru[lnI,4]
laFStru[lnI,1] = 'AMT12'
laFStru[lnI,2] = 'N'
laFStru[lnI,3] = 13
laFStru[lnI,4] = 2

lnI = lnI + 1
DIMENSION laFStru[lnI,4]
laFStru[lnI,1] = 'TOTAMT'
laFStru[lnI,2] = 'N'
laFStru[lnI,3] = 15
laFStru[lnI,4] = 2

CREATE TABLE (gcWorkDir+lcTmpClct) FROM ARRAY laFStru
INDEX ON FLD1+FLD2 TAG &lcTmpClct

*-- end of lfTmpFls.

*:**************************************************************************
*:* Name        : lfTmpFls
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/27/2004
*:* Purpose     : Create Temp files needed to collect data
*:***************************************************************************
FUNCTION lfTmpFls
PARAMETERS lnMnths
PRIVATE laFStru,lnI
*- Create a temp file with 12 - monthes fields to collect data for styles
* The customer needs to sort either by customer of by style , so let the first two fields be with names
* fld1 and fld2, and width 19 , according to sort by selection you will save style or account in either fields
* and the key is fld1+fld2

IF !USED(lcTmpClct)
  lnI = 0

  lnI = lnI + 1
  DIMENSION laFStru[lnI,4]
  laFStru[lnI,1] = 'FLD1'
  laFStru[lnI,2] = 'C'
  laFStru[lnI,3] = MAX(lnMajorLen,5)
  laFStru[lnI,4] = 0

  lnI = lnI + 1
  DIMENSION laFStru[lnI,4]
  laFStru[lnI,1] = 'FLD2'
  laFStru[lnI,2] = 'C'
  laFStru[lnI,3] = MAX(lnMajorLen,5)
  laFStru[lnI,4] = 0
  
  * Create all the 12 monthes due to problems in the report
  FOR lnK = 1 TO 12
    *B122973,1  TMI [End  ] 
    
    *-Book qty fields distributed by monthes
    lnI = lnI + 1  
    DIMENSION laFStru[lnI,4]
    laFStru[lnI,1] = 'BOOK'+LTRIM(STR(lnK))
    laFStru[lnI,2] = 'N'
    laFStru[lnI,3] = 8
    laFStru[lnI,4] = 0
  
    *-Book amout fields distributed by monthes
    lnI = lnI + 1
    DIMENSION laFStru[lnI,4]
    laFStru[lnI,1] = 'AMT'+LTRIM(STR(lnK))
    laFStru[lnI,2] = 'N'
    laFStru[lnI,3] = 13
    laFStru[lnI,4] = 2
  
  ENDFOR
  
  lnI = lnI + 1
  DIMENSION laFStru[lnI,4]
  laFStru[lnI,1] = 'TOTBOOK'
  laFStru[lnI,2] = 'N'
  laFStru[lnI,3] = 10
  laFStru[lnI,4] = 0
  
  lnI = lnI + 1
  DIMENSION laFStru[lnI,4]
  laFStru[lnI,1] = 'TOTAMT'
  laFStru[lnI,2] = 'N'
  laFStru[lnI,3] = 15
  laFStru[lnI,4] = 2
  
  CREATE TABLE (gcWorkDir+lcTmpClct) FROM ARRAY laFStru

ELSE
  
  SELECT &lcTmpClct
  ZAP
    
ENDIF
*- in summary case take only one field in the key 
IF lcRpSumDtl = 'D'
  INDEX ON FLD1+FLD2 TAG &lcTmpClct
ELSE
  INDEX ON FLD1 TAG &lcTmpClct
ENDIF  

*-- end of lfTmpFls.

*:**************************************************************************
*:* Name        : lfRelation
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/28/2004
*:* Purpose     : Set needed relations
*:***************************************************************************
FUNCTION lfRelation
SELECT ORDLINE
IF !'INTO ORDHDR' $ SET('RELATION')
  SET RELATION TO CORDTYPE+ORDER INTO ORDHDR ADDITIVE
ENDIF
IF !'INTO STYLE' $ SET('RELATION')
  SET RELATION TO SUBSTR(STYLE,1,lnMajorLen) INTO STYLE ADDITIVE
ENDIF
*-- end of lfRelation.

*:**************************************************************************
*:* Name        : lfvDtRng
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/02/2004
*:* Purpose     : Valid fn. for Start,Entered Or Complete date, when select 
*:*             : by range is by this date
*:***************************************************************************
*:* Called from : OG
*:***************************************************************************
*:* Parameters : Date field Field : ORDHDR.START,ORDHDR.ENTERED, ORDHDR.COMPLETE
*:***************************************************************************
*B122973,1  
FUNCTION lfvDtRng
PARAMETERS lcDtFld
PRIVATE lcSlcByFld

*-Selct based on month fields  :  Entered Date|Start Date|Complete Date~E|S|C
lcSlcByFld = IIF(lcRpSlctBy = 'E' , 'ORDHDR.ENTERED' ,;
               IIF(lcRpSlctBy = 'S' , 'ORDHDR.START' , 'ORDHDR.COMPLETE' ))
IF lcDtFld = lcSlcByFld
  IF !EMPTY(lcOgValueF) .AND. !EMPTY(lcOgValueT)
    *- Loop to get number of monthes
    lnCurMnth = 0
    lnOldMnth = 0
    lnMnthCnt = 0
    FOR lnDay = 0 TO lcOgValueT-lcOgValueF
      lnCurMnth = MONTH(lcOgValueF+lnDay)
      IF lnCurMnth<>lnOldMnth
        lnMnthCnt = lnMnthCnt + 1
        lnOldMnth = lnCurMnth
      ENDIF
      IF lnMnthCnt > 12
        WAIT WINDOW  'A period with More than 12 monthes, can not accept.'
        RETURN .F.
        EXIT
      ENDIF
    ENDFOR
  ENDIF
ENDIF

*-- end of lfvDtRng.


*:**************************************************************************
*:* Name        : lfMnth
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/03/2004
*:* Purpose     : Month name
*:***************************************************************************
*:* Called from : lfvDtRng
*:***************************************************************************
*:* Parameters : Date
*:***************************************************************************
*:* Return      : Month name in 3 characters .e.g Jan.
*:***************************************************************************
*:* Example     :  = lfMnth()
*:***************************************************************************
FUNCTION lfMnth
PARAMETERS ldDt
PRIVATE lnMnth,lcMnth
lnMnth = MONTH(ldDt)
lcMnth = ' '
DO CASE
CASE lnMnth = 1
  lcMnth = 'Jan'
CASE lnMnth = 2
  lcMnth = 'Feb'
CASE lnMnth = 3
  lcMnth = 'Mar'
CASE lnMnth = 4
  lcMnth = 'Apr'
CASE lnMnth = 5
  lcMnth = 'May'
CASE lnMnth = 6
  lcMnth = 'Jun'
CASE lnMnth = 7
  lcMnth = 'Jul'
CASE lnMnth = 8
  lcMnth = 'Aug'
CASE lnMnth = 9
  lcMnth = 'Sep'
CASE lnMnth = 10
  lcMnth = 'Oct'
CASE lnMnth = 11
  lcMnth = 'Nov'
CASE lnMnth = 12
  lcMnth = 'Dec'
ENDCASE
RETURN lcMnth
*-- end of lfMnth.

*:**************************************************************************
*:* Name        : lfvRpSumDt
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/03/2004
*:* Purpose     : Summary detail form
*:***************************************************************************
*:* Called from : OG
*:***************************************************************************
FUNCTION lfvRpSumDt
IF lcRpSumDtl = 'D'
  lcRpForm = 'SRVTBOK'
ELSE
  lcRpForm = 'SRVTBOKS'
ENDIF  
*-- end of lfvRpSumDt.


*:**************************************************************************
*:* Name        : lfClct
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/28/2004
*:* Purpose     : Collecting data
*:***************************************************************************
*B122973,1  TMI [Start] rewrite this funciton
*FUNCTION lfClct
FUNCTION x_lfClct
*B122973,1  TMI [End  ] 
PRIVATE lnPos
SET SAFETY OFF

*- Sort by options : Customers|Styles~C|S
lcFld1 = IIF(lcRpSortBy = 'C' , 'ORDLINE.ACCOUNT' , 'SUBSTR(ORDLINE.STYLE,1,lnMajorLen)' )
lcFld2 = IIF(lcRpSortBy != 'C' , 'ORDLINE.ACCOUNT' , 'SUBSTR(ORDLINE.STYLE,1,lnMajorLen)' )
m.Title1 = IIF(lcRpSortBy = 'C' , 'Account#' , 'Style#' )
m.Title2 = IIF(lcRpSortBy != 'C' , 'Account#' , 'Style#' )

*-Selct based on month fields  :  Entered Date|Start Date|Complete Date~E|S|C
lcSlcByFld = IIF(lcRpSlctBy = 'E' , 'ORDHDR.ENTERED' ,;
                IIF(lcRpSlctBy = 'S' , 'ORDHDR.START' , 'ORDHDR.COMPLETE' ))
lnPos = ASUBSCRIPT(laOgFxFlt , ASCAN(laOgFxFlt,lcSlcByFld) , 1 )
ldDtFr = CTOD(SUBSTR(laOgFxFlt[lnPos,6],1,10))
ldDtTo = CTOD(SUBSTR(laOgFxFlt[lnPos,6],12))
IF EMPTY(ldDtTo)
  lcDtDesc = LAOGFIELDH(ASCAN(LAOGFIELDN,lcSlcByFld))
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'You must enter End period for &lcDtDesc.')
  RETURN .F.
ENDIF

*- Get report periods
* if the user does not entered start date , get 12 month ending with end date
IF EMPTY(ldDtFr)
  IF MONTH(ldDtTo) = 12
    ldDtFr = CTOD('01/01/'+STR(YEAR(ldDtTo),4))
  ELSE
    lcMnth = PADL(MONTH(ldDtTo)+1,2,'0')
    ldDtFr = CTOD( IIF( SET('DATE')='AMERICAN' , lcMnth+'/01/' , '01/'+lcMnth+'/' ) + STR(YEAR(ldDtTo)-1,4) )
  ENDIF
ENDIF

    
*- Fill title array
lnCurMnth = 0
lnOldMnth = 0
lnMnthCnt = 0
laMonth = ' '
FOR lnDay = 0 TO ldDtTo-ldDtFr
  lnCurMnth = MONTH(ldDtFr+lnDay)
  IF lnCurMnth <> lnOldMnth
    lnMnthCnt = lnMnthCnt + 1    
    laMonth[lnMnthCnt] = lfMnth(ldDtFr+lnDay) + '-' + RIGHT(STR(YEAR(ldDtFr+lnDay),4),2)    
    laMnthOrd[lnMnthCnt] = lnCurMnth
    lnOldMnth = lnCurMnth
  ENDIF
ENDFOR

*- Summation loop
SELECT ORDLINE
LOCATE
SCAN FOR &lcNewExp
  IF !SEEK(PADR(&lcFld1,lnMajorLen)+IIF( lcRpSumDtl = 'D' , PADR(&lcFld2,lnMajorLen) , '' ) ,lcTmpClct)
    INSERT INTO &lcTmpClct (FLD1 , FLD2) VALUES ( &lcFld1 , IIF(lcRpSumDtl = 'D' , &lcFld2 , '') )
  ENDIF
  SELECT &lcTmpClct
  lnM = ASCAN(laMnthOrd,MONTH(&lcSlcByFld))
  lcM = LTRIM(STR(lnM))
  REPLACE BOOK&lcM WITH BOOK&lcM + ORDLINE.TOTBOOK ;
          AMT&lcM  WITH AMT&lcM  + ORDLINE.TOTBOOK*ORDLINE.PRICE ;
          TOTBOOK  WITH TOTBOOK  + ORDLINE.TOTBOOK ;
          TOTAMT   WITH TOTAMT   + ORDLINE.TOTBOOK*ORDLINE.PRICE
ENDSCAN

*-- end of lfClct.

*:**************************************************************************
*:* Name        : lfClct
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/25/2004
*:* Purpose     : Collecting data
*:***************************************************************************
*B122973,1 
FUNCTION lfClct

WAIT WINDOW NOWAIT 'Prepare temp. file and variables'
*-Create temp files
=lfTmpFls(lnMnthCnt)

SELECT ORDHDR
SET RELATION TO
SELECT ORDLINE
SET RELATION TO
SELECT STYLE
SET RELATION TO 

SET ORDER TO STYLE IN STYLE

*- Create collection data plan similar to that of Order Detail report

*-- If user select specific orders, collect data of this orders only, Same resoaning for account , styles
lcTOrdFile = laOGFxFlt[lnOrdPos,6]
llWorkDeal = !EMPTY(lcTOrdFile) AND USED(lcTOrdFile) AND RECCOUNT(lcTOrdFile) > 0

lcAccFil  = laOgFxFlt[lnAccPos,6]
llAccDeal = !EMPTY(lcAccFil) AND USED(lcAccFil) AND RECCOUNT(lcAccFil) > 0
  
lcRepFil  = laOgFxFlt[lnRepPos,6]
llRepDeal = !EMPTY(lcRepFil) AND USED(lcRepFil) AND RECCOUNT(lcRepFil) > 0
  
lcStyFil  = laOgFxFlt[lnStyPos,6]
llStyDeal = !EMPTY(lcStyFil) AND USED(lcStyFil) AND RECCOUNT(lcStyFil) > 0

IF !EMPTY(lcGrpExp+lcSesExp+lcRltExp)
  SELECT ORDLINE
  SET RELATION TO STYLE INTO STYLE
ENDIF

lcAdFld1 = IIF(lcRpSortBy = 'C','PADR(ORDHDR.ACCOUNT,lnMajorLen)','SUBSTR(ORDLINE.STYLE,1,lnMajorLen)')
lcAdFld2 = IIF(lcRpSortBy != 'C','PADR(ORDHDR.ACCOUNT,lnMajorLen)','SUBSTR(ORDLINE.STYLE,1,lnMajorLen)')
lcAdFld2 = IIF(lcRpSumDtl='S',[''],lcAdFld2)

WAIT WINDOW NOWAIT  'Start Collecting Data..'
DO CASE
CASE llWorkDeal
  SET ORDER TO ORDHDR IN ORDHDR
  SET ORDER TO ORDLINE IN ORDLINE
  SELECT &lcTOrdFile
  SCAN
    IF SEEK('O'+&lcTOrdFile..ORDER,'ORDHDR') .AND. ;
      BETWEEN(EVAL(lcSlcByFld),ldDtFr,ldDtTo) .AND. ;
      IIF(llAccDeal , SEEK(ORDHDR.ACCOUNT,lcAccFil) , .T. ) .AND. ;
      IIF(llRepDeal , SEEK(ORDHDR.REP1,lcRepFil) .OR. SEEK(ORDHDR.REP2,lcRepFil) , .T. )
    
      WAIT WINDOW NOWAIT ORDHDR.ORDER
      =lfChkOrd()
    ENDIF
  ENDSCAN
  
CASE llAccDeal
  SET ORDER TO ORDACCT IN ORDHDR
  SET ORDER TO ORDLINE IN ORDLINE
  SELECT &lcAccFil
  SCAN
    IF SEEK(&lcAccFil..ACCOUNT,'ORDHDR')
      SELECT ORDHDR
      SCAN REST WHILE ACCOUNT+CORDTYPE+ORDER = &lcAccFil..ACCOUNT ;
                  FOR BETWEEN(EVAL(lcSlcByFld),ldDtFr,ldDtTo) .AND. ;
                     IIF(llRepDeal , SEEK(ORDHDR.REP1,lcRepFil) .OR. SEEK(ORDHDR.REP2,lcRepFil) , .T. )
        WAIT WINDOW NOWAIT ORDHDR.ACCOUNT+':'+ORDHDR.ORDER
        =lfChkOrd()
      ENDSCAN
    ENDIF
  ENDSCAN

CASE llStyDeal
  SET ORDER TO ORDHDR IN ORDHDR
  SET ORDER TO ORDLINES IN ORDLINE
  SET ORDER TO STYLE IN STYLE
  SELECT ORDLINE
  SET RELATION TO 'O'+ORDER INTO ORDHDR ADDITIVE
  SELECT (lcStyFil)
  SCAN
    IF SEEK(SUBSTR(&lcStyFil..CSTYMAJOR,1,lnMajorLen),'ORDLINE')
      SELECT ORDLINE
      SCAN REST WHILE STYLE+DTOS(COMPLETE)+CORDTYPE+ORDER+STORE+STR(LINENO,6) = SUBSTR(&lcStyFil..CSTYMAJOR,1,lnMajorLen) ;
                  FOR BETWEEN(EVAL(lcSlcByFld),ldDtFr,ldDtTo) .AND. ;
                      IIF(llRepDeal , SEEK(ORDHDR.REP1,lcRepFil) .OR. SEEK(ORDHDR.REP2,lcRepFil) , .T. )
        WAIT WINDOW NOWAIT ORDLINE.STYLE+':'+ORDLINE.ORDER
        =lfChkOrd(.T.)
      ENDSCAN
    ENDIF
    
  ENDSCAN

OTHERWISE

  SET ORDER TO ORDHDR IN ORDHDR
  SET ORDER TO ORDLINE IN ORDLINE
  SELECT ORDHDR
  LOCATE 
  SCAN FOR BETWEEN(EVAL(lcSlcByFld),ldDtFr,ldDtTo) .AND. ;
           IIF(!llRepDeal,.T.,SEEK(ORDHDR.REP1,lcRepFil) .OR. SEEK(ORDHDR.REP1,lcRepFil) )
    WAIT WINDOW NOWAIT ORDHDR.ORDER
    =lfChkOrd()
  ENDSCAN
ENDCASE

*:**************************************************************************
*:* Name        : lfChkOrd
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/27/2004
*:* Purpose     : Check order 
*:***************************************************************************
*:* Called from : lfClct
*:***************************************************************************
*B122973,1 
FUNCTION lfChkOrd
PARAMETERS llIsStyDl

IF (lcRpSlctBy!='S' OR llSrtSides OR BETWEEN(DTOS(ORDHDR.START),lcStartSt,lcStartEd))  AND ;
  (lcRpSlctBy!='C' OR llCmpSides OR BETWEEN(DTOS(ORDHDR.COMPLETE),lcCompSt,lcCompEd)) AND ;
  (lcRpSlctBy!='E' OR llEntSides OR BETWEEN(DTOS(ORDHDR.ENTERED),lcEntSt,lcEntEd))    AND ;
  IIF(EMPTY(lcCpoExp),.T., EVALUATE(lcCpoExp)) AND ; 
  IIF(EMPTY(lcPrtExp),.T., EVALUATE(lcPrtExp)) AND ; 
  IIF(EMPTY(lcStsExp),.T., EVALUATE(lcStsExp)) AND ; 
  IIF(EMPTY(lcTrmExp),.T., EVALUATE(lcTrmExp)) AND ; 
  IIF(EMPTY(lcShpExp),.T., EVALUATE(lcShpExp)) AND ; 
  IIF(EMPTY(lcSpcExp),.T., EVALUATE(lcSpcExp)) AND ;
  IIF(EMPTY(lcDivExp),.T., EVALUATE(lcDivExp))

  IF lcRpSumDtl = 'S' .AND. lcRpSortBy = 'C' .AND. ;
     EMPTY(lcGrpExp+lcSesExp+lcRltExp) .AND. !llStyDeal
    IF !SEEK(&lcAdFld1,lcTmpClct)
      INSERT INTO &lcTmpClct (FLD1) VALUES ( &lcAdFld1 )
    ENDIF
    SELECT &lcTmpClct
    lcM = LTRIM(STR( ASCAN(laMnthOrd,MONTH( EVAL(lcSlcByFld) )) ))
    REPLACE BOOK&lcM WITH BOOK&lcM + ORDHDR.BOOK     ;
            AMT&lcM  WITH AMT&lcM  + ORDHDR.BOOKAMT  ;
            TOTBOOK  WITH TOTBOOK  + ORDHDR.BOOK     ;
            TOTAMT   WITH TOTAMT   + ORDHDR.BOOKAMT
  ELSE

    IF llIsStyDl

      IF IIF(EMPTY(lcGrpExp),.T., EVALUATE(lcGrpExp)) AND ;
          IIF(EMPTY(lcSesExp),.T., EVALUATE(lcSesExp)) AND ;
          IIF(EMPTY(lcRltExp),.T., EVALUATE(lcRltExp))
        IF !SEEK(&lcAdFld1+&lcAdFld2,lcTmpClct)
          INSERT INTO &lcTmpClct (FLD1,FLD2) VALUES ( &lcAdFld1 , &lcAdFld2 )
        ENDIF
        SELECT &lcTmpClct
        lcM = LTRIM(STR( ASCAN(laMnthOrd,MONTH( EVAL(lcSlcByFld) )) ))
        REPLACE BOOK&lcM WITH BOOK&lcM + ORDLINE.TOTBOOK ;
                AMT&lcM  WITH AMT&lcM  + ORDLINE.TOTBOOK*ORDLINE.PRICE ;
                TOTBOOK  WITH TOTBOOK  + ORDLINE.TOTBOOK ;
                TOTAMT   WITH TOTAMT   + ORDLINE.TOTBOOK*ORDLINE.PRICE
      ENDIF
            
    ELSE
          
      IF SEEK('O'+ORDHDR.ORDER,'ORDLINE')
        SELECT ORDLINE
        SCAN REST WHILE CORDTYPE+ORDER = 'O'+ORDHDR.ORDER ;
                    FOR IIF(EMPTY(lcGrpExp),.T., EVALUATE(lcGrpExp)) AND ;
                        IIF(EMPTY(lcSesExp),.T., EVALUATE(lcSesExp)) AND ;
                        IIF(EMPTY(lcRltExp),.T., EVALUATE(lcRltExp)) AND ; 
                        (!llStyDeal .OR. SEEK(ORDLINE.STYLE,lcStyFil)) 
          IF !SEEK(&lcAdFld1+&lcAdFld2,lcTmpClct)
            INSERT INTO &lcTmpClct (FLD1,FLD2) VALUES ( &lcAdFld1 , &lcAdFld2 )
          ENDIF
          SELECT &lcTmpClct
          lcM = LTRIM(STR( ASCAN(laMnthOrd,MONTH( EVAL(lcSlcByFld) )) ))
          REPLACE BOOK&lcM WITH BOOK&lcM + ORDLINE.TOTBOOK ;
                  AMT&lcM  WITH AMT&lcM  + ORDLINE.TOTBOOK*ORDLINE.PRICE ;
                  TOTBOOK  WITH TOTBOOK  + ORDLINE.TOTBOOK ;
                  TOTAMT   WITH TOTAMT   + ORDLINE.TOTBOOK*ORDLINE.PRICE
        ENDSCAN          
      ENDIF  
          
    ENDIF
  ENDIF          
ENDIF
*-- end of lfChkOrd

*!*************************************************************
*! Name      : lfItmPos
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/20/98
*! Purpose   : Evaluate fixed filter position within array.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*! Returns            : Position
*!*************************************************************
*! Example   : = lfItmPos()
*!*************************************************************
*B122973,1  
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos

*-- end of lfItmPos