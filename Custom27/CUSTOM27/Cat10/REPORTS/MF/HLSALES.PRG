*:****************************************************************
*: Program file  : HLSALES.PRG
*: Program desc. : Estimated Constructed Value Statement.
*: System        : Aria Apparel System (A27).
*: Module        : H.L.C. System. (HL)
*: Developer     : ABDOU ELGENDI - (ABD) Due to C#200246,1
*: Date          : 01/10/2002
*:****************************************************************
*: Calls 
*:               : FUNCTIONS  : lfwRepWhen , lfvStyNo , lfCreatemp
*:               :            : lfCollect	  
*:               -----------------------------------------------
*:               : PROCEDURE  : None.
*:****************************************************************
*: Passed Parameters  : None.
*:****************************************************************
*:C#200246,1.
*:****************************************************************
*:Modifications  :
*:B605874,1 ABD 04/17/2002 fix bug that the 'E=TRIM' not Printed.
*:****************************************************************
*:
*-- Begin Declaration variables.
*-- lcTemp    :- Variable Hold the temp file that collect the data.
*-- lcRpDate  :- Variable Hold the Date.
*-- End Declaration variables.



DIMENSION laCompAdd[2]
STORE '' TO laCompAdd
SELECT SYCCOMP
SEEK gcAct_Comp

laCompAdd[1] = gfGetAdr('SYCCOMP' , '' , '' , '' , 1)
laCompAdd[2] = gfGetAdr('SYCCOMP' , '' , '' , '' , 2)

lcComPhon = cCom_Phon
lcPhone   = SUBSTR(lcComPhon,1,3)+'-'+SUBSTR(lcComPhon,4,3)+'-'+SUBSTR(lcComPhon,7,20)

lcComFax  = cCom_Fax
lcFax     = SUBSTR(lcComFax,1,3)+'-'+SUBSTR(lcComFax,4,3)+'-'+SUBSTR(lcComFax,7,20)


*- Message Text   :- You must enter a style number for the report to be displayed.
*- Message No.    :- 000000.
*- Buttom Message :- Ok
*- Buttom Number  :- 00000.
IF EMPTY(ALLTRIM(lcRpStyNo) )
  = gfModalGen('INM00000B00000','F','ALERT',' ','You must enter a style number for the report to be displayed.')
  RETURN
ENDIF

*-- if user change filter criteria then you must collect data again [Begin]
IF llOGFltCh
  STORE 0 To lnTot807 , lnTotDuti , lnTot800
  *-- Function To Create temp File
  = lfCreatemp()
  *-- Collecting Data In The First Time.
  = lfCollect ()
ENDIF

SELECT (lcTemp)
LOCATE
DO gfDispRe WITH EVAL('lcRpForm')

RETURN
*-- End of program code.
*:*************************************************************
*: Name      : lfwRepWhen
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/09/2002
*: Purpose   : When function of OG
*:*************************************************************
*: Called from : OG read cycle
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfwRepWhen()
*:*************************************************************
*
FUNCTION lfwRepWhen

IF EMPTY(lcRpDate)
  lcSavCent= SET('CENTURY')
  SET CENTURY ON
  lcRpDate = MDY(DATE())
  SET CENTURY &lcSavCent
ENDIF

*-- End of lfwRepWhen.
*:*************************************************************
*: Name      : lfvStyNo
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/09/2002
*: Purpose   : Browse all styles.
*:*************************************************************
*: Called from : OG read cycle
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfvStyNo ()
*:*************************************************************
*
FUNCTION lfvStyNo
PRIVATE lnAlias

IF MDOWN()
  RETURN
ENDIF

lnAlias = SELECT (0)

IF !EMPTY(lcRpStyNo) .AND. LASTKEY() = 13
  SELECT HLCSTYLE
  IF !SEEK (lcRpStyNo)
    DECLARE  laBrowArr[1]
    laBrowArr = ''

    lcBrFields = [cstynum:H="Style number ", cstyseacd:H="Style season code", cstyrang:H="Style range",cstrangds:H="Range description"]
    lcRpStyNo  = IIF(ARIABROW('',"Styles",gnBrFSRow1, gnBrFSCol1,;
                      gnBrFSRow2, gnBrFSCol2,'','','cStyNum','laBrowArr'),;
                      hlcStyle.cStyNum,SPACE(07))

  ENDIF
  lcRpDesrpt = ALLTRIM(gfCodDes(ctsuchwcd,'CTSUCHWCD')) + ' ' + ;
               ALLTRIM(gfCodDes(CTSUCONCD,'CTSUCONCD')) + ' ' + ;
               ALLTRIM(gfCodDes(CTSUGARCD,'CTSUGARCD'))
  

  CLEAR READ
ENDIF

SELECT(lnAlias)

*-- End OF lfvStyNo
*:**************************************************************************
*: Name      : lfCreatemp
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/09/2002
*: Purpose   : Create the temp files.
*:**************************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:**************************************************************************
*: Called from : Report code
*:**************************************************************************
*: Passed Parameters  : ...
*:**************************************************************************
*: Returns            : None.
*:**************************************************************************
*: Example   : = lfCreatemp()
*:**************************************************************************
*
FUNCTION lfCreatemp

DIMENSION laTempStru[1,4]
laTempStru = ''
SELECT HLCSTYLE
lnFildLen = AFIELDS(laTempStru)

*-- Add Hlcvslin fields.
*B605874,1 ABD Add new field cFabric to hold the fabric name. [Begin]
*DIMENSION laTempStru[lnFildLen + 12, 4]
DIMENSION laTempStru[lnFildLen + 13, 4]
*B605874,1 ABD [End]

laTempStru[lnFildLen + 1, 1] = 'ccvsctdis'
laTempStru[lnFildLen + 1, 2] = 'C'
laTempStru[lnFildLen + 1, 3] = 30
laTempStru[lnFildLen + 1, 4] = 0


laTempStru[lnFildLen + 2, 1] = 'ccvlitmds'
laTempStru[lnFildLen + 2, 2] = 'C'
laTempStru[lnFildLen + 2, 3] = 30
laTempStru[lnFildLen + 2, 4] = 0

laTempStru[lnFildLen + 3, 1] = 'ncvlitmqt'
laTempStru[lnFildLen + 3, 2] = 'N'
laTempStru[lnFildLen + 3, 3] = 10
laTempStru[lnFildLen + 3, 4] = 3

laTempStru[lnFildLen + 4, 1] = 'ccvlitmuo'
laTempStru[lnFildLen + 4, 2] = 'C'
laTempStru[lnFildLen + 4, 3] = 6
laTempStru[lnFildLen + 4, 4] = 0

laTempStru[lnFildLen + 5, 1] = 'ncvlitmpr'
laTempStru[lnFildLen + 5, 2] = 'N'
laTempStru[lnFildLen + 5, 3] = 8
laTempStru[lnFildLen + 5, 4] = 3

laTempStru[lnFildLen + 6, 1] = 'ccvlitmpu'
laTempStru[lnFildLen + 6, 2] = 'C'
laTempStru[lnFildLen + 6, 3] = 6
laTempStru[lnFildLen + 6, 4] = 0

laTempStru[lnFildLen + 7, 1] = 'ccvsdefty'
laTempStru[lnFildLen + 7, 2] = 'C'
laTempStru[lnFildLen + 7, 3] = 1
laTempStru[lnFildLen + 7, 4] = 0

laTempStru[lnFildLen + 8, 1] = 'ncvlitmex'
laTempStru[lnFildLen + 8, 2] = 'N'
laTempStru[lnFildLen + 8, 3] = 10
laTempStru[lnFildLen + 8, 4] = 3

laTempStru[lnFildLen + 9, 1] = 'ccvstype'
laTempStru[lnFildLen + 9, 2] = 'C'
laTempStru[lnFildLen + 9, 3] = 1
laTempStru[lnFildLen + 9, 4] = 0

laTempStru[lnFildLen + 10, 1] = 'ccvllinno'
laTempStru[lnFildLen + 10, 2] = 'C'
laTempStru[lnFildLen + 10, 3] = 2
laTempStru[lnFildLen + 10, 4] = 0

laTempStru[lnFildLen + 11, 1] = 'ncvlconv'
laTempStru[lnFildLen + 11, 2] = 'N'
laTempStru[lnFildLen + 11, 3] = 8
laTempStru[lnFildLen + 11, 4] = 3

laTempStru[lnFildLen + 12, 1] = 'ccvlitmso'
laTempStru[lnFildLen + 12, 2] = 'C'
laTempStru[lnFildLen + 12, 3] = 30
laTempStru[lnFildLen + 12, 4] = 0

*B605874,1 ABD Add new Field to hold the fabric. [Begin]
laTempStru[lnFildLen + 13, 1] = 'cFabric'
laTempStru[lnFildLen + 13, 2] = 'C'
laTempStru[lnFildLen + 13, 3] = 7
laTempStru[lnFildLen + 13, 4] = 0
*B605874,1 ABD [End]

CREATE Table (lcTemp) FROM ARRAY laTempStru
SELECT (lcTemp)
ZAP
INDEX ON ccvllinno TAG (lcTemp) OF (gcWorkDir+lcTemp+'.CDX')


*-- End OF lfCreatemp
*:*************************************************************
*: Name      : lfCollect
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/09/2002
*: Purpose   : Collecting Data In The First Time.
*:*************************************************************
*: Called from : OG read cycle
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfCollect ()
*:*************************************************************
*
FUNCTION lfCollect
PRIVATE llFound
llFound = .T.

SELECT HLCSTYLE
= SEEK(lcRpStyNo)
SCATTER MEMVAR MEMO
SELECT Hlcvslin
= SEEK(lcRpStyNo)
SCAN REST WHILE cStynum + cCvllinno = ALLTRIM(lcRpStyNo)
  
  INSERT INTO (lcTemp) FROM MEMVAR
  *-- The Image name and Directory.
  SELECT (lcTemp)
 *B605874,1 ABD Update the cfabric Field. [Begin]
  *REPLACE ccvsctdis  WITH Hlcvslin.ccvsctdis ,;
          ccvlitmds  WITH Hlcvslin.ccvlitmds ,;
          ncvlitmqt  WITH Hlcvslin.ncvlitmqt ,;
          ccvlitmuo  WITH Hlcvslin.ccvlitmuo ,;
          ncvlitmpr  WITH Hlcvslin.ncvlitmpr ,;
          ccvlitmpu  WITH Hlcvslin.ccvlitmpu ,;
          ccvsdefty  WITH Hlcvslin.ccvsdefty ,;
          ncvlitmex  WITH Hlcvslin.ncvlitmex ,;
          ccvstype   WITH Hlcvslin.ccvstype  ,;
          ccvllinno  WITH Hlcvslin.ccvllinno ,;
          ncvlconv   WITH Hlcvslin.ncvlconv  ,; 
          ccvlitmso  WITH Hlcvslin.ccvlitmso


  REPLACE ccvsctdis  WITH Hlcvslin.ccvsctdis ,;
          ccvlitmds  WITH Hlcvslin.ccvlitmds ,;
          ncvlitmqt  WITH Hlcvslin.ncvlitmqt ,;
          ccvlitmuo  WITH Hlcvslin.ccvlitmuo ,;
          ncvlitmpr  WITH Hlcvslin.ncvlitmpr ,;
          ccvlitmpu  WITH Hlcvslin.ccvlitmpu ,;
          ccvsdefty  WITH Hlcvslin.ccvsdefty ,;
          ncvlitmex  WITH Hlcvslin.ncvlitmex ,;
          ccvstype   WITH Hlcvslin.ccvstype  ,;
          ccvllinno  WITH Hlcvslin.ccvllinno ,;
          ncvlconv   WITH Hlcvslin.ncvlconv  ,; 
          ccvlitmso  WITH Hlcvslin.ccvlitmso ,;
          cFabric    WITH Hlcvslin.cFabric
  IF cCvstype = '2'
    REPLACE ccvlitmds  WITH ALLTRIM(ccvsctdis) + " "+ ALLTRIM(ccvlitmds)
  ENDIF        
  *B605874,1 ABD [End]
  
 SELECT Hlcvslin
ENDSCAN

SUM ROUND(IIF(ncvlconv > 0,;
         ((ROUND(ncvlitmpr,2) / ncvlconv) *;
           ROUND(ncvlitmqt,1)),ROUND(ncvlitmqt,1)*;
           ROUND(ncvlitmpr,2)),2);
           TO lnTot807  FOR   cstynum = lcRpStyNo .AND. ccvsdefty ="7"

SUM ROUND(IIF(ncvlconv > 0,;
         ((ROUND(ncvlitmpr,2) / ncvlconv) *;
           ROUND(ncvlitmqt,1)),ROUND(ncvlitmqt,1)*;
           ROUND(ncvlitmpr,2)),2);
           TO lnTotDuti  FOR cstynum = lcRpStyNo .AND.  ccvsdefty ="D"
           
SUM ROUND(IIF(ncvlconv > 0,;
         ((ROUND(ncvlitmpr,2) / ncvlconv) *;
           ROUND(ncvlitmqt,1)),ROUND(ncvlitmqt,1)*;
           ROUND(ncvlitmpr,2)),2);
           TO lnTot800  FOR cstynum = lcRpStyNo .AND. ccvsdefty ="8"

*-- End OF lfCollect
*:*************************************************************
*: Name      : lfType
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/09/2002
*: Purpose   : Return The type.
*:*************************************************************
*: Called from : OG read cycle
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfType ()
*:*************************************************************
*
FUNCTION lfType

IF !EMPTY(cCvstype)
  DO CASE
    CASE cCvstype = '1'
     *B605874,1 ABD fix bug that the 'E=TRIM' not Printed. [Begin]
     *RETURN 'A=FABRIC'
     *CASE cCvstype = '2'  
     *RETURN 'B=LINNING'
     *CASE cCvstype = '3'
     IF !EMPTy(cFabric)
       IF LEFT(cFabric,1) = 'L'
         RETURN 'B=LINNING'
       ELSE
         RETURN 'A=FABRIC'
       ENDIF
     ENDIF  
   CASE cCvstype = '2'
     *B605874,1 ABD [End]
      RETURN 'E=TRIM'
  ENDCASE    
ENDIF  
RETURN ' '

*-- End Of lfType.
*:*************************************************************
