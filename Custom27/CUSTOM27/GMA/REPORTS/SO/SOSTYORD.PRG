*:********************************************************************************
*: Program file  : SOSTYORD
*: Program desc. : Custom Style Order Form in Excel
*: System        : Aria Apparel System (A27).
*: Module        : Sales Order [SO]
*: Developer     : ABDOU ELGENDI - (ABD) Due to C#102535,1
*: Date          : 02/04/2002
*:********************************************************************************
*: Calls 
*:               : FUNCTIONS  : lfwRepWhen  , lfvDepart , lfvStyle
*:               :            : lfMajGet    , lfItmPos  ,lfCreatemp 
*:               :            : lfsrSty     , lfStySum   , lfUpt2tmp
*:               -----------------------------------------------
*:               : PROCEDURE  : 
*:********************************************************************************
*: Passed Parameters  : None.
*:********************************************************************************
*:C#102535,1
*:********************************************************************************
*:Modifications  :
*;B605849,1 HBG 05/06/2002 Add Variable to Print date in the header of the report 
*:********************************************************************************
*:
*-- Begin Declaration variables.
*-- laRpTargDp :- Target array hold the Selected department.
*-- laRpSourDp :- Source array hold all the department.
*-- lcFltrExp  :- Varisable hold the Filter Expration.
*-- lcTempFile     :- File Hold the collected data that available to print.
*-- lnGroupPos :- Get the Group Position.
*-- lnCategPos :- Get the category Position.
*-- lnClassPos :- Get the Class Position.
*-- lnSbClsPos :- Get the SubClass Position.
*-- lcRpStyAvl :- Style Available.
*-- lcRpCustil :- Custom Titel
*-- End Declaration variables.


*-- if user change filter criteria then you must collect data again [Begin]
IF llOGFltCh
  *-- Function To Create temp File
  = lfCreatemp()
  *-- Collecting Data In The First Time.
  = lfCollect ()
ENDIF

IF !USED(lcTempFile)
  =gfOpenFile(gcWorkDir+lcTempFile,gcWorkDir+lcTempFile,'SH')
ENDIF

SELECT  (lcTempFile)
IF EOF()
 *---Text : 'No Record Selected for the report..!'
 =gfModalGen('TRM00052B00000','DIALOG')
 RETURN
ENDIF
USE

DIMENSION laCompAdd[5,1] 
STORE '' TO laRpCpAdd1 , laRpCpAdd2 , laRpCpAdd3 ,laCompAdd  ,;
            laRpCpAdd4 , laRpCpAdd5 , lcRpCmpNme , lcRpCPhone,;
            lcRpCFax   , laRpWRAdd1 , laRpWRAdd2 , laRpWRAdd3,;
            laRpWRAdd4 , laRpWRAdd5 , lcRpWRPhne ,  lcRpWRFx
            

*-- Get the address for the selected company.
SELECT SYCCOMP
SEEK gcAct_Comp

= gfGetAdr('SYCCOMP' , '' , '' , '' , @laCompAdd)
lcPhonPict = gfPhoneTem()          && Company Phone Picture Format.

laRpCpAdd1 = laCompAdd[1]
laRpCpAdd2 = laCompAdd[2]
laRpCpAdd3 = laCompAdd[3]
laRpCpAdd4 = laCompAdd[4]
laRpCpAdd5 = laCompAdd[5]

lcRpCmpNme = cCom_Name             && Company Name.
lcRpCPhone = 'TEL : ' + TRANSFORM(cCom_Phon, lcPhonPict)
lcRpCFax   = 'FAX : ' + TRANSFORM(cCom_Fax, lcPhonPict)


SELECT WAREHOUS

IF SEEK ('MAIN')
  *-- If no need to print the warehous data
  *-- redimension the array to be used by the glabal function gfGetAdr.
  DIMENSION laCompAdd[5,1]      
  laCompAdd = ''
  =gfGetAdr('WAREHOUS' , '' , '' , '' , @laCompAdd)

  laRpWRAdd1 = laCompAdd[1]
  laRpWRAdd2 = laCompAdd[2]
  laRpWRAdd3 = laCompAdd[3]
  laRpWRAdd4 = laCompAdd[4]
  laRpWRAdd5 = laCompAdd[5]

  *-- Get the warehous name and its phone in the variables   lcCompName , lcCompPhon
  lcRpWRPhne = 'TEL : ' + TRANSFORM(CPhone, lcPhonPict)
  lcRpWRFx   = 'FAX : ' + TRANSFORM(CFax, lcPhonPict)
ENDIF


lcRpDtDir  = gcDataDir  && Save The Data Directory.
lcRpWrDir  = gcWorkDir  && Save The Work Directory.
lcRpSysDir = gcSysHome  && Save The system  Directory.
lcRpRpDir  = gcRepHome  && Save The report Directory.
lcRpComp   = gcAct_Comp && Save The Active Company.
lcRpDefPth = gcDef_Path && Save The DefPath.
lcRpTmpFle = lcTempFile && Save The TempName for the printed file.
lcConnStr  = ""
lcRpTitle  = lcRpCustil && Printed Custom Titel
lcRpComp   = gcAct_Comp  && Save The Active Company .
*B605849,1 HBG 05/06/2002 Add Variable to Print date in the header of the report [Begin]
ldRpSysDat = gdSysDate
STORE "" TO lcRpDept , lcRpGroup
=lfGetVar()
*B605849,1 [End]

SAVE TO (gcWorkDir+lcTmpMemo+'.MEM') ALL LIKE l?RP* 

lcCommLine = (gcWorkDir+lcTmpMemo+'.MEM')

lcLib=SYS(2004)+"foxtools.fll"

IF FILE(lcLib)
  SET LIBRARY TO (SYS(2004)+"FOXTOOLS.FLL") ADDITIVE
  SW_HIDE = 0
  lnFnWinExec =EVALUATE("RegFn('WinExec', 'CI', 'I')")
  =EVALUATE("CALLFN("+STR(lnFnWinExec)+;
  ",gcRepHome+'SO\'+[SOSTYVFP.EXE ]+lcCommLine,"+STR(SW_Hide)+")")
  RELEASE LIBRARY (SYS(2004)+"FOXTOOLS.FLL")
ELSE
  WAIT "LIBRARY NOT FOUND" WINDOW
  RETURN .F.
ENDIF


RETURN
*-- End of program code.
*:*************************************************************
*: Name      : lfwRepWhen
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 02/04/2002
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

*C605849,1 HBG 10/06/2002 Change the option of Dept in OG to select only one Dept at a time[Begin]
*DIMENSION laRpSourDp [1], laRpTargDp [1]
*STORE '' TO laRpTargDp , laRpSourDp

SELECT IcDepThd
*SELECT dept DISTINCT;
*FROM IcDepThd;
*INTO ARRAY laRpSourDp

SELECT dept,cdeptdesc DISTINCT FROM IcDepThd INTO CURSOR (lcBrowCurs)  
SELECT (lcBrowCurs)
INDEX ON dept TAG (lcBrowCurs)
*C605849,1 [End]

*-- Get the Group Position.
lnStylePos = lfItmPos('STYLE.CSTYMAJOR')


*-- End of lfwRepWhen.
*:*************************************************************
*: Name      : lfvDepart
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 02/04/2002
*: Purpose   : Function to get the department into mover.
*:*************************************************************
*: Called from : OG read cycle
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfvDepart()
*:*************************************************************
*
FUNCTION lfvDepart

*C605849,1 HBG 10/06/2002 Change the option of Dept in OG to select only one Dept at a time[Begin]
*DIMENSION laOldCode[1]
*laOldCode = ''
*= ACOPY(laRpTargDp,laOldCode)

*=gfMover(@laRpSourDp,@laRpTargDp,"Department ",.T.,'')
*IF ALEN (laOldCode) = ALEN(laRpTargDp)
*   =ASORT(laOldCode)
*   =ASORT(laRpTargDp)
*  FOR X = 1 TO ALEN (laRpTargDp)
*    IF laOldCode[X] # laRpTargDp[X]
*      llOGFltCh = .T.
*      EXIT
*    ENDIF  
*  ENDFOR
*ELSE
*  llOGFltCh = .T.
*ENDIF

lcDptFld  = VARREAD()
lcDept  = EVAL(lcDptFld)

SELECT (lcBrowCurs) 
SET ORDER TO (lcBrowCurs)
IF !EMPTY(lcDept) AND !SEEK(ALLTRIM(lcDept),lcBrowCurs)
  lcBrFields = [Dept :R:H='Department':8, cdeptdesc :R:H='Description':30]
  DIMENSION laSelected[1]
  lcFldName = 'DEPT'
  IF ARIABROW('', 'Departments',;
                05,10,30,10,.F.,.F.,lcFldName,'laSelected')
    lcDept = laSelected[1]             
  ELSE
    lcDept = ""
  ENDIF  
ENDIF
&lcDptFld = lcDept 
*C605849,1 [End]

*-- End OF lfvDepart.
*:*************************************************************
*: Name      : lfMajGet
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 02/04/2002
*: Purpose   : To get the title and picture of style major segement 
*:*************************************************************
*: Called from : Option Grid
*:*************************************************************
*: Calls       : ....
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfMajGet()
*:*************************************************************

FUNCTION lfMajGet

lcMajPic  = gfItemMask("PM")
lnMajPic  = LEN(lcMajPic)
lcMajPic  = "@! " + lcMajPic
lcMajTtl  = gfItemMask("HM")
lnColorWid= LEN(gfItemMask('PN'))

*-- End OF lfMajGet
*:*************************************************************
*: Name      : lfItmPos
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 02/04/2002
*: Purpose   : Evaluate fixed filter position within array.
*:*************************************************************
*: Called from : Option Grid
*:*************************************************************
*: Calls       : .....
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : Position.
*:*************************************************************
*: Example     : = lfItmPos()
*:*************************************************************
*:
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos

*-- End of lfItmPos.
*:*************************************************************
*: Name      : lfCollect
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 02/04/2002
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
PRIVATE lcOldOrder ,lcScnExp 
STORE '' TO lcScnExp , lcOldOrder,lcDepExp

*C605849,1 HBG 10/06/2002 Change the option of Dept in OG to select only one Dept at a time[Begin]
*-- Add Department to lcRpExp
*IF !EMPTY(laRpTargDp)
*  FOR I = 1 To ALEN(laRpTargDp)
*    lcDepExp = lcDepExp + laRpTargDp[I]
*  ENDFOR
*  lcScnExp = " INLIST(DEPT,lcDepExp)"
*ENDIF
*C605849,1 [End]

lcScnExp =  IIF (EMPTY(lcScnExp),'.T.',lcScnExp)
lcStyExp = IIF(lcRpStyAvl = 'S','(Style.TotStk > 0)',IIF(lcRpStyAvl = 'O','((Style.TotWip + Style.TotStk - Style.TotOrd) > 0)','.T.'))

SELECT STYLE
lcOldOrder = ORDER()
SET ORDER TO STYLE

*-- Scan On style File Or the Cursur File.

lcStylFile = laOGFxFlt[lnStylePos,6]
llWorkDeal = !EMPTY(lcStylFile) AND USED(lcStylFile) AND RECCOUNT(lcStylFile) > 0

*-- If we will deal With the Curser.
IF llWorkDeal
  SELECT (lcStylFile)
  LOCATE
  SCAN
    IF SEEK(LEFT(&lcStylFile..cStyMajor,lnMajPic),'STYLE'),'')
      SELECT STYLE
      SCAN WHILE LEFT(Style,lnMajPic) = LEFT(&lcStylFile..cStyMajor,lnMajPic) ;
        FOR &lcRpExp .AND. &lcScnExp .AND. Style.Status = 'A'
        WAIT WINDOW 'Collecting data For style :' +cStyMajor+'...' NOWAIT
        IF &lcStyExp
          *-- Add Function to Update the Temp File.
          = lfUpt2tmp ()
        ENDIF
      ENDSCAN
    ENDIF
    SELECT (lcStylFile)    
  ENDSCAN
ELSE
  *-- IF we will deal with the Style file.
  SELECT STYLE
  WAIT WINDOW 'Collecting data .... Please Wait.' NOWAIT
  SCAN FOR &lcRpExp .AND. &lcScnExp .AND. Style.Status = 'A'
    WAIT WINDOW 'Collecting data For style :' +cStyMajor+'...' NOWAIT
    IF &lcStyExp
     *-- Add Function to Update the Temp File.
     = lfUpt2tmp ()
    ENDIF
  ENDSCAN
ENDIF

SELECT STYLE
SET ORDER TO &lcOldOrder 
WAIT CLEAR

*-- End OF lfCollect.
*:*************************************************************
*: Name      : lfCreatemp
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/09/2002
*: Purpose   : Create the temp files.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Called from : Report code
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns    : None.
*:*************************************************************
*: Example   : = lfCreatemp()
*:*************************************************************
*
FUNCTION lfCreatemp

IF !USED(lcTempFile)
  CREATE Table (gcWorkDir+lcTempFile) (STYLE C(19)    , COLOR C(6)     , cstymajor C(8),;
                         desc  C(20)    , desc1 C(60)    , pricea N(12,2),;
                         dept  C(5)     , cstygroup C(6) , cstycatg C(6) ,;
                         cstyclass C(6) , cstysbclas C(6))
ENDIF

SELECT (lcTempFile)
ZAP
INDEX ON Dept+cStygroup+cStyCatg+cStyClass+cStysbClas+style Tag (lcTempFile) of (gcWorkDir+lcTempFile+'.CDX')

*-- End OF lfCreatemp
*:*************************************************************
*: Name      : lfsrSty
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/09/2002
*: Purpose   : Set and Rest functions for style filter.
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Returns            : None
*:*************************************************************
*: Example   : =lfsrSty()
*:*************************************************************
*: Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*:*************************************************************
*:
FUNCTION lfSRSty
PARAMETERS lcParm
IF lcParm = 'S'  && Set code
  *-- open this file in another alias to set order to Style Major 
  *-- unique index.
  USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
  SELECT STYLE
  SET ORDER TO TAG Cstyle
  SET RELATION TO STYLE.STYLE INTO STYLE_X
  GO TOP IN STYLE
ELSE  && Reset code
  USE IN STYLE_X
  SELECT STYLE
  SET ORDER TO TAG STYLE
ENDIF
*-- End of lfsrSty.
*:*************************************************************
*: Name      : lfStySum
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/09/2002
*: Purpose   : sum a specific field for the current style in style file
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Returns            : Calculated field value.
*:*************************************************************
*: Example   : =lfStySum()
*:*************************************************************
*:
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnTotcomp = 0

IF RECCOUNT('STYLE') != 0
  lnStyRec = RECNO('STYLE')
  SELECT Style_X
  SUM &lcCOMP TO lnTotcomp WHILE ALLTRIM(cStyMajor) == ALLTRIM(lcSty)
  SELECT Style
  IF BETWEEN(lnStyRec,1,RECCOUNT())
    GO lnStyRec
  ENDIF  
  DO CASE
    CASE lnAddToVar = 1
  	  lnO_T_S = lnTotcomp
    CASE lnAddToVar = 2
      lnO_T_S = lnO_T_S + lnTotcomp
    CASE lnAddToVar = 3
      lnO_T_S = lnO_T_S - lnTotcomp
  ENDCASE
ENDIF  
RETURN INT(lnTotcomp)
*-- End of lfStySum.
*:*************************************************************
*: Name      : lfUpt2tmp
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/09/2002
*: Purpose   : Add Function to Update the Temp File.
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Returns            : None.
*:*************************************************************
*: Example   : = lfUpt2tmp ()
*:*************************************************************
*:
FUNCTION lfUpt2tmp

SELECT (lcTempFile)
APPEND BLANK
REPLACE STYLE      WITH Style.Style     ,;
        COLOR      WITH PADR(RIGHT(STYLE,lnColorWid),lnColorWid),;
        cStyMajor  WITH Style.cStyMajor ,;
        desc       WITH Style.desc      ,;
        desc1      WITH Style.desc1     ,;
        pricea     WITH Style.pricea    ,;
        cStyGroup  WITH Style.cStyGroup ,;
        Dept       WITH Style.Dept      ,;
        cstycatg   WITH Style.cstycatg  ,;
        cstyclass  WITH Style.cstyclass ,;
        cstysbclas WITH Style.cstysbclas
SELECT STYLE

*-- End OF lfUpt2tmp.

*:*************************************************************
*: Name      : lfGetVar
*: Developer : hend Ghanem
*: Date      : 01/09/2002
*: Purpose   : Get Group And Dept Desc
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Returns            : None.
*:*************************************************************
*: Example   : = lfUpt2tmp ()
*:*************************************************************
*:B605849
FUNCTION lfGetVar

lcSelDept  = SUBSTR(lcRpExp,ATC('STYLE.DEPT',lcRpExp)+14,5)
lcSelGroup = SUBSTR(lcRpExp,ATC('STYLE.CSTYGROUP',lcRpExp)+19,6)

IF ATC('STYLE.DEPT',lcRpExp) <> 0
  lcRpDept = IIF(SEEK(lcSelDept,'IcDepThd'),ALLTRIM(IcDepThd.cdeptdesc),"")
ELSE
  lcRpDept = 'All Departments'  
ENDIF

IF ATC('STYLE.CSTYGROUP',lcRpExp) <> 0
  lcRpGroup = gfCodDes(lcSelGroup ,'CSTYGROUP')
  lcRpGroup = ALLTRIM(lcRpGroup)
ELSE
  lcRpGroup = 'All Groups'
ENDIF  