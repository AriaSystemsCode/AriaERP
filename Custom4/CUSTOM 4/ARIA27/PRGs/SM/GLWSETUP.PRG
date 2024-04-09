*:----------------------------------------------------------------
*: Procedure file: GLWSETUP.PRG
*: Program description : Company Wizard
*: For System          : Aria Advantage Series - Version 2.7
*: For Module          : System Manager - (SM)
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : N000185
*:----------------------------------------------------------------
*: Calls
*:  Procs & Fncts: lpShow
*:               : lfwData_1
*:               : lfwData_2
*:               : lfvData_1
*:               : lfvData_2
*:               : lfvData_14
*:               : lfvData_15
*:               : lfBuildPic
*:               : lfvOk
*:               : lfvCancel
*:----------------------------------------------------------------
*: Called From         : SMWMAIN.PRG
*:----------------------------------------------------------------
*: Modifications       : 
*:----------------------------------------------------------------
*:
PARAMETERS llFromSM,lcCompId,lcComp,lcDumii

EXTERNAL ARRAY laData,laKeyField

DECLARE laKeyField [1,4],laTDatePer[2,2],laSegSize[1,3],laObjdisp[6]

laTDatePer[1,1]='Period'
laTDatePer[1,2]='P'
laTDatePer[2,1]='Dates'
laTDatePer[2,2]='D'

laCtrStat = 'DISABLE' && To disable the browse pad in the menu

laObjdisp = .F.    && Default Display all lines in  screen 
llBrowse  = .F.

puDatePer = 1
lnRitPos  = 2
lnCount   = 0
lnTotlen  = 0
lnFrsSegSz= 15
lcOldBPW  = 0
lcArrow1  = ' '
lcArrow2  = ' '
lcArrow3  = ' '
lcArrow4  = ' '
lcArrow5  = ' '
lcArrow6  = ' '

lcData1   = ' '    && Variable to Hold the laData[1]
lcData2   = ' '    && Variable to Hold the laData[2]
lcData3   = ' '
lcData19  = ' '    && Variable to hold old variable of laData[19]
lcAccMask = ' '    && Variable to Hold the Account Mask

llOpenBy  = .F.
llOpenSeg = .F.
llFromSM  = IIF(TYPE('llFromSM')='U',.F.,llFromSM)
llOpnAccCd = .F.
lcScFields = "CSETRETMJ,CSETSUSMJ,CSETDEBUD,LSETPBBBD,LSETCNTOT,LSETLOKPD,LSETACATE,LSETALBBE,DSETBBDAT,LSETCON,LSETFOREL,LSETFORAT,LSETALADD,NSETBATPW,CSETPCNT,LSETSINTN,LSETDON,NSETCOSTC,CSETEXMJ"
lcWindTitl = "General Ledger Setup"
llDisable = lfEndDis()
lcCherNo  = ""
IF FILE(gcSysHome+'SYWGLSGV.DBF')
  IF USED('SYWGLSGV')
    SELECT SYWGLSGV
    USE 
    llOpenSeg = .F.
  ELSE
    llOpenSeg = .T.
    SELECT 0
  ENDIF
  USE &gcSysHome.SYWGLSGV
  SET ORDER TO TAG ACSSEGVAL
ENDIF  
IF FILE(gcSysHome+'SYWGLSET.DBF')
  IF USED('SYWGLSET')
    SELECT SYWGLSET
    USE
  ELSE
    SELECT 0
  ENDIF  
  USE &gcSysHome.SYWGLSET
ENDIF  
IF FILE(gcSysHome+'SYWGLCHR.DBF')
  IF USED('SYWGLCHR')
    SELECT SYWGLCHR
    USE
  ELSE  
    SELECT 0
  ENDIF  
  USE &gcSysHome.SYWGLCHR
  SET ORDER TO TAG SEGACTIV 
ENDIF  
IF !USED('SYWFSHD')
  llOpenBy = .T.
  SELECT 0
  USE &gcSysHome.SYWFSHD ORDER 1
ELSE
  SELECT SYWFSHD
ENDIF
IF !USED('SYWACCOD')
  llOpnAccCd = .T.
  SELECT 0
  USE &gcSysHome.SYWACCOD ORDER 1
ELSE
  SELECT SYWACCOD
ENDIF  

SELECT SYWACCOD.NACSSEGNO,SYWACCOD.NACSSIZE,;
       SYWACCOD.CACSSHDES;
 FROM  SYWACCOD;
 WHERE NACSSEGNO <> 0  .AND. cIdentify = lcCherNo;
 INTO  ARRAY laSegSize 
IF ALEN(laSegSize) > 1 .AND. !EMPTY(laSegSize[1,1])
  *** Calculate seg. postions & sizes. ***
  FOR lnCount = 1 TO ALEN(laSegSize,1)
    lnTotlen  = lnTotlen  + laSegSize [lnCount,1] + 4
    laObjdisp[lnCount]  = .T.
  ENDFOR
  lnXSize   = 15     && Variable to Hold the X size of laData[1],laData[2] in the screen
  lnFrsSegSz= laSegSize[1,2]
ENDIF


SELECT SYWACCOD
IF !EOF() .AND. !llDisable
  lcAccMask  = cAcsMask
  lnAcsNoSeg = nAcsNoSeg
  SKIP 1
  lnXSize = nAcsSize
ELSE
  lnXSize = 15
ENDIF

SELECT SYWGLSET

SCATTER FIELDS &lcScFields TO laData 
IF laData[9] = {}
  SET ORDER TO Ccomp_id IN SYWCOMP
  SET ORDER TO Compfyear IN SYWFSHD
  SELECT SYWGLSET
  lcCurr_yer = IIF(SEEK(gcPrnt_Cmp,'SYWCOMP'),SYWCOMP.cCurr_Yer,'')
  SELECT SYWFSHD
  LOCATE FOR cFisyStat = "C"
  IF FOUND()
    laData[9]  = SYWFSHD.dFisBgDat
  ENDIF
ENDIF
IF !laData[17]  &&-- Module has not setuped yet .
  laData[4]  = .F.  
  laData[5]  = .T.
  laData[6]  = .T.
  laData[7]  = .T.
  laData[8]  = .T.
  laData[10] = .F.
  laData[11] = .F.
  laData[12] = .F.
  laData[13] = .T.
  laData[14] = 1
  laData[16] = .T.
  laData[15] = 'P'
  lcDatPer   = laTDatePer[1,1]
  puDateper  = 1
  laData[18] = 1
  lcArrow1   = ''
ELSE
  puDatePer  = IIF(AT(ALLTRIM(laData[15]),"PD")>0,AT(ALLTRIM(laData[15]),"PD"),1)
  lcDatPer   = laTDatePer[puDatePer,1]
  =lfPointer()
ENDIF  
*wab -- new release
*DO (gcScrDir + gcAct_Appl + '\GlWsetup.SPR')
DO (gcScrDir + "SM\" + '\GlWsetup.SPR')
*wab "SM\"
glQuitting = .T.
=lfQuit()

*!**************************************************************************
*!
*!      Procedure: lpShow
*!
*!**************************************************************************
*
PROCEDURE lpShow

SELECT SYWACCOD
GO TOP
IF EOF() .OR. llDisable
  SHOW GET laData[1]  DISABLE
  SHOW GET laData[2]  DISABLE
  SHOW GET laData[3]  DISABLE
  SHOW GET laData[19] DISABLE
ELSE
  SHOW GET laData[1]  ENABLE  
  SHOW GET laData[2]  ENABLE
  SHOW GET laData[3]  ENABLE
  SHOW GET laData[19] ENABLE
ENDIF
SHOW GET laData[9] ENABLE

IF laData[14] = 1
  laData[15] = 'P'
  puDateper  = 1
  SHOW GET puDateper ENABLE
ELSE
  SHOW GET puDateper ENABLE
ENDIF  
SHOW GET laData[10] ENABLE

*!**************************************************************************
*!
*!      Function: lfwData_1
*!
*!**************************************************************************
*
FUNCTION lfwData_1

lcData1 = laData[1]

*!**************************************************************************
*!
*!      Function: lfvData_1
*!
*!**************************************************************************
*
FUNCTION lfvData_1

lcRetain = ALLTRIM(laData[1])+STRTRAN(SUBSTR(lcAccMask,AT("-",lcAccMask)),"#","0")

IF llBrowse .OR. (!EMPTY(laData[1]) .AND. lcData1 <> laData[1])

  SELECT SYWGLCHR
  lcSavOrder = SET('ORDER')
  SET ORDER TO ACCTCODE

  LOCATE FOR CACCTCODE = lcRetain .AND. LEFT(cTypeCode,1) = 'Q'

  IF !FOUND() .OR. ATC('?',laData[1]) > 0 .OR. llBrowse

    LOCATE FOR LEFT(cTypeCode,1) = 'Q'

    IF FOUND()

      DIMENSION laTemp[1]
      laTemp[1] = ''
      lcSavBrFld=lcBrfields
      lcSavTitle=lcFile_Ttl

      lcBrfields="CACCTCODE :H= 'Account Code',;
                  CACCNLDES :H= 'Account Description',;
                  CTYPECODE :H= 'Type Code',;
                  CSTANDARD :H= 'Standard'"

      SET ORDER TO TAG TYPACTPOS

      =gfbrows(["Q"FOR cIdentify = lcCherNo .AND. VAL(STRTRAN(SUBSTR(CACCTCODE,lnFrsSegSz+1),"-",""))=0],'CACCTCODE,CACCNLDES,CTYPECODE,CSTANDARD','laTemp')

      lcFile_Ttl=lcSavTitle
      lcBrfields=lcSavBrFld

      SET ORDER TO

      IF !EMPTY(laTemp[1])
        laData[1] = SUBSTR(laTemp[1],1,lnFrsSegSz)
        lcRetain  = ALLTRIM(laData[1])+STRTRAN(SUBSTR(lcAccMask,AT("-",lcAccMask)),"#","0")
      ELSE
        laData[1] = lcData2
        _CUROBJ=OBJNUM(laData[1])
      ENDIF   
    ELSE
      =gfModalGen("TRM00052B00000","Dialog")        
      laData[1] = SPACE(lnFrsSegSz)
    ENDIF  
  ENDIF
  SET ORDER TO &lcSavOrder
ENDIF 

laData[1]  = SUBSTR(laData[1],1,lnFrsSegSz)

llBrowse   = .F.

SHOW GET laData[1] ENABLE

SELECT SYWGLSET

*!**************************************************************************
*!
*!      Function: lfwData_2
*!
*!**************************************************************************
*
FUNCTION lfwData_2

lcData2 = laData[2]

*!**************************************************************************
*!
*!      Function: lfvData_2
*!
*!**************************************************************************
*
FUNCTION lfvData_2

lcSuspen = ALLTRIM(laData[2])+STRTRAN(SUBSTR(lcAccMask,AT("-",lcAccMask)),"#","0")

IF llBrowse .OR. (!EMPTY(laData[2]) .AND. lcData2 <> laData[2])

  SELECT SYWGLCHR
  lcSavOrder = SET('ORDER')
  SET ORDER TO ACCTCODE

  LOCATE FOR CACCTCODE = lcSuspen .AND. cStandard = 'Y'

  IF !FOUND() .OR. ATC('?',laData[2]) > 0 .OR. llBrowse

    LOCATE FOR cStandard = 'Y'

    IF FOUND()

      DIMENSION laTemp[1]
      laTemp[1] = ''
      lcSavBrFld=lcBrfields
      lcSavTitle=lcFile_Ttl

      lcBrfields="CACCTCODE :H= 'Account Code',;
                  CACCNLDES :H= 'Account Description',;
                  CTYPECODE :H= 'Type Code',;
                  CSTANDARD :H= 'Standard'"

      lcFile_Ttl = 'Chart of Account'

      SET ORDER TO TAG STDACTPOS

      =gfbrows(["Y"FOR cIdentify = lcCherNo .AND. VAL(STRTRAN(SUBSTR(CACCTCODE,lnFrsSegSz+1),"-",""))=0],'CACCTCODE,CACCNLDES,CTYPECODE,CSTANDARD','laTemp')

      lcFile_Ttl=lcSavTitle
      lcBrfields=lcSavBrFld

      SET ORDER TO

      IF !EMPTY(laTemp[1])
        laData[2] = SUBSTR(laTemp[1],1,lnFrsSegSz)
        lcSuspen  = ALLTRIM(laData[2])+STRTRAN(SUBSTR(lcAccMask,AT("-",lcAccMask)),"#","0")
      ELSE
        laData[2] = lcData2
        _CUROBJ=OBJNUM(laData[2])
      ENDIF   
    ELSE
      =gfModalGen("TRM00052B00000","Dialog")        
      laData[2] = SPACE(lnFrsSegSz)
      _CUROBJ=OBJNUM(laData[2])
    ENDIF  
  ENDIF
  SET ORDER TO &lcSavOrder
ENDIF

laData[2]  = SUBSTR(laData[2],1,lnFrsSegSz)

llBrowse   = .F.

SHOW GET laData[2] ENABLE

SELECT SYWGLSET

*!**************************************************************************
*!
*!      Function: lfwData_3
*!
*!**************************************************************************
*
FUNCTION lfwData_3

lcData3 = laData[3]

*!**************************************************************************
*!
*!      Function: lfvData_3
*!
*!**************************************************************************
*
FUNCTION lfvData_3

RETURN
lcBudCod=ALLTRIM(laData[3])
IF llBrowse .OR. (!EMPTY(laData[3]) .AND. lcData3 <> laData[3])
  SELECT GLBUDHD
  lcSavOrder = SET('ORDER')
  SET ORDER TO BDCODYR
  LOCATE FOR CBudCode = lcBudCod
  IF !FOUND() .OR. ATC('?',laData[3]) > 0 .OR. llBrowse
      DIMENSION laTemp[1]
      laTemp[1]  = ''
      lcSavBrFld = lcBrfields
      lcSavTitle = lcFile_Ttl
      lcBrfields = "CBudCode :H= 'Budget Code',;
                    CBudYear :H= 'Budget Year',;
                    CBudDes   :H= 'Budget Description'"
      lcFile_Ttl = 'Budget Codes'
      =gfbrows(.F.,'cBudCode,cBudYear,cBudDes','laTemp')
      lcFile_Ttl = lcSavTitle
      lcBrfields = lcSavBrFld
      SET ORDER TO
      IF !EMPTY(laTemp[1])
        laData[3] = laTemp[1]
      ELSE
        laData[3] = lcData3
      ENDIF
   ENDIF
   SET ORDER TO &lcSavOrder
ENDIF
SHOW GET laData[3] ENABLE
SELECT SYWGLSET
llBrowse = .F.

*!*************************************************************
*! Name      : lfwData_14
*! Developer : Ahmed Salah
*! Date      : 06/07/01
*! Purpose   : This function called from the when of the batch
*!             posting window to save the old value.
*!*************************************************************
*! Calls     : 
*!             Procedures : 
*!             Functions  : 
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example   : 
*!*************************************************************
*
FUNCTION lfwData_14

lcOldBPW = laData[14]

*!**************************************************************************
*!
*!      Function: lfvData_14
*!
*!**************************************************************************
*
FUNCTION lfvData_14

IF !BETWEEN(laData[14],1,13)
  =gfModalGen("TRM02244B00000","DIALOG")
  laData[14] = lcOldBPW
ENDIF

*!**************************************************************************
*!
*!      Function: lfvData_15
*!
*!**************************************************************************
*
FUNCTION lfvData_15

IF puDateper =1
  laData[15] = 'P'
  SHOW GET puDateper ENABLE
ELSE
  laData[15] = 'D'
  SHOW GET puDateper ENABLE
ENDIF

*!**************************************************************************
*!
*!      Function: lfBuildPic
*!
*!**************************************************************************
*
FUNCTION lfBuildPic

RETURN "X"+REPLICATE("9",lnFrsSegSz-1)

*!**************************************************************************
*!
*!      Function: lfvOk
*!
*!**************************************************************************
*
FUNCTION lfvOk

llFrstSet  = laData[17]
laData[17] = .T.
laData[1]  = IIF(EMPTY(laData[1]),SPACE(lnXSize),SUBSTR(laData[1],1,lnFrsSegSz))
laData[2]  = IIF(EMPTY(laData[2]),SPACE(lnXSize),SUBSTR(laData[2],1,lnFrsSegSz))
laData[19] = IIF(EMPTY(laData[19]),SPACE(lnXSize),SUBSTR(laData[19],1,lnFrsSegSz))
SELECT SYWGLSET
GO TOP
IF EOF()
  APPEND BLANK
ENDIF  
GATHER FROM laData FIELDS &lcScFields 
=gfAdd_Info()
FLUSH
*** Enter the default segments with zero ***
*** values except the first one...
IF !llFrstSet
  SELECT SYWGLSGV
  FOR lnCount = 2 TO ALEN(laSegSize,1)
    lcSegNo   = ALLTRIM(STR(laSegSize[lnCount,1]))
    lcSegVal  = REPLICATE('0',laSegSize[lnCount,2])
    IF !SEEK(lcSegNo+lcSegVal)
      APPEND BLANK
      REPLACE CACSSEGNO WITH lcSegNo ;
              CSEGVALUE WITH lcSegVal ;
              CSEGLNDES WITH "Default" ;
              CSEGSHDES WITH "Default"
      =gfAdd_Info()
    ENDIF
  ENDFOR
ENDIF  

SELECT SYWGLSET

*!**************************************************************************
*!
*!      Function: lfvCancel
*!
*!**************************************************************************
*
FUNCTION lfvCancel
lcNextActn = 'C'

*!**************************************************************************
*!
*!      Function: lfvCost1
*!
*!**************************************************************************
*
FUNCTION lfvCost1

laData[18] = 1
=lfPointer()

*!**************************************************************************
*!
*!      Function: lfvCost2
*!
*!**************************************************************************
*
FUNCTION lfvCost2

laData[18] = 2
=lfPointer()

*!**************************************************************************
*!
*!      Function: lfvCost3
*!
*!**************************************************************************
*
FUNCTION lfvCost3

laData[18] = 3
=lfPointer()

*!**************************************************************************
*!
*!      Function: lfvCost4
*!
*!**************************************************************************
*
FUNCTION lfvCost4

laData[18] = 4
=lfPointer()

*!**************************************************************************
*!
*!      Function: lfvCost5
*!
*!**************************************************************************
*
FUNCTION lfvCost5
 
laData[18] = 5
=lfPointer()

*!**************************************************************************
*!
*!      Function: lfvCost6
*!
*!**************************************************************************
*
FUNCTION lfvCost6

laData[18] = 6
=lfPointer()

*!**************************************************************************
*!
*!      Function: lfPointer
*!
*!**************************************************************************
*
FUNCTION lfPointer

STORE " " TO lcArrow1,lcArrow2,lcArrow3,lcArrow4,lcArrow5,lcArrow6
lcArrow  = "lcArrow" + ALLTRIM(STR(laData[18]))
&lcArrow = ''

IF WEXIST(gcBaseWind)
  =lfRefresh()
ENDIF

*!**************************************************************************
*!
*!      FUNCTION : lfQuit
*!
*!**************************************************************************
*
FUNCTION lfQuit

IF llOpenSeg .AND. USED('SYWGLSGV')
  USE IN SYWGLSGV
  llOpenSeg = .F.
ENDIF

IF USED('SYWFSHD') .AND. llOpenBy
  llOpenBy = .F.
  SELECT SYWFSHD 
  USE IN SYWFSHD 
ENDIF  
IF USED('SYWACCOD') .AND. llOpnAccCd
  llOpnAccCd = .F.
  USE IN SYWACCOD
ENDIF  
IF llFromSM .AND. !EMPTY(gcSysHome)
  IF USED('SYWGLSET')
    SELECT SYWGLSET
    USE
  ENDIF  
  IF USED('SYWGLCHR')
    SELECT SYWGLCHR
    USE
  ENDIF  
ENDIF
 
*!**************************************************************************
*!
*!      Function: lfwData_19
*!
*!**************************************************************************
*
FUNCTION lfwData_19

lcData19 = laData[19]

*!**************************************************************************
*!
*!      Function: lfvData_19
*!
*!**************************************************************************
*
FUNCTION lfvData_19

lcExMaj = ALLTRIM(laData[19])+STRTRAN(SUBSTR(lcAccMask,AT("-",lcAccMask)),"#","0")

IF llBrowse .OR. (!EMPTY(laData[19]) .AND. lcData19 <> laData[19])

  SELECT SYWGLCHR
  lcSavOrder = SET('ORDER')
  SET ORDER TO ACCTCODE

  LOCATE FOR CACCTCODE = lcExMaj .AND. cStandard = 'Y'

  IF !FOUND() .OR. ATC('?',laData[19]) > 0 .OR. llBrowse

    LOCATE FOR cStandard = 'Y'

    IF FOUND()

      DIMENSION laTemp[1]
      laTemp[1] = ''
      lcSavBrFld=lcBrfields
      lcSavTitle=lcFile_Ttl

      lcBrfields="CACCTCODE :H= 'Account Code',;
                  CACCNLDES :H= 'Account Description',;
                  CTYPECODE :H= 'Type Code',;
                  CSTANDARD :H= 'Standard'"

      lcFile_Ttl = 'Chart of Account'

      SET ORDER TO TAG STDACTPOS

      =gfbrows(["Y"FOR VAL(STRTRAN(SUBSTR(CACCTCODE,lnFrsSegSz+1),"-",""))=0],'CACCTCODE,CACCNLDES,CTYPECODE,CSTANDARD','laTemp')

      lcFile_Ttl=lcSavTitle
      lcBrfields=lcSavBrFld

      SET ORDER TO

      IF !EMPTY(laTemp[1])
        laData[19] = SUBSTR(laTemp[1],1,lnFrsSegSz)
        lcExMaj    = ALLTRIM(laData[19])+STRTRAN(SUBSTR(lcAccMask,AT("-",lcAccMask)),"#","0")
      ELSE
        laData[19] = lcData19
        _CUROBJ=OBJNUM(laData[19])
      ENDIF   
    ELSE
      =gfModalGen("TRM00052B00000","Dialog")        
      laData[19] = SPACE(lnFrsSegSz)
      _CUROBJ=OBJNUM(laData[19])
    ENDIF  
  ENDIF
  SET ORDER TO &lcSavOrder
ENDIF

laData[19]  = SUBSTR(laData[19],1,lnFrsSegSz)

llBrowse   = .F.

SHOW GET laData[19] ENABLE

SELECT SYWGLSET

*!*************************************************************
*! Name      : lfvGlResu
*! Developer : Ahmed Salah
*! Date      : 06/07/01
*! Purpose   : Valid resume later button.
*!*************************************************************
*! Calls     : 
*!             Procedures : 
*!             Functions  : 
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example   : =lfvGlResu()
*!*************************************************************
*
FUNCTION lfvGlResu
=lfvok()
lcNextActn = 'U'

*!*************************************************************
*! Name      : lfvGlPre
*! Developer : Ahmed Salah
*! Date      : 06/07/01
*! Purpose   : Valid previous button.
*!*************************************************************
*! Calls     : 
*!             Procedures : 
*!             Functions  : 
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example   : =lfvGlPre()
*!*************************************************************
*
FUNCTION lfvGlPre
=lfvok()
lcNextActn = 'B'

*!*************************************************************
*! Name      : lfvGlNext
*! Developer : Ahmed Salah
*! Date      : 06/07/01
*! Purpose   : Valid next button.
*!*************************************************************
*! Calls     : 
*!             Procedures : 
*!             Functions  : 
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example   : =lfvGlNext()
*!*************************************************************
*
FUNCTION lfvGlNext

=lfvok()
lcNextActn = 'N'


*!**************************************************************************
*!
*!      Function: lfvGLActBr
*!
*!**************************************************************************
*
FUNCTION lfvGLActBr
PARAMETERS lcObjName

_CUROBJ  = OBJNUM(lcObjName)
llBrowse = .T.
KEYBOARD "{ENTER}"

*!**************************************************************************
*!
*!      Function: lfwGLActBr
*!
*!**************************************************************************
*
FUNCTION lfwGLActBr

IF !MDOWN()
  RETURN .F.
ENDIF

*!*************************************************************
*! Name      : lfEndDis
*! Developer : Ahmed Salah
*! Date      : 06/07/01
*! Purpose   : Function for enable disable function.
*!*************************************************************
*! Calls     : 
*!             Procedures : 
*!             Functions  : 
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example   : =lfEndDis()
*!*************************************************************
*
FUNCTION lfEndDis
PRIVATE lnOldAls , llReturn
llReturn = .T.
lnOldAls = SELECT(0)
IF !USED("SYWACHRT")
   = gfOpenFile(gcSysHome+"SYWACHRT","Cidentify","SH")
ENDIF
SELECT SYWACHRT
LOCATE FOR lSelect
IF FOUND()
  llReturn = (VAL(cIdentify) = 1)
  lcCherNo = IIF(cIdentify='1',0,cIdentify)
ENDIF
SELECT(lnOldAls)
RETURN(llReturn)