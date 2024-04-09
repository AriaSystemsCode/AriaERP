*!**********************************************************************************************
*: Program file  : MATIMREQ.PRG
*: Program desc. : Time Phased Material Requirement.
*: Module        : Aria Apparel Series.
*: Date          : 02/24/2002
*: Developer     : RANIA ABDEL RAZIK (RAE)
*!**********************************************************************************************
*! Refer to 	 : E301854
*!**********************************************************************************************
*:Modifications :
*:****************************************************************************************
*!E301901,1 AMH 05/12/2002 Add a new option: base requirementson (WIP/Plan/Both).
*!E301901,1                and new form to print the detail info.
*!B606107,1 KHM 06/13/2002 Fix the bug of not calculating the on order based on use
*!B606145,1 AMH 06/23/2002 Fix the bug of variable nforqty not found.
*!E301942,1 AMH 07/04/2002 Add a new options: style and style color 
*!E301942,1                change material option from between to in list.
*:B038431,1 MHM 09/13/2004 Fix bug that if you didn't define item code structure you'll get error
*:B038431,1 MHM            [Variable 'laSeg' Not Found ] when you run this report 
*:****************************************************************************************

*--Empty date not accepted.
IF EMPTY(ldStart)
  = gfModalGen('TRM00000B00000',.F.,.F.,.F.,'Start month cannot be empty. Cannot proceed!')
  RETURN
ENDIF

*E301901,1 AMH Variable to save the key of temp. table & Array to handling open/close files [Start]
PRIVATE lcRecKey
DECLARE laFlsOpn[17,3]   && Col.1 = File name, Col.2 = Index tag name, Col.3 = Opened (.T./.F.)
laFlsOpn[1,1] = 'Scale'
laFlsOpn[1,2] = 'Scale'

laFlsOpn[2,1] = 'Matinvjl'
laFlsOpn[2,2] = 'Matinvjl'

laFlsOpn[3,1] = 'Mmfgordh'
laFlsOpn[3,2] = 'Mmfgordd'

laFlsOpn[4,1] = 'Mmfgordd'
laFlsOpn[4,2] = 'Mmfgordd'

laFlsOpn[5,1] = 'pofhdr'
laFlsOpn[5,2] = 'pofhdr'

laFlsOpn[6,1] = 'Pofln'
laFlsOpn[6,2] = 'Poflnf'

laFlsOpn[7,1] = 'Bomline'
laFlsOpn[7,2] = 'Bomline'

laFlsOpn[8,1] = 'cuttkth'
laFlsOpn[8,2] = 'cuttkth'

laFlsOpn[9,1] = 'Cuttktl'
laFlsOpn[9,2] = 'Cuttktls'

laFlsOpn[10,1] = 'POSHDR'
laFlsOpn[10,2] = 'POSHDR'

laFlsOpn[11,1] = 'POSLN'
laFlsOpn[11,2] = 'POSLNs'

laFlsOpn[12,1] = 'FORCAST'
laFlsOpn[12,2] = 'FORCAST'

laFlsOpn[13,1] = 'FISHD'
laFlsOpn[13,2] = 'COMPFYEAR'

laFlsOpn[14,1] = 'BOMCOST'
laFlsOpn[14,2] = ''

laFlsOpn[15,1] = 'Ctktbom'
laFlsOpn[15,2] = 'Ctktyp'

laFlsOpn[16,1] = 'CUTPICK'
laFlsOpn[16,2] = 'CUTPICK'

laFlsOpn[17,1] = 'FSHLD'
laFlsOpn[17,2] = 'COMFYRHDAT'
*E301901,1 AMH [End]

lcIFabTyp = ''
lcITrmTyp = ''
lcMFabTyp = ''
lcMTrmTyp = ''
lcType    = ''
llMFIstall = (OCCURS('MF',gcCmpModules)<>0)
llPOIstall = (OCCURS('PO',gcCmpModules)<>0)
 
DIMENSION laSetups[5,2] 

laSetups[1,1]  = 'M_cMType1'
laSetups[2,1]  = 'M_cMType2'
laSetups[3,1]  = 'M_cMType3'
laSetups[4,1]  = 'M_cMType4'
laSetups[5,1]  = 'M_cMType5'
  
*-- Use gfGetMemVar once per all variables.
=gfGetMemVar(@laSetups,gcAct_Comp)

lcMType1   = laSetups[1,2]
lcMType2   = laSetups[2,2]
lcMType3   = laSetups[3,2]
lcMType4   = laSetups[4,2]
lcMType5   = laSetups[5,2]
lcPacks = ''
FOR lnCheck = 1 TO 5
  lcFabVar  = "lcMType" + STR(lnCheck,1)
  IF EVAL(lcFabVar) = 'F'
    lcMFabTyp = ALLTRIM(STR(lnCheck))
  ELSE
    IF EVAL(lcFabVar) = 'T'
      lcMTrmTyp = ALLTRIM(STR(lnCheck))
    ENDIF
  ENDIF
ENDFOR

DIMENSION laSetups[5,2] 

laSetups[1,1]  = 'M_cIType1'
laSetups[2,1]  = 'M_cIType2'
laSetups[3,1]  = 'M_cIType3'
laSetups[4,1]  = 'M_cIType4'
laSetups[5,1]  = 'M_cIType5'
  
*-- Use gfGetMemVar once per all variables.
=gfGetMemVar(@laSetups,gcAct_Comp)

lcIType1   = laSetups[1,2]
lcIType2   = laSetups[2,2]
lcIType3   = laSetups[3,2]
lcIType4   = laSetups[4,2]
lcIType5   = laSetups[5,2]
lcPacks = ''

FOR lnCheck = 1 TO 5
  lcFabVar  = "lcIType" + STR(lnCheck,1)
  IF EVAL(lcFabVar) = 'F'
    lcIFabTyp = ALLTRIM(STR(lnCheck ))
  ELSE
    IF EVAL(lcFabVar) = 'T'
      lcITrmTyp = ALLTRIM(STR(lnCheck))
    ENDIF
  ENDIF
ENDFOR

DIMENSION laFabType[4,1]
laFabType[1,1] = lcIFabTyp
laFabType[2,1] = lcITrmTyp
laFabType[3,1] = lcMFabTyp
laFabType[4,1] = lcMTrmTyp

*--mhm2004
*B038431,1 MHM 09/13/2004 (Begin) Check if there is item code structure or not
IF !lfSegInfo()
  RETURN
ENDIF
*B038431,1 MHM (End)

*--mhm2004
*--Initialize needed variables.
lcMjrPct  = gfItemMask('PM')
lcStyTtl  = gfItemMask('HI')
lCnMJPct  = gfItemMask('PN')

lnMajorLn = LEN(lcMjrPct)
lnNMjrLn  = LEN(lCnMJPct)

llMScale  = gfGetMemVar('M_USEEXSSC')
DIMENSION laMonths[9]
laMonths = ""
*--Get color segment information.
STORE 0 TO lnClrSrt,lnClrEnd,lnMonthEd

*B606145,1 AMH Add variables to hold the size posation and length [Start]
STORE 0 TO lnSizePos,lnSizeLen
llExtSizSc = gfGetMemVar('M_USEEXSSC',gcAct_Comp)
*B606145,1 AMH [End]

=lfGetColor()

*--Calc months end dates.
=lfGetStrt(ldStart)

*--Open needed files

DO lpOpenFile

*--Create MR temp file with structure.
IF USED(lcMatReq)
  SELECT (lcMatReq)
  ZAP
ELSE
  =lfCreateFl()
  = gfOpenFile(gcWorkDir+lcMatReq,'','EX')
  *--Open needed indeces.
  
  *E301901,1 AMH Change the index to support both summary and detailed layout [Start]
  *INDEX ON Item+IClr TAG Matreq ADDITIVE
  INDEX ON Item+IClr+cRecType+DTOS(DDATE) TAG Matreq ADDITIVE
  INDEX ON ITEM+ICLR+CRECTYPE+CTRANCD TAG MATREQ1 ADDITIVE
  *E301901,1 AMH [End]
  
ENDIF  

*E301901,1 AMH Create cursor to calculate the required by style/color in case of
*E301901,1     print both WIP and plan [Start]
IF lcRpBase = 'B'
  lcStMatReq = gfTempName()
  CREATE CURSOR (lcStMatReq) (CSTYLE C(19),NWIP1 N(7,0),NWIP2 N(7,0),NWIP3 N(7,0),NWIP4 N(7,0),;
                              NPLAN1 N(7,0),NPLAN2 N(7,0),NPLAN3 N(7,0),NPLAN4 N(7,0),;
                              NYEALD N(7,3),DLEADTIME1 D,DLEADTIME2 D,DLEADTIME3 D,DLEADTIME4 D)
  INDEX ON CSTYLE TAG (lcStMatReq) OF (lcStMatReq)
ENDIF
*E301901,1 AMH [End]

lcRpExp = STRTRAN(lcrpexp,'Bom.iclr','Bom.iclr,"******"')

*E301942,1 AMH Adjust lcRpExp for style colors [Start]
lcRpExp = STRTRAN(lcRpExp,'SUBSTR(Bom.citmmask,lnClrSrt,lnClrEnd)',;
                          'SUBSTR(Bom.citmmask,lnClrSrt,lnClrEnd),"'+;
                          REPLICATE('*',lnClrEnd)+'"')
*E301942,1 AMH [End]

*-Cut the Color from the filter.
lcClrExp = ""
lnClrPos = AT("INLIST(Bom.iclr",lcRpExp)
IF lnclrPos > 0
  lnExpEnd  = AT(')',SUBSTR(lcRpExp,lnClrPos))
  lcClrExp  = SUBSTR(lcRpExp,lnClrPos,lnExpEnd)
ENDIF
lcClrExp = STRTRAN(lcclrExp,'Bom.iclr','lcIClr')

IF !lfCollect()
  *-- Message : There are no records to display...!
  =gfModalGen('TRM00052B40011','ALERT')
  SET DEVICE TO SCREEN
  
  *E301901,1 AMH Comment the next line [Start]
  *RETURN
  *E301901,1 AMH [End]
  
ENDIF

*E301901,1 AMH Close opend files [Start]
FOR lnI = 1 TO ALEN(laFlsOpn,1)
  =lfOpenClse(lnI,.T.)
ENDFOR
*E301901,1 AMH [End]

*E301901,1 AMH Close the temp cursor of calculate the required by style [Start]
IF lcRpBase = 'B' .AND. TYPE('lcStMatReq')#'U' .AND. USED('lcStMatReq')
  USE IN (lcStMatReq)
ENDIF
*E301901,1 AMH [End]

*!*************************************************************
*! Name      : lfvFabric
*! Developer : RANIA ABDEL RAZIK (RAE)
*! Date      : 01/13/2002
*! Purpose   : Validate primary fabric and fabrics in range
*!             entered in grid.
*!*************************************************************
*! Calls     : FaBrow()
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfvFabric()
*!*************************************************************
FUNCTION lfvFabric

SET ORDER TO TAG FABRIC IN FABRIC
lcFabric = EVALUATE(SYS(18))
IF !EMPTY(lcOldValue) AND lcFabric = lcOldValue
  RETURN
ENDIF
lcFldLocNam = SYS(18)
IF !EMPTY(lcFabric) AND !SEEK(lcFabric,'FABRIC')
  = FaBrow(@lcFabric,'*')
  &lcFldLocNam = IIF(!EMPTY(lcFabric),lcFabric,lcOldValue)
ENDIF
RETURN

*!*************************************************************
*! Name      : lfOldValue
*! Developer : RANIA ABDEL RAZIK (RAE)
*! Date      : 01/13/2002
*! Purpose   : Function to store old value of the current filed.
*!*************************************************************
FUNCTION lfOldValue
lcOldValue = EVALUATE(SYS(18))
RETURN

*!*************************************************************
*! Name      : lfCreateFl
*! Developer : RANIA ABDEL RAZIK (RAE)
*! Date      : 01/13/2002
*! Purpose   : Create the report temp file.
*!*************************************************************
FUNCTION lfCreateFl

*E301901,1 AMH Add fields to detaile data [Start]
*DIMENSION laFlSruc[45,4]
DIMENSION laFlSruc[51,4]
*E301901,1 AMH Add fields to detaile data [End]

*--First array element [Name].
laFlSruc[1,1] = "STYLE"
laFlSruc[2,1] = "ITEM"
laFlSruc[3,1] = "ICLR"
laFlSruc[4,1] = "DESC"

laFlSruc[5,1]= "CURRHND"
laFlSruc[6,1]= "CURRORD"
laFlSruc[7,1]= "CURRREQ"
laFlSruc[8,1]= "CURRBAL"

laFlSruc[9,1 ]= "ONHND1"
laFlSruc[10,1]= "ONORD1"
laFlSruc[11,1]= "ONREQ1"
laFlSruc[12,1]= "ONBAL1"

laFlSruc[13,1]= "ONHND2"
laFlSruc[14,1]= "ONORD2"
laFlSruc[15,1]= "ONREQ2"
laFlSruc[16,1]= "ONBAL2"

laFlSruc[17,1]= "ONHND3"
laFlSruc[18,1]= "ONORD3"
laFlSruc[19,1]= "ONREQ3"
laFlSruc[20,1]= "ONBAL3"

laFlSruc[21,1]= "ONHND4"
laFlSruc[22,1]= "ONORD4"
laFlSruc[23,1]= "ONREQ4"
laFlSruc[24,1]= "ONBAL4"

laFlSruc[25,1]= "ONHND5"
laFlSruc[26,1]= "ONORD5"
laFlSruc[27,1]= "ONREQ5"
laFlSruc[28,1]= "ONBAL5"

laFlSruc[29,1]= "ONHND6"
laFlSruc[30,1]= "ONORD6"
laFlSruc[31,1]= "ONREQ6"
laFlSruc[32,1]= "ONBAL6"

laFlSruc[33,1]= "ONHND7"
laFlSruc[34,1]= "ONORD7"
laFlSruc[35,1]= "ONREQ7"
laFlSruc[36,1]= "ONBAL7"

laFlSruc[37,1]= "ONHND8"
laFlSruc[38,1]= "ONORD8"
laFlSruc[39,1]= "ONREQ8"
laFlSruc[40,1]= "ONBAL8"

laFlSruc[41,1]= "ONHND9"
laFlSruc[42,1]= "ONORD9"
laFlSruc[43,1]= "ONREQ9"
laFlSruc[44,1]= "ONBAL9"

laFlSruc[45,1]= "TOTAL"

*--Second array element [Type].
STORE "C" TO laFlSruc[1,2] ,laFlSruc[2,2] ,laFlSruc[3,2] ,laFlSruc[4,2]
STORE "N" TO laFlSruc[5,2] ,laFlSruc[6,2],laFlSruc[7,2],laFlSruc[8,2],;
             laFlSruc[9,2] ,laFlSruc[10,2],laFlSruc[11,2],laFlSruc[12,2],;
             laFlSruc[13,2],laFlSruc[14,2],laFlSruc[15,2],laFlSruc[16,2],;
             laFlSruc[17,2],laFlSruc[18,2],laFlSruc[19,2],laFlSruc[20,2],;
             laFlSruc[21,2],laFlSruc[22,2],laFlSruc[23,2],laFlSruc[24,2],;
             laFlSruc[25,2],laFlSruc[26,2],laFlSruc[27,2],laFlSruc[28,2],;
             laFlSruc[29,2],laFlSruc[30,2],laFlSruc[31,2],laFlSruc[32,2],;
             laFlSruc[33,2],laFlSruc[34,2],laFlSruc[35,2],laFlSruc[36,2],;
             laFlSruc[37,2],laFlSruc[38,2],laFlSruc[39,2],laFlSruc[40,2],;             
             laFlSruc[41,2],laFlSruc[42,2],laFlSruc[43,2],laFlSruc[44,2],;
             laFlSruc[45,2]

*--Third array element [Length].
STORE 19  TO laFlSruc[1,3] ,laFlSruc[2,3]
STORE  6  TO laFlSruc[3,3] 
STORE 20  TO laFlSruc[4,3]
STORE 12  TO laFlSruc[5,3] ,laFlSruc[6,3 ],laFlSruc[7,3],laFlSruc[8,3],;
             laFlSruc[9,3] ,laFlSruc[10,3],laFlSruc[11,3],laFlSruc[12,3],;
             laFlSruc[13,3],laFlSruc[14,3],laFlSruc[15,3],laFlSruc[16,3],;
             laFlSruc[17,3],laFlSruc[18,3],laFlSruc[19,3],laFlSruc[20,3],;
             laFlSruc[21,3],laFlSruc[22,3],laFlSruc[23,3],laFlSruc[24,3],;
             laFlSruc[25,3],laFlSruc[26,3],laFlSruc[27,3],laFlSruc[28,3],;
             laFlSruc[29,3],laFlSruc[30,3],laFlSruc[31,3],laFlSruc[32,3],;
             laFlSruc[33,3],laFlSruc[34,3],laFlSruc[35,3],laFlSruc[36,3],;
             laFlSruc[37,3],laFlSruc[38,3],laFlSruc[39,3],laFlSruc[40,3],;
             laFlSruc[41,3],laFlSruc[42,3],laFlSruc[43,3],laFlSruc[44,3],;
             laFlSruc[45,3]

*--Forth array element [Decemal].
STORE  0  TO laFlSruc[1,4] ,laFlSruc[2,4] ,laFlSruc[3,4] ,laFlSruc[4,4]
STORE  3  TO laFlSruc[5,4] ,laFlSruc[6,4] ,laFlSruc[7,4],laFlSruc[8,4],;
             laFlSruc[9,4] ,laFlSruc[10,4],laFlSruc[11,4],laFlSruc[12,4],;
             laFlSruc[13,4],laFlSruc[14,4],laFlSruc[15,4],laFlSruc[16,4],;
             laFlSruc[17,4],laFlSruc[18,4],laFlSruc[19,4],laFlSruc[20,4],;
             laFlSruc[21,4],laFlSruc[22,4],laFlSruc[23,4],laFlSruc[24,4],;
             laFlSruc[25,4],laFlSruc[26,4],laFlSruc[27,4],laFlSruc[28,4],;
             laFlSruc[29,4],laFlSruc[30,4],laFlSruc[31,4],laFlSruc[32,4],;
             laFlSruc[33,4],laFlSruc[34,4],laFlSruc[35,4],laFlSruc[36,4],;
             laFlSruc[37,4],laFlSruc[38,4],laFlSruc[39,4],laFlSruc[40,4],;
             laFlSruc[41,4],laFlSruc[42,4],laFlSruc[43,4],laFlSruc[44,4],;
             laFlSruc[45,4]

*E301901,1 AMH Add fields to detaile data [Start]
laFlSruc[46,1]= "CRECTYPE"
laFlSruc[46,2]= "C"
laFlSruc[46,3]= 1
laFlSruc[46,4]= 0

laFlSruc[47,1]= "CTYPE"
laFlSruc[47,2]= "C"
laFlSruc[47,3]= 7
laFlSruc[47,4]= 0

laFlSruc[48,1]= "CTRANCD"
laFlSruc[48,2]= "C"
laFlSruc[48,3]= 6
laFlSruc[48,4]= 0

laFlSruc[49,1]= "DDATE"
laFlSruc[49,2]= "D"
laFlSruc[49,3]= 8
laFlSruc[49,4]= 0

laFlSruc[50,1]= "CREF"
laFlSruc[50,2]= "C"
laFlSruc[50,3]= 16
laFlSruc[50,4]= 0

laFlSruc[51,1]= "CSTYLES"
laFlSruc[51,2]= "C"
laFlSruc[51,3]= 16
laFlSruc[51,4]= 0
*E301901,1 AMH [End]

CREATE DBF (gcWorkDir+lcMatReq) FROM ARRAY laFlSruc
RETURN

*!*************************************************************
*! Name      : lpOpenFiles
*! Developer : RANIA ABDEL RAZIK (RAE)
*! Date      : 01/10/2002
*! Purpose   : Open needed files.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpOpenFiles
*!*************************************************************
PROCEDURE lpOpenFiles

*E301901,1 AMH Check if files open before open it [Start]
*=gfOpenFile(gcDataDir+'Scale',gcDataDir+'Scale','SH')
*=gfOpenFile(gcDataDir+'Matinvjl',gcDataDir+'Matinvjl','SH')
*=gfOpenFile(gcDataDir+'Mmfgordh',gcDataDir+'Mmfgordd','SH')
*=gfOpenFile(gcDataDir+'Mmfgordd',gcDataDir+'Mmfgordd','SH')
*=gfOpenFile(gcDataDir+'pofhdr',gcDataDir+'pofhdr','SH')
*=gfOpenFile(gcDataDir+'Pofln',gcDataDir+'Poflnf','SH')
*=gfOpenFile(gcDataDir+'Bomline',gcDataDir+'Bomline','SH')
FOR lnI = 1 TO 7
  =lfOpenClse(lnI)
ENDFOR
*E301901,1 AMH [End]


IF llMFIstall 
  
  *E301901,1 AMH Check if files open before open it [Start]
  *=gfOpenFile(gcDataDir+'cuttkth',gcDataDir+'cuttkth','SH')
  *=gfOpenFile(gcDataDir+'Cuttktl',gcDataDir+'CuttktlS','SH')
  =lfOpenClse(8)
  =lfOpenClse(9)
  *E301901,1 AMH [End]
  
  SET RELATION TO CUTTKTL.cuttkt INTO CUTTKTH
ENDIF

IF llPOIstall 
  
  *E301901,1 AMH Check if files open before open it [Start]
  *=gfOpenFile(gcDataDir+'POSHDR','Poshdr','SH')
  *=gfOpenFile(gcDataDir+'POSLN','Poslns','SH')
  =lfOpenClse(10)
  =lfOpenClse(11)
  *E301901,1 AMH [End]
  
  SET RELATION TO POSLN.cstytype + POSLN.po INTO POSHDR
ENDIF

*E301901,1 AMH Open the forcast file to get the plan required quantity
*E301901,1     & fishd file to get the first working day [Start]
=lfOpenClse(12)
=lfOpenClse(13)
=lfOpenClse(16)
=lfOpenClse(17)
*E301901,1 AMH [Start]

*E301901,1 AMH Check if files open before open it [Start]
*=gfOpenFile(gcDataDir+'BOMCOST','','SH')
*=gfOpenFile(gcDataDir+'Ctktbom','Ctktyp','SH')
=lfOpenClse(14)
=lfOpenClse(15)
*E301901,1 AMH [Start]

SET ORDER TO TAG BOM IN BOM
SET ORDER TO TAG FABRIC IN FABRIC

*!*************************************************************
*! Name      : lfCollect
*:     Date  : 12/24/2001
*:  Developer: RANIA ABDEL RAZIK (RAE)
*! Purpose   : Collect data.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfCollect()
*!*************************************************************
FUNCTION lfCollect

*--Get styles having OG Fabrics/Colors
IF !lfStyles()
  RETURN .F.
ENDIF

*E301901,1 AMH Add variable to hold the differance between 
*E301901,1     sunday and the frist working day [Start]
llFound   = .F.
lnWorkDay = 0
lcCurYear = SPACE(4)

SELECT FISHD
lcYear = STR(YEAR(gdSysDate),4)
IF SEEK(lcYear) AND CFISYSTAT = 'C'
  llFound = .T.
ELSE
  LOCATE FOR CFISYSTAT = 'C'
  llFound = FOUND()
ENDIF
  
IF llFound
  IF EMPTY(CFISNONWD)
    lnWorkDay = 1
  ELSE
    FOR lnI = 1 TO LEN(CFISNONWD)
      lnWorkDay = MAX(lnWorkDay,VAL(SUBSTR(CFISNONWD,lnI,1))+1)
    ENDFOR
    lnWorkDay = MOD(lnWorkDay,7)
  ENDIF
  lcCurYear = CFISFYEAR
ENDIF
*E301901,1 AMH [End]

*--Get OnHnad for OG Fabrics/Colors.
DO lpAllQty
SET FILTER TO TOTAL <> 0
LOCATE
IF EOF()
  SET FILTER TO
  RETURN .F.
ENDIF
DO gfDispRe WITH EVAL('lcRpRName')
SET FILTER TO

*!*************************************************************
*! Name      : lfStyles
*: Date      : 01/10/2002
*: Developer : RANIA ABDEL RAZIK (RAE)
*! Purpose   : Collect Styles.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfStyles()
*!*************************************************************
FUNCTION lfStyles

*E301942,1 AMH To get the selected style or style color if any [Start]
lcStylFile = ''
llUseStyle = .F.
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'STYLE.CSTYMAJOR'),1)
IF lnPosition > 0
  lcStylFile = laOGFxFlt[lnPosition,6]
  llUseStyle = IIF(!EMPTY(lcStylFile) .AND. USED(lcStylFile) .AND. RECCOUNT(lcStylFile)>0,.T.,.F.)
ENDIF

lcColors = ''
llUseClr = .F.
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'SUBSTR(Bom.citmmask,lnClrSrt,lnClrEnd)'),1)
IF lnPosition > 0
  lcColors = laOGFxFlt[lnPosition,6]
  llUseClr = !EMPTY(lcColors)
ENDIF

IF llUseStyle
  SELECT (lcStylFile)
  SCAN
    IF SEEK(SUBSTR(CSTYMAJOR,1,lnMajorLn),'STYLE')
      SELECT STYLE
      SCAN REST WHILE STYLE = SUBSTR(EVALUATE(lcStylFile+'.CSTYMAJOR'),1,lnMajorLn);
                  FOR IIF(llUseClr,SUBSTR(STYLE,lnClrSrt,lnClrEnd) $ lcColors,.T.)
        *--Check if the style has a cost sheet.
        IF !SEEK(SUBSTR(STYLE.Style,1,lnMajorLn),'BOM')
          LOOP
        ENDIF
        *--Current style code and style scale.
        lcCStyle = STYLE.Style
        lnCnt    = IIF( SEEK("S"+STYLE.Scale,'SCALE'), SCALE.Cnt , 8 )
        WAIT WINDOW 'Collecting the cost sheet information for Style : '+lcCStyle NOWAIT
        SELECT BOM
  
        SCAN REST WHILE citmmajor+typ+citmmask+mfgcode+item+iclr = SUBSTR(lcCStyle,1,lnMajorLn) ;
               FOR LIKE(STRTRAN(cItmMask,'*','?'),lcCStyle) AND ;
                   cCatgTyp $ 'FT'
          *--Check if maririal/color entered in grid is valid.
          IF (cCatgTyp = 'F' OR (cCatgTyp='T' AND Trim_Invt)) AND !( &lcRpExp) 
            LOOP
          ENDIF
          *--Don't include non inventory trims if not setup to use it.
          IF (cCatgTyp='T' AND !Trim_Invt)
            LOOP
          ENDIF
          *--Read an item code and an item color[lcItem,lcIClr].
          STORE '' TO lcItem,lcIClr
          IF !lfReadItem()
            LOOP
          ENDIF
          
          IF !EMPTY(lcClrExp)
            IF !(&lcClrExp )
              LOOP
            ENDIF
          ENDIF
          
          *--Append or Update record in (lcMatReq) file. 
          =lfUpdReq()
        ENDSCAN
      ENDSCAN
    ENDIF
  ENDSCAN
ELSE
*E301942,1 AMH [End]

SELECT STYLE
LOCATE

SCAN

  *E301942,1 AMH Check the style color [Start]
  IF llUseClr .AND. !(SUBSTR(STYLE,lnClrSrt,lnClrEnd) $ lcColors)
    LOOP
  ENDIF
  *E301942,1 AMH [End]

  *--Check if the style has a cost sheet.
  IF !SEEK(SUBSTR(STYLE.Style,1,lnMajorLn),'BOM')
    LOOP
  ENDIF
  *--Current style code and style scale.
  lcCStyle = STYLE.Style
  lnCnt    = IIF( SEEK("S"+STYLE.Scale,'SCALE'), SCALE.Cnt , 8 )
  WAIT WINDOW 'Collecting the cost sheet information for Style : '+lcCStyle NOWAIT
  SELECT BOM
  
  *E301901,1 AMH Don't need to collect style comp. [Start]
  *SCAN REST WHILE citmmajor+typ+citmmask+mfgcode+item+iclr = SUBSTR(lcCStyle,1,lnMajorLn) ;
         FOR LIKE(STRTRAN(cItmMask,'*','?'),lcCStyle) AND ;
             cCatgTyp $ 'FTS'
  SCAN REST WHILE citmmajor+typ+citmmask+mfgcode+item+iclr = SUBSTR(lcCStyle,1,lnMajorLn) ;
         FOR LIKE(STRTRAN(cItmMask,'*','?'),lcCStyle) AND ;
             cCatgTyp $ 'FT'
  *E301901,1 AMH [End]
  
    *--Check if maririal/color entered in grid is valid.
    IF (cCatgTyp = 'F' OR (cCatgTyp='T' AND Trim_Invt)) AND !( &lcRpExp) 
      LOOP
    ENDIF
    *--Don't include non inventory trims if not setup to use it.
    IF (cCatgTyp='T' AND !Trim_Invt)
      LOOP
    ENDIF
    *--Read an item code and an item color[lcItem,lcIClr].
    STORE '' TO lcItem,lcIClr
    IF !lfReadItem()
      LOOP
    ENDIF 
    
    IF !EMPTY(lcClrExp)
      IF !(&lcClrExp )
        LOOP
      ENDIF
    ENDIF 
    
    *--Append or Update record in (lcMatReq) file. 
    =lfUpdReq()
  ENDSCAN
ENDSCAN

*E301942,1 AMH End if get selected styles [Start]
ENDIF
*E301942,1 AMH [End]

WAIT CLEAR
RETURN RECCOUNT(lcMatReq) > 0

*!*************************************************************
*! Name      : lfReadItem
*! Developer : RANIA ABDEL RAZIK (RAE)
*! Date      : 01/10/2002
*! Purpose   : Read item.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfReadItem()
*!*************************************************************
FUNCTION lfReadItem

*--Get style component code.
IF cCatGTyp = 'S'
 *--If style size not used in cost sheet.
 IF !(STYLE.Scale $ BOM.MSIZES)
   RETURN .F.
 ENDIF
 *--Get an equevelent item non major part.
 lcCompNmj=''
 *--Non major length without scale segment.
 lnNmjPart = lnNMjrLn - IIF(llMScale,3,0)
 FOR lnI = 1 TO lnNmjPart
   lcCutChr  = SUBSTR(BOM.Item ,lnMajorLn+1+lnI,1)
   lcCompNmj = lcCompNmj + ;
      IIF(lcCutChr='*',SUBSTR(STYLE.Style, lnMajorLn+1+lnI ,1),lcCutChr)
 ENDFOR
 lcItem = SUBSTR(Item,1,lnMajorLn+1)+lcCompNmj
 *--Get an equevelent item scale part.       
 IF llMScale
   lcEqSCSz = STYLE.Scale
   FOR lnMI = 1 TO MEMLINES(BOM.MSZCROSREF)
     lcMemSLine = MLINE(BOM.MSZCROSREF,lnMI)
     IF STYLE.Scale $ lcMemSLine
       lcEqSCSz = SUBSTR(lcMemSLine,AT('~',lcMemSLine)+1,3)
       EXIT
     ENDIF 
   ENDFOR
   lcItem = lcItem + lcEqSCSz
 ENDIF
 *--Check existance of style component.
 lnRcSv=IIF(!EOF('STYLE'),RECNO('STYLE'),0)
 SELECT STYLE
 lcStyOrder=ORDER('STYLE')
 SET ORDER TO TAG Style
 llSComFund = SEEK(lcItem,'STYLE')
 IF lnRcSv<>0
   GOTO lnRcSv IN STYLE
 ENDIF
 SET ORDER TO &lcStyOrder
 SELECT BOM
 IF ! llSComFund
   RETURN .F.
 ENDIF   
 lcIClr = ''
ELSE  && CCATGTYP $ 'FT'
  lcItem = BOM.Item
  lcIClr = IIF(IClr='*',SUBSTR(lcCStyle,lnClrSrt,lnClrEnd),BOM.IClr)
ENDIF
RETURN .T.

*!*************************************************************
*! Name      : lfUpdReq
*: Date      : 01/10/2002
*: Developer : RANIA ABDEL RAZIK (RAE)
*! Purpose   : Collect Styles.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfStyles()
*!*************************************************************
FUNCTION lfUpdReq

PRIVATE lnAlias
lnAlias = SELECT()
SELECT (lcMatReq)
IF !SEEK(lcItem+lcIClr)
  APPEND BLANK  
  *REPLACE Style    WITH lcCStyle     ,;
          Item     WITH lcItem       ,;
          IClr     WITH lcIClr       ,;
          Desc     WITH BOM.Desc     
  REPLACE Item     WITH lcItem       ,;
          IClr     WITH lcIClr       ,;
          Desc     WITH BOM.Desc     
ENDIF
SELECT (lnAlias)

*!*************************************************************
*! Name      : lpAllQty
*! Developer : RANIA ABDEL RAZIK (RAE)
*! Date      : 01/10/2002
*! Purpose   : Get All Qty.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpOpenFiles
*!*************************************************************
PROCEDURE lpAllQty

SELECT (lcMatReq)
SCAN

  *E301901,1 AMH Get the frist day of the period [Start]
  *ldThisDate = ldStart - DAY(ldStart)
  ldThisDate = lfWeekStr(ldStart)
  lcRecKey = EVALUATE(KEY())
  IF CRECTYPE = 'D'
    LOOP
  ENDIF
  *E301901,1 AMH [End]
  
  *--Calc Current
  =lfGetAll(.T.,ldThisDate,"CURRHND","CURRORD","CURRREQ","CURRBAL","ONHND1")
  *--Calc Next 9 months
  
  *E301901,1 AMH Comment the next line [Start]
  *ldThisDate = ldThisDate+lnMonthEd
  *E301901,1 AMH [End]
  
	  FOR lnFrom = 1 TO 9
    *--Get months titles for the first Item/color only as they are fixed.
    IF EMPTY(laMonths[lnFrom])
      laMonths[lnFrom]= DTOC(ldThisDate)
    ENDIF  
    =lfGetAll(.F.,ldThisDate,"ONHND"+STR(lnFrom,1),"ONORD"+STR(lnFrom,1),"ONREQ"+STR(lnFrom,1),"ONBAL"+STR(lnFrom,1),;
              IIF(lnFrom<9,"ONHND"+STR(lnFrom+1,1),""))
    
    *E301901,1 AMH Not need to recalculate the end of period [Start]
    *=lfGetStrt(ldThisDate+1)
    *E301901,1 AMH [End]
    
    ldThisDate = ldThisDate+lnMonthEd
  ENDFOR
  SELECT (lcMatReq)
ENDSCAN

*!*************************************************************
*! Name      : lfGetAll
*! Developer : RANIA ABDEL RAZIK (RAE)
*! Date      : 01/14/2002
*! Purpose   : Get all Qty.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfGetAll()
*!*************************************************************
FUNCTION lfGetAll
PARAMETERS llCurr,ldDate,FrstFld,ScndFld,ThrdFld,FrthFld,FithFld

*--Get the first day of the month
ldFrstDay = ldDate-DAY(ldDate-1)

*--No date enterd or it's today, Today is the start date.
IF llCurr
  *wab
  ldOldDate = ldFrstDay
  ldFrstDay = {01/01/1900}
  *wab
  IF SEEK(PADR(Item,7)+ICLR,'Fabric')
    *--Get OnOrder
    IF Fabric.Make
      lnOnOrder = 0
      SELECT MMfgOrdH
      IF SEEK(PADR(&lcMatReq..Item,7))
        
        *E301901,1 AMH get orders delivered before the first period [Start]
        *SCAN REST WHILE  cfabric+cmfgordno = PADR(&lcMatReq..Item,7)  FOR ;
             Complete <= ldDate .AND. STATUS $ 'OH'
        SCAN REST WHILE  cfabric+cmfgordno = PADR(&lcMatReq..Item,7)  FOR ;
             Complete < ldDate .AND. STATUS $ 'OH'
        *E301901,1 AMH [End]
        
          SELECT MMfgOrdd
          lnMMOrder = 0
          IF SEEK(MMfgOrdH.cmfgordno+PADR(&lcMatReq..Item,7)+&lcMatReq..ICLR)          
            SCAN REST WHILE cmfgordno+cfabric+color+dyelot+trancd = MMfgOrdH.cmfgordno+PADR(&lcMatReq..Item,7)+&lcMatReq..ICLR
              lnMMOrder = lnMMOrder +   ( nMFGTotQty * IIF(TranCd='1',1,-1) )
            ENDSCAN
            
            *E301901,1 AMH Add detaile data [Start]

            *B606107,1 KHM 06/13/2002 (Begin) Converting the buy to use
            lnMMOrder = lnMMOrder * Fabric.Conv
            *B606107,1 KHM 06/13/2002 (End)

            IF lcRpRName = 'MATIMRQD'

              SELECT (lcMatReq)
              APPEND BLANK              
              
              REPLACE ITEM      WITH SUBSTR(lcRecKey,1,19),;
                      ICLR      WITH SUBSTR(lcRecKey,20,6),;
                      CRECTYPE  WITH 'D',;
                      CTYPE     WITH 'M.M.O.',;
                      DDATE     WITH MMFGORDH.COMPLETE,;
                      CTRANCD   WITH MMFGORDH.CMFGORDNO,;
                      TOTAL     WITH MAX(lnMMOrder,0),;
                      &FrstFld. WITH MAX(lnMMOrder,0)
            ENDIF
            *E301901,1 AMH [End]
            
            lnOnOrder = lnOnOrder + MAX(lnMMOrder,0)
          ENDIF
        ENDSCAN
      ENDIF
    ELSE
      SELECT POFLN
      lnOnOrder = 0
      
      *E301901,1 AMH Variable to save the last Mat. PO added [Start]
      lcPoMat = SPACE(7)
      *E301901,1 AMH [End]
      
      IF SEEK(PADR(&lcMatReq..Item,7)+&lcMatReq..ICLR)
        SCAN REST WHILE fabric+color+cmattype+pomat+trancd = PADR(&lcMatReq..Item,7)+&lcMatReq..ICLR 
          
          *E301901,1 AMH get orders delivered before the first period [Start]
          *IF SEEK(cMatType+PoMAt,'POFHDR') AND POFHDR.Status $'OH' AND POFHDR.Complete <=ldDate AND cMatType $ 'PR' 
          IF SEEK(cMatType+PoMAt,'POFHDR') AND POFHDR.Status $'OH' AND POFHDR.Complete <ldDate AND cMatType $ 'PR' 
          *E301901,1 AMH [End]
          
            *E301901,1 AMH Add detaile data [Start]
            IF CMATTYPE+POMAT <> lcPoMat
              IF !EMPTY(lcPoMat) .AND. lcRpRName = 'MATIMRQD'
                *B606107,1 KHM 06/13/2002 (Begin) Converting the buy to use
                lnMatPo = lnMatPo * Fabric.Conv
                *B606107,1 KHM 06/13/2002 (End)

                SELECT (lcMatReq)
                APPEND BLANK
                REPLACE ITEM      WITH SUBSTR(lcRecKey,1,19),;
                        ICLR      WITH SUBSTR(lcRecKey,20,6),;
                        CRECTYPE  WITH 'D',;
                        CTYPE     WITH 'Mat. PO',;
                        DDATE     WITH ldCompDate,;
                        CTRANCD   WITH SUBSTR(lcPoMat,2),;
                        TOTAL     WITH lnMatPo,;
                        &FrstFld. WITH lnMatPo
                SELECT POFLN
              ENDIF
              lcPoMat = CMATTYPE+POMAT
              lnMatPo = 0
              ldCompDate = POFHDR.COMPLETE
            ENDIF
            IF cMatType = 'P' 
              lnMatPo = lnMatPo +  ( nFabTOTQTY * IIF(TranCd='1',1,-1) )
            ELSE
              lnMatPo = lnMatPo +  ( nFabTOTQTY * IIF(TranCd='1',-1,1) )
            ENDIF
            *E301901,1 AMH [End]
            
            IF cMatType = 'P' 
              lnOnOrder = lnOnOrder +  ( nFabTOTQTY * IIF(TranCd='1',1,-1) )
            ELSE
              lnOnOrder = lnOnOrder +  ( nFabTOTQTY * IIF(TranCd='1',-1,1) )
            ENDIF
          ENDIF
        ENDSCAN
        
        *E301901,1 AMH Add detaile data [Start]
        IF !EMPTY(lcPoMat) .AND. lcRpRName = 'MATIMRQD'
           *B606107,1 KHM 06/13/2002 (Begin) Converting the buy to use
           lnMatPo = lnMatPo * Fabric.Conv
           *B606107,1 KHM 06/13/2002 (End)

          SELECT (lcMatReq)
          APPEND BLANK
          REPLACE ITEM      WITH SUBSTR(lcRecKey,1,19),;
                  ICLR      WITH SUBSTR(lcRecKey,20,6),;
                  CRECTYPE  WITH 'D',;
                  CTYPE     WITH 'Mat. PO',;
                  DDATE     WITH ldCompDate,;
                  CTRANCD   WITH SUBSTR(lcPoMat,2),;
                  TOTAL     WITH lnMatPo,;
                  &FrstFld. WITH lnMatPo
        ENDIF
        *E301901,1 AMH [End]
        
      ENDIF
      
      *B606107,1 KHM 06/13/2002 (Begin) Converting the buy to use
      lnOnOrder = lnOnOrder * Fabric.Conv
      *B606107,1 KHM 06/13/2002 (End)

    ENDIF
    
    *E301901,1 AMH Calculate the required at the correct interval of date [Start]
    *lnRequied = lfCalReq(ldDate)
    lnRequied = lfCalReq({},ldDate+6)
    *E301901,1 AMH [End]
    
    SELECT (lcMatReq)    
    
    *E301901,1 AMH Calculate the correct total [Start]
    *REPLACE &FrstFld WITH Fabric.OnHand,;
            &ScndFld WITH lnOnOrder;
            &ThrdFld WITH lnRequied;
            &FrthFld WITH Fabric.OnHand+lnOnOrder-lnRequied;
            &FithFld WITH Fabric.OnHand+lnOnOrder-lnRequied;
            TOTAL    WITH TOTAL + &FrstFld+&ScndFld+&ThrdFld+&FrthFld+&FithFld
    =SEEK(lcRecKey)
    REPLACE &FrstFld WITH Fabric.OnHand,;
            &ScndFld WITH lnOnOrder,;
            &ThrdFld WITH lnRequied,;
            &FrthFld WITH Fabric.OnHand+lnOnOrder-lnRequied,;
            &FithFld WITH Fabric.OnHand+lnOnOrder-lnRequied,;
            TOTAL    WITH TOTAL + &FrstFld+&ScndFld+&ThrdFld,;
            CTYPE    WITH 'On Hand'
    *E301901,1 AMH [End]
    
  ENDIF
  *Wab
  ldFrstDay =   ldOldDate 
  *WAB
ELSE
  *--Get OnOrder
  =SEEK(PADR(&lcMatReq..Item,7)+&lcMatReq..ICLR,'Fabric')
  IF Fabric.Make
    lnOnOrder = 0
    SELECT MMfgOrdH
    IF SEEK(PADR(&lcMatReq..Item,7))
       
       *E301901,1 AMH get orders delivered at period [Start]
       *SCAN REST WHILE  cfabric+cmfgordno = PADR(&lcMatReq..Item,7)  FOR ;
           BETWEEN(Complete,ldFrstDay,ldDate) .AND. STATUS $ 'OH'
       SCAN REST WHILE  cfabric+cmfgordno = PADR(&lcMatReq..Item,7)  FOR ;
           BETWEEN(Complete,ldDate,ldDate+IIF(EMPTY(FithFld),365.25*4000,lnMonthEd-1)) .AND. STATUS $ 'OH'
       *E301901,1 AMH [End]
       
        SELECT MMfgOrdd
        lnMMOrder = 0
        IF SEEK(MMfgOrdH.cmfgordno+PADR(&lcMatReq..Item,7)+&lcMatReq..ICLR)
          SCAN REST WHILE cmfgordno+cfabric+color+dyelot+trancd = MMfgOrdH.cmfgordno+PADR(&lcMatReq..Item,7)+&lcMatReq..ICLR
            lnMMOrder = lnMMOrder +   ( nMFGTotQty * IIF(TranCd='1',1,-1) )
          ENDSCAN

          *B606107,1 KHM 06/13/2002 (Begin) Converting the buy to use
          lnMMOrder = lnMMOrder * Fabric.Conv
          *B606107,1 KHM 06/13/2002 (End)
          
          *E301901,1 AMH Add detaile data [Start]
          IF lcRpRName = 'MATIMRQD'

            SELECT (lcMatReq)
            APPEND BLANK
            REPLACE ITEM      WITH SUBSTR(lcRecKey,1,19),;
                    ICLR      WITH SUBSTR(lcRecKey,20,6),;
                    CRECTYPE  WITH 'D',;
                    CTYPE     WITH 'M.M.O.',;
                    DDATE     WITH MMFGORDH.COMPLETE,;
                    CTRANCD   WITH MMFGORDH.CMFGORDNO,;
                    TOTAL     WITH MAX(lnMMOrder,0),;
                    &FrstFld. WITH MAX(lnMMOrder,0)
          ENDIF
          *E301901,1 AMH [End]
          
          lnOnOrder = lnOnOrder + MAX(lnMMOrder,0)
        ENDIF
      ENDSCAN
    ENDIF 
  ELSE
    SELECT POFLN
    lnOnOrder = 0
    
    *E301901,1 AMH Variable to save the last Mat. PO added [Start]
    lcPoMat = SPACE(7)
    *E301901,1 AMH [End]
    
    IF SEEK(PADR(&lcMatReq..Item,7)+&lcMatReq..ICLR)
      SCAN REST WHILE fabric+color+cmattype+pomat+trancd = PADR(&lcMatReq..Item,7)+&lcMatReq..ICLR 
        
        *E301901,1 AMH get orders delivered at period [Start]
        *IF SEEK(cMatType+PoMAt,'POFHDR') AND POFHDR.Status $'OH' AND BETWEEN(POFHDR.Complete,ldFrstDay,ldDate) AND cMatType $ 'PR' 
        IF SEEK(cMatType+PoMAt,'POFHDR') AND POFHDR.Status $'OH' AND;
           BETWEEN(POFHDR.Complete,ldDate,ldDate+IIF(EMPTY(FithFld),365.25*4000,lnMonthEd-1)) AND cMatType $ 'PR'
        *E301901,1 AMH [End]
        
          *E301901,1 AMH Add detaile data [Start]
          IF CMATTYPE+POMAT <> lcPoMat
            IF !EMPTY(lcPoMat) .AND. lcRpRName = 'MATIMRQD'
              *B606107,1 KHM 06/13/2002 (Begin) Converting the buy to use
              lnMatPo = lnMatPo * Fabric.Conv
              *B606107,1 KHM 06/13/2002 (End)

              SELECT (lcMatReq)
              APPEND BLANK
              REPLACE ITEM      WITH SUBSTR(lcRecKey,1,19),;
                      ICLR      WITH SUBSTR(lcRecKey,20,6),;
                      CRECTYPE  WITH 'D',;
                      CTYPE     WITH 'Mat. PO',;
                      DDATE     WITH ldCompDate,;
                      CTRANCD   WITH SUBSTR(lcPoMat,2),;
                      TOTAL     WITH lnMatPo,;
                      &FrstFld. WITH lnMatPo
              SELECT POFLN
            ENDIF
            lcPoMat = CMATTYPE+POMAT
            lnMatPo = 0
            ldCompDate = POFHDR.COMPLETE
          ENDIF
          IF cMatType = 'P' 
            lnMatPo = lnMatPo +  ( nFabTOTQTY * IIF(TranCd='1',1,-1) )
          ELSE
            lnMatPo = lnMatPo +  ( nFabTOTQTY * IIF(TranCd='1',-1,1) )
          ENDIF
          *E301901,1 AMH [End]

          IF cMatType = 'P' 
            lnOnOrder = lnOnOrder +  ( nFabTOTQTY * IIF(TranCd='1',1,-1) )
          ELSE
            lnOnOrder = lnOnOrder +  ( nFabTOTQTY * IIF(TranCd='1',-1,1) )
          ENDIF
        ENDIF
      ENDSCAN
      
      *E301901,1 AMH Add detaile data [Start]
      IF !EMPTY(lcPoMat) .AND. lcRpRName = 'MATIMRQD'

        *B606107,1 KHM 06/13/2002 (Begin) Converting the buy to use
        lnMatPo = lnMatPo * Fabric.Conv
        *B606107,1 KHM 06/13/2002 (End)

        SELECT (lcMatReq)
        APPEND BLANK
        REPLACE ITEM      WITH SUBSTR(lcRecKey,1,19),;
                ICLR      WITH SUBSTR(lcRecKey,20,6),;
                CRECTYPE  WITH 'D',;
                CTYPE     WITH 'Mat. PO',;
                DDATE     WITH ldCompDate,;
                CTRANCD   WITH SUBSTR(lcPoMat,2),;
                TOTAL     WITH lnMatPo,;
                &FrstFld. WITH lnMatPo
      ENDIF
      *E301901,1 AMH [End]
      
    ENDIF
    *B606107,1 KHM 06/13/2002 (Begin) Converting the buy to use
    lnOnOrder = lnOnOrder * Fabric.Conv
    *B606107,1 KHM 06/13/2002 (End)
  
  ENDIF
  *--Get Required
  
  *E301901,1 AMH Calculate the required at the correct interval of date [Start]
  *lnRequied = lfCalReq(ldDate)
  lnRequied = lfCalReq(ldDate+7,ldDate+IIF(EMPTY(FithFld),365.25*4000,lnMonthEd+6))
  *E301901,1 AMH [End]
  
  *--Update
  SELECT (lcMatReq)
  
  *E301901,1 AMH Update in the correct record [Start]
  =SEEK(lcRecKey)
  *E301901,1 AMH [End]
  
  REPLACE &ScndFld WITH lnOnOrder;
          &ThrdFld WITH lnRequied;
          &FrthFld WITH &FrstFld+lnOnOrder-lnRequied
  IF !EMPTY(FithFld)
    REPLACE &FithFld WITH &FrstFld+lnOnOrder-lnRequied
    
    *E301901,1 AMH Not need to add the fithfld to total [Start]
    *REPLACE TOTAL WITH TOTAL + &FithFld
    *E301901,1 AMH [End]
  ENDIF
  
  *E301901,1 AMH Calculate the correct total [Start]
  *REPLACE TOTAL WITH TOTAL + &ScndFld+&ThrdFld+&FrthFld
  REPLACE TOTAL WITH TOTAL + &FrstFld+&ScndFld+&ThrdFld
  *E301901,1 AMH [End]
  
ENDIF

*!*************************************************************
*! Name      : lfCalReq
*! Developer : RANIA ABDEL RAZIK (RAE)
*! Date      : 01/14/2002
*! Purpose   : Get Required Qty.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfCalReq()
*!*************************************************************
FUNCTION lfCalReq
*E301901,1 AMH Add parameter to the end date [Start]
*PARAMETERS ldStrDat
PARAMETERS ldStrDat,ldEndDat
*E301901,1 AMH [End]

PRIVATE lnAlias
lnAlias = SELECT()
lnRequied = 0
lnMFReq   = 0
lnPoReq   = 0

*E301901,1 AMH Calculate the required based on (WIP/Plan/Both) [Start]
lnPlanReq = 0
IF lcRpBase = 'B'
  IF lnMonthEd/7 > 1
    DECLARE laWeeks[lnMonthEd/7,2]
    STORE 0 TO laWeeks
  ENDIF
  SELECT (lcStMatReq)
  ZAP
ENDIF
IF lcRpBase $ 'WB'
*E301901,1 AMH [End]

SELECT Ctktbom
IF llMFIstall
  
  *E301901,1 AMH Calculate the required at the correct interval of date [Start]
  *SUM (Req_Qty-Issue_Qty) TO lnMFReq FOR ITEM + ICLR = PADR(&lcMatReq..Item,19)+&lcMatReq..ICLR;
  AND SEEK(CUTTKT,'CUTTKTH') AND CutTktH.Status <> 'X' AND BETWEEN(CutTktH.Complete,ldFrstDay,ldStrDat);
  AND (Req_Qty-Issue_Qty) >0
  SET ORDER TO CUTTKTL IN CUTTKTL
  SCAN FOR ITEM + ICLR = PADR(&lcMatReq..Item,19)+&lcMatReq..ICLR
    IF SEEK(CUTTKT,'CUTTKTH') .AND. !(CutTktH.Status $ 'SX') .AND. SEEK(CUTTKT,'CUTTKTL')
      lnLeadTime = 0
      SELECT CUTTKTL
      SCAN REST WHILE CUTTKT = CTKTBOM.CUTTKT
        IF SEEK(STYLE,'STYLE')
          lnLeadTime = MAX(lnLeadTime,STYLE.LEADTIME)
        ENDIF
      ENDSCAN
      SELECT CTKTBOM
      IF SEEK(CUTTKT,'CUTTKTH')
        ldLeadTime = lfHolDay(CutTktH.Complete-lnLeadTime,CutTktH.Complete)
        IF BETWEEN(ldLeadTime,ldStrDat,ldEndDat)
          IF lcRpRName = 'MATIMRQD'
            SELECT (lcMatReq)
            APPEND BLANK
            REPLACE ITEM      WITH SUBSTR(lcRecKey,1,19),;
                    ICLR      WITH SUBSTR(lcRecKey,20,6),;
                    CRECTYPE  WITH 'D',;
                    CTYPE     WITH 'C/T',;
                    DDATE     WITH ldLeadTime-7,;
                    CTRANCD   WITH CUTTKTH.CUTTKT,;
                    CSTYLES   WITH SUBSTR(CUTTKTH.STYLE,1,lnMajorLn),;
                    CREF      WITH lfGetRef('1'+CUTTKTH.CUTTKT),;
                    TOTAL     WITH -MAX(CTKTBOM.Req_Qty-CTKTBOM.Issue_Qty,0),;
                    &FrstFld. WITH -MAX(CTKTBOM.Req_Qty-CTKTBOM.Issue_Qty,0)
            SELECT CTKTBOM
          ENDIF
          lnMFReq = lnMFReq + MAX(Req_Qty-Issue_Qty,0)
          IF lcRpBase = 'B' .AND. lnMonthEd/7 > 1
            DO CASE
              CASE BETWEEN(ldLeadTime,ldStrDat,ldStrDat+6) .OR. EMPTY(ldStrDat)
                laWeeks[1,1] = laWeeks[1,1] + MAX(Req_Qty-Issue_Qty,0)
              CASE BETWEEN(ldLeadTime,ldStrDat+7,IIF(EMPTY(FithFld) .AND. lnMonthEd=14,ldEndDat,ldStrDat+13))
                laWeeks[2,1] = laWeeks[2,1] + MAX(Req_Qty-Issue_Qty,0)
              CASE lnMonthEd = 28 .AND. BETWEEN(ldLeadTime,ldStrDat+14,ldStrDat+20)
                laWeeks[3,1] = laWeeks[3,1] + MAX(Req_Qty-Issue_Qty,0)
              CASE lnMonthEd = 28 .AND. BETWEEN(ldLeadTime,ldStrDat+21,IIF(EMPTY(FithFld),ldEndDat,ldStrDat+27))
                laWeeks[4,1] = laWeeks[4,1] + MAX(Req_Qty-Issue_Qty,0)
            ENDCASE
          ENDIF
          IF lcRpBase = 'B' .AND. SEEK(CIMTYP+'1'+CUTTKT,'BOMLINE')
            SELECT BOMLINE
            SCAN REST WHILE cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode =;
                            CTKTBOM.CIMTYP+'1'+CTKTBOM.CUTTKT;
                      FOR ITEM + ICLR = PADR(&lcMatReq..Item,19)+&lcMatReq..ICLR
              SELECT (lcStMatReq)
              IF SEEK(BOMLINE.STYLE)
                FOR lnI = 1 TO lnMonthEd/7
                  lcI = STR(lnI,1)
                  ldLastDate = ldStrDat+(7*lnI)-1
                  IF EMPTY(FithFld)
                    IF (lnI=1 .AND. lnMonthEd=7) .OR. (lnI=2 .AND. lnMonthEd=14) .OR. (lnI=4)
                      ldLastDate = ldEndDat
                    ENDIF
                  ENDIF
                  IF BETWEEN(ldLeadTime,ldStrDat+(7*(lnI-1)),ldLastDate) .OR. EMPTY(ldStrDat)
                    REPLACE ('NWIP'+lcI) WITH EVALUATE('NWIP'+lcI) + BOMLINE.STYQTY
                    EXIT
                  ENDIF
                ENDFOR
              ELSE
                FOR lnI = 1 TO lnMonthEd/7
                  lcI = STR(lnI,1)
                  ldLastDate = ldStrDat+(7*lnI)-1
                  IF EMPTY(FithFld)
                    IF (lnI=1 .AND. lnMonthEd=7) .OR. (lnI=2 .AND. lnMonthEd=14) .OR. (lnI=4)
                      ldLastDate = ldEndDat
                    ENDIF
                  ENDIF
                  IF BETWEEN(ldLeadTime,ldStrDat+(7*(lnI-1)),ldLastDate) .OR. EMPTY(ldStrDat)
                    APPEND BLANK
                    REPLACE CSTYLE       WITH BOMLINE.STYLE,;
                            ('NWIP'+lcI) WITH BOMLINE.STYQTY
                    EXIT
                  ENDIF
                ENDFOR
              ENDIF
            ENDSCAN
          ENDIF
        ENDIF
      ENDIF
    ENDIF
  ENDSCAN
  SET ORDER TO CUTTKTLS IN CUTTKTL
  *E301901,1 AMH [End]
  
ENDIF
SELECT Ctktbom
IF llPOIstall
  
  *E301901,1 AMH Calculate the required at the correct interval of date [Start]
  *SUM (Req_Qty-Issue_Qty) TO lnPoReq FOR ITEM + ICLR = PADR(&lcMatReq..Item,19)+&lcMatReq..ICLR;
  AND SEEK('P'+CUTTKT,'POSHDR') AND POSHDR.Status <> 'X' AND BETWEEN(POSHDR.Complete,ldFrstDay,ldStrDat);
  AND (Req_Qty-Issue_Qty) >0
  SET ORDER TO POSLN IN POSLN
  SCAN FOR ITEM + ICLR = PADR(&lcMatReq..Item,19)+&lcMatReq..ICLR
    IF SEEK('P'+CUTTKT,'POSHDR') .AND. !(POSHDR.Status $ 'SX') .AND. SEEK('P'+CUTTKT,'POSLN')
      lnLeadTime = 0
      lcStyles = ''
      lcStyleKey = SPACE(19)
      llStyleFul = .F.
      SELECT POSLN
      SCAN REST WHILE CSTYTYPE+PO = 'P'+CTKTBOM.CUTTKT
        IF SUBSTR(STYLE,1,lnMajorLn) <> lcStyleKey
          lcStyleKey = SUBSTR(STYLE,1,lnMajorLn)
          IF EMPTY(lcStyles)
            lcStyles = ALLTRIM(SUBSTR(STYLE,1,lnMajorLn))
            llStyleFul = (LEN(lcStyles)>=16)
          ELSE
            IF !llStyleFul
              IF LEN(lcStyles + ',' + ALLTRIM(SUBSTR(STYLE,1,lnMajorLn))) > 16
                lcStyles = lcStyles + ',...'
                llStyleFul = .T.
              ELSE
                lcStyles = lcStyles + ',' + ALLTRIM(SUBSTR(STYLE,1,lnMajorLn))
                llStyleFul = (LEN(lcStyles)=16)
              ENDIF
            ENDIF
          ENDIF
        ENDIF
        IF SEEK(STYLE,'STYLE')
          lnLeadTime = MAX(lnLeadTime,STYLE.LEADTIME)
        ENDIF
      ENDSCAN
      SELECT CTKTBOM
      IF SEEK('P'+CUTTKT,'POSHDR')
        ldLeadTime = lfHolDay(POSHDR.Complete-lnLeadTime,POSHDR.Complete)
        IF BETWEEN(ldLeadTime,ldStrDat,ldEndDat)
          IF lcRpRName = 'MATIMRQD'
            SELECT (lcMatReq)
            APPEND BLANK
            REPLACE ITEM      WITH SUBSTR(lcRecKey,1,19),;
                    ICLR      WITH SUBSTR(lcRecKey,20,6),;
                    CRECTYPE  WITH 'D',;
                    CTYPE     WITH 'Sty. PO',;
                    DDATE     WITH ldLeadTime-7,;
                    CTRANCD   WITH POSHDR.PO,;
                    CSTYLES   WITH lcStyles,;
                    CREF      WITH lfGetRef('2'+POSHDR.PO),;
                    TOTAL     WITH -MAX(CTKTBOM.Req_Qty-CTKTBOM.Issue_Qty,0),;
                    &FrstFld. WITH -MAX(CTKTBOM.Req_Qty-CTKTBOM.Issue_Qty,0)
            SELECT CTKTBOM
          ENDIF
          lnPoReq = lnPoReq + MAX(Req_Qty-Issue_Qty,0)
          IF lcRpBase = 'B' .AND. lnMonthEd/7 > 1
            DO CASE
              CASE BETWEEN(ldLeadTime,ldStrDat,ldStrDat+6) .OR. EMPTY(ldStrDat)
                laWeeks[1,1] = laWeeks[1,1] + MAX(Req_Qty-Issue_Qty,0)
              CASE BETWEEN(ldLeadTime,ldStrDat+7,IIF(EMPTY(FithFld) .AND. lnMonthEd=14,ldEndDat,ldStrDat+13))
                laWeeks[2,1] = laWeeks[2,1] + MAX(Req_Qty-Issue_Qty,0)
              CASE lnMonthEd = 28 .AND. BETWEEN(ldLeadTime,ldStrDat+14,ldStrDat+20)
                laWeeks[3,1] = laWeeks[3,1] + MAX(Req_Qty-Issue_Qty,0)
              CASE lnMonthEd = 28 .AND. BETWEEN(ldLeadTime,ldStrDat+21,IIF(EMPTY(FithFld),ldEndDat,ldStrDat+27))
                laWeeks[4,1] = laWeeks[4,1] + MAX(Req_Qty-Issue_Qty,0)
            ENDCASE
          ENDIF
          IF lcRpBase = 'B' .AND. SEEK(CIMTYP+'1'+CUTTKT,'BOMLINE')
            SELECT BOMLINE
            SCAN REST WHILE cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode =;
                            CTKTBOM.CIMTYP+'1'+CTKTBOM.CUTTKT;
                      FOR ITEM + ICLR = PADR(&lcMatReq..Item,19)+&lcMatReq..ICLR
              SELECT (lcStMatReq)
              IF SEEK(BOMLINE.STYLE)
                FOR lnI = 1 TO lnMonthEd/7
                  lcI = STR(lnI,1)
                  ldLastDate = ldStrDat+(7*lnI)-1
                  IF EMPTY(FithFld)
                    IF (lnI=1 .AND. lnMonthEd=7) .OR. (lnI=2 .AND. lnMonthEd=14) .OR. (lnI=4)
                      ldLastDate = ldEndDat
                    ENDIF
                  ENDIF
                  IF BETWEEN(ldLeadTime,ldStrDat+(7*(lnI-1)),ldLastDate) .OR. EMPTY(ldStrDat)
                    REPLACE ('NWIP'+lcI) WITH EVALUATE('NWIP'+lcI) + BOMLINE.STYQTY
                    EXIT
                  ENDIF
                ENDFOR
              ELSE
                FOR lnI = 1 TO lnMonthEd/7
                  lcI = STR(lnI,1)
                  ldLastDate = ldStrDat+(7*lnI)-1
                  IF EMPTY(FithFld)
                    IF (lnI=1 .AND. lnMonthEd=7) .OR. (lnI=2 .AND. lnMonthEd=14) .OR. (lnI=4)
                      ldLastDate = ldEndDat
                    ENDIF
                  ENDIF
                  IF BETWEEN(ldLeadTime,ldStrDat+(7*(lnI-1)),ldLastDate) .OR. EMPTY(ldStrDat)
                    APPEND BLANK
                    REPLACE CSTYLE       WITH BOMLINE.STYLE,;
                            ('NWIP'+lcI) WITH BOMLINE.STYQTY
                    EXIT
                  ENDIF
                ENDFOR
              ENDIF
            ENDSCAN
          ENDIF
        ENDIF
      ENDIF
    ENDIF
  ENDSCAN
  SET ORDER TO POSLNS IN POSLN
  *E301901,1 AMH [End]
  
ENDIF
lnRequied = lnMFReq + lnPoReq 


SELECT BOM
lcOrder = ORDER()
SET ORDER TO BOMITEM

FOR lnCount = 1 TO 4
  lcType = laFabType[lnCount,1]
  SELECT (lcMatReq)
  
  *E301901,1 AMH Fix bug of duplicate data [Start]
  *IF SEEK(lcType+ITEM,'BOM')
  IF !EMPTY(lcType) .AND. SEEK(lcType+ITEM,'BOM')
  *E301901,1 AMH [End]
  
    SELECT BOM
    SCAN REST WHILE lcType+Item = lcType+&lcMatReq..ITEM
      =SEEK(PADR(BOM.cItmMAjor,lnMajorLn),'STYLE')
      
      *E301901,1 AMH Get the correct style from style file [Start]
      *lcMjrSpr = SUBSTR(STYLE.STYLE,lnMajorLn+1,1)
      IF Iclr = &lcMatReq..ICLR .OR. Iclr = "******"  
        *IF SUBSTR(BOM.cItmMAsk,lnMajorLn+2,lnNMjrLn) = "******" 
        *  lcSlctStyl = PADR(BOM.cItmMAjor,lnMajorLn)+lcMjrSpr + &lcMatReq..ICLR
        *ELSE
        *  lcSlctStyl = BOM.cItmMAsk
        *ENDIF
        *IF SEEK(lcSlctStyl,'STYLE')
        SELECT STYLE
        
        *B606145,1 AMH consider case of color length less than 6 chracters [Start]
        *SCAN REST WHILE STYLE = SUBSTR(BOM.CITMMASK,1,lnMajorLn);
                    FOR LIKE(STRTRAN(BOM.cItmMask,'*','?'),STYLE) .AND.;
          IIF(BOM.ICLR='******',SUBSTR(STYLE,lnClrSrt,lnClrEnd)=EVALUATE(lcMatReq+'.ICLR'),.T.)
        SCAN REST WHILE STYLE = SUBSTR(BOM.CITMMASK,1,lnMajorLn);
                    FOR LIKE(STRTRAN(BOM.cItmMask,'*','?'),STYLE) .AND.;
          IIF(BOM.ICLR='******',SUBSTR(STYLE,lnClrSrt,lnClrEnd)=SUBSTR(EVALUATE(lcMatReq+'.ICLR'),1,lnClrEnd),.T.)
        *B606145,1 AMH [End]
        
          lcSlctStyl = STYLE
      *E301901,1 AMH [End]
              
          IF STYLE.MAKE  
            
            *E301901,1 AMH Fix bug of duplicate data [Start]
            *IF llMFIstall  AND SEEK (lcSlctStyl,'CUTTKTL')
            IF llMFIstall .AND. lnCount > 2 .AND. SEEK (lcSlctStyl,'CUTTKTL')
            *E301901,1 AMH [End]
            
              SELECT CUTTKTL 
              
              *E301901,1 AMH Variable to save the last C/T added [Start]
              lcCuttkt = SPACE(6)
              lcCutLTm = SPACE(6)
              *E301901,1 AMH [End]
              
              SCAN REST WHILE CUTTKTL.Style = lcSlctStyl FOR TranCd = '1'
                
                *E301901,1 AMH Calculate the required at the correct interval of date [Start]
                *IF CUTTKTH.Status = 'H' AND BETWEEN(CutTktH.Complete,ldFrstDay,ldStrDat)
                IF CUTTKT <> lcCutLTm
                  lcCutLTm = CUTTKT
                  lnLeadTime = 0
                  lcCutKey = EVALUATE(KEY())
                  SET ORDER TO CUTTKTL
                  IF SEEK(lcCutLTm)
                    SCAN REST WHILE CUTTKT = lcCutLTm
                      IF SEEK(STYLE,'STYLE')
                        lnLeadTime = MAX(lnLeadTime,STYLE.LEADTIME)
                      ENDIF
                    ENDSCAN
                  ENDIF
                  SET ORDER TO CUTTKTLS
                  =SEEK(lcCutKey)
                ENDIF
                ldLeadTime = lfHolDay(CutTktH.Complete-lnLeadTime,CutTktH.Complete)
                IF CUTTKTH.Status = 'H' AND BETWEEN(ldLeadTime,ldStrDat,ldEndDat)
                *E301901,1 AMH [End]
                  
                  *E301901,1 AMH Add detaile data [Start]
                  IF CUTTKT <> lcCuttkt
                    IF !EMPTY(lcCuttkt) .AND. lcRpRName = 'MATIMRQD'
                      SELECT (lcMatReq)
                      SET ORDER TO MATREQ1
                      IF SEEK(SUBSTR(lcRecKey,1,25)+'D'+lcCuttkt)
                        REPLACE TOTAL     WITH TOTAL - lnCuttkt,;
                                &FrstFld. WITH &FrstFld. - lnCuttkt
                      ELSE
                        APPEND BLANK
                        REPLACE ITEM      WITH SUBSTR(lcRecKey,1,19),;
                                ICLR      WITH SUBSTR(lcRecKey,20,6),;
                                CRECTYPE  WITH 'D',;
                                CTYPE     WITH 'C/T',;
                                DDATE     WITH ldCompDate,;
                                CTRANCD   WITH lcCuttkt,;
                                CSTYLES   WITH SUBSTR(lcStyles,1,lnMajorLn),;
                                CREF      WITH lfGetRef('1'+lcCuttkt),;
                                TOTAL     WITH -lnCuttkt,;
                                &FrstFld. WITH -lnCuttkt
                      ENDIF
                      SET ORDER TO MATREQ
                      SELECT CUTTKTL
                    ENDIF
                    lcCuttkt = CUTTKTH.CUTTKT
                    lcStyles = CUTTKTH.STYLE
                    lnCuttkt = 0
                    ldCompDate = ldLeadTime-7
                  ENDIF
                  *lnRequied = lnRequied + (Totqty * BOM.Nbomtotqty)
                  IF BOM.LBASONSIZ .AND. !EMPTY(BOM.MSIZES)
                    
                    *B606145,1 AMH Consider case of extended size scale [Start]
                    *lnStrSize = AT('~',BOM.MSIZES)
                    *IF lnStrSize > 0 .AND. LEN(BOM.MSIZES) > lnStrSize
                    *  FOR lnI = lnStrSize+1 TO LEN(BOM.MSIZES)
                    *    lcI = SUBSTR(BOM.MSIZES,lnI,1)
                    *    IF VAL(lcI) > 0
                    *      lnCuttkt  = lnCuttkt  + (EVALUATE('QTY'+lcI) * BOM.Nbomtotqty)
                    *      lnRequied = lnRequied + (EVALUATE('QTY'+lcI) * BOM.Nbomtotqty)
                    *    ENDIF
                    *  ENDFOR
                    *ENDIF
                    lnMemWidth = SET('MEMOWIDTH')
                    SET MEMOWIDTH TO 40
                    FOR lnJ = 1 TO MEMLINES(BOM.MSIZES)
                      lnStrSize = AT('~',MLINE(BOM.MSIZES,lnJ))
                      IF lnStrSize > 0 .AND. LEN(MLINE(BOM.MSIZES,lnJ)) > lnStrSize .AND.;
                         IIF(llExtSizSc,SUBSTR(lcSlctStyl,lnSizePos,lnSizeLen)=;
                             SUBSTR(MLINE(BOM.MSIZES,lnJ),1,lnStrSize-1),.T.)
                        FOR lnI = lnStrSize+1 TO LEN(MLINE(BOM.MSIZES,lnJ))
                          lcI = SUBSTR(MLINE(BOM.MSIZES,lnJ),lnI,1)
                          IF VAL(lcI) > 0
                            lnCuttkt  = lnCuttkt  + (EVALUATE('QTY'+lcI) * BOM.Nbomtotqty)
                            lnRequied = lnRequied + (EVALUATE('QTY'+lcI) * BOM.Nbomtotqty)
                          ENDIF
                        ENDFOR
                      ENDIF
                    ENDFOR
                    SET MEMOWIDTH TO lnMemWidth
                    *B606145,1 AMH [End]
                    
                  ELSE
                    lnCuttkt  = lnCuttkt  + (Totqty * BOM.Nbomtotqty)
                    lnRequied = lnRequied + (Totqty * BOM.Nbomtotqty)
                  ENDIF
                  *E301901,1 AMH [End]
        
                  *E301901,1 AMH Get the Week required [Start]
                  IF lcRpBase = 'B' .AND. lnMonthEd/7 > 1
                    DO CASE
                      CASE BETWEEN(ldLeadTime,ldStrDat,ldStrDat+6) .OR. EMPTY(ldStrDat)
                        laWeeks[1,1] = laWeeks[1,1] + (Totqty * BOM.Nbomtotqty)
                      CASE BETWEEN(ldLeadTime,ldStrDat+7,IIF(EMPTY(FithFld) .AND. lnMonthEd=14,ldEndDat,ldStrDat+13))
                        laWeeks[2,1] = laWeeks[2,1] + (Totqty * BOM.Nbomtotqty)
                      CASE lnMonthEd = 28 .AND. BETWEEN(ldLeadTime,ldStrDat+14,ldStrDat+20)
                        laWeeks[3,1] = laWeeks[3,1] + (Totqty * BOM.Nbomtotqty)
                      CASE lnMonthEd = 28 .AND. BETWEEN(ldLeadTime,ldStrDat+21,IIF(EMPTY(FithFld),ldEndDat,ldStrDat+27))
                        laWeeks[4,1] = laWeeks[4,1] + (Totqty * BOM.Nbomtotqty)
                    ENDCASE
                  ENDIF
                  *E301901,1 AMH [End]
        
                  *E301901,1 AMH Get the style/color/Week required [Start]
                  IF lcRpBase = 'B'
                    SELECT (lcStMatReq)
                    IF SEEK(CUTTKTL.STYLE)
                      FOR lnI = 1 TO lnMonthEd/7
                        lcI = STR(lnI,1)
                        ldLastDate = ldStrDat+(7*lnI)-1
                        IF EMPTY(FithFld)
                          IF (lnI=1 .AND. lnMonthEd=7) .OR. (lnI=2 .AND. lnMonthEd=14) .OR. (lnI=4)
                            ldLastDate = ldEndDat
                          ENDIF
                        ENDIF
                        IF BETWEEN(ldLeadTime,ldStrDat+(7*(lnI-1)),ldLastDate) .OR. EMPTY(ldStrDat)
                          IF BOM.LBASONSIZ .AND. !EMPTY(BOM.MSIZES)
                            
                            *B606145,1 AMH Consider case of extended size scale [Start]
                            *lnStrSize = AT('~',BOM.MSIZES)
                            *IF lnStrSize > 0 .AND. LEN(BOM.MSIZES) > lnStrSize
                            *  FOR lnJ = lnStrSize+1 TO LEN(BOM.MSIZES)
                            *    lcJ = SUBSTR(BOM.MSIZES,lnJ,1)
                            *    IF VAL(lcJ) > 0
                            *      REPLACE ('NWIP'+lcI) WITH EVALUATE('NWIP'+lcI) + EVALUATE('CUTTKTL.QTY'+lcJ)
                            *    ENDIF
                            *  ENDFOR
                            *ENDIF
                            lnMemWidth = SET('MEMOWIDTH')
                            SET MEMOWIDTH TO 40
                            FOR lnJ = 1 TO MEMLINES(BOM.MSIZES)
                              lnStrSize = AT('~',MLINE(BOM.MSIZES,lnJ))
                              IF lnStrSize > 0 .AND. LEN(MLINE(BOM.MSIZES,lnJ)) > lnStrSize .AND.;
                                 IIF(llExtSizSc,SUBSTR(lcSlctStyl,lnSizePos,lnSizeLen)=;
                                     SUBSTR(MLINE(BOM.MSIZES,lnJ),1,lnStrSize-1),.T.)
                                FOR lnK = lnStrSize+1 TO LEN(MLINE(BOM.MSIZES,lnJ))
                                  lcK = SUBSTR(MLINE(BOM.MSIZES,lnJ),lnK,1)
                                  IF VAL(lcK) > 0
                                    REPLACE ('NWIP'+lcI) WITH EVALUATE('NWIP'+lcI) + EVALUATE('CUTTKTL.QTY'+lcK)
                                  ENDIF
                                ENDFOR
                              ENDIF
                            ENDFOR
                            SET MEMOWIDTH TO lnMemWidth
                            *B606145,1 AMH [End]
                            
                          ELSE
                            REPLACE ('NWIP'+lcI) WITH EVALUATE('NWIP'+lcI) + CUTTKTL.TOTQTY
                          ENDIF
                          EXIT
                        ENDIF
                      ENDFOR
                    ELSE
                      FOR lnI = 1 TO lnMonthEd/7
                        lcI = STR(lnI,1)
                        ldLastDate = ldStrDat+(7*lnI)-1
                        IF EMPTY(FithFld)
                          IF (lnI=1 .AND. lnMonthEd=7) .OR. (lnI=2 .AND. lnMonthEd=14) .OR. (lnI=4)
                            ldLastDate = ldEndDat
                          ENDIF
                        ENDIF
                        IF BETWEEN(ldLeadTime,ldStrDat+(7*(lnI-1)),ldLastDate) .OR. EMPTY(ldStrDat)
                          APPEND BLANK
                          IF BOM.LBASONSIZ .AND. !EMPTY(BOM.MSIZES)
                            
                            *B606145,1 AMH Consider case of extended size scale [Start]
                            *lnStrSize = AT('~',BOM.MSIZES)
                            *IF lnStrSize > 0 .AND. LEN(BOM.MSIZES) > lnStrSize
                            *  FOR lnJ = lnStrSize+1 TO LEN(BOM.MSIZES)
                            *    lcJ = SUBSTR(BOM.MSIZES,lnJ,1)
                            *    IF VAL(lcJ) > 0
                            *      REPLACE CSTYLE       WITH CUTTKTL.STYLE,;
                            *              ('NWIP'+lcI) WITH EVALUATE('NWIP'+lcI) + EVALUATE('CUTTKTL.QTY'+lcJ)
                            *    ENDIF
                            *  ENDFOR
                            *ENDIF
                            lnMemWidth = SET('MEMOWIDTH')
                            SET MEMOWIDTH TO 40
                            FOR lnJ = 1 TO MEMLINES(BOM.MSIZES)
                              lnStrSize = AT('~',MLINE(BOM.MSIZES,lnJ))
                              IF lnStrSize > 0 .AND. LEN(MLINE(BOM.MSIZES,lnJ)) > lnStrSize .AND.;
                                 IIF(llExtSizSc,SUBSTR(lcSlctStyl,lnSizePos,lnSizeLen)=;
                                     SUBSTR(MLINE(BOM.MSIZES,lnJ),1,lnStrSize-1),.T.)
                                FOR lnK = lnStrSize+1 TO LEN(MLINE(BOM.MSIZES,lnJ))
                                  lcK = SUBSTR(MLINE(BOM.MSIZES,lnJ),lnK,1)
                                  IF VAL(lcK) > 0
                                    REPLACE CSTYLE       WITH CUTTKTL.STYLE,;
                                            ('NWIP'+lcI) WITH EVALUATE('NWIP'+lcI) + EVALUATE('CUTTKTL.QTY'+lcK)
                                  ENDIF
                                ENDFOR
                              ENDIF
                            ENDFOR
                            SET MEMOWIDTH TO lnMemWidth
                            *B606145,1 AMH [End]
                            
                          ELSE
                            REPLACE CSTYLE       WITH CUTTKTL.STYLE,;
                                    ('NWIP'+lcI) WITH CUTTKTL.TOTQTY
                          ENDIF
                          EXIT
                        ENDIF
                      ENDFOR
                    ENDIF
                  ENDIF
                  *E301901,1 AMH [End]
          
                ENDIF
              ENDSCAN
              
              *E301901,1 AMH Add detaile data [Start]
              IF !EMPTY(lcCuttkt) .AND. lcRpRName = 'MATIMRQD'
                SELECT (lcMatReq)
                SET ORDER TO MATREQ1
                IF SEEK(SUBSTR(lcRecKey,1,25)+'D'+lcCuttkt)
                  REPLACE TOTAL     WITH TOTAL - lnCuttkt,;
                          &FrstFld. WITH &FrstFld. - lnCuttkt
                ELSE
                  APPEND BLANK
                  REPLACE ITEM      WITH SUBSTR(lcRecKey,1,19),;
                          ICLR      WITH SUBSTR(lcRecKey,20,6),;
                          CRECTYPE  WITH 'D',;
                          CTYPE     WITH 'C/T',;
                          DDATE     WITH ldCompDate,;
                          CTRANCD   WITH lcCuttkt,;
                          CSTYLES   WITH SUBSTR(lcStyles,1,lnMajorLn),;
                          CREF      WITH lfGetRef('1'+lcCuttkt),;
                          TOTAL     WITH -lnCuttkt,;
                          &FrstFld. WITH -lnCuttkt
                ENDIF
                SET ORDER TO MATREQ
              ENDIF
              *E301901,1 AMH [End]
              
            ENDIF
          ELSE
            
            *E301901,1 AMH Fix bug of duplicate data [Start]
            *IF llPOIstall AND SEEK (lcSlctStyl,'POSLN')
            IF llPOIstall .AND. lnCount < 3 .AND. SEEK (lcSlctStyl,'POSLN')
            *E301901,1 AMH [End]
            
              SELECT POSLN
              
              *E301901,1 AMH Variable to save the last Sty. PO added [Start]
              lcPO = SPACE(6)
              lcPOLTm = SPACE(6)
              *E301901,1 AMH [End]
              
              SCAN REST WHILE POSLN.Style = lcSlctStyl FOR TranCd = '1'
                
                *E301901,1 AMH Calculate the required at the correct interval of date [Start]
                *IF POSHDR.Status = 'H' AND BETWEEN(POSHDR.Complete,ldFrstDay,ldStrDat)
                IF PO <> lcPOLTm
                  lcPOLTm = PO
                  lnLeadTime = 0
                  lcPOKey = EVALUATE(KEY())
                  SET ORDER TO POSLN
                  IF SEEK('P'+lcPOLTm)
                    SCAN REST WHILE CSTYTYPE+PO = 'P'+lcPOLTm
                      IF SEEK(STYLE,'STYLE')
                        lnLeadTime = MAX(lnLeadTime,STYLE.LEADTIME)
                      ENDIF
                    ENDSCAN
                  ENDIF
                  SET ORDER TO POSLNS
                  =SEEK(lcPOKey)
                ENDIF
                ldLeadTime = lfHolDay(POSHDR.Complete-lnLeadTime,POSHDR.Complete)
                IF POSHDR.Status = 'H' AND BETWEEN(ldLeadTime,ldStrDat,ldEndDat)
                *E301901,1 AMH [End]
                
                  *E301901,1 AMH Add detaile data [Start]
                  IF PO <> lcPO
                    IF !EMPTY(lcPO) .AND. lcRpRName = 'MATIMRQD'
                      SELECT (lcMatReq)
                      SET ORDER TO MATREQ1
                      IF SEEK(SUBSTR(lcRecKey,1,25)+'D'+lcPO)
                        REPLACE TOTAL     WITH TOTAL - lnPO,;
                                &FrstFld. WITH &FrstFld. - lnPO
                        lcStyles = ALLTRIM(SUBSTR(lcSlctStyl,1,lnMajorLn))
                        lnStyles = LEN(lcStyles)
                        IF lcStyles + ',' $ ALLTRIM(CSTYLES) .OR.;
                           RIGHT(ALLTRIM(CSTYLES),lnStyles) = lcStyles
                          *-- Do nothing
                        ELSE
                          IF LEN(ALLTRIM(CSTYLES) + ',' + lcStyles) > 16
                            REPLACE CSTYLES WITH ALLTRIM(CSTYLES) + ',...'
                          ELSE
                            REPLACE CSTYLES WITH ALLTRIM(CSTYLES) + ',' + lcstyles
                          ENDIF
                        ENDIF
                      ELSE
                        APPEND BLANK
                        REPLACE ITEM      WITH SUBSTR(lcRecKey,1,19),;
                                ICLR      WITH SUBSTR(lcRecKey,20,6),;
                                CRECTYPE  WITH 'D',;
                                CTYPE     WITH 'Sty. PO',;
                                DDATE     WITH ldCompDate,;
                                CTRANCD   WITH lcPO,;
                                CSTYLES   WITH SUBSTR(lcSlctStyl,1,lnMajorLn),;
                                CREF      WITH lfGetRef('2'+lcPO),;
                                TOTAL     WITH -lnPO,;
                                &FrstFld. WITH -lnPO
                      ENDIF
                      SET ORDER TO MATREQ
                      SELECT POSLN
                    ENDIF
                    lcPO = POSHDR.PO
                    lnPO = 0
                    ldCompDate = ldLeadTime-7
                  ENDIF
                  *lnRequied = lnRequied + (Totqty * BOM.Nbomtotqty)
                  IF BOM.LBASONSIZ .AND. !EMPTY(BOM.MSIZES)
                    
                    *B606145,1 AMH Consider case of extended size scale [Start]
                    *lnStrSize = AT('~',BOM.MSIZES)
                    *IF lnStrSize > 0 .AND. LEN(BOM.MSIZES) > lnStrSize
                    *  FOR lnI = lnStrSize+1 TO LEN(BOM.MSIZES)
                    *    lcI = SUBSTR(BOM.MSIZES,lnI,1)
                    *    IF VAL(lcI) > 0
                    *      lnPO      = lnPO      + (EVALUATE('QTY'+lcI) * BOM.Nbomtotqty)
                    *      lnRequied = lnRequied + (EVALUATE('QTY'+lcI) * BOM.Nbomtotqty)
                    *    ENDIF
                    *  ENDFOR
                    *ENDIF
                    lnMemWidth = SET('MEMOWIDTH')
                    SET MEMOWIDTH TO 40
                    FOR lnJ = 1 TO MEMLINES(BOM.MSIZES)
                      lnStrSize = AT('~',MLINE(BOM.MSIZES,lnJ))
                      IF lnStrSize > 0 .AND. LEN(MLINE(BOM.MSIZES,lnJ)) > lnStrSize .AND.;
                         IIF(llExtSizSc,SUBSTR(lcSlctStyl,lnSizePos,lnSizeLen)=;
                             SUBSTR(MLINE(BOM.MSIZES,lnJ),1,lnStrSize-1),.T.)
                        FOR lnI = lnStrSize+1 TO LEN(MLINE(BOM.MSIZES,lnJ))
                          lcI = SUBSTR(MLINE(BOM.MSIZES,lnJ),lnI,1)
                          IF VAL(lcI) > 0
                            lnPO      = lnPO      + (EVALUATE('QTY'+lcI) * BOM.Nbomtotqty)
                            lnRequied = lnRequied + (EVALUATE('QTY'+lcI) * BOM.Nbomtotqty)
                          ENDIF
                        ENDFOR
                      ENDIF
                    ENDFOR
                    SET MEMOWIDTH TO lnMemWidth
                    *B606145,1 AMH [End]
                    
                  ELSE
                    lnPO = lnPO + (Totqty * BOM.Nbomtotqty)
                    lnRequied = lnRequied + (Totqty * BOM.Nbomtotqty)
                  ENDIF
                  *E301901,1 AMH [End]
        
                  *E301901,1 AMH Get the Week required [Start]
                  IF lcRpBase = 'B' .AND. lnMonthEd/7 > 1
                    DO CASE
                      CASE BETWEEN(ldLeadTime,ldStrDat,ldStrDat+6) .OR. EMPTY(ldStrDat)
                        laWeeks[1,1] = laWeeks[1,1] + (Totqty * BOM.Nbomtotqty)
                      CASE BETWEEN(ldLeadTime,ldStrDat+7,IIF(EMPTY(FithFld) .AND. lnMonthEd=14,ldEndDat,ldStrDat+13))
                        laWeeks[2,1] = laWeeks[2,1] + (Totqty * BOM.Nbomtotqty)
                      CASE lnMonthEd = 28 .AND. BETWEEN(ldLeadTime,ldStrDat+14,ldStrDat+20)
                        laWeeks[3,1] = laWeeks[3,1] + (Totqty * BOM.Nbomtotqty)
                      CASE lnMonthEd = 28 .AND. BETWEEN(ldLeadTime,ldStrDat+21,IIF(EMPTY(FithFld),ldEndDat,ldStrDat+27))
                        laWeeks[4,1] = laWeeks[4,1] + (Totqty * BOM.Nbomtotqty)
                    ENDCASE
                  ENDIF
                  *E301901,1 AMH [End]
        
                  *E301901,1 AMH Get the style/color/Week required [Start]
                  IF lcRpBase = 'B'
                    SELECT (lcStMatReq)
                    IF SEEK(POSLN.STYLE)
                      FOR lnI = 1 TO lnMonthEd/7
                        lcI = STR(lnI,1)
                        ldLastDate = ldStrDat+(7*lnI)-1
                        IF EMPTY(FithFld)
                          IF (lnI=1 .AND. lnMonthEd=7) .OR. (lnI=2 .AND. lnMonthEd=14) .OR. (lnI=4)
                            ldLastDate = ldEndDat
                          ENDIF
                        ENDIF
                        IF BETWEEN(ldLeadTime,ldStrDat+(7*(lnI-1)),ldLastDate) .OR. EMPTY(ldStrDat)
                          IF BOM.LBASONSIZ .AND. !EMPTY(BOM.MSIZES)
                            
                            *B606145,1 AMH Consider case of extended size scale [Start]
                            *lnStrSize = AT('~',BOM.MSIZES)
                            *IF lnStrSize > 0 .AND. LEN(BOM.MSIZES) > lnStrSize
                            *  FOR lnJ = lnStrSize+1 TO LEN(BOM.MSIZES)
                            *    lcJ = SUBSTR(BOM.MSIZES,lnJ,1)
                            *    IF VAL(lcJ) > 0
                            *      REPLACE ('NWIP'+lcI) WITH EVALUATE('NWIP'+lcI) + EVALUATE('POSLN.QTY'+lcJ)
                            *    ENDIF
                            *  ENDFOR
                            *ENDIF
                            lnMemWidth = SET('MEMOWIDTH')
                            SET MEMOWIDTH TO 40
                            FOR lnJ = 1 TO MEMLINES(BOM.MSIZES)
                              lnStrSize = AT('~',MLINE(BOM.MSIZES,lnJ))
                              IF lnStrSize > 0 .AND. LEN(MLINE(BOM.MSIZES,lnJ)) > lnStrSize .AND.;
                                 IIF(llExtSizSc,SUBSTR(lcSlctStyl,lnSizePos,lnSizeLen)=;
                                     SUBSTR(MLINE(BOM.MSIZES,lnJ),1,lnStrSize-1),.T.)
                                FOR lnK = lnStrSize+1 TO LEN(MLINE(BOM.MSIZES,lnJ))
                                  lcK = SUBSTR(MLINE(BOM.MSIZES,lnJ),lnK,1)
                                  IF VAL(lcK) > 0
                                    REPLACE ('NWIP'+lcI) WITH EVALUATE('NWIP'+lcI) + EVALUATE('POSLN.QTY'+lcK)
                                  ENDIF
                                ENDFOR
                              ENDIF
                            ENDFOR
                            SET MEMOWIDTH TO lnMemWidth
                            *B606145,1 AMH [End]
                            
                          ELSE
                            REPLACE ('NWIP'+lcI) WITH EVALUATE('NWIP'+lcI) + POSLN.TOTQTY
                          ENDIF
                          EXIT
                        ENDIF
                      ENDFOR
                    ELSE
                      FOR lnI = 1 TO lnMonthEd/7
                        lcI = STR(lnI,1)
                        ldLastDate = ldStrDat+(7*lnI)-1
                        IF EMPTY(FithFld)
                          IF (lnI=1 .AND. lnMonthEd=7) .OR. (lnI=2 .AND. lnMonthEd=14) .OR. (lnI=4)
                            ldLastDate = ldEndDat
                          ENDIF
                        ENDIF
                        IF BETWEEN(ldLeadTime,ldStrDat+(7*(lnI-1)),ldLastDate) .OR. EMPTY(ldStrDat)
                          APPEND BLANK
                          IF BOM.LBASONSIZ .AND. !EMPTY(BOM.MSIZES)
                            
                            *B606145,1 AMH Consider case of extended size scale [Start]
                            *lnStrSize = AT('~',BOM.MSIZES)
                            *IF lnStrSize > 0 .AND. LEN(BOM.MSIZES) > lnStrSize
                            *  FOR lnJ = lnStrSize+1 TO LEN(BOM.MSIZES)
                            *    lcJ = SUBSTR(BOM.MSIZES,lnJ,1)
                            *    IF VAL(lcJ) > 0
                            *      REPLACE CSTYLE       WITH POSLN.STYLE,;
                            *              ('NWIP'+lcI) WITH EVALUATE('NWIP'+lcI) + EVALUATE('POSLN.QTY'+lcJ)
                            *    ENDIF
                            *  ENDFOR
                            *ENDIF
                            lnMemWidth = SET('MEMOWIDTH')
                            SET MEMOWIDTH TO 40
                            FOR lnJ = 1 TO MEMLINES(BOM.MSIZES)
                              lnStrSize = AT('~',MLINE(BOM.MSIZES,lnJ))
                              IF lnStrSize > 0 .AND. LEN(MLINE(BOM.MSIZES,lnJ)) > lnStrSize .AND.;
                                 IIF(llExtSizSc,SUBSTR(lcSlctStyl,lnSizePos,lnSizeLen)=;
                                     SUBSTR(MLINE(BOM.MSIZES,lnJ),1,lnStrSize-1),.T.)
                                FOR lnK = lnStrSize+1 TO LEN(MLINE(BOM.MSIZES,lnJ))
                                  lcK = SUBSTR(MLINE(BOM.MSIZES,lnJ),lnK,1)
                                  IF VAL(lcK) > 0
                                    REPLACE CSTYLE       WITH POSLN.STYLE,;
                                            ('NWIP'+lcI) WITH EVALUATE('NWIP'+lcI) + EVALUATE('POSLN.QTY'+lcK)
                                  ENDIF
                                ENDFOR
                              ENDIF
                            ENDFOR
                            SET MEMOWIDTH TO lnMemWidth
                            *B606145,1 AMH [End]
                            
                          ELSE
                            REPLACE CSTYLE       WITH POSLN.STYLE,;
                                    ('NWIP'+lcI) WITH POSLN.TOTQTY
                          ENDIF
                          EXIT
                        ENDIF
                      ENDFOR
                    ENDIF
                  ENDIF
                  *E301901,1 AMH [End]
                  
                ENDIF
              ENDSCAN
              
              *E301901,1 AMH Add detaile data [Start]
              IF !EMPTY(lcPO) .AND. lcRpRName = 'MATIMRQD'
                SELECT (lcMatReq)
                SET ORDER TO MATREQ1
                IF SEEK(SUBSTR(lcRecKey,1,25)+'D'+lcPO)
                  REPLACE TOTAL     WITH TOTAL - lnPO,;
                          &FrstFld. WITH &FrstFld. - lnPO
                  lcStyles = ALLTRIM(SUBSTR(lcSlctStyl,1,lnMajorLn))
                  lnStyles = LEN(lcStyles)
                  IF lcStyles + ',' $ ALLTRIM(CSTYLES) .OR.;
                     RIGHT(ALLTRIM(CSTYLES),lnStyles) = lcStyles
                    *-- Do nothing
                  ELSE
                    IF LEN(ALLTRIM(CSTYLES) + ',' + lcStyles) > 16
                      REPLACE CSTYLES WITH ALLTRIM(CSTYLES) + ',...'
                    ELSE
                      REPLACE CSTYLES WITH ALLTRIM(CSTYLES) + ',' + lcstyles
                    ENDIF
                  ENDIF
                ELSE
                  APPEND BLANK
                  REPLACE ITEM      WITH SUBSTR(lcRecKey,1,19),;
                          ICLR      WITH SUBSTR(lcRecKey,20,6),;
                          CRECTYPE  WITH 'D',;
                          CTYPE     WITH 'Sty. PO',;
                          DDATE     WITH ldCompDate,;
                          CTRANCD   WITH lcPo,;
                          CSTYLES   WITH SUBSTR(lcSlctStyl,1,lnMajorLn),;
                          CREF      WITH lfGetRef('2'+lcPo),;
                          TOTAL     WITH -lnPO,;
                          &FrstFld. WITH -lnPO
                ENDIF
                SET ORDER TO MATREQ
              ENDIF
              *E301901,1 AMH [End]
              
            ENDIF
          ENDIF
        
        *E301901,1 AMH endscan not end if [Start]
        *ENDIF
          =SEEK(lcSlctStyl,'STYLE')
        ENDSCAN
        *E301901,1 AMH [End]
        
      ENDIF
    ENDSCAN
  ENDIF
ENDFOR
SELECT BOM
SET ORDER TO lcOrder

*E301901,1 AMH Calculate the required based on (WIP/Plan/Both) [Start]
ENDIF
IF lcRpBase $ 'PB'
  SELECT BOM
  lcOrder = ORDER()
  SET ORDER TO BOMITEM

  FOR lnCount = 1 TO 4
    lcType = laFabType[lnCount,1]
    SELECT (lcMatReq)
    IF !EMPTY(lcType) .AND. SEEK(lcType+ITEM,'BOM')
      SELECT BOM
      SCAN REST WHILE lcType+Item = lcType+&lcMatReq..ITEM
        =SEEK(PADR(BOM.cItmMAjor,lnMajorLn),'STYLE')
        IF Iclr = &lcMatReq..ICLR .OR. Iclr = "******"
          SELECT STYLE
          
          *B606145,1 AMH consider case of color length less than 6 chracters [Start]
          *SCAN REST WHILE STYLE = SUBSTR(BOM.CITMMASK,1,lnMajorLn);
                    FOR LIKE(STRTRAN(BOM.cItmMask,'*','?'),STYLE) .AND.;
              IIF(BOM.ICLR='******',SUBSTR(STYLE,lnClrSrt,lnClrEnd)=EVALUATE(lcMatReq+'.ICLR'),.T.)
          SCAN REST WHILE STYLE = SUBSTR(BOM.CITMMASK,1,lnMajorLn);
                    FOR LIKE(STRTRAN(BOM.cItmMask,'*','?'),STYLE) .AND.;
              IIF(BOM.ICLR='******',SUBSTR(STYLE,lnClrSrt,lnClrEnd)=SUBSTR(EVALUATE(lcMatReq+'.ICLR'),1,lnClrEnd),.T.)
          *B606145,1 AMH [End]
          
            lcSlctStyl = STYLE
            IF IIF(STYLE.MAKE,lnCount>2,lnCount<3) .AND. SEEK(lcSlctStyl,'FORCAST')
              SELECT FORCAST
              SCAN REST WHILE STYLE+STR(NYEAR,4)+STR(NWEEK,2) = lcSlctStyl
                ldWeekDay = CTOD('01/01/'+STR(NYEAR,4))+(7*(NWEEK-1))
                ldWeekDay = lfWeekStr(ldWeekDay)
                ldLeadTime = lfHolDay(ldWeekDay-STYLE.LEADTIME,ldWeekDay)
                IF BETWEEN(ldLeadTime,ldStrDat,ldEndDat)
                  IF BOM.LBASONSIZ .AND. !EMPTY(BOM.MSIZES)
                    
                    *B606145,1 AMH Consider case of extended size scale [Start]
                    *lnStrSize = AT('~',BOM.MSIZES)
                    *IF lnStrSize > 0 .AND. LEN(BOM.MSIZES) > lnStrSize
                    *  FOR lnI = lnStrSize+1 TO LEN(BOM.MSIZES)
                    *    lcI = SUBSTR(BOM.MSIZES,lnI,1)
                    *    IF VAL(lcI) > 0
                    *      lnPlanReq = lnPlanReq + (EVALUATE('NFORQTY'+lcI) * BOM.Nbomtotqty)
                    *    ENDIF
                    *  ENDFOR
                    *ENDIF
                    lnMemWidth = SET('MEMOWIDTH')
                    SET MEMOWIDTH TO 40
                    FOR lnJ = 1 TO MEMLINES(BOM.MSIZES)
                      lnStrSize = AT('~',MLINE(BOM.MSIZES,lnJ))
                      IF lnStrSize > 0 .AND. LEN(MLINE(BOM.MSIZES,lnJ)) > lnStrSize .AND.;
                         IIF(llExtSizSc,SUBSTR(lcSlctStyl,lnSizePos,lnSizeLen)=;
                             SUBSTR(MLINE(BOM.MSIZES,lnJ),1,lnStrSize-1),.T.)
                        FOR lnI = lnStrSize+1 TO LEN(MLINE(BOM.MSIZES,lnJ))
                          lcI = SUBSTR(MLINE(BOM.MSIZES,lnJ),lnI,1)
                          IF VAL(lcI) > 0
                            lnPlanReq = lnPlanReq + (EVALUATE('NFORQTY'+lcI) * BOM.Nbomtotqty)
                          ENDIF
                        ENDFOR
                      ENDIF
                    ENDFOR
                    SET MEMOWIDTH TO lnMemWidth
                    *B606145,1 AMH [End]
                    
                  ELSE
                    lnPlanReq = lnPlanReq + (nForTotqty * BOM.Nbomtotqty)
                  ENDIF
                  IF lcRpBase = 'B' .AND. lnMonthEd/7 > 1
                    DO CASE
                      CASE BETWEEN(ldLeadTime,ldStrDat,ldStrDat+6) .OR. EMPTY(ldStrDat)
                        laWeeks[1,2] = laWeeks[1,2] + (nForTotqty * BOM.Nbomtotqty)
                      CASE BETWEEN(ldLeadTime,ldStrDat+7,IIF(EMPTY(FithFld) .AND. lnMonthEd=14,ldEndDat,ldStrDat+13))
                        laWeeks[2,2] = laWeeks[2,2] + (nForTotqty * BOM.Nbomtotqty)
                      CASE lnMonthEd = 28 .AND. BETWEEN(ldLeadTime,ldStrDat+14,ldStrDat+20)
                        laWeeks[3,2] = laWeeks[3,2] + (nForTotqty * BOM.Nbomtotqty)
                      CASE lnMonthEd = 28 .AND. BETWEEN(ldLeadTime,ldStrDat+21,IIF(EMPTY(FithFld),ldEndDat,ldStrDat+27))
                        laWeeks[4,2] = laWeeks[4,2] + (nForTotqty * BOM.Nbomtotqty)
                    ENDCASE
                  ENDIF
                  IF lcRpBase = 'B'
                    SELECT (lcStMatReq)
                    IF SEEK(lcSlctStyl)
                      FOR lnI = 1 TO lnMonthEd/7
                        lcI = STR(lnI,1)
                        ldLastDate = ldStrDat+(7*lnI)-1
                        IF EMPTY(FithFld)
                          IF (lnI=1 .AND. lnMonthEd=7) .OR. (lnI=2 .AND. lnMonthEd=14) .OR. (lnI=4)
                            ldLastDate = ldEndDat
                          ENDIF
                        ENDIF
                        IF BETWEEN(ldLeadTime,ldStrDat+(7*(lnI-1)),ldLastDate) .OR. EMPTY(ldStrDat)
                          IF BOM.LBASONSIZ .AND. !EMPTY(BOM.MSIZES)
                            
                            *B606145,1 AMH Consider case of extended size scale [Start]
                            *lnStrSize = AT('~',BOM.MSIZES)
                            *IF lnStrSize > 0 .AND. LEN(BOM.MSIZES) > lnStrSize
                            *  FOR lnJ = lnStrSize+1 TO LEN(BOM.MSIZES)
                            *    lcJ = SUBSTR(BOM.MSIZES,lnJ,1)
                            *    IF VAL(lcJ) > 0
                            *      REPLACE NYEALD            WITH BOM.NBOMTOTQTY,;
                            *              ('DLEADTIME'+lcI) WITH IIF(EMPTY(ldStrDat),{},ldLeadTime-7),;
                            *              ('NPLAN'+lcI)     WITH EVALUATE('NPLAN'+lcI) + EVALUATE('FORCAST.NFORQTY'+lcJ)
                            *    ENDIF
                            *  ENDFOR
                            *ENDIF
                            lnMemWidth = SET('MEMOWIDTH')
                            SET MEMOWIDTH TO 40
                            FOR lnJ = 1 TO MEMLINES(BOM.MSIZES)
                              lnStrSize = AT('~',MLINE(BOM.MSIZES,lnJ))
                              IF lnStrSize > 0 .AND. LEN(MLINE(BOM.MSIZES,lnJ)) > lnStrSize .AND.;
                                 IIF(llExtSizSc,SUBSTR(lcSlctStyl,lnSizePos,lnSizeLen)=;
                                     SUBSTR(MLINE(BOM.MSIZES,lnJ),1,lnStrSize-1),.T.)
                                FOR lnK = lnStrSize+1 TO LEN(MLINE(BOM.MSIZES,lnJ))
                                  lcK = SUBSTR(MLINE(BOM.MSIZES,lnJ),lnK,1)
                                  IF VAL(lcK) > 0
                                    REPLACE NYEALD            WITH BOM.NBOMTOTQTY,;
                                            ('DLEADTIME'+lcI) WITH IIF(EMPTY(ldStrDat),{},ldLeadTime-7),;
                                            ('NPLAN'+lcI)     WITH EVALUATE('NPLAN'+lcI) + EVALUATE('FORCAST.NFORQTY'+lcK)
                                  ENDIF
                                ENDFOR
                              ENDIF
                            ENDFOR
                            SET MEMOWIDTH TO lnMemWidth
                            *B606145,1 AMH [End]
                            
                          ELSE
                            REPLACE NYEALD            WITH BOM.NBOMTOTQTY,;
                                    ('DLEADTIME'+lcI) WITH IIF(EMPTY(ldStrDat),{},ldLeadTime-7),;
                                    ('NPLAN'+lcI)     WITH EVALUATE('NPLAN'+lcI) + FORCAST.NFORTOTQTY
                          ENDIF
                          EXIT
                        ENDIF
                      ENDFOR
                    ELSE
                      FOR lnI = 1 TO lnMonthEd/7
                        lcI = STR(lnI,1)
                        ldLastDate = ldStrDat+(7*lnI)-1
                        IF EMPTY(FithFld)
                          IF (lnI=1 .AND. lnMonthEd=7) .OR. (lnI=2 .AND. lnMonthEd=14) .OR. (lnI=4)
                            ldLastDate = ldEndDat
                          ENDIF
                        ENDIF
                        IF BETWEEN(ldLeadTime,ldStrDat+(7*(lnI-1)),ldLastDate) .OR. EMPTY(ldStrDat)
                          APPEND BLANK
                          IF BOM.LBASONSIZ .AND. !EMPTY(BOM.MSIZES)
                            
                            *B606145,1 AMH Consider case of extended size scale [Start]
                            *lnStrSize = AT('~',BOM.MSIZES)
                            *IF lnStrSize > 0 .AND. LEN(BOM.MSIZES) > lnStrSize
                            *  FOR lnJ = lnStrSize+1 TO LEN(BOM.MSIZES)
                            *    lcJ = SUBSTR(BOM.MSIZES,lnJ,1)
                            *    IF VAL(lcJ) > 0
                            *      REPLACE CSTYLE            WITH FORCAST.STYLE,;
                            *              NYEALD            WITH BOM.NBOMTOTQTY,;
                            *              ('DLEADTIME'+lcI) WITH IIF(EMPTY(ldStrDat),{},ldLeadTime-7),;
                            *              ('NPLAN'+lcI)     WITH EVALUATE('NPLAN'+lcI) + EVALUATE('FORCAST.NFORQTY'+lcJ)
                            *    ENDIF
                            *  ENDFOR
                            *ENDIF
                            lnMemWidth = SET('MEMOWIDTH')
                            SET MEMOWIDTH TO 40
                            FOR lnJ = 1 TO MEMLINES(BOM.MSIZES)
                              lnStrSize = AT('~',MLINE(BOM.MSIZES,lnJ))
                              IF lnStrSize > 0 .AND. LEN(MLINE(BOM.MSIZES,lnJ)) > lnStrSize .AND.;
                                 IIF(llExtSizSc,SUBSTR(lcSlctStyl,lnSizePos,lnSizeLen)=;
                                     SUBSTR(MLINE(BOM.MSIZES,lnJ),1,lnStrSize-1),.T.)
                                FOR lnK = lnStrSize+1 TO LEN(MLINE(BOM.MSIZES,lnJ))
                                  lcK = SUBSTR(MLINE(BOM.MSIZES,lnJ),lnK,1)
                                  IF VAL(lcK) > 0
                                    REPLACE CSTYLE            WITH FORCAST.STYLE,;
                                            NYEALD            WITH BOM.NBOMTOTQTY,;
                                            ('DLEADTIME'+lcI) WITH IIF(EMPTY(ldStrDat),{},ldLeadTime-7),;
                                            ('NPLAN'+lcI)     WITH EVALUATE('NPLAN'+lcI) + EVALUATE('FORCAST.NFORQTY'+lcK)
                                  ENDIF
                                ENDFOR
                              ENDIF
                            ENDFOR
                            SET MEMOWIDTH TO lnMemWidth
                            *B606145,1 AMH [End]
                            
                          ELSE
                            REPLACE CSTYLE            WITH FORCAST.STYLE,;
                                    NYEALD            WITH BOM.NBOMTOTQTY,;
                                    ('DLEADTIME'+lcI) WITH IIF(EMPTY(ldStrDat),{},ldLeadTime-7),;
                                    ('NPLAN'+lcI)     WITH FORCAST.NFORTOTQTY
                          ENDIF
                          EXIT
                        ENDIF
                      ENDFOR
                    ENDIF
                  ENDIF
                ENDIF
              ENDSCAN
            ENDIF
          ENDSCAN
        ENDIF
      ENDSCAN
    ENDIF
  ENDFOR
  SELECT BOM
  SET ORDER TO lcOrder
ENDIF
*E301901,1 AMH [End]

*E301901,1 AMH Return the maximum of required of WIP and required of Plan [Start]
IF lcRpBase = 'B'
  DECLARE laPlan[4],laStyles[4],laStyleKey[4],laStyleFul[4],laLeadTime[4]
  STORE 0 TO laPlan
  SELECT (lcStMatReq)
  STORE SPACE(0) TO laStyles
  STORE SPACE(19) TO laStyleKey
  STORE ldEndDat TO laLeadTime
  SCAN
    FOR lnI = 1 TO 4
      lcI = STR(lnI,1)
      IF EVALUATE('NPLAN'+lcI) > EVALUATE('NWIP'+lcI)
        IF SUBSTR(CSTYLE,1,lnMajorLn) <> laStyleKey[lnI]
          laStyleKey[lnI] = SUBSTR(CSTYLE,1,lnMajorLn)
          IF EMPTY(laStyles[lnI])
            laStyles[lnI] = ALLTRIM(SUBSTR(CSTYLE,1,lnMajorLn))
            laStyleFul[lnI] = (LEN(laStyles[lnI])>=16)
          ELSE
            IF !laStyleFul[lnI]
              IF LEN(laStyles[lnI] + ',' + ALLTRIM(SUBSTR(CSTYLE,1,lnMajorLn))) > 16
                laStyles[lnI] = laStyles[lnI] + ',...'
                laStyleFul[lnI] = .T.
              ELSE
                laStyles[lnI] = laStyles[lnI] + ',' + ALLTRIM(SUBSTR(CSTYLE,1,lnMajorLn))
                laStyleFul[lnI] = (LEN(laStyles[lnI])=16)
              ENDIF
            ENDIF
          ENDIF
        ENDIF
        laPlan[lnI] = laPlan[lnI] + ((EVALUATE('NPLAN'+lcI) - EVALUATE('NWIP'+lcI)) * NYEALD)
        laLeadTime[lnI] = MIN(laLeadTime[lnI],EVALUATE('DLEADTIME'+lcI))
      ENDIF
    ENDFOR
  ENDSCAN
ENDIF
IF lcRpBase = 'B' .AND. lnMonthEd/7 > 1
  IF lcRpRName = 'MATIMRQD'
    FOR lnI = 1 TO ALEN(laWeeks,1)
      SELECT (lcMatReq)
      APPEND BLANK
      REPLACE ITEM      WITH SUBSTR(lcRecKey,1,19),;
              ICLR      WITH SUBSTR(lcRecKey,20,6),;
              CRECTYPE  WITH 'D',;
              CTYPE     WITH 'Plan',;
              DDATE     WITH IIF(EMPTY(FithFld),{},laLeadTime[lnI]),;
              CTRANCD   WITH '',;
              CSTYLES   WITH laStyles[lnI],;
              TOTAL     WITH -laPlan[lnI],;
              &FrstFld. WITH -laPlan[lnI]
    ENDFOR
  ENDIF
  lnRequied = 0
  FOR lnI = 1 TO ALEN(laWeeks,1)
    lnRequied = lnRequied + laWeeks[lnI,1] + laPlan[lnI]
  ENDFOR
ELSE
  IF lcRpBase = 'B'
    IF lcRpRName = 'MATIMRQD'
      SELECT (lcMatReq)
      APPEND BLANK
      REPLACE ITEM      WITH SUBSTR(lcRecKey,1,19),;
              ICLR      WITH SUBSTR(lcRecKey,20,6),;
              CRECTYPE  WITH 'D',;
              CTYPE     WITH 'Plan',;
              DDATE     WITH IIF(EMPTY(FithFld),{},laLeadTime[1]),;
              CTRANCD   WITH '',;
              CSTYLES   WITH laStyles[1],;
              TOTAL     WITH -laPlan[1],;
              &FrstFld. WITH -laPlan[1]
    ENDIF
    lnRequied = lnRequied + laPlan[1]
  ELSE
    lnRequied = MAX(lnRequied,lnPlanReq)
  ENDIF
ENDIF
*E301901,1 AMH [End]

SELECT (lnAlias)
RETURN lnRequied

*!**********************************************************************
*! Name      : lfGetColor
*! Developer : RANIA ABDEL RAZIK (RAE)
*! Date      : 01/13/2002
*! Purpose   : Get the color length and width.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfGetColor()
*!*************************************************************
FUNCTION lfGetColor

DIME laMajSeg[1,1]
=gfItemMask(@laMajSeg)
FOR lnCnt=1 TO ALEN(laMajSeg,1)
  *--Check for existance of color segment in style structure.
  IF laMajSeg[lnCnt,1]='C'
    *--Get the color length and width.
    lnClrSrt = laMajSeg[lnCnt,4]
    lnClrEnd = LEN(laMajSeg[lnCnt,3])
    
    *B606145,1 AMH Don't exit when get color info. to can also get the size info. [Start]
    *EXIT
    *B606145,1 AMH [End]
    
  ENDIF
  
  *B606145,1 AMH Get the size posation and length [Start]
  *--Check for existance of color segment in style structure.
  IF llExtSizSc .AND. laMajSeg[lnCnt,1] = 'S'
    *--Get the size length and width.
    lnSizePos = laMajSeg[lnCnt,4]
    lnSizeLen = LEN(laMajSeg[lnCnt,3])
  ENDIF
  *B606145,1 AMH [End]
  
ENDFOR
RETURN

*!*************************************************************
*! Name      : lfGetStrt
*! Developer : RANIA ABDEL RAZIK (RAE)
*! Date      : 01/10/2002
*! Purpose   : Get Start and End of the month.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfGetStrt()
*!*************************************************************
FUNCTION lfGetStrt

PARAMETERS ldStrDat

DO CASE
  *E301901,1 AMH Calculate the end of period based on the O.T.S setting [Start]
  *CASE INLIST(MONTH(ldStrDat),1,3,5,7,8,10,12)
  *  lnMonthEd = 31
  *CASE MONTH(ldStrDat) = 2
  *  lnMonthEd = IIF(YEAR(ldStrDat)/4-INT(YEAR(ldStrDat)/4)<>0,28,29)
  *OTHERWISE
  *  lnMonthEd = 30
  CASE lcPerLen = 'W'
    lnMonthEd = 7
  CASE lcPerLen = 'E'
    lnMonthEd = 14
  OTHERWISE
    lnMonthEd = 28
  *E301901,1 AMH [End]
ENDCASE

FUNCTION lfGetDesc

=SEEK(PADR(Item,7)+ICLR,'Fabric')
lcDesc      = Fabric.Desc
lcColorDesc = SUBSTR(gfCodDes(IClr , 'COLOR'),1,15)
RETURN lcDesc + " "+lcColorDesc

*!*************************************************************
*! Name      : lfWeekStr
*! Developer : AHMED MAHER (AMH)
*! Date      : 05/15/2002
*! Purpose   : Get the start working day.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfWeekStr()
*!*************************************************************
*E301901,1 AMH
FUNCTION lfWeekStr
PARAMETERS ldDay

ldRetDay = ldDay - DOW(ldDay) + lnWorkDay + 1
IF ldDay < ldRetDay
  ldRetDay = ldRetDay - 7
ENDIF
RETURN ldRetDay

*!*************************************************************
*! Name      : lfvBase
*! Developer : AHMED MAHER (AMH)
*! Date      : 05/15/2002
*! Purpose   : Valid function of Base required on.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfvBase()
*!*************************************************************
*E301901,1 AMH
FUNCTION lfvBase

IF lcRpBase = 'P'
  lcRpRName = 'MATIMREQ'
ENDIF
lnItmPos = ASCAN(laOGObjType,'lcRpRName')
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGObjType,lnItmPos,1)
  laOGObjCnt[lnItmPos] = !(lcRpBase='P')
  =lfOGShowGet('lcRpRName')
ENDIF

*!*************************************************************
*! Name      : lfOpenClse
*! Developer : AHMED MAHER (AMH)
*! Date      : 05/16/2002
*! Purpose   : Open and close files
*!*************************************************************
*! Passed Parameters : lnFileNo = the file row in laFlsOpn array
*!                     llClose  = Closing this file (Yes/No)
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfOpenClse()
*!*************************************************************
*E301901,1 AMH
FUNCTION lfOpenClse
PARAMETERS lnFileNo,llClose

IF llClose .AND. laFlsOpn[lnFileNo,3] .AND. USED(laFlsOpn[lnFileNo,1])
  USE IN (laFlsOpn[lnFileNo,1])
  laFlsOpn[lnFileNo,3] = .F.
ELSE
  IF !llClose .AND. !laFlsOpn[lnFileNo,3] .AND. !USED(laFlsOpn[lnFileNo,1])
    =gfOpenFile(gcDataDir+laFlsOpn[lnFileNo,1],gcDataDir+laFlsOpn[lnFileNo,2],'SH')
    laFlsOpn[lnFileNo,3] = .T.
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfGetRef
*! Developer : AHMED MAHER (AMH)
*! Date      : 05/16/2002
*! Purpose   : Get the account and SO number for C/T or PO
*!*************************************************************
*! Passed Parameters : lcSeek = expration to seek in cutpick file
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfGetRef()
*!*************************************************************
*E301901,1 AMH
FUNCTION lfGetRef
PARAMETERS lcSeek

PRIVATE lcRef,lcOrder,lnAlias
STORE SPACE(0) TO lcRef
SET ORDER TO ORDHDR IN ORDHDR
IF SEEK(lcSeek,'CUTPICK') .AND. SEEK('O'+CUTPICK.ORDER,'ORDHDR')
  lcRef = ORDHDR.ACCOUNT + ',' + CUTPICK.ORDER
  lcOrder = CUTPICK.ORDER
  lnAlias = SELECT(0)
  SELECT CUTPICK
  LOCATE REST WHILE TranCd+cTktNo+Style = lcSeek FOR ORDER <> lcOrder
  IF FOUND()
    lcRef = lcRef + ',...'
  ENDIF
  SELECT (lnAlias)
ENDIF
RETURN lcRef

*!*************************************************************
*! Name      : lfHolDay
*! Developer : AHMED MAHER (AMH)
*! Date      : 05/20/2002
*! Purpose   : Get the start day of interval of date after removing the Holidaies from it.
*!*************************************************************
*! Passed Parameters : ldFromDate = Start date of interval
*!                     ldToDate   = End date of interval
*!*************************************************************
*! Return      : Date
*!*************************************************************
*! Example     : =lfHolDay({05/20/2002},{05/15/2002})
*!*************************************************************
*E301901,1 AMH
FUNCTION lfHolDay
PARAMETERS ldFromDate,ldToDate

PRIVATE ldCheckDay,lnWeekEnd,lcWeekEnd,lnAliac
lnAliac = SELECT(0)
ldCheckDay = ldToDate
DO WHILE ldCheckDay >= ldFromDate
  *-- Check if it is weekend day
  lnWeekEnd = DOW(ldCheckDay)-1
  lnWeekEnd = IIF(lnWeekEnd=0,7,lnWeekEnd)
  lcWeekEnd = STR(lnWeekEnd,1)
  SELECT FISHD
  LOCATE FOR BETWEEN(ldCheckDay,DFISBGDAT,DFISENDAT)
  IF FOUND() .AND. lcWeekEnd $ FISHD.CFISNONWD
    ldFromDate = ldFromDate - 1
    ldCheckDay = ldCheckDay - 1
    LOOP
  ENDIF
  *-- Check if it is holiday
  IF SEEK(FISHD.CFISFYEAR+DTOS(ldCheckDay),'FSHLD')
    ldFromDate = ldFromDate - 1
  ENDIF
  ldCheckDay = ldCheckDay - 1
ENDDO
SELECT (lnAliac)
RETURN ldFromDate

*!**************************************************************************
*! Name      : lfFabSum
*! Developer : AHMED MAHER (AMH)
*: Date      : 07/04/2002
*! Purpose   : sum a specific field for the current fabric in fabric file
*!**************************************************************************
*! Called from : Option Grid,fabric browse calculated fields.
*!*************************************************************
*! Returns     : Calculated field value.
*!*************************************************************
*! Example   : =lfFabSum()
*!*************************************************************
*E301942,1 AMH
FUNCTION lfFabSum
PARAMETERS lcFab,lccomp
PRIVATE lnFabRec
lnTotcomp = 0
IF RECCOUNT() != 0
  lnFabRec = RECNO('FABRIC')
  SELECT Fabric_X
  SUM &lcCOMP TO lnTotcomp WHILE Fabric=lcFab
  SELECT Fabric
  IF BETWEEN(lnFabRec,1,RECCOUNT())
    GO lnFabRec
  ENDIF
ENDIF  
RETURN INT(lnTotcomp)
*-- End of lfFabSum.

*!**************************************************************************
*! Name      : lfSRVFab
*! Developer : AHMED MAHER (AMH)
*: Date      : 07/04/2002
*! Purpose   : control browsing primary fabric and validate 
*!           : selecting it in inlist function.
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Example   : =lfSRVFab()
*!**************************************************************************
*! Note      : SRV symbol is [S,Set--R,Reset--V,Valid]
*!**************************************************************************
*E301942,1 AMH
FUNCTION lfSRVFab
PARAMETERS lcParm
PRIVATE lcAlias,llHaveSty
DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to primary fabric
    *-- unique index.
    USE (gcDataDir+'Fabric') AGAIN ALIAS FABRIC_X ORDER TAG FABRIC IN 0
    SELECT FABRIC   
    SET ORDER TO TAG cFabric    
    SET RELATION TO FABRIC.FABRIC INTO FABRIC_X
    GO TOP IN FABRIC
  CASE lcParm = 'R'  && Reset code
    USE IN FABRIC_X
    SELECT FABRIC
    SET ORDER TO TAG FABRIC
    llClearFab = .F.
ENDCASE
*-- End of lfSRVFab.

*!*************************************************************
*! Name      : lfStySum
*! Developer : AHMED MAHER (AMH)
*! Date      : 07/04/2002
*! Purpose   : sum a specific field for the current style in style file
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid,style browse calculated fields.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfStySum()
*!*************************************************************
*E301942,1 AMH
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnStyRec = IIF(BETWEEN(RECNO('STYLE'),1,RECCOUNT('STYLE')),RECNO('STYLE'),1)
lnTotcomp = 0
SELECT Style_X
SUM &lcCOMP TO lnTotcomp WHILE Style = ALLTRIM(lcSty)
SELECT Style
GO lnStyRec
DO CASE
  CASE lnAddToVar = 1
  	lnO_T_S = lnTotcomp
  CASE lnAddToVar = 2
  	lnO_T_S = lnO_T_S + lnTotcomp
  CASE lnAddToVar = 3
  	lnO_T_S = lnO_T_S - lnTotcomp
ENDCASE
RETURN INT(lnTotcomp)
*-- end of lfStySum.

*!*************************************************************
*! Name      : lfsrvSty
*! Developer : AHMED MAHER (AMH)
*! Date      : 07/04/2002
*! Purpose   : Rise change style flag, in range browse screen.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfsrvSty()
*!*************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!*************************************************************
*E301942,1 AMH
FUNCTION lfSRSty
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to Style Major 
    *-- unique index.
    USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
    SELECT STYLE
    SET ORDER TO TAG Cstyle
    SET RELATION TO STYLE.STYLE INTO STYLE_X
    GO TOP IN STYLE
    llChStyle = .T.
  CASE lcParm = 'R'  && Reset code
    USE IN STYLE_X
    SELECT STYLE
    SET ORDER TO TAG STYLE
ENDCASE
*-- end of lfsrvSty.
*!*************************************************************
*! Name      : lfSegInfo
*! Developer : Nader Nabil (NNA)
*! Date      : 09/09/2004
*! Purpose   : Check if The item code Structure found or not.
*!*************************************************************
*! Calls     : Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns : False or True
*!*************************************************************
*! Example   : =lfSegInfo()
*!*************************************************************
*!B038431 (Bld41)
FUNCTION lfSegInfo
lnAlias=SELECT()
llStruOp=gfOpenFile(gcDataDir+'ICISTRU','Segno','SH')
IF !SEEK('U1','ICISTRU')
  IF USED('ICISTRU') AND llStruOp
    USE IN ICISTRU
  ENDIF
  =gfModalGen('QRM42080B42001','DIALOG','Item structure not found')
  llOgtrmnat = .T.
  CLEAR READ  
  RETURN .F.
ENDIF
SELECT(lnalias)
RETURN
*-- End of function lfSegInfo.