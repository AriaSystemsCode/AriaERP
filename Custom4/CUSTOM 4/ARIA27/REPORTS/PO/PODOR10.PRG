*:----------------------------------------------------------------------------
*: Program file        : MFDOR10.PRG
*: Program description : Actual Style Cost Sheet.
*: For System          : Aria Advantage Series - Version 2.7
*: For Module          : Manufactring - (MF)
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : C#101828  FOR DORBY
*:----------------------------------------------------------------------------
*: Calls               : None.
*:----------------------------------------------------------------------------
*: Called From         : - MF/OutPut/TransAction/MFDOR10
*:----------------------------------------------------------------------------
*: Passed Parameters   : None.
*:----------------------------------------------------------------------------
*: Example             : DO MFDOR10
*:----------------------------------------------------------------------------
*:modification : 
*: B#803240,1 SSH Increase font
*: B#803240,1 SSH Remove '/' from sizes
*: B#803240,1 SSH remove selling price box to end of the bage
*: B#803240,1 SSH display only .999 for each cost
*: B#803240,1 SSH fix the frx to not request A4
*:----------------------------------------------------------------------------
*--- Print the FRX from (lcRpToPrn)
*--- Get the temp Po file name
lnSum = 0
DIMENSION laLables[5,2]
laLables[1,1]  = 'M_CISLBL1'
laLables[2,1]  = 'M_CISLBL2'
laLables[3,1]  = 'M_CISLBL3'
laLables[4,1]  = 'M_CISLBL4'
laLables[5,1]  = 'M_CISLBL5'
=gfGetMemVar(@laLables,gcAct_Comp)
IF llOgFltCh
  lnStyPos = lfItmPos('STYLE.CSTYMAJOR','F')
  IF USED(laOgFxFlt[lnStyPos,6])
    lcRpStyAls = laOgFxFlt[lnStyPos,6]
  ENDIF
  lnStyPos = lfItmPos('POSHDR.PO','F')
  IF USED(laOgFxFlt[lnStyPos,6])
    lcRpPOAls = laOgFxFlt[lnStyPos,6]
  ENDIF
  lcSea = '' 
  lcDiv = ''
  lcClr = ''
  lcGrp = ''
  llThrIsSty = .F.
  =lfChekSty()
  *--- SEASON
  lnSeaPos = lfItmPos('STYLE.SEASON','')
  IF !EMPTY(laOgVrFlt[lnSeaPos,6])
    lcSea = laOgVrFlt[lnSeaPos,6]
  ENDIF
  *--- Devision
  lnDivPos = lfItmPos('STYLE.CDIVISION','')
  IF !EMPTY(laOgVrFlt[lnDivPos,6])
    lcDiv = laOgVrFlt[lnDivPos,6]
  ENDIF
  *--- Style Goup
  lnGrpPos = lfItmPos('STYLE.CSTYGROUP','')
  IF !EMPTY(laOgVrFlt[lnGrpPos,6])
    lcGrp = laOgVrFlt[lnGrpPos,6]
  ENDIF
  *--- Style Color
  lnClrPos = lfItmPos('SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen)','F')
  IF !EMPTY(laOgFxFlt[lnClrPos,6])
    lcClr = laOgFxFlt[lnClrPos,6]
  ENDIF
  lcKey = ''
  lcVal = ''
  lnMajLen = LEN(ALLTRIM(SUBSTR(lcStylePic,3)))
  *--- Temp File to print from.
  IF USED(lcRpToPrn)
    SELECT (lcRpToPrn)
    ZAP
    USE
  ENDIF
  SELECT BOMLINE
  COPY STRUC TO (gcWorkDir)+(lcRpToPrn)
  =gfOpenFile(gcWorkDir+lcRpToPrn,'','EX')
  SELECT (lcRpToPrn)
  INDEX ON SUBSTR(STYLE,1,lnMajLen)+CTKTNO TAG (lcRpToPrn)
  =lfGetData()
ENDIF
SELECT(lcRpToPrn)
SET RELATIO TO
SET RELATION TO STYLE INTO STYLE ADDIT
SET RELATION TO 'P'+cTktNo INTO POSHDR ADDIT
SET RELATION TO PADR(ITEM,7)+IClr INTO FABRIC ADDIT
GOTO TOP
DO gfDispRe WITH EVAL('lcRpForm')

*:----------------------------------------------------------------------------
*: Program file        : lfChekSty.
*: Program description : Function to Check if the user select Styles.
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : C#101828  FOR DORBY
*:----------------------------------------------------------------------------
*: Calls               : None.
*:----------------------------------------------------------------------------
*: Passed Parameters   : None.
*:----------------------------------------------------------------------------
*: Example             : = lfChekSty ()
*:----------------------------------------------------------------------------
*:
FUNCTION lfChekSty
PRIVATE llToRet , lnOld

lnOld = SELECT(0)
llToRet = .F.
IF USED(lcRpStyAls)
  SELECT (lcRpStyAls)
  GOTO TOP
  llToRet = !EOF()
ENDIF
SELECT(lnOld)
RETURN(llToRet)

*:----------------------------------------------------------------------------
*: Program file        : lfChekPO.
*: Program description : Function to Check if the user select PO's.
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : C#101828  FOR DORBY
*:----------------------------------------------------------------------------
*: Calls               : None.
*:----------------------------------------------------------------------------
*: Passed Parameters   : None.
*:----------------------------------------------------------------------------
*: Example             : = lfChekPO ()
*:----------------------------------------------------------------------------
*:
FUNCTION lfChekPO

PRIVATE llToRet , lnOld
lnOld = SELECT(0)
llToRet = .F.

IF USED(lcRpPOAls)
  SELECT (lcRpPOAls)
  GOTO TOP
  llToRet = !EOF()
ENDIF
SELECT(lnOld)
RETURN(llToRet)

*:----------------------------------------------------------------------------
*: Program file        : lfGetData
*: Program description : Function to decide which way we are use to collect data .
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : C#101828  FOR DORBY
*:----------------------------------------------------------------------------
*: Calls               : lfChekSty,lfChekPO,lfColect.
*:----------------------------------------------------------------------------
*: Passed Parameters   : None.
*:----------------------------------------------------------------------------
*: Example             : = lfGetData ()
*:----------------------------------------------------------------------------
*:
FUNCTION lfGetData

DO CASE
  CASE  (lfChekPO()  .AND. lfChekSty()) .OR.;
        (lfChekPO()  .AND. !lfChekSty())
    lcKey = 'cstytype+po+style+STR(lineno,6)+trancd'
    =lfColect(.T.,.T.)  && Get data By (PO & Style) OR By PO Only.
  CASE  !lfChekPO() .AND. lfChekSty()
    lcKey = 'style+cstytype+po+STR(lineno,6)+trancd'
    =lfColect(.F.,.T.)  && Get data By Style Only
  CASE  !lfChekPO() .AND. !lfChekSty()
    lcKey = ''
    =lfColect(.F.,.F.)  && Get All data 
ENDCASE

*:----------------------------------------------------------------------------
*: Program file        : lfGetData
*: Program description : Function to Colect data.
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : C#101828  FOR DORBY
*:----------------------------------------------------------------------------
*: Calls               : lfChekSty.
*:----------------------------------------------------------------------------
*: Passed Parameters   : None.
*:----------------------------------------------------------------------------
*: Example             : = lfGetData ()
*:----------------------------------------------------------------------------
*:
FUNCTION lfColect
PARAMETER llByPO,llByStyle

DO CASE
  CASE  llByPO  .AND. llByStyle
    IF lfChekSty()
      SELECT STYLE
      SET ORDER TO cStyle
      SELECT(lcRpStyAls)
      SET RELATION TO
      SET RELATION TO ALLTRIM(cstymajor) INTO STYLE ADDITIVE
    ENDIF
    SELECT (lcRpPOAls)
    GOTO TOP
    SCAN
      lcTempPO = 'P'+PO
      SELECT POSLN
      SET ORDER TO POSLN
      =SEEK(lcTempPO)
      SCAN REST WHILE &lcKey = lcTempPO;
                FOR  IIF(lfChekSty(),SEEK(SUBSTR(POSLN.STYLE,1,lnMajLen),lcRpStyAls),.T.);
                 .AND. IIF(!EMPTY(lcSea),Style.Season$lcSea,.T.);
                 .AND. IIF(!EMPTY(lcDiv),Style.cDivision$lcDiv,.T.);
                 .AND. IIF(!EMPTY(lcGrp),Style.cStyGroup$lcGrp,.T.);
                 .AND. IIF(!EMPTY(lcClr),SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen)$lcClr,.T.);
                 .AND. !SEEK(SUBSTR(STYLE,1,lnMajLen)+PO,lcRpToPrn)
        lcTempSty = ALLTRIM(Style.cstymajor)
        SELECT BOMLINE
        =SEEK('I'+'1'+POSLN.Po)
        SCAN REST WHILE cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode=;
                        'I'+'1'+POSLN.Po;
                  FOR IIF(lfChekSty(),Style = lcTempSty,.T.)
          SCAT MEMVAR MEMO
          IF SEEK(SUBSTR(STYLE,1,lnMajLen)+CTKTNO,lcRpToPrn)
            lcTempKey  = SUBSTR(STYLE,1,lnMajLen)+CTKTNO
            lcClr      = SUBSTR(STYLE,lnNonMajSt,lnColorLen)
            SELECT (lcRpToPrn)
            LOCATE REST WHILE SUBSTR(STYLE,1,lnMajLen)+CTKTNO=;
                              lcTempKey;
                        FOR SUBSTR(STYLE,lnNonMajSt,lnColorLen)=lcClr
            IF FOUND()
              INSERT INTO (lcRpToPrn) FROM MEMVAR
            ENDIF
          ELSE
            INSERT INTO (lcRpToPrn) FROM MEMVAR
          ENDIF
        ENDSCAN
      ENDSCAN
    ENDSCAN
    SELECT POSLN
    SET RELATION TO
  CASE  !llByPO .AND. llByStyle
    SELECT STYLE
    SET ORDER TO cStyle
    SELECT(lcRpStyAls)
    SET RELATION TO
    SET RELATION TO ALLTRIM(cstymajor) INTO STYLE ADDITIVE
    GOTO TOP
    SCAN
      lcTempSty = ALLTRIM(cStyMajor)
      SELECT POSLN
      SET ORDER TO Poslns
      =SEEK(lcTempSty)
      SCAN REST WHILE &lcKey = lcTempSty;
                FOR    IIF(!EMPTY(lcSea),Style.Season$lcSea,.T.);
                 .AND. IIF(!EMPTY(lcDiv),Style.cDivision$lcDiv,.T.);
                 .AND. IIF(!EMPTY(lcGrp),Style.cStyGroup$lcGrp,.T.);
                 .AND. IIF(!EMPTY(lcClr),SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen)$lcClr,.T.);
                 .AND. !SEEK(SUBSTR(STYLE,1,lnMajLen)+PO,lcRpToPrn)
        SELECT BOMLINE
        =SEEK('I'+'1'+POSLN.Po)
        SCAN REST WHILE cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode=;
                        'I'+'1'+POSLN.Po;
                  FOR Style = lcTempSty
          SCAT MEMVAR MEMO
          IF SEEK(SUBSTR(STYLE,1,lnMajLen)+CTKTNO,lcRpToPrn)
            lcTempKey  = SUBSTR(STYLE,1,lnMajLen)+CTKTNO
            lcClr      = SUBSTR(STYLE,lnNonMajSt,lnColorLen)
            SELECT (lcRpToPrn)
            LOCATE REST WHILE SUBSTR(STYLE,1,lnMajLen)+CTKTNO=;
                              lcTempKey;
                        FOR SUBSTR(STYLE,lnNonMajSt,lnColorLen)=lcClr
            IF FOUND()
              INSERT INTO (lcRpToPrn) FROM MEMVAR
            ENDIF
          ELSE
            INSERT INTO (lcRpToPrn) FROM MEMVAR
          ENDIF
        ENDSCAN
      ENDSCAN
      SELECT POSLN
      SET RELATION TO
    ENDSCAN
  CASE  !llByPO .AND. !llByStyle
    SELECT STYLE
    SET ORDER TO Style
    SELECT POSLN
    SET RELATION TO
    SET RELATION TO Style INTO STYLE ADDITIVE
    SELECT POSLN
    SCAN FOR    IIF(!EMPTY(lcSea),Style.Season$lcSea,.T.);
          .AND. IIF(!EMPTY(lcDiv),Style.cDivision$lcDiv,.T.);
          .AND. IIF(!EMPTY(lcGrp),Style.cStyGroup$lcGrp,.T.);
          .AND. IIF(!EMPTY(lcClr),SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen)$lcClr,.T.);
          .AND. !SEEK(SUBSTR(STYLE,1,lnMajLen)+PO,lcRpToPrn)
      lcTmpSty = ALLTRIM(Style.cStyMajor)
      SELECT BOMLINE
      =SEEK('I'+'1'+POSLN.Po)
      m.cTktNo = ''
      SCAN REST WHILE cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode=;
                      'I'+'1'+POSLN.Po;
                FOR Style = lcTmpSty
        SCAT MEMVAR MEMO
        IF SEEK(SUBSTR(STYLE,1,lnMajLen)+CTKTNO,lcRpToPrn)
          lcTempKey  = SUBSTR(STYLE,1,lnMajLen)+CTKTNO
          lcClr      = SUBSTR(STYLE,lnNonMajSt,lnColorLen)
          SELECT (lcRpToPrn)
          LOCATE REST WHILE SUBSTR(STYLE,1,lnMajLen)+CTKTNO=;
                            lcTempKey;
                      FOR SUBSTR(STYLE,lnNonMajSt,lnColorLen)=lcClr
          IF FOUND()
            INSERT INTO (lcRpToPrn) FROM MEMVAR
          ENDIF
        ELSE
          INSERT INTO (lcRpToPrn) FROM MEMVAR
        ENDIF
      ENDSCAN
      SELECT POSLN
      LOCATE REST FOR PO <> m.cTktNo
    ENDSCAN
ENDCASE


*:----------------------------------------------------------------------------
*: Program file        : lfwRep
*: Program description : Option grid when function.
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : C#101828  FOR DORBY
*:----------------------------------------------------------------------------
*: Calls               : None.
*:----------------------------------------------------------------------------
*: Passed Parameters   : None.
*:----------------------------------------------------------------------------
*: Example             : = lfwRep ()
*:----------------------------------------------------------------------------
*:
FUNCTION lfwRep

SELECT POSLN
SET FILTER TO
SELECT POSHDR
SET FILTER TO
SET RELATION TO
SET RELATION TO Vendor INTO ApVendor ADDITIVE
GOTO TOP


*:----------------------------------------------------------------------------
*: Program file        : lfEvalSegs.
*: Program description : Evaluate style segments.
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : C#101828  FOR DORBY
*:----------------------------------------------------------------------------
*: Calls               : None.
*:----------------------------------------------------------------------------
*: Passed Parameters   : None.
*:----------------------------------------------------------------------------
*: Example             : = lfEvalSegs ()
*:----------------------------------------------------------------------------
*:
FUNCTION lfEvalSegs
*-- Compute Free/Color Items in Style Structure. [Begin]
lnMajSeg  = gfItemMask('SM')  && No. of major segments.
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
  
*-- if you does not find Non Major Type Color Code.
IF !lfNMajType('C',lnMajSeg)  
  = lfNMajType('F',lnMajSeg)  && Check for Non Major Type Free code.
ENDIF  && end if you does not find Non Major Type Color Code.

STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
lcColorTlt = 'Only These ' + ALLTRIM(lcNonMajTlt) + 's.'
*-- Compute Free/Color Items in Style Structure. [End]
RETURN ''
*-- end of lfEvalSegs.


*:----------------------------------------------------------------------------
*: Program file        : lfNMajType.
*: Program description : Non Major Mask.
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : C#101828  FOR DORBY
*:----------------------------------------------------------------------------
*: Calls               : None.
*:----------------------------------------------------------------------------
*: Passed Parameters   : None.
*:----------------------------------------------------------------------------
*: Example             : = lfNMajType ()
*:----------------------------------------------------------------------------
*:
FUNCTION lfNMajType
PARAMETERS lcNMajType,lnMajSegs

*-- Loop Around Non Major elements.
FOR lnI = lnMajSegs + 1 TO ALEN(laMajSegs,1)

  IF laMajSegs[lnI,1] = lcNMajType

    lcFree_Clr = IIF(EMPTY(lcFree_Clr),laMajSegs[lnI,1],lcFree_Clr)
    lnNonMajSt = IIF(lnNonMajSt = 0,laMajSegs[lnI,4],lnNonMajSt)

    lcNonMajPi = IIF(EMPTY(lcNonMajPi),laMajSegs[lnI,3],;
                     lcNonMajPi + laMajSegs[lnI-1,6] + laMajSegs[lnI,3])

    lcNonMajTl = IIF(EMPTY(lcNonMajTl),PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])),;
                     lcNonMajTl + laMajSegs[lnI-1,6] + PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])))

  ENDIF

  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSegs[lnI,1] = 'C' OR (!EMPTY(lcFree_Clr) AND laMajSegs[lnI,1] != 'F')
    EXIT
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.

ENDFOR    && end Loop Around Non Major elements.

RETURN !EMPTY(lcFree_Clr)
*-- end of lfNMajType. 



*:----------------------------------------------------------------------------
*: Program file        : lfsrvSty.
*: Program description : Rise change style flag, in range browse screen..
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : C#101828  FOR DORBY
*:----------------------------------------------------------------------------
*: Calls               : None.
*:----------------------------------------------------------------------------
*: Passed Parameters   : None.
*:----------------------------------------------------------------------------
*: Example             : =lfsrvSty()
*:----------------------------------------------------------------------------
*!
FUNCTION lfSRVSty
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


*:----------------------------------------------------------------------------
*: Program file        : lfStySum.
*: Program description : sum a specific field for the current style in style file.
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : C#101828  FOR DORBY
*:----------------------------------------------------------------------------
*: Calls               : None.
*:----------------------------------------------------------------------------
*: Passed Parameters   : None.
*:----------------------------------------------------------------------------
*: Example             : =lfStySum()
*:----------------------------------------------------------------------------
*!
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnStyRec = IIF(RECNO('STYLE') <= RECCOUNT('STYLE'),RECNO('STYLE'),1)
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


*:----------------------------------------------------------------------------
*: Program file        : lfsrCut.
*: Program description : Rise change account flag, in range browse screen.
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : C#101828  FOR DORBY
*:----------------------------------------------------------------------------
*: Calls               : None.
*:----------------------------------------------------------------------------
*: Passed Parameters   : None.
*:----------------------------------------------------------------------------
*: Example             : =lfsrCut()
*:----------------------------------------------------------------------------
*!
FUNCTION lfsrSPO
PARAMETERS lcParm
=lfSetFilt()
DO CASE
  CASE lcParm = 'S'
    *llChAcc = .T.
    GO TOP IN POSHDR
  CASE lcParm = 'R'
    llClearSPO = .F.
ENDCASE
*-- end of lfsrAcc.


*:----------------------------------------------------------------------------
*: Program file        : lfItmPos.
*: Program description : Function to get item positoin in laogfxflt/laogvrflt.
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : C#101828  FOR DORBY
*:----------------------------------------------------------------------------
*: Calls               : None.
*:----------------------------------------------------------------------------
*: Passed Parameters   : None.
*:----------------------------------------------------------------------------
*: Example             : =lfItmPos()
*:----------------------------------------------------------------------------
*!
FUNCTION lfItmPos
PARAMETERS lcItmInFlt,lcArrType
PRIVATE lnItmPos
IF lcArrType = 'F'
  lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
  IF lnItmPos > 0
    lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
 ENDIF
ELSE
  lnItmPos = ASCAN(laOgVrFlt,lcItmInFlt)
  IF lnItmPos > 0
    lnItmPos = ASUBSCRIPT(laOgVrFlt,lnItmPos,1)
 ENDIF
ENDIF
RETURN lnItmPos
*-- End of lfItmPos.


*:----------------------------------------------------------------------------
*: Program file        : lfSetFilt.
*: Program description : Function to set filter.
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : C#101828  FOR DORBY
*:----------------------------------------------------------------------------
*: Calls               : None.
*:----------------------------------------------------------------------------
*: Passed Parameters   : None.
*:----------------------------------------------------------------------------
*: Example             : =lfSetFilt()
*:----------------------------------------------------------------------------
*!
FUNCTION lfSetFilt
PARAMETER llToSet

PRIVATE lnStyPos , lnOld
lnOld = SELECT(0)
lcRpPOAls = ''
lnStyPos = lfItmPos('STYLE.CSTYMAJOR','F')
IF USED(laOgFxFlt[lnStyPos,6])
  lcRpStyAls = laOgFxFlt[lnStyPos,6]
  SELECT (lcRpStyAls)
  GOTO TOP
  IF EOF()
    SELECT POSHDR
    SET FILTER TO
    SELECT POSLN
    SET FILTER TO
  ELSE
    IF lcParm = 'S'
      SELECT POSLN
      SET FILTER TO
      SET ORDER TO Poslns
      lnMajLen = LEN(ALLTRIM(SUBSTR(lcStylePic,3)))
      SET FILTER TO SEEK(SUBSTR(Style,1,lnMajLen),lcRpStyAls)
      SET ORDER TO Posln
      SELECT POSHDR
      SET FILTER TO
      SET ORDER TO POSHDR
      SET FILTER TO SEEK('P'+PO,'POSLN')
      GOTO TOP
    ENDIF
  ENDIF
ELSE
  SELECT POSHDR
  SET FILTER TO
  SELECT POSLN
  SET FILTER TO
ENDIF
SELECT(lnOld)


*:----------------------------------------------------------------------------
*: Program file        : lfClearRep.
*: Program description : Function to Clear temp.
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : C#101828  FOR DORBY
*:----------------------------------------------------------------------------
*: Calls               : None.
*:----------------------------------------------------------------------------
*: Passed Parameters   : None.
*:----------------------------------------------------------------------------
*: Example             : =lfClearRep()
*:----------------------------------------------------------------------------
*!
FUNCTION lfClearRep

llOgFltCh = .T.
USE IN IIF(USED(lcRpToPrn),lcRpToPrn,0)
ERASE &gcWorkDir.&lcRpToPrn+'.DBF'
ERASE &gcWorkDir.&lcRpToPrn+'.CDX'

USE IN IIF(USED(lcRpToPrn),lcRpToPrn,0)
ERASE &gcWorkDir.&lcRpToPrn+'.DBF'
ERASE &gcWorkDir.&lcRpToPrn+'.CDX'


*:----------------------------------------------------------------------------
*: Program file        : lfGetScl.
*: Program description : Function to Get the scale FROM (FRX).
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : C#101828  FOR DORBY
*:----------------------------------------------------------------------------
*: Calls               : None.
*:----------------------------------------------------------------------------
*: Passed Parameters   : None.
*:----------------------------------------------------------------------------
*: Example             : =lfGetScl()
*:----------------------------------------------------------------------------
*!
FUNCTION lfGetScl

*: B#803240,1 SSH Remove '/'
*lcToReturn = IIF(SEEK('S'+Style.Scale,'SCALE'),;
                                   IIF(!EMPTY(SCALE.SZ1),SCALE.SZ1+' / ','')+;
                                   IIF(!EMPTY(SCALE.SZ2),SCALE.SZ2+' / ','')+;
                                   IIF(!EMPTY(SCALE.SZ3),SCALE.SZ3+' / ','')+;
                                   IIF(!EMPTY(SCALE.SZ4),SCALE.SZ4+' / ','')+;
                                   IIF(!EMPTY(SCALE.SZ5),SCALE.SZ5+' / ','')+;
                                   IIF(!EMPTY(SCALE.SZ6),SCALE.SZ6+' / ','')+;
                                   IIF(!EMPTY(SCALE.SZ7),SCALE.SZ7+' / ','')+;
                                   IIF(!EMPTY(SCALE.SZ8),SCALE.SZ8,''),'')
*lcToReturn = IIF(SUBSTR(lcToReturn,LEN(lcToReturn)-2)=' / ',;
                 SUBSTR(lcToReturn,1,LEN(lcToReturn)-2),;
                 lcToReturn)

lcToReturn = IIF(SEEK('S'+Style.Scale,'SCALE'),;
                                   IIF(!EMPTY(SCALE.SZ1),SCALE.SZ1+'  ','')+;
                                   IIF(!EMPTY(SCALE.SZ2),SCALE.SZ2+'  ','')+;
                                   IIF(!EMPTY(SCALE.SZ3),SCALE.SZ3+'  ','')+;
                                   IIF(!EMPTY(SCALE.SZ4),SCALE.SZ4+'  ','')+;
                                   IIF(!EMPTY(SCALE.SZ5),SCALE.SZ5+'  ','')+;
                                   IIF(!EMPTY(SCALE.SZ6),SCALE.SZ6+'  ','')+;
                                   IIF(!EMPTY(SCALE.SZ7),SCALE.SZ7+'  ','')+;
                                   IIF(!EMPTY(SCALE.SZ8),SCALE.SZ8,''),'')
lcToReturn = IIF(SUBSTR(lcToReturn,LEN(lcToReturn)-2)=' ',;
                 SUBSTR(lcToReturn,1,LEN(lcToReturn)-2),;
                 lcToReturn)
*: B#803240,1 SSH [End]
RETURN(lcToReturn)


*:----------------------------------------------------------------------------
*: Program file        : lfSum.
*: Program description : Function to Get Total cost, FROM (FRX).
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : C#101828  FOR DORBY
*:----------------------------------------------------------------------------
*: Calls               : None.
*:----------------------------------------------------------------------------
*: Passed Parameters   : None.
*:----------------------------------------------------------------------------
*: Example             : =lfSum()
*:----------------------------------------------------------------------------
*!
FUNCTION lfSum

lnSum = 0
PRIVATE lnOld 
lnOld = SELECT(0)
lcCurSty = Style
SELECT BOMLINE
=SEEK('I'+'1'+POSHDR.Po)
SUM REST (UnitQty*UnitCost) TO lnSum ;
     WHILE cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode=;
           'I'+'1'+POSHDR.Po;
     FOR Style = lcCurSty
SELECT (lnOld)


*:----------------------------------------------------------------------------
*: Program file        : lfCostTy.
*: Program description : Function to Return cost type, FROM (FRX).
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : C#101828  FOR DORBY
*:----------------------------------------------------------------------------
*: Calls               : None.
*:----------------------------------------------------------------------------
*: Passed Parameters   : None.
*:----------------------------------------------------------------------------
*: Example             : =lfCostTy()
*:----------------------------------------------------------------------------
*!
FUNCTION lfCostTy

DO CASE
  CASE cBomTyp = '1'
    RETURN(laLables[1,2])
  CASE cBomTyp = '2'
    RETURN(laLables[2,2])
  CASE cBomTyp = '3'
    RETURN(laLables[3,2])
  CASE cBomTyp = '4'
    RETURN(laLables[4,2])
  CASE cBomTyp = '5'
    RETURN(laLables[5,2])
ENDCASE