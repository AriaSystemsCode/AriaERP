*:*************************************************************************
*: Program file  : icitemv.prg
*: Program desc. : Item Seg Value
*: For Screen    : 
*:        System : Aria 27
*:        Module : IC
*:     Developer : Hesham El-Sheltawi 
*:*************************************************************************
*: Calls       :
*:            FUNCTION : lfGetClen
*:*************************************************************************
*E300703,1 New program to add modify & delete item code segment values
EXTERNAL ARRAY laData,laDefProc

DECLARE laObjStat[9]  && Array to hold the display status of each seg.
DECLARE laAcDes[9]    && Array to hold the description of each seg.
DECLARE laSeg[9,2],laSegSz[9,1]      && Array to hold the Seg. info.
DECLARE laSegVal[9]      && Array to hold the Seg. info.
DECLARE laKeyField[2,4]
DECLARE laBarCode[7,4]  && Array to hold valid seg. types

laAcDes    = ' '        && Default blank to seg description array
llFirstTime = .F.


laObjStat  = 'ENABLE'  && Default status of all seg. is DISABLE
laObjdisp   = .F.


lnNoSeg   = 0           && Current No of segments
IF !gfSetup()
  RETURN
ENDIF

laDefProc[9]  = .F.              && Disable the control panel save proc.  (lpSavScr)
laDefProc[7]  = .F.              && Disable the control panel global delete proc. and (do lpDelScr)
lcBarDesc = 'Color   DivisionSeason  Scale   DomesticQuality Group   Other   Free    Remove  '
lcStyleFile = 'STYLE.DBF'
lcBarCode = 'CDZSTQGOF '
lcBarType = 'CCCSVVCOF '
DIMEN laBarCode[LEN(lcBarCode),4]
FOR lnCount = 1 TO LEN(lcBarCode)
  laBarCode[lnCount,1] = SUBSTR(lcBarCode,lnCount,1)
  IF laBarCode[lnCount,1] $ 'CDG'
    laBarCode[lnCount,2] = lfGetClen(IIF(laBarCode[lnCount,1]='C','CCOLOR',;
                             IIF(laBarCode[lnCount,1]='G','CSTYGROUP',;
                             IIF(laBarCode[lnCount,1]='Z','SEASON','CDIVISION'))))
  ELSE
    laBarCode[lnCount,2] = IIF(laBarCode[lnCount,1]=' ',0,IIF(laBarCode[lnCount,1]='S',3,6))
  ENDIF
  laBarCode[lnCount,3] = (SUBSTR(lcBarType,lnCount,1) $ 'VOF')
  laBarCode[lnCount,4] = SUBSTR(lcBarDesc,(lnCount-1)*8+1,8)
ENDFOR

IF !WEXIST(gcBaseWind)

  llNoShow = .F.
  DIMENSION laData[4]
  lcScFields = 'CISEGNO,CISEGVAL,CISGVALSD,CISGVALLD'
  llFirstTime = .T.
  lnNoSeg   = 0           && Current No of segments
  FOR lnCount = 1 TO 9
    laSeg[lnCount,1] = 0
  ENDFOR
  SELECT ICISTRU
  &&LOCATE
  IF !SEEK('U1')
    =gfModalGen('QRM42070B00000','DIALOG')
    glQuitting = .T.
    RETURN
  ELSE
    lnNoSeg = 0
    lcSetExact = SET('EXACT')
    SET EXACT OFF
    SCAN WHILE citemrecty+cisegno = 'U'
      lnNoSeg = lnNoSeg + 1
      laSeg[lnNoSeg,1]   = nisegsize
      laSegSz[lnNoSeg,1] = nisegsize+2
      laSeg[lnNoSeg,2]   = ALLT(cisegtype)
      laSegVal[lnNoSeg] = SPACE(laSeg[lnNoSeg,1])
      laAcDes[lnNoSeg] = SUBSTR(cisegsdes,1,laSeg[lnNoSeg,1])
    ENDSCAN
    SET EXACT &lcSetExact
  ENDIF
  SELECT (lcBaseFile)
  SCATTER FIELDS &lcScFields MEMO TO laData  BLANK
ENDIF
laKeyField[1,1] = 'LADATA[1]'
laKeyField[1,2] =.F.
laKeyField[1,3] = 'CISEGNO'
laKeyField[1,4] = 1

laKeyField[2,1] = 'LADATA[2]'
laKeyField[2,2] =.T.
laKeyField[2,3] = 'CISEGNO'
laKeyField[2,4] = 2


SELECT (lcBaseFile)
SET FILTER TO EMPTY(CVLDENTRY)
lcExtPr = gcBMPHome + "EXTKEY.BMP"
DO (gcScrDir+gcWinAppl+"\ICITEMVL.SPX")

SELECT (lcBaseFile)
SET FILTER TO

*!*************************************************************************
*! Name      : LPSHOW
*! Developer : Hesham El-Sheltawi 
*! Date      : 08/06/97    
*! Purpose   :
*!*************************************************************************
*! Returns   :  
*:*************************************************************************     
FUNCTION lpShow
STORE '' TO laSegVal
DO CASE
  CASE !laScrMode[1]
    laSegVal[VAL(laData[1])] = PADR(laData[2],laSeg[VAL(laData[1]),1])
    SHOW GET laSegVal[VAL(laData[1])]
ENDCASE

PRIVATE lnCount
lcObjKey = ''
FOR lnCount = 1 TO lnNoSeg
  IF !(laSeg[lnCount,2] $ 'OF')
     SHOW GET laSegVal[lnCount] DISABLE
     laObjStat[lnCount] = 'DISABLE'
  ELSE
     laObjStat[lnCount] = IIF(laScrMode[1],'ENABLE','DISABLE')
     lcobjnam='laSegVal['+PADR(lnCount,1)+']'
     lcObjStat =laObjStat[lnCount]
     lcObjKey = IIF(EMPTY(lcObjKey),lcobjnam,lcObjKey)
     SHOW GET &lcobjnam &lcObjStat
  ENDIF
  IF !EMPTY(lcObjKey) AND laScrMode[1]
    _CUROBJ = OBJNUM(&lcObjKey)
  ENDIF
ENDFOR

*!*************************************************************************
*! Name      : LFVNEWVAL
*! Developer : Hesham El-Sheltawi 
*! Date      : 08/06/97    
*! Purpose   :
*!*************************************************************************
*! Returns   :  
*:*************************************************************************     
FUNCTION lfvNewVal
IF LASTKEY() = 13 AND !EMPTY(EVAL(VARREAD()))
  laData[1] = SUBSTR(VARREAD(),ATC('(',VARREAD())+1,1)
  laData[2] = EVAL(VARREAD())
  llDataExist = SEEK(laData[1])
  IF Seek(LADATA[1]+LADATA[2])
    laScrMode   = .F.
    laScrMode[2]= .T.
    SCATTER FIELDS &lcScFields MEMO TO laData
  ELSE
    IF RECNO(0) >0 .AND. RECNO(0) <= RECCOUNT()
      GO RECNO(0)
    ELSE
      GO TOP
    ENDIF
    IF ALLT(laData[2])='?'
      lnOption = 1
    ELSE
      lnOption = gfModalGen('QRM00001B00001','Dialog','Seg. Value'+" "+ALLTRIM(laData[2]))
    ENDIF
    DO CASE
      CASE lnOption = 1
        IF llDataExist
          =gfBrows('laData[1]')
        ELSE
           =gfModalGen('QRM00052B00000','Dialog')
           SCATTER FIELDS &lcScFields MEMO TO laData  BLANK
        ENDIF
      CASE lnOption = 2
        IF !llAddRec
          =gfModalGen("TRM00088B00000","ALERT",'Adding')
        ELSE
          laScrMode = .F.
          laScrMode[4]=.T.
        ENDIF
      CASE lnOption = 3
        SCATTER FIELDS &lcScFields MEMO TO laData  BLANK
    ENDCASE
  ENDIF
  SHOW GETS
ENDIF


*!*************************************************************************
*! Name      : LFWHENREAD
*! Developer : Hesham El-Sheltawi 
*! Date      : 08/06/97    
*! Purpose   :
*!*************************************************************************
*! Returns   :  
*:*************************************************************************     
FUNCTION lfWhenread
IF llFirstTime
  llFirstTime = .F.
  MOVE WINDOW (gcBaseWind) CENTER
ENDIF



*!*************************************************************************
*! Name      : LFGETCLEN
*! Developer : Hesham El-Sheltawi 
*! Date      : 08/06/97    
*! Purpose   :
*!*************************************************************************
*: Passed parameters  : lcfieldname
*:*************************************************************************
*! Returns   :  
*:*************************************************************************     
FUNCTION lfGetClen
PARAMETERS lcFieldName
=gfOpenFile(gcDataDir+'CODESET','FILDNAME')
IF SEEK(PADR(UPPER(lcFieldName),10),'CODESET')
  RETURN CODESET.NFLD_WDTH
ELSE
  RETURN 6
ENDIF




*!*************************************************************************
*! Name      : LPSAVSCR
*! Developer : Hesham El-Sheltawi 
*! Date      : 04/22/98
*! Purpose   : Save/Update Style Segment value
*!*************************************************************************
*! Returns   :  
*:*************************************************************************     
PROCEDURE lpSavScr
=gfTraceKey('ICSEGVAL',LADATA[1]+LADATA[2],IIF(laScrMode[4],'A','M'))
SELECT ICSEGVAL
IF laScrMode[4]
  APPEND BLANK
  GATHER FROM laData FIELDS &lcScFields MEMO
ELSE
  GATHER FROM laData FIELDS &lcScFields MEMO
ENDIF

*!*************************************************************************
*! Name      : lpDelScr
*! Developer : Hesham El-Sheltawi 
*! Date      : 04/22/98
*! Purpose   : Delete Style Segment value
*!*************************************************************************
*! Returns   :  
*:*************************************************************************     
PROCEDURE lpDelScr
=gfTraceKey('ICSEGVAL',LADATA[1]+LADATA[2],'D')
SELECT ICSEGVAL
DELETE