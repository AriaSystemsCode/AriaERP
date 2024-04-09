*:***************************************************************************
*: Program file  : MFPRCTJI.PRG
*: Program desc. : Print Cutting Tickets for JILL STUART
*: For Report    : MFPRCTJI.FRX
*: System        : Aria Advantage Series VER. 4XP
*: Module        : Manufactoring
*: Developer     : AYMAN MAHMOUD AHMED (AYM)
*: Reference     : N130721
*:***************************************************************************
*: Passed Parameters  : None.
*:***************************************************************************
*:Modifications:
*:***************************************************************************
*-- Temporary file of the cutting ticket report.

lcTmpFile = gfTempName()
lcVenPos  = lfCheckFilter(1, 'POSHDR.VENDOR')
llVenFlag  = USED(lcVenPos) and RECCOUNT(lcVenPos)> 0  && flag indicate user select Cuttkt#
lcVendName = ''
lcRltColor = SPACE(0)
DECLARE laColor[1,2]
laColor[1,1] = 'CLRLNAME'
laColor[1,2] = 'lcRltColor'
IF llrPrtCs
 SELECT * FROM &lccostf INTO CURSOR 'lccostf2'
 SELECT lccostf2
 = CURSORGETPROP("Buffering","lcCostF2")
 =CURSORSETPROP("Buffering",3,"lcCostF2")
 INDEX ON CutTkt+Typ+cCatgTyp+Item TAG 'lcCostF2'
 SET ORDER TO TAG 'lcCostF2'
 SELECT (lcCostF)
 DELETE ALL
ENDIF
IF TYPE('loMFGOPRHD') <> 'O'
  loMFGOPRHD = CreateObject('RemoteTable','MFGOPRHD','TKTOPER','MFGOPRHD',SET("Datasession"))
ENDIF
lcMFGOPRHDSel="Select cimtyp,ctktno,coprcode,ccontcode,ccontname,coperseq,linhouse,coprcomnt From mfgoprhd(index=tktoper)"
loMFGOPRHD.sqlrun(lcMFGOPRHDSel,"TMFGOPRHD",.T.)
SELECT MFGOPRHD
APPEND FROM DBF("TMFGOPRHD")
=lfcreatTmpFile()
=lfGetColor()
=lfFillTmpFile()
SELECT(lcmainf)
SET RELATION TO
DELETE ALL
APPEND FROM DBF(lcTmpFile)
SELECT (lcTmpFile)
SET RELATION TO "1"+PO+IIF(&lcTmpFile..NoteFlag='N',style,SPACE(20)) INTO CutpickF ADDITIVE
SET RELATION TO STYLE INTO STYLE ADDITIVE 
SELECT STYLE
SET RELATION OFF INTO SCALE
SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE 
IF llRPrtAlo
  SELECT &lcTmpFile
  SET SKIP TO CutpickF 
ENDIF
IF llRpPic
  SELECT &lcTmpFile
  SET RELATION TO 'S'+ &lcTmpFile..Hdrstyle INTO Objlink ADDITIVE
  GO TOP
ENDIF 
SELECT &lcTmpFile
LOCATE
*!*************************************************************
*! Name      : lfGetColor
*! Developer : Ahmed Maher
*! Date      : 04/19/2001
*! Purpose   : Get the color information.
*!*************************************************************
*! Calls     : gfItemMask
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : lfGetColor()
*!*************************************************************
*
FUNCTION lfGetColor

DECLARE laStySeg[1,1]
lnMjorCnt  = gfItemMask("SM")
=gfItemMask(@laStySeg)
FOR lnCnt = lnMjorCnt + 1 TO ALEN(laStySeg,1)
  IF laStySeg[lnCnt , 1] = "C"
    llColorExt = .T.
    lnColorStr = laStySeg[lnCnt , 4]
    lnColorLen = LEN(laStySeg[lnCnt , 3])
  ENDIF
ENDFOR

*!*************************************************************
*! Name      : lfVldVen
*! Developer : Ahmed Maher
*! Date      : 04/17/2001
*! Purpose   : Check Vendors of C/T.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : .T./.F.
*!*************************************************************
*! Example   : lfVldVen()
*!*************************************************************
FUNCTION lfVldVen
LPARAMETERS lcselvendor
PRIVATE llRet, lcOrder, lnAlias,llFound
llRet = .F.
lnAlias = SELECT(0)
SELECT (lcVenPos)
LOCATE FOR &lcVenPos..VENCODE=lcselvendor
  IF FOUND()
     llRet = .T.
     IF SEEK ('M'+&lcmainf.po,"MFGOPRHD") .AND. !LINHOUSE
      SELECT MFGOPRHD
      lcVendName = CCONTNAME
     ELSE
      lcVendName = ''
     ENDIF
  ENDIF 
SELECT (lnAlias)
RETURN llRet .AND. llFound
*-- end of lfVldVen.
*!*************************************************************
*! Name      : lfGetOpr
*! Developer : Ahmed Maher
*! Date      : 04/19/2001
*! Purpose   : Get Opration No. X of C/T.
*!*************************************************************
*! Parameters: lcSequance
*!*************************************************************
*! Returns   : Opration Description
*!*************************************************************
*! Example   : lfGetOpr()
*!*************************************************************
FUNCTION lfGetOpr
PARAMETERS lcSequance
PRIVATE lcRet, lcOrder, lnAlias
lcRet = ''
lnAlias = SELECT(0)
**IF loMFGOPRHD.SEEK('M'+&lcmainf..po+lcSequance)
IF SEEK('M'+&lcmainf..po+lcSequance,"MFGOPRHD")
  SELECT MFGOPRHD
  lcRet = gfCodDes(COPRCODE, 'MFGCODE')
ENDIF
SELECT (lnAlias)
RETURN lcRet
*-- end of lfGetOpr.
*!*************************************************************
*! Name      : lfFillTmpFile
*! Developer : Omar Shaban (OMS)
*! Date      : 10/16/2005
*! Purpose   : Fill File With Custom Data 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfFillTmpFile
SELECT &lcmainf &&TPOSHDR &&Temp poshdr that created from Stanader report
SCAN 
    IF  llVenFlag .AND. !lfVldVen(&lcmainf.Vendor) &&.AND. !EMPTY (lcRpVendor)
      LOOP
    ENDIF
    lcOpr1 = lfGetOpr('1')
    lcOpr2 = lfGetOpr('2')
    SCATTER MEMVAR
    m.cVendor     = lcVendName
    m.cOpration1  = lcOpr1
    m.cOpration2  = lcOpr2
    INSERT INTO (lcTmpFile) FROM MEMVAR
    IF llrPrtCs &&.AND. SEEK('M'+&lcmainf..PO,'lccostf2')
	   IF SEEK(&lcmainf..PO,LCCOSTF)
	   ELSE
	   	 SELECT lccostf2
	     SCAN FOR CutTkt= &lcmainf..PO
          SCATTER MEMVAR
          INSERT INTO (lcCostF) FROM MEMVAR 
         ENDSCAN 
	   ENDIF 
    ENDIF
ENDSCAN  && End loop of file.
*!*************************************************************
*! Name      : lfcreatTmpFile
*! Developer : Omar shaban (OMS)
*! Date      : 10/16/2005
*! Purpose   : crate temp file for posln 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfcreatTmpFile
SELECT &lcmainf
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
lnStru2 = lnFileStru + 1 
DIMENSION laFileStru[lnFileStru + 4 , 18]

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'cVendor'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 30
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'cOpration1'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 30
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'cOpration2'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 30
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'ColorLName'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 50
laFileStru[lnFileStru,4] = 0

FOR  lnLen = 7 TO 18
  FOR lnCount = 0 TO 4
    STORE SPACE(1) TO laFileStru[lnFileStru - lnCount,lnLen]
  ENDFOR 
ENDFOR
gfCrtTmp(lcTmpFile,@laFileStru,"PO+cWareCode+Style+Dyelot+NoteFlag",lcTmpFile,.F.)

*!*************************************************************
*! Name      : lfCheckFilter
*! Developer : Ahmed Salah Shalaby(SSH)
*! Date      : 08/10/2005
*! Purpose   : Check filter Type
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : Filter Values
*!*************************************************************
FUNCTION lfCheckFilter
LPARAMETERS lnArrayType, lcFilter

LOCAL lcReturn, lnPOS   
lcReturn = ""     
DO CASE
CASE lnArrayType = 1 
  lnPOS = ASCAN(loOgScroll.laOGFxFlt,lcFilter)
  IF lnPos > 0
    lnPOS = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPos,1)
    lcReturn = loOgScroll.laOGFxFlt[lnPOS,6]
  ENDIF
CASE lnArrayType = 2  
  lnPOS = ASCAN(loOgScroll.laOGHDFlt,lcFilter) 
  IF lnPos > 0
    lnPOS = ASUBSCRIPT(loOgScroll.laOGHDFlt,lnPos,1)
    lcReturn = loOgScroll.laOGHDFlt[lnPOS,6]    
  ENDIF
CASE lnArrayType = 3  
  lnPOS = ASCAN(loOgScroll.laOGvrFlt,lcFilter) 
  IF lnPos > 0
    lnPOS = ASUBSCRIPT(loOgScroll.laOGvrFlt,lnPos,1)
    lcReturn = loOgScroll.laOGvrFlt[lnPOS,6]    
  ENDIF
ENDCASE
RETURN lcReturn


* End Of Program 
