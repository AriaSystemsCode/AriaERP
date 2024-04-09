*:***************************************************************************
*: Program file  : ICCTSLD
*: Program desc. : TIME DEPENDENT CUT & SOLD Calculation FOR (ANDREW MARC)
*! Date          : 02/08/2000
*: System        : Aria Advantage Series.
*: Module        : INVENTORY CONTROL (IC)
*: Developer     : Ahmed Mohamed Ibrahim (AMM)
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ICCTSLD
*:***************************************************************************
*: This Report Program is due to CP ...101732
*:***************************************************************************
*: Modifications :
*: B603955,1 ABD 10/18/2000 Call DateRng & ObjRng Screen from one place to solve
*: B603955,1 ABD            Failed to convert 00 to 2000, it converts it to 1900.
*:***************************************************************************

*-- Add this line to declare some new arrays that will be used in the
*-- filter expression that will be returned by the option grid in
*-- the case of "In list" operator [Begin]
DIMENSION laHdFlt[1] , laFxFlt[1] , laVrFlt[1]

STORE SPACE(0) TO lcRpStlSls,lcRpOrdSt,lcHoldResn,lcRpOSes,lcRpoDev
STORE {} TO ldFStrtDat , ldTStrtDat, ldFCmplDat, ldTCmplDat, ldFEtaDate, ldTEtaDate,ldFEtaDat, ldTEtaDat 


STORE 0 TO lnClrPo,lnColorLen
lcSoPoFile = gfTempName()
*lcFStrtDat                       && lcStrtDate >> for Start date (FROM)
*lcTStrtDat                       && lcStrtDate >> for Start date (TO)
*lcFCmplDat                       && lcCmplDate >> for Compelet date (FROM)
*lcTCmplDat                       && lcCmplDate >> for Compelet date (TO)
*lcFEtaDate                       && lcEtaDate  >> for Eta date FROM SHPMTHDR file (FROM)
*lcTEtaDate                       && lcEtaDate  >> for Eta date FROM SHPMTHDR file (TO)
*-- Call the Option Grid
lcRpExp = gfOpGrid('ICCTSLD' , .T.)
IF TYPE('lcRpExp') = 'L' .AND. !lcRpExp
  RETURN
ENDIF

lcRpSrtDt = 'S'
*-- Open files
=gfOpenFile(gcDataDir+'Style','Style','SH')
=gfOpenFile(gcDataDir+'OrdLine','OrdLines','SH')
=gfOpenFile(gcDataDir+'OrdHdr','OrdHdr','SH')
=gfOpenFile(gcDataDir+'POSLN','POSLNS','SH')
=gfOpenFile(gcDataDir+'POSHDR','POSHDR','SH')
=gfOpenFile(gcDataDir+'SHPMTHDR','SHPMTHDR','SH')
=gfOpenFile(gcDataDir+'ICCTSOLD','ICCTSOLD','SH')

DIMENSION lafilStru[1,4]
=Afields(lafilStru)
lnFlLen = ALEN(laFilStru,1)
DIMENSION lafilStru[lnFlLen+2,4]
laFilStru[lnFlLen+1,1] = 'LINENO'
laFilStru[lnFlLen+1,2] = 'N'
laFilStru[lnFlLen+1,3] = 6
laFilStru[lnFlLen+1,4] = 0

laFilStru[lnFlLen+2,1] = 'TRANCD'
laFilStru[lnFlLen+2,2] = 'C'
laFilStru[lnFlLen+2,3] = 1
laFilStru[lnFlLen+2,4] = 0

CREATE TABLE (gcWorkDir+lcSoPoFile) FROM ARRAY laFilStru
INDEX ON Season+cDivision+Style+DTOS(START)+DTOS(COMPLETE)+TYPE+IIF(TYPE ='S',ALLTRIM(STR(Price,9,2))+Account,ORDER + SHIPNO) TAG Date

*-- Collect Data
DO LFCOLLDATA IN (gcRepHome+'IC\ICTMCTSD') WITH .T.

=gfModalGen('INM00000B00000',.F.,.F.,.F.,'Process completed')

IF USED(lcSoPoFile)
  USE IN (lcSoPoFile)
  ERASE (gcWorkDir+lcSoPoFile+'.DBF')
  ERASE (gcWorkDir+lcSoPoFile+'.CDX')
ENDIF

*!*************************************************************
*! Name      : lfMajTtlGet
*! Developer : Ahmed Ibrahim (AMM)
*! Date      : 02/08/2000
*! Purpose   : To get the Style major segement title
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfMajTtlGet()
*!*************************************************************

FUNCTION lfMajTtGet

RETURN gfItemMask("HM")

*!*************************************************************
*! Name      : lfMajPic
*! Developer : Ahmed Ibrahim (AMM)
*! Date      : 02/08/2000
*! Purpose   : Get major seg. picture
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfMajPic()
*!*************************************************************

FUNCTION lfMajPic

lcMajPic = "@! " + gfItemMask("PM")

RETURN lcMajPic

*!*************************************************************
*! Name      : lfNonMaj
*! Developer : Ahmed Ibrahim (AMM)
*! Date      : 02/08/2000
*! Purpose   : To get the Style nonmajor segement structure
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfNonMaj()
*!*************************************************************

FUNCTION lfNonMaj

*-- Compute Free/Color Items in Style Structure. [Begin]
lnMajSeg  = gfItemMask('SM')  && No. of major segments.
DIMENSION laMajSeg[1,1]
= gfItemMask(@laMajSeg)

llStopConc = .F.

*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSeg,1)

  lnNonMajPo = IIF(lnNonMajPo = 0,laMajSeg[lnI,4],lnNonMajPo)

  IF laMajSeg[lnI,1] = 'F' AND !llStopConc
  
    lcFreeClr  = IIF(EMPTY(lcFreeClr),laMajSeg[lnI,1],lcFreeClr)

    lcNonMajPi = IIF(EMPTY(lcNonMajPi),laMajSeg[lnI,3],;
                     lcNonMajPi + laMajSeg[lnI-1,6] + laMajSeg[lnI,3])

    lcNonMajT  = IIF(EMPTY(lcNonMajT),PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])),;
                     lcNonMajT + laMajSeg[lnI-1,6] + PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])))

  ENDIF

  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSeg[lnI,1] = 'C' OR (!EMPTY(lcFreeClr) AND laMajSeg[lnI,1] != 'F')

    IF laMajSeg[lnI,1] = 'C'

      lnClrPo    = laMajSeg[lnI,4]

      lcFreeClr  = laMajSeg[lnI,1]    && which will be 'C'
  
      lcNonMajPi = laMajSeg[lnI,3]

      lcNonMajT  = PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3]))
  
      EXIT
  
    ELSE
      
      *-- this means that another type is found rather than color or free
      *-- and so we neednot to concat. to free variables
      llStopConc = .T.
      
    ENDIF

  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.

ENDFOR    && end Loop Around Non Major elements.

STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
lcColorTt = 'Only This ' + ALLTRIM(lcNonMajT)
*-- Compute Free/Color Items in Style Structure. [End]

RETURN ''

*!*************************************************************
*! Name      : lfClrRead
*! Developer : Ahmed Ibrahim (AMM)
*! Date      : 02/08/2000
*! Purpose   : To clear the read to show or hide options in grid.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example   : =lfClrRead()
*!*************************************************************

FUNCTION lfClrRead

CLEAR READ


*!**************************************************************************
*! Name      : lfSetSTY 
*! Developer : Ahmed Ibrahim (AMM)
*! Date      : 02/08/2000
*! Purpose   : Go top in the Style IN RANGE
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
*! Example   : =lfSetSty()
*!**************************************************************************
FUNCTION lfSetSty  
PARAMETERS lcOGParm

DO CASE
  CASE lcOGParm = 'S'
   SET ORDER TO TAG CStyle IN Style   
   GO TOP
  CASE lcOGParm = 'R'
    SET ORDER TO TAG Style IN Style
ENDCASE


*!*************************************************************
*! Name      : lfvDateRng
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 01/31/2000
*! Purpose   : Showes date range screen
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : DateRng.spr
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvDateRng()
*!*************************************************************
FUNCTION lfvDateRng

PRIVATE ldFrom,ldTo
ldFrom = ldFEtaDat
ldTo   = ldTEtaDat

lcTitle = 'Production E.T.A date range'

*B603955,1 ABD -Call DateRng & ObjRng Screen from one place to solve
*B603955,1 ABD -Failed to convert 00 to 2000, it converts it to 1900. [Begin]
*DO (gcRepHome + gcAct_Appl + '\DateRng.SPR')   && Run the advance payment screen 
DO DateRng.Spx
*B603955,1 ABD - [End]


*!*************************************************************
*! Name      : lfvPbOk
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 10/23/1999
*! Purpose   : Validate date range screen's OK button
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvPbOk()
*!*************************************************************

FUNCTION lfvPbOk

IF ldFrom > ldTo
   WAIT WINDOW ["From" value must be less than or equal to "To" value] NOWAIT
  _CUROBJ = OBJNUM(ldFrom)
ELSE
  ldFEtaDat = ldFrom
  ldTEtaDat = ldTo
  CLEAR READ
ENDIF

