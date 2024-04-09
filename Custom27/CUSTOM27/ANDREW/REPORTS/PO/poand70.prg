**************************************************************************
*: Program file  : POAND70.PRG (C# 101392)
*: Program desc. : CUSTOM STYLE PO SHIPMENT STATUS REPORT.
*:                 Convert AND700.PRG from 2.6 to 2.7
*:         System: Aria Apparel System
*:      Developer: AHMED SALAH SHALABY - (SSH)
*:************************************************************************
*: Calls : FUNCTIONS  : 
*:           lfShpVia,lfvShpNo,lfwRepWhen,lfFillDiv,lfDefDiv.
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************
*: Modifications:
*: C101707,1 ADEL 11/25/1999 Senior modifications in OG,PRG and FRX.
*:************************************************************************

*--- Initializing Variables
lnRecNo   = 0
lcOldVal  = ''
lcDivName = ''
lnMajPic  = LEN(gfItemMask("PM"))
*C101707,1 (Begin) Remark the following line and initialize Season name.
*lcDivisn  = lcRpDiv
*--- Function to get division desc.
*lcDivName = gfCodDes(lcDivisn,'CDIVISION ')
lcSeaName = ''
*--Initialize variables on which the OG criteria will be printed.
STORE .F. TO llSeason,llDivision,llShipMent,llStatus,llEntered,llEta,llStyle,llFabric,llGroupCh
STORE '' TO lcSeason,lcStatus,lcDivision,lcShipMent,lcEntered,lcEta,lcStyle,lcFabric
*C101707,1 (End)
lcSortFld = ' '
DO CASE
  *-- SUMMARY BY SHIPMENT#
  CASE lcRPSortBy = 'S'
    lcSortFld = 'SHIPNO'
  *-- SUMMARY BY Style
  CASE lcRPSortBy  = 'T'
    lcSortFld  = 'SUBSTR(STYLE,1,lnMajPic)'
  *--SUMMARY BY E.T.A. DATE
  CASE lcRPSortBy = 'E'
    lcSortFld = 'DTOS(ETA)+SHIPNO'
  *-- SUMMARY BY ENTERED DATE
  CASE lcRPSortBy = 'N'
    lcSortFld = 'DTOS(ENTERED)+SHIPNO'
ENDCASE
*C101707,1 (Begin) Always begin index with 'Season+Division'
lcSortFld = 'SEASON+DIVISION+'+lcSortFld
*C101707,1 (End)

****** Report File & Memory Loop Begin ******
SELECT SHPMTHDR

*C101707,1 (Begin) Remark the following lines and set relation into POSLN and STYLE for the new OG options.
*SET RELATION TO
*SET FILTER TO
SET RELATION TO SHIPNO INTO POSLN ADDITIVE
SELECT POSLN
SET RELATION TO STYLE INTO STYLE ADDITIVE
SELECT SHPMTHDR
*C101707,1 (End)
GOTO TOP
WAIT WINDOW  'Selecting records for report ...' NOWAIT
LOCATE ALL FOR &lcRpExp
IF EOF()
  *--- Text  'NO RECORDS SELECTED FOR REPORT!'
  = gfModalGen('TRM00052B00000','DIALOG' )
  RETURN
ENDIF

WORKFILE = gfTempName()
SET TALK ON
COPY REST TO &gcWorkDir.&WORKFILE FOR &lcRpExp
SET TALK OFF
=gfOpenFile('&gcWorkDir.&WORKFILE','','EX')
=gfOpenFile('&gcDataDir.SHPDATES','&gcDataDir.SHPDATES','SH')
lcAllTmp = gfTempName()

*--- Tran. Code (2) for recieve (3) for shipped.
*C101707,1 (Begin) Remark and chnage the collection way to get season and division fields for the new index.
*SELECT POSLN.*,&WORKFILE..cVessel,&WORKFILE..Entered,&WORKFILE..Eta;
      FROM POSLN,(gcWorkDir+WORKFILE);
      WHERE  (POSLN.ShipNo+ "P" +POSLN.Po = &WORKFILE..ShipNo ) ;
         AND INLIST(PosLn.TranCd,'2','3') ;
      INTO  DBF (gcWorkDir+lcAllTmp)
*--Add this function to collect data.
SELECT SHPMTHDR
SET RELATION TO
SELECT POSLN
SET RELATION TO SHIPNO INTO SHPMTHDR ADDITIVE
=lfCollect()
*C101707,1 (End)      
*---Erase Temp. Files
USE IN &WORKFILE
ERASE &gcWorkDir.&WORKFILE+'.DBF'

SET RELATION TO "P"+Po INTO POSHDR
*--Activate the relation.
GO TOP
*C101707,1 (Begin) Remark the following line as division filter has been done above.
*--Delete the records not facing the entered division code.
*DELETE FOR POSHDR.cDivision <> lcDivisn
*C101707,1 (End).

GO TOP
IF EOF()
  *---Text 'NO RECORDS SELECTED FOR REPORT!'
  = gfModalGen('TRM00052B00000','DIALOG' )
  USE IN &lcAllTmp
  ERASE &gcWorkDir.&lcAllTmp+'.DBF'
  ERASE &gcWorkDir.&lcAllTmp+'.CDX' 
  USE IN SHPDATES
  RETURN
ENDIF
*--- Sort to lcAllTmp Index
Z = LTRIM(STR(RECCOUNT(),7))
WAIT WINDOW  'SORTING &Z RECORDS FOR THE CUSTOM SHIPMENT STATUS REPORT ...' NOWAIT
INDEX ON &lcSortFld TAG &lcAllTmp
SET ORDER TO TAG &lcAllTmp

*--Set a relation to SHPDATES file to get the customs,trucker and warehouse dates in the FRX.
SET RELATION TO ShipNo INTO SHPDATES ADDITIVE
*--Print Landscape
IF gcDevice='PRINTER'
  @ PROW(),PCOL() SAY [&l1O&l14]
ENDIF

SELECT (lcAllTmp)
*--Count records to be used in FRX
COUNT TO lnRecNo
GO TOP
*SET DEVICE TO SCREEN

*C101707,1 (Begin) Prepare selected criteria to be printed on the first page of RX.
=lfCriteria()
*C101707,1 (End) 
DO gfDispRe WITH EVAL('lcFormName')
*--Release Landscape
SET DEVICE TO PRINTER
IF gcDevice='PRINTER'
  @ PROW(),PCOL() SAY [&l0O]
ENDIF
SET DEVICE TO SCREEN
USE IN &lcAllTmp
ERASE &gcWorkDir.&lcAllTmp+'.DBF'
ERASE &gcWorkDir.&lcAllTmp+'.CDX' 
USE IN SHPDATES

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : AHMED SALAH SHALABY (SSH)
*! Date      : 04/02/1999
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ...
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen


*-- Initilize the date range with system date.
lnDatePos = ASCAN(laOGFxFlt,"SHPMTHDR.ENTERED")
IF lnDatePos > 0
  lnDatePos = ASUBSCRIPT(laOGFxFlt,lnDatePos,1)
  IF EMPTY(laOGFxFlt[lnDatePos,6])
    laOGFxFlt[lnDatePos,6] = DTOC(gdSysDate) + '|' + DTOC(gdSysDate)
  ENDIF  
ENDIF

lnDatePos = ASCAN(laOGFxFlt,"SHPMTHDR.ETA")
IF lnDatePos > 0
  lnDatePos = ASUBSCRIPT(laOGFxFlt,lnDatePos,1)
  IF EMPTY(laOGFxFlt[lnDatePos,6])
    laOGFxFlt[lnDatePos,6] = DTOC(gdSysDate) + '|' + DTOC(gdSysDate)
  ENDIF  
ENDIF

R_WIDTH    = 'XW'
lcFormName = 'AND700A'
lcOldVal = SPACE(01)
*C101707,1 (Begin) Open SYCCOMP to get the company name for FRX.
=gfOpenFile(gcSysHome+'SYCCOMP',gcSysHome+'Ccomp_id','SH')
=SEEK(gcAct_Comp)
*--Now we are on the proper record and will print from file directly.
*C101707,1 (End)

*!*************************************************************
*! Name      : lfShpVia
*! Developer : AHMED SALAH SHALABY (SSH)
*! Date      : 04/02/1999
*! Purpose   : Print POSHDR division
*!*************************************************************
*! Called from : REPORT FRX
*!*************************************************************
*! Calls       : ...
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfShpVia()
*!*************************************************************
FUNCTION lfShpVia

*C101707,1 (Begin) Get shipvia from SHPDATES file instead of POSHDR and print 3 chrs only. 
*IF SEEK('N'+POSHDR.SHIPVIA+'N'+'SHIPVIA   ','CODES')
*RETURN (SUBSTR(CODES.cDiscRep,1,8))
*ELSE
*RETURN (SPACE(08))
*ENDIF
IF SEEK('N'+SHPDATES.SHIPVIA+'N'+'SHIPVIA   ','CODES')
  RETURN (SUBSTR(CODES.cDiscRep,1,3))
ELSE
  RETURN (SPACE(03))
ENDIF
*C101707,1 (End)

*!*************************************************************
*! Name      : lfvShpNo
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 04/02/1999
*! Purpose   : Valid function for Shipment #
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
FUNCTION lfvShpNo
PRIVATE lcVar,lcObj,laTemp,lcBrowFields

lcVar = SYS(18)
lcObj = EVALUATE(SYS(18))
lcPrevAl = SELECT(0)

DECLARE laTemp[1]
SELECT SHPMTHDR
SET ORDER TO Shpmthdr
DIMENSION laTemp[1]
IF !EMPTY(lcObj) AND !(SEEK(lcObj , 'SHPMTHDR'))
  *C101707,1 (Begin) Cure lcBrFields as it gives error 'Attempt to use FoxPro function as an array' 
  *C101707,1         and 'Variable Season not found' because this field doesn't exist in the file.
  *C101707,1         Give a title to the browse.
  *  lcBrFields ="SHIPNO    :R :H='Shipment #'   ,"+;
                 "IIF(STATUS='O','Open',IIF(STATUS='H','Hold','Cancelled')) :R :H='Status',"+;
                 "ENTERED   :R :H='Entered'      ,"+;
                 "ETA       :R :H='E.T.A.'       ,"+;
                 "SEASON    :R :H='Season'       ,"+;
                 "TOTQTY    :R :H='TOTQTY'       ,"+;
                 "RECV_STK  :R :H='Received'     ,"+;
                 "RECV_DAM  :R :H='Damaged'      ,"+;
                 "CVESSEL   :R :H='VESSEL'      ,"+;
                 "RECV_CAN  :R :H='Cancelled' "
  *  = gfBrows(' ','SHIPNO','laTemp')                 
  lcBrFields ="SHIPNO    :R :H='Shipment #'   ,"+;
              "STATUS=IIF(STATUS='O','Open',IIF(STATUS='H','Hold','Cancelled')) :R :H='Status',"+;  
              "ENTERED   :R :H='Entered'      ,"+;
              "ETA       :R :H='E.T.A.'       ,"+;
              "TOTQTY    :R :H='TOTQTY'       ,"+;
              "RECV_STK  :R :H='Received'     ,"+;
              "RECV_DAM  :R :H='Damaged'      ,"+;
              "CVESSEL   :R :H='VESSEL'      ,"+;
              "RECV_CAN  :R :H='Cancelled' "
  = gfBrows(' ','SHIPNO','laTemp','Shipment')
  *-- Put these lines here and remark them below to be able to clear any range 
  *-- in shipment no.
  IF !EMPTY(laTemp[1])
    lcObj = laTemp[1]
  ELSE
    lcObj = lcOldVal
  ENDIF
  &lcVar = lcObj      && Update the field
  *C101707,1 (End)                
ELSE
  laTemp[1] = lcObj
ENDIF
*-- IF The user selected a record.
*C101707,1 (Begin) Remark the lines below and put them above.
*IF !EMPTY(laTemp[1])
*  lcObj = laTemp[1]
*ELSE
*  lcObj = lcOldVal
*ENDIF
*&lcVar = lcObj      && Update the field
*C101707,1 (End)
SELECT (lcPrevAl)

*!*************************************************************
*! Name      : lfOldVal
*! Developer : Ahmed Salah Shalaby -(SSH)
*! Date      : 04/02/1999
*! Purpose   : Keeps the old value of the editor when cancel the browse
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfOldVal

lcOldVal = EVALUATE(SYS(18))

*!*************************************************************
*! Name      : lfFillDiv
*! Developer : Ahmed Salah Shalaby -(SSH)
*! Date      : 04/02/1999
*! Purpose   : Fill Division Array.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*C101707,1 (Begin) Rename this function to Remark it as we will allow the  user to anter multiple divisions.
*FUNCTION lfFillDiv
FUNCTION lfFillDiv1
PRIVATE lnOldAls,lcOldOrd

DIMENSION laDivDis[1,1],laDivCode[1,1]
laDivDis  = ''
laDivCode = ''
lnOldAls = SELECT(0)
SELECT CODES
lcOldOrd = SET('ORDER')
SET ORDER TO cCode_No
lnIndex = 1
=SEEK('N'+'CDIVISION ')
SCAN REST WHILE cdefcode+cfld_name+ccode_no+cdiscrep+crltd_nam = ;
                'N' + 'CDIVISION '  FOR cRltField = 'N'
  DIMENSION laDivDis[lnIndex,1],laDivCode[lnIndex,1]
  laDivCode[lnIndex,1] = cCode_No
  laDivDis[lnIndex,1]  = ALLTRIM(cCode_No)+' - '+cDiscRep
  lnIndex = lnIndex + 1
ENDSCAN
SET ORDER TO &lcOldOrd
SELECT(lnOldAls)

*!*************************************************************
*! Name      : lfDefDiv
*! Developer : Ahmed Salah Shalaby -(SSH)
*! Date      : 04/02/1999
*! Purpose   : Set division default value.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfDefDiv
RETURN laDivCode[1,1]


*!*************************************************************
*! Name      : lfwOldVal
*! Developer : Ahmed Salah Shalaby -(SSH)
*! Date      : 04/02/1999
*! Purpose   : Save old valuse for style range in OG.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfwOldVal

lcOldVal = EVAL(SYS(18))

****************************************************************************
* FUNC: lfvStyle
* DESC: To valid the style.
* AUTH: Adel Mohammed El Gazzar (ADEL)
* DATE: 11/25/99
* Refer To : (C101707)
****************************************************************************
FUNCTION lfvStyle

PRIVATE lnCurSelct,lcStyOrder
lnCurSelct = SELECT(0)
SELECT STYLE
lcStyOrder = ORDER()
SET ORDER TO cStyle 
*-- Varible to hold  the name of the memory variable used to create the current GET field
lcObjName = SYS(18)
*-- Varible to hold  the value of the current GET field
lcObjVal = EVALUATE(SYS(18)) 

*--IF The user want to Browse or if the Style he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'STYLE'))
  lcObjVal = gfStyBrw('M',"","",.F.)  &&Browse style major only.
  lcObjVal = IIF(!EMPTY(lcObjVal) , lcObjVal , lcOldVal)
  &lcObjName = lcObjVal
ENDIF 

SELECT STYLE
SET ORDER TO &lcStyOrder
SELECT (lnCurSelct)


*!*************************************************************
*! Name      : lfvFabric
*! Developer : Adel Mohammed El Gazzar (ADEK)
*! Date      : 11/22/99
*! Purpose   : Validation function for validating Fabric Code
*!*************************************************************
*! Called from : Only this color [Option Grid]
*!*************************************************************
*! Calls       : FaBrow()
*! Refer to    : C101707,1
*!*************************************************************
FUNCTION lfvFabric
PRIVATE lnAlias

lnAlias   = SELECT(0)
lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal  = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field
SELECT Fabric
lcFabOrder = ORDER()
SET ORDER TO Fabric
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'FABRIC'))
  llObjRet = FaBrow(@lcObjVal , '*')
  lcObjVal = IIF(llObjRet , lcObjVal , lcOldVal)
  &lcObjName = lcObjVal
ENDIF    && End of IF
SELECT Fabric
SET ORDER TO &lcFabOrder
SELECT(lnAlias)


*!*************************************************************
*! Name      : lfGetNames
*! Developer : Adel Mohammed El Gazzar (ADEK)
*! Date      : 11/22/99
*! Purpose   : Get seasons and divisions' names.
*!*************************************************************
*! Called from : FRX.
*!*************************************************************
*! Refer to    : C101707,1
*!*************************************************************
FUNCTION lfGetNames

lcDivName = gfCodDes(Division,'CDIVISION ')
lcSeaName = gfCodDes(Season,'SEASON    ')
RETURN ''

*!*************************************************************
*! Name      : lfCollect
*! Developer : Adel Mohammed El Gazzar (ADEK)
*! Date      : 11/22/99
*! Purpose   : Collect data.
*!*************************************************************
*! Refer to    : C101707,1
*!*************************************************************
FUNCTION lfCollect

SELECT POSLN
=AFIELDS(laFldsStru)
lnNoOfArr = ALEN(laFldsStru,1)
*ADEL
*DIMENSION laFldsStru[lnNoOfArr+5,4]
DIMENSION laFldsStru[lnNoOfArr+6,4]
*-- cVessel field
laFldsStru[lnNoOfArr+1,1] = 'cVessel'
laFldsStru[lnNoOfArr+1,2] = 'C'
laFldsStru[lnNoOfArr+1,3] = 30
laFldsStru[lnNoOfArr+1,4] = 0
*-- Entered field
laFldsStru[lnNoOfArr+2,1] = 'Entered'
laFldsStru[lnNoOfArr+2,2] = 'D'
laFldsStru[lnNoOfArr+2,3] = 8
laFldsStru[lnNoOfArr+2,4] = 0
*-- ETA field
laFldsStru[lnNoOfArr+3,1] = 'ETA'
laFldsStru[lnNoOfArr+3,2] = 'D'
laFldsStru[lnNoOfArr+3,3] = 8
laFldsStru[lnNoOfArr+3,4] = 0
*-- Season field
laFldsStru[lnNoOfArr+4,1] = 'Season'
laFldsStru[lnNoOfArr+4,2] = 'C'
laFldsStru[lnNoOfArr+4,3] = 6
laFldsStru[lnNoOfArr+4,4] = 0
*-- Division field
laFldsStru[lnNoOfArr+5,1] = 'Division'
laFldsStru[lnNoOfArr+5,2] = 'C'
laFldsStru[lnNoOfArr+5,3] = 6
laFldsStru[lnNoOfArr+5,4] = 0
*--Season+Division change field
laFldsStru[lnNoOfArr+6,1] = 'llGrpChng'
laFldsStru[lnNoOfArr+6,2] = 'l'
laFldsStru[lnNoOfArr+6,3] = 1
laFldsStru[lnNoOfArr+6,4] = 0

*--Create table .
CREATE DBF (gcWorkDir+lcAllTmp) FROM ARRAY laFldsStru
*--Get line for filtered WORKFILE .
SELECT (WORKFILE)
*adel
lcGrpDiv = &lcAllTmp..Season+&lcAllTmp..Division
SCAN
  lcShipNo = ShipNo
  IF SEEK(lcShipNo,'POSLN')
    SELECT POSLN
    SCAN WHILE shipno+cstytype+po+style+STR(lineno,6)+trancd  = lcShipNo;
         FOR   INLIST(TranCd,'2','3') AND &lcRpExp
      SCATTER MEMVAR MEMO
      m.cVessel = &WORKFILE..cVessel
      m.Entered = &WORKFILE..Entered
      m.Eta     = &WORKFILE..Eta
      =SEEK("P"+Po,'POSHDR')
      m.Division = POSHDR.cDivision
      =SEEK(Style,'STYLE')
      m.Season = STYLE.Season
      INSERT INTO (lcAllTmp) FROM MEMVAR
      IF &lcAllTmp..Season+&lcAllTmp..Division <>lcGrpDiv
        REPLACE &lcAllTmp..llGrpChng with .T.
        lcGrpDiv = &lcAllTmp..Season+&lcAllTmp..Division    
      ENDIF
    ENDSCAN
  ENDIF  
ENDSCAN
SELECT (lcAllTmp)

*!***************************************************************************
*! Name      : lfCriteria
*! Developer : Adel Mohammed El Gazzar (ADEK)
*! Date      : 11/22/99
*! Purpose   : Prepare selected criteria to be printed on the first page of FRX.
*!*****************************************************************************
*! Refer to    : C101707,1
*!*****************************************************************************
FUNCTION lfCriteria

IF llRpPrtCrt
  lcSeaVal   = lfRtFltVal('STYLE.SEASON')
  lcSeason   = 'Season         [' +lcSeaVal+' ]'
  lcDivVal   = lfRtFltVal('STYLE.CDIVISION')
  lcDivision = 'Division        [' +lcDivVal+' ]'
  lcShipVal  = lfRtFltVal('SHPMTHDR.SHIPNO')
  llShipMent = !EMPTY(lcShipMent)
  lcShipMent = 'Shipment#    [ From : ' +lcShipVal+' ]'
  lcStatVal  = lfRtFltVal('SHPMTHDR.STATUS')
  lcStatus   = 'Status          [ ' +lcStatVal+' ]'
  lcStatus   = STRTRAN(lcstatus,'O','Open')
  lcStatus   = STRTRAN(lcstatus,'C','Complete')
  lcStatus   = STRTRAN(lcstatus,'X','Canceled')
  lcEntered  = 'Ship Date     [ From : ' +STRTRAN(ALLTRIM(laOgFxFlt[ASUBSCRIPT(laOgFxFlt , ASCAN(laOgFXFlt,'SHPMTHDR.ENTERED'),1),6]),'|',' TO:   ')+' ]'
  lcEtA      = 'E.T.A Date   [ From : ' +STRTRAN(ALLTRIM(laOgFxFlt[ASUBSCRIPT(laOgFxFlt , ASCAN(laOgFXFlt,'SHPMTHDR.ETA'),1),6]),'|',' TO:   ')+' ]'
  lcStyle    = 'Style            [ From : ' +STRTRAN(ALLTRIM(laOgVrFlt[ASUBSCRIPT(laOgVrFlt , ASCAN(laOgVrFlt,'STYLE.CSTYMAJOR'),1),6]),'|',' TO:   ')+' ]'
  lcFabric   = 'Fabric          [ From : ' +STRTRAN(ALLTRIM(laOgVrFlt[ASUBSCRIPT(laOgVrFlt , ASCAN(laOgVrFlt,'STYLE.FABRIC'),1),6]),'|',' TO:   ')+' ]'
  llSeason   = !EMPTY(lcSeaVal) 
  llDivision = !EMPTY(lcDivVal)
  llStatus   = !EMPTY(lcStatVal) 
  llEntered  = ATC('ENTER',lcRpExp) >0
  llEta      = ATC('ETA',lcRpExp) >0
  llStyle    = ATC('CSTYMAJOR',lcRpExp) >0
  llFabric   = ATC('STYLE.FABRIC',lcRpExp) >0
ENDIF  
RETURN ''

*!***************************************************************************
*! Name      : lfRtFltVal
*! Developer : Adel Mohammed El Gazzar (ADEK)
*! Date      : 12/07/99
*! Purpose   : Return filter options value.
*!*****************************************************************************
*! Refer to    : C101707,1
*!*****************************************************************************
FUNCTION lfRtFltVal
PARAMETERS lcStrVal
PRIVATE lnPos

lnPos = ASCAN(laOgFXFlt,lcStrVal)
IF lnPos >0 
  RETURN STRTRAN(ALLTRIM(laOgFxFlt[ASUBSCRIPT(laOgFxFlt , lnPos,1),6]),'|',', ')
ELSE
  RETURN ''
ENDIF    


FUNCTION lfGroupCh

SKIP
llGroupCh = llGrpChng OR EOF()
SKIP-1
RETURN ''