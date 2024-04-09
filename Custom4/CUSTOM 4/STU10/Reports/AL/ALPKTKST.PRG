*:***************************************************************************
*: PROG : ALPKTKST.PRG                FOR : STUNT
*: DESC : Converted piktk from 26 to 27 FORMAT A
*: Modul: Aria Apparel Series.
*: DATE : 09/09/01
*: AUTH : Adel Mohammed El Gazzar (ADEL)
*: Refer to  : (C102289)
*:************************************************************************
*: Calls : 
*:         FUNCTION  : 
*:                   : 
*:                   : 
*:         PROCEDURE : 
***************************************************************************

SELECT (lcTmpOrdL)
GOTO TOP
IF EOF()
  *-- Message : There are no records to display...!
  =gfModalGen('TRM00052B40011','ALERT')
  SET DEVICE TO SCREEN
  llNoRec = .T.
  RETURN
ELSE
  llNoRec = .F.
ENDIF
loOgScroll.lcOGLastForm = 'ALPKTKST'
loOgScroll.cCRorientation = 'P'
loOgScroll.lcLogoPath = ''


lcDistFile = loogscroll.gftempname()
*--Initialize needed variables.
STORE '' TO m.PikTkt,m.Order,m.cShipVia,m.Account,m.cTermData,m.cWDesc,m.cSpcInst,m.PikDate
STORE '' TO m.cBtName,m.cBtAdd1,m.cBtAdd2,m.cBtAdd3,m.cBtAdd4,m.cBtAdd5,m.cBtAdd6,m.cSpcInst,m.cSeason,m.Store
STORE '' TO m.cStName,m.cStAdd1,m.cStAdd2,m.cStAdd3,m.cStAdd4,m.cStAdd5,m.cStAdd6,m.cCustPO,lcStrln1
lcNote1 = ''
lcNote2 = ''

STORE 0  TO lnColorLen,lnNonMajSt,lnRow,lnValue
STORE {} TO ldDate
DIMENSION laAddress[1,1]
STORE '' TO laAddress,m.HLINE2,m.HLINE3,m.HLINE4,m.HLINE5
*--Get the phone format.
lcPhnFrmt = gfPhoneTem()
lcComName = SycComp.cCom_Name
*-- Get the style major and color.
*-Get the style major length
lnMajLen = LEN(gfItemMask('PM'))
*--Get the No. of major segments.
lnMajSeg = gfItemMask('SM')
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
*-- Get the Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
  IF laMajSegs[lnI,1] = 'C'
    lnNonMajSt = laMajSegs[lnI,4]
    lnColorLen = LEN(IIF(lnColorLen = 0 .OR. laMajSegs[lnI,1]='C',;
                 laMajSegs[lnI,3],;
                 lnColorLen + laMajSegs[lnI-1,6] + laMajSegs[lnI,3]))
    EXIT
  ENDIF
ENDFOR 
lnShift = -2
*--Restore the global flags.
llDyeLot   = (gfGetMemVar('M_DYELOT')  = 'Y')
lcFile  = 'ALPKTKST'
IF FILE( "&lcFile..MEM" )
  RESTORE ADDITIVE FROM &lcFile
ELSE
  IF gfModalGen('TRM00000B00006',.F.,.F.,.F.,"Do you wish to print the short color description?") = 1
    lcClrLDsc = 'Y'
  ELSE
    lcClrLDsc = 'N'
  ENDIF
  *- Save variable as default to be used later.
  SAVE ALL LIKE lcClrLDsc TO &lcFile
ENDIF
llWareHous  = (gfGetMemVar('M_WareHouse') = 'Y')

*--Open files
DO lpOpenFiles
*--Create or Open Summary temp file.
IF !USED('&lcSummary')
  lcSummary = &lcSummary
  SELECT ORDLINE
  COPY STRUCTURE TO (gcWorkDir+lcSummary)
  =gfOpenFile(oAriaApplication.WorkDir+lcSummary,'','EX')
  INDEX ON STYLE+STR(LINENO,6) TAG &lcSummary
ELSE
  SELECT (lcSummary)
  ZAP
ENDIF

    *....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....
 A='-------------------------------------------------------------------------------'
 B='  SHIPVIA       | SEASON |SPCL INSTRUCTIONS | STORE#   | DEPT# | PURCH ORDER  |'
B1='                |        |                  |          |       |              |'
 IF llDyeLot
 C='   G STYLE   CLR     DESCRIPTION              DYELOT #        PRICE      AMOUNT'
 ELSE
 C='   G STYLE   CLR     DESCRIPTION                              PRICE      AMOUNT'
 ENDIF
   *....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....
D='   _______ ___ ____________________ ___  ___  ___  ___  ___  ___  ___  ___ ____'
E='|  BILL OF LADING | # CARTONS | WEIGHT| PICKED BY | PACKED BY | SHIPPED VIA   |'

F='|                 |           |       |           |           |'
G='|  MERCHANDISE    |  FREIGHT  | INSUR | OTHER CHGS| TERMS     | COMMENTS:     |'
H='|  $              | $         | $     | $         |           |               |'
   *....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....

** DYELOT # IS PRINTED ONLY FOR SYSTEMS USING DYELOTS ***
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....
*   G STYLE   CLR     DESCRIPTION              DYELOT #         PRICE      AMOUNT
*12 X 1234567 123 12345678901234567890         XXXXXXXXXX     1234.99  123,456.99
*     SIZE: 12345 12345 12345 12345 12345 12345 12345 12345
*     ORDR: 12345 12345 12345 12345 12345 12345 12345 12345
*     ALLO: 12345 12345 12345 12345 12345 12345 12345 12345
*     PICK: 12345 12345 12345 12345 12345 12345 12345 12345

*------------------------------
* SECTION: MAIN LOOP
*------------------------------
NEWDOC = .T.
XTIME = TIME()
*-------------
*-- MAIN LOOP
*-------------
*!*	CLEAR TYPEAHEAD
*!*	SET DEVICE TO PRINT
*-- Get the data file.
*-- This file holds all the lines of the selected pick tickets.
DIMENSION laFileStru[63,4]
laFileStru[1,1] = 'NOTE1'
laFileStru[1,2] = 'C'
laFileStru[1,3] = 30
laFileStru[1,4] = 0

laFileStru[2,1] = 'NOTE2'
laFileStru[2,2] = 'C'
laFileStru[2,3] = 30
laFileStru[2,4] = 0

laFileStru[3,1] = 'cShipVia'
laFileStru[3,2] = 'C'
laFileStru[3,3] = 10
laFileStru[3,4] = 0


laFileStru[4,1] = 'cTermData'
laFileStru[4,2] = 'C'
laFileStru[4,3] = 10
laFileStru[4,4] = 0

laFileStru[5,1] = 'STYLE'
laFileStru[5,2] = 'C'
laFileStru[5,3] = 19
laFileStru[5,4] = 0

laFileStru[6,1] = 'GROUP'
laFileStru[6,2] = 'C'
laFileStru[6,3] = 1
laFileStru[6,4] = 0

laFileStru[7,1] = 'SDESC'
laFileStru[7,2] = 'C'
laFileStru[7,3] = 20
laFileStru[7,4] = 0

laFileStru[8,1] = 'DYELOT'
laFileStru[8,2] = 'C'
laFileStru[8,3] = 10
laFileStru[8,4] = 0

laFileStru[9,1] = 'PRICE'
laFileStru[9,2] = 'N'
laFileStru[9,3] = 7
laFileStru[9,4] = 2

laFileStru[10,1] = 'TOTPIK'
laFileStru[10,2] = 'N'
laFileStru[10,3] = 10
laFileStru[10,4] = 2

laFileStru[11,1] = 'cLongClr'
laFileStru[11,2] = 'C'
laFileStru[11,3] = 30
laFileStru[11,4] = 0

laFileStru[12,1] = 'SZ1'
laFileStru[12,2] = 'C'
laFileStru[12,3] = 6
laFileStru[12,4] = 0

laFileStru[13,1] = 'SZ2'
laFileStru[13,2] = 'C'
laFileStru[13,3] = 6
laFileStru[13,4] = 0

laFileStru[14,1] = 'SZ3'
laFileStru[14,2] = 'C'
laFileStru[14,3] = 6
laFileStru[14,4] = 0

laFileStru[15,1] = 'SZ4'
laFileStru[15,2] = 'C'
laFileStru[15,3] = 6
laFileStru[15,4] = 0

laFileStru[16,1] = 'SZ5'
laFileStru[16,2] = 'C'
laFileStru[16,3] = 6
laFileStru[16,4] = 0

laFileStru[17,1] = 'SZ6'
laFileStru[17,2] = 'C'
laFileStru[17,3] = 6
laFileStru[17,4] = 0

laFileStru[18,1] = 'SZ7'
laFileStru[18,2] = 'C'
laFileStru[18,3] = 6
laFileStru[18,4] = 0

laFileStru[19,1] = 'SZ8'
laFileStru[19,2] = 'C'
laFileStru[19,3] = 6
laFileStru[19,4] = 0

laFileStru[20,1] = 'QTY1'
laFileStru[20,2] = 'N'
laFileStru[20,3] = 5
laFileStru[20,4] = 0

laFileStru[21,1] = 'QTY2'
laFileStru[21,2] = 'N'
laFileStru[21,3] = 5
laFileStru[21,4] = 0

laFileStru[22,1] = 'QTY3'
laFileStru[22,2] = 'N'
laFileStru[22,3] = 5
laFileStru[22,4] = 0

laFileStru[23,1] = 'QTY4'
laFileStru[23,2] = 'N'
laFileStru[23,3] = 5
laFileStru[23,4] = 0

laFileStru[24,1] = 'QTY5'
laFileStru[24,2] = 'N'
laFileStru[24,3] = 5
laFileStru[24,4] = 0

laFileStru[25,1] = 'QTY6'
laFileStru[25,2] = 'N'
laFileStru[25,3] = 5
laFileStru[25,4] = 0

laFileStru[26,1] = 'QTY7'
laFileStru[26,2] = 'N'
laFileStru[26,3] = 5
laFileStru[26,4] = 0

laFileStru[27,1] = 'QTY8'
laFileStru[27,2] = 'N'
laFileStru[27,3] = 5
laFileStru[27,4] = 0

laFileStru[28,1] = 'TOTQTY'
laFileStru[28,2] = 'N'
laFileStru[28,3] = 6
laFileStru[28,4] = 0

laFileStru[29,1] = 'PIK1'
laFileStru[29,2] = 'N'
laFileStru[29,3] = 5
laFileStru[29,4] = 0

laFileStru[30,1] = 'PIK2'
laFileStru[30,2] = 'N'
laFileStru[30,3] = 5
laFileStru[30,4] = 0

laFileStru[31,1] = 'PIK3'
laFileStru[31,2] = 'N'
laFileStru[31,3] = 5
laFileStru[31,4] = 0

laFileStru[32,1] = 'PIK4'
laFileStru[32,2] = 'N'
laFileStru[32,3] = 5
laFileStru[32,4] = 0

laFileStru[33,1] = 'PIK5'
laFileStru[33,2] = 'N'
laFileStru[33,3] = 5
laFileStru[33,4] = 0

laFileStru[34,1] = 'PIK6'
laFileStru[34,2] = 'N'
laFileStru[34,3] = 5
laFileStru[34,4] = 0

laFileStru[35,1] = 'PIK7'
laFileStru[35,2] = 'N'
laFileStru[35,3] = 5
laFileStru[35,4] = 0

laFileStru[36,1] = 'PIK8'
laFileStru[36,2] = 'N'
laFileStru[36,3] = 5
laFileStru[36,4] = 0

laFileStru[37,1] = 'mNotes'
laFileStru[37,2] = 'M'
laFileStru[37,3] = 10
laFileStru[37,4] = 0

laFileStru[38,1] = 'PikTkt'
laFileStru[38,2] = 'C'
laFileStru[38,3] = 6
laFileStru[38,4] = 0

laFileStru[39,1] = 'Order'
laFileStru[39,2] = 'C'
laFileStru[39,3] = 6
laFileStru[39,4] = 0

laFileStru[40,1] = 'PikDate'
laFileStru[40,2] = 'D'
laFileStru[40,3] = 8
laFileStru[40,4] = 0

laFileStru[41,1] = 'Store'
laFileStru[41,2] = 'C'
laFileStru[41,3] = 8
laFileStru[41,4] = 0

laFileStru[42,1] = 'Account'
laFileStru[42,2] = 'C'
laFileStru[42,3] = 5
laFileStru[42,4] = 0

laFileStru[43,1] = 'cCustPO'
laFileStru[43,2] = 'C'
laFileStru[43,3] = 15
laFileStru[43,4] = 0

laFileStru[44,1] = 'cWDesc'
laFileStru[44,2] = 'C'
laFileStru[44,3] = 30
laFileStru[44,4] = 0

laFileStru[45,1] = 'HLINE2'
laFileStru[45,2] = 'C'
laFileStru[45,3] = 30
laFileStru[45,4] = 0

laFileStru[46,1] = 'HLINE3'
laFileStru[46,2] = 'C'
laFileStru[46,3] = 30
laFileStru[46,4] = 0

laFileStru[47,1] = 'START'
laFileStru[47,2] = 'C'
laFileStru[47,3] = 5
laFileStru[47,4] = 0

laFileStru[48,1] = 'COMPLETE'
laFileStru[48,2] = 'C'
laFileStru[48,3] = 5
laFileStru[48,4] = 0

laFileStru[49,1] = 'APPROVAL'
laFileStru[49,2] = 'C'
laFileStru[49,3] = 10
laFileStru[49,4] = 0

laFileStru[50,1] = 'HLine4'
laFileStru[50,2] = 'C'
laFileStru[50,3] = 30
laFileStru[50,4] = 0

laFileStru[51,1] = 'PHONE'
laFileStru[51,2] = 'C'
laFileStru[51,3] = 25
laFileStru[51,4] = 0

laFileStru[52,1] = 'cBtName'
laFileStru[52,2] = 'C'
laFileStru[52,3] = 30
laFileStru[52,4] = 0

laFileStru[53,1] = 'cStName'
laFileStru[53,2] = 'C'
laFileStru[53,3] = 30
laFileStru[53,4] = 0

laFileStru[54,1] = 'cBtadd1'
laFileStru[54,2] = 'C'
laFileStru[54,3] = 30
laFileStru[54,4] = 0

laFileStru[55,1] = 'cStadd1'
laFileStru[55,2] = 'C'
laFileStru[55,3] = 30
laFileStru[55,4] = 0

laFileStru[56,1] = 'cBtadd2'
laFileStru[56,2] = 'C'
laFileStru[56,3] = 30
laFileStru[56,4] = 0

laFileStru[57,1] = 'cStadd2'
laFileStru[57,2] = 'C'
laFileStru[57,3] = 30
laFileStru[57,4] = 0

laFileStru[58,1] = 'cBtadd3'
laFileStru[58,2] = 'C'
laFileStru[58,3] = 30
laFileStru[58,4] = 0

laFileStru[59,1] = 'cStadd3'
laFileStru[59,2] = 'C'
laFileStru[59,3] = 30
laFileStru[59,4] = 0

laFileStru[60,1] = 'cSeason'
laFileStru[60,2] = 'C'
laFileStru[60,3] = 7
laFileStru[60,4] = 0

laFileStru[61,1] = 'cSpcInst'
laFileStru[61,2] = 'C'
laFileStru[61,3] = 15
laFileStru[61,4] = 0

laFileStru[62,1] = 'DEPT'
laFileStru[62,2] = 'C'
laFileStru[62,3] = 5
laFileStru[62,4] = 0

laFileStru[63,1] = 'HasNot'
laFileStru[63,2] = 'N'
laFileStru[63,3] = 1
laFileStru[63,4] = 0

=gfCrtTmp(lcDistFile,@laFileStru,'PIKTKT',lcDistFile,.F.)
SELECT (lcTmpOrdL)
LOCATE
DO WHILE !EOF() 
  =lfGetHData()
  SELECT (lcTmpOrdL)
  SCAN REST WHILE PikTkt+Order+cGrupDetal+STR(LineNo,6)=m.PikTkt+m.Order+'D'
    IF TotPik <= 0
      LOOP
    ENDIF
    =loOrdHdr.SEEK('O'+m.Order,'ORDHDR')
    m.phone =   TRANSFORM(&lcTempOrdHdr..phone,'@R '+lcPhnFrmt)
    m.Start    = SUBSTR(DTOC(&lcTempOrdHdr..START),1,5)
    m.Complete = SUBSTR(DTOC(&lcTempOrdHdr..COMPLETE),1,5)
    m.NOTE1 = &lcTempOrdHdr..NOTE1
    m.NOTE2 = &lcTempOrdHdr..NOTE2
    m.GROUP = GROUP
    m.Style = Style
    m.SDESC = &lcStyleFile..DESC
    m.DYELOT = DYELOT
    m.PRICE  = PRICE
    m.TOTPIK = TOTPIK 
    m.cLongClr = SUBSTR(gfCodDes(SUBSTR(STYLE,lnMajLen+2) , 'COLOR'),1,15)
    IF loScale.SEEK('S'+&lcStyleFile..SCALE,'SCALE') 
      m.SZ1 = SUBSTR(ALLTRIM(&lcScaleFile..SZ1),1,3)
      m.SZ2 = SUBSTR(ALLTRIM(&lcScaleFile..SZ2),1,3)
      m.SZ3 = SUBSTR(ALLTRIM(&lcScaleFile..SZ3),1,3)
      m.SZ4 = SUBSTR(ALLTRIM(&lcScaleFile..SZ4),1,3)
      m.SZ5 = SUBSTR(ALLTRIM(&lcScaleFile..SZ5),1,3)
      m.SZ6 = SUBSTR(ALLTRIM(&lcScaleFile..SZ6),1,3)
      m.SZ7 = SUBSTR(ALLTRIM(&lcScaleFile..SZ7),1,3)
      m.SZ8 = SUBSTR(ALLTRIM(&lcScaleFile..SZ8),1,3)            
    ENDIF
    m.QTY1 =  QTY1
    m.QTY2 =  QTY2
    m.QTY3 =  QTY3
    m.QTY4 =  QTY4
    m.QTY5 =  QTY5
    m.QTY6 =  QTY6
    m.QTY7 =  QTY7
    m.QTY8 =  QTY8
    m.TOTQTY = TOTQTY
    PRTPIK=.F.
    PRTPIK = IIF(PIK1 # QTY1, .T., PRTPIK)
    PRTPIK = IIF(PIK2 # QTY2, .T., PRTPIK)
    PRTPIK = IIF(PIK3 # QTY3, .T., PRTPIK)
    PRTPIK = IIF(PIK4 # QTY4, .T., PRTPIK)
    PRTPIK = IIF(PIK5 # QTY5, .T., PRTPIK)
    PRTPIK = IIF(PIK6 # QTY6, .T., PRTPIK)
    PRTPIK = IIF(PIK7 # QTY7, .T., PRTPIK)
    PRTPIK = IIF(PIK8 # QTY8, .T., PRTPIK)
    m.PIK1 = 0
    m.PIK2 = 0
    m.PIK3 = 0
    m.PIK4 = 0
    m.PIK5 = 0
    m.PIK6 = 0
    m.PIK7 = 0
    m.PIK8 = 0
    m.TOTPIK = TOTPIK 

*    IF PRTPIK
       m.PIK1 = PIK1
       m.PIK2 = PIK2
       m.PIK3 = PIK3
       m.PIK4 = PIK4
       m.PIK5 = PIK5
       m.PIK6 = PIK6
       m.PIK7 = PIK7
       m.PIK8 = PIK8
       m.TOTPIK = TOTPIK 
 *   ENDIF
    m.mNotes = ''
    m.HasNot = 0
    IF llRpOrdNot
      IF loNotePad.SEEK('B'+m.Order)
        SELECT NOTEPAD
        =loOrdHdr.SEEK('O'+m.Order,'ORDHDR')
        lnMemoWdth = SET('MEMOWIDTH')
        SET MEMOWIDTH TO 75
        m.mNotes = &lcTempNotePad..mNotes
        m.HasNot = IIF(!EMPTY(m.mNotes),1,0)
      ENDIF 
     ENDIF  
    SELECT (lcDistFile)    
    APPEND BLANK 
    GATHER MEMO MEMVAR 
  ENDSCAN 
  IF !EOF() AND cGrupDetal = 'H'
    SKIP
  ENDIF 
ENDDO   

SELECT(lcDistFile)
IF RECCOUNT(lcDistFile) = 0
  *-- Message : There are no records to display...!
  *--                < Ok > 
  =gfModalGen('TRM00052B40011','ALERT')
  RETURN
ENDIF

DIMENSION loOgScroll.laCRParams[5,2]
loOgScroll.laCRParams[1,1] = 'llRpOrdNot'
loOgScroll.laCRParams[1,2] = IIF(llRpOrdNot,1,0)

loOgScroll.laCRParams[2,1] = 'llRpStyPrc'
loOgScroll.laCRParams[2,2] = IIF(llRpStyPrc,1,0)

loOgScroll.laCRParams[3,1] = 'lnNonMajSt'
loOgScroll.laCRParams[3,2] = lnNonMajSt

loOgScroll.laCRParams[4,1] = 'llDyeLot'
loOgScroll.laCRParams[4,2] = IIF(llDyeLot,1,0)

loOgScroll.laCRParams[5,1] = 'lcClrLDsc'
loOgScroll.laCRParams[5,2] = IIF(lcClrLDsc = 'Y' ,1,0)

lcTempSub = loogscroll.gftempname()

DIMENSION LOogsCROLL.laCRTables[1]
loOgScroll.laCRTables[1] = oAriaApplication.WorkDir +  lcDistFile + ".DBF"

*-----------
SELECT(lcDistFile)
LOCAL lcReportFileName 
COPY TO  oAriaApplication.WorkDir +  lcTempSub + ".DBF"

lcReportFileName = loOgScroll.lcOGLastForm 

IF FILE(oAriaApplication.ReportHome  +'AL\' + lcReportFileName  + '.RPT')
  lcReportFileName = oAriaApplication.ReportHome +'AL\' +lcReportFileName + '.RPT'
ENDIF   

loMainCr = CREATEOBJECT('CrystalRuntime.Application') 
loMain = CREATEOBJECT('CrystalRuntime.Report') 
loMain = loMainCr.OpenReport(lcReportFileName)

loMain.Database.Tables.Item[1].Setlogoninfo ( oAriaApplication.WorkDir +  lcDistFile+ ".DBF")
loMain.Database.Tables.Item[1].SetTableLocation ( oAriaApplication.WorkDir +  lcDistFile+ ".DBF",'','')
loMain.DiscardSavedData()
loMain.ConvertDateTimeType = 1  && crConvertDateTimeToDate 
loMain.CaseInsensitiveSQLData = .T.
loSub1 = loMain.OpenSubreport ('bulk')

loSub1.Database.Tables.Item[1].Setlogoninfo ( oAriaApplication.WorkDir +  lcTempSub  + ".DBF")  
loSub1.Database.Tables.Item[1].SetTableLocation ( oAriaApplication.WorkDir +  lcTempSub + ".DBF",'','')
loSub1.Database.Verify() && verify database 
loSub1.DiscardSavedData()
loSub1.ConvertDateTimeType = 1  && crConvertDateTimeToDate 
loSub1.CaseInsensitiveSQLData = .T.
lcTempCrFile = loogscroll.gftempname()
loMain.Save (oAriaApplication.WorkDir+lcTempCrFile+'.rpt')
COPY FILE (oAriaApplication.WorkDir+lcTempCrFile+'.rpt') TO (lcReportFileName)
ERASE (oAriaApplication.WorkDir+lcTempCrFile+'.rpt')
loMainCr  = NULL
loMain    = NULL
loSub1    = NULL 

SELECT(lcDistFile)
USE 


*-----------
gfDispRe()
RETURN 

*------------------------------
* SECTION: MAIN LOOP
*------------------------------
DO WHILE !EOF() 
*  WAIT WINDOW 'PRINT PICK TICKETS - <Space Bar> TO ABORT' NOWAIT
  *----------------------------
  * GET ORDERHDR & LINE ITEMS
  *----------------------------
  *-- Function to get piktkt HEADER information.
  =lfGetHData()
  *--Print the piktkt HEADER information.
  =lfPriHdr()
  *------------------
  * LINE LOOP
  *------------------
  lnTOTQTY = 0
  lnPieces = 0

  SELECT (lcTmpOrdL)
  SCAN REST WHILE PikTkt+Order+cGrupDetal+STR(LineNo,6)=lcPikTkt+lcOrder+'D'
    DO CASE
      CASE TotPik <= 0
        LOOP
      CASE lnRow >=47
        =SEEK('O'+lcOrder,'ORDHDR')
        @ lnRow,30 SAY 'CONTINUED NEXT PAGE ...'
        @ 54+lnShift,01 SAY ORDHDR.NOTE1
        @ 54+lnShift,41 SAY ORDHDR.NOTE2
        @ 55+lnShift,00 SAY A
        @ 56+lnShift,00 SAY E
        @ 57+lnShift,00 SAY F
	    @ 57+lnShift,64 SAY SUBSTR(lcShipVia,1,10)
		@ 57+lnShift,78 SAY '|'
		@ 58+lnShift,00 SAY A
		@ 59+lnShift,00 SAY G
		@ 60+lnShift,00 SAY H
        @ 61+lnShift,05 SAY IIF(llRpStyPrc,'*******.**','')&&paramter
        @ 61+lnShift,52 SAY SUBSTR(lcTermData,1,10)
        @ 62+lnShift,00 SAY A
        @ 63+lnShift,00 SAY lcRpMsg1
        @ 64+lnShift,00 SAY lcRpMsg2
        @ 65+lnShift,00 SAY lcRpMsg3
        *--Print the piktkt HEADER information.
        =lfPriHdr()
    ENDCASE
    @ lnRow,03 SAY GROUP
    @ lnRow,05 SAY SUBSTR(STYLE, 1, 7 )
    @ lnRow,13 SAY SUBSTR(STYLE, lnNonMajSt, 3 )
    @ lnRow,17 SAY STYLE.DESC
    IF llDyeLot
       @ lnRow,46 SAY DYELOT
    ENDIF
    IF llRpStyPrc
       @ lnRow,60 SAY PRICE         PICTURE '9999.99'
       @ lnRow,69 SAY PRICE*TOTPIK  PICTURE '999,999.99'
    ENDIF
    lcLongClr = SUBSTR(gfCodDes(SUBSTR(STYLE,lnMajLen+2) , 'COLOR'),1,15)
    IF lcClrLDsc = 'Y' .AND. !EMPTY(lcLongClr)
      lnRow = lnRow + 1
      @ lnRow,13 SAY lcLongClr
    ENDIF
    lnRow = lnRow + 1
    Y   = '   '
    lcScl = SPACE(0)
    IF SEEK('S'+Style.SCALE,'SCALE') 
      lcScl=Y+PADL(SUBSTR(ALLTRIM(SCALE.SZ1),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SCALE.SZ2),1,3),3,' ')+Y+;
                          PADL(SUBSTR(ALLTRIM(SCALE.SZ3),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SCALE.SZ4),1,3),3,' ')+Y+;
                          PADL(SUBSTR(ALLTRIM(SCALE.SZ5),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SCALE.SZ6),1,3),3,' ')+Y+;
                          PADL(SUBSTR(ALLTRIM(SCALE.SZ7),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SCALE.SZ8),1,3),3,' ')   
    ENDIF
    @ lnRow,11 SAY lcScl
    lnRow = lnRow+2
    @ lnRow,05 SAY 'ORDR:'
    @ lnRow,12 SAY QTY1     PICTURE '99999'
    @ lnRow,18 SAY QTY2     PICTURE '99999'
    @ lnRow,24 SAY QTY3     PICTURE '99999'
    @ lnRow,30 SAY QTY4     PICTURE '99999'
    @ lnRow,36 SAY QTY5     PICTURE '99999'
    @ lnRow,42 SAY QTY6     PICTURE '99999'
    @ lnRow,48 SAY QTY7     PICTURE '99999'
    @ lnRow,54 SAY QTY8     PICTURE '99999'
    @ lnRow,61 SAY TOTQTY   PICTURE '999999'
    * CUM ORDER TOTALS
    lnValue   = lnValue + TOTPIK * PRICE
    PRTPIK=.F.
    PRTPIK = IIF(PIK1 # QTY1, .T., PRTPIK)
    PRTPIK = IIF(PIK2 # QTY2, .T., PRTPIK)
    PRTPIK = IIF(PIK3 # QTY3, .T., PRTPIK)
    PRTPIK = IIF(PIK4 # QTY4, .T., PRTPIK)
    PRTPIK = IIF(PIK5 # QTY5, .T., PRTPIK)
    PRTPIK = IIF(PIK6 # QTY6, .T., PRTPIK)
    PRTPIK = IIF(PIK7 # QTY7, .T., PRTPIK)
    PRTPIK = IIF(PIK8 # QTY8, .T., PRTPIK)
    IF PRTPIK
       lnRow = lnRow+1
       @ lnRow,05 SAY 'ALLO:'
       @ lnRow,12 SAY PIK1     PICTURE '99999'
       @ lnRow,18 SAY PIK2     PICTURE '99999'
       @ lnRow,24 SAY PIK3     PICTURE '99999'
       @ lnRow,30 SAY PIK4     PICTURE '99999'
       @ lnRow,36 SAY PIK5     PICTURE '99999'
       @ lnRow,42 SAY PIK6     PICTURE '99999'
       @ lnRow,48 SAY PIK7     PICTURE '99999'
       @ lnRow,54 SAY PIK8     PICTURE '99999'
       @ lnRow,61 SAY TOTPIK   PICTURE '999999'
    ENDIF
    lnPieces = lnPieces + TOTPIK
    lnRow =lnRow+1
    @ lnRow,05 SAY 'PICK:'
    @ lnRow,12 SAY '_____'
    @ lnRow,18 SAY '_____'
    @ lnRow,24 SAY '_____'
    @ lnRow,30 SAY '_____'
    @ lnRow,36 SAY '_____'
    @ lnRow,42 SAY '_____'
    @ lnRow,48 SAY '_____'
    @ lnRow,54 SAY '_____'
    @ lnRow,62 SAY '_____'
    lnRow=lnRow+2
    SELECT (lcSummary)
    =SEEK(&lcTmpOrdL..Style)
    IF EOF()
       APPEND BLANK
       REPLACE STYLE WITH &lcTmpOrdL..Style
    ENDIF
    REPLACE PIK1     WITH &lcTmpOrdL..PIK1 + PIK1,;
            PIK2     WITH &lcTmpOrdL..PIK2 + PIK2,;
            PIK3     WITH &lcTmpOrdL..PIK3 + PIK3,;
            PIK4     WITH &lcTmpOrdL..PIK4 + PIK4,;
            PIK5     WITH &lcTmpOrdL..PIK5 + PIK5,;
            PIK6     WITH &lcTmpOrdL..PIK6 + PIK6,;
            PIK7     WITH &lcTmpOrdL..PIK7 + PIK7,;
            PIK8     WITH &lcTmpOrdL..PIK8 + PIK8,;
            TOTPIK   WITH &lcTmpOrdL..TOTPIK + TOTPIK,;
            QTY1     WITH &lcTmpOrdL..QTY1 + QTY1,;
            QTY2     WITH &lcTmpOrdL..QTY2 + QTY2,;
            QTY3     WITH &lcTmpOrdL..QTY3 + QTY3,;
            QTY4     WITH &lcTmpOrdL..QTY4 + QTY4,;
            QTY5     WITH &lcTmpOrdL..QTY5 + QTY5,;
            QTY6     WITH &lcTmpOrdL..QTY6 + QTY6,;
            QTY7     WITH &lcTmpOrdL..QTY7 + QTY7,;
            QTY8     WITH &lcTmpOrdL..QTY8 + QTY8,;
            TOTQTY   WITH &lcTmpOrdL..TOTQTY + TOTQTY
    * GET NEXT LINE ITEM
    SELECT (lcTmpOrdL)
  ENDSCAN
  *------------------------ END PRINT LINE LOOP ----------------------
  IF lnPieces >0
    lnRow = lnRow + 1
    @ lnRow,10 SAY "TOTAL PIECES TO PICK ............"
    @ lnRow,61 SAY lnPieces      PICTURE '999999'
    lnPieces = 0
  ENDIF
  *------------------------
  * [NOTEPAD] COMMENT LINES
  *------------------------
  IF llRpOrdNot
    SELECT NOTEPAD
    IF SEEK('B'+lcOrder)
      =SEEK('O'+lcOrder,'ORDHDR')
      lnMemoWdth = SET('MEMOWIDTH')
      SET MEMOWIDTH TO 75
      lnMemoLnNo = 0      
      lnMemolins = 0
      lnRow = lnRow +1
      @ lnRow,02 SAY '*-- N O T E P A D --*'
      lnRow = lnRow +1
      lnMemolins = MEMLINES(mNotes)
      IF lnMemolins > 0
        DO WHILE lnMemoLnNo <> lnMemolins .AND. INKEY() <> 32
          IF lnRow > 51
            lnRow = lnRow + 1
            @ lnRow,2 SAY 'CONTINUED NEXT PAGE ...'
            @ 54+lnShift,01 SAY ORDHDR.NOTE1
            @ 54+lnShift,41 SAY ORDHDR.NOTE2
            @ 55+lnShift,00 SAY A
    		@ 56+lnShift,00 SAY E
	     	@ 57+lnShift,00 SAY F
		    @ 57+lnShift,64 SAY SUBSTR(lcShipVia,1,10)
		    @ 57+lnShift,78 SAY '|'
			@ 58+lnShift,00 SAY A
			@ 59+lnShift,00 SAY G
			@ 60+lnShift,00 SAY H
            @ 61+lnShift,05 SAY IIF(llRpStyPrc,'*******.**','')
            @ 61+lnShift,52 SAY SUBSTR(lcTermData,1,10)
            @ 62+lnShift,00 SAY A
            @ 63+lnShift,00 SAY lcRpMsg1
            @ 64+lnShift,00 SAY lcRpMsg2
            @ 65+lnShift,00 SAY lcRpMsg3
            *--Print the piktkt HEADER information.
            =lfPriHdr()
           ENDIF
           lnMemoLnNo = lnMemoLnNo + 1
           @ lnRow,02 SAY MLINE(NOTEPAD.mNotes,lnMemoLnNo)
           lnRow = lnRow + 1
         ENDDO
       ENDIF
       @ lnRow,02 SAY '*-- END OF NOTEPAD --*'
       lnMemoLnNo = 0      
       lnMemolins = 0
       SET MEMOWIDTH TO (lnMemoWdth)
     ENDIF
   ENDIF
   @ 54+lnShift,01 SAY lcNote1
   @ 54+lnShift,41 SAY lcNote2
   SELECT (lcTmpOrdL)
   @ 55+lnShift,00 SAY A
   @ 56+lnShift,00 SAY E
   @ 57+lnShift,00 SAY F
   @ 57+lnShift,64 SAY SUBSTR(lcShipVia,1,10)
   @ 57+lnShift,78 SAY '|'
   @ 58+lnShift,00 SAY A
   @ 59+lnShift,00 SAY G
   @ 60+lnShift,00 SAY H
   IF llRpStyPrc
     @ 61+lnShift,05 SAY lnValue     PICTURE '9999999.99'
     lnValue   = 0.00
   ENDIF
   @ 61+lnShift,52 SAY SUBSTR(lcTermData,1,10)
   @ 62+lnShift,00 SAY A
   @ 63+lnShift,00 SAY lcRpMsg1
   @ 64+lnShift,00 SAY lcRpMsg2
   @ 65+lnShift,00 SAY lcRpMsg3
   IF !EOF() AND cGrupDetal = 'H'
     SKIP
   ENDIF  
ENDDO
SELECT (lcSummary)
LOCATE
lnRow  = 99
PAGECT = 0
DO WHILE INKEY() <>32 .AND. !EOF()
  WAIT WINDOW 'PRINT BULK TICKETS - <Space Bar> TO ABORT' NOWAIT
   IF lnRow > 55
      PAGECT=PAGECT+1
      @ 01,00 SAY 'ALPKTKST'
      @ 01,30 SAY 'BULK PICKING TICKET'
      @ 01,65 SAY 'PAGE  '+STR(PAGECT,3)
      @ 02,00 SAY DATE()
      @ 02,65 SAY 'TIME  ' + XTIME
      @ 04,00 SAY '-------------------------------------------------------------------------------'
      @ 05,00 SAY '     STYLE   CLR     DESCRIPTION                                               '
      @ 06,00 SAY '-------------------------------------------------------------------------------'
      lnRow=8
   ENDIF
   @ lnRow,05 SAY SUBSTR(STYLE, 1, 7 )
   @ lnRow,13 SAY SUBSTR(STYLE, lnNonMajSt, 3 )
   @ lnRow,17 SAY IIF(SEEK(STYLE,'STYLE'),STYLE.DESC,'')
   lnRow = lnRow+1
   @ lnRow,05 SAY 'ORDR:'
   @ lnRow,12 SAY QTY1     PICTURE '99999'
   @ lnRow,18 SAY QTY2     PICTURE '99999'
   @ lnRow,24 SAY QTY3     PICTURE '99999'
   @ lnRow,30 SAY QTY4     PICTURE '99999'
   @ lnRow,36 SAY QTY5     PICTURE '99999'
   @ lnRow,42 SAY QTY6     PICTURE '99999'
   @ lnRow,48 SAY QTY7     PICTURE '99999'
   @ lnRow,54 SAY QTY8     PICTURE '99999'
   @ lnRow,61 SAY TOTQTY   PICTURE '999999'
   lnRow = lnRow+1
   @ lnRow,05 SAY 'ALLO:'
   @ lnRow,12 SAY PIK1     PICTURE '99999'
   @ lnRow,18 SAY PIK2     PICTURE '99999'
   @ lnRow,24 SAY PIK3     PICTURE '99999'
   @ lnRow,30 SAY PIK4     PICTURE '99999'
   @ lnRow,36 SAY PIK5     PICTURE '99999'
   @ lnRow,42 SAY PIK6     PICTURE '99999'
   @ lnRow,48 SAY PIK7     PICTURE '99999'
   @ lnRow,54 SAY PIK8     PICTURE '99999'
   @ lnRow,61 SAY TOTPIK   PICTURE '999999'
   lnRow = lnRow+1
   @ lnRow,00 SAY REPLICATE('-',79)
   lnRow=lnRow+2
   SKIP
ENDDO
SET DEVICE TO SCREEN
RETURN

*--------------------------------
*    END ALPKTKST.PRG
*--------------------------------


*!*************************************************************
*! Name      : lpOpenFiles
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 09/09/01
*! Purpose   : To Open files.
*!*************************************************************
PROCEDURE lpOpenFiles


IF TYPE('loCUSTOMER') <> 'O'
  loCUSTOMER = CreateObject("RemoteTable","CUSTOMER","CUSTOMER",'CUSTOMER',SET("DATASESSION")) 
ENDIF 

IF TYPE('loPIKTKT') <> 'O'
  loPIKTKT = CreateObject("RemoteTable","PIKTKT","PIKTKT",'PIKTKT',SET("DATASESSION")) 
ENDIF 

IF TYPE('loSCALE') <> 'O'
  loSCALE = CreateObject("RemoteTable","SCALE","SCALE",'SCALE',SET("DATASESSION")) 
ENDIF 

IF TYPE('loWareHous') <> 'O' AND llWareHous 
  loWareHous = CreateObject("RemoteTable","WareHous","WareHous",'WareHous',SET("DATASESSION")) 
ENDIF 




=IIF(llWareHous AND !USED('WareHous'),gfOpenFile(gcDataDir+'WareHous',gcDataDir+'WareHous','SH'),.T.)

*!*************************************************************
*! Name      : lfGetHData
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 09/09/01
*! Purpose   : TO get the information of the pick ticket's header.
*!*************************************************************
FUNCTION lfGetHData
PRIVATE lnAlias

lnAlias   = SELECT(0)
m.PikTkt  = PikTkt
m.Order   = Order
m.PikDate = PikDate
m.Store   = &lcPiktktTemp..Store
m.Account = &lcPiktktTemp..Account
m.cCustPO  = IIF(&lcOrdHdr..MULTIPO,&lcOrdLnTmp..CUSTPO,&lcOrdHdr..CUSTPO)
*--Get the warehouse address
m.cWDesc = SPACE(1)
IF llWareHous
  =loWareHous.SEEK(&lcPiktktTemp..cWareCode,'WareHous')
  m.cWDesc = 'WARHSE: '+ &lcTempWareHous..cWareCode+ ' '+SUBSTR(&lcTempWareHous..cDesc,1,18)
ELSE
  GO TOP IN WareHous
ENDIF
m.HLINE2  = &lcTempWareHous..cAddress1
m.HLINE3  = &lcTempWareHous..cAddress2
m.HLine4  = SUBSTR(&lcTempWareHous..cAddress3,1,15) + ' ' + SUBSTR(&lcTempWareHous..cAddress4,1,3)+ ' ' + SUBSTR(&lcTempWareHous..cAddress5,1,10)
m.HLINE5  = &lcTempWareHous..cPhone
IF EMPTY(m.HLINE3)
  m.HLINE3 = HLINE4
  m.HLINE4 = HLINE5
  m.HLINE5 = ''
ENDIF 
STORE 0 TO lnValue,lnPieces
STORE '' TO lcBtName,lcBtAdd1,lcBtAdd2,lcBtAdd3,lcBtAdd4,lcBtAdd5,lcBtAdd6,lcSpcInst,lcSeason
STORE '' TO lcStName,lcStAdd1,lcStAdd2,lcStAdd3,lcStAdd4,lcStAdd5,lcStAdd6,lcCustPO,lcStrln1
*--Get the Bill To adddess.
*--Get the proper record in the customer file.
=IIF(EMPTY(m.Store),loCustomer.SEEK('M'+m.Account,'Customer'),loCustomer.SEEK('S'+m.Account+m.Store,'Customer'))
STORE '' TO m.cBtName,m.cBtAdd1,m.cBtAdd2,m.cBtAdd3,m.cBtAdd4,m.cBtAdd5,m.cBtAdd6
m.cBtName  = &lcTempCustomer..BTNAME
=gfGetAdr(lcTempCustomer,'','','',1,'2')
FOR lnCount = 1 TO ALEN(laAddress,1)
  lcCount = STR(laAddress[lnCount,1],1)
  m.cBtAdd&lcCount = m.cBtAdd&lcCount + IIF(EMPTY(m.cBtAdd&lcCount),'',',')+;
  ALLTRIM(SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3]))
ENDFOR
IF EMPTY(m.cBtAdd2)
  m.cBtAdd2 = m.cBtAdd3
  m.cBtAdd3 = ''
ENDIF
*-- SHIP_TO ADDRESS FOR THIS STORE
STORE '' TO m.cStName,m.cStAdd1,m.cStAdd2,m.cStAdd3,m.cStAdd4,m.cStAdd5,m.cStAdd6
IF &lcOrdHdr..Alt_ShpTo
  m.cStName  = &lcOrdHdr..STNAME
  =gfGetAdr(lcOrdHdr,'','','',1,'')
  FOR lnCount = 1 TO ALEN(laAddress,1)
    lcCount = STR(laAddress[lnCount,1],1)
    m.cStAdd&lcCount = m.cStAdd&lcCount + IIF(EMPTY(m.cStAdd&lcCount),'',',')+;
    ALLTRIM(SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3]))
  ENDFOR  
ELSE
  m.cStName  = IIF(EMPTY(&lcTempCustomer..DBA),&lcTempCustomer..STNAME,&lcTempCustomer..DBA)
  =gfGetAdr(lcTempCustomer,'','','',1,'')
  *--Get the Ship To adddess except the country.    
  FOR lnCount = 1 TO ALEN(laAddress,1)
    lcCount = STR(laAddress[lnCount,1],1)
    m.cStAdd&lcCount = m.cStAdd&lcCount + IIF(EMPTY(m.cStAdd&lcCount),'',',')+;
    ALLTRIM(SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3]))
  ENDFOR  
ENDIF
IF EMPTY(m.cStAdd2)
  m.cStAdd2 = m.cStAdd3
  m.cStAdd3 = ''
ENDIF
*-- GET DESCRIPTIONS FOR CODED FIELDS
*--Terms
m.cTermData = gfCodDes(&lcOrdHdr..CTERMCODE , 'CTERMCODE')
*--ShipVia
m.cShipVia = gfCodDes(&lcOrdHdr..SHIPVIA , 'SHIPVIA')
*--Special instruction
m.cSpcInst = gfCodDes(&lcOrdHdr..SPCINST , 'SPCINST')
*---Season
m.cSeason = gfCodDes(&lcOrdHdr..SEASON , 'SEASON')


*:*************************************************************************
*: PROGRAM   : lfPriHdr
*: DESC.     : PRINT PIKTKT Header
*! Date      : 09/09/01
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*:*************************************************************************
FUNCTION lfPriHdr
PRIVATE lcAlias

lcAlias = ALIAS()
SELECT ORDHDR
* LINE 1
@ 00,01 SAY 'P I C K I N G   T I C K E T'
@ 00,35 SAY lcComName
@ 00,70 SAY DATE()
@ 01,01 SAY 'TICKET: '+lcPikTkt
@ 01,16 SAY ldDate
@ 01,35 SAY HLINE2
lcOrder=ORDER
@ 02,01 SAY 'ORDER : '+ORDER
@ 02,16 SAY SUBSTR(DTOC(START),1,5)
@ 02,24 SAY SUBSTR(DTOC(COMPLETE),1,5)
@ 02,35 SAY HLINE3
@ 02,70 SAY XTIME
@ 03,01 SAY IIF(!EMPTY(APPROVAL),'APPRVL: '+APPROVAL,'') 
@ 03,35 SAY HLINE4
IF llWareHous .AND. !EMPTY(lcWDesc)
  @ 04,01 SAY lcWDesc
ENDIF
@ 04,35 SAY HLINE5  PICTURE lcPhnFrmt SIZE 1,16
@ 06,04 SAY '.....SOLD TO .....'
@ 06,46 SAY '.....SHIP TO .....'
@ 07,04 SAY ACCOUNT
@ 07,10 SAY PHONE  PICTURE lcPhnFrmt SIZE 1,16
IF LEN(TRIM(lcStore))<>0 .AND. lcStore<>'*'
   @ 07,46 SAY 'STORE#: ' + lcStore
ENDIF
@ 08,04 SAY lcBtName
@ 08,46 SAY lcStName
@ 09,04 SAY lcBtadd1
@ 09,46 SAY lcStadd1
@ 10,04 SAY lcBtadd2
@ 10,46 SAY lcStadd2
@ 11,04 SAY lcBtadd3
@ 11,46 SAY lcStadd3
@ 13,00 SAY A
@ 14,00 SAY B
@ 15,00 SAY A
@ 16,01 SAY SUBSTR(lcShipVia,1,15)
@ 16,16 SAY '|'
@ 16,18 SAY SUBSTR(lcSeason,1,7) 
@ 16,25 SAY '|'
@ 16,26 SAY SUBSTR(lcSpcInst,1,15) 
@ 16,44 SAY '|'
@ 16,47 SAY &lcTmpOrdL..STORE
@ 16,55 SAY '|'
@ 16,58 SAY DEPT
@ 16,63 SAY '|'
@ 16,66 SAY SUBSTR(IIF(ORDHDR.MULTIPO,&lcTmpOrdL..CUSTPO,ORDHDR.CUSTPO),1,10)
@ 16,78 SAY '|'
@ 17,00 SAY A
@ 18,00 SAY C
@ 19,00 SAY A
lnRow=20
lcNote1 = ORDHDR.NOTE1
lcNote2 = ORDHDR.NOTE2

SELECT (lcAlias)