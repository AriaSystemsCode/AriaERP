*!****************************************************************************
*! Program file        : ALPKTKFL.PRG
*! Program desc.       : Order confirmation Form For Flowers By Zoe [FLO09]
*! For Report          : ALPKTKFL.FRX
*! System              : Aria Advantage Series 4XP.
*! Module              : Sales Oredr Allocation - (AL)
*! Developer           : Walid Hamed (WLD)
*! Date                : 07/16/2007
*! Reference           : C200818
*!****************************************************************************
*! Calls :  Functions  : lfCrtTemp - lfGetData - lfGetSblCO - lfvGtSclSp - lfFacCode
*!                     : lfAdrShift - lfGetSizes - lfTotPages - lfSolAdr
*!****************************************************************************
*! Called From         : System Menu (AL --> Output --> Picking Ticket)
*!****************************************************************************
*! Passed Parameters   : None
*!****************************************************************************
*! Example             : DO ALPKTKFL
*!****************************************************************************
*! Modification        :
*!****************************************************************************

PRIVATE lnOldAls 
 STORE 0 TO lnOldAls
lnOldAls = SELECT(0)
IF llOGFltCh
  *-- Define Arrays to Hold the codes data for SABLON Code ,SABLON Color , Trim Color
  PRIVATE lcSblName , lcSblColor , lcTrmColor
  DECLARE laCodeInfo[3,10] , laSblName[1,2] , laSblColor[1,2] , laTrmColor[1,2] , laFacCode[6,1]
  STORE '' TO laCodeInfo , laSblName , laSblColor , laTrmColor , laFacCode
  STORE '' TO lcSblName , lcSblColor , lcTrmColor

  STORE .F. TO llMScale

  lnMjrLen   = LEN(gfItemMask("PM"))             && Style major length.
  llMScale   = gfGetMemVar('M_USEEXSSC')
  lnSepMins  = lfvGtSclSp()                      && the length of Scale with Separator.
  lnColorLen = LEN(gfItemMask("PN"))-lnSepMins   && Style Color length.
  = lfCrtTemp()
  = lfGetData()

  =gfOpentable(oAriaApplication.SysPath+'SYCFACT',oAriaApplication.SysPath+'CFACCODE','SH')

  SELECT (lcTmpOrdL)
  USE
  IF !USED(lcTmpCode)
    =gfOpentable(oAriaApplication.WorkDir+lcTmpQty,oAriaApplication.WorkDir+lcTmpQty,'SH', @lcTmpOrdL, .T.)
  ELSE
    SELECT (lcTmpCode)
    USE
    =gfOpentable(oAriaApplication.WorkDir+lcTmpCode,oAriaApplication.WorkDir+lcTmpCode,'SH', @lcTmpOrdL, .T.)
  ENDIF
  SELECT (lcTmpOrdL)
  SET RELATION TO ORDER + PikTkt INTO &lcTmpOrdH
  SET RELATION TO PikTkt INTO PIKTKT ADDITIVE
  SET RELATION TO 'O' + ORDER INTO ORDHDR ADDITIVE
  SET RELATION TO 'O' + ORDER INTO &lcOrdHdr ADDITIVE
  SET RELATION TO STYLE INTO STYLE ADDITIVE
  SELECT PIKTKT
  SET RELATION TO cWareCode INTO WAREHOUS
  SET RELATION TO IIF(EMPTY(STORE) , 'M' + Account ,;
    'S' + Account + STORE) INTO CUSTOMER ADDITIVE

ENDIF
loOGScroll.cCROrientation='L'
loOGScroll.cCRPapersize = 'Legal'

SELECT (lcTmpOrdL)
GO TOP
SELECT (lnOldAls)


*-- END OF REPORT PROG.
*!*************************************************************
*! Name      : lfCrtTemp
*! Developer : Walid Hamed (WLD)
*! Date      : 07/16/2007
*! Purpose   : Create Temp File to collect the SoCodes Data
*!*************************************************************
*! Called from : ALPKTKFL.PRG
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfCrtTemp()
*!*************************************************************
FUNCTION lfCrtTemp
  DIMENSION laFileStru[58,4]
  laFileStru[ 1 , 1 ] = 'PIKTKT'
  laFileStru[ 1 , 2 ] = 'C'
  laFileStru[ 1 , 3 ] = 6
  laFileStru[ 1 , 4 ] = 0

  laFileStru[ 2 , 1 ] = 'ORDER'
  laFileStru[ 2 , 2 ] = 'C'
  laFileStru[ 2 , 3 ] = 6
  laFileStru[ 2 , 4 ] = 0

  laFileStru[ 3 , 1 ] = 'Account'
  laFileStru[ 3 , 2 ] = 'C'
  laFileStru[ 3 , 3 ] = 5
  laFileStru[ 3 , 4 ] = 0

  laFileStru[ 4 , 1 ] = 'Store'
  laFileStru[ 4 , 2 ] = 'C'
  laFileStru[ 4 , 3 ] = 8
  laFileStru[ 4 , 4 ] = 0

  laFileStru[ 5 , 1 ] = 'Note_Mem'
  laFileStru[ 5 , 2 ] = 'M'
  laFileStru[ 5 , 3 ] = 10
  laFileStru[ 5 , 4 ] = 0

  laFileStru[ 6 , 1 ] = 'LINENO'
  laFileStru[ 6 , 2 ] = 'N'
  laFileStru[ 6 , 3 ] = 6
  laFileStru[ 6 , 4 ] = 0

  laFileStru[ 7 , 1 ] = 'STYLE'
  laFileStru[ 7 , 2 ] = 'C'
  laFileStru[ 7 , 3 ] = lnMjrLen
  laFileStru[ 7 , 4 ] = 0

  laFileStru[ 8 , 1 ] = 'COLOR'
  laFileStru[ 8 , 2 ] = 'C'
  laFileStru[ 8 , 3 ] = lnColorLen
  laFileStru[ 8 , 4 ] = 0

  laFileStru[ 9 , 1 ] = 'CSBLNAME'
  laFileStru[ 9 , 2 ] = 'C'
  laFileStru[ 9 , 3 ] = 30
  laFileStru[ 9 , 4 ] = 0

  laFileStru[ 10, 1 ] = 'CSBLCOLOR'
  laFileStru[ 10, 2 ] = 'C'
  laFileStru[ 10, 3 ] = 30
  laFileStru[ 10, 4 ] = 0

  laFileStru[ 11, 1 ] = 'CTRMCOLOR'
  laFileStru[ 11, 2 ] = 'C'
  laFileStru[ 11, 3 ] = 30
  laFileStru[ 11, 4 ] = 0

  FOR I=1 TO 20
    laFileStru[ I+11 , 1 ] = 'SIZ' + STR(I,IIF(I<10,1,2))
    laFileStru[ I+11 , 2 ] = 'C'
    laFileStru[ I+11 , 3 ] = 5
    laFileStru[ I+11 , 4 ] = 0
  ENDFOR
  FOR I=1 TO 20
    laFileStru[ I+31 , 1 ] = 'QTY' + STR(I,IIF(I<10,1,2))
    laFileStru[ I+31 , 2 ] = 'N'
    laFileStru[ I+31 , 3 ] = 6
    laFileStru[ I+31 , 4 ] = 0
  ENDFOR

  laFileStru[ 52 , 1 ] = 'TOTPIK'
  laFileStru[ 52 , 2 ] = 'N'
  laFileStru[ 52 , 3 ] = 8
  laFileStru[ 52 , 4 ] = 0

  FOR I=1 TO 4
    laFileStru[ I+52 , 1 ] = 'PRICE' + STR(I,1)
    laFileStru[ I+52 , 2 ] = 'N'
    laFileStru[ I+52 , 3 ] = 7
    laFileStru[ I+52 , 4 ] = 2
  ENDFOR

  laFileStru[ 57 , 1 ] = 'TOTQAMNT'
  laFileStru[ 57 , 2 ] = 'N'
  laFileStru[ 57 , 3 ] = 10
  laFileStru[ 57 , 4 ] = 2

  laFileStru[ 58 , 1 ] = 'CGroup'
  laFileStru[ 58 , 2 ] = 'C'
  laFileStru[ 58 , 3 ] = 3
  laFileStru[ 58 , 4 ] = 0


  CREATE TABLE (oAriaApplication.WorkDir+lcTmpCode) FROM ARRAY laFileStru
  INDEX ON PikTkt TAG (lcTmpOrdU) UNIQUE
  INDEX ON PIKTKT+STYLE+COLOR+STR(LINENO,6)+ALLTRIM(CSBLNAME) + ;
    ALLTRIM(CSBLCOLOR) + ALLTRIM(CTRMCOLOR) TAG (lcTmpCode)
  SET ORDER TO TAG (lcTmpCode)

  *-- END OF FUNCTION lfCrtTemp.
  *!*************************************************************
  *! Name      : lfGetData
  *! Developer : Walid Hamed (WLD)
  *! Date      : 07/16/2007
  *! Purpose   : Function To Collect data from the Socodes File
  *!             and save it in lcTmpCode with codes's Description
  *!*************************************************************
  *! Called from : ALPKTKFL.PRG
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *! Example     : = lfGetData()
  *!*************************************************************
FUNCTION lfGetData

  PRIVATE lnCurTQty , lnLineNo , lcPrice , lcGrup , lcScale , lcOrder , llFillGrup , lcPikTkt
  STORE 0 TO lnLineNo , lnCurTQty
  STORE '' TO lcPrice , lcGrup , lcScale , lcOrder , lcPikTkt
  STORE .F. TO llFillGrup
  *-- [lcGrup] I store 'A' as for every new Piktkt that to not Print the sizes group after I Already
  *- printed the title Header , then I go down with next Chrs. if the scale changed inside
  *- the same Piktkt that to Reprint Sizes Codes only Every Time Scale changing
  *-- [lcPrice] I'm using this variable to get No. of the price that by getting the scale Sequence
  *- No. from the laSize array (row 2)
  *-- [llFillGrup] I used this variable only to hold .T. if the codition of the new Piktkt achieved
  *- because I get a new lcGrup that will be 'A' in this case but if the codition of a new scale
  *- achieved too with a new Piktkt i'm going to get a new lcGrup that will be 'B' and this in wrong

  =gfOpenTable('SOCODES','SOCODES','SH','SOCODES')
  SELECT (lcTmpOrdL)
  SET RELATION TO 'S'+SCALE INTO SCALE ADDITIVE

  SELECT (lcTmpOrdL)
  SCAN FOR CgrupDetal='D'
    IF !(lcOrder == &lcTmpOrdL..ORDER) .OR. !(lcPikTkt == &lcTmpOrdL..PikTkt)
      lcOrder = &lcTmpOrdL..ORDER
      lcPikTkt = &lcTmpOrdL..PikTkt
      IF !(EMPTY(lcGrup) .AND. llFillGrup)
        lcGrup     = ''
        lcGrup     = CHR(65)
        llFillGrup = .T.
      ENDIF
    ENDIF
    IF lcScale <> SUBSTR(SCALE,1,1)
      IF !EMPTY(lcScale)
        lnLineNo = lnLineNo + 1
      ENDIF
      lcScale = SUBSTR(SCALE,1,1)
      IF !llFillGrup
        lcGrup  = IIF(EMPTY(lcGrup) , CHR(65) , CHR(65 +(ASC(lcGrup)-65) + 1))
      ENDIF
      =lfGetSizes()
    ENDIF
    llFillGrup = .F.
    SCATTER MEMVAR MEMO
    =lfGetSblCO()

    IF gfSEEK(Piktkt + SUBSTR(STYLE,1,lnMjrLen) + SUBSTR(STYLE,lnMjrLen+2,lnColorLen)+ ;
        STR(lnLineNo,6) + lcSblName + lcSblColor + lcTrmColor,lcTmpCode)
      lnCurTQty  = m.PIK1+m.PIK2+m.PIK3+m.PIK4+m.PIK5
      SELECT (lcTmpCode)
      FOR N = 1 TO IIF(SCALE.CNT > 5 , 5 , SCALE.CNT)
        lcI      = STR(N,1)
        lnSizeNo = ASCAN(laSize , SCALE.SZ&lcI,1)
        lcI2     = STR(lnSizeNo , IIF(lnSizeNo<10,1,2))
        lcPrice  = IIF(laSize[2,lnSizeNo]>4,'',STR(laSize[2,lnSizeNo],1))
        REPLACE QTY&lcI2         WITH QTY&lcI2  + m.PIK&lCI   ,;
          TotPik           WITH TotPik  + m.Pik&lCI
        IF !EMPTY(lcPrice)
          REPLACE PRICE&lcPrice  WITH m.PRICE
        ENDIF
      ENDFOR
      REPLACE TOTQAMNT         WITH (TOTQAMNT+(PRICE&lcPrice * lnCurTQty))
      lnCurTQty  = 0
    ELSE
      SELECT (lcTmpCode)
      lnLineNo = lnLineNo + 1
      APPEND BLANK
      REPLACE ORDER              WITH m.Order                                 ,;
        Piktkt             WITH m.Piktkt                                ,;
        Account            WITH m.Account                               ,;
        cGroup             WITH lcGrup                                  ,;
        Note_Mem           WITH m.Note_Mem                              ,;
        LINENO             WITH lnLineNo                                ,;
        STYLE              WITH SUBSTR(m.Style,1,lnMjrLen)              ,;
        COLOR              WITH SUBSTR(m.Style,lnMjrLen+2,lnColorLen)   ,;
        CSblName           WITH lcSblName                               ,;
        CSblColor          WITH lcSblColor                              ,;
        CTrmColor          WITH lcTrmColor                              ,;
        TOTPIK             WITH m.PIK1+m.PIK2+m.PIK3+m.PIK4+m.PIK5
      FOR N = 1 TO IIF(SCALE.CNT > 5 , 5 , SCALE.CNT)
        lcI      = STR(N,1)
        lnSizeNo = ASCAN(laSize , SCALE.SZ&lcI,1)
        lcI2     = STR(lnSizeNo , IIF(lnSizeNo<10,1,2))
        lcPrice  = IIF(laSize[2,lnSizeNo]>4,'',STR(laSize[2,lnSizeNo],1))
        REPLACE QTY&lcI2  WITH m.PIK&lCI
        IF !EMPTY(lcPrice)
          REPLACE PRICE&lcPrice  WITH m.PRICE
        ENDIF
      ENDFOR
      REPLACE TOTQAMNT WITH TOTPIK*PRICE&lcPrice

      FOR N = 1 TO 20
        lcI = STR(N,IIF(N<10,1,2))
        REPLACE SIZ&lcI WITH laSize[1,N]
      ENDFOR
    ENDIF
  ENDSCAN

  *-- END OF FUNCTION lfGetData.
  *!*************************************************************
  *! Name      : lfGetSblCO
  *! Developer : Walid Hamed (WLD)
  *! Date      : 07/16/2007
  *! Purpose   : Function To Collect data from the Socodes File
  *!             and save it in lcTmpCode with codes's Description
  *!*************************************************************
  *! Called from : ALPKTKFL.PRG
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *! Example     : = lfGetSblCO()
  *!*************************************************************
FUNCTION lfGetSblCO

  *-- Get Sablon Codes in the temp file
  IF gfSEEK('O'+ ORDER + STR(LINENO,6),'SOCODES')
    lcSblName  = ALLTRIM(gfCodDes(SOCODES.CSblName  , 'CSBLNAME' ))
    lcSblColor = ALLTRIM(gfCodDes(SOCODES.CSblColor , 'CSBLCOLOR'))
    lcTrmColor = ALLTRIM(gfCodDes(SOCODES.CTrmColor , 'CTRMCOLOR'))
  ELSE
    STORE '' TO lcSblName,lcSblColor,lcTrmColor
  ENDIF

  *-- END OF FUNCTION lfGetSblCO.
  *!*************************************************************
  *! Name      : lfvGtSclSp
  *! Developer : Walid Hamed (WLD)
  *! Date      : 07/16/2007
  *! Purpose   : Function returns the length of Scale with Separator.
  *!*************************************************************
  *! Returns   : len of Scale 3 or 4 if separator exist.
  *!*************************************************************
  *! Example   : = lfvGtSclSp()
  *!*************************************************************
FUNCTION lfvGtSclSp
  PRIVATE lnAlias
  lnAlias = SELECT(0)
  IF llMScale
    =gfOpenTable('ICISTRU','Segno','SH','ICISTRU')
    SELECT ICISTRU
    SCAN
      IF cItemRecty='U' AND cISegType='S'
        SKIP -1
        lnSepMins = IIF(!lSegEndMaj AND !EMPTY(cISegSepr),4,3)
        EXIT
      ELSE
        lnSepMins = 3
      ENDIF
    ENDSCAN
  ELSE
    lnSepMins = 0
  ENDIF
  SELECT(lnAlias)
  RETURN (lnSepMins)

  *--End of lfvGtSclSp.
  *!*************************************************************
  *! Name      : lfFacCode
  *! Developer : Walid Hamed (WLD)
  *! Date      : 07/16/2007
  *! Purpose   : Get the Factor Addresses If there is a factor for
  *!           : the order in the Ordhdr file
  *!*************************************************************
  *! Called from : ALPKTKFL.Frx
  *!*************************************************************
  *! Passed Para.: None
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *! Example     : = lfFacCode()
  *!*************************************************************
FUNCTION lfFacCode
  PARAMETER lcReturn
  PRIVATE lnAlias
  lnAlias = SELECT(0)
  SELECT SYCFACT
  IF !EMPTY(ORDHDR.CFACCODE) .AND. gfSEEK(ORDHDR.CFACCODE,'SYCFACT')
    laFacCode[1] = gfGetAdr('SYCFACT' , '' , '' , '' , 1)
    laFacCode[2] = gfGetAdr('SYCFACT' , '' , '' , '' , 2)
    laFacCode[3] = gfGetAdr('SYCFACT' , '' , '' , '' , 3)
    laFacCode[4] = gfGetAdr('SYCFACT' , '' , '' , '' , 4)
    laFacCode[5] = gfGetAdr('SYCFACT' , '' , '' , '' , 5)
    *--search for the Phone No. in the SycFact File and show it with the Phone Format
    laFacCode[6] = 'Tel: '+ TRANSFORM(LOOKUP(sycFact.CPhoneNo,OrdHdr.CfacCode,;
      SycFact.cFacCode,'cFacCode'), lcPhonPict)
    = lfAdrShift('laFacCode')    && Shift Company address if there is empty line.
  ENDIF
  SELECT(lnAlias)
  RETURN
  *--End of lfFacCode.

  *!*************************************************************
  *! Name      : lfAdrShift
  *! Developer : Walid Hamed (WLD)
  *! Date      : 07/16/2007
  *! Purpose   : Function to Shift the Address array if there is any
  *!             empty lines in the address
  *!*************************************************************
  *! Called from : SOORDCNFL.PRG
  *!*************************************************************
  *! Passed Parameters : Address Array name
  *!*************************************************************
  *! Return      : None
  *!*************************************************************
  *! Example     : = lfAdrShift()
  *!*************************************************************
FUNCTION lfAdrShift
  PARAMETERS lcArrayNam
  *! For Statment to loop on the Address Array
  FOR lnCount = 1 TO 6
    *! IF The current Array element is of type character and empty
    IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
        EMPTY(&lcArrayNam.[lnCount])
      =ADEL(&lcArrayNam , lnCount)
      lnCount = lnCount - 1
    ENDIF    && End of IF
  ENDFOR    && End of FOR Loop
  *! For Statment to loop the Address Array
  FOR lnCount = 1 TO 6
    *! IF The current Array element is not of type character
    IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
      &lcArrayNam.[lnCount] = ''
    ENDIF    && End of IF
  ENDFOR    && End of FOR Loop
  *-- End of Function lfAdrShift.
  *!***************************************************************************
  *! Name      : lfGetSizes
  *! Developer : Walid Hamed (WLD)
  *! Date      : 07/16/2007
  *! Purpose   : to fill the lasize array with sizes of the scale file
  *!***************************************************************************
  *! Called from : Report
  *!***************************************************************************
  *! Example     : = lfGetSizes()
  *!***************************************************************************
FUNCTION lfGetSizes

  PRIVATE lnOldAls , lnRecNo , lnSizNo , lnScaleNo
  DECLARE laSize[2,20]
  STORE '' TO laSize
  STORE 0 TO lnOldAls , lnRecNo , lnSizNo , lnScaleNo
  lnOldAls = SELECT(0)
  lnRecNo  = RECNO('SCALE')
  SELECT SCALE
  SCAN FOR SUBSTR(SCALE,1) = lcScale
    lnScaleNo = lnScaleNo + 1
    FOR I=1 TO IIF(SCALE.CNT > 5 , 5 , SCALE.CNT)
      lcI = STR(I,1)
      lnSizNo = lnSizNo + 1
      laSize[1,lnSizNo] = SZ&lcI
      laSize[2,lnSizNo] = lnScaleNo
      IF EOF() .OR. lnSizNo = 20
        EXIT
      ENDIF
    ENDFOR
  ENDSCAN
  IF BETWEEN (lnRecNo,1,RECCOUNT())
    GOTO lnRecNo
  ENDIF
  SELECT(lnOldAls)

  *-- End of lfGetSizes.
  *!***************************************************************************
  *! Name      : lfTotPages
  *! Developer : Walid Hamed (WLD)
  *! Date      : 07/16/2007
  *! Purpose   : to get the total number of pages
  *!***************************************************************************
  *! Called from : Report Code
  *!***************************************************************************
  *! Example     : = lfTotPages()
  *!***************************************************************************
  *: Notes : if a change is done in Frx Remember to
  *:         1) Print the Report and count the number of lines per page
  *:         2) Adjust the number of lines (Hardcoded) in lfTotPages()

FUNCTION lfTotPages
  PARAMETER lcReturn
  PRIVATE lnRecNo,lnRecCount , lnRowNo , lcGroup , lcPikTkt , lnOldAls
  STORE '' TO lcGroup , lcPikTkt
  STORE 0  TO lnRecNo,lnRecCount , lnRowNo , lnOldAls
  lnOldAls = SELECT(0)
  SELECT (lcTmpOrdL)
  lnRecNo = RECNO()
  lnRowNo    = 22
  *LOCATE FOR &lcTmpOrdL..Piktkt = Piktkt.Piktkt
  lcGroup = cGroup
  lcPikTkt = PikTkt
  LOCATE
  SCAN FOR &lcTmpOrdL..Piktkt = lcPikTkt
    lnRecCount = lnRecCount + 1
    IF !(lcGroup==cGroup)
      lcGroup=cGroup
      lnRecCount = lnRecCount + 1
    ENDIF
    IF !(lcPikTkt == PikTkt)
      lcPikTkt = PikTkt
      lnRecCount = lnRecCount + lnRowNo
    ENDIF
  ENDSCAN
  lnPages = CEILING(lnRecCount/lnRowNo)
  IF BETWEEN (lnRecNo,1,RECCOUNT())
    GOTO lnRecNo
  ENDIF
  SELECT(lnOldAls)
  RETURN ''
  *-- End of lfTotPages.

*!*************************************************************
*! Name      : lfSolAdr
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 02/16/2005 
*! Purpose   : Function to Get the Sold to Address & Ship to Address
*!             & the Description of the Ship Via , Season ,
*!             Special Instructions , Terms
*!*************************************************************
*! Called from : ALPKTKTFL.FRX (Inside the FRX)
*!*************************************************************
*
FUNCTION lfSolAdr
PRIVATE lcDistCntr


llEndGroup = .F.
=gfRltFld(EVALUATE(lcORDHDR+'.cDivision') , @laDivLName , 'CDIVISION')

SELECT(lcCUSTOMER)
SEEK IIF(EMPTY(Piktkt.STORE),'M','S')+ Piktkt.Account + Piktkt.STORE
lcShipVia = gfCodDes(IIF(CUSTOMER.nBrkWeight <> 0 .AND.;
                         ORDHDR.nWeight > CUSTOMER.nBrkWeight ,;
                         CUSTOMER.cAltShpvia ,IIF(&lcORDHDR..ShipVia ='*',CUSTOMER.ShipVia,&lcORDHDR..ShipVia)), 'SHIPVIA')

lcSeason = gfCodDes(&lcORDHDR..Season , 'SEASON')
lcSpcInst = gfCodDes(&lcORDHDR..SpcInst , 'SPCINST')
lcTerms = gfCodDes(&lcORDHDR..cTermCode , 'CTERMCODE')

SELECT CUSTOMER
SEEK IIF(EMPTY(Piktkt.Store) , 'M' , 'S') + Piktkt.Account + Piktkt.Store

lcSolTName = BTName
lcShpTName = IIF(&lcORDHDR..Alt_ShpTo , &lcORDHDR..STName , IIF(EMPTY(DBA) , STName , DBA))

DIMENSION laSoldTo[6,1]
laSoldTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
laSoldTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2')
laSoldTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')
laSoldTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')
laSoldTo[5] = ALLTRIM(gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2'))+ ' Phone# '+ TRANSFORM(Customer.Phone1 , '@R '+lcPhonPict)
laSoldTo[6] =   ''

=lfAdrShift('laSoldTo')


DIMENSION laShipTo[6,1]

IF &lcORDHDR..Alt_ShpTo

  SELECT (lcORDHDR)
  lcShpTName = STName
  laShipTo[1] = cAddress1
  laShipTo[2] = cAddress2
  laShipTo[3] = cAddress3
  laShipTo[4] = cAddress4
  laShipTo[5] = cAddress5
  laShipTo[6] = ''

ELSE    && Else
  SELECT CUSTOMER
  lcDistCntr = CUSTOMER.Dist_Ctr
  *--If there is a distribution center
  *N000592,1 HBG 02/27/2007 Print Store Address or DC Address depnding on the Flag of Dircet To Store in ORDHDR [Begin]
  *IF !EMPTY(lcDistCntr)
  IF !EMPTY(lcDistCntr) AND !(&lcORDHDR..lStrDirct)
  *N000592,1 HBG [End]
    SEEK 'S' + Piktkt.Account + lcDistCntr
  ENDIF

  lcShpTName = IIF(EMPTY(DBA) , STName , DBA)
  laShipTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1)
  laShipTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2)
  laShipTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3)
  laShipTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4)
  laShipTo[5] = ALLTRIM(gfGetAdr('CUSTOMER' , '' , '' , '' , 5)) + ' Phone#' + TRANSFORM(Customer.Phone1 ,'@R '+lcPhonPict)
ENDIF    && End of IF

=lfAdrShift('laShipTo')

SELECT (lcTmpOrdL)
RETURN ''
*-- end of lfSolSpAdr.

