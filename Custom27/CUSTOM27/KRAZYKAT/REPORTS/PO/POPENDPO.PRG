*:***************************************************************************
*: Program file  : POPENDPO  102599
*: Program desc. : Pending Purchase orders
*: For Report    : POPENDPO.FRX
*: System        : Aria Advantage Series VER. 2.7
*: Module        : Purchase Order (PO)
*: Developer     : AHMED MAHER
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO POPENDPO
*:***************************************************************************
*: B606102,1 AMH Exporting to DBF file instead of XLS file.
*:***************************************************************************

*B606102,1 AMH Comment the next lines since we don't neet to export to an XLS file [Start]
*IF UPPER(RIGHT(lcRpFile,3)) # 'XLS'
*  =gfModalGen("TRM00000B00000","DIALOG",'','',"Invalid extension. The extension should be XLS.")
*  RETURN
*ENDIF
*IF !FILE(lcRpFile)
*  PRIVATE lnCrtFile
*  lnCrtFile = FCREATE(lcRpFile)
*  IF lnCrtFile = -1
*    =gfModalGen("TRM00000B00000","DIALOG",'','',"Invalid path or file name.")
*    RETURN
*  ELSE
*    =FCLOSE(lnCrtFile)
*  ENDIF
*ENDIF
IF !FILE(gcDataDir+'POPENDPO.DBF')
  =gfModalGen("TRM00000B00000","DIALOG",'','',"File "+gcDataDir+"POPENDPO.DBF does not exist.")
  RETURN
ELSE
  IF !gfOpenFile(gcDataDir+'POPENDPO','','EX')
    =gfModalGen("TRM00000B00000","DIALOG",'','',"File POPENDPO.DBF is in use by another user. Cannot proceed.")
    RETURN
  ENDIF
ENDIF
*B606102,1 AMH [End]

PRIVATE lcTmpCurSr,lcWhileExp,lnVendPos,lcVendor,lcStatus,lnI,lnGrdTot
PRIVATE ldFromDate,ldToDate,lnDatePos,lnSeprPos,lcIndexExp,lnOpenFile
lcTmpCurSr = gfTempName()
STORE SPACE(0) TO lcVendor,lcWhileExp,lcStatus,lcIndexExp
STORE {  /  /    } TO ldFromDate,ldToDate

CREATE CURSOR (lcTmpCurSr) (STATUS C(1),POLineCode C(1),GROUP C(6),CITY C(30),VEND C(11),PO C(6),MD C(6),;
                            CAT C(18),DIVISION C(6),PATTERN C(10),;
                            STYLE C(19),COLOR C(6),TotalQty N(7,0),EnterDate D,;
                            COMP_DATE D,AVAILABLE D,DESC C(20),CONTACT C(30),CSORT C(41))
INDEX ON CSORT TAG (lcTmpCurSr) OF (lcTmpCurSr)

lnVendPos = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'POSHDR.VENDOR'),1)
IF lnVendPos > 0
  lcVendor = laOGFxFlt[lnVendPos,6]
ENDIF
lnDatePos = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'POSHDR.COMPLETE'),1)
IF lnDatePos > 0 .AND. !EMPTY(laOGFxFlt[lnDatePos,6])
  lnSeprPos = AT('|',laOGFxFlt[lnDatePos,6])
  ldFromDate = CTOD(SUBSTR(laOGFxFlt[lnDatePos,6],1,lnSeprPos-1))
  ldToDate   = CTOD(SUBSTR(laOGFxFlt[lnDatePos,6],lnSeprPos+1))
ENDIF
IF !EMPTY(laRpTarSt)
  FOR lnI = 1 TO ALEN(laRpTarSt)
    DO CASE
      CASE laRpTarSt[lnI] = 'Open'
        lcStatus = lcStatus + 'O'
      CASE laRpTarSt[lnI] = 'Hold'
        lcStatus = lcStatus + 'H'
      CASE laRpTarSt[lnI] = 'Actual'
        lcStatus = lcStatus + 'A'
      CASE laRpTarSt[lnI] = 'Closed'
        lcStatus = lcStatus + 'S'
      CASE laRpTarSt[lnI] = 'Canceled'
        lcStatus = lcStatus + 'X'
      CASE laRpTarSt[lnI] = 'Complete'
        lcStatus = lcStatus + 'C'
    ENDCASE
  ENDFOR
ENDIF
IF llRpGroup .OR. lcRpSort = 'V'
  lcIndexExp = 'VEND'
ENDIF
DO CASE
  CASE lcRpSort = 'S'
    lcIndexExp = lcIndexExp + IIF(EMPTY(lcIndexExp),'','+') + 'STYLE'
  CASE lcRpSort = 'P'
    lcIndexExp = lcIndexExp + IIF(EMPTY(lcIndexExp),'','+') + 'PO'
  CASE lcRpSort = 'T'
    lcIndexExp = lcIndexExp + IIF(EMPTY(lcIndexExp),'','+') + 'CITY'
  CASE lcRpSort = 'C'
    lcIndexExp = lcIndexExp + IIF(EMPTY(lcIndexExp),'','+') + 'DTOS(COMP_DATE)'
  CASE lcRpSort = 'E'
    lcIndexExp = lcIndexExp + IIF(EMPTY(lcIndexExp),'','+') + 'DTOS(EnterDate)'
ENDCASE
SELECT POSHDR
IF !EMPTY(lcVendor)
  SET ORDER TO POSHDRV
  IF !SEEK(lcVendor+'P')
    =gfModalGen('TRM00052B00000','DIALOG' )
    SET ORDER TO POSHDR
    USE IN (lcTmpCurSr)
    
    *B606102,1 AMH Close the POPENDPO also [Start]
    USE IN POPENDPO
    *B606102,1 AMH [End]
    
    RETURN
  ENDIF
  lcWhileExp = 'VENDOR+CSTYTYPE+PO=PADR(lcVendor,8)+"P"'
ELSE
  IF !SEEK('P')
    =gfModalGen('TRM00052B00000','DIALOG' )
    USE IN (lcTmpCurSr)
    
    *B606102,1 AMH Close the POPENDPO also [Start]
    USE IN POPENDPO
    *B606102,1 AMH [End]
    
    RETURN
  ENDIF
  lcWhileExp = 'CSTYTYPE+PO="P"'
ENDIF
STORE 0 TO lnGrdTot

*B606102,1 AMH Zap the POPENDPO file before inserting new data [Start]
SELECT POPENDPO
ZAP
SELECT POSHDR
*B606102,1 AMH [End]

SCAN REST WHILE &lcWhileExp. FOR IIF(EMPTY(lcStatus),.T.,STATUS$lcStatus) .AND.;
                                 IIF(EMPTY(ldToDate),.T.,BETWEEN(COMPLETE,ldFromDate,ldToDate))
  IF SEEK(CSTYTYPE+PO,'POSLN')
    =SEEK(VENDOR,'APVENDOR')
    SELECT POSLN
    SCAN REST WHILE cStyType+Po+Style+STR(LineNo,6)+TranCd = 'P'+POSHDR.PO;
              FOR TRANCD=lcRpTranCd
      =SEEK(STYLE,'STYLE')
      
      SELECT (lcTmpCurSr)
      APPEND BLANK
      REPLACE STATUS     WITH POSHDR.STATUS,;
              CITY       WITH APVENDOR.CADDRESS3,;
              VEND       WITH POSHDR.VENDOR,;
              PO         WITH POSHDR.PO,;
              MD         WITH POSHDR.SHIPVIA,;
              CAT        WITH POSHDR.QUOTACAT,;
              GROUP      WITH STYLE.CSTYGROUP,;
              PATTERN    WITH STYLE.PATTERN,;
              DIVISION   WITH STYLE.CDIVISION,;
              STYLE      WITH STYLE.CSTYMAJOR
      REPLACE COLOR      WITH SUBSTR(POSLN.STYLE,lnColorStr,lnColorLen),;
              POLineCode WITH POSLN.TRANCD,;
              TotalQty   WITH POSLN.TOTQTY,;
              EnterDate  WITH POSHDR.ENTERED,;
              COMP_DATE  WITH POSHDR.COMPLETE,;
              AVAILABLE  WITH POSHDR.AVAILABLE,;
              DESC       WITH STYLE.DESC,;
              CONTACT    WITH POSHDR.CONTACT,;
              CSORT      WITH EVALUATE(lcIndexExp)
      lnGrdTot = lnGrdTot + TotalQty
      WAIT WINDOW 'PO # : '+PO+' Style : '+ALLTRIM(STYLE)+' Color : '+ALLTRIM(COLOR) NOWAIT
    ENDSCAN
  ENDIF
ENDSCAN
WAIT CLEAR

IF RECCOUNT(lcTmpCurSr) > 0
  SELECT (lcTmpCurSr)
  LOCATE
  IF llRpGroup
    SELECT CSORT,VEND,SUM(TotalQty) AS SUBTOT;
      FROM (lcTmpCurSr);
      GROUP BY VEND;
      INTO CURSOR SUBTOT
    SELECT SUBTOT
    LOCATE
    SCAN
      INSERT INTO (lcTmpCurSr) (CSORT,VEND,TotalQty);
                        VALUES (SUBTOT.CSORT,'Total',SUBTOT.SUBTOT)
    ENDSCAN
    USE IN SUBTOT
  ENDIF
  SELECT (lcTmpCurSr)
  APPEND BLANK
  REPLACE CSORT    WITH REPLICATE('Z',41),;
          VEND     WITH 'Grand Total',;
          TotalQty WITH lnGrdTot
  LOCATE
  
  *B606102,1 AMH Comment the next lines since we don't neet to exporting XLS file [Start]
  *IF FILE(lcRpFile)
  *  lnOverWrt = gfModalGen("QRM00000B00006","DIALOG",'','',"File is already exist. OverWrite it?")
  *  IF lnOverWrt = 1
  *    lnOpenFile = FOPEN(lcRpFile,2)
  *    IF lnOpenFile = -1
  *      =gfModalGen("TRM00000B00000","DIALOG",'','',"File is in use. Cannot proceed.")
  *      USE IN (lcTmpCurSr)
  *      RETURN
  *    ELSE
  *      =FCLOSE(lnOpenFile)
  *    ENDIF
  *  ELSE
  *    USE IN (lcTmpCurSr)
  *    RETURN
  *  ENDIF
  *ENDIF
  *EXPORT TO (lcRpFile) FIELDS EXCEPT CSORT XLS
  *=gfModalGen("INM00000B00000","DIALOG",'','',"Exporting to XLS file has been successfully done.")
  SCAN
    SCATTER MEMO MEMVAR
    m.cGroup = m.Group
    m.cCity_Id = m.City
    INSERT INTO POPENDPO FROM MEMVAR
  ENDSCAN
  =gfModalGen("INM00000B00000","DIALOG",'','',"Exporting to POPENDPO file has been successfully done.")
  *B606102,1 AMH [End]
  
ELSE
  =gfModalGen('TRM00052B00000','DIALOG' )
ENDIF
USE IN (lcTmpCurSr)

*B606102,1 AMH Close the POPENDPO also [Start]
USE IN POPENDPO
*B606102,1 AMH [End]

*!*************************************************************
*! Name      : lfwOldVal
*! Developer : AHMED MAHER
*! Date      : 04/21/2002
*! Purpose   : To get the old value of the field
*!*************************************************************
*! Called from : POSTYPO.PRG
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwOldVal()
*!*************************************************************
FUNCTION lfwOldVal

laOldVal = EVALUATE(SYS(18))

*!*************************************************************
*! Name      : lfvVend
*! Developer : AHMED MAHER
*! Date      : 04/21/2002
*! Purpose   : Valid function of the Vendor field.
*!*************************************************************
*! Called from : POSTYPO.PRG
*!*************************************************************
*! Calls       : gfApVnBrow().
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvVend()
*!*************************************************************
FUNCTION lfvVend

PRIVATE lcVar, lcObj
lcVar = SYS(18)             && Varible to hold  the name of the memory variable used to create the current GET control
lcObj = ALLTRIM(  ( EVALUATE(SYS(18)) )  )   && Varible to hold the current field value
SELECT APVENDOR
SET ORDER TO TAG VenCode 
IF !EMPTY(lcObj) .AND. ('?' $ lcObj .OR. !SEEK(lcObj , 'APVENDOR'))
  =gfApVnBrow(@lcObj)
  IF !EMPTY(lcObj)
    &lcVar = lcObj      && Update the field
  ELSE
    &lcVar = laOldVal
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : AHMED MAHER
*! Date      : 04/21/2002
*! Purpose   : The when function of the option grid
*!*************************************************************
*! Called from : Option grid (Variable lcRpForm)
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : .....
*!*************************************************************
*! Return      : Logical 
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen

DECLARE laRpSorSt[6] , laRpTarSt[1]
STORE 'Open'     TO laRpSorSt[1]
STORE 'Hold'     TO laRpSorSt[2]
STORE 'Actual'   TO laRpSorSt[3]
STORE 'Closed'   TO laRpSorSt[4]
STORE 'Canceled' TO laRpSorSt[5]
STORE 'Complete' TO laRpSorSt[6]
STORE 'Open'     TO laRpTarSt[1]

SHOW GET pbOGpreview DISABLE
=lfGetColor()

*!*************************************************************
*! Name      : lfvOStatus
*! Developer : AHMED MAHER
*! Date      : 04/21/2002
*! Purpose   : Valid function of the Status.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : None.
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example   : = lfvOStatus()
*!*************************************************************
*
FUNCTION lfvOStatus

= gfMover(@laRpSorSt,@laRpTarSt,'Select Status',.T.,'')  && call mover function.

*!*************************************************************
*! Name      : lfGetColor
*! Developer : Ahmed Maher
*! Date      : 04/22/2002
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

*-- Array hold the segmants specifications of the style code structure.
DECLARE laStySeg[1,1]

*-- Count of the major part.
lnMjorCnt  = gfItemMask("SM")
  
*-- Fill an array with the segments strucure, & loop in it to 
*-- know if there a color segment in the style code strucure.
=gfItemMask(@laStySeg)
FOR lnCnt = lnMjorCnt + 1 TO ALEN(laStySeg,1)
  IF laStySeg[lnCnt , 1] = "C"
    *-- Flag to know if there is color in the style code strucure.
    llColorExt = .T.
    *-- Var. hold the start position of the color segment in the style code strucure.
    lnColorStr = laStySeg[lnCnt , 4]
    *-- Var. hold the color segment lenght in the style code strucure.
    lnColorLen = LEN(laStySeg[lnCnt , 3])
  ENDIF
ENDFOR

*!*************************************************************
*! Name      : lfvFile
*! Developer : AHMED MAHER
*! Date      : 04/23/2002
*! Purpose   : Valid function of export to file.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : None.
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example   : = lfvFile()
*!*************************************************************
*
FUNCTION lfvFile

PRIVATE lcFileName
lcFileName = PUTFILE('Export to file',lcRpFile,'XLS')
IF !EMPTY(lcFileName)
  lcRpFile = lcFileName
ENDIF