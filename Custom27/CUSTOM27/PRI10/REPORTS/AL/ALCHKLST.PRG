*:************************************************************************
*: Procedure file: ALCHKLST.PRG (Custom P\K Check List)
*:         System: ARIA 2.7
*:         Module: Allocation
*:         Author: Adel Mohammed El Gazzar (ADEL)
*:  Date         : 12/18/2000
*:  Procs & Fncts: lfwRepWhen
*:               : lfvOptMsg
*:               : lfwOldVal
*:               : lfvPikTkt
*:               : lfvcustom
*:               : lfvWareCod
*:               : lfGTmpOrdL
*:               : lfAFields
*:               : lfSolSpAdr
*:               : lfAdrShift
*:               : lfEndGroup
*: Refer to      : (C#101991)
*:***************************************************************************

PRIVATE lnCount 
lcSolTName = ''        && Variable to hold the Sold to name
lcShpTName = ''        && Variable to hold the Ship to name
llEndGroup = .F.       && Flag to know if we are at the end of the Group
*-- lnMajorLen : Style Major Length.
lnMajorLen = LEN(gfItemMask("PM"))  && Major Length
llLogo = IIF(SEEK('*' + 'LOGO' , 'OBJLINK') .AND. SEEK(OBJLINK.cObject_ID ,;
             'OBJECTS') , .T. , .F.)        && Flag to know if we are to print the Company Logo
llPrntComp = TYPE('llPrntComp') = 'L' .AND. llPrntComp 
DECLARE laSoldTo[5,1] , laShipTo[5,1] ,laShipped[8]
laSoldTo = ''           && Array to hold the Sold To address
laShipTo = ''           && Array to hold the Ship To address
lcScale =' '
laShipped = 0
=lfGTmpOrdL()
SELECT (lcTmpOrdL)
SET ORDER TO TAG (lcTmpOrdL)
SET ORDER TO TAG (lcTmpOrdH) IN &lcTmpOrdH
SET RELATION TO Order + PikTkt INTO &lcTmpOrdH
SET RELATION TO PikTkt INTO PIKTKT ADDITIVE
SET RELATION TO 'O' + Order INTO ORDHDR ADDITIVE
SET ORDER TO TAG Invlineo IN INVLINE
SET RELATION TO Order+STR(LineNo,6) INTO INVLINE ADDITIVE
*--IF We are to Print Order Lines Note Pad
*IF llRpOrdLNt
  SET RELATION TO 'O' + Order + STR(LineNo,6) INTO ORDLINE ADDITIVE
*ENDIF    && End of IF
SET RELATION TO Style INTO STYLE ADDITIVE
SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE
SELECT PIKTKT
SET RELATION TO cWareCode INTO WAREHOUS
SET RELATION TO IIF(EMPTY(Store) , 'M' + Account ,;
                    'S' + Account + Store) INTO CUSTOMER ADDITIVE
SELECT (lcTmpOrdL)
GO TOP
DO gfDispRe WITH 'ALCHKLST'
*-- HDM Added for Running @SAY[end]
SELECT PIKTKT
SET RELATION TO
SET DEVICE TO SCREEN
IF gcDevice <> 'SCREEN'
  SELECT (lcTmpOrdL)
  SET ORDER TO TAG (lcTmpOrdU)
  SCAN
    IF SEEK(EVAL(lcTmpOrdL+'.PIKTKT'),"PIKTKT")
      SELECT PIKTKT
      REPLACE PIKTKT.PrtFlag WITH 'P'
    ENDIF  
  ENDSCAN
ENDIF    && End of IF

*--IF The Temp. Order Line file is Opened in one of the work areas
IF USED(lcTmpOrdL)
  SELECT (lcTmpOrdL)
  SET RELATION TO
  USE
ENDIF    && End of IF

*--IF The Temp. Order Header file is Opened in one of the work areas
IF USED(lcTmpOrdH)
  SELECT (lcTmpOrdH)
  SET RELATION TO
  USE
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/18/2000
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : gfADel() , lfAFields() , gfGetMemVar() , lfOGShowGet()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfwRepWhen

PRIVATE lnWareHElm , lnVrFltElm , llRefresh
*--IF The multi Warehouse is No
IF !llMultWare
  lnWareHElm = ASCAN(laOGFieldN , 'PIKTKT.CWARECODE')      && Variable to hold the number of the array elment for the Warehouse in the Option grid Available for felter array
  *--IF The Warehouse is in the Available for felter fields
  IF lnWareHElm > 0
    =gfADel(@laOGFieldN , lnWareHElm , 1)
    =gfADel(@laOGFieldH , lnWareHElm , 1)
  ENDIF    && End of IF
  llRefresh = .F.          && Flag to know if we need to refresh the Option Grid or not
  DO WHILE .T.
    lnVrFltElm = ASCAN(laOGVrFlt , 'PIKTKT.CWARECODE')      && Variable to hold the number of the array elment for the Warehouse in the Option grid Variable felter array
    lnVrFltElm = IIF(lnVrFltElm = 0 , 0 , ASUBSCRIPT(laOGVrFlt , lnVrFltElm , 1))
  
    *--IF There is a row for the Warehouse in the Variable filter
    IF lnVrFltElm > 0
      =gfADel(@laOGVrFlt , lnVrFltElm , 1)
      lnOGVarFl = lnOGVarFl - 1        && Variable to hold the number of rows in the variable filter [Used by Option grid]
      llRefresh = .T.
    ELSE
      EXIT
    ENDIF    && End of IF
  ENDDO

  *--IF We are to refresh the option grid
  IF llRefresh
    CLEAR READ
    RETURN
  ENDIF    && End of IF
ENDIF    && End of IF
*--IF The Add notes to order lines seting is set to No
IF gfGetMemVar('M_OR_NOTE') <> 'Y'
  lnOrdLn = ASCAN(laOgObjType,"llRpOrdLNt")
  IF lnOrdLn > 0
    lnOrdLn = ASUBSCRIPT(laOgObjType,lnOrdLn,1)
    laOgObjCnt[lnOrdLn] = .F.
    llRpOrdLNt = .F.
    =lfOgShowGet("llRpOrdLNt")	  
  ENDIF
ENDIF    && End of IF

DIMENSION laTmpOrdLS[1,4]
=lfAFields('laTmpOrdLS' , 'ORDLINE' , 'LIKE QTY* , ORD* , PIK* , LINENO' +;
                          ' , SCALE , TOTQTY , TOTPIK , PRICE , GROUP' +;
                          ' , STYLE , DYELOT , CUSTPO , STORE')
DIMENSION laTmpOrdLS[ALEN(laTmpOrdLS , 1) + 1 , 4]
laTmpOrdLS[ALEN(laTmpOrdLS , 1) , 1] = 'cGrupDetal'
laTmpOrdLS[ALEN(laTmpOrdLS , 1) , 2] = 'C'
laTmpOrdLS[ALEN(laTmpOrdLS , 1) , 3] = 1
laTmpOrdLS[ALEN(laTmpOrdLS , 1) , 4] = 0

DIMENSION laTmpOrdHS[5,4]
=ACOPY(laTmpOrdLS , laTmpOrdHS , ASCAN(laTmpOrdLS , 'ORDER') , 4 , 1)
=ACOPY(laTmpOrdLS , laTmpOrdHS , ASCAN(laTmpOrdLS , 'PIKTKT') , 4 , 5)
laTmpOrdHS[3 , 1] = 'NoOfPages'
laTmpOrdHS[3 , 2] = 'N'
laTmpOrdHS[3 , 3] = 3
laTmpOrdHS[3 , 4] = 0
laTmpOrdHS[4 , 1] = 'STATUS'
laTmpOrdHS[4 , 2] = 'C'
laTmpOrdHS[4 , 3] = 1
laTmpOrdHS[4 , 4] = 0
laTmpOrdHS[5 , 1] = 'CUSTPO'
laTmpOrdHS[5 , 2] = 'C'
laTmpOrdHS[5 , 3] = 15
laTmpOrdHS[5 , 4] = 0
SET ORDER TO PIKTKT IN PIKTKT
SET ORDER TO CUSTOMER IN CUSTOMER
SET ORDER TO WAREHOUS IN WAREHOUS
SET ORDER TO PIKLINE IN PIKLINE
SET ORDER TO ORDHDR IN ORDHDR
SET ORDER TO ORDLINE IN ORDLINE
IF !USED('INVLINE')
  =gfOpenFile(gcDataDir+'INVLINE','Invlineo')
ENDIF

*!*************************************************************
*! Name      : lfvOptMsg
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/18/2000
*! Purpose   : Function to get Optional Message from the User
*!             [Validation function for the Push button Optional Message]
*!*************************************************************
*! Called from : Option Grid    [Optional Message option]
*!*************************************************************
*! Calls       : gfOptMsg()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvOptMsg

PRIVATE laOptMsg
DECLARE laOptMsg[3,2]       && Array to hold the name and length of the variables to be used in the Optional message screen

laOptMsg[1,1] = 'lcRpMsg1'        && 1st. line Variable
laOptMsg[2,1] = 'lcRpMsg2'        && 2nd. line Variable
laOptMsg[3,1] = 'lcRpMsg3'        && 3rd. line Variable
laOptMsg[1,2] = 75                && Line length

=gfOptMsg('laOptMsg')

*!*************************************************************
*! Name      : lfwOldVal
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/18/2000
*! Purpose   : When function to get the Old value
*!*************************************************************
*! Called from : Some of the Option Grid fields
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfwOldVal

laOldVal = EVALUATE(SYS(18))      && Varible to hold the old value

*!*************************************************************
*! Name      : lfvcustom
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/18/2000
*! Purpose   : Validation function for the Customer Account field
*!*************************************************************
*! Called from : Customer Account field [Option Grid]
*!*************************************************************
*! Calls       : CusBrowM()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvcustom

PRIVATE lcObjName , lcObjVal , llObjRet

lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field

*--IF The user want to Browse or if the Account he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK('M' + lcObjVal , 'CUSTOMER'))
  llObjRet = CusBrowM(@lcObjVal , '' , 'M')
  lcObjVal = IIF(llObjRet , lcObjVal , laOldVal)
  &lcObjName = lcObjVal
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfvWareCod
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/18/2000
*! Purpose   : Validation function for the Warehouse Code field
*!*************************************************************
*! Called from : Warehouse Code field [Option Grid]
*!*************************************************************
*! Calls       : gfBrowWare()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvWareCod

PRIVATE lcObjName , lcObjVal

lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field

*--IF The user want to Browse or if the Warehouse he entered is not in the
*file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'WAREHOUS'))
  lcObjVal = gfBrowWare(.T.)
  lcObjVal = IIF(EMPTY(lcObjVal) , laOldVal , lcObjVal)
  &lcObjName = lcObjVal
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfGTmpOrdL
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/18/2000
*! Purpose   : Function to colect the neede data in a temp. Order lines file
*!*************************************************************
*! Called from : ALPKTK.PRG
*!*************************************************************
*! Calls       : gfThermo()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
 *! Return      : None
*!*************************************************************
*
FUNCTION lfGTmpOrdL

PRIVATE lcForExp , lnTotRec , lnCurRec , lnRest , lnSavRec , lcOrder,;
 	    lcCDXName, lcFullPath
lcForExp = '!EOF("PIKTKT")' + IIF(EMPTY(lcRpExp) , '' ,;
           ' .AND. (' + lcRpExp + ')')        && Variable to hold the FOR Condition to SCAN for on the ORDLINE file

*--IF The Temp. Order Line file is Opened in one of the work areas
IF USED(lcTmpOrdL)
  USE IN (lcTmpOrdL)
ENDIF    && End of IF

CREATE CURSOR (lcTmpOrdL) FROM ARRAY laTmpOrdLS
ZAP
lcFullPath = SET("FULLPATH")
SET FULLPATH ON
lcCDXName  = STRTRAN(DBF() , ".TMP" , ".CDX")
SET FULLPATH &lcFullPath
IF gcDevice <> 'SCREEN'
  INDEX ON PikTkt TAG (lcTmpOrdU) UNIQUE
ENDIF  
INDEX ON PikTkt + Order + cGrupDetal + STR(LineNo , 6) ;
	 TAG (lcTmpOrdL) OF (lcCDXName)
*--IF The Temp. Order Header file is Opened in one of the work areas
IF USED(lcTmpOrdH)
  USE IN (lcTmpOrdH)
ENDIF    && End of IF

CREATE CURSOR (lcTmpOrdH) FROM ARRAY laTmpOrdHS
ZAP
lcFullPath = SET("FULLPATH")
SET FULLPATH ON
lcCDXName = STRTRAN(DBF() , ".TMP" , ".CDX")
SET FULLPATH &lcFullPath
INDEX ON Order + PikTkt ; 
	 TAG (lcTmpOrdH) OF (lcCDXName)
SELECT (lcTmpOrdH)
SET ORDER TO

SELECT PIKTKT
SET ORDER TO
SET RELATION TO 'O' + Order INTO ORDHDR

PRIVATE lcOldSelD
lcOldSelD = SET ("DEVICE")
SET DEVICE TO SCREEN
WAIT 'Selecting picking tickets...' WINDOW NOWAIT
SCAN FOR &lcRpExp
  INSERT INTO (lcTmpOrdH) (Order , PikTkt , Status) VALUES;
                          (PIKTKT.Order , PIKTKT.PikTkt , PIKTKT.Status)
ENDSCAN    && End of SCAN Loop
*--IF The number of records Processed is less than the Total count to be done

SET ORDER TO TAG PIKTKT
SET RELATION TO

SELECT ORDLINE
SET RELATION TO PikTkt INTO PIKTKT
SET RELATION TO Style INTO STYLE ADDITIVE

SELECT (lcTmpOrdH)
SET ORDER TO TAG (lcTmpOrdH)
lnTotRec = RECCOUNT()      && Varible to hold the Total count to be done [For the thermometer]
lnCurRec = 0               && Varible to hold the current count to be done [For the thermometer]
SCAN FOR Status <> 'X'
  lnCurRec = lnCurRec + 1
  lnSavRec = RECNO()
  *--IF There is one or more records for this Order in the ORDLINE file
  IF SEEK('O' + &lcTmpOrdH..Order , 'ORDLINE')
    M.cGrupDetal = 'D'
    lcOrder = Order
    SELECT ORDLINE
    SCAN REST;
        WHILE Order = lcOrder;
          FOR &lcForExp
    
      SELECT (lcTmpOrdH)
      SEEK ORDLINE.Order + ORDLINE.PikTkt
      REPLACE NoOfPages WITH NoOfPages +1

      IF(SEEK("O"+ORDLINE.ORDER,"ORDHDR") AND ORDHDR.Multipo)
        REPLACE Custpo WITH IIF(Custpo <> ORDLINE.Custpo,IIF(EMPTY(Custpo),ORDLINE.Custpo,;
        "Multi PO"),ORDLINE.Custpo)
      ELSE
        REPLACE Custpo WITH OrdHdr.Custpo
      ENDIF      
      SELECT ORDLINE
      SCATTER MEMVAR MEMO
      INSERT INTO (lcTmpOrdL)  FROM MEMVAR
    ENDSCAN    && End of SCAN Loop
    SCATTER MEMVAR MEMO BLANK
    SELECT (lcTmpOrdH)
    GO lnSavRec
    M.Order = Order
    *--IF We are to Print the Order Note Pad
    LOCATE REST FOR Order + PikTkt > M.Order
    SKIP -1
  ENDIF    && End of IF
  =gfThermo(lnTotRec , lnCurRec , "Selecting order lines for pick tickets. " ,;
                       'Pick ticket # :' + PikTkt)
ENDSCAN    && End of SCAN Loop

SELECT ORDLINE
SET RELATION TO

SELECT PIKLINE
SET RELATION TO Style INTO STYLE

SELECT (lcTmpOrdH)

SCAN FOR Status $ 'XC'
  lnCurRec = lnCurRec + 1
  *--IF There is one or more records for this Order in the PIKLINE file
  IF SEEK(&lcTmpOrdH..PikTkt + &lcTmpOrdH..Order , 'PIKLINE')
    M.cGrupDetal = 'D'
    SELECT PIKLINE

    *SCAN Loop to scan the PIKLINE file FOR The current Pick Ticket & Order
    SCAN REST;
        WHILE PikTkt + Order = &lcTmpOrdH..PikTkt + &lcTmpOrdH..Order
      SELECT (lcTmpOrdH)
      REPLACE NoOfPages WITH NoOfPages + 1
      SELECT PIKLINE
      SCATTER MEMVAR MEMO
      INSERT INTO (lcTmpOrdL)  FROM MEMVAR
    ENDSCAN    && End of SCAN Loop
    
    SCATTER MEMVAR MEMO BLANK
    SELECT (lcTmpOrdH)
  ENDIF    && End of IF
  =gfThermo(lnTotRec , lnCurRec , "Selecting order lines for pick tickets. " ,;
                       'Pick ticket # :' + PikTkt)
ENDSCAN    && End of SCAN Loop
*--Update PageNo
SELECT (lcTmpOrdH)
SCAN
  REPLACE NoOfPages WITH MAX(IIF(MOD(NoOfPages/6,1)<>0,INT(NoOfPages/6)+1,INT(NoOfPages/6)),1)
ENDSCAN

SELECT PIKLINE
SET RELATION TO

*--IF The number of records Processed is less than the Total count to be done
IF lnTotRec > lnCurRec .AND. lnCurRec <> 0
  
  *FOR Loop to loop for the remains of Total count to be done
  FOR lnRest = lnCurRec TO lnTotRec
    =gfThermo(lnTotRec , lnRest ,;
              "Selecting order lines for pick tickets. " , '')
  ENDFOR    && End of FOR Loop
ENDIF    && End of IF
SET DEVICE TO &lcOldSelD

*!*************************************************************
*! Name      : lfAFields
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/18/2000
*! Purpose   : Function to places information about the structure of the
*!             a selected table into an array for a selected fields
*!*************************************************************
*! Called from : lfwRepWhen()
*!*************************************************************
*! Calls       : gfTempName()
*!*************************************************************
*! Passed Parameters : 1) The Array name
*!                     2) The File name
*!                     3) The Fields to get information about 
*!                        [You can use keleton <skel>]
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfAFields

PARAMETERS lcArrayNam , lcFile , lcFields

PRIVATE lcOldAlias , lcTempName

*--IF The Array name Parameter is not of type character or empty
IF TYPE('lcArrayNam') <> 'C' .OR. EMPTY(lcArrayNam)
  RETURN
ENDIF    && End of IF

lcFile = IIF(TYPE('lcFile') <> 'C' , '' , lcFile)
lcFields = IIF(TYPE('lcFields') <> 'C' .OR. EMPTY(lcFields) , '' ,;
               'FIELDS ' + lcFields)

lcOldAlias = ALIAS()               && Variable to save the old Alis

*--IF The File name Parameter is not empty and this file is Opened in one
*--of the work areas
IF !EMPTY(lcFile) .AND. USED(lcFile)
  SELECT (lcFile)
ENDIF    && End of IF

*--IF The Fields Parameter is empty
IF EMPTY(lcFields)
  =AFIELDS(&lcArrayNam)
ELSE    && Else
  lcTempName = gfTempName()          && Variable to hold a temp name
  COPY STRUCTURE EXTENDED TO &gcWorkDir.&lcTempName &lcFields
  SELECT * FROM &gcWorkDir.&lcTempName INTO ARRAY &lcArrayNam
  *--IF The temp. file (lcTempName) is Opened in one of the work areas
  *[The file we used for COPY STRUCTURE EXTENDED]
  IF USED(lcTempName)
    USE IN &lcTempName
  ENDIF    && End of IF
  
  *--IF The temp. file (lcTempName) exist in the Temp. Dir.
  *[The file we used for COPY STRUCTURE EXTENDED]
  IF FILE(gcWorkDir + lcTempName + '.DBF')
    ERASE &gcWorkdir.&lcTempName..DBF
  ENDIF    && End of IF
ENDIF    && End of IF

*--IF The Variable we used to save the old Alias is not empty
IF !EMPTY(lcOldAlias)
  SELECT (lcOldAlias)
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfSolSpAdr
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/18/2000
*! Purpose   : Function to Get the Sold to Address & Ship to Address
*!             & the Description of the Ship Via , Season ,
*!             Special Instructions , Terms
*!*************************************************************
*! Called from : ALPKTKTA.FRX
*!*************************************************************
*! Calls       : gfRltFld() , gfCodDes() , gfGetAdr() , lfAdrShift()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*
FUNCTION lfSolSpAdr

PRIVATE lcDistCntr

SELECT SYCCOMP
SEEK gcAct_Comp
lcPhonPict = gfPhoneTem()          && Variable to hold the Company Phone Format

llEndGroup = .F.
SELECT CUSTOMER
SEEK IIF(EMPTY(PIKTKT.STORE),'M','S')+ PIKTKT.Account + PIKTKT.STORE 
SELECT CUSTOMER
SEEK IIF(EMPTY(PIKTKT.Store) , 'M' , 'S') + PIKTKT.Account + PIKTKT.Store
lcSolTName = BTName
lcShpTName = IIF(ORDHDR.Alt_ShpTo , ORDHDR.STName , IIF(EMPTY(DBA) , STName , DBA))
laSoldTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
laSoldTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2')
laSoldTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')
laSoldTo[4] = IIF(EMPTY(Phone1),'',gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2')+TRANSFORM(Customer.Phone1 , lcPhonPict))
laSoldTo[5] = IIF(EMPTY(Fax),'',TRANSFORM(Customer.Fax, lcPhonPict))
=lfAdrShift('laSoldTo')
*--IF ORDHDR.Alt_ShpTo = .T.
IF ORDHDR.Alt_ShpTo
  SELECT ORDHDR
  lcShpTName = STName
  laShipTo[1] = cAddress1
  laShipTo[2] = cAddress2
  laShipTo[3] = cAddress3
  laShipTo[4] = cAddress4
  laShipTo[5] = cAddress5
ELSE    && Else
  SELECT CUSTOMER
  lcDistCntr = CUSTOMER.Dist_Ctr
  *--IF there is a distribution center
  IF !EMPTY(lcDistCntr)
    SEEK 'S' + PIKTKT.Account + lcDistCntr
  ENDIF
  lcShpTName = IIF(EMPTY(DBA) , STName , DBA)
  laShipTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1)
  laShipTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2)
  laShipTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3)
  laShipTo[4] = IIF(EMPTY(Phone1),'',ALLTRIM(gfGetAdr('CUSTOMER' , '' , '' , '' , 5)) + TRANSFORM(Customer.Phone1 , lcPhonPict))
  laShipTo[5] = IIF(EMPTY(Fax),'',TRANSFORM(Customer.Fax, lcPhonPict))
ENDIF    && End of IF
=lfAdrShift('laShipTo')
SELECT (lcTmpOrdL)
RETURN ''

*!*************************************************************
*! Name      : lfAdrShift
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/18/2000
*! Purpose   : Function to Shift the Address array if there is any
*!             empty lines in the address
*!*************************************************************
*! Called from : ALPKTKT.PRG , lfSolSpAdr()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : Address Array name
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfAdrShift

PARAMETERS lcArrayNam

*--FOR Loop to loop the Address Array
FOR lnCount = 1 TO 5
  *--IF The current Array element is of type character and empty
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
    EMPTY(&lcArrayNam.[lnCount])
    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

*--FOR Loop to loop the Address Array
FOR lnCount = 1 TO 5
  
  *--IF The current Array element is not of type character
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

*!*************************************************************
*! Name      : lfEndGroup
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/18/2000
*! Purpose   : Function to Update the End of Group flag and to update
*!             the PrtFlag field in the PIKTKT file if the divice is not
*!             Screen
*!*************************************************************
*! Called from : ALPKTKTA.FRX
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*
FUNCTION lfEndGroup

llEndGroup = .T.

*--IF The Divice is not Screen
IF gcDevice <> 'SCREEN'
  REPLACE PIKTKT.PrtFlag WITH 'P'
ENDIF    && End of IF

RETURN ''

*!*************************************************************
*! Name      : lfPktktSet
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/18/2000
*! Purpose   : Function to set and release the relations needed
*!             for the Pick ticket # field [For the In range]
*!*************************************************************
*! Called from : Picking ticket field [Option Grid]
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 1) 'S' To set the relations
*!                     2) 'R' To release the relations
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfPktktSet

PARAMETERS lcParm
IF lcParm = 'S'
  SELECT PIKTKT
  SET RELATION TO 'O' + Order INTO ORDHDR
  SET RELATION TO IIF(EMPTY(Store) , 'M' + Account ,;
                      'S' + Account + Store) INTO CUSTOMER ADDITIVE
ELSE
  SELECT PIKTKT
  SET RELATION TO
ENDIF

FUNCTION lfGetShp

PRIVATE lnAlias
lnAlias = SELECT()
laShipped = 0
IF SEEK(ORDER+STR(LINENO,6),'INVLINE')
  lcKey = order+str(lineno,6)
  SELECT INVLINE
  SUM REST qty1,qty2,qty3,qty4,qty5,qty6,qty7,qty8 WHILE order+STR(lineno,6)+invoice =   lcKey TO ARRAY laShipped
ENDIF
SELECT (lnAlias)

FUNCTION lfGetNotes

PRIVATE lnAlias
lnAlias = SELECT()
SELECT ORDLINE
lnOldMemW = SET("MEMOWIDTH")
SET MEMOWIDTH TO 75
lnMemLins = MEMLINES(Note_mem)
lnMemoLnNo = 0      
lcNote = ''
DO WHILE lnMemoLnNo <= lnMemLins
  lnMemoLnNo = lnMemoLnNo+1
  lcNote = lcNote+ALLTRIM(SUBSTR(MLINE(Note_mem,lnMemoLnNo),1,75)) + ' '
  IF LEN(lcNote) > 75
    EXIT
  ENDIF
ENDDO  
SELECT (lnAlias)
RETURN SUBSTR(lcNote,1,75)