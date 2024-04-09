*:****************************************************************
*: Program file  : MAOPMGRE.PRG
*: Program desc. : Custom Programe# 126215 of Material Operations Management Report for Restix.
*: System        : Aria Apparel System (A27).
*: Module        : Material (MA)
*: Developer     : Ehab Ismail Hamed - (EIH) Due to C#000400,1
*: Date          : 03/30/2005
*:****************************************************************
*: Calls 
*:               : FUNCTIONS  : lfCreatemp , lfShiftArr , lfGetTit
*:               :            : lfAddField , lfRpHeader , lfAddFltr  
*:               :            : lfCollect  , lfForACC   , lfVendor
*:               :            : lfCustPO   , lfSetVenRp
*:               :            :  
*:               -----------------------------------------------
*:               : PROCEDURE  : lfShiftArr 
*:****************************************************************
*: Passed Parameters  : None.
*:****************************************************************
*:*:****************************************************************
*:Modifications  :
*:****************************************************************

*-- Begin Declaration variables.
*-- llLogo     ==> Variable hold true in case we found a loge for the company.
*-- lcTime     ==> Variable Hold the time.
*-- laCompAdd  ==> Array hold company address.
*-- lcTime     ==> Variable to hold the Time.
*-- lcCompName ==> Variable hold the company name.
*-- lcCompPhon ==> Variable hold the company Phone.
*-- lcPhonPict ==> Variable hold the company Phone format.
*-- llIsAparel ==> Variable o know if this program At say or not.
*-- lcPrgName  ==> Variable Hold the form name
*-- lcForAcc   ==> Variable to hold the "For Account".
*-- lcCustomer ==> Variable to hold the "Customer".
*-- lcCustPO   ==> Variable to hold the "Customer PO#".
*-- lcShipVia  ==> Variable to hold the "Ship Via".
*-- laVendor   ==> Array hold "Vendor" data.
*-- laShipTo   ==> Array hold "Ship To" data.
*-- lcWareCode ==> Variable to  hold "warehouse code" data.
*-- llMultiLoc ==> Variable to  hold "MULTI LOCATION OR NOT" data.    
DIMENSION  laCompAdd[6] , lashpadr[6] 
STORE '' TO lcCompName , lcCompPhon , lcPhonPict  ,laCompAdd , lcBOMTit,;
            lcShpname  , lashpadr , lcObj_Id 

DIMENSION laVendor[8], laShipTo[7]
STORE '' TO lcForAcc , lcCustomer ,lcCustPO  ,lcShipVia  , lcItemType, lmOrdMemo , lcOperName ,lcWareCode 

STORE .F. TO llLogo , llMultiLoc 
STORE ''  TO lcVenFlt

lcTime     = TIME()
lcPrgName  = lcFormName

*-- This Function to check if the fxp For the at say fom on the root o 
*-- Under the module directory.
llIsAparel = lfIsApparl(@lcPrgName)
*-- End Declaration variables.

*-- Function to get the header variables and fields.

= lfRpHeader ()
llOGFltCh = .T.
*-- if user change filter criteria then you must collect data again [Begin]
IF llOGFltCh
  *-- Create Temp Files.
  = lfCreatemp ()
  *--  Add Some filter to filter Exp. 
  = lfAddFltr ()
  *-- Collecting Code...
  = lfCollect ()
ENDIF
*-- End if for user change filter .
SELECT  (lcMainF)
LOCATE
IF !EOF()
  DO gfDispRe WITH EVAL('lcFormName')
ELSE
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'There no records to display')
ENDIF      
SET RELATION TO 
RETURN
*-- End of Code.

*:*************************************************************
*: Name      : lfGetTit
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 06/26/2002
*: Purpose   : function to get the option titel in Setup file.
*:*************************************************************
*: Called from : The Option Grid
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None.
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfGetTit()
*:*************************************************************
FUNCTION lfGetTit
lcTitle = ALLTRIM(gfGetMemvar('M_MMOLNLBL',gcAct_Comp))
lcTitle = IIF(RIGHT(lcTitle,1) ='#', lcTitle,lcTitle+'#')
RETURN lcTitle
*-- End OF lfGetTit
*:*************************************************************
*: Name      : lfShiftArr
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 06/26/2002
*: Purpose   : arrange the elements of the array
*:*************************************************************
*: Called from : MFOPMG.PRG
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None.
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfShiftArr()
*:*************************************************************
*:
PROCEDURE lfShiftArr

PARAMETERS laArray
PRIVATE lnAlen,lnCount, lnC
*-- Get length of the array.
lnALen = ALEN(laArray,1)
*-- Check each element of the array if it is empty
FOR lnCount = 1 TO lnALen
  IF EMPTY(laArray[lnCount])
    * If any element is empty shift down the later elements
    FOR lnC = lnCount TO lnALen-1
      laArray[lnC]=laArray[lnC+1]
    ENDFOR
    laArray[lnAlen]=''
  ENDIF
ENDFOR
*!*************************************************************
*! Name      : lfAddField
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 03/23/2005
*! Purpose   : Add fields to the array of file structure.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Passed Parameters : lcFldName -- Field Name
*!                   : lcFldType -- Field Type (C;N;L....M)
*!                   : lnFldLen  -- Field Length
*!                   : lnFldDec  -- Field Decimal
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfAddField()
*!*************************************************************
FUNCTION lfAddField
PARAMETERS lcStruArry , lcFldName , lcFldType , lnFldLen , lnFldDec
lnFldPos  = ALEN(&lcStruArry,1) + IIF(TYPE('&lcStruArry') = 'L', 0 , 1 )
DIMENSION &lcStruArry[lnFldPos , 4]
&lcStruArry[lnFldPos , 1]	= lcFldName
&lcStruArry[lnFldPos , 2]	= lcFldType
&lcStruArry[lnFldPos , 3]	= lnFldLen
&lcStruArry[lnFldPos , 4]	= lnFldDec

*-------End of lfAddField.

*:****************************************************************
*: Name      : lfCreatemp
*: Developer : Ehab Ismail. [EIH]
*: Date      : 03/30/2005
*: Purpose   : Create the temp files.
*:****************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:****************************************************************
*: Called from : Program.
*:****************************************************************
*: Passed Parameters  : ...
*:****************************************************************
*: Returns            : None.
*:****************************************************************
*: Example   : = lfCreatemp()
*:****************************************************************
*
FUNCTION lfCreatemp
PRIVATE lnAlias
lnAlias = SELECT(0)
DIMENSION laFileStru[1,4]
*--- These fields To collecting data from ctktbom.dbf
=lfAddField("laFileStru"  ,   "ITEM"          , "C"   ,19   ,0)
=lfAddField("laFileStru"  ,   "ICLR"          , "C"   ,6    ,0)
=lfAddField("laFileStru"  ,   "UOM"           , "C"   ,3    ,0)
=lfAddField("laFileStru"  ,   "UNTQTY"        , "N"   ,7    ,3)
=lfAddField("laFileStru"  ,   "PIECES"        , "N"   ,8    ,0)
=lfAddField("laFileStru"  ,   "REQ_QTY"       , "N"   ,12   ,3)
=lfAddField("laFileStru"  ,   "ISSUE_QTY"     , "N"   ,12   ,3)
=lfAddField("laFileStru"  ,   "USED_QTY"      , "N"   ,12   ,3)
=lfAddField("laFileStru"  ,   "VENDCODE"      , "C"   ,8    ,0)
=lfAddField("laFileStru"  ,   "VENDCODE2"      , "C"   ,8    ,0)
=lfAddField("laFileStru"  ,   "ITEM_TYPE"     , "C"   ,15   ,0)
*--- These fields To collecting data from mmfgordd.dbf
=lfAddField("laFileStru"  ,   "CFABRIC"       , "C"   ,7    ,0)
=lfAddField("laFileStru"  ,   "COLOR"         , "C"   ,6    ,0)
=lfAddField("laFileStru"  ,   "NMFGTOTQTY"    , "N"   ,11   ,3)
=lfAddField("laFileStru"  ,   "CLOTNO"        , "C"   ,2    ,0)
=lfAddField("laFileStru"  ,   "DESC"          , "C"   ,20   ,0)
=lfAddField("laFileStru"  ,   "WIDTH"         , "C"   ,6    ,0)
=lfAddField("laFileStru"  ,   "CFABWEIGHT"    , "C"   ,15   ,0)
=lfAddField("laFileStru"  ,   "UOMBUY"        , "C"   ,3    ,0)
=lfAddField("laFileStru"  ,   "UOMUSE"        , "C"   ,3    ,0)
=lfAddField("laFileStru"  ,   "CONTENT"       , "C"   ,60   ,0)
=lfAddField("laFileStru"  ,   "MORDMEMO"      , "M"   ,10   ,0)
=lfAddField("laFileStru"  ,   "CMFGORDNO"     , "C"   ,6    ,0)
*--- These fields to grouping data for displaying correctly  grouping
=lfAddField("laFileStru"  ,   "CUSTPO"         , "C"   ,15  ,0)
=lfAddField("laFileStru"  ,   "TABNAME"        , "C"   ,4   ,0)
=lfAddField("laFileStru"  ,   "BOMFirst"       , "L"   ,1   ,0)
=lfAddField("laFileStru"  ,   "COPERCODE"      , "C"   ,6   ,0)
=lfAddField("laFileStru"  ,   "COPERNAME"      , "C"   ,30  ,0)
=lfAddField("laFileStru"  ,   "CWARCODE"       , "C"   ,6   ,0)
=lfAddField("laFileStru"  ,   "LMULTILOC"      , "L"   ,1   ,0)
=lfAddField("laFileStru"  ,   "LDELETE"        , "L"   ,1   ,0) && TO DELETE THIS RECORD IF THER IS MULTI LOCATION AFTER COLLECTING LOCATIONS DATA
=lfAddField("laFileStru"  ,   "CTRANSNO"       , "C"   ,2   ,0)

*--- This fields To Print notebad to the report in text formate
=lfAddField("laFileStru"  ,   "LLLAST"       , "L"   ,0    ,0)
*--- These fields To collecting data from for headr of report
=lfAddField("laFileStru"  ,   "CFORACC"        , "C"   ,30   ,0)
=lfAddField("laFileStru"  ,   "CCUSTOMER"      , "C"   ,30   ,0)
=lfAddField("laFileStru"  ,   "CSHIPVIA"       , "C"   ,30   ,0)
=lfAddField("laFileStru"  ,   "CVENNAME"       , "C"   ,30   ,0)
=lfAddField("laFileStru"  ,   "CVENADR1"       , "C"   ,30   ,0)
=lfAddField("laFileStru"  ,   "CVENADR2"       , "C"   ,30   ,0)
=lfAddField("laFileStru"  ,   "CVENADR3"       , "C"   ,30   ,0)
=lfAddField("laFileStru"  ,   "CVENADR4"       , "C"   ,30   ,0)
=lfAddField("laFileStru"  ,   "CVENADR5"       , "C"   ,30   ,0)
=lfAddField("laFileStru"  ,   "CVENADR6"       , "C"   ,20   ,0)
=lfAddField("laFileStru"  ,   "CSHIPNAME"      , "C"   ,30   ,0)
=lfAddField("laFileStru"  ,   "CSHIPADR1"      , "C"   ,30   ,0)
=lfAddField("laFileStru"  ,   "CSHIPADR2"      , "C"   ,30   ,0)
=lfAddField("laFileStru"  ,   "CSHIPADR3"      , "C"   ,30   ,0)
=lfAddField("laFileStru"  ,   "CSHIPADR4"      , "C"   ,30   ,0)
=lfAddField("laFileStru"  ,   "CSHIPADR5"      , "C"   ,30   ,0)
CREATE TABLE (gcWorkDir+lcMainF) FROM ARRAY laFileStru

INDEX ON  custpo+CTRANSNO+CWARCODE+Tabname+COPERCODE TAG Copercode ADDITIVE   && for arranging line in temp file
SELECT(lnAlias)
*-- End Of lfCreatemp.
*:*************************************************************
*: Name      : lfRpHeader
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 06/26/2002
*: Purpose   : get the header data that print in the report
*:*************************************************************
*: Called from : the Program.
*:*************************************************************
*: Calls       : .....
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : Position.
*:*************************************************************
*: Example     : = lfRpHeader()
*:*************************************************************
*:
FUNCTION lfRpHeader
PRIVATE lnAlias

lnAlias = SELECT (0)

SELECT SYCCOMP
SEEK gcAct_Comp
lcCompName = cCom_Name
lcCompPhon = cCom_Phon
lcCompFax = ccom_fax
lcPhonPict = gfPhoneTem()
lcFaxPict = gfPhoneTem()
*--  Get the company addresses
laCompAdd[1]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 1)
laCompAdd[2]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 2)
laCompAdd[3]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 3)
laCompAdd[4]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 4)
laCompAdd[5]    = 'Phone# '+TRANSFORM(lcCompPhon , lcPhonPict)
laCompAdd[6]    = 'Fax# '+TRANSFORM(lcCompFax , lcFaxPict )
DO lfShiftArr WITH laCompAdd

SELECT OBJLINK
SET RELATION TO Objlink.cobject_id INTO Objects ADDITIVE

IF SEEK('*' + 'LOGO' , 'OBJLINK') AND SEEK(OBJLINK.cObject_ID,'OBJECTS')
  llLogo = .T.
  lcObj_Id = OBJLINK.cObject_ID
  *-- Make cursor contain one field and one record holding the company logo
  SELECT gobject;
   FROM Objects         ;
   WHERE Objects.cobject_id = lcObj_Id ;
   INTO CURSOR (lcLogoPic)
ENDIF

SELECT(lnAlias)
RETURN

*-- End OF lfRpHeader.

*:*************************************************************
*: Name      : lfAddFltr
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 06/26/2002
*: Purpose   : Add new Filter To Filter Exp.
*:*************************************************************
*: Called from : MAMCTCS.prg
*:*************************************************************
*: Calls       : .....
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None..
*:*************************************************************
*: Example     : = lfAddFltr()
*:*************************************************************
*
FUNCTION lfAddFltr

*--  Enter the status in the lcRpExp
IF lcStatus <> "L"
  IF EMPTY(lcRpExp)
    lcRpExp= " Mmfgordh.Status = lcStatus "
  ELSE
    lcRpExp = '(' + lcRpExp+ ") .AND. (Mmfgordh.Status = '" + lcStatus +  "')"
  ENDIF
ENDIF

*-- End OF lfAddFltr.

*:*************************************************************
*: Name      : lfCollect
*: Developer : Ehab Ismail. [EIH]
*: Date      : 30/03/2005
*: Purpose   : Function Collect data into temp file.
*:*************************************************************
*: Called from : The Option Grid
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None.
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfCollect()
*:*************************************************************
*:
FUNCTION lfCollect
PRIVATE lcCustPONO , lcTabName , lcOperCode ,lcTranCd , llBOMFirst ,lcOldPo
STORE ''   TO  lcTranCd 
STORE ''  TO  lcCustPONO , lcTabName , lcOperCode
STORE .F. TO  llBOMFirst
*--- we Set Orders to used files here .
SELECT  Notepad
SET ORDER TO TAG  Notepad
SELECT  WAREHOUS
SET ORDER TO TAG Warehous
SELECT  CODES
SET ORDER TO TAG Ccode_no 
SELECT  POFHDR
SET ORDER TO TAG Pofhdr 
SELECT  CTKTBOM
SET ORDER TO TAG Ctktbom 
SELECT  MASOHDR
SET ORDER TO TAG Custpo
SELECT  MFGOPRHD
SET ORDER TO TAG Tktoper 
SELECT  CUSTOMER
SET ORDER TO TAG Customer
SELECT  APVENDOR
SET ORDER TO TAG Vencode
SELECT  MMFGORDD
SET ORDER TO TAG Mmfgordd
SELECT  BOMLINE
SET ORDER TO TAG Mfgopr
SELECT  MMFGORDH
SET ORDER TO TAG MMFGORDH
STORE '' TO lcOldPo 

USE (gcDataDir+'MATINVJL') AGAIN ALIAS MATINRE IN 0 ORDER TAG Mtinvseq 
*--- Begin of collecting data 
*--mhm
*Get our creteria Filter
lnARowOper    = ascan(laOgVrFlt     ,'MFGOPRHD.COPRCODE' )
lnAColOper    = asubscript(laOgVrFlt,lnARowOper        ,1)
lnARowVend    = ascan(laOgVrFlt     ,'APVENDOR.CVENDCODE')
lnAColVend    = asubscript(laOgVrFlt   ,lnARowVend        ,1)

IF !EMPTY(laOgVrFlt[lnAColOper,6])
  lnpos = AT("AND(INLIST(MFGOPRHD.COPRCODE",lcRpExp)
  IF lnPos > 0 
    lcRpExp = LEFT(lcRpExp,lnpos - 1)
  ELSE
    lnpos = AT("INLIST(MFGOPRHD.COPRCODE",lcRpExp)
    lcRpExp = LEFT(lcRpExp,lnpos - 1)
  ENDIF
ENDIF
IF USED(laOgVrFlt[lnAColVend,6])
  STORE '' TO lcVenFlt 
  SELECT (laOgVrFlt[lnAColVend,6])  
  SCAN 
    lcVenFlt = lcVenFlt +  cVendCode + "|"
  ENDSCAN
  lcVenFlt = SUBSTR(lcVenFlt , 1 , LEN(lcVenFlt) - 1 )
  IF !EMPTY(lcVenFlt)
  lnpos = AT("AND(INLIST(APVENDOR.CVENDCODE",lcRpExp)
    IF lnpos > 0
      lcRpExp1 = SUBSTR(lcRpExp , 1 , lnPos - 1 )
    ELSE
      lnpos = AT("INLIST(APVENDOR.CVENDCODE",lcRpExp)
      lcRpExp1 = SUBSTR(lcRpExp , 1 , lnPos - 1 )
    ENDIF
      lcRpExp2 = SUBSTR(lcRpExp , lnPos + 2 , LEN(lcRpExp) )
      lnAndPos = AT("AND",lcRpExp2)
    IF lnAndPos > 0
      lcRpExp2 = SUBSTR(lcVenFlt , lnAndPos + 3  , LEN(lcRpExp2)  )
    ELSE
      lcRpExp2 = ''
    ENDIF
    lcRpExp = lcRpExp1 + lcRpExp2
  ENDIF
ENDIF
IF EMPTY(lcRpExp)
  lcRpExp = ".T."
ENDIF
SELECT  MMFGORDH
SCAN FOR &lcRpExp

*--mhm
  SELECT MFGOPRHD
  =SEEK('T'+MMFGORDH.cmfgordno)
  SCAN REST WHILE cimtyp+ctktno+coprcode ='T'+MMFGORDH.cmfgordno   
    IF !EMPTY(laOgVrFlt[lnAColOper,6]) AND !(MFGOPRHD.COPRCODE $ (laOgVrFlt[lnAColOper,6]))
      LOOP
    ENDIF
    lcTranCd = MFGOPRHD.COPERSEQ
    lcOperCode = MFGOPRHD.coprcode
    ***
    =lfOperName(lcOperCode)
    IF SEEK('O'+MMFGORDH.cmfgordno ,'NotePad')
      m.lmOrdMemo = Notepad.MNotes
    ENDIF
    SELECT mmfgordd
    IF SEEK(MMFGORDH.cmfgordno)
      llBOMFirst = .F.
      SCAN REST WHILE cmfgordno+cfabric+color+dyelot+trancd = MMFGORDH.cmfgordno &&FOR Trancd = STR(lcTranCd,4)
        SCATTER MEMVAR MEMO
        SELECT(lcMainF)
        APPEND BLANK
        GATHER MEMVAR MEMO
        REPLACE CUSTPO                  WITH   MMFGORDH.custpo    ,;
                TABNAME                 WITH   'A'                ,;
                BOMFirst                WITH   .F.                ,;
                COPERCODE               WITH   lcOperCode         ,;
                CTRANSNO                WITH   lcTranCd           ,;
                COPERNAME               WITH   lcOperName         

        IF SEEK(m.CFABRIC+m.COLOR , 'FABRIC')
          REPLACE &lcMainF..DESC        WITH   FABRIC.DESC         ,; 
                  &lcMainF..WIDTH       WITH   FABRIC.WIDTH        ,; 
                  &lcMainF..cFABWEIGHT  WITH   FABRIC.cFABWEIGHT   ,; 
                  &lcMainF..UOMBUY      WITH   FABRIC.UOMBUY       ,; 
                  &lcMainF..UOMUSE      WITH   FABRIC.UOMUSE       ,; 
                  &lcMainF..mOrdMemo    WITH   m.lmOrdMemo         ,; 
                  &lcMainF..CONTENT     WITH   FABRIC.CONTENT      
        ENDIF
        m.lmOrdMemo = ''      
        *here we put header data
        store ' ' to laShipTo , laVendor
        =lfForACC(MMFGORDH.custpo) 
        lcShipVia = Mfgoprhd.coprcomnt
        *-- Ship to data   1- = vendore data of next operation  2- = Alternative ship to data 3- = Warehouse data 
        SELECT MFGOPRHD
        SKIP 1
        IF ALLTRIM(Mfgoprhd.ctktno) == ALLTRIM(MMFGORDH.cmfgordno) && *--- case 1 the Vendore data of Next operation
          lcNextOper = Mfgoprhd.coprcode 
          =lfVendor(lcNextOper)
          laShipTo[1] = laVendor[1]
          laShipTo[2] = laVendor[2]
          laShipTo[3] = laVendor[3]
          laShipTo[4] = laVendor[4]
          laShipTo[5] = laVendor[5]
          laShipTo[6] = laVendor[6]
          laShipTo[7] = laVendor[7]
          SKIP -1
        ELSE
          *--- Customer data  from Alternative ship to of Materal sales order
          SKIP -1
          SELECT MASOHDR
          IF SEEK(MMFGORDH.custpo)     AND MASOHDR.ALT_SHPTO &&   case 2 the alternative ship to adresses
            laShipTo[1] = MASOHDR.stname    &&MASOHDR.stname
            laShipTo[2] = MASOHDR.caddress1            
            laShipTo[3] = MASOHDR.caddress2
            laShipTo[4] = MASOHDR.caddress3
            laShipTo[5] = MASOHDR.caddress4
            laShipTo[6] = MASOHDR.caddress5
            laShipTo[7] = ' '
          ELSE       && the warehouse data case OF  mmfg not multi location AND case OF  mmfg multi location the warehouse data    
            SELECT WAREHOUS
            IF SEEK(mmfgordd.cwarecode)
              lcWareCode  = mmfgordd.cwarecode
              llMultiLoc  = .T.
              laShipTo[1] = WAREHOUS.cdesc  
              laShipTo[2] = WAREHOUS.caddress1
              laShipTo[3] = WAREHOUS.caddress2
              laShipTo[4] = WAREHOUS.caddress3
              laShipTo[5] = WAREHOUS.caddress4
              laShipTo[6] = WAREHOUS.caddress5
              laShipTo[7] = WAREHOUS.caddress6
            ENDIF
          ENDIF
        ENDIF        
        =lfVendor(lcOperCode)                          
        *here we enter the data in temp file
        SELECT  (lcMainF)    
        REPLACE &lcMainF..cForAcc         WITH   lcForAcc     ,;   
                &lcMainF..cCustomer       WITH   lcCustomer   ,; 
                &lcMainF..cShipVia        WITH   lcShipVia    ,; 
                &lcMainF..cVenName        WITH   laVendor[1]  ,;
                &lcMainF..cVenAdr1        WITH   laVendor[2]  ,; 
                &lcMainF..cVenAdr2        WITH   laVendor[3]  ,; 
                &lcMainF..cVenAdr3        WITH   laVendor[4]  ,; 
                &lcMainF..cVenAdr4        WITH   laVendor[5]  ,;
                &lcMainF..cVenAdr5        WITH   laVendor[6]  ,;
                &lcMainF..cVenAdr6        WITH   laVendor[7]  ,;
                &lcMainF..VendCode        WITH   laVendor[8]  ,;
                &lcMainF..VendCode2       WITH   laVendor[8]  ,;
                &lcMainF..cShipName       WITH   laShipTo[1]  ,;
                &lcMainF..cShipAdr1       WITH   laShipTo[2]  ,;
                &lcMainF..cShipAdr2       WITH   laShipTo[3]  ,;
                &lcMainF..cShipAdr3       WITH   laShipTo[4]  ,;
                &lcMainF..cShipAdr4       WITH   laShipTo[5]  ,;
                &lcMainF..cShipAdr5       WITH   laShipTo[6]  ,;
                &lcMainF..CWARCODE       WITH   lcWareCode    ,;
                &lcMainF..LMULTILOC       WITH  llMultiLoc    
                lcWareCode  = ''
                llMultiLoc  = .F.
        lcOldPo = MMFGORDH.custpo
        SCATTER MEMVAR BLANK
      ENDSCAN
    ENDIF    




    SELECT  (lcMainF)    
    STORE .T. TO llBlank
    =SEEK(MMFGORDH.custpo+lcTranCd)
    SCAN WHILE custpo+CTRANSNO+CWARCODE+Tabname+COPERCODE = MMFGORDH.custpo+lcTranCd  FOR  TABNAME = 'B'
      llBlank = .F.
    ENDSCAN
    IF llBlank
      SCATTER MEMVAR BLANK
      SELECT (lcMainF)
      APPEND BLANK
      m.TABNAME    = 'B' 
      m.CUSTPO     = MMFGORDH.CUSTPO
      m.CMFGORDNO  = MMFGORDH.cmfgordno 
      m.CTRANSNO   = lcTranCd
      m.COPERCODE  = lcOperCode
      m.COPERNAME  = lcOperName   
      GATHER MEMVAR MEMO
    ENDIF
    llBlank = .T.
    SELECT CTKTBOM
    llBOMFirst = .T.
    IF SEEK('T'+MMFGORDH.cmfgordno+'1') AND !EMPTY(MMFGORDH.custpo)
      SCAN REST WHILE cimtyp+cuttkt+typ+item+iclr+mfgcode+dyelot = 'T'+MMFGORDH.cmfgordno+'1'  &&BEGIN OF CTKTBOM SCAN
        SCATTER MEMVAR MEMO
        lcVendore = ' '
        IF CTKTBOM.issue_qty > 0 
          *then item is issued we must use lot# from mfgoprdt to get the vendore code
          SELECT MATINVJL
          SET ORDER TO TAG Mfgopr
          IF SEEK(MMFGORDH.cmfgordno,'MATINVJL')
          **********
            SCAN REST WHILE ctran = MMFGORDH.cmfgordno
            * we must here loop
              lcSession = MATINVJL.CRSESSION
              *BEGIN
              SELECT MATINRE
              SEEK(lcSession)
              SCAN REST WHILE ctrn_seq = lcSession FOR  cfabric = PADR(ALLTRIM(m.item),7,' ') AND ccolor = m.iclr  AND ctrantype = '1' && &&BEGIN OF INNER SCAN
                lcTranNo   = MATINRE.ctran
                SELECT POFHDR
                IF SEEK('P'+lcTranNo)
                  lcVendore = POFHDR.vendor
                ENDIF
                *****WE MUST INSERT ROW HERE 
                SELECT (lcMainF)
                APPEND BLANK
                GATHER MEMVAR MEMO
                REPLACE CUSTPO    WITH MMFGORDH.custpo  , ;
                        TABNAME   WITH 'C'              , ;
                        BOMFirst  WITH llBOMFirst       , ;
                        COPERCODE WITH lcOperCode       , ;
                        CTRANSNO  WITH lcTranCd         , ; 
                        CMFGORDNO WITH MMFGORDH.cmfgordno ,;
                        COPERNAME WITH lcOperName       
  
                IF llBOMFirst 
                  llBOMFirst =.F.
                ENDIF
                  IF EMPTY(lcVendore)
                    lcVendore = FABRIC.vendor 
                  ENDIF
                  REPLACE &lcMainF..DESC         WITH    FABRIC.DESC         ,; 
                          &lcMainF..vendcode     WITH    lcVendore           ,; 
                          &lcMainF..WIDTH        WITH    FABRIC.WIDTH        ,; 
                          &lcMainF..CFABWEIGHT   WITH    FABRIC.cFABWEIGHT   ,; 
                          &lcMainF..UOMBUY       WITH    FABRIC.UOMBUY       ,; 
                          &lcMainF..UOMUSE       WITH    FABRIC.UOMUSE       ,; 
                          &lcMainF..CONTENT      WITH    FABRIC.CONTENT      ,;
                          &lcMainF..Item_type    WITH    FABRIC.WIDTH        
                SELECT MATINRE  
              ENDSCAN &&END OF INNER SCAN
            ENDSCAN
          ELSE && IS ISSUED AND WE CAN'T FIND IT IN MATINVJL
            SELECT (lcMainF)
            APPEND BLANK
            GATHER MEMVAR MEMO
            REPLACE CUSTPO    WITH MMFGORDH.custpo  , ;
                    TABNAME   WITH 'C'              , ;
                    BOMFirst  WITH llBOMFirst       , ;
                    COPERCODE WITH lcOperCode       , ;
                    CTRANSNO  WITH lcTranCd         , ; 
                    CMFGORDNO WITH MMFGORDH.cmfgordno ,;
                    COPERNAME WITH lcOperName       

            IF llBOMFirst 
              llBOMFirst =.F.
            ENDIF
            IF SEEK(PADR(ALLTRIM(m.item),7,' ')+ALLTRIM(m.iclr), 'FABRIC')
              IF EMPTY(lcVendore)
                lcVendore = FABRIC.vendor 
              ENDIF
              REPLACE &lcMainF..DESC         WITH    FABRIC.DESC         ,; 
                      &lcMainF..vendcode     WITH    lcVendore           ,; 
                      &lcMainF..WIDTH        WITH    FABRIC.WIDTH        ,; 
                      &lcMainF..CFABWEIGHT   WITH    FABRIC.cFABWEIGHT   ,; 
                      &lcMainF..UOMBUY       WITH    FABRIC.UOMBUY       ,; 
                      &lcMainF..UOMUSE       WITH    FABRIC.UOMUSE       ,; 
                      &lcMainF..CONTENT      WITH    FABRIC.CONTENT      ,;
                      &lcMainF..Item_type    WITH    FABRIC.WIDTH        
            ENDIF
          ENDIF          
        ELSE  && IF NOT ISSUED
          SELECT (lcMainF)
          APPEND BLANK
          GATHER MEMVAR MEMO
          REPLACE CUSTPO    WITH MMFGORDH.custpo  , ;
                  TABNAME   WITH 'C'              , ;
                  BOMFirst  WITH llBOMFirst       , ;
                  COPERCODE WITH lcOperCode       , ;
                  CTRANSNO  WITH lcTranCd         , ;
                  CMFGORDNO WITH MMFGORDH.cmfgordno ,;                   
                  COPERNAME WITH lcOperName       

          IF llBOMFirst 
            llBOMFirst =.F.
          ENDIF
          IF SEEK(PADR(ALLTRIM(m.item),7,' ')+ALLTRIM(m.iclr), 'FABRIC')
            IF EMPTY(lcVendore)
              lcVendore = FABRIC.vendor 
            ENDIF
            REPLACE &lcMainF..DESC         WITH    FABRIC.DESC         ,; 
                    &lcMainF..vendcode     WITH    lcVendore           ,; 
                    &lcMainF..WIDTH        WITH    FABRIC.WIDTH        ,; 
                    &lcMainF..CFABWEIGHT   WITH    FABRIC.cFABWEIGHT   ,; 
                    &lcMainF..UOMBUY       WITH    FABRIC.UOMBUY       ,; 
                    &lcMainF..UOMUSE       WITH    FABRIC.UOMUSE       ,; 
                    &lcMainF..CONTENT      WITH    FABRIC.CONTENT      ,;
                    &lcMainF..Item_type    WITH    FABRIC.WIDTH        
          ENDIF
        ENDIF
        SCATTER MEMVAR BLANK
      ENDSCAN    && END OF SCAN CTKTBOM
    ENDIF
  ENDSCAN
ENDSCAN
  

IF !EMPTY(lcVenFlt)
  SELECT (lcMainF)
  GO TOP
  SCAN FOR !EMPTY(VendCode2)
    lcCustPO  = &lcMainF..CUSTPO
    lcOperSeq = &lcMainF..CTRANSNO
    lcOperVen =  &lcMainF..VendCode2
    lnRecordNO =RECNO()
    REPLACE FOR   EMPTY(VendCode2)  AND CUSTPO = lcCustPO  AND  CTRANSNO = lcOperSeq VendCode2 WITH lcOperVen 
    IF BETWEEN(lnRecordNo , 1, RECCOUNT())
      GOTO lnRecordNo 
    ENDIF
  ENDSCAN
    SET FILTER TO &lcMainF..VendCode2 $ lcVenFlt
ENDIF



SELECT MATINRE  
USE
SELECT (lcMainF)
GO TOP
SCAN 
lcShipMny = &lcMainF..CSHIPVIA
  IF ALLTRIM(lcShipMny) <> 'MULTIPLE' AND !EMPTY(lcShipMny) 
    lcCustPO  = &lcMainF..CUSTPO
    lcOperSeq = &lcMainF..CTRANSNO
    lnRecordNO =RECNO()
    LOCATE FOR CSHIPVIA <> lcShipMny AND !EMPTY(CSHIPVIA) AND CUSTPO = lcCustPO AND CTRANSNO = lcOperSeq 
    IF FOUND()
      REPLACE FOR  CUSTPO = lcCustPO AND CTRANSNO = lcOperSeq CSHIPVIA WITH 'MULTIPLE'
      GO TOP
    ELSE
      IF BETWEEN(lnRecordNo , 1, RECCOUNT())
        GOTO lnRecordNo 
      ENDIF
    ENDIF
  ENDIF
ENDSCAN

* here we arrange data if there is mmfg order on multi location
GO TOP
STORE .F. TO llMulti 
SCAN FOR LMULTILOC = .T.
  lcWareCode = &lcMainF..Cwarcode
  lcOperSeq  = &lcMainF..Ctransno
  lcCustPo   = &lcMainF..Custpo
  lcRecNo = RECNO()  
  SCAN FOR custpo = lcCustPo  AND CTRANSNO=lcOperSeq  AND  TABNAME = 'A   ' AND CWARCODE <> lcWareCode
    llMulti = .T.
  ENDSCAN
  IF !llMulti 
    GO TOP
    SCAN WHILE custpo+CTRANSNO+CWARCODE+Tabname+COPERCODE = lcCustPo FOR  LMULTILOC = .T.  AND !EMPTY(CWARCODE)
      REPLACE  CWARCODE WITH '' ,;
               LMULTILOC  WITH .F. 
    ENDSCAN
  ENDIF
  IF BETWEEN(lcRecNo , 1, RECCOUNT())
      GOTO lcRecNo 
  ENDIF
ENDSCAN
GO TOP
SCAN FOR LMULTILOC = .T.
  lcWareCode = &lcMainF..Cwarcode
  lcOperSeq  = &lcMainF..Ctransno
  lcCustPo   = &lcMainF..Custpo
  lcOrdno    = &lcMainF..CMFGORDNO 
  lcRecNo = RECNO()  
  IF !SEEK(lcCustPo+lcOperSeq+lcWareCode+'B   ')
    GO TOP
    =SEEK(lcCustPo+lcOperSeq)
    SCAN REST WHILE custpo+CTRANSNO+CWARCODE+Tabname+COPERCODE = lcCustPo+lcOperSeq  FOR  TABNAME <> 'A   ' AND EMPTY(CWARCODE) 
      SCATTER MEMVAR MEMO
      REPLACE LDELETE WITH .T. 
      lcNeword  = &lcMainF..CMFGORDNO 
      lcRecNo2 = RECNO()  
      m.Cwarcode = lcWareCode 
      IF TABNAME = 'B   '
        APPEND BLANK
        GATHER MEMVAR MEMO
        REPLACE LLLAST WITH .F. 
        REPLACE LDELETE WITH .F. 
      ELSE 
        IF lcNeword = lcOrdno    
          APPEND BLANK
          GATHER MEMVAR MEMO
          REPLACE LLLAST WITH .F. 
          REPLACE LDELETE WITH .F. 
        ENDIF
        
      ENDIF
      IF BETWEEN(lcRecNo2, 1, RECCOUNT())
          GOTO lcRecNo2
      ENDIF  
    ENDSCAN
    IF BETWEEN(lcRecNo , 1, RECCOUNT())
      GOTO lcRecNo
    ENDIF
  ENDIF
ENDSCAN
DELETE FOR LDELETE = .T.


*FOR GET NOTEPADS
IF llrprtmmon
  STORE ' ' TO lcOdCustPO  , lMNewNot ,lcOdOprSeq
  GO TOP
  llTextMode = (UPPER(ALLTRIM(lcRepMode))=="TEXT")  && Print Text Format
  SCAN FOR lmultiloc <> .T.
    lcCustPO  = &lcMainF..CUSTPO
    lcOprSeq  = &lcMainF..CTRANSNO 
    SEEK (lcCustPO+lcOprSeq)
    SCAN WHILE &lcMainF..CUSTPO = lcCustPO AND  &lcMainF..CTRANSNO = lcOprSeq 
      lmNewOrd  = &lcMainF..MORDMEMO
      IF !EMPTY(lmNewOrd) 
        IF !llTextMode 
          lcOrdHdr = CHR(10)+"Notebad For MMFG Order#  "+&lcMainF..CMFGORDNO +"     "+CHR(10)
        ELSE
          lcOrdHdr = CHR(10)+CHR(13)+"Notebad For MMFG Order#  "+&lcMainF..CMFGORDNO+"     "+CHR(10)+CHR(13)
        ENDIF
        lMNewNot = lMNewNot + lcOrdHdr +lmNewOrd
      ENDIF
    ENDSCAN
    IF OCCURS("Notebad For MMFG Order#  ",lMNewNot ) = 1
      lMNewNot  =STRTRAN(lMNewNot  , "Notebad For MMFG Order#  ", ' ')
      lMNewNot  =SUBSTR(lMNewNot,12)
    ENDIF
    lnRecordNo = RECNO()
    REPLACE FOR  CUSTPO = lcCustPO  AND CTRANSNO = lcOprSeq  AND lmultiloc <> .T. MORDMEMO WITH lMNewNot 
    IF BETWEEN(lnRecordNo , 1, RECCOUNT())
      GOTO lnRecordNo 
    ENDIF
    lMNewNot = ''
  ENDSCAN
  
  SCAN FOR lmultiloc = .T.
    lnRecordNo = RECNO()
    lcCustPO  = &lcMainF..CUSTPO
    lcOprSeq  = &lcMainF..CTRANSNO 
    lcOrdNo = &lcMainF..CMFGORDNO
    REPLACE FOR  CUSTPO = lcCustPO  AND CTRANSNO = lcOprSeq AND CMFGORDNO = lcOrdNo  AND TABNAME <> 'A   '  MORDMEMO WITH ''
    IF BETWEEN(lnRecordNo , 1, RECCOUNT())
      GOTO lnRecordNo 
    ENDIF
  ENDSCAN
  SCAN FOR lmultiloc = .T.
    lcCustPO  = &lcMainF..CUSTPO
    lcOprSeq  = &lcMainF..CTRANSNO 
    lcWareCode = &lcMainF..Cwarcode
    lnRecordNo = RECNO()
    SCAN FOR &lcMainF..CUSTPO = lcCustPO AND  &lcMainF..CTRANSNO = lcOprSeq AND Cwarcode = lcWareCode AND !EMPTY(MORDMEMO) 
      lmNewOrd  = &lcMainF..MORDMEMO
      IF !EMPTY(lmNewOrd) 
        IF !llTextMode 
          lcOrdHdr = CHR(10)+"Notebad For MMFG Order#  "+&lcMainF..CMFGORDNO +"     "+CHR(10)
        ELSE
          lcOrdHdr = CHR(10)+CHR(13)+"Notebad For MMFG Order#  "+&lcMainF..CMFGORDNO+"     "+CHR(10)+CHR(13)
        ENDIF
        lMNewNot = lMNewNot + lcOrdHdr +lmNewOrd
      ENDIF
    ENDSCAN
    IF OCCURS("Notebad For MMFG Order#  ",lMNewNot ) = 1
      lMNewNot  =STRTRAN(lMNewNot  , "Notebad For MMFG Order#  ", ' ')
      lMNewNot  =SUBSTR(lMNewNot,12)
    ENDIF
    REPLACE FOR  CUSTPO = lcCustPO  AND CTRANSNO = lcOprSeq  AND  Cwarcode = lcWareCode  AND !EMPTY(MORDMEMO)   MORDMEMO WITH lMNewNot 
    IF BETWEEN(lnRecordNo , 1, RECCOUNT())
      GOTO lnRecordNo 
    ENDIF
    lMNewNot = ''
  ENDSCAN
  SCAN FOR lmultiloc = .T.
    lMNewNot = &lcMainF..MORDMEMO
    lcCustPO  = &lcMainF..CUSTPO
    lcOprSeq  = &lcMainF..CTRANSNO 
    lcWareCode = &lcMainF..Cwarcode
    lnRecordNo = RECNO()
    REPLACE FOR  CUSTPO = lcCustPO  AND CTRANSNO = lcOprSeq  AND  Cwarcode = lcWareCode  AND EMPTY(MORDMEMO) MORDMEMO WITH lMNewNot 
    IF BETWEEN(lnRecordNo , 1, RECCOUNT())
      GOTO lnRecordNo 
    ENDIF
    lMNewNot = ''
  ENDSCAN
ENDIF

* for text format report 
IF llrprtmmon
  GO TOP
  lcOdOprSeq  = &lcMainF..CTRANSNO 
  lcOdWare  = &lcMainF..Cwarcode 
  SCAN 
    lcOprSeq  = &lcMainF..CTRANSNO 
    lcNewWare  = &lcMainF..Cwarcode 
    IF lcOdOprSeq = lcOprSeq   AND lcNewWare  = lcOdWare  
      REPLACE LLLAST WITH .F. 
    ELSE
     SKIP -1
     REPLACE LLLAST WITH .T. 
     lcOdOprSeq  = lcOprSeq  
     SKIP 1
    ENDIF
    lcOdWare = lcNewWare  
  ENDSCAN
  GO BOTTOM
  REPLACE LLLAST WITH .T. 
ENDIF



*EIH
*-- End OF lfCollect.
*:****************************************************************
*: Name      : lfForACC
*: Developer : Ehab Ismail. [EIH]
*: Date      : 03/30/2005
*: Purpose   : 'For Account' and 'Customer' fields
*:****************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:****************************************************************
*: Called from : Program.
*:****************************************************************
*: Passed Parameters  : ...
*:****************************************************************
*: Returns            : None.
*:****************************************************************
*: Example   : = lfForACC()
*:****************************************************************
*-- Begin OF lfForACC .
FUNCTION lfForACC
PARAMETERS lcCustPO
STORE '' TO lcForAcc , lcCustomer 
SELECT MASOHDR
IF SEEK(lcCustPO) 
  IF MASOHDR.ALT_SHPTO
    lcForAcc   = MASOHDR.ACCOUNT  
    IF SEEK('M'+lcForAcc, 'CUSTOMER')
      lcForAcc = CUSTOMER.btname
    ENDIF
    lcCustomer = MASOHDR.STNAME
  ELSE
    lcCustomer = MASOHDR.ACCOUNT
    IF SEEK('M'+lcCustomer, 'CUSTOMER')
      lcCustomer = CUSTOMER.btname
    ENDIF
    SELECT CODES
    IF SEEK('D'+'CDIVISION ')
      lcDefault = CODES.cCode_NO
      =SEEK('N'+'CDIVISION '+lcDefault )
      SCAN REST WHILE cdefcode+cfld_name+ccode_no+cdiscrep+crltd_nam = 'N'+'CDIVISION '  FOR  crltd_nam = 'DIVLNAME  '
        lcForAcc   = CODES.crltd_vlu
      ENDSCAN
    ENDIF   
  ENDIF
ELSE
  SELECT CODES
  IF SEEK('D'+'CDIVISION ')
    lcDefault = CODES.cCode_NO
    =SEEK('N'+'CDIVISION '+lcDefault )
    SCAN REST WHILE cdefcode+cfld_name+ccode_no+cdiscrep+crltd_nam = 'N'+'CDIVISION '  FOR  crltd_nam = 'DIVLNAME  '
      lcForAcc   = CODES.crltd_vlu
      lcCustomer = ' '
    ENDSCAN
  ENDIF
ENDIF

*-- End OF lfForACC .
*:****************************************************************
*: Name      : lfVendor
*: Developer : Ehab Ismail Hamed. [EIH]
*: Date      : 03/30/2005
*: Purpose   : get the vendor data .
*:****************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : gfRltFld()
*:****************************************************************
*: Called from : Program.
*:****************************************************************
*: Passed Parameters  : ...
*:****************************************************************
*: Returns            : None.
*:****************************************************************
*: Example   : = lfVendor()
*:****************************************************************

*-- Begin OF lfVendor .
FUNCTION lfVendor
PARAMETERS lcOprCode 
PRIVATE laInHouse ,laVenCode,laVenName
DIMENSION laInHouse[1,2] , laVenCode[1,2] , laVenName[1,2] 
laInHouse[1,1]  = 'LINHOUSE'
laInHouse[1,2]  = 'llInHouse'
laVenCode[1,1]  = 'CCONTCODE'
laVenCode[1,2]  = 'lcVenCode'
laVenName[1,1]  = 'CCONTNAME'
laVenName[1,2]  = 'lcVenName'

STORE ' ' TO lcVenName , lcVenCode , laVendor
llInHouse = .F.
= gfRltFld(@lcOprCode , @laInHouse , 'MFGCODE')  && Get the inhouse flage value .
= gfRltFld(@lcOprCode , @laVenName , 'MFGCODE')  && Get the vendore Name value .
laVendor[1] = lcVenName
IF !llInHouse 
  = gfRltFld(@lcOprCode , @laVenCode , 'MFGCODE')  && Get the vendore code value if exist.
  IF SEEK(ALLTRIM(lcVenCode) , 'APVENDOR')
    laVendor[2] = APVENDOR.caddress1    &&gfGetAdr('APVENDOR' , '' , '' , '' , 1) && APVENDOR.caddress1
    laVendor[3] = APVENDOR.caddress2    &&gfGetAdr('APVENDOR' , '' , '' , '' , 2) &&APVENDOR.caddress2
    laVendor[4] = APVENDOR.caddress3    &&gfGetAdr('APVENDOR' , '' , '' , '' , 3) &&APVENDOR.caddress3
    laVendor[5] = APVENDOR.caddress4    &&gfGetAdr('APVENDOR' , '' , '' , '' , 4) &&APVENDOR.caddress4
    laVendor[6] = APVENDOR.caddress5    &&gfGetAdr('APVENDOR' , '' , '' , '' , 5) &&APVENDOR.caddress5
    laVendor[7] = APVENDOR.cvencont      &&Vendor contact
    laVendor[8] = lcVenCode
  ENDIF
ENDIF
*-- End OF lfVendor .

*:****************************************************************
*: Name      : lfOperName
*: Developer : Ehab Ismail Hamed. [EIH]
*: Date      : 03/30/2005
*: Purpose   : get the operaton name .
*:****************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : gfRltFld()
*:****************************************************************
*: Called from : Program.
*:****************************************************************
*: Passed Parameters  : ...
*:****************************************************************
*: Returns            : None.
*:****************************************************************
*: Example   : = lfOperName()
*:****************************************************************
*-- Begin OF lfOperName .
FUNCTION lfOperName 
PARAMETERS lcOprCode 

lcAlias = ALIAS()
SELECT CODES
*Here we get the operation name
STORE ' ' TO  lcOperName 
IF SEEK('N'+'MFGCODE   '+lcOprCode ,'CODES') 
SCAN REST WHILE cdefcode+cfld_name+ccode_no+cdiscrep+crltd_nam = 'N'+'MFGCODE   '+lcOprCode FOR  crltfield = 'N'
  lcOperName = codes.CDISCREP
ENDSCAN
ENDIF
SELECT &lcAlias
*-- End OF lfOperName .
*:****************************************************************
*: Name      : lfCustPO  
*: Developer : Ehab Ismail Hamed. [EIH]
*: Date      : 03/30/2005
*: Purpose   : To set relation as 'DISTINCT' in Customer PO#.
*:****************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:****************************************************************
*: Called from : Program.
*:****************************************************************
*: Passed Parameters  : ...
*:****************************************************************
*: Returns            : None.
*:****************************************************************
*: Example   : = lfCustPO()
*:****************************************************************
*-- Begin OF lfCustPO 
FUNCTION lfCustPO  
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to Style Major 
    *-- unique index.
    USE (gcDataDir+'mmfgordh') AGAIN ALIAS mmfgor_X ORDER TAG CustPo IN 0
    SELECT mmfgordh
    SET ORDER TO TAG CustPo
    SET RELATION TO mmfgordh.CustPo INTO mmfgor_X
    GO TOP IN mmfgordh
    llChStyle = .T.
  CASE lcParm = 'R'  && Reset code
    USE IN mmfgor_X
    SELECT mmfgordh
    SET ORDER TO TAG CustPo
ENDCASE
*-- End OF lfCustPO .




*:*************************************************************
*: Name      : lfSetVenRp
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 06/26/2002
*: Purpose   : Go top in Vendor file.
*:*************************************************************
*: Called from : Option Grid
*:*************************************************************
*: Calls       : .....
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : Position.
*:*************************************************************
*: Example     : = lfSetVenRp()
*:*************************************************************
*:
FUNCTION lfSetVenRp
PARAMETERS OpGrdParm
PRIVATE lnAlias

lnAlias = SELECT (0)

DO CASE
  CASE OpGrdParm = 'S'
   SELECT APVENDOR
   LOCATE
ENDCASE

SELECT(lnAlias)
RETURN
*:*************************************************************
*: Name      : lfSetmmoRp
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 06/26/2002
*: Purpose   : Go top in Mfg. Order file.
*:*************************************************************
*: Called from : Option Grid
*:*************************************************************
*: Calls       : .....
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : Position.
*:*************************************************************
*: Example     : = lfSetmmoRp()
*:*************************************************************
*:
FUNCTION lfSetmmoRp
PARAMETERS OpGrdParm
PRIVATE lnAlias

lnAlias = SELECT (0)

DO CASE
  CASE OpGrdParm = 'S'
   SELECT MMFGORDH
   LOCATE
ENDCASE

SELECT(lnAlias)
RETURN



*!*************************************************************
*! Name      : lfGetFrx
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 06/26/2002
*! Purpose   : Check If the file exist or not.
*:*************************************************************
*: Called from : MAOPMG.PRG
*:*************************************************************
*: Calls       : None.
*:*************************************************************
*: Passed Parameters : None.
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfGetFrx()
*:*************************************************************
*:
FUNCTION lfGetFrx
IF RAT('\',lcFormName)=0
  lcFrxPath= IIF(FILE(gcRepHome+lcFormName+'.FRX') ;
  				     .OR. FILE(gcRepHome+lcFormName+'.LBX'),;
  				     gcRepHome+lcFormName,gcRepHome+gcAct_Appl+'\'+lcFormName)
ENDIF  				   
RETURN FILE(lcFrxPath +'.FRX') .OR. FILE(lcFrxPath+'.LBX')  				   

*-- End Of lfGetFrx.




*:*************************************************************
*: Name      : lfvOMsg
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 06/26/2002
*: Purpose   : To Open the Optional message screen
*:*************************************************************
*: Called from : Option Grid.
*:*************************************************************
*: Calls       : None.
*:*************************************************************
*: Passed Parameters : None.
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfvOMsg()
*:*************************************************************
*:
FUNCTION lfvOMsg

PRIVATE laOptMsg
DECLARE laOptMsg[3,2]       && Array to hold the name and length of the variables to be used in the Optional message screen
laOptMsg[1,1] = 'lcOMsg1'   && 1st. line Variable
laOptMsg[1,2] = 75          && Line length
laOptMsg[2,1] = 'lcOMsg2'   && 1st. line Variable
laOptMsg[2,2] = 75          && Line length
laOptMsg[3,1] = 'lcOMsg3'   && 1st. line Variable
laOptMsg[3,2] = 75          && Line length

= gfOptMsg('laOptMsg')      && Call Function to write optional message.

*-- End OF lfvOMsg.