*:****************************************************************
*: Program file  : HLCMSEF.PRG
*: Program desc. : Multiple / Single Entry Form.
*: System        : Aria Apparel System (A27).
*: Module        : H.L.C. System. (HL)
*: Developer     : ABDOU ELGENDI - (ABD) Due to C#200246,1
*: Date          : 01/20/2002
*:****************************************************************
*: Calls 
*:               : FUNCTIONS  : lfwRepWhen , lfvInvcNo , lfRprtText
*:               :            : lfVTextok  , lfStyle   , lfStyTot
*:               :            : lfStyDis   , lfCut     , lfSew
*:               :            : lfCutCont  , lfSewCont , lfExportA
*:               :            : lfStyFab   , lfCreatemp, lfCollect
*:               :            : 
*:               -----------------------------------------------
*:               : PROCEDURE  : lfFillCode , lfFillOne , lpClose
*:****************************************************************
*: Passed Parameters  : None.
*:****************************************************************
*:C#200246,1.
*:****************************************************************
*:Modifications  :
*:****************************************************************
*:
*-- Begin Declaration variables.
*-- lcTemp    :- Variable Hold the temp file that collect the data.
*-- lcRpInvce :- Variable Hold invoice number.
*-- lcRpVenVal:- Variable Hold the Vendor.
*-- lcDis     :- Variable to hold Quota+Cheif Code+Const Code+Garment Code.
*-- End Declaration variables.

STORE '' TO lcDis , lcStyle , lcFab , lcDes , lcExport , lcCut , lcCut2,;
            lcCut3, lcSew   , lcCutCont,lcSewCont , lcStyDis , lcMonth

*- Message Text   :- You must enter a invoice number for the report to be displayed.
*- Message No.    :- 000000.
*- Buttom Message :- Ok
*- Buttom Number  :- 00000.
IF EMPTY(ALLTRIM(lcRpInvce) )
  = gfModalGen('INM00000B00000','F','ALERT',' ','You must enter an invoice number for the report to be displayed.')
  RETURN
ENDIF


lcSavCent= SET('CENTURY')
SET CENTURY ON
lcDate = MDY(DATE())
SET CENTURY &lcSavCent

DIMENSION laCompAdd[2]
STORE '' TO laCompAdd
SELECT SYCCOMP
SEEK gcAct_Comp

laCompAdd[1] = gfGetAdr('SYCCOMP' , '' , '' , '' , 1)
laCompAdd[2] = gfGetAdr('SYCCOMP' , '' , '' , '' , 2)


*-- if user change filter criteria then you must collect data again [Begin]
IF llOGFltCh
  *-- Function To Create temp File
  = lfCreatemp()
  *-- Collecting Data In The First Time.
  = lfCollect ()

ENDIF

SELECT (lcTemp)
LOCATE
DO gfDispRe WITH EVAL('lcRpForm')

RETURN
*-- End of program code.
*:*************************************************************
*: Name      : lfwRepWhen
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/20/2002
*: Purpose   : When function of OG
*:*************************************************************
*: Called from : OG read cycle
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfwRepWhen()
*:*************************************************************
*
FUNCTION lfwRepWhen

*-- lcRpSewVal :- hold the def. so the filter can work in default case
lcRpVenVal = PADR(laVenVal,6)
lcOldVal   = lcRpVenVal

IF FILE('&gcRepHome.hlcMefTxt.MEM')
  RESTORE FROM &gcRepHome.hlcMefTxt.MEM ADDITIVE 
ELSE
  lcRpPrtTxt = SPACE(700)
ENDIF

*-- End OF lfwRepWhen.
*:*************************************************************
*: Name      : lfFillCode
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/20/2002
*: Purpose   : Collecting Data In The First Time.
*:*************************************************************
*: Called from : OG read cycle
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfFillCode ()
*:*************************************************************
*
PROCEDURE lfFillCode
PRIVATE lcCodeOrd , lnCount

lcCodeOrd = ORDER("CODES")

SET ORDER TO 0 IN CODES
DIMENSION laVenVal[1] , laVenCode[1]

= lfFillOne("laVenCode" ,"laVenVal","CTSUVENCD "   )


*-- End OF lfFillCode
*:*************************************************************
*: Name      : lfFillOne
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/20/2002
*: Purpose   : Fill one array from codes file
*:*************************************************************
*: Called from : OG read cycle
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters  : lcShowArry  : Array that appears in popup
*:                      lcValArray  : Return values array
*:                      lcCodeVal   : Filter Codes table on this Code 
*:*************************************************************
*: Returns            : Position
*:*************************************************************
*: Example   : = lfFillOne("laVenCode","laVenVal","CTSUVENCD ")
*:*************************************************************
*:
PROCEDURE lfFillOne
PARAMETERS lcShowArry , lcValArray , lcCodeVal
DECLARE &lcShowArry.[1,1]

SELECT CCODE_NO+"-"+CDISCREP FROM CODES ;
  WHERE cDefCode + cRltField + cFld_Name = "NN" + lcCodeVal ;
  ORDER BY CCODE_NO ;
  INTO ARRAY &lcShowArry.

IF _TALLY > 0
  DECLARE &lcValArray.[_TALLY,1]
  lnCount = 0
  FOR lnCount = 1 TO _TALLY
    &lcValArray.[lnCount] = PADL(&lcShowArry.[lnCount],6) 
  ENDFOR
ENDIF

*-- End of lfFillOne.
*:*************************************************************
*: Name      : lfvInvcNo
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/20/2002
*: Purpose   : Browse The invoices for the selected vendor,
*:*************************************************************
*: Called from : OG read cycle
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfvInvcNo ()
*:*************************************************************
*
FUNCTION lfvInvcNo
PRIVATE lnAlias
lnAlias = SELECT (0)

IF !EMPTY(lcRpInvce) .AND. !EMPTY(lcRpVenVal) .AND. !SEEK(lcRpVenVal+PADR(lcRpInvce,12),'HLCINVHD')
  SELECT HLCINVHD
  DECLARE  laBrowArr[1]
  laBrowArr = ''
  lcBrFields = [lcTsuvencd=gfCodDes(CTSUVENCD,'CTSUVENCD'):H='Vendor',cinvtype:H='Invoice Type', ccinvno:H='Invoice Number' , dcinvdate:H='Invoice Date',]+;
               [ccinvesel:H='Vessel' , ccinconno:H='Container Number',ccinterms:H='Terms',ncinpackg:H='Packages',]+;
               [ccinshipv:H='Shipped Via' , ncintunit:H='Total Units' , ncintdoz:H='Total Dozens' , ncintval:H='Total Value',]+;
               [namountp:H='Amount Paid']
  
  lcRpInvce   = IIF(ARIABROW('FOR cTsuvencd = lcRpVenVal ',"CommercialInvoice Header",gnBrFSRow1, gnBrFSCol1,;
                    gnBrFSRow2, gnBrFSCol2,'','','cTsuvencd','laBrowArr'),;
                    HLCINVHD.ccInvNo,SPACE(12))


ENDIF

SELECT(lnAlias)

*-- End OF lfvInvcNo
*:*************************************************************
*: Name      : lfRprtText
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/20/2002
*: Purpose   : Show report Text header For the report.
*:*************************************************************
*: Called from : OG read cycle
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfRprtText ()
*:*************************************************************
*
FUNCTION lfRprtText

lnCurObj   = 1
lcRprtText = lcRpPrtTxt

PUSH KEY
ON KEY LABEL ESC 
ON KEY LABEL ESC DO lpClose
DO (gcRepHome+gcAct_Appl+"\HlcmSef.SPX")
POP KEY


*-- End Of lfRprtText
*:*************************************************************
*: Name      : lpClose
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 11/19/2001
*: Purpose   : Trap Esc for lines entry.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  DO lpClose
*:*************************************************************
*
PROCEDURE lpClose

_CUROBJ = OBJNUM(pbCancel)
KEYBOARD '{ENTER}'
RETURN

*-- End Of lpClose
*:*************************************************************
*: Name      : lfVTextok
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/20/2002
*: Purpose   : Show report Text header For the report.
*:*************************************************************
*: Called from : OG read cycle
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfVTextok ()
*:*************************************************************
*
FUNCTION lfVTextok

lcRpPrtTxt = lcRprtText

lcSavSaf = SET('SAFETY')
SET SAFETY OFF
SAVE TO &gcRepHome.hlcMefTxt.MEM ALL LIKE lcRpPrtTxt
SET SAFETY &lcSavSaf

*-- End OF lfVTextok
*:*************************************************************
*: Name      : lfStyle
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/20/2002
*: Purpose   : 
*:*************************************************************
*: Called from : Frx
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfStyle ()
*:*************************************************************
*
FUNCTION lfStyle

***This function is for Grouping the style for each line
lcStyle  = ''
IF lcStyDis = ctsuqtcat+ctsuchwcd+ctsuconcd+ctsugarcd
   RETURN ''
ELSE
  lcStyDis = ctsuqtcat+ctsuchwcd+ctsuconcd+ctsugarcd
  lnLine = IIF(RECNO() > RECCOUNT(),0,RECNO())
  GO TOP
  SCAN FOR lcStyDis = ctsuqtcat+ctsuchwcd+ctsuconcd+ctsugarcd
    lcStyle = ALLTRIM(lcStyle)+IIF(EMPTY(lcStyle),'',' ')+ALLTRIM(cStyNum)
  ENDSCAN 
  IF lnLine > 0
    GO lnLine
  ENDIF  
  RETURN ALLTRIM(lcStyle)
ENDIF

*-- End Of lfStyle.
*:*************************************************************
*: Name      : lfStyTot
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/20/2002
*: Purpose   : 
*:*************************************************************
*: Called from : Frx
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfStyTot ()
*:*************************************************************
*
FUNCTION lfStyTot

***This function is to Calculate the total styles 
lnStyTot = 0
IF lcDis = ctsuqtcat+ctsuchwcd+ctsuconcd+ctsugarcd
   RETURN 0
ELSE
  lcDis = ctsuqtcat+ctsuchwcd+ctsuconcd+ctsugarcd
  lnLine = IIF(RECNO() > RECCOUNT(),0,RECNO())
  SUM ncintlnvl TO lnStyTot FOR lcDis=ctsuqtcat+ctsuchwcd+ctsuconcd+ctsugarcd 
  IF lnLine > 0
    GO lnLine
  ENDIF  
  RETURN lnStyTot
ENDIF

*-- End OF lfStyTot.
*:*************************************************************
*: Name      : lfStyDis
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/20/2002
*: Purpose   : 
*:*************************************************************
*: Called from : Frx
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfStyDis ()
*:*************************************************************
*
FUNCTION lfStyDis

***This function is to print the Style Discription once per group
IF lcDes = ctsuqtcat+ctsuchwcd+ctsuconcd+ctsugarcd 
   RETURN ''
ELSE
  lcDes = ctsuqtcat+ctsuchwcd+ctsuconcd+ctsugarcd
  RETURN (mstydescb)
ENDIF

*-- End of lfStyDis.
*:*************************************************************
*: Name      : lfCut
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/20/2002
*: Purpose   : 
*:*************************************************************
*: Called from : Frx
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfCut ()
*:*************************************************************
*
FUNCTION lfCut

***This function is to print the cut once per group
IF lcCut3 = ctsuqtcat+ctsuchwcd+ctsuconcd+ctsugarcd
  RETURN ''
ELSE
  lcCut3 = ctsuqtcat+ctsuchwcd+ctsuconcd+ctsugarcd  
  DO CASE
    CASE lCutA
      RETURN 'A - CUT'
    CASE lCutB
      RETURN 'B - CUT'
    OTHERWISE
      RETURN ''    
  ENDCASE    
ENDIF

*-- End OF lfCut.
*:*************************************************************
*: Name      : lfSew
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/20/2002
*: Purpose   : 
*:*************************************************************
*: Called from : Frx
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfSew ()
*:*************************************************************
*
FUNCTION lfSew

***This function is to print the Sew once per group
IF lcSew = ctsuqtcat+ctsuchwcd+ctsuconcd+ctsugarcd
  RETURN ''
ELSE
  lcSew = ctsuqtcat+ctsuchwcd+ctsuconcd+ctsugarcd
  DO CASE
    CASE lSewA
      RETURN 'A - Sew & Assemble Garments'
    CASE lSewB
      RETURN 'B - Sew & Assemble Garments'
    OTHERWISE
      RETURN ''    
  ENDCASE    
ENDIF  

*-- End OF lfSew
*:*************************************************************
*: Name      : lfCutCont
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/20/2002
*: Purpose   : 
*:*************************************************************
*: Called from : Frx
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfCutCont ()
*:*************************************************************
*
FUNCTION lfCutCont

***This function is to print the cutting country once per group
lcCutCont=''
IF lcCut2 = ctsuqtcat+ctsuchwcd+ctsuconcd+ctsugarcd
  RETURN ''
ELSE  
  lcCut2 = ctsuqtcat+ctsuchwcd+ctsuconcd+ctsugarcd
  DO CASE
    CASE lCutA
      lcCutCont='A'
      RETURN 'A'
    CASE lCutB
      lcCutCont='B'
      RETURN 'B'
    OTHERWISE
      lcCutCont='None'
      RETURN 'None'    
  ENDCASE    
ENDIF

*-- End OF lfCutCont.
*:*************************************************************
*: Name      : lfSewCont
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/20/2002
*: Purpose   : 
*:*************************************************************
*: Called from : Frx
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfSewCont ()
*:*************************************************************
*
FUNCTION lfSewCont

***This function is to print the Sew Country once per group
lcSewCont=''
IF lcCut = ctsuqtcat+ctsuchwcd+ctsuconcd+ctsugarcd
  RETURN ''
ELSE  
  lcCut = ctsuqtcat+ctsuchwcd+ctsuconcd+ctsugarcd
  IF lCutA = .F. AND lCutB = .F.
    RETURN ''
  ELSE
    DO CASE
      CASE lSewA
        lcSewCont='A'
        RETURN 'A'
      CASE lSewB
        lcSewCont='B'
        RETURN 'B'
      OTHERWISE
        lcSewCont=''
        RETURN ''    
    ENDCASE    
  ENDIF  
ENDIF  

*-- End OF lfSewCont.
*:*************************************************************
*: Name      : lfExportA
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/20/2002
*: Purpose   : 
*:*************************************************************
*: Called from : Frx
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfExportA ()
*:*************************************************************
*
FUNCTION lfExportA

***This function is for Change the the date format to be like Month/Year
lcExport=''
IF lcMonth <> ctsuqtcat+ctsuchwcd+ctsuconcd+ctsugarcd
  lcMonth  = ctsuqtcat+ctsuchwcd+ctsuconcd+ctsugarcd
  lcSavCen = SET('CENT')
  SET CENT ON
  lcExport = SUBSTR(DMY(dExportA),3)
  SET CENT &lcSavCen
ENDIF  
RETURN ALLTRIM(lcExport)

*-- End OF lfExportA.  
*:*************************************************************
*: Name      : lfStyFab
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/20/2002
*: Purpose   : 
*:*************************************************************
*: Called from : Frx
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfStyFab()
*:*************************************************************
*
FUNCTION lfStyFab

***This function is to print the Fabric Content once per group
IF lcFab = ctsuqtcat+ctsuchwcd+ctsuconcd+ctsugarcd
   RETURN ''
ELSE
  lcFab = ctsuqtcat+ctsuchwcd+ctsuconcd+ctsugarcd
  RETURN (ALLTRIM(cstyfabcn))
ENDIF    

*-- End of lfStyFab
*:**************************************************************************
*: Name      : lfCreatemp
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/09/2002
*: Purpose   : Create the temp files.
*:**************************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:**************************************************************************
*: Called from : Report code
*:**************************************************************************
*: Passed Parameters  : ...
*:**************************************************************************
*: Returns            : None.
*:**************************************************************************
*: Example   : = lfCreatemp()
*:**************************************************************************
*
FUNCTION lfCreatemp

IF !USED(lcTemp)
  CREATE Table (lcTemp) (ctsuvencd C(6) , ccinvno C(12) , ctsuqtcat C(8)   ,;
                         ctsuchwcd C(6) , ctsuconcd C(6), ctsugarcd C(6)   ,;
                         lcuta L(1)     , lsewa L(1)    , dexporta D(8)    ,;
                         lcutb L(1)     , lsewb L(1)    , dexportb D(8)    ,;
                         cstynum C(7)   , ncintlnvl N(10,2),mstydescb M(10),;
                         cstyfabcn C(40))
ENDIF

SELECT (lcTemp)
ZAP
INDEX ON ctsuqtcat+ctsuchwcd+ctsuconcd+ctsugarcd TAG (lcTemp) OF (gcWorkDir+lcTemp+'.CDX')


*-- End OF lfCreatemp
*:*************************************************************
*: Name      : lfCollect
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/09/2002
*: Purpose   : Collecting Data In The First Time.
*:*************************************************************
*: Called from : OG read cycle
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfCollect ()
*:*************************************************************
*
FUNCTION lfCollect
*-- lcRpInvce :- Variable Hold invoice number.
*-- lcRpVenVal:- Variable Hold the Vendor.

SELECT Hlcinvln
*-- Check If we found this invoice in the Hlcinvln File.
IF SEEK(lcRpVenVal+lcRpInvce)
  *-- Scan in the hlcInvLn File.
  SCAN REST WHILE ctsuvencd+ccinvno = lcRpVenVal+lcRpInvce
    IF SEEK(Hlcinvln.cstyNum,'hlcStyle')
      *-- Add New Recored In the Temp File.
      SELECT (lcTemp)
      APPEND BLANK
      REPLACE cstynum   WITH Hlcinvln.cstynum  ,;
              ncintlnvl WITH Hlcinvln.ncintlnvl,;
              mstydescb WITH Hlcstyle.mstydescb,;
              cstyfabcn WITH Hlcstyle.cstyfabcn
    
       IF SEEK(lcRpVenVal+lcRpInvce,'HLCMEF')
         SELECT HLCMEF
         SCAN REST WHILE ctsuvencd+ccinvno = lcRpVenVal+lcRpInvce ;
            FOR ctsuchwcd  + ctsuconcd  + ctsugarcd  + ctsuqtcat = ;
                    Hlcstyle.ctsuchwcd + Hlcstyle.ctsuconcd+ Hlcstyle.ctsugarcd +;
                    Hlcstyle.ctsuqtcat
    	 SELECT (lcTemp)
         REPLACE ctsuvencd WITH Hlcmef.ctsuvencd  ,;
                 ccinvno   WITH Hlcmef.ccinvno    ,;
                 ctsuqtcat WITH Hlcmef.ctsuqtcat  ,;
                 ctsuchwcd WITH Hlcmef.ctsuchwcd  ,;
                 ctsuconcd WITH Hlcmef.ctsuconcd  ,;
                 ctsugarcd WITH Hlcmef.ctsugarcd  ,;
                 lcuta     WITH Hlcmef.lcuta      ,;
                 lsewa     WITH Hlcmef.lsewa      ,;
                 dexporta  WITH Hlcmef.dexporta   ,;
                 lcutb     WITH Hlcmef.lcutb      ,;
                 lsewb     WITH Hlcmef.lsewb      ,;
                 dexportb  WITH Hlcmef.dexportb
           SELECT HLCMEF
         ENDSCAN
       ENDIF
    ENDIF
    SELECT Hlcinvln    
  ENDSCAN
ENDIF

*-- End OF lfCollect
*:*************************************************************

