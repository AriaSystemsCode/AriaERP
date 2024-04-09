*!**************************************************************************
*! Name      : STUMAIN.PRG
*! Developer : WAB ( Walid A. Wahab)
*! Date      : 12/13/2003
*! Purpose   : STU10 Custom Process Program . (120733)
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.       
*!**************************************************************************
PARAMETER lcEvntFun,lcFunPars

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLT(lcEvntFun)+'('+lcFunPars+')'

*--Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue

*!**************************************************************************
*! Name      : lfVStates
*! Developer : WAB ( Walid A. Wahab)
*! Date      : 12/13/2003
*! Purpose   : functio nto valid the States button i nth option grid
*!**************************************************************************
*! Parameters:
*!**************************************************************************
*! Returns   :
*!**************************************************************************
FUNCTION lfVStates
*lcStates = ''
IISel = lfVInlist("C","@! XXXXXXXX",8,"lfvOGStates()","E","","lcStates",7)


*!**************************************************************************
*! Name      : lfVStores
*! Developer : WAB ( Walid A. Wahab)
*! Date      : 12/13/2003
*! Purpose   : functio nto valid the Stores button i nth option grid
*!**************************************************************************
*! Parameters:
*!**************************************************************************
*! Returns   :
*!**************************************************************************
FUNCTION lfVStores
lcRpAcc = lfGetAcc()
IF EMPTY(lcRpAcc)
  RETURN 
ENDIF

IISel = lfVInlist("C","@! XXXXXXXX",8,"lfvOgStores()","E","","lcStores",8)

*!**************************************************************************
*! Name      : lfStitle
*! Developer : WAB ( Walid A. Wahab)
*! Date      : 12/13/2003
*! Purpose   : get the state tilte 
*!**************************************************************************
*! Parameters:
*!**************************************************************************
*! Returns   :
*!**************************************************************************
FUNCTION lfStitle
IF !USED('SYCINT')
  = gfOpenFile(gcSysHome+'SYCINT',gcSysHome+'Ccontcode','SH')
ENDIF

SET ORDER TO Ccomp_id  IN SYCCOMP   && To use it to get state title.
SET ORDER TO CCONTCODE IN SYCINT   && To use it to get state title. IN SYCCOMP   && To use it to get state title.
IF SEEK(gcAct_Comp,'SYCCOMP') AND SEEK(SYCCOMP.CCONT_CODE,'SYCINT')
  lcStatTitle = (SYCINT.CPART4LAB)
ENDIF

RETURN lcStatTitle

*!**************************************************************************
*! Name      : lfVInlist
*! Developer : WAB ( Walid A. Wahab)
*! Date      : 12/13/2003
*! Purpose   : call the goInlist Function
*!**************************************************************************
*! Parameters:
*!**************************************************************************
*! Returns   :
*!**************************************************************************
FUNCTION lfVInlist
PARAMETERS LcOgDataTyp, LcOgPicture ,LnOgSize , lcValidFunc ,LcvldEntt ,LcOgVldent ,LcOgVar ,  LnFldLenth   

*-- Paramater that pass to  Program OGINLIST.PRG
*-- 1- LcOgDataTyp  ----- > "C"
*-- 2- LcOgPicture  ----- > field picture (order)
*-- 3- LnOgSize     ----- > 7
*-- 4- LcOgVld      ----- > " lfVorder() .and. lfwOldVal() "
*-- 5- LcvldEntt    ----- > "E"
*-- 6- LcOgVldent   ----- > ""
*-- 7- LcOgVar      ----- >  "lcVal "   variable hold the select order  "000001|000002|000004|000008"
*-- 8- LnFldLenth   ----- >  7



*-- restore memo field to variable.

lcOldValue = LcOgVar
IISel = OgInList(LcOgDataTyp,LcOgPicture,LnOgSize,lcValidFunc,LcvldEntt,LcOgVldent,LcOgVar,LnFldLenth)
*IISel = OgInList("C","@! XXXXXX",6,"lfvOrderNo()","E","","lcSleOrd",7)



*!**************************************************************************
*! Name      : lfvOgStores
*! Developer : WAB ( Walid A. Wahab)
*! Date      : 12/13/2003
*! Purpose   : valid the Store variable
*!**************************************************************************
*! Parameters:
*!**************************************************************************
*! Returns   :
*!**************************************************************************
FUNCTION lfvOgStores
PRIVATE lcItsName , lcItsVal , llObjRet , lnAlias
lnAlias   = SELECT()
SELECT CUSTOMER
lcItsName = SYS(18)               && Varible to hold  the name of the memory variable used to create the current GET field
lcItsVal  = EVALUATE(SYS(18))     && Varible to hold  the value of the current GET field
lcRpAcc   = lfGetAcc()
IF USED('SYCINT')
  SET ORDER TO TAG Ccontcode IN SYCINT
ENDIF

*--IF The user want to Browse or if the Account he entered is not in the file
IF '?' $ lcItsVal .OR. (!EMPTY(lcItsVal) .AND. !SEEK('S' + lcRpAcc + lcItsVal , 'CUSTOMER'))
  XSTORE   = SPACE(08) 
  llObjRet = CusBrowS(lcRpAcc ,.T.)
  lcItsVal = XSTORE
  &lcItsName = lcItsVal
ENDIF

SELECT (lnAlias)


*!**************************************************************************
*! Name      : lfGetAcc
*! Developer : WAB ( Walid A. Wahab)
*! Date      : 12/13/2003
*! Purpose   : get the selected account
*!**************************************************************************
*! Parameters:
*!**************************************************************************
*! Returns   :
*!**************************************************************************
FUNCTION lfGetAcc
lcSelcFile = ''
lcRetVal = ''
lnACCFLT   =  ASUBSCRIPT(laogfxflt,ASCAN(laogfxflt,"ORDHDR.ACCOUNT"),1)
lcSelcFile = laogfxflt[lnACCFLT,6]
IF !USED(lcSelcFile) 
  = gfModalGen('INM00000B00000',.F.,.F.,.F.,' You have to select the Account first')
ELSE
  IF RECCOUNT(lcSelcFile) > 1
    = gfModalGen('INM00000B00000',.F.,.F.,.F.,' You have to select one Account only')
  ELSE
    lcRetVal =  &lcSelcFile..Account
  ENDIF
ENDIF

RETURN lcRetVal

*!**************************************************************************
*! Name      : lfvOGStates
*! Developer : WAB ( Walid A. Wahab)
*! Date      : 12/13/2003
*! Purpose   : validate the states field 
*!**************************************************************************
*! Parameters:
*!**************************************************************************
*! Returns   :
*!**************************************************************************
FUNCTION lfvOGStates
PRIVATE lcFile_Ttl, lcBrfields, lcStateObj , lcStateVal

IF !USED('CODES')
  =gfOpenFile(gcDataDir+'CODES',gcDataDir+'Ccode_no','SH')
ENDIF
lcStateObj = SYS(18)                    && Varible to hold  the name of the memory variable used to create the current GET field
lcStateVal = EVALUATE(SYS(18))  && Varible to hold  the value of the current GET field

*-- Index on cdefcode+cfld_name+ccode_no+cdiscrep+crltd_nam

IF '?' $ lcStateVal .OR. (!EMPTY(lcStateVal) .AND. !SEEK('N'+'STATE     '+PADR(lcStateVal,6),'CODES'))
   lnCurAlias = SELECT(0)
   *-- browse all country codes [begin]
   SELECT CODES
   DECLARE laCodeFld[2]
   lcFile_Ttl = 'State Codes'
   lcBrfields = 'cCode_No :H= "State Code" ,cDiscrep :H="Description" :30'
   IF gfBrows('FOR cfld_name+ccode_no+cdiscrep = ;
     "STATE" AND ;
     cRltField="N"','cCode_No,cDiscrep','laCodeFld')
     lcStateVal = laCodeFld[1]
   ELSE
     lcstateval=""  
   ENDIF
   *-- browse all country codes [end]
   SELECT (lnCurAlias)
ENDIF    
*-- If user type inValid code does not have ? Accept it.
&lcStateObj = lcStateVal
*-- end of lfogvStates.

*!**************************************************************************
*! Name      : lfCrtNExp
*! Developer : WAB ( Walid A. Wahab)
*! Date      : 12/13/2003
*! Purpose   : ....
*!**************************************************************************
*! Parameters:
*!**************************************************************************
*! Returns   :
*!**************************************************************************
*!
FUNCTION lfCrtNExp
PARAMETERS lcOldExp
lcRetVal= ""
FOR lnCount = 1 to OCCURS('|',lcOldExp)
  lcRetVal = lcRetVal  + "'"
  lnAtPos = AT('|',lcOldExp)
  lcRetVal = lcRetVal + SUBSTR(lcOldExp,1,lnAtPos-1)+"'," 
  lcOldExp = SUBSTR(lcOldExp,lnAtPos+1)
ENDFOR
lcRetVal = lcRetVal + "'" + lcOldExp + "'"

RETURN lcRetVal

*!**************************************************************************
*! Name      : lfGetOrdln
*! Developer : Abdou Elgendy [Abd]
*! Date      : 12/29/2003
*! Purpose   : Get the correct lines from the order line file.
*!**************************************************************************
*! Parameters:
*!**************************************************************************
*! Returns   :
*!**************************************************************************
*!
FUNCTION lfGetOrdln
PRIVATE  lnOldAls 

lnOldAls  = SELECT(0)
*-- Add the State Expr.
IF !EMPTY(lcStates)
  lcexptoAdd = lfCrtNExp(lcStates)
  lcForCond2 = lcForCond2 + " AND INLIST(CUSTOMER.CADDRESS4," +lcexptoAdd +")"
ENDIF

*-- Add the Store Expr.
IF !EMPTY(lcStores)
  lcexptoAdd = lfCrtNExp(lcStores)
  lcForCond2 = lcForCond2 + " AND INLIST(CUSTOMER.STORE," +lcexptoAdd +")"
ENDIF

lcForCond2 = lcForCond2 + ' AND CUSTOMER.STATUS = "A"'

SELECT ORDLINE
SET RELATION TO IIF(EMPTY(Store) , 'M' + Account,'S' + Account + Store) INTO CUSTOMER ADDITIVE

SELECT ORDHDR
SCAN &lcScanExpr FOR &lcForCond1
  IF SEEK('O' + Order , lcChildFil)
    SELECT (lcChildFil)
    
    SCAN REST WHILE cordtype+order+STR(lineno,6) = "O"+ORDHDR.ORDER ;
          FOR &lcForCond2
      IF llExclude
        WAIT WINDOW NOWAIT 'Excluding ' + lcWaitStr + &lcWaitStr
        =lfExclRec()
      ELSE  && Scope mode
        WAIT WINDOW NOWAIT 'Selecting ' + lcWaitStr + &lcWaitStr
        SCATTER MEMVAR MEMO
        =lfUpdAloVr()
      ENDIF
    ENDSCAN    && End of SCAN Loop
    SELECT ORDHDR
  ENDIF    && End of IF
ENDSCAN    && End of SCAN Loop

IF llLinCmplt
  SET RELATION OFF INTO ORDLINE
ENDIF  

SELECT ORDLINE
SET RELATION off INTO CUSTOMER

SELECT (lnOldAls)
RETURN
*-- End Of lfGetOrdln

*:**************************************************************************
*:* Name        : lfGtStyles
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/14/2004
*:* Purpose     : Get styles
*:***************************************************************************
*C037824,1  
FUNCTION lfGtStyles
PRIVATE lnSlct,lcTmpSty,lcBrowFlds,lcSvOrdr

lnSlct = ALIAS()
lcTmpSty = '_'+SUBSTR(lcTmpOrdLn,2)

IF !USED(lcTmpSty)
  CREATE CURSOR (lcTmpSty) (STYLE C(19))
  INDEX ON STYLE TAG (lcTmpSty) OF (gcWorkDir+lcTmpSty+'.CDX')
ENDIF  

lcBrowFlds = [Style :R :25 :H= 'Style#' ,]+;
             [DESC  :R     :H= 'Description' ,]+;
             [SEASON :R :H= 'Season' ,]+; 
             [CDIVISION :R :H= 'Division' ,]+;
             [PRICEA :R :H= 'Price' ,]+;
             [TotWip :R :H = 'WIP' ,]+;
             [TotStk :R :H= 'Stock' ,]+;
             [TotOrd :R :H= 'Orders' ,]+;
             [FABRIC :R :H= 'Fabric']

SELECT STYLE
lcSvOrdr = ORDER()
SET ORDER TO STYLE
LOCATE

PUSH KEY 
ON KEY
llRturnUsr =gfRange(lcBrowFlds,lcTmpSty,"STYLE","","","","@! XXXXXXXXXXXXXXXXXXX")
POP KEY

   
SET ORDER TO &lcSvOrdr IN STYLE   
SELECT (lnSlct)

*-- end of lfGtStyles.

*:**************************************************************************
*:* Name        : lfFltOnSty
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/14/2004
*:* Purpose     : Filter on selected styles
*:***************************************************************************
*C037824,1  
FUNCTION lfFltOnSty
PRIVATE lnSlct,lcTmpSty,lnRecno,lcSvOrd

lnSlct = ALIAS()
lcTmpSty = '_'+SUBSTR(lcTmpOrdLn,2)

SELECT &lcTmpOrdLn
lnRecno = RECNO()
lcSvOrd = ORDER()
SET ORDER TO &LCTMSTYTAG

IF USED(lcTmpSty) 
  
  *- After collecting data in the main temp file lcTmpOrdLn , remove styles not found in the customized
  *- temp file created for STU10
  IF RECCOUNT(lcTmpSty) > 0
    SELECT &lcTmpOrdLn
    SCAN
      IF !SEEK(STYLE,lcTmpSty)
        DELETE
      ENDIF
    ENDSCAN

    *- Remove the definition of ESC key due to a non-obvious error
    ON KEY LABEL ESC llDumy = .T.
    
  ENDIF

  SELECT &lcTmpSty
  ZAP

ENDIF  
  
SELECT &lcTmpOrdLn
SET ORDER TO &lcSvOrd
LOCATE
llSelData = FOUND()

SELECT (lnSlct)
*-- end of lfFltOnSty.