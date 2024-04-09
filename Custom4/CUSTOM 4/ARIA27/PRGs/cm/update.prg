* -------------------------------------------------------------

SET CLASSLIB TO Comm ADDITIVE
PUBLIC oUpdate
oUpdate = CREATEOBJECT("Update")

IF TYPE("oUpdate") = "O" AND oUpdate.lAutomatic
  oUpdate.StartUpdate()
ENDIF

RELEASE oUpdate
RELEASE CLASSLIB Comm

*!*************************************************************
*! Name      : gfGetMemVar
*! Developer : Hesham El-Sheltawi
*! Date      : 10/05/95
*! Purpose   : Return Variable(s) for company settings from SYCSETUP
*!*************************************************************
*! Parameters: lcArray   && variable to restore
*!                       && OR one dimension array to restore the variable(s)
*!                       name with the same variable name
*!                       && OR two dimension array to restore the variable(2)
*!                       in column 1 into variable names in column 2
*!             lcCompID  &&company id to get its settings
*!*************************************************************
*! Called by : 
*!*************************************************************
*! Returns            : VALUE OF VARIABLE OR no of variables restored
*!*************************************************************
*! Example   : lcVarName=gfGetMemVar('LLMULCURR ','01')
*!             WILL return from the sycsetup file the setting
*!             value for company 01 the variable called "LLMULCURR "
*!*************************************************************
*
FUNCTION gfGetMemVar
PARAMETERS lcArray,oFileObject 
PRIVATE lnAliasNo,llCUsedBy,llArrayORvar,llTwoDimen,lcSetupTag,;
        lcConfgTag,lnRetCount,lcOnErr,llError,laVarArr,llUseSycC,gcSysHome
*B601818,1  Get company path
PRIVATE lcCompDir, lcSetPath, llReUsedBy, lnCurTag, lnCurRec
*B601818,1  end

lnAliasNo=SELECT()
llUsedBy  = .F.
llCUsedBy = .F.
llSUsedBy = .F.
llUseSycC = .F.

lcCompDir  = oFileObject.CurrDataPath
gcSysHome  = oFileObject.cAriaPath
llReUsedBy = .F.

llArrayORvar = TYPE('lcArray[1]')='C'
llTwoDimen = IIF(llArrayORvar AND ALEN(lcArray,2)=2,IIF(TYPE('lcArray[1,2]')='L','A','N'),'V' )
IF !llArrayORvar AND ',' $ lcArray
   DIMENSION laVarArr[1]
   =gfSubStr(lcArray,@laVarArr)
   DIMENSION lcArray[ALEN(laVarArr)]
   =ACOPY(laVarArr,lcArray)
   llArrayORvar = .T.
   llTwoDimen = 'V'
*ELSE
*  lcArray = UPPER(PADR(LCARRAY,10))   
ENDIF
IF !USED('SETUPS')
  SELECT 0
  *B601818,1 use SETUPS from the company directory
  *USE (gcDataDir+'SETUPS')
  USE (lcCompDir+'SETUPS') AGAIN
  *B601818,1 end
  llSUsedBy = .T.
ELSE
  SELECT SETUPS
  *B601818,1 Check if the file is opened from the company path
  lcSetPath = SET('FULLPATH')
  SET FULLPATH ON
  lcSetupsDir = DBF()
  SET FULLPATH &lcSetPath
  IF !(lcCompDir) $ lcSetupsDir
    lnCurTag  = VAL(SYS(21))
    lnCurRec  = RECNO()
    USE (lcCompDir+'SETUPS') AGAIN
    llReUsedBy = .T.
  ENDIF  
  *B601818,1 end  
ENDIF
lcSetupTag =TAG()
SET ORDER TO TAG VARNAME
lcRetVal=''
lnRetCount=0
lcOnErr=ON('ERROR')
ON ERROR llError = .T.
IF !llArrayORvar
  IF SEEK(PADR(UPPER(lcArray),10))
    DO CASE
      CASE cDefa_Typ='V'
        lcRetVal = lfTrnsStr(STRTRAN(mData_Def,CHR(13)+CHR(10),''),cData_Typ)
      CASE cDefa_Typ='E'
       lcRetVal=EVAL(STRTRAN(mData_Def,CHR(13)+CHR(10),''))
    ENDCASE
  ELSE
    llUseSycC = .T.
    IF !USED('SYCCONFG')
      SELECT 0
      USE (gcSysHome+'SYCCONFG')
      llCUsedBy = .T.
    ELSE
      SELECT SYCCONFG
    ENDIF
    lcConfgTag=TAG()
    SET ORDER TO TAG VARNAME

    IF SEEK(PADR(UPPER(lcArray),10))
      DO CASE
        CASE cDefa_Typ='V'
          lcRetVal=lfTrnsStr(STRTRAN(mData_Def,CHR(13)+CHR(10),''),cData_Typ)
        CASE cDefa_Typ='E'
         lcRetVal=EVAL(STRTRAN(mData_Def,CHR(13)+CHR(10),''))
      ENDCASE    
    ENDIF
  ENDIF
ELSE
  llUseSycC = .T.
  IF !USED('SYCCONFG')
    SELECT 0
    USE (gcSysHome+'SYCCONFG')
    llCUsedBy = .T.
  ELSE
    SELECT SYCCONFG
  ENDIF
  lcConfgTag=TAG()
  SET ORDER TO TAG VARNAME

  FOR lnCount = 1 TO ALEN(lcArray,1)
    llError = .F.
    lcRetVal=''
    SELECT SETUPS
    IF SEEK(PADR(UPPER(IIF(llTwoDimen = 'V',lcArray[lnCount],lcArray[lnCount,1])),10))
      DO CASE
        CASE cDefa_Typ='V'
          lcRetVal=lfTrnsStr(STRTRAN(mData_Def,CHR(13)+CHR(10),''),cData_Typ)
        CASE cDefa_Typ='E'
         lcRetVal=EVAL(STRTRAN(mData_Def,CHR(13)+CHR(10),''))
      ENDCASE
    ELSE
      SELECT SYCCONFG
      IF SEEK(PADR(UPPER(IIF(llTwoDimen = 'V',lcArray[lnCount],lcArray[lnCount,1])),10))
        DO CASE
          CASE cDefa_Typ='V'
            lcRetVal=lfTrnsStr(STRTRAN(mData_Def,CHR(13)+CHR(10),''),cData_Typ)
          CASE cDefa_Typ='E'
           lcRetVal=EVAL(STRTRAN(mData_Def,CHR(13)+CHR(10),''))
        ENDCASE    
      ENDIF
    ENDIF  
    DO CASE
      CASE llTwoDimen = 'N'
        &lcArray[lnCount,2] = lcRetVal
      CASE llTwoDimen = 'V'
        &lcArray[lnCount] = lcRetVal
      CASE llTwoDimen = 'A'
        lcArray[lnCount,2] = lcRetVal
    ENDCASE
    lnRetCount=lnRetCount+IIF(!llError,1,0)    
  ENDFOR
ENDIF
ON ERROR &lcOnErr
IF llUseSycC
  SELECT SYCCONFG
  IF !EMPTY(lcConfgTag)
    SET ORDER TO TAG (lcConfgTag)
  ELSE
    SET ORDER TO
  ENDIF
  IF llcUsedBy
    USE IN SYCCONFG
  ENDIF
ENDIF  

SELECT SETUPS
IF !EMPTY(lcSetupTag)
  SET ORDER TO TAG (lcSetupTag)
ELSE
  SET ORDER TO
ENDIF
IF llSUsedBy
  USE IN SETUPS
ENDIF

*B601818,1 ReUse SETUPS file
IF llReUsedBy .AND. !EMPTY(lcSetupsDir)
  SELECT SETUPS
  USE (lcSetupsDir) ORDER lnCurTag
  IF BETWEEN(lnCurRec, 1, RECCOUNT())
    GO lnCurRec
  ELSE
    GO TOP
  ENDIF  
ENDIF  
*B601818,1 end  

SELECT (lnAliasNo)
RETURN IIF(!llArrayORvar,lcRetVal,lnRetCount)

*!********************************************************************
*!
*!              Function: lfTrnsStr
*!
*!********************************************************************
*
FUNCTION lfTrnsStr
PARAMETERS lcValueStr,lcDataType,lcDirection
DO CASE
  CASE lcDataType $ 'CM'
     RETURN ALLT(lcValueStr)
  CASE lcDataType = 'N'
      RETURN VAL(lcValueStr)
  CASE lcDataType='D'
     RETURN CTOD(lcValueStr)
  CASE lcDataType = 'L'
     RETURN IIF(UPPER(ALLTRIM(lcValueStr))='.F.',.F.,.T.)
ENDCASE

*!*************************************************************
*! Name      : gfSubStr
*! Developer : Yasser Saad Ibrahime
*! Date      : 1993-1995 
*! Purpose   : To extract element from string or to convert string to array
*!*************************************************************
*! Calls     : 
*!      Called by: ARIA3.PRG                
*!      Called by: GFSETUP()                (function  in ARIA3.PRG)
*!      Called by: GFSCRINI()               (function  in ARIA3.PRG)
*!      Called by: GFMODALGEN()             (function  in ARIA3.PRG)
*!      Called by: GFSEEKREC()              (function  in ARIA3.PRG)
*!      Called by: GFDBFFIELD()             (function  in ARIA3.PRG)
*!      Called by: GFFLOCK()                (function  in ARIA3.PRG)
*!      Called by: GFRLOCK()                (function  in ARIA3.PRG)
*!      Called by: GFWAIT()                 (function  in ARIA3.PRG)
*!      Called by: GFGETVLD()               (function  in ARIA3.PRG)
*!*************************************************************
*! Passed Parameters  : String to be used
*!                      poiter to array or element position
*!                      sparators used in the string
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : 
*!*************************************************************
* This function will return eather a string part # OR an array of all
* the string parts according to the type of the second parameter. The
* firest parameter will be the string or string variable. If the
* second parameter have a numeric type, the function will return the
* but if it is an array the function will return the array with each
*  element having a part from the string.
* 
*:->
FUNCTION gfSubStr
PARAMETERS lcString,lnAryOrPos,lcSepta

lcSubstr  =' '
lnAryDim  = 1
lnAryRows = 1
lnAryCols = 1
lcSepta   = IIF(TYPE('lcSepta')='C',lcSepta,',') 

IF LEN(ALLTRIM(lcSepta))>1
  lcColSep  = SUBSTR(lcSepta,2,1)
  lcSepta   = LEFT(lcSepta,1)
  lnAryDim  = IIF(OCCURS(lcSepta,lcString)>0,;
              OCCURS(lcSepta,lcString)+;
              IIF(RIGHT(lcString,1)<>lcSepta,1,0),;
              lnAryDim)
  lnAryCols = IIF(OCCURS(lcColSep,lcString)>0,;
              OCCURS(lcColSep,lcString)+;
              IIF(RIGHT(lcString,1)<>lcColSep,1,0),;
              lnAryDim)
  lnAryRows = (lnAryDim+(lnAryCols-1)) / lnAryCols
  lnAryDim  = lnAryDim +(lnAryCols-1)     
  lcString  = STRTRAN(lcString,lcColSep,lcSepta)
ELSE
  lnAryDim = IIF(OCCURS(lcSepta,lcString)>0,;
             OCCURS(lcSepta,lcString)+;
             IIF(RIGHT(lcString,1)<>lcSepta,1,0),;
             lnAryDim)
ENDIF

*** Chek if second parameter array or numeric
DO CASE
  *** If no parameter found assume firest part of string
  CASE TYPE ('lnAryOrPos')='U'
    lnAryOrPos = 1

  *** If array strich it to hold all string parts
  CASE TYPE ('lnAryOrPos') $ 'C,L'    
    IF lnAryCols > 1
      DIMENSION lnAryOrPos[lnAryRows,lnAryCols]
    ELSE
      IF ALEN(lnAryOrPos,2) > 0
        DIMENSION lnAryOrPos[lnAryDim,ALEN(lnAryOrPos,2)]
      ELSE
        DIMENSION lnAryOrPos[lnAryDim]
      ENDIF  

    ENDIF
    lnAryOrPos  = ' '

ENDCASE

FOR lnArElem  = 1 TO lnAryDim
  IF TYPE ('lnAryOrPos')='N'
    lnArElem = lnAryOrPos
  ENDIF  

  DO CASE
    *** In case of firest string part
    CASE lnArElem = 1
      lcSubstr = SUBSTR(lcString,1,;
      IIF(lcSepta $ lcString,AT(lcSepta,lcString)-1,LEN(lcString)))

    *** In case of last string part
    CASE lnArElem = lnAryDim
      lcSubstr = SUBSTR(lcString,AT(lcSepta,lcString,lnArElem-1)+1)
      lcSubstr = IIF(RIGHT(lcSubstr,1)=lcSepta,;
                 SUBSTR(lcSubstr,1,LEN(lcSubstr)-1),lcSubstr)
    *** In case of any string part from the meddel
    CASE lnArElem > 1
      lcSubstr = SUBSTR(lcString,AT(lcSepta,lcString,lnArElem-1)+1,;
                 AT(lcSepta,lcString,lnArElem)-;
                 AT(lcSepta,lcString,lnArElem-1)-1)
  ENDCAS

  IF TYPE ('lnAryOrPos')='N'
    RETURN lcSubstr
  ENDIF  
  
  IF lnAryCols > 1
    lnAryOrPos[((lnArElem-1)%lnAryRows)+1,INT((lnArElem-1)/lnAryRows)+1] = lcSubstr
  ELSE
    lnAryOrPos[lnArElem] = lcSubstr
  ENDIF
ENDFOR

******************************************************************
*-- YMA (Start)
FUNCTION LFUPDPOSH
LPARAMETERS lcAction, oUpdObj
PRIVATE lnCurWorkAlias, lcMastAlias, lcTempAlias

lnCurWorkAlias = SELECT(0)
lcMastAlias    = oUpdObj.CurrMastAlias
lcTempAlias    = oUpdObj.CurrTmprAlias

IF lcAction = 'P'
  IF SEEK(oUpdObj.MasterFileExpValue, lcMastAlias)
    SELECT (lcTempAlias)
    REPLACE Status WITH &lcMastAlias..Status
    SELECT(lnCurWorkAlias)
  ENDIF
ENDIF
*-- YMA (End)
******************************************************************

FUNCTION LFUPPOS
LPARAMETERS lcAction, oUpdObj
PRIVATE lnAlias,lcAlias,lcSysPath,lcDataPath,lcMasterAlias

lnAlias       = SELECT(0)
lcSysPath     = oUpdObj.cAriaPath
lcDataPath    = oUpdObj.CurrDataPath
lcMasterAlias = oUpdObj.CurrMastAlias
lcAlias       = oUpdObj.CurrTmprAlias

SELECT (lcAlias)
DO CASE
  CASE lcAction = 'I'
    USE (lcDataPath+'POSHDR') IN 0 ORDER TAG POSHDR
    *-- YMA (start)
    USE (lcDataPath+'WAREHOUS') IN 0 ORDER TAG WareHous AGAIN ALIAS WareH
    *-- YMA (end)

  CASE lcAction = 'T'
    *-- YMA (start)
    USE IN WareH
    *-- YMA (end)
    USE IN POSHDR

  CASE lcAction = 'P'
    IF SEEK(cstytype+po,'POSHDR')
      SELECT POSHDR
      DO CASE
        CASE &lcAlias.._STATUS = 'M' AND &lcAlias..TRANCD = '1' 
            REPLACE nICost1   WITH nICost1 + (&lcAlias..totqty*&lcAlias..necost1) - (&lcMasterAlias..totqty*&lcMasterAlias..necost1),;
                    nICost2   WITH nICost2 + (&lcAlias..totqty*&lcAlias..necost2) - (&lcMasterAlias..totqty*&lcMasterAlias..necost2),;
                    nICost3   WITH nICost3 + (&lcAlias..totqty*&lcAlias..necost3) - (&lcMasterAlias..totqty*&lcMasterAlias..necost3),;
                    nICost4   WITH nICost4 + (&lcAlias..totqty*&lcAlias..necost4) - (&lcMasterAlias..totqty*&lcMasterAlias..necost4),;
                    nICost5   WITH nICost5 + (&lcAlias..totqty*&lcAlias..necost5) - (&lcMasterAlias..totqty*&lcMasterAlias..necost5),;
                    nfCost1   WITH nfCost1 + (&lcAlias..totqty*&lcAlias..ncost1) - (&lcMasterAlias..totqty*&lcMasterAlias..ncost1),;
                    nfCost2   WITH nfCost2 + (&lcAlias..totqty*&lcAlias..ncost2) - (&lcMasterAlias..totqty*&lcMasterAlias..ncost2),;
                    nfCost3   WITH nfCost3 + (&lcAlias..totqty*&lcAlias..ncost3) - (&lcMasterAlias..totqty*&lcMasterAlias..ncost3),;
                    nfCost4   WITH nfCost4 + (&lcAlias..totqty*&lcAlias..ncost4) - (&lcMasterAlias..totqty*&lcMasterAlias..ncost4),;
                    nfCost5   WITH nfCost5 + (&lcAlias..totqty*&lcAlias..ncost5) - (&lcMasterAlias..totqty*&lcMasterAlias..ncost5),;
                    OPEN      WITH OPEN + (&lcAlias..totqty) - (&lcMasterAlias..totqty),;
                    NSTYORDER WITH NSTYORDER + &lcAlias..totqty - &lcMasterAlias..totqty,;
                    POTOTAL   WITH nICost1+nICost2+nICost3+nICost4+nICost5 ,;
                    TOTORD    WITH TOTORD + (&lcAlias..totord) - (&lcMasterAlias..totord),;
                    Status    WITH IIF(Open=0,IIF(Receive>0,'C','X'),Status)

        CASE &lcAlias.._STATUS = 'A'
          DO CASE
            CASE &lcAlias..TRANCD = '1'
              REPLACE nICost1 WITH nICost1 + (&lcAlias..totqty*&lcAlias..necost1) ,;
                      nICost2 WITH nICost2 + (&lcAlias..totqty*&lcAlias..necost2),;
                      nICost3 WITH nICost3 + (&lcAlias..totqty*&lcAlias..necost3),;
                      nICost4 WITH nICost4 + (&lcAlias..totqty*&lcAlias..necost4),;
                      nICost5 WITH nICost5 + (&lcAlias..totqty*&lcAlias..necost5),;
                      nfCost1 WITH nfCost1 + (&lcAlias..totqty*&lcAlias..ncost1),;
                      nfCost2 WITH nfCost2 + (&lcAlias..totqty*&lcAlias..ncost2),;
                      nfCost3 WITH nfCost3 + (&lcAlias..totqty*&lcAlias..ncost3),;
                      nfCost4 WITH nfCost4 + (&lcAlias..totqty*&lcAlias..ncost4),;
                      nfCost5 WITH nfCost5 + (&lcAlias..totqty*&lcAlias..ncost5),;
                      OPEN WITH OPEN + &lcAlias..totqty,;
                      NSTYORDER WITH NSTYORDER + &lcAlias..totqty,;
                      POTOTAL WITH nICost1+nICost2+nICost3+nICost4+nICost5,;
                      TOTORD WITH TOTORD + &lcAlias..totord,;
                      LastLine WITH LastLine+1
             CASE &lcAlias..TRANCD = '2'          
                REPLACE Receive WITH Receive + &lcAlias..totqty,;
                        OPEN    WITH IIF(open - &lcAlias..totqty<0,0,open - &lcAlias..totqty) ,;
                        Status  WITH IIF(Open=0,IIF(Receive>0,'C','X'),Status)
                *-- YMA
                REPLACE nFlanCost1 WITH nFlanCost1 +(&lcAlias..nlan_cst1  * &lcAlias..TotQty),;
                        nFlanCost2 WITH nFlanCost2 +(&lcAlias..nlan_cst2  * &lcAlias..TotQty),;
                        nFlanCost3 WITH nFlanCost3 +(&lcAlias..nlan_cst3  * &lcAlias..TotQty),;
                        nFlanCost4 WITH nFlanCost4 +(&lcAlias..nlan_cst4  * &lcAlias..TotQty),;
                        nFlanCost5 WITH nFlanCost5 +(&lcAlias..nlan_cst5  * &lcAlias..TotQty),;
                        nlan_cost1 WITH nlan_cost1 +(&lcAlias..nElanCost1 * &lcAlias..TotQty),;
                        nlan_cost2 WITH nlan_cost2 +(&lcAlias..nElanCost2 * &lcAlias..TotQty),;
                        nlan_cost3 WITH nlan_cost3 +(&lcAlias..nElanCost3 * &lcAlias..TotQty),;
                        nlan_cost4 WITH nlan_cost4 +(&lcAlias..nElanCost4 * &lcAlias..TotQty),;
                        nlan_cost5 WITH nlan_cost5 +(&lcAlias..nElanCost5 * &lcAlias..TotQty),;
                        nTot_Cost  WITH nlan_cost1+nlan_cost2+nlan_cost3+nlan_cost4+nlan_cost5
                *-- YMA

             CASE &lcAlias..TRANCD = '4' 
                REPLACE DAMAGE WITH DAMAGE + &lcAlias..totqty
                *-- YMA
                REPLACE nFlanCost1 WITH nFlanCost1 +(&lcAlias..nlan_cst1  * &lcAlias..TotQty),;
                        nFlanCost2 WITH nFlanCost2 +(&lcAlias..nlan_cst2  * &lcAlias..TotQty),;
                        nFlanCost3 WITH nFlanCost3 +(&lcAlias..nlan_cst3  * &lcAlias..TotQty),;
                        nFlanCost4 WITH nFlanCost4 +(&lcAlias..nlan_cst4  * &lcAlias..TotQty),;
                        nFlanCost5 WITH nFlanCost5 +(&lcAlias..nlan_cst5  * &lcAlias..TotQty),;
                        nlan_cost1 WITH nlan_cost1 +(&lcAlias..nElanCost1 * &lcAlias..TotQty),;
                        nlan_cost2 WITH nlan_cost2 +(&lcAlias..nElanCost2 * &lcAlias..TotQty),;
                        nlan_cost3 WITH nlan_cost3 +(&lcAlias..nElanCost3 * &lcAlias..TotQty),;
                        nlan_cost4 WITH nlan_cost4 +(&lcAlias..nElanCost4 * &lcAlias..TotQty),;
                        nlan_cost5 WITH nlan_cost5 +(&lcAlias..nElanCost5 * &lcAlias..TotQty),;
                        nTot_Cost  WITH nlan_cost1+nlan_cost2+nlan_cost3+nlan_cost4+nlan_cost5
                *-- YMA

             CASE &lcAlias..TRANCD = '5' 
                REPLACE CANCEL WITH CANCEL + &lcAlias..totqty             
           ENDCASE  
        CASE &lcAlias.._STATUS = 'D'        
          REPLACE nICost1 WITH nICost1 - (&lcMasterAlias..totqty*&lcMasterAlias..necost1) ,;
                  nICost2 WITH nICost2 - (&lcMasterAlias..totqty*&lcMasterAlias..necost2),;
                  nICost3 WITH nICost3 - (&lcMasterAlias..totqty*&lcMasterAlias..necost3),;
                  nICost4 WITH nICost4 - (&lcMasterAlias..totqty*&lcMasterAlias..necost4),;
                  nICost5 WITH nICost5 - (&lcMasterAlias..totqty*&lcMasterAlias..necost5),;
                  nfCost1 WITH nfCost1 - (&lcMasterAlias..totqty*&lcMasterAlias..ncost1),;
                  nfCost2 WITH nfCost2 - (&lcMasterAlias..totqty*&lcMasterAlias..ncost2),;
                  nfCost3 WITH nfCost3 - (&lcMasterAlias..totqty*&lcMasterAlias..ncost3),;
                  nfCost4 WITH nfCost4 - (&lcMasterAlias..totqty*&lcMasterAlias..ncost4),;
                  nfCost5 WITH nfCost5 - (&lcMasterAlias..totqty*&lcMasterAlias..ncost5),;
                  OPEN WITH OPEN -  &lcMasterAlias..totqty,;
                  NSTYORDER WITH NSTYORDER - &lcMasterAlias..totqty,;
                  POTOTAL WITH nICost1+nICost2+nICost3+nICost4+nICost5 ,;
                  Status WITH IIF(Open=0,IIF(Receive>0,'C','X'),Status)
      ENDCASE
      
      *-- YMA If the Source/destination warehouse site is not this site, AND
      *--     the site that owns the record is not the Source/destingation site
      *--     Add record in the trace file
      IF oUpdObj.CurSiteType = "B"
        lcCurSite      = oUpdObj.CurSite
        lcDestWareCode = &lcAlias..cWareCode
        lcSorcWareCode = LEFT(&lcAlias..Vendor,6)
        lcSourceSite   = IIF(SEEK(lcSorcWareCode, "WareH"), WareH.cSiteID, SPACE(10))
        lcDestSite     = IIF(SEEK(lcDestWareCode, "WareH"), WareH.cSiteID, SPACE(10))
        lcOwnerSite    = PADR(UPPER(ALLTRIM(&lcAlias..COWNER)),6)

        IF lcSourceSite # lcCurSite AND lcOwnerSite # lcSourceSite
          = gfTraceKey('POSHDR',POSHDR.cstytype+POSHDR.po,"A",lcDataPath,"CM")
          *= gfTraceKey('POSLN',&lcAlias..cstytype+&lcAlias..po+;
                       &lcAlias..style+STR(&lcAlias..lineno,6)+;
                       &lcAlias..trancd,&lcAlias.._STATUS,lcDataPath,"CM")
          = gfTraceKey('POSLN',&lcAlias..cStyType+&lcAlias..Po+;
                       &lcAlias..cRsession+&lcAlias..Shipno+;
                       &lcAlias..Style+STR(&lcAlias..Lineno,6)+;
                       &lcAlias..Trancd,&lcAlias.._STATUS,lcDataPath,"CM")
          = gfTraceKey('STYLE',&lcAlias..style,"A",lcDataPath,"CM")
          = gfTraceKey('STYDYE',&lcAlias..style+&lcAlias..cWareCode,"A",lcDataPath,"CM")
        ENDIF

        IF lcDestSite # lcCurSite AND lcOwnerSite # lcDestSite
          = gfTraceKey('POSHDR',POSHDR.cstytype+POSHDR.po,"A",lcDataPath,"CM")
          *= gfTraceKey('POSLN',&lcAlias..cstytype+&lcAlias..po+;
                       &lcAlias..style+STR(&lcAlias..lineno,6)+;
                       &lcAlias..trancd,&lcAlias.._STATUS,lcDataPath,"CM")
          = gfTraceKey('POSLN',&lcAlias..cStyType+&lcAlias..Po+;
                       &lcAlias..cRsession+&lcAlias..Shipno+;
                       &lcAlias..Style+STR(&lcAlias..Lineno,6)+;
                       &lcAlias..Trancd,&lcAlias.._STATUS,lcDataPath,"CM")
          = gfTraceKey('STYLE',&lcAlias..style,"A",lcDataPath,"CM")
          = gfTraceKey('STYDYE',&lcAlias..style+&lcAlias..cWareCode,"A",lcDataPath,"CM")
        ENDIF
      ENDIF
      *-- YMA (end)
    
    ENDIF
ENDCASE  

SELECT (lnAlias)

* -------------------------------------------------------------

FUNCTION LFUPDCOMP
LPARAMETERS lcAction, oUpdObj
PRIVATE lnAlias, lcTmpAlias, lcTmpCurs, lcCurVal, lcCurMod, lcNewValue

IF ALLTRIM(UPPER(lcAction)) = "T"
  lnAlias    = SELECT(0)
  lcTmpAlias = oUpdObj.CurrTmprAlias
  lcSysPath  = oUpdObj.cAriaPath

  USE (lcSysPath+"SYCCOMP") IN 0 AGAIN ALIAS CompFile ORDER CCOMP_ID
  SELECT CompFile
  IF SEEK(oUpdObj.ToComp)
    SELECT (lcTmpAlias)
    lnTmpRecNo = RECNO()

    lcTmpCurs = "X" + SUBSTR(SYS(2015),4)
    SELECT DISTINCT cApp_ID FROM (lcTmpAlias) INTO CURSOR (lcTmpCurs)
    SELECT (lcTmpCurs)
    SCAN
      lcCurVal = ALLTRIM(UPPER(CompFile.mModlSet))
      lcCurMod = UPPER(ALLTRIM(cApp_ID))
      IF !(lcCurMod $ lcCurVal)
        lcNewValue = lcCurVal + IIF(EMPTY(lcCurVal), "", ",") + lcCurMod
        SELECT CompFile
        REPLACE mModlSet WITH UPPER(ALLTRIM(lcNewValue))
      ENDIF
    ENDSCAN
    USE IN (lcTmpCurs)

    SELECT (lcTmpAlias)
    IF lnTmpRecNo <= RECCOUNT() AND lnTmpRecNo > 0
      GOTO lnTmpRecNo
    ENDIF
  ENDIF
  USE IN CompFile
  SELECT(lnAlias)
ENDIF  

* -------------------------------------------------------------

FUNCTION LFUPORDER 
LPARAMETERS lcAction,oUpdObj
lnAlias = SELECT()
lcSysPath     = oUpdObj.cAriaPath
lcDataPath    = oUpdObj.CurrDataPath
lcMasterAlias = oUpdObj.CurrMastAlias
lcOrdLine     = oUpdObj.CurrTmprAlias

DO CASE
  CASE lcAction = 'I'
    USE (lcDataPath+'ORDHDR') IN 0 ORDER TAG ORDHDR
  CASE lcAction = 'T'
    USE IN ORDHDR
  CASE lcAction = 'P'

    IF SEEK(&lcOrdLine..cordtype+&lcOrdLine..order,'ORDHDR')
      llUpdate = (ORDHDR.STATUS='B') .OR. _Status = 'A' OR ;
                 (ORDHDR.dAdd_Date <= ORDHDR.ENTERED + gfgetmemvar('M_CANAFTER'))
      SELECT ORDHDR    
      DO CASE
        CASE &lcOrdLine.._Status = 'A'

          REPLACE BOOK      WITH BOOK     + &lcOrdLine..TotBook ,;
                  BOOKAMT   WITH BOOKAMT  + (&lcOrdLine..TotBook*&lcOrdLine..Price) ,;
                  OPEN      WITH OPEN     + &lcOrdLine..TOTQTY ,;
                  OPENAMT   WITH OPENAMT  + (&lcOrdLine..TOTQTY*&lcOrdLine..PRICE),;
                  LastLine  WITH LastLine +1
        
        CASE &lcOrdLine.._Status = 'D'
          REPLACE BOOK      WITH BOOK    - IIF(llUpdate,&lcMasterAlias..TOTQTY,0) ,;
                  BOOKAMT   WITH BOOKAMT - IIF(llUpdate,&lcMasterAlias..TOTQTY*&lcMasterAlias..PRICE,0) ,;
                  CANCEL    WITH CANCEL  + IIF(llUpdate,0,&lcMasterAlias..TOTQTY),;
                  CANCELAMT WITH CANCELAMT+IIF(llUpdate,0,&lcMasterAlias..TOTQTY*&lcMasterAlias..PRICE) ,;
                  OPEN      WITH OPEN    - &lcMasterAlias..TOTQTY ,;
                  OPENAMT   WITH OPENAMT - (&lcMasterAlias..TOTQTY*&lcMasterAlias..PRICE)

        CASE &lcOrdLine.._Status = 'M'
          REPLACE BOOK      WITH BOOK    - IIF(llUpdate,&lcMasterAlias..TOTQTY-&lcOrdLine..TOTQTY,0) ,;
                  BOOKAMT   WITH BOOKAMT - IIF(llUpdate,&lcMasterAlias..TOTQTY*&lcMasterAlias..PRICE-&lcOrdLine..TOTQTY*&lcOrdLine..PRICE,0) ,;
                  CANCEL    WITH CANCEL  + IIF(llUpdate,0,MAX(&lcMasterAlias..TOTQTY- &lcOrdLine..TOTQTY,0)),;
                  CANCELAMT WITH CANCELAMT+IIF(llUpdate,0,MAX(&lcMasterAlias..TOTQTY*&lcMasterAlias..PRICE-&lcOrdLine..TOTQTY*&lcOrdLine..PRICE,0)) ,;
                  OPEN      WITH OPEN    - &lcMasterAlias..TOTQTY + &lcOrdLine..TOTQTY,;
                  OPENAMT   WITH OPENAMT - (&lcMasterAlias..TOTQTY*&lcMasterAlias..PRICE)+(&lcOrdLine..TOTQTY*&lcOrdLine..PRICE)
        
      ENDCASE        
    ENDIF          
  
ENDCASE  
SELECT (lnAlias)

* -------------------------------------------------------------

FUNCTION LFUPCODES
LPARAMETERS lcAction,oUpdObj

lnAlias       = SELECT(0)
lcSysPath     = oUpdObj.cAriaPath
lcDataPath    = oUpdObj.CurrDataPath
lcMasterAlias = oUpdObj.CurrMastAlias
lcCodes       = oUpdObj.CurrTmprAlias
lcCompID      = oUpdObj.laToUpd[lnCurStcm,3]

DO CASE
  CASE lcAction = 'I'
  *--man
*    SELECT (lcCodes)
*    REPLACE ALL CCOMP_ID WITH lcCompID ,;
*                _KEYEXPR WITH lcCompID+SUBSTR(_KEYEXPR,3);
*                 FOR !EMPTY(LEFT(_KEYEXPR,2))
  CASE lcAction = 'T'  
  CASE lcAction = 'P'
ENDCASE

SELECT (lnAlias)             

* -------------------------------------------------------------

FUNCTION LFUPSty
LPARAMETERS lcAction,oUpdObj
lnAlias = SELECT()
lcSysPath     = oUpdObj.cAriaPath
lcDataPath    = oUpdObj.CurrDataPath
lcMasterAlias = oUpdObj.CurrMastAlias
lcStyDye     = oUpdObj.CurrTmprAlias
DO CASE
  CASE lcAction = 'I'
    USE (lcDataPath+'STYLE') IN 0 ORDER TAG STYLE
  CASE lcAction = 'T'
    USE IN STYLE
  CASE lcAction = 'P'
    IF SEEK(&lcStyDye..STYLE,'STYLE')
      SELECT STYLE
      DO CASE
        CASE &lcStyDye.._Status $ 'AM'
          lcSizFlds = "ORD   ,WIP   ,STK   ,ALO   ,SHP   ,RET   ,RA   ,INTRANS ,NWO"
          lcTotFlds = "TOTORD,TOTWIP,TOTSTK,TOTALO,TOTSHP,TOTRET,TOTRA,TOTINTRN,NTOTWO"
          lnCatgs   = OCCURS(",", lcSizFlds) + 1
          FOR lnCatg = 1 TO lnCatgs
            lnPos     = ATC(",", lcSizFlds)
            lnNoOfCh  = IIF(lnPos = 0, LEN(lcSizFlds), lnPos)
            lcSzCatg  = SUBSTR(lcSizFlds, 1, lnPos)
            lcSizFlds = STRTRAN(lcSizFlds, lcSzCatg, SPACE(0), 1, 1)
            lcSzCatg  = UPPER(ALLTRIM(STRTRAN(lcSzCatg, ",", SPACE(0))))
  
            lnPos     = ATC(",", lcTotFlds)
            lnNoOfCh  = IIF(lnPos = 0, LEN(lcTotFlds), lnPos)
            lcTtCatg  = SUBSTR(lcTotFlds, 1, lnPos)
            lcTotFlds = STRTRAN(lcTotFlds, lcTtCatg, SPACE(0), 1, 1)
            lcTtCatg  = UPPER(ALLTRIM(STRTRAN(lcTtCatg, ",", SPACE(0))))
            
            
            IF !EMPTY(lcSzCatg)
              FOR lnSize = 1 TO 8
                lcSzFldName = lcSzCatg + STR(lnSize, 1)
                REPLACE &lcSzFldName. WITH &lcSzFldName. + &lcStyDye..&lcSzFldName
              ENDFOR
            ENDIF
  
            IF !EMPTY(lcTtCatg)
               REPLACE &lcTtCatg WITH &lcTtCatg + &lcStyDye..&lcTtCatg
            ENDIF
          ENDFOR
        
      ENDCASE        
    ENDIF          
  
ENDCASE  
SELECT (lnAlias)


* -------------------------------------------------------------




*!*************************************************************
*! Name      : gfTraceKey
*! Developer : Hesham El-Sheltawi (Hesham)
*! Date      : 03/30/98
*! Purpose   : Adding Record in the transaction tracing file
*!             For modification happed in specific data file
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : None.
*!*************************************************************
*! Passed Parameters  : lcFileName   file that was modified
*!                      lcKeyExpr    key of the record modified
*!                      lcEventOccr  Event occurs on record "E,A,D"
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = gfTraceKey('INVHDR','INV#1','E')
*!*************************************************************
*!E300842,4 
*!*************************************************************
*! Modifications *E301058,1 11/01/1998 support multi companies system.
*!
FUNCTION gfTraceKey
*E301058,1 add new two parameters to update in multi companies system (From SM)[begin]
*PARAMETERS lcFileName,lcKeyExpr,lcEventOccr
PARAMETERS lcFileName,lcKeyExpr,lcEventOccr,lcUpdtDir,lcUpdtModl
*E301058,1 add new two parameters to update in multi companies system (From SM)[end]

IF TYPE('lcUpdtDir') $ 'UL' OR EMPTY(lcUpdtDir) OR TYPE('lcUpdtModl') $ 'UL' OR EMPTY(lcUpdtModl)
  lcUpdtDir  = gcDataDir
  lcUpdtModl = gcComp_Mdl
ENDIF

** If the communication module is not installed for the active company
** return

*E301058,1 Change (gcComp_Mdl to lcUpdtModl) to support multi-companies system
*IF ! ('CM' $ gcComp_Mdl)
IF ! ('CM' $ lcUpdtModl)
  RETURN
ENDIF
** end checking module existance
** define private variables used by the function
*E301058,1 I will open files in another alias avoiding using same name for another company. [Begin]

*PRIVATE lnAlias,llCMTRACE,llViewUsed,llViewSite
PRIVATE lnAlias
lnAlias = SELECT()            && save the active work area

*E301058,1 I will open files in another alias avoiding using same name for another company. [Begin]
*llCMTRACE  = USED('CMTRACE')  && check opening of CMTRACE FILE
*llViewUsed = USED('CMVIEWD')  && check opening of CMVIEWD FILE
*llViewSite = USED('CMSITVEW') && check opening of CMSITEVEW FILE 

** IF trans. tracing file was not opend then open it
*E301058,1 no codition wanted Open in another alias.
*IF !llCMTRACE
*E301058,1 Change (gcDataDir to lcUpdtDir) to support multi-companies system
  *USE (gcDataDir+'CMTRACE') IN 0 ORDER TAG CTRANS
USE (lcUpdtDir+'CMTRACE') IN 0 ORDER TAG CTRANS AGAIN ALIAS TRACE_ALIS
*ENDIF
** end IF trans.

** IF site views file was not opend then open it
*E301058,1 no codition wanted Open in another alias.
*IF !llViewSite
*E301058,1 Change (gcDataDir to lcUpdtDir) to support multi-companies system
  *USE (gcDataDir+'CMSITVEW') IN 0 ORDER TAG CVIEWID
USE (lcUpdtDir+'CMSITVEW') IN 0 ORDER TAG CVIEWID AGAIN ALIAS SITVW_ALIS
*ENDIF
** End IF site views

** IF view details file was not opend then open it
*E301058,1 no codition wanted Open in another alias.
*IF !llViewUsed
*E301058,1 Change (gcDataDir to lcUpdtDir) to support multi-companies system
  *USE (gcDataDir+'CMVIEWD') IN 0 ORDER TAG FILENAME
USE (lcUpdtDir+'CMVIEWD') IN 0 ORDER TAG FILENAME AGAIN ALIAS VIEW_ALIS
*ENDIF
** End IF view details 
*E301058,1 I will open files in another alias avoiding using same name for another company. [End]

lcFileName = PADR(UPPER(lcFileName),8)  && make the file name 8 char width
lcKeyExpr = lcKeyExpr+CHR(250)
*E301058,1 Change expresion to use new alias
*lcKeyExpr = PADR(lcKeyExpr,LEN(CMTRACE.CKEYEXPR))  && make the key expr 80 char width
lcKeyExpr = PADR(lcKeyExpr,LEN(TRACE_ALIS.CKEYEXPR))  && make the key expr 80 char width

*E301058,1 Change order to use new alias
*SET ORDER TO TAG CVIEWID IN CMSITVEW    
SET ORDER TO TAG CVIEWID IN SITVW_ALIS    

*E301058,1 Change relation to use new alias
*SELECT CMVIEWD
SELECT VIEW_ALIS
SET ORDER TO TAG FILENAME

*SET RELATION TO CVIEWID INTO CMSITVEW ADDI
SET RELATION TO CVIEWID INTO SITVW_ALIS ADDI

*E301058,1 Change select to use new alias
*SELECT CMTRACE
SELECT TRACE_ALIS
SET ORDER TO TAG CTRANS
** if the file we want to add record for exist in any user defined view
** and this view is selected for any site to send data for

*E301058,1 Change next command to use new alias
*IF SEEK(lcFileName,'CMVIEWD') AND !EOF(CMSITVEW)
IF SEEK(lcFileName,'VIEW_ALIS') AND !EOF('SITVW_ALIS')
  ** check the event occured on the record
  DO CASE
    ** if the event was add new record then add the record directly in
    ** the trans. tracing file
    CASE lcEventOccr = 'A'
      
     *E301058,1 Change insert to use new alias
     *INSERT INTO CMTRACE (CFILE_NAM,CKEYEXPR,CSTATUS);
     *            VALUES  (lcFileName,lcKeyExpr,lcEventOccr)
     INSERT INTO TRACE_ALIS (CFILE_NAM,CKEYEXPR,CSTATUS);
                    VALUES  (lcFileName,lcKeyExpr,lcEventOccr)

    ** otherwise if the event was delete or modify then             
    OTHERWISE
      ** check if there was a record added for the same file+key in the
      ** trans. tracing file
      IF SEEK(lcFileName+lcKeyExpr)
        ** if there was record for the file+key then
        DO CASE
          ** if the event was deleting a record and the record in the
          ** trans. tracing file was just added the delete the record
          ** from the trans. tracing file
          CASE lcEventOccr = 'D' AND CSTATUS = 'A'
            BLANK
            DELETE
          ** if the event was deleting a record and the record in the
          ** trans. tracing file was modied then change the status
          ** of the record in the trans. tracing file to deleted
          CASE lcEventOccr = 'D' AND CSTATUS = 'M'        
            REPLACE CSTATUS WITH 'D'
        ENDCASE
      ** else if there was no record for file+key in the trans. tracing
      ** file then add new record with status like the event passed for
      ** the function
      ELSE
       *E301058,1 Change insert to use new alias
        *INSERT INTO CMTRACE (CFILE_NAM,CKEYEXPR,CSTATUS);
        *            VALUES  (lcFileName,lcKeyExpr,lcEventOccr)
        INSERT INTO TRACE_ALIS (CFILE_NAM,CKEYEXPR,CSTATUS);
                       VALUES  (lcFileName,lcKeyExpr,lcEventOccr)
      ENDIF
  ENDCASE  
ENDIF
** clear relation on the site views file

*E301058,1 Change next command to use new alias
*SELECT CMVIEWD
SELECT VIEW_ALIS
*SET RELATION OFF INTO CMSITVEW
SET RELATION OFF INTO SITVW_ALIS

** if the trace file was opend by the function the close it

*E301058,1 I open file in another alias then will close it now not under condition [begin]

*IF !llCMTRACE
  *USE IN CMTRACE
USE IN TRACE_ALIS
*ENDIF
** end closing trace file

** if the sites view file was opend by the function the close it
*IF !llViewSite
  *USE IN CMSITVEW
USE IN SITVW_ALIS
*ENDIF
** end closing sites view file

** if the views details file was opend by the function the close it
*IF !llViewUsed
  *USE IN CMVIEWD
USE IN VIEW_ALIS
*ENDIF
** end closing views details file

** restore the current work area
SELECT (lnAlias)
*-- end of gfTraceKey.
