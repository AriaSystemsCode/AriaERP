*:************************************************************************
*:  Program File: \ARIA4XP\REPORTS\GL\GLAUTREC.PRG
*:  Module      : General Ledger
*:  Desc.       : Automatic reccurings
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 09/27/2012
*:  Reference   : E303242,1
*:************************************************************************
*B610132,1 [T20121023.0022] TMI 10/24/2012 remove the calling to the function lfChangeGrid
*:************************************************************************
*:
*** Report Setup

* Define the variables that will be used in the FRX, so no need to edit the frx and change these variables
gcUser_Id = oAriaApplication.User_ID
gcCom_Name = oAriaApplication.ActiveCompanyName
gdSysDate = oAriaApplication.SystemDate
* Define the variables that will be used in the FRX, so no need to edit the frx and change these variables

DO CASE
  CASE lcRpForm = "GLAUTRES"    && if use the summary report
    SELECT GLAUTHD
    GO TOP
    
    *- data collection
    =lfCollect()
    SELECT(lcTmpFile)
    DO gfDispRe WITH EVAL('lcRpForm')

     
  CASE lcRpForm = "GLAUTRED"    && If use the detail report
    
    lcAddExp = " AND GLAUTHD.CAUTTYPE+GLAUTHD.CAUTCODE = GLAUTDT.CAUTTYPE+GLAUTDT.CAUTCODE AND GLAUTDT.CACCTCODE = GLACCHAR.CACCTCODE"
    IF !lcAddExp $ loOgScroll.lcRpExp
      loOgScroll.lcRpExp = loOgScroll.lcRpExp + lcAddExp
    ENDIF   

    lcRpFiles  = "GLAUTDT,GLAUTHD,GLACCHAR"  && Get slected files name

    lnCount    = 0
    lnTotal    = RECCOUNT('GLAUTDT')
    lnOldAlias = SELECT() 
    IF llOGFltCh 
       
      *** Save escape setting
      lcSaveEsc = SET('ESCAP')
      lcSaveOnEs = ON('ESCAPE')
      SET ESCAP ON
      ON ESCAP DO gpSQLBrak
        
      *** Intialize the varliable that count rows selected
      _TALLY = 0
      
      *** Activate the system select therom.
      SET TALK ON
      
      WAIT 'Collecting data...' WINDOW NOWAIT     
      
      ***   Create select  statment
      SELECT  DISTINCT &lcRpFields.;
          FROM  &lcRpFiles. ;
         WHERE  &lcRpExp.  .AND. lfWaitMsg();
          &lcRpOrder.;
         INTO CURSOR &lcRpTargt
         
      *** Restore the old setting
      WAIT CLEAR
      SET TALK OFF   
      SET ESCAPE &lcSaveEsc  
      On  ESCAPE &lcSaveOnEs 
      
      IF _TALLY = 0        && No records collected
        ** NO recoeds hove been collected
        =gfModalGen("INM00052B00000","DIALOG")
      ELSE
        *** Display the report to screen , file or printer
        DO gfDispRe WITH EVAL('lcRpForm')
      ENDIF
    ELSE  
      SELECT (lcRpTargt)
      DO gfDispRe WITH EVAL('lcRpForm')
    ENDIF  
    SELECT (lnOldAlias)
ENDCASE


*!************************************************************************
*!
*!      FUNCTION : lfChngCond
*!
*!************************************************************************
*
***  Function to switch between two FRX reports
FUNCTION lfChngCond

llOGFilter=IIF(lcRpForm="GLAUTRED",.F.,.T.)
DO CASE
  CASE lcRpForm =   "GLAUTRED"
*B610132,1 [T20121023.0022] TMI 10/24/2012 [Start] 
*      =lfChangeGrid('GLAUTRE2')
*B610132,1 [T20121023.0022] TMI 10/24/2012 [End  ] 
  CASE lcRpForm =   "GLAUTRES"
*B610132,1 [T20121023.0022] TMI 10/24/2012 [Start] 
*      =lfChangeGrid('GLAUTREC')  
*B610132,1 [T20121023.0022] TMI 10/24/2012 [End  ] 
ENDCASE  
ClearRead()

*!************************************************************************
*!
*!      FUNCTION : lfClearRep
*!
*!************************************************************************
*
FUNCTION lfClearRep


*!************************************************************************
*!
*!      FUNCTION : lfvAutCode
*!
*!************************************************************************
*
FUNCTION lfvAutCode

DECLARE laRpRetFld(1)
lcBrFields    = 'CAutCode:H="Code",CAutDes:H="Description"'
laRpRetFld[1] = ''
lcRpCurFld = loOgScroll.ActiveControl.Parent.cOgArray
SELECT GLAUTHD
SET ORDER TO TAG typecode
IF ('?' $ &lcRpCurFld. .OR. !SEEK('R'+&lcRpCurFld.)) .AND. !EMPTY(ALLTRIM(&lcRpCurFld.))
  =gfBrows(["R"],'CAutCode',"laRpRetFld",'Codes File',.F.)
  &lcRpCurFld = laRpRetFld[1]
ENDIF
SET ORDER TO

************************************************************
*! Name      : lfRepWhen
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 09/30/2012
*! Purpose   : When Function
************************************************************
FUNCTION lfRepWhen
SET PROCEDURE TO (loOgScroll.gcRepHome + loOgScroll.gcAct_Appl + '\glrepfnc.fxp') ADDITIVE 

*- End of lfRepWhen.

*!*****************************************************************************************
*! Name      : lfRightGet
*! Developer : 
*! Date      : 10/13/2002 10:41:47 AM
*! Purpose   : Get Right value
*!*****************************************************************************************
*! Parameters: 
*!****************************************************************************************
*! Returns   : 
*!****************************************************************************************
*! Runs as following:
*!
*!****************************************************************************************

FUNCTION lfRightGet
PARAMETERS mRightHead, cLeftType, cOperator, lcElmSep, cRightType, lcDBEngine, llCaseInsensitive, llEnglishQuery

lcDBEngine = IIF(TYPE('lcDBEngine') = 'C', lcDBEngine, oAriaApplication.cNativeDBID)

lcRetVal=mRightHead
DO CASE
  CASE cRightType='V'
    DO CASE      
      CASE cLeftType $ 'CM'
        IF INLIST(COPERATOR,'Between','In List')
          lcSeper = IIF(!llFilter AND cOperator = 'Between', IIF(llEnglishQuery, ' and ', ' AND '), ',')
          
          IF !EMPTY(mRightHead)
            LOCAL lcRight
            
            IF llCaseInsensitive
              lcRight = UPPER(mRightHead)
            *! B129154,1 MAH 07/28/2005 [BEGIN] 
            ELSE
              lcRight = mRightHead
            *! B129154,1 MAH 07/28/2005 [END] 
            ENDIF
            
            LOCAL laRight[1]
            =gfSubStr(lcRight, @laRight, lcElmSep)
            
            lcRight = ''
            LOCAL lnIndex
            FOR lnIndex = 1 TO ALEN(laRight, 1)
              lcRight = lcRight + gfStrToExp(laRight[lnIndex], lcDBEngine, llEnglishQuery) + lcElmSep
            ENDFOR
            
            lcRight = SUBSTR(lcRight, 1, LEN(lcRight) - 1)
          ENDIF
            
          lcRetVal = STRTRAN(UPPER(lcRight), lcElmSep, lcSeper)

        ELSE
          IF cOperator = 'Greater Than'
            IF llCaseInsensitive
              RETURN gfStrToExp(RTRIM(UPPER(mrightHead)), lcDBEngine, llEnglishQuery)
            ELSE
              RETURN gfStrToExp(RTRIM(mrightHead), lcDBEngine, llEnglishQuery)
            ENDIF
          ELSE
            IF llCaseInsensitive
              RETURN gfStrToExp(IIF(cOperator = 'Contains', UPPER(RTRIM(mrightHead)), RTRIM(UPPER(mrightHead))), lcDBEngine, llEnglishQuery)
            ELSE
              RETURN gfStrToExp(IIF(cOperator = 'Contains', ALLTRIM(mrightHead), RTRIM(mrightHead)), lcDBEngine, llEnglishQuery)
            ENDIF
          ENDIF

        ENDIF
      CASE cLeftType = 'N'
        lcSeper = IIF(COPERATOR='Between' AND !llFilter, IIF(llEnglishQuery, ' and ', ' AND '), ',')

        lcRetVal=STRTRAN(mRightHead,lcElmSep,lcSeper)
        IF EMPTY(lcRetVal)
          lcRetVal='0'
        ENDIF

      CASE cLeftType = 'D'
        lcOldCen = SET('CENT')
        SET CENTURY ON
        IF INLIST(COPERATOR,'Between','In List')
          LOCAL lnDates, lcRightHand, lnDate, lcDate
          lcRightHand = lcElmSep + ALLTRIM(mRightHead) + lcElmSep
          lnDates = OCCURS(lcElmSep,lcRightHand) - 1
          lcRetVal = ""
          FOR lnDate = 1 TO lnDates
            lcDate = STREXTRACT(lcRightHand,lcElmSep,lcElmSep,lnDate)
      
            IF !llFilter AND cOperator = 'Between'
              IF llEnglishQuery
                IF EMPTY(lcRetVal)
                  lcRetVal = lcRetVal + " '" + CDateToString(lcDate) + "'"
                ELSE
                  lcRetVal = lcRetVal + " and '" + CDateToString(lcDate) + "'"
                ENDIF
              ELSE
                IF EMPTY(lcRetVal)
                  lcRetVal = lcRetVal + " '" + CDateToString(lcDate) + "'"
                ELSE
                  lcRetVal = lcRetVal + " AND '" + CDateToString(lcDate) + "'"
                ENDIF
              ENDIF
            ELSE
              lcRetVal = lcRetVal + ",'" + CDateToString(lcDate) + "'"
            ENDIF
          ENDFOR 
          lcRetVal = SUBSTR(lcRetVal,2)
          
        ELSE
          IF BETWEEN(YEAR(CTOD(MRIGHTHEAD)) ,1900,1950)
            lcTmpYear  = ALLTRIM(STR(YEAR(CTOD(MRIGHTHEAD)) + 100))
            MRIGHTHEAD = SUBSTR(MRIGHTHEAD,1,6)+lcTmpYear
          ENDIF
          lcRetVal = "'" + CDateToString(mRightHead) + "'"
        ENDIF   
        SET CENTURY &lcOldCen
      
      CASE cLeftType = 'L'
        DO CASE
          CASE ALLTRIM(lcDBEngine) == oAriaApplication.cNativeDBID
            RETURN ' ' + lcRetVal + ' '
            
          CASE ALLTRIM(lcDBEngine) == oAriaApplication.cSQLDBID
            RETURN ' ' + IIF(UPPER(ALLTRIM(lcRetVal)) = '.T.' .OR. UPPER(ALLTRIM(lcRetVal)) = 'T', '1','0') + ' '
        ENDCASE
    ENDCASE  

  CASE cRightType='F'
    DO CASE
      CASE ALLTRIM(lcDBEngine) == oAriaApplication.cNativeDBID
        IF cLeftType $ 'MC'
          IF llCaseInsensitive
            lcRetVal = 'UPPER(RTRIM(' + ALLTRIM(mrightHead) + '))'
          ELSE
            lcRetVal = STRTRAN(IIF(cOperator = 'Contains', ALLTRIM(mrightHead) , mrightHead), lcElmSep, ',')
          ENDIF
        ENDIF
        
        IF cLeftType $ 'NL'
          lcRetVal = ALLTRIM(mrightHead)
        ENDIF
        
        IF cLeftType $ 'D'
          lcRetVal = 'DTOS(' + ALLTRIM(mrightHead) + ')'
        ENDIF
      
      CASE ALLTRIM(lcDBEngine) == oAriaApplication.cSQLDBID
        lcRetVal = '[' + ALLTRIM(mrightHead) + ']'
        IF cLeftType = 'D'
          lcRetVal = ' CONVERT(CHAR(8),' + lcRetVal + ', 112) '
        ENDIF
    ENDCASE
ENDCASE  

IF INLIST(cOperator,'Between','In List') AND EMPTY(ALLTRIM(mRightHead))
  lcSeper = IIF(!llFilter AND cOperator='Between', IIF(llEnglishQuery, ' and ', ' AND '), ',')

  lcRetVal=lcRetVal+lcSeper+lcRetVal
ENDIF
RETURN lcRetVal
*-- end of lfRightGet. DTOS

************************************************************
*! Name      : lfvDate
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 10/10/2012
*! Purpose   : Valid fn for begin data and enddata
************************************************************
FUNCTION lfvDate
loFld = loOgScroll.ActiveControl
IF loFld.Value == loFld.OldValue OR EMPTY(loFld.Value)
  RETURN 
ENDIF   
lcRpCurFld = loOgScroll.ActiveControl.Parent.cOgArray
*- End of lfvDate.