*:************************************************************************
*:  Program File: \ARIA4XP\REPORTS\GL\GLUNTRAN.PRG
*:  Desc.       :  Unposted transactions  
*:  System      : Aria 4lfAddCommen
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 09/27/2012
*:  Reference   : E303250,1
*:************************************************************************
*B610132,1 [T20121023.0022] TMI 10/24/2012 remove the calling to the function lfChangeGrid
*:************************************************************************
*** Report Setup

* Define the variables that will be used in the FRX, so no need to edit the frx and change these variables
gcUser_Id = oAriaApplication.User_ID
gcCom_Name = oAriaApplication.ActiveCompanyName
gdSysDate = oAriaApplication.SystemDate
* Define the variables that will be used in the FRX, so no need to edit the frx and change these variables


SELECT GLTRNSHD
DO CASE
  CASE lcRpGroup == 'cTrnStat'
    SET ORDER TO TAG TRANSTAT
  OTHERWISE
    SET ORDER TO TAG BATCHTRN
ENDCASE
IF lcRpForm = "GLUNTRAD"
  SELECT GLTRNSHD
  SET ORDER TO TAG BATCHTRN IN GLTRNSDT
  SET ORDER TO TAG ACCTCODE IN GLACCHAR
  SET RELATION TO cbatchno + ctranno INTO GLTRNSDT ADDITIVE
  SELECT GLTRNSDT
  SET RELATION TO cacctcode INTO GLACCHAR ADDITIVE
  SELECT GLTRNSHD
  SET SKIP TO GLTRNSDT
ENDIF

*- data collection
=lfCollect()
SELECT (lcTmpFile)
DO gfDispRe WITH EVAL('lcRpForm')

SET RELATION TO
SET SKIP TO
IF lcRpForm = "GLUNTRAD"
  SELECT GLTRNSHD
  SET RELATION TO
  SELECT GLTRNSDT
  SET RELATION TO  
ENDIF
RETURN

************************************************************
*! Name      : lfCollect
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 10/03/2012
*! Purpose   : Collect data
************************************************************
FUNCTION lfCollect
PARAMETERS lcFile
lcFile = IIF(EMPTY(lcFile),ALIAS(),lcFile)

LOCAL lnSlct
lnSlct = SELECT(0)

  LOCAL laStru[1,18],lnLen,i
  SELECT &lcFile
  AFIELDS(laStru)
  lnLen = ALEN(laStru,1)
  lnI = 2
  lnMore = 0
  IF lcRpForm = "GLUNTRAD"
  
    lnMore = 5
    DIMENSION laStru[lnlen+lnMore ,18]
    
    FOR i=1 TO lnMore 
      AINS(laStru,3)
    ENDFOR 
    
    lnI = lnI + 1
    laStru[lnI,1] = 'cacctcode'
    laStru[lnI,2] = "C"
    laStru[lnI,3] = 24
    laStru[lnI,4] = 0
        
    lnI = lnI + 1
    laStru[lnI,1] = 'caccnsdes' 
    laStru[lnI,2] = "C"
    laStru[lnI,3] = 65
    laStru[lnI,4] = 0
        
    lnI = lnI + 1
    laStru[lnI,1] = 'cdrorcr' 
    laStru[lnI,2] = "C"
    laStru[lnI,3] = 1
    laStru[lnI,4] = 0    
    
    lnI = lnI + 1
    laStru[lnI,1] = 'namount' 
    laStru[lnI,2] = "N"
    laStru[lnI,3] = 15 
    laStru[lnI,4] = 2
    
    lnI = lnI + 1
    laStru[lnI,1] = 'ctrdtexp' 
    laStru[lnI,2] = "C"
    laStru[lnI,3] = 40
    laStru[lnI,4] = 0    
  
  ELSE
    
    laStru[ASCAN(laStru,'CTRNTYPE')+2] = 20
    laStru[ASCAN(laStru,'CTRNSTAT')+2] = 20
      
  ENDIF 
    
  FOR i= 3 TO lnMore+2
    STORE .F. TO laStru[i,5],laStru[i,6]
    STORE '' TO laStru[i,7],laStru[i,8],laStru[i,9],laStru[i,10],laStru[i,11],laStru[i,12],laStru[i,13],laStru[i,14],laStru[i,15],laStru[i,16]
    STORE 0 TO laStru[i,17],laStru[i,18]    
  ENDFOR 

  CREATE TABLE (oAriaApplication.WorkDir+lcTmpFile) FROM ARRAY laStru
  

SELECT &lcFile
lcRel = SET("Relation")
SELECT (lcTmpFile)
SET RELATION TO &lcRel

SELECT &lcFile
lcFor = IIF(!EMPTY(ALLTRIM(lcRpExp)),'FOR ','')+lcRpExp
SCAN &lcFor
  SCATTER MEMVAR 
  IF lcRpForm = "GLUNTRAD"
    m.cacctcode = IIF(EOF('GLTRNSDT'),'',GLTRNSDT.cacctcode)
    m.caccnsdes = IIF(EOF('GLTRNSDT'),'',GLACCHAR.caccnsdes)
    m.cdrorcr = GLTRNSDT.cdrorcr
    m.namount = GLTRNSDT.namount
    m.ctrdtexp = IIF(EOF('GLTRNSDT'),'',GLTRNSDT.ctrdtexp)
  ELSE
    m.ctrntype = lfRpName(ctrntype)
    m.ctrnstat = lfRpName(ctrnstat)       
  ENDIF 
  INSERT INTO (lcTmpFile) FROM MEMVAR 
ENDSCAN 
  
SELECT (lnSlct)
*- End of lfCollect.

    
*!************************************************************************
*!
*!      FUNCTION lfChngCond
*!
*!************************************************************************
*
***  Function to switch between two FRX reports
FUNCTION lfChngCond

DO CASE
  CASE lcRpForm =   "GLUNTRAD"
*B610132,1 [T20121023.0022] TMI 10/24/2012 [Start] 
**      =lfChangeGrid('GLUNTRA2')
*B610132,1 [T20121023.0022] TMI 10/24/2012 [End  ] 
  CASE lcRpForm =   "GLUNTRAS"
*B610132,1 [T20121023.0022] TMI 10/24/2012 [Start] 
**      =lfChangeGrid('GLUNTRAN')  
*B610132,1 [T20121023.0022] TMI 10/24/2012 [End  ] 
ENDCASE  
ClearRead()

*!************************************************************************
*!
*!      FUNCTION lfClearRep
*!
*!************************************************************************
*
FUNCTION lfClearRep



*!************************************************************************
*!
*!      Function lfRpName
*!
*!************************************************************************
* Return the expersion accourding to its character
Function lfRpName
PARAMETERS lcRpValue

RETURN  SUBSTR(lcRpVldEnt,;
                  ATC('~',lcRpVldEnt,ATC(lcRpValue,'NSBLZEOUAVPYH'))+1,;
                 (ATC('~',lcRpVldEnt,ATC(lcRpValue,'NSBLZEOUAVPYH')+1)-1)-;
                 (ATC('~',lcRpVldEnt,ATC(lcRpValue,'NSBLZEOUAVPYH'))))

*!************************************************************************
*!
*!      FUNCTION lfvTrnCode
*!
*!************************************************************************
*
FUNCTION lfvTrnCode

loFld = loOgScroll.ActiveControl
IF loFld.Value == loFld.OldValue OR EMPTY(loFld.Value)
  RETURN 
ENDIF   

DECLARE laRpRetFld(1)
lcBrFields    = 'CTranNO:H="Code",CTrnDesc:H="Description"'
laRpRetFld[1] = ''
lcRpCurFld = loOgScroll.ActiveControl.Parent.oItem.cAssociate
SELECT GLTRNSHD
SET ORDER TO 1

SET FILTER TO ALLTRIM(CTRNSTAT) <> 'P'.AND. ALLTRIM(CTRNSTAT) <> 'Z'
LOCATE FOR ALLTRIM(CTRANNO) = &lcRpCurFld.
IF ('?' $ &lcRpCurFld. .OR. !FOUND()) .AND. !EMPTY(ALLTRIM(&lcRpCurFld.))
  IF !FOUND() AND BETWEEN(RECNO(0),1,RECCOUNT(ALIAS()))
    GOTO RECNO(0)
  ENDIF 
  =gfBrows('','CTRANNO',"laRpRetFld",'Transaction Codes ',.F.)  
  IF EMPTY(laRpRetFld[1])
    laRpRetFld[1] = loFld.OldVAlue
  ENDIF 
  loOgScroll.&lcRpCurFld = laRpRetFld[1]

ENDIF
SET FILTER TO

*!************************************************************************
*!
*!      FUNCTION lfvAutCode
*!
*!************************************************************************
*
FUNCTION lfvAutCode
PARAMETERS lcWhichFile

lcWhichFile=IIF(TYPE('lcWhichFile') $ 'ULND','GLAUTHD',lcWhichFile)
IF !EMPTY(loOgScroll.ActiveControl.Value)
  DECLARE laRpRetFld(2)
  lcRpVldEnt="|Recaring|Distribuation|Template|Automatic|"
  lcAutType=[IIF(EMPTY(CautType) OR !(CautType $ 'RDTA'),'',SUBSTR(lcRpVldEnt,]+;
                    [ATC('~',lcRpVldEnt,ATC(CautType,'RDTA'))+1,]+;
                    [(ATC('~',lcRpVldEnt,ATC(CautType,'RDTA')+1)-1)-]+;
                    [(ATC('~',lcRpVldEnt,ATC(CautType,'RDTA')))))]
  lcBrFields    = 'CautType='+lcAutType+':H="Automatic type",CAutCode:H="Code",CAutDes:H="Description"'
  laRpRetFld[1] = ''
lcRpCurFld = loOgScroll.ActiveControl.Parent.cOgArray
  llGLAUTHD = .F.
  IF !USED('GLAUTHD')
    SELECT 0
    gcDataDir = oAriaApplication.DataDir
    USE &gcDataDir.GLAUTHD
    llGLAUTHD = .T.
  ENDIF
  SELECT GLAUTHD
  SET ORDER TO TAG typecode
  DIMENSION laOGFltr[ALEN(laOGHDFlt,1)+ALEN(laOGFXFlt,1)+ALEN(laOGVRFlt,1),ALEN(laOGHDFlt,2)]
  =ACOPY(laOGHDFlt,laOGFltr)
  =ACOPY(laOGFXFlt,laOGFltr,1,ALEN(laOGFXFlt),ALEN(laOGHDFlt)+1)
  =ACOPY(laOGVRFlt,laOGFltr,1,ALEN(laOGVRFlt),ALEN(laOGHDFlt)+ALEN(laOGFxFlt)+1)
  lnAutCodType= ASCAN(laOGFltr,lcWhichFile+'.CAUTTYPE  ')
  lcAutCodType= IIF(lnAutCodType<>0,laOGFltr[ASUBSCR(laOGFltr,lnAutCodType,1),6],'')
  llScanAll=EMPTY(lcAutCodType)    
  llFoundHD = .F.
  lnAutCount=1
  DO WHILE !llFoundHD AND lnAutCount<=4
    lcAutCodType = IIF(llScanAll,SUBSTR('ADRT',lnAutCount,1),lcAutCodType)
    llFoundHD=  SEEK(lcAutCodType+&lcRpCurFld.) 
    IF llFoundHD OR !llScanAll
      EXIT
    ENDIF
    lnAutCount=lnAutCount+1
  ENDDO
  IF !llFoundHD
    =gfBrows(IIF(llScanAll,.f.,"lcAutCodType"),'CAutCode,CAutType',"laRpRetFld",'Codes File',.F.)
    &lcRpCurFld = laRpRetFld[1]
  ELSE
    llScanAll=.F.
  ENDIF
  IF llScanAll
    DO CASE
      CASE BETWEEN(lnAutCodType,ALEN(laOGHDFlt)+1,ALEN(laOGHDFlt)+ALEN(laOGFXFlt)+1)
        lnAutCodType= ASCAN(laOGFXFlt,lcWhichFile+'.CAUTTYPE  ')
        IF lnAutCodTyp>0
          laOGFxFlt[ASUBSCR(laOGFxFlt,lnAutCodType,1),6]=laRpRetFld[2]
           =lfOGShowGet()
        ENDIF
       CASE BETWEEN(lnAutCodType,ALEN(laOGHDFlt)+ALEN(laOGFXFlt)+2,ALEN(laOGFltr))
         lnAutCodType= ASCAN(laOGVRFlt,lcWhichFile+'.CAUTTYPE  ')
         IF lnAutCodTyp>0
           laOGVRFlt[ASUBSCR(laOGVRFlt,lnAutCodType,1),6]=laRpRetFld[2]
           =lfOGShowGet()
         ENDIF    
    ENDCASE  
  ENDIF  

  SET ORDER TO
  IF llGLAUTHD
    USE IN GLAUTHD
  ENDIF
ENDIF  


************************************************************
*! Name      : lfRepWhen
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 09/30/2012
*! Purpose   : When Function
************************************************************
FUNCTION lfRepWhen
SET PROCEDURE TO (loOgScroll.gcRepHome + loOgScroll.gcAct_Appl + '\glrepfnc.fxp') ADDITIVE 

*- End of lfRepWhen.