*-------------------------------------------------------------------
SET CPDialog OFF
SET CLASSLIB TO comm ADDI
PUBLIC AriaCollectObj
AriaCollectObj = CREATEOBJECT('collect')
RELEASE AriaCollectObj
RELEASE CLASSLIB comm

*-------------------------------------------------------------------

FUNCTION lfSlPos
PARAMETERS lcAction,CollectObject,lcProcessSite
PRIVATE lnCount,lcTransDir,lcTransFile,lcFileName ,lnAlias
lnAlias = SELECT()
DO CASE
  CASE lcAction = 'I'
    FOR lnCount = 1 TO ALEN(CollectObject.FileDestinations,1)
        lcFileName = CollectObject.GetFileName('POSLN',CollectObject.FileDestinations[lnCount,1])
        IF !EMPTY(lcFileName)
          USE (lcFileName) IN 0 EXCL
          lcPosAlias = SUBSTR(lcFileName,RAT('\',lcFileName)+1)                  
          SELECT (lcPosAlias)
          INDEX ON cstytype+po TO (lcFileName+'.IDX')
        ENDIF
        
    ENDFOR

  CASE lcAction = 'T'
    FOR lnCount = 1 TO ALEN(CollectObject.FileDestinations,1)
      lcFileName = CollectObject.GetFileName('POSLN',CollectObject.FileDestinations[lnCount,1])    
      IF !EMPTY(lcFileName)
        lcSiteAlias = SUBSTR(lcFileName,RAT('\',lcFileName)+1)
        IF USED(lcSiteAlias)
          USE IN (lcSiteAlias)
        ENDIF  
        ERASE (lcFileName+'.IDX')
      ENDIF
    ENDFOR

  CASE lcAction = 'B'
      lcFileName = CollectObject.GetFileName('POSLN',lcProcessSite)    
      IF !EMPTY(lcFileName)
        lcFileName = SUBSTR(lcFileName,RAT('\',lcFileName)+1)
        IF USED(lcFileName)
            RETURN SEEK(cstytype+po,lcFileName)
        ENDIF
      ELSE
        RETURN .F.
      ENDIF
  
  CASE lcAction = 'A'
ENDCASE
SELECT (lnAlias)

*-------------------------------------------------------------------

FUNCTION lfSelSetup
PARAMETERS lcAction,CollectObject,lcProcessSite
PRIVATE lnAlias
IF lcAction = 'B'
  RETURN EVAL(CollectObject.ALIAS+'.lCanTransf')
ENDIF

*-------------------------------------------------------------------

FUNCTION lfSlOrder
PARAMETERS lcAction,CollectObject,lcProcessSite
PRIVATE lnCount,lcTransDir,lcTransFile,lcFileName ,lnAlias
lnAlias = SELECT()
DO CASE
  CASE lcAction = 'I'
    USE (CollectObject.DataPath+'ORDHDR') AGAIN ALIAS ORDSEL ORDER TAG ORDHDR IN 0
    USE (CollectObject.DataPath+'WAREHOUS') AGAIN ALIAS WARESEL ORDER TAG WAREHOUS IN 0
  CASE lcAction = 'T'
    USE IN ORDSEL
    USE IN WARESEL
  CASE lcAction = 'B'
   IF EMPTY(CWARECODE) 
     IF SEEK(cordtype+order,'ORDSEL') 
        RETURN IIF(SEEK(ORDSEL.CWARECODE,'WARESEL'),WARESEL.CSITEID=lcProcessSite,.F.)
     ELSE
       RETURN .F.
     ENDIF
   ELSE
       RETURN IIF(SEEK(CWARECODE,'WARESEL'),WARESEL.CSITEID=lcProcessSite,.F.)   
   ENDIF  
ENDCASE
SELECT (lnAlias)

*-------------------------------------------------------------------

FUNCTION lfSlStyle
PARAMETERS lcAction,CollectObject,lcProcessSite
PRIVATE lnCount,lcTransDir,lcTransFile,lcFileName ,lnAlias
lnAlias = SELECT()
DO CASE
  CASE lcAction = 'I'
    FOR lnCount = 1 TO ALEN(CollectObject.FileDestinations,1)
        lcFileName = CollectObject.GetFileName('STYDYE',CollectObject.FileDestinations[lnCount,1])
        IF !EMPTY(lcFileName)
          USE (lcFileName) IN 0 EXCL
          lcPosAlias = SUBSTR(lcFileName,RAT('\',lcFileName)+1)                  
          SELECT (lcPosAlias)
          INDEX ON STYLE TO (lcFileName+'.IDX')

        *MAN START
        ELSE
          IF !USED("StyDye")
            USE (CollectObject.datapath+"StyDye") IN 0 EXCL
          ENDIF
            
          SELECT StyDye
          SET ORDER TO TAG StyDye
        *MAN End  

        ENDIF
    ENDFOR

  CASE lcAction = 'T'
    FOR lnCount = 1 TO ALEN(CollectObject.FileDestinations,1)
      lcFileName = CollectObject.GetFileName('STYDYE',CollectObject.FileDestinations[lnCount,1])    
      IF !EMPTY(lcFileName)
        lcSiteAlias = SUBSTR(lcFileName,RAT('\',lcFileName)+1)
        IF USED(lcSiteAlias)
          USE IN (lcSiteAlias)
        ENDIF  
        ERASE (lcFileName+'.IDX')
      *MAN Start  
      ELSE
        lcTmpAls = "StyDye" 
        IF USED(lcTmpAls)
          USE IN (lcTmpAls)
        ENDIF  
      *MAN End
      ENDIF
    ENDFOR
  CASE lcAction = 'B'
      lcFileName = CollectObject.GetFileName('STYDYE',lcProcessSite)    
      IF !EMPTY(lcFileName)
        lcFileName = SUBSTR(lcFileName,RAT('\',lcFileName)+1)
        IF USED(lcFileName)
            RETURN SEEK(STYLE,lcFileName)
        ENDIF
      ELSE
        *MAN Start
        *RETURN .F.  
        
        lcTmpAls = "StyDye"
        IF USED(lcTmpAls)
          llWareIsUsed = USED("WAREHOUS")
          SELECT cWareCode ;
            FROM (CollectObject.datapath+"WAREHOUS");
           WHERE CSITEID = lcProcessSite INTO ARRAY laDesWares
          llRet = .F. 
          lnWareNo = 0
          IF _Tally > 0 
            DO WHILE !llRet AND lnWareNo < ALEN(laDesWares,1)
              lnWareNo = lnWareNo + 1 
              llRet = SEEK(STYLE + laDesWares[lnWareNo],lcTmpAls)
            ENDDO
          ENDIF  
          IF !llWareIsUsed
            USE IN WAREHOUS
          ENDIF

          RETURN llRet
        ELSE 
          RETURN .F.  
        ENDIF   
        *MAN End
      ENDIF
  
  CASE lcAction = 'A'
ENDCASE
SELECT (lnAlias)

*-------------------------------------------------------------------
