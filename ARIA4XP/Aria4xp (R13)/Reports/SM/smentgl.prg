****************************************************************************
*: Program file  : SMENTGL.PRG --- SMENTGL.RPT
*: Program desc. : GL Entries Report
*: System        : Aria Apparel System (Aria4XP).
*: Module        : SM 
*: Developer     : Mariam Mazhar - (MMT) E611849
*: Date          : 12/11/2019
*:**************************************************************************
#INCLUDE R:\Aria4xp\reports\SM\SMENTGL.h
IF llOGFltCh
  lfCreateTmpGl()
  lfCollectGLENData()
ELSE
  IF !USED(lcGLTEMP)
    USE (oAriaApplication.WorkDir + lcGLTEMP + ".DBF") IN 0 
  ENDIF
  SELECT (lcGLTEMP)
  LOCATE 
  IF EOF()
    =gfModalGen('TRM00052B00000','DIALOG')
    RETURN
  ENDIF
  lfAdjustCRSettings()
  IF USED(lcGLTEMP)
    USE IN (lcGLTEMP)
  ENDIF
  DO gfDispRe WITH EVAL('lcRpForm')
ENDIF


*!*************************************************************
*! Name      : lfCollectGLENData
*: Developer : Mariam Mazhar (MMT)
*: Date      : 12/11/2019
*! Purpose   : Collect Report Data
*!*************************************************************
FUNCTION lfCollectGLENData

lcWhereCond = ''
ldDateStart = {}
ldDateEnd = {}
lnDatePos = ASCAN(loogscroll.laOGFXFlt,"GLTRNDT.TRAN_DATE")
IF lnDatePos > 0
  lnDatePos = ASUBSCRIPT(loogscroll.laOGFXFlt,lnDatePos,1)
  IF !EMPTY(loogscroll.laOGFXFlt[lnDatePos,6])
    lnSepPos = AT('|',loogscroll.laOGFXFlt[lnDatePos,6])
    ldDateStart = CTOD(SUBSTR(loogscroll.laOGFXFlt[lnDatePos,6],1,lnSepPos-1))
    ldDateEnd = CTOD(SUBSTR(loogscroll.laOGFXFlt[lnDatePos,6],lnSepPos+1))
  ENDIF
ENDIF
IF !EMPTY(ldDateStart) AND !EMPTY(ldDateEnd)
  lcWhereCond = "GLTRNDT.TRAN_DATE BETWEEN '"+DTOS(ldDateStart)+"' AND '"+DTOS(ldDateEnd)+"'"
ENDIF  

lnStTrn = ''
lnEndTrn = ''
lnTranPos = ASCAN(loogscroll.laOGFXFlt,"GLTRNDT.TRAN_NO")
IF lnTranPos > 0
  lnTranPos = ASUBSCRIPT(loogscroll.laOGFXFlt,lnTranPos ,1)
  IF !EMPTY(loogscroll.laOGFXFlt[lnTranPos ,6])
    lnSepPos = AT('|',loogscroll.laOGFXFlt[lnTranPos ,6])
    lnStTrn = SUBSTR(loogscroll.laOGFXFlt[lnTranPos ,6],1,lnSepPos-1)
    lnEndTrn = SUBSTR(loogscroll.laOGFXFlt[lnTranPos ,6],lnSepPos+1)
  ENDIF
ENDIF
IF !EMPTY(lnStTrn) AND !EMPTY(lnEndTrn)
  lcWhereCond = lcWhereCond +IIF(!EMPTY(lcWhereCond),' AND ','')+"GLTRNDT.CTRAN_NO BETWEEN '"+lnStTrn+"' AND '"+lnEndTrn+"'"
ENDIF 




*"GLTRNHD.GLSESSION"
*
lcStSession = ''
lcEndSession = ''
lnSessionPos = ASCAN(loogscroll.laOGFXFlt,"GLTRNHD.GLSESSION")
IF lnSessionPos > 0
  lnSessionPos = ASUBSCRIPT(loogscroll.laOGFXFlt,lnSessionPos ,1)
  IF !EMPTY(loogscroll.laOGFXFlt[lnSessionPos ,6])
    lnSepPos = AT('|',loogscroll.laOGFXFlt[lnSessionPos ,6])
    lcStSession = SUBSTR(loogscroll.laOGFXFlt[lnSessionPos ,6],1,lnSepPos-1)
    lcEndSession= SUBSTR(loogscroll.laOGFXFlt[lnSessionPos ,6],lnSepPos+1)
  ENDIF
ENDIF
IF !EMPTY(lcStSession ) AND !EMPTY(lcEndSession)
  lcWhereCond = lcWhereCond +IIF(!EMPTY(lcWhereCond),' AND ','')+"GLTRNDT.GLSESSION BETWEEN '"+lcStSession +"' AND '"+lcEndSession+"'"
ENDIF 

lcTranType = ''
IF !EMPTY(lcRpTrType)
  lcTranType = "'"+STRTRAN(lcRpTrType,"|","','")+"'"
  lcWhereCond = lcWhereCond +IIF(!EMPTY(lcWhereCond),' AND ','')+"GLTRNDT.TRAN_TYPE IN  ("+lcTranType+")"
ENDIF 
lcGLTran = ''
lnGLTranTyPos = ASCAN(loogscroll.laOGFXFlt,"GLTRNHD.CGLTRANNO")
IF lnGLTranTyPos > 0
  lnGLTranTyPos = ASUBSCRIPT(loogscroll.laOGFXFlt,lnGLTranTyPos ,1)
  IF !EMPTY(loogscroll.laOGFXFlt[lnGLTranTyPos ,6])
    lcGLTran= loogscroll.laOGFXFlt[lnGLTranTyPos ,6]
  ENDIF
ENDIF
  IF !EMPTY(lcGLTran)
   lcWhereCond = lcWhereCond +IIF(!EMPTY(lcWhereCond),' AND ','')+"GLTRNDT.CGLTRANNO='"+lcGLTran+"'"
  ENDIF 

  lcGLBat = ''
  lnGLBatPos = ASCAN(loogscroll.laOGFXFlt,"GLTRNHD.CGLBATCHNO")
  IF lnGLBatPos> 0
    lnGLBatPos= ASUBSCRIPT(loogscroll.laOGFXFlt,lnGLBatPos,1)
    IF !EMPTY(loogscroll.laOGFXFlt[lnGLBatPos,6])
      lcGLBat = loogscroll.laOGFXFlt[lnGLBatPos,6]
    ENDIF
  ENDIF
  IF !EMPTY(lcGLBat)
    lcWhereCond = lcWhereCond +IIF(!EMPTY(lcWhereCond),' AND ','')+"GLTRNDT.CGLBATCHNO ='"+lcGLBat +"'"
  ENDIF 


llSelGLAcc = .F.
lcGLAccCur = ''
lnGLAccPos = ASCAN(loogscroll.laOGFXFlt,"GLTRNDT.CACCTCODE")
IF lnGLAccPos > 0
  lnGLAccPos = ASUBSCRIPT(loogscroll.laOGFXFlt,lnGLAccPos ,1)
  IF !EMPTY(loogscroll.laOGFXFlt[lnGLAccPos ,6])
    IF USED(loogscroll.laOGFXFlt[lnGLAccPos ,6]) AND RECCOUNT(loogscroll.laOGFXFlt[lnGLAccPos ,6])> 0
      SELECT (loogscroll.laOGFXFlt[lnGLAccPos ,6])
      LOCATE 
      IF !EOF()
        llSelGLAcc  = .T.
        lcGLAccCur = loogscroll.laOGFXFlt[lnGLAccPos ,6]
      ENDIF
    ENDIF  
  ENDIF
ENDIF

llSelCust = .F.
lcCustCur = ''
lnCustPos = ASCAN(loogscroll.laOGFXFlt,"CUSTOMER.ACCOUNT")
IF lnCustPos > 0
  lnCustPos = ASUBSCRIPT(loogscroll.laOGFXFlt,lnCustPos ,1)
  IF !EMPTY(loogscroll.laOGFXFlt[lnCustPos ,6])
    IF USED(loogscroll.laOGFXFlt[lnCustPos ,6]) AND RECCOUNT(loogscroll.laOGFXFlt[lnCustPos ,6])> 0
      SELECT (loogscroll.laOGFXFlt[lnCustPos ,6])
      LOCATE 
      IF !EOF()
        llSelCust = .T.
        lcCustCur = loogscroll.laOGFXFlt[lnCustPos ,6]
      ENDIF
    ENDIF  
  ENDIF
ENDIF

llSelVend = .F.
lcVendCur = ''
lnVendPos = ASCAN(loogscroll.laOGFXFlt,"APVENDOR.CVENCODE")
IF lnVendPos > 0
  lnVendPos = ASUBSCRIPT(loogscroll.laOGFXFlt,lnVendPos ,1)
  IF !EMPTY(loogscroll.laOGFXFlt[lnVendPos ,6])
    IF USED(loogscroll.laOGFXFlt[lnVendPos ,6]) AND RECCOUNT(loogscroll.laOGFXFlt[lnVendPos ,6])> 0
      SELECT (loogscroll.laOGFXFlt[lnVendPos ,6])
      LOCATE 
      IF !EOF()
        llSelVend = .T.
        lcVendCur = loogscroll.laOGFXFlt[lnVendPos ,6]
      ENDIF
    ENDIF  
  ENDIF
ENDIF

IF llSelCust OR llSelVend 
  IF llSelCust 
    SELECT (lcCustCur) 
    LOCATE 
    SCAN
      SELECT GLTRNHD
      IF gfSqlRun("Select * FROM GLTRNHD Where CCONT_ID ='"+&lcCustCur..Account+"'" +;
                   IIF(!EMPTY(lcGLBat)," AND CGLBATCHNO ='"+lcGLBat+"'","")+ IIF(!EMPTY(lcGLTran)," AND cGLTRANNO ='"+lcGLTran+"'","")+;
                   IIF(!EMPTY(lcStSession) AND !EMPTY(lcEndSession)," AND GLSESSION BETWEEN '"+lcStSession +"' AND '"+lcEndSession+"'","")+;
                   IIF(!EMPTY(ldDateStart) AND !EMPTY(ldDateEnd)," AND DTRNPDATE BETWEEN '"+DTOS(ldDateStart)+"' AND '"+DTOS(ldDateEnd)+"'","")+;
                   IIF(!EMPTY(lcTranType)," AND TRAN_TYPE IN  ("+lcTranType+")" ,'')+;
                   IIF(!EMPTY(lnStTrn) AND !EMPTY(lnEndTrn)," AND CTRAN_NO BETWEEN '"+lnStTrn+"' AND '"+lnEndTrn+"'",''),'GLTRNHD')
         SELECT GLTRNHD
         SCAN 
           WAIT WINDOW LANG_Collect_Data+GLTRNHD.CGLBATCHNO+LANG_GL_Transaction + GLTRNHD.cGLTRANNO NOWAIT 
           lcGLBatchNo = GLTRNHD.CGLBATCHNO
           lcGLTranNO = GLTRNHD.cGLTRANNO
           SELECT GLTRNDT
           =gfSetOrder('GLTRNDT') 
           =gfSeek(lcGLBatchNo+lcGLTranNO)
           SCAN REST WHILE CGLBATCHNO+CGLTRANNO+cTRAN_NO= lcGLBatchNo+lcGLTranNO FOR ;
                     IIF(llSelGLAcc,SEEK(cAcctCode,lcGLAccCur),.T.)
               
              m.cAcctCode = GLTRNDT.cAcctCode
              m.Tran_No = GLTRNDT.CTRAN_NO
              m.TRAN_DATE = GLTRNDT.TRAN_DATE 
              m.CGLREF = GLTRNDT.CGLREF
              m.GLSESSION = GLTRNDT.GLSESSION
              m.TRAN_TYPE = GLTRNDT.TRAN_TYPE 
              m.CGLBATCHNO = GLTRNDT.CGLBATCHNO 
              m.CGLTRANNO = GLTRNDT.CGLTRANNO 
              m.NAMOUNT = IIF(GLTRNDT.cDRORCR ='C',-1,1)*GLTRNDT.NAMOUNT 
              m.GLPeriod = GLTRNDT.CTRNPPRD
              m.GLFYEAR = GLTRNDT.CTRNPYR 
              m.TRAN_DESC = GLTRNHD.TRAN_DESC
              m.CCONT_ID = GLTRNHD.CCONT_ID 
              =gfSeek(m.cAcctCode,'GLACCHAR','ACCTCODE')
              m.CACCNLDES =GLACCHAR.CACCNLDES 
              INSERT INTO (lcGLTEMP) FROM MEMVAR
           ENDSCAN
         ENDSCAN 
      ENDIF
    ENDSCAN 
  ENDIF
  IF llSelVend 
    SELECT (lcVendCur)
    LOCATE
    SCAN
      SELECT GLTRNHD
      IF gfSqlRun("Select * FROM GLTRNHD Where CCONT_ID ='"+&lcVendCur..CVENDCODE+"'" +;
                   IIF(!EMPTY(lcGLBat)," AND CGLBATCHNO ='"+lcGLBat+"'","")+ IIF(!EMPTY(lcGLTran)," AND cGLTRANNO ='"+lcGLTran+"'","")+;
                   IIF(!EMPTY(lcStSession) AND !EMPTY(lcEndSession)," AND GLSESSION BETWEEN '"+lcStSession +"' AND '"+lcEndSession+"'","")+;
                   IIF(!EMPTY(ldDateStart) AND !EMPTY(ldDateEnd)," AND DTRNPDATE BETWEEN '"+DTOS(ldDateStart)+"' AND '"+DTOS(ldDateEnd)+"'","")+;
                   IIF(!EMPTY(lcTranType)," AND TRAN_TYPE IN  ("+lcTranType+")" ,'')+;
                   IIF(!EMPTY(lnStTrn) AND !EMPTY(lnEndTrn)," AND cTRAN_NO BETWEEN '"+lnStTrn+"' AND '"+lnEndTrn+"'",''),'GLTRNHD')
         SELECT GLTRNHD
         SCAN 
           WAIT WINDOW LANG_Collect_Data+GLTRNHD.CGLBATCHNO+LANG_GL_Transaction + GLTRNHD.cGLTRANNO NOWAIT 
           lcGLBatchNo = GLTRNHD.CGLBATCHNO
           lcGLTranNO = GLTRNHD.cGLTRANNO
           SELECT GLTRNDT
           =gfSetOrder('GLTRNDT') 
           =gfSeek(lcGLBatchNo+lcGLTranNO)
           SCAN REST WHILE CGLBATCHNO+CGLTRANNO+cTRAN_NO= lcGLBatchNo+lcGLTranNO FOR ;
                     IIF(llSelGLAcc,SEEK(cAcctCode,lcGLAccCur),.T.)
               
              m.cAcctCode = GLTRNDT.cAcctCode
              m.Tran_No = GLTRNDT.CTRAN_NO
              m.TRAN_DATE = GLTRNDT.TRAN_DATE 
              m.CGLREF = GLTRNDT.CGLREF
              m.GLSESSION = GLTRNDT.GLSESSION
              m.TRAN_TYPE = GLTRNDT.TRAN_TYPE 
              m.CGLBATCHNO = GLTRNDT.CGLBATCHNO 
              m.CGLTRANNO = GLTRNDT.CGLTRANNO 
              m.NAMOUNT = IIF(GLTRNDT.cDRORCR ='C',-1,1)*GLTRNDT.NAMOUNT 
              m.GLPeriod = GLTRNDT.CTRNPPRD
              m.GLFYEAR = GLTRNDT.CTRNPYR 
              m.TRAN_DESC = GLTRNHD.TRAN_DESC
              m.CCONT_ID = GLTRNHD.CCONT_ID 
              =gfSeek(m.cAcctCode,'GLACCHAR','ACCTCODE')
              m.CACCNLDES =GLACCHAR.CACCNLDES 
              INSERT INTO (lcGLTEMP) FROM MEMVAR
           ENDSCAN
         ENDSCAN 
      ENDIF
    ENDSCAN
  ENDIF
ELSE
  IF !EMPTY(lcGLBat)  
    SELECT GLTRNHD
    =gfSeek('GLTRNHD')
    IF gfSeek(lcGLBat+IIF(!EMPTY(lcGLTran),lcGLTran,''))
      SCAN REST WHILE CGLBATCHNO+CGLTRANNO = lcGLBat+IIF(!EMPTY(lcGLTran),lcGLTran,'') FOR ;
                   IIF(!EMPTY(lcStSession) AND !EMPTY(lcEndSession),BETWEEN(GLSESSION,lcStSession,lcEndSession),.T.) AND ;
                   IIF(!EMPTY(ldDateStart) AND !EMPTY(ldDateEnd),BETWEEN(DTRNPDATE,ldDateStart,ldDateEnd),.T.) AND ;
                   IIF(!EMPTY(lcTranType), TRAN_TYPE $ lcTranType,.T.) AND ;
                   IIF(!EMPTY(lnStTrn) AND !EMPTY(lnEndTrn),BETWEEN(cTRAN_NO,lnStTrn,lnEndTrn),.T.)
        WAIT WINDOW LANG_Collect_Data+GLTRNHD.CGLBATCHNO+LANG_GL_Transaction + GLTRNHD.cGLTRANNO NOWAIT            
        lcGLBatchNo = GLTRNHD.CGLBATCHNO
        lcGLTranNO = GLTRNHD.cGLTRANNO
        SELECT GLTRNDT
        =gfSetOrder('GLTRNDT') 
        =gfSeek(lcGLBatchNo+lcGLTranNO)
        SCAN REST WHILE CGLBATCHNO+CGLTRANNO+cTRAN_NO= lcGLBatchNo+lcGLTranNO FOR ;
                 IIF(llSelGLAcc,SEEK(cAcctCode,lcGLAccCur),.T.)
          m.cAcctCode = GLTRNDT.cAcctCode
          m.Tran_No = GLTRNDT.CTRAN_NO
          m.TRAN_DATE = GLTRNDT.TRAN_DATE 
          m.CGLREF = GLTRNDT.CGLREF
          m.GLSESSION = GLTRNDT.GLSESSION
          m.TRAN_TYPE = GLTRNDT.TRAN_TYPE 
          m.CGLBATCHNO = GLTRNDT.CGLBATCHNO 
          m.CGLTRANNO = GLTRNDT.CGLTRANNO 
          m.NAMOUNT = IIF(GLTRNDT.cDRORCR ='C',-1,1)*GLTRNDT.NAMOUNT 
          m.GLPeriod = GLTRNDT.CTRNPPRD
          m.GLFYEAR = GLTRNDT.CTRNPYR 
          m.TRAN_DESC = GLTRNHD.TRAN_DESC
          m.CCONT_ID = GLTRNHD.CCONT_ID 
          =gfSeek(m.cAcctCode,'GLACCHAR','ACCTCODE')
          m.CACCNLDES =GLACCHAR.CACCNLDES 
          INSERT INTO (lcGLTEMP) FROM MEMVAR
        ENDSCAN           
      ENDSCAN
    ENDIF
  ELSE
    SELECT GLTRNHD
    IF gfSqlRun("Select * FROM GLTRNHD Where 1=1 "+;
                   IIF(!EMPTY(lcStSession) AND !EMPTY(lcEndSession)," AND GLSESSION BETWEEN '"+lcStSession +"' AND '"+lcEndSession+"'","")+;
                   IIF(!EMPTY(ldDateStart) AND !EMPTY(ldDateEnd)," AND DTRNPDATE BETWEEN '"+DTOS(ldDateStart)+"' AND '"+DTOS(ldDateEnd)+"'","")+;
                   IIF(!EMPTY(lcTranType)," AND TRAN_TYPE IN  ("+lcTranType+")" ,'')+;
                   IIF(!EMPTY(lnStTrn) AND !EMPTY(lnEndTrn)," AND cTRAN_NO BETWEEN '"+lnStTrn+"' AND '"+lnEndTrn+"'",''),'GLTRNHD')
         SELECT GLTRNHD
         SCAN 
           WAIT WINDOW LANG_Collect_Data+GLTRNHD.CGLBATCHNO+LANG_GL_Transaction + GLTRNHD.cGLTRANNO NOWAIT 
           lcGLBatchNo = GLTRNHD.CGLBATCHNO
           lcGLTranNO = GLTRNHD.cGLTRANNO
           SELECT GLTRNDT
           =gfSetOrder('GLTRNDT') 
           =gfSeek(lcGLBatchNo+lcGLTranNO)
           SCAN REST WHILE CGLBATCHNO+CGLTRANNO+cTRAN_NO= lcGLBatchNo+lcGLTranNO FOR ;
                     IIF(llSelGLAcc,SEEK(cAcctCode,lcGLAccCur),.T.)
               
              m.cAcctCode = GLTRNDT.cAcctCode
              m.Tran_No = GLTRNDT.CTRAN_NO
              m.TRAN_DATE = GLTRNDT.TRAN_DATE 
              m.CGLREF = GLTRNDT.CGLREF
              m.GLSESSION = GLTRNDT.GLSESSION
              m.TRAN_TYPE = GLTRNDT.TRAN_TYPE 
              m.CGLBATCHNO = GLTRNDT.CGLBATCHNO 
              m.CGLTRANNO = GLTRNDT.CGLTRANNO 
              m.NAMOUNT = IIF(GLTRNDT.cDRORCR ='C',-1,1)*GLTRNDT.NAMOUNT 
              m.GLPeriod = GLTRNDT.CTRNPPRD
              m.GLFYEAR = GLTRNDT.CTRNPYR 
              m.TRAN_DESC = GLTRNHD.TRAN_DESC
              m.CCONT_ID = GLTRNHD.CCONT_ID 
              =gfSeek(m.cAcctCode,'GLACCHAR','ACCTCODE')
              m.CACCNLDES =GLACCHAR.CACCNLDES 
              INSERT INTO (lcGLTEMP) FROM MEMVAR
           ENDSCAN
         ENDSCAN 
      ENDIF 
  ENDIF
ENDIF

SELECT (lcGLTEMP)
LOCATE 
IF EOF()
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN
ENDIF
lfAdjustCRSettings()
IF USED(lcGLTEMP)
  USE IN (lcGLTEMP)
ENDIF


DO gfDispRe WITH EVAL('lcRpForm')

*!*************************************************************
*! Name      : lfCreateTmpGl
*: Developer : Mariam Mazhar (MMT)
*: Date      : 12/11/2019
*! Purpose   : Create Temp. File to collect data
*!*************************************************************
FUNCTION lfCreateTmpGl
DIMENSION laFileStru[14,4]

laFileStru[1,1] = 'cAcctCode'
laFileStru[1,2] = 'C'
laFileStru[1,3] = 24
laFileStru[1,4] = 0

laFileStru[2,1] = 'Tran_No'
laFileStru[2,2] = 'C'
laFileStru[2,3] = 12
laFileStru[2,4] = 0

laFileStru[3,1] = 'TRAN_DATE'
laFileStru[3,2] = 'D'
laFileStru[3,3] = 8
laFileStru[3,4] = 0

laFileStru[4,1] = 'CGLREF'
laFileStru[4,2] = 'C'
laFileStru[4,3] = 24
laFileStru[4,4] = 0

laFileStru[5,1] = 'GLSESSION'
laFileStru[5,2] ='C'
laFileStru[5,3] = 6
laFileStru[5,4] = 0

laFileStru[6,1] = 'TRAN_TYPE'
laFileStru[6,2] ='C'
laFileStru[6,3] = 2
laFileStru[6,4] = 0

laFileStru[7,1] = 'CGLBATCHNO'
laFileStru[7,2] = 'C'
laFileStru[7,3] = 6
laFileStru[7,4] = 0

laFileStru[8,1] = 'CGLTRANNO'
laFileStru[8,2] = 'C'
laFileStru[8,3] = 8
laFileStru[8,4] = 0

laFileStru[9,1] = 'NAMOUNT'
laFileStru[9,2] = 'N'
laFileStru[9,3] = 15
laFileStru[9,4] = 2

laFileStru[10,1] = 'GLPeriod'
laFileStru[10,2] = 'C'
laFileStru[10,3] = 2
laFileStru[10,4] = 0

laFileStru[11,1] ='GLFYEAR'
laFileStru[11,2] ='C'
laFileStru[11,3] = 4
laFileStru[11,4] = 0

laFileStru[12,1] = 'TRAN_DESC'
laFileStru[12,2] = 'C'
laFileStru[12,3] = 30
laFileStru[12,4] = 0

laFileStru[13,1] = 'CCONT_ID'
laFileStru[13,2] = 'C'
laFileStru[13,3] = 8
laFileStru[13,4] = 0

laFileStru[14,1] = 'CACCNLDES'
laFileStru[14,2] = 'C'
laFileStru[14,3] = 65
laFileStru[14,4] = 0


=gfCrtTmp(lcGLTEMP,@laFileStru,'TRAN_NO+cAcctCode',lcGLTEMP,.F.)


*************************************************************
*! Name      : lfAdjustCRSettings
*: Developer : Saber A.Razek (SAB)
*: Date      : 02/13/2012
*! Purpose   : To set the report data files and parameters
*!*************************************************************
PROCEDURE lfAdjustCRSettings

DIMENSION loOgScroll.laCRTables[1]
DIMENSION loOgScroll.laCRParams[4,2]

loOGScroll.cCROrientation='P'
loOgScroll.laCRTables[1] = oAriaApplication.WorkDir + lcGLTEMP + ".DBF"

LOCAL lnI
lnI  = 0
lnI = lnI + 1
loOgScroll.laCRParams[lnI, 1] = 'ReportName'
loOgScroll.laCRParams[lnI ,2] = LANG_GL_ENTRIES_REP&&IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_DISCOUNTS,oAriaApplication.GetHeaderText("LANG_DISCOUNTS",AHEADERFILE))

lnI = lnI + 1
loOgScroll.laCRParams[lnI, 1] = 'Layout'
loOgScroll.laCRParams[lnI, 2] = IIF(lcRpFormat ="D",LANG_GL_DETAIL,LANG_GL_SUMMARY)&&"Account\Store"

lnI = lnI + 1
loOgScroll.laCRParams[lnI, 1] = 'SortBy'
loOgScroll.laCRParams[lnI, 2] = lcRPSortBy 

* = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_VENDOR,oAriaApplication.GetHeaderText("LANG_VENDOR",AHEADERFILE))

lnI = lnI + 1
loOgScroll.laCRParams[lnI, 1] = 'OpTitle'
loOgScroll.laCRParams[lnI, 2] = lcRpTitle 

ENDPROC


*!*************************************************************
*! Name      : lfRepWhen
*: Developer : Mariam Mazhar (MMT)
*: Date      : 12/11/2019
*! Purpose   : OG when function
*!*************************************************************

FUNCTION lfRepWhen
IF !USED('GENERALLEDGERBATCH')
  =gfOpenTable('GENERALLEDGERBATCH','GLBATCHU','SH')
ENDIF
IF !USED('GLTRNDT')
=gfOpenTable('GLTRNDT','GLTRNDTU','SH')
ENDIF
IF !USED('GLTRNHD')
  =gfOpenTable('GLTRNHD','GLTRNHD','SH')
ENDIF  
IF !USED('accod')
  =gfOpenTable('accod','ACCSEGNO','SH')
ENDIF  
IF !USED('CUSTOMER')
  =gfOpenTable('CUSTOMER','CUSTOMER','SH')
ENDIF
IF !USED('APVENDOR')
  =gfOpenTable('APVENDOR','VENCODE','SH')
ENDIF
IF !USED('GLACCHAR')
  =gfOpenTable('GLACCHAR','ACCTCODE','SH')
ENDIF

*!*************************************************************
*! Name      : lfGetPic
*! Developer : Mariam Mazhar(MMT)
*! Date      : 12/11/2019
*! Purpose   : Get Gl Account Picture
*!*************************************************************
FUNCTION lfGetPic
lnOldAlias = SELECT(0)    && Save the current alias
 IF !USED('SYDFIELD')
    =gfOpenTable('SYDFIELD','CFLD_NAME','SH')
  ENDIF
IF !USED('ACCOD')
  =gfOpenTable('accod','ACCSEGNO','SH')
ENDIF
SELECT ACCOD
GO TOP
IF !EOF()
  lcRpSegMas = ALLTRIM(ACCOD.cacsmask)
  lcRpSegMas = STRTRAN(lcRpSegMas, '#', '9',2) 
  lcRpSegMas = STRTRAN(lcRpSegMas, '#', 'X',1,1) 
ELSE
  lcRpSegMas = " "
ENDIF

SELECT (lnOldAlias)
RETURN lcRpSegMas
*-- end of lfGetPic.

*!*************************************************************
*! Name      : lfvClrRead
*! Developer : Mariam Mazhar
*! Date      : 12/11/2019
*! Purpose   : validate Transaction Type
*!*************************************************************
FUNCTION lfvClrRead
CLEARREAD()

*!*************************************************************
*! Name      : lfAllTypes
*! Developer : Mariam Mazhar(MMT)
*! Date      : 12/11/2019
*! Purpose   : Fill type array with transactions types
*!*************************************************************
FUNCTION lfAllTypes

= lfGLFiles()

IF !llMultCurr
  DIMENSION laRPType[24,1],laRPTypeRet[24,1]
ELSE
  DIMENSION laRPType[25,1],laRPTypeRet[25,1]
ENDIF  

laRPTypeRet[1,1]  = 'IN'
laRPType[1,1]     = LANG_INVOICE
laRPTypeRet[2,1]  = 'VI'
laRPType[2,1]     = LANG_VOID_INV
laRPTypeRet[3,1]  = 'CR'
laRPType[3,1]     = LANG_CASH_REC
laRPTypeRet[4,1]  = 'CA'
laRPType[4,1]     = LANG_CRED_ADJ
laRPTypeRet[5,1]  = 'DA'
laRPType[5,1]     = LANG_DEB_ADJ
laRPTypeRet[6,1]  = 'RM'
laRPType[6,1]     = LANG_RET_MERCH
laRPTypeRet[7,1]  = 'VR'
laRPType[7,1]     = LANG_VOID_RET
laRPTypeRet[8,1]  = 'IP'
laRPType[8,1]     = LANG_INV_PHYISCAL
laRPTypeRet[9,1]  = 'IA'
laRPType[9,1]     =LANG_INV_ADJ
laRPTypeRet[10,1] = 'PO'
laRPType[10,1]    = LANG_PO_REC
laRPTypeRet[11,1] = 'CT'
laRPType[11,1]    =  LANG_CT_REC
laRPTypeRet[12,1] = 'ZE'
laRPType[12,1]    = LANG_ZERO_STK
laRPTypeRet[13,1] = 'MP'
laRPType[13,1]    = LANG_MA_INV_PHY
laRPTypeRet[14,1] = 'MA'
laRPType[14,1]    = LANG_MA_INV_ADJ
laRPTypeRet[15,1] = 'MO'
laRPType[15,1]    = LANG_MA_PO_REC

laRPTypeRet[16,1] = 'JC'
laRPType[16,1]    = LANG_PO_JOB_CLOSING

laRPTypeRet[17,1] = 'NL'
laRPType[17,1]    = LANG_NON_MAT_LIAB

laRPTypeRet[18,1] = 'KO'
laRPType[18,1]    = LANG_KEY_OFF

laRPTypeRet[19,1] = 'MC'
laRPType[19,1]    = LANG_MA_JOB_CLOS

laRPTypeRet[20,1] = 'JP'
laRPType[20,1]    = LANG_CT_HON_CLOS

laRPTypeRet[21,1] = 'PI'
laRPType[21,1]    = LANG_PAY_INVOICE

laRPTypeRet[22,1] = 'BA'
laRPType[22,1]    = LANG_BANK_ADJ

laRPTypeRet[23,1] = 'AP'
laRPType[23,1]    = LANG_AP_PAYMENT

laRPTypeRet[24,1] = 'DM'
laRPType[24,1]    = LANG_DEBIT_MEMO   



IF llMultCurr
  laRPTypeRet[25,1] = 'EX'
  laRPType[25,1]    = LANG_EX_DIFF
ENDIF  

*-- end of lfAllTypes.

*!*************************************************************
*! Name      : lfGLFiles
*! Developer : Mariam Mazhar(MMT)
*! Date      : 12/11/2019
*! Purpose   : open the GL_Link file account chart file according to 
*!             the GL version(ARIA,SBT,Others) and to the active company
*!*************************************************************
FUNCTION lfGLFiles

STORE SPACE(0) TO lcTypeDesc,lcGLAcc,lcGLDesc,laComp,lcGLChrTag,;
                  lcGLDir,lcFile,lcGLCo,lcCodeFld,lcDescFld,lcSysDir,;
                  lcGLVer,lcGLCo,lcOldVal,lcPrntCo,lcPrSysDir,;
                  lcPrGLVer,lcPrGLCo,lcPrGLDir,lpopcPrFile,;
                  lcGLChart,lcAcMask
                  
STORE .F. TO llOtherVer,llChldComp

STORE 0 To lnAcLen

PRIVATE llContinue

llContinue = .T.

IF EMPTY(oAriaApplication.ActiveCompanyID)
  llContinue = .F.
  *-- You have to select company first
  *-- <OK>
  = gfModalGen("INM00192B00000","Dialog")  
  llContinue = .F.
ENDIF

IF llContinue
  llGL_Link  = ALLTRIM(UPPER(gfGetMemVar('M_Link_GL',oAriaApplication.ActiveCompanyID)))   = 'Y'
  IF !llGL_Link 
    *-- System has not been linked to gl_link yet
    *-- <OK>
    = gfModalGen("INM00292B00000","Dialog")  
    llContinue = .F.
  ENDIF
ENDIF
IF llContinue
  IF !USED('SYCCOMP')
    =gfOpenTable('SYCCOMP','CCOMP_ID','SH')
  ENDIF
  
  llChldComp = gfSEEK(oAriaApplication.ActiveCompanyID,'SycComp') AND !EMPTY(SycComp.cCompPrnt)
  lcDataDir  = IIF(gfSEEK(oAriaApplication.ActiveCompanyID,'SycComp'),gfGetDataDir(ALLTRIM(SycComp.cCom_dDir)),'')
  lcPrntCo   = IIF(llChldComp,ALLTRIM(SycComp.cCompPrnt),'')
  lcGLVer    = ALLTRIM(UPPER(gfGetMemVar('M_GL_VERS',oAriaApplication.ActiveCompanyID)))
  lcGLCo     = ALLTRIM(UPPER(gfGetMemVar('M_GL_CO',oAriaApplication.ActiveCompanyID)))

  llMultCurr = gfGetMemVar("LLMULCURR",oAriaApplication.ActiveCompanyID)

  DO CASE
    *-- GL Version is SBT
    CASE lcGLVer = 'S'
      lcSBTGLDir = ALLTRIM(UPPER(gfGetMemVar('M_SYS_DIR',oAriaApplication.ActiveCompanyID)))
      USE (lcSBTGLDir+'SYSDATA') IN 0 AGAIN ALIAS (lcSysTmp)
      SELECT (lcSysTmp)
      LOCATE FOR SYSID = "GL" + lcGLCo
      IF !FOUND()
        *--lcInfoMsg = 'Company not found !!!'
        =gfModalGen('INM00269B00000','DIALOG')
        llContinue = .F.
      ELSE  &&FOUND
        *-- Get path for gl data and company name
        lcGLDir    = ALLTRIM(SUBSTR(DRIVE,61,30))         && DATA DIRECTORY PATH
        lcFile     = "GLACNT"+lcGLCo
        lcPrGLDir  = lcGLDir
      ENDIF
      USE IN (lcSysTmp)
      lcCodeFld   = 'GLACNT'       
      lcDescFld   = 'GLDESC'
      llOtherVer  = .F.

    *-- GL Version is ARIA
    CASE lcGLVer  = 'A'
      lcGLDir     = IIF(gfSEEK(IIF(llChldComp,lcPrntCo,oAriaApplication.ActiveCompanyID),'SycComp'),gfGetDataDir(ALLTRIM(SycComp.cCom_dDir)),'')
      lcFile      = "GLACCHAR"
      lcPrGLDir   = IIF(gfSEEK(lcPrntCo,'SycComp'),gfGetDataDir(ALLTRIM(SycComp.cCom_dDir)),'')
      lcCodeFld   = 'CACCTCODE'       
      lcDescFld   = 'CACCNLDES'
      llOtherVer  = .F.

    *-- Other type of GL version
    OTHERWISE
      lcGLDir     = IIF(SEEK(IIF(llChldComp,lcPrntCo,oAriaApplication.ActiveCompanyID),'SycComp'),gfGetDataDir(ALLTRIM(SycComp.cCom_dDir)),'')

      lcFile      = ''      
      lcCodeFld   = ''       
      lcDescFld   = ''
      llOtherVer  = .T.

  ENDCASE

  IF lcGLVer <> 'O'
    IF !FILE(IIF(llChldComp,lcPrGLDir,lcGLDir)+lcFile+'.DBF')
      *-- Chart of account file for this company not found !!!
      *-- <OK>
      = gfModalGen("INM00293B00000","Dialog")
      llContinue = .F.
    ELSE
      IF USED(lcFile)
        USE IN (lcFile)
      ENDIF
      USE (IIF(llChldComp,lcPrGLDir,lcGLDir)+lcFile) IN 0 AGAIN SHARED
      DO CASE
        CASE lcGLVer = 'S'
          SET ORDER TO GLACNT IN (lcFile)
        CASE lcGLVer = 'A'
          SET ORDER TO ACCTCODE IN (lcFile)        
      ENDCASE
    ENDIF    &&IF !FILE(IIF(llChldComp,lcPrGLDir,lcGLDir)+......
  ENDIF   &&IF lcGLVer <> 'O'
ENDIF  

IF llContinue
  lcFileName = lcFile
  lcFieldNam = lcDescFld
  *: E303960,1 AHH 1/04/2018 use the GLDIST table remotely not native, because of conversion to SQL[Start]
  *SET ORDER TO GLDISTNO IN GLDIST
*!*	  IF !USED('GLDIST')
*!*	    =gfOpenTable('GLDIST','GLDISTNO','SH')
*!*	  ENDIF
*!*	  SELECT GLDIST
*!*	  =gfSetOrder('GLDISTNO')
  *: E303960,1 AHH 1/04/2018 use the GLDIST table remotely not native, because of conversion to SQL[End]
ELSE
  llOgTrmnat = .T.
  CLEAR READ
ENDIF
*!*************************************************************
*! Name      : lfvTranNo
*! Developer : Mariam Mazhar(MMT)
*! Date      : 12/11/2019
*! Purpose   : validate tran. No.
*!*************************************************************
FUNCTION lfvTranNum
loFld = _screen.ActiveForm.ActiveControl

lcTranVal = loFld.Value
IF !EMPTY(lcTranVal) AND !gfSEEK(lcTranVal,'GLTRNDT','GLTRNDTNO')
  llNothing  = lfTranBrwse()
  loFld.Value = IIF(llNothing,GLTRNDT.cTran_No,'')
ENDIF  
*!*************************************************************
*! Name      : lfTranBrw
*! Developer : Mariam Mazhar(MMT)
*! Date      : 12/11/2019
*! Purpose   : Browse all transctions No.
*!*************************************************************
FUNCTION lfTranBrwse

PRIVATE lcFields,laBrow,lnCurAlias,lcCurTag,llReturn,lcTag,lcBrFields,lcFile_Ttl
DIMENSION laBrow[1]
STORE SPACE(0) TO lcFields,laBrow
llReturn = .F.

lnCurAlias = SELECT(0)

lcFields    = "linktype,link_code"

lcBrFields = "CTran_No   :H='"+LANG_TRANS_NO+"',"+;
             "Tran_Type :H='"+LANG_TRANS_TYPE +"',"+;
             "Tran_date :H='"+LANG_TRANS_DATE +"'"
             
lcFile_Ttl  = 'Tarn. No.'

SELECT GLTRNDT
=gfSetOrder('GLTRNDTNO')
=gfSeek('')
DECLARE laTemp[1]

llReturn  = gfBrows(.F., 'CTran_No', 'laTemp',lcFile_Ttl)

SELECT(lnCurAlias)

RETURN llReturn
*!*************************************************************
*! Name      : lfvSessionNo
*: Developer : Mariam Mazhar (MMT)
*: Date      : 12/11/2019
*! Purpose   : Session# validation
*!*************************************************************
FUNCTION lfvSessionNo
loFld = _screen.ActiveForm.ActiveControl


lcGLSessVal = loFld.Value
IF !EMPTY(lcGLSessVal) AND !gfSEEK(lcGLSessVal,'GLTRNHD','GLTRNHSE')
  llNothing  = lfGLSessBrw()
  loFld.Value = IIF(llNothing,GLTRNHD.GLSession,'')
ENDIF  
*!*************************************************************
*! Name      : lfGLSessBrw
*! Developer : Mariam Mazhar(MMT)
*! Date      : 12/11/2019
*! Purpose   : Browse all GL Sessions No.
*!*************************************************************
FUNCTION lfGLSessBrw

PRIVATE lcFields,laBrow,lnCurAlias,lcCurTag,llReturn,lcTag,lcBrFields,lcFile_Ttl
DIMENSION laBrow[1]
STORE SPACE(0) TO lcFields,laBrow
llReturn = .F.

lnCurAlias = SELECT(0)

lcFields    = "linktype,link_code"

lcBrFields = "GLSession :H='"+LANG_GL_SESSION+"',"+;
             "CGLBATCHNO :H='"+LANG_BATCH_NO+"',"+;
             "CGLTRANNO :H='"+LANG_TRAN_NO +"'"
             
lcFile_Ttl  = LANG_GL_SESSION

SELECT GLTRNHD
=gfSetOrder('GLTRNHSE')
=gfSeek('')
DECLARE laTemp[1]

llReturn  = gfBrows(.F., 'GLSession', 'laTemp',lcFile_Ttl)

SELECT(lnCurAlias)
*!*************************************************************
*! Name      : lfGLBATCH_LOCATE
*! Developer : Mariam Mazhar(MMT)
*! Date      : 12/11/2019
*! Purpose   : LOCATE record in GLBATCH file
*!*************************************************************
FUNCTION lfGLBATCH_LOCATE_File

SELECT GENERALLEDGERBATCH
=gfSetOrder('GLBATCHNO')
lnBatPos = ASUBSCRIPT(loogscroll.laOGFxFlt,ASCAN(loogscroll.laOGFxFlt,'GLTRNHD.CGLBATCHNO'),1)
IF !EMPTY(loogscroll.laOGFxFlt[lnBatPos,6])
  =gfSEEK(loogscroll.laOGFxFlt[lnBatPos,6])
ENDIF

************************************************************
*! Name      : lfBatchBrow
*! Developer : Mariam Mazhar(MMT)
*! Date      : 12/11/2019
*! Purpose   : Batch browse valid function 
************************************************************
FUNCTION lfGLBatchBrow

=lfGLBATCH_LOCATE_File()
IF !'GLREPFNC' $ UPPER(SET("Procedure"))
  SET PROCEDURE TO (loOgScroll.gcRepHome + 'GL\glrepfnc.fxp') ADDITIVE 
ENDIF  
*GFVLFLD('GLBATCH','CBATCHNO',VARREAD(),'',.F.,.F.,.F.,'[BATCHNO]',[FOR !(CSRCMODUL $ 'GL')] ,.T.))
lcBrFields    = "CGLBATCHNO :H='"+LANG_BATCH_NO+"',CBATSTAT :H='"+LANG_BATCH_STATUS+"',CBATTYPE :H='"+LANG_BATCH_TYPE +"',CBATPYR :H='"+LANG_POST_YEAR+"',DBATPBEG :H='"+LANG_BATCH_BEG_DATE +"',"+;
                "DBATPEND :H='"+LANG_BATCH_END_DATE+"',CBATREFER :H='"+LANG_BATCH_REFER+"',CBATDESC :H='"+LANG_BATCH_DESC+"',NBATCNTOT :H='"+LANG_BATCH_CONT_TOT+"',"+;
                "NBATOTDR :H='"+LANG_TOTAL_DEBIT_S +"',NBATOTCR :H='"+LANG_TOTAL_CREDIT_S +"',CSRCMODUL :H='"+LANG_SOURCE_MODULE +"',CCOMP_ID :H='"+LANG_COMP_ID+"'"
SELECT GENERALLEDGERBATCH
=gfSeek('')
lfGetBrow('CGLBATCHNO','GENERALLEDGERBATCH','GLBATCHNO',LANG_BATCH ,'CGLBATCHNO')
*!*************************************************************
*! Name      : lfvTrnCode
*! Developer : Mariam Mazhar(MMT)
*! Date      : 12/11/2019
*! Purpose   : validate Transaction Type
*!*************************************************************
FUNCTION lfvGLTrnCode

lfGLBATCH_LOCATE_File()

lnBatPos = ASUBSCRIPT(loogscroll.laOGFxFlt,ASCAN(loogscroll.laOGFxFlt,'GLTRNHD.CGLBATCHNO'),1)

loFld = _screen.ActiveForm.ActiveControl
lcOldVal = loFld.OldValue

IF ('?' $ loFld.Value OR loFld.Value<>loFld.OldValue) AND !EMPTY(loogscroll.laOGFxFlt[lnBatPos,6]) AND gfSeek(loogscroll.laOGFxFlt[lnBatPos,6],'GENERALLEDGERBATCH','GLBATCHNO')
  lcKey = loogscroll.laOGFxFlt[lnBatPos,6]+ loFld.Value
  IF !gfSeek(lcKey,'GLTRNHD','GLTRNHD')&&CGLBATCHNO+CGLTRANNO                                                                                                    
  lcBrFields = "CGLBATCHNO :H='"+LANG_BATCH_NO+"',CGLTRANNO :H='"+LANG_TRANS_NO+"',NTRNINDIC :H='"+LANG_TRAN_IND +"'"+;
             ",CTRNDESC :H='"+LANG_TRAN_DESC+"',CTRNREFER :H='"+LANG_TRAN_REF+"',DTRNPDATE :H='"+LANG_POST_DATE+"',CTRNPYR :H='"+LANG_POST_YEAR+"',"+;
             "CTRNPPRD :H='"+LANG_POST_PERIOD+"',CTRNSTAT :H='"+LANG_TRAN_STATUS+"',CTRNTYPE :H='"+LANG_TRAN_TYPE +"',CTRNREVER :H='"+LANG_REV_TRAN_ENTRY+"',"+;
             "DTRNREVDT :H='"+LANG_REV_DATE+"',CTRNREVYR :H='"+LANG_REV_YEAR+"',CTRNREVPR :H='"+LANG_REV_PERIOD+"',NTRNTOTDR :H='"+LANG_TOTAL_DEBIT+"',"+;
             "NTRNTOTCR :H='"+LANG_TOTAL_CREDIT+"',CSRCMODUL :H='"+LANG_SOURCE_MODULE +"',CSTANDARD :H='"+LANG_STANDARD_TYPE+"',CSRCJRNL :H='"+LANG_SOURCE_JRNL_ENTRY+"',"+;
             "CCOMP_ID :H='"+LANG_COMP_ID+"',CAUTTYPE :H='"+LANG_Automatic_entry_TYPE+"',CAUTCODE :H='"+LANG_Automatic_entry_code+"'"
    SELECT GLTRNHD
    =gfSeek(loogscroll.laOGFxFlt[lnBatPos,6])
    lfGetBrow('CGLTRANNO','GLTRNHD','GLTRNHD',LANG_GLTRAN_BATCH,'CGLTRANNO')
  ENDIF 
ENDIF
*!*************************************************************
*! Name      : lfvTrnType
*: Developer : Mariam Mazhar (MMT)
*: Date      : 12/11/2019
*! Purpose   : Transaction type option validation
*!*************************************************************
FUNCTION lfvTrnType
SET STEP ON 
= lfOGMover(@laRPType,@laRPTarType,LANG_TRAN_TYPE,.T.,'')  && call mover function.
lcRpTrType  = ''
*-- Loop to make Status expression.
IF !EMPTY(laRPTarType[1])
  FOR lnI = 1 TO ALEN(laRPTarType,1)
    lcRpTrType  = lcRpTrType +IIF(!EMPTY(lcRpTrType) ,"|","")+ laRPTypeRet[ASCAN(laRPType,laRPTarType[lnI])]
  ENDFOR
ENDIF
Clearread()
*!*************************************************************
*! Name      : RefreshType
*: Developer : Mariam Mazhar (MMT)
*: Date      : 12/11/2019
*! Purpose   : Refresh Type option display value
*!*************************************************************

FUNCTION RefreshType

  LOCAL lcStatusStr, lnTarget
  lcStatusStr = ""
  IF !EMPTY(laRPTarType)
    FOR lnTarget = 1 TO ALEN(laRPTarType,1)
      lcStatusStr = lcStatusStr + ", " + laRPTarType[lnTarget]
    ENDFOR 
    lcStatusStr = SUBSTR(lcStatusStr,3)
  ENDIF   
  RETURN lcStatusStr
ENDFUNC 