*:--------------------------------------------------------------------------
*: Program file        : SMFIXGL.PRG
*: Program description : Fix GL-Enteries for closing CutTkt/Po.
*: For System          : Aria Advantage Series - Version 2.7
*: For Module          : System Manager - (SM)
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : B603573
*:--------------------------------------------------------------------------
*: Calls               : None.
*:--------------------------------------------------------------------------
*: Called From         : - System Manager - (SM)
*:--------------------------------------------------------------------------
*: Passed Parameters   : lcDataDir = on the form  EX: 'W:\aria27\defs\99'
*:--------------------------------------------------------------------------
*: Example             : DO SMFIXGL.PRG
*:--------------------------------------------------------------------------
*: Modifications       :
*:--------------------------------------------------------------------------
*: Notes               : This fix program due to bug 603573 when closing
*:                     : CutTkt/Po and there is a deferant between landed
*:                     : and actual cost, these deferances are updated
*:										 : Uncorrect GL-entereies .
*:                     : So, We will scan for Closed C/T & PO and corect 
*:                     : the GL-E in GLDIST file.
*:--------------------------------------------------------------------------
PARAMETER lcDataDir

WAIT WINDOW "Fix Gl-Transaction for closed C/T and PO" NOWAIT

IF !lfvProcced()
  RETURN
ENDIF
=lfvCancel()
WAIT CLEAR
*:----------------------------------------------------------------
*: Name       : lfvProcced
*: Developer  : Ahmed Salah Shalaby - (SSH)
*: Date       : 18/04/200
*: Purpose    : Valid function for proceed button
*:----------------------------------------------------------------
*: Parameters : None
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfvProcced()
*:----------------------------------------------------------------
*:
FUNCTION lfvProcced
PRIVATE llCTXST,llPOEXST

llCTEXST = .T.
llPOEXST = .T.
*--- Check Existing of GLDIST FILE
IF FILE(lcDataDir+'GlDIST.DBF') 
  IF USED('GlDIST')
    USE IN GlDIST
  ENDIF
  USE lcDataDir+'GlDIST.DBF' IN 0 ORDER  Gldistno
ELSE
  RETURN(.F.)  
ENDIF

*--- Check Existing of Link File
IF FILE(lcDataDir+'Gl_Link.DBF') 
  IF USED('Gl_Link')
    USE IN Gl_Link
  ENDIF
  USE lcDataDir+'Gl_Link.DBF' IN 0 ORDER  Gl_Link
ELSE
   RETURN(.F.) 
ENDIF


*--- Check CutTkt File
IF FILE(lcDataDir+'CUTTKTH.DBF') 
  IF USED('CUTTKTH')
    USE IN CUTTKTH
  ENDIF
  USE lcDataDir+'CUTTKTH.DBF' IN 0 ORDER  CUTTKTH
ELSE
  llCTEXST = .F. 
ENDIF

*--- Check PO File
IF FILE(lcDataDir+'POSHDR.DBF') 
  IF USED('POSHDR')
    USE IN POSHDR
  ENDIF
  USE lcDataDir+'POSHDR.DBF' IN 0 ORDER  POSHDR
ELSE
  llPOEXST = .F. 
ENDIF
lcTempTran=gfTempName()
IF llCTEXST
  SELECT CutTktH
  GOTO TOP
  SCAN FOR Status='S'
    WAIT WINDOW "Fix CutTkt # "+CutTkt  NOWAIT
    SELECT GLDIST
    IF SEEK(CutTktH.CutTkt)
      SELECT GLDIST
      LOCATE REST WHILE tran_no+tran_type+glsession+catg_key=;
                        CutTktH.CutTkt
      IF FOUND()
        m.CutTkt = CutTktH.CutTkt
        SELECT * FROM GLDIST WHERE tran_no+tran_type+glsession+catg_key=;
                        m.CutTkt;
                  .AND.  catg_key <> '013' .AND. Tran_Type = 'JC' .AND. EMPTY(Posted);
                  INTO DBF &gcWorkDir.&lcTempTran
        SELECT (lcTempTran)
        GOTO TOP
        SCAN REST WHILE tran_no+tran_type+glsession+catg_key=;
                        CutTktH.CutTkt;
                  FOR  catg_key <> '013' .AND. Tran_Type = 'JC' .AND. EMPTY(Posted)
          DO CASE
            CASE  catg_key = '021'
              SELECT GL_Link
              =SEEK(CutTktH.Link_Code+'022')
              IF !EMPTY(GL_Link.GlAcnt)
                SELECT (lcTempTran)
                REPLACE Catg_Key WITH '022',;
                        GlAccount WITH GL_Link.GlAcnt
              ELSE
                =SEEK(CutTktH.Link_Code+'019')
                SELECT (lcTempTran)
                REPLACE Catg_Key WITH '019',;
                        GlAccount WITH GL_Link.GlAcnt
              ENDIF
              SELECT CutTktH
              REPLACE cvaracnt1 WITH GL_Link.GlAcnt
            CASE  catg_key = '022'
              SELECT GL_Link
              =SEEK(CutTktH.Link_Code+'023')
              IF !EMPTY(GL_Link.GlAcnt)
                SELECT (lcTempTran)
                REPLACE Catg_Key WITH '023',;
                        GlAccount WITH GL_Link.GlAcnt
              ELSE
                =SEEK(CutTktH.Link_Code+'019')
                SELECT (lcTempTran)
                REPLACE Catg_Key WITH '019',;
                        GlAccount WITH GL_Link.GlAcnt
              ENDIF
              SELECT CutTktH
              REPLACE cvaracnt2 WITH GL_Link.GlAcnt
            CASE  catg_key = '023'
              SELECT GL_Link
              =SEEK(CutTktH.Link_Code+'024')
              IF !EMPTY(GL_Link.GlAcnt)
                SELECT (lcTempTran)
                REPLACE Catg_Key WITH '024',;
                        GlAccount WITH GL_Link.GlAcnt
              ELSE
                =SEEK(CutTktH.Link_Code+'019')
                SELECT (lcTempTran)
                REPLACE Catg_Key WITH '019',;
                        GlAccount WITH GL_Link.GlAcnt
              ENDIF
              SELECT CutTktH
              REPLACE cvaracnt3 WITH GL_Link.GlAcnt
            CASE  catg_key = '024'
              SELECT GL_Link
              =SEEK(CutTktH.Link_Code+'025')
              IF !EMPTY(GL_Link.GlAcnt)
                SELECT (lcTempTran)
                REPLACE Catg_Key WITH '025',;
                        GlAccount WITH GL_Link.GlAcnt
              ELSE
                =SEEK(CutTktH.Link_Code+'019')
                SELECT (lcTempTran)
                REPLACE Catg_Key WITH '019',;
                        GlAccount WITH GL_Link.GlAcnt
              ENDIF
              SELECT CutTktH
              REPLACE cvaracnt4 WITH GL_Link.GlAcnt
            CASE  catg_key = '025'
              SELECT GL_Link
              =SEEK(CutTktH.Link_Code+'026')
              IF !EMPTY(GL_Link.GlAcnt)
                SELECT (lcTempTran)
                REPLACE Catg_Key WITH '026',;
                        GlAccount WITH GL_Link.GlAcnt
              ELSE
                =SEEK(CutTktH.Link_Code+'019')
                SELECT (lcTempTran)
                REPLACE Catg_Key WITH '019',;
                        GlAccount WITH GL_Link.GlAcnt
              ENDIF
              SELECT CutTktH
              REPLACE cvaracnt5 WITH GL_Link.GlAcnt
          ENDCASE
          SELECT (lcTempTran)
        ENDSCAN
        SELECT GLDIST
        LOCATE REST WHILE tran_no+tran_type+glsession+catg_key=;
                          CutTktH.CutTkt;
                    FOR   catg_key <> '013' .AND. Tran_Type = 'JC'
        DELETE REST FOR tran_no+tran_type+glsession+catg_key=;
                        CutTktH.CutTkt;
                  .AND.  catg_key <> '013' .AND. Tran_Type = 'JC' .AND. EMPTY(Posted)
        APPEND FROM &gcWorkDir.&lcTempTran
        SELECT (lcTempTran)
        ZAP
      ENDIF
    ENDIF
  ENDSCAN
ENDIF

IF llPOEXST 
  SELECT POSHDR
  SCAN FOR STATUS = 'S'
    SELECT GLDIST
    IF SEEK(POSHDR.PO)
      SELECT GLDIST
      LOCATE REST WHILE tran_no+tran_type+glsession+catg_key=;
                        POSHDR.PO;
                  FOR  catg_key <> '013' .AND. Tran_Type = 'JC' .AND. EMPTY(Posted)
      IF FOUND()
        m.PO = POSHDR.PO
        SELECT * FROM GLDIST WHERE tran_no+tran_type+glsession+catg_key=;
                        m.PO;
                  .AND.  catg_key <> '013' .AND. Tran_Type = 'JC' .AND. EMPTY(Posted);
                  INTO DBF &gcWorkDir.&lcTempTran
        SELECT (lcTempTran)
        GOTO TOP
        SCAN REST WHILE tran_no+tran_type+glsession+catg_key=;
                        m.PO;
                  FOR  catg_key <> '013' .AND. Tran_Type = 'JC' .AND. EMPTY(Posted)
          DO CASE
            CASE  catg_key = '021'
              SELECT GL_Link
              =SEEK(POSHDR.Link_Code+'022')
              IF !EMPTY(GL_Link.GlAcnt)
                SELECT (lcTempTran)
                REPLACE Catg_Key WITH '022',;
                        GlAccount WITH GL_Link.GlAcnt
              ELSE
                =SEEK(POSHDR.Link_Code+'019')
                SELECT (lcTempTran)
                REPLACE Catg_Key WITH '019',;
                        GlAccount WITH GL_Link.GlAcnt
              ENDIF
              SELECT POSHDR
              REPLACE cvaracnt1 WITH GL_Link.GlAcnt
            CASE  catg_key = '022'
              SELECT GL_Link
              =SEEK(POSHDR.Link_Code+'023')
              IF !EMPTY(GL_Link.GlAcnt)
                SELECT (lcTempTran)
                REPLACE Catg_Key WITH '023',;
                        GlAccount WITH GL_Link.GlAcnt
              ELSE
                =SEEK(POSHDR.Link_Code+'019')
                SELECT (lcTempTran)
                REPLACE Catg_Key WITH '019',;
                        GlAccount WITH GL_Link.GlAcnt
              ENDIF
              SELECT POSHDR
              REPLACE cvaracnt2 WITH GL_Link.GlAcnt
            CASE  catg_key = '023'
              SELECT GL_Link
              =SEEK(POSHDR.Link_Code+'024')
              IF !EMPTY(GL_Link.GlAcnt)
                SELECT (lcTempTran)
                REPLACE Catg_Key WITH '024',;
                        GlAccount WITH GL_Link.GlAcnt
              ELSE
                =SEEK(POSHDR.Link_Code+'019')
                SELECT (lcTempTran)
                REPLACE Catg_Key WITH '019',;
                        GlAccount WITH GL_Link.GlAcnt
              ENDIF
              SELECT POSHDR
              REPLACE cvaracnt3 WITH GL_Link.GlAcnt
            CASE  catg_key = '024'
              SELECT GL_Link
              =SEEK(POSHDR.Link_Code+'025')
              IF !EMPTY(GL_Link.GlAcnt)
                SELECT (lcTempTran)
                REPLACE Catg_Key WITH '025',;
                        GlAccount WITH GL_Link.GlAcnt
              ELSE
                =SEEK(POSHDR.Link_Code+'019')
                SELECT (lcTempTran)
                REPLACE Catg_Key WITH '019',;
                        GlAccount WITH GL_Link.GlAcnt
              ENDIF
              SELECT POSHDR
              REPLACE cvaracnt4 WITH GL_Link.GlAcnt
            CASE  catg_key = '025'
              SELECT GL_Link
              =SEEK(POSHDR.Link_Code+'026')
              IF !EMPTY(GL_Link.GlAcnt)
                SELECT (lcTempTran)
                REPLACE Catg_Key WITH '026',;
                        GlAccount WITH GL_Link.GlAcnt
              ELSE
                =SEEK(POSHDR.Link_Code+'019')
                SELECT (lcTempTran)
                REPLACE Catg_Key WITH '019',;
                        GlAccount WITH GL_Link.GlAcnt
              ENDIF
              SELECT POSHDR
              REPLACE cvaracnt5 WITH GL_Link.GlAcnt
          ENDCASE
          SELECT (lcTempTran)
        ENDSCAN
        SELECT GLDIST
        LOCATE REST WHILE tran_no+tran_type+glsession+catg_key=;
                          POSHDR.PO;
                    FOR   catg_key <> '013' .AND. Tran_Type = 'JC'
        DELETE REST FOR tran_no+tran_type+glsession+catg_key=;
                        POSHDR.PO;
                  .AND.  catg_key <> '013' .AND. Tran_Type = 'JC' .AND. EMPTY(Posted)
        APPEND FROM &gcWorkDir.&lcTempTran
        SELECT (lcTempTran)
        ZAP
      ENDIF
    ENDIF     
  ENDSCAN
ENDIF
*:----------------------------------------------------------------
*: Name       : lfvCancel
*: Developer  : Ahmed Salah Shalaby - (SSH)
*: Date       : 18/04/200
*: Purpose    : Valid function for Cancel button
*:----------------------------------------------------------------
*: Parameters : None
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfvCancel()
*:----------------------------------------------------------------
*:
FUNCTION lfvCancel

IF USED('GlDIST')
  USE IN GlDIST
ENDIF
IF USED('Gl_Link')
  USE IN Gl_Link
ENDIF
IF USED('CUTTKTH')
  USE IN CUTTKTH
ENDIF
IF USED('POSHDR')
  USE IN POSHDR
ENDIF
