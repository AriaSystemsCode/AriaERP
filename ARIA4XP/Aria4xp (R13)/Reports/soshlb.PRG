*:************************************************************************
*: Program file  : SOSHLB.Prg
*: Program desc. : SHIPPING LABELS REPORT
*: System        : Aria Advantage Series VER. 4XP
*: Module        : SO,AR,AL
*: Developer     : Heba Mohamed Amin(HMA)
*: Date          : 05/20/2005
*:************************************************************************
*: Calls :
*:    Procedures : ....
*:    Functions  : lfwOGWhen(), lfSRVLBL(), lfPikData(), lfInvData(),
*:                 lfOrdData(), lfvLabels(), lfClearRep()
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************
*: Example : DO SOSHLB
*:************************************************************************
*: Modifications:
*! B608335,1 MMT 10/29/2007 Fix bug of error while printing labels [T20071016.0010]
*! E303086,1 New Shipping label form T[T20120216.0114]
*:************************************************************************
#INCLUDE R:\Aria4xp\reports\soshlb.H
IF RECCOUNT(lclblTbl) = 0
  *-- Message 'There are no records to display...!'
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN
ENDIF

*IF loOGScroll.llOGFltCh   &&If Filter Changed
  *-- Variable to open the customer file again with it.
  lcCust_A  = loOgScroll.gfTempName()

  *-- The main temporary file we collect data in it.
  lcLblTemp = loOgScroll.gfTempName()

  *-- Open Customer file again to get the Distribution center store addresses
  *-- from , because Customer file is engaged in a relation of the main store.
  *! B608335,1 MMT 10/29/2007 Fix bug of error while printing labels[Start]
  *lolcCust_A =CreateObject("RemoteTable","CUSTOMER","CUSTOMER","LCCUST_A",SET("Datasession"))
  lolcCust_A =CreateObject("RemoteTable","CUSTOMER","CUSTOMER",LCCUST_A,SET("Datasession"))
  *! B608335,1 MMT 10/29/2007 Fix bug of error while printing labels[End]

  STORE SPACE(0) TO lcCmAdr1, lcCmAdr2, lcCmAdr3, lcCmAdr4, lcAddr1 ,lcAddr2,;
                    lcAddr3,lcActNam, lcAcct, lcMainF

  *-- Open Syccomp file remotly  (System file)
  *-- Get company Address
  PRIVATE  lcSqlCommand , lnResult
  lcSqlCommand=[SELECT cCom_Name,cCont_Code,cCom_Phon,cAddress1,cAddress2,cAddress3,cAddress4,cAddress5,cAddress6 FROM SYCCOMP WHERE Ccomp_id=']+oAriaApplication.ActiveCompanyID+[']
  lnResult  = oAriaApplication.remotesystemdata.execute(lcSqlCommand,"","SYCCOMP","",oAriaApplication.SystemConnectionString,3,"",SET("DATASESSION"))
  * lcPhonPict = gfPhoneTem()          && Variable to hold the Company Phone Format
  *-- Get the setting of print Company/warehouse addresses.

  lcPrtAdr = IIF(TYPE('lcPrtAdr') = 'C', lcPrtAdr, 'C')
  IF lnResult >= 1
    IF lcPrtAdr  = 'C'
      lcCompName = cCom_Name
      *-- Get the company addresses
      lcCmAdr1 = gfGetAdr('SYCCOMP' , '' , '' , '' , 1)
      lcCmAdr2 = gfGetAdr('SYCCOMP' , '' , '' , '' , 2)
      lcCmAdr3 = gfGetAdr('SYCCOMP' , '' , '' , '' , 3)
      lcCmAdr4 = gfGetAdr('SYCCOMP' , '' , '' , '' , 4)
      lcCmAdr5 = gfGetAdr('SYCCOMP' , '' , '' , '' , 5)
    ENDIF
  ENDIF

  DO CASE
    *-- Current module is Sales Order Allocation.
    CASE lcXTYPE = 'P'
      SELECT PIKTKT
      =AFIELDS(laFileStru)
      IF ASCAN(laFileStru,'LABELS') = 0
        lnFileStru = ALEN(laFileStru,1)
        DIMENSION laFileStru[lnFileStru+1,18]
        lnFileStru = lnFileStru+1
        laFileStru[lnFileStru,1] = 'LABELS'   &&Number of labels required
        laFileStru[lnFileStru,2] = 'N'
        laFileStru[lnFileStru,3] = 4
        laFileStru[lnFileStru,4] = 0
      ENDIF
      lnFileStru = ALEN(laFileStru,1)
      DIMENSION laFileStru[lnFileStru+1,18]
      lnFileStru = lnFileStru+1
      laFileStru[lnFileStru,1] = 'nLblsNo'    &&Label Number
      laFileStru[lnFileStru,2] = 'N'
      laFileStru[lnFileStru,3] = 4
      laFileStru[lnFileStru,4] = 0

      FOR lncnt=7 TO 16
        FOR lnInc=1 TO lnFileStru
          STORE SPACE(0) TO laFileStru[lnInc,lnCnt],laFileStru[lnInc,lnCnt]
        ENDFOR
      ENDFOR
      FOR lnInc=1 TO lnFileStru
        STORE 0 TO laFileStru[lnInc,17],laFileStru[lnInc,18]
      ENDFOR
      *! E303086,1 New Shipping label form T[T20120216.0114][Start]
      IF lcFormName = 'SOSHLBT'
        lnFileStru = ALEN(laFileStru,1)
        DIMENSION laFileStru[lnFileStru+1,18]
        lnFileStru = lnFileStru+1
        laFileStru[lnFileStru,1] = 'cCrtRange'    &&Label Number
        laFileStru[lnFileStru,2] = 'C'
        laFileStru[lnFileStru,3] = 50
        laFileStru[lnFileStru,4] = 0
        FOR lncnt=7 TO 16
          FOR lnInc=1 TO lnFileStru
            STORE SPACE(0) TO laFileStru[lnInc,lnCnt],laFileStru[lnInc,lnCnt]
          ENDFOR
        ENDFOR
        FOR lnInc=1 TO lnFileStru
          STORE 0 TO laFileStru[lnInc,17],laFileStru[lnInc,18]
        ENDFOR
      ENDIF
      *! E303086,1 New Shipping label form T[T20120216.0114][ENd]
      =gfCrtTmp(lcLblTemp,@laFileStru,'PIKTKT',lcLblTemp,.F.)
      *! E303086,1 New Shipping label form T[T20120216.0114][Start]
      IF lcFormName = 'SOSHLBT'
        DIMENSION laTmpCrtStr[7,4]
        laTmpCrtStr[1,1] = 'PIKTKT'
        laTmpCrtStr[1,2] = 'C'
        laTmpCrtStr[1,3] = 6
        laTmpCrtStr[1,4] = 0

        laTmpCrtStr[2,1] = 'nLblsNo'
        laTmpCrtStr[2,2] = 'N'
        laTmpCrtStr[2,3] = 4
        laTmpCrtStr[2,4] = 0

        laTmpCrtStr[3,1] = 'Style'
        laTmpCrtStr[3,2] = 'C'
        laTmpCrtStr[3,3] = 19
        laTmpCrtStr[3,4] = 0

        laTmpCrtStr[4,1] = 'CDIM1'
        laTmpCrtStr[4,2] = 'C'
        laTmpCrtStr[4,3] = 5
        laTmpCrtStr[4,4] = 0

        laTmpCrtStr[5,1] = 'Size'
        laTmpCrtStr[5,2] = 'C'
        laTmpCrtStr[5,3] = 5
        laTmpCrtStr[5,4] = 0

        laTmpCrtStr[6,1] = 'Qty'
        laTmpCrtStr[6,2] = 'N'
        laTmpCrtStr[6,3] = 6
        laTmpCrtStr[6,4] = 0

        laTmpCrtStr[7,1] = 'SCLCNT'
        laTmpCrtStr[7,2] = 'N'
        laTmpCrtStr[7,3] = 1
        laTmpCrtStr[7,4] = 0

        =gfCrtTmp(lcCrtTmp,@laTmpCrtStr,'PIKTKT+STR(nLblsNo,4)+STYLE+STR(SCLCNT,1)',lcCrtTmp,.F.)
      ENDIF
      *! E303086,1 New Shipping label form T[T20120216.0114][END]
      IF !lfpikData()
        RETURN
      ENDIF

    *-- Current module is Accounts Receivable.
    CASE lcXTYPE = 'I'
      SELECT INVHDR
      =AFIELDS(laFileStru)
      IF ASCAN(laFileStru,'LABELS') = 0
        lnFileStru = ALEN(laFileStru,1)
        DIMENSION laFileStru[lnFileStru+1,18]
        lnFileStru = lnFileStru+1
        laFileStru[lnFileStru,1] = 'LABELS'    &&Number of labels required
        laFileStru[lnFileStru,2] = 'N'
        laFileStru[lnFileStru,3] = 4
        laFileStru[lnFileStru,4] = 0
      ENDIF
      lnFileStru = ALEN(laFileStru,1)
      DIMENSION laFileStru[lnFileStru+1,18]
      lnFileStru = lnFileStru+1
      laFileStru[lnFileStru,1] = 'nLblsNo'      &&Label Number
      laFileStru[lnFileStru,2] = 'N'
      laFileStru[lnFileStru,3] = 4
      laFileStru[lnFileStru,4] = 0
      FOR lncnt=7 TO 16
        FOR lnInc=1 TO lnFileStru
          STORE SPACE(0) TO laFileStru[lnInc,lnCnt],laFileStru[lnInc,lnCnt]
        ENDFOR
      ENDFOR
      FOR lnInc=1 TO lnFileStru
        STORE 0 TO laFileStru[lnInc,17],laFileStru[lnInc,18]
      ENDFOR
      *! E303086,1 New Shipping label form T[T20120216.0114][Start]
      IF lcFormName = 'SOSHLBT'
        lnFileStru = ALEN(laFileStru,1)
        DIMENSION laFileStru[lnFileStru+1,18]
        lnFileStru = lnFileStru+1
        laFileStru[lnFileStru,1] = 'cCrtRange'    &&Label Number
        laFileStru[lnFileStru,2] = 'C'
        laFileStru[lnFileStru,3] = 50
        laFileStru[lnFileStru,4] = 0
        FOR lncnt=7 TO 16
          FOR lnInc=1 TO lnFileStru
            STORE SPACE(0) TO laFileStru[lnInc,lnCnt],laFileStru[lnInc,lnCnt]
          ENDFOR
        ENDFOR
        FOR lnInc=1 TO lnFileStru
          STORE 0 TO laFileStru[lnInc,17],laFileStru[lnInc,18]
        ENDFOR
      ENDIF
      *! E303086,1 New Shipping label form T[T20120216.0114][END]

      =gfCrtTmp(lcLblTemp,@laFileStru,'INVOICE+STORE',lcLblTemp,.F.)
      *! E303086,1 New Shipping label form T[T20120216.0114][Start]
      IF lcFormName = 'SOSHLBT'
        DIMENSION laTmpCrtStr[8,4]
        laTmpCrtStr[1,1] = 'INVOICE'
        laTmpCrtStr[1,2] = 'C'
        laTmpCrtStr[1,3] = 6
        laTmpCrtStr[1,4] = 0

        laTmpCrtStr[2,1] = 'nLblsNo'
        laTmpCrtStr[2,2] = 'N'
        laTmpCrtStr[2,3] = 4
        laTmpCrtStr[2,4] = 0

        laTmpCrtStr[3,1] = 'Style'
        laTmpCrtStr[3,2] = 'C'
        laTmpCrtStr[3,3] = 19
        laTmpCrtStr[3,4] = 0

        laTmpCrtStr[4,1] = 'CDIM1'
        laTmpCrtStr[4,2] = 'C'
        laTmpCrtStr[4,3] = 5
        laTmpCrtStr[4,4] = 0

        laTmpCrtStr[5,1] = 'Size'
        laTmpCrtStr[5,2] = 'C'
        laTmpCrtStr[5,3] = 5
        laTmpCrtStr[5,4] = 0

        laTmpCrtStr[6,1] = 'Qty'
        laTmpCrtStr[6,2] = 'N'
        laTmpCrtStr[6,3] = 6
        laTmpCrtStr[6,4] = 0

        laTmpCrtStr[7,1] = 'STORE'
        laTmpCrtStr[7,2] = 'C'
        laTmpCrtStr[7,3] = 8
        laTmpCrtStr[7,4] = 0

        laTmpCrtStr[8,1] = 'SCLCNT'
        laTmpCrtStr[8,2] = 'N'
        laTmpCrtStr[8,3] = 1
        laTmpCrtStr[8,4] = 0
        =gfCrtTmp(lcCrtTmp,@laTmpCrtStr,'INVOICE+STORE+STR(nLblsNo,4)+STYLE+STR(SCLCNT,1)',lcCrtTmp,.F.)
      ENDIF
      *! E303086,1 New Shipping label form T[T20120216.0114][END]

      IF !lfInvData()
        RETURN
      ENDIF

    *-- Current module is Sales Order.
    CASE lcXTYPE = 'O'
      SELECT ORDHDR
      =AFIELDS(laFileStru)
      IF ASCAN(laFileStru,'LABELS') = 0
        lnFileStru = ALEN(laFileStru,1)
        DIMENSION laFileStru[lnFileStru+1,18]
        lnFileStru = lnFileStru+1
        laFileStru[lnFileStru,1] = 'LABELS'
        laFileStru[lnFileStru,2] = 'N'
        laFileStru[lnFileStru,3] = 4
        laFileStru[lnFileStru,4] = 0
      ENDIF
      lnFileStru = ALEN(laFileStru,1)
      DIMENSION laFileStru[lnFileStru+1,18]
      lnFileStru = lnFileStru+1
      laFileStru[lnFileStru,1] = 'nLblsNo'
      laFileStru[lnFileStru,2] = 'N'
      laFileStru[lnFileStru,3] = 4
      laFileStru[lnFileStru,4] = 0
      FOR lncnt=7 TO 16
        FOR lnInc=1 TO lnFileStru
          STORE SPACE(0) TO laFileStru[lnInc,lnCnt],laFileStru[lnInc,lnCnt]
        ENDFOR
      ENDFOR
      FOR lnInc=1 TO lnFileStru
        STORE 0 TO laFileStru[lnInc,17],laFileStru[lnInc,18]
      ENDFOR

      *! E303086,1 New Shipping label form T[T20120216.0114][Start]
      IF lcFormName = 'SOSHLBT'
        lnFileStru = ALEN(laFileStru,1)
        DIMENSION laFileStru[lnFileStru+2,18]
        lnFileStru = lnFileStru+1
        laFileStru[lnFileStru,1] = 'cCrtRange'    &&Label Number
        laFileStru[lnFileStru,2] = 'C'
        laFileStru[lnFileStru,3] = 50
        laFileStru[lnFileStru,4] = 0
        lnFileStru = lnFileStru+1
        laFileStru[lnFileStru,1] = 'PIKTKT'    &&Label Number
        laFileStru[lnFileStru,2] = 'C'
        laFileStru[lnFileStru,3] = 6
        laFileStru[lnFileStru,4] = 0
        FOR lncnt=7 TO 16
          FOR lnInc=1 TO lnFileStru
            STORE SPACE(0) TO laFileStru[lnInc,lnCnt],laFileStru[lnInc,lnCnt]
          ENDFOR
        ENDFOR
        FOR lnInc=1 TO lnFileStru
          STORE 0 TO laFileStru[lnInc,17],laFileStru[lnInc,18]
        ENDFOR
      ENDIF
      *! E303086,1 New Shipping label form T[T20120216.0114][END]

      =gfCrtTmp(lcLblTemp,@laFileStru,'ORDER+STORE',lcLblTemp,.F.)
      *! E303086,1 New Shipping label form T[T20120216.0114][Start]
      IF lcFormName = 'SOSHLBT'
        DIMENSION laTmpCrtStr[8,4]
        laTmpCrtStr[1,1] = 'ORDER'
        laTmpCrtStr[1,2] = 'C'
        laTmpCrtStr[1,3] = 6
        laTmpCrtStr[1,4] = 0

        laTmpCrtStr[2,1] = 'nLblsNo'
        laTmpCrtStr[2,2] = 'N'
        laTmpCrtStr[2,3] = 4
        laTmpCrtStr[2,4] = 0

        laTmpCrtStr[3,1] = 'Style'
        laTmpCrtStr[3,2] = 'C'
        laTmpCrtStr[3,3] = 19
        laTmpCrtStr[3,4] = 0

        laTmpCrtStr[4,1] = 'CDIM1'
        laTmpCrtStr[4,2] = 'C'
        laTmpCrtStr[4,3] = 5
        laTmpCrtStr[4,4] = 0

        laTmpCrtStr[5,1] = 'Size'
        laTmpCrtStr[5,2] = 'C'
        laTmpCrtStr[5,3] = 5
        laTmpCrtStr[5,4] = 0

        laTmpCrtStr[6,1] = 'Qty'
        laTmpCrtStr[6,2] = 'N'
        laTmpCrtStr[6,3] = 6
        laTmpCrtStr[6,4] = 0

        laTmpCrtStr[7,1] = 'STORE'
        laTmpCrtStr[7,2] = 'C'
        laTmpCrtStr[7,3] = 8
        laTmpCrtStr[7,4] = 0

        laTmpCrtStr[8,1] = 'SCLCNT'
        laTmpCrtStr[8,2] = 'N'
        laTmpCrtStr[8,3] = 7
        laTmpCrtStr[8,4] = 0

        =gfCrtTmp(lcCrtTmp,@laTmpCrtStr,'ORDER+STORE+STR(nLblsNo,4)+STYLE+STR(SCLCNT,1)',lcCrtTmp,.F.)
      ENDIF
      *! E303086,1 New Shipping label form T[T20120216.0114][END]

      *-- Fill the temprary file with report data
      IF !lfOrdData()
        RETURN
      ENDIF
  ENDCASE

  SELECT (lcLblTemp)
  SET RELATION TO 'O'+ORDER INTO ORDHDR, IIF(STORE=SPACE(8) ,;
          'M'+ACCOUNT, 'S'+ACCOUNT+STORE) INTO CUSTOMER


  *Create a new temporary table and insert records equal to number of labels
  lcMainF = loOgScroll.gfTempName()
  *HMA
  SELECT (lcLblTemp)
  =AFIELDS(laFileStru)
  lcTmpIndex=IIF(lcXTYPE = 'O','ORDER+STORE',IIF(lcXTYPE = 'I','INVOICE+STORE','PIKTKT'))
  =gfCrtTmp(lcMainF,@laFileStru,lcTmpIndex,lcMainF,.F.)
*  COPY STRUCTURE TO (oAriaApplication.WorkDir+lcMainF) WITH CDX
*  USE (oAriaApplication.WorkDir+lcMainF) IN 0 ORDER 1
  *HMA
  *! E303086,1 New Shipping label form T[T20120216.0114][STart]
  * SCAN FOR !EOF('ORDHDR') .AND. LABELS # 0
  SCAN FOR !EOF('ORDHDR') .AND. IIF(lcFormName = 'SOSHLBT' ,!EMPTY(&lcLblTemp..cCrtRange),LABELS # 0)
  *! E303086,1 New Shipping label form T[T20120216.0114][END]
    WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soshlb_Printing,oAriaApplication.GetHeaderText("LANG_Soshlb_Printing",AHEADERFILE))+' '+;
                IIF(lcXType='O',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soshlb_Order,oAriaApplication.GetHeaderText("LANG_Soshlb_Order",AHEADERFILE))+' '+ORDHDR.Order,;
                IIF(lcXType='I',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soshlb_invoice,oAriaApplication.GetHeaderText("LANG_Soshlb_invoice",AHEADERFILE))+' '+Invoice,;
                IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soshlb_pick_ticket,oAriaApplication.GetHeaderText("LANG_Soshlb_pick_ticket",AHEADERFILE))+' '+PIKTKT)) NOWAIT
    *-- Print Required number of labels
    *! E303086,1 New Shipping label form T[T20120216.0114][Start]
    IF lcFormName = 'SOSHLBT'
      SCATTER MEMVAR
      DIMENSION laCrtRang[1]
      =gfSubstr(&lcLblTemp..cCrtRange,@laCrtRang,',')
      IF !EMPTY(laCrtRang[1])
        FOR lnTCnt = 1 TO ALEN(laCrtRang,1)
          IF '-' $ laCrtRang[lnTCnt]
            DIMENSION laInCrtRang[1]
            =gfSubstr(laCrtRang[lnTCnt],@laInCrtRang,'-')
		    IF !EMPTY(laInCrtRang[1]) AND !EMPTY(laInCrtRang[2])
		      FOR lnInCt = VAL(laInCrtRang[1]) TO VAL(laInCrtRang[2])
		        IF  lnInCt  = 0
		          LOOP
		        ENDIF
	            m.nLblsNo = lnInCt
    	        INSERT INTO (lcMainF) FROM MEMVAR
		      ENDFOR
		    ENDIF
          ELSE
           IF !EMPTY(laCrtRang[lnTCnt]) AND VAL(laCrtRang[lnTCnt]) <> 0
             m.nLblsNo = VAL(laCrtRang[lnTCnt])
             INSERT INTO (lcMainF) FROM MEMVAR
           ENdIF
          ENDIF
        ENDFOR
      ENDIF
    ELSE
    *! E303086,1 New Shipping label form T[T20120216.0114][END]
      FOR lnc=1 TO &lcLblTemp..LABELS
        SCATTER MEMVAR
        *--store the print label no to m.nlblsno
        m.nLblsNo = lnc
        INSERT INTO (lcMainF) FROM MEMVAR
      ENDFOR
    *! E303086,1 New Shipping label form T[T20120216.0114][Start]
    ENDIF
    *! E303086,1 New Shipping label form T[T20120216.0114][END]
  ENDSCAN
  SET RELATION TO
  SELECT (lcMainF)
  SET RELATION TO 'O'+ORDER INTO ORDHDR, IIF(STORE=SPACE(8) ,;
        'M'+ACCOUNT, 'S'+ACCOUNT+STORE) INTO CUSTOMER
  *Add function to call optional program.
  lcCompName = ''
*  =lfOptProg()
*ENDIF   &&If Filter Changed
  *! E303086,1 New Shipping label form T[T20120216.0114][Start]
  IF lcFormName = 'SOSHLBT'

    SELECT (lcMainF)
    SCAN
      SCATTER MEMO MEMVAR
      DO CASE
        CASE lcXTYPE = 'O'
          lfGetCrtDet(ORDER+STORE,nLblsNo,lcXTYPE)
        CASE lcXTYPE = 'I'
          lfGetCrtDet(INVOICE+STORE,nLblsNo,lcXTYPE)
       CASE lcXTYPE = 'P'
          lfGetCrtDet(PIKTKT,nLblsNo,lcXTYPE)
     ENDCaSE
    ENDSCAN
    DO CASE
     CASE lcXTYPE = 'O'
   	   SET RELATION TO  ORDER+STORE+STR(nLblsNo,4) INTO (lcCrtTmp) ADDITIVE
     CASE lcXTYPE = 'I'
 	   SET RELATION TO  INVOICE+STORE+STR(nLblsNo,4) INTO (lcCrtTmp) ADDITIVE
     CASE lcXTYPE = 'P'
 	   SET RELATION TO  PIKTKT+STR(nLblsNo,4) INTO (lcCrtTmp) ADDITIVE
    ENDCASE
    SET SKIP TO (lcCrtTmp)
  ENDIF
  *! E303086,1 New Shipping label form T[T20120216.0114][END]
=gfDispRe(lcFormName,'LABELS # 0',.F.,'L',.T.)

*--Close tables and erase temproray files
*HMA
*!*  IF USED(lcMainF)
*!*    USE IN (lcMainF)
*!*  ENDIF
*!*  ERASE (gcWorkDir+lcMainF+'.DBF')
*!*  ERASE (gcWorkDir+lcMainF+'.CDX')
*HMA

*!*************************************************************
*! Name      : lfPikData
*! Developer : Heba Mohamed Amin(HMA)
*! Date      : 05/23/2005
*! Purpose   : Collect the PikTkt data for the report
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : gfModalGen
*!*************************************************************
*! Called from : SOSHLB.PRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfPikData()
*!*************************************************************
FUNCTION lfPikData
*-- Don't collect PIKTKTs with status "Canceled"
loOgScroll.lcRpExp = loOgScroll.lcRpExp + IIF(EMPTY(loOgScroll.lcRpExp),'',' .AND. ') + "PIKTKT.STATUS # 'X'"
SELECT PIKTKT
SET RELATION TO PIKTKT INTO (lclblTbl) ADDITIVE
lcRpExp = loOgScroll.lcRpExp
*! E303086,1 New Shipping label form T[T20120216.0114][Start]
*SCAN FOR &lcRpExp .AND. &lclblTbl..LABELS > 0
SCAN FOR &lcRpExp .AND.  IIF(lcFormName = 'SOSHLBT',!EMPTY(&lclblTbl..cCrtRange),&lclblTbl..LABELS > 0)
*! E303086,1 New Shipping label form T[T20120216.0114][END]
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
  *WAIT WINDOW LANG_Soshlb_Coll_Pick + PikTkt NOWAIT
  WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soshlb_Coll_Pick,oAriaApplication.GetHeaderText("LANG_Soshlb_Coll_Pick",AHEADERFILE)) + PikTkt NOWAIT
  *N000682,1 11/20/2012 MMT Globlization changes[End]

  SCATTER MEMVAR
  M.LABELS = &lclblTbl..LABELS
  *! E303086,1 New Shipping label form T[T20120216.0114][Start]
  IF lcFormName = 'SOSHLBT'
    m.cCrtRange= &lclblTbl..cCrtRange
  ENDIF
  *! E303086,1 New Shipping label form T[T20120216.0114][END]
  INSERT INTO (lcLblTemp) FROM MEMVAR
ENDSCAN

IF RECCOUNT(lcLblTemp) = 0
  *-- Message 'There are no records to display...!'
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN .F.
ENDIF
*-- End of lfPikData.

*!*************************************************************
*! Name      : lfInvData
*! Developer : Heba Mohamed Amin(HMA)
*! Date      : 05/24/2005
*! Purpose   : Collect the invoice data for the report
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : gfModalGen
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfInvData()
*!*************************************************************
FUNCTION lfInvData

*-- Don't collect voided invoices
loOgScroll.lcRpExp = loOgScroll.lcRpExp + IIF(EMPTY(loOgScroll.lcRpExp),'',' .AND. ') + "INVHDR.STATUS # 'V'"
SET ORDER TO ORDHDR IN ORDHDR
SELECT INVHDR
SET RELATION TO INVOICE INTO (lclblTbl) ADDITIVE
SET RELATION TO 'O'+ORDER INTO ORDHDR ADDITIVE
lcRpExp= loOgScroll.lcRpExp
LOCATE ALL FOR &lcRpExp .AND. !EOF('ORDHDR')
IF EOF()
  *-- Message 'There are no records to display...!'
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN .F.
ENDIF
SCAN FOR &lcRpExp .AND. !EOF('ORDHDR')
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_Soshlb_Coll_Invoice + Invoice NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soshlb_Coll_Invoice,oAriaApplication.GetHeaderText("LANG_Soshlb_Coll_Invoice",AHEADERFILE)) + Invoice NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]

  SCATTER MEMVAR
  *-- In consolidated invoice, Add record with the required labels for
  *-- each store
  IF INVHDR.CONSOL = 'Y' .AND. SEEK(INVHDR.INVOICE,'CONSINVH')
    SELECT CONSINVH
    m.Store = CONSINVH.STORE
    lnLab = 0
    SCAN WHILE INVOICE = INVHDR.INVOICE
      IF m.STORE # CONSINVH.STORE
        m.LabELS = lnLab
          *! E303086,1 New Shipping label form T[T20120216.0114][Start]
          IF lcFormName = 'SOSHLBT' AND  SEEK(INVHDR.INVOICE+m.STORE,lclblTbl)
             m.cCrtRange= &lclblTbl..cCrtRange
	         m.Labels = &lclblTbl..Labels
             m.PIKTKT = &lclblTbl..PACK_NO	
          ENDIF
          *! E303086,1 New Shipping label form T[T20120216.0114][END]

        INSERT INTO (lcLblTemp) FROM MEMVAR
        m.Store  = CONSINVH.Store
        lnLab    = CONSINVH.CARTONS
      ELSE
        lnLab = lnLab + CONSINVH.CARTONS
      ENDIF
    ENDSCAN
    m.Labels = lnLab
  ELSE
    M.LABELS = &lclblTbl..LABELS
  ENDIF
  *! E303086,1 New Shipping label form T[T20120216.0114][START]
  *IF M.LABELS > 0
  IF lcFormName = 'SOSHLBT'
    IF SEEK(INVHDR.INVOICE+m.STORE,lclblTbl)
      m.cCrtRange= &lclblTbl..cCrtRange
	  m.Labels = &lclblTbl..Labels
      m.PIKTKT = &lclblTbl..PACK_NO	
    ENDIF
  ENDIF
  IF IIF(lcFormName = 'SOSHLBT',!EMPTY(&lclblTbl..cCrtRange),&lclblTbl..LABELS > 0)
  *! E303086,1 New Shipping label form T[T20120216.0114][END]
    INSERT INTO (lcLblTemp) FROM MEMVAR
  ENDIF
ENDSCAN
SET RELATION TO

IF EOF(lcLblTemp)
  *-- Message 'There are no records to display...!'
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN .F.
ENDIF
*-- End of lfInvData.

*!*************************************************************
*! Name      : lfOrdData
*! Developer : Heba Mohamed Amin(HMA)
*! Date      : 05/24/2005
*! Purpose   : Collect Order data for the report
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : gfModalGen
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfOrdData()
*!*************************************************************
FUNCTION lfOrdData
*-- Don't collect orders with status "CANCELED"
loOgScroll.lcRpExp = loOgScroll.lcRpExp + IIF(EMPTY(loOgScroll.lcRpExp),'',' .AND. ') + "ORDHDR.STATUS # 'X'"
IF !EOF(lcLblTemp)
  SELECT lcLblTemp
  ZAP
ENDIF
SELECT ORDLINE
SET RELATION TO STYLE INTO STYLE ADDITIVE

SELECT ORDHDR
*-- Set relation to that temporary file to get the default number of labels
SET RELATION TO ORDER INTO (lclblTbl)
lcRpExp =loOgScroll.lcRpExp
*! E303086,1 New Shipping label form T[T20120216.0114][Start]
*LOCATE ALL FOR &lcRpExp .AND. &lclblTbl..LABELS > 0
LOCATE ALL FOR &lcRpExp .AND. IIF(lcFormName = 'SOSHLBT',!EMPTY(&lclblTbl..cCrtRange),&lclblTbl..LABELS > 0)
*! E303086,1 New Shipping label form T[T20120216.0114][END]
IF EOF()
  *Message 'There are no records to display...!'
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN .F.
ENDIF
*! E303086,1 New Shipping label form T[T20120216.0114][Start]
*SCAN FOR &lcRpExp .AND. &lclblTbl..LABELS > 0
SCAN FOR &lcRpExp .AND. IIF(lcFormName = 'SOSHLBT',!EMPTY(&lclblTbl..cCrtRange),&lclblTbl..LABELS > 0)
*! E303086,1 New Shipping label form T[T20120216.0114][END]
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_Soshlb_Coll_Order + Order NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soshlb_Coll_Order,oAriaApplication.GetHeaderText("LANG_Soshlb_Coll_Order",AHEADERFILE)) + Order NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]

  SCATTER MEMVAR
  M.LABELS = &lclblTbl..LABELS
  *! E303086,1 New Shipping label form T[T20120216.0114][Start]
  IF lcFormName = 'SOSHLBT'
    m.cCrtRange= &lclblTbl..cCrtRange
  ENDIF
  *! E303086,1 New Shipping label form T[T20120216.0114][END]
  IF ORDHDR.Multi # 'Y'
    INSERT INTO (lcLblTemp) FROM MEMVAR
  ELSE
    *-- If multi store order, insert a record in lcLblTemp file for each
    *-- store to print required labels for each store
    SELECT ORDLINE
    IF SEEK(ORDHDR.CORDTYPE+ORDHDR.ORDER)
      m.CustPO = ORDLINE.CustPO     &&Print ORDLINE.CustPO in case of multi PO.
      m.Store = ORDLINE.STORE
      lnLab = 0
      SCAN WHILE ORDER = ORDHDR.ORDER
        IF m.STORE # ORDLINE.STORE
          m.Labels = lnLab
          *! E303086,1 New Shipping label form T[T20120216.0114][Start]
          IF lcFormName = 'SOSHLBT' AND SEEK(ORDHDR.ORDER+m.STORE,lclblTbl)
             m.cCrtRange= &lclblTbl..cCrtRange
	         m.Labels = &lclblTbl..Labels
             m.PIKTKT = &lclblTbl..PACK_NO	
          ENDIF
          *! E303086,1 New Shipping label form T[T20120216.0114][END]
          INSERT INTO (lcLblTemp) FROM MEMVAR
          m.STORE = ORDLINE.STORE
          m.CustPO = ORDLINE.CustPO     &&Print ORDLINE.CustPO in case of multi PO.
          lnLab = IIF(STYLE.Qty_Ctn=0,1,CEILING(ORDLINE.TotQty/STYLE.Qty_Ctn))
        ELSE
          lnLab = lnLab + IIF(STYLE.Qty_Ctn=0,1,CEILING(ORDLINE.TotQty/STYLE.Qty_Ctn))
        ENDIF
      ENDSCAN
      m.Labels = lnLab
      *! E303086,1 New Shipping label form T[T20120216.0114][Start]
      IF SEEK(ORDHDR.ORDER+m.STORE,lclblTbl)
         m.cCrtRange= &lclblTbl..cCrtRange
         m.Labels = &lclblTbl..Labels
         m.PIKTKT = &lclblTbl..PACK_NO
      ENDIF
      *! E303086,1 New Shipping label form T[T20120216.0114][END]
      INSERT INTO (lcLblTemp) FROM MEMVAR
    ENDIF
  ENDIF
ENDSCAN
SET RELATION TO

SELECT ORDLINE
SET RELATION TO
*-- End of lfOrdData.

*!*************************************************************
*! Name      : lfwOGWhen
*! Developer : Heba Mohamed Amin(HMA)
*! Date      : 05/22/2005
*! Purpose   : The when function of the option grid
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : gfTempName()
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfwOGWhen()
*!*************************************************************
FUNCTION lfwOGWhen
llcbDspDlg=.F.
*-- lclblTbl Variable defined by the option grid takes temporary name
DO CASE

  *-- Current module is Sales Order
  CASE oAriaApplication.ActiveModuleID ='SO'
    lcXType = 'O'
    DIMENSION laTempLabel[2,18]

    laTempLabel[1,1]="ORDER"
    laTempLabel[1,2]="C"
    laTempLabel[1,3]=6
    laTempLabel[1,4]=0

    laTempLabel[2,1]="LABELS"
    laTempLabel[2,2]="N"
    laTempLabel[2,3]=4
    laTempLabel[2,4]=0
    FOR lncnt=7 TO 16
      STORE SPACE(0) TO laTempLabel[1,lnCnt],laTempLabel[2,lnCnt]
    ENDFOR
    STORE 0 TO laTempLabel[1,17],laTempLabel[1,18],laTempLabel[2,17],laTempLabel[2,18]
    *! E303086,1 New Shipping label form T[T20120216.0114][START]
    IF lcFormName = 'SOSHLBT'
      lnFileStru = ALEN(laTempLabel,1)
      DIMENSION laTempLabel[lnFileStru+3,18]
      lnFileStru = lnFileStru+1
      laTempLabel[lnFileStru,1] = 'cCrtRange'    &&Label Number
      laTempLabel[lnFileStru,2] = 'C'
      laTempLabel[lnFileStru,3] = 50
      laTempLabel[lnFileStru,4] = 0
      lnFileStru = lnFileStru+1
      laTempLabel[lnFileStru,1] = 'Store'    &&Label Number
      laTempLabel[lnFileStru,2] = 'C'
      laTempLabel[lnFileStru,3] = 8
      laTempLabel[lnFileStru,4] = 0
      lnFileStru = lnFileStru+1
      laTempLabel[lnFileStru,1] = 'PACK_NO'    &&Label Number
      laTempLabel[lnFileStru,2] = 'C'
      laTempLabel[lnFileStru,3] = 6
      laTempLabel[lnFileStru,4] = 0

      FOR lncnt=7 TO 16
        FOR lnInc=1 TO lnFileStru
          STORE SPACE(0) TO laTempLabel[lnInc,lnCnt],laTempLabel[lnInc,lnCnt]
        ENDFOR
      ENDFOR
      FOR lnInc=1 TO lnFileStru
        STORE 0 TO laTempLabel[lnInc,17],laTempLabel[lnInc,18]
      ENDFOR
      =gfCrtTmp(lclblTbl,@laTempLabel,'ORDER+STORE+PACK_NO',lclblTbl,.F.)
    ELSE
    *! E303086,1 New Shipping label form T[T20120216.0114][END]
      =gfCrtTmp(lclblTbl,@laTempLabel,'ORDER',lclblTbl,.F.)
    *! E303086,1 New Shipping label form T[T20120216.0114][Start]
    ENDIF
     *! E303086,1 New Shipping label form T[T20120216.0114][END]

    SET ORDER TO ORDLINE IN ORDLINE
    SET ORDER TO STYLE IN STYLE
    SET ORDER TO ORDHDR IN ORDHDR

  *-- Current module is Account Receivable
  CASE oAriaApplication.ActiveModuleID ='AR'
    lcXType = 'I'
    DIMENSION laTempLabel[2,18]

    laTempLabel[1,1]="INVOICE"
    laTempLabel[1,2]="C"
    laTempLabel[1,3]=6
    laTempLabel[1,4]=0

    laTempLabel[2,1]="LABELS"
    laTempLabel[2,2]="N"
    laTempLabel[2,3]=4
    laTempLabel[2,4]=0
    FOR lncnt=7 TO 16
      STORE SPACE(0) TO laTempLabel[1,lnCnt],laTempLabel[2,lnCnt]
    ENDFOR
    STORE 0 TO laTempLabel[1,17],laTempLabel[1,18],laTempLabel[2,17],laTempLabel[2,18]
    *! E303086,1 New Shipping label form T[T20120216.0114][Start]
    IF lcFormName = 'SOSHLBT'
      lnFileStru = ALEN(laTempLabel,1)
      DIMENSION laTempLabel[lnFileStru+3,18]
      lnFileStru = lnFileStru+1
      laTempLabel[lnFileStru,1] = 'cCrtRange'    &&Label Number
      laTempLabel[lnFileStru,2] = 'C'
      laTempLabel[lnFileStru,3] = 50
      laTempLabel[lnFileStru,4] = 0
      lnFileStru = lnFileStru+1
      laTempLabel[lnFileStru,1] = 'Store'    &&Label Number
      laTempLabel[lnFileStru,2] = 'C'
      laTempLabel[lnFileStru,3] = 8
      laTempLabel[lnFileStru,4] = 0
      lnFileStru = lnFileStru+1
      laTempLabel[lnFileStru,1] = 'PACK_NO'    &&Label Number
      laTempLabel[lnFileStru,2] = 'C'
      laTempLabel[lnFileStru,3] = 6
      laTempLabel[lnFileStru,4] = 0

      FOR lncnt=7 TO 16
        FOR lnInc=1 TO lnFileStru
          STORE SPACE(0) TO laTempLabel[lnInc,lnCnt],laTempLabel[lnInc,lnCnt]
        ENDFOR
      ENDFOR
      FOR lnInc=1 TO lnFileStru
        STORE 0 TO laTempLabel[lnInc,17],laTempLabel[lnInc,18]
      ENDFOR
      =gfCrtTmp(lclblTbl,@laTempLabel,'INVOICE+STORE+PACK_NO',lclblTbl,.F.)
    ELSE
    *! E303086,1 New Shipping label form T[T20120216.0114][END]
      =gfCrtTmp(lclblTbl,@laTempLabel,'INVOICE',lclblTbl,.F.)
    *! E303086,1 New Shipping label form T[T20120216.0114][Start]
    ENDIF
    *! E303086,1 New Shipping label form T[T20120216.0114][END]

    *=gfDoTriger('SOSHLB',PADR('CREATEFILE',10))
    SET ORDER TO INVHDR IN INVHDR

  *-- Current module is Sales order Allocation
  CASE oAriaApplication.ActiveModuleID='AL'
    lcXType = 'P'
    DIMENSION laTempLabel[3,18]

    laTempLabel[1,1]="PIKTKT"
    laTempLabel[1,2]="C"
    laTempLabel[1,3]=6
    laTempLabel[1,4]=0

    laTempLabel[2,1]="ORDER"
    laTempLabel[2,2]="C"
    laTempLabel[2,3]=6
    laTempLabel[2,4]=0

    laTempLabel[3,1]="LABELS"
    laTempLabel[3,2]="N"
    laTempLabel[3,3]=4
    laTempLabel[3,4]=0
    FOR lncnt=7 TO 16
      STORE SPACE(0) TO laTempLabel[1,lnCnt],laTempLabel[2,lnCnt],laTempLabel[3,lnCnt]
    ENDFOR
    STORE 0 TO laTempLabel[1,17],laTempLabel[1,18],laTempLabel[2,17],laTempLabel[2,18],;
               laTempLabel[3,17],laTempLabel[3,18]
    *! E303086,1 New Shipping label form T[T20120216.0114][Start]
    IF lcFormName = 'SOSHLBT'
      lnFileStru = ALEN(laTempLabel,1)
      DIMENSION laTempLabel[lnFileStru+1,18]
      lnFileStru = lnFileStru+1
      laTempLabel[lnFileStru,1] = 'cCrtRange'    &&Label Number
      laTempLabel[lnFileStru,2] = 'C'
      laTempLabel[lnFileStru,3] = 50
      laTempLabel[lnFileStru,4] = 0
      FOR lncnt=7 TO 16
        FOR lnInc=1 TO lnFileStru
          STORE SPACE(0) TO laTempLabel[lnInc,lnCnt],laTempLabel[lnInc,lnCnt]
        ENDFOR
      ENDFOR
      FOR lnInc=1 TO lnFileStru
        STORE 0 TO laTempLabel[lnInc,17],laTempLabel[lnInc,18]
      ENDFOR
    ENDIF
    *! E303086,1 New Shipping label form T[T20120216.0114][END]
    =gfCrtTmp(lclblTbl,@laTempLabel,'PIKTKT',lclblTbl,.F.)
    SET ORDER TO ORDLINE IN ORDLINE
    SET ORDER TO PIKLINE IN PIKLINE
    SET ORDER TO STYLE   IN STYLE
ENDCASE
*! E303086,1 New Shipping label form T[T20120216.0114][Start]
IF lcFormName = 'SOSHLBT'
IF !USED("PACK_LIN_C")
  gfOpenTable("PACK_LIN","PACK_LIN",'SH',"PACK_LIN_C")
ENDIF
IF !USED("PACK_HDR_C")
  gfOpenTable("PACK_HDR","PACK_HDR",'SH',"PACK_HDR_C")
ENDIF
IF !USED("SCALE_C")
  gfOpenTable("SCALE","SCALE",'SH',"SCALE_C")
ENDIF
IF !USED("STYLE_C")
  gfOpenTable("STYLE","STYLE",'SH',"STYLE_C")
ENDIF
ENDIF
*! E303086,1 New Shipping label form T[T20120216.0114][END]
*-- End of lfwOGWhen.

*!*************************************************************
*! Name      : lfSRVLBL
*! Developer : Heba Mohamed Amin(HMA)
*! Date      : 05/22/2005
*! Purpose   : control browsing (Order , Invoice or PikTkt) and validate
*!           : selecting it in inlist function.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : gfModalGen
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfSRVLBL()
*!*************************************************************
*! Note      : SRV symbol is [S,Set--R,Reset--V,Valid]
*!*************************************************************
FUNCTION lfSRVLBL
PARAMETERS lcParm
PRIVATE lcAlias,llHaveSty

DO CASE
  CASE lcParm = 'S'  && Set code
    DO CASE
      CASE lcxType = 'O'    &&transaction is Orders
        SELECT ORDHDR
        *-- Set relation to that temporary file to get the default number of labels
        SET RELATION TO ORDER INTO (lclblTbl)

        *-- Set relation to customer to get the account address in the INRANGE browse
        SET RELATION TO IIF(EMPTY(STORE),'M'+ACCOUNT,'S'+ACCOUNT+STORE) ;
                INTO CUSTOMER ADDITIVE

      CASE lcxType = 'I'    &&transaction is invoices
        SELECT INVHDR
        SET RELATION TO IIF(EMPTY(STORE),'M'+ACCOUNT,'S'+ACCOUNT+STORE) ;
                INTO CUSTOMER ADDITIVE

        SET RELATION TO INVOICE INTO (lclblTbl) ADDITIVE

      CASE lcxType = 'P'    &&transaction is PikTkt
        SET ORDER TO TAG CUSTOMER IN CUSTOMER
        SELECT PIKTKT
        SET RELATION TO 'O' + Order INTO ORDHDR
        SET RELATION TO IIF(EMPTY(Store) , 'M' + Account ,;
                        'S' + Account + Store) INTO CUSTOMER ADDITIVE
        SET RELATION TO PIKTKT INTO (lclblTbl) ADDITIVE
    ENDCASE

  CASE lcParm = 'R'  && Reset code
    DO CASE
      CASE lcxType = 'O'
        SELECT ORDHDR
        SET RELATION TO
      CASE lcxType = 'I'
        SELECT INVHDR
        SET RELATION TO
      CASE lcxType = 'P'
        SELECT PIKTKT
        SET RELATION TO
    ENDCASE

  OTHERWISE      && Valid code
    =!lfLblFound() AND lfNoOfLbls()
    DO CASE

      CASE (lcxType = 'O' .OR. lcxType = 'P' ) AND !llcbDspDlg
        lnLabels = EVAL(lclblTbl+'.Labels')
        lcTrans=IIF(lcxType = 'O',&lcLblTbl..Order,&lcLblTbl..PikTkt)
        *-- Call the screen to edit the number of labels
        *! E303086,1 New Shipping label form T[T20120216.0114][Start]
        IF lcFormName = 'SOSHLBT'
          lnLabels = EVAL(lclblTbl+'.cCrtRange')
          DO FORM (oAriaApplication.ReportHome+"SOTSHLB.SCX") WITH IIF(lcxType = 'O' ,'O','P')
        ELSE
        *! E303086,1 New Shipping label form T[T20120216.0114][END]
          DO FORM (oAriaApplication.ReportHome+"SOSHLB.SCX") WITH lnLabels
          IF lnLabels <> lnLabelNo
            REPLACE &lcLblTbl..LABELS WITH lnLabelNo
          ENDIF
        *! E303086,1 New Shipping label form T[T20120216.0114][Start]
        ENDIF
        *! E303086,1 New Shipping label form T[T20120216.0114][END]

      CASE lcxType = 'I'

        *-- Calculate required number of lables, however don't allow user to edit.
        SET ORDER TO CONSINVH IN CONSINVH
        lnCurAls = SELECT(0)
        lcKey = KEY()
        *! E303086,1 New Shipping label form T[T20120216.0114][START]
        IF lcFormName = 'SOSHLBT'
          lnLabels = EVAL(lclblTbl+'.cCrtRange')
          DO FORM (oAriaApplication.ReportHome+"SOTSHLB.SCX") WITH 'I'
        ELSE
        *! E303086,1 New Shipping label form T[T20120216.0114][END]
        lcExpr = INVHDR.INVOICE
        IF INVHDR.CONSOL # 'Y'
          SELECT (lclblTbl)
          REPLACE LABELS WITH INVHDR.CARTONS
          lnLab = INVHDR.CARTONS
        ELSE
          *-- If consolidated invoice, calculate number of labels from CONSINVH
          SELECT CONSINVH
          IF SEEK(lcExpr,'CONSINVH')
            lnLab = 0
            SCAN WHILE INVOICE = INVHDR.INVOICE
                lnLab = lnLab + CONSINVH.CARTONS
            ENDSCAN
          ELSE
            lnLab = INVHDR.CARTONS
          ENDIF
          SELECT (lclblTbl)
          IF SEEK(lcExpr)
            REPLACE LABELS WITH lnLab
          ENDIF
        ENDIF
*        IF gfDoTriger('SOSHLB',PADR('LABELSINFO',10))
        lcTrans=&lcLblTbl..Invoice
        *-- Call the screen to edit the number of labels


        DO FORM (oAriaApplication.ReportHome+"SOSHLB.SCX") WITH lnLab
        SELECT (lclblTbl)
        *-- Update table with the new number of labels entered by user.
        IF lnLab <> lnLabelNo
          REPLACE &lcLblTbl..LABELS WITH lnLabelNo
        ENDIF

        SELECT (lnCurAls)
        =SEEK(lcKey)
        *! E303086,1 New Shipping label form T[T20120216.0114][Start]
        ENDIF
        *! E303086,1 New Shipping label form T[T20120216.0114][END]
    ENDCASE
ENDCASE

*!*************************************************************
*! Name      : lfLblFound
*! Developer : Heba Mohamed Amin(HMA)
*! Date      : 05/22/2005
*! Purpose   : Check the Existance Of the No. Of Label In the temp File.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Return    : .T. if label found else .F.
*!*************************************************************
*! Example   : =lfLblFound()
*!*************************************************************

FUNCTION lfLblFound
PRIVATE lcToSeek
lnselected=SELECT()
lnRecNo=RECNO()
lcToSeek = IIF(oAriaApplication.ActiveModuleID ='SO',[ORDHDR.ORDER],IIF(oAriaApplication.ActiveModuleID ='AR',[INVHDR.INVOICE],;
                               [PIKTKT.PIKTKT]))
GOTO lnRecNo
RETURN SEEK(&lcToSeek,lclblTbl)
*-- end of lfLblFound.

*!*************************************************************
*! Name      : lfNoOfLbls
*! Developer : Heba Mohamed Amin(HMA)
*! Date      : 05/22/2005
*! Purpose   : Calculate Number of labels in all cases [AR-AL-SO]
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Return    : .T. or .F.
*!*************************************************************
*! Example   : =lfNoOfLbls()
*!*************************************************************

FUNCTION lfNoOfLbls
PRIVATE lnCurrAls , lnNoOfLbls
lnRecNo=RECNO()
lnNoOfLbls = 0
lnCurrAls = SELECT(0)  && Save Active Alias

DO CASE
  *-- Current module is Sales Order
  CASE oAriaApplication.ActiveModuleID='SO'
    *! E303086,1 New Shipping label form T[T20120216.0114][Start]
    IF lcFormName = 'SOSHLBT'
      IF gfSeek(OrdHdr.Order,'PACK_HDR_C','ORDERPCK')
        lnNoOfLbls = 0
        SELECT PACK_HDR_C
        =gfSetOrder('ORDERPCK')
        SCAN REST WHILE ORDER+STORE+PACK_NO = OrdHdr.Order
          lnNoOfLbls = PACK_HDR_C.tot_cart
          IF !SEEK(OrdHdr.Order+PACK_HDR_C.STORE+PACK_HDR_C.PACK_NO,lclblTbl)
            INSERT INTO (lclblTbl) (Order,STORE,PACK_NO,Labels, cCrtRange) VALUES (OrdHdr.Order,PACK_HDR_C.STORE,PACK_HDR_C.PACK_NO,lnNoOfLbls,ALLTRIM("1-"+ALLTRIM(STR(lnNoOfLbls,10,0))))
          ELSE
            REPLACE Labels WITH Labels+ lnNoOfLbls IN (lclblTbl)
            REPLACE cCrtRange WITH ALLTRIM("1-"+ALLTRIM(STR(lnNoOfLbls,10,0))) IN (lclblTbl)
          ENDIF
        ENDSCAN
      ELSE
        IF OrdHdr.MULTI = 'N'
          =SEEK(OrdHdr.cOrdType+OrdHdr.Order,"ORDLINE")
          SELECT ORDLINE
          lnNoOfLbls = 0
          SCAN REST WHILE cordtype+order+STR(lineno,6) = OrdHdr.cOrdType+OrdHdr.Order
            lnNoOfLbls = lfCalcrtns(lnNoOfLbls)
          ENDSCAN
          IF !SEEK(OrdHdr.Order+OrdHdr.STORE,lclblTbl)
            INSERT INTO (lclblTbl) (Order,STORE,Labels, cCrtRange) VALUES (OrdHdr.Order,OrdHdr.STORE,lnNoOfLbls,ALLTRIM("1-"+ALLTRIM(STR(lnNoOfLbls,10,0))))
          ELSE
            REPLACE Labels WITH Labels + lnNoOfLbls IN (lclblTbl)
            REPLACE cCrtRange WITH ALLTRIM("1-"+ALLTRIM(STR(lnNoOfLbls,10,0))) IN (lclblTbl)
          ENDIF
        ELSE
          SELECT ORDLINE
          lcOldInd = ORDER()
          SET ORDER TO ORDLINST   && CORDTYPE+ORDER+STORE+STYLE+STR(LINENO,6)
           =SEEK(OrdHdr.cOrdType+OrdHdr.Order,"ORDLINE",'ORDLINST')
          lcStore = SPACE(8)
          lnNoOfLbls = 0
          SCAN REST WHILE CORDTYPE+ORDER+STORE+STYLE+STR(LINENO,6)
            IF lcStore <> ORDLINE.STORE AND !EMPTY(lcStore)
	          IF !SEEK(OrdHdr.Order+lcStore ,lclblTbl)
	            INSERT INTO (lclblTbl) (Order,STORE,Labels, cCrtRange) VALUES (OrdHdr.Order,lcStore ,lnNoOfLbls,ALLTRIM("1-"+ALLTRIM(STR(lnNoOfLbls,10,0))))
	          ENDIF
	          lnNoOfLbls = 0
            ENDIF
            lcStore = ORDLINE.STORE
            lnNoOfLbls = lfCalcrtns(lnNoOfLbls)
          ENDSCAN
          IF !SEEK(OrdHdr.Order+lcStore ,lclblTbl)
	        INSERT INTO (lclblTbl) (Order,STORE,Labels, cCrtRange) VALUES (OrdHdr.Order,lcStore ,lnNoOfLbls,ALLTRIM("1-"+ALLTRIM(STR(lnNoOfLbls,10,0))))
	      ENDIF
          SELECT ORDLINE
          SET ORDER TO (lcOldInd)
        ENDIF
      ENDIF
    ELSE
    *! E303086,1 New Shipping label form T[T20120216.0114][END]
      IF SEEK(OrdHdr.cOrdType+OrdHdr.Order,"ORDLINE")
        SELECT ORDLINE
        SCAN REST WHILE cordtype+order+STR(lineno,6) = OrdHdr.cOrdType+OrdHdr.Order
          lnNoOfLbls = lfCalcrtns(lnNoOfLbls)
        ENDSCAN
      ENDIF
      INSERT INTO (lclblTbl) (Order, Labels) VALUES (OrdHdr.Order,lnNoOfLbls)
    *! E303086,1 New Shipping label form T[T20120216.0114][Start]
    ENDIF
    *! E303086,1 New Shipping label form T[T20120216.0114][END]

  *-- Current module is Account Receivable
  CASE oAriaApplication.ActiveModuleID='AR'
    IF InvHdr.STATUS # 'V'

      *! E303086,1 New Shipping label form T[T20120216.0114][Start]
      IF lcFormName = 'SOSHLBT'
        lnNoOfLbls =  0
        lcpackNum = ''
        IF INVHDR.CONSOL <> 'Y'
          IF !EMPTY(INVHDR.PIKTKT) AND gfSeek(INVHDR.PIKTKT,'PACK_HDR_C','PACK_HDR')
            lnNoOfLbls = PACK_HDR_C.tot_cart
            lcpackNum = PACK_HDR_C.PACK_NO
          ELSE
            lcpackNum  = ''
            lnNoOfLbls = INVHDR.cartons
          ENDIF
          INSERT INTO (lclblTbl) (INVOICE,Store,Labels,PACK_NO, cCrtRange) VALUES (INVHDR.INVOICE,INVHDR.STORE,lnNoOfLbls ,lcpackNum ,ALLTRIM("1-"+ALLTRIM(STR(lnNoOfLbls,10,0))))
        ELSE
          =gfSeek(INVHDR.INVOICE,'CONSINVH')
          SELECT CONSINVH
          SCAN REST WHILE INVOICE+STORE+ORDER+PIKTKT = INVHDR.INVOICE
            lcpackNum  = ''
            IF !EMPTY(CONSINVH.PIKTKT) AND gfSeek(CONSINVH.PIKTKT,'PACK_HDR_C','PACK_HDR')
              lnNoOfLbls = PACK_HDR_C.tot_cart
              lcpackNum = PACK_HDR_C.PACK_NO
            ELSE
              lnNoOfLbls = CONSINVH.cartons

            ENDIF
            IF !SEEK(INVHDR.INVOICE+CONSINVH.STORE,lclblTbl)
              INSERT INTO (lclblTbl) (INVOICE,STORE,PACK_NO,Labels, cCrtRange) VALUES (INVHDR.INVOICE,CONSINVH.STORE,lcpackNum, lnNoOfLbls,ALLTRIM("1-"+ALLTRIM(STR(lnNoOfLbls,10,0))))
            ELSE
              REPLACE Labels WITH Labels + lnNoOfLbls IN (lclblTbl)
              REPLACE cCrtRange WITH ALLTRIM("1-"+ALLTRIM(STR(lnNoOfLbls,10,0))) IN (lclblTbl)
            ENDIF
          ENDSCAN
        ENDIF
      ELSE
      *! E303086,1 New Shipping label form T[T20120216.0114][END]
       INSERT INTO (lclblTbl) (INVOICE) VALUES (INVHDR.INVOICE)
      *! E303086,1 New Shipping label form T[T20120216.0114][Start]
      ENDIF
      *! E303086,1 New Shipping label form T[T20120216.0114][END]
    ENDIF

  *-- Current module is Allocation
  CASE oAriaApplication.ActiveModuleID='AL'
    *-- if PikTkt is not cancelled.

    *! E303086,1 New Shipping label form T[T20120216.0114][Start]
    IF lcFormName = 'SOSHLBT'
      lnNoOfLbls =  0
      IF PIKTKT.STATUS # "X"
      IF gfSeek(PIKTKT.PIKTKT,'PACK_HDR_C','PACK_HDR')
        lnNoOfLbls = PACK_HDR_C.tot_cart
      ELSE
       IF PIKTKT.STATUS $ 'OH' AND SEEK("O"+PikTkt.Order,"ORDLINE")
        SELECT ORDLINE
        SCAN REST WHILE cOrdType+ORDER+STR(LineNo,6) = 'O'+PIKTKT.ORDER
          IF PICKED AND PIKTKT = PIKTKT.PIKTKT
            lnNoOfLbls = lfCalcrtns(lnNoOfLbls)
          ENDIF
        ENDSCAN
      ENDIF
      ENDIF
      lnNoOfLbls = IIF(lnNoOfLbls = 0,1,lnNoOfLbls)
	  INSERT INTO (lclblTbl) (PIKTKT,Order,Labels ,cCrtRange) VALUES ;
                           (PIKTKT.PIKTKT,PIKTKT.ORDER,lnNoOfLbls ,ALLTRIM("1-"+ALLTRIM(STR(lnNoOfLbls,10,0))))
                           ENDIF
    ELSE
    *! E303086,1 New Shipping label form T[T20120216.0114][END]
    IF PIKTKT.STATUS # "X"
      IF PIKTKT.STATUS = "C" AND SEEK(PIKTKT.PIKTKT,"PIKLINE")
         lcScanCond  = PikTkt.PikTkt + PikTkt.Order
         SELECT PIKLINE
         SCAN REST WHILE piktkt+order+STR(lineno,6) = lcScanCond
           lnNoOfLbls = lfCalcrtns(lnNoOfLbls)
         ENDSCAN
      ENDIF
      IF PIKTKT.STATUS $ 'OH' AND SEEK("O"+PikTkt.Order,"ORDLINE")
        SELECT ORDLINE
        SCAN REST WHILE cOrdType+ORDER+STR(LineNo,6) = 'O'+PIKTKT.ORDER
          IF PICKED AND PIKTKT = PIKTKT.PIKTKT
            lnNoOfLbls = lfCalcrtns(lnNoOfLbls)
          ENDIF
        ENDSCAN
      ENDIF
    ENDIF  && end if PikTkt is not cancelled.
    INSERT INTO (lclblTbl) (PIKTKT,Order,LABELS) VALUES ;
                           (PIKTKT.PIKTKT,PIKTKT.ORDER,lnNoOfLbls)

   *! E303086,1 New Shipping label form T[T20120216.0114][Start]
   ENDIF
   *! E303086,1 New Shipping label form T[T20120216.0114][END]
ENDCASE
lnRecNo=RECNO()
SELECT (lnCurrAls)
*-- end of lfNoOfLbls.

*!*************************************************************
*! Name      : lfCalcrtns
*! Developer : Heba Mohamed Amin(HMA)
*! Date      : 05/23/2005
*! Purpose   : General formula to Calculate Number of Cartons.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Return    : No. Of Cartons.
*!*************************************************************
*! Example   : =lfCalcrtns(No. of labels)
*!*************************************************************
FUNCTION lfCalcrtns
PARAMETERS lnCalcrtns
PRIVATE lnCalcrtns
=SEEK(Style,"STYLE")
RETURN lnCalcrtns + IIF(STYLE.Qty_Ctn=0,1,CEILING(TotQty/STYLE.Qty_Ctn))
*-- end of lfCalcrtns.

*!*************************************************************
*! Name      : lfGetAddr
*! Developer : Heba Mohamed Amin(HMA)
*! Date      : 05/29/2005
*! Purpose   : Get warehouse and ship to addresses
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Label
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfGetAddr()
*!*************************************************************
FUNCTION lfGetAddr
lnAlias = SELECT(0)

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcAcct   = ORDHDR.Account+IIF(EMPTY(STORE),'',LANG_Soshlb_Store+Store)
lcAcct   = ORDHDR.Account+IIF(EMPTY(STORE),'',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soshlb_Store,oAriaApplication.GetHeaderText("LANG_Soshlb_Store",AHEADERFILE))+Store)
*N000682,1 11/20/2012 MMT Globlization changes[End]

lcDivLName = ''
DIMENSION laDivLName[1,2]
laDivLName[1,1] = 'DIVLNAME'     && Array to get the Division long name
laDivLName[1,2] = 'lcDivLName'
*! E303086,1 New Shipping label form T[T20120216.0114][Start]
IF lcFormName = 'SOSHLBT'
lfGetDivLName()
ELSE
*! E303086,1 New Shipping label form T[T20120216.0114][END]
=gfRltFld(ORDHDR.cDivision , @laDivLName , 'CDIVISION')
*! E303086,1 New Shipping label form T[T20120216.0114][Start]
ENDIF
*! E303086,1 New Shipping label form T[T20120216.0114][END]
lcCompName = IIF(!EMPTY(lcDivLName),lcDivLName,SUBSTR(gcCom_Name,1,29))
IF ORDHDR.ALT_SHPTO
  lcActNam = ORDHDR.STNAME
  lcAddr1  = ORDHDR.cAddress1
  lcAddr2  = ORDHDR.cAddress2
  lcAddr3  = ORDHDR.cAddress3
ELSE
  IF !EMPTY(CUSTOMER.DIST_CTR) .AND. SEEK('S'+CUSTOMER.ACCOUNT+CUSTOMER.DIST_CTR,LCCUST_A)
    lcActNam = IIF(EMPTY(&LCCUST_A..DBA),&LCCUST_A..STNAME,&LCCUST_A..DBA)
  ELSE
    lcActNam = IIF(EMPTY(CUSTOMER.DBA),CUSTOMER.STNAME,CUSTOMER.DBA)
  ENDIF
  IF EMPTY(CUSTOMER.DIST_CTR)
    SELECT CUSTOMER
    lcAddr1 = gfGetAdr('CUSTOMER' , '' , '' , '' , 1)
    lcAddr2 = gfGetAdr('CUSTOMER' , '' , '' , '' , 2)
    lcAddr3 = gfGetAdr('CUSTOMER' , '' , '' , '' , 3)
  ELSE
    SELECT (LCCUST_A)
    lcAddr1 = gfGetAdr(LCCUST_A,'' , '' , '' , 1)
    lcAddr2 = gfGetAdr(LCCUST_A,'' , '' , '' , 2)
    lcAddr3 = gfGetAdr(LCCUST_A,'' , '' , '' , 3)
  ENDIF
ENDIF
DECLARE lasort[3]
FOR lnI = 1 TO 3
  lcI = ALLTRIM(STR(lnI))
  lasort[lnI] = lcAddr&lcI
ENDFOR
FOR lnI = 1 TO 2
  FOR lnC = 1 to 2
    IF Empty(laSort[lnC])
      laSort[lnC] = laSort[lnC+1]
      laSort[lnC+1] = ' '
    ENDIF
  ENDFOR
ENDFOR
FOR lnI = 1 TO 3
  lcI = ALLTRIM(STR(lnI))
  lcAddr&lcI =  lasort[lnI]
ENDFOR
IF lcPrtAdr = 'W'
  DO CASE
    CASE lcXTYPE = 'P' .OR. lcXTYPE = 'I'
      =SEEK(&lcMainF..cWareCode,'WAREHOUS')
    CASE lcXTYPE = 'O'
      =SEEK(ORDHDR.cWareCode,'WAREHOUS')
  ENDCASE
  *-- Get the company addresses
  lcCmAdr1 = gfGetAdr('WAREHOUS' , '' , '' , '' , 1)
  lcCmAdr2 = gfGetAdr('WAREHOUS' , '' , '' , '' , 2)
  lcCmAdr3 = gfGetAdr('WAREHOUS' , '' , '' , '' , 3)
  lcCmAdr4 = gfGetAdr('WAREHOUS' , '' , '' , '' , 4)
  lcCmAdr5 = gfGetAdr('WAREHOUS' , '' , '' , '' , 5)
ENDIF
SELECT (lnAlias)
RETURN ''
*!**************************************************************************
*! Name      : lfBringStr
*! Developer : Heba Mohamed Amin(HMA)
*! Date      : 05/20/2005
*! Purpose   : Function to collect (Department, Cust Po, Store and ShipVia)
*!           : as one string
*!           : to Print In Shipping Labels.
*!**************************************************************************
*! Called from : Label
*!**************************************************************************
*! Calls       : .......
*!**************************************************************************
*! Returns     : None
*!**************************************************************************
*! Example     : =lfBringStr()
*!**************************************************************************
FUNCTION lfBringStr
PARAMETER lcDummy
PRIVATE lcShipStr,lcPoStr

lcStr = ""
IF lcXTYPE = 'I'

  = SEEK(Invoice,"INVHDR")
  lcShipStr = gfCodDes(InvHdr.ShipVia,'SHIPVIA',.T.)
  lcStr = INVHDR.DEPT + '     ' + INVHDR.CUSTPO + SPACE(5) + STORE + " "

ELSE

  IF "*" $ OrdHdr.ShipVia
    lcShipStr = Customer.ShipVia
  ELSE
    lcShipStr = OrdHdr.ShipVia
  ENDIF
  lcShipStr = gfCodDes(lcShipStr,'SHIPVIA',.T.)

  IF OrdHdr.MultiPo
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcPoStr = LANG_Soshlb_MultiPo
lcPoStr = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soshlb_MultiPo,oAriaApplication.GetHeaderText("LANG_Soshlb_MultiPo",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  ELSE
    lcPoStr = ORDHDR.CUSTPO
  ENDIF
  lcStr = ORDHDR.DEPT + '     ' + lcPoStr + SPACE(6)

ENDIF
lcStr = lcStr + lcShipStr
RETURN ""
*-- end of lfBringStr

*!*************************************************************
*! Name      : gfDoTriger
*! Developer : HS (Haytham El_Sheltawi)
*! Date      : 07/22/99
*! Purpose   : Function to control any triggers found in the
*!             triggers file, customized processes and workflow
*!             server requests.
*!*************************************************************
*! Calls              : None.
*!*************************************************************
*! Passed Parameters  : 1) lcProgName, Object ID.
*!                      2) lcEvent, Event ID.
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example            :  =gfDoTriger()
*!*************************************************************
*E301297,1 this function was added by HS for the entry E301297,1.
*!*************************************************************
*
FUNCTION gfDoTriger

PARAMETERS lcProgName , lcEvent

PRIVATE lnOldAlias , lcProgToDo , laParamExp , laParam , lcParmStr ,;
        lnCount    , llReturn

llReturn = .T.

*-- If any of the parameters is not passed or passed incorrectly
IF TYPE('lcProgName') <> 'C' .OR. EMPTY(lcProgName) .OR.;
   TYPE('lcEvent') <> 'C' .OR. EMPTY(lcEvent)
  RETURN
ENDIF

*-- Save the old alias
lnOldAlias = SELECT(0)

*-- Open the Trigger file if it was not opened
IF !USED('SYCTRIGG')
  =gfOpenFile(gcSysHome + 'SYCTRIGG' , 'OBJEVENT' , 'SH')
ENDIF

SELECT SYCTRIGG

*-- If there is triggers for this Object/Event
IF SEEK(PADR(lcProgName , 10) + PADR(lcEvent , 10))

  *-- Scan loop to scan the Object/Event triggers
  SCAN REST;
      WHILE cAPObjNam + cEvent_ID = PADR(lcProgName , 10) +;
            PADR(lcEvent , 10)

    *-- Get the name of the program that should be executed
    lcProgToDo = cTrig_ID

    *-- Initialize the parameter string variable
    lcParmStr  = ''

    *-- Restore the old alias to be able to evaluate the parameter
    *-- expressions properly
    SELECT (lnOldAlias)

    *-- If there is one or more parameters that should be passed to the
    *-- program
    IF !EMPTY(SYCTRIGG.mParmExpr)

      *-- Get the parameter expressions
      DIMENSION laParamExp[OCCURS('~' , SYCTRIGG.mParmExpr) + 1]
      =gfSubStr(SYCTRIGG.mParmExpr , @laParamExp , '~')

      *-- Initialize the parameters array
      DIMENSION laParam[ALEN(laParamExp , 1)]
      laParam = ""

      *-- Get the parameters values that will be passed to the program
      FOR lnCount = 1 TO ALEN(laParamExp , 1)
        laParam[lnCount] = EVALUATE(laParamExp[lnCount])
        lcParmStr = lcParmStr + IIF(lnCount = 1 , '' , ' , ') +;
                    'laParam[' + ALLTRIM(STR(lnCount)) + ']'

      ENDFOR    && End of FOR lnCount = 1 TO ALEN(laParamExp , 1)
    ENDIF    && End of IF !EMPTY(SYCTRIGG.mParmExpr)

    *-- If custom process
    IF SYCTRIGG.cActvTyp = 'C'
      *-- Call the program and get the returned value
      llReturn = &lcProgToDo(&lcParmStr)
    ELSE    && Else [If Server request]
    ENDIF    && End of IF SYCTRIGG.cActvTyp = 'C'

    SELECT SYCTRIGG
  ENDSCAN    && End of SCAN REST WHILE cAPObjNam + cEvent_ID = ...

  *B603662,1 BWA 05/25/2000 In case the process doesn't exist.[START]
ELSE
  llReturn = .F.
  *B603662,1 [END]

ENDIF    && End of IF SEEK(PADR(lcProgName , 10) + PADR(lcEvent , 10))

*-- Restore the old alias
SELECT (lnOldAlias)

RETURN (llReturn)


*!*************************************************************
*! Name      : lfInit
*! Developer : Heba Amin
*! Date      : 05/31/2005
*! Purpose   : Init Function of the screen
*!*************************************************************
FUNCTION lfInit
PARAMETERS loFormSet
DO CASE
  CASE lcXType = 'O'
    lcCaption=IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soshlb_Order,oAriaApplication.GetHeaderText("LANG_Soshlb_Order",AHEADERFILE))+' # '+;
              lcTrans+ IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soshlb_Labels,oAriaApplication.GetHeaderText("LANG_Soshlb_Labels",AHEADERFILE))

  CASE lcXType = 'P'
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
    *lcCaption=LANG_Soshlb_pick_ticket+' '+lcTrans
    lcCaption=IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soshlb_pick_ticket,oAriaApplication.GetHeaderText("LANG_Soshlb_pick_ticket",AHEADERFILE))+' '+lcTrans
    *N000682,1 11/20/2012 MMT Globlization changes[End]

  CASE lcXType = 'I'
    lcCaption= IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soshlb_Invoice,oAriaApplication.GetHeaderText("LANG_Soshlb_Invoice",AHEADERFILE)) +' # '+lcTrans+ ;
	IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Soshlb_Labels,oAriaApplication.GetHeaderText("LANG_Soshlb_Labels",AHEADERFILE))

    loFormSet.AriaForm1.check1.visible=.F.
    loFormSet.AriaForm1.Label2.visible=.F.
ENDCASE
loFormSet.AriaForm1.Caption = lcCaption
*!*************************************************************
*! Name      : lfEditLabelNo
*! Developer : Heba Amin
*! Date      : 05/31/2005
*! Purpose   : Code Executed when click on Ok Button Of the Screen
*!*************************************************************
FUNCTION lfEditLabelNo
PARAMETERS loFormSet
lnLabelNo = loFormSet.AriaForm1.Text1.value
RETURN lnLabelNo

*!*************************************************************
*! Name      : lfEmpty
*! Developer : Heba Amin
*! Date      : 05/31/2005
*! Purpose   : Code Executed when click on Cancel Button Of the Screen
*!*************************************************************
FUNCTION lfEmpty
PARAMETERS loFormSet
lnLabelNo = IIF(lcXType = 'I',lnLab,lnLabels)

*!*************************************************************
*! Name      : lfDisplay
*! Developer : Heba Amin
*! Date      : 05/31/2005
*! Purpose   : Don't Show Function of the screen
*!*************************************************************
FUNCTION lfDisplay
PARAMETERS loFormSet
llcbDspDlg =IIF(loFormSet.ariaForm1.check1.Value=1,.T.,.F.)

*! E303086,1 New Shipping label form T[T20120216.0114][start]
*!*************************************************************
*! Name      : lfGetCrtDet
*! Developer : Mariam Mazhar(MMT)
*! Date      : 03/11/2012
*! Purpose   : get carton details
*!*************************************************************
FUNCTION lfGetCrtDet
LPARAMETERS lcKeyValue,lnCartonNo,lcTranType

DO CASE
  CASE lcTranType= 'O'
    *ORDERPCK   && ORDER+STORE+PACK_NO
    SELECT PACK_HDR_C
    =gfSetOrder('ORDERPCK')
    IF gfSeek(lcKeyValue)
      SELECT PACK_LIN_C
      =gfSetOrder('PACKSTYLE')
      =gfSeek(PACK_HDR_C.PACK_No++STR(lnCartonNo,4))
      SCAN REST WHILE PACK_NO+STR(NO_CART,4)+STYLE+DYELOT = PACK_HDR_C.PACK_No++STR(lnCartonNo,4)
       =gfSeek(PACK_LIN_C.STYLE,'STYLE_C')
       =gfSeek('S'+STYLE_C.SCALE,'SCALE_C')
       FOR lnCntS =  1 TO SCALE_C.cnt
         lcCntS  = STR(lnCntS ,1)
         IF PACK_LIN_C.QTY&lcCntS. <> 0
           m.Style =PACK_LIN_C.STYLE
           m.PIKTKT = PACK_LIN_C.PACK_NO
           m.SCLCNT = lnCntS
           m.Qty = PACK_LIN_C.QTY&lcCntS.
           m.cDim1 = SCALE_C.cDim1
           m.Size = SCALE_C.SZ&lcCntS.
           m.nLblsNo = PACK_LIN_C.NO_CART
           INSERT INTO (lcCrtTmp) FROM MEMVAR
         ENDIF
       ENDFOR
      ENDSCAN
    ENDIF
  CASE lcTranType= 'I'
    *  lfGetCrtDet(INVOICE+STORE,nLblsNo)
    =SEEK(SUBSTR(lcKeyValue,1,6),'INVHDR')
    IF INVHDR.CONSOL = 'Y' .AND. SEEK(lcKeyValue,'CONSINVH','CONSINVH')
      lfGetCrtDet(CONSINVH.ORDER+CONSINVH.STORE,lnCartonNo,'O')
    ELSE
      lfGetCrtDet(INVHDR.ORDER+INVHDR.STORE,lnCartonNo,'O')
    ENDIF
  CASE lcTranType= 'P'
    SELECT PACK_LIN_C
    =gfSetOrder('PACKSTYLE')
    IF  gfSeek(lcKeyValue+STR(lnCartonNo,4),'PACK_LIN_C','PACKSTYLE')
      SCAN REST WHILE PACK_NO+STR(NO_CART,4)+STYLE+DYELOT = lcKeyValue+STR(lnCartonNo,4)
       =gfSeek(PACK_LIN_C.STYLE,'STYLE_C')
       =gfSeek('S'+STYLE_C.SCALE,'SCALE_C')
       FOR lnCntS =  1 TO SCALE_C.cnt
         lcCntS  = STR(lnCntS ,1)
         IF PACK_LIN_C.QTY&lcCntS. <> 0
           m.Style =PACK_LIN_C.STYLE
           m.PIKTKT = PACK_LIN_C.PACK_NO
           m.SCLCNT = lnCntS
           m.Qty = PACK_LIN_C.QTY&lcCntS.
           m.cDim1 = SCALE_C.cDim1
           m.Size = SCALE_C.SZ&lcCntS.
           m.nLblsNo = PACK_LIN_C.NO_CART
           INSERT INTO (lcCrtTmp) FROM MEMVAR
         ENDIF
       ENDFOR
      ENDSCAN
    ENDIF
ENDCASE
*!*************************************************************
*! Name      : lfGetDivLName
*! Developer : Mariam Mazhar(MMT)
*! Date      : 03/11/2012
*! Purpose   : get division long Name related field
*!*************************************************************
FUNCTION lfGetDivLName
lcAlias = ALIAS()
IF !USED('Codes')
  =gfOpenTable('Codes','Codes','SH')
ENDIF
SELECT Codes
gfSetorder('Codes')
IF gfSEEK('N'+ORDHDR.cDivision +'Y'+'CDIVISION')
  SCAN REST WHILE cdefcode+ccode_no+crltfield+cfld_name = 'N'+ORDHDR.cDivision +'Y'+'CDIVISION'
    IF crltd_nam = 'DIVLNAME  '
      lcDivLName = crltd_vlu
    ENDIF
  ENDSCAN
ENDIF
SELECT (lcAlias)
*!*************************************************************
*! Name      : lfvCrtRng
*! Developer : Mariam Mazhar(MMT)
*! Date      : 03/11/2012
*! Purpose   : Validate carton range field
*!*************************************************************
FUNCTION lfvCrtRng
LPARAMETERS lcCrtrng,lcROldValue
IF !EMPTY(lcCrtrng)
  lcCrtRange = ALLTRIM(lcCrtrng)
  STORE '' TO lcCarton,lcList,lcRange,lcFinal
  FOR lnCount = 1 TO LEN(lcCrtRange)
    IF (!ISDIGIT(SUBSTR(lcCrtRange,lnCount,1)) AND !INLIST(SUBSTR(lcCrtRange,lnCount,1),',','-')) OR ;
      INLIST(LEFT(lcCrtRange,1),',','-') OR ;
      (INLIST(SUBSTR(lcCrtRange,lnCount,1),',','-') AND !ISDIGIT(SUBSTR(lcCrtRange,lnCount+1,1)))
      *N000682,1 MMT 02/06/2013 Globalization changes[Start]
      *=MessageBox("Invalid Carton range.",16,_Screen.Caption)
	  =MessageBox(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_INVALIDCRTRANGE,oAriaApplication.GetHeaderText("LANG_INVALIDCRTRANGE",AHEADERFILE)),16,_Screen.Caption)
      *N000682,1 MMT 02/06/2013 Globalization changes[END]
      REPLACE cCrtRange WITH lcROldValue IN (lclblTbl)
      RETURN
    ELSE
      REPLACE cCrtRange WITH lcCrtrng IN (lclblTbl)
    ENDIF
  ENDFOR
ENDIF
*!*************************************************************
*! Name      : lfAftrInit
*! Developer : Mariam Mazhar(MMT)
*! Date      : 03/11/2012
*! Purpose   : carton range screen init
*!*************************************************************
FUNCTION lfAftrInit
PARAMETERS loFormSet
lnCurAlis = SELECT()
STORE '' TO loFormSet.lastoresarr
IF loFormSet.lctrantype = 'P'
  loFormSet.AriaForm1.cbostore.Enabled = .F.
  loFormSet.lastoresarr  [1] = PIKTKT.STORE
ELSE
  DO CASE
    CASE loFormSet.lctrantype  ="I"  and INVHDR.CONSOL = 'Y'
      IF SEEK(INVHDR.INVOICE,'CONSINVH')
        STORE '' TO loFormSet.lastoresarr
        SELECT CONSINVH
        SCAN REST WHILE INVOICE+STORE+ORDER+PIKTKT= INVHdR.INVOICE
          IF ASCAN(loFormSet.lastoresarr,CONSINVH.STORE)>0
            LOOP
          ENDIF
          IF EMPTY(loFormSet.lastoresarr[1])
            loFormSet.lastoresarr[1] = CONSINVH.STORE
          ELSE
            DIMENSION  loFormSet.lastoresarr[ALEN(loFormSet.lastoresarr,1)+1]
            loFormSet.lastoresarr[ALEN(loFormSet.lastoresarr,1)] =CONSINVH.STORE
          ENDIF
        ENDSCAN
      ENDIF
    CASE loFormSet.lctrantype  ="O"  and ordhdr.multi = 'Y'
      IF SEEK(ORDHdR.CORDTYPE+ORDHdR.ORDER,'ORDLINE')
        STORE '' TO loFormSet.lastoresarr
        SELECT ORDLINE
        SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6)= ORDHdR.CORDTYPE+ORDHdR.ORDER
          IF ASCAN(loFormSet.lastoresarr,ORDLINE.STORE)>0
            LOOP
          ENDIF
          IF EMPTY(loFormSet.lastoresarr[1])
            loFormSet.lastoresarr[1] = ORDLINE.STORE
          ELSE
            DIMENSION  loFormSet.lastoresarr[ALEN(loFormSet.lastoresarr,1)+1]
            loFormSet.lastoresarr[ALEN(loFormSet.lastoresarr,1)] =ORDLINE.STORE
          ENDIF
        ENDSCAN
      ENDIF
    CASE loFormSet.lctrantype  ="I"  and INVHDR.CONSOL <> 'Y'
      loFormSet.lastoresarr[1] = INVHDR.STORE
      loFormSet.AriaForm1.cbostore.Enabled = .F.
    CASE loFormSet.lctrantype  ="O"  and ordhdr.multi <> 'Y'
      loFormSet.lastoresarr[1] = ORDHDR.STORE
      loFormSet.AriaForm1.cbostore.Enabled = .F.
  ENDCASE
ENDIF
loFormSet.AriaForm1.cbostore.requery ()
loFormSet.AriaForm1.cbostore.Value =loFormSet.lastoresarr[1]
lfRefCrtRng(loFormSet)
SELECT(lnCurAlis)
*!*************************************************************
*! Name      : lfRefCrtRng
*! Developer : Mariam Mazhar(MMT)
*! Date      : 03/11/2012
*! Purpose   : Refresh carton range field
*!*************************************************************
FUNCTION lfRefCrtRng
LPARAMETERS loFormSet
lcTranNo = ''
DO CASE
 CASE loFormSet.lctrantype  ="I"
  lcTranNo = INVHDR.INVOICE
 CASE loFormSet.lctrantype  ="O"
  lcTranNo = ORDHDR.ORDER
 CASE loFormSet.lctrantype  ="P"
 lcTranNo = PIKTKT.PIKTKT
ENDCASE
=SEEK(lcTranNo+IIF(loFormSet.lctrantype  $ "IO",loFormSet.AriaForm1.cbostore.Value,''),lclblTbl)
loFormSet.AriaForm1.Text1.Value =&lclblTbl..cCrtRange
*! E303086,1 New Shipping label form T[T20120216.0114][END]
