*:***************************************************************************
*: Program file  : MSCONJL.PRG
*: Program desc. : CONVERT CUTTING TICKETS TO PURCHASE ORDERS FOR J&L
*: Date          : 15/05/2001
*: System        : Aria Advantage Series.
*: Module        : System Manager (SM)
*: Developer     : Ahmed Maher (AMH)
*: Tracking Job Number: C102229
*:***************************************************************************
*: Calls : 						
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO SMCONJL
*:***************************************************************************
*: Modifications :
*:***************************************************************************

*--You have to select the company before running the conversion,
*--since this company will be the target location for the conversion.

*--variable to get color Segmant.
STORE 0 TO lnColorStr, lnColorLen, lnMajorLen

*-- Initializing the necessary variables.
STORE SPACE(0)  TO lcSorcCo

IF EMPTY(gcAct_Comp)
  =gfModalGen('TRM00321B00000','DIALOG')
  RETURN
ENDIF

*C102229,4 AMH Get Pur. Price label [Start]
lcPPrcLbl = gfGetMemVar('M_CISLBL1' , gcAct_Comp)
*C102229,4 [End]

IF !gfSetup()
  RETURN
ENDIF  

*-- Get color Segmant and major length.
=lfGetColor()
lnMajorLen = LEN(gfItemMask("PM"))

DO (gcScrDir+gcWinAppl+"\SMCONJL.SPX")

*!*************************************************************
*! Name      : lfvGetDir
*: Date      : 15/05/2001
*: Developer : Ahmed Maher (AMH)
*! Purpose   : To allow the user to select company directory.
*!*************************************************************
*! Example            : =lfvGetDir()
*!*************************************************************
*
FUNCTION lfvGetDir

lcSorcCo  = GETDIR('','Select source company')
SHOW GET lcSorcCo
IF EMPTY(lcSorcCo)
  SHOW GET pbOK DISABLE
ELSE
  SHOW GET pbOK ENABLE
ENDIF
*-- end of lfvGetDir.

*!*************************************************************
*! Name      : lfvCancel
*: Date      : 15/05/2001
*: Developer : Ahmed Maher (AMH)
*! Purpose   : Valid function of the cancel button.
*!*************************************************************
*! Example            : =lfvCancel()
*!*************************************************************
*
FUNCTION lfvCancel

CLEAR READ
*-- end of lfvCancel.

*!*************************************************************
*! Name      : lfvOk
*: Date      : 15/05/2001
*: Developer : Ahmed Maher (AMH)
*! Purpose   : Valid function of the Ok button.
*!*************************************************************
*! Example            : =lfvOk()
*!*************************************************************
*
FUNCTION lfvOk

*-- Check the existance file in the selected directory.
IF !FILE(lcSorcCo+"CUTTKTH.DBF")
  =gfModalGen("INM000000B00000","DIALOG",'','',;
  'CUTTKTH.DBF does not exist in '+lcSorcCo+'. Cannot proceed.')  
  RETURN
ENDIF

IF !FILE(lcSorcCo+"CUTTKTL.DBF")
  =gfModalGen("INM000000B00000","DIALOG",'','',;
  'CUTTKTL.DBF does not exist in '+lcSorcCo+'. Cannot proceed.')  
  RETURN
ENDIF

*-- Open needed files.
=gfOpenFile(lcSorcCo+"CUTTKTH" , lcSorcCo+"CUTTKTHS" , "SH")
=gfOpenFile(lcSorcCo+"CUTTKTL" , lcSorcCo+"CUTTKTL" , "SH")

*-- Convert sequence file.
SELECT SEQUENCE
IF SEEK('CUTTKT')
  lnSeq_No = NSEQ_NO
  IF SEEK('PO'+SPACE(8))
    REPLACE NSEQ_NO WITH lnSeq_No
  ELSE
    SEEK 'CUTTKT'
    REPLACE CSEQ_TYPE WITH 'PO',;
            CFILE_NAM WITH 'POSHDR',;
            CFILE_TAG WITH 'POSHDR'
  ENDIF
ENDIF

*-- Convert style file.
SELECT CODES
SEEK 'DSHIPVIA'
lcShipVia = CCODE_NO
SEEK 'DCPURCODE'
lcPurCode = CCODE_NO
DECLARE laWareAdr[5]
SELECT STYLE
lcStyMajor = SPACE(19)
LOCATE
SCAN
  WAIT WINDOW 'Convert style : ' + STYLE NOWAIT
  REPLACE MAKE    WITH .F.,;
          NICOST1 WITH NMCOST3,;
          NMCOST3 WITH 0
  IF !(CSTYMAJOR == lcStyMajor)
    lcStyMajor = CSTYMAJOR
    IF SEEK(SUBSTR(lcStyMajor,1,lnMajorLen),'CUTTKTH')
      *-- Convert CUTTKTH file.
      SELECT CUTTKTH
      SCAN REST WHILE STYLE + CUTTKT = SUBSTR(lcStyMajor,1,lnMajorLen)
        WAIT WINDOW 'Convert C/T # : ' + CUTTKT NOWAIT
        SELECT APVENDOR
        SEEK IIF(!EMPTY(CUTTKTH.CONTR1),CUTTKTH.CONTR1,;
             IIF(!EMPTY(CUTTKTH.CONTR2),CUTTKTH.CONTR2,;
             IIF(!EMPTY(CUTTKTH.CONTR3),CUTTKTH.CONTR3,'')))
        =SEEK('DEFWHE','WAREHOUS')
        lcWareDesc   = WAREHOUS.cDesc
        laWareAdr[1] = gfGetAdr('WAREHOUS' , '' , '' , '' , 1)
        laWareAdr[2] = gfGetAdr('WAREHOUS' , '' , '' , '' , 2)
        laWareAdr[3] = gfGetAdr('WAREHOUS' , '' , '' , '' , 3)
        laWareAdr[4] = gfGetAdr('WAREHOUS' , '' , '' , '' , 4)
        laWareAdr[5] = gfGetAdr('WAREHOUS' , '' , '' , '' , 5)
        SELECT POSHDR
        IF !SEEK('P'+CUTTKTH.CUTTKT)
          APPEND BLANK
          REPLACE PO         WITH CUTTKTH.CUTTKT,;
                  STATUS     WITH CUTTKTH.STATUS,;
                  ENTERED    WITH CUTTKTH.ENTERED,;
                  COMPLETE   WITH CUTTKTH.COMPLETE,;
                  NSTYORDER  WITH CUTTKTH.PCS_BUD,;
                  RECEIVE    WITH CUTTKTH.PCS_REC,;
                  DAMAGE     WITH CUTTKTH.PCS_DAM,;
                  OPEN       WITH CUTTKTH.PCS_OPN,;
                  PCS_ACT    WITH CUTTKTH.PCS_ACT,;
                  CANCEL     WITH CUTTKTH.PCS_CAN
          REPLACE POCANCEL   WITH CUTTKTH.DEL_DATE,;
                  ACT_DATE   WITH CUTTKTH.ACT_DATE,;
                  ORDER_COMP WITH CUTTKTH.INI_COMP,;
                  NICOST1    WITH CUTTKTH.NEST_COST3,;
                  NACT_COST1 WITH CUTTKTH.NACT_COST3,;
                  NLAN_COST1 WITH CUTTKTH.NLAN_COST3,;
                  TOTORD     WITH CUTTKTH.TOTORD
          REPLACE CWARECODE  WITH 'DEFWHE',;
                  VENDOR     WITH APVENDOR.CVENDCODE,;
                  DADD_DATE  WITH CUTTKTH.ADD_DATE,;
                  CADD_TIME  WITH CUTTKTH.ADD_TIME,;
                  CADD_USER  WITH CUTTKTH.ADD_USERID,;
                  CSTYTYPE   WITH 'P',;
                  CPURCODE   WITH lcPurCode,;
                  AVAILABLE  WITH CUTTKTH.COMPLETE,;
                  CTERMCODE  WITH APVENDOR.CTERMCODE,;
                  SHIPVIA    WITH lcShipVia
          REPLACE SHPNAME    WITH lcWareDesc,;
                  POTOTAL    WITH NICOST1   ,;
                  LMULTIWARE WITH .F.,;
                  COUTADDR1  WITH laWareAdr[1],;
                  COUTADDR2  WITH laWareAdr[2],;
                  COUTADDR3  WITH laWareAdr[3],;
                  COUTADDR4  WITH laWareAdr[4],;
                  COUTADDR5  WITH laWareAdr[5],;
                  CPRICECUR  WITH gcBaseCurr,;
                  CDUTYCUR   WITH gcBaseCurr
          REPLACE NPRICERAT  WITH 1,;
                  NDUTYRAT   WITH 1,;
                  NFCOST1    WITH NICOST1,;
                  NFACTCOST1 WITH NACT_COST1,;
                  NFLANCOST1 WITH NLAN_COST1,;
                  CLASTOPR   WITH IIF(EMPTY(CUTTKTH.MFG_OPR3),CUTTKTH.MFG_OPR3,;
                                  IIF(EMPTY(CUTTKTH.MFG_OPR2),CUTTKTH.MFG_OPR2,CUTTKTH.MFG_OPR1)),;
                  NCURRUNIT  WITH 1
          REPLACE NDCURUNIT  WITH 1,;
                  LHASNOTES  WITH SEEK('I'+PO,'NOTEPAD')
          IF SEEK(CUTTKTH.CUTTKT,'CUTTKTL')
            *-- Convert CUTTKTL file.
            SELECT CUTTKTL
            lnLineNo = 0
            SCAN REST WHILE CutTkt+Style+Color+Dyelot+TranCd+STR(RECNO(),7) = CUTTKTH.CUTTKT
              WAIT WINDOW 'Convert lines for C/T # : ' + CUTTKT NOWAIT
              SCATTER MEMVAR
              SELECT POSLN
              APPEND BLANK
              GATHER MEMVAR
              lnLineNo = lnLineNo + 1
              *C102229,4 AMH Fix converting style field [Start]
              *REPLACE PO         WITH m.CutTkt,;
                      STYLE      WITH Style + '-' + m.Color,;
                      CSTYTYPE   WITH 'P',;
                      LINENO     WITH lnLineNo,;
                      VENDOR     WITH POSHDR.VENDOR,;
                      SCALE      WITH STYLE.SCALE,;
                      CVENSTY    WITH STYLE.CVENSTY,;
                      NCOST1     WITH NCOST3
              REPLACE PO         WITH m.CutTkt,;
                      STYLE      WITH SUBSTR(Style,1,lnMajorLen) + '-' + m.Color,;
                      CSTYTYPE   WITH 'P',;
                      LINENO     WITH lnLineNo,;
                      VENDOR     WITH POSHDR.VENDOR,;
                      SCALE      WITH STYLE.SCALE,;
                      CVENSTY    WITH STYLE.CVENSTY,;
                      NCOST1     WITH NCOST3
              *C102229,4 [End]
              REPLACE NECOST1    WITH NCOST3,;
                      NCOST3     WITH 0,;
                      NLAN_CST1  WITH NLAN_CST3,;
                      NELANCOST1 WITH NLAN_CST3,;
                      NLAN_CST3  WITH 0,;
                      NACT_CST1  WITH NACT_CST3,;
                      NEACTCOST1 WITH NACT_CST3,;
                      NACT_CST3  WITH 0,;
                      GROS_PRICE WITH NCOST1,;
                      DPOSTDATE  WITH DATE
              REPLACE NLANPRRAT  WITH 1,;
                      NLANDURAT  WITH 1,;
                      NACTPRRAT  WITH 1,;
                      CWARECODE  WITH 'DEFWHE'
              IF VAL(TRANCD) > 2
                REPLACE TRANCD WITH STR(VAL(TRANCD)+1,1)
              ENDIF
            ENDSCAN
            REPLACE POSHDR.LASTLINE WITH lnLineNo
          ENDIF
          IF SEEK('M'+CUTTKTH.CUTTKT+'3','CTKTBOM')
            *-- Convert CTKTBOM file.
            SELECT CTKTBOM
            DO WHILE cImTyp+CutTkt+Typ+Item+Iclr+MfgCode+Dyelot = 'M'+CUTTKTH.CUTTKT+'3'             
              WAIT WINDOW 'Convert C/T cost sheet for C/T # : ' + CUTTKT NOWAIT
              REPLACE TYP      WITH '1',;
                      DESC     WITH '',;
                      CCATGTYP WITH 'P',;
                      MFGCODE  WITH IIF(STYLE.LDETCOST,'******','*1'),;
                      CIMTYP   WITH 'I'
              =SEEK('M'+CUTTKTH.CUTTKT+'3')
            ENDDO
          ENDIF
          IF SEEK('M'+CUTTKTH.CUTTKT,'BOMLINE')
            *-- Convert BOMLINE file.
            SELECT BOMLINE
            DO WHILE cImTyp+cTktNo+cOprCode+cType+Style+Sclr+;
                            IIF(cCatgTyp$"MDP",PADR(MfgCode,12),Item)+Iclr = 'M'+CUTTKTH.CUTTKT
              WAIT WINDOW 'Convert C/T cost sheet for C/T # : ' + CTKTNO NOWAIT
              REPLACE CBOMTYP  WITH '1',;
                      CCATGTYP WITH 'P',;
                      MFGCODE  WITH IIF(STYLE.LDETCOST,'******','*1'),;
                      CIMTYP   WITH 'I'
              =SEEK('M'+CUTTKTH.CUTTKT)
            ENDDO
          ENDIF
          IF SEEK('3M'+CUTTKTH.CUTTKT,'BOMCOST')
            *-- Convert BOMCOST file.
            SELECT BOMCOST
            DO WHILE cBomType+cImTyp+CtktNo+Item+Iclr+MfgCode+cWareCode+cDyelot+cRsession+;
                            cIsession = '3M'+CUTTKTH.CUTTKT
              WAIT WINDOW 'Convert C/T cost sheet for C/T # : ' + CTKTNO NOWAIT
              REPLACE CBOMTYPE  WITH '1',;
                      CCOSTTYPE WITH 'P',;
                      MFGCODE   WITH IIF(STYLE.LDETCOST,'******','*1'),;
                      CIMTYP    WITH 'I'
              =SEEK('3M'+CUTTKTH.CUTTKT)
            ENDDO
          ENDIF
          IF SEEK('I'+CUTTKTH.CUTTKT,'NOTEPAD')
            *-- Convert NOTEPAD file.
            SELECT NOTEPAD
            WAIT WINDOW 'Convert C/T Note Pad for C/T # : ' + KEY NOWAIT
            REPLACE TYPE  WITH 'P',;
                    CDESC WITH 'Notes For Style P/o Number : ' + ALLTRIM(KEY)
          ENDIF
          IF SEEK(SUBSTR(STYLE.STYLE,1,lnMajorLen),'STYINVJL')
            *-- Convert STYINVJL file.
            SELECT STYINVJL
            SCAN REST WHILE Style+cWareCode+cSession+DTOS(dTrDate)+cTrCode+STR(LineNo,6) =;
                            SUBSTR(STYLE.STYLE,1,lnMajorLen) FOR CTRCODE = CUTTKTH.CUTTKT
              REPLACE CTRTYPE WITH '6'
            ENDSCAN
          ENDIF
        ENDIF
      ENDSCAN
    ENDIF
    IF SEEK(lcStyMajor+'3','BOM')
      *-- Convert BOM file.
      SELECT BOM
      DO WHILE cItmMajor+Typ+cItmMask+MfgCode+Item+Iclr = lcStyMajor + '3'
        WAIT WINDOW 'Convert style cost sheet for style : ' + CITMMAJOR NOWAIT
        *C102229,4 AMH Fix conversion of citmmask and desc fields [Start]
        *REPLACE TYP      WITH '1',;
                DESC     WITH 'Purchase Price',;
                CCATGTYP WITH 'P',;
                MFGCODE  WITH IIF(STYLE.LDETCOST,'******','*1'),;
                CITMMASK WITH STUFF(STYLE.STYLE,lnColorStr,lnColorLen,'******')
        REPLACE TYP      WITH '1',;
                DESC     WITH lcPPrcLbl,;
                CCATGTYP WITH 'P',;
                MFGCODE  WITH IIF(STYLE.LDETCOST,'******','*1')
        *C102229,4 [End]
        =SEEK(lcStyMajor+'3')
      ENDDO
    ENDIF
  ENDIF
ENDSCAN

WAIT CLEAR
=gfModalGen("INM000000B00000","DIALOG",'','','Converting data has been successfully completed.')
STORE SPACE(0)  TO lcSorcCo
SHOW GET lcSorcCo
SHOW GET pbOK DISABLE
*-- end of lfvOk.

*!*************************************************************
*! Name      : lfGetColor
*! Developer : Ahmed Maher
*! Date      : 05/15/2001
*! Purpose   : Get the color information.
*!*************************************************************
*! Calls     : gfItemMask
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : lfGetColor()
*!*************************************************************
*
FUNCTION lfGetColor

*-- Array hold the segmants specifications of the style code structure.
DECLARE laStySeg[1,1]

*-- Count of the major part.
lnMjorCnt  = gfItemMask("SM")
  
*-- Fill an array with the segments strucure, & loop in it to 
*-- know if there a color segment in the style code strucure.
=gfItemMask(@laStySeg)
FOR lnCnt = lnMjorCnt + 1 TO ALEN(laStySeg,1)
  IF laStySeg[lnCnt , 1] = "C"
    *-- Flag to know if there is color in the style code strucure.
    llColorExt = .T.
    *-- Var. hold the start position of the color segment in the style code strucure.
    lnColorStr = laStySeg[lnCnt , 4]
    *-- Var. hold the color segment lenght in the style code strucure.
    lnColorLen = LEN(laStySeg[lnCnt , 3])
  ENDIF
ENDFOR
*-- end of lfGetColor.