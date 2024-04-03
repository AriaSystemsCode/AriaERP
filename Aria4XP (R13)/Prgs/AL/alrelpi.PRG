*!*****************************************************************************************
*! Name      : ALRELPI.PRG
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 07/27/2005
*! Purpose   : Release Picking Ticket Screen
*! Entry no. : N037426 - Release Picking Ticket Screen
*!*****************************************************************************************
*: Modifications:
*: B607897,1 MMT 12/21/2006 Bug of no data in Case of England Company (T20060829.0003)
*: B608043,1 AYM 04/15/2007  when select by style release only the lines for this style (t20060829.0002)
*: C200876,1 TMI 05/02/2008 Adding the BIN LOcation triggers
*: C201334,1 MMT 05/11/2011 Custom Pick and Pack monitor screen[T20110401.0003]
*! E303079,1 MMT 06/28/2012 Fixing Media issues[T20120304.0004]
*! B611624,1 Es 08/30/2018 Modify the issue of warehouse mismatching  between Ordhdr and ORDLINE"  [T20180717.0024]
*!*****************************************************************************************
#INCLUDE R:\ARIA4xp\SCREENS\AL\ALRELPI.H

*Define a public variable to be access from the option grid and screen class
PUBLIC lnPrintStatV ,lnDataSessPre,llCallOption,laScopExpr,llSaveCriteria

DECLARE laScopExpr[1]

STORE "" TO laScopExpr
lnPrintStatV = 1
llCallOption = .F.
lndataSessPre = ""
llSaveCriteria = .F.
*Call the form
DO FORM (oAriaApplication.ScreenHome+"\AL\ALRELPIK.SCX")


RETURN

*Business class
DEFINE CLASS Release_PkTkt AS Custom
  lcTmpKtTk = ""
  loFormSet = .F.
  loForm = .F.
  llBrowse = .F.
  llExCalled = .F.
  lcSentPik = ""
  llSekPak = .F.    &&picking ticket has packing list or not
  llKeyBrowse = .F. &&use to browse by pressing key
  DIMENSION laFileStru[1]
  laFileStru = ""
  lnUnSelRec = 0
  lnSelRec = 0
  lnDelRec   = 0
  lcCaptionSel = ""
  llCalledFromOp = .F.
  llMultiWH  = .F.
  llDyelot   = .F.
  lcTempPikTkt = ""
  *N037426,1 MMT 08/11/2005,make option grid opened after screen opened [START]
  llCallScop = .F.   &&Flag to hold the first time of the session
  llFrstTime = .T.
  *N037426,1 MMT 08/11/2005,make option grid opened after screen opened [END]

*!*************************************************************
*! Name      : lfInit
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 07/27/2005
*! Purpose   : init function of th from
*!*************************************************************
*!
FUNCTION lfInit
  LPARAMETERS loFrm

  SET MULTILOCKS ON
  This.loForm    = loFrm
  This.loFormSet = loFrm.Parent

  This.lfopenfile('PikTkt','PikTkt')
  This.lfopenfile('OrdHdr','OrdHdr')
  This.lfopenfile('OrdLine','OrdLine')
  This.lfopenfile('PikLine','PikLine')
  This.lfopenfile('PACK_HDR','PACK_HDR')
  This.lfopenfile('WAREHOUS','WAREHOUS')
  This.lfopenfile('Scale','Scale')
  This.lfopenfile('STYDYE','STYDYE')
  This.lfopenfile('STYLE','STYLE')
  This.lcTmpKtTk  = gfTempName()

  SELECT PIKTKT
  =AFIELDS(This.laFileStru)
  lnFileStru = ALEN(This.laFileStru,1)

  DIMENSION This.laFileStru[lnFileStru+3,18]

  This.laFileStru[lnFileStru+1,1] = 'LLSEL'
  This.laFileStru[lnFileStru+1,2] = 'L'
  This.laFileStru[lnFileStru+1,3] = 1
  This.laFileStru[lnFileStru+1,4] = 0

  This.laFileStru[lnFileStru+2,1] = 'NORDLNNO'
  This.laFileStru[lnFileStru+2,2] = 'N'
  This.laFileStru[lnFileStru+2,3] = 9
  This.laFileStru[lnFileStru+2,4] = 0

  This.laFileStru[lnFileStru+3,1] = 'NPROCNO'
  This.laFileStru[lnFileStru+3,2] = 'N'
  This.laFileStru[lnFileStru+3,3] = 2
  This.laFileStru[lnFileStru+3,4] = 0

  FOR lnLoop = 1 TO  3
    STORE ' ' TO  This.laFileStru[lnFileStru +lnLoop,7],This.laFileStru[lnFileStru+lnLoop,8],;
                  This.laFileStru[lnFileStru+lnLoop,9],This.laFileStru[lnFileStru+lnLoop,10],;
                  This.laFileStru[lnFileStru+lnLoop,11],This.laFileStru[lnFileStru+lnLoop,12],;
                  This.laFileStru[lnFileStru+lnLoop,13],This.laFileStru[lnFileStru+lnLoop,14],;
                  This.laFileStru[lnFileStru+lnLoop,15],This.laFileStru[lnFileStru+lnLoop,16]
    STORE 0   TO  This.laFileStru[lnFileStru+lnLoop,17] ,This.laFileStru[lnFileStru+lnLoop,18]
  ENDFOR

  lcTempFileTkt = This.lcTmpKtTk
  ACOPY(This.laFileStru,laStaracture)
  =gfCrtTmp(lcTempFileTkt,@laStaracture,"PIKTKT",lcTempFileTkt,.T.)
  This.lfAddControlSource()

  WITH This.loFormSet
    .nWorkArea        = 'PikTkt'
    .DataEnvironment.InitialSelectedAlias = 'PikTkt'
    .cbrowsetabledbengine   = 'NATIVE'
  ENDWITH

  This.llMultiWH  = (ALLTRIM(gfGetMemVar('M_WareHouse')) = 'Y')
  This.llDyelot   = (ALLTRIM(gfGetMemVar('M_DYELOT'))    = 'Y')
  This.lcTempPikTkt = gfTempName()
  *N037426,1 MMT 08/11/2005,make option grid opened after screen opened [START]
  This.llCallScop = .F.   &&Flag to hold the first time of the session
  This.llFrstTime = .T.       && Flag to know if we are going to call lpShow for the first time
  *N037426,1 MMT 08/11/2005,make option grid opened after screen opened [END]
ENDFUNC
*!*************************************************************
*! Name      : lfvScope
*! Developer : Mariam Mazhar [MMT]
*! Date      : 08/02/2005
*! Purpose   : Valid function of push button Scope
*!*************************************************************
*! Called from : Control Panel , lpShow
*!*************************************************************
*! Calls       : gfOpGrid() , lfCrTmKtTk() , lfDispBrow()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*!
FUNCTION lfvScope

 lndataSessPre = SET("Datasession" )
 IF This.llCalledFromOp
   llCallOption = This.llCalledFromOp
   =ACOPY(This.loFormSet.laSeleCritria,laScopExpr)
 ENDIF
 *N037426,1 MMT 08/11/2005,make option grid opened after screen opened [START]
 This.llCallScop = .F.             && Screen Already Initialized
 *N037426,1 MMT 08/11/2005,make option grid opened after screen opened [END]
 lcDataSessI = SET("Datasession" )&&THIS.loFormSet.DatasessionID

 lcExpr = gfOpGrid('ALRELPI' , .T.)&&,.F.,.F.,.T.,.T.)
 SET DATASESSION TO lcDataSessI

 IF lcExpr <> ".F."
   This.lfCrTmKtTk()
   =ACOPY(laScopExpr,This.loFormSet.laSeleCritria)
   SELECT (This.lcTmpKtTk)
   LOCATE
   This.lnSelRec   = 0
   This.lnDelRec   = 0
   This.loFormSet.llenablerel = .F.
   This.lnUnSelRec = RECCOUNT()
   IF !EOF()
     This.loFormSet.llEnableInvert = .T.
     This.loFormSet.llEnableSelect = .T.
     This.loFormSet.llEnableSelectall = .T.
     This.loFormSet.llEnableSelectnone = .F.
   ELSE    && Else
     This.loFormSet.llEnableInvert = .F.
     This.loFormSet.llEnableSelect = .F.
     This.loFormSet.llEnableSelectAll = .F.
     This.loFormSet.llEnableSelectNone = .F.
   ENDIF    && End of IF
 ELSE
  RETURN
 ENDIF
*!*****************************************************************************************
*! Name      : lfOpenFile
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 08/02/2005
*! Purpose   : open Files remotely
*!*****************************************************************************************
FUNCTION lfOpenFile
PARAMETERS lcFile, lcTag
LOCAL lcProp
  lcFile = JUSTSTEM(lcFile)
  lcTag = JUSTSTEM(lcTag)
  lcProp = lcFile
  IF !PEMSTATUS(This,lcProp,5)
    This.addproperty(lcProp)
  ENDIF
  lcProp = 'This.'+lcProp

  IF TYPE(lcProp)<>'O'
    &lcProp = CREATEOBJECT("RemoteTable",lcFile,lcTag,lcFile,This.loFormSet.DataSessionID)
  ELSE
    &lcProp..SetOrder(lcTag)
  ENDIF
ENDFUNC

*!*************************************************************
*! Name      : lfAddControlSource
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 07/27/2005
*! Purpose   : function to control source
*!*************************************************************
FUNCTION lfAddControlSource

  WITH This.loFormSet.ariaform1.grdPiktkt.grdMultiSelectionGrid
    .RecordSource = ""
    .RecordSource = This.lcTmpKtTk
    .Column1.Header1.Caption = ""
    .Column1.CurrentControl = "AriaCheckBox1"
    .column1.ControlSource ='Thisformset.loreleaseobj.mgetValueLogic()'
    .column2.ControlSource = This.lcTmpKtTk+'.PIKTKT'
    .column3.ControlSource = This.lcTmpKtTk+'.ACCOUNT'
    .column4.ControlSource = This.lcTmpKtTk+'.STORE'
    .column5.ControlSource = This.lcTmpKtTk+'.ORDER'
    .column6.ControlSource = This.lcTmpKtTk+'.DATE'
    .column7.ControlSource = This.lcTmpKtTk+'.CWARECODE'
    .column8.ControlSource = This.lcTmpKtTk+'.CUSTPO'
    .Column1.Enabled = .T.
    .SETALL('ReadOnly',.T.,'COLUMN')
    .Column1.readonly = .F.
	.Enabled = .T.
    .Column1.AriaCheckBox1.Enabled = .T.
    .refresh()
  ENDWITH
ENDFUNC

*!*************************************************************
*! Name      : lfRelScr
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 07/27/2005
*! Purpose   : function to confirm release pick ticker
*!*************************************************************
FUNCTION lfRelScr
  *** Message : "Are you sure you want to release the selected picking ticket(s) "
  ***           "                   < Yes >           < No >                    "
  IF gfModalGen("TRM44005B00006","DIALOG") = 1
    =This.lfDelScr()
  ENDIF    && End of IF
ENDFUNC

*!*************************************************************
*! Name      : lfDelScr
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 07/27/2005
*! Purpose   : function to release pick ticket
*!*************************************************************
FUNCTION lfDelScr

PRIVATE lcOrdLnTag

*: B608043,1 AYM 04/15/2007  when select by style release only the lines for this style (t20060829.0002)  begin
*check if user has selected a style or not
  llSeleStyle = .F. && flag to indicate if there is any style selected
  lnPosStyle = ASCAN(laScopExpr,"ORDLINE.STYLE")
  IF lnPosStyle  > 0
    lnPosStyle  = ASUBSCRIPT(laScopExpr,lnPosStyle ,1)
    lcFileStyle  =IIF(!EMPTY(laScopExpr[lnPosStyle ,6]),laScopExpr[lnPosStyle ,6],'')
    IF !EMPTY(lcFileStyle ) AND USED(lcFileStyle ) AND RECCOUNT(lcFileStyle )> 0
      llSeleStyle = .T.
    ENDIF
  ENDIF
*: B608043,1 AYM 04/15/2007  when select by style release only the lines for this style (t20060829.0002)  end
SELECT(This.lcTmpKtTk)

*!*	IF ASCAN(This.loFormSet.laEvntTrig , PADR('UPDRELEASE',10)) <> 0
*!*	  llPrepere = .T.
*!*	  =gfDoTriger('ALRELPI',PADR('UPDRELEASE',10))
*!*	ENDIF


*!*	IF ASCAN(This.loFormSet.laEvntTrig , PADR('RELEASPT',10)) <> 0
*!*	  =gfDoTriger('ALRELPI',PADR('RELEASPT',10))
*!*	ENDIF

lcTmpKtTk = This.lcTmpKtTk
LOCATE
This.llSekPak = .F.
SCAN FOR llSel

  This.llSekPak = This.lfSekPak(&lcTmpKtTk..pikTkt)
  IF This.llSekPak
    **Message:"Warning: picking tickets having packing lists where not released"
    **        "                             < Ok >                             "
    =gfModalGen("TRM44004B00000","ALERT")
    RETURN .F.
  ENDIF    && End of IF


  SELECT ORDLINE
  *IF Statment to find the first record in ORDLINE
  IF This.lfSekFirst()

    *SCAN Loop to scan the ORDLINE file For the selected critirea
*: B608043,1 AYM 04/15/2007  when select by style release only the lines for this style (t20060829.0002) begin
*!*	    SCAN REST ;
*!*	      WHILE CORDTYPE+ORDER+STORE+STYLE+STR(LINENO,6) = 'O'+&lcTmpKtTk..Order + &lcTmpKtTk..STORE ;
*!*	          FOR PIKTKT = &lcTmpKtTk..PIKTKT

     SCAN REST ;
      WHILE CORDTYPE+ORDER+STORE+STYLE+STR(LINENO,6) = 'O'+&lcTmpKtTk..Order + &lcTmpKtTk..STORE ;
          FOR PIKTKT = &lcTmpKtTk..PIKTKT AND IIF(llSeleStyle ,SEEK(style,lcfilestyle),.T.)

*: B608043,1 AYM 04/15/2007  when select by style release only the lines for this style (t20060829.0002)  end


      SCATTER MEMVAR MEMO

      IF &lcTmpKtTk..nProcNo = 1
        This.PikLine.Insert("FROM MEMVAR")

        =gfAdd_Info('PIKLINE')
        *This.lfAdd_Info(This.Pikline,'PIKLINE')&& For edit info.

        REPLACE &lcTmpKtTk..nProcNo WITH 2 IN (lcTmpKtTk)
        =RLOCK(lcTmpKtTk)
        UNLOCK IN &lcTmpKtTk
      ENDIF    && End of IF

      IF &lcTmpKtTk..nProcNo = 2

        *IF Statment to check if the system is Dyelot and this style has Dyelot
*        IF This.llDyelot .AND. This.Style.Seek(ORDLINE.STYLE)AND STYLE.cDye_Flg = 'Y'
        IF This.llDyelot .AND. This.Style.Seek(IIF(EMPTY(ORDLINE.AltStyle),ORDLINE.Style,ORDLINE.AltStyle))AND STYLE.cDye_Flg = 'Y'

          *IF Statment to find the STYDYE record for the current PIKTKT record and ORDLINE record
          This.PIKTKT.Seek(&lcTmpKtTk..PIKTKT)

*          IF This.STYDYE.SEEK(ORDLINE.Style + PIKTKT.cWareCode + ORDLINE.Dyelot )
          IF This.STYDYE.SEEK(IIF(EMPTY(ORDLINE.AltStyle),ORDLINE.Style,ORDLINE.AltStyle)+ PIKTKT.cWareCode + ORDLINE.Dyelot )
            SELECT STYDYE
            =RLOCK()
            This.StyDye.REPLACE('Alo1   WITH MAX( Alo1 - ORDLINE.Pik1, 0 ),;
            				 Alo2   WITH MAX( Alo2 - ORDLINE.Pik2, 0 ),;
                     Alo3   WITH MAX( Alo3 - ORDLINE.Pik3, 0 ),;
                     Alo4   WITH MAX( Alo4 - ORDLINE.Pik4, 0 )')

            This.StyDye.REPLACE('Alo5   WITH MAX( Alo5 - ORDLINE.Pik5, 0 ),;
               Alo6   WITH MAX( Alo6 - ORDLINE.Pik6, 0 ),;
   				     Alo7   WITH MAX( Alo7 - ORDLINE.Pik7, 0 ),;
               Alo8   WITH MAX( Alo8 - ORDLINE.Pik8, 0 )')

   	        This.StyDye.REPLACE('TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8')

            =gfAdd_Info('STYDYE')
	          *This.lfAdd_Info(This.STYDYE,'STYDYE')

            UNLOCK
          ENDIF    && End of IF
        ENDIF    && End of IF

        REPLACE &lcTmpKtTk..nProcNo WITH 3 IN (lcTmpKtTk)
        =RLOCK(lcTmpKtTk)
        UNLOCK IN &lcTmpKtTk
      ENDIF    && End of IF

      IF &lcTmpKtTk..nProcNo = 3
        This.PIKTKT.Seek(&lcTmpKtTk..PIKTKT)

*        IF This.STYDYE.SEEK(ORDLINE.Style + PIKTKT.cWareCode + SPACE(10))
        IF This.STYDYE.SEEK(IIF(EMPTY(ORDLINE.AltStyle),ORDLINE.Style,ORDLINE.AltStyle)+ PIKTKT.cWareCode + SPACE(10))
            SELECT STYDYE
            =RLOCK()
            This.StyDye.REPLACE('Alo1 WITH MAX( Alo1 - ORDLINE.Pik1, 0 ),;
             				            Alo2 WITH MAX( Alo2 - ORDLINE.Pik2, 0 ),;
			                            Alo3 WITH MAX( Alo3 - ORDLINE.Pik3, 0 )')
            This.StyDye.REPLACE('Alo4 WITH MAX( Alo4 - ORDLINE.Pik4, 0 )')

            This.StyDye.REPLACE('Alo5 WITH MAX( Alo5 - ORDLINE.Pik5, 0 ),;
	                             Alo6 WITH MAX( Alo6 - ORDLINE.Pik6, 0 ),;
	                             Alo7 WITH MAX( Alo7 - ORDLINE.Pik7, 0 ),;
         	                     Alo8 WITH MAX( Alo8 - ORDLINE.Pik8, 0 )')
            This.StyDye.REPLACE('TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8')
            =gfAdd_Info('STYDYE')
	          *This.lfAdd_Info(This.STYDYE,'STYDYE')
            UNLOCK
          ENDIF    && End of IF
        ENDIF    && End of IF

        REPLACE &lcTmpKtTk..nProcNo WITH 4 IN (lcTmpKtTk)
        =RLOCK(lcTmpKtTk)
        UNLOCK IN &lcTmpKtTk

      IF &lcTmpKtTk..nProcNo = 4


        *IF Statment to find the STYLE record for the current ORDLINE record

*        IF THIS.STYLE.SEEK(ORDLINE.Style)
        IF THIS.STYLE.SEEK(IIF(EMPTY(ORDLINE.AltStyle),ORDLINE.Style,ORDLINE.AltStyle))

          SELECT STYLE
          =RLOCK()

          THIS.STYLE.REPLACE('Alo1 WITH MAX(Alo1 - ORDLINE.Pik1, 0),;
                  Alo2 WITH MAX(Alo2 - ORDLINE.Pik2, 0),;
                  Alo3 WITH MAX(Alo3 - ORDLINE.Pik3, 0),;
                  Alo4 WITH MAX(Alo4 - ORDLINE.Pik4, 0)')

          THIS.STYLE.REPLACE('Alo5 WITH MAX(Alo5 - ORDLINE.Pik5, 0),;
                  Alo6 WITH MAX(Alo6 - ORDLINE.Pik6, 0),;
                  Alo7 WITH MAX(Alo7 - ORDLINE.Pik7, 0),;
                  Alo8 WITH MAX(Alo8 - ORDLINE.Pik8, 0)')

          THIS.STYLE.REPLACE('TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8')
          =gfAdd_Info('STYLE')
    		  *This.lfAdd_Info(This.STYLE,'STYLE')
          UNLOCK
        ENDIF    && End of IF

        REPLACE &lcTmpKtTk..nProcNo WITH 5 IN (lcTmpKtTk)
        =RLOCK(lcTmpKtTk)
        UNLOCK IN &lcTmpKtTk
      ENDIF    && End of IF


*!*	      IF ASCAN(This.loFormSet.laEvntTrig,PADR("ALRELORD",10)) <> 0
*!*	        = gfDoTriger("ALRELPI",PADR("ALRELORD",10))
*!*	      ENDIF

      *T20060818.0001(C200876) TMI [Start]
      IF ASCAN(This.loFormSet.laEvntTrig,PADR("ALRELORD",10)) <> 0
        This.loFormSet.mDoTrigger(PADR('ALRELORD',10))
      ENDIF
      *T20060818.0001(C200876) TMI [End  ]


      SELECT ORDLINE
       IF &lcTmpKtTk..nProcNo = 5

        =RLOCK()
        
        THIS.ORDLINE.REPLACE(' Pik1 WITH 0,;
                               Pik2 WITH 0,;
                               Pik3 WITH 0,;
                               Pik4 WITH 0')

        THIS.ORDLINE.REPLACE('Pik5 WITH 0,;
                              Pik6 WITH 0,;
                              Pik7 WITH 0,;
                              Pik8 WITH 0')

        THIS.ORDLINE.REPLACE("PikTkt WITH ' ',;
  	                 					TotPik WITH 0,;
                              Picked WITH .F.,;
                              PikDate WITH {}")

        THIS.ORDLINE.REPLACE("cAllocatBy      WITH  ' '")


		*B611624,1 Es 08/30/2018 Modify the issue of warehouse mismatching  between Ordhdr and ORDLINE"  [Start]
		IF (ORDHDR.cWARECODE <> ORDLINE.cWareCode)
          THIS.ORDLINE.REPLACE("REPLACE  ORDLINE.cWareCode WITH '"+ORDHDR.cWARECODE+"'")
	    ENDIF 
		*B611624,1 Es 08/30/2018 Modify the issue of warehouse mismatching  between Ordhdr and ORDLINE"  [End]
 

        IF !EMPTY(ORDLINE.AltStyle)
          THIS.ORDLINE.REPLACE("AltStyle     WITH SPACE(19) ")
        ENDIF

        =gfAdd_Info('ORDLINE')


         REPLACE Pik1 WITH 0,;
                 Pik2 WITH 0,;
                 Pik3 WITH 0,;
                 Pik4 WITH 0 ,;
                 Pik5 WITH 0,;
                 Pik6 WITH 0,;
                 Pik7 WITH 0,;
                 Pik8 WITH 0 ,;
                 PikTkt WITH ' ',;
                 TotPik WITH 0,;
                 Picked WITH .F.,;
                 PikDate WITH {}

*        This.lfAdd_Info(This.ORDLINE,'ORDLINE')

        UNLOCK

        REPLACE &lcTmpKtTk..nProcNo WITH 6 IN (lcTmpKtTk)
        =RLOCK(lcTmpKtTk)
        UNLOCK IN &lcTmpKtTk
      ENDIF    && End of IF

      *--ORDLINE record correct, PikTkt record correct.
      IF ASCAN(This.loFormSet.laEvntTrig , PADR('RELEASE_PK',10)) <> 0 AND PIKTKT.cPickType = 'A'
        =gfDoTriger('ALRELPI',PADR('RELEASE_PK',10))
      ELSE
        *IF Statment for the rall back
        IF BETWEEN(&lcTmpKtTk..nProcNo , 6 , 11)
          =This.OrdHdr.SEEK('O' + ORDLINE.Order)

          *IF There is an alternative style
*!*            IF !EMPTY(AltStyle)
*!*              This.lfSwchSty(Style,AltStyle)
*!*            ELSE     && Else
*!*              This.PIKTKT.Seek(&lcTmpKtTk..PIKTKT)
*!*              This.lfMoveOrd(Style,Style,PIKTKT.cWareCode , ORDHDR.cWareCode)
*!*            ENDIF    && End of IF
        ENDIF    && End of IF

      ENDIF


      SELECT ORDLINE
      SKIP 1

*      This.OrdLine.REPLACE('&lcTmpKtTk..nProcNo  WITH 1 ,;
                            &lcTmpKtTk..nOrdLnNo WITH RECNO()')
       REPLACE &lcTmpKtTk..nProcNo  WITH 1 ,;
               &lcTmpKtTk..nOrdLnNo WITH RECNO() IN (lcTmpKtTk)



      SKIP -1
      =RLOCK(lcTmpKtTk)
      UNLOCK IN &lcTmpKtTk

    ENDSCAN    && End of SCAN Loop

  ENDIF    && End of IF

  lcOrdLnTag = ORDER()

  This.OrdLine.SetOrder('ORDLINST')
  This.PIKTKT.Seek(&lcTmpKtTk..PIKTKT)
*  =This.OrdLine.SEEK('O' + PIKTKT.Order + PIKTKT.STORE)


  *LOCATE Statment to loock for another record for this Picking ticket
  *in the ORDLINE file [a record with style not in the style range]
  SELECT ORDLINE

  *: B608043,1 MMT 05/23/2007  when select by style release only the lines for this style (t20060829.0002)  begin
  =SEEK('O' +PIKTKT.Order + PIKTKT.STORE)
 *: B608043,1 MMT 05/23/2007  when select by style release only the lines for this style (t20060829.0002)  End
  LOCATE REST WHILE CORDTYPE+ORDER+STORE+STYLE+STR(LINENO,6)  = 'O' +PIKTKT.Order + PIKTKT.STORE;
          FOR PIKTKT = PIKTKT.PIKTKT

  lcDelPktkt = PIKTKT.PIKTKT

  *IF Statment to check if the last LOCATE Statment did not found a record
  IF !FOUND()

    SELECT PIKTKT

    lcPkTkTag  = EVALUATE(KEY())

    This.PIKTKT.Seek(lcDelPktkt)
    SELECT PIKTKT
*: B608043,1 AYM 04/15/2007  when select by style release only the lines for this style (t20060829.0002)  begin
    *SCAN REST WHILE  PIKTKT = lcDelPktkt
    SCAN REST WHILE  PIKTKT = lcDelPktkt && AND !llSeleStyle
*: B608043,1 AYM 04/15/2007  when select by style release only the lines for this style (t20060829.0002)  end
      =RLOCK()
      THIS.PIKTKT.REPLACE('Status WITH "X"')
      =gfAdd_Info('PIKTKT')
*      This.lfAdd_Info(This.PIKTKT,'PIKTKT')
      UNLOCK
	  *: C201334,1 MMT 05/11/2011 Custom Pick and Pack monitor screen[T20110401.0003][Start]
	  IF ASCAN(This.loFormSet.laEvntTrig,PADR('CANPCKPACK',10),1,ALEN(This.loFormSet.laEvntTrig,1),1) > 0
        =This.loFormSet.mDoTrigger(PADR('CANPCKPACK',10))
      ENDIF	
	  *: C201334,1 MMT 05/11/2011 Custom Pick and Pack monitor screen[T20110401.0003][End]	
*!*        *-- Audit Trial.
*!*        IF ASCAN(This.loFormSet.laEvntTrig,PADR("REL_PIK",10)) <> 0
*!*          =gfDoTriger('ALRELPI',PADR("REL_PIK",10))
*!*        ENDIF

    ENDSCAN
 *--mmttest
    =This.PIKTKT.SEEK(lcPkTkTag)
  ENDIF    && End of IF


  lcOrdLnTag = ORDER()
  This.OrdLine.SetOrder('ORDLINST')

  SELECT(lcTmpKtTk)

  lnTmpRec = RECNO()
  SCAN FOR PIKTKT = lcDelPktkt REST WHILE llSel
    DELETE
    =RLOCK(lcTmpKtTk)
    UNLOCK IN &lcTmpKtTk
    This.lnSelRec = This.lnSelRec - 1
    This.lnDelRec = This.lnDelRec + 1
  ENDSCAN

  IF BETWEEN(lnTmpRec,1,RECCOUNT())
    GO lnTmpRec
  ENDIF

ENDSCAN    && End of SCAN Loop


*!*  IF ASCAN(This.loFormSet.laEvntTrig , PADR('UPDRELEASE',10)) <> 0 AND llLinkToGl
*!*    llPrepere = .F.
*!*    =gfDoTriger('ALRELPI',PADR('UPDRELEASE',10))
*!*  ENDIF


SELECT(lcTmpKtTk)
LOCATE


*IF One or more of the selected picking tickets has a packing list

IF This.lnSelRec = 0
  This.loFormSet.llenableselectnone = .F.
  This.loFormSet.llenablerel 		= .F.
ENDIF    && End of IF lnSelRec = 0

*IF Statment to check if there is some of the picking tickets in the
*temp file was not released for they have not being selected
IF !EOF(lcTmpKtTk)
  This.loFormSet.llEnableInvert = .T.
  This.loFormSet.llenablerel 	= .F.
  This.lfvpbSel()
ELSE    && Else
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*This.lcCaptionSel 			= LANG_Select
This.lcCaptionSel 			= IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Select,loFormSet.GetHeaderText("LANG_Select",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  This.loFormSet.llEnableSelect = .F.
  This.loFormSet.llEnableInvert = .F.
  This.loFormSet.llenableselectnone = .F.
  This.loFormSet.llEnableSelectAll = .F.
  This.loFormSet.llenablerel	= .F.
ENDIF    && End of IF
This.lnSelRec = 0

DIMENSION laTableUpdate[6]
laTableUpdate[1] = This.Style
laTableUpdate[2] = This.Ordline
laTableUpdate[3] = This.piktkt
laTableUpdate[4] = This.pikline
laTableUpdate[5] = This.stydye
laTableUpdate[6] = This.ordhdr
=This.lfTableUpdate()

RETURN .T.
ENDFUNC
*!*************************************************************
*! Name      : lfTableUpdate
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 07/10/2005
*! Purpose   : function to Update Sql Tables.
*!*************************************************************
FUNCTION lfTableUpdate

*--Open Dictionary files.
LOCAL lnAlias,lnConnectionHandlar,lcTranCode,lnI,llUpdate
lnAlias = SELECT(0)

lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'')
IF TYPE('lcTranCode') = 'N'
  SELECT (lnAlias)
  RETURN .F.
ENDIF

FOR lnI = 1 TO ALEN(laTableUpdate,1)
  llUpdate = laTableUpdate[lnI].TableUpdate(lcTranCode)
  IF !llUpdate
    =oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
    SELECT (lnAlias)
    RETURN .F.
  ENDIF
ENDFOR

lnConnectionHandlar = oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode)
IF lnConnectionHandlar # 1
  =oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
  SELECT(lnAlias)
  RETURN .F.
ENDIF

SELECT(lnAlias)
*--end of lfTableUpdate.

*!*************************************************************
*! Name      : lfSekFirst
*! Developer : Haytham El_Sheltawi
*! Date      : 05/13/1997
*! Purpose   : Function find the first record in ORDLINE for the
*!             current record in PIKTKT that meet the selected
*!             critirea
*!*************************************************************
*! Called from : lfDelScr()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : .T. or .F.
*!*************************************************************
FUNCTION lfSekFirst

PRIVATE lcOrdLnTag

IF &lcTmpKtTk..nOrdLnNo <> 0

  *IF The ORDLINE record number is valid
  IF &lcTmpKtTk..nOrdLnNo <= RECCOUNT()
    GO &lcTmpKtTk..nOrdLnNo
    RETURN .T.
  ELSE    && Else
    RETURN .F.
  ENDIF    && End of IF
ELSE    && Else find the first record

  lcOrdLnTag = ORDER()
  This.Ordline.SetOrder('ORDLINST')

  This.Ordline.SEEK('O' + &lcTmpKtTk..Order + &lcTmpKtTk..STORE)

  SELECT OrdLine
  LOCATE REST WHILE CORDTYPE+ORDER+STORE+STYLE+STR(LINENO,6) = 'O'+&lcTmpKtTk..Order + &lcTmpKtTk..STORE ;
          FOR PIKTKT = &lcTmpKtTk..PIKTKT



  RETURN FOUND()
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfCrTmKtTk
*! Developer : Mariam Mazhar [MMT]
*! Date      : 08/02/2005
*! Purpose   : Function to create the Temp file [lcTmpKtTk]
*!*************************************************************
*! Called from : lfvScope()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfCrTmKtTk


  lnCurrAl = SELECT(0)

  IF USED(This.lcTmpKtTk) AND RECCOUNT(This.lcTmpKtTk)> 0
    SELECT(This.lcTmpKtTk)
    ZAP
  ENDIF

  *-- change Ordline Index to be on 'O'+Order
  PRIVATE lcSavOrder
  SELECT ORDLINE
  lcSavOrder = ORDER()
  This.Ordline.SetOrder('OrdLinSt')


  SELECT PIKTKT
  lcCurOrder = TAG()
  lcCurKey   = PIKTKT
  This.PIKTKT.SetOrder('PIKTKT')

  M.LLSEL = .F.
  M.NORDLNNO = 0
  M.NPROCNO = 1


  lcSetNear = SET('NEAR')
  SET NEAR ON

  *check if user has selected a piktkt or not
  llSelePik = .F. && flag to indicate if there is any piktkt selected
  lnPosPik = ASCAN(laScopExpr,"PIKTKT.PIKTKT")
  IF lnPosPik > 0
    lnPosPik = ASUBSCRIPT(laScopExpr,lnPosPik,1)
    lcFilePik =IIF(!EMPTY(laScopExpr[lnPosPik,6]),laScopExpr[lnPosPik,6],'')
    IF !EMPTY(lcFilePik) AND USED(lcFilePik) AND RECCOUNT(lcFilePik)> 0
      llSelePik = .T.
    ENDIF
  ENDIF

  *check if user has selected a style or not
  llSeleStyle = .F. && flag to indicate if there is any style selected
  lnPosStyle = ASCAN(laScopExpr,"ORDLINE.STYLE")
  IF lnPosStyle  > 0
    lnPosStyle  = ASUBSCRIPT(laScopExpr,lnPosStyle ,1)
    lcFileStyle  =IIF(!EMPTY(laScopExpr[lnPosStyle ,6]),laScopExpr[lnPosStyle ,6],'')
    IF !EMPTY(lcFileStyle ) AND USED(lcFileStyle ) AND RECCOUNT(lcFileStyle )> 0
      llSeleStyle = .T.
    ENDIF
  ENDIF

  llDateSel = .F. && flag to indicate if user Selected date range or not
  lcDate = ""
  lnPosPikDate = ASCAN(laScopExpr,"PIKTKT.DATE")
  IF lnPosPikDate > 0
    lnPosPikDate = ASUBSCRIPT(laScopExpr,lnPosPikDate,1)
    lcDatePik =IIF(!EMPTY(laScopExpr[lnPosPikDate,6]),laScopExpr[lnPosPikDate,6],'')
    IF !EMPTY(lcDatePik)
      llDateSel = .T.
      ldEnd =CTOD(SUBSTR(lcDatePik,ATC('|',lcDatePik)+1))
      ldStart = CTOD(SUBSTR(lcDatePik,1,ATC('|',lcDatePik)-1))
      lcDate =" BETWEEN(PIKTKT.DATE,CTOD('"+DTOC(ldStart)+"'),CTOD('"+DTOC(ldEnd)+"'))"
      lcSelectCond = lcDate
      IF !this.piktkt.llnative
        lcSqlDate = "(DATE BETWEEN '"+DTOC(ldStart)+"' AND '"+DTOC(ldEnd)+"')"
      ENDIF
    ENDIF
  ENDIF

  lcPrntStatus =  IIF(lnPrintStatV  = 1 , "" , IIF(lnPrintStatV   = 2 , " PIKTKT.PrtFlag = 'P' ", "PIKTKT.PrtFlag <> 'P'"))

  *--In Case of user select a picking ticket no.[Start]
  IF llSelePik
    SELECT(lcFilePik)
    SCAN
      IF This.PikTkt.Seek(&lcFilePik..PikTkt)
        SELECT PIKTKT
        SCAN REST WHILE  PIKTKT.PIKTKT = &lcFilePik..PikTkt ;
          FOR IIF(llDateSel,EVALUATE(lcDate),.T.) AND IIF(llSeleStyle,This.lfFindStyl(lcFileStyle),.T.) AND ;
              IIF(!EMPTY(lcPrntStatus),EVALUATE(lcPrntStatus),.T.) AND !(PIKTKT.STATUS$"CX") .AND. ;
              piktkt.PIKTKT <> "******" .AND. !This.PACK_HDR.SEEK(PIKTKT.PIKTKT)
          SCATTER MEMVAR MEMO
          INSERT INTO (This.lcTmpKtTk) FROM MEMVAR
        ENDSCAN
      ENDIF
    ENDSCAN
  ELSE
    IF llSeleStyle
      This.OrdLine.SetOrder('ORDLINES')
      SELECT(lcFileStyle)
      SCAN
        IF This.OrdLine.Seek(&lcFileStyle..Style)
          SELECT OrdLine
          SCAN REST WHILE STYLE+DTOS(COMPLETE)+CORDTYPE+ORDER+STORE+STR(LINENO,6) =&lcFileStyle..Style ;
            FOR !EMPTY(ordline.PIKTKT) AND ordline.PIKTKT <>  "******"
            IF This.piktkt.seek(ordline.PIKTKT)
              SELECT piktkt
              SCAN REST WHILE  PIKTKT.PIKTKT = ordline.PIKTKT ;
                FOR IIF(llDateSel,EVALUATE(lcDate),.T.) AND ;
                IIF(!EMPTY(lcPrntStatus),lcPrntStatus,.T.) AND !(PIKTKT.STATUS$"CX") .AND. ;
                piktkt.PIKTKT <> "******" .AND. !This.PACK_HDR.SEEK(PIKTKT.PIKTKT)
                IF !SEEK(ordline.PIKTKT,This.lcTmpKtTk)
                  SCATTER MEMVAR MEMO
                  INSERT INTO (This.lcTmpKtTk) FROM MEMVAR
                ENDIF
              ENDSCAN
            ENDIF
          ENDSCAN
        ENDIF
      ENDSCAN
    ELSE
      lcSeleExp = ""
      IF !this.piktkt.llnative
        lcSeleExp =lcSeleExp +IIF(!EMPTY(lcSeleExp)," AND ","")+" PIKTKT.STATUS NOT IN ('C','X') AND piktkt.PIKTKT != '******'"
        lcSeleExp = lcSeleExp +IIF(!EMPTY(lcSeleExp) and llDateSel," AND ","")+ IIF(llDateSel,lcSqlDate,"")
        lcSeleExp =lcSeleExp +IIF(!EMPTY(lcPrntStatus)," AND ","")+lcPrntStatus
      ELSE
        lcSeleExp =lcSeleExp +IIF(!EMPTY(lcSeleExp)," AND ","")+"!(PIKTKT.STATUS$'CX') AND piktkt.PIKTKT <> '******'"
        lcSeleExp = lcSeleExp +IIF(!EMPTY(lcSeleExp) and llDateSel," AND ","")+ IIF(llDateSel,lcDate,"")
        lcSeleExp =lcSeleExp +IIF(!EMPTY(lcPrntStatus)," AND ","")+lcPrntStatus
      ENDIF
      IF !EMPTY(lcSeleExp)
      *B607897,1 MMT 12/21/2006 Bug of no data in Case of England Company[Start]
*!*	        THIS.PIKTKT.SQLRUN("select * from piktkt" + IIF(!EMPTY(lcSeleExp)," WHERE ","")+lcSeleExp,This.lcTempPikTkt)
*!*	        lcTempPk = This.lcTempPikTkt
*!*	        SELECT (lcTempPk)
*!*	        SCAN  FOR !This.PACK_HDR.SEEK(&lcTempPk..PIKTKT)
*!*	          IF !SEEK(&lcTempPk..PIKTKT,This.lcTmpKtTk)
*!*	            SCATTER MEMVAR MEMO
*!*	            INSERT INTO (This.lcTmpKtTk) FROM MEMVAR
*!*	          ENDIF
*!*	        ENDSCAN
		IF !this.piktkt.llnative
          THIS.PIKTKT.SQLRUN("select * from piktkt" + IIF(!EMPTY(lcSeleExp)," WHERE ","")+lcSeleExp,This.lcTempPikTkt)
          lcTempPk = This.lcTempPikTkt
          SELECT (lcTempPk)
        ELSE
         THIS.PIKTKT.Seek('')
         SELECT PIKTKT
        ENDIF

        SCAN  FOR IIF(this.piktkt.llnative AND !EMPTY(lcSeleExp),EVALUATE(lcSeleExp),.T.) AND !This.PACK_HDR.SEEK(PIKTKT)
          IF !SEEK(PIKTKT,This.lcTmpKtTk)
            SCATTER MEMVAR MEMO
            INSERT INTO (This.lcTmpKtTk) FROM MEMVAR
          ENDIF
        ENDSCAN
	  *B607897,1 MMT 12/21/2006 Bug of no data in Case of England Company[End]
      ENDIF
    ENDIF
  ENDIF
  *--In Case of user select a picking ticket no.[END]

  SET NEAR &lcSetNear
  SELECT OrdLine
  This.Ordline.setorder(lcSavOrder)

  SELECT PIKTKT
  This.PikTkt.SEEK(lcCurKey)
  This.PikTkt.SetOrder(lcCurOrder)

  This.lfAddControlSource()
  SELECT (lnCurrAl)
ENDFUNC
*-- end of lfCrTmKtTk.
*!*************************************************************
*! Name      : lfFindStyl
*! Developer : Mariam Mazhar [MMT]
*! Date      : 07/28/2005
*! Purpose   : Function to check if the Pick ticket has one
*!             or more Style in the Style range
*!*************************************************************
*! Called from : lfCrTmKtTk
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : .T. or .F.
*!*************************************************************
*
FUNCTION lfFindStyl
  PARAMETERS lcStyleFile

  This.OrdLine.SEEK('O' + PIKTKT.Order + PIKTKT.STORE)
  SELECT ORDLINE
  LOCATE REST WHILE ORDER + STORE = PIKTKT.Order + PIKTKT.STORE  ;
        FOR PIKTKT = PIKTKT.PIKTKT AND SEEK(Ordline.style,lcStyleFile)

  SELECT PIKTKT
  RETURN FOUND('ORDLINE')
ENDFUNC
*!*************************************************************
*! Name      : mgetValueLogic
*! Developer : Mariam Mazhar [MMT]
*! Date      : 07/28/2005
*! Purpose   : Function to fill check box in grid
*!*************************************************************
*! Called from : lfCrTmKtTk
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : .T. or .F.
*!*************************************************************
*!
FUNCTION mgetValueLogic
  PRIVATE lnRetVal
  lnRetVal = EVAL(This.lcTmpKtTk+'.llSel')
  RETURN lnRetVal
ENDFUNC
*!*************************************************************
*! Name      : lfvpbSel
*! Developer : Mariam Mazhar [MMT]
*! Date      : 07/31/2005
*! Purpose   : Function to arange the push button select prompt
*!*************************************************************
*! Called from : lfvSelect() , lfvInvert() , The Browse [lcPickBrow]
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : .T.
*!*************************************************************
FUNCTION lfvpbSel

IF LLSEL
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*This.lcCaptionSel = LANG_unSelect
This.lcCaptionSel = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_unSelect,this.loFormSet.GetHeaderText("LANG_unSelect",this.loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

ELSE
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*This.lcCaptionSel = LANG_Select
This.lcCaptionSel = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Select,this.loFormSet.GetHeaderText("LANG_Select",this.loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

ENDIF

RETURN .T.
*!*************************************************************
*! Name      : lfvSelect
*! Developer : Mariam Mazhar [MMT]
*! Date      : 07/31/2005
*! Purpose   : Valid function of push button Select
*!*************************************************************
*! Called from : Scrren ALRELPIK
*!*************************************************************
*! Calls       : lfvpbSel()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvSelect
SELECT(This.lcTmpKtTk)
*  REPLACE LLSEL WITH !(This.loFormSet.ariaForm1.grdPiktkt.grdMultiSelectionGrid.column1.ariacheckbox1.Value)
  REPLACE LLSEL WITH !LLSEL
  This.lfvpbSel()

This.lnSelRec   = IIF(llSel , This.lnSelRec + 1 , This.lnSelRec - 1)
This.lnUnSelRec = IIF(llSel , This.lnUnSelRec - 1 , This.lnUnSelRec + 1)

*No records was selected
IF This.lnSelRec = 0
  This.loFormSet.llenableinvert = .T.
  This.loFormSet.llenableselect = .T.
  This.loFormSet.llenableselectall = .T.
  This.loFormSet.llenableselectnone = .F.
  This.loFormSet.llenablerel = .F.
ELSE    && Else
  This.loFormSet.llenableselectnone = .T.
  This.loFormSet.llenablerel = .T.

  *-- All the records were selected
  IF This.lnUnSelRec = 0
    This.loFormSet.llenableselectall = .F.
  ELSE
    This.loFormSet.llenableselectall = .T.
  ENDIF
ENDIF
*!*************************************************************
*! Name      : lfvSelAll
*! Developer : Mariam Mazhar [MMT]
*! Date      : 08/02/2005
*! Purpose   : Valid function of push button Select all
*!*************************************************************
*! Called from : Scrren ALRELPIK
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvSelAll

SELECT(This.lcTmpKtTk)
lnRecCurrn = RECNO()

REPLACE ALL LLSEL WITH .T.
This.lnSelRec   = RECCOUNT() - This.lnDelRec
This.lnUnSelRec = 0
GO lnRecCurrn

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*This.lcCaptionSel = LANG_unSelect
This.lcCaptionSel = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_unSelect,loFormSet.GetHeaderText("LANG_unSelect",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]


This.loFormSet.llenableselectall = .F.
This.loFormSet.llenableselectnone = .T.
This.loFormSet.llenablerel = .T.

*!*************************************************************
*! Name      : lfvSelNon
*! Developer : Mariam Mazhar [MMT]
*! Date      : 08/02/2005
*! Purpose   : Valid function of push button Select none
*!*************************************************************
*! Called from : Scrren ALRELPIK
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvSelNon

SELECT(This.lcTmpKtTk)

lnRecCurr = RECNO()

REPLACE ALL LLSEL WITH .F.

This.lnSelRec   = 0

This.lnUnSelRec = RECCOUNT() - This.lnDelRec

GO lnRecCurr

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*This.lcCaptionSel = LANG_Select
This.lcCaptionSel = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Select,loFormSet.GetHeaderText("LANG_Select",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

This.loFormSet.llEnableSelectAll  = .T.
This.loFormSet.llEnableSelectNone = .F.
This.loFormSet.llEnableRel = .F.

*!*************************************************************
*! Name      : lfvInvert
*! Developer : Haytham El_Sheltawi
*! Date      : 08/02/2005
*! Purpose   : Valid function of push button Invert
*!*************************************************************
*! Called from : Scrren ALRELPIK
*!*************************************************************
*! Calls       : lfvpbSel()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvInvert

SELECT(This.lcTmpKtTk)
lnRecNOCurr = RECNO()

REPLACE ALL LLSEL WITH !LLSEL

GO lnRecNOCurr

this.lfvpbSel()

This.lnUnSelRec = This.lnSelRec
This.lnSelRec   = RECCOUNT() - This.lnDelRec - This.lnSelRec

*there is no selected records
IF This.lnSelRec = 0
  This.loFormSet.llenableselectall = .T.
  This.loFormSet.llenableselectnone = .F.
  This.loFormSet.llenablerel = .F.
ELSE
  This.loFormSet.llenableselectnone = .T.
  This.loFormSet.llenablerel = .T.

  *--All the records were selected
  IF This.lnUnSelRec = 0
    This.loFormSet.llenableselectall = .F.
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfAdd_Info
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 08/02/2005
*! Purpose   : function to Update user info.
*!*************************************************************
FUNCTION lfAdd_Info,
LPARAMETERS loFileName,lcFileName
LOCAL llEdtFld,llAddFld,lcOldAlias
lcOldAlias = SELECT(0)

lcShortVersion = 'A40'
lcUser_ID = oAriaApplication.User_ID

SELECT(lcFileName)
llEdtFld = (TYPE('cEdit_USER') <> 'U') .AND. (TYPE('dEdit_Date') <> 'U') .AND. (TYPE('cEdit_Time') <> 'U');
           AND (TYPE('cEdt_Ver') <> 'U')
llAddFld = (TYPE('cAdd_user') <> 'U') AND (TYPE('dAdd_Date') <> 'U') AND (TYPE('cAdd_Time') <> 'U') ;
           AND (TYPE('cAdd_Ver') <> 'U') AND EMPTY(cAdd_user)

*-- New Record
IF llAddFld
  *** stamp the record for this user with date and time
  loFileName.REPLACE('cAdd_User  WITH lcUser_ID ,;
          dAdd_Date  WITH DATE()    ,;
          cAdd_Time  WITH gfGetTime(),;
          cAdd_Ver   WITH lcShortVersion')
ELSE && Modified Record
  IF llEdtFld
	loFileName.REPLACE('cEdit_User  WITH lcUser_ID ,;
             		dEdit_Date  WITH DATE()    ,;
            		cEdit_Time  WITH gfGetTime(),;
		            cEdt_Ver    WITH lcShortVersion')
  ENDIF        		
ENDIF
SELECT(lcOldAlias)
ENDFUNC
*!*************************************************************
*! Name      : lfSwchSty
*! Developer : Mariam Mazhar [MMT]
*! Date      : 08/02/2005
*! Purpose   : Function to restore the old style
*!*************************************************************
*! Called from : lfDelScr()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : Current Style , Old Style
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfSwchSty

PARAMETERS lcSStyle , lcOStyle

lcTmpKtTk = This.lcTmpKtTk

IF &lcTmpKtTk..nProcNo = 6

    This.PIKTKT.Seek(&lcTmpKtTk..PIKTKT)
    This.STYDYE.SEEK(lcSStyle + PIKTKT.cWareCode + SPACE(10))
    SELECT STYDYE
    =RLOCK()

    This.STYDYE.REPLACE('Ord1 WITH MAX(Ord1 - ORDLINE.Qty1, 0),;
            Ord2 WITH MAX(Ord2 - ORDLINE.Qty2, 0),;
            Ord3 WITH MAX(Ord3 - ORDLINE.Qty3, 0),;
            Ord4 WITH MAX(Ord4 - ORDLINE.Qty4, 0)')

    This.STYDYE.REPLACE('Ord5 WITH MAX(Ord5 - ORDLINE.Qty5, 0),;
            Ord6 WITH MAX(Ord6 - ORDLINE.Qty6, 0),;
            Ord7 WITH MAX(Ord7 - ORDLINE.Qty7, 0),;
            Ord8 WITH MAX(Ord8 - ORDLINE.Qty8, 0)')

    This.STYDYE.REPLACE('TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8')
     =gfAdd_Info('StyDye')
*    this.lfAdd_Info(This.STYDYE,'StyDye')

    UNLOCK

    REPLACE &lcTmpKtTk..nProcNo WITH 7 IN (lcTmpKtTk)
    =RLOCK(lcTmpKtTk)
    UNLOCK IN &lcTmpKtTk
ENDIF


IF &lcTmpKtTk..nProcNo = 7


  =This.STYLE.SEEK(lcSStyle)
  SELECT STYLE
  =RLOCK()

  This.Style.REPLACE('Ord1 WITH MAX(Ord1 - ORDLINE.Qty1, 0),;
                      Ord2 WITH MAX(Ord2 - ORDLINE.Qty2, 0),;
                      Ord3 WITH MAX(Ord3 - ORDLINE.Qty3, 0),;
                      Ord4 WITH MAX(Ord4 - ORDLINE.Qty4, 0)')
  This.Style.REPLACE('Ord5 WITH MAX(Ord5 - ORDLINE.Qty5, 0),;
                      Ord6 WITH MAX(Ord6 - ORDLINE.Qty6, 0),;
                      Ord7 WITH MAX(Ord7 - ORDLINE.Qty7, 0),;
                      Ord8 WITH MAX(Ord8 - ORDLINE.Qty8, 0)')
  This.Style.REPLACE('TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8')
  =gfAdd_Info('Style')
*  This.lfAdd_Info(This.Style,'Style')

  UNLOCK

  REPLACE &lcTmpKtTk..nProcNo WITH 8 IN (lcTmpKtTk)
  =RLOCK(lcTmpKtTk)
  UNLOCK IN &lcTmpKtTk
ENDIF

IF &lcTmpKtTk..nProcNo = 8

  =This.Style.SEEK(lcSStyle)
  SELECT STYLE

  =This.Scale.SEEK('S'+STYLE.Scale)
  lnCnT1 = SCALE.Cnt

  =This.Style.SEEK(lcOStyle)

  =This.Scale.SEEK('S'+STYLE.Scale)

  lnCnT2 = SCALE.Cnt
  lcOScale=SCALE.Scale

  *--IF Statment to check if the current Style has more Scales than the old Style
  IF lnCnT1 > lnCnT2
    SELECT ORDLINE
    lnCanc = 0          && Varible to hold the canceled quantity
    lnCancAmt = 0       && Varible to hold the canceled amount

    FOR lnElm = lnCnT2 + 1 TO lnCnT1
      lcElm = STR(lnElm , 1)
      lnCanc = lnCanc + Qty&lcElm
      lnCancAmt = lnCancAmt + (Qty&lcElm * Price)
    ENDFOR
    =RLOCK()
    this.OrdLine.REPLACE('Qty2 WITH IIF(lnCnT2 < 2 , 0 , Qty2) ,;
            Qty3 WITH IIF(lnCnT2 < 3 , 0 , Qty3) ,;
            Qty4 WITH IIF(lnCnT2 < 4 , 0 , Qty4) ,;
            Qty5 WITH IIF(lnCnT2 < 5 , 0 , Qty5) ')

    this.OrdLine.REPLACE('Qty6 WITH IIF(lnCnT2 < 6 , 0 , Qty6) ,;
            Qty7 WITH IIF(lnCnT2 < 7 , 0 , Qty7) ,;
            Qty8 WITH IIF(lnCnT2 < 8 , 0 , Qty8) ,;
          TotQty WITH Qty1 + Qty2 + Qty3 + Qty4 + Qty5 + Qty6 + Qty7 + Qty8')

    this.OrdLine.REPLACE('Book2 WITH IIF(lnCnT2 < 2 , 0 , Book1) ,;
           Book3 WITH IIF(lnCnT2 < 3 , 0 , Book3) ,;
           Book4 WITH IIF(lnCnT2 < 4 , 0 , Book4) ,;
           Book5 WITH IIF(lnCnT2 < 5 , 0 , Book5) ')

    this.OrdLine.REPLACE('Book6 WITH IIF(lnCnT2 < 6 , 0 , Book6) ,;
                          Book7 WITH IIF(lnCnT2 < 7 , 0 , Book7) ,;
                          Book8 WITH IIF(lnCnT2 < 8 , 0 , Book8) ')

    this.OrdLine.REPLACE(' TotBook WITH Book1 + Book2 + Book3 + Book4 + Book5 + Book6 + Book7 + Book8')
    =gfAdd_Info('Ordline')
*    This.lfAdd_Info(This.Ordline,'Ordline')

    UNLOCK

    SELECT ORDHDR
    =RLOCK()
    This.ORDHDR.REPLACE('Book    WITH Book - lnCanc,;
                         BookAmt WITH BookAmt - lnCancAmt,;
                         Open    WITH Open - lnCanc,;
                         OpenAmt WITH OpenAmt - lnCancAmt')
    =gfAdd_Info('ORDHDR')
*    this.lfAdd_Info(THIS.ORDHDR,'ORDHDR')

    UNLOCK
  ENDIF

  REPLACE &lcTmpKtTk..nProcNo WITH 9 IN (lcTmpKtTk)
  =RLOCK(lcTmpKtTk)
  UNLOCK IN &lcTmpKtTk
ENDIF


IF &lcTmpKtTk..nProcNo = 9


  =this.STYDYE.SEEK(lcOStyle)
  SELECT STYDYE
  =RLOCK()

  this.STYDYE.REPLACE('Ord1 WITH (Ord1 + ORDLINE.Qty1),;
          Ord2 WITH (Ord2 + ORDLINE.Qty2),;
          Ord3 WITH (Ord3 + ORDLINE.Qty3),;
          Ord4 WITH (Ord4 + ORDLINE.Qty4)')

  this.STYDYE.REPLACE('Ord5 WITH (Ord5 + ORDLINE.Qty5),;
          Ord6 WITH (Ord6 + ORDLINE.Qty6),;
          Ord7 WITH (Ord7 + ORDLINE.Qty7),;
          Ord8 WITH (Ord8 + ORDLINE.Qty8)')
  this.STYDYE.REPLACE('TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8')

  =gfAdd_Info('STYDYE')
*  this.lfAdd_Info(this.STYDYE,'STYDYE')
  UNLOCK

  REPLACE &lcTmpKtTk..nProcNo WITH 10 IN (lcTmpKtTk)
  =RLOCK(lcTmpKtTk)
  UNLOCK IN &lcTmpKtTk
ENDIF

IF &lcTmpKtTk..nProcNo = 10


  =This.Style.SEEK(lcOStyle)
  SELECT Style
  =RLOCK()
  This.Style.REPLACE('Ord1 WITH (Ord1 + ORDLINE.Qty1),;
                      Ord2 WITH (Ord2 + ORDLINE.Qty2),;
                      Ord3 WITH (Ord3 + ORDLINE.Qty3),;
                      Ord4 WITH (Ord4 + ORDLINE.Qty4)')
  This.Style.REPLACE('Ord5 WITH (Ord5 + ORDLINE.Qty5),;
                      Ord6 WITH (Ord6 + ORDLINE.Qty6),;
                      Ord7 WITH (Ord7 + ORDLINE.Qty7),;
                      Ord8 WITH (Ord8 + ORDLINE.Qty8)')
  This.Style.REPLACE('TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8')
  =gfAdd_Info('Style')
*  =this.lfAdd_Info(This.Style,'Style')

  UNLOCK

  REPLACE &lcTmpKtTk..nProcNo WITH 11 IN (lcTmpKtTk)
  =RLOCK(lcTmpKtTk)
  UNLOCK IN &lcTmpKtTk
ENDIF

IF &lcTmpKtTk..nProcNo = 11

  SELECT ORDLINE
  =RLOCK()
  this.ORDLINE.REPLACE('Style    WITH lcOStyle,;
          AltStyle WITH SPACE(19),;
          Scale    WITH lcOScale')
  =gfAdd_Info('OrdLine')
*  this.lfAdd_Info(this.ORDLINE,'OrdLine')
  UNLOCK

  REPLACE &lcTmpKtTk..nProcNo WITH 12 IN (lcTmpKtTk)
  =RLOCK(lcTmpKtTk)
  UNLOCK IN &lcTmpKtTk
ENDIF
*!*************************************************************
*! Name      : lfMoveOrd
*! Developer : Mariam Mazhar [MMT]
*! Date      : 08/02/2005
*! Purpose   : Function to remove the style from one warehouse
*!             to another
*!*************************************************************
*! Called from : lfDelScr()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : Current Style , Old Style ,
*!                     Current warehouse , Old warehouse
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfMoveOrd

PARAMETERS lcFrmSty,lcToSty,lcFrmWare,lcToWare

IF This.llDyelot .OR. !This.llMultiWH .OR. (lcFrmSty+lcFrmWare=lcToSty+lcToWare)
  RETURN
ENDIF

lnSe=SELECT()

IF &lcTmpKtTk..nProcNo = 6



  IF this.STYDYE.SEEK( lcFrmSty + lcFrmWare + SPACE(10))
    SELECT STYDYE
    =RLOCK()

    this.STYDYE.REPLACE('Ord1 WITH MAX(Ord1 - ORDLINE.Qty1, 0),;
            Ord2 WITH MAX(Ord2 - ORDLINE.Qty2, 0),;
            Ord3 WITH MAX(Ord3 - ORDLINE.Qty3, 0),;
            Ord4 WITH MAX(Ord4 - ORDLINE.Qty4, 0)')

    this.STYDYE.REPLACE('Ord5 WITH MAX(Ord5 - ORDLINE.Qty5, 0),;
            Ord6 WITH MAX(Ord6 - ORDLINE.Qty6, 0),;
            Ord7 WITH MAX(Ord7 - ORDLINE.Qty7, 0),;
            Ord8 WITH MAX(Ord8 - ORDLINE.Qty8, 0)')

    this.STYDYE.REPLACE('TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8')
    =gfAdd_Info('STYDYE')
*    This.lfAdd_Info(this.STYDYE,'STYDYE')
    UNLOCK
  ENDIF

  REPLACE &lcTmpKtTk..nProcNo WITH 7
  =RLOCK(lcTmpKtTk)
  UNLOCK IN &lcTmpKtTk
ENDIF

IF &lcTmpKtTk..nProcNo = 7

  IF this.STYDYE.SEEK(lcToSty + lcToWare + SPACE(10))
    =RLOCK()

    this.STYDYE.REPLACE('Ord1 WITH (Ord1 + ORDLINE.Qty1),;
            Ord2 WITH (Ord2 + ORDLINE.Qty2),;
            Ord3 WITH (Ord3 + ORDLINE.Qty3),;
            Ord4 WITH (Ord4 + ORDLINE.Qty4)')

    this.STYDYE.REPLACE('Ord5 WITH (Ord5 + ORDLINE.Qty5),;
            Ord6 WITH (Ord6 + ORDLINE.Qty6),;
            Ord7 WITH (Ord7 + ORDLINE.Qty7),;
            Ord8 WITH (Ord8 + ORDLINE.Qty8)')

    This.STYDYE.REPLACE('TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8')
    =gfAdd_Info('STYDYE')
*    This.lfAdd_Info(this.STYDYE,'STYDYE')

    UNLOCK
  ENDIF

  REPLACE &lcTmpKtTk..nProcNo WITH 12

  =RLOCK(lcTmpKtTk)
  UNLOCK IN &lcTmpKtTk
ENDIF

SELECT(lnSe)
*!*************************************************************
*! Name      : lfSekPak
*! Developer : Mariam Mazhar [MMT]
*! Date      : 08/02/2005
*! Purpose   : Function to check if the Pick ticket has a packing list
*!*************************************************************
*! Called from : lfDelScr()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : .T. or .F.
*!*************************************************************
*
FUNCTION lfSekPak
PARAMETERS lcPikTkt

*--IF The Pick ticket has a packing list

IF this.pack_hdr.SEEK(lcPikTkt,"PACK_HDR")
  RETURN .T.
ELSE
  RETURN .F.
ENDIF


ENDDEFINE

*!*************************************************************
*! Name      : lfOGWhen
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 07/27/2005
*! Purpose   : When function for the Option Grid
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfOGWhen

*--create objects of remote table for the required files
IF TYPE('loPiktkt') <> 'O'
  loPiktkt = CreateObject("RemoteTable","Piktkt","Piktkt","Piktkt",SET("DATASESSION"))
ENDIF

IF TYPE('loOrdline') <> 'O'
  loOrdline = CreateObject("RemoteTable","Ordline","Ordline","Ordline",SET("DATASESSION"))
ENDIF

IF TYPE('loStyle') <> 'O'
  loStyle = CreateObject("RemoteTable","Style","Style","Style",SET("DATASESSION"))
ENDIF

IF llCallOption
  =ACOPY(laScopExpr , loOGScroll.laOgFxFlt)
  =ACOPY(laScopExpr , laOgFxFlt)
ENDIF
*This part of code is used to fill option grid inlist in case of user
*opened and closed the option grid after selecting from the inlist options [Start]
IF llSaveCriteria
  lnRpPrtFlg = lnPrintStatV
  lnPosPik = ASCAN(laScopExpr,"PIKTKT.PIKTKT")
  IF lnPosPik > 0
    lnPosPik = ASUBSCRIPT(laScopExpr,lnPosPik,1)
    lcFilePik =IIF(!EMPTY(laScopExpr[lnPosPik,6]),laScopExpr[lnPosPik,6],'')
    IF !EMPTY(lcFilePik)

      lnDataSess = SET("Datasession")

      SET DATASESSION TO lndataSessPre

      IF USED(lcFilePik) AND RECCOUNT(lcFilePik)> 0
        SELECT(lcFilePik)
        lcTempPikSel = gfTempName()

        COPY TO oariaapplication.workdir+lcTempPikSel+".dbf"

        SET DATASESSION TO lnDataSess

        IF !USED(lcFilePik)
          *! E303079,1 MMT 06/28/2012 Fixing Media issues[T20120304.0004][Start]
          *USE oariaapplication.workdir+lcTempPikSel+".dbf"
          USE oariaapplication.workdir+lcTempPikSel+".dbf" IN 0
          *! E303079,1 MMT 06/28/2012 Fixing Media issues[T20120304.0004][END]
          SELECT(lcTempPikSel)
          =AFIELDS(laTempStr)
    	    =gfCrtTmp(lcFilePik,@laTempStr,"PIKTKT",lcFilePik,.T.)
	        SELECT(lcTempPikSel)
	        INSERT INTO (lcFilePik) SELECT * FROM  &lcTempPikSel
  	      SELECT(lcTempPikSel)
  	      USE
	        ERASE oAriaApplication.WorkDir+lcTempPikSel+".dbf"
        ENDIF
      ENDIF
    ENDIF
  ENDIF

  lnPosStyle = ASCAN(laScopExpr,"ORDLINE.STYLE")
  IF lnPosStyle  > 0
    lnPosStyle  = ASUBSCRIPT(laScopExpr,lnPosStyle ,1)
    lcFileStyle  =IIF(!EMPTY(laScopExpr[lnPosStyle ,6]),laScopExpr[lnPosStyle ,6],'')
    IF !EMPTY(lcFileStyle)
      lnDataSess = SET("Datasession")
      SET DATASESSION TO lndataSessPre

      IF  USED(lcFileStyle) AND RECCOUNT(lcFileStyle)> 0
        SELECT(lcFileStyle)

        lcTempStyleSel = gfTempName()

        COPY TO oariaapplication.workdir+lcTempStyleSel +".dbf"

        SET DATASESSION TO lnDataSess

        IF !USED(lcFileStyle)
          *! E303079,1 MMT 06/28/2012 Fixing Media issues[T20120304.0004][Start]
          *USE oariaapplication.workdir+lcTempStyleSel +".dbf"
          USE oariaapplication.workdir+lcTempStyleSel +".dbf" IN 0
          *! E303079,1 MMT 06/28/2012 Fixing Media issues[T20120304.0004][END]
          SELECT(lcTempStyleSel )
          =AFIELDS(laTempStr)
	        =gfCrtTmp(lcFileStyle,@laTempStr,"Style",lcFileStyle,.T.)
     	    SELECT(lcTempStyleSel)
	        INSERT INTO (lcFileStyle) SELECT * FROM  &lcTempStyleSel
      		SELECT(lcTempStyleSel)
	        USE
	        ERASE oAriaApplication.WorkDir+lcTempStyleSel +".dbf"
        ENDIF
      ENDIF
    ENDIF
  ENDIF
ENDIF
*--This part of code is used to fill option grid inlist in case of user
*--opened and closed the option grid after selecting from the inlist options [End]

*--Copy the fixed filter array to the public array
=ACOPY(loOGScroll.laOGFxFlt , laScopExpr)
=loOGScroll.RefreshScroll()

lcStyTtl = gfItemMask('HN')
lcTitle  = gfItemMask('HI')
lcXMjr   = gfItemMask('HM')
lcXNMjr  = gfItemMask('HN')
lcStylePct   = gfItemMask("PI")
lcClrTtl = lcXMjr +'/'+ lcXNMjr
lnMajLen = LEN(gfItemMask('PM'))

*--Style file browse fields
lcBrowseFields = [STYLE :19 :H= ALLTRIM(lcClrTtl),DESC :15 :H="Description",DESC1 :15 :H= "Long Description",]+;
                 [lcSesDesc=gfCodDes(Season,'SEASON'):12 :H="Season",lcDivDesc=gfCodDes(cdivision,'CDIVISION'):12:H="Division",]

lcBrowseFields = lcBrowseFields+;
                 [pricea :6:h="Price A" , PRICEB :6:h="Price B",PRICEC :6:h="Price C",]

lcBrowseFields = lcBrowseFields+;
                 [totWip:7:h="WIP",totstk:7:h="Stock",]+;
                 [totord:7:h="Orders",]

lcBrowseFields= lcBrowseFields+;
                 [Fabric:9:h="Fabric",OTS=(TOTWIP+TOTSTK-TOTORD):7:H="O.T.S.",]

lcBrowseFields = lcBrowseFields+;
                 [CSTYGRADE :H='Grade', lcRoyDesc=gfCodDes(ROYALTY,'ROYALTY') :H='Royalty' , PATTERN , STATUS,]

lcBrowseFields = lcBrowseFields + ;
                 [SCALE , PREPAK , CBUYPREPK :H='Buy Prepack', QTY_CTN , COMMISSION , LINK_CODE :H='Link Code',]+;
                 [lcMaked = IIF(MAKE,'Y','N') :H='Manuf.', NMCOST1 , NMCOST2 , NMCOST3 , NMCOST4,NMCOST5,]

lcBrowseFields = lcBrowseFields + ;
                 [NICOST1 , NICOST2 , NICOST3 , NICOST4 , NICOST5,]

lcBrowseFields = lcBrowseFields+;
                 [NPRCOST2,NPRCOST3,NPRCOST4,NPRCOST5,TOTCOST :H='Total Cost',]+;
                 [AVE_COST,NSTKVAL :H='Stock Value',SOLDOUT,START,FABRIC,LOCATION,LINVSTY :H='Inventory',]

lcBrowseFields =lcBrowseFields+;
                [MARKA :H='MarkUp A',MARKB:H='MarkUp B',MARKC :H='MarkUp C',]+;
                [CCONSINFO1 :h='Cons. Info. 1',CCONSINFO2 :h='Cons. Info. 2']
*!*************************************************************
*! Name      : lfVprntStat
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 07/28/2005
*! Purpose   : function to validate print status
*!*************************************************************
*!
FUNCTION lfVprntStat

*--Empty file that hold selected piktkt
*!*  IF !EMPTY(lcPrint)
*!*    lnPosPik = ASCAN(loOgScroll.laOgFXFlt,"PIKTKT.PIKTKT")
*!*    IF lnPosPik > 0
*!*      lnPosPik  = ASUBSCRIPT(loOGScroll.laOgFxFlt,lnPosPik,1)
*!*      lcFilePik = IIF(!EMPTY(loOGScroll.laOgFxFlt[lnPosPik,6]),loOGScroll.laOgFxFlt[lnPosPik,6],'')
*!*      IF !EMPTY(lcFilePik) AND USED(lcFilePik) AND RECCOUNT(lcFilePik)> 0
*!*        SELECT(lcFilePik)
*!*        DELETE ALL
*!*        PACK
*!*  *      ZAP
*!*      ENDIF
*!*    ENDIF
*!*  ENDIF

*--Customize the browse condition dependent on user print status choice
lcBrowCond = " FOR PIKTKT <> '******' AND !(STATUS $ 'CX') "
IF lnRpPrtFlg = 2
 lcBrowCond = lcBrowCond+ [ AND (PrtFlag = 'P')]
ENDIF

IF lnRpPrtFlg > 2
 lcBrowCond = lcBrowCond + [ AND (PrtFlag <> 'P')]
ENDIF
*!*************************************************************
*! Name      : lfCreatExp
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 07/28/2005
*! Purpose   : function to save the user selection criteria
*!*************************************************************
FUNCTION lfCreatExp

	*--Copy the fixed filter array to a public array in order to be
	*--available after closing the optiion grid
	=ACOPY(loOGScroll.laOGFxFlt , laScopExpr)

  *--Save the print status option in order to be
  *--available after closing the optiion grid
  lnPrintStatV  = lnRpPrtFlg
  llSaveCriteria = .T.
*!*    *--release the option grid objects
*!*    loPiktkt  = null
*!*    loOrdline = null
*!*    loStyle   = null

ENDFUNC

