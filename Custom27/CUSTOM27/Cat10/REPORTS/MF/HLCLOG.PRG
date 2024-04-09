*:****************************************************************
*: Program file  : HLCLOG.PRG
*: Program desc. : Invoice Log.
*: System        : Aria Apparel System (A27).
*: Module        : H.L.C. System. (HL)
*: Developer     : ABDOU ELGENDI - (ABD) Due to C#200246,1
*: Date          : 01/24/2002
*:****************************************************************
*: Calls 
*:               : FUNCTIONS  : lfwRepWhen , lfCollect  , lfCreatemp
*:               :            : lfItmPos   , lfvOpnInvc.
*:               -----------------------------------------------
*:               : PROCEDURE  : None.
*:****************************************************************
*: Passed Parameters  : None.
*:****************************************************************
*:C#200246,1.
*:****************************************************************
*:Modifications  :
*:****************************************************************
*:
*-- Begin Declaration variables.
*-- lcImgDir  :- Variable Hold the Path for image Directory.
*-- lcTemp    :- Variable Hold the temp file that collect the data.
*-- lcComPhon :- variable hold the company phone.
*-- lcComFax  :- Variable to hold the Company Fax
*-- laCompAdd :- Array Hod the Company Address1 ,2
*-- llRpOpnInv:- Hold True if we print Open Invoice.
*-- lcRpQulty :- Hold The First Quality or Second .
*-- End Declaration variables.


DIMENSION laCompAdd[2]
STORE '' TO laCompAdd
SELECT SYCCOMP
SEEK gcAct_Comp

laCompAdd[1] = gfGetAdr('SYCCOMP' , '' , '' , '' , 1)
laCompAdd[2] = gfGetAdr('SYCCOMP' , '' , '' , '' , 2)

lcComPhon = cCom_Phon
lcPhone   = SUBSTR(lcComPhon,1,3)+'-'+SUBSTR(lcComPhon,4,3)+'-'+SUBSTR(lcComPhon,7,20)

lcComFax  = cCom_Fax
lcFax     = SUBSTR(lcComFax,1,3)+'-'+SUBSTR(lcComFax,4,3)+'-'+SUBSTR(lcComFax,7,20)

*-- if user change filter criteria then you must collect data again [Begin]
IF llOGFltCh
  *-- Function To Create temp File
  = lfCreatemp()
  *-- Collecting Data In The First Time.
  = lfCollect ()
ENDIF


llOneVendr = IIF(ATC('HLCINVHD.CTSUVENCD',lcRpExp)> 0,.T.,.F.) 

SELECT (lcTemp)
DO gfDispRe WITH EVAL('lcRpForm')

RETURN
*-- End of program code.
*:*************************************************************
*: Name      : lfwRepWhen
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/24/2002
*: Purpose   : When function of OG
*:*************************************************************
*: Called from : OG read cycle
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfwRepWhen()
*:*************************************************************
*
FUNCTION lfwRepWhen

IF llRpOpnInv
  
  *-- Store the invoice Date position
  lnInvDtPos = lfItmPos('HLCINVHD.DCINVDATE')
  laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGFxFlt,1) + lnInvDtPos] = !llRpOpnInv
  = lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnInvDtPos)) + ',6]')  && Show get Object .

  lnQltPos = ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,'lcRpQulty'),1)
  laOGObjCnt[lnQltPos] = !llRpOpnInv
  = lfOGShowGet('lcRpQulty')
  
ENDIF

*-- End of lfwRepWhen.
*:*************************************************************
*: Name      : lfCollect
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/24/2002
*: Purpose   : Collecting Data In The First Time.
*:*************************************************************
*: Called from : OG read cycle
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfCollect ()
*:*************************************************************
*
FUNCTION lfCollect

SELECT Hlcheckl
LOCATE
*-- In case we will print Open invoice only.
IF llRpOpnInv

    SELECT Hlcinvhd.dcinvdate,Hlcinvhd.ctsuvencd,;
           Hlcinvhd.cinvtype,Hlcinvhd.ccinvno,;
           Hlcinvhd.ncintunit,Hlcinvhd.ncintval,;
           Hlcheckh.dcheckdat,Hlcheckl.namountp,;
           Hlcheckl.ccheckno,;
           (Hlcinvhd.ncintval-Hlcinvhd.namountp) AS 'nBalance',;
           HlcheckH.cBolStat;
      FROM &gcDataDir.Hlcheckh,&gcDataDir.Hlcheckl,;
           &gcDataDir.Hlcinvhd;
     WHERE Hlcheckl.ccheckno = Hlcheckh.ccheckno;
       AND Hlcheckl.ctsuvencd+Hlcheckl.cinvtype+Hlcheckl.ccinvno=;
           Hlcinvhd.ctsuvencd+Hlcinvhd.cinvtype+Hlcinvhd.ccinvno;
       AND Hlcinvhd.ncintval >  Hlcinvhd.namountp;
     UNION SELECT Hlcinvhd.dcinvdate,Hlcinvhd.ctsuvencd,;
           Hlcinvhd.cinvtype,Hlcinvhd.ccinvno,;
           Hlcinvhd.ncintunit,Hlcinvhd.ncintval,;
           {} AS 'dcheckdat',000000000.00 AS 'namountp',;
           SPACE(20) AS 'ccheckno',;
           (Hlcinvhd.ncintval-Hlcinvhd.namountp) AS 'nBalance',;
           'O' AS 'cBolStat';
      FROM &gcDataDir.Hlcinvhd;
     WHERE Hlcinvhd.namountp = 0;
     ORDER BY 1;
      INTO DBF &gcWorkDir.&lcTemp
ELSE
  *-- In case we will not print Open invoice only.
     SELECT Hlcinvhd.dcinvdate,Hlcinvhd.ctsuvencd,;
            Hlcinvhd.cinvtype,Hlcinvhd.ccinvno,;
            Hlcinvhd.ncintunit,Hlcinvhd.ncintval,;
            Hlcheckh.dcheckdat,Hlcheckl.namountp,;
            Hlcheckl.ccheckno,;
            (Hlcinvhd.ncintval-Hlcinvhd.namountp) AS 'nBalance',;
            HlcheckH.cBolStat;
       FROM &gcDataDir.Hlcheckh,&gcDataDir.Hlcheckl,;
            &gcDataDir.Hlcinvhd;
      WHERE Hlcheckl.ccheckno = Hlcheckh.ccheckno;
        AND (Hlcheckl.ctsuvencd+Hlcheckl.cinvtype+Hlcheckl.ccinvno=;
             Hlcinvhd.ctsuvencd+Hlcinvhd.cinvtype+Hlcinvhd.ccinvno;
        AND Hlcinvhd.cinvtype = lcRpQulty);
        AND &lcRpExp ; 
      UNION SELECT Hlcinvhd.dcinvdate,Hlcinvhd.ctsuvencd,;
            Hlcinvhd.cinvtype,Hlcinvhd.ccinvno,;
            Hlcinvhd.ncintunit,Hlcinvhd.ncintval,;
            {} AS 'dcheckdat',000000000.00 AS 'namountp',;
            SPACE(20) AS 'ccheckno',;
            (Hlcinvhd.ncintval-Hlcinvhd.namountp) AS 'nBalance',;
            'O' AS 'cBolStat';
       FROM &gcDataDir.Hlcinvhd;
      WHERE &lcRpExp ;
        AND Hlcinvhd.namountp = 0;
        AND Hlcinvhd.cinvtype = lcRpQulty;
      ORDER BY 1;
     INTO DBF &gcWorkDir.&lcTemp
ENDIF


*-- End OF lfCollect
*:**************************************************************************
*: Name      : lfCreatemp
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/24/2002
*: Purpose   : Create the temp files.
*:**************************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:**************************************************************************
*: Called from : Report code
*:**************************************************************************
*: Passed Parameters  : ...
*:**************************************************************************
*: Returns            : None.
*:**************************************************************************
*: Example   : = lfCreatemp()
*:**************************************************************************
*
FUNCTION lfCreatemp

IF USED(lcTemp)
 SELECT (lcTemp)
  ZAP
ELSE
  CREATE Table (gcWorkDir+lcTemp) (dcinvdate D(10) , ctsuvencd C(6) , cInvType C(1)   ,;
                                   ccinvno C(12)   , ncintunit N(7) , ncintval N(10,2),;
                                   dcheckdat D(10) , namountp N(12,2),ccheckno C(20) ,;
                                   nBalance N(12,2), cBolStat C(1))
  = gfOpenFile(gcWorkDir+lcTemp,'','EX')
  INDEX ON dcinvdate TAG (lcTemp) OF (gcWorkDir+lcTemp+'.CDX')
  
ENDIF


*-- End OF lfCreatemp
*:**************************************************************************
*: Name      : lfItmPos
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/24/2002
*: Purpose   : Evaluate fixed filter position within array.
*:**************************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:**************************************************************************
*: Called from : Report code
*:**************************************************************************
*: Passed Parameters  : ...
*:**************************************************************************
*: Returns            : Position
*:**************************************************************************
*: Example   : = lfItmPos()
*:**************************************************************************
*
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos

*-- End of lfItmPos.
*:**************************************************************************
*: Name      : lfvOpnInvc
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/24/2002
*: Purpose   : Enable and disable some fields.
*:**************************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:**************************************************************************
*: Called from : Report code
*:**************************************************************************
*: Passed Parameters  : ...
*:**************************************************************************
*: Returns            : Position
*:**************************************************************************
*: Example   : = lfvOpnInvc()
*:**************************************************************************
*
FUNCTION lfvOpnInvc

*-- Store the invoice Date position
IF llRpOpnInv
  lcRpQulty = 'R'
  laOGFxFlt[lnInvDtPos,6]= DTOC({})+'|'+DTOC({})
ENDIF


laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGFxFlt,1) + lnInvDtPos] = !llRpOpnInv
= lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnInvDtPos)) + ',6]')  && Show get Object .


laOGObjCnt[lnQltPos] = !llRpOpnInv
= lfOGShowGet('lcRpQulty')

*-- End OF lfvOpnInvc
*:**************************************************************************