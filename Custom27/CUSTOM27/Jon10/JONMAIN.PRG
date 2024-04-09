*!**************************************************************************
*! Name      : JONMAIN.PRG             C102090
*! Developer : MOHAMED SHOKRY (MHM)
*! Date      : 12/12/2000
*! Purpose   : JON Custom Cut Tickt cost sheet Program .
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : 
*!**************************************************************************
*! Modifications
*!**************************************************************************
PARAMETER lcEvntFun,lcFunPars

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLT(lcEvntFun)+'('+lcFunPars+')'

*--Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue

*!**************************************************************************
*! Name      : AddOptn
*! Developer : Mohamed Shokry (MHM)
*! Date      : 12/12/2000
*! Purpose   : Add a new Option to the the menu Bar
*!**************************************************************************
*! Example   : lfAddOptn()
*!**************************************************************************
*!
FUNCTION lfAddOptn

DEFINE PAD _Option OF _MSYSMENU PROMPT 'O\<ptions' KEY ALT+P , ' '
ON PAD _Option OF _msysmenu ACTIVATE POPUP _OPTIONPOP

DEFINE POPUP _OPTIONPOP MARGIN SHADOW
DEFINE BAR 1 OF _OPTIONPOP PROMPT 'Costing'  SKIP FOR EMPTY(laData[1])

ON SELECTION BAR 1 OF _OPTIONPOP DO Po_Cost IN JONMAIN

*!**************************************************************************
*! Name      : Po_Cost
*! Developer : Mohamed Shokry (MHM)
*! Date      : 12/13/2000
*! Purpose   : call secreen of PoCtCost 
*!**************************************************************************
*! Example   : =lfPoCost()
*!**************************************************************************
*!
PROCEDURE Po_Cost
STORE SPACE(0) TO lcPair, lcFClr, lcBClr, lcColor, lcColor1
lcTit   = "Average Unit Cost"
lcComma = ":"
llNoThing = lfSwtClr(.T.)

*-- Store the alias .
lcOldAlias = ALIAS()
llByDoz=.F.

DO CASE
  CASE lcTranType = 'I'
    SELECT POSHDR
    lcAlias = ALIAS()
  CASE lcTranType = 'M'
    SELECT CUTTKTH
    lcAlias = ALIAS()
  CASE lcTranType = 'T'
    SELECT MMFGORDH
    lcAlias = ALIAS()
ENDCASE   
lnEsti= 0 
lnActu= 0
lnLand= 0                
lcEAveCost=0
lcLAveCost=0
lcAAveCost=0
FOR lnCount = 1 TO 5
  Z=STR(lnCount,1)  
  lnEst&Z=0
  lnLan&Z=0
  lnAct&Z=0
ENDFOR
DO DISPCOST

SELECT (lcOldAlias)

DO (GCSCRDIR+"POCTCOST.SPX")



*!**************************************************************************
*! Name      : lfvSwitch
*! Developer : Mohammed Shokry (MHM)
*! Date      : 12/13/2000
*! Purpose   : switch between cuttict/po and dozen
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Example            : DO DispCost
*!**************************************************************************
FUNCTION lfvSwitch

lnEsti= 0 
lnActu= 0
lnLand= 0                
FOR lnCount = 1 TO 5 
  Z=STR(lnCount,1)  
  lnEst&Z=0
  lnLan&Z=0
  lnAct&Z=0
ENDFOR

IF !llByDoz
  IF UPPER(lcAlias) = 'POSHDR'
    SHOW GET pbSwitch,1 PROMPT 'C\<ost by PO'
  ELSE
    SHOW GET pbSwitch,1 PROMPT 'C\<ost by C/T'
  ENDIF
  llByDoz=.T.
  llNoThing = lfSwtClr(.F.)
ELSE
  SHOW GET pbSwitch,1 PROMPT 'C\<ost by Dozen'
  llByDoz=.F.
  llNoThing = lfSwtClr(.T.)
ENDIF
SHOW GET PbSwitch
SELECT (lcAlias)
IF llByDoz
  DO CASE
    CASE UPPER(lcAlias) = 'POSHDR'
      FOR lnCount = 1 TO 5
        Z=STR(lnCount,1)  
        lnEst&Z = IIF(POSHDR.nStyOrder>0,ROUND(EVAL('NICOST'+Z)/POSHDR.nStyOrder,3),0)
        lnEsti  = lnEsti + EVAL('lnEst'+Z)

        lnLan&Z = IIF(POSHDR.Receive>0,ROUND(EVAL('nlan_COST'+Z)/POSHDR.Receive,3),0)
        lnLand  = lnLand + EVAL('lnLan'+Z)       
 
        lnAct&Z = IIF(POSHDR.Receive+POSHDR.OPEN>0,ROUND(EVAL('nact_COST'+Z)/(POSHDR.Receive+POSHDR.OPEN),3),0)
        lnActu  = lnactu + EVAL('lnAct'+Z)
      ENDFOR
  
    CASE UPPER(lcAlias) = 'CUTTKTH'
      FOR lnCount = 1 TO 5
        Z=STR(lnCount,1)  
        Z=STR(lnCount,1)  
        lnEst&Z = IIF(CUTTKTH.Pcs_Bud>0,ROUND(EVAL('NEST_COST'+Z)/CUTTKTH.Pcs_Bud*12,3),0)
        lnEsti  = lnEsti + EVAL('lnEst'+Z)

        lnLan&Z = IIF(CUTTKTH.Pcs_Rec>0,ROUND(EVAL('NLAN_COST'+Z)/CUTTKTH.Pcs_Rec*12,3),0)
        lnLand  = lnLand + EVAL('lnLan'+Z)       
 
        lnAct&Z = IIF(CUTTKTH.Pcs_Rec+CUTTKTH.Pcs_Opn>0,ROUND(EVAL('NACT_COST'+Z)/(CUTTKTH.Pcs_Rec+CUTTKTH.Pcs_Opn)*12,3),0)
        lnActu  = lnactu + EVAL('lnAct'+Z)
      ENDFOR

    CASE UPPER(lcAlias) = 'MMFGORDH'
      FOR lnCount = 1 TO 4
        Z=STR(lnCount,1)  
        lnEst&Z = IIF(MMFGORDH.NMMGBudget>0,ROUND(EVAL('NEST_COST'+Z)/MMFGORDH.NMMGBudget,3),0)
        lnEsti  = lnEsti + EVAL('lnEst'+Z)

        lnLan&Z = IIF(MMFGORDH.Received>0,ROUND(EVAL('NLAN_COST'+Z)/MMFGORDH.Received,3),0)
        lnLand  = lnLand + EVAL('lnLan'+Z)       
 
        lnAct&Z = IIF(MMFGORDH.Received+MMFGORDH.NMMFGOPEN>0,ROUND(EVAL('NACT_COST'+Z)/(MMFGORDH.Received+MMFGORDH.NMMFGOPEN),3),0)
        lnActu  = lnactu + EVAL('lnAct'+Z)
      ENDFOR
  ENDCASE
ELSE
  DO DISPCOST  
ENDIF  
SHOW GETS WINDOW 'POCTCOST' OFF
SELECT (lcOldAlias)

RETURN


*!**************************************************************************
*! Name      : DispCost
*! Developer : Mohammed Shokry (MHM)
*! Date      : 12/13/2000
*! Purpose   : Display cost either unit or PO/C/T cost.
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Example            : DO DispCost
*!**************************************************************************

PROCEDURE DispCost
SELECT (lcAlias)

DO CASE
  CASE UPPER(lcAlias) = 'POSHDR'
    FOR lnCount = 1 TO 5
      Z=STR(lnCount,1)  
      lnEst&Z = EVAL('NICOST'+Z)
      lnEsti  = lnEsti + EVAL('lnEst'+Z)
    
      lnLan&Z = EVAL('NLAN_COST'+Z)            
      lnLand  = lnLand + EVAL('lnLan'+Z)           

      lnAct&Z = EVAL('NACT_COST'+Z)
      lnActu  = lnactu + EVAL('lnAct'+Z)
    ENDFOR

  CASE UPPER(lcAlias) = 'CUTTKTH'
    FOR lnCount = 1 TO 5
      Z=STR(lnCount,1)  
      lnEst&Z = EVAL('NEST_COST'+Z)
      lnEsti  = lnEsti + EVAL('lnEst'+Z)
    
      lnLan&Z = EVAL('NLAN_COST'+Z)            
      lnLand  = lnLand + EVAL('lnLan'+Z)           

      lnAct&Z = EVAL('NACT_COST'+Z)
      lnActu  = lnactu + EVAL('lnAct'+Z)
    ENDFOR
  CASE UPPER(lcAlias) = 'MMFGORDH'
    FOR lnCount = 1 TO 4
      Z=STR(lnCount,1)  
      lnEst&Z = EVAL('NEST_COST'+Z)
      lnEsti  = lnEsti + EVAL('lnEst'+Z)
    
      lnLan&Z = EVAL('NLAN_COST'+Z)            
      lnLand  = lnLand + EVAL('lnLan'+Z)           

      lnAct&Z = EVAL('NACT_COST'+Z)
      lnActu  = lnactu + EVAL('lnAct'+Z)
    ENDFOR
ENDCASE

DO CASE
  CASE UPPER(lcAlias) = 'POSHDR'
    lcEAveCost = IIF(POSHDR.nStyOrder > 0,ROUND(lnEsti/POSHDR.nStyOrder,3),'N/A')
    lcLAveCost = IIF(POSHDR.Receive > 0,ROUND(lnLand/POSHDR.Receive,3),'N/A')
    lcAAveCost = IIF(POSHDR.Receive+POSHDR.Open >0,;
                     ROUND(lnActu/(POSHDR.Receive+POSHDR.Open),3),'N/A')
  CASE UPPER(lcAlias) = 'CUTTKTH'
    lcEAveCost = IIF(CUTTKTH.PCS_BUD > 0,ROUND(lnEsti/CUTTKTH.PCS_BUD,3),'N/A')
    lcLAveCost = IIF(CUTTKTH.PCS_REC > 0,ROUND(lnLand/CUTTKTH.PCS_REC,3),'N/A')
    lcAAveCost = IIF(CUTTKTH.PCS_REC+CUTTKTH.PCS_OPN>0,;
                     ROUND(lnActu/(CUTTKTH.PCS_REC+CUTTKTH.PCS_OPN),3),'N/A')
  CASE UPPER(lcAlias) = 'MMFGORDH'
    lcEAveCost = IIF(MMFGORDH.NMMGBUDGET >0,ROUND(lnEsti/MMFGORDH.NMMGBUDGET,3),'N/A')
    lcLAveCost = IIF(MMFGORDH.Received >0,ROUND(lnLand/MMFGORDH.Received,3),'N/A')
    lcAAveCost = IIF(MMFGORDH.STATUS $ 'CS' .AND. MMFGORDH.Received >0,;
                     ROUND(lnActu/MMFGORDH.Received,3),'N/A')
ENDCASE



*!**************************************************************************
*! Name      : lfSwtClr
*! Developer : Mohammed Shokry (MHM)
*! Date      : 12/13/2000
*! Purpose   : To make the unit average line visible or not.
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Passed Parameters  : llVis (.T.,.F.)
*!**************************************************************************
*! Example            : =lfSwtClr(.T.)
*!**************************************************************************

FUNCTION lfSwtClr
PARAMETERS llVis

IF llVis
  lcColor  = SCHEME(13,1) + ",,,,,,,,," 
  lcColor1 = SCHEME(5,1)  + ",,,,,,,,," 
ELSE
  lcPair   = SCHEME(5,1)
  lcBClr   = SUBSTR(lcPair,ATC("/",lcPair)+1)
  lcFClr   = STRTRAN(lcBClr,"*","+")
  lcTorep  = lcFClr+"/"+lcBClr+","
  lcColor  = lcTorep+lcTorep+lcTorep+lcTorep+lcTorep+lcTorep+lcTorep+lcTorep+lcTorep+lcFClr+"/"+lcBClr
  lcColor1 = lcColor
ENDIF  

