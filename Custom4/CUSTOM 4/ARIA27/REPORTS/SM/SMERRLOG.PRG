*:****************************************************************************
*: Program file        : SMERRLOG
*: Program desc.       : Print the error log file
*: For Report          : SMERRLOG.FRX
*: System              : Aria Advantage Series VER. 2.7
*: Module              : System Manager (SM)
*: Developer           : RAMY MABROUK (RAM)
*: Tracking Job Number : E500
*: Date                : July/2000
*:****************************************************************************
*: Calls :
*:    Procedures : ....
*:    Functions  : gfDispRe()
*:****************************************************************************
*: Passed Parameters  : None
*:****************************************************************************
*: Example : DO SMERRLOG
*:****************************************************************************
*: Modification        :
*:****************************************************************************

DO gfDispRe WITH EVAL('lcRpForm') , 'FOR ' + lcRpExp
