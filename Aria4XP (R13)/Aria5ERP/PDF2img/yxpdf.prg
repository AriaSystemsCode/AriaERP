
*1*
*save this code as yxPDF.prg (main file)
set safe off
publi m.yrep
m.yrep=addbs(justpath(sys(16,1)))
set defa to (yrep)
*test the xpdf downloaded folder if set
if !directory ("xpdfbin-win-3.04")
messagebox([Folder "xpdfbin-win-3.04" must be in source mandatory ....cancelling!],16+4096,"error")
endi


text to m.myvar noshow
DEFINE POPUP raccourci SHORTCUT RELATIVE FROM MROW(),MCOL()
DEFINE BAR 1 OF raccourci PROMPT "pdfImages"
DEFINE BAR 2 OF raccourci PROMPT "pdftoText"
DEFINE BAR 3 OF raccourci PROMPT "pdfToPNG"
DEFINE BAR 4 OF raccourci PROMPT "pdftoHTML"
DEFINE BAR 5 OF raccourci PROMPT "pdftoPPM"
DEFINE BAR 6 OF raccourci PROMPT "pdftoPS"
ON SELECTION BAR 1 OF raccourci ;
DO _4kd1cou4d ;
IN LOCFILE("YXPDF\YMENU1" ,"MPX;MPR|FXP;PRG" ,"WHERE is YMENU1?")
ON SELECTION BAR 2 OF raccourci ;
DO _4kd1cou4g ;
IN LOCFILE("YXPDF\YMENU1" ,"MPX;MPR|FXP;PRG" ,"WHERE is YMENU1?")
ON SELECTION BAR 3 OF raccourci ;
DO _4kd1cou4l ;
IN LOCFILE("YXPDF\YMENU1" ,"MPX;MPR|FXP;PRG" ,"WHERE is YMENU1?")
ON SELECTION BAR 4 OF raccourci ;
DO _4kd1cou4s ;
IN LOCFILE("YXPDF\YMENU1" ,"MPX;MPR|FXP;PRG" ,"WHERE is YMENU1?")
ON SELECTION BAR 5 OF raccourci ;
DO _4kd1cou4v ;
IN LOCFILE("YXPDF\YMENU1" ,"MPX;MPR|FXP;PRG" ,"WHERE is YMENU1?")
ON SELECTION BAR 6 OF raccourci ;
DO _4kd1cou4x ;
IN LOCFILE("YXPDF\YMENU1" ,"MPX;MPR|FXP;PRG" ,"WHERE is YMENU1?")

ACTIVATE POPUP raccourci
*
PROCEDURE _4kd1cou4d
local m.x,m.ytemp
SET STEP ON 
m.ytemp=addbs(sys(2023))+"ytemp.txt"
strtofile(filetostr(m.yrep+"xpdfbin-win-3.04\doc\pdfimages.txt"),m.ytemp)
run/n notepad  &ytemp

*
PROCEDURE _4kd1cou4g
local m.x,m.ytemp
m.ytemp=addbs(sys(2023))+"ytemp.txt"
strtofile(filetostr(m.yrep+"xpdfbin-win-3.04\doc\pdftoText.txt"),m.ytemp)
run/n notepad  &ytemp

*
PROCEDURE _4kd1cou4l
local m.x,m.ytemp
m.ytemp=addbs(sys(2023))+"ytemp.txt"
strtofile(filetostr(m.yrep+"xpdfbin-win-3.04\doc\pdftoPNG.txt"),m.ytemp)
run/n notepad  &ytemp

*
PROCEDURE _4kd1cou4s
local m.x,m.ytemp
m.ytemp=addbs(sys(2023))+"ytemp.txt"
strtofile(filetostr(m.yrep+"xpdfbin-win-3.04\doc\pdftohtml.txt"),m.ytemp)
run/n notepad  &ytemp

*
PROCEDURE _4kd1cou4v
local m.x,m.ytemp
m.ytemp=addbs(sys(2023))+"ytemp.txt"
strtofile(filetostr(m.yrep+"xpdfbin-win-3.04\doc\pdftoPPM.txt"),m.ytemp)
run/n notepad  &ytemp
*
PROCEDURE _4kd1cou4x
local m.x,m.ytemp
m.ytemp=addbs(sys(2023))+"ytemp.txt"
strtofile(filetostr(m.yrep+"xpdfbin-win-3.04\doc\pdftoPS.txt"),m.ytemp)
run/n notepad  &ytemp

endtext
strtofile(m.myvar,m.yrep+"ymenu1.mpr")

publi yform
yform=newObject("yxpdf")
yform.show
read events
retu
*
DEFINE CLASS yxPDF AS form
BorderStyle = 0
Top = 4
Left = 48
Height = 85
Width = 266
ShowWindow = 2
Caption = "XPDF"
MaxButton = .F.
AlwaysOnTop = .T.
BackColor = RGB(212,208,200)
Name = "Form1"

ADD OBJECT command1 AS commandbutton WITH ;
Top = 14, ;
Left = 7, ;
Height = 61, ;
Width = 216, ;
FontBold = .T., ;
FontSize = 14, ;
Caption = "XPDF menu", ;
MousePointer = 15, ;
SpecialEffect = 2, ;
ForeColor = RGB(255,128,0), ;
BackColor = RGB(128,255,0), ;
Name = "Command1"

ADD OBJECT label1 AS label WITH ;
AutoSize = .T., ;
FontSize = 28, ;
BackStyle = 0, ;
Caption = "?", ;
Height = 45, ;
Left = 228, ;
Top = 20, ;
Width = 23, ;
ForeColor = RGB(255,128,64), ;
BackColor = RGB(128,255,0), ;
Name = "Label1"

add object yop as optiongroup with ;
AutoSize = .T.,;
ButtonCount = 1,;
BackStyle = 0,;
Value = 1,;
Height = 27,;
Left = 229,;
MousePointer = 15,;
Top = -5,;
Width = 28,;
Name = "yop",;
Option1.BackStyle = 0,;
Option1.Caption = "",;
Option1.Value = 1,;
Option1.Height = 17,;
Option1.Left = 5,;
Option1.MousePointer = 15,;
Option1.Top = 5,;
Option1.Width = 18,;
Option1.AutoSize = .T.,;
Option1.Name = "Option1"    

PROCEDURE  yop.Click
	DECLARE INTEGER MessageBox IN user32 As MessageBoxA;
		INTEGER hwnd,;
		STRING  lpText,;
		STRING  lpCaption,;
		INTEGER wType
	#define MB_ICONINFORMATION 0x00000040
	#define MB_OK 0x00000000
	#define MB_APPLMODAL 0x00000000
	#define  MB_DEFBUTTON1 0x00000000

	local m.myvar
	text to m.myvar noshow
Googling, i discover this day a usefull tool named xPDF.
Xpdf is the original open source PDF viewer, first released in 1995.
The Xpdf software and documentation are copyright 1996-2014 Glyph & Cog, LLC.

Xpdf is an open source viewer for Portable Document Format (PDF) files. (These are also sometimes  also called 'Acrobat' files, from the name of Adobe's PDF software.) The Xpdf project also
includes a PDF text extractor, PDF-to-PostScript converter, and various other utilities.

Xpdf runs under the X Window System on UNIX, VMS, and OS/2. The non-X components (pdftops,pdftotext, etc.) also run on Win32 systems and should run on pretty much any system.
Xpdf is licensed under the GNU General Public License (GPL), version 2.this include restrictions and conditions to redistribute xpdf (read licence).
Download the zip package (10.2 Moctets) and unzip in the source folder (same as yxpdf.prg).can cut the subfolder bin64.keep the folders
bin32 and doc.(+licences..) as in image below.the last version released in may 2014.
xPDF home:  http://www.foolabs.com/xpdf/home.html
xPDF download :http://www.foolabs.com/xpdf/download.html   or ftp://ftp.foolabs.com/pub/xpdf/
this code runs some interesting librarires
  -pdftoImages : extracts  images from a PDF file
  -pdftoText: convert a pdf as txt file
  -pdftoHtml: convert each pdf page as html with a global index
  -pdftoPNG reads the PDF file and writes one PNG file for each page
  -pdftoPPM converts a PDF file to a series of PPM/PGM/PBM-format bitmaps
  -pdftoPS reads the PDF file and writes a PostScript file, PS file.this can be sent to printer.
	
can apply same method to another converters in the zip
can complete the command lines as provided in the help txt files in code in function or user conditions.this is a demo only.
warning:if the pdfs are locked with password must enter the valid password in the command line to make the code working.

this library makes a part of adobe acrobat professional works.
Run the code: there is a main menu to work with library and see help in second menu as original xPDF txt files.

Note: xPDf have many Language Support Packages can be downloaded from the links above.

Important :this code is tested under windows10 & VFP9SP2.
	endtext

	MessageBoxA(thisform.hwnd,m.myvar,"Summary Help",MB_APPLMODAL+MB_OK +MB_ICONINFORMATION +MB_DEFBUTTON1 )

ENDPROC
PROCEDURE Load
 &&shellexecute
DECLARE INTEGER ShellExecute IN SHELL32.DLL INTEGER nWinHandle,;
STRING cOperation,;
STRING cFileName,;
STRING cParameters,;
STRING cDirectory,;
INTEGER nShowWindow
DECLARE INTEGER Sleep IN KERNEL32 INTEGER
ENDPROC

PROCEDURE Destroy
clea events
ENDPROC

PROCEDURE command1.Click
do ymenu.mpr
ENDPROC

PROCEDURE command1.RightClick
do ymenu.mpr
ENDPROC

PROCEDURE label1.Click
do ymenu1.mpr
ENDPROC

ENDDEFINE
*
*-- EndDefine: yxPDF

