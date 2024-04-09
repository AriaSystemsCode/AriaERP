*T20140730.0010
* This program is to take a backup of the following three files into a backup folder T20140730.0010 under each Co. folder
* This action was taken based on the comment added 09/10/2014 07:20 PM - 08:20 PM to the ticket
* The files are :
* 1. gltypes.*
* 2. glsegval.*
* 3. glacchar.*
******************************
set step on
use syccomp
scan
lcDir = allt(ccom_ddir)
cd &lcDir
md T20140730.0010
lcNewDir = lcDir+'T20140730.0010\'
copy file gltypes.* to (lcNewDir+'gltypes.*')
copy file glsegval.* to (lcNewDir+'glsegval.*')
copy file glacchar.* to (lcNewDir+'glacchar.*')
endscan