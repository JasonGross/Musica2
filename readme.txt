:Title: readme.txt

:Summary: ReadMe-file for Musica2

:Author: Bo C. Herlin

:Licence: GPL

Copyright (C) 2004  Bo C. Herlin

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

:Contact: bo@gcab.net

------------------------------------------------------------

Welcome to Musica2.

Musica is an open source project that aims at the creation of a complete Mathematica package for the exploration of the interconnection between Mathematics and Music.

History:

The original musica was first created by Costis Merziotis and placed at Wolfram:
http://library.wolfram.com/infocenter/MathSource/4945/
I contacted Costis and we decided to place musica at SourceForge so that we could both contribute to it.
When he got too busy at his daytime job and decided to hand musica over to me I took the liberty to make some changes.
Musica2 is an attempt to rewrite musica using a kind of generic programming for elements and lists so that extending it would be easy.
While this might not be achieved I have left the original Musica untouched at SourceForge, both the released package and the code in the CVS.

Download:

The latest packaged version can be downloaded at :
http://sourceforge.net/projects/musica/

If You cant wait for the next package to be properly released You can always get the latest files from the CVS by these commands :
cvs -d:pserver:anonymous@cvs.sourceforge.net:/cvsroot/musica login
cvs -z3 -d:pserver:anonymous@cvs.sourceforge.net:/cvsroot/musica co Musica2

You can also point your browser to :
http://cvs.sourceforge.net/viewcvs.py/musica/Musica2/

Installation:

Copy the Musica2 folder from /Musica2.yyymmdd/Applications into your "Applications" folder.
If you don't know in what folder it is located, evaluate in Mathematica the variable $BaseDirectory or $UserBaseDirectory, which contains the compelte path of the Application Folder.
The first one is available to all users of the computer, the second is only for the logged user.

Then You just have to do <<Musica2` inside Mathematica as expected.

Feedback:

Please give me feedback! This is the only way this package might get useful for You too.
You can mail me at bo@gcab.net, and/or You can attend to one of the forums at :
http://sourceforge.net/forum/?group_id=90149

But most important, Have fun!

/Bo C. Herlin
