Typical OS-X programs are self-contained within the *.app "folder".  When compiling an OS-X program with Lazarus/Free Pascal in order to be able to run the debugger and other programming tools the actual executable is NOT placed inside the *.app folder when not compiled for "release".  It is placed as a sibling of the *.app folder and a hardlink is placed within the *.app folder back out to the executable.

To make this a "stand alone" OS-X *.app simply copy the exectuable into the 

*.app/Contents/MacOS/

folder.  It will overwrite the link that is in this folder but that is fine.

Jim Kueneman