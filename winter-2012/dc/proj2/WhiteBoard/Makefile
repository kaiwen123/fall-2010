.SUFFIXES: .java .class

.java.class:
	javac $<

rmic: LinesFrameImpl.class WbClientImpl.class WbServerImpl.class \
	Invoke.class WbAdmin.class 
	(cd ..; \
	rmic WhiteBoard.WbServerImpl \
		WhiteBoard.LinesFrameImpl WhiteBoard.WbClientImpl  )

WbServerImpl.class: WbServer.class
LinesFrameImpl.class: LinesFrame.class
WbClientImpl.class: WbClient.class

clean:
	rm -f *.class *~  *_Skel* *_Stub* WhiteBoard/*.class

run:
	killall rmiregistry java
	rmiregistry &
	sleep 5			# perhaps not necessary, but to be sure ...
	java WhiteBoard.WbServerImpl 1 localhost &
	sleep 1			# to make sure the server is ready
	java WhiteBoard.WbClientImpl 22 b0 localhost //localhost/S1 FF0000 &
	java WhiteBoard.WbClientImpl 4  b0 localhost //localhost/S1 00FF00 &
	java WhiteBoard.WbClientImpl 7 b2 localhost //localhost/S1 0000FF &
	java WhiteBoard.WbClientImpl 65 b2 localhost //localhost/S1 00FFFF &

tar:	clean
	(cd ..;	tar cvvfj 730-WB-Java-2011-`date +%G%m%d%H%M`.tbz WhiteBoard;\
	ls -ltr *.tbz)
