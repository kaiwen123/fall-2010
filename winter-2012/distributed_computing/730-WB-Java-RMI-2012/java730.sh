# source this @file java730.sh, as in 
# source java-env.sh
#

export CLASSPATH=.:..:WhiteBoard:~/JavaClasses # or whatever
export PATH=/home/java/bin:/usr/local/sbin:/usr/sbin:/usr/local/bin:/usr/bin:/sbin:/bin

alias pj='ps aux | grep java'		# find java* processes

wb730build() {
  # Current dir must be the parent of WhiteBoard/ Java files
  rm -f WhiteBoard/*.class
  javac WhiteBoard/*.java
  rmic -keep WhiteBoard.LinesFrameImpl \
     WhiteBoard.WbClientImpl \
     WhiteBoard.WbServerImpl
}

wb730rmi() {
    killall rmiregistry
    rmiregistry &
    sleep 5
}

wb730run() {
    killall java

    java WhiteBoard.WbServerImpl 1 localhost &
    sleep 2

    java WhiteBoard.WbClientImpl 22 b0 localhost //localhost/S1 FF0000 &
    java WhiteBoard.WbClientImpl  4 b0 localhost //localhost/S1 00FF00 &
    java WhiteBoard.WbClientImpl  7 b1 localhost //localhost/S1 0000FF &
    java WhiteBoard.WbClientImpl 65 b1 localhost //localhost/S1 00FFFF &
}

# -eof-

