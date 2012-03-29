#
# Makefile for the CEG 730 C+RPC  based WhiteBoard project
# Linux version
# end-of-line comments are helpful in non-Linux OS

CURRENT_DIR=730-WB-C-RPC-2012
SOBJ = server.o transient.o server_s.o server_clnt.o client_clnt.o server_xdr.o
COBJ = client.o transient.o server_clnt.o client_s.o server_xdr.o xwindow.o
WOBJ = wbadmin.o server_clnt.o server_xdr.o
CFLAGS = -ggdb -pedantic -ansi -std=c99 -ggdb3 -D__DEBUG__ # -Wall -DBSD_SIGNALS 
CC = gcc

all: server730 client730 deregall730 xwindow730 wbadmin730 
	@ ls -l server730 client730 xwindow730 deregall730

wbadmin730: server.h $(WOBJ)
	gcc -o $@ $(WOBJ)

xwindow730: xwindow.c
	gcc -o xwindow730 -DMAIN xwindow.c -lX11

deregall730: deregall.o
	gcc -o deregall730 deregall.o

server730: server.h $(SOBJ) 
	gcc -o server730 $(SOBJ) # -lrpcsvc

client730: server.h client.h $(COBJ)
	gcc -o client730 $(COBJ) -lX11 # /usr/lib/libX11.so # -lrpcsvc -lX11

server.h server_xdr.c server_clnt.c server_svc.c: server.x wb.h
	rpcgen server.x

client.h client_xdr.c client_svc.c client_clnt.c: client.x wb.h
	rpcgen client.x

client_s.c: client_svc.c
	cp -f client_svc.c client_s.c
	ed client_s.c < ed-script-client 2> /dev/null > /dev/null

server_s.c: server_svc.c
	cp -f server_svc.c server_s.c
	ed server_s.c < ed-script-server 2> /dev/null > /dev/null

print:
	enscript -B -fCourier8 -2r Makefile 0README.txt \
		server.x server.c client.x  client.c
indent:
	indent -kr -di1 client.c client.x server.c server.x xwindow.c

tar archive: clean
	(cd ..; tar cvvfj $(CURRENT_DIR)-`date +%G%m%d%H%M`.tbz $(CURRENT_DIR))

clean:
	rm -f *.o *.BAK *~ *.out *_svc.c *_clnt.c *_xdr.c \
		wbadmin730 server730 client730 xwindow730 deregall730 \
		client_s.c server_s.c server.h client.h

# -eof-

