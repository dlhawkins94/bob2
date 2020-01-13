CFLAGS = -O3 -I src
EGGS = srfi-1 srfi-13 srfi-69 format http-client matchable medea openssl regex shell vector-lib
OBJS = obj/packaging.o obj/sb-info.o obj/slackbuilds.o obj/util.o

bin/bob: src/bob.scm $(OBJS)
	csc $(CFLAGS) src/bob.scm $(OBJS) -o bin/bob

obj/packaging.o: src/packaging.scm src/macros.scm
	csc $(CFLAGS) -c src/packaging.scm -o obj/packaging.o

obj/slackbuilds.o: src/slackbuilds.scm src/macros.scm
	csc $(CFLAGS) -c src/slackbuilds.scm -o obj/slackbuilds.o

obj/sb-info.o: src/sb-info.scm src/macros.scm
	csc $(CFLAGS) -c src/sb-info.scm -o obj/sb-info.o

obj/util.o: src/util.scm
	csc $(CFLAGS) -c src/util.scm -o obj/util.o

clean:
	rm bin/* obj/*

install: bin/bob
	cp -f bin/bob /usr/local/sbin/bob
	cp -f tools/bob-graph.sh /usr/local/bin/bob-graph
	chmod +x /usr/local/bin/bob-graph
	mkdir -p /var/cache/bob
	mkdir -p /var/lib/bob

install-deps:
	chicken-install $(EGGS)

uninstall:
	rm -f /usr/local/sbin/bob
	rm -f /usr/local/bin/bob-graph
	rmdir -f /var/cache/bob
	rmdir -f /var/lib/bob

