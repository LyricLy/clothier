CC ?= gcc
CP ?= cp
RM ?= rm

PREFIX ?= /usr/local
BINDIR ?= $(PREFIX)/bin

CFLAGS ?= -O2
REQ_FLAGS = -lreadline -lpcre2-8
FLAGS = $(REQ_FLAGS) $(CFLAGS)

clothier: clothier.c
	$(CC) $(FLAGS) clothier.c -o clothier

install: clothier
	mkdir -p $(DESTDIR)$(BINDIR)
	$(CP) clothier $(DESTDIR)$(BINDIR)

uninstall:
	$(RM) $(addprefix $(DESTDIR)$(BINDIR)/, clothier)

clean:
	$(RM) clothier

.PHONY: install uninstall clean
