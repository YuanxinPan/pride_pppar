
SUBDIR = lib \
		 de405 \
		 lambda \
		 orbit \
		 lsq \
		 arsig \
		 tedit \
		 redig \
		 get_ctrl

all : $(SUBDIR)

$(SUBDIR) :
	make -j4 -C $@

install :
	@mkdir -p ../bin/
	cp arsig/arsig  lsq/lsq tedit/tedit redig/redig orbit/sp3orb \
		orbit/mergesp3 orbit/mergeerp get_ctrl/get_ctrl ../bin/

.PHONY : all $(SUBDIR) install

clean :
	-rm -rf $(addsuffix /.obj, $(SUBDIR))

