
SUBDIR = lib \
		 de405 \
		 lambda \
		 orbit \
		 lsq \
		 arsig \
		 tedit \
		 redig \
		 get_ctrl \
		 mergeerp \
		 mergesp3

all : $(SUBDIR)

$(SUBDIR) :
	make -C $@

install :
	@mkdir -p ../bin/
	cp arsig/arsig  lsq/lsq tedit/tedit redig/redig orbit/sp3orb \
		get_ctrl/get_ctrl mergesp3/mergesp3 mergeerp/mergeerp ../bin/

.PHONY : all $(SUBDIR) install

clean :
	-rm -rf $(addsuffix /.obj, $(SUBDIR))

