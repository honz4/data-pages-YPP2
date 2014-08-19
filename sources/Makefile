
lisp = $(addsuffix .lisp, 09-examples 09-circle_arrow clmg-examples flags trees undo)
lisp+= $(addsuffix .lisp, semaphore semaphore-light serializable countries cv01 cv03)
lisp+= $(addsuffix .lisp, expr-shapes expressions mixins protos-0 protos-0-graphics)
lisp+= $(addsuffix .lisp, 04 05 06 07 08 09 09-1)

lisp : $(lisp)

%.lisp : ; wget -O $@ http://krupka.inf.upol.cz/2008L/sources/$@

micro-graphics.zip : FORCE ; wget -O $@ http://krupka.inf.upol.cz/2008L/sources/$@


FORCE : ;
