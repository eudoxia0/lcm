lcm: lcm.asd src/*.lisp
	sbcl --noinform \
         --eval "(asdf:load-system :lcm)" \
	     --eval "(sb-ext:save-lisp-and-die #p\"lcm\" :toplevel #'lcm:entrypoint :executable t :purify t)"

clean:
	rm lcm
