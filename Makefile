default: lcm

lcm: lcm.asd src/*.lisp
	sbcl --noinform \
		 --eval "(require 'asdf)" \
         --eval "(asdf:load-system :lcm)" \
	     --eval "(sb-ext:save-lisp-and-die #p\"lcm\" :toplevel #'lcm:entrypoint :executable t :purify t)"

install: lcm
	install -m 755 lcm /usr/local/bin/lcm

uninstall:
	sudo rm /usr/local/bin/lcm

clean:
	rm lcm
