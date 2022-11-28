all:
	for i in $$(ls -l | grep "^d.*$$" | awk '{print $$NF}'); do make -C $$i; done

