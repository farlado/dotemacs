# Makefile to choose font size

lores:
	git checkout init.el
	git pull

hires: lores
	cat init.el | sed 's/100/220/' > init.el.new
	mv init.el.new init.el
