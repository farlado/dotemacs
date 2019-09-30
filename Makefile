# Makefile to choose font size

update:
	git checkout init.el
	git pull

lores: update

hires: update
	cat init.el | sed 's/100/220/' > init.el.new
	mv init.el.new init.el

gitprep:
	cat init.el | sed 's/220/100/' > init.el.new
	mv init.el.new init.el
