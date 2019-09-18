# Makefile to choose font size

update:
	git checkout init.el
	git pull

smallfont: update
	cat init.el | sed 's/220/100/; s/160/100/' > init.el.new
	mv init.el.new init.el

mediumfont: update
	cat init.el | sed 's/220/160/; s/100/160/' > init.el.new
	mv init.el.new init.el

bigfont: update
	cat init.el | sed 's/100/220/; s/160/220/' > init.el.new
	mv init.el.new init.el

