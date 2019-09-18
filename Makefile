# Makefile to choose font size

smallfont:
	cat init.el | sed 's/220/100/' > init.el.new
	mv init.el.new init.el

bigfont:
	cat init.el | sed 's/100/220/' > init.el.new
	mv init.el.new init.el

