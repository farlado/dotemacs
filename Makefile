# Makefile to choose font size

update:
	git checkout config.org init.el
	git pull

lowres: update

hires: update
	cat init.el | sed 's/100/220/' > init.el.new
	cat config.org | sed 's/-width 2/-width 4/; s/-width 3/-width 6/' > config.org.new
	mv init.el.new init.el
	mv config.org.new config.org

gitprep:
	cat init.el | sed 's/220/100/' > init.el.new
	cat config.org | sed 's/-width 4/-width 2/; s/-width 6/-width 3/' > config.org.new
	mv init.el.new init.el
	mv config.org.new config.org
