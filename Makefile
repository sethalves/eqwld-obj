#
#
#


LIBS='(seth pbm)' '(foldling command-line)' '(seth obj-model)' '(srfi 95)' '(seth graph)' '(seth scad-model)' '(seth octree)' '(seth port-extras)' '(seth ieee-754)' '(snow filesys)'

%.obj.gz: %.obj
	cat $< | gzip -9 - > $@


all: eqwld-to-obj

eqwld-to-obj: eqwld-to-obj-chicken.scm eqwld-to-obj-main.sld
	csc -X r7rs $^ -o $@

libs:
	snow2 -p 'http://foldling.org/snow2/index.scm' install $(LIBS)

link-libs: very-clean
	snow2 -s \
		-p 'http://foldling.org/snow2/index.scm' \
		-p '../snow2-packages/seth' \
		-p '../snow2-packages/snow' \
		-p '../seth-snow2-misc' \
		install $(LIBS)


clean:
	rm -f eqwld-to-obj eqwld-to-obj-chicken.c *~

very-clean: clean
	rm -rf seth snow srfi foldling
