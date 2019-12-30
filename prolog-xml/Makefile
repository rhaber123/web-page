################## XTL transformer & parser/serializer
xtl:
	javac -classpath lib/2p.jar:lib/xercesImpl.jar src/serializer/*.java src/transform/*.java  -d bin  -Xlint:deprecation

jar-xtl: bin/transform/TransformLibrary.class
	rm -Rf jar
	mkdir jar
	cp -R bin/* operators.pl jar/
	cp lib/*.jar jar/
	cd jar; jar xf 2p.jar
	cd jar; jar xf xercesImpl.jar
	cd jar; rm *.jar
	cd jar; jar cmf ../MANIFEST-xtl.MF ../xtl.jar *
	rm -Rf jar

test-xtl: xtl.jar
	rm -f _t.out
	java -jar xtl.jar examples/sample1/in.xml examples/sample1/schema.pl _t.out
	diff -q _t.out examples/sample1/expected2.out
	java -jar xtl.jar examples/sample2/bytes.xml examples/sample2/schema.pl _t.out
	diff -q _t.out examples/sample2/expected.xml
	rm -f _t.out


################## Xerces' XSLT transformer
xslt:
	javac src/xslt/SimpleTransform.java -d bin

jar-xslt: bin/xslt/SimpleTransform.class
	rm -Rf jar
	mkdir jar
	mkdir -p jar/xslt
	cp -R bin/xslt/SimpleTransform.class jar/xslt/
	cd jar; jar cmf ../MANIFEST-xslt.MF ../xslt.jar *
	rm -Rf jar

test-xslt: xslt.jar
	rm -f _t.out
	java -jar xslt.jar examples/sample1/in.xml examples/sample1/schema.xsl _t.out
	diff -q _t.out examples/sample1/expected.out
	rm -f _t.out


###############################

all:
	echo "Available targets: xtl -> jar-xtl -> test-xtl OR  xslt -> jar-xslt -> test-xslt OR run-xtl OR run-2p OR clean"

run-2p: lib/2p.jar
	java -jar lib/2p.jar

run-2p-xtl: xtl.jar
	echo "(1) ADD transform.TransformLibrary to the LibraryManager, and then load"
	echo "  alternatively, call: load_library('transform.TransformLibrary'). or unload_library('..')."
	echo "(2) OPEN operators.pl"
	echo "(3) append your template/tranformation and goal:"
	echo "    e.g. parse2('input1.xml', T), traverse(T,T2s), parse2('output1.xml',element(top,[],T2s))."
	java -cp xtl.jar alice.tuprologx.ide.GUILauncher

clean:
	rm -fR bin jar xslt.jar xtl.jar

