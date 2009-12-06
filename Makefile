default: package

clean:
	rm -rf classes inkly.jar

classes:
	mkdir classes

clojure.jar:
	@echo "Error: need a copy of clojure.jar in the top-level directory."
	@false

compile: classes clojure.jar
	CLASSPATH=clojure.jar:classes:. java clojure.main -e "(compile 'inkly)"

inkly.jar: compile
	cd classes && jar xf ../clojure.jar
	rm -rf classes/META-INF
	jar cfe $@ inkly -C classes .
	jar i $@

run: clojure.jar
	CLASSPATH=clojure.jar:. java clojure.main inkly.clj

package: inkly.jar
