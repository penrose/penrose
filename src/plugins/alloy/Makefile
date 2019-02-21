JFLAGS = -g -cp ".:alloy4.2.jar"
JC = javac
.SUFFIXES: .java .class

.java.class:
	$(JC) $(JFLAGS) $*.java

CLASSES = AlloyPlugin.java
# CLASSES = Evaluator.java

default: classes

all: clean default

classes: $(CLASSES:.java=.class)

clean:
	$(RM) *.class *.hi *.o Main __temp__.als .DS_Store trace.log
