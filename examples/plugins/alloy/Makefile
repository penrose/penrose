JFLAGS = -nowarn -g -cp "*:."
JC = javac
.SUFFIXES: .java .class

.java.class:
	$(JC) $(JFLAGS) $*.java

CLASSES = AlloyPlugin.java

default: classes

all: clean default

classes: $(CLASSES:.java=.class)

clean:
	$(RM) *.class *.hi *.o Main temp.als .DS_Store trace.log
