JFLAGS = -g -cp "*:."
JC = javac
.SUFFIXES: .java .class

.java.class:
	$(JC) $(JFLAGS) $*.java

CLASSES = AlloyPlugin.java
# CLASSES = Evaluator.java

default: classes

all: clean default

classes: $(CLASSES:.java=.class)

jarfile: 
	wget http://alloy.lcs.mit.edu/alloy/downloads/alloy4.2.jar

clean:
	$(RM) *.class *.hi *.o Main __temp__.als .DS_Store trace.log
