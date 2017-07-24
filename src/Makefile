JFLAGS = -g -cp ".:alloy4.2.jar"
JC = javac
.SUFFIXES: .java .class

.java.class:
	$(JC) $(JFLAGS) $*.java

CLASSES = Evaluator.java 

default: classes

classes: $(CLASSES:.java=.class)

clean:
	$(RM) *.class

