#ISGCI SmallGraph replacement metrics

Ziel des ersten Tasks ist es, die abstrkate Klasse *teo.isgci.SmallGraph* 
durch ein JgraphT Objekt zu ersetzen. 

##Klassenbeschreibung

###Member Variablen
- List<String> names: *speichert Namen für Graphen, z.B. C4*
- String link: *URL? zu was?*
- SmallGraph complement: *- klar*
- boolean primary: *True, wenn der Graph aus der Eingabe XML stammt und nicht durch complementing erzeugt wurde*
- Vector< Vector<SmallGraph> > induced *Induzierte SmallGraphs* **common for all members of the SG**

###Konstruktor
Initialisiert names, link, complement, primary als **null**

###interessante Methoden
- void addInduced(Vector<SmallGraph>) *<-Wo kommen die induzierten Teilgraphen her? Klingt nach VF2...*
- SmallGraph halfComplement() *Gibt unvollständiges Komplement (ohne Inhalt) zurück.*
- void copyFromComplement()  *Kopiert induced von this.complement, complemented die Graphen dann.*
- SmallGraph makeComplement() *halfComplement + copyFromComplement*

*SmallGraph* ist wie folgt mit anderen Komponenten von ISGCI verknüpft:

##Vorkommen in extend clause
- teo.isgci.smallgraph.Configuration
- teo.isgci.smallgraph.Family
- teo.isgci.smallgraph.Graph

Interfaces sind hierbei möglichst zu erhalten, sonst treten weitere transitive
*extends* auf: Family => SimpleFamily|HTMFamily|UnionFamily...

##Vorkommen als local variable
- teo.isgci.appl.FindISG : 315
- teo.isgci.gc.ForbiddenClass : Mehrfach. Interssante Klasse, implementiert 
	die Forbids-Relation
- teo.isgci.smallgraph.Configuration : 57
- to.isgci.xml.SmallGraphWriter : 
- teo.isgci.xml.SmallGraphReader : 532 **mehrere Klassen in einer Datei!**


##Fehlende Libraries, von uns ergänzt:
- Mouse.runtime
- gnu.getopt
Haben wir gefunden, Programm compiliert, ggf. "richtige" Versionen nachliefern,
um Fehler zu vermeiden.

##Fehlend, nicht auffindbar:
- "vf" in exec Statement, FindISG.java, 607

Laut Kommentar "Use VFLib2 to decide whether small is an induced subgraph of large."
ist das ein externes Script, um die VFLib2 aufzurufen. Java Code compiliert,
das Programm lässt sich auch mit data/smallgraphs.xml aufrufen, bricht jedoch
bei besagtem exec Statement mit Stacktrace ab. -> Nachliefern, wichtig um Tests
zu schreiben.

