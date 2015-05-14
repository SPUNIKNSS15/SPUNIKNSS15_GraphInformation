HOWTO deprecate methods in the smallgraphs package
==================================================

Each team member is assigned a set of methods from the classes
*teo.isgci.smallgraph.Graph* and *teo.isgci.smallgraph.Configuration*.
The team members add comments to these methods, according to the
following conventions:


1. **Case 1:** The implementation of the method **will be replaced using
   a jgrapht-method.**
   ```Java
   /**
    * REIMPLEMENT WITH:
    *   - org.jgrapht.xxxx
    *   - org.jgrapht.yyyy
    *   - some more
    */
   public void addEdge(int a,int b){
       ...
   }
   ```
   If the **method signature will be changed** in addition to the body
   (e.g. because we don't pass adjacency matrices as arguments anymore)
   we also add the `@Deprecated` annotation and a comment which explains
   the signature change:
   ```Java
   /**
    * REIMPLEMENT WITH:
    *   - org.jgrapht.xxxx
    *   - org.jgrapht.yyyy
    *   - some more
    *
    * @deprecated
    * why did the signature change?
    */
   @Deprecated
   public void addEdge(int a,int b){
       ...
   }
   ```

2. **Case 2:** The method will become unused in the future **because its
   past caller is now using a jgrapht method instead.**
   We use the `@Deprecated `class annotation in addition to the Javadoc
   keyword.
   ```Java
   /**
    * @deprecated
    */
   @Deprecated
   private boolean isIsomorphicIntern(Graph g,
                        boolean darf[][], boolean mask[][]){
       ...
   }
   ```

3. **Case 3:** The method will become unused in the future **because of
   API changes.**
   Almost the same as *2.*:
   ```Java
   /**
    * @deprecated
    * here comes a short description why and how the API was changed
    */
   @Deprecated
   public int getComponents(){
        return Komponenten;
   }
   ```

*Please note that the methods mentioned here serve merely as examples.*

