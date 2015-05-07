#include <iostream>
#include <cstdlib>
#include "argraph.h"
#include "argedit.h"
#include "match.h"
#include "vf2_sub_state.h"
//#include "ull_sub_state.h"

#define MAXNODES 1000

Graph readgraph();
void print(Graph);

int main()
{
   Graph sub = readgraph();
   Graph super = readgraph();
   node_id nsub[MAXNODES], nsuper[MAXNODES];
   int res, n;

   //print(sub);
   //print(super);
   VF2SubState s0(&sub, &super);
   //UllSubState s0(&sub, &super);
   res = match(&s0, &n, nsub, nsuper);
   std::cout << res << std::endl;
   exit(0);
}

Graph readgraph()
{
   ARGEdit ed;
   int nodecount, i, j;

   std::cin >> nodecount;
   for (i = 0; i < nodecount; i++)
      ed.InsertNode(NULL);

   while (std::cin >> i, i != -1) {
      std::cin >> j;
      if (i < 0  ||  i >= nodecount  ||  j < 0  ||  j >= nodecount) {
	 std::cout <<"Bad edge " << i << ", " << j;
	 exit(1);
      }
      ed.InsertEdge(i, j, NULL);
      ed.InsertEdge(j, i, NULL);
   }

   Graph g(&ed);
   return g;
}

void print(Graph g)
{
   int i, j;
   int nodecount = g.NodeCount();
   std::cout << nodecount << std::endl;
   for (i = 0; i < nodecount; i++)
      for (j = 0; j < nodecount; j++) {
	 std::cout << "[" << i << " " << j << "]";
	 if (g.HasEdge(i,j))
	    std::cout << i << " " << j << std::endl;
      }
   std::cout << -1 <<std::endl;
}
