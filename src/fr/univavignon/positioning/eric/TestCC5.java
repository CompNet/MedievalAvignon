package fr.univavignon.positioning.eric;

import org.chocosolver.solver.Model;
import org.chocosolver.solver.Solution;
import org.chocosolver.solver.Solver;
import org.chocosolver.solver.search.strategy.Search;
import org.chocosolver.solver.search.strategy.selectors.values.IntDomainMin;
import org.chocosolver.solver.search.strategy.selectors.variables.Smallest;
import org.chocosolver.solver.variables.IntVar;

public class TestCC5 
{	/**
	 * Tests a small CC using ChocoSolver.
	 * 
	 */
	public static void main(String[] args) throws Exception
	{	Model model = new Model("Positioning test for CC6");
		
		// init variables
		int n = 10; 
		String[] names = new String[] {"0-B3","1-B4","2-B5","3-BN801","4-BN802","5-BN803","6-BN804","7-R68","8-R75","9-E152"};
		IntVar[] X   = new IntVar[n];  // rank value on x axis
		IntVar[] Y   = new IntVar[n];  // rank value on y axis
		IntVar[] XLB = new IntVar[n];  // Lower Bound value for x
		IntVar[] YLB = new IntVar[n];  // Lower Bound value for y
		IntVar[] XUB = new IntVar[n];  // Upper Bound value for x
		IntVar[] YUB = new IntVar[n];  // Upper Bound value for y
		IntVar[] H   = new IntVar[n];  // Height value 
		IntVar[] W   = new IntVar[n];  // Width value
		IntVar[] Ex  = new IntVar[n];  // End value for x+W
		IntVar[] Ey  = new IntVar[n];  // End value for y+H
		
		IntVar[] ranks  = new IntVar[2*n]; // aggregated rank to enumerate position variables
		IntVar[] bounds = new IntVar[4*n]; // aggregated vector to enumerate bounds variables
		IntVar[] sizes  = new IntVar[2*n]; // aggregated vector to enumerate size variables
		
		for(int i=0; i<n; i++)
		{	X[i] = model.intVar("x"+i, 1, n);
			Y[i] = model.intVar("y"+i, 1, n);
			ranks[2*i] = X[i];
			ranks[2*i+1] = Y[i];
			XLB[i] = model.intVar("xLB"+i, 1, n);
			XUB[i] = model.intVar("xUB"+i, 1, n);
			YLB[i] = model.intVar("yLB"+i, 1, n);
			YUB[i] = model.intVar("yUB"+i, 1, n);
			bounds[4*i  ] = XLB[i];
			bounds[4*i+1] = XUB[i];
			bounds[4*i+2] = YLB[i];
			bounds[4*i+3] = YUB[i];
			W[i] = model.intVar("W"+i, 1, n);
			H[i] = model.intVar("H"+i, 1, n);
			Ex[i] = model.intVar("Ex"+i, 1, n+1);
			Ey[i] = model.intVar("Ey"+i, 1, n+1);
			sizes[2*i] = W[i];
			sizes[2*i+1] = H[i];
		}
		
		for(int i=0; i<n; i++)
		{	model.arithm(XUB[i], ">=", X[i]).post();
			model.arithm(XLB[i], "<=", X[i]).post();
			model.arithm(YUB[i], ">=", Y[i]).post();
			model.arithm(YLB[i], "<=", Y[i]).post();
			model.arithm(X[i], "+", W[i], "=", Ex[i]).post();
			model.arithm(Y[i], "+", H[i], "=", Ey[i]).post();
		};
		for(int i=0; i<n-3; i++)
		{	model.arithm(H[i], "=", 1).post();
			model.arithm(W[i], "=", 1).post();
		}
		
		//B2B
		model.arithm(Y[1], ">", Y[0]).post();
		model.arithm(Y[2], ">", Y[1]).post();
		//B2BN
		model.arithm(Y[0], ">", Y[4]).post();
		model.arithm(Y[6], ">", Y[2]).post();
		//B2E
		model.arithm(Y[0], ">", Y[9]).post();
		
		//B2BN
		model.arithm(X[3], ">", X[0]).post();
		model.arithm(X[5], ">", X[1]).post();
		model.arithm(X[5], ">", X[2]).post();
		//B2R
		model.arithm(X[7], ">", X[0]).post();
		model.arithm(X[8], ">", X[0]).post();
		model.arithm(X[8], ">", X[1]).post();
		model.arithm(X[8], ">", X[2]).post();
		
		// orthogonal constraints
		model.min(YLB[5], new IntVar[] {Y[1],Y[2]}).post();
		model.max(YUB[5], new IntVar[] {Y[1],Y[2]}).post();

		model.min(YLB[3], new IntVar[] {Y[0]}).post();
		model.max(YUB[3], new IntVar[] {Y[0]}).post();

		model.min(YLB[7], new IntVar[] {Y[0]}).post();
		model.max(YUB[7], new IntVar[] {Y[0]}).post();
		
		
		// sizes
		model.min(Y[8], new IntVar[] {Y[0],Y[1],Y[2]}).post();
		model.max(Ey[8], new IntVar[] {Ey[0],Ey[1],Ey[2]}).post();
		
		
		// solve problem
		Solver solver = model.getSolver();
		solver.setSearch(
				Search.intVarSearch(new Smallest(), new IntDomainMin(), ranks),
				Search.intVarSearch(new Smallest(), new IntDomainMin(), sizes),
				Search.intVarSearch(new Smallest(), new IntDomainMin(), bounds)
				);						
		solver.showStatistics();
		//solver.showStatisticsDuringResolution(1000);
		//solver.showDecisions();
		//solver.showContradiction();
		//solver.showSolutions();
		//solver.limitTime(maxTimeSearch + "s");
		//solver.setLDS(2000);


		
		Solution solution = model.getSolver().findSolution();
		if(solution != null){
		    System.out.println(solution.toString());
		}
		for(int i=0; i<n; i++) {
			System.out.println(names[i]+" x="+solution.getIntVal(X[i])+";y="+solution.getIntVal(Y[i])+"  L="+solution.getIntVal(W[i])+";H="+solution.getIntVal(H[i]));
		}

	}
}

// NSEO v0 : Solution: x0=1, y0=2, x1=1, y1=3, x2=1, y2=4, x3=2, y3=1, x4=1, y4=1, x5=2, y5=1, x6=1, y6=5, x7=2, y7=1, x8=2, y8=1, x9=1, y9=1, 
/* 101v 94c 49n 0b
Solution: x0=1, y0=2, xLB0=1, xUB0=1, yLB0=1, yUB0=2, W0=1, H0=1, Ex0=2, Ey0=3, x1=1, y1=3, xLB1=1, xUB1=1, yLB1=1, yUB1=3, W1=1, H1=1, Ex1=2, Ey1=4, x2=1, y2=4, xLB2=1, xUB2=1, yLB2=1, yUB2=4, W2=1, H2=1, Ex2=2, Ey2=5, x3=2, y3=2, xLB3=1, xUB3=2, yLB3=2, yUB3=2, W3=1, H3=1, Ex3=3, Ey3=3, x4=1, y4=1, xLB4=1, xUB4=1, yLB4=1, yUB4=1, W4=1, H4=1, Ex4=2, Ey4=2, x5=2, y5=3, xLB5=1, xUB5=2, yLB5=3, yUB5=4, W5=1, H5=1, Ex5=3, Ey5=4, x6=1, y6=5, xLB6=1, xUB6=1, yLB6=1, yUB6=5, W6=1, H6=1, Ex6=2, Ey6=6, x7=2, y7=2, xLB7=1, xUB7=2, yLB7=2, yUB7=2, W7=1, H7=1, Ex7=3, Ey7=3, x8=2, y8=2, xLB8=1, xUB8=2, yLB8=1, yUB8=2, W8=1, H8=3, Ex8=3, Ey8=5, x9=1, y9=1, xLB9=1, xUB9=1, yLB9=1, yUB9=1, W9=1, H9=1, Ex9=2, Ey9=2, 
0-B3 x=1;y=2  L=1;H=1
1-B4 x=1;y=3  L=1;H=1
2-B5 x=1;y=4  L=1;H=1
3-BN801 x=2;y=2  L=1;H=1
4-BN802 x=1;y=1  L=1;H=1
5-BN803 x=2;y=3  L=1;H=1
6-BN804 x=1;y=5  L=1;H=1
7-R68 x=2;y=2  L=1;H=1
8-R75 x=2;y=2  L=1;H=3
9-E152 x=1;y=1  L=1;H=1
*/
