package fr.univavignon.positioning.vincent;

import org.chocosolver.solver.Model;
import org.chocosolver.solver.Solution;
import org.chocosolver.solver.variables.IntVar;

public class Test0 
{	/**
	 * Tests a toy problem using ChocoSolver.
	 * 
	 * @param args
	 * 		Not used.
	 * 
	 * @throws Exception
	 * 		Some problem occurred.
	 */
	public static void main(String[] args) throws Exception
	{	Model model = new Model("Positioning test");
		
		// init variables
		int n = 6;
		IntVar[] x = new IntVar[n];
		IntVar[] y = new IntVar[n];
		for(int i=0; i<n; i++)
		{	x[i] = model.intVar("x"+i, 1, n);
			y[i] = model.intVar("y"+i, 1, n);
		}
		
		// init x constaints
		model.arithm(x[1], ">", x[0]).post();
		model.arithm(x[1], ">", x[3]).post();
		model.arithm(x[1], ">", x[4]).post();
		model.arithm(x[5], ">", x[0]).post();
		model.arithm(x[5], ">", x[3]).post();
		model.arithm(x[5], ">", x[4]).post();
		//
		model.arithm(x[0], ">", x[2]).post();
		model.arithm(x[3], ">", x[2]).post();
		model.arithm(x[4], ">", x[2]).post();
		
		// init y constraints
		model.arithm(y[0], ">", y[2]).post();
		model.arithm(y[0], ">", y[3]).post();
		model.arithm(y[1], ">", y[2]).post();
		model.arithm(y[1], ">", y[3]).post();
		//
		model.arithm(y[2], ">", y[4]).post();
		model.arithm(y[2], ">", y[5]).post();
		model.arithm(y[3], ">", y[4]).post();
		model.arithm(y[3], ">", y[5]).post();
		
		// solve problem
		Solution solution = model.getSolver().findSolution();
		if(solution != null){
		    System.out.println(solution.toString());
		}
	}
}
