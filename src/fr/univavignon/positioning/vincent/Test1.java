package fr.univavignon.positioning.vincent;

import org.chocosolver.solver.Model;
import org.chocosolver.solver.Solution;

import fr.univavignon.tools.log.HierarchicalLogger;
import fr.univavignon.tools.log.HierarchicalLoggerManager;

public class Test1 
{	/** Object used to log the process */
	private final static HierarchicalLogger LOGGER = HierarchicalLoggerManager.getHierarchicalLogger();
	
	/**
	 * Launches the main program.
	 * Loops over the constraints, trying to remove those causing the
	 * exception.
	 * 
	 * @param args
	 * 		Not used.
	 * 
	 * @throws Exception
	 * 		Some problem occurred.
	 */
	public static void main(String[] args) throws Exception
	{	LOGGER.log("Starting the process");
		LOGGER.increaseOffset();
		
		Solution solution = null;
		int cut = 0;
		LOGGER.log("Looping over various number of constraints");
		LOGGER.increaseOffset();
		while(solution==null)
		{	LOGGER.log("Cutting the last "+cut+" constraints");
			LOGGER.increaseOffset();
			
			// load and build the model
			Model model = ChocoCommon.loadConstraints(0,cut);
			
			// solve the problem
			solution = ChocoCommon.searchSolution(model);
			
			if(solution != null)
			{	// write the solution
				ChocoCommon.writeSolution(solution);
				
				// display the solution
			    System.out.println(solution.toString());
			}
			else
				cut++;
			
			Thread.sleep(500);
			LOGGER.decreaseOffset();
		}
		LOGGER.decreaseOffset();
		
		LOGGER.decreaseOffset();
		LOGGER.log("Process complete");
		LOGGER.close();
	}
}
