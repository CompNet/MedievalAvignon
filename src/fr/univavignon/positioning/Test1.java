package fr.univavignon.positioning;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.io.UnsupportedEncodingException;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Scanner;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import org.chocosolver.solver.Model;
import org.chocosolver.solver.Solution;
import org.chocosolver.solver.variables.IntVar;

import fr.univavignon.tools.file.FileTools;
import fr.univavignon.tools.log.HierarchicalLogger;
import fr.univavignon.tools.log.HierarchicalLoggerManager;
import fr.univavignon.tools.time.TimeFormatting;

public class Test1 
{	
	
	/**
	 * Launches the main program.
	 * 
	 * @param args
	 * 		Not used.
	 * 
	 * @throws Exception
	 * 		Some problem occurred.
	 */
	public static void main(String[] args) throws Exception
	{	logger = HierarchicalLoggerManager.getHierarchicalLogger();
		logger.log("Starting the process");
		logger.increaseOffset();
		
		Solution solution = null;
		int cut = 0;
		logger.log("Looping over various number of constraints");
		logger.increaseOffset();
		while(solution==null)
		{	logger.log("Cuttint the last "+cut+" constraints");
			logger.increaseOffset();
			
			// load and build the model
			Model model = loadConstraints(cut);
			
			// solve the problem
			solution = searchSolution(model);
			
			if(solution != null)
			{	// write the solution
				writeSolution(solution);
				
				// display the solution
			    System.out.println(solution.toString());
			}
			else
				cut++;
			
			Thread.sleep(500);
			logger.decreaseOffset();
		}
		logger.decreaseOffset();
		
		logger.decreaseOffset();
		logger.log("Process complete");
		logger.close();
	}
	
	/** Object used to log the process */
	private static HierarchicalLogger logger;
	
	/**
	 * Look for a solution for the specified model.
	 * 
	 * @param model
	 * 		Model containing the variables and constraints.
	 * @return
	 * 		A solution identified for the specified model. 
	 */
	private static Solution searchSolution(Model model)
	{	logger.log("Searching for a solution");
		logger.increaseOffset();
		
		long start = System.currentTimeMillis();
		Solution result = model.getSolver().findSolution();
		long end = System.currentTimeMillis();
		long duration = end - start;
		logger.log("Search over, duration: "+TimeFormatting.formatDuration(duration));
		
		if(result==null)
			logger.log("No solution could be found");

		logger.decreaseOffset();
		return result;
	}
	
	/**
	 * Record the solution through serialization, for possible alter use.
	 * 
	 * @param solution
	 * 		The solution to record.
	 * 
	 * @throws IOException
	 * 		Problem while recording the solution.
	 */
	private static void writeSolution(Solution solution) throws IOException
	{	String solutionFile = "out" + File.separator + "solution.txt";
		logger.log("Recording the solution in file \""+solutionFile+"\"");
		logger.increaseOffset();
		
		FileOutputStream fos = new FileOutputStream(solutionFile);
		ObjectOutputStream oos = new ObjectOutputStream(fos);
		oos.writeObject(solution);
		oos.close();
		
		logger.log("Recording done");
		logger.decreaseOffset();
	}
	
	/**
	 * Method allowing to test one example from the ChocoSolver doc.
	 */
	@SuppressWarnings("unused")
	private static void test()
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
	
	/**
	 * Loads the text file containing the description of the objects and constraints,
	 * and builds the corresponding model.
	 * 
	 * @return
	 * 		The model built based on the files content.
	 * 
	 * @throws FileNotFoundException
	 * 		Problem while accessing the files.
	 * @throws UnsupportedEncodingException
	 * 		Problem while accessing the files.
	 */
	private static Model loadConstraints(int cut) throws FileNotFoundException, UnsupportedEncodingException
	{	logger.log("Load the data and build the model");
		logger.increaseOffset();
		Model result = new Model("Positioning");
		
		// allowed types of objects
		List<String> allowedPrefixes = Arrays.asList(
			"/BD_", 	// Bien Déclaré
			"/BND_"		// Bien non déclaré
//			"/BG_",		// Bourg
//			"/BGN_",	// Bourg non localisé
//			"/E_",		// édifice
//			"/EN_",		// édifice non localisé
//			"/L_",		// livrée cardinalice (palais de cardinal)
//			"/H_",		// Hydrologie
//			"/M_",		// rempart
//			"/R_",		// rue
//			"/RN_"		// rue non localisée
		);
		// allowed constraints
		List<String> allowedConstraints = Arrays.asList(
			"a-circio",					// au nord
			"a-meridie",				// au sud
			"a-orient",					// à l'est
			"a-occident"				// à l'ouest
//			"ab-una-part",				// d'une part (confront non orienté, simplement mention de « confront d'une part avec… »
//			"in",						// dans
//			"in compito sive cantono",	// au carrefour, à l'intersection de rues
//			"in medio",					// au milieu
//			"in capite",				// au début (à la tête de..)
//			"juxta",					// à côté de
//			"prope",					// proche
//			"ante",						// devant
//			"retro",					// derrière
//			"seu cum",					// ou avec (hésitation du notaire pour les confronts..)
//			"subtus",					// sous
//			"versus"					// vers
		);
		
		// load the file, retrieving only the authorized objects and contraints
		String constraintFile = "in" + File.separator + "constraints.txt";
		logger.log("Loading constraint file \""+constraintFile+"\"");
		Scanner scanner = FileTools.openTextFileRead(constraintFile, StandardCharsets.UTF_8.name());
		String line = scanner.nextLine();
		List<List<String>> data = new ArrayList<List<String>>();
		Set<String> objectNames = new TreeSet<String>();
		logger.increaseOffset();
		while(scanner.hasNextLine())
		{	line = scanner.nextLine();
			logger.log("Considering line "+line);
			logger.increaseOffset();
			
			// break down the line
			String[] splitLine = line.split("\t");
			String source = splitLine[0].trim();
			String target = splitLine[1].trim();
			String constraint = splitLine[2].trim();

			// get the object prefixes
			String srcPref = source.split("_")[0] + "_";
			String tgtPref = target.split("_")[0] + "_";
			
			// check if authorized
			if(allowedPrefixes.contains(srcPref) && allowedPrefixes.contains(tgtPref) && allowedConstraints.contains(constraint))
			{	logger.log("Adding to the list");
				List<String> list = new ArrayList<String>();
				list.add(source);
				objectNames.add(source);
				list.add(target);
				objectNames.add(target);
				list.add(constraint);
				data.add(list);
			}
			
			logger.decreaseOffset();
		}
		int n = objectNames.size();
		logger.log("Kept "+n+" objects and "+data.size()+" constraints");
		logger.decreaseOffset();
		
		logger.log("Removing the last "+cut+" constraints");
		for(int i=0;i<cut;i++)
			data.remove(data.size()-1);
		
		logger.log("Building the model");
		Map<String,IntVar> objectVars = new TreeMap<String,IntVar>();
		logger.increaseOffset();
		for(List<String> list: data)
		{	// set the source variable
			String sourceName = list.get(0);
			IntVar sourceX = objectVars.get("x"+sourceName);
			IntVar sourceY = objectVars.get("y"+sourceName);
			if(sourceX==null)
			{	sourceX = result.intVar("x"+sourceName, 1, n);
				objectVars.put("x"+sourceName,sourceX);
				sourceY = result.intVar("y"+sourceName, 1, n);
				objectVars.put("y"+sourceName,sourceY);
			}
			
			// set the target variable
			String targetName = list.get(1);
			IntVar targetX = objectVars.get("x"+targetName);
			IntVar targetY = objectVars.get("y"+targetName);
			if(targetX==null)
			{	targetX = result.intVar("x"+targetName, 1, n);
				objectVars.put("x"+targetName,targetX);
				targetY = result.intVar("y"+targetName, 1, n);
				objectVars.put("y"+targetName,targetY);
			}
			
			// set the constraint
			String constraintName = list.get(2);
			if(constraintName.equals("a-circio"))				// north
				result.arithm(sourceY, ">", targetY).post();
			else if(constraintName.equals("a-meridie"))			// south
				result.arithm(sourceY, "<", targetY).post();
			else if(constraintName.equals("a-orient"))			// east
				result.arithm(sourceX, ">", targetX).post();
			else if(constraintName.equals("a-occident"))		// west
				result.arithm(sourceX, "<", targetX).post();
			
			logger.log("Processed "+sourceName+" ("+constraintName+") "+targetName);
		}
		logger.decreaseOffset();
		
		logger.log("Model complete: "+objectVars.size()+" variables and "+data.size()+" constraints");
		logger.decreaseOffset();
		return result;
	}
}

// TODO mieux: rentrer les variables une par une (et toutes les contraintes aférentes)
