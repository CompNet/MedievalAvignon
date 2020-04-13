package fr.univavignon.positioning.vincent;

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
import java.util.TreeMap;
import java.util.TreeSet;

import org.chocosolver.solver.Model;
import org.chocosolver.solver.Solution;
import org.chocosolver.solver.variables.IntVar;

import fr.univavignon.tools.file.FileNames;
import fr.univavignon.tools.file.FileTools;
import fr.univavignon.tools.log.HierarchicalLogger;
import fr.univavignon.tools.log.HierarchicalLoggerManager;
import fr.univavignon.tools.time.TimeFormatting;

/**
 * Class used to load/write resources related to the models.
 * 
 * @author Vincent Labatut
 */
public class ChocoCommon 
{	/** Object used to log the process */
	private final static HierarchicalLogger LOGGER = HierarchicalLoggerManager.getHierarchicalLogger();
	
	/**
	 * Looks for a solution for the specified model.
	 * 
	 * @param model
	 * 		Model containing the variables and constraints.
	 * @return
	 * 		A solution identified for the specified model. 
	 */
	public static Solution searchSolution(Model model)
	{	LOGGER.log("Searching for a solution");
		LOGGER.increaseOffset();
		
		long start = System.currentTimeMillis();
		Solution result = model.getSolver().findSolution();
		long end = System.currentTimeMillis();
		long duration = end - start;
		LOGGER.log("Search over, duration: "+TimeFormatting.formatDuration(duration));
		
		if(result==null)
			LOGGER.log("No solution could be found");

		LOGGER.decreaseOffset();
		return result;
	}
	
	/**
	 * Records the solution through serialization, for possible later use.
	 * 
	 * @param solution
	 * 		The solution to record.
	 * 
	 * @throws IOException
	 * 		Problem while recording the solution.
	 */
	public static void writeSolution(Solution solution) throws IOException
	{	String solutionFile = FileNames.FO_OUTPUT + File.separator + "solution.txt";
		LOGGER.log("Recording the solution in file \""+solutionFile+"\"");
		LOGGER.increaseOffset();
		
		FileOutputStream fos = new FileOutputStream(solutionFile);
		ObjectOutputStream oos = new ObjectOutputStream(fos);
		oos.writeObject(solution);
		oos.close();
		
		LOGGER.log("Recording done");
		LOGGER.decreaseOffset();
	}
	
	/**
	 * Loads the text file containing the description of the objects and constraints,
	 * and builds the corresponding model.
	 * 
	 * @param objectCut
	 * 		Number of objects to ignore, in order to simplify the problem (debug).
	 * @param constCut
	 * 		Number of constraints to ignore, in order to simplify the problem (debug).
	 * @return
	 * 		The model built based on the files content.
	 * 
	 * @throws FileNotFoundException
	 * 		Problem while accessing the files.
	 * @throws UnsupportedEncodingException
	 * 		Problem while accessing the files.
	 */
	public static Model loadConstraints(int objectCut, int constCut) throws FileNotFoundException, UnsupportedEncodingException
	{	LOGGER.log("Load the data and build the model");
		LOGGER.increaseOffset();
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
		String constraintFile = FileNames.FO_INPUT + File.separator + "constraints.txt";
		LOGGER.log("Loading constraint file \""+constraintFile+"\"");
		Scanner scanner = FileTools.openTextFileRead(constraintFile, StandardCharsets.UTF_8.name());
		String line = scanner.nextLine();
		List<List<String>> data = new ArrayList<List<String>>();
		TreeSet<String> objectNames = new TreeSet<String>();
		LOGGER.increaseOffset();
		while(scanner.hasNextLine())
		{	line = scanner.nextLine();
			LOGGER.log("Considering line "+line);
			LOGGER.increaseOffset();
			
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
			{	LOGGER.log("Adding to the list");
				List<String> list = new ArrayList<String>();
				list.add(source);
				objectNames.add(source);
				list.add(target);
				objectNames.add(target);
				list.add(constraint);
				data.add(list);
			}
			
			LOGGER.decreaseOffset();
		}
		int n = objectNames.size();
		LOGGER.log("Kept "+n+" objects and "+data.size()+" constraints");
		LOGGER.decreaseOffset();
		
		LOGGER.log("Removing the last "+objectCut+" objects");
		for(int i=0;i<objectCut;i++)
			objectNames.remove(objectNames.last());
		LOGGER.log("Removing the last "+constCut+" constraints");
		for(int i=0;i<constCut;i++)
			data.remove(data.size()-1);
		
		LOGGER.log("Building the model");
		Map<String,IntVar> objectVars = new TreeMap<String,IntVar>();
		int constNbr = 0;
		LOGGER.increaseOffset();
		for(List<String> list: data)
		{	String sourceName = list.get(0);
			String targetName = list.get(1);
			String constraintName = list.get(2);
		
			if(objectNames.contains(sourceName) && objectNames.contains(targetName))
			{	LOGGER.log("Processed "+sourceName+" ("+constraintName+") "+targetName);
				// set the source variable
				IntVar sourceX = objectVars.get("x"+sourceName);
				IntVar sourceY = objectVars.get("y"+sourceName);
				if(sourceX==null)
				{	sourceX = result.intVar("x"+sourceName, 1, n);
					objectVars.put("x"+sourceName,sourceX);
					sourceY = result.intVar("y"+sourceName, 1, n);
					objectVars.put("y"+sourceName,sourceY);
				}
				
				// set the target variable
				IntVar targetX = objectVars.get("x"+targetName);
				IntVar targetY = objectVars.get("y"+targetName);
				if(targetX==null)
				{	targetX = result.intVar("x"+targetName, 1, n);
					objectVars.put("x"+targetName,targetX);
					targetY = result.intVar("y"+targetName, 1, n);
					objectVars.put("y"+targetName,targetY);
				}
				
				// set the constraint
				if(constraintName.equals("a-circio"))				// north
					result.arithm(sourceY, ">", targetY).post();
				else if(constraintName.equals("a-meridie"))			// south
					result.arithm(sourceY, "<", targetY).post();
				else if(constraintName.equals("a-orient"))			// east
					result.arithm(sourceX, ">", targetX).post();
				else if(constraintName.equals("a-occident"))		// west
					result.arithm(sourceX, "<", targetX).post();
				constNbr++;
			}
		}
		LOGGER.decreaseOffset();
		
		LOGGER.log("Model complete: "+objectVars.size()+" variables and "+constNbr+" constraints");
		LOGGER.decreaseOffset();
		return result;
	}
}
