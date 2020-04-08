package fr.univavignon.tools.file;

/*
 * CommonTools
 * Copyright 2010-19 Vincent Labatut
 * 
 * This file is part of CommonTools.
 * 
 * CommonTools is free software: you can redistribute it and/or modify it under 
 * the terms of the GNU General Public License as published by the Free Software 
 * Foundation, either version 3 of the License, or (at your option) any later version.
 * 
 * CommonTools is distributed in the hope that it will be useful, but WITHOUT ANY 
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A 
 * PARTICULAR PURPOSE. See the GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with CommonTools. If not, see <http://www.gnu.org/licenses/>.
 */

import java.io.File;

/**
 * This class contains various constants
 * related to file and folder names.
 *  
 * @version 2.3
 * @author Vincent Labatut
 */
public class FileNames
{	
	/////////////////////////////////////////////////////////////////
	// FOLDERS		/////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////
	/** Input folder */
	public final static String FO_INPUT = "in";
	/** Log folder */
	public final static String FO_LOG = "log";
	/** Output folder */
	public static String FO_OUTPUT = "out";
	/** Resources folder */
	public final static String FO_RESOURCES = "res";
		/** Folder used to store various data */
		public final static String FO_MISC = FO_RESOURCES + File.separator + "misc";
		/** Folder containing the XML schemas */
		public final static String FO_SCHEMA = FO_RESOURCES + File.separator + "schemas";
	
	/////////////////////////////////////////////////////////////////
	// EXTENSIONS	/////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////
	/** Bash file extension */
	public final static String EX_BASH = ".bash";
	/** Binary file extension */
	public final static String EX_BIN = ".bin";
	/** BibTeX file extension */
	public final static String EX_BIBTEX = ".bib";
	/** Comma-separated values (CSV) file extension */
	public final static String EX_CSV = ".csv";
	/** GIF image format */
	public final static String EX_GIF = ".gif";
	/** Graphml file extension */
	public final static String EX_GRAPHML = ".graphml";
	/** HTML file extension */
	public final static String EX_HTML = ".html";
	/** Thomson ISI file extension */
	public final static String EX_ISI = ".ciw";
	/** JPEG image format */
	public final static String EX_JPEG = ".jpeg";
	/** PDF file extension */
	public final static String EX_PDF = ".pdf";
	/** PNG image format */
	public final static String EX_PNG = ".png";
	/** Scripts in Python */
	public final static String EX_PYTHON = ".py";
	/** Scripts in R language */
	public final static String EX_R = ".R";
	/** XML Schema file extension */
	public final static String EX_SCHEMA = ".xsd";
	/** SQL scripts */
	public final static String EX_SQL = ".sql";
	/** Text file extension */
	public final static String EX_TEXT = ".txt";
	/** XML file extension */
	public final static String EX_XML = ".xml";

	/////////////////////////////////////////////////////////////////
	// FILES		/////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////
	/** XML schema file used for graphml  */
	public final static String FI_GRAPHML_SCHEMA = "graphml" + FileNames.EX_SCHEMA;
	/** XML file used to store keys */
	public final static String FI_KEY_LIST = "keys" + FileNames.EX_XML;
	/** XML schema of the file used to store keys */
	public final static String FI_KEY_SCHEMA = "keys" + FileNames.EX_SCHEMA;
}
