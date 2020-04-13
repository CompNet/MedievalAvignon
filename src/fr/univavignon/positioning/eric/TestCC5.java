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
	 * CC display using ChocoSolver.
	 * 
	 */
	static boolean shape = false;
	static int n = 92; // nb total nodes
	static int nbRoad = 0;
	
	public static void main(String[] args) throws Exception
	{	Model model = new Model("Positioning test for CC5");
		
		// init variables
//		int n; 
//		String[] names = new String[] {"0-B3","1-B4","2-B5","3-BN801","4-BN802","5-BN803","6-BN804","7-R68","8-R75","9-E152"};
		String[] names = new String[] {
		"BD_219","BD_220","BD_221","BD_222","BD_223","BD_224","BD_225","BD_226","BD_227","BD_228","BD_229",
		"BD_230","BD_231","BD_232","BD_233","BD_234","BD_235","BD_236","BD_237","BD_238","BD_239","BD_240",
		"BD_241","BD_242","BD_243","BD_244","BD_245","BD_246","BD_247","BD_248","BD_249","BD_250","BD_251",
		"BD_252","BD_253","BD_254","BD_255","BD_256","BD_257","BD_456","BD_466","BD_467","BD_468","BD_469",
		"BD_470","BD_471","BD_472","BD_473","BD_474","BD_475","BD_476","BD_477","BD_478","BD_479","BD_480",
		"BD_481","BD_482","BD_483","BD_484","BD_485","BD_486","BD_487","BD_488","BD_489","BD_490","BD_491",
		"BD_492","BD_493","BD_494","BD_495","BD_496","BD_497","BD_498","BD_499","BD_500","BD_501","BD_502",
		"BD_503","BD_504","BND_869","BND_870","BND_871","BND_872","BND_873","BND_874","BND_875","BND_876",
		"BND_877","BND_885","BND_886","BND_887","E_160"
		};	
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
			if (shape) {
				XLB[i] = model.intVar("xLB"+i, 1, n);
				XUB[i] = model.intVar("xUB"+i, 1, n);
				YLB[i] = model.intVar("yLB"+i, 1, n);
				YUB[i] = model.intVar("yUB"+i, 1, n);
				bounds[4*i  ] = XLB[i];
				bounds[4*i+1] = XUB[i];
				bounds[4*i+2] = YLB[i];
				bounds[4*i+3] = YUB[i];
//			}
			W[i] = model.intVar("W"+i, 1, n);
			H[i] = model.intVar("H"+i, 1, n);
			Ex[i] = model.intVar("Ex"+i, 1, n+1);
			Ey[i] = model.intVar("Ey"+i, 1, n+1);
			sizes[2*i] = W[i];
			sizes[2*i+1] = H[i];
			}
		}
		
		for(int i=0; i<n; i++) 
		{				
			if (shape) {
				model.arithm(XUB[i], ">=", X[i]).post();
				model.arithm(XLB[i], "<=", X[i]).post();
				model.arithm(YUB[i], ">=", Y[i]).post();
				model.arithm(YLB[i], "<=", Y[i]).post();
//			}
			model.arithm(X[i], "+", W[i], "=", Ex[i]).post();
			model.arithm(Y[i], "+", H[i], "=", Ey[i]).post();
			}
		};
		
		if (shape) {
		for(int i=0; i<n-nbRoad; i++)
		{	model.arithm(H[i], "=", 1).post();
			model.arithm(W[i], "=", 1).post();
		}
		}
		
		model.arithm(Y[1],">",Y[0]).post();
		model.arithm(Y[2],">",Y[1]).post();
		model.arithm(Y[3],">",Y[2]).post();
		model.arithm(Y[10],">",Y[9]).post();
		model.arithm(Y[11],">",Y[10]).post();
		model.arithm(Y[12],">",Y[11]).post();
		model.arithm(Y[13],">",Y[12]).post();
		model.arithm(Y[14],">",Y[13]).post();
		model.arithm(Y[15],">",Y[14]).post();
		model.arithm(Y[16],">",Y[15]).post();
//		model.arithm(Y[18],">",Y[18]).post();
		model.arithm(Y[19],">",Y[18]).post();
		model.arithm(Y[20],">",Y[19]).post();
		model.arithm(Y[20],">",Y[18]).post();
		model.arithm(Y[21],">",Y[20]).post();
		model.arithm(Y[22],">",Y[25]).post();
		model.arithm(Y[23],">",Y[25]).post();
		model.arithm(Y[24],">",Y[29]).post();
		model.arithm(Y[24],">",Y[31]).post();
		model.arithm(Y[25],">",Y[26]).post();
		model.arithm(Y[25],">",Y[27]).post();
		model.arithm(Y[34],">",Y[33]).post();
		model.arithm(Y[35],">",Y[34]).post();
		model.arithm(Y[36],">",Y[32]).post();
		model.arithm(Y[37],">",Y[36]).post();
		model.arithm(Y[38],">",Y[35]).post();
		model.arithm(Y[38],">",Y[24]).post();
		model.arithm(Y[40],">",Y[88]).post();
		model.arithm(Y[46],">",Y[45]).post();
		model.arithm(Y[48],">",Y[49]).post();
		model.arithm(Y[54],">",Y[53]).post();
		model.arithm(Y[55],">",Y[54]).post();
		model.arithm(Y[56],">",Y[55]).post();
		model.arithm(Y[56],">",Y[52]).post();
		model.arithm(Y[57],">",Y[50]).post();
		model.arithm(Y[58],">",Y[48]).post();
		model.arithm(Y[59],">",Y[48]).post();
		model.arithm(Y[60],">",Y[48]).post();
		model.arithm(Y[61],">",Y[90]).post();
		model.arithm(Y[62],">",Y[41]).post();
		model.arithm(Y[65],">",Y[64]).post();
		model.arithm(Y[66],">",Y[65]).post();
		model.arithm(Y[67],">",Y[66]).post();
		model.arithm(Y[68],">",Y[67]).post();
		model.arithm(Y[69],">",Y[68]).post();
		model.arithm(Y[70],">",Y[71]).post();
		model.arithm(Y[71],">",Y[72]).post();
		model.arithm(Y[72],">",Y[73]).post();
		model.arithm(Y[73],">",Y[74]).post();
		model.arithm(Y[74],">",Y[75]).post();
		model.arithm(Y[75],">",Y[76]).post();
		model.arithm(Y[76],">",Y[77]).post();
		model.arithm(Y[77],">",Y[78]).post();
		model.arithm(Y[77],">",Y[34]).post();
		model.arithm(Y[0],"<",Y[40]).post();
		model.arithm(Y[0],"<",Y[41]).post();
		model.arithm(Y[0],"<",Y[42]).post();
		model.arithm(Y[0],"<",Y[43]).post();
		model.arithm(Y[0],"<",Y[44]).post();
		model.arithm(Y[0],"<",Y[45]).post();
		model.arithm(Y[0],"<",Y[46]).post();
		model.arithm(Y[0],"<",Y[47]).post();
		model.arithm(Y[0],"<",Y[48]).post();
		model.arithm(Y[0],"<",Y[49]).post();
		model.arithm(Y[0],"<",Y[50]).post();
		model.arithm(Y[0],"<",Y[51]).post();
		model.arithm(Y[0],"<",Y[52]).post();
		model.arithm(Y[0],"<",Y[53]).post();
		model.arithm(Y[0],"<",Y[54]).post();
		model.arithm(Y[0],"<",Y[55]).post();
		model.arithm(Y[0],"<",Y[56]).post();
		model.arithm(Y[0],"<",Y[57]).post();
		model.arithm(Y[0],"<",Y[58]).post();
		model.arithm(Y[0],"<",Y[59]).post();
		model.arithm(Y[0],"<",Y[60]).post();
		model.arithm(Y[0],"<",Y[61]).post();
		model.arithm(Y[0],"<",Y[62]).post();
		model.arithm(Y[0],"<",Y[63]).post();
		model.arithm(Y[1],"<",Y[3]).post();
		model.arithm(Y[2],"<",Y[3]).post();
		model.arithm(Y[7],"<",Y[91]).post();
		model.arithm(Y[10],"<",Y[11]).post();
		model.arithm(Y[11],"<",Y[12]).post();
		model.arithm(Y[12],"<",Y[13]).post();
		model.arithm(Y[13],"<",Y[14]).post();
		model.arithm(Y[14],"<",Y[15]).post();
		model.arithm(Y[16],"<",Y[19]).post();
		model.arithm(Y[17],"<",Y[18]).post();
		model.arithm(Y[18],"<",Y[20]).post();
		model.arithm(Y[19],"<",Y[20]).post();
		model.arithm(Y[20],"<",Y[21]).post();
		model.arithm(Y[20],"<",Y[82]).post();
		model.arithm(Y[21],"<",Y[83]).post();
		model.arithm(Y[22],"<",Y[86]).post();
		model.arithm(Y[22],"<",Y[87]).post();
		model.arithm(Y[25],"<",Y[23]).post();
		model.arithm(Y[25],"<",Y[22]).post();
		model.arithm(Y[26],"<",Y[25]).post();
		model.arithm(Y[27],"<",Y[25]).post();
		model.arithm(Y[28],"<",Y[65]).post();
		model.arithm(Y[29],"<",Y[24]).post();
		model.arithm(Y[30],"<",Y[77]).post();
		model.arithm(Y[31],"<",Y[24]).post();
		model.arithm(Y[32],"<",Y[36]).post();
		model.arithm(Y[33],"<",Y[34]).post();
		model.arithm(Y[34],"<",Y[35]).post();
		model.arithm(Y[35],"<",Y[36]).post();
		model.arithm(Y[36],"<",Y[37]).post();
		model.arithm(Y[37],"<",Y[38]).post();
		model.arithm(Y[39],"<",Y[1]).post();
		model.arithm(Y[45],"<",Y[46]).post();
		model.arithm(Y[47],"<",Y[60]).post();
		model.arithm(Y[48],"<",Y[58]).post();
		model.arithm(Y[49],"<",Y[48]).post();
		model.arithm(Y[49],"<",Y[58]).post();
		model.arithm(Y[50],"<",Y[57]).post();
		model.arithm(Y[51],"<",Y[56]).post();
		model.arithm(Y[52],"<",Y[56]).post();
		model.arithm(Y[53],"<",Y[56]).post();
		model.arithm(Y[53],"<",Y[54]).post();
		model.arithm(Y[54],"<",Y[55]).post();
		model.arithm(Y[55],"<",Y[56]).post();
		model.arithm(Y[64],"<",Y[65]).post();
		model.arithm(Y[65],"<",Y[66]).post();
		model.arithm(Y[66],"<",Y[67]).post();
		model.arithm(Y[68],"<",Y[69]).post();
		model.arithm(Y[69],"<",Y[24]).post();
		model.arithm(Y[70],"<",Y[24]).post();
		model.arithm(Y[71],"<",Y[70]).post();
		model.arithm(Y[72],"<",Y[71]).post();
		model.arithm(Y[73],"<",Y[72]).post();
		model.arithm(Y[74],"<",Y[73]).post();
		model.arithm(Y[75],"<",Y[74]).post();
		model.arithm(Y[76],"<",Y[75]).post();
//		model.arithm(Y[77],"<",Y[77]).post();
		model.arithm(Y[78],"<",Y[77]).post();
		model.arithm(X[4],"<",X[5]).post();
		model.arithm(X[5],"<",X[6]).post();
		model.arithm(X[6],"<",X[7]).post();
		model.arithm(X[7],"<",X[8]).post();
		model.arithm(X[16],"<",X[17]).post();
		model.arithm(X[16],"<",X[18]).post();
		model.arithm(X[17],"<",X[18]).post();
		model.arithm(X[19],"<",X[16]).post();
		model.arithm(X[21],"<",X[82]).post();
		model.arithm(X[22],"<",X[23]).post();
		model.arithm(X[23],"<",X[24]).post();
		model.arithm(X[24],"<",X[37]).post();
		model.arithm(X[25],"<",X[29]).post();
		model.arithm(X[25],"<",X[24]).post();
		model.arithm(X[26],"<",X[27]).post();
		model.arithm(X[27],"<",X[28]).post();
		model.arithm(X[28],"<",X[29]).post();
		model.arithm(X[29],"<",X[30]).post();
		model.arithm(X[29],"<",X[31]).post();
//		model.arithm(X[30],"<",X[29]).post();
		model.arithm(X[30],"<",X[34]).post();
		model.arithm(X[31],"<",X[32]).post();
		model.arithm(X[31],"<",X[36]).post();
		model.arithm(X[31],"<",X[37]).post();
		model.arithm(X[32],"<",X[33]).post();
		model.arithm(X[36],"<",X[35]).post();
		model.arithm(X[40],"<",X[41]).post();
		model.arithm(X[41],"<",X[42]).post();
		model.arithm(X[42],"<",X[43]).post();
		model.arithm(X[43],"<",X[44]).post();
		model.arithm(X[44],"<",X[45]).post();
		model.arithm(X[44],"<",X[46]).post();
		model.arithm(X[47],"<",X[48]).post();
		model.arithm(X[47],"<",X[49]).post();
		model.arithm(X[48],"<",X[50]).post();
		model.arithm(X[49],"<",X[50]).post();
		model.arithm(X[50],"<",X[51]).post();
		model.arithm(X[51],"<",X[52]).post();
		model.arithm(X[52],"<",X[53]).post();
		model.arithm(X[57],"<",X[56]).post();
		model.arithm(X[58],"<",X[57]).post();
		model.arithm(X[59],"<",X[58]).post();
		model.arithm(X[60],"<",X[59]).post();
		model.arithm(X[61],"<",X[60]).post();
		model.arithm(X[62],"<",X[61]).post();
		model.arithm(X[70],"<",X[31]).post();
		model.arithm(X[71],"<",X[31]).post();
		model.arithm(X[72],"<",X[31]).post();
		model.arithm(X[73],"<",X[31]).post();
		model.arithm(X[74],"<",X[31]).post();
		model.arithm(X[75],"<",X[31]).post();
		model.arithm(X[76],"<",X[31]).post();
		model.arithm(X[77],"<",X[31]).post();
		model.arithm(X[0],">",X[79]).post();
		model.arithm(X[1],">",X[80]).post();
		model.arithm(X[2],">",X[3]).post();
		model.arithm(X[2],">",X[81]).post();
		model.arithm(X[3],">",X[81]).post();
//		model.arithm(X[4],">",X[5]).post();
		model.arithm(X[5],">",X[4]).post();
		model.arithm(X[6],">",X[5]).post();
		model.arithm(X[7],">",X[6]).post();
		model.arithm(X[8],">",X[7]).post();
		model.arithm(X[17],">",X[16]).post();
		model.arithm(X[18],">",X[19]).post();
		model.arithm(X[18],">",X[16]).post();
		model.arithm(X[18],">",X[17]).post();
		model.arithm(X[22],">",X[84]).post();
		model.arithm(X[22],">",X[85]).post();
		model.arithm(X[23],">",X[22]).post();
		model.arithm(X[24],">",X[23]).post();
		model.arithm(X[24],">",X[25]).post();
		model.arithm(X[26],">",X[25]).post();
		model.arithm(X[27],">",X[26]).post();
		model.arithm(X[28],">",X[27]).post();
		model.arithm(X[29],">",X[28]).post();
		model.arithm(X[29],">",X[25]).post();
		model.arithm(X[31],">",X[29]).post();
		model.arithm(X[32],">",X[31]).post();
		model.arithm(X[33],">",X[32]).post();
		model.arithm(X[34],">",X[36]).post();
		model.arithm(X[35],">",X[36]).post();
		model.arithm(X[36],">",X[31]).post();
		model.arithm(X[37],">",X[31]).post();
		model.arithm(X[37],">",X[24]).post();
		model.arithm(X[38],">",X[24]).post();
		model.arithm(X[38],">",X[23]).post();
		model.arithm(X[39],">",X[0]).post();
		model.arithm(X[41],">",X[40]).post();
		model.arithm(X[42],">",X[41]).post();
		model.arithm(X[43],">",X[42]).post();
		model.arithm(X[44],">",X[43]).post();
		model.arithm(X[45],">",X[44]).post();
		model.arithm(X[46],">",X[89]).post();
		model.arithm(X[47],">",X[90]).post();
		model.arithm(X[49],">",X[47]).post();
		model.arithm(X[50],">",X[49]).post();
		model.arithm(X[51],">",X[50]).post();
		model.arithm(X[52],">",X[51]).post();
		model.arithm(X[53],">",X[52]).post();
		model.arithm(X[54],">",X[55]).post();
		model.arithm(X[55],">",X[56]).post();
		model.arithm(X[56],">",X[57]).post();
		model.arithm(X[57],">",X[58]).post();
		model.arithm(X[58],">",X[59]).post();
		model.arithm(X[59],">",X[60]).post();
		model.arithm(X[60],">",X[61]).post();
		model.arithm(X[61],">",X[62]).post();
		model.arithm(X[63],">",X[62]).post();
		model.arithm(X[64],">",X[68]).post();
		model.arithm(X[65],">",X[27]).post();
		model.arithm(X[66],">",X[27]).post();
		model.arithm(X[67],">",X[27]).post();
		model.arithm(X[67],">",X[25]).post();
		model.arithm(X[68],">",X[25]).post();
		model.arithm(X[69],">",X[25]).post();
		model.arithm(X[78],">",X[34]).post();
	
		if (shape) {

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
		
		};
		
		// solve problem
		Solver solver = model.getSolver();
		
		solver.propagate();
		
		System.out.println("solving");
		
		solver.setSearch(
				Search.intVarSearch(new Smallest(), new IntDomainMin(), ranks)
//				,
//				Search.intVarSearch(new Smallest(), new IntDomainMin(), sizes),
//				Search.intVarSearch(new Smallest(), new IntDomainMin(), bounds)
				);						
		solver.showStatistics();
		//solver.showStatisticsDuringResolution(1000);
		//solver.showDecisions();
		//solver.showContradiction();
		//solver.showSolutions();
		//solver.limitTime(60 + "s");
		//solver.setLDS(100);


		
		Solution solution = model.getSolver().findSolution();
		if(solution != null){
		    System.out.println(solution.toString());
		}
		for(int i=0; i<n; i++) {
//			System.out.println(names[i]+" x="+solution.getIntVal(X[i])+";y="+solution.getIntVal(Y[i])+"  L="+solution.getIntVal(W[i])+";H="+solution.getIntVal(H[i]));
			System.out.println(names[i]+" x="+solution.getIntVal(X[i])+";y="+solution.getIntVal(Y[i]));
		}

	}
}

// NSEO v0 : Solution: x0=1, y0=2, x1=1, y1=3, x2=1, y2=4, x3=2, y3=1, x4=1, y4=1, x5=2, y5=1, x6=1, y6=5, x7=2, y7=1, x8=2, y8=1, x9=1, y9=1, 
/* 92 lieux : 184v 249c 185n 0b
Solution: x0=2, y0=1, x1=2, y1=2, x2=3, y2=3, x3=2, y3=4, x4=1, y4=1, x5=2, y5=1, x6=3, y6=1, x7=4, y7=1, x8=5, y8=1, x9=1, y9=1, x10=1, y10=2, x11=1, y11=3, x12=1, y12=4, x13=1, y13=5, x14=1, y14=6, x15=1, y15=7, x16=2, y16=8, x17=3, y17=1, x18=4, y18=2, x19=1, y19=9, x20=1, y20=10, x21=1, y21=11, x22=2, y22=3, x23=3, y23=3, x24=4, y24=11, x25=1, y25=2, x26=2, y26=1, x27=3, y27=1, x28=4, y28=1, x29=5, y29=1, x30=6, y30=1, x31=6, y31=1, x32=7, y32=1, x33=8, y33=1, x34=8, y34=2, x35=8, y35=3, x36=7, y36=4, x37=7, y37=5, x38=5, y38=12, x39=3, y39=1, x40=1, y40=2, x41=2, y41=2, x42=3, y42=2, x43=4, y43=2, x44=5, y44=2, x45=6, y45=2, x46=6, y46=3, x47=2, y47=2, x48=3, y48=3, x49=3, y49=2, x50=4, y50=2, x51=5, y51=2, x52=6, y52=2, x53=7, y53=2, x54=9, y54=3, x55=8, y55=4, x56=7, y56=5, x57=6, y57=3, x58=5, y58=4, x59=4, y59=4, x60=3, y60=4, x61=2, y61=2, x62=1, y62=3, x63=2, y63=2, x64=3, y64=1, x65=4, y65=2, x66=4, y66=3, x67=4, y67=4, x68=2, y68=5, x69=2, y69=6, x70=1, y70=10, x71=1, y71=9, x72=1, y72=8, x73=1, y73=7, x74=1, y74=6, x75=1, y75=5, x76=1, y76=4, x77=1, y77=3, x78=9, y78=1, x79=1, y79=1, x80=1, y80=1, x81=1, y81=1, x82=2, y82=11, x83=1, y83=12, x84=1, y84=1, x85=1, y85=1, x86=1, y86=4, x87=1, y87=4, x88=1, y88=1, x89=1, y89=1, x90=1, y90=1, x91=1, y91=2, 
BD_219 x=2;y=1
BD_220 x=2;y=2
BD_221 x=3;y=3
BD_222 x=2;y=4
BD_223 x=1;y=1
BD_224 x=2;y=1
BD_225 x=3;y=1
BD_226 x=4;y=1
BD_227 x=5;y=1
BD_228 x=1;y=1
BD_229 x=1;y=2
BD_230 x=1;y=3
BD_231 x=1;y=4
BD_232 x=1;y=5
BD_233 x=1;y=6
BD_234 x=1;y=7
BD_235 x=2;y=8
BD_236 x=3;y=1
BD_237 x=4;y=2
BD_238 x=1;y=9
BD_239 x=1;y=10
BD_240 x=1;y=11
BD_241 x=2;y=3
BD_242 x=3;y=3
BD_243 x=4;y=11
BD_244 x=1;y=2
BD_245 x=2;y=1
BD_246 x=3;y=1
BD_247 x=4;y=1
BD_248 x=5;y=1
BD_249 x=6;y=1
BD_250 x=6;y=1
BD_251 x=7;y=1
BD_252 x=8;y=1
BD_253 x=8;y=2
BD_254 x=8;y=3
BD_255 x=7;y=4
BD_256 x=7;y=5
BD_257 x=5;y=12
BD_456 x=3;y=1
BD_466 x=1;y=2
BD_467 x=2;y=2
BD_468 x=3;y=2
BD_469 x=4;y=2
BD_470 x=5;y=2
BD_471 x=6;y=2
BD_472 x=6;y=3
BD_473 x=2;y=2
BD_474 x=3;y=3
BD_475 x=3;y=2
BD_476 x=4;y=2
BD_477 x=5;y=2
BD_478 x=6;y=2
BD_479 x=7;y=2
BD_480 x=9;y=3
BD_481 x=8;y=4
BD_482 x=7;y=5
BD_483 x=6;y=3
BD_484 x=5;y=4
BD_485 x=4;y=4
BD_486 x=3;y=4
BD_487 x=2;y=2
BD_488 x=1;y=3
BD_489 x=2;y=2
BD_490 x=3;y=1
BD_491 x=4;y=2
BD_492 x=4;y=3
BD_493 x=4;y=4
BD_494 x=2;y=5
BD_495 x=2;y=6
BD_496 x=1;y=10
BD_497 x=1;y=9
BD_498 x=1;y=8
BD_499 x=1;y=7
BD_500 x=1;y=6
BD_501 x=1;y=5
BD_502 x=1;y=4
BD_503 x=1;y=3
BD_504 x=9;y=1
BND_869 x=1;y=1
BND_870 x=1;y=1
BND_871 x=1;y=1
BND_872 x=2;y=11
BND_873 x=1;y=12
BND_874 x=1;y=1
BND_875 x=1;y=1
BND_876 x=1;y=4
BND_877 x=1;y=4
BND_885 x=1;y=1
BND_886 x=1;y=1
BND_887 x=1;y=1
E_160 x=1;y=2

*/
