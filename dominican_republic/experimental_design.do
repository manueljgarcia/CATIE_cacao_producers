*EXPERIMENTAL DESIGN FOR CHOICE EXPERIMENT*
*Author: Manuel Garcia*

/*Generic Choice design for first experiment: only intrisic attributes   

Design factors and levels 

Factor   Attribute                   Number of levels   Levels 
X1        Practicas                         5           Poda fitosanitaria, Desmonte mazorcas, Poda arboles, Funigicidas/insecticidas, cacao clonal
X2        Asistencia tec                    3          	1.2.3                      
X3        Suministro                        3           1.2.3                        
X4        Creditos							3           1.2.3                      
X5        Variedades						3           1.2.3                       
X6        Herramientas						3           1.2.3                      
X7        Subsidios							3           1.2.3                      

*get commnad */
clear 
ssc install dcreate


/* Step 1: Generate full factorial design matrix */

 matrix levmat = 5,3,3,3,3,3,3
 genfact, levels(levmat)
 list, separator(4)
 
 /*Step 2: Change variable names and recode levels*/ 
 
 rename x1 Practicas
 rename x2 Asistencia
 rename x3 Suministro
 rename x4 Creditos
 rename x5 Variedades
 rename x6 Herramientas
 rename x7 Subsidios
 
 recode Practicas 	(1=1) (2=2) (3=3) (4=4) (5=5)
 recode Asistencia 	(1=1) (2=2) (3=3)
 recode Suministro 	(1=1) (2=2) (3=3)
 recode Creditos	(1=1) (2=2) (3=3)
 recode Variedades 	(1=1) (2=2) (3=3)
 recode Herramientas (1=1) (2=2) (3=3)
 recode Subsidios 	(1=1) (2=2) (3=3)
 
	
 
  /*Step 3: Run decreate*/
  
matrix b = 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 /*vector of coefficients; you can assumme zero if do not know*/
matrix optout = 1, 1, 1, 1, 1, 1, 1 /*usually defined at the base levels*/
matrix V = I(17)
dcreate i.Practicas i.Asistencia i.Suministro i.Creditos i.Variedades i.Herramientas i.Subsidios, nalt(5) nset(24) fixedalt(optout) asc(1) bmat(b) seed(123) vmat(V)
*dcreate i.Region i.Cost i.Timeframe i.Aid, nalt(2) nset(24) fixedalt(optout) asc(1) bmat(b) seed(123) 
list, separator(4) abbreviate(16)

/*Evaluate the efficiency of the generated design for the model specification */

matrix b = 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
matrix V = I(16)
evaldes i.Practicas i.Asistencia i.Suministro i.Creditos i.Variedades i.Herramientas i.Subsidios, bmat(b) 


/*Options for dcreate

    nalt(#) is required; it specifies the number of alternatives in the design.
    nset(#) is required; it specifies the number of choice sets in the design.
    bmat(name) is required; it specifies a matrix of coefficient priors used to evaluate the D-efficiency of the design.
    asc(numlist) include alternative-specific constants (ASCs) for the alternatives in numlist.  Coefficients for the ASCs must be included as the final elements of bmat.
    fixedalt(name) specifies a matrix with attribute levels for an additional alternative in the design. The attribute levels for this alternative are held constant at the specified levels in all of the generated choice sets. This
        option can for example be used to include an "opt-out" (or "no purchase") alternative in the design (see Example 3 below).
   seed(#) */

   
    /*Step 3: Create blocks*/
 
 blockdes block, nblock(3)
 list 
