LoadPackage("orb");;

runtimeSum := function()

	local t;;

	t := Runtimes();;

	return t.user_time + t.user_time_children;;
end;;

#########################     Run SAUCY Fucntion ###################################################################
runSaucy := function(inputFile, sizeOfElements)
	
	local inputg,g,symStr,numOfOrbits,orbs,time1;;
	#Print("Symmetry detection with saucy...");;
	time1 :=runtimeSum();;
	Exec( "./saucy -w ",inputFile," | sed '$s/,$//' > sym.tmp" );;
				
    #Exec( "./saucy -s -w ",inputFile," > temp1.txt" );;
	# read the Saucy-generated file with the group generators
	inputg := InputTextFile("sym.tmp");;
	symStr := ReadAll(inputg);;

	# write the group definition with the generators in a GAP file
	PrintTo("sym.g", "g := Group(", symStr, ");;");;
	# read and interpret this GAP file (so as to load the group)
	# g is then the permutation group
	CloseStream(inputg);;
	if symStr = fail then
		return 0;;
	else
		return 1;;
	fi;;
	
	 

end;;

##########################################################################################################################################




# the correct marginals for evaluation purposes
# this is very problem dependent (here social network MLN)
Read("marginalsfs.g");;

# computes the KL divergence between two sets of marginals
# smoothing constant is 0.000000001
# type=1 -> accumulated symmetric KL divergence
# type=2 -> average KL divergence
smooth := 0.000000001;;
#smooth := 0.0001;;

kullback := function(m1, m2, type, numOfNonEvidenceVariables)
 

       local i, sum1, sum2;;
        
        sum1 := 0.0;;
        sum2 := 0.0;;
        
        for i in [1..Length(m1)] do
 

               m1[i] := Float(m1[i]);;
                m2[i] := Float(m2[i]);;
                
                if m1[i] > 0.0 then
                        if m2[i] > 0.0 then
                                sum1 := sum1 + (m1[i] * Log(m1[i] / m2[i]));;
                        else
                                sum1 := sum1 + (m1[i] * Log(m1[i] / smooth));;
                        fi;;
                fi;;
 

               if m1[i] < 1.0 then
                        if m2[i] < 1.0 then
                                sum1 := sum1 + ((1.0-m1[i]) * Log((1.0-m1[i]) / (1.0-m2[i])));;
                        else
                                sum1 := sum1 + ((1.0-m1[i]) * Log((1.0-m1[i]) / smooth));;
                        fi;;
                fi;;
 

               if m2[i] > 0.0 then
                        if m1[i] > 0.0 then
                                sum2 := sum2 + (m2[i] * Log(m2[i] / m1[i]));;
                        else
                                sum2 := sum2 + (m2[i] * Log(m2[i] / smooth));;
                        fi;;
                fi;;
 

               if m2[i] < 1.0 then
                        if m1[i] < 1.0 then
                                sum2 := sum2 + ((1.0-m2[i]) * Log((1.0-m2[i]) / (1.0-m1[i])));;
                        else
                                sum2 := sum2 + ((1.0-m2[i]) * Log((1.0-m2[i]) / smooth));;
                        fi;;
                fi;;
        od;;
 

       if type = 1 then
                return sum1 + sum2;;
        elif type = 2 then
                return sum1 / Float(Length(m1));;
        elif type = 3 then
                return sum1 / Float(numOfNonEvidenceVariables);;
        else
                return sum2 / Float(Length(m2));;
        fi;;
 end;;

# updates the list of marginal counts based on the current world
updatem := function(m, s)
	
	local i;;

	for i in s do
		m[i] := m[i] + 1;;
	od;;
end;;
# takes a set of deterministic formulas 
# returns true if the current world is a satisfing assignment
# return false otherwise
isValidAssignment := function(D, s)

	local i, ic, f, satisfied, listLength, numOfLiterals;;

	listLength := Length(D);;

	# there are no deterministic formulas --> every assignment has non-zero probability
	if listLength < 1 then
		# Print("length < 1","\n");;
		return true;;
	fi;;

	# iterate over all deterministic formulas
	for f in D do

		satisfied := false;;

		# iterate over all negative literals
		for i in [1..f.lneg] do
			if not \in(f.neg[i], s) then
				satisfied := true;;
				break;;
			fi;;
	    	od;;

		if not satisfied then
			#iterate over all positive literals
			for i in [1..f.lpos] do
				if \in(f.pos[i], s) then
					satisfied := true;;
					break;;
				fi;;
    			od;;
		fi;;

		# if one of the deterministic formulas is violated, return false
		if not satisfied then
			return false;;
		fi;;
	od;;

	return true;;
end;;

# evaluates a feature in the current world
evaluate := function (f, s)
     
	local i;;

	# iterate over all negative literals
    	for i in [1..f.lneg] do
		if not \in(f.neg[i], s) then
			return f.weight;;
		fi;;
    	od;;

	#iterate over all positive literals
	for i in [1..f.lpos] do
		if \in(f.pos[i], s) then
			return f.weight;;
		fi;;
    	od;;

	return 0.0;;
end;;

# computes the sum of weights of satisfied formulas in W by s
evaluateWeight := function (W, s)

	local w, sumOfWeights;;

	sumOfWeights := 0.0;;

	for w in W do
		sumOfWeights := sumOfWeights + evaluate(w, s);;
	od;;

	return sumOfWeights;;
end;;


# function that takes (1) a list  of variables, (2) the deterministic and (3) weighted constraints the variables
# are involved in, and (4) the current state and 
# returns a list with marginal distribution for the variables
marginalDistr := function ( V, D, W, s, rs )

	local i, j, sumOfAllWeights, c, fv, combs, currWorld, weightOfWorld, accProb, prob, blockCombinations,pr;;
	# numerical index for the configurations
	#local i;;
	# the sum of all weights (normalizing constant)
	#local sumOfAllWeights;;
	# iterates over all configurations
	#local c;;
	# variable that stores a random number between 0 and 1
	#local fv;;
	# variable that stores the combinations of block variables for each of their numerical index
	#local combs;;
	# the world that is currently under consideration for ther weight computations
	#local currWorld;;

	#Print("V: ",V, "\n");
	#Print("baseWorld: ",baseWorld, "\n");
	pr :=[];;
	combs := [];;
	weightOfWorld := [];;
	sumOfAllWeights := 0.0;;
	i := 0;;

	blockCombinations := Combinations(V);;
	#Print("blockcombs",blockCombinations,"S is",s,"\n");;
	
	for c in blockCombinations do

		# construct a new world by unioning the base world and the current subset
		# of the block variables
		currWorld := ShallowCopy(s);;		
		for j in c do
			AddSet(currWorld, j);;
		od;;

		# determines whether the current world satisfies the deterministic constraints
		if isValidAssignment(D, currWorld) = true then
			# increment the counter i (numerical index for possible configurations)
			i := i + 1;;

			# store combination under its numerical index (needed later)
			combs[i] := c;;

			# compute the weight (exponentiated) of the current world
			#weightOfWorld[i] := Exp(evaluateWeight(W, currWorld));;
			#Print("Here weight ",V," ",evaluateWeight(W, currWorld)," ",weightOfWorld[i],"\n");;
			# compute the sum of all worlds
			weightOfWorld[i] := (evaluateWeight(W, currWorld));;
			#sumOfAllWeights := sumOfAllWeights + weightOfWorld[i];;
		fi;;
	od;;
	pr[1]:=1.0/(1+Exp(weightOfWorld[2]-weightOfWorld[1]));;
	pr[2]:=1.0/(1+Exp(weightOfWorld[1]-weightOfWorld[2]));;

	# return a uniform sample from [0,1]
	fv := Random(rs, 0, 100000000000) / 100000000000.0;;
	# stores accumulated probability when going through possible worlds
	accProb := 0.0;;

	# iterate over all worlds for which we have given a probability
	for c in [1..i] do
		# compute the conditional probability of the current world c
		prob := pr[c];;#weightOfWorld[c] / sumOfAllWeights;;
		# compute the accumulated probability mass
		accProb := accProb + prob;;
		
		# if we are reaching the accumulated prob mass, we return the world c
		if fv <= accProb then
			
			return combs[c];;
		fi;;
	od;;

	return [];;
end;;

# stores all of the features in a list
flist := [];;

# read the DIMACS weighted CNF input file
readDIMACTime := runtimeSum();;
#Print("Reading the DIMAC weighted CNF input file...\n");;
input := InputTextFile("dimacs.cnf");;
wcnfString := ReadAll(input);;

# break the string into lines of the input file
wncfLines := SplitString(wcnfString, "\n" );;
# print how many lines we have read

#Print("done! processed ", Length(wncfLines), " lines from the input file in ", Float((runtimeSum()-readDIMACTime)/1000.0)," seconds.\n");;

# read number of variables
firstLine := SplitString(wncfLines[1], " ");;
sizeOfElements := Int(firstLine[3]);;

#Print("There are ",sizeOfElements," random variables.\n");;

# the hash table that stores, for each variable, the weighted features it appear in
weightedFeaturesTable := NewDictionary(1, true, Elements([1..sizeOfElements]));;

# the hash table that stores, for each variable, the deterministic features it appear in
deterministicFeaturesTable := NewDictionary(1, true, Elements([1..sizeOfElements]));;

# here we build the hash table with the features
# iterate over all lines that contain a clause (all except the first one in the DIMACS file)
#Print("Building the hash table for the sampler...");;
buildHashTime := runtimeSum();;

# we can also have evidence (this has to be replaced by a more general approach)
positiveEvidence := Set([]);;
negativeEvidence := Set([]);;
deterministicClauses := NewDictionary(1, true, Set([]));;
VariableMappings := NewDictionary("a", true, 1);;
for i in [2..Length(wncfLines)] do

	# split clause string into literals (all except last number which is end marker 0)
	clause := SplitString(wncfLines[i], " ");;

	# ignore comment lines in the dimacs file
	if clause[1] = "c" then
		
		if i<=sizeOfElements+2 and i>2 then
			AddDictionary(VariableMappings,clause[3],Int(clause[2]));;
		fi;;
		continue;;


	fi;;


	# indeictes whether the clause is deterministic or weighted
	isDeterministic := false;;

	# number of literals in the clause + 1
	clauseLength := Length(clause)-1;;

	# stores the number of variables per clause
	numOfVariables := clauseLength-1;;

	# deterministic clauses have a weight of "inf"
	if clause[1] = "inf" then
		wsc := FLOAT.INFINITY;;
		isDeterministic := true;;
		
		for j in [2..clauseLength] do
			lit := Int(clause[j]);;

			if lit < 0 then
				lit := -lit;;
			fi;;
			clauseSet := LookupDictionary(deterministicClauses, lit);;
			unweightClause := clause{[2..clauseLength]};;     
			if clauseSet = fail then # not yet 
				clauseSetNew := [unweightClause];;
				AddDictionary(deterministicClauses, lit, clauseSetNew);;
			else
				if not unweightClause in clauseSet then
					Add(clauseSet, unweightClause);;
					AddDictionary(deterministicClauses, lit, clauseSet);;
				fi;;
			fi;;
		od;;
	else
		# initialize an empty feature with the appropriate weight
		wsc := Float(clause[1]);;
	fi;;
	
	# this is the actual feature
	# we store for each clause the negated (neg), unnegated (pos), number of negated (lneg),
	# number of unnegated (lpos) variables, and the weight of the clause
	feature := rec( pos := [], neg := [], lpos := 0, lneg:= 0, weight := wsc);;

	# stores the variables occuring in this clause
	variables := [];;

	# stores whether we want to add the clause to the set of features
	# we don't want to add it if it is a deterministic cclause of size 1 (=evidence)
	addToFeatureSet := true;;

	# iterate over all literals (1 would be the weight, Length(clause) the end marker 0)
	# and generate the list of variables occurring in the formula under consideration
	for j in [2..clauseLength] do

		lit := Int(clause[j]);;

		if lit < 0 then 
			# add negative literal (negated variable)
			lit := -lit;;
			Add(feature.neg, lit);;
			feature.lneg := feature.lneg + 1;;
			# if it is a single ground atom, add as evidence
			if isDeterministic and numOfVariables = 1 then
				AddSet(negativeEvidence, lit);;
				addToFeatureSet := false;;
			fi;;
		else
			# add positive literal (unnegated variable)
			Add(feature.pos, lit);;
			feature.lpos := feature.lpos + 1;;
			# if it is a single ground atom, add as evidence
			if isDeterministic and numOfVariables = 1 then
				AddSet(positiveEvidence, lit);;
				addToFeatureSet := false;;
			fi;;
		fi;;

		Add(variables, lit);;
	od;;
	
	if not addToFeatureSet then
		continue;
	fi;;

	# now store the clause under the variables in the hash table
	for lit in variables do
	
		if isDeterministic = true then

			# retrieve list of weighted features this variable occurs in
			featSet := LookupDictionary(deterministicFeaturesTable, lit);;
			if featSet = fail then # not yet 
				featSetNew := [feature];;
				AddDictionary(deterministicFeaturesTable, lit, featSetNew);;
			else
				if not feature in featSet then
					Add(featSet, feature);;
					AddDictionary(deterministicFeaturesTable, lit, featSet);;
				fi;;
			fi;;
		else
			# retrieve list of weighted features this variable occurs in
			featSet := LookupDictionary(weightedFeaturesTable, lit);;
			if featSet = fail then # not yet 
				featSetNew := [feature];;
				AddDictionary(weightedFeaturesTable, lit, featSetNew);;
			else
				if not feature in featSet then				
					Add(featSet, feature);;
					AddDictionary(weightedFeaturesTable, lit, featSet);;
				fi;;
			fi;;
		fi;;
	od;;


od;;

buildHashTime := Float((runtimeSum()-buildHashTime)/1000.0);;
Print("done! (", buildHashTime, " seconds) \n");;

readSymTime := runtimeSum();;
symStr := runSaucy("dimacs.cnf.saucy", sizeOfElements);;
if symStr = 0 then
	Print("No Generators found...");;
	#orbs:=[[1],[2],[3],[4],[5],[6],[7],[8],[9]];;
	no_original_sym :=1;;
else
	Read("sym.g");;
	no_original_sym :=0;;
	gOriginal :=g;;
	Print("original_symmetry found",Size(GeneratorsOfGroup(g)),"\n");;
	
	

	#orbs := OrbitsDomain(g, [1..sizeOfElements]);;
fi;;

#Print(g);;
#Print("Group size: ",Size(GeneratorsOfGroup(g)), "\n");;
#numOfOrbits := Length(orbs);;
#Print(orbs,"\n");;
#Print("done! (", Float((runtimeSum()-readSymTime)/1000.0), " seconds) \n");;
#Print("Number of variable orbits: ",numOfOrbits,"\n");;


# for v in [1..Length(VariableMappings)] do
# 	for truth_vals in [1..2] do
# 		if truth_vals=2 then
# 			v := -v;
# 		fi;;
# 		context_vars_list := [];;

####################################### Generating contextual Dimac.cnf.saucy ##############################
rs1 := RandomSource(IsMersenneTwister);;

context_vars_list :=[];;
context_vars_list[1] := 632;;
#context_vars_list[2] :=sizeOfElements-1;;
contextSize := 1;; 

curr_count :=1;;


input_dimacs_file := InputTextFile("dimacs.cnf");;
dimacs_string := ReadAll(input_dimacs_file);;
dimacs_lines := SplitString(dimacs_string,"\n");;
inf_clause_list := [];;

evid_found :=false;;
for i in [1..Length(dimacs_lines)] do 
	curr_clause := SplitString(dimacs_lines[i]," ");;
	if curr_clause[1]="inf" then
		inf_clause_list := curr_clause{[2..Length(curr_clause)]};;
		evid_found := true;;
		
		break;;
	fi;;
od;;
 
inf_color_set := false;;
	
#Print("Size orig",Size(GeneratorsOfGroup(gOriginal)),"\n");;

sym_found := false;;
gContext :=[];;
symmetry :=[];;
prplContext :=[];;
for contextPart in [1..2^(contextSize)] do #maxContextSize] do
	gContext[contextPart] :=[];;
	temp_context_vars_list := [];;
	for contextIndex in [1..Length(context_vars_list)] do
		curr_var_context :=context_vars_list[contextIndex];;
		var_value :=Int(((contextPart-1) mod (2 ^(contextIndex))) /(2^ (contextIndex-1)));;
		
		if var_value=0 then
			temp_context_vars_list:=Concatenation(temp_context_vars_list,[-1*curr_var_context]);;
		else
			temp_context_vars_list:=Concatenation(temp_context_vars_list,[curr_var_context]);;
		fi;;
	od;;
	#Print(temp_context_vars_list);;
	input_saucy := InputTextFile("dimacs.cnf.saucy");;
	temp :=OutputTextFile("temp",false);;
	Saucy_String := ReadAll(input_saucy);;
	saucy_lines := SplitString(Saucy_String,"\n");;
	AppendTo(temp, saucy_lines[1],"\n");;
	old_original_color := 1;;
	color := 1;;
	clauses :=0;;
	flag := false;;
	for i in [2..Length(saucy_lines)]do
		flag := false;;
		curr_clause :=SplitString(saucy_lines[i]," ");;
		omit_line :=false;;
		curr_list :=[];;
		curr_list :=curr_clause{[2..Length(curr_clause)]};;
		if evid_found=true then
			if curr_list=inf_clause_list then
				inf_color_set := true;;
			fi;;
		fi;;
		curr_list_preds := [];;
		for j in [2..Length(curr_clause)-1]do
			l:=Length(curr_clause[j]);;
			check_var := -1*Int(curr_clause[j]);;
			if Length(curr_clause)=3 then						
				if check_var in temp_context_vars_list then
					omit_line := true;;
					break;;
				fi;;
			fi;;
			if Int(curr_clause[j]) in temp_context_vars_list then
				omit_line := true;;
				break;;
			fi;;
			if not check_var in temp_context_vars_list then
				curr_list_preds := Concatenation(curr_list_preds," ",String(-1*check_var));;	
			fi;;
		od;;
		if omit_line=false then
			if old_original_color <> Int(curr_clause[1]) then
				color := color + 1;;						
				old_original_color := Int(curr_clause[1]);;
			fi;;
			new_clause := Concatenation(String(color),curr_list_preds," 0");;
			AppendTo(temp,new_clause,"\n");
			clauses := clauses + 1;;
		fi;;
	od;;
	if evid_found=false then
		for x in [1..Length(temp_context_vars_list)] do
			new_clause := Concatenation(String(color+1)," ",String(temp_context_vars_list[x])," 0\n");;
			AppendTo(temp,new_clause,"\n");;
			clauses := clauses + 1;;
		od;
	fi;;


			#CloseStream(contextual_saucy);;
	CloseStream(input_saucy);;
	num_clauses :=clauses;;
	input_saucy1 := InputTextFile("temp");;
	contextual_saucy1 :=OutputTextFile("dimacs_context.cnf.saucy",false);;

	Saucy_String := ReadAll(input_saucy1);;
	saucy_lines := SplitString(Saucy_String,"\n");;
	first_line :=SplitString(saucy_lines[1]," ");;
	new_num_vars :=Int(first_line[3]);;#-Length(context_vars_list);; //removeLater
	new_str:=Concatenation(first_line[1]," ",first_line[2]," ",String(new_num_vars)," ",String(clauses),"\n");;
	AppendTo(contextual_saucy1,new_str);;
	for i in [2..Length(saucy_lines)] do
		AppendTo(contextual_saucy1,saucy_lines[i],"\n");;
	od;;
	CloseStream(contextual_saucy1);;
	CloseStream(input_saucy1);;		
		
	symStr := runSaucy("dimacs_context.cnf.saucy", new_num_vars);;
	if symStr = 0 then
		if no_original_sym=0 then
			g:= gOriginal;;
			symmetry[contextPart]:=1;;
		else
			symmetry[contextPart]:=0;;
		fi;;	

	else
		Read("sym.g");;
		symmetry[contextPart]:=1;;
		sym_found :=true;;
	fi;;
	if symmetry[contextPart]=1 then
		Print("original_symmetry found",contextPart," Generators",Size(GeneratorsOfGroup(g)),"\n");;
		gContext[contextPart] :=g;;	
		prplContext[contextPart] := ProductReplacer(gContext[contextPart]);;
	fi;;
	g := [];;
	
od;;

symDiscoveryTime := (runtimeSum()-readSymTime)/1000.0;;

# #  this is where the actual sampling process starts # #
# initial sample (the empty set)
s := Set([]);;

# sample a random initial configuration
#numOfElemsInS := Random(rs1, 0, sizeOfElements);;
#for i in [1..numOfElemsInS] do
#	Add(s, Random(rs1, Difference([1..sizeOfElements], s)));;
#od;;

# load an initial precomputed state (e.g., by using maxwalksat).
# Important for comparison to MCSAT
#Read("start_state.g");
#s := start_state;;
for order in [1..sizeOfElements] do	
	f2 := Random(rs1, 0, 100000000000) / 100000000000.0;;
	if f2 < 0.5 then
		
		AddSet(s,order );;
		
	fi;;
od;;
# the evidence is always part of the samples
s := UnionSet(s, positiveEvidence);;
SubtractSet(s, negativeEvidence);;

# build the set of variables to sample from (substract evidence variables)
variablesToSampleFrom := Set([1..sizeOfElements]);;
SubtractSet(variablesToSampleFrom, positiveEvidence);;
SubtractSet(variablesToSampleFrom, negativeEvidence);;

# stores the number of non-evidence variables
numOfNonEvidenceVariables := Length(variablesToSampleFrom);;

# the list storing the marginals


# the number of samples we want to draw




#Print("Running the orbital Markov chain for ", numSamples, " iterations... please be patient...\n");;



### start to load the block structure of the blocked  sampler ###
blocks := [];;
for i in variablesToSampleFrom do
	Add(blocks, [i]);;
od;;

# if the blocks have been computed beforehand, load them here
#Read("blocks.g");
#Print(blocks,"\n");;

# number of blocks
numOfBlocks := Length(blocks);;

# dictionary assigning integer id of blocks to block
blockTable := NewDictionary(1, true, Elements([1..numOfBlocks]));;

# add the blocks to the hash table
for i in [1..numOfBlocks] do
	AddDictionary(blockTable, i, blocks[i]);;
od;;

###  end loading the blocks for the blocked Gibbs sampling algorithm ####

#ProfileGlobalFunctions( true );
#ProfileOperationsAndMethods( true );

# counts the actual number of samples (discarding burn-in etc.)

Print("\nSym Discovery ", symDiscoveryTime,"\n");;

# we now iterate over the number of samples we want to generate
#for counter in [1..numSamples] do
timeArr := [];;
klArr :=[];;
kl_vals := [];;
maxIterations := 20;;
numSamples := 3000000;;
interval := 10000;;
sizeArr := numSamples/interval;
for iter in [1..sizeArr] do
	timeArr[iter] := 0;;
	klArr[iter] := 0;;
od;;
out_file :=OutputTextFile(Concatenation("CON-MCMC_",String(sizeOfElements),".csv"),false);;
for iter in [0..maxIterations-1] do
	s := Set([]);;
	for order in [1..sizeOfElements] do	
		f2 := Random(rs1, 0, 100000000000) / 100000000000.0;;
		if f2 < 0.5 then
		
			AddSet(s,order );;
		
		fi;;
	od;;
# the evidence is always part of the samples
	s := UnionSet(s, positiveEvidence);;
	SubtractSet(s, negativeEvidence);;
	sampleCounter := 0;;
	sampleTimer := runtimeSum();;
	marginals := [];;
	for i in [1..sizeOfElements] do
		Add(marginals, 0);;
	od;;

	
	while sampleCounter < numSamples do
		
		
		# sample one of the block IDs uniformly at random
		bNr := Random(rs1, 1, numOfBlocks);;
		# bNr := Random(rs1, 1, numOfBlocks-contextSize);;
		# pBias := 0.2;;
		# Print("Num of Blocks",numOfBlocks);;
		# pVal := Random(rs1, 0, 100000000000) / 100000000000.0;;
		# if pVal < pBias then
		# 	bNr := contextSize;;
		# fi;;

		# based on the block ID get the correpsonding block
		b := LookupDictionary(blockTable, bNr);;

		dfList := [];;
		wfList := [];;

		# distinguish blocksize = 1 and blocksize > 1 -> more efficient
		blockSize := Length(b);;

		if blockSize > 1 then

			# get the set of features that contain variables in block b (from the hash table)
			for v in b do

				tmpL := LookupDictionary(deterministicFeaturesTable, v);;
				if tmpL <> fail then
					UniteSet(dfList, tmpL);;
				fi;;

				tmpL := LookupDictionary(weightedFeaturesTable, v);;
				if tmpL <> fail then
					UniteSet(wfList, tmpL);;
				fi;;

				# remove the variables in the block from the current state
				RemoveSet(s, v);;
			od;;
		else
			tmpL := LookupDictionary(deterministicFeaturesTable, b[1]);;
			if tmpL <> fail then
				dfList := tmpL;;
			fi;;

			tmpL := LookupDictionary(weightedFeaturesTable, b[1]);;
			if tmpL <> fail then
				wfList := tmpL;;
			fi;;
			
			# remove the variable in the block from the current state
			RemoveSet(s, b[1]);;
		fi;;

		# compute the next gibbs sampling step
		toAdd := marginalDistr(b, dfList, wfList, s, rs1);;

		# add the flipped variables to the current set
		for c in toAdd do
			#s := UnionSet(s, toAdd);;
			AddSet(s, c);;
		od;;

		# sample uniformly at random from the orbit of s (= orbital Markov chain)
		
		#Print("s:", s, "\n");

		# TODO: currently assuming context_vars_list contains only one 
		context_valid := false;;
		if sym_found=true then
			contextNumber := 0;
			for i in [1..Length(context_vars_list)] do
				contextNumber :=contextNumber*2;;
				if context_vars_list[i] in s then
					contextNumber :=contextNumber+1;;
				fi;;
				
			od;;
			#Print(s,contextNumber);;
			if symmetry[contextNumber+1] =1 then
				s := OnSets(s, Next(prplContext[contextNumber+1]));;
			fi;;
		fi;;

		# for i in [1..Length(context_vars_list)] do
		# 	if context_vars_list[i] > 0 then
		# 		if context_vars_list[i] in s then
		# 			context_valid := true;;
		# 		fi;;
		# 	 else 
		# 	 	neg_context_var := -1*context_vars_list[i];;
			 	
		# 	 	if not neg_context_var in s then
		# 			context_valid := true;;
					
		# 	 	fi;;
		# 	fi;;
		# od;;
	

		# #context_valid := false;;
		# if context_valid = false then	
		# 	s := OnSets(s, Next(prplOriginal));;
			
		# else
		# 	olds := s;;
		# 	s := OnSets(s, Next(prplContext));;
			
			
		# fi;;	

		# update the counts of the variables
		updatem(marginals, s);;


		# increment the sample counter
		
		sampleCounter := sampleCounter + 1;;
		# print something every 10000 samples
		if RemInt(sampleCounter, interval) = 0 then
			# evaluation -- can be removed in general
			tmpMargs := [];;
			for i in [1..sizeOfElements] do
				tmpMargs[i] := Float(marginals[i]) / Float(sampleCounter);;
			od;;

			kl := kullback(tmpMargs,correctMargs, 3, numOfNonEvidenceVariables);;
			#Print((runtimeSum()-sampleTimer)/1000.0+buildHashTime, " ", kl, " ", sampleCounter, "\n");;
			timeArr[sampleCounter/interval] := timeArr[sampleCounter/interval] + (runtimeSum()-sampleTimer)/1000.0 + buildHashTime ;;
			klArr[sampleCounter/interval] := klArr[sampleCounter/interval] + kl ;;
			kl_vals[iter*sizeArr+sampleCounter/interval] := kl;;
			#Print("Values",iter*sizeArr+sampleCounter/interval,"\n");;


			#if kl < 0.0001 then 
			#	break;;
			#fi;;
			
			if iter = maxIterations-1 then
				timeArr[sampleCounter/interval] := timeArr[sampleCounter/interval]/maxIterations;;
				klArr[sampleCounter/interval] := klArr[sampleCounter/interval]/maxIterations;;
				#Print(timeArr[sampleCounter/interval],",",klArr[sampleCounter/interval],",",sampleCounter,"\n");;
				variance := 0;;

				for x in [0..maxIterations-1] do
					
					variance :=  variance +(kl_vals[x*sizeArr+sampleCounter/interval]-klArr[sampleCounter/interval])*(kl_vals[x*sizeArr+sampleCounter/interval]-klArr[sampleCounter/interval]);;
				od;;
				standard_error := 2*Sqrt(variance)/maxIterations;;
				Total_time := timeArr[sampleCounter/interval]+symDiscoveryTime;;
				
				AppendTo(out_file,String(Total_time),",",String(klArr[sampleCounter/interval]),",",String(sampleCounter),",",standard_error,"\n");;
				Print(Total_time,",",klArr[sampleCounter/interval],",",sampleCounter,"\n");;
				
			fi;;
		fi;;

	od;;
od;;
CloseStream(out_file);;

	# for i in [1..sizeOfElements] do
	# 	Print("Temp Marginals",tmpMargs[i],"\n");;
	# od;;


	# evaluation -- can be removed in general
	#for i in [1..sizeOfElements] do
	#	marginals[i] := Float(marginals[i]) / Float(sampleCounter);;
	#od;;
	#Print(marginals,"\n");;

	#PrintTo("marginals_hard.g", "correctMargs := ");;
	#AppendTo("marginals_hard.g", marginals);;
	#AppendTo("marginals_hard.g", ";;");;

	#ProfileGlobalFunctions( false );
	#ProfileOperationsAndMethods( false );
	#DisplayProfile();:Q!

