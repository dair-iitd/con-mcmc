import sys, re, math, copy, operator, random
from copy import deepcopy
global_count=1
global_count_formulas=0
dict_domain_vars={}
dict_vars_no_evid={}
predicates={}
formulas={}
var_mappings={}
rev_var_mappings={}
var_mappings_updated={}
det_clauses=[]
formula_vars={}
ground_formula_dict={}
var_list=[]
SingletonPredicates={}
from datetime import datetime
random.seed(datetime.now())

############################  Formula Grounding Generator ##################################################################

def generate_ground_formulas_rec(curr_formula,original_list_vars,curr_list_vars,counter):
	global global_count,var_mappings,dict_domain_vars,predicates,formulas,var_mappings,det_clauses,formula_vars,global_count_formulas
	if counter==len(curr_list_vars):
		temp_formula=curr_formula
		for i in range(len(curr_list_vars)):
			#temp_formula=temp_formula.replace(original_list_vars[i]+")",curr_list_vars[i]+")")
			#temp_formula=temp_formula.replace(","+original_list_vars[i]+",",","+curr_list_vars[i]+",")
			temp_formula=temp_formula.replace(original_list_vars[i],curr_list_vars[i])
		#temp_formula=temp_formula.translate(None,' ')
		global_count_formulas=global_count_formulas+1
		ground_formula_dict[temp_formula]=curr_formula
		#print curr_formula,"for",original_list_vars,"curr",curr_list_vars,temp_formula
	

	else:
		var_dict=formula_vars[curr_formula]
		domain_var=var_dict[original_list_vars[counter]]
		temp_list=dict_domain_vars[domain_var]
		for i in range(len(temp_list)):
			curr_list_vars[counter]=temp_list[i]
			generate_ground_formulas_rec(curr_formula,original_list_vars,curr_list_vars,counter+1)
			#print curr_formula,original_list_vars,"curr",curr_list_vars
	return		



################################      Variable Grounding Generator #########################################################

def generate_ground_vars_rec(base_var,var_types):
	global global_count,var_mappings,dict_domain_vars,predicates,formulas,var_mappings,det_clauses,rev_var_mappings
	if len(var_types)==0:
		temp_var=base_var+")"
		temp_var=temp_var.translate(None,' ')
		var_mappings[temp_var]=global_count
		rev_var_mappings[global_count]=temp_var
		global_count=global_count+1
		return 
	list2=dict_domain_vars[var_types[0]]
	for i in range(len(list2)):
		
		temp_var=base_var+list2[i]
		if len(var_types)!=1:
			temp_var=temp_var+","
		generate_ground_vars_rec(temp_var,var_types[1:len(var_types)])
	return	


##################################   The core function ######################################################################

def generate_groundings():
	
	
	global global_count,var_mappings,dict_domain_vars,predicates,formulas,var_mappings,det_clauses,global_count_formulas,formula_vars, rev_var_mappings, dict_vars_no_evid
	infile = open(sys.argv[1],'r')
	lines = infile.readlines()
	#print "hello"
	if lines[0]!="// domain declarations\n":
		print "Error in format"
	else:
		i=1
############################      Parsing Input File    #######################################################################
		
		while(1):
			if lines[i]=="// predicate declarations\n":
				break
			if lines[i]=="\n":
				i=i+1
				continue
			temp_str=re.split('=',lines[i])
			##print temp_str[1]
			list1=temp_str[1].translate(None,'{}\n')
			list2= list1.split(',')
			temp_str[0]=temp_str[0].translate(None,' ')
			##print list2
			i=i+1			
			dict_domain_vars[temp_str[0]]=list2
			#print list2
		##print dict_domain_vars
		i=i+1
		
		while(1):
			if lines[i]=="// formulas\n":
				break
			if lines[i]=="\n":
				i=i+1
				continue
			temp_str=lines[i].split('(')
			if len(temp_str)==1:
				temp_str[0]=temp_str[0].translate(None,'\n')
				SingletonPredicates[temp_str[0]]=1
				i=i+1
				continue	
			list1=temp_str[1].translate(None,')\n ')
			list2=list1.split(',')
			list
			predicates[temp_str[0]]=list2
			i=i+1
		##print predicates
		i=i+1
		
		while(1):
			if i>=len(lines):
				break
			if lines[i]=="\n":
				i=i+1
				continue
			temp_str=lines[i]
			temp_str=temp_str.strip('\n')
			if temp_str[len(temp_str)-1]=='.':
				temp_str=temp_str.strip('.')
				temp_str=temp_str.replace(" ","")
				temp_str=temp_str.strip('\n')
				det_clauses.append(temp_str)
				##print "groundings", temp_str
			else:
				weighted_clause=temp_str.split();
				weight=weighted_clause[0]
				rule=' '.join(weighted_clause[1:len(weighted_clause)])
				formulas[rule]=weight
				#print rule
			i=i+1
		num_vars=0
		i=0

################################### Generate Variable Mappings ##########################################################################		
		var_mappings={}
		for key in predicates:
			var_types=predicates[key]
			temp_count=1
			base_var=key
			base_var=base_var+"("
			temp_var=base_var
			ground_vars=[]
			##print base_var
			for j in range(len(var_types)):
				temp_count=temp_count*len(dict_domain_vars[var_types[j]])
			num_vars=num_vars+temp_count
			generate_ground_vars_rec(base_var,var_types)
		#for key in var_mappings:
		#	print(key, "invars", var_mappings[key],"\n")
		##print num_vars
		for key in SingletonPredicates:
			var_mappings[key]=global_count
			rev_var_mappings[global_count]=key
			num_vars=num_vars+1
			global_count=global_count+1

################################## Generate Ground Clauses #############################################################################
		# for key in var_mappings:
		# 	print key,'in', var_mappings[key]
		
		for key in formulas:
			curr_formula=key
			#preds_curr_formula=re.split("\(|\)",curr_formula)
			preds_curr_formula=curr_formula.split()
			##print preds_curr_formula
		 	
			temp_dict={}
			for j in xrange(0,len(preds_curr_formula),2):
				present_clause=preds_curr_formula[j]
				##print present_clause
				#if j>0:
				#	present_clause=present_clause[1:len(present_clause)]
				present_clause=present_clause.split('(')
				#print present_clause
				var_type=present_clause[0]
				if var_type[0]=='!':
					var_type=var_type[1:len(var_type)]
				if(len(present_clause)==1):
					continue;	
				vars_current_pred=present_clause[1]
				vars_current_pred=vars_current_pred.strip(')')
				vars_current_pred=vars_current_pred.split(',')
				#print var_type
				temp_list=predicates[var_type]
				##print temp_list
				for k in range(len(vars_current_pred)):
					temp_dict[vars_current_pred[k]]=temp_list[k]
			curr_list_vars=[None]*len(temp_dict)
			k=0
			for key in temp_dict:
				curr_list_vars[k]=key
				k=k+1
				

			formula_vars[curr_formula]=temp_dict
			original_list_vars=copy.deepcopy(curr_list_vars)
			#print "here1",original_list_vars,curr_formula
			generate_ground_formulas_rec(curr_formula,original_list_vars,curr_list_vars,0)
	
		##print global_count_formulas
		######################## Dealing Deterministic Variables ####################################################################

		# for j in range(len(det_clauses)):
		# 	formulas[det_clauses[j]]=float("inf")
		# 	det_clauses[j]=det_clauses[j].replace(" ","")
		# 	if det_clauses[j][0]=='!':
		# 	 	if det_clauses[j][1:len(det_clauses[j])] in ground_formula_dict.keys():
		# 	 		ground_formula_dict.pop(det_clauses[j][1:len(det_clauses[j])],None)
		# 	 		global_count_formulas=global_count_formulas-1

		# 	else:
		# 	 	if "!"+det_clauses[j] in ground_formula_dict.keys():
		# 	 		ground_formula_dict.pop("!"+det_clauses[j],None)
		# 	 		global_count_formulas=global_count_formulas-1
		# 	if not det_clauses[j] in ground_formula_dict:
		# 		global_count_formulas=global_count_formulas+1
		# 	ground_formula_dict[det_clauses[j]]=det_clauses[j]


			#global_count_formulas=global_count_formulas+1
		#for key in ground_formula_dict:
		 #	print key,'in',ground_formula_dict[key]
		curr_count =1;
		#print det_clauses
		for i in range(num_vars):
			#print rev_var_mappings
			if not (rev_var_mappings[i+1] in det_clauses or "!"+rev_var_mappings[i+1] in det_clauses):
				dict_vars_no_evid[i+1]=curr_count				
				var_mappings_updated[rev_var_mappings[i+1]]=curr_count
				curr_count=curr_count+1
		#for key in dict_vars_no_evid:
		#	print key,"in",dict_vars_no_evid[key]
		num_vars = curr_count-1	
##################################################### Writing Dimacs File############################################################
		#for key in ground_formula_dict:
		#	#print "formulas",key,ground_formula_dict[key]
		out_file1=open("temp_dimacs.cnf","w")
		out_file2=open("temp_dimacs.cnf.saucy","w")
		out_file1.write("p wcnf "+str(num_vars)+" "+str(global_count_formulas)+"\n")
		out_file2.write("p wcnf "+str(num_vars)+" "+str(global_count_formulas)+"\n")
		out_file1.write("c variable mappings:\n")
		sorted_vars = sorted(var_mappings_updated.items(), key=operator.itemgetter(1))
		for j in range(len(sorted_vars)):
			out_file1.write("c "+str(sorted_vars[j][1])+" "+str(sorted_vars[j][0])+"\n")
		out_file1.write("c clauses:\n")
		color_dict={}
		color_formula_dict={}
		color=2		
		omitted_lines=0
		#print key,ground_formula_dict
		for key in ground_formula_dict:
			#print key,ground_formula_dict[key],"\n"
			original_formula=ground_formula_dict[key]
			weight=formulas[original_formula]
			
	
			key_with_spaces=key.replace(")v",") v ")
		
			
			clauses_curr_formula=key_with_spaces.split(' ')
			if str(weight)=='+w':
					##print"Yes I am here"
					weight=str(random.normalvariate(0,0.001))
			if str(weight)=='+W':
					##print"Yes I am here"
					weight=str(0.2+random.normalvariate(0,0.001))
			formula_var_number_form=str(weight)+" "
			formula_color_form=" "
			var_dupl={}
			omit_line=False
			
			omit_variable=False
			for j in range(0,len(clauses_curr_formula),2):
		
				formula_var_number_form=formula_var_number_form
				flag_positive=True
				omit_variable=False
				# Changed part after Evidence and Context has same behavior
				if clauses_curr_formula[j] in det_clauses or ("!"+clauses_curr_formula[j] in det_clauses):
					##print "here I am",clauses_curr_formula[j]

					if len(clauses_curr_formula)>2:
						if omit_line==False:
							omit_line=True
							omitted_lines=omitted_lines+1
				if clauses_curr_formula[j] in det_clauses:
					if omit_line==False:
						omit_line=True
						omitted_lines=omitted_lines+1
				if clauses_curr_formula[j][0]=='!':
					check_clause =clauses_curr_formula[j][1:len(clauses_curr_formula[j])]
				else:
					check_clause="!"+clauses_curr_formula[j]
				if check_clause in det_clauses: 
					omit_variable =True
				#######################################################			
				if clauses_curr_formula[j][0]=='!':
					flag_positive=False
					clauses_curr_formula[j]=clauses_curr_formula[j][1:len(clauses_curr_formula[j])]	
				
				var_number=var_mappings[clauses_curr_formula[j]]
				if flag_positive==False:
					var_number=var_number*-1
					if -1*var_number in var_dupl.keys():
						get_sign=var_dupl[-1*var_number]
						if get_sign==True:
							if omit_line==False:
								omit_line=True
								omitted_lines=omitted_lines+1
								
						else:
							omit_variable=True
					else:
						var_dupl[-1*var_number]=False
				else:
					if var_number in var_dupl.keys():
						get_sign=var_dupl[var_number]
						if get_sign==False:
							if omit_line==False:
								omit_line=True
								omitted_lines=omitted_lines+1
								##print"Omiited3",clauses_curr_formula
						else:
							omit_variable=True
					else:
						var_dupl[var_number]=True
							
						
				
				if omit_variable==False and omit_line==False:
					if var_number < 0:
						updated_var_number=-1*dict_vars_no_evid[-1*var_number]
					else:
						updated_var_number=dict_vars_no_evid[var_number]
					formula_color_form=formula_color_form+str(updated_var_number)+" "
					formula_var_number_form=formula_var_number_form+str(updated_var_number)+" "
			formula_var_number_form=formula_var_number_form+"0\n"
			formula_color_form=formula_color_form+"0\n"
			split_formula_color_form=formula_color_form.split(" ")
			if len(split_formula_color_form)<=2:
				omit_line=True
				omitted_lines=omitted_lines+1
				#Print("omitted_lines
			if omit_line==False:
				##print weight

				if weight in color_dict.keys():
					curr_color=color_dict[weight]
				else:
					color_dict[weight]=color
					curr_color=color
					color=color+1
				formula_color_form=str(curr_color)+formula_color_form
				if formula_color_form in color_formula_dict.keys():
					omitted_lines=omitted_lines+1	
				color_formula_dict[formula_color_form]=curr_color

			# else :
			# 	print("omitted lines",omitted_lines,"\n")
			if omit_line==False:	
				out_file1.write("c "+str(weight)+" "+key_with_spaces+"\n")
				out_file1.write(formula_var_number_form)
		sorted_color_list=sorted(color_formula_dict.items(), key=operator.itemgetter(1))
		for j in range(len(sorted_color_list)):
			out_file2.write(sorted_color_list[j][0])

					
		out_file1.close()
		out_file2.close()

		infile1=open("temp_dimacs.cnf","r")
		out_file1=open("dimacs.cnf","w")
		lines=infile1.readlines()
		out_file1.write("p wcnf "+str(num_vars)+" "+str(global_count_formulas-omitted_lines)+"\n")
		for i in range(1,len(lines)):
			out_file1.write(lines[i])
		infile1.close()
		out_file1.close()


		infile2=open("temp_dimacs.cnf.saucy","r")
		out_file2=open("dimacs.cnf.saucy","w")
		lines=infile2.readlines()
		out_file2.write("p wcnf "+str(num_vars)+" "+str(global_count_formulas-omitted_lines)+"\n")
		for i in range(1,len(lines)):
			out_file2.write(lines[i])
		infile2.close()
		out_file2.close()




	return



	






def main():
	generate_groundings()
	#writeGroundedDimacs()
	return

if __name__ == '__main__':
	main()
#EOF
