import sys, re, math, copy, operator,random
def generate_random_MLN():
	from datetime import datetime
	random.seed(datetime.now())
	
	out_file=open(sys.argv[1],"w")
	out_file_evid=open("evidence.txt","w")
	num_persons=int(sys.argv[2])
	out_file.write("// domain declarations\n")
	temp_str="person={P1"
	for i in range(1,num_persons):
		temp_str=temp_str+",P"+str(i+1)
	temp_str=temp_str+"}\n"
	out_file.write(temp_str)
	out_file.write("\n")
	out_file.write("// predicate declarations\n")
	out_file.write("IsYoung\n")
	out_file.write("mb(person)\n")
	out_file.write("nbr(person,person)\n")
	out_file.write("friends(person,person)\n")
	out_file.write("smokes(person)\n")
	out_file.write("cancer(person)\n")
	out_file.write("db(person)\n")
	out_file.write("\n")
	out_file.write("// formulas\n")
	p1=float(sys.argv[4])
	out_file.write(str(-1*math.log(p1))+" !IsYoung\n")
	#out_file.write(str(-1*math.log(1-p1))+" IsYoung\n")
	#str1 = str(random.normalvariate(0,0.1))
	#str2 = str(random.normalvariate(0,0.1))
	#str3 = str(0.1+random.normalvariate(0,0.1))
	out_file.write("0.1	!IsYoung v smokes(v1)\n")
	out_file.write("+W	IsYoung v !smokes(v1)\n")
	out_file.write("+W	!IsYoung v mb(v1)\n")
	out_file.write("0.1	IsYoung v !mb(v1)\n")
	out_file.write("0.001	!cancer(v1)\n")
	out_file.write("0.003	!db(v1)\n")
	out_file.write("4	!friends(v1,v2)\n")
	out_file.write("4.001	!nbr(v1,v2)\n")
	out_file.write("0.002	!smokes(v1) v cancer(v1)\n")
	out_file.write("0.003	!mb(v1) v db(v1)\n")
	out_file.write("1.00001	!friends(v1,v2) v !smokes(v1) v smokes(v2)\n")
	out_file.write("1.00002	!friends(v1,v2) v smokes(v1) v !smokes(v2)\n")
	out_file.write("1.00001	!nbr(v1,v2) v !mb(v1) v mb(v2)\n")
	out_file.write("1.00002	!nbr(v1,v2) v mb(v1) v !mb(v2)\n")
	
	i=0
	ev_percent= float(sys.argv[3])
	for i in range(num_persons):
		if random.random()<ev_percent:
			if random.random()<0.5:
				out_file.write("smokes(P"+str(i+1)+").\n")
				out_file_evid.write("smokes(P"+str(i+1)+").\n")
			else:
				out_file.write("!smokes(P"+str(i+1)+").\n")
                out_file_evid.write("!smokes(P"+str(i+1)+").\n")
	i=0
	for i in range(num_persons):
		if random.random()<ev_percent:
			if random.random()<0.5:
				out_file.write("mb(P"+str(i+1)+").\n")
				out_file_evid.write("mb(P"+str(i+1)+").\n")
			else:
				out_file.write("!mb(P"+str(i+1)+").\n")
                out_file_evid.write("!mb(P"+str(i+1)+").\n")
	i=0
	for i in range(num_persons):
		if random.random()<ev_percent:
			if random.random()<0.5:
				out_file.write("cancer(P"+str(i+1)+").\n")
				out_file_evid.write("cancer(P"+str(i+1)+").\n")
			else:
				out_file.write("!cancer(P"+str(i+1)+").\n")
                out_file_evid.write("!cancer(P"+str(i+1)+").\n")
 	i=0
 	for i in range(num_persons):
		if random.random()<ev_percent:
			if random.random()<0.5:
				out_file.write("db(P"+str(i+1)+").\n")
				out_file_evid.write("db(P"+str(i+1)+").\n")
			else:
				out_file.write("!db(P"+str(i+1)+").\n")
                out_file_evid.write("!db(P"+str(i+1)+").\n")
	out_file.close()
	out_file_evid.close()


def main():
	generate_random_MLN()
	return

if __name__ == '__main__':
	main()
#EOF
