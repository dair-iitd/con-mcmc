import sys, re, math, copy, operator,random
from datetime import datetime
random.seed(datetime.now())
def generate_random_MLN():
	out_file=open(sys.argv[1],"w")
	n=int(sys.argv[2])
	k=int(sys.argv[3])
	ev_percent=float(sys.argv[4])
	out_file.write("// domain declarations\n")
	out_file.write("a0Value={A0_VAL}\n")
	for i in range(1,n+1):
		out_file.write("a"+str(i)+"Value={A"+str(i)+"_VAL}\n")
	out_file.write("\n")
	out_file.write("// predicate declarations\n")
	out_file.write("A"+str(0)+"(a"+str(0)+"Value)\n")
	for i in range(1,n+1):
		out_file.write("A"+str(i)+"(a"+str(i)+"Value)\n")
	for p in range(0,n/(k*k)):
		for q in range(1,k*k+1):
			for r in range(q+1,k*k+1):
				out_file.write("JointA"+str(p*k*k+q)+"A"+str(p*k*k+r)+"(a"+str(p*k*k+q)+"Value,a"+str(p*k*k+r)+"Value)\n")		
	out_file.write("\n")
	out_file.write("// formulas\n")
	p1=float(sys.argv[5])
	out_file.write(str(-1*math.log(p1))+" !A0(A0_VAL)\n")
	#out_file.write(str(-1*math.log(1-p1))+" A0(A0_VAL)\n")
	bit=[None]*k
	for p in range(0,n/k):
		rn=random.normalvariate(0,0.0001)
		for q in range(0,2**(k)):
			count_ones=0
			for x in range(0,k):
				bit[x]=((q & (2**x))!=0)
				if(bit[x]):
					count_ones=count_ones+1				
			temp_string="!A0(A0_VAL)"
			for r in range(1,k+1):
				if bit[r-1]==False:
					temp_string=temp_string+" v !A"+str(r+p*k)+"(A"+str(r+p*k)+"_VAL)"
				else:
					temp_string=temp_string+" v A"+str(r+p*k)+"(A"+str(r+p*k)+"_VAL)"
			out_file.write(str(count_ones*0.01+rn)+" "+temp_string+"\n")
	for o in range(0,n/(k*k)):
		for p in range(1,k+1):
			#rn=random.random()
			rn=random.normalvariate(0,0.0001)
			for q in range(0,2**(k)):
				 # For one sided Sym Add random here
				count_ones=0
				for x in range(0,k):
					bit[x]=((q & (2**x))!=0)
					if(bit[x]):
						count_ones=count_ones+1				
				temp_string="A0(A0_VAL)"
				for r in range(0,k):
					var=o*k*k+r*k+p
					if bit[r-1]==False:
						temp_string=temp_string+" v !A"+str(var)+"(A"+str(var)+"_VAL)"
					else:
						temp_string=temp_string+" v A"+str(var)+"(A"+str(var)+"_VAL)"
				out_file.write(str(count_ones*0.01+rn)+" "+temp_string+"\n")
	for p in range(0,n/(k*k)):
		for q in range(1,k*k+1):
			for r in range(q+1,k*k+1):
				out_file.write("0.1 !A"+str(p*k*k+q)+"(A"+str(p*k*k+q)+"_VAL) v"+" !A"+str(p*k*k+r)+"(A"+str(p*k*k+r)+"_VAL)"+" v JointA"+str(p*k*k+q)+"A"+str(p*k*k+r)+"(a"+str(p*k*k+q)+"Value,a"+str(r)+"Value)\n")		
	for i in range(0,n):
		rn=random.random()
		if rn<ev_percent:
			rn=random.random()
			if rn<0.5:
				out_file.write("!A"+str(i)+"(A"+str(i)+"_VAL).\n")
			else:
				out_file.write("A"+str(i)+"(A"+str(i)+"_VAL).\n")
		
		



def main():
	generate_random_MLN()
	return

if __name__ == '__main__':
	main()
#EOF
