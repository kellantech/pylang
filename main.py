import sys,os,re,math

global _error_color
_error_color = True

class Error:
	def __init__(self,typ,desc,tok=None):
		self.type = typ
		self.desc = desc
		self.tok = tok or token("???",ln="NA",col="NA")
		self._pref = "\033[0;31m" if _error_color else ""
		self._post = "\033[0m" if _error_color else ""
		
	def __repr__(self):
		if self.tok.col != "NA":
			self.tok.col += 1
			
		return f"{self._pref}error: line {self.tok.ln},column {self.tok.col} \n{self.desc}{self._post}"

class IllegalCharError(Error):
	def __init__(self,des,tok=None):
		super().__init__("IllegalCharError",des,tok)

class IllegalDecimalLiteral(Error):
	def __init__(self,des,tok=None):
		super().__init__("IllegalDecimalLiteral",des,tok)

class invalidSyntax(Error):
	def __init__(self,des,tok=None):
		super().__init__("InvalidSyntax",des,tok)

class IllegalOperation(Error):
	def __init__(self,des,tok=None):
		super().__init__("IllegalOperation",des,tok)

class DivisionByZero(Error):
	def __init__(self,des,tok=None):
		super().__init__("DivisonByZero",des,tok)

class SymbolNotFound(Error):
	def __init__(self,des,tok=None):
		super().__init__("SymbolNotFound",des,tok)


class InvalidFunctionCall(Error):
	def __init__(self,des,tok=None):
		super().__init__("InvalidFunctionCall",des,tok)
		
class InvalidPropertyAccess(Error):
	def __init__(self,des,tok=None):
		super().__init__("InvalidPropertyAccess",des,tok)

class IndexOutOfRange(Error):
	def __init__(self,des,tok=None):
		super().__init__("IndexOutOfRange",des,tok)

class IOError(Error):
	def __init__(self,des,tok=None):
		super().__init__("IOError",des,tok)

class PYError(Error):
	def __init__(self,des,tok=None):
		super().__init__("PYError",des,tok)


def CONV(pyt):
			if isinstance(pyt,float) or isinstance(pyt,int):
				return numberType(pyt),None
			if isinstance(pyt,str):
				return stringType(pyt),None
			if isinstance(pyt,list):
				return listType(pyt),None
			if isinstance(pyt,tuple) :
				return listType(list(pyt)),None
			if pyt == None:
				return lNoneType(),None
			return None,PYError(f"can not convert python type '{type(pyt).__name__}' to ...",self.cur)

class symbolTable:
	def __init__(self,par):
		self.parent = par
		self.tbl = {}
	def set(self,nm,val):
		self.tbl[nm] = val
	def get(self,name):
		if name in self.tbl.keys():
			return self.tbl[name],None
		else:
			if self.parent:
				return self.parent.get(name)
		return None,SymbolNotFound(f"symbol '{name}' not found",token(name,ln=-2,col=-2))

	def addFn(self,nm,fn,narg):
		f = PyfnType(nm,["arg"*narg],lambda *x:CONV(fn(*x)))
		self.set(nm,f)

class ltype:
	def illegal_operation(self,op,):
		return IllegalOperation(f"can't perform operator '{op}'",self.cur)

	def __add__(self,o):return self.illegal_operation('+')
	def __sub__(self,o):return self.illegal_operation('-')
	def __mul__(self,o):return self.illegal_operation('*')
	def __truediv__(self,o):return self.illegal_operation('/')
	def __lt__(self,o):return self.illegal_operation('<')
	def __gt__(self,o):return self.illegal_operation('>')
	def __ge__(self,o):return self.illegal_operation('>=')
	def __le__(self,o):return self.illegal_operation('<=')
	def __eq__(self,o):return self.illegal_operation('==')
	def __ne__(self,o):return self.illegal_operation('!=')
	def getProp(self,pr):return None,InvalidPropertyAccess(f"cant find property '{pr}'",self.cur)
	def callProp(self,pr,arg):return None,InvalidPropertyAccess(f"cant find property '{pr}'",self.cur)


class lNoneType(ltype):
	def __repr__(self):
		return "NULL"
	def truthy(self):
		return False
		
class listType(ltype):
	def __init__(self,val):
		self.val = val
	def __repr__(self):
		return f"{self.val}"
	def truthy(self):
		return len(self.val) > 0
	def __eq__(self,othr):
		return self.val == othr.val
	def __ne__(self,othr):
		return self.val != othr.val
	def __add__(self,o):
		if isinstance(o,listType):
			return listType(self.val+o.val),None
		return None,self.illegal_operation('+',self.cur)

	def getProp(self,p):
		if p == "len":
			return len(self.val),None
		else:
			return None,InvalidPropertyAccess(f"cant find property '{p}'",self.cur)

	def callProp(self,p,a):
		if p == "get":
			if len(a) != 1:
				return None,InvalidFunctionCall(f"'get' expected 1 argument, got {len(a)}",self.cur)
			if not isinstance(a[0].val,int):
				return None,InvalidFunctionCall(f"'get' argument should be integer",self.cur)
			if a[0].val >= len(self.val):
				return None,IndexOutOfRange(f"Index {a[0].val} out of range",self.cur)
			return self.val[a[0].val],None
		elif p == "set":
			if len(a) != 2:
				return None,InvalidFunctionCall(f"'set' expected 2 arguments, got {len(a)}",self.cur)
			if not isinstance(a[0].val,int):
				return None,InvalidFunctionCall(f"'set' argument should be integer",self.cur)
			if a[0].val >= len(self.val):
				return None,IndexOutOfRange(f"Index {a[0].val} out of range",self.cur)
			self.val[a[0].val] = a[1]
			return lNoneType(),None
		elif p == "append":
			if len(a) != 1:
				return None,InvalidFunctionCall(f"'append' expected 1 argument, got {len(a)}",self.cur)
			self.val.append(a[0])
			return lNoneType(),None
		else:
			return None,InvalidPropertyAccess(f"cant find property '{p}'",self.cur)
			

class numberType(ltype):
	def __init__(self,val):
		self.val = val
	def __repr__(self):
		s= '{0:.5f}'.format(self.val)
		while s[-1] == '0':
			s = s[:-1]
		if s[-1] == '.':
			s = s[:-1]
		return s
	def truthy(self):
		return self.val != 0
	def __eq__(self,othr):

		return self.val == othr.val
	def __ne__(self,othr):
		return self.val != othr.val
	def __gt__(self,othr):
		return self.val > othr.val
	def __lt__(self,othr):
		return self.val < othr.val
	def __ge__(self,othr):
		return self.val >= othr.val
	def __le__(self,othr):
		return self.val <= othr.val
	def __add__(self,o):
		if isinstance(o,numberType):
			return numberType(self.val+o.val),None
		return None,self.illegal_operation('+')
		
	def __sub__(self,o):
		if isinstance(o,numberType):
			return numberType(self.val-o.val),None
		return None,self.illegal_operation('-')
		
	def __mul__(self,o):
		if isinstance(o,numberType):
			return numberType(self.val*o.val),None
		return None,self.illegal_operation('*')
	
	def pow(self,o):
		if isinstance(o,numberType):
			return numberType(self.val**o.val),None
		return None,self.illegal_operation('pow')
	
	def __truediv__(self,o):
		if isinstance(o,numberType):
			if o.val != 0:
				return numberType(self.val/o.val),None
			return None,DivisionByZero("Cant divide by 0")
			
		return None,self.illegal_operation('/')
	def callProp(self,p,a):
		if p == "pow":
			if len(a) != 1:
				return None,InvalidFunctionCall(f"'pow' expected 1 argument, got {len(a)}")
			return self.pow(a[0])
		else:
			return None,InvalidPropertyAccess(f"cant find property '{p}'")

class stringType(ltype):
	def __init__(self,val):
		self.val = val
	def __repr__(self):
		return f'{self.val}'
	def truthy(self):
		return len(self.val) > 0
	def __eq__(self,othr):
		return self.val == othr.val
	def __ne__(self,othr):
		return self.val != othr.val

		
	def __add__(self,o):
		if isinstance(o,stringType):
			return stringType(self.val+o.val),None
		return None,self.illegal_operation('+')
				
	def __mul__(self,o):
		if isinstance(o,numberType):
			return stringType(self.val*o.val),None
		return None,self.illegal_operation('*')

	def callProp(self,p,a):
		if p == "get":
			if len(a) != 1:
				return None,InvalidFunctionCall(f"'get' expected 1 argument, got {len(a)}")
			if not isinstance(a[0].val,int):
				return None,InvalidFunctionCall(f"'get' argument should be integer")
			if a[0].val >= len(self.val):
				return None,InvalidFunctionCall(f"index {a[0].val} out of range")
			return stringType(self.val[a[0].val]),None
		if p == 'set':
			if len(a) != 2:
				return None,InvalidFunctionCall(f"'set' expected 2 arguments, got {len(a)}")
			if not isinstance(a[0].val,int):
				return None,InvalidFunctionCall(f"'set' argument 0 should be integer")
			if not isinstance(a[1].val,str):
				return None,InvalidFunctionCall(f"'set' argument 1 should be string")
			
			if a[0].val >= len(self.val):
				return None,InvalidFunctionCall(f"index {a[0].val} out of range")

			iv = list(self.val)
			iv[a[0].val] = a[1].val
			self.val = ''.join(iv)
			
			return lNoneType(),None
			
		
		else:
			return None,InvalidPropertyAccess(f"cant find property '{p}'")


class fnType(ltype):
	def __init__(self,nt,argn,blk):
		self.nt = nt
		self.argn = argn
		self.blk = blk

	def __repr__(self):
		return f"fn {self.nt}: {self.argn} -> {self.blk}"
		
	def call(self,st,argl):
		if len(argl) != len(self.argn):
			return None,InvalidFunctionCall(f"expected {len(self.argn)} arguments, got {len(argl)}",)
		nst = symbolTable(st)
		for idx in range(len(argl)):
			e,er = argl[idx].exec(st)
			if er:return None,er
			nst.set(self.argn[idx].val,e)
		iterp = interpreter(self.blk,nst)
		ex,er = iterp.run(True)
		if ex is None:ex=lNoneType()
			
		return ex,er


class PyfnType(ltype):
	def __init__(self,nt,argn,fn):
		self.nt = nt
		self.argn = argn
		self.fn = fn

	def __repr__(self):
		return f"fn {self.nt}: {self.argn} -> <builitn>"
		
	def call(self,st,argl):
		if len(argl) != len(self.argn):
			return None,InvalidFunctionCall(f"expected {len(self.argn)} arguments, got {len(argl)}")
		
		agl2 = []
		for idx in range(len(argl)):
			e,er = argl[idx].exec(st)
			if er:return None,er
			agl2.append(e)
		
		e,er = self.fn(*agl2)
		if e is None:e=lNoneType()
		return e,er
class fnDefNode:
	def __init__(self,ntk,argn,bk):
		self.ntk = ntk
		self.argn = argn
		self.bk = bk
	def __repr__(self):
		return f"fnDef {self.ntk}: {self.argn} -> {self.bk}"
	
	def exec(self,st):
		nd = fnType(self.ntk,self.argn,self.bk)
		st.set(self.ntk.val,nd)
		return lNoneType(),None

class fnCallNode:
	def __init__(self,fnn,arg):
		self.fnn = fnn
		if isinstance(self.fnn,token):
			self.fnn = self.fnn.val
			
		self.arg = arg
	def __repr__(self):
		return f"fnCALL {self.fnn}({self.arg})"

	
	def exec(self,st):
		fc,fe = st.get(self.fnn)
		if fe:return None,fe
		return fc.call(st,self.arg)
		

class retNode:
	def __init__(self,ex):
		self.ex = ex
	def __repr__(self):
		return "return %s"%self.ex
	def exec(self,st):
		return self.ex.exec(st)

class numberNode:
	def __init__(self,val):
		self.val = val	
		
	def __repr__(self):
		return str(self.val)

	def exec(self,gst):
		return numberType(self.val),None


class listNode:
	def __init__(self,val):
		self.val = val	
		
	def __repr__(self):
		return f"[{self.val}]"

	def exec(self,gst):
		rs = []
		for v in self.val:
			e,r = v.exec(gst)
			if r:return None,r
			rs += [e]
		return listType(rs),None


class stringNode:
	def __init__(self,val):
		self.val = val	
		
	def __repr__(self):
		return f"\"{self.val}\""

	def exec(self,gst):
		return stringType(self.val),None


class dotNode:
	def __init__(self,lh,rh):
		self.lh = lh
		self.rh = rh
	
	def __repr__(self):
		return f"{self.lh} . {self.rh}"

	def exec(self,gst):
		l,e = self.lh.exec(gst)
		if e : return None,e
		r = self.rh.val
		return l.getProp(r)

class dotCallNode:
	def __init__(self,lh,rh,arg):
		self.lh = lh
		self.rh = rh
		self.args = arg
	def __repr__(self):
		return f"{self.lh} . {self.rh} ({self.args})"

	def exec(self,gst):
		l,e = self.lh.exec(gst) 
		if e : return None,e
		rh = self.rh.val
		exar = []
		for a in self.args:
			r,e = a.exec(gst)
			if e:return None,e
			exar.append(r)
			
			
		return l.callProp(rh,exar)


class incNode:
	def __init__(self,nm,adr=1):
		self.nm = nm
		self.adr = adr 
	def __repr__(self):
		return "%s++"%self.nm
		
	def exec(self,st):
		sv,er = st.get(self.nm)
		if er:return None,er
		v,ve = sv + numberType(self.adr)
		if ve:return None,ve
		x,e =  varSetNode(self.nm,numberNode(v.val)).exec(st)
		if e:return None,e
		return v,None

class forNode:
	def __init__(self,init,comp,inc,blk):
		self.init = init
		self.comp = comp
		self.inc = inc 
		self.blk = blk

	def __repr__(self):
		return f"FOR ({self.init};{self.comp};{self.inc}) DO {self.blk}"


	def exec(self,st):
		_,e = self.init.exec(st)
		if e:return None,e

		while True:
			cr,cre = self.comp.exec(st)
			if cre:return None,cre
			if not cr.truthy():
				break
			
			_,be = self.blk.exec(st)
			if be:return None,be
				
			_,er = self.inc.exec(st)
			if er:return None,er
		return lNoneType(),None

class whileNode:
	def __init__(self,cm,bk):
		self.cm = cm
		self.bk = bk
		
		
	def __repr__(self):
		return f"WHILE {self.cm} DO {self.bk}"

	def exec(self,st):
		c,e = self.cm.exec(st)
		if e:return e
		while c.truthy():
			_,er = self.bk.exec(st)
			c,e = self.cm.exec(st)
			if e:return None,e
		return lNoneType(),None

class ifNode:
	def __init__(self,cond,blk,elseblk=None,elcon=[],elbks=[]):
		self.cond = cond
		self.blk = blk

		self.elcon = elcon
		self.elbks = elbks
	
		self.elseblk=elseblk
		
		
	def __repr__(self):
		el = " "
		for i,v in enumerate(self.elcon):
			el += f"ELIF {v} THEN {self.elbks[i]}"
		if len(el)>1:
			el += ' '
		return f'IF {self.cond}{el}THEN {self.blk} ELSE !!{self.elseblk}!!'

	def exec(self,st):
		tr,tre = self.cond.exec(st)
		if tre:return None,tre
			
		if tr.truthy():
			return self.blk.exec(st)			

		for ind,cnd in enumerate(self.elcon):
			c,e = cnd.exec(st)
			if e:return None,e
			if c.truthy():
				bk = self.elbks[ind]
				return bk.exec(st)
		
		if self.elseblk != None:
			return self.elseblk.exec(st)

		return lNoneType(),None


class blockNode:
	def __init__(self,exprs):
		self.exp = exprs
		
	def __repr__(self):
		return str(self.exp)

	def exec(self,gst,fn=False):
		for ex in self.exp:
			if fn and isinstance(ex,retNode):
				return ex.exec(gst)
			r,err = ex.exec(gst)
			if err:return None,err
		return lNoneType(),None

class varSetNode:
	def __init__(self,nm,val):
		self.nm = nm
		self.val = val	
		
	def __repr__(self):
		return "%s=%s"%(self.nm,self.val)

	def exec(self,st):
		v,e = self.val.exec(st)
		if e:return None,e
			
		st.set(self.nm,v)
		return lNoneType(),None

class varGetNode:
	def __init__(self,nm):
		self.nm = nm
		
	def __repr__(self):
		return "%s"%(self.nm)

	def exec(self,st):
		return st.get(self.nm)

class compNode:
	def __init__(self,l,o,r):
		self.l = l
		self.r = r
		self.o = o
		
	def __repr__(self):
		return "%s%s%s"%(self.l,self.o,self.r)

	def exec(self,st):
		l,e = self.l.exec(st) 
		if e:return None,e
		r,rr = self.r.exec(st)
		if rr:return None,rr
		if self.o.type == tt.eql:
			return numberType((l== r)*1),None
		if self.o.type == tt.neq:
			return numberType((l!= r)*1),None
		if self.o.type == tt.gtn:
			return numberType((l>r)*1),None
		if self.o.type == tt.ltn:
			return numberType((l<r)*1),None
		if self.o.type == tt.geq:
			return numberType((l>=r)*1),None
		if self.o.type == tt.leq:
			return numberType((l<=r)*1),None

class logicalNode:
	def __init__(self,l,op,r):
		self.l = l
		self.op = op
		self.r = r
	def __repr__(self):
		return f"[{self.l}] {self.op} [{self.r}]"

	def exec(self,st):
		lh,lr = self.l.exec(st)
		if lr:return None,lr
		rh,rr = self.r.exec(st)
		if rr:return None,rr
		print(self.r,"!")
		if self.op.type == tt.land:
			if lh.truthy() and rh.truthy():
				return numberType(1),None
			else:
				return numberType(0),None
		elif self.op.type == tt.lor:
			if lh.truthy() or rh.truthy():
				return numberType(1),None
			else:
				return numberType(0),None
		elif self.op.type == tt.lnor:
			if not (lh.truthy() or rh.truthy()):
				return numberType(1),None
			else:
				return numberType(0),None


class augmentNode:
	def __init__(self,l,op,r):
		self.l = l
		self.op = op
		self.r = r
	def __repr__(self):
		return f"{self.l} {self.op}= {self.r}"

	def exec(self,st):
		lr = varGetNode(self.l.val)

		nd = binopNode(lr,self.op,self.r)
		
		print('!',nd)
		_,er = varSetNode(self.l.val,nd).exec(st)
		if er:return None,er
		return lNoneType(),None
		
class binopNode:
	def __init__(self,l,op,r):
		self.l = l
		self.op = op
		self.r = r
		
	def __repr__(self):
		return f"[{self.l},{self.op},{self.r}]"

	def exec(self,gs):
		lh,le = self.l.exec(gs)
		if le:return None,le
			
		rh,re = self.r.exec(gs)
		if re:return None,re
			
		o =  self.op.type
		if o == tt.add:
			return lh+rh
		elif o == tt.sub:
			return lh-rh
		elif o == tt.mul:
			return lh*rh
		elif o == tt.div:
			return lh/rh
			

class unaryopNode:
	def __init__(self,op,r):
		self.op = op
		self.r = r
		
	def __repr__(self):
		return f"[{self.op},{self.r}]"
	def exec(self,st):
		rh,rhe = self.r.exec(st)
		if rhe:return None,rhe
		if self.op.type == tt.sub:
			
			return rh * numberType(-1)
			
		return rh,None



class tt:
	int        = 'INT'
	float      = 'FLOAT'
	string     = 'STRING'
	add        = 'ADD'
	sub        = 'SUB'
	mul        = 'MUL'
	div        = 'DIV'
	inc        = 'INC'
	dec        = 'DEC'
	eof        = 'EOF'
	lp         = 'LP'
	rp         = 'RP'
	lb         = "LB"
	rb         = "RB"
	eq         = 'EQ'
	identifier = "IDENTIFIER"
	keyword    = "KEYWORD"
	eql        = "EQL"
	neq        = "NEQ"
	gtn        = "GTN"
	ltn        = "LTN"
	leq        = "LEQ"
	geq        = "GEQ"
	scln       = "SCLN"
	lcb        = "LCB"
	rcb        = "RCB"
	trthy      = "TRTHY"
	coma       = "COMA"
	dot        = "DOT"
	aadd       = "AADD"
	asub       = "ASUB"
	amul       = "AMUL"
	land       = "LAND"
	lor        = "LOR"
	lnor       = "LNOR"
	
	
class token:
	def __init__(self,type_,val=None,*,col=None,ln=None):
		self.type = type_
		self.val = val
		self.col = col
		self.ln = ln

	
	def __repr__(self):
		if self.val: return f"{self.type}:{self.val}"
		return f"{self.type}"
		
nums = '0123456789'

ltrs = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'

keywords = ["var","if","else","fn","ret","elif","while","for"]

class lexer:
	def __init__(self,txt):
		self.txt = txt
		self.ind = -1
		self.char = None
		self.bklv = 0
		self.ln = 0
		self.col = -1
		self.next()

	def next(self):
		self.ind += 1
		self.col +=1
		if self.char == '\n':
			self.col = 0 
			self.ln += 1
		self.char = self.txt[self.ind] if self.ind < len(self.txt) else None
		

	def gen(self):
		tok = []
		while self.char != None:
			if self.char == '+':
				self.next()
				if self.char == '+':
					tok.append(token(tt.inc,ln=self.ln,col=self.col))
					self.next()
				elif self.char == "=":
					tok.append(token(tt.aadd,ln=self.ln,col=self.col))
					self.next()
				else:
					tok.append(token(tt.add,ln=self.ln,col=self.col))
					
			if self.char == '-':
				self.next()
				if self.char == '-':
					tok.append(token(tt.dec,ln=self.ln,col=self.col))
					self.next()
				elif self.char == "=":
					tok.append(token(tt.asub,ln=self.ln,col=self.col))
					self.next()
				else:
					tok.append(token(tt.sub,ln=self.ln,col=self.col))
			elif self.char == '*':
				self.next()
				if self.char == "=":
					tok.append(token(tt.amul,ln=self.ln,col=self.col))
					self.next()
				else:	
					tok.append(token(tt.mul,ln=self.ln,col=self.col))
			elif self.char == '/':
				tok.append(token(tt.div,ln=self.ln,col=self.col))
				self.next()
			elif self.char == ';':
				if self.bklv > 0 :
					tok.append(token(tt.scln,ln=self.ln,col=self.col))
					self.next()
					
				else:
					self.next()
					return tok+[token(tt.eof,ln=self.ln,col=self.col)],None
			elif self.char == '?':
				tok.append(token(tt.trthy,ln=self.ln,col=self.col))
				self.next()
			elif self.char == '(':
				tok.append(token(tt.lp,ln=self.ln,col=self.col))
				self.next()
			elif self.char == ')':
				tok.append(token(tt.rp,ln=self.ln,col=self.col))
				self.next()
			elif self.char == '{':
				tok.append(token(tt.lcb,ln=self.ln,col=self.col))
				self.bklv += 1
				self.next()
			elif self.char == '}':
				tok.append(token(tt.rcb,ln=self.ln,col=self.col))
				self.bklv -= 1
				self.next()
			elif self.char == '[':
				tok.append(token(tt.lb,ln=self.ln,col=self.col))
				self.next()
			elif self.char == ']':
				tok.append(token(tt.rb,ln=self.ln,col=self.col))
				self.next()
			elif self.char == ',':
				tok.append(token(tt.coma,ln=self.ln,col=self.col))
				self.next()
			elif self.char == '.':
				tok.append(token(tt.dot,ln=self.ln,col=self.col))
				self.next()

			elif self.char == '>':
				self.next()
				if self.char == '=':
					tok.append(token(tt.geq,ln=self.ln,col=self.col))
					self.next()
				else:
					tok.append(token(tt.gtn,ln=self.ln,col=self.col))
			elif self.char == '<':
				self.next()
				if self.char == '=':
					tok.append(token(tt.leq,ln=self.ln,col=self.col))
					self.next()
				else:
					tok.append(token(tt.ltn,ln=self.ln,col=self.col))
			elif self.char == '&':
				tok.append(token(tt.land,ln=self.ln,col=self.col))
				self.next()
			elif self.char == '|':
				tok.append(token(tt.lor,ln=self.ln,col=self.col))
				self.next()
			elif self.char == '#':
				tok.append(token(tt.lnor,ln=self.ln,col=self.col))
				self.next()

			elif self.char == '=':
				self.next()
				if self.char == '=':
					tok.append(token(tt.eql,ln=self.ln,col=self.col))
					self.next()
				else:
					tok.append(token(tt.eq,ln=self.ln,col=self.col))
			elif self.char == '!':
				self.next()
				if self.char == '=':
					tok.append(token(tt.neq,ln=self.ln,col=self.col))
					self.next()
				else:
					return None,IllegalCharError("Illegal character: '!'",)####
				
			elif self.char in ltrs + '_':
				tok.append(self.identifier())
			elif self.char in '\n\t ':
				self.next()
			elif self.char in ["'",'"']:
				s,e = self.string()
				if e:return None,e
				tok.append(s)
			elif self.char in nums:
				num,err = self.number()
				if err:
					return None,err
				else:
					tok.append(num)
			else:
				return None,IllegalCharError(f"Illegal character: '{self.char}'")
				
		return tok + [token(tt.eof,ln=self.ln,col=self.col)],None
	def identifier(self):
		s = self.char
		self.next()
		while self.char != None and self.char in ltrs + nums + '_':
			s += self.char
			self.next()
			
		if s in keywords:
			return token(tt.keyword,s,ln=self.ln,col=self.col)
		return token(tt.identifier,s,ln=self.ln,col=self.col)

	def string(self):
		st = self.char
		r = ''
		self.next()
		esc = False
		escd = { "n":"\n","\\":"\\","t":"\t","'":"'","\"":"\""}
		
		while self.char != st or esc:
			if self.char == None:
				return None,invalidSyntax("unterminated string")
			if esc:
				e = escd.get(self.char,None)
				if e == None:
					return None,invalidSyntax("unknown escape sequence")
				r += e
				esc = False
			else:
				if self.char == '\\':
					esc = True
				else:
					r += self.char
			self.next()
			
		self.next()
		return token(tt.string,r,ln=self.ln,col=self.col),None
		
	def number(self):
		r = ''
		dot = 0
		while self.char != None and self.char in nums+'.':
			
			if self.char == '.':
					dot += 1					
			r += self.char
			self.next()
		if dot > 1:
			return None,IllegalDecimalLiteral(f'Illegal decimal literal: {r}')
		
		if dot: return token(tt.float,float(r)),None
		else: return token(tt.int,int(r)),None


class parser:
	def __init__(self,toks):
		self.toks = toks
		self.ind = -1
		self.cur = None
		
		self.next()
	def next(self):
		self.ind += 1
		self.cur = self.toks[self.ind] if self.ind < len(self.toks) else None
		return self.cur
	def look(self):
		if self.ind + 1 == len(self.toks):
			return None
		return self.toks[self.ind + 1]
		
	def value(self):
		if self.cur.type in [tt.int,tt.float]:
			nn = numberNode(self.cur.val)
			self.next()
			return nn,None

		if self.cur.type == tt.lb:
			self.next()
			r = []
			if self.cur.type == tt.rb:
				self.next()
				return listNode([]),None
			ie,ir = self.logic_expr()
			if ir:return None,ir
			r += [ie]
			while self.cur.type == tt.coma:
				self.next()
				ex,er = self.logic_expr()
				if er:return None,er
				r += [ex]
			if self.cur.type == tt.rb:
				self.next()
				return listNode(r),None
			else:
				return None,invalidSyntax("expected ']'",self.cur)
			
		if self.cur.type == tt.string:
			sn = stringNode(self.cur.val)
			self.next()
			return sn,None
			
		elif self.cur.type in [tt.add,tt.sub]:
			
			op = self.cur
			self.next()
			val,ve = self.value()
			if ve:return None,ve
				
			return unaryopNode(op,val),None
		
		elif self.cur.type == tt.lp:
			self.next()
			p = self.logic_expr()
			self.next()
			return p
		elif self.cur.type == tt.identifier:
			nm = self.cur.val
			self.next()
			if self.cur.type == tt.lp:
				return self.fn_call(nm)
			elif self.cur.type == tt.dot:
				vg = varGetNode(nm)
				self.next()
				if self.cur.type == tt.identifier:
					pr = self.cur
					self.next()
					if self.cur.type == tt.lp:
						self.next()
						if self.cur.type == tt.rp:
							return dotCallNode(vg,pr,[]),None
						args = []
						e,er = self.logic_expr()
						if er:return er
						args.append(e)
						while self.cur.type == tt.coma:
							self.next()
							if self.cur.type == tt.rp:
								break
							e,er = self.logic_expr()
							if er:return er
							args.append(e)
						if self.cur.type != tt.rp:
							return None,invalidSyntax("expected ')'",self.cur)
						self.next()
						return dotCallNode(vg,pr,args),None
					
					else:
						dn = dotNode(vg,pr)
						self.next()
						return dn,None
				else:
					return None,invalidSyntax("expected identifier",self.cur)
			else:
				print(nm,'!')
				vn = varGetNode(nm)
				return vn,None
		elif self.cur.type in [tt.inc,tt.dec]:
			t= [-1,1][self.cur.type == tt.inc]
			self.next()
			if self.cur.type == tt.identifier:
				vl = self.cur.val
				self.next()
				return incNode(vl,t),None
			else:
				return None,invalidSyntax("cant increment a litteral",self.cur)
		else:
			return None,invalidSyntax(f"expected int, float, or identifier",self.cur)
			
			
	def term(self):
		return self.binOp(self.value,[tt.mul,tt.div])

	def var_set(self):
		if self.cur.type == tt.keyword and self.cur.val == "var":
			self.next()
		if self.cur.type == tt.identifier:
				n = self.cur.val
				self.next()
				if self.cur.type == tt.eq:
					self.next()
					ex,xr = self.logic_expr()
					if xr:
						return None,xr
					else:
						return varSetNode(n,ex),None
				else:
					return None,invalidSyntax("expected '='",self.cur)
		else:
			return None,invalidSyntax(f"expected identifier",self.cur)
	
	def fn_expr(self):
		if self.cur != None and self.cur.type == tt.keyword and self.cur.val == 'fn':
			self.next()
			if self.cur != None and self.cur.type == tt.identifier:
				ntk = self.cur
				self.next()
				if self.cur != None and self.cur.type == tt.lp:
					self.next()
					ids = []
					while self.cur.type == tt.identifier:
						ids.append(self.cur)
						self.next()
						if self.cur.type == tt.coma:
							self.next()
						else:
							break
					if self.cur != None and self.cur.type == tt.rp:
						self.next()
						bk,be = self.block(True)
						if be:return None,be
						return fnDefNode(ntk,ids,bk),None
					else:
						return None,invalidSyntax("expected ')'",self.cur)
						
				else:
					return None,invalidSyntax("expected '('",self.cur)
			else:
				return None,invalidSyntax('expected identifier',self.cur)
		else:
			return None,invalidSyntax('expected \'fn\'',self.cur)

	def compexpr(self):
		return self.binOp(self.expr,(tt.eql,tt.neq,tt.gtn,tt.ltn,tt.leq,tt.geq),compNode)
	def logic_expr(self):
		return self.binOp(self.compexpr,(tt.land,tt.lor,tt.lnor),logicalNode)
	
	
	def expr(self):
		if self.cur.type == tt.keyword and self.cur.val == "var":
			self.next()
			return self.var_set()
		if self.cur.type == tt.keyword and self.cur.val == "if":
			return self.if_expr()
			
		if self.cur.type == tt.keyword and self.cur.val == "while":
			return self.while_expr()
			
		if self.cur.type == tt.keyword and self.cur.val == "for":
			return self.for_expr()
		
		if self.cur.type == tt.keyword and self.cur.val == "fn":
			ex =  self.fn_expr()
			self.next()
			return ex
		if self.cur.type == tt.identifier:
			if self.look().type == tt.eq:
				return self.var_set()		
			elif self.look().type in [tt.aadd,tt.asub,tt.amul]:
				nm = self.cur
				self.next()
				if self.cur.type == tt.aadd:
					op = token(tt.add)
				elif self.cur.type == tt.asub:
					op = token(tt.sub)
				elif self.cur.type == tt.amul:
					op = token(tt.mul)
					
				self.next()
				r,re = self.logic_expr()
				if re:return None,re
				return augmentNode(nm,op,r),None
				
		r,rer = self.binOp(self.term,[tt.add,tt.sub])
		
		if rer:return None,rer
		
		
		return r,None
	def fn_call(self,cn):
		self.next()
		ars = []
		while self.cur.type != tt.rp:
			ex,xr = self.logic_expr()
			
			if xr:return None,xr
			ars.append(ex)
			if self.cur.type != tt.coma:
				break
			self.next()
		self.next()
		return fnCallNode(cn,ars),None

	def while_expr(self):
		if self.cur == None or self.cur.type != tt.keyword or self.cur.val != 'while':
			return None,invalidSyntax("expected 'while'",self.cur)
		else:
			self.next()
			cm,e = self.logic_expr()
			if e:return None,e
			bk,e = self.block()
			if e:return None,e
		self.next()
		return whileNode(cm,bk),None
			
	def for_expr(self):
		if self.cur == None or self.cur.type != tt.keyword or self.cur.val != 'for':
			return None,invalidSyntax("expected 'for'",self.cur)
		else:
			self.next()
			if self.cur == None or self.cur.type != tt.lp:
				return None,invalidSyntax("expected '('",self.cur)
			else:
				self.next()
				vs,er = self.var_set()
				if er:return None,er
				if self.cur == None or self.cur.type != tt.coma:
					return None,invalidSyntax("expected ','",self.cur)
				else:
					self.next()
					cmp,er = self.logic_expr()
					if er:return None,er
					if self.cur == None or self.cur.type != tt.coma:
						return None,invalidSyntax("expected ','",self.cur)
					else:
						self.next()
						ic,er = self.logic_expr()
						if self.cur.type in [tt.inc,tt.dec]:	
							self.next()
						if er:return None,er
						if self.cur == None or self.cur.type != tt.rp:
							return None,invalidSyntax(f"expected ')'",self.cur)
						else:
							self.next()
							bk,be = self.block()
							if be:return None,be
							self.next()
							return forNode(vs,cmp,ic,bk),None
				
		
	def if_expr(self):
		if self.cur == None or self.cur.type != tt.keyword or self.cur.val != 'if':
			return None,invalidSyntax("expected 'if'",self.cur)
		self.next()
		cn,cr = self.logic_expr()
		if cr:return None,cr
		bk,br = self.block()
		if br:return None,br
		eb = None
		self.next()
		elc = []
		elb = []
		
		while self.cur != None and self.cur.type == tt.keyword and self.cur.val == 'elif':
			self.next()
			c,e = self.logic_expr()
			if e:return None,e
			elc.append(c)
			b,e = self.block()
			if e:return None,e
			elb.append(b)
			self.next()

		if self.cur != None and self.cur.type == tt.keyword and self.cur.val == 'else':
			self.next()
			eb,er = self.block()
			if er:return None,er
			self.next()
		return ifNode(cn,bk,eb,elc,elb),None
		
	# def comp(self):
	# 	l,lr = self.logic_expr()
	# 	if lr:return None,lr
	# 	op = self.cur
	# 	if op.type in [tt.eql,tt.neq,tt.gtn,tt.ltn,tt.leq,tt.geq]:
	# 		self.next()
	# 		r,rr = self.logic_expr()
	# 		if rr:return None,rr
	# 		return compNode(l,op,r),None
	# 	elif op.type == tt.trthy:
	# 		self.next()
	# 		return l,None

			
	#	return None,invalidSyntax("expected comparison operator")
	def ret(self,fn):
			if fn:
				self.next()
				e,ex = self.logic_expr()
				if ex:return None,ex
				return retNode(e),None
			else:
				return None,invalidSyntax("'ret' only allowed in functions",self.cur)
		
	def block(self,fn=False):
		if self.cur.type != tt.lcb:
			return None,invalidSyntax("expected '{'",self.cur)
		self.next()
		if self.cur.type == tt.rcb:
			return blockNode([]),None
			
		if self.cur !=None and self.cur.type == tt.keyword and self.cur.val == "ret":
				r,rx = self.ret(fn)
				if rx:return None,rx
				return blockNode([r]),None
			
		e,er = self.logic_expr()
		if er:return None,er
		exs = [e]
		while self.cur != None and self.cur.type == tt.scln:
			self.next()
			if self.cur == None or self.cur.type in [tt.rcb,tt.eof]: break
			if fn and self.cur !=None and self.cur.type == tt.keyword and self.cur.val == "ret":
				
				r,rx = self.ret(fn)
				if rx:return None,rx
				return blockNode(exs + [r]),None
				
			e,er = self.logic_expr()
			
			if er:return None,er
			exs += [e]
		if self.cur.type == None or self.cur.type != tt.rcb:
			return None,invalidSyntax("expected '}'",self.cur)			
		return blockNode(exs),None

		
	def binOp(self,fn,tks,nodeT=binopNode):
		node,er = fn()
		if er:return None,er
		while 1:
			if self.cur == None or self.cur.type not in tks:
				break
				
			o = self.cur
			self.next()
			r,e = fn()
			if e:return None,e		
			node = nodeT(node,o,r)
			
			if self.cur != None and self.cur.type in [tt.int,tt.float]:
				return None,invalidSyntax("expected '*','/','+','-'",self.cur)
						
		return node,None

class interpreter:
	def __init__(self,ast,gst):
			self.ast = ast
			self.gst = gst
	def run(self,*args):		
		return self.ast.exec(self.gst,*args)

def modsearch(mn):
	try:
		with open(mn+".?"):
			pass
		return mn+".?"
	except:
		try:
			pth = f"{os.path.dirname(__file__)}/stdlib/{mn}.?"
			with open(pth):
				pass
			return pth
		except:
			return mn

class runContext:
	def __init__(self,text,st):
		self.txt = text
		def imp(t):
			mt = re.search("import <[a-zA-z_]+>",self.txt)
			mt = modsearch(mt.group().split(' ')[1][1:-1])
			try:
				with open(mt)as im:
					c =im.read()
				return c
			except FileNotFoundError:
				print(f"error: module {mt} not found")
				exit()
		while "import <" in self.txt:	
			self.txt = re.sub("import <[a-zA-z_]+>",imp,self.txt)

				
		def readf(fnm):
			try:
				with open(fnm) as f:
					txt = f.read()
				return stringType(txt),None
			except:
				return None,IOError(f"could not open file {fnm}",self.cur)
		def writef(fnm,txt):
			try:
				with open(fnm,'a') as f:
					f.write(txt)
				return lNoneType(),None
			except:
				return None,IOError(f"could not open file {fnm}",self.cur)

		def pyexec(pycode):
			try:
				if pycode.startswith("import"):
					import importlib
					mn = pycode.split(' ')[1]
					globals()[mn] = importlib.import_module(mn)
					
					return lNoneType(),None
				else:
					#_RES_ = None
					exec (f"_RES_ = {pycode}",globals())
					global _RES_
					return CONV(_RES_)
			except Exception as e:
					return None,e
		
		gst = symbolTable(None)
		while "importPY <" in self.txt:	
			mname = re.search("importPY <[a-zA-z_]+>",self.txt).group().split(' ')[1][1:-1]
			__import__('importlib').import_module(f"stdlib.{mname}").init(gst)
			self.txt = re.sub(f"importPY <{mname}>","",self.txt)
		gst.set("print",PyfnType("print",["text"],lambda a:(print(a),None)))
		gst.set("input",PyfnType("input",["prompt"],lambda p:(stringType(input(p)),None)))
		
		
		gst.set("number",PyfnType("number",["str"],lambda a:(numberType(int(a.val)),None)))
		gst.set("string",PyfnType("string",["num"],lambda a:(stringType(str(a.val)),None)))
		gst.set("intRound",PyfnType("intRound",["float"],lambda a:(numberType(round(a.val)),None)))
		gst.set("floor",PyfnType("floor",["float"],lambda a:(numberType(math.floor(a.val)),None)))

		gst.set("readf",PyfnType("readf",["fname"],lambda fname:readf(fname.val)))
		gst.set("writef",PyfnType("writef",["fname","text"],lambda fname,txt:writef(fname.val,txt.val)))

		gst.set("_PY_EXEC",PyfnType("_PY_EXEC",["pycode"],lambda pycode:pyexec(pycode.val)))
		self.gst = gst
		if st:
			self.gst = st

		self.lxr = lexer(self.txt)
	def runOne(self):
		tk,er = self.lxr.gen()
		if er: 
			print(er)
			return -1
		elif tk[0].type == tt.eof:
			return 1
		else:
			print(tk)
			i = parser(tk)
			a,e = i.logic_expr()
			if e:
				print(e)
				return -1
			else: 
				print(a)
				i = interpreter(a,self.gst)
				rs,rse = i.run()
				if rse:
					print(rse)
					return 1
				return 0
	def run(self):
		while (ext := self.runOne()) == 0:pass
		

def run(tx,st=None):
	rcx = runContext(tx,st)
	rcx.run()
	return rcx
hrf = False


for arg in sys.argv[1:]:
	hrf = True
	with open(arg) as f:
		c = f.read()
	run(c)
	
if not hrf:
	while (t := input(">")) != 'q':
		run(t)
	
