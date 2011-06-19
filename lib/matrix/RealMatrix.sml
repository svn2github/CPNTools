(******************************************************************************
 ** RealMatrix.sml
 ** sml
 **
 ** Guy Blelloch
 ** Dense implementation of the Matrix Library
 ******************************************************************************)

structure RealMatrix : MATRIX =
    struct
	structure A2 = Array2

	type matrix = real A2.array
	type colVec = matrix
	type rowVec = matrix

	type scalar = real
	type index = int
	type subscript = (index * index)
	type size      = (index * index)

	val size = A2.dimensions
	val nRows = A2.nRows
	val nCols = A2.nCols

	exception badDimension
	exception notInvertable
	exception notImplemented

	fun sub(a,(i,j)) = A2.sub(a,i,j)

	fun matrix((n,m),v) = A2.array(n,m,v)
	    
	fun tabulate((n,m),f) = A2.tabulate A2.RowMajor (n,m,f)

	fun row(m,i) = 
	    A2.tabulate A2.RowMajor (1, nCols(m), (fn (_,j) => A2.sub(m,i,j)))

	fun column(m,j) = 
	    A2.tabulate A2.ColMajor (nRows(m), 1, (fn (i,_) => A2.sub(m,i,j)))

	fun rows(m,iv) = 
	    A2.tabulate A2.RowMajor (Vector.length(iv), nCols(m),
				     (fn (i,j) => A2.sub(m,Vector.sub(iv,i),j)))

	fun columns(m,iv) = 
	    A2.tabulate A2.ColMajor (nRows(m), Vector.length(iv),
				     (fn (i,j) => A2.sub(m,i,Vector.sub(iv,j))))

	fun subRegion(m,(rv,cv)) =
	    A2.tabulate A2.ColMajor (Vector.length(rv),Vector.length(cv),
				     (fn (i,j) => A2.sub(m,
							 Vector.sub(rv,i),
							 Vector.sub(cv,j))))	    

	fun fromDiag(a) =
	    let
		val (n,m) = size(a)
		val n' = Int.min(n,m)
	    in
		tabulate((n',1),(fn (i,_) => A2.sub(a,i,i)))
	    end

	fun toDiag(a) =
	    let
		val (n,m) = size(a)
		val _ = if (m=1) then () else raise badDimension
	    in
		tabulate((n,n),(fn (i,j) => if (i=j) then A2.sub(a,i,0)
				            else 0.0))
	    end

	fun copy(a)  =
	    let
		val (n,m) = A2.dimensions(a)
	    in
		A2.tabulate A2.RowMajor (n,m,(fn (i,j) => A2.sub(a,i,j)))
	    end

	fun insertElt(a,(i,j),v) = 
	    let
		val a' = copy(a)
		val _ = A2.update(a',i,j,v)
	    in
		a'
	    end

	fun updateElt f (a,(i,j)) = 
	    let
		val a' = copy(a)
		val _ = A2.update(a',i,j,f(A2.sub(a,i,j)))
	    in
		a'
	    end

	fun fold f init a =
	    A2.fold A2.RowMajor f init a

	fun foldi f init a =
	    A2.foldi A2.RowMajor (fn (i,j,v,m) => f((i,j),v,m)) init 
	             {base=a,col=0,row=0,ncols=NONE,nrows=NONE}

	fun insertElts(a,mods) = 
	    let
		val a' = copy(a)
		val _ = Vector.app (fn ((i,j),v) => A2.update(a',i,j,v)) mods
	    in
		a'
	    end

	fun insertRegion(a,(rv,cv),b) = 
	    let
		val a' = copy(a)
		fun appi f v = Vector.appi f (v,0,NONE)
		fun icol(i,i') = appi (fn (j,j') => A2.update(a',i',j',
							      A2.sub(b,i,j)))
		                      cv
		val _ = appi icol rv
	    in
		a'
	    end

	fun appendRows(a,b) = 
	    let 
		val (na,ma) = size(a)
		val (nb,mb) = size(b)
		val _ = if (ma=mb) then () else raise badDimension
		fun getElt(i,j) = (if (i >= na) then A2.sub(b,i-na,j) 
		                   else A2.sub(a,i,j))
	    in
		tabulate((na+nb, ma), getElt)
	    end

	fun appendCols(a,b) = 
	    let 
		val (na,ma) = size(a)
		val (nb,mb) = size(b)
		val _ = if (na=nb) then () else raise badDimension
		fun getElt(i,j) = (if (j >= ma) then A2.sub(b,i,j-ma) 
		                   else A2.sub(a,i,j))
	    in
		tabulate((na, ma+mb), getElt)
	    end

	fun map (f : real -> real) (a : matrix) =
	    let
		val (n,m) = size(a)
	    in
		tabulate ((n, m), (fn (i,j) => f(A2.sub(a,i,j))))
	    end

	fun mapi (f : subscript * real -> real) (a : matrix) =
	    let
		val (n,m) = size(a)
	    in
		tabulate ((n, m), (fn (i,j) => f((i,j),A2.sub(a,i,j))))
	    end

	fun identity(n) = 
	    let
		fun id(i,j) = if (i = j) then 1.0 else 0.0
	    in
		tabulate((n,n),id)
	    end

	fun trans(a) = 
	    let
		val (n,m) = size(a)
		fun tr(i,j) = A2.sub(a,j,i)    
	    in 
		tabulate((m,n),tr)
	    end

	fun scale(a, s) = map (fn a => Real.*(s,a)) a

	fun det2(M) : real =
	    A2.sub(M,0,0)*A2.sub(M,1,1) - A2.sub(M,0,1)*A2.sub(M,1,0)

	fun invDet2(M) =
	    let
		val a = A2.sub(M,0,0)
		val b = A2.sub(M,0,1)
		val c = A2.sub(M,1,0)
		val d = A2.sub(M,1,1)
		val det = (a*d - b*c)
		val x = 1.0/det
		val l = #[d*x, ~b*x, ~c*x, a*x];
		val inv = tabulate ((2,2), (fn (i,j) => Vector.sub(l,2*i+j)))
	    in
		(inv,det)
	    end

	fun gaussJordanV(i, n, m, 
			 A : real Vector.vector Vector.vector, 
			 d : real) =
	    if (i >= n) then (A,d)
	    else
		let
		    open Vector
		    val p = sub(A,i)
		    val pivot : real = sub(p,0)
		    val _ = if Real.==(pivot,0.0) then raise notInvertable 
			    else ()
		    val pinv = 1.0/pivot
		    fun adjustRow(j) =
			let
			    val r = sub(A,j)
			    val s = sub(r,0)*pinv
			    val f = (if (i = j) then
					 (fn(k) => pinv * sub(r,k+1))
				     else
					 (fn(k) => sub(r,k+1) - s*sub(p,k+1)))
			in
			    tabulate(m-i-1,f)
			end
		    val A' = tabulate(n,adjustRow)
		in
		    gaussJordanV(i+1,n,m,A',d*pivot)
		end

	fun gaussJordan(a) =
	    let
		val (n,m) = size(a)
		fun getrow(i) = Vector.tabulate(m, (fn (j) => A2.sub(a,i,j)))
		val A =  Vector.tabulate(n,getrow)
		val (A',d) = gaussJordanV(0, n, m, A, 1.0)
		fun geta(i,j) = Vector.sub(Vector.sub(A',i),j)
	    in
		(tabulate((n,m-n),geta),d)
	    end

	fun invDet(a) =
	    let
		val (n,m) = size(a)
		val _ = if (n=m) then () else raise badDimension
	    in
		if (n = 2) then invDet2(a)
		else
		    gaussJordan(appendCols(a,identity(n)))
	    end

	fun inv(a) = 
	    let
		val (b,d) = invDet(a)
	    in
		b
	    end

	fun det(a) = 
	    let
		val (n,m) = size(a)
		val _ = if (n=m) then () else raise badDimension
	    in
		if (n = 2) then det2(a)
		else
		    #2(invDet(a))
	    end

	fun solve(a,b) = 
	    let
		val (n,m) = size(a)
		val (m',l) = size(b)
		val _ = if (n=m andalso m=m') then () else raise badDimension
		val (r,d) = gaussJordan(appendCols(a,b))
	    in
		r
	    end

	fun crossProduct(a) =
	    let
		val (n,m) = size(a)
		val _ = if (n+1=m) then () else raise badDimension
	    in
		if (n = 1) then 
		    insertElt(matrix((2,1),A2.sub(a,0,0)),(0,0),~(A2.sub(a,0,1)))
		else
		    let
			val (r,d) = gaussJordan(a)
			val r' = appendRows(r,matrix((1,1),~1.0))
		    in
			scale(r',~d)
		    end
	    end

	fun norm(a) =
	    let
		fun sum(v,s:real) = v*v + s
		val ss = A2.fold A2.RowMajor sum 0.0 a
	    in
		Math.sqrt(ss)
	    end

	fun a * b = 
	    let
		val (l,n1) = size(a)
		val (n2,m) = size(b)
		val _ = if (n1 = n2) then () else raise badDimension
		fun dot(i,j) : real =
		    let
			fun dotr (k,sum) = 
			    if (k < 0) then
				sum
			    else
				dotr(k-1, Real.+(sum, Real.*(A2.sub(a,i,k),
							     A2.sub(b,k,j))))
		    in
			dotr(n1-1,0.0)
		    end
	    in
		tabulate((l,m),dot)
	    end

	fun map2 f (a,b) = 
	    let
		val (na,ma) = size(a)
		val (nb,mb) = size(b)
		val _ = if (na = nb) andalso (ma = mb) then ()
		        else raise badDimension
	    in
		tabulate((na,ma),(fn (i,j) => f(A2.sub(a,i,j),A2.sub(b,i,j))))
	    end

	fun fromArray2(a) = a

	fun fromList(a) = A2.fromList(a)

        fun toList(a) = 
	    let
		val (n,m) = size(a)
		fun getrow(i) = List.tabulate(m,(fn j => A2.sub(a,i,j)))
	    in
		List.tabulate(n,getrow)
	    end

	fun toStringF f a =
	    let
		val l = List.map 
		           (fn l => List.map (fn e => f(e)) l) 
		           (toList(a))
		val maxl = List.foldr 
                           (fn (v,s) => Int.max(s,String.size(v)))
			   0 
			   (List.concat(l))
		val pad = StringCvt.padLeft #" " (maxl+2)
		fun row(r,l) = 
		    "  "::(List.foldr (fn (e,l') => (pad e)::l') 
		                      ("\n"::l) 
				      r)
	    in
		String.concat(foldr row nil l)
	    end


        fun eltFormat(v) = (Real.fmt (StringCvt.GEN(SOME(4))) v)

	fun toString a = toStringF eltFormat a

	fun a + b = map2 Real.+ (a,b)
	fun a - b = map2 Real.- (a,b)

    end
