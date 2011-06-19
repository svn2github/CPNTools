(******************************************************************************
 ** RealSparseMatrix.sml
 ** sml
 **
 ** Guy Blelloch
 ** Sparse implementation of the Matrix Library
 ******************************************************************************)

structure RealSparseMatrix : MATRIX =
    struct
	structure T = IntTable

	type matrix = (int * int * (real T.table) T.table)
	type colVec = matrix
	type rowVec = matrix

	type scalar = real
	type index = int
	type subscript = (index * index)
	type size      = (index * index)

	fun size(n,m,_) = (n,m)
	fun nRows(n,_,_) = n
	fun nCols(_,m,_) = m

	exception badDimension
	exception notInvertable
	exception notImplemented

	fun checkZero(v) = if Real.==(v,0.0) then NONE else SOME(v)
	fun checkEmpty(v) = if T.isEmpty(v) then NONE else SOME(v)

	fun insertItem(j,v,row) = 
	    if Real.==(v,0.0) then row
	    else T.insert((j,v),row)

	fun insertRow(i,row,a) =
	    if T.isEmpty(row) then a
	    else T.insert((i,row),a)

	fun find'(i,t) =
	    case T.find(i,t) of
		NONE => T.empty
	      | SOME(v) => v

	fun rowSub(r,j) =
	    case T.find(j,r) of 
		NONE => 0.0 
	      | SOME(v) => v

	fun rowAdd(a,b) = T.union (fn (a,b) => checkZero(Real.+(a,b))) (a,b)
	fun rowDiff(a,b) = T.union (fn (a,b) => checkZero(Real.-(a,b))) (a,b)
	fun rowMult(a,b) = T.intersect (fn (a,b) => checkZero(Real.*(a,b))) (a,b)
	fun rowScale(s,a) = T.filter (fn (a) => checkZero(Real.*(s,a))) a
	fun rowDot(a,b) = T.foldrIdx (fn ((i,v),sum) => v + sum) 0.0 (rowMult(a,b))

	fun sub'(a,i,j) =
	    case T.find(i,a) of
		NONE => 0.0
	      | SOME(row) => rowSub(row,j)

	fun sub((n,m,a),(i,j)) = sub'(a,i,j)

	fun insertElt'(a,i,j,v) = 
	    T.update (fn NONE => SOME(T.insert((j,v),T.empty))
	               | SOME(r) => SOME(T.insert((j,v),r)))
	             (i,a)
	           
	fun insertElt((n,m,a),(i,j),v) = (n,m,insertElt'(a,i,j,v))

	fun insertElts(a, elts) = 
	    Vector.foldr (fn ((ij,v),m) => insertElt(m,ij,v)) a elts

	fun mapRows2 rowMap2 (a,b) = 
	    T.union (fn (ra,rb) => checkEmpty(rowMap2(ra,rb))) (a,b)

	fun matAdd(a,b) = mapRows2 rowAdd (a,b)
	fun matDiff(a,b) = mapRows2 rowDiff (a,b)

	fun for_n n init f =
	    let
		fun next(i,a) = 
		    if (i=n) then a
		    else next(i+1,f(i,a))
	    in
		next(0,init)
	    end

	fun insert(a,i,j,v) =
	    let 
		fun updaterow(NONE)    = SOME(insertItem(j,v,T.empty))
		  | updaterow(SOME(r)) = SOME(insertItem(j,v,r))
	    in
		T.update updaterow (i,a)
	    end

	fun matrix((n,m),v) : matrix =
	    if Real.==(v,0.0) then (n,m,T.empty)
	    else 
		let
		    val row = for_n m T.empty (fn (j,a) => T.insert((j,v),a))
		in
		    (n,m,for_n m T.empty (fn (j,a) => T.insert((j,row),a)))
		end

	fun tabulate((n,m),f) = 
	    let
		fun addrow(i,a) = 
		    let
			fun addelt(j,row) = insertItem(j,f(i,j),row)
			val row = for_n m T.empty addelt
		    in
			insertRow(i,row,a)
		    end
	    in
		(n,m,for_n n T.empty addrow)
	    end

	fun identity(n) = 
	    let
		fun addrow(i,a) = T.insert((i,T.insert((i,1.0),T.empty)),a)
	    in
		(n,n,for_n n T.empty addrow)
	    end

	fun map f (n,m,a) =
	    let
		fun maprow(r) = checkEmpty(T.filter (checkZero o f) r)
	    in
		(n, m, T.filter maprow a)
	    end

	fun fromList(a) = 
	    let
		fun makeRow(nil,j,row) = (j,row)
		  | makeRow(h::r,j,row) = makeRow(r,j+1,insertItem(j,h,row))
		fun makeAll(nil,i,m,a) = (i,m,a)
		  | makeAll(h::r,i,m,a) =
		       let
			   val (m',row) = makeRow(h,0,T.empty)
		       in
			   makeAll(r,i+1,Int.max(m,m'),insertRow(i,row,a))
		       end
	    in
		makeAll(a,0,0,T.empty)
	    end

        fun toList(a) = 
	    let
		val (n,m) = size(a)
		fun getrow(i) = List.tabulate(m,(fn j => sub(a,(i,j))))
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

	fun foldNonZero f i a = 
	    let
		fun addrow((i,r),a) = 
		    (T.foldrIdx (fn ((j,v),a) => f((i,j,v),a)) a r)
	    in
		T.foldrIdx addrow i a
	    end

	fun trans((n,m,a)) = 
	    (m,n,foldNonZero (fn ((i,j,v),a) => insert(a,j,i,v)) T.empty a)

	fun row((n,m,a),i) = (1,m,insertRow(0,find'(i,a),T.empty))

	fun column((n,m,a),i) =
	    let
		fun rowElt(r) = 
		    case T.find(i,r) of
			NONE => NONE
		      | SOME(v) => SOME(T.insert((0,v),T.empty))
	    in
		(n, 1, T.filter rowElt a)
	    end

	fun appendRows((na,ma,a),(nb,mb,b)) =
	    let 
		val _ = if (ma=mb) then () else raise badDimension
		fun moveRow((i,v),b') = T.insert((i+na,v),b')
	    in
		(na+nb,ma,T.foldrIdx moveRow a b)
	    end

	fun appendCols((na,ma,a),(nb,mb,b)) =
	    let 
		val _ = if (na=nb) then () else raise badDimension
		fun shiftRow(r) =
		    T.foldrIdx (fn ((j,v),b) => T.insert((j+ma,v),b)) T.empty r
		val b' = T.map shiftRow b
	    in
		(na, ma+mb, matAdd(a,b'))
	    end

	structure PQ = IntPrioritySet

	fun solveRec(a,pq,i,n) =
	    let
		val ((_,pivotRowIndex),pq) = PQ.deleteMin(pq)
		(* val _ = print("pivot row: " ^ Int.toString(pivotRowIndex) ^ "\n") *)
		(* val _ = print(toString((3,4,a))) *)
		val (a,SOME(pivotRow)) = T.deleteReturn(pivotRowIndex,a)
		val pivot = rowSub(pivotRow, pivotRowIndex)
		val _ = if Real.==(0.0,pivot) then raise notInvertable else ()
		val pivotRow' = rowScale(1.0/pivot,pivotRow)
	    in
		if (i = 1) then
		    T.insert((pivotRowIndex, rowSub(pivotRow',n)), T.empty)
		else
		    let
			fun adjustRow(r,_) = 
			    let
				val scale = (case T.find(pivotRowIndex,r) of
						 NONE => raise notInvertable
					       | SOME(v) => ~v)
				val r' = rowAdd(r,rowScale(scale, pivotRow'))
			    in
				SOME(T.size(r'),r')
			    end
			val adjRows = T.intersect adjustRow (a,pivotRow')
			val a' = T.diff (fn (a,(m,b)) => SOME(b)) (a,adjRows)
			val pq' = T.foldrIdx (fn ((k,(m,b)),pq) => 
					      PQ.insert((m,k),pq)) 
			                  pq adjRows
			val b = solveRec(a',pq',i-1,n)
			val x = Real.-(rowSub(pivotRow',n), rowDot(pivotRow', b))
		    in
			T.insert((pivotRowIndex,x),b)
		    end
	    end

	fun solve((n,m,a),(n',m',b)) = 
	    let
		val _ = (if (n = m) andalso (m = n') andalso (m' = 1) then ()
			 else raise badDimension)
		val _ = if (n' = m) then () else raise badDimension

		fun insertQ((i,r),pq) = PQ.insert((T.size(r),i),pq)
		val pq = T.foldrIdx insertQ PQ.empty a

		(* The following shifts the column vector b up to column n 
		   and then puts it in as the last column of the matrix a *)
		val (_,_,a') = appendCols((n,m,a),(n',m',b))
	    in
		trans(1,n,T.insert((0,solveRec(a',pq,n,n)),T.empty))
	    end

	fun sizeCheck f ((na,ma,a),(nb,mb,b)) =
	    if not(na=nb andalso ma=mb) then 
		raise badDimension
	    else
		(na,ma, f(a,b))

	fun a + b = sizeCheck matAdd (a,b)
	fun a - b = sizeCheck matDiff (a,b)

	fun scale(a,s) = map (fn (v) => Real.*(v,s)) a

	fun a * b = 
	    let
		val (n1,l,at') = trans(a)
		val (n2,m,b') = b
		val _ = if (n1 = n2) then () else raise badDimension

                val subMatrices = T.intersect (fn (ac,br) => SOME(ac,br)) (at',b')

		fun mergerows((i,(acol,brow)),mat) = 
		    let
			fun row(av) = (T.map (fn bv => Real.*(av,bv)) brow)
		    in
			matAdd((T.map row acol),mat)
		    end
	    in
		(l,m,T.foldrIdx mergerows T.empty subMatrices)
	    end

	fun fromArray2(a) = raise notImplemented
	fun det(a) = raise notImplemented
	fun inv(a) = raise notImplemented
	fun invDet(a) = raise notImplemented
	fun crossProduct(a) = raise notImplemented
	fun norm(a) = raise notImplemented
	fun mapi(a) = raise notImplemented
	fun map2(a) = raise notImplemented
	fun fold(a) = raise notImplemented
	fun foldi(a) = raise notImplemented
	fun fromDiag(a) = raise notImplemented
	fun toDiag(a) = raise notImplemented
	fun rows(a) = raise notImplemented
	fun columns(a) = raise notImplemented
	fun insertRegion(a) = raise notImplemented
	fun subRegion(a) = raise notImplemented
	fun updateElt f a = raise notImplemented

    end
