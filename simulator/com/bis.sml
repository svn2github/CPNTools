functor BIS(structure Stream: STREAM
            structure Err : GRAMERROR) =
struct
	fun read_message ins = let 

	    fun getList (getFn, 0) = nil
	      | getList (getFn, n) = (getFn ins)::getList(getFn,n-1);

	    val bargs = Stream.getInteger ins;
	    val iargs = Stream.getInteger ins;
	    val sargs = Stream.getInteger ins;

	    val blist = getList (Stream.getBool, bargs);
	    val ilist = getList (Stream.getInteger, iargs);
	    val slist = getList (Stream.getString, sargs); 
	in
	    Err.dumpBISlists("Read:\n",blist,ilist,slist);
	    (blist,ilist,slist) 
	end

   	(* Function used to write simulator responses *)
	fun write_message outs (blist,ilist,slist) = let 

	    fun putList (putFn, xs) =  app (fn x => putFn outs x) xs;
		    
	in
	    Stream.putInteger outs (length blist);
	    Stream.putInteger outs (length ilist);
	    Stream.putInteger outs (length slist);
	    putList (Stream.putBool,blist);
	    putList (Stream.putInteger,ilist);
	    putList (Stream.putString,slist); 
	    Err.dumpBISlists("Wrote:\n",blist,ilist,slist)
      end

	fun send_result opcode outs lists = 
	    (Stream.putInteger outs opcode;
	     write_message outs lists;
	     Stream.flush outs)
end
