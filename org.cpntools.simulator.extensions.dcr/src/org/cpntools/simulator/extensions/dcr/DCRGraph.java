package org.cpntools.simulator.extensions.dcr;

import java.util.HashSet;

public class DCRGraph {
	public HashSet<Tuple<String, String>> conditions = new HashSet<Tuple<String, String>>();
	public HashSet<Tuple<String, String>> responses = new HashSet<Tuple<String, String>>();
	public HashSet<Tuple<String, String>> includes = new HashSet<Tuple<String, String>>();
	public HashSet<Tuple<String, String>> excludes = new HashSet<Tuple<String, String>>();
	public HashSet<Tuple<String, String>> milestones = new HashSet<Tuple<String, String>>();
	public HashSet<String> events = new HashSet<String>();
	
	public DCRMarking Execute(DCRMarking m, String e)
	{
		if (!events.contains(e)) 
			return m;
		
		if (!Enabled(m, e))
			return m;
		
		//DCRMarking result = new DCRMarking();
		
		//for (String s : m.executed)
				
		m.executed.add(e);
		
		m.pending.remove(e);
		m.pending.addAll(RelationsOf(responses, e));
		m.included.removeAll(RelationsOf(excludes, e));
		m.included.addAll(RelationsOf(includes, e));			
		
		//TODO
		return m;
	}	
	
	public Boolean Enabled(DCRMarking m, String e)
	{
		if (!events.contains(e)) 
			return true;
		// check included
		if (!m.included.contains(e)) 
			return false;
		// check conditions		
		if (!m.executed.containsAll(RelationsFor(conditions, e)))
			return false;	
		// check milestones 
		//TODO
		for (String p : m.pending)
			if (RelationsFor(milestones, e).contains(p))
				return false;	
		return true;
	}	
	
	
	private HashSet<String> RelationsOf(HashSet<Tuple<String, String>> hs, String e)
	{
		HashSet<String> result = new HashSet<String>();
		
		for (Tuple<String, String> r : hs)
			if (r.getLeft().equals(e))
				result.add(r.getRight());
		return result;	
	}
	
	
	private HashSet<String> RelationsFor(HashSet<Tuple<String, String>> hs, String e)
	{
		HashSet<String> result = new HashSet<String>();
		
		for (Tuple<String, String> r : hs)
			if (r.getRight().equals(e))
				result.add(r.getLeft());
		return result;	
	}	
	
	public DCRMarking InitialMarking()
	{
		DCRMarking result = new DCRMarking();
		for(String e : events)
			result.included.add(e);		
		return result;
	}
	
	

}
