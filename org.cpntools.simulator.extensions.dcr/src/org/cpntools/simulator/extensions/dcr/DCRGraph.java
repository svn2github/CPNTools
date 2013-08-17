package org.cpntools.simulator.extensions.dcr;

import java.util.HashMap;
import java.util.HashSet;

public class DCRGraph {
	public HashSet<Tuple<String, String>> conditions = new HashSet<Tuple<String, String>>();
	public HashSet<Tuple<String, String>> responses = new HashSet<Tuple<String, String>>();
	public HashSet<Tuple<String, String>> includes = new HashSet<Tuple<String, String>>();
	public HashSet<Tuple<String, String>> excludes = new HashSet<Tuple<String, String>>();
	public HashSet<Tuple<String, String>> milestones = new HashSet<Tuple<String, String>>();
	public HashSet<String> events = new HashSet<String>();
	
	public HashMap<String, Tuple<Integer, Tuple<String, String>>> relationID = new HashMap<String, Tuple<Integer, Tuple<String, String>>>();
	
	public void RemoveRealtion(String ID)
	{
		if (relationID.containsKey(ID))
		{
			Integer relationType = relationID.get(ID).getLeft();
			Tuple<String, String> relation = relationID.get(ID).getRight();
			if (relationType == 1)
				conditions.remove(relation);
			if (relationType == 2)
				responses.remove(relation);
			if (relationType == 3)
				includes.remove(relation);
			if (relationType == 4)
				excludes.remove(relation);
			if (relationType == 5)
				milestones.remove(relation);			
		}		
	}
	
	
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
	
	
	  @Override public String toString() {
		    StringBuilder result = new StringBuilder();
		    String NEW_LINE = System.getProperty("line.separator");

		    result.append(this.getClass().getName() + " DCR Graph {" + NEW_LINE);
		    result.append(" Events: ");
		    for (String e : events)
		    	result.append(e + "; ");
		    result.append(NEW_LINE);
		    
		    result.append(" Responses: ");
		    for (Tuple<String, String> r : responses)
		    	result.append(r.getLeft() + " *-> " + r.getRight() + ";");
		    result.append(NEW_LINE);		    

		    result.append(" Conditions: ");
		    for (Tuple<String, String> r : conditions)
		    	result.append(r.getLeft() + " ->* " + r.getRight() + ";");
		    result.append(NEW_LINE);		    
		    
		    //Note that Collections and Maps also override toString
		    //result.append(" RelationID: " + relationID.toString() + NEW_LINE);
		    result.append("}");

		    return result.toString();
		  }	
	
	

}
