package org.cpntools.simulator.extensions.test;

import java.util.ArrayList;
import java.util.Collection;

import org.clapper.util.classutil.AbstractClassFilter;
import org.clapper.util.classutil.AndClassFilter;
import org.clapper.util.classutil.ClassFilter;
import org.clapper.util.classutil.ClassFinder;
import org.clapper.util.classutil.ClassInfo;
import org.clapper.util.classutil.InterfaceOnlyClassFilter;
import org.clapper.util.classutil.NotClassFilter;
import org.clapper.util.classutil.SubclassClassFilter;
import org.cpntools.simulator.extensions.Extension;

public class DiscoveryTest {
	public static void main(final String... args) {
		final ClassFinder finder = new ClassFinder();
		finder.addClassPath();

		final ClassFilter filter = new AndClassFilter(new NotClassFilter(new InterfaceOnlyClassFilter()),
		        new SubclassClassFilter(Extension.class), new NotClassFilter(new AbstractClassFilter()));

		final Collection<ClassInfo> foundClasses = new ArrayList<ClassInfo>();
		System.out.println("Searching...");
		finder.findClasses(foundClasses, filter);

		for (final ClassInfo classInfo : foundClasses) {
			System.out.println("Found " + classInfo.getClassName());
		}
		System.out.println("Done");
	}

}
