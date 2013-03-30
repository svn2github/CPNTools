package org.cpntools.simulator.extensions.utils;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.clapper.util.classutil.AbstractClassFilter;
import org.clapper.util.classutil.AndClassFilter;
import org.clapper.util.classutil.ClassFilter;
import org.clapper.util.classutil.ClassFinder;
import org.clapper.util.classutil.ClassInfo;
import org.clapper.util.classutil.InterfaceOnlyClassFilter;
import org.clapper.util.classutil.NotClassFilter;
import org.clapper.util.classutil.SubclassClassFilter;

import dk.klafbang.tools.Pair;

/**
 * @author michael
 */
public class Discovery {
	private static List<File> jars = new ArrayList<File>();

	private static ClassLoader loader = null;

	/**
	 * @param f
	 */
	public static void addFile(final File f) {
		Discovery.jars.add(f);
	}

	/**
	 * @param clazz
	 * @return
	 */
	public static <T> Pair<Collection<ClassInfo>, Class<T>> findExtensions(final Class<T> clazz) {
		return Discovery.findExtensions(clazz, true);
	}

	/**
	 * @param clazz
	 * @param pluginsOnly
	 * @return
	 */
	public static <T> Pair<Collection<ClassInfo>, Class<T>> findExtensions(final Class<T> clazz,
	        final boolean pluginsOnly) {
		final ClassFinder finder = new ClassFinder();
		if (!pluginsOnly || Discovery.jars.isEmpty()) {
			finder.addClassPath();
		}
		finder.add(Discovery.jars);

		final ClassFilter filter = new AndClassFilter(new NotClassFilter(new InterfaceOnlyClassFilter()),
		        new SubclassClassFilter(clazz), new NotClassFilter(new AbstractClassFilter()));

		final Collection<ClassInfo> foundClasses = new ArrayList<ClassInfo>();
		finder.findClasses(foundClasses, filter);
		return Pair.createPair(foundClasses, clazz);
	}

	/**
	 * @param extensionsClasses
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public static <T> Collection<T> instantiate(final Pair<Collection<ClassInfo>, Class<T>> extensionsClasses) {
		if (Discovery.loader == null) {
			Discovery.loader = ClassLoader.getSystemClassLoader();
			if (!Discovery.jars.isEmpty()) {
				final URL[] urls = new URL[Discovery.jars.size()];
				int i = 0;
				for (final File f : Discovery.jars) {
					try {
						urls[i] = f.toURI().toURL();
					} catch (final MalformedURLException e) {
						assert false;
					}
					i++;
				}
				Discovery.loader = new URLClassLoader(urls, Discovery.loader);
			}
		}

		final List<T> result = new ArrayList<T>();
		for (final ClassInfo ci : extensionsClasses.getFirst()) {
			try {
				final Class<?> clazz = Class.forName(ci.getClassName(), true, Discovery.loader);
				final Object instance = clazz.newInstance();
				if (extensionsClasses.getSecond().isInstance(instance)) {
					result.add((T) instance);
				}
			} catch (final Exception _) {
				_.printStackTrace();
			}
		}
		return result;
	}

}
