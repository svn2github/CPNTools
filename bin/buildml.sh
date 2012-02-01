#!/bin/bash
MIN=74
MAX_TESTED=74

if [ "x$1" == "x" ]; then
	echo "Please enter the minor version you want to install, e.g., \`buildml.sh $MIN' to install version 110.$MIN"
	echo "The minor version must be >= $MIN and should not be > $MAX_TESTED as that is the latest tested version."
	exit
fi

if [ $1 -lt $MIN ]; then
	echo "Please enter a version above $MIN"
	exit 1
fi

if [ $1 -gt $MAX_TESTED ]; then
	echo "You have entered a version above the latest tested one (110.$MAX_TESTED); press enter to continue at your own risk or Ctrl-C to cancel"
	read
else
	echo "Will install in current directory and remove the sml directory.  Ctri-C to cancel."
	read
fi

rm -Rf sml/*

function pause() {
	echo -ne $*
#	read
}

cd "$( dirname "$0" )/../"
runDir=$( pwd )
cd -
echo "$runDir"

# Get installer package
mkdir -p sml
cd sml
wget http://smlnj.cs.uchicago.edu/dist/working/110.$1/config.tgz
tar -zxvf config.tgz
pause "Downloaded and unpacked package"

export OLDPATH=$PATH
export PATH=$( pwd )/bin:$PATH
OLD_RUN=$RUN
RUN=""

# Determine architecture
ARCH=$( uname | sed -e "s/CYGWIN_NT.*/CYGWIN_NT/" )
if [ $ARCH == Linux ]; then
	MLARCH=x86-linux
	SML=run.x86-linux
fi
if [ $ARCH == Darwin ]; then
	MLARCH=x86-darwin
	SML=run.x86-darwin
fi
if [ $ARCH == CYGWIN_NT ]; then
	export SMLNJ_CYGWIN_RUNTIME=YES
	MLARCH=x86-cygwin
	SML=run.x86-cygwin
fi

echo "ARCH="$ARCH
echo "MLARCH="$MLARCH
pause "Found Arch"


# Perform installation using our settings
cp "$runDir/config/targets" "$runDir/config/preloads" config
./config/install.sh

pause "Compiled 1st"

# Download additional packages needed to compile compiler
wget http://smlnj.cs.uchicago.edu/dist/working/110.$1/pgraph.tgz
tar -zxvf pgraph.tgz
mkdir -p base
wget http://smlnj.cs.uchicago.edu/dist/working/110.$1/{cm,compiler,system}.tgz
cd base
for file in cm compiler system; do
	tar -zxvf ../$file.tgz
done
cd ../

pause "Downloaded additional packages"

# Patch compiler, runtime and system
cd base
patch -p0 < "$runDir/simulator/patch/compiler.diff"
pause "Patched compiler"
patch -p0 < "$runDir/simulator/patch/runtime.diff"
pause "Patched runtime"
patch -p0 < "$runDir/simulator/patch/system.diff"
pause "Patched system"
patch -p0 < "$runDir/simulator/patch/cm.diff"
pause "Patched cm"


# Build patched runtime
cd runtime/objs
make -f mk.$MLARCH
rm ../../../bin/.run/run.*.exe
echo cp $SML* ../../../bin/.run/$SML
cp $SML* ../../../bin/.run/$SML
echo cp $SML* ../../../bin/.run/
cp $SML* ../../../bin/.run/
cd ../../
pause "Runtime built"

# Recompile compiler
cd compiler
echo | ../../bin/sml "core.cm"
cd ../
pause "Compiled 2nd"

# Make and install new image
cd system
echo "CMB.make();" | ../../bin/sml '$smlnj/cmb.cm'
pause "Made image"
./makeml
pause "makeml"
./installml
pause "installml"
cd ../../

# Rebuild libraries
ROOT=$( pwd )
INSTALLDIR=$ROOT
CONFIGDIG=$ROOT/config
BINDIR=$ROOT/bin
export ROOT INSTALLDIR CONFIGDIR BINDIR
CM_TOLERATE_TOOL_FAILURES=true
export CM_TOLERATE_TOOL_FAILURES
"$BINDIR"/sml -m \$smlnj/installer.cm
pause "Images rebuilt"

# Copy new image to correct locations
cd bin/
echo "These copies will fail on other than Windows"
cp /cygdrive/c/cygwin/bin/cygwin1.dll /cygdrive/c/cygwin/bin/cyggcc_s-1.dll "$runDir/simulator/runtime"
chmod +x .run/$SML*
if [ -f .run/$SML ]; then
	cp .run/$SML "$runDir/simulator/runtime/$SML"*
	cp .run/$SML "$runDir/simulator/runtime/"
else
	cp .run/$SML* "$runDir/simulator/runtime/$SML"*
fi
cd ../
pause "Image copied"

# Clean up
export PATH=$OLDPATH
export RUN=$OLD_RUN
