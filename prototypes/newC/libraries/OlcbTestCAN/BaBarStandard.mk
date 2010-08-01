# standard.mk
# $Id: standard.mk,v 1.475 2008/01/09 22:30:41 kelsey Exp $
#
# GNUmakefile fragment for use with the BaBar versioning system
# 
# see also GNUmakefile.example in the SoftRelTools package
# for an example of how to use this.
#
# Bob Jacobsen, December 1994
#
# This is the common portion of the GNUmakefile for all package.
# So each package's GNUmakefile should include this.
#
## 11/12/95 T. Wenaus 
#   - Modify rules to read dependency makefiles to not print complaints
#     about .d files not existing (-include)
#   - Add printout of what's going on for default 'quiet' case
#   - Add tests for file existence before attempting to use the file,
#     to eliminate error messages
#   - Protect ln -s against file existing already
# 9/23/96 T. Hung for T. Wenaus
#   - modify  "$(workdir)%.d:  %.cc" rule 
#   - and add "$(libdir)%.o:   %.cc" rule.
# 12Mar97 S. Gowdy
#   - add BINOFILES, which is made from BINC/CFILES
#   - modify bin to depend on BINOFILES and rbin only
#   - add rbin rule to do what bin used to do
#   - add rule "$(workdir)%.o: %.cc" (needed to get dependencies right)
#   - modify "$(workdir)%.d:  %.cc" rule for BINCCFILES && LIB
# 19Mar97 S. Gowdy
#   - add rule to make shared library from archive library on Digital UNIX
#   - add new stage slib to make shared libraries. NOT used by default.
#   - add vpath so gmake can find OSF shared libraries
# 17Jun97 E. Frank
#   - SunOS5 fixes.  See Code Mgmt & support #96.1
# 15Jun98 M. Kelsey
#   - Add message at start of regression test; replace complex filename
#     expressions with $@
# Jul98 M. Kunze
#   - add shlib support for Solaris
# 28Jul98 S. Gowdy
#   - add dep target which does a check on your package's link_PACKAGE.mk file,
#	this only works on OSF at the moment
# 14Sep98 S. Gowdy
#   - split fortran rules for g77 as this will do the .F->.f conversion
#       internally
#  6Oct98 S. Gowdy
#   - added rule called templates which puts template files in library
#       for Digital UNIX. This is required to get round a bug in the
#       prelinker.
#  10Nov98 E. Frank
#   - added a target echoMacro which echos out the value of a macro.  The
#       macro to echo is set via ECHOMACRONAME, e.g.,
#       gmake ECHOMACRONAME=BINS myPackage.echoMacro
#  03Jun99 S. Gowdy
#   - modified [F-1] target for Linux so it looks in the package directory
#	and $(workdir) for the .F file.
#  24Nov99 M. Kelsey
#   - exclude huge LINKLISTDEPENDS and PACKAGELIST variables from exporting
# 	by .printenv target.
#  7Dec99 M. Kelsey
#   - |make-linkfiles| script needs to see LIBxxxFILES etc. variables.
#     extend "printenv" export handling to "linkfiles" target as well.
#  1Nov05 M. Kelsey
#   - "root:" target needs modifications to support MacOSX: SHLDFLAGS, etc.,
#     add "lib" as dependency in place of recursive Make, add copying of
#     other packages' libraries to tmp/..., instead of running |ar| in situ.
# 24Jan06 M. Kelsey
#   - binscripts: and extrabinscripts: targets extended to handle scripts
#     with associated bin_XXX.mk dependency files.  Only known use case are
#     three SRT scripts (bbROOT, bbObjy, and bbooddlx) which pre-expand some
#     GMake environment-like variables during mangling to have hardwired 
#     behaviour.
# 8Jan08 M. Kelsey
#   - add "extrabin", "binscripts" and "extrabinscripts" to list of targets
#     which are forced to no-action by OPTIONALMISSING (line 1040ff).  Hide
#     single-binary targets when forced to no-action by OPTIONALMISSING.
###########################################################################
#++ Variables defined in pkg/GNUmakefile
#
#  Major Variables:
#
#    BINCCFILES C++ files to be compiled and linked for executable binary.
#		Including files for BINS and EXTRABINS.
#    BINS	executable binaries and scripts to be built automatically.
#		you must provide binary dependency.
#    BINSCRIPTS shell script products
#    LIB	library products
#    LIBCCFILES	C++ files to be compiled into library.
#    LIBCCOBJS  C++ files to be compiled as objects into library directory.
#    MANPAGES	man pages.
#    TESTSCRIPTS  scripts for regression test.
#
#  Minor Variables:
#
#    ALLBINS	BINS + EXTRABINS + BINSCRIPTS + EXTRABINSCRIPTS
#    BINARIES	BINS + EXTRABINS
#    DOCS	Files to copy to appropriate directories	
#    INC	include file products (no rules yet)
#    LIBCFILES	C files to be compiled into library.
#    LIBFFILES	Fortran files to be compiled into library.
#    LIBF90FILES  FORTRAN-90 source to compile into library
#    LIBDDLFILES  DDL files to be processed/compiled into library.
#    LIBIDLFILES  IDL files to be processed/compiled into library.
#    LIBTMPLFILES  template class files.
#    NO_INSTALLDIRS do not run installdirs from 'gmake pkg.clean'.
#    SUBDIRS	directories which should do recursive MAKEs
#
#  Shared-library Variables:
#
#    LIBCCSTATICFILES  C++ file to keep in static library for -Shared build
#    LIBDDLSTATICFILES  DDL file to keep in static library for -Shared build
#    LIBIDLSTATICFILES  IDL file to keep in static library for -Shared build
# 
#  Extra Binary Variables:
#
#    EXTRABINS	executable binaries to be built manually. 
#               Define its source in BINCCFILES.
#		You must provide extra binary dependency and rules.
#    EXTRABINSCRIPTS scripts to be build manually. 
#
###########################################################################
#++ Variables normally defined in arch_spec.mk
#
#  Compiler/linker flags
#
#    CXXDEBUG	Debugging Flag for CXX compiler. e.g 'override CXXFLAGS=-g0'.
#    CXXFLAGS	Flags for the C++ compiler.
#    FCFLAGS	Flags for the FORTRAN-77 compiler, for .f files.
#    FFC	FORTRAN-77 compiler, for .F files.
#    FFCFLAGS	Flags for the FORTRAN-77 compiler, for .F files.
#    FFCPP	pre-processor for .F files.
#    FFCPPFLAGS	Flags for the pre-processor of .F files.
#    F90CFLAGS	Flags for the FORTRAN-90 compiler
#		Note: F-90 does _not_ use the FCFLAGS
#    MFLAG	ask preprocessor to make dependency file. Normally -M.
#    appendDir  append full name or first stem to path. e.g.
#               $(appendDir /u,Linux-Debug) returns /u/Linux-Debug if exists.
#
#________________________________________________________________________
#  Major Targets:
#
#    bin	build binaries and scripts defined in BINS  	
#    lib	build library
#    libext	build library that can be loaded independently of build
#
#  Minor targets:
#
#    allbin	build all binaries and scripts in ALLBINS.
#    binscripts	build scripts defined in BINSCRIPTS
#    extrabin	build extra binaries defined in EXTRABINS.
#    extrabinscripts	build extra scripts defined in EXTRABINSCRIPTS.
#    pkg.<foo>	build individual binary with 'gmake pkg.foo'.
#    cleanbin	clean binary only.
#    cleanlib	clean library only.
#    cleanarch	clean one architecture only.
#    clean	clean all architectures.
#
###########################################################################
#++ Variables used by BaBar GNUmakefile system
#
#  Major Variables:
#
#    ALLBINS	BINS + EXTRABINS +BINSCRIPTS +EXTRABINSCRIPTS
#    ALLLIB	LOADLIBES plus SYSLIB. 
#    CC_SUFFIX	suffix of C++ source, can be .cc or .cxx or .cpp	
#    ECHOMACRONAME Determines which macro is selected for echoing by echoMacro:
#    LOADLIBES	SRT libraries
#    SYSLIB	used to accumulate non-SRT system-level libraries.
#    REALSYSLIB	actual non-SRT system-level libraries used to link.
#    SYSLIB_REMOVALS non-SRT libraries to move to end of link line
#    TOPDIR	top level directory of this production/test release.
#
# Variables used to define destinations (note lower case):
#
#    bindir	executable products
#    docdir	documentation
#    includedir  constructed include files
#    libdir	.a products
#    mandir	man files
#    objdir	.o for static library if -Fastbuild option is on. 
#    shobjdir	.o for shared library.
#    shworkdir	.o and .d files for shared library build.
#    srcdir	source directory in packages/sub-directories.
#    testdir	test files
#    workdir	.o and .d files
# 
###########################################################################
#++ Targets:
#
#  Major targets:
#
#    all	include, lib, bin
#    bin	build executables
#    clean	remove *~, $(workdir)/*.d, $(workdir)*.o
#    lib	build libraries
#    <name>.lib  'lib' request on the named subdirectory
#    <name>.bin  'bin' request on the named subdirectory
#
#  Minor Targets:
#
#    allbin	build all binaries and scripts defined in ALLBINS.
#    check	do installation checks
#          (Note that by doing include for all packages, then lib for all
#          packages, then bin, you avoid worrying about dependency order.)
#    cleanbin	remove all executables
#    cleanlib	remove all libraries
#    dep	Does a dependency check of link_PACKAGE.mk file
#    doc	documentation.  This is also included in the bin targer
#    include	create any include files needed
#    man	man pages.  This is also included in the bin target
#    <name>.echoMacro
#		echo value of a macro.  Macro to echo is spec'd in
#		ECHOMACRONAME, e.g., gmake ECHOMACRONAME=BINS myPkg.echoMacro
#    <name>.preprocess
#		Preprocess C++ files to .ii files.
#    <name>.assembly
#		Generate assembly code (.s) from C++ files.
#    schema	generate schema for database
#
#__________________________________________________________________________
# Include BaBar specific lists for special consideration

# Skip most of standard.mk for 'gmake <pkg>.echoMacro ECHOMACRONAME=<val>" 
ifneq (BINS,$(ECHOMACRONAME))
ifneq (EXTRABINS,$(ECHOMACRONAME))
include SoftRelTools/babar_specials.mk

############################################################
# standard definitions
############################################################
ifdef DEBUG04
ERROR := $(shell echo "MAKEOVERRDIES=$(MAKEOVERRIDES) [s-1]" >& 2)
endif
ifdef DEBUG29
  ERROR := $(shell echo "   standard.mk: $(MAKECMDGOALS) [s-2]" >& 2)
endif

#__________________________________________________________________________
# define standard directories: libdir bindir includedir
# It is EXPECTED that these will be overridden in the command line.
# Note that these are _destination_ directories - see below for
# search path directories

libdir =     $(shell /bin/pwd)/
bindir =     $(shell /bin/pwd)/
includedir = $(shell /bin/pwd)/
testdir =    $(shell /bin/pwd)/
# workdir is the host-specific location for creating 
#    object files (if needed) and include dependency (.d) files
workdir = $(shell /bin/pwd)/

#__________________________________________________________________________
# Fastbuild: collect obj files all at once at the end of lib target 

ifneq (,$(findstring -Fastbuild,$(OPTIONS)))
  ifneq (,$(findstring $(MAKEGOAL),lib all))
    ifeq (,$(filter $(skip_fastbuild),$(PACKAGE)))
      fastbuild := 1
    endif ## skip_fastbuild 
  endif   ## lib all
endif     ## Fastbuild
ifneq (,$(findstring Shared,$(OPTIONS)))
fastbuild :=
endif

ifdef fastbuild
objdir = $(workdir)obj
obj2dir = $(workdir)obj2
else
objdir = $(workdir)
obj2dir = $(workdir)
endif   ## fastbuild

shobjdir = $(shworkdir)shobj/
shobj2dir = $(shworkdir)shobj2/

BINARIES := $(BINS) $(EXTRABINS)

# scripts
ifneq (,$(BINSCRIPTS))
  override BINS += $(BINSCRIPTS)
endif

ifneq (,$(BINS)$(EXTRABINS)$(EXTRABINSCRIPTS))
ALLBINS := $(BINS) $(EXTRABINS) $(EXTRABINSCRIPTS)
endif

#__________________________________________________________________________
# ldlink: get third party shared libraries throu symlinks in ldlink

ifneq (,$(findstring Fixl,$(OPTIONS)))
override OPTIONS += -Ldlink
include SoftRelTools/ldlink.mk
else
REALSYSLIB = $(filter-out $(SYSLIB_REMOVALS),$(SYSLIB)) $(SYSLIB_REMOVALS)
endif

ifeq (,$(findstring Lshared,$(OPTIONS)))
ALLLIB=$(LOADLIBES) $(REALSYSLIB)
endif

# mandir and workdir point at tmp to avoid overwritting files
#  is nothing has been specified
mandir = /tmp
docdir = /tmp

# CC_SUFFIX can be set to .cc or .cxx or .cpp
ifeq ($(CC_SUFFIX),)
    CC_SUFFIX := .cc
endif  	 
ifeq ($(HH_SUFFIX),)
    HH_SUFFIX := .hh
endif  	 

DDL_CC_SUFFIX  = _ddl$(CC_SUFFIX)
REF_HH_SUFFIX  = _ref$(HH_SUFFIX)

# Append full name or first stem to directory. e.g. $(appendDir dir,linux-deb)
appendDir = $(firstword $(wildcard $(1)/$(2)) $(wildcard $(1)/$(firstword $(subst -,  , $(2)))))

############################################################
#++ include PackageList dependencies

override LINK_$(PACKAGE)	+= $(PACKAGE)_standard.mk
ifneq ($(BINARIES)$(BINNAME),)
-include $(PACKAGE)/bin_$(PACKAGE).mk
ifdef DEBUG21
ERROR :=$(shell echo "BINNAME=$(BINNAME)  MAKEGOAL=$(MAKEGOAL)\t[s-2]" >& 2)
endif
-include $(PACKAGE)/bin_$(BINNAME).mk
  ifeq ($(BINNAME),)
  -include $(PACKAGE)/bin_$(MAKEGOAL).mk
  endif # ~ BINNAME
endif # BINARIES || BINNAME

# get root_*.pk for multi-package ROOT shared libraries  
ifneq ('',$(findstring root,$(MAKEGOAL)))
ifeq (1,$(shell if [ -r root_$(PACKAGE).mk ]; then echo 1; fi))
  include root_$(PACKAGE).mk	# for multi-package root shared libraries
  override REAL_LINK_ROOT += $(PACKAGE)
  override LINK_$(word 1,$(CINTPACKAGES)) += $(PACKAGE)
  override LINK_$(word 2,$(CINTPACKAGES)) += $(PACKAGE)
  override LINK_$(word 3,$(CINTPACKAGES)) += $(PACKAGE)
  override LINK_$(word 4,$(CINTPACKAGES)) += $(PACKAGE)
  override LINK_$(word 5,$(CINTPACKAGES)) += $(PACKAGE)
  override LINK_$(word 6,$(CINTPACKAGES)) += $(PACKAGE)
  override LINK_$(word 7,$(CINTPACKAGES)) += $(PACKAGE)
  override LINK_$(word 8,$(CINTPACKAGES)) += $(PACKAGE)
  override LINK_$(word 9,$(CINTPACKAGES)) += $(PACKAGE)
  override LINK_$(word 10,$(CINTPACKAGES)) += $(PACKAGE)
  override LINK_$(word 11,$(CINTPACKAGES)) += $(PACKAGE)
  override LINK_$(word 12,$(CINTPACKAGES)) += $(PACKAGE)
  override LINK_$(word 13,$(CINTPACKAGES)) += $(PACKAGE)
  override LINK_$(word 14,$(CINTPACKAGES)) += $(PACKAGE)
  override LINK_$(word 15,$(CINTPACKAGES)) += $(PACKAGE)
  override LINK_$(word 16,$(CINTPACKAGES)) += $(PACKAGE)
  override LINK_$(word 17,$(CINTPACKAGES)) += $(PACKAGE)
  override LINK_$(word 18,$(CINTPACKAGES)) += $(PACKAGE)
  override LINK_$(word 19,$(CINTPACKAGES)) += $(PACKAGE)
  override LINK_$(word 20,$(CINTPACKAGES)) += $(PACKAGE)
  override LINK_$(word 21,$(CINTPACKAGES)) += $(PACKAGE)
  override LINK_$(word 22,$(CINTPACKAGES)) += $(PACKAGE)
  override LINK_$(word 23,$(CINTPACKAGES)) += $(PACKAGE)
  override LINK_$(word 24,$(CINTPACKAGES)) += $(PACKAGE)
  override LINK_$(word 25,$(CINTPACKAGES)) += $(PACKAGE)
  override LINK_$(word 26,$(CINTPACKAGES)) += $(PACKAGE)
  override LINK_$(word 27,$(CINTPACKAGES)) += $(PACKAGE)
  override LINK_$(word 28,$(CINTPACKAGES)) += $(PACKAGE)
  override LINK_$(word 29,$(CINTPACKAGES)) += $(PACKAGE)
  override LINK_$(word 30,$(CINTPACKAGES)) += $(PACKAGE)
  override LINK_$(word 31,$(CINTPACKAGES)) += $(PACKAGE)
  override LINK_$(word 32,$(CINTPACKAGES)) += $(PACKAGE)
  override LINK_$(word 33,$(CINTPACKAGES)) += $(PACKAGE)
  override LINK_$(word 34,$(CINTPACKAGES)) += $(PACKAGE)
  override LINK_$(word 35,$(CINTPACKAGES)) += $(PACKAGE)
  override LINK_$(word 36,$(CINTPACKAGES)) += $(PACKAGE)
  override LINK_$(word 37,$(CINTPACKAGES)) += $(PACKAGE)
  override LINK_$(word 38,$(CINTPACKAGES)) += $(PACKAGE)
  override LINK_$(word 39,$(CINTPACKAGES)) += $(PACKAGE)
  override LINK_$(word 40,$(CINTPACKAGES)) += $(PACKAGE)
  override LINK_$(word 51,$(CINTPACKAGES)) += $(PACKAGE)
  override LINK_$(word 52,$(CINTPACKAGES)) += $(PACKAGE)
  override LINK_$(word 53,$(CINTPACKAGES)) += $(PACKAGE)
  override LINK_$(word 54,$(CINTPACKAGES)) += $(PACKAGE)
  override LINK_$(word 55,$(CINTPACKAGES)) += $(PACKAGE)
  override LINK_$(word 56,$(CINTPACKAGES)) += $(PACKAGE)
  override LINK_$(word 57,$(CINTPACKAGES)) += $(PACKAGE)
  override LINK_$(word 58,$(CINTPACKAGES)) += $(PACKAGE)
  override LINK_$(word 59,$(CINTPACKAGES)) += $(PACKAGE)
  override LINK_$(word 60,$(CINTPACKAGES)) += $(PACKAGE)
ifneq (, $(shell if [ `expr $(words $(CINTPACKAGES)) \> 60` -gt 0 ]; then echo 1; fi))
    ERROR := $(shell echo "Error: only 60 packages are supported in standard.mk, ask PC of SRT to increase the limit" >&2)
    exit 1
endif
endif # root_<pkg>.mk
endif # root

# include the section which generates additional dependencies and
# pseudo-dynamic loading code for conditions. This must be right above 
# link_all_reco.mk
include SoftRelTools/cdb_dynamic_load.mk

# build a complete dependency tree and include need arch_spec_xyz files
include PackageList/link_all_reco.mk

#
###################################
## The end of CDB specific section
####################################


############################################################
# Architecture Specific Information
############################################################
include SoftRelTools/arch_spec.mk
ifeq (, $(findstring $(MAKEGOAL),cleanarch clean))
  include SoftRelTools/compiler_check.mk
endif

############################################################
# Determine is we need any products which are not installed
############################################################
include SoftRelTools/optional_products.mk

############################################################
# Standard wildcards and filters
############################################################
MANPAGES := $(wildcard *.[1-8])
LIBCCFILES := $(filter-out $(BINCCFILES) $(LIBTMPLFILES) $(LIBREMOVEFILES) $(LIBCCOBJS), $(wildcard *$(CC_SUFFIX))) $(LIBEXTRACCFILES)
LIBFFILES  := $(filter-out $(BINFFILES) $(LIBREMOVEFILES),$(wildcard *.F) $(wildcard *.f)) $(LIBEXTRAFFILES)
LIBCFILES  := $(filter-out $(BINCFILES) $(LIBREMOVEFILES),$(wildcard *.c)) $(LIBEXTRACFILES)
LIBIDLFILES := $(filter-out $(LIBIDLORDERED) $(LIBREMOVEFILES),$(wildcard *.idl)) $(LIBEXTRAIDLFILES) $(LIBIDLORDERED)

ifneq (,$(findstring Objy,$(OPTIONS)))
  LIBDDLFILES := $(filter-out $(LIBDDLORDERED) $(LIBREMOVEFILES),$(wildcard *.ddl)) $(LIBEXTRADDLFILES) $(LIBDDLORDERED)
endif

LIBPYFILES := $(filter-out $(LIBREMOVEFILES) $(BINSCRIPTS),$(wildcard *.py))
TESTSCRIPTS := $(wildcard *.t)
BINBIGFFILES := $(filter %.F, $(BINFFILES))
BINSMALLFFILES := $(filter %.f, $(BINFFILES))

############################################################
# More standard definitions

ifeq ($(CXXAR),)
    CXXAR = $(AR) r$(ARFLAGS)	
endif

# for historical reasons, LIBDDLFILES is in reverse order
ifneq (,$(findstring Objy,$(OPTIONS)))
  LIBDDLFILESrev := $(shell str=; for var in xx $(LIBDDLFILES); do if [ "$$var" != "xx" ]; then str="$$var $$str"; fi; done; echo $$str)
endif

# standard definitions for external symbols
SHELL = /bin/sh

ifeq ($(strip $(F90C)),)
ifneq (,$(findstring AIX,$(BFARCH)))
    F90C = xlf90
else
    F90C = f90
endif
endif

curdir := $(shell /bin/pwd)
moddir := $(libdir)$(F90C)/

#__________________________________________________________________________
#-> Get idl to really work with TAO.
#   The user needs to set IDLFILES which should contain the list
#   of idl files. They are then processed according to the _idl.d
#   rule below.  LIBIDLCCFILES is determined as below and used.
#   Sridhara Dasu 2 July 1998
override LIBIDLCCFILES += $(LIBIDLFILES:%.idl=%C.cc)
override LIBIDLCCFILES += $(LIBIDLFILES:%.idl=%S.cc)
override LIB2IDLCCFILES += $(LIB2IDLFILES:%.idl=%C.cc)
override LIB2IDLCCFILES += $(LIB2IDLFILES:%.idl=%S.cc)

#--> This is an attempt to put AppUserBuild.o type files in tmp/$BFARCH/Pkg
#   CC_SUFFIX can be set to .cc or .cxx or .cpp
BINOFILES := $(BINCCFILES:$(CC_SUFFIX)=.o) $(BINCFILES:.c=.o) $(BINBIGFFILES:.F=.o) \
	$(BINSMALLFFILES:.f=.o)

override F90CFLAGS += -I$(libdir)$(F90C)

#__________________________________________________________________________
# all references from here on to $(LIB) should get a fully qualified name
# but not if no library is needed

ifneq (,$(strip $(LIBTMPLFILES)$(LIBDDLFILES)$(LIBIDLFILES)$(LIBCCFILES)$(LIBCFILES)$(LIBFFILES)$(LIBF90FILES)$(LIBCCSTATICFILES)$(LIBDDLSTATICFILES)$(LIBIDLCCSTATICFILES)))

ifeq (,$(findstring -Skiplib,$(OPTIONS)))
ifndef LIB
  override LIB := lib$(PACKAGE).a
endif
  override LIB := $(notdir $(LIB))
endif #need library
ifdef LIB
  override LIBNAME := $(notdir $(LIB))
  override LIB := $(libdir)$(LIBNAME)
endif #LIB
ifdef LIB2 
  override LIB2NAME := $(notdir $(LIB2))
  override LIB2 := $(libdir)$(LIB2NAME)
endif #LIB2
endif   # Skiplib

.PHONY: SHOBJ SHOBJ2 all allbin bin check clean corba doc include lib man mod installdirs prunebin pruneextrabin schema test extrabin extrabinscripts binscripts python wLIB wLIB2 wSHOBJ wSHOBJ2 wall wallbin wbin wbincomp wcheck wclean wcleanarch wcorba winc wlib wlib2 wshlib wshlib2 wslib wtest wextrabin wextrabinscripts wbinscripts wsiteinstall wldlink preprocess assembly

#__________________________________________________________________________
#++ SETSRC: set srcdir to current source directory then cd to $(workdir)
override define SETSRC
	srcdir=$(<D); if [ "$(<D)" = "." ]; then srcdir=`/bin/pwd`; fi; \
	if [ `expr "$(OPTIONS)" : ".*Shared.*" ` -gt 0 ]; then \
	    cd $(shworkdir); else cd $(workdir); fi
endef

#__________________________________________________________________________
#++ CPPFLAGSmod: insert -Isrcdir to CPPFLAGS
override define CPPFLAGSmod
$(patsubst -I$(TOPDIR)/tmp/$(BFARCH),-I$$srcdir -I$(TOPDIR)/tmp/$(BFARCH), $(CPPFLAGS))
endef

#__________________________________________________________________________
#++ macro used by w<..> targets to find the relative path to TOPDIR.
define wecho
	if [ "$(DEBUG03)" != "" ]; then set -x; fi; \
	pwd=`/bin/pwd`; topdir=`dirname $(TOPDIR)`; export pwd; export topdir; \
	remark="pwd: $$pwd"; \
	dir=`expr $$pwd : ".*/packages\(.*\)"`; \
	if [ "$$dir" != "" ]; then \
	    remark="curdir is in package area dir=$$dir"; \
	    OIFS=$$IFS; IFS=/; set $$dir; IFS=$$OIFS; top=$$1; \
	    if [ $$# -ge 2 ] ; then shift; shift; fi; \
	else \
	    remark="curdir is not in package area"; \
	    dir=`expr $$pwd : "$$topdir/\(.*\)"`; \
	    if [ "$$dir" != "" ]; then \
	    	remark="topdir/pkg: $$dir"; \
	    	OIFS=$$IFS; IFS=/; set $$dir; IFS=$$OIFS; shift; top=$$1;shift;\
	    else \
		remark="dir still empty"; \
		top=`basename $$pwd`; \
	    fi; \
	fi; \
	dir=$$top; \
	while [ $$# -ge 1 ]; do dir="$$dir.$$1"; shift; done 
endef

#__________________________________________________________________________
#++ wdir: echo "-> make <path>.<target>:" from a bourne-shell.
#   It is activated when a target is buit in a sub-make.

define wdir
    if [ "$(findstring Showdir, $(OPTIONS))" ]; then \
	$(wecho); \
	echo "-> make $$dir.$@"; \
    fi
endef

#__________________________________________________________________________
#++ 'w<target>': simply echo "-> <path>.<target>:". Explciit target.

wall wbin wbincomp wextrabin wextrabinscripts wbinscripts wsiteinstall wbintest walltest wcheck wclean wcorba winc wlib wlib2 wLIB wLIB2 wshlib wshlib2 wSHOBJ wSHOBJ2 wslib wtest wldlink:
	@if [ -z "$(findstring Showdir, $(OPTIONS))" ]; then exit; fi; \
	$(wecho); \
	echo "-> $$dir.$(shell echo $@ | cut -c2-):"; \
	if [ "$(DEBUG08)" != "" ]; then \
	    echo "++ srcdir=$(srcdir) PACKAGE=$(PACKAGE) [DEBUG08=1]"; \
	fi;

##############################################################################
#++ standard targets

# This variable is set in optional_products.mk
ifeq (,$(OPTIONALMISSING))

# NOBIN means skip bin target
bin:=bin
ifdef NOBIN
    bin := 
endif	

#__________________________________________________________________________
#-> if pkg is on binonly_sunos5, build 'bin' on SunOS5 only.

ifneq (,$(filter $(PACKAGE), $(binonly_sunos5)))
ifeq (,$(findstring SunOS5,$(BFARCH)))
ifdef LOOPPKG
    bin :=
    NOBIN := 1
endif  ## LOOPPKG
endif  ## ~SunOS5
ifdef DEBUG17
    ERROR := $(shell echo "bin=$(bin) NOBIN=$(NOBIN) PACKAGE=$(PACKAGE) binonly_sunos5=$(binonly_sunos5) LOOPPKG=$(LOOPPKG) \t[s-3]" >& 2)
endif   ## DEBUG17
endif   ## binonly_sunos5

#__________________________________________________________________________
#-> all: make sure this is the first (default) target in file.

all:	wall include corba lib $(bin)
	@$(MAKE) check

# do not make .d files for installdirs
ifneq (,$(findstring installdirs,$(MAKEGOAL))$(findstring clean,$(MAKEGOAL)))
    override OPTIONS:=$(OPTIONS)-Skipdfile	
endif

installdirs:
	@$(wdir); cd $(TOPDIR); $(MAKE) installdirs PACKAGES=$(PACKAGE) ONEDIR=1

schema:	$(LIBDDLFILES) corba
	@echo schema stage done in $(curdir)

inc:	winc $(INC)  $(foreach v,$(SUBDIRS),$v.inc)
	@echo inc stage done in $(curdir)

corba:  wcorba $(LIBIDLFILES) $(foreach v,$(SUBDIRS),$v.corba)
	@echo corba stage done in $(curdir)

#-> lib target for Static library
ifeq (,$(findstring Shared,$(OPTIONS)))

lib:	wlib $(foreach V,$(LIBCCOBJS),$(libdir)$(V:$(CC_SUFFIX)=.o)) $(LIB) $(LIB2) $(foreach v,$(SUBDIRS),$v.lib) templates checkmain python
	@echo lib stage done in $(curdir)

libext:
	$(MAKE) libext OPTIONS+=-Shared

endif

#__________________________________________________________________________
# checkmain: check main() in libraries

override define CheckMain
	if [ "$(LIB)" -a -r "$(LIB)" ]; then \
	    if [ "$(DEBUG25)" ]; then set -x ; fi; \
	    if [ "$(findstring MacOS,$(BFARCH))" ]; then \
		IS=`$(NM) $(LIB) | grep 'T _main'`; \
		if [ "$$IS" ]; then \
		    OIFS=$$IFS; \
		    IFS=" "; \
		    for ISS in $$IS; do \
		    	IFS=$$OIFS; \
		    	MOD=`expr "$$ISS" : '.*\.a\:\(.*\)\:.*'`; \
	    	    	if [ "$$MOD" != "" -a `expr "$(keep_main)" : ".*$$LIBNAME\[$$MOD\].*" ` -eq 0 ]; then \
		    	    echo "[Warning]: $$MOD is removed from $(LIBNAME) since it has main() and it's not in keep_main list"; \
		    	    $(AR) -d $(LIB) $$MOD; \
		    	fi; \
		    done; \
		fi; \
	    else \
		IS=`$(NM) $(LIB) | grep ': main '`; \
		if [ "$$IS" ]; then \
		    OIFS=$$IFS; \
		    IFS=":"; \
		    for ISS in $$IS; do \
		    	IFS=$$OIFS; \
		    	MOD=`expr "$$ISS" : '.*\.a\[\(.*\)\].*'`; \
	    	    	if [ "$$MOD" != "" -a `expr "$(keep_main)" : ".*$$LIBNAME\[$$MOD\].*" ` -eq 0 ]; then \
		    	    echo "[Warning]: $$MOD is removed from $(LIBNAME) since it has main() and it's not in keep_main list"; \
		    	    $(AR) -d $(LIB) $$MOD; \
		    	fi; \
		    done; \
	    	fi; \
	    fi; \
	fi; \
	if [ "$(LIB2)" -a -r "$(LIB2)" ]; then \
	    IS2=`$(NM) $(LIB2) | grep ': main '`; \
	    if [ "$(DEBUG25)" ]; then set -x ; fi; \
	    if [ "$$IS2" ]; then \
		OIFS=$$IFS; \
		IFS=":"; \
		for ISS2 in $$IS2; do \
		    IFS=$$OIFS; \
		    MOD2=`expr "$$ISS2" : '.*\.a\[\(.*\)\].*'`; \
	    	    if [ "$$MOD2" != "" -a `expr "$(keep_main)" : ".*$$LIB2NAME\[$$MOD2\].*" ` -eq 0 ]; then \
		    	echo "[Warning]: $$MOD2 is removed from $(LIB2NAME) since it has main() and it's not in keep_main list"; \
		    	$(AR) -d $(LIB2) $$MOD2; \
		    fi; \
		done; \
	    fi; \
	fi; \
	if [ -s "$(LIB)" -a "$(RANLIB)" != "" ]; then \
	    REMARK="$(RANLIB) $(LIB)  [ran-1]"; \
	    $(RANLIB) $(LIB) 1>&2; \
	fi;
endef

checkmain:
	$(CheckMain)

#__________________________________________________________________________
# templates

templates:
ifneq (,$(LIB))
ifneq (,$(templatedir))
ifneq (,$(findstring OSF,$(BFARCH)))
	if [ -d $(templatedir) ]; then \
		echo Adding template files to library; \
		cd $(templatedir); \
		$(AR) -t $(LIB) >/tmp/lib$(PACKAGE).lis$$$$; \
		ls | diff -bw - /tmp/lib$(PACKAGE).lis$$$$ | \
			grep \< | awk '{print $$2}' | \
			xargs $(AR) qz$(ARFLAGS) $(LIB); \
		rm -f /tmp/lib$(PACKAGE).lis$$$$; \
		$(AR) -rs $(LIB); \
	fi
else
ifneq (,$(findstring Linux,$(BFARCH)))
ifneq (,$(BBR_USE_REPOSITORY))
	if [ -d $(templatedir) ]; then \
		echo Adding template files to library; \
		cd $(objdir); \
		$(AR) x $(LIB); \
		$(CXX) -frepo *.o; \
		$(AR) r$(ARFLAGS) $(LIB) *.o; \
		$(RM) *.o; \
	fi
endif # BBR_USE_REPOSITORY
endif # Linux
endif # OSF
endif # has templatedir
endif # making a LIB

#__________________________________________________________________________
# slib

slib:	wslib $(SLIB) $(foreach v,$(SUBDIRS),$v.lib)
	@echo slib stage done in $(curdir)

ifneq (1,$(NOBIN))
bin: 	wbin $(foreach v,$(BINOFILES),$(workdir)$v)
	for target in $(foreach v,$(filter-out $(BINSCRIPTS),$(BINS)),$(bindir)$v) xx; do \
		if [ $$target != xx ]; then \
			$(MAKE) $$target BINNAME=`basename $$target`; \
		fi; \
		done
	@$(MAKE) rbin
	@echo bin stage done in $(curdir)
endif

rbin: binscripts prunebin pruneextrabin $(foreach v,$(SUBDIRS),$v.bin)

bincomp: wbincomp $(foreach v,$(filter-out $(foreach f,$(EXTRABINS),$(f).o),$(BINOFILES)),$(workdir)$v) $(foreach v,$(SUBDIRS),$v.bincomp)
	@echo bincomp stage done in $(curdir)

preprocess: $(foreach V,$(LIBCCFILES),$(workdir)$(V:$(CC_SUFFIX)=.ii))
	@echo preprocess stage done in $(curdir)

assembly: $(foreach V,$(LIBCCFILES),$(workdir)$(V:$(CC_SUFFIX)=.s))
	@echo assembly stage done in $(curdir)

#__________________________________________________________________________
#++ prune binaries to save space

prunebin:
    ifneq (,$(findstring Prunebin,$(OPTIONS)))
    ifdef PACKAGE
    ifdef BINS
	if [ "$(DEBUG07)" ];then set -x; fi; \
	remark="PACKAGE=$(PACKAGE) BINS=$(BINS)"; \
	if [ "$(keep_bin_opt)" = "ERROR" ]; then \
	    echo "error in processing $(PACKAGE)/binlist"; \
	    exit 1; \
	fi; \
	prunebins=; \
	if [ "$(filter $(keep_pkg),$(PACKAGE))" ]; then \
	    prunebins="$(filter $(nokeep_bin),$(BINS))"; \
	else \
	    prunebins="$(filter-out $(keep_bin),$(BINS))"; \
	fi; \
	remark="list of binaries to be pruned: prunebins=$$prunebins"; \
	KBINS=`/usr/local/bin/perl -e '$$bindir=$$ARGV[0]; shift; foreach $$fn (@ARGV) {$$file="$$bindir$$fn"; if ( -B $$file ) {print "$$fn "}}' $(bindir) $$prunebins`; \
	if [ -n "$$KBINS" ]; then \
	    remark="non-script binary pruning list: KBINS=$$KBINS"; \
	    echo "Running regression test..  [prunebin-1]"; \
	    $(MAKE) test; \
	    echo "remove '$$KBINS' from package $(PACKAGE) [prunebin-2]"; \
	    cd $$bindir; $(RM) $$KBINS; touch $$KBINS; \
	fi;
    endif
    endif
    endif

	-if [ -r "$(bindir)/Index" ]; then \
	    echo "$(foreach v,$(BINS),$(PACKAGE):  $v\n)" >> "$(bindir)/Index"; \
	fi	

	#-> strip the executable for production build
	if [ "${HPBUILD}" = yes   -o "${AIXBUILD}" = yes -o "${OSFBUILD}" = yes ] ; then  \
	    /usr/local/bin/perl -e '$$bindir=$$ARGV[0]; shift; foreach $$fn (@ARGV) {$$file="$$bindir$$fn"; if ( -B $$file && -w $$file && -f $$file) {system("strip $$file > /dev/null 2>&1"); exit 0;}}' $(bindir) $(BINS); \
	fi;

pruneextrabin:
    ifneq (,$(findstring Prunebin,$(OPTIONS)))
    ifdef PACKAGE
    ifdef EXTRABINS
	remark="prune binaries. PACKAGE=$(PACKAGE) EXTRABINS=$(EXTRABINS)"; \
	prunebins=; \
	if [ "$(filter $(keep_pkg),$(PACKAGE))" ]; then \
	    prunebins="$(filter $(nokeep_bin),$(EXTRABINS))"; \
	else \
	    prunebins="$(filter-out $(keep_bin),$(EXTRABINS))"; \
	fi; \
	remark="list of binaries to be pruned: prunebins=$$prunebins"; \
	KBINS=`/usr/local/bin/perl -e '$$bindir=$$ARGV[0]; shift; foreach $$fn (@ARGV) {$$file="$$bindir$$fn"; if ( -B $$file ) {print "$$fn "}}' $(bindir) $$prunebins`; \
	if [ -n "$$KBINS" ]; then \
	    remark="non-script binary pruning list: KBINS=$$KBINS"; \
	    echo "Running regression test..  [prunebin-3]"; \
	    $(MAKE) test; \
	    echo "remove '$$KBINS' from package $(PACKAGE) [prunebin-4]"; \
	    cd $$bindir; $(RM) $$KBINS; touch $$KBINS; \
	fi;	
	@echo pruneextrabin stage done in $(curdir)
    endif
    endif
    endif

#__________________________________________________________________________
# allbin: build all binaries

allbin: wallbin extrabin bin extrabinscripts
	@echo allbin stage done in $(curdir)

# Python byte-code repository
pydir := $(libdir)python/
pydirbbr := $(pydir)BaBar/
pydirpkg := $(pydirbbr)$(PACKAGE)/

ifneq (,$(LIBPYFILES)$(BINSCRIPTS)$(EXTRABINSCRIPTS))

# Bring in Python support
include SoftRelTools/arch_spec_Python.mk

# define python target, depends on whether Python is available or no.
# Also when siteinstall is performed we need unconditionally update
# all links, or remove repository completely if Python is unavalable.
ifeq ($(Python_AVAILABLE),yes)

ifneq (,$(LIBPYFILES))

LIBPYCFILES := $(addprefix $(pydirpkg),$(LIBPYFILES:.py=.pyc))
LIBPYOFILES := $(addprefix $(pydirpkg),$(LIBPYFILES:.py=.pyo))

ifneq ($(LIBPYCFILES),)
ifdef SITEINSTALL
# force re-linking and re-compilation of everything when siteinstall runs
$(LIBPYCFILES) : SiteInstall
$(addprefix $(pydirpkg),$(LIBPYFILES)) : SiteInstall
endif
endif

python:
	@echo python stage done in $(curdir)

python: $(LIBPYCFILES) $(LIBPYOFILES)
python: $(pydirpkg)__init__.pyc $(pydirpkg)__init__.pyo
python: $(pydirbbr)__init__.pyc $(pydirbbr)__init__.pyo


else # LIBPYFILES

python:

endif # LIBPYFILES

else # ifeq ($(Python_AVAILABLE),yes)

#redefine PYTHONBIN to get a message from bbrpywrapper
PYTHONBIN = 

ifdef SITEINSTALL

# remove python stuff 
python:
	if [ -d $(pydirbbr) ] ; then \
	  echo "Python is not available, removing python byte-code" ;\
	  rm -rf $(pydirbbr) ;\
	fi

else # ifdef SITEINSTALL

# do nothing
python:

endif # SITEINSTALL
endif # Python_AVAILABLE
endif # LIBPYFILES or BINSCRIPTS or EXTRABINSCRIPTS


ifneq (,$(BINSCRIPTS)$(EXTRABINSCRIPTS))

# Bring in perl support
include SoftRelTools/arch_spec_PERL.mk

# Bring in MySQL support
include SoftRelTools/arch_spec_mysql.mk

# Specify the list of names eligible for "script mangling" - the substitution
# of strings of the form @NAME@ with the value of the gmake variable name,
# evaluated at the time of the build.  This allows "freezing in" values of
# particular variables that are set in the build, rather than leaving the
# scripts dependent on the transient run-time values of environment variables.

SEDFILTER := s%@PERL@%$(PERL)%g; \
	s%@PERL64@%$(PERL64)%g; \
	s:@PYTHON@:$(PYTHON):g; \
	s:@PYTHONBIN@:$(PYTHONBIN):g; \
	s%@PYEXTDIRS@%$(PYEXTDIRS)%g; \
	s:@BFARCH@:$(BFARCH):g; \
	s:@PACKAGE@:$(PACKAGE):g; \
	s:@PACKDIR@:$(PWD):g; \
	s:@ACE_ROOT@:$(ACE_ROOT):g; \
	s:@JASROOT@:$(JASROOT):g; \
	s:@JASVER@:$(JASVER):g; \
	s:@DIMVER@:$(DIMVER):g; \
	s:@SMIVER@:$(SMIVER):g; \
	s:@BBRROOT_TCLDIR@:$(BBRROOT_TCLDIR):g; \
	s:@BBRROOT_TCLNAME@:$(BBRROOT_TCLNAME):g; \
	s:@ROOTVER@:$(ROOTVER):g; \
	s:@ROOTSYS@:$(BBRROOTSYS):g; \
	s:@CERNPATH@:$(CERNPATH):g; \
	s:@OBJYVER@:$(OBJY_VERSION):g; \
	s:@OBJYBASE@:$(OBJYBASE):g; \
	s:@OBJYARCH@:$(OBJY_ARCH):g; \
	s:@CC@:$(CC):g; \
	s:@NM@:$(NM):g; \
	s:@cdev_VER@:$(cdev_VER):g; \
	s:@MYSQL_DIR@:$(MYSQL_DIR):g

endif # BINSCRIPTS or EXTRABINSCRIPTS

ifneq (,$(BINSCRIPTS))

ifdef SITEINSTALL
.PHONY:  SiteInstall
$(addprefix $(bindir),$(BINSCRIPTS)) : SiteInstall
endif
$(addprefix $(bindir),$(BINSCRIPTS)) : $(bindir)%:%
	@echo "Mangling $(@F) to binary directory [binscripts-1]"
	$(RM) -f $@
	sed  "$(SEDFILTER)" $(filter-out SiteInstall,$^) > $@
	chmod +x $@

endif # BINSCRIPTS

ifneq (,$(EXTRABINSCRIPTS))
$(addprefix $(bindir),$(EXTRABINSCRIPTS)) : $(bindir)%:%
	@echo "Mangling $(@F) to binary directory [extrabinscripts-1]"
	sed  "$(SEDFILTER)" $^ > $@
	chmod +x $@

endif # EXTRABINSCRIPTS

check: wcheck

# for backward compatibility, treat all .t files in the package as
# test scripts if $(TESTSCRIPTS) is not defined
ifeq "$(origin TESTSCRIPTS)" "undefined"
  TESTSCRIPTS = $(wildcard *.t)
endif

test:	wtest $(foreach v,$(TESTSCRIPTS),$(testdir)$(v:.t=.pass)) $(foreach v,$(SUBDIRS),$v.test)
	@echo test stage done in $(curdir)

man: 	$(foreach v,$(filter $(wildcard *.1),$(MANPAGES)),$(mandir)man1/$v)
man: 	$(foreach v,$(filter $(wildcard *.2),$(MANPAGES)),$(mandir)man2/$v)
man: 	$(foreach v,$(filter $(wildcard *.3),$(MANPAGES)),$(mandir)man3/$v)
man: 	$(foreach v,$(filter $(wildcard *.4),$(MANPAGES)),$(mandir)man4/$v)
man: 	$(foreach v,$(filter $(wildcard *.5),$(MANPAGES)),$(mandir)man5/$v)

doc: 	$(foreach v,$(DOCS),$(docdir)$v)

dep:
	BINCCFILES=$(BINCCFILES); export BINCCFILES; \
	PACKAGE=$(PACKAGE); export PACKAGE; \
	workdir=$(workdir); export workdir; \
	if [ -x ../SoftRelTools/SRTpkgListDepend ]; then \
		../SoftRelTools/SRTpkgListDepend; \
	else \
		$(BFDIST)/releases/$(BFCURRENT)/SoftRelTools/SRTpkgListDepend; \
	fi
	@echo dep stage done in $(curdir)

$(foreach v,$(filter $(wildcard *.1),$(MANPAGES)),$(mandir)man1/$v) : $(MANPAGES)
	rm -f $@
	if [ ! -d $(@D) ] ; then mkdir $(@D); fi
	cp $(@F) $@
	chmod 444 $@

$(foreach v,$(filter $(wildcard *.2),$(MANPAGES)),$(mandir)man2/$v) : $(MANPAGES)
	rm -f $@
	if [ ! -d $(@D) ] ; then mkdir $(@D); fi
	cp $(@F) $@
	chmod 444 $@

$(foreach v,$(filter $(wildcard *.3),$(MANPAGES)),$(mandir)man3/$v) : $(MANPAGES)
	rm -f $@
	if [ ! -d $(@D) ] ; then mkdir $(@D); fi
	cp $(@F) $@
	chmod 444 $@

$(foreach v,$(filter $(wildcard *.4),$(MANPAGES)),$(mandir)man4/$v) : $(MANPAGES)
	rm -f $@
	if [ ! -d $(@D) ] ; then mkdir $(@D); fi
	cp $(@F) $@
	chmod 444 $@

$(foreach v,$(filter $(wildcard *.5),$(MANPAGES)),$(mandir)man5/$v) : $(MANPAGES)
	rm -f $@
	if [ ! -d $(@D) ] ; then mkdir $(@D); fi
	cp $(@F) $@
	chmod 444 $@

$(foreach v,$(DOCS),$(docdir)$v): $(DOCS)
	rm -f $@
	if [ ! -d $(@D) ] ; then mkdir $(@D); fi
	cp $(@F) $@
	chmod 444 $@


#__________________________________________________________________________
# add doc and man to bin
#

bin:  doc man

linkfiles:
	@echo Creating link_$(PACKAGE).mk files...
ifeq ($(VERBOSE),)
	make-linkfiles
else
	make-linkfiles -v
endif

else # OPTIONALMISSING
bin binscripts extrabin extrabinscripts all schema inc corba lib preprocess assembly prunebin pruneextrabin allbin test man doc dep linkfiles:
	@echo Skipping $(PACKAGE)\'s $@ stage
endif # OPTIONALMISSING

#__________________________________________________________________________
# Java targets (simply pass it off to jstandard.mk in java subdirectory)
#
JAVATARGETS = jar jclasses jclean jenv jdoc jbin jlib jcorba java
.PHONY: $(JAVATARGETS)

$(JAVATARGETS):
	if [ -f java/GNUmakefile ] ; then $(MAKE) -C java $@ ; fi


#__________________________________________________________________________
# Special doc targets (simply pass it off to doctools.mk in doc subdirectory)
#
DOCTARGETS = guide reference lightcpp
.PHONY: $(DOCTARGETS)

guide:
	if [ -f doc/GNUmakefile ] ; then cd doc; $(MAKE) $(OVERRIDES) $@ ; fi

lightcpp: 
	echo LIGHT: $(filter -I%, $(CXX) $(CXXFLAGS) $(CPPFLAGS)) 
	echo LIGHT: $(filter -D%, $(CXX) $(CXXFLAGS) $(CPPFLAGS))
	-for subdir in $(SUBDIRS) NOTADIRECTORY ; do \
	  test -d "$$subdir" && $(MAKE) -C $$subdir lightcpp ; \
	done

#__________________________________________________________________________
#++ search paths
# Note that CPPFLAGS should already have the .c and .cc include search path
#    and LDFLAGS should have the library search path

# Because $(LIB) is a fully qualified target (i.e. pathname), we can
# allow searching for .a - otherwise, if $(libdir)$(LIB) had
# not already existed, we'd have found (and used as a dependency)
# the one refrenced via $BFCURRENT
#

vpath %.$(SOEXT) $(patsubst -L%,%,$(filter -L%,$(LDFLAGS)))

vpath %.a $(patsubst -L%,%,$(filter -L%,$(LDFLAGS)))

# These are so we can also find the dependacies for shared libraries.
#vpath %.tso $(patsubst -L%,%,$(filter -L%,$(LDFLAGS)))

# this vpath enable us omitting the $(workdir) in binary dependencies.
vpath %.o $(objdir) $(workdir)

vpath %.mod $(moddir)

# the following three are for finding .h, .inc and .mod files in .d lists
# make by awk, who does not put in full pathnames
vpath %.h   $(patsubst -I%,%,$(filter -I%,$(CPPFLAGS)))
vpath %.inc $(patsubst -I%,%,$(filter -I%,$(CPPFLAGS)))
vpath %.mod $(patsubst -I%,%,$(filter -I%,$(F90CFLAGS)))
##
## vpath for files which include other .F
##
vpath %.F   $(patsubst -I%,%,$(filter -I%,$(CPPFLAGS)))   

# note that we _dont_ search for .o and .d files - the rules are written
#   to explicitly look for and create them in workdir

#__________________________________________________________________________
#++ lib.a tries to build lib.a(*.o)

# collect *.o to library, use 'gmake -k' to make sure this target will
# be executed even if there is compilation error.    [Fastbuild-1]
ifdef fastbuild

#-> create obj directory for Fastbuild option.
CMD := $(shell if [ ! -d $(objdir) ]; then mkdir -p $(objdir); fi; \
	     if [ ! -d $(obj2dir) ]; then mkdir -p $(obj2dir); fi)

ifdef LIB

#++ collect $(objdir)/*.o left behind by ctrl-C. 
ERROR := $(shell cd $(objdir); \
	list=`find . -name '*.o' ! -size 0 -print`; \
	if [ -n "$$list" ]; then echo ".o files found in obj, collecting $(objdir) to $(LIB) \t[s-5]" >& 2;\
		$(CXXAR) $(LIB) $$list; \
		rm $(objdir)/*.o; \
		if [ -s "$(LIB)" -a "$(RANLIB)" != "" ]; then \
			REMARK="$(RANLIB) $(LIB)  [ran-2]"; \
			$(RANLIB) $(LIB) 1>&2; \
		fi; \
	fi;) 

col$(LIB):
	libname=`echo $@ | cut -c4-`; \
	cd $(objdir); \
	if [ -n "`ls|grep \.o`" ]; then \
	    echo "Collecting $(objdir)/*.o to library $$libname [lib-1]";\
	    $(CXXAR) $$libname *.o; \
	    rm $(objdir)/*.o; \
	fi
endif   ## LIB

ifdef LIB2 

#++ collect $(obj2dir)/*.o left behind by ctrl-C. 
ERROR := $(shell cd $(obj2dir); \
	list=`find . -name '*.o' ! -size 0 -print`; \
	if [ -n "$$list" ]; then echo ".o files found in obj2, collecting $(objdir) to $(LIB)  \t[s-6]" >& 2;\
		$(CXXAR) $(LIB2) $$list; \
		rm $(obj2dir)/*.o; \
	fi;) 

col$(LIB2):
	libname=`echo $@ | cut -c4-`; \
	cd $(obj2dir); \
	if [ -n "`ls`" ]; then \
	    echo "Collecting $(obj2dir)/*.o to library $$libname [lib-2]";\
	    $(CXXAR) $$libname *.o; \
	    echo "removing $(obj2dir)/*.o";\
	    rm $(obj2dir)/*.o; \
	    if [ -s "$$libname" -a "$(RANLIB)" != "" ]; then \
		REMARK="$(RANLIB) $$libname  [ran-3]"; \
		$(RANLIB) $$libname 1>&2; \
	    fi; \
	fi
endif   ## LIB2

endif   ## fastbuild

ifeq (,$(findstring Shared,$(OPTIONS)))
$(LIB): wLIB
  ifneq (,$(findstring Objy,$(OPTIONS)))
  ifneq (has_not_been_set,$(OBJYBASE))
$(LIB): $(foreach V,$(LIBDDLFILES),$(LIB)($(workdir)$(V:.ddl=_ddl.o)))
  endif
  endif
$(LIB): $(foreach V,$(LIBIDLCCFILES),$(LIB)($(workdir)$(V:$(CC_SUFFIX)=.o)))
$(LIB): $(foreach V,$(LIBCCFILES),$(LIB)($(V:$(CC_SUFFIX)=.o)))
$(LIB): $(foreach V,$(LIBCFILES),$(LIB)($(V:.c=.o)))
endif   # ~Shared 
$(LIB): $(foreach V,$(filter %.f,$(LIBFFILES)),$(LIB)($(V:.f=.o)))
$(LIB): $(foreach V,$(filter %.f90,$(LIBF90FILES)),$(LIB)($(V:.f90=.o)))
$(LIB): $(foreach V,$(filter %.F,$(LIBFFILES)),$(LIB)($(V:.F=.o)))
ifdef fastbuild
$(LIB): col$(LIB)
else
ifdef RANLIB
	if [ -s "$(libdir)$(@F)" -a "$(RANLIB)" != "" ]; then \
	    REMARK="$(RANLIB) $(libdir)$(@F)  [ran-4]"; \
	    $(RANLIB) $(libdir)$(@F) 1>&2; \
	fi
endif
endif   ## fastbuild

ifeq (,$(findstring Shared,$(OPTIONS)))
$(LIB2): wLIB2
  ifneq (,$(findstring Objy,$(OPTIONS)))
  ifneq (has_not_been_set,$(OBJYBASE))
$(LIB2): $(foreach V,$(LIB2DDLFILES),$(LIB2)($(workdir)$(V:.ddl=_ddl.o)))
  endif
  endif
$(LIB2): $(foreach V,$(LIB2IDLCCFILES),$(LIB2)($(workdir)$(V:$(CC_SUFFIX)=.o)))
$(LIB2): $(foreach V,$(LIB2CCFILES),$(LIB2)($(V:$(CC_SUFFIX)=.o)))
$(LIB2): $(foreach V,$(LIB2CFILES),$(LIB2)($(V:.c=.o)))
endif    # ~Shared
$(LIB2): $(foreach V,$(filter %.f,$(LIB2FFILES)),$(LIB2)($(V:.f=.o)))
$(LIB2): $(foreach V,$(filter %.f90,$(LIB2F90FILES)),$(LIB2)($(V:.f90=.o)))
$(LIB2): $(foreach V,$(filter %.F,$(LIB2FFILES)),$(LIB2)($(V:.F=.o)))
ifdef fastbuild
$(LIB2): col$(LIB2)
else
ifdef RANLIB
	if [ -s "$(libdir)$(@F)" -a "$(RANLIB)" != "" ]; then \
	    REMARK="$(RANLIB) $(libdir)$(@F)  [ran-5]"; \
	    $(RANLIB) $(libdir)$(@F) 1>&2; \
	fi
endif
endif  ## fastbuild

##########################################################

.SUFFIXES: 

.SUFFIXES:	.a .o .cc .cxx .cpp .c .f .F .mod .f90 .ddl .idl .lex .d .t. .T .pass .$(SOEXT)
# IDLFILES not used for cc, S. Dasu, July 1998
#.PRECIOUS:	$(IDLFILES:.idl=.ccstub.cc)
.PRECIOUS:	$(IDLFILES:.idl=.f90stub.f90) $(testdir)%.T
.PRECIOUS:	$(workdir)%_ddl.cc $(workdir)%_ddl.cpp $(workdir)%_ddl.cxx

#__________________________________________________________________________
#++ rules for TAO IDL files. 
# Should update the "touch $@;" below with appropriate rule to allow inclusion
# of idl files within other idl files.  Can be done using makedepend with
# "touch $@; makedepend -o .idl -f $@ $<;"
# However, I am not sure how to do this using gcc - will work on it,
# nor do I know if makedepend can be used - for now dependencies of
# included idl files within idl files do not work properly
# - Sridhara Dasu, July 98

# The TAO IDL compiler generates incorrect #include statements when an IDL
# file uses classes from another IDL file in a different package. The package
# name is lost in the generated #include statements, resulting in inclusion of
# header files that cannot be located. To fix this, a sed filter is generated
# by examining #include statements in the IDL sources and then applying it
# to the generated .h, .cc, and .i files to fix the #include lines there.

# automatically-generated sed filter to fix #include statements in generated
# source files
CPPIDLSED = $(workdir)IDL-cpp.sed

# temporary file used when fixiing generated source files
CPPIDLSEDTMP = $(workdir)tmpidl

# NOTE that recent versions of ACE generate *[CS]_T.{cpp,i,h} files as well
# as the the files specified in "for srcfile in..." below. It may be
# necessary to run the IDL sed filter on them at some point in the future.
# However the extensions should first be changed to comply with BaBar
# conventions.                                             samuel 1998-11-5

ifeq (,$(findstring Shared,$(OPTIONS)))
$(workdir)%_idl.d:	%.idl $(CPPIDLSED)
	if test -f $< ; then \
		if [ ! -z "$(findstring $<, $(LIBIDLFILES))" ]; then \
			echo "Making $(@F) [from $(<F)] [idl1]"; \
			rm -f $(workdir)$<; \
			cp $< $(workdir); \
			$(SETSRC); \
			touch $@; \
			$(CCFIDLE) $(CCIDLFLAGS) $(IDLINCLFLAGS) $<; \
			rm -f $(CPPIDLSEDTMP); \
			for srcfile in $*S.cc $*S.h $*S.icc \
	                               $*C.cc $*C.h $*C.icc; \
			do \
			  sed -f $(CPPIDLSED) $$srcfile > $(CPPIDLSEDTMP); \
			  mv $(CPPIDLSEDTMP) $$srcfile; \
			done \
		fi \
	fi

else
CPPIDLSED = $(shworkdir)IDL-cpp.sed
CPPIDLSEDTMP = $(shworkdir)tmpidl
$(shworkdir)%_idl.d:	%.idl $(CPPIDLSED)
	if test -f $< ; then \
		if [ ! -z "$(findstring $<, $(LIBIDLFILES))" ]; then \
			echo "Making $(@F) [from $(<F)] [shared-idl1]"; \
			cp $< $(shworkdir); \
			cd $(shworkdir); \
			touch $@; \
			$(CCFIDLE) $(CCIDLFLAGS) $(IDLINCLFLAGS) $<; \
			rm -f $(CPPIDLSEDTMP); \
			for srcfile in $*S.cc $*S.h $*S.icc \
	                               $*C.cc $*C.h $*C.icc; \
			do \
			  sed -f $(CPPIDLSED) $$srcfile > $(CPPIDLSEDTMP); \
			  mv $(CPPIDLSEDTMP) $$srcfile; \
			done \
		fi \
	fi
endif

# Automatically generate a sed filter which fixes incorrect #include
# statements. It is generated using another sed filter, called
# idlfilter-cpp.sed, which lives in SoftRelTools.

ifneq (,$(wildcard $(TOPDIR)/SoftRelTools))
  CPPIDLFILTERSRCPATH = $(TOPDIR)/SoftRelTools/
else
  CPPIDLFILTERSRCPATH = $(BFDIST)/releases/$(BFCURRENT)/SoftRelTools/
endif

$(CPPIDLSED): $(LIBIDLFILES)
	echo Making include sed filter from IDL; \
	rm -f $@ ; \
	echo \# sed filter to fix include statements \
                automatically generated by standard.mk >> $@ ; \
	for idlfile in $(LIBIDLFILES); \
	do \
	  sed -f $(CPPIDLFILTERSRCPATH)/idlfilter-cpp.sed $$idlfile >> $@; \
	done

###########################################################################
#++ rules for shared libraries.
#   build non-shared *.o in lib.a. Build shared *.o in shobj/*.o then
#   convert them to lib_<rel>.$(SOEXT).

  ifneq ($(LIB),)
    override SHLIB = $(shlibdir)$(LIBNAME:.a=_$(BFCURRENT).$(SOEXT))
    ifneq (,$(findstring MacOSX,$(BFARCH)))
        override SHLIB = $(shlibdir)$(LIBNAME:.a=.$(BFCURRENT).$(SOEXT))
    endif
  endif	

  ifneq ($(LIB2),)
    override SHLIB2 = $(shlibdir)$(LIB2NAME:.a=_$(BFCURRENT).$(SOEXT))
    ifneq (,$(findstring MacOSX,$(BFARCH)))
        override SHLIB2 = $(shlibdir)$(LIB2NAME:.a=.$(BFCURRENT).$(SOEXT))
    endif
  endif

#_________________________________________________________________________
# make links for shared libraries defined in $SHAREDLIB

ifeq  ($(MAKECMDGOALS), ldlink)
   include SoftRelTools/ldlink.mk

ldlink:  wldlink $(foreach v,$(SUBDIRS),$v.ldlink)
	$(ldlink)
endif  # ldlink

ifneq (,$(findstring Shared,$(OPTIONS)))

#_________________________________________________________________________
#++ include .d files.
#   these rules force gmake to create .d files.		[share-1]

ifneq (,$(filter-out $(skip_include_d),$(MAKEGOAL)))
ifeq (,$(findstring Skipdfile,$(OPTIONS)))
ifneq ($(MAKEGOAL),schema)
    ifdef LIBCCFILES 
      -include $(foreach var,$(LIBCCFILES:$(CC_SUFFIX)=.d),$(shworkdir)$(var:$(workdir)/%=%)) /dev/null
    endif
    ifdef LIB2CCFILES 
      -include $(foreach var,$(LIB2CCFILES:$(CC_SUFFIX)=.d),$(shworkdir)$(var:$(workdir)/%=%)) /dev/null
    endif
    ifdef LIBCFILES
      -include $(foreach var,$(LIBCFILES:.c=.d),$(shworkdir)$(var:$(workdir)/%=%)) /dev/null
    endif
    ifdef LIB2CFILES
      -include $(foreach var,$(LIB2CFILES:.c=.d),$(shworkdir)$(var:$(workdir)/%=%)) /dev/null
    endif

    ifdef LIBIDLFILES
      -include $(foreach var,$(LIBIDLFILES:.idl=_idl.d),$(shworkdir)$(var)) /dev/null
    endif
    ifdef LIB2IDLFILES
      -include $(foreach var,$(LIB2IDLFILES:.idl=_idl.d),$(shworkdir)$(var)) /dev/null
    endif
endif    # ~schema
endif    # skipdfile

  #-> include _ddl.d files if (schema) or (not Skipdfile)
  ifeq (,$(findstring Skipdfile,$(OPTIONS)))
    getddl:=1
  endif
  ifeq ($(MAKEGOAL),schema)
    getddl:=1
  endif
  ifeq (1,$(getddl))
  ifdef LIBDDLFILES 
    -include $(foreach var,$(LIBDDLFILES:.ddl=_ddl.d),$(shworkdir)$(var)) /dev/null
  endif
  ifdef LIB2DDLFILES 
    -include $(foreach var,$(LIB2DDLFILES:.ddl=_ddl.d),$(shworkdir)$(var)) /dev/null
  endif
  endif    # getddl
  endif    # skip_include_d

#_________________________________________________________________________
#++ .d tries to pre-process .cc so that any changes in headers etc will
#   cause *.d and *.o files to be updated.			[share-2]

$(shworkdir)%.d: %$(CC_SUFFIX)
	if test -f $< ; then \
		if [ ! -z "$(findstring $<, $(LIBCCFILES))" ]; then \
			echo "Making $@ [shared-cc1]"; \
			$(SHELL) -ec '$(CPP) $(MFLAG) $(GXXFLAGS) $(CPPFLAGS) $< | sed '\''s?$*\.o?$(shobjdir)$*.o \$@ ?g'\'' | sed '\''/\: \/opt\/SUNWs/d'\'' > $@'; \
		else \
		if [ ! -z "$(findstring $<, $(LIB2CCFILES))" ]; then \
			echo "Making $@ [shared-cc2]"; \
			$(SHELL) -ec '$(CPP) $(MFLAG) $(GXXFLAGS) $(CPPFLAGS) $< | sed '\''s?$*\.o?$(shobj2dir)$*.o \$@ ?g'\'' | sed '\''/\: \/opt\/SUNWs/d'\'' > $@'; \
		else \
		if [ ! -z "$(findstring $<, $(BINCCFILES))" ]; then \
			echo "Making $@ [shared-cc3]"; \
			$(SHELL) -ec '$(CPP) $(MFLAG) $(GXXFLAGS) $(CPPFLAGS) $< | sed '\''s?$*\.o?$(workdir)$*.o \$@ ?g'\'' | sed '\''/\: \/opt\/SUNWs/d'\'' > $@'; \
		else \
			echo "Making $@ [shared-cc4]"; \
			$(SHELL) -ec '$(CPP) $(MFLAG) $(GXXFLAGS) $(CPPFLAGS) $< | sed '\''s?$*\.o?$(shobjdir)$*.o \$@ ?g'\'' | sed '\''/\: \/opt\/SUNWs/d'\'' > $@' ; \
		fi \
		fi \
		fi \
	fi

$(shworkdir)%.d:	%.c
	if test -f $< ; then \
		if [ ! -z "$(findstring $<, $(LIBCFILES))" ]; then \
			echo "Making $(@F) [shared-c1]"; \
			$(SHELL) -ec '$(CPP) $(MFLAG) $(CPPFLAGS) $< | sed '\''s?$*\.o?$(shobjdir)\$*.o \$@ ?g'\'' > $@' ; \
		else \
		if [ ! -z "$(findstring $<, $(LIB2CFILES))" ]; then \
			echo "Making $(@F) [shared-c2]"; \
			$(SHELL) -ec '$(CPP) $(MFLAG) $(CPPFLAGS) $< | sed '\''s?$*\.o?$(shobj2dir)\$*.o \$@ ?g'\'' > $@'; \
		else \
		if [ ! -z "$(findstring $<, $(BINCFILES))" ]; then \
			echo "Making $(@F) [shared-c3]"; \
			$(SHELL) -ec '$(CPP) $(MFLAG) $(CPPFLAGS) $< | sed '\''s?$*\.o?$(workdir)\$*.o \$@ ?g'\'' > $@'; \
		else \
			echo "Making $(@F) [shared-c4]"; \
			$(SHELL) -ec '$(CPP) $(MFLAG) $(CPPFLAGS) $< | sed '\''s?$*\.o?$(shobjdir)\$*.o \$@ ?g'\'' > $@' ; \
		fi \
		fi \
		fi \
	fi

#_________________________________________________________________________
#-> 'lib' tries to build lib_<rel>.$(SOEXT) (shared) and lib.a (static)    [share-3]

  ifneq ($(LIB),)
    KPICFILES:=$(filter-out $(LIBCCSTATICFILES),$(LIBCCFILES))$(filter-out $(LIBDDLSTATICFILES),$(LIBDDLFILES))$(filter-out $(LIBIDLCCSTATICFILES),$(LIBIDLCCFILES))$(filter-out $(LIBCSTATICFILES),$(LIBCFILES))
    ifneq (,$(KPICFILES))
        CMD := $(shell if [ ! -d $(shobjdir) ]; then mkdir $(shobjdir); fi)
    endif
  endif	

  ifneq ($(LIB2),)
    KPIC2FILES:=$(filter-out $(LIB2CCSTATICFILES),$(LIB2CCFILES))$(filter-out $(LIB2DDLSTATICFILES),$(LIB2DDLFILES))$(filter-out $(LIB2IDLCCSTATICFILES),$(LIB2IDLCCFILES))
    KPIC2FILES:=$(filter-out $(LIB2CCSTATICFILES),$(LIB2CCFILES))$(filter-out $(LIB2DDLSTATICFILES),$(LIB2DDLFILES))$(filter-out $(LIB2IDLCCSTATICFILES),$(LIB2IDLCCFILES))$(filter-out $(LIB2CSTATICFILES),$(LIB2CFILES))
    ifneq (,$(KPIC2FILES))
        CMD := $(shell if [ ! -d $(shobj2dir) ]; then mkdir $(shobj2dir); fi)
    endif
  endif

  ifdef DEBUG05
    ERROR := $(shell echo "lib=$(lib) \t[s-7]"  >& 2)
      ERROR := $(shell echo "KPICFILES=$(KPICFILES) KPIC2FILES=$(KPIC2FILES)  \t[s-8]"  >& 2)
  endif

lib:  wlib $(foreach V,$(LIBCCOBJS),$(libdir)$(V:$(CC_SUFFIX)=.o)) $(LIB) $(LIB2) $(SHLIB) $(SHLIB2) $(foreach v,$(SUBDIRS),$v.lib)
	@echo lib stage done in $(curdir)

#_________________________________________________________________________
#++ lib.a tries to build static lib.a(*.o) where *.o are non-sharable obj
#   such as .F		[share-4]

#-> $(LIB) tries to build $(LIB)(*.o) for non-sharable obj files.
$(LIB): $(foreach V,$(LIBCCSTATICFILES),$(LIB)($(V:$(CC_SUFFIX)=.o)))
  ifneq (,$(findstring Objy,$(OPTIONS)))
  ifneq (has_not_been_set,$(OBJYBASE))
$(LIB): $(foreach V,$(LIBDDLSTATICFILES),$(LIB)($(V:.ddl=_ddl.o)))
  endif
  endif
$(LIB): $(foreach V,$(LIBIDLCCSTATICFILES),$(LIB)($(V:$(CC_SUFFIX)=.o)))
$(LIB): $(foreach V,$(LIBCSTATICFILES),$(LIB)($(V:.c=.o)))
$(LIB): $(foreach V,$(filter %.F,$(LIBFFILES)),$(LIB)($(V:.F=.o)))
$(LIB): $(foreach V,$(filter %.f,$(LIBFFILES)),$(LIB)($(V:.f=.o)))
$(LIB): $(foreach V,$(filter %.f90,$(LIBF90FILES)),$(LIB)($(V:.f90=.o)))
ifdef RANLIB
	if [ -s "$(LIB)" -a "$(RANLIB)" != "" ]; then \
	    REMARK="$(RANLIB) $(LIB)  [ran-6]"; \
	    $(RANLIB) $(LIB) 1>&2; \
	fi
endif

#-> $(LIB2) tries to build $(LIB2)(*.o) for non-sharable obj files.
$(LIB2): $(foreach V,$(LIB2CCSTATICFILES),$(LIB2)($(V:$(CC_SUFFIX)=.o)))
  ifneq (,$(findstring Objy,$(OPTIONS)))
  ifneq (has_not_been_set,$(OBJYBASE))
$(LIB2): $(foreach V,$(LIB2DDLSTATICFILES),$(LIB2)($(V:.ddl=_ddl.o)))
  endif
  endif
$(LIB2): $(foreach V,$(LIB2IDLCCSTATICFILES),$(LIB2)($(V:$(CC_SUFFIX)=.o)))
$(LIB2): $(foreach V,$(LIB2CSTATICFILES),$(LIB2)($(V:.c=.o)))
$(LIB2): $(foreach V,$(filter %.F,$(LIB2FFILES)),$(LIB2)($(V:.F=.o)))
$(LIB2): $(foreach V,$(filter %.f,$(LIB2FFILES)),$(LIB2)($(V:.f=.o)))
$(LIB2): $(foreach V,$(filter %.f90,$(LIB2F90FILES)),$(LIB2)($(V:.f90=.o)))
ifdef RANLIB
	if [ -s "$(LIB2)" -a "$(RANLIB)" != "" ]; then \
	    REMARK="$(RANLIB) $(LIB2)  [ran-7]"; \
	    $(RANLIB) $(LIB2) 1>&2; \
	fi
endif

#_________________________________________________________________________
#++ lib_<rel>.$(SOEXT) tries to build 'SHOBJ' which represents all shobj/*.o files.
#				 		[share-5]

$(SHLIB): wshlib SHOBJ
	if [ -r $(shobjdir) ]; then \
	    cd $(shobjdir); \
	    if [ "`ls -lda $(shobjdir)*.o`" ]; then \
		if [ -z "$(filter $(keep_main),$(LIBNAME))" ] ;then \
		    (for ofile in `ls *.o`; do \
		    	if [ "$(DEBUG25)" ]; then set -x ; fi; \
			if [ "$(findstring MacOS,$(BFARCH))" ]; then \
			    ISS=`$(NM) $$ofile | grep 'T _main'`; \
			    MOD=`expr "$$ISS" : '\(.*\)\.o:.*'`; \
	    		    if [ "$$MOD" != "" -a `expr "$(keep_main)" : ".*$(LIBNAME)\[$$MOD\.o\].*" ` -eq 0 ]; then \
		    	    	echo "[Warning]: $$MOD is removed from $(LIBNAME) since it has main() and it's not in keep_main list"; \
				$(RM) $$ofile ; \
			    fi; \
			else \
		     	    ISS=`$(NM) $$ofile | grep ': main '`; \
			    MOD=`expr "$$ISS" : '\(.*\)\.o:.*'`; \
	    		    if [ "$$MOD" != "" -a `expr "$(keep_main)" : ".*$(LIBNAME)\[$$MOD\.o\].*" ` -eq 0 ]; then \
			    	echo "Warning: $$ofile is removed since it has main() and it's not in keep_main list" ; \
			    	$(RM) $$ofile ; \
			    fi; \
			fi; \
		     done); \
		fi; \
		if [ `expr "$(BFARCH)" : ".*MacOSX.*" ` -gt 0 ]; then \
		    install_name="-install_name $(@F)"; \
		else \
	   	    install_name=; \
		fi; \
	        echo "shlib: Converting $(shobjdir)*.o to Shared Library $@ [shared-lib-1]"; \
	        $(CXX) -g *.o $(SHLDFLAGS) $$install_name -o $@; \
	    fi \
	fi

$(SHLIB2): wshlib2 SHOBJ2
	if [ -r $(shobj2dir) ]; then \
	    cd $(shobj2dir); \
	    if [ "`ls -lda $(shobj2dir)*.o`" ]; then \
		if [ -z "$(filter $(keep_main),$(LIB2NAME))" ] ;then \
		    (for ofile in `ls *.o`; do \
		     ISS2=`$(NM) $$ofile | grep ': main '`; \
		     if [ "$$ISS2" ]; then \
	    	     	if [ "$(DEBUG25)" ]; then set -x ; fi; \
			MOD2=`expr "$$ISS2" : '\(.*\)\.o:.*'`; \
	    		if [ `expr "$(keep_main)" : ".*$(LIB2NAME)\[$$MOD2\.o\].*" ` -eq 0 ]; then \
			    echo "Warning: $$ofile is removed since it has main()" ; \
			    $(RM) $$ofile ; \
			fi; \
		     fi; \
		    done); \
		fi; \
	    	echo "Converting $(shobj2dir)*.o to Shared Library $@ [shared-lib-2]";\
	    	$(CXX) -g $(SHLDFLAGS) *.o -o $@; \
	    fi \
	fi

#_________________________________________________________________________
#++ SHOBJ  tries to build shobj/*.o
#   SHOBJ2 tries to build shobj2/*.o		[share-6]

ifneq ($(KPICFILES),)
ifneq (has_not_been_set,$(OBJYBASE))

SHOBJ: wSHOBJ $(foreach V,$(filter-out $(LIBDDLSTATICFILES),$(LIBDDLFILES)),$(shobjdir)$(V:.ddl=_ddl.o))
endif    # OBJYBASE
SHOBJ: $(foreach V,$(filter-out $(LIBIDLCCSTATICFILES),$(LIBIDLCCFILES)),$(shobjdir)$(V:$(CC_SUFFIX)=.o))
SHOBJ: $(foreach V,$(filter-out $(LIBCCSTATICFILES),$(LIBCCFILES)),$(shobjdir)$(V:$(CC_SUFFIX)=.o)) $(foreach V,$(filter-out $(LIBCSTATICFILES),$(LIBCFILES)),$(shobjdir)$(V:.c=.o))

ifneq ($(KPIC2FILES),)
SHOBJ2: wSHOBJ2 $(foreach V,$(filter-out $(LIB2DDLSTATICFILES),$(LIB2DDLFILES)),$(shobj2dir)$(V:.ddl=_ddl.o))
endif    # OBJYBASE
SHOBJ2: $(foreach V,$(filter-out $(LIB2IDLSTATICFILES),$(LIB2IDLFILES)),$(shobj2dir)$(V:$(CC_SUFFIX)=.o))
SHOBJ2: $(foreach V,$(filter-out $(LIB2CCSTATICFILES),$(LIB2CCFILES)),$(shobj2dir)$(V:$(CC_SUFFIX)=.o)) $(foreach V,$(filter-out $(LIB2CSTATICFILES),$(LIB2CFILES)),$(shobj2dir)$(V:.c=.o))
endif

#_________________________________________________________________________
#++ shobj/*.o tries to compile .cc	[share-7]

#++ _ddl.cc files are created in $(shworkdir)
$(shobjdir)%.o: $(shworkdir)%$(CC_SUFFIX)
	echo "Compiling $(<F) [$(@F)] [shared-cc-1]"; \
	$(SETSRC); \
	$(CXX) $(SHCXXFLAGS) $(CXXFLAGS) $(CPPFLAGSmod) -c $$srcdir/$(<F) -o $(shobjdir)$(*F).o ; 

$(shobj2dir)%.o: $(shworkdir)%$(CC_SUFFIX)
	echo "Compiling $(<F) [$(@F)] [shared-cc-2]"; \
	$(SETSRC); \
	$(CXX) $(SHCXXFLAGS) $(CXXFLAGS) $(CPPFLAGSmod) -c $$srcdir/$(<F) -o $(shobj2dir)$(*F).o ; 

#++ this rule matches .cc files in $(curdir) and $(workdir), etc
$(shobjdir)%.o: %$(CC_SUFFIX)
	echo "Compiling $(<F) [$(@F)] [shared-cc-3]"; \
	$(SETSRC); \
	$(CXX) $(SHCXXFLAGS) $(CXXFLAGS) $(CPPFLAGSmod) -c $$srcdir/$(<F) -o $(shobjdir)$(*F).o ; 

$(shobj2dir)%.o: %$(CC_SUFFIX)
	echo "Compiling $(<F) [$(@F)] [shared-cc-4]"; \
	$(SETSRC); \
	$(CXX) $(SHCXXFLAGS) $(CXXFLAGS) $(CPPFLAGSmod) -c $$srcdir/$(<F) -o $(shobj2dir)$(*F).o ; 

#++ this rule matches .c files in $(shworkdir) 
$(shobjdir)%.o:  $(shworkdir)%.c
	echo Compiling $(<F) [$(@F)] [shared-c-1]; \
	$(SETSRC); \
	$(CC) $(SHCCFLAGS) $(CCFLAGS) $(CPPFLAGSmod) -c $$srcdir/$(<F) -o $(shobjdir)$(*F).o

$(shobj2dir)%.o:  $(shworkdir)%.c
	echo Compiling $(<F) [$(@F)] [shared-c-2]; \
	$(SETSRC); \
	$(CC) $(SHCCFLAGS) $(CCFLAGS) $(CPPFLAGSmod) -c $$srcdir/$(<F) -o $(shobj2dir)$(*F).o

#++ this rule matches .c files in $(curdir)
$(shobjdir)%.o: %.c
	echo Compiling $(<F) [$(@F)] [shared-c-3]; \
	$(SETSRC); \
	$(CC) $(SHCCFLAGS) $(CCFLAGS) $(CPPFLAGSmod) -c $$srcdir/$(<F) -o $(shobjdir)$(*F).o

$(shobj2dir)%.o: %.c
	echo Compiling $(<F) [$(@F)] [shared-c-4]; \
	$(SETSRC); \
	$(CC) $(SHCCFLAGS) $(CCFLAGS) $(CPPFLAGSmod) -c $$srcdir/$(<F) -o $(shobj2dir)$(*F).o

#_________________________________________________________________________
#++ if -Lshared, set ALLLIB to all existing shared and static libraries.
#   [share-8] 

ifneq (,$(findstring Lshared,$(OPTIONS)))
    #-> define ALLLIB for -Lshared
    define ALLLIB
	$(shell \
	if [ "$(DEBUG06)" ]; then set -x; fi; \
	findlib () { \
	    ret=; \
	    if [ -r "$(TOPDIR)/shlib/$(BFARCH)/lib$${lib}_$(BFCURRENT).$(SOEXT)" -o -r "$(BFDIST)/releases/$(BFCURRENT)/shlib/$(BFARCH)/lib$${lib}_$(BFCURRENT).$(SOEXT)" ]; then \
	    	ret="$$ret -l$${lib}_$(BFCURRENT)" ; \
	    fi; \
	    if [ -r "$(TOPDIR)/lib/$(BFARCH)/lib$$lib.a" -o -r "$(BFDIST)/releases/$(BFCURRENT)/lib/$(BFARCH)/lib$${lib}.a" ]; then \
	    	ret="$$ret -l$$lib" ; \
	    fi; \
	    echo $$ret; \
	}; \
        \
    if [ "$(LOADLIBES)" != "" ]; then \
	for lib in xx $(LOADLIBES); do \
	    if [ "$$lib" = "xx" ]; then continue; fi; \
	    if [ "x`echo $$lib | cut -c1-2`x" != "x-lx" ]; then \
		alllib="$$alllib $$lib"; continue; \
	    fi; \
	    lib=`echo $$lib | cut -c3-`; \
	    eval "foundso=\$$$${lib}FOUNDSO"; \
	    if [ "$$foundso" = "1" ]; then continue; fi; \
	    Findlib=`findlib`; \
	    if [ `expr "$$Findlib" : '.*_$(BFCURRENT).*'` -ne 0 ]; then \
		eval $${lib}FOUNDSO=1; \
	    fi; \
	    if [ "$$Findlib" = "" ]; then Findlib=-l$$lib; fi; \
	    alllib="$$alllib $$Findlib"; \
	done; \
    fi; \
    echo $$alllib $(REALSYSLIB) )
  endef
endif    # end-Lshared


#_________________________________________________________________________
#++ These rules allow the building of a library to be used externally
#   [share-9] 

ifneq ($(LIB),)
   override SHLIBEXT = $(shlibdir)$(LIBNAME:.a=_$(BFCURRENT)_ext.$(SOEXT))
endif	

libext:  wlib $(foreach V,$(LIBCCOBJS),$(libdir)$(V:$(CC_SUFFIX)=.o)) $(LIB) $(LIB2) $(SHLIBEXT) $(foreach v,$(SUBDIRS),$v.libext)
	@echo lib stage done in $(curdir)

$(SHLIBEXT): wshlib SHOBJ
	if [ -r $(shobjdir) ]; then \
	    cd $(shobjdir); \
	    if [ "`ls -lda $(shobjdir)*.o`" ]; then \
		if [ -z "$(filter $(keep_main),$(LIBNAME))" ] ;then \
		    (for ofile in `ls *.o`; do \
		     ISS=`$(NM) $$ofile | grep ': main '`; \
		     if [ "$$ISS" ]; then \
		    	if [ "$(DEBUG25)" ]; then set -x ; fi; \
			MOD=`expr "$$ISS" : '\(.*\)\.o:.*'`; \
	    		if [ `expr "$(keep_main)" : ".*$(LIBNAME)\[$$MOD\.o\].*" ` -eq 0 ]; then \
			    echo "Warning: $$ofile is removed since it has main() and it's not in keep_main list" ; \
			    $(RM) $$ofile ; \
			fi; \
		     fi; \
		     done); \
		fi; \
	        echo "shlib: Converting $(shobjdir)*.o to Shared Library $@ [shared-lib-3]"; \
	        $(CXX) -g *.o $(SHLDFLAGS) $(LDFLAGS) -o $@ -Bstatic $(LOADLIBES) $(REALSYSLIBS) $(LDENDFLAGS); \
	    fi \
	fi

endif    # Shared

###########################################################################
#++ rules for multi-package ROOT shared libraries

ifneq ('',$(findstring root,$(MAKEGOAL)))
ifeq (1,$(shell if [ -r root_$(PACKAGE).mk ]; then echo 1; fi))

# This should probably be moved to arch_spec.mk PE 030703
ifneq (,$(findstring SunOS5,$(BFARCH)))
  override SHLDFLAGS += -G -library=%none,iostream,Cstd,Crun
else
  ifneq (,$(findstring MacOSX,$(BFARCH)))
    override SOEXT := so	### THIS SUPERSEDES .dylib
    override SHLDFLAGS += -bundle -single_module -undefined dynamic_lookup
    override SHLDFLAGS := $(filter-out -dynamiclib -single_module,$(SHLDFLAGS))
    export MACOSX_DEPLOYMENT_TARGET := 10.3
  else
    override SHLDFLAGS += -static --export-dynamic
  endif
endif

# target multi-package ROOT library
ROOTMULTILIB := $(shlibdir)lib$(PACKAGE)_root.$(SOEXT)

# Cint.o files in current working directory
ROOTLINKOBJFILES := $(foreach v, $(CINTPACKAGES), \
                $(TOPDIR)/tmp/$(BFARCH)/$(PACKAGE)/${v}Cint.o)

root: lib $(ROOTLINKOBJFILES) $(CDBDYNDEPOBJFILES)
	-echo "Creating ROOT Shared Library for $(ROOTMULTILIB): $(CINTPACKAGES) [root-1]"; \
	$(CXX) -g $(SHLDFLAGS) $(LDFLAGS) \
	  -o $(ROOTMULTILIB) $(ROOTLINKOBJFILES) $(CDBDYNDEPOBJFILES) $(ALLLIB); \
	$(RM) $(ROOTLINKOBJFILES); \
	echo root stage done in $(curdir)

# compile the Cint.cc files
$(ROOTLINKOBJFILES) :
	-cd $(workdir); \
	pkg=$(subst Cint.o, ,$(@F)); lib=lib$$pkg.a; \
	echo "Extracting $(@F) from $$lib [root-2]"; \
	if [ -r "$(TOPDIR)/lib/$(BFARCH)/$$lib" ]; then \
	    ldir="$(TOPDIR)/lib/$(BFARCH)"; \
	elif [ -r "$(BFDIST)/releases/$(BFCURRENT)/lib/$(BFARCH)/$$lib" ]; then \
	    ldir="$(BFDIST)/releases/$(BFCURRENT)/lib/$(BFARCH)"; \
	else \
	    echo "$$lib not found   [root-3]" >&2; \
	fi; \
	if [ "$(DEBUG32)" != "" ]; then echo "lib=$$ldir/$$lib">&2; fi; \
	cp -f $$ldir/$$lib . ; \
        $(AR) -xv $$lib $(@F); \
	$(RM) $$lib

endif   # root_<pkg>.mk
endif   # root

###########################################################################
#++ rules for C++ files. 
#   CC_SUFFIX can be set to .cc or .cxx or .cpp


#-> .d tries to pre-process .cc
ifeq (,$(findstring Shared,$(OPTIONS)))
$(workdir)%.d:	%$(CC_SUFFIX)
	if test -f $< ; then \
		if [ ! -z "$(findstring $<, $(LIBCCFILES))" ]; then \
			echo "Making $(@F) [cc1]"; \
			$(SHELL) -ec '$(CPP) $(MFLAG) $(GXXFLAGS) $(CPPFLAGS) $< | sed '\''s?$*\.o?$(LIB)\($*.o) \$@ ?g'\'' | sed '\''/\: \/opt\/SUNWs/d'\'' > $@'; \
		else \
		if [ ! -z "$(findstring $<, $(LIB2CCFILES))" ]; then \
			echo "Making $(@F) [cc2]"; \
			$(SHELL) -ec '$(CPP) $(MFLAG) $(GXXFLAGS) $(CPPFLAGS) $< | sed '\''s?$*\.o?$(LIB2)\($*.o) \$@ ?g'\'' | sed '\''/\: \/opt\/SUNWs/d'\'' > $@'; \
		else \
		if [ ! -z "$(findstring $<, $(BINCCFILES))" ]; then \
			echo "Making $(@F) [cc3]"; \
			$(SHELL) -ec '$(CPP) $(MFLAG) $(GXXFLAGS) $(CPPFLAGS) $< | sed '\''s?$*\.o?$(workdir)$*.o \$@ ?g'\'' | sed '\''/\: \/opt\/SUNWs/d'\'' > $@'; \
		else \
			echo "Making $(@F) [cc4]"; \
			$(SHELL) -ec '$(CPP) $(MFLAG) $(GXXFLAGS) $(CPPFLAGS) $< | sed '\''s?$*\.o?$(LIB)\($*.o) \$@ ?g'\'' | sed '\''/\: \/opt\/SUNWs/d'\'' > $@' ; \
		fi \
		fi \
		fi \
	fi

#-> .d tries to pre-process .c
$(workdir)%.d:	%.c
	if test -f $< ; then \
		if [ ! -z "$(findstring $<, $(LIBCFILES))" ]; then \
			echo "Making $(@F) [c1]"; \
			$(SHELL) -ec '$(CPP) $(MFLAG) $(CPPFLAGS) $< | sed '\''s?$*\.o?$(LIB)\($*.o) \$@ ?g'\'' > $@' ; \
		else \
		if [ ! -z "$(findstring $<, $(LIB2CFILES))" ]; then \
			echo "Making $(@F) [c2]"; \
			$(SHELL) -ec '$(CPP) $(MFLAG) $(CPPFLAGS) $< | sed '\''s?$*\.o?$(LIB2)\($*.o) \$@ ?g'\'' > $@'; \
		else \
			echo "Making $(@F) [c3]"; \
			$(SHELL) -ec '$(CPP) $(MFLAG) $(CPPFLAGS) $< | sed '\''s?$*\.o?$(LIB)\($*.o) \$@ ?g'\'' > $@' ; \
		fi \
		fi \
	fi

endif   # ~Shared 

#-> this section is for static library only.
#   lib.a(.o) tries to find and compile .cc
(%.o): %$(CC_SUFFIX)
ifeq (,$(findstring HP-UX, $(BFARCH)))
	ext=;if [ -n "$(fastbuild)" -a "$(@F)" = "$(LIB2NAME)" ]; then ext=2; fi; \
	echo "Compiling $(<F) [$(@F)] [cc-1]"; \
	$(SETSRC); \
	$(CXX) $(CXXFLAGS) $(CPPFLAGSmod) -c $$srcdir/$(<F) -o $(objdir)$$ext/$(*F).o ; \
	if [ $$? -eq 0 ]; then \
	    if [ -z "$(fastbuild)" ]; then $(CXXAR) $(libdir)$(@F) $(workdir)$(*F).o; fi; \
	else false; fi
else
	ext=;if [ -n "$(fastbuild)" -a "$(@F)" = "$(LIB2NAME)" ]; then ext=2; fi; \
	echo "Compiling $(<F) [$(@F)] [cc-2]";\
	$(SETSRC); \
	$(CXX) $(CXXFLAGS) $(CPPFLAGSmod) -c $$srcdir/$(<F) -o $(objdir)$$ext/$(*F).o  2>&1 |\
	  grep -v 'defined but not used (160)' | grep -v 'not used (117)'
	if [ -z "$(fastbuild)" ]; then $(CXXAR) $(libdir)$(@F) $(workdir)$(*F).o; fi
endif
ifeq (,$(INCR))
ifndef fastbuild
	$(RM) $(workdir)$(*F).o
endif   ## fastbuild
else
	$(IMV)
endif

# $(workdir)%.o and $(libdir)%.o tries to compile %.cc
$(libdir)%.o: %$(CC_SUFFIX)
$(workdir)%.o: %$(CC_SUFFIX)
ifeq (,$(findstring HP-UX, $(BFARCH)))
	echo "Compiling $< [$(@F)] [cc-3]"; \
	$(SETSRC); \
	$(CXX) $(BINCXXFLAGS) $(CXXFLAGS) $(CPPFLAGSmod) -c $$srcdir/$(<F) -o $(workdir)$(*F).o
else
	echo "Compiling $< [$(@F)] [cc-4]";\
	$(SETSRC); \
	$(CXX) $(BINCXXFLAGS) $(CXXFLAGS) $(CPPFLAGSmod) -c $$srcdir/$(<F) -o $(workdir)$(*F).o 2>&1 |\
	grep -v 'defined but not used (160)' | grep -v 'not used (117)'; \
	echo "\c"
endif

#-> this rule is seldom used
$(CC_SUFFIX).o:
ifeq (,$(findstring HP-UX, $(BFARCH)))
	echo "Compiling $< [$(@F)] [cc-5]"; \
	$(SETSRC); \
	$(CXX) $(CXXFLAGS) $(CPPFLAGSmod) -c $$srcdir/$(<F) -o $(workdir)$(*F).o
else
	echo "Compiling $< [$(@F)] [cc-6]"; \
	$(SETSRC); \
	$(CXX) $(CXXFLAGS) $(CPPFLAGSmod) -c $$srcdir/$(<F)  -o $(workdir)$(*F).o 2>&1 |\
	grep -v 'defined but not used (160)' | grep -v 'not used (117)'; \
	echo "\c"
endif

# this rule is seldom used
$(libdir)%.o: %$(CC_SUFFIX)
	echo "Compiling $< [$(@F)] [cc-7]"; \
	$(SETSRC); \
	$(CXX) $(CXXFLAGS) $(CPPFLAGSmod) -c $$srcdir/$(<F) -o $(libdir)$*.o

# Add new rules for making .ii and .s files
$(workdir)%.ii : %$(CC_SUFFIX)
	echo "Preprocessing $(<F) [$(@F)] [ppcc-1]"; \
	$(SETSRC); \
	$(CXX) $(CXXFLAGS) $(CPPFLAGSmod) -E $$srcdir/$(<F) > $@
$(workdir)%.s : %$(CC_SUFFIX)
	echo "Compiling to assembly $(<F) [$(@F)] [ascc-1]"
# -nopt needed to avoid problem on OSF CXX 6.0
ifneq (,$(findstring OSF1V4,$(BFARCH)))
	$(SETSRC); \
	$(CXX) $(CXXFLAGS) $(CPPFLAGSmod) -S $$srcdir/$(<F) -o $@ -nopt 
else
	$(SETSRC); \
	$(CXX) $(CXXFLAGS) $(CPPFLAGSmod) -S $$srcdir/$(<F) -o $@
endif


###########################################################################
#++ rules for linking BINS

ifeq (,$(OPTIONALMISSING))
extrabin: wextrabin 
	for target in $(foreach v,$(EXTRABINS),$(bindir)$v) xx; do \
		if [ $$target != xx ]; then \
			$(MAKE) $$target BINNAME=`basename $$target`; \
		fi; \
		done
	@$(MAKE) rextrabin

rextrabin: pruneextrabin $(foreach v,$(SUBDIRS),$v.extrabin)

#++ Identify scripts with special dependency Makefiles, which must be built
#   via recursive $(MAKE).  All others can be built as direct dependencies.

BINSWITHMK   := $(patsubst bin_%.mk,%,$(wildcard bin_*.mk))

BINSCRWITHMK := $(filter $(BINSWITHMK),$(BINSCRIPTS))
BINSCRWOMK   := $(filter-out $(BINSCRWITHMK),$(BINSCRIPTS))

binscripts: wbinscripts $(foreach v,$(BINSCRWOMK),$(bindir)$v)
	for target in $(foreach v,$(BINSCRWITHMK),$(bindir)$v) xx; do \
		if [ $$target != xx ]; then \
			$(MAKE) $$target BINNAME=`basename $$target`; \
		fi; \
		done
	@$(MAKE) rbinscripts

rbinscripts: $(foreach v,$(SUBDIRS),$v.binscripts)

EBINSCRWITHMK := $(filter $(BINSWITHMK),$(EXTRABINSCRIPTS))
EBINSCRWOMK   := $(filter-out $(EBINSCRWITHMK),$(EXTRABINSCRIPTS))

extrabinscripts: wextrabinscripts $(foreach v,$(EBINSCRWOMK),$(bindir)$v)
	for target in $(foreach v,$(EBINSCRWITHMK),$(bindir)$v) xx; do \
		if [ $$target != xx ]; then \
			$(MAKE) $$target BINNAME=`basename $$target`; \
		fi; \
		done
	@$(MAKE) rextrabinscripts

rextrabinscripts: $(foreach v,$(SUBDIRS),$v.extrabinscripts)

siteinstall: wsiteinstall
	$(MAKE) binscripts SITEINSTALL=1
endif	# OPTIONALMISSING

#__________________________________________________________________________
#++ Default binary rule.

ifneq (,$(BINARIES))
ifeq (,$(OPTIONALMISSING))
ifeq (,$(findstring Lshared,$(OPTIONS)))
$(addprefix $(bindir),$(BINARIES)) : $(filter -l%, $(LOADLIBES))
else
$(addprefix $(bindir),$(BINARIES)) :
endif
ifeq (,$(findstring -Shared,$(OPTIONS)))
	$(CheckMain) 
endif
        override outfile =
ifneq (,$(findstring -Linkmap,$(OPTIONS)))   # ROPT=-Linkmap
ifneq (,$(findstring Linux2,$(BFARCH)))
        override LDFLAGS+=-Wl,-Map,$@.map
endif # Linux2
ifneq (,$(findstring SunOS,$(BFARCH)))
        override LDFLAGS+=-m
        override outfile = > $@.map
endif # SunOS
endif # -Linkmap
	echo "Linking $(@F) in $(PACKAGE) [link-1]"; \
	if [ -n "$(findstring -Showdep,$(OPTIONS))" ]; then echo "The dependents of \"$(BINARIES)\": $(filter -l%, $(LOADLIBES))" >&2; fi; \
	if [ -z "$(filter %.o,$^)$(filter %.cc,$^)" ] ;then echo "*** Warning: missing dependency rule for $(@F)"; fi; \
	for v in $(filter %.cc,$^) xx; do \
	    if [ "$$v" = "xx" ]; then break; fi; \
	    CCFILES="$$CCFILES `/bin/pwd`/$$v"; done; \
	$(SETSRC); \
        $(CXXLD) $(filter %.o,$^) $$CCFILES $(filter-out -g,$(CXXFLAGS)) $(CPPFLAGS) $(LDFLAGS) -o  $@ \
	       $(ALLLIB) $(LDENDFLAGS) $(outfile)

#__________________________________________________________________________
#++ This rule makes individual binary build possible.
#   It works for first level directory only. If there are multiple directory
#   levels, the first level would have no idea how to pass this target down.

.PHONY: $(BINARIES)

ifneq (,$(filter $(MAKEGOAL),$(BINARIES)))
$(MAKEGOAL) : $(bindir)$(MAKEGOAL)
endif

else  # OPTIONALMISSING
$(addprefix $(bindir),$(BINARIES)) :
	echo "Skipping $(@F) in $(PACKAGE) [link-2]"

ifneq (,$(filter $(MAKEGOAL),$(BINARIES)))
$(MAKEGOAL) :
	echo "Skipping $(@F) in $(PACKAGE) [link-2]"
endif

endif # OPTIONALMISSING
endif # BINARIES

#__________________________________________________________________________
#++ rules for C files.

#-> lib.a(%.o) tries to compile %.c
#(%.o): %.c
.c.a:
	ext=;if [ -n "$(fastbuild)" -a "$(@F)" = "$(LIB2NAME)" ]; then ext=2; fi; \
	echo "Compiling $(<F) [$(@F)$$ext] [c-1]"; \
	$(SETSRC); \
	$(CC) $(CCFLAGS) $(CPPFLAGSmod) -c $$srcdir/$(<F) -o $(objdir)$$ext/$(*F).o
	if [ $$? -eq 0 ]; then \
	    if [ -z "$(fastbuild)" ]; then $(AR) r$(ARFLAGS) $(libdir)$(@F) $(workdir)$(*F).o; fi; \
	else false; fi
ifeq (,$(INCR))
ifndef fastbuild
	$(RM) $(workdir)$(*F).o
endif   ## fastbuild
else
	$(IMV)
endif

#-> $(workdir)%.o and $(libdir)%.o tries to compile %.cc
#$(libdir)%.o: %.c
#$(workdir)%.o: %.c
#	echo "Compiling $(<F) [$(@F)] [c-2]"; \
#	$(SETSRC); \
#	$(CC) $(CCFLAGS) $(CPPFLAGSmod) -c $$srcdir/$(<F) -o $(workdir)$(*F).o

#-> this rule is seldom used. One invocation is for dbin/lex.yy.c
.c.o:
	echo "Compiling $(<F) [$(@F)] [c-2]"; \
	$(SETSRC); \
	$(CC) $(CCFLAGS) $(CPPFLAGSmod) -c $$srcdir/$(<F) -o $(workdir)$(*F).o

#-> this rule is seldom used
#$(libdir)%.o: %.c
#	echo "Compiling $< [$(@F)] [c-4]"; \
#	$(SETSRC); \
#	$(CC) $(CCFLAGS) $(CPPFLAGSmod) -c $$srcdir/$(<F) -o $(libdir)$*.o

#-> this rule is seldom used
$(workdir)%.o: %.c
	echo "Compiling $< [$(@F)] [c-3]"; \
	$(SETSRC); \
	$(CC) $(CCFLAGS) $(CPPFLAGSmod) -c $$srcdir/$(<F) -o $(workdir)$(*F).o


#__________________________________________________________________________
#++ rules for F files. Use pre-processor to create .f files.
##
$(workdir)%.d:	%.F
	if test -f $< ; then \
		if [ ! -z "$(findstring $<, $(LIBFFILES))" ]; then \
			echo "Making $(@F) [F1]"; \
			echo $(LIB)\($*.o\)  $@ " : " $^ "\\" > $@; \
		else \
		if [ ! -z "$(findstring $<, $(LIB2FFILES))" ]; then \
			echo "Making $(@F) [F2]"; \
			echo $(LIB2)\($*.o\) $@ " : " $^ "\\" > $@; \
		else \
			echo "Making $(@F) [F3]"; \
			echo $(workdir)$*.o  $@ " : " $^ "\\" > $@; \
		fi \
		fi \
	fi
	if test -f $< ; then \
		awk '$$1=="#include" { print $$2 " \\" }' $< | \
		tr -d \"\>\< >> $@ ; echo >> $@ ; \
	fi

# This rule has been split as g77 will do this preprocessing for us
.F.a:
	echo "Compiling $(<F) [$(@F)] [F-1]";
	if [ -f $(workdir)$(*F).f ]; then rm $(workdir)$(*F).f; fi
ifeq (,$(findstring Linux,$(BFARCH))$(findstring MacOSX,$(BFARCH)))
	here=`/bin/pwd`; export here; $(FFCPP) "-I$$here" $(FFCPPFLAGS) $(CPPFLAGS) $< $(workdir)$(*F).f

	#-> run vms2f90
	if [ "$(VMS2F90)" = "yes" ]; then \
		cd $(workdir); vms2f90 $(workdir)$(*F).f; \
		if [ -f $(workdir)$(*F).f90 ]; then mv $(workdir)$(*F).f90 $(workdir)$(*F).f; fi; \
	fi 
	$(FFC) $(FFCFLAGS) -c $(workdir)$(*F).f -o $(workdir)$*.o
else # LINUX
	$(FFC) $(FFCFLAGS) $(FFCPPFLAGS) $(CPPFLAGS) -c $< -o $(workdir)$*.o
endif # LINUX
	$(AR) r$(ARFLAGS) $(libdir)$(@F) $(workdir)$*.o
ifeq (,$(INCR))
	$(RM) $(workdir)$(*F).o 
else
	$(IMV)
endif
	if [ -s "$(LIB)" -a "$(RANLIB)" != "" ]; then \
		REMARK="$(RANLIB) $(LIB)  [ran-8]"; \
		$(RANLIB) $(LIB) 1>&2; \
	fi

# This rule has been split as g77 will do this preprocessing for us
$(libdir)%.o: %.F
	echo "Compiling $< [$(@F)] [F-2]";
ifeq (,$(findstring Linux,$(BFARCH))$(findstring MacOSX,$(BFARCH)))
	if [ -f $(workdir)$(*F).f ]; then rm $(workdir)$(*F).f; fi
	here=`/bin/pwd`; export here; $(FFCPP) "-I$$here" $(FFCPPFLAGS) $(CPPFLAGS) $< $(workdir)$(*F).f

	#-> run vms2f90
	if [ "$(VMS2F90)" = "yes" ]; then \
		cd $(workdir); vms2f90 $(workdir)$(*F).f; \
		if [ -f $(workdir)$(*F).f90 ]; then mv $(workdir)$(*F).f90 $(workdir)$(*F).f; fi; \
	fi 
	$(FFC) $(FFCFLAGS) -c $(workdir)$(*F).f -o $(workdir)$*.o
else
	$(FFC) $(FFCFLAGS) $(CPPFLAGS) -c $(<F) -o $(workdir)$*.o
endif
	mv $(workdir)$*.o $(libdir)$*.o
	if [ $(testLink) $(workdir)$< ] ; then rm $(workdir)$< ; fi

# This rule has been split as g77 will do this preprocessing for us
$(workdir)%.o: %.F
	echo "Compiling $< [$(@F)] [F-3]";
ifeq (,$(findstring Linux,$(BFARCH))$(findstring MacOSX,$(BFARCH)))
	if [ -f $(workdir)$(*F).f ]; then rm $(workdir)$(*F).f; fi
	here=`/bin/pwd`; export here; $(FFCPP) "-I$$here" $(FFCPPFLAGS) $(CPPFLAGS) $< $(workdir)$(*F).f

	#-> run vms2f90
	if [ "$(VMS2F90)" = "yes" ]; then \
		cd $(workdir); vms2f90 $(workdir)$(*F).f; \
		if [ -f $(workdir)$(*F).f90 ]; then mv $(workdir)$(*F).f90 $(workdir)$(*F).f; fi; \
	fi 

	$(FFC) $(FFCFLAGS) -c $(workdir)$(*F).f -o $(workdir)$*.o
else
	$(FFC) $(FFCFLAGS) $(CPPFLAGS) -c $(<F) -o $(workdir)$*.o
endif

#__________________________________________________________________________
#++ rules for f files.
# .f is NOT handled by f77bb; instead, the arch_spec.mk file 
# does the architecture specific things to the $(FCFLAGS) variable
# this rule is used a few times only.
.f.a:
	echo "Compiling $(<F) [$(@F)] [f-1]"; $(FC) $(FCFLAGS) -c $< -o $(workdir)$(*F).o
	$(AR) r$(ARFLAGS) $(libdir)$(@F) $(workdir)$(*F).o
#	$(RM) $(workdir)$(*F).o
ifeq (,$(INCR))
	$(RM) $(workdir)$(*F).o 
else
	$(IMV)
endif
	if [ -s "$(LIB)" -a "$(RANLIB)" != "" ]; then \
		REMARK="$(RANLIB) $(LIB)  [ran-9]"; \
		$(RANLIB) $(LIB) 1>&2; \
	fi

#-> this rule is seldom used 
# .f is NOT handled by f77bb; instead, the arch_spec.mk file 
# does the architecture specific things to the $(FCFLAGS) variable
.f.o:
	echo "Compiling $< [$(@F)] [f-2]"; $(FC) $(FCFLAGS) -c $< -o $(workdir)$(*F).o

#-> this rule is seldom used 
# .f is NOT handled by f77bb; instead, the arch_spec.mk file 
# does the architecture specific things to the $(FCFLAGS) variable
$(workdir)%.o: %.f
	echo "Compiling $< [$(@F)] [f-3]"; $(FC) $(FCFLAGS) -c $< -o $(workdir)$(*F).o

#__________________________________________________________________________
#++ rules for f90 files.
$(workdir)%.d:	%.f90
	tr A-Z a-z < $*.f90 | awk '$$1=="module" { print $$2 ".mod" }' > $@.1
	echo $(LIB)\($*.o\) $@ " : " $*.f90 "\\" > $@.2
	tr A-Z a-z < $*.f90 | awk '$$1=="use" { print $$2 ".mod \\" }' >> $@.2
	echo >> $@.2
	echo >> $@.2
	grep -v -f $@.1 $@.2 > $@
	awk '{ print $$1 " : $*.f90" }' $@.1 >> $@
	echo >> $@
	rm -f $@.1 $@.2

#-> this rule is seldom used
.f90.a:
	echo "Compiling $< [$(@F)] [f90-1]"; if [ ! -d $(moddir) ] ; then mkdir $(moddir); fi
ifeq ($(F90C),xlf90)
	cp $< $(workdir)$(*F).f
	cd $(moddir); $(F90C) $(F90CFLAGS) -c $(workdir)$(*F).f -o $(workdir)$(*F).o
else
	cd $(moddir); $(F90C) $(F90CFLAGS) -c $(curdir)/$< -o $(workdir)$(*F).o
endif
	$(AR) r$(ARFLAGS) $(libdir)$(@F) $(workdir)$(*F).o
#	$(RM) $(workdir)$(*F).o
ifeq (,$(INCR))
	$(RM) $(workdir)$(*F).o 
else
	$(IMV)
endif

#-> this rule is seldom used
.f90.o:
	echo "Compiling $< [$(@F)] [f90-2]"; if [ ! -d $(moddir) ] ; then mkdir $(moddir); fi
ifeq ($(F90C),xlf90)
	cp $< $(workdir)$*.f
	cd $(moddir); $(F90C) $(F90CFLAGS) -c $(workdir)$*.f -o $(workdir)$*.o
else
	cd $(moddir); $(F90C) $(F90CFLAGS) -c $(curdir)/$< -o $(workdir)$*.o
endif

.f90.mod:
	if [ ! -d $(moddir) ] ; then mkdir $(moddir); fi
ifeq ($(F90C),xlf90)
	cp $< $(workdir)$(*F).f
	cd $(moddir); $(F90C) $(F90CFLAGS) -c $(workdir)$(*F).f -o $(workdir)$(*F).o
else
	cd $(moddir); $(F90C) $(F90CFLAGS) -c $(curdir)/$< -o $(workdir)$(*F).o
endif

%.mod: %.f90
	if [ ! -d $(moddir) ] ; then mkdir $(moddir); fi
ifeq ($(F90C),xlf90)
	cp $< $(workdir)$*.f
	cd $(moddir); $(F90C) $(F90CFLAGS) -c $(workdir)$*.f -o $(workdir)$*.o
else
	cd $(moddir); $(F90C) $(F90CFLAGS) -c $(curdir)/$< -o $(workdir)$*.o
endif

# The following two are kludges because the source directory gets
# write protected for official versions. Ideally the .f90 and .cc files
# should get created somewhere else (e.g. in tmp), but that severely
# complicates other rules.

# IDL rules have been changed drastically to allow TAO to work
# Those changes should not effect f90stub rules - but who knows!
# - Sridhara Dasu, July 1998

%.f90stub.f90:	%.idl
	prot=0; \
	if [ ! -w $(curdir) ]; then chmod u+w $(curdir); prot=1; fi; \
	$(FIDLE) $(IDLFLAGS) $<; \
	if [ $$prot ]; then chmod u-w $(curdir); fi

#__________________________________________________________________________
#++ rules for ddl (database) files.

#-> rule to make _ddl.cc files. (also .hh, _ref.hh)
ifneq (,$(findstring Shared,$(OPTIONS)))
$(shworkdir)%$(DDL_CC_SUFFIX): %.ddl
else
$(workdir)%$(DDL_CC_SUFFIX): %.ddl
endif
ifeq (,$(findstring -Objy,$(OPTIONS)))
	@echo "Objectivity not supported for $(BFARCH).  Skip DDL processing."
else
ifeq (has_not_been_set,$(OBJYBASE))
	@echo "OBJYBASE not setup - check for correct Objectivity dependencies"
else
	@if [ "$(OODDL)" = "" ]; then \
	    echo "OODDL not setup - check for correct Objectivity dependencies"; \
	    exit 2; \
	fi; 
	#
	# The KEEP_TYPEDEFS flag is used to retain backwards compatibility with
	# Objectivity V5.1 for templated classes. The processing of these was
	# changed in Objectivity V5.2 such that some such classes would result
	# in different schema ids. This flag is set for problem classes that
	# already existed in the V5.1 production schema. It should not be set for
	# any new classes following the migration to V5.2.
	#
	# The SCHEMA_FROZEN flag freezes the existing schema and treats an attempt
	# at the creation of a new class, or the evolution of an existing class,
	# as errors. This flag overrides the EVOLVE_SCHEMA and VERSION_SCHEMA flags.
	#
	# The EVOLVE_SCHEMA flag enables schema evolution on a set of classes.
	# The value is a list of class names to be evolved. This flag is disabled
	# if the SCHEMA_FROZEN or VERSION_SCHEMA flags are also set. Note also
	# that EVOLVE_SCHEMA=all enables schema evolution for all classes, which
	# is also the default. Finally, EVOLVE_SCHEMA=none disables schema evolution 
	# for all classes.
	#
	# The VERSION_SCHEMA flag enables schema versioning on a set of classes.
	# The value is a list of class names to be evolved. This flag is disabled
	# if the SCHEMA_FROZEN or EVOLVE_SCHEMA flag are also set.
	#
	# The UPGRADE_SCHEMA flag enables schema upgrading on a set of classes.
	# This flag is disabled unless EVOLVE_SCHEMA is also set.
	#
	# The AVOID_COLLISIONS flag controls a DDL Compiler collision avoidance scheme.
	# This is enabled by default, but can be disabled by AVOID_COLLISIONS=no or
	# the informational messages can be disabled by AVOID_COLLISIONS=silent.
	#
	workdir=$(workdir); \
	if [ `expr "$(OPTIONS)" : ".*Shared.*" ` -gt 0 ]; then \
	    echo "Making $@) [from $(<F)] [shared-ddl-1]"; \
	    workdir=$(shworkdir); \
	else \
	    echo "Making $@) [from $(<F)] [ddl-1]"; \
	fi; \
	if [ -r $${workdir}$(<F) ]; then \
	    $(RM) -f $${workdir}$(<F); \
	fi; \
	cp -f $< $${workdir}$(<F); \
	usePkg=`echo $(PACKAGE) | sed 's/++/pp/g'`; \
	schema=""; \
	evolve=""; \
	upgrade=""; \
	version=""; \
	frozen=""; \
	keep_typedefs=""; \
	if [ "$(OBJY_VERSION)" != "5.1" -a "$(KEEP_TYPEDEFS)" != "" ]; then \
	    if [ "$(filter $<, $(KEEP_TYPEDEFS))" != "" ]; then \
		    keep_typedefs="-keep_typedefs"; \
		    echo "The -keep_typedefs option has been set for backwards compatibility"; \
	    fi; \
	fi; \
	if [ "$(SCHEMA_FROZEN)" != "" ]; then \
	    frozen="-nochange"; \
	    evolve=""; \
	    version=""; \
	fi; \
	if [ "$(VERSION_SCHEMA)" != "" ]; then \
	    if [ "$(filter $(<F:.ddl=), $(VERSION_SCHEMA))" != "" ]; then \
		if [ "$$frozen" = "" ]; then \
		    version="-version"; \
		else \
		    echo "The schema are frozen - schema versioning is disabled"; \
		    version=""; \
		fi; \
	    fi; \
	fi; \
	if [ "$(EVOLVE_SCHEMA)" != "" ]; then \
	    if [ "$$frozen" = "" ]; then \
		evolve=""; \
		if [ "$(EVOLVE_SCHEMA)" = "all" -o "$(filter $(<F:.ddl=), $(EVOLVE_SCHEMA))" != "" ]; then \
		    evolve="-evolve"; \
		fi; \
	    else \
		echo "The schema are frozen - schema evolution is disabled"; \
		evolve=""; \
	    fi; \
	fi; \
	if [ "$$evolve" != "" -a "$$version" != "" ]; then \
	    echo "Schema evolution and versioning cannot both be specified - both ignored"; \
	    evolve=""; \
	    version=""; \
	fi; \
	if [ "$$evolve" != "" ]; then \
	    echo "Schema evolution enabled\c"; \
	    if [ "$(UPGRADE_SCHEMA)" != "" ]; then \
		upgrade="-upgrade"; \
		echo ":upgrade application required"; \
	    else \
		echo; \
	    fi; \
	fi; \
	if [ "$$version" != "" ]; then \
	    echo "Schema versioning enabled"; \
	fi; \
	cd $${workdir}; \
	if [ "$(AVOID_COLLISIONS)" != "no" ]; then \
	    wait=""; \
	    unset noclobber; \
	    count=0; \
	    myhost=`hostname`; \
	    while [ "$$wait" = "" ]; do \
		if [ ! -f $(BOOT_FILE_DIR)/.ddllock ]; then \
		    hostname > $(BOOT_FILE_DIR)/.ddllock; \
		fi; \
		owner=`cat -s $(BOOT_FILE_DIR)/.ddllock`; \
		if [ "$$owner" != "" -a "$$owner" != "$$myhost" ]; then \
		    if [ $$count -lt 60 ]; then \
			if [ $$count -eq 0 -a "$(AVOID_COLLISIONS)" != "silent" ]; then \
			    echo "DDL compiler collision with $$owner - waiting..."; \
			fi; \
			count=`expr $$count + 1`; \
			sleep 1; \
		    else \
			if [ "$(AVOID_COLLISIONS)" != "silent" ]; then \
			    echo "DDL compiler collision timeout reached - attempting override..."; \
			fi; \
			$(RM) -f $(BOOT_FILE_DIR)/.ddllock; \
			count=0; \
			sleep 30; \
		    fi; \
		else \
		    wait="no"; \
		fi; \
	    done; \
	fi; \
	failed=0; \
	mv $${workdir}$*$(HH_SUFFIX) $${workdir}$*$(HH_SUFFIX).before 1>/dev/null 2>&1; \
	mv $${workdir}$*_ref$(HH_SUFFIX) $${workdir}$*_ref$(HH_SUFFIX).before 1>/dev/null 2>&1; \
	$(OODDL) $(DDLFLAGS) \
		 $$evolve $$upgrade $$version $$schema $$frozen $$keep_typedefs \
		 $(filter-out $(DDLCPPREMOVALS),$(CPPFLAGS)) \
		 $$useNamed $$useAssigned \
		-include_header $(PACKAGE)/$(<F:.ddl=$(HH_SUFFIX)) \
		-include_ref $(PACKAGE)/$(<F:.ddl=$(REF_HH_SUFFIX)) \
		$< $(BOOT_FILE); \
	mv $@ $@.toReplace; \
	echo \#include \"BaBar/BaBar.hh\" | cat - $@.toReplace >>$@; \
	rm -f $@.toReplace; \
	if [ $$? -ne 0 ]; then \
	    failed=1; \
	    echo "DDL processing failed"; \
	    ddlout=$(<F:.ddl=$(DDL_CC_SUFFIX)); \
	    if [ -f $$ddlout ]; then \
		echo "Warning!!!! DDL generated files exist - being cleaned up"; \
		$(RM) -f $(<F:.ddl=$(HH_SUFFIX)) \
			 $(<F:.ddl=$(REF_HH_SUFFIX)) \
			 $(<F:.ddl=$(DDL_CC_SUFFIX)); \
	    fi; \
	fi; \
	if [ "$(AVOID_COLLISIONS)" != "no" ]; then \
	    $(RM) -f $(BOOT_FILE_DIR)/.ddllock; \
	fi; \
	if [ $$failed -ne 0 ]; then \
	    if [ "$(PRODUCTION_RELEASE)" != "" -a  "$(SCHEMA_ERROR_IGNORE)" = "" ]; then \
		echo "Production release being aborted"; \
	    fi; \
	    rm -f $${workdir}$*$(HH_SUFFIX).before $${workdir}$*_ref$(HH_SUFFIX).before; \
	    exit 2; \
	else \
	    if [ "$(OBJY_VERSION_MAJOR)" = "5" -a -r $${workdir}$(<F:.ddl=$(HH_SUFFIX)) ]; then \
	        bugs=`grep -c "enum [a-zA-Z].*::" $${workdir}$(<F:.ddl=$(HH_SUFFIX))`; \
		if [ $$bugs -gt 0 ]; then \
		    echo "$(<F:.ddl=$(HH_SUFFIX)) being postprocessed for Objectivity V5 enum bug"; \
		    sed 's/enum \([a-zA-Z].*\)::/\1::/g' $${workdir}$(<F:.ddl=$(HH_SUFFIX)) > $${workdir}$(<F:.ddl=$(HH_SUFFIX))_tmp; \
		    mv -f $${workdir}$(<F:.ddl=$(HH_SUFFIX))_tmp $${workdir}$(<F:.ddl=$(HH_SUFFIX)); \
		fi; \
	    fi; \
	    diff $${workdir}$*$(HH_SUFFIX).before $${workdir}$*$(HH_SUFFIX) 1>/dev/null 2>&1; rc1=$$?; \
	    diff $${workdir}$*_ref$(HH_SUFFIX).before $${workdir}$*_ref$(HH_SUFFIX) 1>/dev/null 2>&1; rc2=$$?; \
	    if [ "$$rc1" -eq 0 -a "$$rc2" -eq 0 ]; then cd $${workdir}; mv $*$(HH_SUFFIX).before $*$(HH_SUFFIX); mv $*_ref$(HH_SUFFIX).before $*_ref$(HH_SUFFIX); fi; \
	    rm -f $${workdir}$*$(HH_SUFFIX).before $${workdir}$*_ref$(HH_SUFFIX).before; \
	fi;
ifneq (,$(findstring OSF1,$(BFARCH)))
	workdir=$(workdir); \
	if [ `expr "$(OPTIONS)" : ".*Shared.*" ` -gt 0 ]; then \
	    workdir=$(shworkdir); \
	fi; \
	touch $${workdir}$(<F:.ddl=_ref$(CC_SUFFIX))
endif
endif	# OBJYBASE
endif	# -Objy

#-> multiple dependency rule to make _ddl.d files. Force gmake to update
#   all out-of-date _ddl.cc first.
ifneq (,$(findstring Objy,$(OPTIONS)))
ifneq (,$(findstring Shared,$(OPTIONS)))
$(foreach var,$(LIBDDLFILES:.ddl=_ddl.d),$(shworkdir)$(var)): $(foreach var,$(LIBDDLFILESrev:.ddl=$(DDL_CC_SUFFIX)),$(shworkdir)$(var))
else
$(foreach var,$(LIBDDLFILES:.ddl=_ddl.d),$(workdir)$(var)): $(foreach var,$(LIBDDLFILESrev:.ddl=$(DDL_CC_SUFFIX)),$(workdir)$(var))
endif
	workdir=$(workdir); \
	stem=`echo $(notdir $*) | sed -e 's/_ddl//g'`; \
	if [ `expr "$(OPTIONS)" : ".*Shared.*" ` -gt 0 ]; then \
	    echo "Making $@ [from $${stem}$(DDL_CC_SUFFIX)] [shared-ddl1]"; \
	    workdir=$(shworkdir); \
	else \
	    echo "Making $@ [from $${stem}$(DDL_CC_SUFFIX)}] [ddl1]"; \
	fi; \
	cd $$workdir; \
	uselib=$(libdir); \
	if [ $(LIB) != "" ]; then \
	    uselib=$(LIB); \
	fi; \
	ddlout="$${workdir}$${stem}$(DDL_CC_SUFFIX)"; \
        if [ -f "$$ddlout" ]; then \
	    if [ `expr "$(OPTIONS)" : ".*Shared.*" ` -gt 0 ]; then \
		$(CPP) $(MFLAG) $(GXXFLAGS) $(CPPFLAGS) $$ddlout | \
		sed "s?$${workdir}$${stem}$(DDL_CC_SUFFIX)?$${stem}.ddl?g" | \
		sed "s?$${stem}_ddl\.o?$(shobjdir)$${stem}_ddl.o $${workdir}$${stem}$(DDL_CC_SUFFIX) ?g" \
		> $@ ; \
	    else \
		$(CPP) $(MFLAG) $(GXXFLAGS) $(CPPFLAGS) $$ddlout | \
		sed "s?$${workdir}$${stem}$(DDL_CC_SUFFIX)?$${stem}.ddl?g" | \
		sed "s?$${stem}_ddl\.o?$$uselib\($${stem}_ddl.o) $${workdir}$${stem}$(DDL_CC_SUFFIX) ?g" \
		> $@ ; \
	    fi; \
	fi
endif	# -Objy

#__________________________________________________________________________
#++ cleaning up

cleanlib: $(foreach v,$(SUBDIRS),$v.cleanlib)
	-if [ "$(LIB)$(LIB2)$(SHLIB)$(SHLIB2)$(SHLIBEXT)$(ROOTLIB)$(ROOTMULTILIB)" ]; then $(RM) $(LIB) $(LIB2) $(SHLIB) $(SHLIB2) $(SHLIBEXT) $(ROOTLIB) $(ROOTMULTILIB); fi
	-if [ "$(LIBCCOBJS)" ]; then $(RM) $(foreach V,$(LIBCCOBJS),$(libdir)$(V:$(CC_SUFFIX)=.o)); fi

cleanbin: $(foreach v,$(SUBDIRS),$v.cleanbin)
	-if [ "$(ALLBINS)" ] ; then $(RM) $(addprefix $(bindir),$(ALLBINS)); fi

clean:	wclean $(foreach v,$(SUBDIRS),$v.clean)
	@(true; cd $(TOPDIR); $(foreach var,$(BFARCHES),$(MAKE) BFARCH=$(var) NOTFIRST=0 $(PACKAGE).cleanarch;))

cleanarch:	wcleanarch $(foreach v,$(SUBDIRS),$v.cleanarch)
	-if [ "$(LIB)$(LIB2)$(SHLIB)$(SHLIB2)$(SHLIBEXT)$(ROOTLIB)$(ROOTMULTILIB)" ]; then \
	    if [ "$(DEBUG20)" ]; then set -x; fi; \
	    echo $(RM) "$(LIB) $(LIB2) $(SHLIB) $(SHLIB2) $(SHLIBEXT) $(ROOTLIB) $(ROOTMULTILIB)" | sed s@$(TOPDIR)/@@g; \
	    $(RM) $(LIB) $(LIB2) $(SHLIB) $(SHLIB2) $(SHLIBEXT) $(ROOTLIB) $(ROOTMULTILIB); \
	fi
	-if [ "$(LIBCCOBJS)" ]; then \
	    if [ "$(DEBUG20)" ]; then set -x; fi; \
	    echo $(RM) "$(foreach V,$(LIBCCOBJS),$(libdir)$(V:$(CC_SUFFIX)=.o))" | sed s@$(TOPDIR)/@@g; \
	    $(RM) $(foreach V,$(LIBCCOBJS),$(libdir)$(V:$(CC_SUFFIX)=.o)); \
	fi
	-if [ "$(ALLBINS)" ] ; then \
	    echo "$(RM) $(addprefix $(bindir),$(ALLBINS))" | sed s@$(TOPDIR)/@@g; \
	    $(RM) $(addprefix $(bindir),$(ALLBINS)); \
	fi
	-if [ -d $(pydirpkg) ] ; then \
	    echo "$(RM) -r $(pydirpkg)" | sed s@$(TOPDIR)/@@g; \
	    $(RM) -r $(pydirpkg); \
	fi
	-list=; \
	cd $(TOPDIR); \
	for v in lib/$(BFARCH)/templates/$(PACKAGE) shlib/$(BFARCH)/templates/$(PACKAGE) tmp/$(BFARCH)/$(PACKAGE) shtmp/$(BFARCH)/$(PACKAGE) test/$(BFARCH)/$(PACKAGE); do \
	    dir="$$v"; \
	    if [ -h "$$v" ]; then \
		linkto=`ls -ldt $$v | awk '{print $$NF}' -`; \
		echo "$$v is soft link to $$linkto"; \
	        list="$$list $$linkto"; \
	        if [ "$(NO_INSTALLDIRS)" ] ; then \
	    	    list="$$list $$dir"; \
	        fi; \
	    else \
	        list="$$list $$dir"; \
	    fi; \
	done; \
        if [ "$$list" ]; then \
	    echo "$(RM) -r $$list"; \
	    $(RM) -r $$list; \
	fi; \
	if [ "$(NO_INSTALLDIRS)" = "" ] ; then \
	   if [ -n "$$list" ]; then \
	       echo "mkdir $$list"; \
	       mkdir $$list; \
	   fi; \
	   $(MAKE) -C $(TOPDIR) installdirs PACKAGES=$(PACKAGE); ONEDIR=1; \
	fi

#__________________________________________________________________________
#++ rules for running tests
$(testdir)%.T: %.t $(foreach v,$(BINS),$(bindir)$v)
	echo "Testing $<" ; \
	srcdir=`/bin/pwd`; \
	export srcdir; \
	cd $(testdir); \
	if [ -r shlib ] ; then rm shlib; fi; \
	ln -f -s ../../../shlib shlib ; \
	if [ -r bin ] ; then rm bin; fi; \
	ln -f -s ../../../bin bin ; \
	if [ -d RELEASE ] ; then rm RELEASE; fi; \
	ln -f -s ../../.. RELEASE ; \
	if [ -d PARENT ] ; then rm PARENT; fi; \
	if [ -r RELEASE/.current ] ; then \
	  ln -f -s ${BFDIST}/releases/`cat RELEASE/.current` PARENT ; \
	else \
	  ln -f -s RELEASE PARENT ; \
	fi; \
	testit $$srcdir/$(<F) $@ ; \
	$(RM) PARENT RELEASE shlib bin

$(testdir)%.pass: $(testdir)%.T
	if [ -f $@ ]; then rm $@; fi; \
	if [ -f $(@:.pass=.fail) ]; then rm $(@:.pass=.fail); fi; \
	if ( diff $(*F).t $< > $(@:.pass=.fail) ); then \
	  rm -f $(@:.pass=.fail); \
	  touch $@; \
	  echo Test $(*) passed; \
	else \
	  echo Test $(*) failed; \
	fi

#__________________________________________________________________________
#++ rules to read dependency makefiles
#  these rules force gmake to create .d files.
#  (Note problem if you have both foo.c and foo.cc)
#  (Note no .d files for .f, as no includes unless .F suffix used
ifneq (,$(filter-out $(skip_include_d),$(MAKEGOAL)))
ifeq (,$(findstring Skipdfile,$(OPTIONS)))
  ifdef DEBUG10
    ERROR := $(shell echo "including .d files. MAKEGOAL=$(MAKEGOAL) \t[s-9]" >& 2)
  endif
ifneq ($(MAKEGOAL),schema)
  ifneq ($(BINARIES),)
    ifdef BINCCFILES 
      -include $(addprefix $(workdir),$(BINCCFILES:$(CC_SUFFIX)=.d)) /dev/null
    endif
    ifdef BINCFILES 
      -include $(addprefix $(workdir),$(BINCFILES:.c=.d)) /dev/null
    endif
    ifdef BINBIGFFILES
      -include $(addprefix $(workdir),$(BINBIGFFILES:.F=.d)) /dev/null
    endif
    ifdef BINSMALLFFILES
      -include $(addprefix $(workdir),$(BINSMALLFFILES:.f=.d)) /dev/null
    endif
  endif # BINARIES
  ifdef LIBCFILES
    -include $(foreach var,$(LIBCFILES:.c=.d),$(workdir)$(var:$(workdir)/%=%)) /dev/null
  endif
  ifdef LIB2CFILES
    -include $(foreach var,$(LIB2CFILES:.c=.d),$(workdir)$(var:$(workdir)/%=%)) /dev/null
  endif
  ifdef LIBFFILES
    -include $(foreach var,$(LIBFFILES:.F=.d),$(workdir)$(var:$(workdir)/%=%)) /dev/null
  endif
  ifdef LIB2FFILES
    -include $(foreach var,$(LIB2FFILES:.F=.d),$(workdir)$(var:$(workdir)/%=%)) /dev/null
  endif
  ifdef LIBF90FILES
    -include $(foreach var,$(LIBF90FILES:.f90=.d),$(workdir)$(var:$(workdir)/%=%)) /dev/null
  endif
  ifdef LIB2F90FILES
    -include $(foreach var,$(LIB2F90FILES:.f90=.d),$(workdir)$(var:$(workdir)/%=%)) /dev/null
  endif

  #-> include the following lines for Static mode only
  ifeq (,$(findstring Shared,$(OPTIONS)))
    ifdef LIBCCFILES 
      -include $(foreach var,$(LIBCCFILES:$(CC_SUFFIX)=.d),$(workdir)$(var:$(workdir)/%=%)) /dev/null
    endif
    ifdef LIB2CCFILES 
      -include $(foreach var,$(LIB2CCFILES:$(CC_SUFFIX)=.d),$(workdir)$(var:$(workdir)/%=%)) /dev/null
    endif
    ifdef LIBCFILES
      -include $(foreach var,$(LIBCFILES:.c=.d),$(workdir)$(var:$(workdir)/%=%)) /dev/null
    endif
    ifdef LIB2CFILES
      -include $(foreach var,$(LIB2CFILES:.c=.d),$(workdir)$(var:$(workdir)/%=%)) /dev/null
    endif
    ifdef LIBIDLFILES
      -include $(foreach var,$(LIBIDLFILES:.idl=_idl.d),$(workdir)$(var)) /dev/null
    endif
    ifdef LIB2IDLFILES
      -include $(foreach var,$(LIB2IDLFILES:.idl=_idl.d),$(workdir)$(var)) /dev/null
    endif
  endif
endif    # schema
endif    # skipdfile

#-> include _ddl.d files if (schema) or (not Skipdfile)
ifeq (,$(findstring Skipdfile,$(OPTIONS)))
  getddl:=1
endif
ifeq ($(MAKEGOAL),schema)
  getddl:=1
endif

ifeq (1,$(getddl)) 
    ifdef LIBIDLFILES
      -include $(foreach var,$(LIBIDLFILES:.idl=_idl.d),$(workdir)$(var)) /dev/null
    endif
    ifdef LIB2IDLFILES
      -include $(foreach var,$(LIB2IDLFILES:.idl=_idl.d),$(workdir)$(var)) /dev/null
    endif
  #-> include DDL dependency files for Static mode only
  ifneq (,$(findstring Objy,$(OPTIONS)))
  ifeq (,$(findstring Shared,$(OPTIONS)))
    ifdef LIBDDLFILES 
      -include $(foreach var,$(LIBDDLFILES:.ddl=_ddl.d),$(workdir)$(var)) /dev/null
    endif
    ifdef LIB2DDLFILES 
      -include $(foreach var,$(LIB2DDLFILES:.ddl=_ddl.d),$(workdir)$(var)) /dev/null
    endif
  endif  # ~Shared
  endif  # Objy
endif    # getddl
endif    # skip_include_d

#__________________________________________________________________________
#++ pattern rule to process subdirectories
%.schema:
	@echo "-> $(PACKAGE).$@"; \
	$(MAKE) -C $(@:.schema=) srcdir=$$srcdir/$(@:.schema=) schema

%.include:
	@$(wdir); $(MAKE) -C $(@:.include=) srcdir=$$srcdir/$(@:.include=) include

%.inc:
	@$(wdir); $(MAKE) -C $(@:.inc=) srcdir=$$srcdir/$(@:.inc=) inc

%.corba:
	@$(wdir); $(MAKE) -C $(@:.corba=) srcdir=$$srcdir/$(@:.corba=) corba

%.lib:
	@$(wdir); $(MAKE) -C $(@:.lib=) srcdir=$$srcdir/$(@:.lib=) lib

%.bin:
	@$(wdir); $(MAKE) -C $(@:.bin=) srcdir=$$srcdir/$(@:.bin=) bin

%.bincomp:
	@$(wdir); $(MAKE) -C $(@:.bincomp=) srcdir=$$srcdir/$(@:.bincomp=) bincomp

%.extrabin:
	@$(wdir); $(MAKE) -C $(@:.extrabin=) srcdir=$$srcdir/$(@:.extrabin=) extrabin

%.binscripts:
	@$(wdir); $(MAKE) -C $(@:.binscripts=) srcdir=$$srcdir/$(@:.binscripts=) binscripts

%.extrabinscripts:
	@$(wdir); $(MAKE) -C $(@:.extrabinscripts=) srcdir=$$srcdir/$(@:.extrabinscripts=) extrabinscripts

%.allbin:
	@$(wdir); $(MAKE) -C $(@:.allbin=) srcdir=$$srcdir/$(@:.allbin=) allbin

%.test:
	@$(wdir); $(MAKE) -C $(@:.test=) srcdir=$$srcdir/$(@:.test=) test

%.man:
	@$(wdir); $(MAKE) -C $(@:.man=) srcdir=$$srcdir/$(@:.man=) man

%.clean:
	@$(wdir); $(MAKE) -C $(@:.clean=) srcdir=$$srcdir/$(@:.clean=) clean

%.cleanarch:
	@$(wdir); $(MAKE) -C $(@:.cleanarch=) srcdir=$$srcdir/$(@:.cleanarch=) OPTIONS+=-Skipdfile cleanarch

%.cleanbin:
	@$(wdir); $(MAKE) -C $(@:.cleanbin=) srcdir=$$srcdir/$(@:.cleanbin=) cleanbin

%.cleanlib:
	@$(wdir); $(MAKE) -C $(@:.cleanlib=) srcdir=$$srcdir/$(@:.cleanlib=) cleanlib

%.ldlink:
	@$(wdir); $(MAKE) -C $(@:.ldlink=) srcdir=$$srcdir/$(@:.ldlink=) ldlink

#__________________________________________________________________________
# Make all variables visible for printing, except extra-large ones
ifneq (,$(findstring $(MAKEGOAL),printenv linkfiles))

  #_____________________________________________________________________
  # Gmake 3.79: export all variables to STDOUT except big ones.

  ifndef .VARIABLES
      unexport BINCOMPPACKAGES BINCOMPPACKAGES_1 BINCOMPPACKAGES_2 \
	BINPACKAGES BINPACKAGES_1 BINPACKAGES_2 \
	GROUP_DIR \
	INCPACKAGES INCPACKAGES_1 INCPACKAGES_2 \
	LIBPACKAGES_1 LIBPACKAGES_2 LINKLISTDEPENDS PACKAGES PACKAGELIST \
	ROOTCPPFLAGS SCHEMALIST SCHEMAPACK SUBDIRS binlist keep_bin \
	CPPFLAGSmod SYSLIB ALLLIB REALSYSLIB JAVATUPLE ROOTONLYLIBS \
	ROOTCPPFLAGS MAKEFLAGS TUPLEJAR LDFLAGS LOADLIBES ROOTALLLIBS OVERRIDES
      export

  #_____________________________________________________________________
  # Gmake 3.80: display ALL variables to STDERR directly

  else
      ERR := $(foreach V,$(sort $(.VARIABLES)), $(if $(filter-out default automatic, $(origin $V)), $(if $(filter-out MAKEFILE_LIST, $V), $(shell echo '$V=$(subst ','"'"',$($V))'>&2))))
  endif
endif

#____________________________________________________________________________
# End of part that is not to be executed by 'gmake echoMacro'

endif   # ECHOMACRONAME=EXTRABINS
endif   # ECHOMACRONAME=BINS

.PHONY: printenv

#__________________________________________________________________________
#  The target echoMacro echos out the value of a macro.  The macro to
#  echo is set via ECHOMACRONAME, e.g.,
#       gmake ECHOMACRONAME=BINS myPackage.echoMacro
#  We start the echo with a token, "echoMacro($ECHOMACRONAME)=" to facilitate
#  extraction via scripts (NB:  the makefile prints out lots of other stuff
#  too, which we can not turn off).  -Ed Frank
#
#  Some variables can be too long to sent in full to the shell, so we break
#  this up into safer chunks (assuming the long strings have spaces!) and use
#  |printf| instead of |echo| to avoid line breaks until the very end.

echoMacro:
	@printf 'echoMacro(%s)= ' $(ECHOMACRONAME)
	@printf '%s ' "$(wordlist 1,2000,$($(ECHOMACRONAME)))"
	@printf '%s ' "$(wordlist 2001,4000,$($(ECHOMACRONAME)))"
	@printf '%s ' "$(wordlist 4001,6000,$($(ECHOMACRONAME)))"
	@printf '%s ' "$(wordlist 6001,8000,$($(ECHOMACRONAME)))"
	@printf '%s ' "$(wordlist 8001,10000,$($(ECHOMACRONAME)))"
	@printf '%s\n' "$(wordlist 10001,$(words $($(ECHOMACRONAME))), $($(ECHOMACRONAME)))"

printenv: $(foreach v,$(SUBDIRS),$v.printenv)
	@echo "-> `pwd | sed -e 's%$(TOPDIR)/%%'`:"
ifndef .VARIABLES
	printenv | sort
endif

%.printenv:
	@$(wdir); $(MAKE) -C $(@:.printenv=) srcdir=$$srcdir/$(@:.printenv=) printenv

#--> include add-on file
-include SoftRelTools/standard.mk+
include SoftRelTools/incr.mk
