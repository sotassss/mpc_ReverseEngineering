################################################################################
#                               NetCOBOL V7.2L10
################################################################################

.SUFFIXES:

COBOL_PATH = C:\Program Files\NetCOBOL
PROJECT_PATH = P:\COMSYS\PG
OBJECT_PATH = 
LINK_DEBUG_OPTION = 
LINK_MAP_OPTION = 


ALL : "obj\k41410.dll" "C:\makishisys\hmobj\k41410.dll" "C:\makishisys\yawobj\k41410.dll" 

REBUILD : CLEAN ALL
CLEAN :
	DEL "obj\k41410.dll" 
	DEL "obj\k41410.LIB" 
	DEL "k41410.OBJ" 
	DEL "C:\makishisys\hmobj\k41410.dll" 
	DEL "C:\makishisys\hmobj\k41410.LIB" 
	DEL "C:\makishisys\yawobj\k41410.dll" 
	DEL "C:\makishisys\yawobj\k41410.LIB" 


"k41410.OBJ" : "k41410.cob" "K41410.CBI"
	COBOL32.EXE -i"K41410.CBI" -NM "k41410.cob"

"obj\k41410.dll" : "k41410.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\k41410.dll" \
	@"K41410.001" 

"obj\k41410.LIB" : "k41410.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\k41410.dll" \
	@"K41410.001" 


"C:\makishisys\hmobj\k41410.dll" : "k41410.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\k41410.dll" \
	@"K41410.002" 

"C:\makishisys\hmobj\k41410.LIB" : "k41410.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\k41410.dll" \
	@"K41410.002" 


"C:\makishisys\yawobj\k41410.dll" : "k41410.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\k41410.dll" \
	@"K41410.003" 

"C:\makishisys\yawobj\k41410.LIB" : "k41410.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\k41410.dll" \
	@"K41410.003" 


