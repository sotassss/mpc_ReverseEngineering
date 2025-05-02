################################################################################
#                               NetCOBOL V7.2L10
################################################################################

.SUFFIXES:

COBOL_PATH = C:\Program Files\NetCOBOL
PROJECT_PATH = P:\COMSYS\PG
OBJECT_PATH = 
LINK_DEBUG_OPTION = 
LINK_MAP_OPTION = 


ALL : "obj\KHT41410.dll" "C:\makishisys\hmobj\KHT41410.dll" "C:\makishisys\yawobj\KHT41410.dll" 

REBUILD : CLEAN ALL
CLEAN :
	DEL "obj\KHT41410.dll" 
	DEL "obj\KHT41410.LIB" 
	DEL "KHT41410.OBJ" 
	DEL "C:\makishisys\hmobj\KHT41410.dll" 
	DEL "C:\makishisys\hmobj\KHT41410.LIB" 
	DEL "C:\makishisys\yawobj\KHT41410.dll" 
	DEL "C:\makishisys\yawobj\KHT41410.LIB" 


"KHT41410.OBJ" : "KHT41410.COB" "KHT41410.CBI"
	COBOL32.EXE -i"KHT41410.CBI" -NM "KHT41410.COB"

"obj\KHT41410.dll" : "KHT41410.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\KHT41410.dll" \
	@"KHT41410.001" 

"obj\KHT41410.LIB" : "KHT41410.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\KHT41410.dll" \
	@"KHT41410.001" 


"C:\makishisys\hmobj\KHT41410.dll" : "KHT41410.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\KHT41410.dll" \
	@"KHT41410.002" 

"C:\makishisys\hmobj\KHT41410.LIB" : "KHT41410.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\KHT41410.dll" \
	@"KHT41410.002" 


"C:\makishisys\yawobj\KHT41410.dll" : "KHT41410.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\KHT41410.dll" \
	@"KHT41410.003" 

"C:\makishisys\yawobj\KHT41410.LIB" : "KHT41410.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\KHT41410.dll" \
	@"KHT41410.003" 


