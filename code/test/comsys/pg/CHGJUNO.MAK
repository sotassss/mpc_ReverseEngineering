################################################################################
#                               NetCOBOL V7.2L10
################################################################################

.SUFFIXES:

COBOL_PATH = C:\Program Files\NetCOBOL
PROJECT_PATH = P:\COMSYS\PG
OBJECT_PATH = 
LINK_DEBUG_OPTION = 
LINK_MAP_OPTION = 


ALL : "obj\chgjuno.dll" "C:\makishisys\hmobj\chgjuno.dll" "C:\makishisys\yawobj\chgjuno.dll" 

REBUILD : CLEAN ALL
CLEAN :
	DEL "obj\chgjuno.dll" 
	DEL "obj\chgjuno.LIB" 
	DEL "chgjuno.OBJ" 
	DEL "C:\makishisys\hmobj\chgjuno.dll" 
	DEL "C:\makishisys\hmobj\chgjuno.LIB" 
	DEL "C:\makishisys\yawobj\chgjuno.dll" 
	DEL "C:\makishisys\yawobj\chgjuno.LIB" 


"chgjuno.OBJ" : "chgjuno.cob" "CHGJUNO.CBI"
	COBOL32.EXE -i"CHGJUNO.CBI" -NM "chgjuno.cob"

"obj\chgjuno.dll" : "chgjuno.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\chgjuno.dll" \
	@"CHGJUNO.001" 

"obj\chgjuno.LIB" : "chgjuno.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\chgjuno.dll" \
	@"CHGJUNO.001" 


"C:\makishisys\hmobj\chgjuno.dll" : "chgjuno.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\chgjuno.dll" \
	@"CHGJUNO.002" 

"C:\makishisys\hmobj\chgjuno.LIB" : "chgjuno.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\chgjuno.dll" \
	@"CHGJUNO.002" 


"C:\makishisys\yawobj\chgjuno.dll" : "chgjuno.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\chgjuno.dll" \
	@"CHGJUNO.003" 

"C:\makishisys\yawobj\chgjuno.LIB" : "chgjuno.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\chgjuno.dll" \
	@"CHGJUNO.003" 


