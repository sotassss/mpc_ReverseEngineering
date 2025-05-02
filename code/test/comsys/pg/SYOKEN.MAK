################################################################################
#                               NetCOBOL V7.2L10
################################################################################

.SUFFIXES:

COBOL_PATH = C:\Program Files\NetCOBOL
PROJECT_PATH = P:\COMSYS\PG
OBJECT_PATH = 
LINK_DEBUG_OPTION = 
LINK_MAP_OPTION = 


ALL : "obj\syoken.dll" "C:\makishisys\hmobj\syoken.dll" "C:\makishisys\yawobj\syoken.dll" 

REBUILD : CLEAN ALL
CLEAN :
	DEL "obj\syoken.dll" 
	DEL "obj\syoken.LIB" 
	DEL "syoken.OBJ" 
	DEL "C:\makishisys\hmobj\syoken.dll" 
	DEL "C:\makishisys\hmobj\syoken.LIB" 
	DEL "C:\makishisys\yawobj\syoken.dll" 
	DEL "C:\makishisys\yawobj\syoken.LIB" 


"syoken.OBJ" : "syoken.cob" "SYOKEN.CBI"
	COBOL32.EXE -i"SYOKEN.CBI" -NM "syoken.cob"

"obj\syoken.dll" : "syoken.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\syoken.dll" \
	@"SYOKEN.001" 

"obj\syoken.LIB" : "syoken.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\syoken.dll" \
	@"SYOKEN.001" 


"C:\makishisys\hmobj\syoken.dll" : "syoken.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\syoken.dll" \
	@"SYOKEN.002" 

"C:\makishisys\hmobj\syoken.LIB" : "syoken.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\syoken.dll" \
	@"SYOKEN.002" 


"C:\makishisys\yawobj\syoken.dll" : "syoken.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\syoken.dll" \
	@"SYOKEN.003" 

"C:\makishisys\yawobj\syoken.LIB" : "syoken.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\syoken.dll" \
	@"SYOKEN.003" 


