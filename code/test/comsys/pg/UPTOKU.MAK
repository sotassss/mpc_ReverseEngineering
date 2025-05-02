################################################################################
#                               NetCOBOL V7.2L10
################################################################################

.SUFFIXES:

COBOL_PATH = C:\Program Files\NetCOBOL
PROJECT_PATH = P:\COMSYS\PG
OBJECT_PATH = 
LINK_DEBUG_OPTION = 
LINK_MAP_OPTION = 


ALL : "obj\uptoku.dll" "C:\makishisys\hmobj\uptoku.dll" "C:\makishisys\yawobj\uptoku.dll" 

REBUILD : CLEAN ALL
CLEAN :
	DEL "obj\uptoku.dll" 
	DEL "obj\uptoku.LIB" 
	DEL "uptoku.OBJ" 
	DEL "C:\makishisys\hmobj\uptoku.dll" 
	DEL "C:\makishisys\hmobj\uptoku.LIB" 
	DEL "C:\makishisys\yawobj\uptoku.dll" 
	DEL "C:\makishisys\yawobj\uptoku.LIB" 


"uptoku.OBJ" : "uptoku.cob" "UPTOKU.CBI"
	COBOL32.EXE -i"UPTOKU.CBI" -NM "uptoku.cob"

"obj\uptoku.dll" : "uptoku.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\uptoku.dll" \
	@"UPTOKU.001" 

"obj\uptoku.LIB" : "uptoku.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\uptoku.dll" \
	@"UPTOKU.001" 


"C:\makishisys\hmobj\uptoku.dll" : "uptoku.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\uptoku.dll" \
	@"UPTOKU.002" 

"C:\makishisys\hmobj\uptoku.LIB" : "uptoku.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\uptoku.dll" \
	@"UPTOKU.002" 


"C:\makishisys\yawobj\uptoku.dll" : "uptoku.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\uptoku.dll" \
	@"UPTOKU.003" 

"C:\makishisys\yawobj\uptoku.LIB" : "uptoku.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\uptoku.dll" \
	@"UPTOKU.003" 


