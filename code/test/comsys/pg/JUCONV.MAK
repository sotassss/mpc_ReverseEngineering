################################################################################
#                               NetCOBOL V7.2L10
################################################################################

.SUFFIXES:

COBOL_PATH = C:\Program Files\NetCOBOL
PROJECT_PATH = P:\COMSYS\PG
OBJECT_PATH = 
LINK_DEBUG_OPTION = 
LINK_MAP_OPTION = 


ALL : "obj\juconv.dll" "C:\makishisys\hmobj\juconv.dll" "C:\makishisys\yawobj\juconv.dll" 

REBUILD : CLEAN ALL
CLEAN :
	DEL "obj\juconv.dll" 
	DEL "obj\juconv.LIB" 
	DEL "juconv.OBJ" 
	DEL "C:\makishisys\hmobj\juconv.dll" 
	DEL "C:\makishisys\hmobj\juconv.LIB" 
	DEL "C:\makishisys\yawobj\juconv.dll" 
	DEL "C:\makishisys\yawobj\juconv.LIB" 


"juconv.OBJ" : "juconv.cob" "JUCONV.CBI"
	COBOL32.EXE -i"JUCONV.CBI" -NM "juconv.cob"

"obj\juconv.dll" : "juconv.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\juconv.dll" \
	@"JUCONV.001" 

"obj\juconv.LIB" : "juconv.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\juconv.dll" \
	@"JUCONV.001" 


"C:\makishisys\hmobj\juconv.dll" : "juconv.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\juconv.dll" \
	@"JUCONV.002" 

"C:\makishisys\hmobj\juconv.LIB" : "juconv.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\juconv.dll" \
	@"JUCONV.002" 


"C:\makishisys\yawobj\juconv.dll" : "juconv.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\juconv.dll" \
	@"JUCONV.003" 

"C:\makishisys\yawobj\juconv.LIB" : "juconv.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\juconv.dll" \
	@"JUCONV.003" 


