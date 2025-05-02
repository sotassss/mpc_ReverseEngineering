################################################################################
#                               NetCOBOL V7.2L10
################################################################################

.SUFFIXES:

COBOL_PATH = C:\Program Files\NetCOBOL
PROJECT_PATH = \\HOST2\MAKISHI\COMSYS\PG
OBJECT_PATH = 
LINK_DEBUG_OPTION = 
LINK_MAP_OPTION = 


ALL : "obj\edachk.dll" "C:\makishisys\hmobj\edachk.dll" "C:\makishisys\yawobj\edachk.dll" 

REBUILD : CLEAN ALL
CLEAN :
	DEL "obj\edachk.dll" 
	DEL "obj\edachk.LIB" 
	DEL "edachk.OBJ" 
	DEL "C:\makishisys\hmobj\edachk.dll" 
	DEL "C:\makishisys\hmobj\edachk.LIB" 
	DEL "C:\makishisys\yawobj\edachk.dll" 
	DEL "C:\makishisys\yawobj\edachk.LIB" 


"edachk.OBJ" : "edachk.cob" "EDACHK.CBI"
	COBOL32.EXE -i"EDACHK.CBI" -NM "edachk.cob"

"obj\edachk.dll" : "edachk.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\edachk.dll" \
	@"EDACHK.001" 

"obj\edachk.LIB" : "edachk.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\edachk.dll" \
	@"EDACHK.001" 


"C:\makishisys\hmobj\edachk.dll" : "edachk.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\edachk.dll" \
	@"EDACHK.002" 

"C:\makishisys\hmobj\edachk.LIB" : "edachk.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\edachk.dll" \
	@"EDACHK.002" 


"C:\makishisys\yawobj\edachk.dll" : "edachk.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\edachk.dll" \
	@"EDACHK.003" 

"C:\makishisys\yawobj\edachk.LIB" : "edachk.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\edachk.dll" \
	@"EDACHK.003" 


