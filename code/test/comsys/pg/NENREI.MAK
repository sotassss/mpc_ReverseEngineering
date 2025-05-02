################################################################################
#                               NetCOBOL V7.2L10
################################################################################

.SUFFIXES:

COBOL_PATH = C:\Program Files\NetCOBOL
PROJECT_PATH = P:\COMSYS\PG
OBJECT_PATH = 
LINK_DEBUG_OPTION = 
LINK_MAP_OPTION = 


ALL : "obj\nenrei.dll" "C:\makishisys\hmobj\nenrei.dll" "C:\makishisys\yawobj\nenrei.dll" 

REBUILD : CLEAN ALL
CLEAN :
	DEL "obj\nenrei.dll" 
	DEL "obj\nenrei.LIB" 
	DEL "nenrei.OBJ" 
	DEL "C:\makishisys\hmobj\nenrei.dll" 
	DEL "C:\makishisys\hmobj\nenrei.LIB" 
	DEL "C:\makishisys\yawobj\nenrei.dll" 
	DEL "C:\makishisys\yawobj\nenrei.LIB" 


"nenrei.OBJ" : "nenrei.cob" "NENREI.CBI"
	COBOL32.EXE -i"NENREI.CBI" -NM "nenrei.cob"

"obj\nenrei.dll" : "nenrei.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\nenrei.dll" \
	@"NENREI.001" 

"obj\nenrei.LIB" : "nenrei.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\nenrei.dll" \
	@"NENREI.001" 


"C:\makishisys\hmobj\nenrei.dll" : "nenrei.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\nenrei.dll" \
	@"NENREI.002" 

"C:\makishisys\hmobj\nenrei.LIB" : "nenrei.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\nenrei.dll" \
	@"NENREI.002" 


"C:\makishisys\yawobj\nenrei.dll" : "nenrei.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\nenrei.dll" \
	@"NENREI.003" 

"C:\makishisys\yawobj\nenrei.LIB" : "nenrei.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\nenrei.dll" \
	@"NENREI.003" 


