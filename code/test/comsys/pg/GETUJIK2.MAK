################################################################################
#                               NetCOBOL V7.2L10
################################################################################

.SUFFIXES:

COBOL_PATH = C:\Program Files\NetCOBOL
PROJECT_PATH = \\HOST\MAKISHI\COMSYS\PG
OBJECT_PATH = 
LINK_DEBUG_OPTION = 
LINK_MAP_OPTION = 


ALL : "obj\GETUJIK2.dll" "C:\makishisys\hmobj\GETUJIK2.dll" "C:\makishisys\yawobj\GETUJIK2.dll" 

REBUILD : CLEAN ALL
CLEAN :
	DEL "obj\GETUJIK2.dll" 
	DEL "obj\GETUJIK2.LIB" 
	DEL "GETUJIK2.OBJ" 
	DEL "C:\makishisys\hmobj\GETUJIK2.dll" 
	DEL "C:\makishisys\hmobj\GETUJIK2.LIB" 
	DEL "C:\makishisys\yawobj\GETUJIK2.dll" 
	DEL "C:\makishisys\yawobj\GETUJIK2.LIB" 


"GETUJIK2.OBJ" : "GETUJIK2.COB" "GETUJIK2.CBI"
	COBOL32.EXE -i"GETUJIK2.CBI" -NM "GETUJIK2.COB"

"obj\GETUJIK2.dll" : "GETUJIK2.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\GETUJIK2.dll" \
	@"GETUJIK2.001" 

"obj\GETUJIK2.LIB" : "GETUJIK2.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\GETUJIK2.dll" \
	@"GETUJIK2.001" 


"C:\makishisys\hmobj\GETUJIK2.dll" : "GETUJIK2.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\GETUJIK2.dll" \
	@"GETUJIK2.002" 

"C:\makishisys\hmobj\GETUJIK2.LIB" : "GETUJIK2.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\GETUJIK2.dll" \
	@"GETUJIK2.002" 


"C:\makishisys\yawobj\GETUJIK2.dll" : "GETUJIK2.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\GETUJIK2.dll" \
	@"GETUJIK2.003" 

"C:\makishisys\yawobj\GETUJIK2.LIB" : "GETUJIK2.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\GETUJIK2.dll" \
	@"GETUJIK2.003" 


