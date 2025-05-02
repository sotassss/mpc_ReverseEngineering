################################################################################
#                               NetCOBOL V7.2L10
################################################################################

.SUFFIXES:

COBOL_PATH = C:\Program Files\NetCOBOL
PROJECT_PATH = \\HOST\MAKISHI\COMSYS\PG
OBJECT_PATH = 
LINK_DEBUG_OPTION = 
LINK_MAP_OPTION = 


ALL : "obj\GETUJIK.dll" "C:\makishisys\hmobj\GETUJIK.dll" "C:\makishisys\yawobj\GETUJIK.dll" 

REBUILD : CLEAN ALL
CLEAN :
	DEL "obj\GETUJIK.dll" 
	DEL "obj\GETUJIK.LIB" 
	DEL "GETUJIK.OBJ" 
	DEL "C:\makishisys\hmobj\GETUJIK.dll" 
	DEL "C:\makishisys\hmobj\GETUJIK.LIB" 
	DEL "C:\makishisys\yawobj\GETUJIK.dll" 
	DEL "C:\makishisys\yawobj\GETUJIK.LIB" 


"GETUJIK.OBJ" : "GETUJIK.COB" "GETUJIK.CBI"
	COBOL32.EXE -i"GETUJIK.CBI" -NM "GETUJIK.COB"

"obj\GETUJIK.dll" : "GETUJIK.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\GETUJIK.dll" \
	@"GETUJIK.001" 

"obj\GETUJIK.LIB" : "GETUJIK.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\GETUJIK.dll" \
	@"GETUJIK.001" 


"C:\makishisys\hmobj\GETUJIK.dll" : "GETUJIK.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\GETUJIK.dll" \
	@"GETUJIK.002" 

"C:\makishisys\hmobj\GETUJIK.LIB" : "GETUJIK.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\GETUJIK.dll" \
	@"GETUJIK.002" 


"C:\makishisys\yawobj\GETUJIK.dll" : "GETUJIK.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\GETUJIK.dll" \
	@"GETUJIK.003" 

"C:\makishisys\yawobj\GETUJIK.LIB" : "GETUJIK.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\GETUJIK.dll" \
	@"GETUJIK.003" 


