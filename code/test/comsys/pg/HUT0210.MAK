################################################################################
#                               NetCOBOL V7.2L10
################################################################################

.SUFFIXES:

COBOL_PATH = C:\Program Files\NetCOBOL
PROJECT_PATH = P:\COMSYS\PG
OBJECT_PATH = 
LINK_DEBUG_OPTION = 
LINK_MAP_OPTION = 


ALL : "obj\HUT0210.dll" "C:\makishisys\hmobj\HUT0210.dll" "C:\makishisys\yawobj\HUT0210.dll" 

REBUILD : CLEAN ALL
CLEAN :
	DEL "obj\HUT0210.dll" 
	DEL "obj\HUT0210.LIB" 
	DEL "HUT0210.OBJ" 
	DEL "C:\makishisys\hmobj\HUT0210.dll" 
	DEL "C:\makishisys\hmobj\HUT0210.LIB" 
	DEL "C:\makishisys\yawobj\HUT0210.dll" 
	DEL "C:\makishisys\yawobj\HUT0210.LIB" 


"HUT0210.OBJ" : "HUT0210.cob" "HUT0210.CBI"
	COBOL32.EXE -i"HUT0210.CBI" -NM "HUT0210.cob"

"obj\HUT0210.dll" : "HUT0210.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\HUT0210.dll" \
	@"HUT0210.001" 

"obj\HUT0210.LIB" : "HUT0210.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\HUT0210.dll" \
	@"HUT0210.001" 


"C:\makishisys\hmobj\HUT0210.dll" : "HUT0210.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\HUT0210.dll" \
	@"HUT0210.002" 

"C:\makishisys\hmobj\HUT0210.LIB" : "HUT0210.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\HUT0210.dll" \
	@"HUT0210.002" 


"C:\makishisys\yawobj\HUT0210.dll" : "HUT0210.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\HUT0210.dll" \
	@"HUT0210.003" 

"C:\makishisys\yawobj\HUT0210.LIB" : "HUT0210.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\HUT0210.dll" \
	@"HUT0210.003" 


