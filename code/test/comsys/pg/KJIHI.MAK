################################################################################
#                               NetCOBOL V7.2L10
################################################################################

.SUFFIXES:

COBOL_PATH = C:\Program Files\NetCOBOL
PROJECT_PATH = P:\COMSYS\PG
OBJECT_PATH = 
LINK_DEBUG_OPTION = 
LINK_MAP_OPTION = 


ALL : "obj\kjihi.dll" "C:\makishisys\hmobj\kjihi.dll" "C:\makishisys\yawobj\kjihi.dll" 

REBUILD : CLEAN ALL
CLEAN :
	DEL "obj\kjihi.dll" 
	DEL "obj\kjihi.LIB" 
	DEL "kjihi.OBJ" 
	DEL "C:\makishisys\hmobj\kjihi.dll" 
	DEL "C:\makishisys\hmobj\kjihi.LIB" 
	DEL "C:\makishisys\yawobj\kjihi.dll" 
	DEL "C:\makishisys\yawobj\kjihi.LIB" 


"kjihi.OBJ" : "kjihi.cob" "KJIHI.CBI"
	COBOL32.EXE -i"KJIHI.CBI" -NM "kjihi.cob"

"obj\kjihi.dll" : "kjihi.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\kjihi.dll" \
	@"KJIHI.001" 

"obj\kjihi.LIB" : "kjihi.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\kjihi.dll" \
	@"KJIHI.001" 


"C:\makishisys\hmobj\kjihi.dll" : "kjihi.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\kjihi.dll" \
	@"KJIHI.002" 

"C:\makishisys\hmobj\kjihi.LIB" : "kjihi.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\kjihi.dll" \
	@"KJIHI.002" 


"C:\makishisys\yawobj\kjihi.dll" : "kjihi.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\kjihi.dll" \
	@"KJIHI.003" 

"C:\makishisys\yawobj\kjihi.LIB" : "kjihi.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\kjihi.dll" \
	@"KJIHI.003" 


