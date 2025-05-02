################################################################################
#                               NetCOBOL V10.0.0
################################################################################

.SUFFIXES:

COBOL_PATH = C:\Program Files\NetCOBOL
PROJECT_PATH = \\HOST\MAKISHI\COMSYS\PG
OBJECT_PATH = 
LINK_DEBUG_OPTION = 
LINK_MAP_OPTION = 


ALL : "obj\JIB9709R.dll" "C:\makishisys\hmobj\JIB9709R.dll" "C:\makishisys\yawobj\JIB9709R.dll" 

REBUILD : CLEAN ALL
CLEAN :
	DEL "obj\JIB9709R.dll" 
	DEL "obj\JIB9709R.LIB" 
	DEL "JIB9709R.OBJ" 
	DEL "C:\makishisys\hmobj\JIB9709R.dll" 
	DEL "C:\makishisys\hmobj\JIB9709R.LIB" 
	DEL "C:\makishisys\yawobj\JIB9709R.dll" 
	DEL "C:\makishisys\yawobj\JIB9709R.LIB" 


"JIB9709R.OBJ" : "JIB9709R.cob" "JIB9709R.CBI"
	COBOL32.EXE -i"JIB9709R.CBI" -NM "JIB9709R.cob"

"obj\JIB9709R.dll" : "JIB9709R.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\JIB9709R.dll" \
	@"JIB9709R.001" 

"obj\JIB9709R.LIB" : "JIB9709R.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\JIB9709R.dll" \
	@"JIB9709R.001" 


"C:\makishisys\hmobj\JIB9709R.dll" : "JIB9709R.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\JIB9709R.dll" \
	@"JIB9709R.002" 

"C:\makishisys\hmobj\JIB9709R.LIB" : "JIB9709R.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\JIB9709R.dll" \
	@"JIB9709R.002" 


"C:\makishisys\yawobj\JIB9709R.dll" : "JIB9709R.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\JIB9709R.dll" \
	@"JIB9709R.003" 

"C:\makishisys\yawobj\JIB9709R.LIB" : "JIB9709R.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\JIB9709R.dll" \
	@"JIB9709R.003" 


