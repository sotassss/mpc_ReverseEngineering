################################################################################
#                               NetCOBOL V7.2L10
################################################################################

.SUFFIXES:

COBOL_PATH = C:\Program Files\NetCOBOL
PROJECT_PATH = \\HOST\MAKISHI\COMSYS\PG
OBJECT_PATH = 
LINK_DEBUG_OPTION = 
LINK_MAP_OPTION = 


ALL : "obj\ROU9709R.dll" "C:\makishisys\hmobj\ROU9709R.dll" "C:\makishisys\yawobj\ROU9709R.dll" 

REBUILD : CLEAN ALL
CLEAN :
	DEL "obj\ROU9709R.dll" 
	DEL "obj\ROU9709R.LIB" 
	DEL "ROU9709R.OBJ" 
	DEL "C:\makishisys\hmobj\ROU9709R.dll" 
	DEL "C:\makishisys\hmobj\ROU9709R.LIB" 
	DEL "C:\makishisys\yawobj\ROU9709R.dll" 
	DEL "C:\makishisys\yawobj\ROU9709R.LIB" 


"ROU9709R.OBJ" : "ROU9709R.cob" "ROU9709R.CBI"
	COBOL32.EXE -i"ROU9709R.CBI" -NM "ROU9709R.cob"

"obj\ROU9709R.dll" : "ROU9709R.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\ROU9709R.dll" \
	@"ROU9709R.001" 

"obj\ROU9709R.LIB" : "ROU9709R.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\ROU9709R.dll" \
	@"ROU9709R.001" 


"C:\makishisys\hmobj\ROU9709R.dll" : "ROU9709R.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\ROU9709R.dll" \
	@"ROU9709R.002" 

"C:\makishisys\hmobj\ROU9709R.LIB" : "ROU9709R.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\ROU9709R.dll" \
	@"ROU9709R.002" 


"C:\makishisys\yawobj\ROU9709R.dll" : "ROU9709R.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\ROU9709R.dll" \
	@"ROU9709R.003" 

"C:\makishisys\yawobj\ROU9709R.LIB" : "ROU9709R.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\ROU9709R.dll" \
	@"ROU9709R.003" 


