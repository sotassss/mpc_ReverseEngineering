################################################################################
#                               NetCOBOL V7.2L10
################################################################################

.SUFFIXES:

COBOL_PATH = C:\Program Files\NetCOBOL
PROJECT_PATH = P:\COMSYS\PG
OBJECT_PATH = 
LINK_DEBUG_OPTION = 
LINK_MAP_OPTION = 


ALL : "obj\HUJ9709.dll" "C:\makishisys\hmobj\HUJ9709.dll" "C:\makishisys\yawobj\HUJ9709.dll" 

REBUILD : CLEAN ALL
CLEAN :
	DEL "obj\HUJ9709.dll" 
	DEL "obj\HUJ9709.LIB" 
	DEL "HUJ9709.OBJ" 
	DEL "C:\makishisys\hmobj\HUJ9709.dll" 
	DEL "C:\makishisys\hmobj\HUJ9709.LIB" 
	DEL "C:\makishisys\yawobj\HUJ9709.dll" 
	DEL "C:\makishisys\yawobj\HUJ9709.LIB" 


"HUJ9709.OBJ" : "HUJ9709.cob" "HUJ9709.CBI"
	COBOL32.EXE -i"HUJ9709.CBI" -NM "HUJ9709.cob"

"obj\HUJ9709.dll" : "HUJ9709.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\HUJ9709.dll" \
	@"HUJ9709.001" 

"obj\HUJ9709.LIB" : "HUJ9709.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\HUJ9709.dll" \
	@"HUJ9709.001" 


"C:\makishisys\hmobj\HUJ9709.dll" : "HUJ9709.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\HUJ9709.dll" \
	@"HUJ9709.002" 

"C:\makishisys\hmobj\HUJ9709.LIB" : "HUJ9709.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\HUJ9709.dll" \
	@"HUJ9709.002" 


"C:\makishisys\yawobj\HUJ9709.dll" : "HUJ9709.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\HUJ9709.dll" \
	@"HUJ9709.003" 

"C:\makishisys\yawobj\HUJ9709.LIB" : "HUJ9709.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\HUJ9709.dll" \
	@"HUJ9709.003" 


