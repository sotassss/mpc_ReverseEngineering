################################################################################
#                               NetCOBOL V7.2L10
################################################################################

.SUFFIXES:

COBOL_PATH = C:\Program Files\NetCOBOL
PROJECT_PATH = P:\COMSYS\PG
OBJECT_PATH = 
LINK_DEBUG_OPTION = 
LINK_MAP_OPTION = 


ALL : "obj\HUR0210.dll" "C:\makishisys\hmobj\HUR0210.dll" "C:\makishisys\yawobj\HUR0210.dll" 

REBUILD : CLEAN ALL
CLEAN :
	DEL "obj\HUR0210.dll" 
	DEL "obj\HUR0210.LIB" 
	DEL "HUR0210.OBJ" 
	DEL "C:\makishisys\hmobj\HUR0210.dll" 
	DEL "C:\makishisys\hmobj\HUR0210.LIB" 
	DEL "C:\makishisys\yawobj\HUR0210.dll" 
	DEL "C:\makishisys\yawobj\HUR0210.LIB" 


"HUR0210.OBJ" : "HUR0210.cob" "HUR0210.CBI"
	COBOL32.EXE -i"HUR0210.CBI" -NM "HUR0210.cob"

"obj\HUR0210.dll" : "HUR0210.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\HUR0210.dll" \
	@"HUR0210.001" 

"obj\HUR0210.LIB" : "HUR0210.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\HUR0210.dll" \
	@"HUR0210.001" 


"C:\makishisys\hmobj\HUR0210.dll" : "HUR0210.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\HUR0210.dll" \
	@"HUR0210.002" 

"C:\makishisys\hmobj\HUR0210.LIB" : "HUR0210.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\HUR0210.dll" \
	@"HUR0210.002" 


"C:\makishisys\yawobj\HUR0210.dll" : "HUR0210.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\HUR0210.dll" \
	@"HUR0210.003" 

"C:\makishisys\yawobj\HUR0210.LIB" : "HUR0210.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\HUR0210.dll" \
	@"HUR0210.003" 


