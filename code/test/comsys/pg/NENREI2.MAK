################################################################################
#                               NetCOBOL V7.2L10
################################################################################

.SUFFIXES:

COBOL_PATH = C:\Program Files\NetCOBOL
PROJECT_PATH = P:\COMSYS\PG
OBJECT_PATH = 
LINK_DEBUG_OPTION = 
LINK_MAP_OPTION = 


ALL : "obj\nenrei2.dll" "C:\makishisys\hmobj\nenrei2.dll" "C:\makishisys\yawobj\nenrei2.dll" 

REBUILD : CLEAN ALL
CLEAN :
	DEL "obj\nenrei2.dll" 
	DEL "obj\nenrei2.LIB" 
	DEL "nenrei2.OBJ" 
	DEL "C:\makishisys\hmobj\nenrei2.dll" 
	DEL "C:\makishisys\hmobj\nenrei2.LIB" 
	DEL "C:\makishisys\yawobj\nenrei2.dll" 
	DEL "C:\makishisys\yawobj\nenrei2.LIB" 


"nenrei2.OBJ" : "nenrei2.cob" "NENREI2.CBI"
	COBOL32.EXE -i"NENREI2.CBI" -NM "nenrei2.cob"

"obj\nenrei2.dll" : "nenrei2.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\nenrei2.dll" \
	@"NENREI2.001" 

"obj\nenrei2.LIB" : "nenrei2.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\nenrei2.dll" \
	@"NENREI2.001" 


"C:\makishisys\hmobj\nenrei2.dll" : "nenrei2.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\nenrei2.dll" \
	@"NENREI2.002" 

"C:\makishisys\hmobj\nenrei2.LIB" : "nenrei2.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\nenrei2.dll" \
	@"NENREI2.002" 


"C:\makishisys\yawobj\nenrei2.dll" : "nenrei2.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\nenrei2.dll" \
	@"NENREI2.003" 

"C:\makishisys\yawobj\nenrei2.LIB" : "nenrei2.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\nenrei2.dll" \
	@"NENREI2.003" 


