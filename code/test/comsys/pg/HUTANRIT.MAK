################################################################################
#                               NetCOBOL V7.2L10
################################################################################

.SUFFIXES:

COBOL_PATH = C:\Program Files\NetCOBOL
PROJECT_PATH = P:\COMSYS\PG
OBJECT_PATH = 
LINK_DEBUG_OPTION = 
LINK_MAP_OPTION = 


ALL : "obj\hutanrit.dll" "C:\makishisys\hmobj\hutanrit.dll" "C:\makishisys\yawobj\hutanrit.dll" 

REBUILD : CLEAN ALL
CLEAN :
	DEL "obj\hutanrit.dll" 
	DEL "obj\hutanrit.LIB" 
	DEL "hutanrit.OBJ" 
	DEL "C:\makishisys\hmobj\hutanrit.dll" 
	DEL "C:\makishisys\hmobj\hutanrit.LIB" 
	DEL "C:\makishisys\yawobj\hutanrit.dll" 
	DEL "C:\makishisys\yawobj\hutanrit.LIB" 


"hutanrit.OBJ" : "hutanrit.cob" "HUTANRIT.CBI"
	COBOL32.EXE -i"HUTANRIT.CBI" -NM "hutanrit.cob"

"obj\hutanrit.dll" : "hutanrit.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\hutanrit.dll" \
	@"HUTANRIT.001" 

"obj\hutanrit.LIB" : "hutanrit.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\hutanrit.dll" \
	@"HUTANRIT.001" 


"C:\makishisys\hmobj\hutanrit.dll" : "hutanrit.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\hutanrit.dll" \
	@"HUTANRIT.002" 

"C:\makishisys\hmobj\hutanrit.LIB" : "hutanrit.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\hutanrit.dll" \
	@"HUTANRIT.002" 


"C:\makishisys\yawobj\hutanrit.dll" : "hutanrit.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\hutanrit.dll" \
	@"HUTANRIT.003" 

"C:\makishisys\yawobj\hutanrit.LIB" : "hutanrit.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\hutanrit.dll" \
	@"HUTANRIT.003" 


