################################################################################
#                               NetCOBOL V7.2L10
################################################################################

.SUFFIXES:

COBOL_PATH = C:\Program Files\NetCOBOL
PROJECT_PATH = P:\COMSYS\PG
OBJECT_PATH = 
LINK_DEBUG_OPTION = 
LINK_MAP_OPTION = 


ALL : "obj\keisan.dll" "C:\makishisys\hmobj\keisan.dll" "C:\makishisys\yawobj\keisan.dll" 

REBUILD : CLEAN ALL
CLEAN :
	DEL "obj\keisan.dll" 
	DEL "obj\keisan.LIB" 
	DEL "keisan.OBJ" 
	DEL "C:\makishisys\hmobj\keisan.dll" 
	DEL "C:\makishisys\hmobj\keisan.LIB" 
	DEL "C:\makishisys\yawobj\keisan.dll" 
	DEL "C:\makishisys\yawobj\keisan.LIB" 


"keisan.OBJ" : "keisan.cob" "KEISAN.CBI"
	COBOL32.EXE -i"KEISAN.CBI" -NM "keisan.cob"

"obj\keisan.dll" : "keisan.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\keisan.dll" \
	@"KEISAN.001" 

"obj\keisan.LIB" : "keisan.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\keisan.dll" \
	@"KEISAN.001" 


"C:\makishisys\hmobj\keisan.dll" : "keisan.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\keisan.dll" \
	@"KEISAN.002" 

"C:\makishisys\hmobj\keisan.LIB" : "keisan.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\keisan.dll" \
	@"KEISAN.002" 


"C:\makishisys\yawobj\keisan.dll" : "keisan.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\keisan.dll" \
	@"KEISAN.003" 

"C:\makishisys\yawobj\keisan.LIB" : "keisan.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\keisan.dll" \
	@"KEISAN.003" 


