################################################################################
#                               NetCOBOL V7.2L10
################################################################################

.SUFFIXES:

COBOL_PATH = C:\Program Files\NetCOBOL
PROJECT_PATH = P:\COMSYS\PG
OBJECT_PATH = 
LINK_DEBUG_OPTION = 
LINK_MAP_OPTION = 


ALL : "obj\msg003.dll" "C:\makishisys\hmobj\msg003.dll" "C:\makishisys\yawobj\msg003.dll" 

REBUILD : CLEAN ALL
CLEAN :
	DEL "obj\msg003.dll" 
	DEL "obj\msg003.LIB" 
	DEL "msg003.OBJ" 
	DEL "C:\makishisys\hmobj\msg003.dll" 
	DEL "C:\makishisys\hmobj\msg003.LIB" 
	DEL "C:\makishisys\yawobj\msg003.dll" 
	DEL "C:\makishisys\yawobj\msg003.LIB" 


"msg003.OBJ" : "msg003.cob" "MSG003.CBI"
	COBOL32.EXE -i"MSG003.CBI" -NM "msg003.cob"

"obj\msg003.dll" : "msg003.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\msg003.dll" \
	@"MSG003.001" 

"obj\msg003.LIB" : "msg003.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\msg003.dll" \
	@"MSG003.001" 


"C:\makishisys\hmobj\msg003.dll" : "msg003.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\msg003.dll" \
	@"MSG003.002" 

"C:\makishisys\hmobj\msg003.LIB" : "msg003.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\msg003.dll" \
	@"MSG003.002" 


"C:\makishisys\yawobj\msg003.dll" : "msg003.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\msg003.dll" \
	@"MSG003.003" 

"C:\makishisys\yawobj\msg003.LIB" : "msg003.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\msg003.dll" \
	@"MSG003.003" 


