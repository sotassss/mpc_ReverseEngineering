################################################################################
#                               NetCOBOL V7.2L10
################################################################################

.SUFFIXES:

COBOL_PATH = C:\Program Files\NetCOBOL
PROJECT_PATH = P:\COMSYS\PG
OBJECT_PATH = 
LINK_DEBUG_OPTION = 
LINK_MAP_OPTION = 


ALL : "obj\msg007.dll" "C:\makishisys\hmobj\msg007.dll" "C:\makishisys\yawobj\msg007.dll" 

REBUILD : CLEAN ALL
CLEAN :
	DEL "obj\msg007.dll" 
	DEL "obj\msg007.LIB" 
	DEL "msg007.OBJ" 
	DEL "C:\makishisys\hmobj\msg007.dll" 
	DEL "C:\makishisys\hmobj\msg007.LIB" 
	DEL "C:\makishisys\yawobj\msg007.dll" 
	DEL "C:\makishisys\yawobj\msg007.LIB" 


"msg007.OBJ" : "msg007.cob" "MSG007.CBI"
	COBOL32.EXE -i"MSG007.CBI" -NM "msg007.cob"

"obj\msg007.dll" : "msg007.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\msg007.dll" \
	@"MSG007.001" 

"obj\msg007.LIB" : "msg007.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\msg007.dll" \
	@"MSG007.001" 


"C:\makishisys\hmobj\msg007.dll" : "msg007.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\msg007.dll" \
	@"MSG007.002" 

"C:\makishisys\hmobj\msg007.LIB" : "msg007.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\msg007.dll" \
	@"MSG007.002" 


"C:\makishisys\yawobj\msg007.dll" : "msg007.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\msg007.dll" \
	@"MSG007.003" 

"C:\makishisys\yawobj\msg007.LIB" : "msg007.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\msg007.dll" \
	@"MSG007.003" 


