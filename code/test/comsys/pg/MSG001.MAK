################################################################################
#                               NetCOBOL V7.2L10
################################################################################

.SUFFIXES:

COBOL_PATH = C:\Program Files\NetCOBOL
PROJECT_PATH = P:\COMSYS\PG
OBJECT_PATH = 
LINK_DEBUG_OPTION = 
LINK_MAP_OPTION = 


ALL : "obj\msg001.dll" "C:\makishisys\hmobj\msg001.dll" "C:\makishisys\yawobj\msg001.dll" 

REBUILD : CLEAN ALL
CLEAN :
	DEL "obj\msg001.dll" 
	DEL "obj\msg001.LIB" 
	DEL "msg001.OBJ" 
	DEL "C:\makishisys\hmobj\msg001.dll" 
	DEL "C:\makishisys\hmobj\msg001.LIB" 
	DEL "C:\makishisys\yawobj\msg001.dll" 
	DEL "C:\makishisys\yawobj\msg001.LIB" 


"msg001.OBJ" : "msg001.cob" "MSG001.CBI"
	COBOL32.EXE -i"MSG001.CBI" -NM "msg001.cob"

"obj\msg001.dll" : "msg001.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\msg001.dll" \
	@"MSG001.001" 

"obj\msg001.LIB" : "msg001.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\msg001.dll" \
	@"MSG001.001" 


"C:\makishisys\hmobj\msg001.dll" : "msg001.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\msg001.dll" \
	@"MSG001.002" 

"C:\makishisys\hmobj\msg001.LIB" : "msg001.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\msg001.dll" \
	@"MSG001.002" 


"C:\makishisys\yawobj\msg001.dll" : "msg001.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\msg001.dll" \
	@"MSG001.003" 

"C:\makishisys\yawobj\msg001.LIB" : "msg001.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\msg001.dll" \
	@"MSG001.003" 


