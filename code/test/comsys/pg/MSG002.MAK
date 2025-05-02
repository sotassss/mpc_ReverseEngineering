################################################################################
#                               NetCOBOL V7.2L10
################################################################################

.SUFFIXES:

COBOL_PATH = C:\Program Files\NetCOBOL
PROJECT_PATH = P:\COMSYS\PG
OBJECT_PATH = 
LINK_DEBUG_OPTION = 
LINK_MAP_OPTION = 


ALL : "obj\msg002.dll" "C:\makishisys\hmobj\msg002.dll" "C:\makishisys\yawobj\msg002.dll" 

REBUILD : CLEAN ALL
CLEAN :
	DEL "obj\msg002.dll" 
	DEL "obj\msg002.LIB" 
	DEL "msg002.OBJ" 
	DEL "C:\makishisys\hmobj\msg002.dll" 
	DEL "C:\makishisys\hmobj\msg002.LIB" 
	DEL "C:\makishisys\yawobj\msg002.dll" 
	DEL "C:\makishisys\yawobj\msg002.LIB" 


"msg002.OBJ" : "msg002.cob" "MSG002.CBI"
	COBOL32.EXE -i"MSG002.CBI" -NM "msg002.cob"

"obj\msg002.dll" : "msg002.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\msg002.dll" \
	@"MSG002.001" 

"obj\msg002.LIB" : "msg002.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\msg002.dll" \
	@"MSG002.001" 


"C:\makishisys\hmobj\msg002.dll" : "msg002.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\msg002.dll" \
	@"MSG002.002" 

"C:\makishisys\hmobj\msg002.LIB" : "msg002.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\msg002.dll" \
	@"MSG002.002" 


"C:\makishisys\yawobj\msg002.dll" : "msg002.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\msg002.dll" \
	@"MSG002.003" 

"C:\makishisys\yawobj\msg002.LIB" : "msg002.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\msg002.dll" \
	@"MSG002.003" 


