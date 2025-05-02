################################################################################
#                               NetCOBOL V7.2L10
################################################################################

.SUFFIXES:

COBOL_PATH = C:\Program Files\NetCOBOL
PROJECT_PATH = \\HOST2\MAKISHI\COMSYS\PG
OBJECT_PATH = 
LINK_DEBUG_OPTION = 
LINK_MAP_OPTION = 


ALL : "obj\msg010.dll" "C:\makishisys\hmobj\msg010.dll" "C:\makishisys\yawobj\msg010.dll" 

REBUILD : CLEAN ALL
CLEAN :
	DEL "obj\msg010.dll" 
	DEL "obj\msg010.LIB" 
	DEL "msg010.OBJ" 
	DEL "C:\makishisys\hmobj\msg010.dll" 
	DEL "C:\makishisys\hmobj\msg010.LIB" 
	DEL "C:\makishisys\yawobj\msg010.dll" 
	DEL "C:\makishisys\yawobj\msg010.LIB" 


"msg010.OBJ" : "msg010.cob" "MSG010.CBI"
	COBOL32.EXE -i"MSG010.CBI" -NM "msg010.cob"

"obj\msg010.dll" : "msg010.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\msg010.dll" \
	@"MSG010.001" 

"obj\msg010.LIB" : "msg010.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\msg010.dll" \
	@"MSG010.001" 


"C:\makishisys\hmobj\msg010.dll" : "msg010.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\msg010.dll" \
	@"MSG010.002" 

"C:\makishisys\hmobj\msg010.LIB" : "msg010.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\msg010.dll" \
	@"MSG010.002" 


"C:\makishisys\yawobj\msg010.dll" : "msg010.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\msg010.dll" \
	@"MSG010.003" 

"C:\makishisys\yawobj\msg010.LIB" : "msg010.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\msg010.dll" \
	@"MSG010.003" 


