################################################################################
#                               NetCOBOL V7.2L10
################################################################################

.SUFFIXES:

COBOL_PATH = C:\Program Files\NetCOBOL
PROJECT_PATH = P:\COMSYS\PG
OBJECT_PATH = 
LINK_DEBUG_OPTION = 
LINK_MAP_OPTION = 


ALL : "obj\msg004.dll" "C:\makishisys\hmobj\msg004.dll" "C:\makishisys\yawobj\msg004.dll" 

REBUILD : CLEAN ALL
CLEAN :
	DEL "obj\msg004.dll" 
	DEL "obj\msg004.LIB" 
	DEL "msg004.OBJ" 
	DEL "C:\makishisys\hmobj\msg004.dll" 
	DEL "C:\makishisys\hmobj\msg004.LIB" 
	DEL "C:\makishisys\yawobj\msg004.dll" 
	DEL "C:\makishisys\yawobj\msg004.LIB" 


"msg004.OBJ" : "msg004.cob" "MSG004.CBI"
	COBOL32.EXE -i"MSG004.CBI" -NM "msg004.cob"

"obj\msg004.dll" : "msg004.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\msg004.dll" \
	@"MSG004.001" 

"obj\msg004.LIB" : "msg004.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\msg004.dll" \
	@"MSG004.001" 


"C:\makishisys\hmobj\msg004.dll" : "msg004.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\msg004.dll" \
	@"MSG004.002" 

"C:\makishisys\hmobj\msg004.LIB" : "msg004.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\msg004.dll" \
	@"MSG004.002" 


"C:\makishisys\yawobj\msg004.dll" : "msg004.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\msg004.dll" \
	@"MSG004.003" 

"C:\makishisys\yawobj\msg004.LIB" : "msg004.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\msg004.dll" \
	@"MSG004.003" 


