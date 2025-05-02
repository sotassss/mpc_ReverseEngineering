################################################################################
#                               NetCOBOL V7.2L10
################################################################################

.SUFFIXES:

COBOL_PATH = C:\Program Files\NetCOBOL
PROJECT_PATH = P:\YAWARA\YAW.YJB
OBJECT_PATH = 
LINK_DEBUG_OPTION = 
LINK_MAP_OPTION = 


ALL : "OBJ\YJB662.DLL" "C:\makishisys\yawobj\YJB662.DLL" 

REBUILD : CLEAN ALL
CLEAN :
	DEL "OBJ\YJB662.DLL" 
	DEL "OBJ\YJB662.LIB" 
	DEL "YJB662.OBJ" 
	DEL "C:\makishisys\yawobj\YJB662.DLL" 
	DEL "C:\makishisys\yawobj\YJB662.LIB" 


"YJB662.OBJ" : "YJB662.COB" "YJB662.CBI"
	COBOL32.EXE -i"YJB662.CBI" -NM "YJB662.COB"

"OBJ\YJB662.DLL" : "YJB662.OBJ" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"OBJ\YJB662.DLL" \
	@"YJB662.001" 

"OBJ\YJB662.LIB" : "YJB662.OBJ" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"OBJ\YJB662.DLL" \
	@"YJB662.001" 


"C:\makishisys\yawobj\YJB662.DLL" : "YJB662.OBJ" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\YJB662.DLL" \
	@"YJB662.002" 

"C:\makishisys\yawobj\YJB662.LIB" : "YJB662.OBJ" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\YJB662.DLL" \
	@"YJB662.002" 


