################################################################################
#                               NetCOBOL V7.2L10
################################################################################

.SUFFIXES:

COBOL_PATH = C:\Program Files\NetCOBOL
PROJECT_PATH = P:\YAWARA\YAW.YJB
OBJECT_PATH = 
LINK_DEBUG_OPTION = 
LINK_MAP_OPTION = 


ALL : "OBJ\YJB6621.DLL" "C:\makishisys\yawobj\YJB6621.DLL" 

REBUILD : CLEAN ALL
CLEAN :
	DEL "OBJ\YJB6621.DLL" 
	DEL "OBJ\YJB6621.LIB" 
	DEL "YJB6621.OBJ" 
	DEL "C:\makishisys\yawobj\YJB6621.DLL" 
	DEL "C:\makishisys\yawobj\YJB6621.LIB" 


"YJB6621.OBJ" : "YJB6621.COB" "YJB6621.CBI"
	COBOL32.EXE -i"YJB6621.CBI" -NM "YJB6621.COB"

"OBJ\YJB6621.DLL" : "YJB6621.OBJ" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"OBJ\YJB6621.DLL" \
	@"YJB6621.001" 

"OBJ\YJB6621.LIB" : "YJB6621.OBJ" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"OBJ\YJB6621.DLL" \
	@"YJB6621.001" 


"C:\makishisys\yawobj\YJB6621.DLL" : "YJB6621.OBJ" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\YJB6621.DLL" \
	@"YJB6621.002" 

"C:\makishisys\yawobj\YJB6621.LIB" : "YJB6621.OBJ" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\YJB6621.DLL" \
	@"YJB6621.002" 


