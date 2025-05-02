################################################################################
#                               NetCOBOL V7.2L10
################################################################################

.SUFFIXES:

COBOL_PATH = C:\Program Files\NetCOBOL
PROJECT_PATH = P:\YAWARA\YAW.YCK
OBJECT_PATH = 
LINK_DEBUG_OPTION = 
LINK_MAP_OPTION = 


ALL : "OBJ\YCK6621.DLL" "C:\makishisys\yawobj\YCK6621.DLL" 

REBUILD : CLEAN ALL
CLEAN :
	DEL "OBJ\YCK6621.DLL" 
	DEL "OBJ\YCK6621.LIB" 
	DEL "YCK6621.OBJ" 
	DEL "C:\makishisys\yawobj\YCK6621.DLL" 
	DEL "C:\makishisys\yawobj\YCK6621.LIB" 


"YCK6621.OBJ" : "YCK6621.COB" "YCK6621.CBI"
	COBOL32.EXE -i"YCK6621.CBI" -NM "YCK6621.COB"

"OBJ\YCK6621.DLL" : "YCK6621.OBJ" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"OBJ\YCK6621.DLL" \
	@"YCK6621.001" 

"OBJ\YCK6621.LIB" : "YCK6621.OBJ" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"OBJ\YCK6621.DLL" \
	@"YCK6621.001" 


"C:\makishisys\yawobj\YCK6621.DLL" : "YCK6621.OBJ" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\YCK6621.DLL" \
	@"YCK6621.002" 

"C:\makishisys\yawobj\YCK6621.LIB" : "YCK6621.OBJ" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\YCK6621.DLL" \
	@"YCK6621.002" 


