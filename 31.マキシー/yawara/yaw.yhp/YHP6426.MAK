################################################################################
#                               NetCOBOL V10.0.0
################################################################################

.SUFFIXES:

COBOL_PATH = C:\Program Files\NetCOBOL
PROJECT_PATH = \\HOST\MAKISHI\YAWARA\YAW.YHP
OBJECT_PATH = 
LINK_DEBUG_OPTION = 
LINK_MAP_OPTION = 


ALL : "OBJ\YHP6426.DLL" "C:\makishisys\yawobj\YHP6426.DLL" 

REBUILD : CLEAN ALL
CLEAN :
	DEL "OBJ\YHP6426.DLL" 
	DEL "OBJ\YHP6426.LIB" 
	DEL "YHP6426.OBJ" 
	DEL "C:\makishisys\yawobj\YHP6426.DLL" 
	DEL "C:\makishisys\yawobj\YHP6426.LIB" 


"YHP6426.OBJ" : "YHP6426.COB" "YHP6426.CBI"
	COBOL32.EXE -i"YHP6426.CBI" -NM "YHP6426.COB"

"OBJ\YHP6426.DLL" : "YHP6426.OBJ" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"OBJ\YHP6426.DLL" \
	@"YHP6426.001" 

"OBJ\YHP6426.LIB" : "YHP6426.OBJ" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"OBJ\YHP6426.DLL" \
	@"YHP6426.001" 


"C:\makishisys\yawobj\YHP6426.DLL" : "YHP6426.OBJ" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\YHP6426.DLL" \
	@"YHP6426.002" 

"C:\makishisys\yawobj\YHP6426.LIB" : "YHP6426.OBJ" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\YHP6426.DLL" \
	@"YHP6426.002" 


