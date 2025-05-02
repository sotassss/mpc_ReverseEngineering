################################################################################
#                               NetCOBOL V10.0.0
################################################################################

.SUFFIXES:

COBOL_PATH = C:\Program Files\NetCOBOL
PROJECT_PATH = \\HOST\MAKISHI\YAWARA\YAW.YHP
OBJECT_PATH = 
LINK_DEBUG_OPTION = 
LINK_MAP_OPTION = 


ALL : "OBJ\YHP6126.DLL" "C:\makishisys\yawobj\YHP6126.DLL" 

REBUILD : CLEAN ALL
CLEAN :
	DEL "OBJ\YHP6126.DLL" 
	DEL "OBJ\YHP6126.LIB" 
	DEL "YHP6126.OBJ" 
	DEL "C:\makishisys\yawobj\YHP6126.DLL" 
	DEL "C:\makishisys\yawobj\YHP6126.LIB" 


"YHP6126.OBJ" : "YHP6126.COB" "YHP6126.CBI"
	COBOL32.EXE -i"YHP6126.CBI" -NM "YHP6126.COB"

"OBJ\YHP6126.DLL" : "YHP6126.OBJ" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"OBJ\YHP6126.DLL" \
	@"YHP6126.001" 

"OBJ\YHP6126.LIB" : "YHP6126.OBJ" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"OBJ\YHP6126.DLL" \
	@"YHP6126.001" 


"C:\makishisys\yawobj\YHP6126.DLL" : "YHP6126.OBJ" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\YHP6126.DLL" \
	@"YHP6126.002" 

"C:\makishisys\yawobj\YHP6126.LIB" : "YHP6126.OBJ" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\YHP6126.DLL" \
	@"YHP6126.002" 


