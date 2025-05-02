################################################################################
#                               NetCOBOL V10.0.0
################################################################################

.SUFFIXES:

COBOL_PATH = C:\Program Files\NetCOBOL
PROJECT_PATH = \\HOST\MAKISHI\YAWARA\YAW.YHP
OBJECT_PATH = 
LINK_DEBUG_OPTION = 
LINK_MAP_OPTION = 


ALL : "OBJ\YHP6425.DLL" "C:\makishisys\yawobj\YHP6425.DLL" 

REBUILD : CLEAN ALL
CLEAN :
	DEL "OBJ\YHP6425.DLL" 
	DEL "OBJ\YHP6425.LIB" 
	DEL "YHP6425.OBJ" 
	DEL "C:\makishisys\yawobj\YHP6425.DLL" 
	DEL "C:\makishisys\yawobj\YHP6425.LIB" 


"YHP6425.OBJ" : "YHP6425.COB" "YHP6425.CBI"
	COBOL32.EXE -i"YHP6425.CBI" -NM "YHP6425.COB"

"OBJ\YHP6425.DLL" : "YHP6425.OBJ" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"OBJ\YHP6425.DLL" \
	@"YHP6425.001" 

"OBJ\YHP6425.LIB" : "YHP6425.OBJ" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"OBJ\YHP6425.DLL" \
	@"YHP6425.001" 


"C:\makishisys\yawobj\YHP6425.DLL" : "YHP6425.OBJ" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\YHP6425.DLL" \
	@"YHP6425.002" 

"C:\makishisys\yawobj\YHP6425.LIB" : "YHP6425.OBJ" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\YHP6425.DLL" \
	@"YHP6425.002" 


