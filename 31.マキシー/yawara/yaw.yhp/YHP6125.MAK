################################################################################
#                               NetCOBOL V10.0.0
################################################################################

.SUFFIXES:

COBOL_PATH = C:\Program Files\NetCOBOL
PROJECT_PATH = \\HOST\MAKISHI\YAWARA\YAW.YHP
OBJECT_PATH = 
LINK_DEBUG_OPTION = 
LINK_MAP_OPTION = 


ALL : "OBJ\YHP6125.DLL" "C:\makishisys\yawobj\YHP6125.DLL" 

REBUILD : CLEAN ALL
CLEAN :
	DEL "OBJ\YHP6125.DLL" 
	DEL "OBJ\YHP6125.LIB" 
	DEL "YHP6125.OBJ" 
	DEL "C:\makishisys\yawobj\YHP6125.DLL" 
	DEL "C:\makishisys\yawobj\YHP6125.LIB" 


"YHP6125.OBJ" : "YHP6125.COB" "YHP6125.CBI"
	COBOL32.EXE -i"YHP6125.CBI" -NM "YHP6125.COB"

"OBJ\YHP6125.DLL" : "YHP6125.OBJ" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"OBJ\YHP6125.DLL" \
	@"YHP6125.001" 

"OBJ\YHP6125.LIB" : "YHP6125.OBJ" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"OBJ\YHP6125.DLL" \
	@"YHP6125.001" 


"C:\makishisys\yawobj\YHP6125.DLL" : "YHP6125.OBJ" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\YHP6125.DLL" \
	@"YHP6125.002" 

"C:\makishisys\yawobj\YHP6125.LIB" : "YHP6125.OBJ" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\YHP6125.DLL" \
	@"YHP6125.002" 


