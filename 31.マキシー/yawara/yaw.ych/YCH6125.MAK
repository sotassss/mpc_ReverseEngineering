################################################################################
#                               NetCOBOL V10.0.0
################################################################################

.SUFFIXES:

COBOL_PATH = C:\Program Files\NetCOBOL
PROJECT_PATH = \\HOST\MAKISHI\YAWARA\YAW.YCH
OBJECT_PATH = 
LINK_DEBUG_OPTION = 
LINK_MAP_OPTION = 


ALL : "OBJ\Ych6125.DLL" "C:\makishisys\yawobj\Ych6125.DLL" 

REBUILD : CLEAN ALL
CLEAN :
	DEL "OBJ\Ych6125.DLL" 
	DEL "OBJ\Ych6125.LIB" 
	DEL "Ych6125.OBJ" 
	DEL "C:\makishisys\yawobj\Ych6125.DLL" 
	DEL "C:\makishisys\yawobj\Ych6125.LIB" 


"Ych6125.OBJ" : "Ych6125.COB" "YCH6125.CBI"
	COBOL32.EXE -i"YCH6125.CBI" -NM "Ych6125.COB"

"OBJ\Ych6125.DLL" : "Ych6125.OBJ" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"OBJ\Ych6125.DLL" \
	@"YCH6125.001" 

"OBJ\Ych6125.LIB" : "Ych6125.OBJ" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"OBJ\Ych6125.DLL" \
	@"YCH6125.001" 


"C:\makishisys\yawobj\Ych6125.DLL" : "Ych6125.OBJ" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\Ych6125.DLL" \
	@"YCH6125.002" 

"C:\makishisys\yawobj\Ych6125.LIB" : "Ych6125.OBJ" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\Ych6125.DLL" \
	@"YCH6125.002" 


