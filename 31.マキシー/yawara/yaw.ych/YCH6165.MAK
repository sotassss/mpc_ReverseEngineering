################################################################################
#                               NetCOBOL V10.0.0
################################################################################

.SUFFIXES:

COBOL_PATH = C:\Program Files\NetCOBOL
PROJECT_PATH = \\HOST\MAKISHI\YAWARA\YAW.YCH
OBJECT_PATH = 
LINK_DEBUG_OPTION = 
LINK_MAP_OPTION = 


ALL : "OBJ\Ych6165.DLL" "C:\makishisys\yawobj\Ych6165.DLL" 

REBUILD : CLEAN ALL
CLEAN :
	DEL "OBJ\Ych6165.DLL" 
	DEL "OBJ\Ych6165.LIB" 
	DEL "Ych6165.OBJ" 
	DEL "C:\makishisys\yawobj\Ych6165.DLL" 
	DEL "C:\makishisys\yawobj\Ych6165.LIB" 


"Ych6165.OBJ" : "Ych6165.COB" "YCH6165.CBI"
	COBOL32.EXE -i"YCH6165.CBI" -NM "Ych6165.COB"

"OBJ\Ych6165.DLL" : "Ych6165.OBJ" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"OBJ\Ych6165.DLL" \
	@"YCH6165.001" 

"OBJ\Ych6165.LIB" : "Ych6165.OBJ" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"OBJ\Ych6165.DLL" \
	@"YCH6165.001" 


"C:\makishisys\yawobj\Ych6165.DLL" : "Ych6165.OBJ" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\Ych6165.DLL" \
	@"YCH6165.002" 

"C:\makishisys\yawobj\Ych6165.LIB" : "Ych6165.OBJ" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\Ych6165.DLL" \
	@"YCH6165.002" 


