################################################################################
#                               NetCOBOL V7.2L10
################################################################################

.SUFFIXES:

COBOL_PATH = C:\Program Files\NetCOBOL
PROJECT_PATH = P:\COMSYS\PG
OBJECT_PATH = 
LINK_DEBUG_OPTION = 
LINK_MAP_OPTION = 


ALL : "obj\Yaw9222.dll" "C:\makishisys\hmobj\Yaw9222.dll" "C:\makishisys\yawobj\Yaw9222.dll" 

REBUILD : CLEAN ALL
CLEAN :
	DEL "obj\Yaw9222.dll" 
	DEL "obj\Yaw9222.LIB" 
	DEL "Yaw9222.OBJ" 
	DEL "C:\makishisys\hmobj\Yaw9222.dll" 
	DEL "C:\makishisys\hmobj\Yaw9222.LIB" 
	DEL "C:\makishisys\yawobj\Yaw9222.dll" 
	DEL "C:\makishisys\yawobj\Yaw9222.LIB" 


"Yaw9222.OBJ" : "Yaw9222.cob" "YAW9222.CBI"
	COBOL32.EXE -i"YAW9222.CBI" -NM "Yaw9222.cob"

"obj\Yaw9222.dll" : "Yaw9222.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\Yaw9222.dll" \
	@"YAW9222.001" 

"obj\Yaw9222.LIB" : "Yaw9222.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\Yaw9222.dll" \
	@"YAW9222.001" 


"C:\makishisys\hmobj\Yaw9222.dll" : "Yaw9222.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\Yaw9222.dll" \
	@"YAW9222.002" 

"C:\makishisys\hmobj\Yaw9222.LIB" : "Yaw9222.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\Yaw9222.dll" \
	@"YAW9222.002" 


"C:\makishisys\yawobj\Yaw9222.dll" : "Yaw9222.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\Yaw9222.dll" \
	@"YAW9222.003" 

"C:\makishisys\yawobj\Yaw9222.LIB" : "Yaw9222.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\Yaw9222.dll" \
	@"YAW9222.003" 


