################################################################################
#                               NetCOBOL V7.2L10
################################################################################

.SUFFIXES:

COBOL_PATH = C:\Program Files\NetCOBOL
PROJECT_PATH = P:\COMSYS\PG
OBJECT_PATH = 
LINK_DEBUG_OPTION = 
LINK_MAP_OPTION = 


ALL : "obj\Yaw9223.dll" "C:\makishisys\hmobj\Yaw9223.dll" "C:\makishisys\yawobj\Yaw9223.dll" 

REBUILD : CLEAN ALL
CLEAN :
	DEL "obj\Yaw9223.dll" 
	DEL "obj\Yaw9223.LIB" 
	DEL "Yaw9223.OBJ" 
	DEL "C:\makishisys\hmobj\Yaw9223.dll" 
	DEL "C:\makishisys\hmobj\Yaw9223.LIB" 
	DEL "C:\makishisys\yawobj\Yaw9223.dll" 
	DEL "C:\makishisys\yawobj\Yaw9223.LIB" 


"Yaw9223.OBJ" : "Yaw9223.cob" "YAW9223.CBI"
	COBOL32.EXE -i"YAW9223.CBI" -NM "Yaw9223.cob"

"obj\Yaw9223.dll" : "Yaw9223.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\Yaw9223.dll" \
	@"YAW9223.001" 

"obj\Yaw9223.LIB" : "Yaw9223.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\Yaw9223.dll" \
	@"YAW9223.001" 


"C:\makishisys\hmobj\Yaw9223.dll" : "Yaw9223.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\Yaw9223.dll" \
	@"YAW9223.002" 

"C:\makishisys\hmobj\Yaw9223.LIB" : "Yaw9223.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\Yaw9223.dll" \
	@"YAW9223.002" 


"C:\makishisys\yawobj\Yaw9223.dll" : "Yaw9223.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\Yaw9223.dll" \
	@"YAW9223.003" 

"C:\makishisys\yawobj\Yaw9223.LIB" : "Yaw9223.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\Yaw9223.dll" \
	@"YAW9223.003" 


