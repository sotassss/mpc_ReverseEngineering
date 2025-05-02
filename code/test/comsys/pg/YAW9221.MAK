################################################################################
#                               NetCOBOL V7.2L10
################################################################################

.SUFFIXES:

COBOL_PATH = C:\Program Files\NetCOBOL
PROJECT_PATH = P:\COMSYS\PG
OBJECT_PATH = 
LINK_DEBUG_OPTION = 
LINK_MAP_OPTION = 


ALL : "obj\Yaw9221.dll" "C:\makishisys\hmobj\Yaw9221.dll" "C:\makishisys\yawobj\Yaw9221.dll" 

REBUILD : CLEAN ALL
CLEAN :
	DEL "obj\Yaw9221.dll" 
	DEL "obj\Yaw9221.LIB" 
	DEL "Yaw9221.OBJ" 
	DEL "C:\makishisys\hmobj\Yaw9221.dll" 
	DEL "C:\makishisys\hmobj\Yaw9221.LIB" 
	DEL "C:\makishisys\yawobj\Yaw9221.dll" 
	DEL "C:\makishisys\yawobj\Yaw9221.LIB" 


"Yaw9221.OBJ" : "Yaw9221.cob" "YAW9221.CBI"
	COBOL32.EXE -i"YAW9221.CBI" -NM "Yaw9221.cob"

"obj\Yaw9221.dll" : "Yaw9221.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\Yaw9221.dll" \
	@"YAW9221.001" 

"obj\Yaw9221.LIB" : "Yaw9221.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\Yaw9221.dll" \
	@"YAW9221.001" 


"C:\makishisys\hmobj\Yaw9221.dll" : "Yaw9221.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\Yaw9221.dll" \
	@"YAW9221.002" 

"C:\makishisys\hmobj\Yaw9221.LIB" : "Yaw9221.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\Yaw9221.dll" \
	@"YAW9221.002" 


"C:\makishisys\yawobj\Yaw9221.dll" : "Yaw9221.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\Yaw9221.dll" \
	@"YAW9221.003" 

"C:\makishisys\yawobj\Yaw9221.LIB" : "Yaw9221.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\Yaw9221.dll" \
	@"YAW9221.003" 


