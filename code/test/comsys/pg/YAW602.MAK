################################################################################
#                               NetCOBOL V7.2L10
################################################################################

.SUFFIXES:

COBOL_PATH = C:\Program Files\NetCOBOL
PROJECT_PATH = P:\COMSYS\PG
OBJECT_PATH = 
LINK_DEBUG_OPTION = 
LINK_MAP_OPTION = 


ALL : "obj\Yaw602.dll" "C:\makishisys\hmobj\Yaw602.dll" "C:\makishisys\yawobj\Yaw602.dll" 

REBUILD : CLEAN ALL
CLEAN :
	DEL "obj\Yaw602.dll" 
	DEL "obj\Yaw602.LIB" 
	DEL "Yaw602.OBJ" 
	DEL "C:\makishisys\hmobj\Yaw602.dll" 
	DEL "C:\makishisys\hmobj\Yaw602.LIB" 
	DEL "C:\makishisys\yawobj\Yaw602.dll" 
	DEL "C:\makishisys\yawobj\Yaw602.LIB" 


"Yaw602.OBJ" : "Yaw602.cob" "YAW602.CBI"
	COBOL32.EXE -i"YAW602.CBI" -NM "Yaw602.cob"

"obj\Yaw602.dll" : "Yaw602.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\Yaw602.dll" \
	@"YAW602.001" 

"obj\Yaw602.LIB" : "Yaw602.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\Yaw602.dll" \
	@"YAW602.001" 


"C:\makishisys\hmobj\Yaw602.dll" : "Yaw602.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\Yaw602.dll" \
	@"YAW602.002" 

"C:\makishisys\hmobj\Yaw602.LIB" : "Yaw602.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\Yaw602.dll" \
	@"YAW602.002" 


"C:\makishisys\yawobj\Yaw602.dll" : "Yaw602.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\Yaw602.dll" \
	@"YAW602.003" 

"C:\makishisys\yawobj\Yaw602.LIB" : "Yaw602.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\Yaw602.dll" \
	@"YAW602.003" 


