################################################################################
#                               NetCOBOL V7.2L10
################################################################################

.SUFFIXES:

COBOL_PATH = C:\Program Files\NetCOBOL
PROJECT_PATH = P:\COMSYS\PG
OBJECT_PATH = 
LINK_DEBUG_OPTION = 
LINK_MAP_OPTION = 


ALL : "obj\Yaw9224.dll" "C:\makishisys\hmobj\Yaw9224.dll" "C:\makishisys\yawobj\Yaw9224.dll" 

REBUILD : CLEAN ALL
CLEAN :
	DEL "obj\Yaw9224.dll" 
	DEL "obj\Yaw9224.LIB" 
	DEL "Yaw9224.OBJ" 
	DEL "C:\makishisys\hmobj\Yaw9224.dll" 
	DEL "C:\makishisys\hmobj\Yaw9224.LIB" 
	DEL "C:\makishisys\yawobj\Yaw9224.dll" 
	DEL "C:\makishisys\yawobj\Yaw9224.LIB" 


"Yaw9224.OBJ" : "Yaw9224.cob" "YAW9224.CBI"
	COBOL32.EXE -i"YAW9224.CBI" -NM "Yaw9224.cob"

"obj\Yaw9224.dll" : "Yaw9224.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\Yaw9224.dll" \
	@"YAW9224.001" 

"obj\Yaw9224.LIB" : "Yaw9224.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\Yaw9224.dll" \
	@"YAW9224.001" 


"C:\makishisys\hmobj\Yaw9224.dll" : "Yaw9224.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\Yaw9224.dll" \
	@"YAW9224.002" 

"C:\makishisys\hmobj\Yaw9224.LIB" : "Yaw9224.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\Yaw9224.dll" \
	@"YAW9224.002" 


"C:\makishisys\yawobj\Yaw9224.dll" : "Yaw9224.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\Yaw9224.dll" \
	@"YAW9224.003" 

"C:\makishisys\yawobj\Yaw9224.LIB" : "Yaw9224.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\Yaw9224.dll" \
	@"YAW9224.003" 


