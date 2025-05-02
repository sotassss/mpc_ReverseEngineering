################################################################################
#                               NetCOBOL V7.2L10
################################################################################

.SUFFIXES:

COBOL_PATH = C:\Program Files\NetCOBOL
PROJECT_PATH = P:\YAWARA\YAW.YJB
OBJECT_PATH = 
LINK_DEBUG_OPTION = 
LINK_MAP_OPTION = 


ALL : "obj\Yjb6061.dll" "C:\makishisys\yawobj\Yjb6061.dll" 

REBUILD : CLEAN ALL
CLEAN :
	DEL "obj\Yjb6061.dll" 
	DEL "obj\Yjb6061.LIB" 
	DEL "Yjb6061.OBJ" 
	DEL "C:\makishisys\yawobj\Yjb6061.dll" 
	DEL "C:\makishisys\yawobj\Yjb6061.LIB" 


"Yjb6061.OBJ" : "Yjb6061.cob" "YJB6061.CBI"
	COBOL32.EXE -i"YJB6061.CBI" -NM "Yjb6061.cob"

"obj\Yjb6061.dll" : "Yjb6061.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\Yjb6061.dll" \
	@"YJB6061.001" 

"obj\Yjb6061.LIB" : "Yjb6061.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\Yjb6061.dll" \
	@"YJB6061.001" 


"C:\makishisys\yawobj\Yjb6061.dll" : "Yjb6061.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\Yjb6061.dll" \
	@"YJB6061.002" 

"C:\makishisys\yawobj\Yjb6061.LIB" : "Yjb6061.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\Yjb6061.dll" \
	@"YJB6061.002" 


