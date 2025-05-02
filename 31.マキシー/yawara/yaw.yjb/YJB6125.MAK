################################################################################
#                               NetCOBOL V7.2L10
################################################################################

.SUFFIXES:

COBOL_PATH = C:\Program Files\NetCOBOL
PROJECT_PATH = P:\YAWARA\YAW.YJB
OBJECT_PATH = 
LINK_DEBUG_OPTION = 
LINK_MAP_OPTION = 


ALL : "obj\Yjb6125.dll" "C:\makishisys\yawobj\Yjb6125.dll" 

REBUILD : CLEAN ALL
CLEAN :
	DEL "obj\Yjb6125.dll" 
	DEL "obj\Yjb6125.LIB" 
	DEL "Yjb6125.OBJ" 
	DEL "C:\makishisys\yawobj\Yjb6125.dll" 
	DEL "C:\makishisys\yawobj\Yjb6125.LIB" 


"Yjb6125.OBJ" : "Yjb6125.cob" "YJB6125.CBI"
	COBOL32.EXE -i"YJB6125.CBI" -NM "Yjb6125.cob"

"obj\Yjb6125.dll" : "Yjb6125.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\Yjb6125.dll" \
	@"YJB6125.001" 

"obj\Yjb6125.LIB" : "Yjb6125.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\Yjb6125.dll" \
	@"YJB6125.001" 


"C:\makishisys\yawobj\Yjb6125.dll" : "Yjb6125.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\Yjb6125.dll" \
	@"YJB6125.002" 

"C:\makishisys\yawobj\Yjb6125.LIB" : "Yjb6125.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\Yjb6125.dll" \
	@"YJB6125.002" 


