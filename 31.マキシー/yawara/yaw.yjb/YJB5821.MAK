################################################################################
#                               NetCOBOL V7.2L10
################################################################################

.SUFFIXES:

COBOL_PATH = C:\Program Files\NetCOBOL
PROJECT_PATH = P:\YAWARA\YAW.YJB
OBJECT_PATH = 
LINK_DEBUG_OPTION = 
LINK_MAP_OPTION = 


ALL : "obj\Yjb5821.dll" "C:\makishisys\yawobj\Yjb5821.dll" 

REBUILD : CLEAN ALL
CLEAN :
	DEL "obj\Yjb5821.dll" 
	DEL "obj\Yjb5821.LIB" 
	DEL "Yjb5821.OBJ" 
	DEL "C:\makishisys\yawobj\Yjb5821.dll" 
	DEL "C:\makishisys\yawobj\Yjb5821.LIB" 


"Yjb5821.OBJ" : "Yjb5821.cob" "YJB5821.CBI"
	COBOL32.EXE -i"YJB5821.CBI" -NM "Yjb5821.cob"

"obj\Yjb5821.dll" : "Yjb5821.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\Yjb5821.dll" \
	@"YJB5821.001" 

"obj\Yjb5821.LIB" : "Yjb5821.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\Yjb5821.dll" \
	@"YJB5821.001" 


"C:\makishisys\yawobj\Yjb5821.dll" : "Yjb5821.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\Yjb5821.dll" \
	@"YJB5821.002" 

"C:\makishisys\yawobj\Yjb5821.LIB" : "Yjb5821.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\Yjb5821.dll" \
	@"YJB5821.002" 


