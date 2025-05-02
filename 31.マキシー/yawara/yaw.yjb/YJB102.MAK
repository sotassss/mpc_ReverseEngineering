################################################################################
#                               NetCOBOL V7.2L10
################################################################################

.SUFFIXES:

COBOL_PATH = C:\Program Files\NetCOBOL
PROJECT_PATH = P:\YAWARA\YAW.YJB
OBJECT_PATH = 
LINK_DEBUG_OPTION = 
LINK_MAP_OPTION = 


ALL : "obj\Yjb102.dll" "C:\makishisys\yawobj\Yjb102.dll" 

REBUILD : CLEAN ALL
CLEAN :
	DEL "obj\Yjb102.dll" 
	DEL "obj\Yjb102.LIB" 
	DEL "Yjb102.OBJ" 
	DEL "C:\makishisys\yawobj\Yjb102.dll" 
	DEL "C:\makishisys\yawobj\Yjb102.LIB" 


"Yjb102.OBJ" : "Yjb102.cob" "YJB102.CBI"
	COBOL32.EXE -i"YJB102.CBI" -NM "Yjb102.cob"

"obj\Yjb102.dll" : "Yjb102.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\Yjb102.dll" \
	@"YJB102.001" 

"obj\Yjb102.LIB" : "Yjb102.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\Yjb102.dll" \
	@"YJB102.001" 


"C:\makishisys\yawobj\Yjb102.dll" : "Yjb102.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\Yjb102.dll" \
	@"YJB102.002" 

"C:\makishisys\yawobj\Yjb102.LIB" : "Yjb102.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\Yjb102.dll" \
	@"YJB102.002" 


