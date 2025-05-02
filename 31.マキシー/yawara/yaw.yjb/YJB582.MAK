################################################################################
#                               NetCOBOL V7.2L10
################################################################################

.SUFFIXES:

COBOL_PATH = C:\Program Files\NetCOBOL
PROJECT_PATH = P:\YAWARA\YAW.YJB
OBJECT_PATH = 
LINK_DEBUG_OPTION = 
LINK_MAP_OPTION = 


ALL : "obj\Yjb582.dll" "C:\makishisys\yawobj\Yjb582.dll" 

REBUILD : CLEAN ALL
CLEAN :
	DEL "obj\Yjb582.dll" 
	DEL "obj\Yjb582.LIB" 
	DEL "Yjb582.OBJ" 
	DEL "C:\makishisys\yawobj\Yjb582.dll" 
	DEL "C:\makishisys\yawobj\Yjb582.LIB" 


"Yjb582.OBJ" : "Yjb582.cob" "YJB582.CBI"
	COBOL32.EXE -i"YJB582.CBI" -NM "Yjb582.cob"

"obj\Yjb582.dll" : "Yjb582.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\Yjb582.dll" \
	@"YJB582.001" 

"obj\Yjb582.LIB" : "Yjb582.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\Yjb582.dll" \
	@"YJB582.001" 


"C:\makishisys\yawobj\Yjb582.dll" : "Yjb582.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\Yjb582.dll" \
	@"YJB582.002" 

"C:\makishisys\yawobj\Yjb582.LIB" : "Yjb582.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\Yjb582.dll" \
	@"YJB582.002" 


