################################################################################
#                               NetCOBOL V7.2L10
################################################################################

.SUFFIXES:

COBOL_PATH = C:\Program Files\NetCOBOL
PROJECT_PATH = P:\YAWARA\YAW.YJB
OBJECT_PATH = 
LINK_DEBUG_OPTION = 
LINK_MAP_OPTION = 


ALL : "obj\Yjb721.dll" "C:\makishisys\yawobj\Yjb721.dll" 

REBUILD : CLEAN ALL
CLEAN :
	DEL "obj\Yjb721.dll" 
	DEL "obj\Yjb721.LIB" 
	DEL "Yjb721.OBJ" 
	DEL "C:\makishisys\yawobj\Yjb721.dll" 
	DEL "C:\makishisys\yawobj\Yjb721.LIB" 


"Yjb721.OBJ" : "Yjb721.cob" "YJB721.CBI"
	COBOL32.EXE -i"YJB721.CBI" -NM "Yjb721.cob"

"obj\Yjb721.dll" : "Yjb721.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\Yjb721.dll" \
	@"YJB721.001" 

"obj\Yjb721.LIB" : "Yjb721.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\Yjb721.dll" \
	@"YJB721.001" 


"C:\makishisys\yawobj\Yjb721.dll" : "Yjb721.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\Yjb721.dll" \
	@"YJB721.002" 

"C:\makishisys\yawobj\Yjb721.LIB" : "Yjb721.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\Yjb721.dll" \
	@"YJB721.002" 


