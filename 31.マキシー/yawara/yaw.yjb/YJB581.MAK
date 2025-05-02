################################################################################
#                               NetCOBOL V7.2L10
################################################################################

.SUFFIXES:

COBOL_PATH = C:\Program Files\NetCOBOL
PROJECT_PATH = P:\YAWARA\YAW.YJB
OBJECT_PATH = 
LINK_DEBUG_OPTION = 
LINK_MAP_OPTION = 


ALL : "obj\Yjb581.dll" "C:\makishisys\yawobj\Yjb581.dll" 

REBUILD : CLEAN ALL
CLEAN :
	DEL "obj\Yjb581.dll" 
	DEL "obj\Yjb581.LIB" 
	DEL "Yjb581.OBJ" 
	DEL "C:\makishisys\yawobj\Yjb581.dll" 
	DEL "C:\makishisys\yawobj\Yjb581.LIB" 


"Yjb581.OBJ" : "Yjb581.cob" "YJB581.CBI"
	COBOL32.EXE -i"YJB581.CBI" -NM "Yjb581.cob"

"obj\Yjb581.dll" : "Yjb581.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\Yjb581.dll" \
	@"YJB581.001" 

"obj\Yjb581.LIB" : "Yjb581.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\Yjb581.dll" \
	@"YJB581.001" 


"C:\makishisys\yawobj\Yjb581.dll" : "Yjb581.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\Yjb581.dll" \
	@"YJB581.002" 

"C:\makishisys\yawobj\Yjb581.LIB" : "Yjb581.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\Yjb581.dll" \
	@"YJB581.002" 


