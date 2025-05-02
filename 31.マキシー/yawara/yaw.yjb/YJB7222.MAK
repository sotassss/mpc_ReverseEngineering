################################################################################
#                               NetCOBOL V7.2L10
################################################################################

.SUFFIXES:

COBOL_PATH = C:\Program Files\NetCOBOL
PROJECT_PATH = P:\YAWARA\YAW.YJB
OBJECT_PATH = 
LINK_DEBUG_OPTION = 
LINK_MAP_OPTION = 


ALL : "obj\Yjb7222.dll" "C:\makishisys\yawobj\Yjb7222.dll" 

REBUILD : CLEAN ALL
CLEAN :
	DEL "obj\Yjb7222.dll" 
	DEL "obj\Yjb7222.LIB" 
	DEL "Yjb7222.OBJ" 
	DEL "C:\makishisys\yawobj\Yjb7222.dll" 
	DEL "C:\makishisys\yawobj\Yjb7222.LIB" 


"Yjb7222.OBJ" : "Yjb7222.cob" "YJB7222.CBI"
	COBOL32.EXE -i"YJB7222.CBI" -NM "Yjb7222.cob"

"obj\Yjb7222.dll" : "Yjb7222.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\Yjb7222.dll" \
	@"YJB7222.001" 

"obj\Yjb7222.LIB" : "Yjb7222.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\Yjb7222.dll" \
	@"YJB7222.001" 


"C:\makishisys\yawobj\Yjb7222.dll" : "Yjb7222.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\Yjb7222.dll" \
	@"YJB7222.002" 

"C:\makishisys\yawobj\Yjb7222.LIB" : "Yjb7222.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\Yjb7222.dll" \
	@"YJB7222.002" 


