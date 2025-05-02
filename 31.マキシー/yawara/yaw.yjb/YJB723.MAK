################################################################################
#                               NetCOBOL V7.2L10
################################################################################

.SUFFIXES:

COBOL_PATH = C:\Program Files\NetCOBOL
PROJECT_PATH = P:\YAWARA\YAW.YJB
OBJECT_PATH = 
LINK_DEBUG_OPTION = 
LINK_MAP_OPTION = 


ALL : "obj\Yjb723.dll" "C:\makishisys\yawobj\Yjb723.dll" 

REBUILD : CLEAN ALL
CLEAN :
	DEL "obj\Yjb723.dll" 
	DEL "obj\Yjb723.LIB" 
	DEL "Yjb723.OBJ" 
	DEL "C:\makishisys\yawobj\Yjb723.dll" 
	DEL "C:\makishisys\yawobj\Yjb723.LIB" 


"Yjb723.OBJ" : "Yjb723.cob" "YJB723.CBI"
	COBOL32.EXE -i"YJB723.CBI" -NM "Yjb723.cob"

"obj\Yjb723.dll" : "Yjb723.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\Yjb723.dll" \
	@"YJB723.001" 

"obj\Yjb723.LIB" : "Yjb723.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\Yjb723.dll" \
	@"YJB723.001" 


"C:\makishisys\yawobj\Yjb723.dll" : "Yjb723.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\Yjb723.dll" \
	@"YJB723.002" 

"C:\makishisys\yawobj\Yjb723.LIB" : "Yjb723.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\Yjb723.dll" \
	@"YJB723.002" 


