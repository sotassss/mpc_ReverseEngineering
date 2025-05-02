################################################################################
#                               NetCOBOL V7.2L10
################################################################################

.SUFFIXES:

COBOL_PATH = C:\Program Files\NetCOBOL
PROJECT_PATH = P:\YAWARA\YAW.YJB
OBJECT_PATH = 
LINK_DEBUG_OPTION = 
LINK_MAP_OPTION = 


ALL : "obj\Yjb101.dll" "C:\makishisys\yawobj\Yjb101.dll" 

REBUILD : CLEAN ALL
CLEAN :
	DEL "obj\Yjb101.dll" 
	DEL "obj\Yjb101.LIB" 
	DEL "Yjb101.OBJ" 
	DEL "C:\makishisys\yawobj\Yjb101.dll" 
	DEL "C:\makishisys\yawobj\Yjb101.LIB" 


"Yjb101.OBJ" : "Yjb101.cob" "YJB101.CBI"
	COBOL32.EXE -i"YJB101.CBI" -NM "Yjb101.cob"

"obj\Yjb101.dll" : "Yjb101.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\Yjb101.dll" \
	@"YJB101.001" 

"obj\Yjb101.LIB" : "Yjb101.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\Yjb101.dll" \
	@"YJB101.001" 


"C:\makishisys\yawobj\Yjb101.dll" : "Yjb101.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\Yjb101.dll" \
	@"YJB101.002" 

"C:\makishisys\yawobj\Yjb101.LIB" : "Yjb101.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\Yjb101.dll" \
	@"YJB101.002" 


