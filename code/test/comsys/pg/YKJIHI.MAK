################################################################################
#                               NetCOBOL V7.2L10
################################################################################

.SUFFIXES:

COBOL_PATH = C:\Program Files\NetCOBOL
PROJECT_PATH = P:\COMSYS\PG
OBJECT_PATH = 
LINK_DEBUG_OPTION = 
LINK_MAP_OPTION = 


ALL : "obj\Ykjihi.dll" "C:\makishisys\hmobj\Ykjihi.dll" "C:\makishisys\yawobj\Ykjihi.dll" 

REBUILD : CLEAN ALL
CLEAN :
	DEL "obj\Ykjihi.dll" 
	DEL "obj\Ykjihi.LIB" 
	DEL "Ykjihi.OBJ" 
	DEL "C:\makishisys\hmobj\Ykjihi.dll" 
	DEL "C:\makishisys\hmobj\Ykjihi.LIB" 
	DEL "C:\makishisys\yawobj\Ykjihi.dll" 
	DEL "C:\makishisys\yawobj\Ykjihi.LIB" 


"Ykjihi.OBJ" : "Ykjihi.cob" "YKJIHI.CBI"
	COBOL32.EXE -i"YKJIHI.CBI" -NM "Ykjihi.cob"

"obj\Ykjihi.dll" : "Ykjihi.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\Ykjihi.dll" \
	@"YKJIHI.001" 

"obj\Ykjihi.LIB" : "Ykjihi.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\Ykjihi.dll" \
	@"YKJIHI.001" 


"C:\makishisys\hmobj\Ykjihi.dll" : "Ykjihi.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\Ykjihi.dll" \
	@"YKJIHI.002" 

"C:\makishisys\hmobj\Ykjihi.LIB" : "Ykjihi.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\Ykjihi.dll" \
	@"YKJIHI.002" 


"C:\makishisys\yawobj\Ykjihi.dll" : "Ykjihi.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\Ykjihi.dll" \
	@"YKJIHI.003" 

"C:\makishisys\yawobj\Ykjihi.LIB" : "Ykjihi.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\Ykjihi.dll" \
	@"YKJIHI.003" 


