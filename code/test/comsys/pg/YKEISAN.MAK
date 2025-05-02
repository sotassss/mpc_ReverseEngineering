################################################################################
#                               NetCOBOL V7.2L10
################################################################################

.SUFFIXES:

COBOL_PATH = C:\Program Files\NetCOBOL
PROJECT_PATH = \\HOST\MAKISHI\COMSYS\PG
OBJECT_PATH = 
LINK_DEBUG_OPTION = 
LINK_MAP_OPTION = 


ALL : "obj\ykeisan.dll" "C:\makishisys\hmobj\ykeisan.dll" "C:\makishisys\yawobj\ykeisan.dll" 

REBUILD : CLEAN ALL
CLEAN :
	DEL "obj\ykeisan.dll" 
	DEL "obj\ykeisan.LIB" 
	DEL "ykeisan.OBJ" 
	DEL "C:\makishisys\hmobj\ykeisan.dll" 
	DEL "C:\makishisys\hmobj\ykeisan.LIB" 
	DEL "C:\makishisys\yawobj\ykeisan.dll" 
	DEL "C:\makishisys\yawobj\ykeisan.LIB" 


"ykeisan.OBJ" : "ykeisan.cob" "YKEISAN.CBI"
	COBOL32.EXE -i"YKEISAN.CBI" -NM "ykeisan.cob"

"obj\ykeisan.dll" : "ykeisan.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\ykeisan.dll" \
	@"YKEISAN.001" 

"obj\ykeisan.LIB" : "ykeisan.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\ykeisan.dll" \
	@"YKEISAN.001" 


"C:\makishisys\hmobj\ykeisan.dll" : "ykeisan.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\ykeisan.dll" \
	@"YKEISAN.002" 

"C:\makishisys\hmobj\ykeisan.LIB" : "ykeisan.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\ykeisan.dll" \
	@"YKEISAN.002" 


"C:\makishisys\yawobj\ykeisan.dll" : "ykeisan.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\ykeisan.dll" \
	@"YKEISAN.003" 

"C:\makishisys\yawobj\ykeisan.LIB" : "ykeisan.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\ykeisan.dll" \
	@"YKEISAN.003" 


