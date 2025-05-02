################################################################################
#                               NetCOBOL V7.2L10
################################################################################

.SUFFIXES:

COBOL_PATH = C:\Program Files\NetCOBOL
PROJECT_PATH = P:\COMSYS\PG
OBJECT_PATH = 
LINK_DEBUG_OPTION = 
LINK_MAP_OPTION = 


ALL : "obj\jreceof2.dll" "C:\makishisys\hmobj\jreceof2.dll" "C:\makishisys\yawobj\jreceof2.dll" 

REBUILD : CLEAN ALL
CLEAN :
	DEL "obj\jreceof2.dll" 
	DEL "obj\jreceof2.LIB" 
	DEL "jreceof2.OBJ" 
	DEL "C:\makishisys\hmobj\jreceof2.dll" 
	DEL "C:\makishisys\hmobj\jreceof2.LIB" 
	DEL "C:\makishisys\yawobj\jreceof2.dll" 
	DEL "C:\makishisys\yawobj\jreceof2.LIB" 


"jreceof2.OBJ" : "jreceof2.cob" "JRECEOF2.CBI"
	COBOL32.EXE -i"JRECEOF2.CBI" -NM "jreceof2.cob"

"obj\jreceof2.dll" : "jreceof2.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\jreceof2.dll" \
	@"JRECEOF2.001" 

"obj\jreceof2.LIB" : "jreceof2.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\jreceof2.dll" \
	@"JRECEOF2.001" 


"C:\makishisys\hmobj\jreceof2.dll" : "jreceof2.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\jreceof2.dll" \
	@"JRECEOF2.002" 

"C:\makishisys\hmobj\jreceof2.LIB" : "jreceof2.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\jreceof2.dll" \
	@"JRECEOF2.002" 


"C:\makishisys\yawobj\jreceof2.dll" : "jreceof2.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\jreceof2.dll" \
	@"JRECEOF2.003" 

"C:\makishisys\yawobj\jreceof2.LIB" : "jreceof2.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\jreceof2.dll" \
	@"JRECEOF2.003" 


