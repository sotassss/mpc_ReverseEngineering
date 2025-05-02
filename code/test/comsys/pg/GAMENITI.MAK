################################################################################
#                               NetCOBOL V7.2L10
################################################################################

.SUFFIXES:

COBOL_PATH = C:\Program Files\NetCOBOL
PROJECT_PATH = \\HOST2\MAKISHI\COMSYS\PG
OBJECT_PATH = 
LINK_DEBUG_OPTION = 
LINK_MAP_OPTION = 


ALL : "obj\gameniti.dll" "C:\makishisys\hmobj\gameniti.dll" "C:\makishisys\yawobj\gameniti.dll" 

REBUILD : CLEAN ALL
CLEAN :
	DEL "obj\gameniti.dll" 
	DEL "obj\gameniti.LIB" 
	DEL "gameniti.OBJ" 
	DEL "C:\makishisys\hmobj\gameniti.dll" 
	DEL "C:\makishisys\hmobj\gameniti.LIB" 
	DEL "C:\makishisys\yawobj\gameniti.dll" 
	DEL "C:\makishisys\yawobj\gameniti.LIB" 


"gameniti.OBJ" : "gameniti.cob" "GAMENITI.CBI"
	COBOL32.EXE -i"GAMENITI.CBI" -NM "gameniti.cob"

"obj\gameniti.dll" : "gameniti.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\gameniti.dll" \
	@"GAMENITI.001" 

"obj\gameniti.LIB" : "gameniti.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\gameniti.dll" \
	@"GAMENITI.001" 


"C:\makishisys\hmobj\gameniti.dll" : "gameniti.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\gameniti.dll" \
	@"GAMENITI.002" 

"C:\makishisys\hmobj\gameniti.LIB" : "gameniti.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\gameniti.dll" \
	@"GAMENITI.002" 


"C:\makishisys\yawobj\gameniti.dll" : "gameniti.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\gameniti.dll" \
	@"GAMENITI.003" 

"C:\makishisys\yawobj\gameniti.LIB" : "gameniti.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\gameniti.dll" \
	@"GAMENITI.003" 


