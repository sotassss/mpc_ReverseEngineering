################################################################################
#                               NetCOBOL V7.2L10
################################################################################

.SUFFIXES:

COBOL_PATH = C:\Program Files\NetCOBOL
PROJECT_PATH = P:\COMSYS\PG
OBJECT_PATH = 
LINK_DEBUG_OPTION = 
LINK_MAP_OPTION = 


ALL : "obj\getkyori.dll" "C:\makishisys\hmobj\getkyori.dll" "C:\makishisys\yawobj\getkyori.dll" 

REBUILD : CLEAN ALL
CLEAN :
	DEL "obj\getkyori.dll" 
	DEL "obj\getkyori.LIB" 
	DEL "getkyori.OBJ" 
	DEL "C:\makishisys\hmobj\getkyori.dll" 
	DEL "C:\makishisys\hmobj\getkyori.LIB" 
	DEL "C:\makishisys\yawobj\getkyori.dll" 
	DEL "C:\makishisys\yawobj\getkyori.LIB" 


"getkyori.OBJ" : "getkyori.cob" "GETKYORI.CBI"
	COBOL32.EXE -i"GETKYORI.CBI" -NM "getkyori.cob"

"obj\getkyori.dll" : "getkyori.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\getkyori.dll" \
	@"GETKYORI.001" 

"obj\getkyori.LIB" : "getkyori.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\getkyori.dll" \
	@"GETKYORI.001" 


"C:\makishisys\hmobj\getkyori.dll" : "getkyori.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\getkyori.dll" \
	@"GETKYORI.002" 

"C:\makishisys\hmobj\getkyori.LIB" : "getkyori.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\getkyori.dll" \
	@"GETKYORI.002" 


"C:\makishisys\yawobj\getkyori.dll" : "getkyori.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\getkyori.dll" \
	@"GETKYORI.003" 

"C:\makishisys\yawobj\getkyori.LIB" : "getkyori.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\getkyori.dll" \
	@"GETKYORI.003" 


