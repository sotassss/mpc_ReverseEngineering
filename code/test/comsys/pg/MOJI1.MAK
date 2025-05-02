################################################################################
#                               NetCOBOL V7.2L10
################################################################################

.SUFFIXES:

COBOL_PATH = C:\Program Files\NetCOBOL
PROJECT_PATH = P:\COMSYS\PG
OBJECT_PATH = 
LINK_DEBUG_OPTION = 
LINK_MAP_OPTION = 


ALL : "obj\moji1.dll" "C:\makishisys\hmobj\moji1.dll" "C:\makishisys\yawobj\moji1.dll" 

REBUILD : CLEAN ALL
CLEAN :
	DEL "obj\moji1.dll" 
	DEL "obj\moji1.LIB" 
	DEL "moji1.OBJ" 
	DEL "C:\makishisys\hmobj\moji1.dll" 
	DEL "C:\makishisys\hmobj\moji1.LIB" 
	DEL "C:\makishisys\yawobj\moji1.dll" 
	DEL "C:\makishisys\yawobj\moji1.LIB" 


"moji1.OBJ" : "moji1.cob" "MOJI1.CBI"
	COBOL32.EXE -i"MOJI1.CBI" -NM "moji1.cob"

"obj\moji1.dll" : "moji1.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\moji1.dll" \
	@"MOJI1.001" 

"obj\moji1.LIB" : "moji1.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\moji1.dll" \
	@"MOJI1.001" 


"C:\makishisys\hmobj\moji1.dll" : "moji1.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\moji1.dll" \
	@"MOJI1.002" 

"C:\makishisys\hmobj\moji1.LIB" : "moji1.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\moji1.dll" \
	@"MOJI1.002" 


"C:\makishisys\yawobj\moji1.dll" : "moji1.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\moji1.dll" \
	@"MOJI1.003" 

"C:\makishisys\yawobj\moji1.LIB" : "moji1.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\moji1.dll" \
	@"MOJI1.003" 


