################################################################################
#                               NetCOBOL V7.2L10
################################################################################

.SUFFIXES:

COBOL_PATH = C:\Program Files\NetCOBOL
PROJECT_PATH = P:\COMSYS\PG
OBJECT_PATH = 
LINK_DEBUG_OPTION = 
LINK_MAP_OPTION = 


ALL : "obj\moji2.dll" "C:\makishisys\hmobj\moji2.dll" "C:\makishisys\yawobj\moji2.dll" 

REBUILD : CLEAN ALL
CLEAN :
	DEL "obj\moji2.dll" 
	DEL "obj\moji2.LIB" 
	DEL "moji2.OBJ" 
	DEL "C:\makishisys\hmobj\moji2.dll" 
	DEL "C:\makishisys\hmobj\moji2.LIB" 
	DEL "C:\makishisys\yawobj\moji2.dll" 
	DEL "C:\makishisys\yawobj\moji2.LIB" 


"moji2.OBJ" : "moji2.cob" "MOJI2.CBI"
	COBOL32.EXE -i"MOJI2.CBI" -NM "moji2.cob"

"obj\moji2.dll" : "moji2.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\moji2.dll" \
	@"MOJI2.001" 

"obj\moji2.LIB" : "moji2.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\moji2.dll" \
	@"MOJI2.001" 


"C:\makishisys\hmobj\moji2.dll" : "moji2.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\moji2.dll" \
	@"MOJI2.002" 

"C:\makishisys\hmobj\moji2.LIB" : "moji2.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\moji2.dll" \
	@"MOJI2.002" 


"C:\makishisys\yawobj\moji2.dll" : "moji2.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\moji2.dll" \
	@"MOJI2.003" 

"C:\makishisys\yawobj\moji2.LIB" : "moji2.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\moji2.dll" \
	@"MOJI2.003" 


