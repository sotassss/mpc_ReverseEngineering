################################################################################
#                               NetCOBOL V7.2L10
################################################################################

.SUFFIXES:

COBOL_PATH = C:\Program Files\NetCOBOL
PROJECT_PATH = P:\COMSYS\PG
OBJECT_PATH = 
LINK_DEBUG_OPTION = 
LINK_MAP_OPTION = 


ALL : "obj\kkcnv.dll" "C:\makishisys\hmobj\kkcnv.dll" "C:\makishisys\yawobj\kkcnv.dll" 

REBUILD : CLEAN ALL
CLEAN :
	DEL "obj\kkcnv.dll" 
	DEL "obj\kkcnv.LIB" 
	DEL "kkcnv.OBJ" 
	DEL "C:\makishisys\hmobj\kkcnv.dll" 
	DEL "C:\makishisys\hmobj\kkcnv.LIB" 
	DEL "C:\makishisys\yawobj\kkcnv.dll" 
	DEL "C:\makishisys\yawobj\kkcnv.LIB" 


"kkcnv.OBJ" : "kkcnv.cob" "KKCNV.CBI"
	COBOL32.EXE -i"KKCNV.CBI" -NM "kkcnv.cob"

"obj\kkcnv.dll" : "kkcnv.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\kkcnv.dll" \
	@"KKCNV.001" 

"obj\kkcnv.LIB" : "kkcnv.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\kkcnv.dll" \
	@"KKCNV.001" 


"C:\makishisys\hmobj\kkcnv.dll" : "kkcnv.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\kkcnv.dll" \
	@"KKCNV.002" 

"C:\makishisys\hmobj\kkcnv.LIB" : "kkcnv.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\kkcnv.dll" \
	@"KKCNV.002" 


"C:\makishisys\yawobj\kkcnv.dll" : "kkcnv.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\kkcnv.dll" \
	@"KKCNV.003" 

"C:\makishisys\yawobj\kkcnv.LIB" : "kkcnv.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\kkcnv.dll" \
	@"KKCNV.003" 


