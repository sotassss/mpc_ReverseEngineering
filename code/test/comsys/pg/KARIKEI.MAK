################################################################################
#                               NetCOBOL V7.2L10
################################################################################

.SUFFIXES:

COBOL_PATH = C:\Program Files\NetCOBOL
PROJECT_PATH = P:\COMSYS\PG
OBJECT_PATH = 
LINK_DEBUG_OPTION = 
LINK_MAP_OPTION = 


ALL : "obj\KARIKEI.dll" "C:\makishisys\hmobj\KARIKEI.dll" "C:\makishisys\yawobj\KARIKEI.dll" 

REBUILD : CLEAN ALL
CLEAN :
	DEL "obj\KARIKEI.dll" 
	DEL "obj\KARIKEI.LIB" 
	DEL "KARIKEI.OBJ" 
	DEL "C:\makishisys\hmobj\KARIKEI.dll" 
	DEL "C:\makishisys\hmobj\KARIKEI.LIB" 
	DEL "C:\makishisys\yawobj\KARIKEI.dll" 
	DEL "C:\makishisys\yawobj\KARIKEI.LIB" 


"KARIKEI.OBJ" : "KARIKEI.COB" "KARIKEI.CBI"
	COBOL32.EXE -i"KARIKEI.CBI" -NM "KARIKEI.COB"

"obj\KARIKEI.dll" : "KARIKEI.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\KARIKEI.dll" \
	@"KARIKEI.001" 

"obj\KARIKEI.LIB" : "KARIKEI.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\KARIKEI.dll" \
	@"KARIKEI.001" 


"C:\makishisys\hmobj\KARIKEI.dll" : "KARIKEI.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\KARIKEI.dll" \
	@"KARIKEI.002" 

"C:\makishisys\hmobj\KARIKEI.LIB" : "KARIKEI.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\KARIKEI.dll" \
	@"KARIKEI.002" 


"C:\makishisys\yawobj\KARIKEI.dll" : "KARIKEI.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\KARIKEI.dll" \
	@"KARIKEI.003" 

"C:\makishisys\yawobj\KARIKEI.LIB" : "KARIKEI.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\KARIKEI.dll" \
	@"KARIKEI.003" 


