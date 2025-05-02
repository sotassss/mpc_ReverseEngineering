################################################################################
#                               NetCOBOL V7.2L10
################################################################################

.SUFFIXES:

COBOL_PATH = C:\Program Files\NetCOBOL
PROJECT_PATH = P:\COMSYS\PG
OBJECT_PATH = 
LINK_DEBUG_OPTION = 
LINK_MAP_OPTION = 


ALL : "obj\hm012.dll" "C:\makishisys\hmobj\hm012.dll" "C:\makishisys\yawobj\hm012.dll" 

REBUILD : CLEAN ALL
CLEAN :
	DEL "obj\hm012.dll" 
	DEL "obj\hm012.LIB" 
	DEL "hm012.OBJ" 
	DEL "C:\makishisys\hmobj\hm012.dll" 
	DEL "C:\makishisys\hmobj\hm012.LIB" 
	DEL "C:\makishisys\yawobj\hm012.dll" 
	DEL "C:\makishisys\yawobj\hm012.LIB" 


"hm012.OBJ" : "hm012.cob" "HM012.CBI"
	COBOL32.EXE -i"HM012.CBI" -NM "hm012.cob"

"obj\hm012.dll" : "hm012.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\hm012.dll" \
	@"HM012.001" 

"obj\hm012.LIB" : "hm012.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\hm012.dll" \
	@"HM012.001" 


"C:\makishisys\hmobj\hm012.dll" : "hm012.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\hm012.dll" \
	@"HM012.002" 

"C:\makishisys\hmobj\hm012.LIB" : "hm012.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\hm012.dll" \
	@"HM012.002" 


"C:\makishisys\yawobj\hm012.dll" : "hm012.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\hm012.dll" \
	@"HM012.003" 

"C:\makishisys\yawobj\hm012.LIB" : "hm012.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\hm012.dll" \
	@"HM012.003" 


