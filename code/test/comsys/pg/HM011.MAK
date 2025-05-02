################################################################################
#                               NetCOBOL V7.2L10
################################################################################

.SUFFIXES:

COBOL_PATH = C:\Program Files\NetCOBOL
PROJECT_PATH = P:\COMSYS\PG
OBJECT_PATH = 
LINK_DEBUG_OPTION = 
LINK_MAP_OPTION = 


ALL : "obj\hm011.dll" "C:\makishisys\hmobj\hm011.dll" "C:\makishisys\yawobj\hm011.dll" 

REBUILD : CLEAN ALL
CLEAN :
	DEL "obj\hm011.dll" 
	DEL "obj\hm011.LIB" 
	DEL "hm011.OBJ" 
	DEL "C:\makishisys\hmobj\hm011.dll" 
	DEL "C:\makishisys\hmobj\hm011.LIB" 
	DEL "C:\makishisys\yawobj\hm011.dll" 
	DEL "C:\makishisys\yawobj\hm011.LIB" 


"hm011.OBJ" : "hm011.cob" "HM011.CBI"
	COBOL32.EXE -i"HM011.CBI" -NM "hm011.cob"

"obj\hm011.dll" : "hm011.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\hm011.dll" \
	@"HM011.001" 

"obj\hm011.LIB" : "hm011.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\hm011.dll" \
	@"HM011.001" 


"C:\makishisys\hmobj\hm011.dll" : "hm011.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\hm011.dll" \
	@"HM011.002" 

"C:\makishisys\hmobj\hm011.LIB" : "hm011.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\hm011.dll" \
	@"HM011.002" 


"C:\makishisys\yawobj\hm011.dll" : "hm011.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\hm011.dll" \
	@"HM011.003" 

"C:\makishisys\yawobj\hm011.LIB" : "hm011.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\hm011.dll" \
	@"HM011.003" 


