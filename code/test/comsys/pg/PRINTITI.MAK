################################################################################
#                               NetCOBOL V7.2L10
################################################################################

.SUFFIXES:

COBOL_PATH = C:\Program Files\NetCOBOL
PROJECT_PATH = P:\COMSYS\PG
OBJECT_PATH = 
LINK_DEBUG_OPTION = 
LINK_MAP_OPTION = 


ALL : "obj\printiti.dll" "C:\makishisys\hmobj\printiti.dll" "C:\makishisys\yawobj\printiti.dll" 

REBUILD : CLEAN ALL
CLEAN :
	DEL "obj\printiti.dll" 
	DEL "obj\printiti.LIB" 
	DEL "printiti.OBJ" 
	DEL "C:\makishisys\hmobj\printiti.dll" 
	DEL "C:\makishisys\hmobj\printiti.LIB" 
	DEL "C:\makishisys\yawobj\printiti.dll" 
	DEL "C:\makishisys\yawobj\printiti.LIB" 


"printiti.OBJ" : "printiti.cob" "PRINTITI.CBI"
	COBOL32.EXE -i"PRINTITI.CBI" -NM "printiti.cob"

"obj\printiti.dll" : "printiti.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\printiti.dll" \
	@"PRINTITI.001" 

"obj\printiti.LIB" : "printiti.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\printiti.dll" \
	@"PRINTITI.001" 


"C:\makishisys\hmobj\printiti.dll" : "printiti.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\printiti.dll" \
	@"PRINTITI.002" 

"C:\makishisys\hmobj\printiti.LIB" : "printiti.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\printiti.dll" \
	@"PRINTITI.002" 


"C:\makishisys\yawobj\printiti.dll" : "printiti.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\printiti.dll" \
	@"PRINTITI.003" 

"C:\makishisys\yawobj\printiti.LIB" : "printiti.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\printiti.dll" \
	@"PRINTITI.003" 


