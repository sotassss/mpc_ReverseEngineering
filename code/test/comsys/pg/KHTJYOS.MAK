################################################################################
#                               NetCOBOL V7.2L10
################################################################################

.SUFFIXES:

COBOL_PATH = C:\Program Files\NetCOBOL
PROJECT_PATH = P:\COMSYS\PG
OBJECT_PATH = 
LINK_DEBUG_OPTION = 
LINK_MAP_OPTION = 


ALL : "obj\Khtjyos.dll" "C:\makishisys\hmobj\Khtjyos.dll" "C:\makishisys\yawobj\Khtjyos.dll" 

REBUILD : CLEAN ALL
CLEAN :
	DEL "obj\Khtjyos.dll" 
	DEL "obj\Khtjyos.LIB" 
	DEL "Khtjyos.OBJ" 
	DEL "C:\makishisys\hmobj\Khtjyos.dll" 
	DEL "C:\makishisys\hmobj\Khtjyos.LIB" 
	DEL "C:\makishisys\yawobj\Khtjyos.dll" 
	DEL "C:\makishisys\yawobj\Khtjyos.LIB" 


"Khtjyos.OBJ" : "Khtjyos.cob" "KHTJYOS.CBI"
	COBOL32.EXE -i"KHTJYOS.CBI" -NM "Khtjyos.cob"

"obj\Khtjyos.dll" : "Khtjyos.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\Khtjyos.dll" \
	@"KHTJYOS.001" 

"obj\Khtjyos.LIB" : "Khtjyos.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\Khtjyos.dll" \
	@"KHTJYOS.001" 


"C:\makishisys\hmobj\Khtjyos.dll" : "Khtjyos.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\Khtjyos.dll" \
	@"KHTJYOS.002" 

"C:\makishisys\hmobj\Khtjyos.LIB" : "Khtjyos.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\Khtjyos.dll" \
	@"KHTJYOS.002" 


"C:\makishisys\yawobj\Khtjyos.dll" : "Khtjyos.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\Khtjyos.dll" \
	@"KHTJYOS.003" 

"C:\makishisys\yawobj\Khtjyos.LIB" : "Khtjyos.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\Khtjyos.dll" \
	@"KHTJYOS.003" 


