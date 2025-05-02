################################################################################
#                               NetCOBOL V10.0.0
################################################################################

.SUFFIXES:

COBOL_PATH = C:\Program Files\NetCOBOL
PROJECT_PATH = \\HOST\MAKISHI\COMSYS\PG
OBJECT_PATH = 
LINK_DEBUG_OPTION = 
LINK_MAP_OPTION = 


ALL : "obj\HMGetuji.dll" "C:\makishisys\hmobj\HMGetuji.dll" "C:\makishisys\yawobj\HMGetuji.dll" 

REBUILD : CLEAN ALL
CLEAN :
	DEL "obj\HMGetuji.dll" 
	DEL "obj\HMGetuji.LIB" 
	DEL "HMGetuji.OBJ" 
	DEL "C:\makishisys\hmobj\HMGetuji.dll" 
	DEL "C:\makishisys\hmobj\HMGetuji.LIB" 
	DEL "C:\makishisys\yawobj\HMGetuji.dll" 
	DEL "C:\makishisys\yawobj\HMGetuji.LIB" 


"HMGetuji.OBJ" : "HMGetuji.cob" "HMGETUJI.CBI"
	COBOL32.EXE -i"HMGETUJI.CBI" -NM "HMGetuji.cob"

"obj\HMGetuji.dll" : "HMGetuji.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\HMGetuji.dll" \
	@"HMGETUJI.001" 

"obj\HMGetuji.LIB" : "HMGetuji.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\HMGetuji.dll" \
	@"HMGETUJI.001" 


"C:\makishisys\hmobj\HMGetuji.dll" : "HMGetuji.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\HMGetuji.dll" \
	@"HMGETUJI.002" 

"C:\makishisys\hmobj\HMGetuji.LIB" : "HMGetuji.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\HMGetuji.dll" \
	@"HMGETUJI.002" 


"C:\makishisys\yawobj\HMGetuji.dll" : "HMGetuji.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\HMGetuji.dll" \
	@"HMGETUJI.003" 

"C:\makishisys\yawobj\HMGetuji.LIB" : "HMGetuji.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\HMGetuji.dll" \
	@"HMGETUJI.003" 


