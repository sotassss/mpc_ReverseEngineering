################################################################################
#                               NetCOBOL V7.2L10
################################################################################

.SUFFIXES:

COBOL_PATH = C:\Program Files\NetCOBOL
PROJECT_PATH = P:\COMSYS\PG
OBJECT_PATH = 
LINK_DEBUG_OPTION = 
LINK_MAP_OPTION = 


ALL : "obj\CRTPRTF.dll" "C:\makishisys\hmobj\CRTPRTF.dll" "C:\makishisys\yawobj\CRTPRTF.dll" 

REBUILD : CLEAN ALL
CLEAN :
	DEL "obj\CRTPRTF.dll" 
	DEL "obj\CRTPRTF.LIB" 
	DEL "CRTPRTF.OBJ" 
	DEL "C:\makishisys\hmobj\CRTPRTF.dll" 
	DEL "C:\makishisys\hmobj\CRTPRTF.LIB" 
	DEL "C:\makishisys\yawobj\CRTPRTF.dll" 
	DEL "C:\makishisys\yawobj\CRTPRTF.LIB" 


"CRTPRTF.OBJ" : "CRTPRTF.cob" "CRTPRTF.CBI"
	COBOL32.EXE -i"CRTPRTF.CBI" -NM "CRTPRTF.cob"

"obj\CRTPRTF.dll" : "CRTPRTF.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\CRTPRTF.dll" \
	@"CRTPRTF.001" 

"obj\CRTPRTF.LIB" : "CRTPRTF.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\CRTPRTF.dll" \
	@"CRTPRTF.001" 


"C:\makishisys\hmobj\CRTPRTF.dll" : "CRTPRTF.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\CRTPRTF.dll" \
	@"CRTPRTF.002" 

"C:\makishisys\hmobj\CRTPRTF.LIB" : "CRTPRTF.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\CRTPRTF.dll" \
	@"CRTPRTF.002" 


"C:\makishisys\yawobj\CRTPRTF.dll" : "CRTPRTF.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\CRTPRTF.dll" \
	@"CRTPRTF.003" 

"C:\makishisys\yawobj\CRTPRTF.LIB" : "CRTPRTF.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\CRTPRTF.dll" \
	@"CRTPRTF.003" 


