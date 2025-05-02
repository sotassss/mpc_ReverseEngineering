################################################################################
#                               NetCOBOL V7.2L10
################################################################################

.SUFFIXES:

COBOL_PATH = C:\Program Files\NetCOBOL
PROJECT_PATH = \\HOST\MAKISHI\COMSYS\PG
OBJECT_PATH = 
LINK_DEBUG_OPTION = 
LINK_MAP_OPTION = 


ALL : "obj\HIY0210R.dll" "C:\makishisys\hmobj\HIY0210R.dll" "C:\makishisys\yawobj\HIY0210R.dll" 

REBUILD : CLEAN ALL
CLEAN :
	DEL "obj\HIY0210R.dll" 
	DEL "obj\HIY0210R.LIB" 
	DEL "HIY0210R.OBJ" 
	DEL "C:\makishisys\hmobj\HIY0210R.dll" 
	DEL "C:\makishisys\hmobj\HIY0210R.LIB" 
	DEL "C:\makishisys\yawobj\HIY0210R.dll" 
	DEL "C:\makishisys\yawobj\HIY0210R.LIB" 


"HIY0210R.OBJ" : "HIY0210R.cob" "HIY0210R.CBI"
	COBOL32.EXE -i"HIY0210R.CBI" -NM "HIY0210R.cob"

"obj\HIY0210R.dll" : "HIY0210R.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\HIY0210R.dll" \
	@"HIY0210R.001" 

"obj\HIY0210R.LIB" : "HIY0210R.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"obj\HIY0210R.dll" \
	@"HIY0210R.001" 


"C:\makishisys\hmobj\HIY0210R.dll" : "HIY0210R.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\HIY0210R.dll" \
	@"HIY0210R.002" 

"C:\makishisys\hmobj\HIY0210R.LIB" : "HIY0210R.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\hmobj\HIY0210R.dll" \
	@"HIY0210R.002" 


"C:\makishisys\yawobj\HIY0210R.dll" : "HIY0210R.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\HIY0210R.dll" \
	@"HIY0210R.003" 

"C:\makishisys\yawobj\HIY0210R.LIB" : "HIY0210R.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/ENTRY:COBDMAIN /DLL \
	/OUT:"C:\makishisys\yawobj\HIY0210R.dll" \
	@"HIY0210R.003" 


