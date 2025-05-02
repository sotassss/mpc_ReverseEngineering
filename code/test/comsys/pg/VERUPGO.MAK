################################################################################
#                               NetCOBOL V7.2L10
################################################################################

.SUFFIXES:

COBOL_PATH = C:\Program Files\NetCOBOL
PROJECT_PATH = P:\COMSYS\PG
OBJECT_PATH = 
LINK_DEBUG_OPTION = 
LINK_MAP_OPTION = 


ALL : "VERUPGO.EXE" 

REBUILD : CLEAN ALL
CLEAN :
	DEL "VERUPGO.EXE" 
	DEL "VERUPGO.OBJ" 


"VERUPGO.OBJ" : "VERUPGO.COB" "VERUPGO.CBI"
	COBOL32.EXE -i"VERUPGO.CBI" -M "VERUPGO.COB"

"VERUPGO.EXE" : "VERUPGO.OBJ" "C:\Program Files\NetCOBOL\uuid.Lib" 
	"$(COBOL_PATH)\LINK.EXE" $(LINK_DEBUG_OPTION) $(LINK_MAP_OPTION) \
	/OUT:"VERUPGO.EXE" \
	@"VERUPGO.001" 


