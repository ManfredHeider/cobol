      *****************************************************************
      *    COPY-MEMBER: DRKI3986                                      * 
      *    --------------------------------------------------------   *
      *    Cursor Full SELECT ZXKI0488 G79_KUNDEVERTRAG bei Man-      *
      *    dantenwechsel                                              *
      *    --------------------------------------------------------   *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !      AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *DBO-10790!19.01.23!Neuerstellung                   !u131774   *
      *         !        !                                !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3986
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION: LIST
      *
      ****************************************************************
      *
       01  PI-DRKI3986.
           05 PI-DRKI3986-UFKT-CODE            PIC  9(002).
              88 PI-DRKI3986-UFKT-FIRST             VALUE 1.
              88 PI-DRKI3986-UFKT-NEXT              VALUE 2.
              88 PI-DRKI3986-UFKT-QUIT              VALUE 3.
 
           05  PI-DRKI3986-KEY.
              10 PI-DRKI3986-G79MANDANT        PIC S9(9) COMP.
              10 PI-DRKI3986-G79VERTRAGSNR     PIC  X(0015).   
      *
       01  PO-DRKI3986.
           05 PO-DRKI3986-RC                   PIC  X(003).
              88 PO-DRKI3986-RC-OK                  VALUE 'OK '.
              88 PO-DRKI3986-RC-LEER                VALUE 'LER'.
              88 PO-DRKI3986-RC-EOF                 VALUE 'EOF'.
      *
           05 PO-DRKI3986-KEY.
              10 PO-DRKI3986-G79VERTRAGSNR         PIC  X(0015).        
              10 PO-DRKI3986-G79SPARRATEID         PIC S9(9) COMP.      
           05 PO-DRKI3986-DATEN.
              10 PO-DRKI3986-G79MANDANT            PIC S9(9) COMP.      
              
      ******* ENDE COPY-MEMBER ***************************************