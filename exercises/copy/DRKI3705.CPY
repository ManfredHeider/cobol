      *****************************************************************
      *    COPY-MEMBER: DRKI3705                                      *
      *    --------------------------------------------------------   *
      *    Cursor Full SELECT ZXKI0499 G79_DIREKTKAUF                 *
      *    --------------------------------------------------------   *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !AENDERUNG                       ! NAME     *
      * -------------------------------------------------------------*
      * AKI01692!05.08.19!Neuerstellung                   ! tfl      *
      * -------------------------------------------------------------*
      * DBO10172!13.12.22!Anpassung, weg. Silberdirektkauf! u131774  *  
      *         !        !                                !          *
      *         !        !                                !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3705
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION: LIST
      *
      ****************************************************************
      *
       01  PI-DRKI3705.
           05 PI-DRKI3705-UFKT-CODE            PIC  9(002).
              88 PI-DRKI3705-UFKT-FIRST        VALUE 1.
              88 PI-DRKI3705-UFKT-NEXT         VALUE 2.
              88 PI-DRKI3705-UFKT-QUIT         VALUE 3.
 
           05  PI-DRKI3705-KEY.
              10 PI-DRKI3705-G79DMANDANT        PIC S9(9) COMP.
              10 PI-DRKI3705-G79DGESID          PIC S9(4) COMP. 
 
      *
       01  PO-DRKI3705.
           05 PO-DRKI3705-RC                   PIC  X(003).
              88 PO-DRKI3705-RC-OK             VALUE 'OK '.
              88 PO-DRKI3705-RC-LEER           VALUE 'LER'.
              88 PO-DRKI3705-RC-EOF            VALUE 'EOF'.
      *
           05 PO-DRKI3705-DATEN.
              10 PO-DRKI3705-G79DVERTRAGSNR    PIC X(15).
              10 PO-DRKI3705-G79DMANDANT       PIC S9(9) USAGE COMP.
              10 PO-DRKI3705-G79DABSCHLVBNR    PIC S9(9) USAGE COMP.
              10 PO-DRKI3705-G79DNACHNAME      PIC X(30).
              10 PO-DRKI3705-G79DVORNAME       PIC X(30).
              10 PO-DRKI3705-G79DFIRMENNAME    PIC X(70).
              10 PO-DRKI3705-G79DGEBDAT        PIC X(8).
              10 PO-DRKI3705-G79DANREDE        PIC X(1).
              10 PO-DRKI3705-G79DSTRHNR        PIC X(40).
              10 PO-DRKI3705-G79DPLZ           PIC X(9).
              10 PO-DRKI3705-G79DORT           PIC X(30).
              10 PO-DRKI3705-G79DLAND          PIC X(3).
              10 PO-DRKI3705-G79DEMAIL         PIC X(40).
              10 PO-DRKI3705-G79DGESID         PIC S9(4) COMP.                  
              10 PO-DRKI3705-G79DKUNDENID      PIC S9(9) COMP. 
