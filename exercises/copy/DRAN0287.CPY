      ****************************************************************
      *    COPY-MEMBER: DRAN0287  FUNKTION SELECT STANDARD-OZP       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN01211!09.06.15!2.1 ! NEUERSTELLUNG             ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0287                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT MIT VORSCHLAG <<<  TABELLE DB TBAN0022        *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0287.
           05 PI-DRAN0287-DB-KEY.
               10 PI-DRAN0287-PGEB-GES        PIC S9(4) COMP.
               10 PI-DRAN0287-DB-HVANTNUM     PIC X(9).
      *
       01  PO-DRAN0287.
           05 PO-DRAN0287-RC                   PIC  X(003).
             88 PO-DRAN0287-RC-OK                   VALUE 'OK '.
             88 PO-DRAN0287-RC-NOT-FOUND            VALUE 'NFD'.
             88 PO-DRAN0287-RC-KEY-NOT-UNIQUE       VALUE 'KNU'.
      *
      *   DB-DATEN AUS PC-DATEN
           COPY DRAN0289.
      *
      ****** ENDE COPY-MEMBER ****************************************
 


