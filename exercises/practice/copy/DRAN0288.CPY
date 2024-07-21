      ****************************************************************
      *    COPY-MEMBER: DRAN0288  FUNKTION SELECT STANDARD-OZP       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN01211!09.06.15!2.1 ! NEUERSTELLUNG             ! HEIDER   *
      *****************************************************************
      * LOGBUCH:                                                      *
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     *
      * --------------------------------------------------------------*
      * ANTRAG-1246!02.11.21! PARKDEPOT (C+L WG. DRAN0097) ! RAHNER   *
      * ANTRAG-1401!03.02.22! WIEDERHOLUNG ÜBERGABE AN-1246! RAHNER  *
      *            !        !                              !          *
      *            !        !                              !          *
      *            !        !                              !          *
      *            !        !                              !          *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0288                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT MIT PIN <<<  TABELLE DB TBAN0022              *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0288.
           05 PI-DRAN0288-DB-KEY.
               10 PI-DRAN0288-PGEB-GES        PIC S9(4) COMP.
               10 PI-DRAN0288-DB-PIN          PIC S9(9) COMP.
      *
       01  PO-DRAN0288.
           05 PO-DRAN0288-RC                   PIC  X(003).
             88 PO-DRAN0288-RC-OK                   VALUE 'OK '.
             88 PO-DRAN0288-RC-NOT-FOUND            VALUE 'NFD'.
             88 PO-DRAN0288-RC-KEY-NOT-UNIQUE       VALUE 'KNU'.
      *
      *   DB-DATEN AUS PC-DATEN
           COPY DRAN0289.
      *
      ****** ENDE COPY-MEMBER ****************************************
 


