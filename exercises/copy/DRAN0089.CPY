      ****************************************************************
      *    COPY-MEMBER: DRAN0089  FUNKTION SELECT STANDARD-OZP       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00455!06.08.04!2.1 ! NEUERSTELLUNG             ! RAHNER   *
      * AAN00684!31.08.06!2.2 ! KUNDENBESTANDS-ID FÜR AVS ! RAHNER   *
      * AAN00730!30.11.07!2.3 ! ALLFINANZ                 ! RAHNER   *
      *****************************************************************
      * LOGBUCH:                                                      *
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     *
      * --------------------------------------------------------------*
      * ANTRAG-1246!02.11.21! PARKDEPOT (C+L WG. DRAN0097) ! RAHNER   *
      *            !        !                              !          *
      *            !        !                              !          *
      *            !        !                              !          *
      *            !        !                              !          *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0089                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT  <<<       TABELLE DWS      TBAN0006          *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0089.
           05 PI-DRAN0089-DWS-KEY.
               10 PI-DRAN0089-PGEB-GES       PIC S9(4) COMP.
               10 PI-DRAN0089-DWS-PIN        PIC S9(9) COMP.
      *
       01  PO-DRAN0089.
           05 PO-DRAN0089-RC                   PIC  X(003).
             88 PO-DRAN0089-RC-OK                   VALUE 'OK '.
             88 PO-DRAN0089-RC-NOT-FOUND            VALUE 'NFD'.
             88 PO-DRAN0089-RC-KEY-NOT-UNIQUE       VALUE 'KNU'.
      *
           05 PO-DRAN0089-DWS-DATEN.
               10 PO-DRAN0089-DWS-PIN         PIC S9(9) COMP.
               10 PO-DRAN0089-DWS-LAST-UPDATE PIC X(26).
               10 PO-DRAN0089-DWS-KENNUNG     PIC X(8).

      *
      *    DWS-DATEN AUS PC-DATEN
           COPY DRAN0097.
      ****** ENDE COPY-MEMBER ****************************************

