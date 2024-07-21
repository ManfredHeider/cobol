      ****************************************************************
      *    COPY-MEMBER: DRAN0093  FUNKTION SELECT ÜBER GES, VN, AVERM*
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00455!06.08.04!2.1 ! NEUERSTELLUNG             ! RAHNER   *
      * AAN00730!30.11.07!2.2  !ALLFINANZ                  !RAHNER   *
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
      *    FÜR DIE FUNKTION DRAN0093                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT  <<<       TABELLE TBAN0006 DWS               *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0093.
           05 PI-DRAN0093-DWS-KEY.
               10 PI-DRAN0093-PGEB-GES         PIC S9(4) COMP.
               10 PI-DRAN0093-DWS-HVANTNUM     PIC X(9).
               10 PI-DRAN0093-DWS-VDABVERM     PIC S9(9) COMP.
      *
       01  PO-DRAN0093.
           05 PO-DRAN0093-RC                   PIC  X(003).
             88 PO-DRAN0093-RC-OK                   VALUE 'OK '.
             88 PO-DRAN0093-RC-NOT-FOUND            VALUE 'NFD'.
             88 PO-DRAN0093-RC-KEY-NOT-UNIQUE       VALUE 'KNU'.
      *
           05 PO-DRAN0093-DWS-DATEN.
               10 PO-DRAN0093-PIN              PIC S9(9) COMP.
               10 PO-DRAN0093-DWS-LAST-UPDATE  PIC X(26).
               10 PO-DRAN0093-DWS-KENNUNG      PIC X(8).
      *    DRAN0093-DATEN AUS PC-DATEN
           COPY DRAN0097.
      ****** ENDE COPY-MEMBER ****************************************

