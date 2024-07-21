      ****************************************************************
      *    COPY-MEMBER: DRAN0314                                     *
      *    FUNKTION   : ERM. ZUSÄTZLICHE INHABER ÜBER GES, VNR       *
      *                                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                  ! NAME    *
      * -------------------------------------------------------------*
      * AAN01354!20.03.18!2.1 !NEU                         !RAHNER   *
      *         !        !    !                            !         *
      * AENDERUNGEN:                                                *
      * AUFTRAG    ! DATUM  ! AENDERUNG                   ! NAME    *
      * ------------------------------------------------------------*
      * ANTRAG-1930!        ! NEU FELD DEVISENINLÄNDER    ! RAHNER  *
      *            !        !                             !         *
      *            !        !                             !         *
      *            !        !                             !         *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0314                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0314.
           05 PI-DRAN0314-INHA-KEY.
               10 PI-DRAN0314-PGEB-GES      PIC  S9(04) COMP.
               10 PI-DRAN0314-INHA-VNR      PIC  S9(15) COMP-3.
      *
       01 PO-DRAN0314.
          05 PO-DRAN0314-DATEN.
              10 PO-DRAN0314-VB-NR             PIC S9(009) COMP.
              10 PO-DRAN0314-VM-NR             PIC S9(009) COMP.
              10 PO-DRAN0314-INHABER OCCURS 5.
                 20 PO-DRAN0314-INH-ZUNAME     PIC  X(026).
                 20 PO-DRAN0314-INH-VORNAME    PIC  X(026).
                 20 PO-DRAN0314-INH-STAANG     PIC S9(004) COMP.
                 20 PO-DRAN0314-INH-PLZ        PIC  X(009).
                 20 PO-DRAN0314-INH-ORT        PIC  X(021).
                 20 PO-DRAN0314-INH-AUSLKZ     PIC S9(004) COMP.
                 20 PO-DRAN0314-INH-GEBDAT     PIC  X(010).
                 20 PO-DRAN0314-INH-PEP-STAT   PIC  X(001).
                 20 PO-DRAN0314-INH-DIL-STAT   PIC  X(001).
                 20 PO-DRAN0314-INH-BGRP       PIC S9(004) COMP.
                 20 PO-DRAN0314-INH-BRAN       PIC S9(004) COMP.
      *
          05 PO-DRAN0314-RC                 PIC  X(003).
             88 PO-DRAN0314-RC-OK                  VALUE 'OK '.
             88 PO-DRAN0314-RC-NOT-FOUND           VALUE 'NFD'.
             88 PO-DRAN0314-RC-NOK                 VALUE 'NOK'.
      *


