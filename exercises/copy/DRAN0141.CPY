      ****************************************************************
      *    COPY-MEMBER: DRAN0141 WORKFLOW ANTRAG KOPFDATEN           *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00655!21.09.06!2.1 ! NEUERSTELLUNG             ! HEIDER   *
      * AAN00655!14.11.06!2.4 ! ZUSÄTZLICHE ATTRIBUTE     ! HEIDER   *
      * AAN01033!14.12.11!2.5 ! DATEN GELDWÄSCHE          ! RAHNER   *
      * AAN01045!05.03.12!2.6 ! ZEICHNER KONZESSION       ! RAHNER   *
      * FAN01127!07.03.18!2.7 ! ERSTELLZEITPUNKT          ! HEIDER   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      *****************************************************************
      * LOGBUCH:
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME
      * --------------------------------------------------------------*
      * ANTRAG-1928!14.02.23! NEU ANTA-KUNDE-DIL           ! RAHNER
      *            !        !                              !
      *            !        !                              !
      *            !        !                              !
      *
      * ENDE-LOGBUCH
      *****************************************************************
      ****************************************************************
      *                                                              *
      *    DIESES COPY IST EIN WORKFLOW DER ANTRAG-DATEN             *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
             30 ANTA-ANTRAG-KOPF-DATEN.
                35 PGEB-GES                       PIC S9(4) COMP.
                35 ANTA-NR                        PIC S9(15)V COMP-3.
                35 ANTA-PIN                       PIC S9(9) COMP.
                35 ANTA-PIN-PZ                    PIC X(2).
                35 ANTA-VORSCHLAG-NR              PIC X(17).
                35 ANTA-AVERM                     PIC S9(9) COMP.
                35 ANTA-AVERM-PZ                  PIC X.
                35 ANTA-VM                        PIC S9(9) COMP.
                35 ANTA-VM-PZ                     PIC X.
                35 ANTA-DIR                       PIC S9(4) COMP.
                35 ANTA-KUNDE-NAME                PIC X(26).
                35 ANTA-MC-KUNDE-NAME             PIC X(8).
                35 ANTA-KUNDE-VNAME               PIC X(26).
                35 ANTA-PLZ                       PIC X(9).
                35 ANTA-ORT                       PIC X(21).
                35 ANTA-AUSL                      PIC S9(4) COMP.
                35 ANTA-ANTRAGS-ART               PIC S9(4) COMP.
                35 ANTA-AN-AUFN-DAT               PIC X(10).
                35 ANTA-STRUK-DAT                 PIC X(10).
                35 ANTA-STATISTIK-DAT             PIC X(10).
                35 ANTA-ANTR-STATUS-1             PIC S9(4) COMP.
                35 ANTA-STATUS-DAT-1              PIC X(10).
                35 ANTA-ANTR-STATUS-2             PIC S9(4) COMP.
                35 ANTA-STATUS-DAT-2              PIC X(10).
                35 ANTA-ANTR-STATUS-3             PIC S9(4) COMP.
                35 ANTA-STATUS-DAT-3              PIC X(10).
                35 ANTA-STATISTIK                 PIC X.
                35 ANTA-ERST-ERFASSER             PIC X(8).
                35 ANTA-KD-BEST-ID                PIC S9(18)V COMP-3.
                35 ANTA-BERATER-EXEM              PIC X.
                35 ANTA-VERS-BEGINN               PIC X(10).
                35 ANTA-VERS-ENDE                 PIC X(10).
                35 ANTA-EIGEN-ANTRAG              PIC X.
                35 ANTA-GEB-DAT                   PIC X(10).
                35 ANTA-BZ-ENDE                   PIC X(10).
                35 ANTA-RBA                       PIC S9(4) COMP.
                35 ANTA-KOLLEKTSTUFE              PIC S9(4) COMP.
                35 ANTA-DIREKTVERS                PIC X.
                35 ANTA-DTA-STRUK-DAT             PIC X(10).
                35 ANTA-HISTORIE                  PIC S9(4) COMP.
                35 ANTA-AG8-AENDERUNG             PIC S9(4) COMP.
                35 ANTA-ZUSATZ-1                  PIC X(10).
                35 ANTA-KUNDE-STAA                PIC S9(4) COMP.
                35 ANTA-KUNDE-BGRP                PIC X(40).
                35 ANTA-KUNDE-BRAN                PIC X(40).
                35 ANTA-KUNDE-PEP                 PIC X(1).
                35 ANTA-VB-KONZESSION             PIC S9(9) COMP.
                35 ANTA-ERSTELLZEITPUNKT          PIC X(26).
                35 ANTA-FILLER                    PIC X(274).


