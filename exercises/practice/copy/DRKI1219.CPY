      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1219.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI1219.CPv  $
      *
      *   Rev 2.1   28 Mar 2001 08:09:08   T13EX39
      *FKI00467 DIT-BUCHUNG
      *
      *   Rev 2.0   22 Mar 2001 15:50:34   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1219                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1219                    *
      *    --------------------------------------------------------   *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FKI00467!27.03.01! 1.0! NEU                       ! WHO      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI1219
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1219.
             10 PI-DRKI1219-GHBFONDID   PIC S9(4) COMP.
             10 PI-DRKI1219-GHBVERTRAGSID   PIC X(13).

             10 PI-DRKI1219-GHBBETRAGLETZTBUCH   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1219-GHBBUCHUNGSDATUM     PIC X(10).
             10 PI-DRKI1219-GHBGUTHABENANT       PIC S9(6)V9(3) COMP-3.
             10 PI-DRKI1219-GHBKZFINHLETZTBUID   PIC S9(4) COMP.
             10 PI-DRKI1219-GHBSTEUBETRLETZTBU   PIC S9(9)V9(2) COMP-3.

      *
       01  PO-DRKI1219.
           05  PO-DRKI1219-RC                 PIC  X(003).
               88 PO-DRKI1219-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1219-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI1219-DATEN.

             10 PO-DRKI1219-GHBBUCHUNGLFDNR   PIC S9(4) COMP.
             10 PO-DRKI1219-GHBFONDID   PIC S9(4) COMP.
             10 PO-DRKI1219-GHBGHALFDNR   PIC S9(4) COMP.
             10 PO-DRKI1219-GHBGUTHABPRODLFDNR   PIC S9(4) COMP.
             10 PO-DRKI1219-GHBVERTRAGSID   PIC X(13).

             10 PO-DRKI1219-GHBBETRAGLETZTBUCH   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1219-GHBBETRLBUWKZ   PIC X(01).
             10 PO-DRKI1219-GHBBUCHUNGSDATUM   PIC X(10).
             10 PO-DRKI1219-GHBGUTHABENANT   PIC S9(6)V9(3) COMP-3.
             10 PO-DRKI1219-GHBKZFINHLETZTBUID   PIC S9(4) COMP.
             10 PO-DRKI1219-GHBSTEUBETRLETZTBU   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1219-GHBSTLETZTBUWKZ   PIC X(01).

