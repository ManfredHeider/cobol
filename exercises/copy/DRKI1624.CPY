      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1624.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1624.CPv  $
      *
      *   Rev 2.1   Dec 10 2003 11:46:06   t13ex39
      *FKI00806 KORREKTUR DOPPELTER BUCHUNGEN
      *
      *   Rev 2.1   Jul 10 2003 13:30:00   t13ex39
      *AKI00327.UE005 DB-PRODUKTE
      *
      *   Rev 2.1   23 Mar 2001 12:01:06   T13EX39
      *FKI00466 DIT-BUCHUNG
      *
      *   Rev 2.0   22 Mar 2001 15:50:30   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI1624 FUNKTION:                           *
      *    -------------------------------------------------------   *
      *    ERSTELLT: WHO
      *    DATUM:    08.12.2003                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FKI00806!05.12.03! 01 !NEU                        !WHO       *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI1624                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    FUNKTION:                                                 *
      *              - ALLE GUTHABENBUCHUNGEN ZU EINER GESELLSCHAFT  *
      *                ERMITTELN                                     *
      *                                                              *
      ****************************************************************
       01 PI-DRKI1624.
           05  PI-DRKI1624-PROGRAMM             PIC  X(008).
           05  PI-DRKI1624-UFKT                 PIC  X(002).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT
           05  PI-DRKI1624-DATEN.
             10 PI-DRKI1624-GES-VNR-VON         PIC  X(013).
             10 PI-DRKI1624-GES-VNR-BIS         PIC  X(013).
             10 PI-DRKI1624-LTZ-VON             PIC  X(026).
             10 PI-DRKI1624-LTZ-BIS             PIC  X(026).

      ***
       01 PO-DRKI1624.
          05 PO-DRKI1624-RC                     PIC  X(003).
             88 PO-DRKI1624-RC-OK                        VALUE 'OK '.
             88 PO-DRKI1624-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI1624-RC-EOF                       VALUE 'EOF'.
          05 PO-DRKI1624-KEY.
             10 PO-DRKI1624-GHBBUCHUNGLFDNR     PIC S9(4) COMP.
             10 PO-DRKI1624-GHBFONDID           PIC S9(4) COMP.
             10 PO-DRKI1624-GHBGHALFDNR         PIC S9(4) COMP.
             10 PO-DRKI1624-GHBGUTHABPRODLFDNR  PIC S9(4) COMP.
             10 PO-DRKI1624-GHBVERTRAGSID       PIC X(13).

      *
      ******** Ende Copy-Member **************************************
