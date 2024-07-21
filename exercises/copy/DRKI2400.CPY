      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.05.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2400.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2400.CPv  $
      *
      *   Rev 2.1   Sep 28 2010 11:20:24   t13ex27
      *AKI00966: Datenbestandsverarbeitung  DVAG Österreich-Abgleic
      *h-DB-Änderung und OZP
      *
      *   Rev 2.0   May 04 2010 12:14:08   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI2400  FUNKTIONEN LIST-OZP                *
      *                                                              *
      *                                                              *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FKI00911!02.05.08!2.1 ! NEUERSTELLUNG             !I. Frenzel*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01 PI-DRKI2400.
          05 PI-DRKI2400-PROGRAMM               PIC X(008).
          05 PI-DRKI2400-UFKT                   PIC 9(002).

       01 PO-DRKI2400.
          05 PO-DRKI2400-DATEN.
           10 PO-DRKI2400-ASTTABELLE           PIC X(30).
           10 PO-DRKI2400-ASTATTRIBUT          PIC X(30).
           10 PO-DRKI2400-ASTPRODUKTID         PIC S9(4) USAGE COMP.
           10 PO-DRKI2400-ASTPARTGESELLSCH     PIC S9(4) USAGE COMP.
           10 PO-DRKI2400-ASTGESCHAEFTSZW      PIC X(2).
           10 PO-DRKI2400-ASTVEZSPARTE         PIC S9(4) USAGE COMP.
           10 PO-DRKI2400-ASTOKISPARTE         PIC X(2).
           10 PO-DRKI2400-ASTSTEUERSATZTYP     PIC X(3).
           10 PO-DRKI2400-ASTALLEPRODUKTID     PIC X(1).

          05 PO-DRKI2400-RC                       PIC  X(003).
             88 PO-DRKI2400-RC-OK                        VALUE 'OK '.
             88 PO-DRKI2400-RC-EOF                       VALUE 'EOF'.
             88 PO-DRKI2400-RC-LEER                      VALUE 'LER'.
