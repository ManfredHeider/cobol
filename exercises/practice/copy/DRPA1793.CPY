      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.03.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1793.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1793.CPv  $
      *
      *   Rev 2.1   Mar 21 2014 09:13:52   t13ae37
      *APA01766: GESAMTABRECHNUNGSPROTOKOLL DAISY: zus. Kontrolliste
      *
      *   Rev 2.0   Mar 19 2014 10:32:36   t13ae37
      *Archiverstellung
      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA01766!19.03.14! 2.1! NEUERSTELLUNG             ! BUCH     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1793                                 *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1793.
          05 PI-DRPA1793-PROGRAMM              PIC  X(008).
          05 PI-DRPA1793-UFKT                  PIC  9(002).
      *
       01 PO-DRPA1793.
          05 PO-DRPA1793-RC                    PIC  X(003).
             88 PO-DRPA1793-RC-OK                   VALUE 'OK '.
             88 PO-DRPA1793-RC-EOF                  VALUE 'EOF'.
             88 PO-DRPA1793-RC-LEER                 VALUE 'LER'.
          05 PO-DRPA1793-DATEN.
             10 PO-DRPA1793-KNR-VGES           PIC S9(004) BINARY.
             10 PO-DRPA1793-KOSU-PROVISION     PIC S9(9)V9(2) COMP-3.
             10 PO-DRPA1793-KOSU-AUSZAHLUNG    PIC S9(9)V9(2) COMP-3.
             10 PO-DRPA1793-KOSU-RUECKSTELLUNG PIC S9(9)V9(2) COMP-3.
      *

      *---------------------------------------------------------------*
