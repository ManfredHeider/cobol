      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.03.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1792.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1792.CPv  $
      *
      *   Rev 2.1   Mar 21 2014 09:13:44   t13ae37
      *APA01766: GESAMTABRECHNUNGSPROTOKOLL DAISY: zus. Kontrolliste
      *
      *   Rev 2.0   Mar 19 2014 10:01:42   t13ae37
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
      *    FÜR DIE FUNKTION DRPA1792                                 *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1792.
          05 PI-DRPA1792-PROGRAMM              PIC  X(008).
          05 PI-DRPA1792-UFKT                  PIC  9(002).
          05 PI-DRPA1792-DATEN.
             10 PI-DRPA1792-KNR-MANDANT        PIC S9(004) BINARY.
      *
       01 PO-DRPA1792.
          05 PO-DRPA1792-RC                    PIC  X(003).
             88 PO-DRPA1792-RC-OK                   VALUE 'OK '.
             88 PO-DRPA1792-RC-EOF                  VALUE 'EOF'.
             88 PO-DRPA1792-RC-LEER                 VALUE 'LER'.
          05 PO-DRPA1792-DATEN.
             10 PO-DRPA1792-KNR-MANDANT        PIC S9(004) BINARY.
             10 PO-DRPA1792-PGEB-GES           PIC S9(004) BINARY.
             10 PO-DRPA1792-KOSU-PROVISION     PIC S9(9)V9(2) COMP-3.
             10 PO-DRPA1792-KOSU-AUSZAHLUNG    PIC S9(9)V9(2) COMP-3.
             10 PO-DRPA1792-KOSU-RUECKSTELLUNG PIC S9(9)V9(2) COMP-3.
      *
