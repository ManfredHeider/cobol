      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.09.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2429.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2429.CPv  $
      *
      *   Rev 2.2   Sep 18 2012 13:57:46   t13ex27
      *AKI00886: Host - neues Design Kundenaktualisierung-OZP Kunde
      *
      *   Rev 2.1   Nov 24 2010 10:49:20   t13ex27
      *AKI00966: Datenbestandsverarbeitung  DVAG Österreich Ticket-
      *DTA Teil 2
      *
      *   Rev 2.0   Sep 22 2010 14:27:44   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI2429 FUNKTION:Select auf älteste VRB     *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00966!20.09.10! 01 !NEU                        !I. Frenzel*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI2429                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    FUNKTION: Select auf alle Verträge einer Person           *
      *                                                              *
      ****************************************************************

       01 PI-DRKI2429.

           05  PI-DRKI2429-UFKT                 PIC  X(002).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT
           05  PI-DRKI2429-DATEN.
             10  PI-DRKI2429-VRBPERSONENID        PIC  X(013).

       01 PO-DRKI2429.
          05 PO-DRKI2429-RC                     PIC  X(003).
             88 PO-DRKI2429-RC-OK                        VALUE 'OK '.
             88 PO-DRKI2429-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI2429-RC-EOF                       VALUE 'EOF'.
          05 PO-DRKI2429-DATEN.
             10 PO-DRKI2429-VERPARTGESELLSCHID  PIC S9(4) USAGE COMP.
             10 PO-DRKI2429-VERVERTRAGSNUMMER   PIC X(15).
             10 PO-DRKI2429-LANDISOKZ           PIC X(2).
             10 PO-DRKI2429-ADRESSEPLZ          PIC X(15).
             10 PO-DRKI2429-ADRESSEORT          PIC X(30).
             10 PO-DRKI2429-ADRESSESTRHAUSNUM   PIC X(45).
             10 PO-DRKI2429-KNDANZVORNAME       PIC X(30).
             10 PO-DRKI2429-KNDANZNAME          PIC X(30).
             10 PO-DRKI2429-KNDGEBGRUENDDAT     PIC X(10).
             10 PO-DRKI2429-KNDANREDEID         PIC S9(4) COMP.
             10 PO-DRKI2429-KNDSUCHVORNAME      PIC X(30).
             10 PO-DRKI2429-KNDSUCHNAME         PIC X(30).
      *
      ******** Ende Copy-Member **************************************
