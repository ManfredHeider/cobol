      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  30.09.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2431.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2431.CPv  $
      *
      *   Rev 2.2   Sep 18 2012 13:57:56   t13ex27
      *AKI00886: Host - neues Design Kundenaktualisierung-OZP Kunde
      *
      *   Rev 2.1   Nov 24 2010 10:49:24   t13ex27
      *AKI00966: Datenbestandsverarbeitung  DVAG Österreich Ticket-
      *DTA Teil 2
      *
      *   Rev 2.0   Sep 30 2010 16:55:58   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI2431 FUNKTION:Select auf älteste VRB     *
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
      *    FÜR DIE FUNKTION DRKI2431                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    FUNKTION: Select auf Vertrag/Person                       *
      *                                                              *
      ****************************************************************

       01 PI-DRKI2431.

           05  PI-DRKI2431-KEY.
             10  PI-DRKI2431-VRBPERSONENID        PIC  X(013).
             10  PI-DRKI2431-VRBVERTRAGSID        PIC  X(013).

       01 PO-DRKI2431.
          05 PO-DRKI2431-RC                     PIC  X(003).
             88 PO-DRKI2431-RC-OK                        VALUE 'OK '.
             88 PO-DRKI2431-RC-NOT-FOUND                 VALUE 'NFD'.
          05 PO-DRKI2431-DATEN.
             10 PO-DRKI2431-VERPARTGESELLSCHID  PIC S9(4) USAGE COMP.
             10 PO-DRKI2431-VERVERTRAGSNUMMER   PIC X(15).
             10 PO-DRKI2431-LANDISOKZ           PIC X(2).
             10 PO-DRKI2431-ADRESSEPLZ          PIC X(15).
             10 PO-DRKI2431-ADRESSEORT          PIC X(30).
             10 PO-DRKI2431-ADRESSESTRHAUSNUM   PIC X(45).
             10 PO-DRKI2431-KNDANZVORNAME    PIC X(30).
             10 PO-DRKI2431-KNDANZNAME       PIC X(30).
             10 PO-DRKI2431-KNDGEBGRUENDDAT  PIC X(10).
             10 PO-DRKI2431-KNDANREDEID      PIC S9(4) COMP.
             10 PO-DRKI2431-KNDSUCHVORNAME   PIC X(30).
             10 PO-DRKI2431-KNDSUCHNAME      PIC X(30).
      *
      ******** Ende Copy-Member **************************************
