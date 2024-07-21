      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25.04.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2596.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRKI2596.CPv  $
      *
      *   Rev 2.5   Apr 22 2014 11:44:18   t13ex78
      *AKI01063: Wartungsauftrag-Ticketverarbeitung Kundenlöschung 
      *Gemeinschaftskonten
      *
      *   Rev 2.4   Apr 22 2014 09:51:18   t13ex78
      *AKI01180: Speicherfristen bei Interessenten und Altkunden-Mi
      *gration Tabelle Neukunde und Datum_Altkunde
      *
      *   Rev 2.3   Apr 22 2014 09:38:00   t13ex78
      *AKI01180: Speicherfristen bei Interessenten und Altkunden-Mi
      *gration Tabelle Neukunde und Datum_Altkunde
      *
      *   Rev 2.2   Apr 15 2014 14:20:52   t13ex78
      *AKI01180: Speicherfristen bei Interessenten und Altkunden - 
      *Upro Kundenlöschung
      *
      *   Rev 2.1   Jul 20 2012 10:17:46   t13ex27
      *AKI01135: Kundendaten physisch löschen-Ticket-HP
      *
      *   Rev 2.0   Apr 25 2012 11:47:44   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: UPKI0233                                   *
      *    ----------------------------------------------------------*
      *    SCHNITTSTELLE FÜR DAS UPRO UPKI0233                     *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN       *
      *    ----------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01135!08.05.12! 2.1! Neu                       ! WMI      *
      *         !        !    !                           !          *
      * AKI01180!18.02.14! 2.2! Änderungs KTY und KNR wird! T.Flierl *
      *         !        !    ! mit übergeben             !          *
      *                                                              *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      ****************************************************************
      *
       01  PI-DRKI2596.
           05  PI-DATEN.
               10  PI-DRKI2596-OTTICKETID             PIC 9(018).
               10  PI-DRKI2596-OTERSTELLT             PIC X(026).
               10  PI-DRKI2596-OTERSTELLTVON          PIC X(030).
               10  PI-DRKI2596-OTHOSTKUNDENID         PIC X(013).
               10  PI-DRKI2596-OTAENDGRUND-KTY        PIC S9(004) COMP.
               10  PI-DRKI2596-OTAENDGRUND-KNR        PIC S9(004) COMP.


       01  PO-DRKI2596.
           05  PO-DRKI2596-RC               PIC  X(003).
               88 PO-DRKI2596-RC-OK              VALUE 'OK '.
               88 PO-DRKI2596-RC-NOK             VALUE 'NOK'.
               88 PO-DRKI2596-RC-NFD             VALUE 'NFD'.
           05  PO-DATEN.
               10  PO-DRKI2596-OTFEHLER-KNR        PIC S9(4)  COMP.

      ******* ENDE COPY-MEMBER **************************************



