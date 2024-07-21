      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.06.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2414.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2414.CPv  $
      *
      *   Rev 2.2   Apr 09 2013 17:48:46   t13ex27
      *AKI01215: DVAG Wien: Aktivieren KoKUe-Ticket-DTA
      *
      *   Rev 2.1   Nov 24 2010 10:46:50   t13ex27
      *AKI00966: Datenbestandsverarbeitung  DVAG Österreich Ticket-
      *DTA Teil 1
      *
      *   Rev 2.0   Jun 28 2010 10:58:20   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00966!15.09.10!2.1 ! NEU                       !I. Frenzel*
      * AKI01215!09.04.12!2.2 ! Aktivierung KoKÜ für Wien !I. Frenzel*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *                                                              *
      *   Satzbeschreibung für Ticket-DTA Wien                       *
      *                                                              *
      *   Diese Daten sind auch Bestandteil der Tabelle TICKETDTAWIEN*
      *                                                              *
      *   Satzlänge: 455                                             *
      *                                                              *
      ****************************************************************
       01  DRKI2414.
           05  TICKETID                     PIC  9(018).
           05  TICKETLFN                    PIC  9(005).
           05  TICKETVERARBEITET            PIC  X(026).
           05  TICKETTYP                    PIC  9(005).

           05  DATENTEIL                    PIC  X(400).

      *    TICKTETTYP=989
      *    Die Kundenübertragung entspricht einer Vertragsübertragung
      *    Es werden alle Kunden des Vertrags dem Ziel-VB zugeordnet
           05  KUNDENUEBERTRAGUNG      REDEFINES DATENTEIL.
             10  T989-PARTGESELLSCHID       PIC  9(002).
             10  T989-VERTRAGSNUMMER        PIC  X(015).
             10  T989-KUNDENNUMMER          PIC  9(010).
             10  T989-VBNR-QUELLE           PIC  9(008).
             10  T989-VBNR-ZIEL             PIC  9(008).
080413       10  T989-VRBKOMMBETRKZ         PIC  X(001).
      *    TICKTETTYP=990
           05  VERTRAGSBEREINIGUNG     REDEFINES DATENTEIL.
             10  T990-PARTGESELLSCHID       PIC  9(002).
             10  T990-VERTRAGSNUMMER        PIC  X(015).
             10  T990-KUNDENNUMMER-QUELLE   PIC  9(010).
             10  T990-KUNDENNUMMER-ZIEL     PIC  9(010).
             10  T990-PERSONANZVORNAME      PIC  X(030).
             10  T990-PERSONANZNAME         PIC  X(030).
             10  T990-LAND                  PIC  X(002).
             10  T990-PLZ                   PIC  X(015).
             10  T990-STRHAUSNUM            PIC  X(045).
             10  T990-ORT                   PIC  X(030).
             10  T990-GEBGRUENDDAT          PIC  X(010).
             10  T990-PERSONANREDE          PIC  9(001).
             10  T990-PERSONSUCHVORNAME     PIC  X(030).
             10  T990-PERSONSUCHNAME        PIC  X(030).
      *    TICKTETTYP=992
           05  KUNDENNUMMERVERGABE     REDEFINES DATENTEIL.
             10  T992-PARTGESELLSCHID       PIC  9(002).
             10  T992-VERTRAGSNUMMER        PIC  X(015).
             10  T992-KUNDENNUMMER          PIC  9(010).
             10  T992-PERSONANZVORNAME      PIC  X(030).
             10  T992-PERSONANZNAME         PIC  X(030).
             10  T992-LAND                  PIC  X(002).
             10  T992-PLZ                   PIC  X(015).
             10  T992-GEBGRUENDDAT          PIC  X(010).
             10  T992-PERSONANREDE          PIC  9(001).
             10  T992-PERSONSUCHVORNAME     PIC  X(030).
             10  T992-PERSONSUCHNAME        PIC  X(030).
      *    TICKTETTYP=994
           05  KUNDENZUSAMMENFUEHRUNG  REDEFINES DATENTEIL.
             10  T994-PARTGESELLSCHID       PIC  9(002).
             10  T994-VERTRAGSNUMMER        PIC  X(015).
             10  T994-KUNDENNUMMER-QUELLE   PIC  9(010).
             10  T994-KUNDENNUMMER-ZIEL     PIC  9(010).
             10  T994-PERSONANZVORNAME      PIC  X(030).
             10  T994-PERSONANZNAME         PIC  X(030).
             10  T994-LAND                  PIC  X(002).
             10  T994-PLZ                   PIC  X(015).
             10  T994-STRHAUSNUM            PIC  X(045).
             10  T994-ORT                   PIC  X(030).
             10  T994-GEBGRUENDDAT          PIC  X(010).
             10  T994-PERSONANREDE          PIC  9(001).
             10  T994-PERSONSUCHVORNAME     PIC  X(030).
             10  T994-PERSONSUCHNAME        PIC  X(030).
      *    TICKTETTYP=997
           05  ADRESSAENDERUNG         REDEFINES DATENTEIL.
             10  T997-PARTGESELLSCHID       PIC  9(002).
             10  T997-VERTRAGSNUMMER        PIC  X(015).
             10  T997-STRHAUSNUM            PIC  X(045).
             10  T997-LAND                  PIC  X(002).
             10  T997-PLZ                   PIC  X(015).
             10  T997-ORT                   PIC  X(030).
             10  T997-ORTSTEIL              PIC  X(030).
      *    TICKTETTYP=998
           05  NAMENSAENDERUNG         REDEFINES DATENTEIL.
             10  T998-PARTGESELLSCHID       PIC  9(002).
             10  T998-VERTRAGSNUMMER        PIC  X(015).
             10  T998-KUNDENNUMMER          PIC  9(010).
             10  T998-PERSONANZNAME         PIC  X(030).
             10  T998-PERSONANZVORNAME      PIC  X(030).
             10  T998-GEBGRUENDDAT          PIC  X(010).
             10  T998-PERSONANREDE          PIC  9(001).
             10  T998-PERSONSUCHVORNAME     PIC  X(030).
             10  T998-PERSONSUCHNAME        PIC  X(030).
      ************************** Ende DRKI2414 *************************
