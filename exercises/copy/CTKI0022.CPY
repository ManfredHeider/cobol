      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  23/05/11
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0022.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/CTKI0022.CPv  $
      *
      *   Rev 2.4   Dec 19 2016 11:00:14   t13ex27
      *AKI01378: Kundenaktualisierung - Kundennummer aus dem Antrag
      *
      *   Rev 2.3   Nov 16 2016 08:49:08   t13ex73
      *AKI01378: Kundenaktualisierung - Kundennummer aus dem Antrag
      *
      *   Rev 2.2   Oct 27 2016 11:43:56   t13ex73
      *AKI01378: Kundenaktualisierung - Kundennummer aus dem Antrag
      *
      *   Rev 2.1   Sep 18 2012 14:00:16   t13ex27
      *AKI00886: Host - neues Design Kundenaktualisierung-OZP Kunde
      *
      *   Rev 2.0   May 23 2011 16:36:27   T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE KUNDE TABLE
CCC004*    ( KNDPERSONENID                  CHAR(13) NOT NULL,
CCC004*      KNDANREDEID                    SMALLINT NOT NULL,
CCC004*      KNDANZNAME                     CHAR(30) NOT NULL,
CCC004*      KNDANZVORNAME                  CHAR(30),
CCC004*      KNDSUCHNAME                    CHAR(30) NOT NULL,
CCC004*      KNDSUCHVORNAME                 CHAR(30),
CCC004*      KNDGEBGRUENDDAT                DATE,
CCC004*      KNDGESCHLECHT                  CHAR(1) NOT NULL,
CCC004*      KNDNUMMER                      INTEGER,
CCC004*      KNDSTATUSID                    CHAR(1) NOT NULL,
CCC004*      KNDANZBETREUER                 SMALLINT NOT NULL,
CCC004*      KNDMANDANTNR                   INTEGER NOT NULL,
CCC004*      KNDBDSGSPERRDAT                DATE,
CCC004*      KNDNEUKUNDE                    DATE,
CCC004*      KNDALTKUNDE                    DATE,
CCC004*      KNDWIK                         DATE,
CCC004*      KNDGEANDERTVON                 CHAR(4) NOT NULL,
CCC004*      KNDGEANDERTPGM                 CHAR(8),
CCC004*      LETZTEAENDERUNG                TIMESTAMP,
CCC004*      KNDANTRAGSKNR                  INTEGER
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE KUNDE                       *
      ******************************************************************
       01  KUNDE-GESAMT.
        05 NI-KUNDE-TAB.
           10 NI-KUNDE-ELE PIC S9(04) COMP OCCURS 20 TIMES.
        05 NI-KUNDE REDEFINES NI-KUNDE-TAB.
           10 NI-KNDPERSONENID      PIC S9(4) COMP.
           10 NI-KNDANREDEID        PIC S9(4) COMP.
           10 NI-KNDANZNAME         PIC S9(4) COMP.
           10 NI-KNDANZVORNAME      PIC S9(4) COMP.
           10 NI-KNDSUCHNAME        PIC S9(4) COMP.
           10 NI-KNDSUCHVORNAME     PIC S9(4) COMP.
           10 NI-KNDGEBGRUENDDAT    PIC S9(4) COMP.
           10 NI-KNDGESCHLECHT      PIC S9(4) COMP.
           10 NI-KNDNUMMER          PIC S9(4) COMP.
           10 NI-KNDSTATUSID        PIC S9(4) COMP.
           10 NI-KNDANZBETREUER     PIC S9(4) COMP.
           10 NI-KNDMANDANTNR       PIC S9(4) COMP.
           10 NI-KNDBDSGSPERRDAT    PIC S9(4) COMP.
           10 NI-KNDNEUKUNDE        PIC S9(4) COMP.
           10 NI-KNDALTKUNDE        PIC S9(4) COMP.
           10 NI-KNDWIK             PIC S9(4) COMP.
           10 NI-KNDGEANDERTVON     PIC S9(4) COMP.
           10 NI-KNDGEANDERTPGM     PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
           10 NI-KNDANTRAGSKNR      PIC S9(4) COMP.
        05 DCL-KUNDE.
           10 KNDPERSONENID        PIC X(13).
           10 KNDANREDEID          PIC S9(4) USAGE COMP.
           10 KNDANZNAME           PIC X(30).
           10 KNDANZVORNAME        PIC X(30).
           10 KNDSUCHNAME          PIC X(30).
           10 KNDSUCHVORNAME       PIC X(30).
           10 KNDGEBGRUENDDAT      PIC X(10).
           10 KNDGESCHLECHT        PIC X(1).
           10 KNDNUMMER            PIC S9(9) USAGE COMP.
           10 KNDSTATUSID          PIC X(1).
           10 KNDANZBETREUER       PIC S9(4) USAGE COMP.
           10 KNDMANDANTNR         PIC S9(9) USAGE COMP.
           10 KNDBDSGSPERRDAT      PIC X(10).
           10 KNDNEUKUNDE          PIC X(10).
           10 KNDALTKUNDE          PIC X(10).
           10 KNDWIK               PIC X(10).
           10 KNDGEANDERTVON       PIC X(4).
           10 KNDGEANDERTPGM       PIC X(8).
           10 LETZTEAENDERUNG      PIC X(26).
           10 KNDANTRAGSKNR        PIC S9(9) USAGE COMP.
      ************************** ENDE-DCLGEN *************************
 


