      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.01.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTAT0040.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CTAT0040.CPv  $
      *
      *   Rev 2.2   Jan 28 2009 10:42:18   t13ex14
      *AKI00779: Masch. DTA-Kontrolle Badenia täglich
      *
      *   Rev 2.1   Jan 26 2009 12:04:38   t13ex14
      *AKI00779: Masch. DTA-Kontrolle Badenia täglich
      *
      *   Rev 2.0   Jan 26 2009 09:20:18   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE TBAT0040
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBAT0040 TABLE
CCC004*    ( DTAMANDANTNR                   INTEGER NOT NULL,
CCC004*      DTAPARTGESID                   SMALLINT NOT NULL,
CCC004*      DTAGRUPPE                      SMALLINT NOT NULL,
CCC004*      DTADATEINUMMER                 SMALLINT NOT NULL,
CCC004*      DTALFN                         INTEGER NOT NULL,
CCC004*      DTADATEI                       CHAR(36) NOT NULL,
CCC004*      DTAGEN                         CHAR(8) NOT NULL,
CCC004*      DTASATZANZAHL                  INTEGER NOT NULL,
CCC004*      DTAPARTNERDATUM                DATE NOT NULL,
CCC004*      DTAKATALOGDATUM                DATE NOT NULL,
CCC004*      DTAJOBNAME                     CHAR(8) NOT NULL,
CCC004*      DTAGDGSTATUS                   CHAR(1) NOT NULL,
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL,
CCC004*      DTAERSTERSATZ                  VARCHAR(1000)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE  TBAT0040
      ******************************************************************
       01  TBAT0040-GESAMT.
         05 NI-TBAT0040-TAB.
           10 NI-TBAT0040-ELE PIC S9(4) COMP OCCURS 14 TIMES.
         05 NI-TBAT0040 REDEFINES NI-TBAT0040-TAB.
           10 NI-DTAMANDANTNR       PIC S9(4) COMP.
           10 NI-DTAPARTGESID       PIC S9(4) COMP.
           10 NI-DTAGRUPPE          PIC S9(4) COMP.
           10 NI-DTADATEINUMMER     PIC S9(4) COMP.
           10 NI-DTALFN             PIC S9(4) COMP.
           10 NI-DTADATEI           PIC S9(4) COMP.
           10 NI-DTAGEN             PIC S9(4) COMP.
           10 NI-DTASATZANZAHL      PIC S9(4) COMP.
           10 NI-DTAPARTNERDATUM    PIC S9(4) COMP.
           10 NI-DTAKATALOGDATUM    PIC S9(4) COMP.
           10 NI-DTAJOBNAME         PIC S9(4) COMP.
           10 NI-DTAGDGSTATUS       PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
           10 NI-DTAERSTERSATZ      PIC S9(4) COMP.
         05 DCL-TBAT0040.
           10 DTAMANDANTNR         PIC S9(9) COMP.
           10 DTAPARTGESID   PIC S9(4) COMP.
           10 DTAGRUPPE            PIC S9(4) COMP.
           10 DTADATEINUMMER       PIC S9(4) COMP.
           10 DTALFN               PIC S9(9) COMP.
           10 DTADATEI             PIC X(36).
           10 DTAGEN               PIC X(8).
           10 DTASATZANZAHL        PIC S9(9) COMP.
           10 DTAPARTNERDATUM      PIC X(10).
           10 DTAKATALOGDATUM      PIC X(10).
           10 DTAJOBNAME           PIC X(8).
           10 DTAGDGSTATUS         PIC X(1).
           10 LETZTEAENDERUNG      PIC X(26).
           10 DTAERSTERSATZ.
              49 DTAERSTERSATZ-LEN  PIC S9(4) COMP.
              49 DTAERSTERSATZ-TEXT PIC X(1000).
      ******************************************************************