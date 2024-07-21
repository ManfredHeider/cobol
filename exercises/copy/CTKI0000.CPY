      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08/05/12
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0000.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/CTKI0000.CPv  $
      *
      *   Rev 2.5   Nov 04 2014 08:56:12   t13ex78
      *AKI01324: Erweiterung der Tabelle KUNDENLOESCHUNG - OZP's an
      *passen
      *
      *   Rev 2.4   Oct 29 2014 11:00:22   t13ex78
      *AKI01324: Erweiterung der Tabelle KUNDENLOESCHUNG - OZP's an
      *passen
      *
      *   Rev 2.3   May 16 2014 13:09:22   t13ex78
      *AKI01180: Speicherfristen bei Interessenten und Altkunden -
      *ZTKI0000
      *
      *   Rev 2.2   May 15 2014 11:55:14   t13ex78
      *AKI01180: Speicherfristen bei Interessenten und Altkunden -
      *Neugenerien des ZTKI0000 wegen NULL-Indikator
      *
      *   Rev 2.1   Jul 20 2012 09:55:44   t13ex27
      *AKI01135: Kundendaten physisch löschen-OZPs und NKDA-Copys
      *
      *   Rev 2.0   May 8 2012 11:12:13   T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE KUNDENLOESCHUNG TABLE
CCC004*    ( KULPERSONENID                  CHAR(13) NOT NULL,
CCC004*      KULVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      KULTICKETID                    DECIMAL(18, 0) NOT NULL,
CCC004*      KULVERTRAGNUMMER               CHAR(15),
CCC004*      KULPARTGESELLSCH               SMALLINT,
CCC004*      KULMANDANTNR                   INTEGER NOT NULL,
CCC004*      KULKNDNUMMER                   INTEGER NOT NULL,
CCC004*      KULSTATUS                      CHAR(1) NOT NULL,
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL,
CCC004*      KULERSTELLTTST                 TIMESTAMP NOT NULL,
CCC004*      KULVBNR                        INTEGER NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE KUNDENLOESCHUNG             *
      ******************************************************************
       01  KUNDENLOESCHUNG-GESAMT.
        05 NI-KUNDENLOESCHUNG-TAB.
           10 NI-KUNDENLOESCHUNG-ELE PIC S9(04) COMP OCCURS 11 TIMES.
        05 NI-KUNDENLOESCHUNG REDEFINES NI-KUNDENLOESCHUNG-TAB.
           10 NI-KULPERSONENID      PIC S9(4) COMP.
           10 NI-KULVERTRAGSID      PIC S9(4) COMP.
           10 NI-KULTICKETID        PIC S9(4) COMP.
           10 NI-KULVERTRAGNUMMER   PIC S9(4) COMP.
           10 NI-KULPARTGESELLSCH   PIC S9(4) COMP.
           10 NI-KULMANDANTNR       PIC S9(4) COMP.
           10 NI-KULKNDNUMMER       PIC S9(4) COMP.
           10 NI-KULSTATUS          PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
           10 NI-KULERSTELLTTST     PIC S9(4) COMP.
           10 NI-KULVBNR            PIC S9(4) COMP.
        05 DCL-KUNDENLOESCHUNG.
           10 KULPERSONENID        PIC X(13).
           10 KULVERTRAGSID        PIC X(13).
           10 KULTICKETID          PIC S9(18)V USAGE COMP-3.
           10 KULVERTRAGNUMMER     PIC X(15).
           10 KULPARTGESELLSCH     PIC S9(4) USAGE COMP.
           10 KULMANDANTNR         PIC S9(9) USAGE COMP.
           10 KULKNDNUMMER         PIC S9(9) USAGE COMP.
           10 KULSTATUS            PIC X(1).
           10 LETZTEAENDERUNG      PIC X(26).
           10 KULERSTELLTTST       PIC X(26).
           10 KULVBNR              PIC S9(9) USAGE COMP.
      ************************** ENDE-DCLGEN *************************
