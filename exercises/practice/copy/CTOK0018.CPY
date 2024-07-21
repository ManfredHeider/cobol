      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  29/09/10
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTOK0018.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CTOK0018.CPv  $
      *
      *   Rev 2.2   Sep 29 2010 11:00:18   t13ex48
      *AKI00966: Einführung DVAG Wien:-OZP TICKETDTAWIEN
      *
      *   Rev 2.0   Sep 29 2010 09:31:13   T13EX48
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE TICKETDTAWIEN TABLE
CCC004*    ( TDWTICKETID                    DECIMAL(18, 0) NOT NULL,
CCC004*      TDWLFDNR                       SMALLINT NOT NULL,
CCC004*      TDWTICKETTYP                   SMALLINT NOT NULL,
CCC004*      TDWSTATUS                      CHAR(1) NOT NULL,
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL,
CCC004*      TDWDTADATEN                    VARCHAR(400) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TICKETDTAWIEN               *
      ******************************************************************
       01  TICKETDTAWIEN-GESAMT.
        05 NI-TICKETDTAWIEN-TAB.
           10 NI-TICKETDTAWIEN-ELE PIC S9(04) COMP OCCURS 6 TIMES.
        05 NI-TICKETDTAWIEN REDEFINES NI-TICKETDTAWIEN-TAB.
           10 NI-TDWTICKETID        PIC S9(4) COMP.
           10 NI-TDWLFDNR           PIC S9(4) COMP.
           10 NI-TDWTICKETTYP       PIC S9(4) COMP.
           10 NI-TDWSTATUS          PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
           10 NI-TDWDTADATEN        PIC S9(4) COMP.
        05 DCL-TICKETDTAWIEN.
           10 TDWTICKETID          PIC S9(18)V USAGE COMP-3.
           10 TDWLFDNR             PIC S9(4) USAGE COMP.
           10 TDWTICKETTYP         PIC S9(4) USAGE COMP.
           10 TDWSTATUS            PIC X(1).
           10 LETZTEAENDERUNG      PIC X(26).
           10 TDWDTADATEN.
              49 TDWDTADATEN-LEN   PIC S9(4) USAGE COMP.
              49 TDWDTADATEN-TEXT
                 PIC X(400).
      ************************** ENDE-DCLGEN *************************
