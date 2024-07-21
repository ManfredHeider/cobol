      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.03.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTOK0008.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTOK0008.CPv  $
      *
      *   Rev 2.1   19 Jan 2007 09:50:18   t13ex43
      *AKI00556: Steuersätze für Kundenzusammenführung
      *
      *   Rev 2.0   Mar 14 2006 16:40:26   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE OKITICKET
      ******************************************************************
CCC005*    EXEC SQL DECLARE OKITICKET TABLE
CCC004*    ( OTTICKETID                     DECIMAL(18, 0) NOT NULL,
CCC004*      OTTICKETTYP_KTY                SMALLINT NOT NULL,
CCC004*      OTTICKETTYP_KNR                SMALLINT NOT NULL,
CCC004*      OTAENDGRUND_KTY                SMALLINT NOT NULL,
CCC004*      OTAENDGRUND_KNR                SMALLINT NOT NULL,
CCC004*      OTERSTELLT                     TIMESTAMP NOT NULL,
CCC004*      OTERSTELLTVON                  CHAR(30) NOT NULL,
CCC004*      OTOKIKUNDENID                  DECIMAL(18, 0),
CCC004*      OTHOSTKUNDENID                 CHAR(13) NOT NULL,
CCC004*      OTOKIVERTRAGID                 DECIMAL(18, 0),
CCC004*      OTHOSTVERTRAGID                CHAR(13),
CCC004*      OTSWVERSION                    VARCHAR(100),
CCC004*      OTSTATUS_KTY                   SMALLINT NOT NULL,
CCC004*      OTSTATUS_KNR                   SMALLINT NOT NULL,
CCC004*      OTAUSLESEJOBID                 DECIMAL(18, 0),
CCC004*      OTFEHLER_KTY                   SMALLINT NOT NULL,
CCC004*      OTFEHLER_KNR                   SMALLINT NOT NULL,
CCC004*      OTPARAMETER                    VARCHAR(1000)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE  OKITICKET
      ******************************************************************
       01  OKITICKET-GESAMT.
         05 NI-OKITICKET-TAB.
           10 NI-OKITICKET-ELE PIC S9(4) COMP OCCURS 18 TIMES.
         05 NI-OKITICKET REDEFINES NI-OKITICKET-TAB.
           10 NI-OTTICKETID        PIC S9(4) COMP.
           10 NI-OTTICKETTYP-KTY   PIC S9(4) COMP.
           10 NI-OTTICKETTYP-KNR   PIC S9(4) COMP.
           10 NI-OTAENDGRUND-KTY   PIC S9(4) COMP.
           10 NI-OTAENDGRUND-KNR   PIC S9(4) COMP.
           10 NI-OTERSTELLT        PIC S9(4) COMP.
           10 NI-OTERSTELLTVON     PIC S9(4) COMP.
           10 NI-OTOKIKUNDENID     PIC S9(4) COMP.
           10 NI-OTHOSTKUNDENID    PIC S9(4) COMP.
           10 NI-OTOKIVERTRAGID    PIC S9(4) COMP.
           10 NI-OTHOSTVERTRAGID   PIC S9(4) COMP.
           10 NI-OTSWVERSION       PIC S9(4) COMP.
           10 NI-OTSTATUS-KTY      PIC S9(4) COMP.
           10 NI-OTSTATUS-KNR      PIC S9(4) COMP.
           10 NI-OTAUSLESEJOBID    PIC S9(4) COMP.
           10 NI-OTFEHLER-KTY      PIC S9(4) COMP.
           10 NI-OTFEHLER-KNR      PIC S9(4) COMP.
           10 NI-OTPARAMETER       PIC S9(4) COMP.
         05 DCL-OKITICKET.
           10 OTTICKETID           PIC S9(18)V COMP-3.
           10 OTTICKETTYP-KTY      PIC S9(4) COMP.
           10 OTTICKETTYP-KNR      PIC S9(4) COMP.
           10 OTAENDGRUND-KTY      PIC S9(4) COMP.
           10 OTAENDGRUND-KNR      PIC S9(4) COMP.
           10 OTERSTELLT           PIC X(26).
           10 OTERSTELLTVON        PIC X(30).
           10 OTOKIKUNDENID        PIC S9(18)V COMP-3.
           10 OTHOSTKUNDENID       PIC X(13).
           10 OTOKIVERTRAGID       PIC S9(18)V COMP-3.
           10 OTHOSTVERTRAGID      PIC X(13).
           10 OTSWVERSION.
              49 OTSWVERSION-LEN   PIC S9(4) COMP.
              49 OTSWVERSION-TEXT  PIC X(100).
           10 OTSTATUS-KTY         PIC S9(4) COMP.
           10 OTSTATUS-KNR         PIC S9(4) COMP.
           10 OTAUSLESEJOBID       PIC S9(18)V COMP-3.
           10 OTFEHLER-KTY         PIC S9(4) COMP.
           10 OTFEHLER-KNR         PIC S9(4) COMP.
           10 OTPARAMETER.
              49 OTPARAMETER-LEN   PIC S9(4) COMP.
              49 OTPARAMETER-TEXT  PIC X(1000).
      ******************************************************************
