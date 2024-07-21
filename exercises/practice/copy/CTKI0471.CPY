      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  09/10/14
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0471.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/CTKI0471.CPv  $
      *
      *   Rev 2.6   Mar 17 2015 14:38:44   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank - Co
      *pies
      *
      *   Rev 2.5   Mar 11 2015 12:29:10   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.4   Mar 09 2015 11:17:56   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.3   Mar 02 2015 10:53:56   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.2   Feb 25 2015 11:44:40   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.1   Jan 13 2015 13:08:58   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.0   Oct 9 2014 12:53:28   T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE G68_KUNDE TABLE
CCC004*    ( G68KVERTRAGSNR                 CHAR(15) NOT NULL,
CCC004*      G68KLFDNR                      SMALLINT NOT NULL,
CCC004*      G68KNACHNAME                   CHAR(30),
CCC004*      G68KVORNAME                    CHAR(30),
CCC004*      G68KGEBURTSDATUM               DATE,
CCC004*      G68KANREDE                     CHAR(1),
CCC004*      G68KGESCHLECHT                 CHAR(1),
CCC004*      G68KSTRASSE                    CHAR(40),
CCC004*      G68KPOSTLEITZAHL               CHAR(9),
CCC004*      G68KORT                        CHAR(30),
CCC004*      G68KLAND                       CHAR(3),
CCC004*      G68KLOESCHKZ                   CHAR(1) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE G68_KUNDE                   *
      ******************************************************************
       01  G68_KUNDE-GESAMT.
        05 NI-G68_KUNDE-TAB.
           10 NI-G68_KUNDE-ELE PIC S9(04) COMP OCCURS 12 TIMES.
        05 NI-G68_KUNDE REDEFINES NI-G68_KUNDE-TAB.
           10 NI-G68KVERTRAGSNR     PIC S9(4) COMP.
           10 NI-G68KLFDNR          PIC S9(4) COMP.
           10 NI-G68KNACHNAME       PIC S9(4) COMP.
           10 NI-G68KVORNAME        PIC S9(4) COMP.
           10 NI-G68KGEBURTSDATUM   PIC S9(4) COMP.
           10 NI-G68KANREDE         PIC S9(4) COMP.
           10 NI-G68KGESCHLECHT     PIC S9(4) COMP.
           10 NI-G68KSTRASSE        PIC S9(4) COMP.
           10 NI-G68KPOSTLEITZAHL   PIC S9(4) COMP.
           10 NI-G68KORT            PIC S9(4) COMP.
           10 NI-G68KLAND           PIC S9(4) COMP.
           10 NI-G68KLOESCHKZ       PIC S9(4) COMP.
        05 DCL-G68_KUNDE.
           10 G68KVERTRAGSNR       PIC X(15).
           10 G68KLFDNR            PIC S9(4) USAGE COMP.
           10 G68KNACHNAME         PIC X(30).
           10 G68KVORNAME          PIC X(30).
           10 G68KGEBURTSDATUM     PIC X(10).
           10 G68KANREDE           PIC X(1).
           10 G68KGESCHLECHT       PIC X(1).
           10 G68KSTRASSE          PIC X(40).
           10 G68KPOSTLEITZAHL     PIC X(9).
           10 G68KORT              PIC X(30).
           10 G68KLAND             PIC X(3).
           10 G68KLOESCHKZ         PIC X(1).
      ************************** ENDE-DCLGEN *************************
 
 
 
 
 

