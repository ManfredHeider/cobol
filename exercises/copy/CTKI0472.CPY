      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  09/10/14
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0472.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/CTKI0472.CPv  $
      *
      *   Rev 2.6   Mar 17 2015 14:38:50   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank - Co
      *pies
      *
      *   Rev 2.5   Mar 11 2015 12:29:14   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.4   Mar 09 2015 11:18:00   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.3   Mar 02 2015 10:54:00   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.2   Feb 25 2015 11:44:44   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.1   Jan 13 2015 13:09:04   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.0   Oct 9 2014 12:56:26   T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE G68_BAUFI TABLE
CCC004*    ( G68BVERTRAGSNR                 CHAR(15) NOT NULL,
CCC004*      G68BLFDNR                      SMALLINT NOT NULL,
CCC004*      G68BPRODBEGINN                 DATE NOT NULL,
CCC004*      G68BPRODUKTENDE                DATE,
CCC004*      G68BBETRAGANFANG               DECIMAL(14, 2) NOT NULL,
CCC004*      G68BFESTZINSENDE               DATE,
CCC004*      G68BBETRGAKTUELL               DECIMAL(14, 2),
CCC004*      G68BTILGBEGINN                 DATE,
CCC004*      G68BTILGUNGSART                CHAR(1) NOT NULL,
CCC004*      G68BTILGUNGPROZ                DECIMAL(6, 4),
CCC004*      G68BNOMINALZINS                DECIMAL(6, 4),
CCC004*      G68BZAHLUNGSART                CHAR(1) NOT NULL,
CCC004*      G68BANNUITAET                  DECIMAL(14, 2),
CCC004*      G68BZUSAGEDATUM                DATE,
CCC004*      G68BAUSZAHLDATUM               DATE,
CCC004*      G68BDARLEHENSART               CHAR(1) NOT NULL,
CCC004*      G68BIBAN                       CHAR(34),
CCC004*      G68BBIC                        CHAR(11),
CCC004*      G68BMANDATREF                  CHAR(35),
CCC004*      G68BABWEICHBZ                  CHAR(1) NOT NULL,
CCC004*      G68BNACHNAME                   CHAR(30),
CCC004*      G68BVORNAME                    CHAR(30),
CCC004*      G68BGEBURTSDATUM               DATE,
CCC004*      G68BANREDE                     CHAR(1),
CCC004*      G68BSTRASSE                    CHAR(40),
CCC004*      G68BPOSTLEITZAHL               CHAR(9),
CCC004*      G68BORT                        CHAR(30),
CCC004*      G68BLAND                       CHAR(3)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE G68_BAUFI                   *
      ******************************************************************
       01  G68_BAUFI-GESAMT.
        05 NI-G68_BAUFI-TAB.
           10 NI-G68_BAUFI-ELE PIC S9(04) COMP OCCURS 28 TIMES.
        05 NI-G68_BAUFI REDEFINES NI-G68_BAUFI-TAB.
           10 NI-G68BVERTRAGSNR     PIC S9(4) COMP.
           10 NI-G68BLFDNR          PIC S9(4) COMP.
           10 NI-G68BPRODBEGINN     PIC S9(4) COMP.
           10 NI-G68BPRODUKTENDE    PIC S9(4) COMP.
           10 NI-G68BBETRAGANFANG   PIC S9(4) COMP.
           10 NI-G68BFESTZINSENDE   PIC S9(4) COMP.
           10 NI-G68BBETRGAKTUELL   PIC S9(4) COMP.
           10 NI-G68BTILGBEGINN     PIC S9(4) COMP.
           10 NI-G68BTILGUNGSART    PIC S9(4) COMP.
           10 NI-G68BTILGUNGPROZ    PIC S9(4) COMP.
           10 NI-G68BNOMINALZINS    PIC S9(4) COMP.
           10 NI-G68BZAHLUNGSART    PIC S9(4) COMP.
           10 NI-G68BANNUITAET      PIC S9(4) COMP.
           10 NI-G68BZUSAGEDATUM    PIC S9(4) COMP.
           10 NI-G68BAUSZAHLDATUM   PIC S9(4) COMP.
           10 NI-G68BDARLEHENSART   PIC S9(4) COMP.
           10 NI-G68BIBAN           PIC S9(4) COMP.
           10 NI-G68BBIC            PIC S9(4) COMP.
           10 NI-G68BMANDATREF      PIC S9(4) COMP.
           10 NI-G68BABWEICHBZ      PIC S9(4) COMP.
           10 NI-G68BNACHNAME       PIC S9(4) COMP.
           10 NI-G68BVORNAME        PIC S9(4) COMP.
           10 NI-G68BGEBURTSDATUM   PIC S9(4) COMP.
           10 NI-G68BANREDE         PIC S9(4) COMP.
           10 NI-G68BSTRASSE        PIC S9(4) COMP.
           10 NI-G68BPOSTLEITZAHL   PIC S9(4) COMP.
           10 NI-G68BORT            PIC S9(4) COMP.
           10 NI-G68BLAND           PIC S9(4) COMP.
        05 DCL-G68_BAUFI.
           10 G68BVERTRAGSNR       PIC X(15).
           10 G68BLFDNR            PIC S9(4) USAGE COMP.
           10 G68BPRODBEGINN       PIC X(10).
           10 G68BPRODUKTENDE      PIC X(10).
           10 G68BBETRAGANFANG     PIC S9(12)V9(2) USAGE COMP-3.
           10 G68BFESTZINSENDE     PIC X(10).
           10 G68BBETRGAKTUELL     PIC S9(12)V9(2) USAGE COMP-3.
           10 G68BTILGBEGINN       PIC X(10).
           10 G68BTILGUNGSART      PIC X(1).
           10 G68BTILGUNGPROZ      PIC S9(2)V9(4) USAGE COMP-3.
           10 G68BNOMINALZINS      PIC S9(2)V9(4) USAGE COMP-3.
           10 G68BZAHLUNGSART      PIC X(1).
           10 G68BANNUITAET        PIC S9(12)V9(2) USAGE COMP-3.
           10 G68BZUSAGEDATUM      PIC X(10).
           10 G68BAUSZAHLDATUM     PIC X(10).
           10 G68BDARLEHENSART     PIC X(1).
           10 G68BIBAN             PIC X(34).
           10 G68BBIC              PIC X(11).
           10 G68BMANDATREF        PIC X(35).
           10 G68BABWEICHBZ        PIC X(1).
           10 G68BNACHNAME         PIC X(30).
           10 G68BVORNAME          PIC X(30).
           10 G68BGEBURTSDATUM     PIC X(10).
           10 G68BANREDE           PIC X(1).
           10 G68BSTRASSE          PIC X(40).
           10 G68BPOSTLEITZAHL     PIC X(9).
           10 G68BORT              PIC X(30).
           10 G68BLAND             PIC X(3).
      ************************** ENDE-DCLGEN *************************
 
 
 
 
 

