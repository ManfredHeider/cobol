      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  09/10/14
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0470.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/CTKI0470.CPv  $
      *
      *   Rev 2.6   Mar 17 2015 14:38:40   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank - Co
      *pies
      *
      *   Rev 2.5   Mar 11 2015 12:29:06   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.4   Mar 09 2015 11:17:52   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.3   Mar 02 2015 10:53:50   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.2   Feb 25 2015 11:44:36   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.1   Jan 13 2015 13:08:52   t13ex78
      *AKI01251: Bestandsdatenübermittlung mit der Commerzbank
      *
      *   Rev 2.0   Oct 9 2014 12:45:43   T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE G68_VERTRAG TABLE
CCC004*    ( G68VVERTRAGSNR                 CHAR(15) NOT NULL,
CCC004*      G68VSTATUS                     CHAR(1) NOT NULL,
CCC004*      G68VMANDANT                    INTEGER NOT NULL,
CCC004*      G68VABSCHLVBNR                 INTEGER NOT NULL,
CCC004*      G68VVRTRGBEGINN                DATE NOT NULL,
CCC004*      G68VVRTRGENDE                  DATE,
CCC004*      G68VDSSPERRE                   CHAR(1) NOT NULL,
CCC004*      G68VDSSPERRDATUM               DATE
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE G68_VERTRAG                 *
      ******************************************************************
       01  G68_VERTRAG-GESAMT.
        05 NI-G68_VERTRAG-TAB.
           10 NI-G68_VERTRAG-ELE PIC S9(04) COMP OCCURS 8 TIMES.
        05 NI-G68_VERTRAG REDEFINES NI-G68_VERTRAG-TAB.
           10 NI-G68VVERTRAGSNR     PIC S9(4) COMP.
           10 NI-G68VSTATUS         PIC S9(4) COMP.
           10 NI-G68VMANDANT        PIC S9(4) COMP.
           10 NI-G68VABSCHLVBNR     PIC S9(4) COMP.
           10 NI-G68VVRTRGBEGINN    PIC S9(4) COMP.
           10 NI-G68VVRTRGENDE      PIC S9(4) COMP.
           10 NI-G68VDSSPERRE       PIC S9(4) COMP.
           10 NI-G68VDSSPERRDATUM   PIC S9(4) COMP.
        05 DCL-G68_VERTRAG.
           10 G68VVERTRAGSNR       PIC X(15).
           10 G68VSTATUS           PIC X(1).
           10 G68VMANDANT          PIC S9(9) USAGE COMP.
           10 G68VABSCHLVBNR       PIC S9(9) USAGE COMP.
           10 G68VVRTRGBEGINN      PIC X(10).
           10 G68VVRTRGENDE        PIC X(10).
           10 G68VDSSPERRE         PIC X(1).
           10 G68VDSSPERRDATUM     PIC X(10).
      ************************** ENDE-DCLGEN *************************
 
 
 
 
 

