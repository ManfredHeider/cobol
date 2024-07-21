      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0328.CPY  $
      *    $Revision:   2.21  $
      *
      *    $Log:   L:/mf/CPY/CTKI0328.CPv  $
      *
      *   Rev 2.21   Feb 04 2020 13:02:56   t13ac20
      *AKI01757: VDL-Leben: HOST Verarbeitung und Speicherung der D
      *aten für die neuen Pflegetarife (pflegeSmart und pflegePro) 
      *b2
      *
      *   Rev 2.20   Feb 04 2020 07:44:40   t13ac20
      *AKI01757: VDL-Leben: HOST Verarbeitung und Speicherung der D
      *aten für die neuen Pflegetarife (pflegeSmart und pflegePro)
      *b2
      *
      *   Rev 2.19   Aug 14 2018 09:31:14   t13ac17
      *AKI01605: G20 EVWS - neue Produkte zum RT 09/2018 Standard-OZP
      *
      *   Rev 2.18   Sep 20 2017 15:22:02   t13ac20
      *AKI01503: Bereinigung AML-Tabellen
      *
      *   Rev 2.17   Mar 15 2017 14:31:18   t13ex27
      *AKI01492: AML-Änderungen zum RT 03/2017OZPs
      *
      *   Rev 2.16   Jan 17 2017 10:58:14   t13ex27
      *AKI01488: AM-Leben Konsortialverträge
      *
      *   Rev 2.15   Jun 24 2016 09:03:54   t13ex48
      *AKI01434: AML-Neue Produkte zum RT 06/16: Rechenwerte
      *
      *   Rev 2.14   Mar 18 2015 17:19:42   t13ex48
      *AKI01349: AMLeben: VEP-Bestandsmasken: OZP LRW
      *
      *   Rev 2.13   Jun 26 2014 14:03:34   t13ex48
      *AKI01296: VERMÖGENS- & EINKOMMENSPLAN - VEP: Cobol RW
      *
      *   Rev 2.12   Mar 28 2014 01:18:18   t13ex48
      *AKI01153: Neues Verfahren: Aktualisierung AML-Rechenwerte
      *
      *   Rev 2.11   Aug 31 2011 12:40:24   t13ex48
      *AKI01065: AMLeben - Einführung Impulsprodukt - VASP: Source RW
      *
      *   Rev 2.10   Jan 04 2011 13:45:26   t13ex27
      *AKI00961: Anzeige "Frühester Rentenbeginn" in KI-Vertragsdat
      *en-OZP
      *
      *   Rev 2.8   Aug 25 2009 11:33:12   t13ex27
      *AKI00863: DB-Änderung für Rel. 4.58 AML
      *
      *   Rev 2.7   Aug 10 2009 14:57:30   t13ex27
      *AKI00843: DB-Änderungen für Rel. 4.60 - AML
      *
      *   Rev 2.6   16 Dec 2008 11:29:58   t13ex27
      *AKI00773: DB-Änderung für Rel. 4.44
      *
      *   Rev 2.5   04 Jun 2008 15:58:58   t13ex27
      *AKI00711: Neue DTA-Felder AMLeben DB-Änderung
      *
      *   Rev 2.4   Sep 18 2006 10:48:58   t13ex27
      *AKI00624: DB-Änderung für Fix 93 DCLGENs
      *
      *   Rev 2.3   Jun 17 2004 14:13:50   t13ex27
      *Integration Kinderprodukt in der Vermögensplanung - Rechenwerte
      *
      *   Rev 2.2   May 13 2004 14:13:08   t13ex27
      *DB-Änderung für Fix 61 CTKI's
      *
      *   Rev 2.1   Apr 02 2003 16:04:32   t13ex27
      *AKI00347 - DB-Änderung für Fix 38
      *
      *   Rev 2.0   Feb 27 1999 12:50:34   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:29:10   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:58:02   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AKI00001 V006.000 19980325'
      ******************************************************************
CCC005*    EXEC SQL DECLARE LEBENRECHENWERTE TABLE
CCC004*    ( LRWVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      LRWVERSTARLFDNR                SMALLINT NOT NULL,
CCC004*      LRWDATUMSTAND                  DATE,
CCC004*      LRWABLEISTVOR                  DECIMAL(11, 2),
CCC004*      LRWABLEISTVORWKZ               CHAR(1) NOT NULL,
CCC004*      LRWKAPABFVOR                   DECIMAL(11, 2),
CCC004*      LRWKAPABFGAR                   DECIMAL(11, 2),
CCC004*      LRWVSRTOD                      DECIMAL(11, 2),
CCC004*      LRWUEBETTOD                    DECIMAL(11, 2),
CCC004*      LRWBEITRFREIESUM               DECIMAL(11, 2),
CCC004*      LRWRKWERTGES                   DECIMAL(11, 2),
CCC004*      LRWBONUSRENTE                  DECIMAL(11, 2),
CCC004*      LRWKAPABFINKDYN                DECIMAL(11, 2),
CCC004*      LRWABLEISTSIM                  DECIMAL(11, 2),
CCC004*      LRWVORAUSZDARLBETR             DECIMAL(11, 2),
CCC004*      LRWMOEGLDARLBETR               DECIMAL(11, 2),
CCC004*      LRWGBURENTE                    DECIMAL(9, 2),
CCC004*      LRWGBUZVRENTE                  DECIMAL(9, 2),
CCC004*      LRWGUZVSUMME                   DECIMAL(13, 2),
CCC004*      LRWERMABL                      DATE,
CCC004*      LRWGARLSTABL                   DECIMAL(13, 2),
CCC004*      LRWFORENDITE                   DECIMAL(5, 2),
CCC004*      LRWZAHLBEITRAG                 DECIMAL(11, 2),
CCC004*      LRWGKIRENTE                    DECIMAL(11, 2),
CCC004*      LRWGPFRENTE                    DECIMAL(11, 2),
CCC004*      LRWGPFRENTEANW                 DECIMAL(11, 2),
CCC004*      LRWSUMBETBWRRK                 DECIMAL(13, 2),
CCC004*      LRWBEGABRUFPHASE               DATE,
CCC004*      LRWVVGLSTABLSIM                DECIMAL(13, 2),
CCC004*      LRWVVGKAPABFSIM                DECIMAL(13, 2),
CCC004*      LRWABLEISTVORBKZ               CHAR(1),
CCC004*      LRWKAPABFVORBKZ                CHAR(1),
CCC004*      LRWKAPABFGARBKZ                CHAR(1),
CCC004*      LRWVSRTODBKZ                   CHAR(1),
CCC004*      LRWUEBETTODBKZ                 CHAR(1),
CCC004*      LRWBETRFSUMBKZ                 CHAR(1),
CCC004*      LRWRKWERTGESBKZ                CHAR(1),
CCC004*      LRWBONUSRENTEBKZ               CHAR(1),
CCC004*      LRWKAPABFINKDBKZ               CHAR(1),
CCC004*      LRWABLEISTSIMBKZ               CHAR(1),
CCC004*      LRWVORAUSZDBBKZ                CHAR(1),
CCC004*      LRWMOEGLDBBKZ                  CHAR(1),
CCC004*      LRWGBURENTEBKZ                 CHAR(1),
CCC004*      LRWGBUZVRENTEBKZ               CHAR(1),
CCC004*      LRWGUZVSUMMEBKZ                CHAR(1),
CCC004*      LRWERMABLBKZ                   CHAR(1),
CCC004*      LRWGARLSTABLBKZ                CHAR(1),
CCC004*      LRWFORENDITEBKZ                CHAR(1),
CCC004*      LRWZAHLBEITRBKZ                CHAR(1),
CCC004*      LRWGKIRENTEBKZ                 CHAR(1),
CCC004*      LRWGPFRENTEBKZ                 CHAR(1),
CCC004*      LRWGPFRENTEANBKZ               CHAR(1),
CCC004*      LRWSUMBETBWRKBKZ               CHAR(1),
CCC004*      LRWVVGLSTASIBKZ                CHAR(1),
CCC004*      LRWVVKAPABFSIBKZ               CHAR(1),
CCC004*      LRWSUMDK                       DECIMAL(13, 2),
CCC004*      LRWSUMDKBKZ                    CHAR(1),
CCC004*      LRWFMBTFRTT                    DATE,
CCC004*      LRWFMBTFRTTINFO                CHAR(1),
CCC004*      LRWBTFRISTWERT                 DECIMAL(13, 2),
CCC004*      LRWBTFRMINDWERT                DECIMAL(13, 2),
CCC004*      LRWFRMGLBEGRENTE               DATE,
CCC004*      LRWGESEURENTE                  DECIMAL(9, 2),
CCC004*      LRWGESEURENTEBKZ               CHAR(1),
CCC004*      LRWGESEUZVRENTE                DECIMAL(9, 2),
CCC004*      LRWGESEUZVRNTBKZ               CHAR(1),
CCC004*      LRWGESEUZBONRNT                DECIMAL(9, 2),
CCC004*      LRWGESEUZBONRBKZ               CHAR(1),
CCC004*      LETZTEAENDERUNG                TIMESTAMP,
CCC004*      LRWRKWHV                       DECIMAL(13, 2),
CCC004*      LRWRKWHVBKZ                    CHAR(1),
CCC004*      LRWRKWBUZV                     DECIMAL(13, 2),
CCC004*      LRWRKWBUZVBKZ                  CHAR(1),
CCC004*      LRWRKWTZV                      DECIMAL(13, 2),
CCC004*      LRWRKWTZVBKZ                   CHAR(1),
CCC004*      LRWRKWPZV                      DECIMAL(13, 2),
CCC004*      LRWRKWPZVBKZ                   CHAR(1),
CCC004*      LRWRKWEUZV                     DECIMAL(13, 2),
CCC004*      LRWRKWEUZVBKZ                  CHAR(1),
CCC004*      LRWKONVGH                      DECIMAL(13, 2),
CCC004*      LRWKONVGHBKZ                   CHAR(1),
CCC004*      LRWDATUMSTANDSIM               DATE,
CCC004*      LRWGARVORGLSTABL               DECIMAL(13, 2),
CCC004*      LRWGARVORGLSTABBKZ             CHAR(1),
CCC004*      LRWVORVORGLSTABL               DECIMAL(13, 2),
CCC004*      LRWVORVORGLSTABBKZ             CHAR(1),
CCC004*      LRWGARVORGKAPABF               DECIMAL(13, 2),
CCC004*      LRWGARVORGKAPABBKZ             CHAR(1),
CCC004*      LRWVORVORGKAPABF               DECIMAL(13, 2),
CCC004*      LRWVORVORGKAPABBKZ             CHAR(1),
CCC004*      LRWABZUG                       DECIMAL(13, 2),
CCC004*      LRWABZUGBKZ                    CHAR(1),
CCC004*      LRWAUSZAHLBETRAG               DECIMAL(13, 2),
CCC004*      LRWAUSZAHLBETBKZ               CHAR(1),
CCC004*      LRWSUMRKWNATAZ                 DECIMAL(13, 2),
CCC004*      LRWSUMRKWNATAZBKZ              CHAR(1),
CCC004*      LRWVORLSTABLNATAZ              DECIMAL(13, 2),
CCC004*      LRWVORLSTABLNABKZ              CHAR(1),
CCC004*      LRWVORKAPABFTNATAZ             DECIMAL(13, 2),
CCC004*      LRWVORKAPABFNABKZ              CHAR(1),
CCC004*      LRWVERMOEGTOPF                 DECIMAL(13, 2),
CCC004*      LRWVERMOEGTOPBKZ               CHAR(1),
CCC004*      LRWEINKOMTOPF                  DECIMAL(13, 2),
CCC004*      LRWEINKOMTOPFBKZ               CHAR(1),
CCC004*      LRWBSPBEGRNTZHLG               DATE,
CCC004*      LRWSUMTEILRENTEN               DECIMAL(13, 2),
CCC004*      LRWSUMTEILRNTBKZ               CHAR(1),
CCC004*      LRWGARVGABLSTABL               DECIMAL(13, 2),
CCC004*      LRWGARVGABLSTKZ                CHAR(1),
CCC004*      LRWGARVGABLKAPABF              DECIMAL(13, 2),
CCC004*      LRWGARVGABLKAPKZ               CHAR(1),
CCC004*      LRWSICHERHRESERV               DECIMAL(13, 2),
CCC004*      LRWSICHERHRESBKZ               CHAR(1),
CCC004*      LRWVSRSK                       DECIMAL(13, 2),
CCC004*      LRWVSRSKBKZ                    CHAR(1),
CCC004*      LRWVSRSKUEB                    DECIMAL(13, 2),
CCC004*      LRWVSRSKUEBBKZ                 CHAR(1),
CCC004*      LRWRKWSK                       DECIMAL(13, 2),
CCC004*      LRWRKWSKBKZ                    CHAR(1),
CCC004*      LRWBTFRSUMSK                   DECIMAL(13, 2),
CCC004*      LRWBTFRSUMSKBKZ                CHAR(1),
CCC004*      LRWFMBTFRTTSK                  DATE,
CCC004*      LRWFMBTFRTTSKBKZ               CHAR(1),
CCC004*      LRWBTFRISTWERTSK               DECIMAL(13, 2),
CCC004*      LRWBTFRMINDWRTSK               DECIMAL(13, 2),
CCC004*      LRWKONSORTIALKZ                CHAR(1) NOT NULL,
CCC004*      LRWGARABLRNTPRL                DECIMAL(13, 2),
CCC004*      LRWGARABRNTPRLBKZ              CHAR(1),
CCC004*      LRWAKTVTRWERT                  DECIMAL(13, 2),
CCC004*      LRWAKTVTRWERTBKZ               CHAR(1),
CCC004*      LRWGBURENTANW                  DECIMAL(13, 2),
CCC004*      LRWGBURENTANWBKZ               CHAR(1),
CCC004*      LRWPFSTAFFELKZN                CHAR(1),
CCC004*      LRWGARPFRENTE1                 DECIMAL(13, 2),
CCC004*      LRWGARPFRENTE2                 DECIMAL(13, 2),
CCC004*      LRWGARPFRENTE3                 DECIMAL(13, 2),
CCC004*      LRWGARPFRENTE4                 DECIMAL(13, 2),
CCC004*      LRWGARPFRENTE5                 DECIMAL(13, 2),
CCC004*      LRWGARPFRENTEBKZ               CHAR(1),
CCC004*      LRWGESPFRENTE1                 DECIMAL(13, 2),
CCC004*      LRWGESPFRENTE2                 DECIMAL(13, 2),
CCC004*      LRWGESPFRENTE3                 DECIMAL(13, 2),
CCC004*      LRWGESPFRENTE4                 DECIMAL(13, 2),
CCC004*      LRWGESPFRENTE5                 DECIMAL(13, 2),
CCC004*      LRWGESPFRENTEBKZ               CHAR(1),
CCC004*      LRWGARPFRENTANW                DECIMAL(13, 2),
CCC004*      LRWGARPFRENTANWBKZ             CHAR(1),
CCC004*      LRWGESPFRENTANW                DECIMAL(13, 2),
CCC004*      LRWGESPFRENTANWBKZ             CHAR(1),
CCC004*      LRWLSTPROZPFL1                 DECIMAL(3, 0),
CCC004*      LRWLSTPROZPFL2                 DECIMAL(3, 0),
CCC004*      LRWLSTPROZPFL3                 DECIMAL(3, 0),
CCC004*      LRWLSTPROZPFL4                 DECIMAL(3, 0),
CCC004*      LRWLSTPROZPFL5                 DECIMAL(3, 0)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE LEBENRECHENWERTE            *
      ******************************************************************
       01  LEBENRECHENWERTE-GESAMT.
        05 NI-LEBENRECHENWERTE-TAB.
           10 NI-LEBENRECHENWERTE-ELE PIC S9(04) COMP OCCURS 154 TIMES.
        05 NI-LEBENRECHENWERTE REDEFINES NI-LEBENRECHENWERTE-TAB.
           10 NI-LRWVERTRAGSID      PIC S9(4) COMP.
           10 NI-LRWVERSTARLFDNR    PIC S9(4) COMP.
           10 NI-LRWDATUMSTAND      PIC S9(4) COMP.
           10 NI-LRWABLEISTVOR      PIC S9(4) COMP.
           10 NI-LRWABLEISTVORWKZ   PIC S9(4) COMP.
           10 NI-LRWKAPABFVOR       PIC S9(4) COMP.
           10 NI-LRWKAPABFGAR       PIC S9(4) COMP.
           10 NI-LRWVSRTOD          PIC S9(4) COMP.
           10 NI-LRWUEBETTOD        PIC S9(4) COMP.
           10 NI-LRWBEITRFREIESUM   PIC S9(4) COMP.
           10 NI-LRWRKWERTGES       PIC S9(4) COMP.
           10 NI-LRWBONUSRENTE      PIC S9(4) COMP.
           10 NI-LRWKAPABFINKDYN    PIC S9(4) COMP.
           10 NI-LRWABLEISTSIM      PIC S9(4) COMP.
           10 NI-LRWVORAUSZDARLBETR PIC S9(4) COMP.
           10 NI-LRWMOEGLDARLBETR   PIC S9(4) COMP.
           10 NI-LRWGBURENTE        PIC S9(4) COMP.
           10 NI-LRWGBUZVRENTE      PIC S9(4) COMP.
           10 NI-LRWGUZVSUMME       PIC S9(4) COMP.
           10 NI-LRWERMABL          PIC S9(4) COMP.
           10 NI-LRWGARLSTABL       PIC S9(4) COMP.
           10 NI-LRWFORENDITE       PIC S9(4) COMP.
           10 NI-LRWZAHLBEITRAG     PIC S9(4) COMP.
           10 NI-LRWGKIRENTE        PIC S9(4) COMP.
           10 NI-LRWGPFRENTE        PIC S9(4) COMP.
           10 NI-LRWGPFRENTEANW     PIC S9(4) COMP.
           10 NI-LRWSUMBETBWRRK     PIC S9(4) COMP.
           10 NI-LRWBEGABRUFPHASE   PIC S9(4) COMP.
           10 NI-LRWVVGLSTABLSIM    PIC S9(4) COMP.
           10 NI-LRWVVGKAPABFSIM    PIC S9(4) COMP.
           10 NI-LRWABLEISTVORBKZ   PIC S9(4) COMP.
           10 NI-LRWKAPABFVORBKZ    PIC S9(4) COMP.
           10 NI-LRWKAPABFGARBKZ    PIC S9(4) COMP.
           10 NI-LRWVSRTODBKZ       PIC S9(4) COMP.
           10 NI-LRWUEBETTODBKZ     PIC S9(4) COMP.
           10 NI-LRWBETRFSUMBKZ     PIC S9(4) COMP.
           10 NI-LRWRKWERTGESBKZ    PIC S9(4) COMP.
           10 NI-LRWBONUSRENTEBKZ   PIC S9(4) COMP.
           10 NI-LRWKAPABFINKDBKZ   PIC S9(4) COMP.
           10 NI-LRWABLEISTSIMBKZ   PIC S9(4) COMP.
           10 NI-LRWVORAUSZDBBKZ    PIC S9(4) COMP.
           10 NI-LRWMOEGLDBBKZ      PIC S9(4) COMP.
           10 NI-LRWGBURENTEBKZ     PIC S9(4) COMP.
           10 NI-LRWGBUZVRENTEBKZ   PIC S9(4) COMP.
           10 NI-LRWGUZVSUMMEBKZ    PIC S9(4) COMP.
           10 NI-LRWERMABLBKZ       PIC S9(4) COMP.
           10 NI-LRWGARLSTABLBKZ    PIC S9(4) COMP.
           10 NI-LRWFORENDITEBKZ    PIC S9(4) COMP.
           10 NI-LRWZAHLBEITRBKZ    PIC S9(4) COMP.
           10 NI-LRWGKIRENTEBKZ     PIC S9(4) COMP.
           10 NI-LRWGPFRENTEBKZ     PIC S9(4) COMP.
           10 NI-LRWGPFRENTEANBKZ   PIC S9(4) COMP.
           10 NI-LRWSUMBETBWRKBKZ   PIC S9(4) COMP.
           10 NI-LRWVVGLSTASIBKZ    PIC S9(4) COMP.
           10 NI-LRWVVKAPABFSIBKZ   PIC S9(4) COMP.
           10 NI-LRWSUMDK           PIC S9(4) COMP.
           10 NI-LRWSUMDKBKZ        PIC S9(4) COMP.
           10 NI-LRWFMBTFRTT        PIC S9(4) COMP.
           10 NI-LRWFMBTFRTTINFO    PIC S9(4) COMP.
           10 NI-LRWBTFRISTWERT     PIC S9(4) COMP.
           10 NI-LRWBTFRMINDWERT    PIC S9(4) COMP.
           10 NI-LRWFRMGLBEGRENTE   PIC S9(4) COMP.
           10 NI-LRWGESEURENTE      PIC S9(4) COMP.
           10 NI-LRWGESEURENTEBKZ   PIC S9(4) COMP.
           10 NI-LRWGESEUZVRENTE    PIC S9(4) COMP.
           10 NI-LRWGESEUZVRNTBKZ   PIC S9(4) COMP.
           10 NI-LRWGESEUZBONRNT    PIC S9(4) COMP.
           10 NI-LRWGESEUZBONRBKZ   PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
           10 NI-LRWRKWHV           PIC S9(4) COMP.
           10 NI-LRWRKWHVBKZ        PIC S9(4) COMP.
           10 NI-LRWRKWBUZV         PIC S9(4) COMP.
           10 NI-LRWRKWBUZVBKZ      PIC S9(4) COMP.
           10 NI-LRWRKWTZV          PIC S9(4) COMP.
           10 NI-LRWRKWTZVBKZ       PIC S9(4) COMP.
           10 NI-LRWRKWPZV          PIC S9(4) COMP.
           10 NI-LRWRKWPZVBKZ       PIC S9(4) COMP.
           10 NI-LRWRKWEUZV         PIC S9(4) COMP.
           10 NI-LRWRKWEUZVBKZ      PIC S9(4) COMP.
           10 NI-LRWKONVGH          PIC S9(4) COMP.
           10 NI-LRWKONVGHBKZ       PIC S9(4) COMP.
           10 NI-LRWDATUMSTANDSIM   PIC S9(4) COMP.
           10 NI-LRWGARVORGLSTABL   PIC S9(4) COMP.
           10 NI-LRWGARVORGLSTABBKZ PIC S9(4) COMP.
           10 NI-LRWVORVORGLSTABL   PIC S9(4) COMP.
           10 NI-LRWVORVORGLSTABBKZ PIC S9(4) COMP.
           10 NI-LRWGARVORGKAPABF   PIC S9(4) COMP.
           10 NI-LRWGARVORGKAPABBKZ PIC S9(4) COMP.
           10 NI-LRWVORVORGKAPABF   PIC S9(4) COMP.
           10 NI-LRWVORVORGKAPABBKZ PIC S9(4) COMP.
           10 NI-LRWABZUG           PIC S9(4) COMP.
           10 NI-LRWABZUGBKZ        PIC S9(4) COMP.
           10 NI-LRWAUSZAHLBETRAG   PIC S9(4) COMP.
           10 NI-LRWAUSZAHLBETBKZ   PIC S9(4) COMP.
           10 NI-LRWSUMRKWNATAZ     PIC S9(4) COMP.
           10 NI-LRWSUMRKWNATAZBKZ  PIC S9(4) COMP.
           10 NI-LRWVORLSTABLNATAZ  PIC S9(4) COMP.
           10 NI-LRWVORLSTABLNABKZ  PIC S9(4) COMP.
           10 NI-LRWVORKAPABFTNATAZ PIC S9(4) COMP.
           10 NI-LRWVORKAPABFNABKZ  PIC S9(4) COMP.
           10 NI-LRWVERMOEGTOPF     PIC S9(4) COMP.
           10 NI-LRWVERMOEGTOPBKZ   PIC S9(4) COMP.
           10 NI-LRWEINKOMTOPF      PIC S9(4) COMP.
           10 NI-LRWEINKOMTOPFBKZ   PIC S9(4) COMP.
           10 NI-LRWBSPBEGRNTZHLG   PIC S9(4) COMP.
           10 NI-LRWSUMTEILRENTEN   PIC S9(4) COMP.
           10 NI-LRWSUMTEILRNTBKZ   PIC S9(4) COMP.
           10 NI-LRWGARVGABLSTABL   PIC S9(4) COMP.
           10 NI-LRWGARVGABLSTKZ    PIC S9(4) COMP.
           10 NI-LRWGARVGABLKAPABF  PIC S9(4) COMP.
           10 NI-LRWGARVGABLKAPKZ   PIC S9(4) COMP.
           10 NI-LRWSICHERHRESERV   PIC S9(4) COMP.
           10 NI-LRWSICHERHRESBKZ   PIC S9(4) COMP.
           10 NI-LRWVSRSK           PIC S9(4) COMP.
           10 NI-LRWVSRSKBKZ        PIC S9(4) COMP.
           10 NI-LRWVSRSKUEB        PIC S9(4) COMP.
           10 NI-LRWVSRSKUEBBKZ     PIC S9(4) COMP.
           10 NI-LRWRKWSK           PIC S9(4) COMP.
           10 NI-LRWRKWSKBKZ        PIC S9(4) COMP.
           10 NI-LRWBTFRSUMSK       PIC S9(4) COMP.
           10 NI-LRWBTFRSUMSKBKZ    PIC S9(4) COMP.
           10 NI-LRWFMBTFRTTSK      PIC S9(4) COMP.
           10 NI-LRWFMBTFRTTSKBKZ   PIC S9(4) COMP.
           10 NI-LRWBTFRISTWERTSK   PIC S9(4) COMP.
           10 NI-LRWBTFRMINDWRTSK   PIC S9(4) COMP.
           10 NI-LRWKONSORTIALKZ    PIC S9(4) COMP.
           10 NI-LRWGARABLRNTPRL    PIC S9(4) COMP.
           10 NI-LRWGARABRNTPRLBKZ  PIC S9(4) COMP.
           10 NI-LRWAKTVTRWERT      PIC S9(4) COMP.
           10 NI-LRWAKTVTRWERTBKZ   PIC S9(4) COMP.
           10 NI-LRWGBURENTANW      PIC S9(4) COMP.
           10 NI-LRWGBURENTANWBKZ   PIC S9(4) COMP.
           10 NI-LRWPFSTAFFELKZN    PIC S9(4) COMP.
           10 NI-LRWGARPFRENTE1     PIC S9(4) COMP.
           10 NI-LRWGARPFRENTE2     PIC S9(4) COMP.
           10 NI-LRWGARPFRENTE3     PIC S9(4) COMP.
           10 NI-LRWGARPFRENTE4     PIC S9(4) COMP.
           10 NI-LRWGARPFRENTE5     PIC S9(4) COMP.
           10 NI-LRWGARPFRENTEBKZ   PIC S9(4) COMP.
           10 NI-LRWGESPFRENTE1     PIC S9(4) COMP.
           10 NI-LRWGESPFRENTE2     PIC S9(4) COMP.
           10 NI-LRWGESPFRENTE3     PIC S9(4) COMP.
           10 NI-LRWGESPFRENTE4     PIC S9(4) COMP.
           10 NI-LRWGESPFRENTE5     PIC S9(4) COMP.
           10 NI-LRWGESPFRENTEBKZ   PIC S9(4) COMP.
           10 NI-LRWGARPFRENTANW    PIC S9(4) COMP.
           10 NI-LRWGARPFRENTANWBKZ PIC S9(4) COMP.
           10 NI-LRWGESPFRENTANW    PIC S9(4) COMP.
           10 NI-LRWGESPFRENTANWBKZ PIC S9(4) COMP.
           10 NI-LRWLSTPROZPFL1     PIC S9(4) COMP.
           10 NI-LRWLSTPROZPFL2     PIC S9(4) COMP.
           10 NI-LRWLSTPROZPFL3     PIC S9(4) COMP.
           10 NI-LRWLSTPROZPFL4     PIC S9(4) COMP.
           10 NI-LRWLSTPROZPFL5     PIC S9(4) COMP.
        05 DCL-LEBENRECHENWERTE.
           10 LRWVERTRAGSID        PIC X(13).
           10 LRWVERSTARLFDNR      PIC S9(4) USAGE COMP.
           10 LRWDATUMSTAND        PIC X(10).
           10 LRWABLEISTVOR        PIC S9(9)V9(2) USAGE COMP-3.
           10 LRWABLEISTVORWKZ     PIC X(1).
           10 LRWKAPABFVOR         PIC S9(9)V9(2) USAGE COMP-3.
           10 LRWKAPABFGAR         PIC S9(9)V9(2) USAGE COMP-3.
           10 LRWVSRTOD            PIC S9(9)V9(2) USAGE COMP-3.
           10 LRWUEBETTOD          PIC S9(9)V9(2) USAGE COMP-3.
           10 LRWBEITRFREIESUM     PIC S9(9)V9(2) USAGE COMP-3.
           10 LRWRKWERTGES         PIC S9(9)V9(2) USAGE COMP-3.
           10 LRWBONUSRENTE        PIC S9(9)V9(2) USAGE COMP-3.
           10 LRWKAPABFINKDYN      PIC S9(9)V9(2) USAGE COMP-3.
           10 LRWABLEISTSIM        PIC S9(9)V9(2) USAGE COMP-3.
           10 LRWVORAUSZDARLBETR   PIC S9(9)V9(2) USAGE COMP-3.
           10 LRWMOEGLDARLBETR     PIC S9(9)V9(2) USAGE COMP-3.
           10 LRWGBURENTE          PIC S9(7)V9(2) USAGE COMP-3.
           10 LRWGBUZVRENTE        PIC S9(7)V9(2) USAGE COMP-3.
           10 LRWGUZVSUMME         PIC S9(11)V9(2) USAGE COMP-3.
           10 LRWERMABL            PIC X(10).
           10 LRWGARLSTABL         PIC S9(11)V9(2) USAGE COMP-3.
           10 LRWFORENDITE         PIC S9(3)V9(2) USAGE COMP-3.
           10 LRWZAHLBEITRAG       PIC S9(9)V9(2) USAGE COMP-3.
           10 LRWGKIRENTE          PIC S9(9)V9(2) USAGE COMP-3.
           10 LRWGPFRENTE          PIC S9(9)V9(2) USAGE COMP-3.
           10 LRWGPFRENTEANW       PIC S9(9)V9(2) USAGE COMP-3.
           10 LRWSUMBETBWRRK       PIC S9(11)V9(2) USAGE COMP-3.
           10 LRWBEGABRUFPHASE     PIC X(10).
           10 LRWVVGLSTABLSIM      PIC S9(11)V9(2) USAGE COMP-3.
           10 LRWVVGKAPABFSIM      PIC S9(11)V9(2) USAGE COMP-3.
           10 LRWABLEISTVORBKZ     PIC X(1).
           10 LRWKAPABFVORBKZ      PIC X(1).
           10 LRWKAPABFGARBKZ      PIC X(1).
           10 LRWVSRTODBKZ         PIC X(1).
           10 LRWUEBETTODBKZ       PIC X(1).
           10 LRWBETRFSUMBKZ       PIC X(1).
           10 LRWRKWERTGESBKZ      PIC X(1).
           10 LRWBONUSRENTEBKZ     PIC X(1).
           10 LRWKAPABFINKDBKZ     PIC X(1).
           10 LRWABLEISTSIMBKZ     PIC X(1).
           10 LRWVORAUSZDBBKZ      PIC X(1).
           10 LRWMOEGLDBBKZ        PIC X(1).
           10 LRWGBURENTEBKZ       PIC X(1).
           10 LRWGBUZVRENTEBKZ     PIC X(1).
           10 LRWGUZVSUMMEBKZ      PIC X(1).
           10 LRWERMABLBKZ         PIC X(1).
           10 LRWGARLSTABLBKZ      PIC X(1).
           10 LRWFORENDITEBKZ      PIC X(1).
           10 LRWZAHLBEITRBKZ      PIC X(1).
           10 LRWGKIRENTEBKZ       PIC X(1).
           10 LRWGPFRENTEBKZ       PIC X(1).
           10 LRWGPFRENTEANBKZ     PIC X(1).
           10 LRWSUMBETBWRKBKZ     PIC X(1).
           10 LRWVVGLSTASIBKZ      PIC X(1).
           10 LRWVVKAPABFSIBKZ     PIC X(1).
           10 LRWSUMDK             PIC S9(11)V9(2) USAGE COMP-3.
           10 LRWSUMDKBKZ          PIC X(1).
           10 LRWFMBTFRTT          PIC X(10).
           10 LRWFMBTFRTTINFO      PIC X(1).
           10 LRWBTFRISTWERT       PIC S9(11)V9(2) USAGE COMP-3.
           10 LRWBTFRMINDWERT      PIC S9(11)V9(2) USAGE COMP-3.
           10 LRWFRMGLBEGRENTE     PIC X(10).
           10 LRWGESEURENTE        PIC S9(7)V9(2) USAGE COMP-3.
           10 LRWGESEURENTEBKZ     PIC X(1).
           10 LRWGESEUZVRENTE      PIC S9(7)V9(2) USAGE COMP-3.
           10 LRWGESEUZVRNTBKZ     PIC X(1).
           10 LRWGESEUZBONRNT      PIC S9(7)V9(2) USAGE COMP-3.
           10 LRWGESEUZBONRBKZ     PIC X(1).
           10 LETZTEAENDERUNG      PIC X(26).
           10 LRWRKWHV             PIC S9(11)V9(2) USAGE COMP-3.
           10 LRWRKWHVBKZ          PIC X(1).
           10 LRWRKWBUZV           PIC S9(11)V9(2) USAGE COMP-3.
           10 LRWRKWBUZVBKZ        PIC X(1).
           10 LRWRKWTZV            PIC S9(11)V9(2) USAGE COMP-3.
           10 LRWRKWTZVBKZ         PIC X(1).
           10 LRWRKWPZV            PIC S9(11)V9(2) USAGE COMP-3.
           10 LRWRKWPZVBKZ         PIC X(1).
           10 LRWRKWEUZV           PIC S9(11)V9(2) USAGE COMP-3.
           10 LRWRKWEUZVBKZ        PIC X(1).
           10 LRWKONVGH            PIC S9(11)V9(2) USAGE COMP-3.
           10 LRWKONVGHBKZ         PIC X(1).
           10 LRWDATUMSTANDSIM     PIC X(10).
           10 LRWGARVORGLSTABL     PIC S9(11)V9(2) USAGE COMP-3.
           10 LRWGARVORGLSTABBKZ   PIC X(1).
           10 LRWVORVORGLSTABL     PIC S9(11)V9(2) USAGE COMP-3.
           10 LRWVORVORGLSTABBKZ   PIC X(1).
           10 LRWGARVORGKAPABF     PIC S9(11)V9(2) USAGE COMP-3.
           10 LRWGARVORGKAPABBKZ   PIC X(1).
           10 LRWVORVORGKAPABF     PIC S9(11)V9(2) USAGE COMP-3.
           10 LRWVORVORGKAPABBKZ   PIC X(1).
           10 LRWABZUG             PIC S9(11)V9(2) USAGE COMP-3.
           10 LRWABZUGBKZ          PIC X(1).
           10 LRWAUSZAHLBETRAG     PIC S9(11)V9(2) USAGE COMP-3.
           10 LRWAUSZAHLBETBKZ     PIC X(1).
           10 LRWSUMRKWNATAZ       PIC S9(11)V9(2) USAGE COMP-3.
           10 LRWSUMRKWNATAZBKZ    PIC X(1).
           10 LRWVORLSTABLNATAZ    PIC S9(11)V9(2) USAGE COMP-3.
           10 LRWVORLSTABLNABKZ    PIC X(1).
           10 LRWVORKAPABFTNATAZ   PIC S9(11)V9(2) USAGE COMP-3.
           10 LRWVORKAPABFNABKZ    PIC X(1).
           10 LRWVERMOEGTOPF       PIC S9(11)V9(2) USAGE COMP-3.
           10 LRWVERMOEGTOPBKZ     PIC X(1).
           10 LRWEINKOMTOPF        PIC S9(11)V9(2) USAGE COMP-3.
           10 LRWEINKOMTOPFBKZ     PIC X(1).
           10 LRWBSPBEGRNTZHLG     PIC X(10).
           10 LRWSUMTEILRENTEN     PIC S9(11)V9(2) USAGE COMP-3.
           10 LRWSUMTEILRNTBKZ     PIC X(1).
           10 LRWGARVGABLSTABL     PIC S9(11)V9(2) USAGE COMP-3.
           10 LRWGARVGABLSTKZ      PIC X(1).
           10 LRWGARVGABLKAPABF    PIC S9(11)V9(2) USAGE COMP-3.
           10 LRWGARVGABLKAPKZ     PIC X(1).
           10 LRWSICHERHRESERV     PIC S9(11)V9(2) USAGE COMP-3.
           10 LRWSICHERHRESBKZ     PIC X(1).
           10 LRWVSRSK             PIC S9(11)V9(2) USAGE COMP-3.
           10 LRWVSRSKBKZ          PIC X(1).
           10 LRWVSRSKUEB          PIC S9(11)V9(2) USAGE COMP-3.
           10 LRWVSRSKUEBBKZ       PIC X(1).
           10 LRWRKWSK             PIC S9(11)V9(2) USAGE COMP-3.
           10 LRWRKWSKBKZ          PIC X(1).
           10 LRWBTFRSUMSK         PIC S9(11)V9(2) USAGE COMP-3.
           10 LRWBTFRSUMSKBKZ      PIC X(1).
           10 LRWFMBTFRTTSK        PIC X(10).
           10 LRWFMBTFRTTSKBKZ     PIC X(1).
           10 LRWBTFRISTWERTSK     PIC S9(11)V9(2) USAGE COMP-3.
           10 LRWBTFRMINDWRTSK     PIC S9(11)V9(2) USAGE COMP-3.
           10 LRWKONSORTIALKZ      PIC X(1).
           10 LRWGARABLRNTPRL      PIC S9(11)V9(2) USAGE COMP-3.
           10 LRWGARABRNTPRLBKZ    PIC X(1).
           10 LRWAKTVTRWERT        PIC S9(11)V9(2) USAGE COMP-3.
           10 LRWAKTVTRWERTBKZ     PIC X(1).
           10 LRWGBURENTANW        PIC S9(11)V9(2) USAGE COMP-3.
           10 LRWGBURENTANWBKZ     PIC X(1).
           10 LRWPFSTAFFELKZN      PIC X(1).
           10 LRWGARPFRENTE1       PIC S9(11)V9(2) USAGE COMP-3.
           10 LRWGARPFRENTE2       PIC S9(11)V9(2) USAGE COMP-3.
           10 LRWGARPFRENTE3       PIC S9(11)V9(2) USAGE COMP-3.
           10 LRWGARPFRENTE4       PIC S9(11)V9(2) USAGE COMP-3.
           10 LRWGARPFRENTE5       PIC S9(11)V9(2) USAGE COMP-3.
           10 LRWGARPFRENTEBKZ     PIC X(1).
           10 LRWGESPFRENTE1       PIC S9(11)V9(2) USAGE COMP-3.
           10 LRWGESPFRENTE2       PIC S9(11)V9(2) USAGE COMP-3.
           10 LRWGESPFRENTE3       PIC S9(11)V9(2) USAGE COMP-3.
           10 LRWGESPFRENTE4       PIC S9(11)V9(2) USAGE COMP-3.
           10 LRWGESPFRENTE5       PIC S9(11)V9(2) USAGE COMP-3.
           10 LRWGESPFRENTEBKZ     PIC X(1).
           10 LRWGARPFRENTANW      PIC S9(11)V9(2) USAGE COMP-3.
           10 LRWGARPFRENTANWBKZ   PIC X(1).
           10 LRWGESPFRENTANW      PIC S9(11)V9(2) USAGE COMP-3.
           10 LRWGESPFRENTANWBKZ   PIC X(1).
           10 LRWLSTPROZPFL1       PIC S9(3)V USAGE COMP-3.
           10 LRWLSTPROZPFL2       PIC S9(3)V USAGE COMP-3.
           10 LRWLSTPROZPFL3       PIC S9(3)V USAGE COMP-3.
           10 LRWLSTPROZPFL4       PIC S9(3)V USAGE COMP-3.
           10 LRWLSTPROZPFL5       PIC S9(3)V USAGE COMP-3.
      ************************** ENDE-DCLGEN *************************

