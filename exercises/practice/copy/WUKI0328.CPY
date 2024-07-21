      *****************************************************************
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0328.CPY  $
      *    $Revision:   2.23  $
      *
      *    $Log:   L:/mf/CPY/WUKI0328.CPv  $
      *
      *   Rev 2.23   Feb 04 2020 12:58:44   t13ac20
      *AKI01757: VDL-Leben: HOST Verarbeitung und Speicherung der D
      *aten für die neuen Pflegetarife (pflegeSmart und pflegePro) 
      *b2
      *
      *   Rev 2.22   Feb 04 2020 07:39:52   t13ac20
      *AKI01757: VDL-Leben: HOST Verarbeitung und Speicherung der D
      *aten für die neuen Pflegetarife (pflegeSmart und pflegePro)
      *b2
      *
      *   Rev 2.21   Aug 14 2018 09:31:44   t13ac17
      *AKI01605: G20 EVWS - neue Produkte zum RT 09/2018 Standard-OZP
      *
      *   Rev 2.20   Sep 20 2017 15:18:24   t13ac20
      *AKI01503: Bereinigung AML-Tabellen
      *
      *   Rev 2.19   Mar 15 2017 14:30:54   t13ex27
      *AKI01492: AML-Änderungen zum RT 03/2017OZPs
      *
      *   Rev 2.18   Jan 17 2017 10:58:06   t13ex27
      *AKI01488: AM-Leben Konsortialverträge
      *
      *   Rev 2.17   Mar 18 2015 17:19:36   t13ex48
      *AKI01349: AMLeben: VEP-Bestandsmasken: OZP LRW
      *
      *   Rev 2.16   Jun 29 2014 21:07:20   t13ex48
      *AKI01296: VERMÖGENS- & EINKOMMENSPLAN - VEP: Cobol RW
      *
      *   Rev 2.14   Mar 28 2014 01:18:08   t13ex48
      *AKI01153: Neues Verfahren: Aktualisierung AML-Rechenwerte
      *
      *   Rev 2.13   Aug 31 2011 12:41:16   t13ex48
      *AKI01065: AMLeben - Einführung Impulsprodukt - VASP: Source RW
      *
      *   Rev 2.12   Jan 04 2011 13:45:20   t13ex27
      *AKI00961: Anzeige "Frühester Rentenbeginn" in KI-Vertragsdat
      *en-OZP
      *
      *   Rev 2.11   Aug 25 2009 11:32:58   t13ex27
      *AKI00863: DB-Änderung für Rel. 4.58 AML
      *
      *   Rev 2.10   Aug 10 2009 14:57:16   t13ex27
      *AKI00843: DB-Änderungen für Rel. 4.60 - AML
      *
      *   Rev 2.9   16 Dec 2008 11:29:28   t13ex27
      *AKI00773: DB-Änderung für Rel. 4.44
      *
      *   Rev 2.8   Oct 12 2006 13:36:16   t13ex27
      *AKI00555: DB-Änderung für FSAEIGENVERTRÄGE-PMKI0079
      *
      *   Rev 2.7   May 24 2004 11:44:52   t13ex27
      *DB-Änderung für Fix 61 PMKI0079 und WUKI's
      *
      *   Rev 2.6   Jun 11 2003 13:11:44   t13ex27
      *AKI00351 - DB-Änderung für Fix 41
      *
      *   Rev 2.5   25 Jul 2001 10:33:16   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-WUKI0328.
           10 LRWVERTRAGSID                 PIC  X(0013).
           10 LRWVERSTARLFDNR               PIC S9(4) COMP.
           10 LRWDATUMSTAND                 PIC  X(010).
           10 NIL-LRWDATUMSTAND             PIC  X(01).
           10 LRWABLEISTVOR                 PIC S9(9)V9(2) COMP-3.
           10 NIL-LRWABLEISTVOR             PIC  X(01).
           10 LRWABLEISTVORWKZ              PIC  X(0001).
           10 LRWKAPABFVOR                  PIC S9(9)V9(2) COMP-3.
           10 NIL-LRWKAPABFVOR              PIC  X(01).
           10 LRWKAPABFGAR                  PIC S9(9)V9(2) COMP-3.
           10 NIL-LRWKAPABFGAR              PIC  X(01).
           10 LRWVSRTOD                     PIC S9(9)V9(2) COMP-3.
           10 NIL-LRWVSRTOD                 PIC  X(01).
           10 LRWUEBETTOD                   PIC S9(9)V9(2) COMP-3.
           10 NIL-LRWUEBETTOD               PIC  X(01).
           10 LRWBEITRFREIESUM              PIC S9(9)V9(2) COMP-3.
           10 NIL-LRWBEITRFREIESUM          PIC  X(01).
           10 LRWRKWERTGES                  PIC S9(9)V9(2) COMP-3.
           10 NIL-LRWRKWERTGES              PIC  X(01).
           10 LRWBONUSRENTE                 PIC S9(9)V9(2) COMP-3.
           10 NIL-LRWBONUSRENTE             PIC  X(01).
           10 LRWKAPABFINKDYN               PIC S9(9)V9(2) COMP-3.
           10 NIL-LRWKAPABFINKDYN           PIC  X(01).
           10 LRWABLEISTSIM                 PIC S9(9)V9(2) COMP-3.
           10 NIL-LRWABLEISTSIM             PIC  X(01).
           10 LRWVORAUSZDARLBETR            PIC S9(9)V9(2) COMP-3.
           10 NIL-LRWVORAUSZDARLBETR        PIC  X(01).
           10 LRWMOEGLDARLBETR              PIC S9(9)V9(2) COMP-3.
           10 NIL-LRWMOEGLDARLBETR          PIC  X(01).
           10 LRWGBURENTE                   PIC S9(7)V9(2) COMP-3.
           10 NIL-LRWGBURENTE               PIC  X(01).
           10 LRWGBUZVRENTE                 PIC S9(7)V9(2) COMP-3.
           10 NIL-LRWGBUZVRENTE             PIC  X(01).
           10 LRWGUZVSUMME                  PIC S9(11)V9(2) COMP-3.
           10 NIL-LRWGUZVSUMME              PIC  X(01).
           10 LRWERMABL                     PIC  X(010).
           10 NIL-LRWERMABL                 PIC  X(01).
           10 LRWGARLSTABL                  PIC S9(11)V9(2) COMP-3.
           10 NIL-LRWGARLSTABL              PIC  X(01).
           10 LRWFORENDITE                  PIC S9(3)V9(2) COMP-3.
           10 NIL-LRWFORENDITE              PIC  X(01).
           10 LRWZAHLBEITRAG                PIC S9(9)V9(2) COMP-3.
           10 NIL-LRWZAHLBEITRAG            PIC  X(01).
           10 LRWGKIRENTE                   PIC S9(9)V9(2) COMP-3.
           10 NIL-LRWGKIRENTE               PIC  X(01).
           10 LRWGPFRENTE                   PIC S9(9)V9(2) COMP-3.
           10 NIL-LRWGPFRENTE               PIC  X(01).
           10 LRWGPFRENTEANW                PIC S9(9)V9(2) COMP-3.
           10 NIL-LRWGPFRENTEANW            PIC  X(01).
           10 LRWSUMBETBWRRK                PIC S9(11)V9(2) COMP-3.
           10 NIL-LRWSUMBETBWRRK            PIC  X(01).
           10 LRWBEGABRUFPHASE              PIC  X(010).
           10 NIL-LRWBEGABRUFPHASE          PIC  X(01).
           10 LRWVVGLSTABLSIM               PIC S9(11)V9(2) COMP-3.
           10 NIL-LRWVVGLSTABLSIM           PIC  X(01).
           10 LRWVVGKAPABFSIM               PIC S9(11)V9(2) COMP-3.
           10 NIL-LRWVVGKAPABFSIM           PIC  X(01).
           10 LRWABLEISTVORBKZ              PIC  X(0001).
           10 NIL-LRWABLEISTVORBKZ          PIC  X(01).
           10 LRWKAPABFVORBKZ               PIC  X(0001).
           10 NIL-LRWKAPABFVORBKZ           PIC  X(01).
           10 LRWKAPABFGARBKZ               PIC  X(0001).
           10 NIL-LRWKAPABFGARBKZ           PIC  X(01).
           10 LRWVSRTODBKZ                  PIC  X(0001).
           10 NIL-LRWVSRTODBKZ              PIC  X(01).
           10 LRWUEBETTODBKZ                PIC  X(0001).
           10 NIL-LRWUEBETTODBKZ            PIC  X(01).
           10 LRWBETRFSUMBKZ                PIC  X(0001).
           10 NIL-LRWBETRFSUMBKZ            PIC  X(01).
           10 LRWRKWERTGESBKZ               PIC  X(0001).
           10 NIL-LRWRKWERTGESBKZ           PIC  X(01).
           10 LRWBONUSRENTEBKZ              PIC  X(0001).
           10 NIL-LRWBONUSRENTEBKZ          PIC  X(01).
           10 LRWKAPABFINKDBKZ              PIC  X(0001).
           10 NIL-LRWKAPABFINKDBKZ          PIC  X(01).
           10 LRWABLEISTSIMBKZ              PIC  X(0001).
           10 NIL-LRWABLEISTSIMBKZ          PIC  X(01).
           10 LRWVORAUSZDBBKZ               PIC  X(0001).
           10 NIL-LRWVORAUSZDBBKZ           PIC  X(01).
           10 LRWMOEGLDBBKZ                 PIC  X(0001).
           10 NIL-LRWMOEGLDBBKZ             PIC  X(01).
           10 LRWGBURENTEBKZ                PIC  X(0001).
           10 NIL-LRWGBURENTEBKZ            PIC  X(01).
           10 LRWGBUZVRENTEBKZ              PIC  X(0001).
           10 NIL-LRWGBUZVRENTEBKZ          PIC  X(01).
           10 LRWGUZVSUMMEBKZ               PIC  X(0001).
           10 NIL-LRWGUZVSUMMEBKZ           PIC  X(01).
           10 LRWERMABLBKZ                  PIC  X(0001).
           10 NIL-LRWERMABLBKZ              PIC  X(01).
           10 LRWGARLSTABLBKZ               PIC  X(0001).
           10 NIL-LRWGARLSTABLBKZ           PIC  X(01).
           10 LRWFORENDITEBKZ               PIC  X(0001).
           10 NIL-LRWFORENDITEBKZ           PIC  X(01).
           10 LRWZAHLBEITRBKZ               PIC  X(0001).
           10 NIL-LRWZAHLBEITRBKZ           PIC  X(01).
           10 LRWGKIRENTEBKZ                PIC  X(0001).
           10 NIL-LRWGKIRENTEBKZ            PIC  X(01).
           10 LRWGPFRENTEBKZ                PIC  X(0001).
           10 NIL-LRWGPFRENTEBKZ            PIC  X(01).
           10 LRWGPFRENTEANBKZ              PIC  X(0001).
           10 NIL-LRWGPFRENTEANBKZ          PIC  X(01).
           10 LRWSUMBETBWRKBKZ              PIC  X(0001).
           10 NIL-LRWSUMBETBWRKBKZ          PIC  X(01).
           10 LRWVVGLSTASIBKZ               PIC  X(0001).
           10 NIL-LRWVVGLSTASIBKZ           PIC  X(01).
           10 LRWVVKAPABFSIBKZ              PIC  X(0001).
           10 NIL-LRWVVKAPABFSIBKZ          PIC  X(01).
           10 LRWSUMDK                      PIC S9(11)V9(2) COMP-3.
           10 NIL-LRWSUMDK                  PIC  X(01).
           10 LRWSUMDKBKZ                   PIC  X(0001).
           10 NIL-LRWSUMDKBKZ               PIC  X(01).
           10 LRWFMBTFRTT                   PIC  X(010).
           10 NIL-LRWFMBTFRTT               PIC  X(01).
           10 LRWFMBTFRTTINFO               PIC  X(0001).
           10 NIL-LRWFMBTFRTTINFO           PIC  X(01).
           10 LRWBTFRISTWERT                PIC S9(11)V9(2) COMP-3.
           10 NIL-LRWBTFRISTWERT            PIC  X(01).
           10 LRWBTFRMINDWERT               PIC S9(11)V9(2) COMP-3.
           10 NIL-LRWBTFRMINDWERT           PIC  X(01).
           10 LRWFRMGLBEGRENTE              PIC  X(010).
           10 NIL-LRWFRMGLBEGRENTE          PIC  X(01).
           10 LRWGESEURENTE                 PIC S9(7)V9(2) COMP-3.
           10 NIL-LRWGESEURENTE             PIC  X(01).
           10 LRWGESEURENTEBKZ              PIC  X(0001).
           10 NIL-LRWGESEURENTEBKZ          PIC  X(01).
           10 LRWGESEUZVRENTE               PIC S9(7)V9(2) COMP-3.
           10 NIL-LRWGESEUZVRENTE           PIC  X(01).
           10 LRWGESEUZVRNTBKZ              PIC  X(0001).
           10 NIL-LRWGESEUZVRNTBKZ          PIC  X(01).
           10 LRWGESEUZBONRNT               PIC S9(7)V9(2) COMP-3.
           10 NIL-LRWGESEUZBONRNT           PIC  X(01).
           10 LRWGESEUZBONRBKZ              PIC  X(0001).
           10 NIL-LRWGESEUZBONRBKZ          PIC  X(01).
           10 LETZTEAENDERUNG               PIC  X(0026).
           10 NIL-LETZTEAENDERUNG           PIC  X(01).
           10 LRWRKWHV                      PIC S9(11)V9(2) COMP-3.
           10 NIL-LRWRKWHV                  PIC  X(01).
           10 LRWRKWHVBKZ                   PIC  X(0001).
           10 NIL-LRWRKWHVBKZ               PIC  X(01).
           10 LRWRKWBUZV                    PIC S9(11)V9(2) COMP-3.
           10 NIL-LRWRKWBUZV                PIC  X(01).
           10 LRWRKWBUZVBKZ                 PIC  X(0001).
           10 NIL-LRWRKWBUZVBKZ             PIC  X(01).
           10 LRWRKWTZV                     PIC S9(11)V9(2) COMP-3.
           10 NIL-LRWRKWTZV                 PIC  X(01).
           10 LRWRKWTZVBKZ                  PIC  X(0001).
           10 NIL-LRWRKWTZVBKZ              PIC  X(01).
           10 LRWRKWPZV                     PIC S9(11)V9(2) COMP-3.
           10 NIL-LRWRKWPZV                 PIC  X(01).
           10 LRWRKWPZVBKZ                  PIC  X(0001).
           10 NIL-LRWRKWPZVBKZ              PIC  X(01).
           10 LRWRKWEUZV                    PIC S9(11)V9(2) COMP-3.
           10 NIL-LRWRKWEUZV                PIC  X(01).
           10 LRWRKWEUZVBKZ                 PIC  X(0001).
           10 NIL-LRWRKWEUZVBKZ             PIC  X(01).
           10 LRWKONVGH                     PIC S9(11)V9(2) COMP-3.
           10 NIL-LRWKONVGH                 PIC  X(01).
           10 LRWKONVGHBKZ                  PIC  X(0001).
           10 NIL-LRWKONVGHBKZ              PIC  X(01).
           10 LRWDATUMSTANDSIM              PIC  X(010).
           10 NIL-LRWDATUMSTANDSIM          PIC  X(01).
           10 LRWGARVORGLSTABL              PIC S9(11)V9(2) COMP-3.
           10 NIL-LRWGARVORGLSTABL          PIC  X(01).
           10 LRWGARVORGLSTABBKZ            PIC  X(0001).
           10 NIL-LRWGARVORGLSTABBKZ        PIC  X(01).
           10 LRWVORVORGLSTABL              PIC S9(11)V9(2) COMP-3.
           10 NIL-LRWVORVORGLSTABL          PIC  X(01).
           10 LRWVORVORGLSTABBKZ            PIC  X(0001).
           10 NIL-LRWVORVORGLSTABBKZ        PIC  X(01).
           10 LRWGARVORGKAPABF              PIC S9(11)V9(2) COMP-3.
           10 NIL-LRWGARVORGKAPABF          PIC  X(01).
           10 LRWGARVORGKAPABBKZ            PIC  X(0001).
           10 NIL-LRWGARVORGKAPABBKZ        PIC  X(01).
           10 LRWVORVORGKAPABF              PIC S9(11)V9(2) COMP-3.
           10 NIL-LRWVORVORGKAPABF          PIC  X(01).
           10 LRWVORVORGKAPABBKZ            PIC  X(0001).
           10 NIL-LRWVORVORGKAPABBKZ        PIC  X(01).
           10 LRWABZUG                      PIC S9(11)V9(2) COMP-3.
           10 NIL-LRWABZUG                  PIC  X(01).
           10 LRWABZUGBKZ                   PIC  X(0001).
           10 NIL-LRWABZUGBKZ               PIC  X(01).
           10 LRWAUSZAHLBETRAG              PIC S9(11)V9(2) COMP-3.
           10 NIL-LRWAUSZAHLBETRAG          PIC  X(01).
           10 LRWAUSZAHLBETBKZ              PIC  X(0001).
           10 NIL-LRWAUSZAHLBETBKZ          PIC  X(01).
           10 LRWSUMRKWNATAZ                PIC S9(11)V9(2) COMP-3.
           10 NIL-LRWSUMRKWNATAZ            PIC  X(01).
           10 LRWSUMRKWNATAZBKZ             PIC  X(0001).
           10 NIL-LRWSUMRKWNATAZBKZ         PIC  X(01).
           10 LRWVORLSTABLNATAZ             PIC S9(11)V9(2) COMP-3.
           10 NIL-LRWVORLSTABLNATAZ         PIC  X(01).
           10 LRWVORLSTABLNABKZ             PIC  X(0001).
           10 NIL-LRWVORLSTABLNABKZ         PIC  X(01).
           10 LRWVORKAPABFTNATAZ            PIC S9(11)V9(2) COMP-3.
           10 NIL-LRWVORKAPABFTNATAZ        PIC  X(01).
           10 LRWVORKAPABFNABKZ             PIC  X(0001).
           10 NIL-LRWVORKAPABFNABKZ         PIC  X(01).
           10 LRWVERMOEGTOPF                PIC S9(11)V9(2) COMP-3.
           10 NIL-LRWVERMOEGTOPF            PIC  X(01).
           10 LRWVERMOEGTOPBKZ              PIC  X(0001).
           10 NIL-LRWVERMOEGTOPBKZ          PIC  X(01).
           10 LRWEINKOMTOPF                 PIC S9(11)V9(2) COMP-3.
           10 NIL-LRWEINKOMTOPF             PIC  X(01).
           10 LRWEINKOMTOPFBKZ              PIC  X(0001).
           10 NIL-LRWEINKOMTOPFBKZ          PIC  X(01).
           10 LRWBSPBEGRNTZHLG              PIC  X(010).
           10 NIL-LRWBSPBEGRNTZHLG          PIC  X(01).
           10 LRWSUMTEILRENTEN              PIC S9(11)V9(2) COMP-3.
           10 NIL-LRWSUMTEILRENTEN          PIC  X(01).
           10 LRWSUMTEILRNTBKZ              PIC  X(0001).
           10 NIL-LRWSUMTEILRNTBKZ          PIC  X(01).
           10 LRWGARVGABLSTABL              PIC S9(11)V9(2) COMP-3.
           10 NIL-LRWGARVGABLSTABL          PIC  X(01).
           10 LRWGARVGABLSTKZ               PIC  X(0001).
           10 NIL-LRWGARVGABLSTKZ           PIC  X(01).
           10 LRWGARVGABLKAPABF             PIC S9(11)V9(2) COMP-3.
           10 NIL-LRWGARVGABLKAPABF         PIC  X(01).
           10 LRWGARVGABLKAPKZ              PIC  X(0001).
           10 NIL-LRWGARVGABLKAPKZ          PIC  X(01).
           10 LRWSICHERHRESERV              PIC S9(11)V9(2) COMP-3.
           10 NIL-LRWSICHERHRESERV          PIC  X(01).
           10 LRWSICHERHRESBKZ              PIC  X(0001).
           10 NIL-LRWSICHERHRESBKZ          PIC  X(01).
           10 ZUGANG                        PIC  X(0032).
           10 VON                           PIC  X(0032).
           10 BIS                           PIC  X(0032).
           10 LRWVSRSK                      PIC S9(11)V9(2) COMP-3.
           10 NIL-LRWVSRSK                  PIC  X(01).
           10 LRWVSRSKBKZ                   PIC  X(0001).
           10 NIL-LRWVSRSKBKZ               PIC  X(01).
           10 LRWVSRSKUEB                   PIC S9(11)V9(2) COMP-3.
           10 NIL-LRWVSRSKUEB               PIC  X(01).
           10 LRWVSRSKUEBBKZ                PIC  X(0001).
           10 NIL-LRWVSRSKUEBBKZ            PIC  X(01).
           10 LRWRKWSK                      PIC S9(11)V9(2) COMP-3.
           10 NIL-LRWRKWSK                  PIC  X(01).
           10 LRWRKWSKBKZ                   PIC  X(0001).
           10 NIL-LRWRKWSKBKZ               PIC  X(01).
           10 LRWBTFRSUMSK                  PIC S9(11)V9(2) COMP-3.
           10 NIL-LRWBTFRSUMSK              PIC  X(01).
           10 LRWBTFRSUMSKBKZ               PIC  X(0001).
           10 NIL-LRWBTFRSUMSKBKZ           PIC  X(01).
           10 LRWFMBTFRTTSK                 PIC  X(010).
           10 NIL-LRWFMBTFRTTSK             PIC  X(01).
           10 LRWFMBTFRTTSKBKZ              PIC  X(0001).
           10 NIL-LRWFMBTFRTTSKBKZ          PIC  X(01).
           10 LRWBTFRISTWERTSK              PIC S9(11)V9(2) COMP-3.
           10 NIL-LRWBTFRISTWERTSK          PIC  X(01).
           10 LRWBTFRMINDWRTSK              PIC S9(11)V9(2) COMP-3.
           10 NIL-LRWBTFRMINDWRTSK          PIC  X(01).
           10 LRWKONSORTIALKZ               PIC  X(0001).
           10 LRWGARABLRNTPRL               PIC S9(11)V9(2) COMP-3.
           10 NIL-LRWGARABLRNTPRL           PIC  X(01).
           10 LRWGARABRNTPRLBKZ             PIC  X(0001).
           10 NIL-LRWGARABRNTPRLBKZ         PIC  X(01).
           10 LRWAKTVTRWERT                 PIC S9(11)V9(2) COMP-3.
           10 NIL-LRWAKTVTRWERT             PIC  X(01).
           10 LRWAKTVTRWERTBKZ              PIC  X(0001).
           10 NIL-LRWAKTVTRWERTBKZ          PIC  X(01).
           10 LRWGBURENTANW                 PIC S9(11)V9(2) COMP-3.
           10 NIL-LRWGBURENTANW             PIC  X(01).
           10 LRWGBURENTANWBKZ              PIC  X(0001).
           10 NIL-LRWGBURENTANWBKZ          PIC  X(01).
           10 DCOP                          PIC  X(0001).
           10 NIL-DCOP                      PIC  X(01).
           10 LRWPFSTAFFELKZN               PIC  X(0001).
           10 NIL-LRWPFSTAFFELKZN           PIC  X(01).
           10 LRWGARPFRENTE1                PIC S9(11)V9(2) COMP-3.
           10 NIL-LRWGARPFRENTE1            PIC  X(01).
           10 LRWGARPFRENTE2                PIC S9(11)V9(2) COMP-3.
           10 NIL-LRWGARPFRENTE2            PIC  X(01).
           10 LRWGARPFRENTE3                PIC S9(11)V9(2) COMP-3.
           10 NIL-LRWGARPFRENTE3            PIC  X(01).
           10 LRWGARPFRENTE4                PIC S9(11)V9(2) COMP-3.
           10 NIL-LRWGARPFRENTE4            PIC  X(01).
           10 LRWGARPFRENTE5                PIC S9(11)V9(2) COMP-3.
           10 NIL-LRWGARPFRENTE5            PIC  X(01).
           10 LRWGARPFRENTEBKZ              PIC  X(0001).
           10 NIL-LRWGARPFRENTEBKZ          PIC  X(01).
           10 LRWGESPFRENTE1                PIC S9(11)V9(2) COMP-3.
           10 NIL-LRWGESPFRENTE1            PIC  X(01).
           10 LRWGESPFRENTE2                PIC S9(11)V9(2) COMP-3.
           10 NIL-LRWGESPFRENTE2            PIC  X(01).
           10 LRWGESPFRENTE3                PIC S9(11)V9(2) COMP-3.
           10 NIL-LRWGESPFRENTE3            PIC  X(01).
           10 LRWGESPFRENTE4                PIC S9(11)V9(2) COMP-3.
           10 NIL-LRWGESPFRENTE4            PIC  X(01).
           10 LRWGESPFRENTE5                PIC S9(11)V9(2) COMP-3.
           10 NIL-LRWGESPFRENTE5            PIC  X(01).
           10 LRWGESPFRENTEBKZ              PIC  X(0001).
           10 NIL-LRWGESPFRENTEBKZ          PIC  X(01).
           10 LRWGARPFRENTANW               PIC S9(11)V9(2) COMP-3.
           10 NIL-LRWGARPFRENTANW           PIC  X(01).
           10 LRWGARPFRENTANWBKZ            PIC  X(0001).
           10 NIL-LRWGARPFRENTANWBKZ        PIC  X(01).
           10 LRWGESPFRENTANW               PIC S9(11)V9(2) COMP-3.
           10 NIL-LRWGESPFRENTANW           PIC  X(01).
           10 LRWGESPFRENTANWBKZ            PIC  X(0001).
           10 NIL-LRWGESPFRENTANWBKZ        PIC  X(01).
           10 LRWLSTPROZPFL1                PIC S9(3)V COMP-3.
           10 NIL-LRWLSTPROZPFL1            PIC  X(01).
           10 LRWLSTPROZPFL2                PIC S9(3)V COMP-3.
           10 NIL-LRWLSTPROZPFL2            PIC  X(01).
           10 LRWLSTPROZPFL3                PIC S9(3)V COMP-3.
           10 NIL-LRWLSTPROZPFL3            PIC  X(01).
           10 LRWLSTPROZPFL4                PIC S9(3)V COMP-3.
           10 NIL-LRWLSTPROZPFL4            PIC  X(01).
           10 LRWLSTPROZPFL5                PIC S9(3)V COMP-3.
           10 NIL-LRWLSTPROZPFL5            PIC  X(01).

