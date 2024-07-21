      * T13EX33   Stand: 15/10/98    08:17:07
      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0719.CPY  $
      *    $Revision:   2.23  $
      *
      *    $Log:   L:/mf/CPY/DRKI0719.CPv  $
      *
      *   Rev 2.23   Feb 27 2020 14:17:06   t13ac20
      *AKI01757: VDL-Leben: HOST Verarbeitung und Speicherung der D
      *aten für die neuen Pflegetarife (pflegeSmart und pflegePro) 
      *echenwerte
      *
      *   Rev 2.22   Aug 14 2018 09:34:50   t13ac17
      *AKI01605: G20 EVWS - neue Produkte zum RT 09/2018 Sourcen
      *
      *   Rev 2.21   May 23 2017 10:54:44   t13ac17
      *AKI01510: Fehlende Rechenwerte übernehmen   Sourcen
      *
      *   Rev 2.20   Apr 04 2017 14:30:16   t13ac20
      *AKI01503: Copies und Programme bereinigen
      *
      *   Rev 2.19   Mar 13 2017 08:31:00   t13ac17
      *AKI01492: AML-Änderungen zum RT 03/2017-Rechenwerte(PORTOS)
      *
      *   Rev 2.18   Jan 17 2017 10:57:50   t13ex27
      *AKI01488: AM-Leben Konsortialverträge
      *
      *   Rev 2.17   Jun 24 2016 09:03:42   t13ex48
      *AKI01434: AML-Neue Produkte zum RT 06/16: Rechenwerte
      *
      *   Rev 2.16   Mar 19 2015 07:34:12   t13ae31
      *AKI01349: AMLeben: VEP-Bestandsmasken
      *
      *   Rev 2.15   Jun 26 2014 15:20:58   t13ex48
      *AKI01296: VERMÖGENS- & EINKOMMENSPLAN - VEP: Cobol RW
      *
      *   Rev 2.14   Jun 26 2014 14:03:20   t13ex48
      *AKI01296: VERMÖGENS- & EINKOMMENSPLAN - VEP: Cobol RW
      *
      *   Rev 2.13   Mar 28 2014 01:17:54   t13ex48
      *AKI01153: Neues Verfahren: Aktualisierung AML-Rechenwerte
      *
      *   Rev 2.12   Aug 31 2011 12:40:10   t13ex48
      *AKI01065: AMLeben - Einführung Impulsprodukt - VASP: Source RW
      *
      *   Rev 2.11   Jan 04 2011 13:52:56   t13ex27
      *AKI00961: Anzeige "Frühester Rentenbeginn" in KI-Vertragsdat
      *en-Aktualisierung
      *
      *   Rev 2.10   Dec 10 2010 10:23:50   t13ex48
      *AKI00969: AML-DTA: neue Felder zum RT 06/2010: RW
      *
      *   Rev 2.9   Sep 07 2009 16:19:58   t13ex48
      *AKI00860: CR 22378 - AML-Wunschpolice;LEBENRECHENWERTE
      *
      *   Rev 2.8   02 Feb 2009 10:26:38   t13ex27
      *AKI00768: CR 22257 - Bearbeitungskennzeichen Rechenwerte-Pro
      *gramm
      *
      *   Rev 2.7   04 Jun 2008 16:03:38   t13ex27
      *AKI00711: Neue DTA-Felder AMLeben Datei-Copys
      *
      *   Rev 2.6   19 Oct 2006 09:06:16   t13ae31
      *AKI00615: Einführung Pflegerentenversicherung
      *
      *   Rev 2.5   May 30 2005 08:25:20   t13ae31
      *FKI00933: AML erweitern LFO Fondbezeichnung
      *
      *   Rev 2.4   Jun 17 2004 14:13:36   t13ex27
      *Integration Kinderprodukt in der Vermögensplanung - Rechenwerte
      *
      *   Rev 2.3   Apr 28 2003 14:38:02   t13ae31
      *AKI00348 neues Feld Zahlbeitrag
      *
      *   Rev 2.2   14 Jun 2000 11:30:26   T13AE31
      *AKI0105
      *
      *   Rev 2.1   Sep 03 1999 12:57:16   T13EX39
      *AKI00044 WKZ eingebaut
      *
      *   Rev 2.0   Mar 23 1999 15:53:18   T13EX33
      *aki00019 Produktionsuebergabe
      *
      *   Rev 1.0   06 Oct 1998 15:32:30   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *    COPY-MEMBER ZUM AUFBEREITUNGSPROGRAMM                       *
      *        - EINGABE DTA VERTRAGSDATENAENDERUNGEN RECHENWERTE AML -*
      *                                                                *
      *    SATZLÄNGE 900                                               *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   ! AENDERUNG                              !NAME     !VER
      *AUFTRAG !                                        !         !SION*
      *--------!----------------------------------------!--------_!----*
      *13.10.98!NEUERSTELLUNG                           !HUEBNER  ! 01 *
      *KI-NEU  !                                        !         !    *
      *--------!----------------------------------------!---------!----*
      *2.9.99  ! WKZ eingebaut                          !WHO      !
      *        !                                        !         !    *
      *06.6.00 ! FELD TAZRENTE UND TAZRENTE-INFO EINGEB.!HUEBNER  !    *
      *        !                                        !         !    *
      *16.4.03 ! FELD ZAHLBTG UND ZAHLBTG-INFO EINGEB.  !HUEBNER  !    *
      *        ! AKI00348                               !         !    *
      *07.06.04! Feld LRWGKIRENTE und LRWGKIRENTEWKZ neu!Frenzel  !    *
      *        ! AKI00418                               !         !    *
      *        !                                        !         !    *
      *04.05.05!erweitern LFO Fondbez. FKI00933         !Hübner   !    *
      *        !                                        !         !    *
      *30.09.06!Neue Felder LRWGPFRENTE / GPFRENTANW    !Hübner   !    *
      *        ! AKI615                                 !         !    *
      *10.04.08!Neue Felder AKI00711                    !Frenzel  !2.7 *
      *--------!----------------------------------------!---------!----*
280809*28.08.09!AKI00860 Neues Feld: DECKUNGSKAPITAL    !HSE      !2.8 *
      *        !DRKI0719-NI-LRWBEGABRUFPHASE auskomment.!         !    *
      *--------!----------------------------------------!---------!----*
070610*07.06.10!AKI00969 Neue Felder: FMBTFRTT          !HSE      !2.9 *
      *        !                      BTFRISTWERT       !         !    *
      *        !                      BTFRMINDWERT      !         !    *
      *        !         FILLER eingefuegt (Neue SL 500)!         !    *
      *--------!----------------------------------------!---------!----*
260811*26.08.11!AKI01065 Neue Felder zum AML-RT 09/2011 !HSE      !2.10*
      *--------!----------------------------------------!---------!----*
030314*03.03.14!AKI01153 Neue Felder zum AML-RT 12/2013 !HSE      !2.11*
      *        !Neue Satzlaenge 700 (vorher 500)        !         !    *
      *--------!----------------------------------------!---------!----*
020614*02.06.14!Neue Felder fuer VEP zum AML-RT 06/2014 !HSE      !2.12*
      *--------!----------------------------------------!---------!----*
100315*10.03.15!Neue Felder fuer VEP zum AML-RT 03/2015 !HHU      !2.13*
      *--------!----------------------------------------!---------!----*
010616*01.06.16! 19 Neue Felder zum AML-RT 06/2016      !HSE      !2.14*
      *        ! fuer Rechenwerte (69 Byte)             !         !    *
      *AKI01434!(fuer Produkt SK - schwere Krankheit)   !         !    *
      *--------!----------------------------------------!---------!----*
130117*13.01.17! AKI01488 Neu: Konsortial-KZ            !IFR      !2.18*
      *--------!----------------------------------------!---------!----*
200217*20.02.17! AKI01492 Neu: GARABLRNTPRL, AKTVTRWERT !GWA      !2.19*
      *        ! GARABLRNTPRLBKZ, AKTVTRWERTBKZ         !         !    *
      *--------!----------------------------------------!---------!----*
030417*03.04.17! WÄHRUNGSKENNZEICHEN GELÖSCHT:          !SWO      !2.20*
      *AKI01503! LRWABLEISTSIMWKZ                       !         !    *
      *        ! LRWBETRFSUMWKZ                         !         !    *
      *        ! LRWBONUSRENTEWKZ                       !         !    *
      *        ! LRWGARLSTABLWKZ                        !         !    *
      *        ! LRWGBURENTEWKZ                         !         !    *
      *        ! LRWGBUZVRENTEWKZ                       !         !    *
      *        ! LRWGKIRENTEWKZ                         !         !    *
      *        ! LRWGPFRENTEANWWKZ                      !         !    *
      *        ! LRWGPFRENTEWKZ                         !         !    *
      *        ! LRWGUZVSUMMEWKZ                        !         !    *
      *        ! LRWKAPABFGARWKZ                        !         !    *
      *        ! LRWKAPABFINKDWKZ                       !         !    *
      *        ! LRWKAPABFVORWKZ                        !         !    *
      *        ! LRWMOEGLDBWKZ      143+18              !         !    *
      *        ! LRWRKWERTGESWKZ                        !         !    *
      *        ! LRWUEBETODWKZ                          !         !    *
      *        ! LRWVORAUSZDBWKZ                        !         !    *
      *        ! LRWVSRTODWKZ                           !         !    *
      *        ! LRWZAHLBTGWKZ                          !         !    *
      *        ! LTLAUSZAHLBETRAGWKZ                    !         !    *
      *--------!----------------------------------------!---------!----*
180417*18.04.17! Felder für neue Tabelle LEBENRWVASP    !GWA      !2.21*
      *AKI01510! integriert                             !         !    *
      *--------!----------------------------------------!---------!----*
      *--------!----------------------------------------!---------!----*
040718*04.07.18! neue Felder Gesamt-BU-Rentenanwartsch. !GWA      !2.22*
      *AKI01605! integriert                             !         !    *
      *--------!----------------------------------------!---------!----*
      *30.01.20! AKI01757 ÄNDERUNG AML-RT 03/2020       !T13AC20  !2.23*
      *        !   NEUE FELDER:                         !         !    *
      *        !     PFSTAFFELKZN                       !         !    *
      *        !     GARPFRENTE1-5                      !         !    *
      *        !     GARPFRENTEBKZ                      !         !    *
      *        !     GESPFRENTE1-5                      !         !    *
      *        !     GESPFRENTEBKZFO                    !         !    *
      *        !     GARPFRENTANW                       !         !    *
      *        !     GARPFRENTANWBKZ                    !         !    *
      *        !     GESPFRENTANW                       !         !    *
      *        !     GESPFRENTANWBKZ                    !         !    *
      *        !     LSTPROZPFL1-5                      !         !    *
      *        !   ENTFALLENDE FELDER:                  !         !    *
      *        !     LRWGPFRENTE                        !         !    *
      *        !     NI-LRWGPFRENTE                     !         !    *
      *        !     LRWGPFRENTEBKZ                     !         !    *
      *        !     LRWGPFRENTEANW                     !         !    *
      *        !     NI-LRWGPFRENTEANW                  !         !    *
      *        !     LRWGPFRENTEANBKZ                   !         !    *
      *--------!----------------------------------------!---------!----*
      *
      *
CCC009*    SKIP1
       01  DRKI0719.
      *
           10  DRKI0719-SATZ.
      *
               15  DRKI0719-SA              PIC 9.
               15  DRKI0719-VERTRAGSID      PIC X(13).
               15  DRKI0719-VNR             PIC 9(10).
               15  DRKI0719-RECHENWERTE.
      *
             20 DRKI0719-LRWVERSTARLFDNR       PIC S9(4) COMP.
             20 DRKI0719-LRWABLEISTSIM         PIC S9(9)V9(2) COMP-3.
             20 DRKI0719-NI-LRWABLEISTSIM      PIC S9(4) COMP.
030417*      20 DRKI0719-LRWABLEISTSIMWKZ      PIC X(01).
             20 DRKI0719-LRWABLEISTVOR         PIC S9(9)V9(2) COMP-3.
             20 DRKI0719-NI-LRWABLEISTVOR      PIC S9(4) COMP.
             20 DRKI0719-LRWABLEISTVORWKZ      PIC X(01).
             20 DRKI0719-LRWBEITRFREIESUM      PIC S9(9)V9(2) COMP-3.
             20 DRKI0719-N-LRWBTRFRSM          PIC S9(4) COMP.
030417*      20 DRKI0719-LRWBETRFSUMWKZ        PIC X(01).
             20 DRKI0719-LRWBONUSRENTE         PIC S9(9)V9(2) COMP-3.
             20 DRKI0719-NI-LRWBONUSRENTE      PIC S9(4) COMP.
030417*      20 DRKI0719-LRWBONUSRENTEWKZ      PIC X(01).
             20 DRKI0719-LRWDATUMSTAND         PIC X(10).
             20 DRKI0719-LRWERMABL             PIC X(10).
             20 DRKI0719-LRWFORENDITE          PIC S9(3)V9(2) COMP-3.
             20 DRKI0719-NI-LRWFORENDITE       PIC S9(4) COMP.
             20 DRKI0719-LRWGARLSTABL          PIC S9(11)V9(2) COMP-3.
             20 DRKI0719-NI-LRWGARLSTABL       PIC S9(4) COMP.
030417*      20 DRKI0719-LRWGARLSTABLWKZ       PIC X(01).
             20 DRKI0719-LRWGBURENTE           PIC S9(7)V9(2) COMP-3.
             20 DRKI0719-NI-LRWGBURENTE        PIC S9(4) COMP.
030417*      20 DRKI0719-LRWGBURENTEWKZ        PIC X(01).
             20 DRKI0719-LRWGBUZVRENTE         PIC S9(7)V9(2) COMP-3.
             20 DRKI0719-NI-LRWGBUZVRENTE      PIC S9(4) COMP.
030417*      20 DRKI0719-LRWGBUZVRENTEWKZ      PIC X(01).
             20 DRKI0719-LRWGUZVSUMME          PIC S9(11)V9(2) COMP-3.
             20 DRKI0719-NI-LRWGUZVSUMME       PIC S9(4) COMP.
030417*      20 DRKI0719-LRWGUZVSUMMEWKZ       PIC X(01).
             20 DRKI0719-LRWKAPABFGAR          PIC S9(9)V9(2) COMP-3.
             20 DRKI0719-NI-LRWKAPABFGAR       PIC S9(4) COMP.
030417*      20 DRKI0719-LRWKAPABFGARWKZ       PIC X(01).
030417*      20 DRKI0719-LRWKAPABFINKDWKZ      PIC X(01).
             20 DRKI0719-LRWKAPABFINKDYN       PIC S9(9)V9(2) COMP-3.
             20 DRKI0719-NI-LRWKAPABFINKDYN    PIC S9(4) COMP.
             20 DRKI0719-LRWKAPABFVOR          PIC S9(9)V9(2) COMP-3.
             20 DRKI0719-NI-LRWKAPABFVOR       PIC S9(4) COMP.
030417*      20 DRKI0719-LRWKAPABFVORWKZ       PIC X(01).
             20 DRKI0719-LRWMOEGLDARLBETR      PIC S9(9)V9(2) COMP-3.
             20 DRKI0719-N-LRWMGLDRLBTR        PIC S9(4) COMP.
030417*      20 DRKI0719-LRWMOEGLDBWKZ         PIC X(01).
             20 DRKI0719-LRWRKWERTGES          PIC S9(9)V9(2) COMP-3.
             20 DRKI0719-NI-LRWRKWERTGES       PIC S9(4) COMP.
030417*      20 DRKI0719-LRWRKWERTGESWKZ       PIC X(01).
030417*      20 DRKI0719-LRWUEBETODWKZ         PIC X(01).
             20 DRKI0719-LRWUEBETTOD           PIC S9(9)V9(2) COMP-3.
             20 DRKI0719-NI-LRWUEBETTOD        PIC S9(4) COMP.
             20 DRKI0719-LRWVORAUSZDARLBETR    PIC S9(9)V9(2) COMP-3.
             20 DRKI0719-N-LRWVRSZDRLBTR       PIC S9(4) COMP.
030417*      20 DRKI0719-LRWVORAUSZDBWKZ       PIC X(01).
             20 DRKI0719-LRWVSRTOD             PIC S9(9)V9(2) COMP-3.
             20 DRKI0719-NI-LRWVSRTOD          PIC S9(4) COMP.
030417*      20 DRKI0719-LRWVSRTODWKZ          PIC X(01).
160403       20 DRKI0719-LRWZAHLBTG            PIC S9(11)V9(2) COMP-3.
160403       20 DRKI0719-NI-LRWZAHLBTG         PIC S9(4) COMP.
030417*      20 DRKI0719-LRWZAHLBTGWKZ         PIC X(01).
070604       20 DRKI0719-LRWGKIRENTE           PIC S9(9)V9(2) COMP-3.
070604       20 DRKI0719-NI-LRWGKIRENTE        PIC S9(4) COMP.
030417*      20 DRKI0719-LRWGKIRENTEWKZ        PIC X(01).
100408       20 DRKI0719-NI-LRWSUMBETBWRRK     PIC S9(4) COMP.
100408       20 DRKI0719-LRWSUMBETBWRRK        PIC S9(11)V9(2) COMP-3.
280809*      20 DRKI0719-NI-LRWBEGABRUFPHASE   PIC S9(4) COMP.
100408       20 DRKI0719-LRWBEGABRUFPHASE      PIC X(010).
100408       20 DRKI0719-NI-LRWVVGLSTABLSIM    PIC S9(4) COMP.
100408       20 DRKI0719-LRWVVGLSTABLSIM       PIC S9(11)V9(2) COMP-3.
100408       20 DRKI0719-NI-LRWVVGKAPABFSIM    PIC S9(4) COMP.
100408       20 DRKI0719-LRWVVGKAPABFSIM       PIC S9(11)V9(2) COMP-3.
291208       20 DRKI0719-LRWABLEISTVORBKZ      PIC X(01).
291208       20 DRKI0719-LRWKAPABFVORBKZ       PIC X(01).
291208       20 DRKI0719-LRWKAPABFGARBKZ       PIC X(01).
291208       20 DRKI0719-LRWVSRTODBKZ          PIC X(01).
291208       20 DRKI0719-LRWUEBETTODBKZ        PIC X(01).
291208       20 DRKI0719-LRWBETRFSUMBKZ        PIC X(01).
291208       20 DRKI0719-LRWRKWERTGESBKZ       PIC X(01).
291208       20 DRKI0719-LRWBONUSRENTEBKZ      PIC X(01).
291208       20 DRKI0719-LRWKAPABFINKDBKZ      PIC X(01).
291208       20 DRKI0719-LRWABLEISTSIMBKZ      PIC X(01).
291208       20 DRKI0719-LRWVORAUSZDBBKZ       PIC X(01).
291208       20 DRKI0719-LRWMOEGLDBBKZ         PIC X(01).
291208       20 DRKI0719-LRWGBURENTEBKZ        PIC X(01).
291208       20 DRKI0719-LRWGBUZVRENTEBKZ      PIC X(01).
291208       20 DRKI0719-LRWGUZVSUMMEBKZ       PIC X(01).
291208       20 DRKI0719-LRWERMABLBKZ          PIC X(01).
291208       20 DRKI0719-LRWGARLSTABLBKZ       PIC X(01).
291208       20 DRKI0719-LRWFORENDITEBKZ       PIC X(01).
291208       20 DRKI0719-LRWZAHLBEITRBKZ       PIC X(01).
291208       20 DRKI0719-LRWGKIRENTEBKZ        PIC X(01).
291208       20 DRKI0719-LRWSUMBETBWRKBKZ      PIC X(01).
291208       20 DRKI0719-LRWVVGLSTASIBKZ       PIC X(01).
291208       20 DRKI0719-LRWVVKAPABFSIBKZ      PIC X(01).
280809       20 DRKI0719-NI-LRWSUMDK           PIC S9(4) COMP.
280809       20 DRKI0719-LRWSUMDK              PIC S9(11)V9(2) COMP-3.
280809       20 DRKI0719-LRWSUMDKBKZ           PIC X(01).
070610       20 DRKI0719-LRWFMBTFRTT           PIC X(10).
>>           20 DRKI0719-LRWFMBTFRTTBKZ        PIC X(01).
             20 DRKI0719-NI-LRWBTFRISTWERT     PIC S9(04) COMP.
             20 DRKI0719-LRWBTFRISTWERT        PIC S9(11)V9(2) COMP-3.
    <<       20 DRKI0719-NI-LRWBTFRMINDWERT    PIC S9(04) COMP.
070610       20 DRKI0719-LRWBTFRMINDWERT       PIC S9(11)V9(2) COMP-3.
131210       20 DRKI0719-LRWFRMGLBEGRENTE      PIC  X(010).
>>           20 DRKI0719-LRWGESEURENTE         PIC S9(7)V9(2) COMP-3.
             20 DRKI0719-NI-LRWGESEURENTE      PIC S9(4) COMP.
             20 DRKI0719-LRWGESEURENTEBKZ      PIC  X(0001).
             20 DRKI0719-LRWGESEUZVRENTE       PIC S9(7)V9(2) COMP-3.
             20 DRKI0719-NI-LRWGESEUZVRENTE    PIC S9(4) COMP.
             20 DRKI0719-LRWGESEUZVRNTBKZ      PIC  X(0001).
             20 DRKI0719-LRWGESEUZBONRNT       PIC S9(7)V9(2) COMP-3.
    <<       20 DRKI0719-NI-LRWGESEUZBONRNT    PIC S9(4) COMP.
131210       20 DRKI0719-LRWGESEUZBONRBKZ      PIC  X(0001).
260811* Neue Felder zum Produkt VASP(Vermögensaufbau und Sicherungsplan)
      * (10 X 10 = insgesamt 100 Byte)
>>           20 DRKI0719-NI-LRWRKWHV           PIC S9(4) COMP.
             20 DRKI0719-LRWRKWHV              PIC S9(11)V9(2) COMP-3.
             20 DRKI0719-LRWRKWHVBKZ           PIC  X(0001).
             20 DRKI0719-NI-LRWRKWBUZV         PIC S9(4) COMP.
             20 DRKI0719-LRWRKWBUZV            PIC S9(11)V9(2) COMP-3.
             20 DRKI0719-LRWRKWBUZVBKZ         PIC  X(0001).
             20 DRKI0719-NI-LRWRKWTZV          PIC S9(4) COMP.
             20 DRKI0719-LRWRKWTZV             PIC S9(11)V9(2) COMP-3.
             20 DRKI0719-LRWRKWTZVBKZ          PIC  X(0001).
             20 DRKI0719-NI-LRWRKWPZV          PIC S9(4) COMP.
             20 DRKI0719-LRWRKWPZV             PIC S9(11)V9(2) COMP-3.
             20 DRKI0719-LRWRKWPZVBKZ          PIC  X(0001).
             20 DRKI0719-NI-LRWRKWEUZV         PIC S9(4) COMP.
    <<       20 DRKI0719-LRWRKWEUZV            PIC S9(11)V9(2) COMP-3.
260811       20 DRKI0719-LRWRKWEUZVBKZ         PIC  X(0001).
100315* Neues Feld
100315       20 DRKI0719-NI-LRWSICHERHEITRES   PIC S9(4) COMP.
100315       20 DRKI0719-LRWSICHERHEITRES      PIC S9(11)V9(2) COMP-3.
100315       20 DRKI0719-LRWSICHERHEITRESKZ    PIC  X(0001).
030314* Neue Felder: u. A. nicht in Anspruch genommene Teilauszahlungn
             20 DRKI0719-NI-KONVGH             PIC S9(4) COMP.
             20 DRKI0719-KONVGH                PIC S9(11)V9(2) COMP-3.
             20 DRKI0719-KONVGHBKZ             PIC  X(0001).
             20 DRKI0719-NI-GARVORGLSTABL      PIC S9(4) COMP.
             20 DRKI0719-GARVORGLSTABL         PIC S9(11)V9(2) COMP-3.
             20 DRKI0719-GARVORGLSTABLBKZ      PIC  X(0001).
             20 DRKI0719-NI-VORVORGLSTABL      PIC S9(4) COMP.
             20 DRKI0719-VORVORGLSTABL         PIC S9(11)V9(2) COMP-3.
             20 DRKI0719-VORVORGLSTABLBKZ      PIC  X(0001).
             20 DRKI0719-NI-GARVORGKAPABF      PIC S9(4) COMP.
             20 DRKI0719-GARVORGKAPABF         PIC S9(11)V9(2) COMP-3.
             20 DRKI0719-GARVORGKAPABFBKZ      PIC  X(0001).
             20 DRKI0719-NI-VORVORGKAPABF      PIC S9(4) COMP.
             20 DRKI0719-VORVORGKAPABF         PIC S9(11)V9(2) COMP-3.
             20 DRKI0719-VORVORGKAPABFBKZ      PIC  X(0001).
             20 DRKI0719-NI-ABZUG              PIC S9(4) COMP.
             20 DRKI0719-ABZUG                 PIC S9(11)V9(2) COMP-3.
             20 DRKI0719-ABZUGBKZ              PIC  X(0001).
             20 DRKI0719-NI-AUSZBETR           PIC S9(4) COMP.
             20 DRKI0719-AUSZBETR              PIC S9(11)V9(2) COMP-3.
             20 DRKI0719-AUSZBETRBKZ           PIC  X(0001).
             20 DRKI0719-NI-SUMRKWPROLTAZ      PIC S9(4) COMP.
             20 DRKI0719-SUMRKWPROLTAZ         PIC S9(11)V9(2) COMP-3.
             20 DRKI0719-SUMRKWPROLTAZBKZ      PIC  X(0001).
             20 DRKI0719-NI-VORLSTABLPROLTAZ   PIC S9(4) COMP.
             20 DRKI0719-VORLSTABLPROLTAZ      PIC S9(11)V9(2) COMP-3.
             20 DRKI0719-VORLSTABLPROLTAZBKZ   PIC  X(0001).
             20 DRKI0719-NI-VORPROLTAZKAPABF   PIC S9(4) COMP.
    <<       20 DRKI0719-VORPROLTAZKAPABF      PIC S9(11)V9(2) COMP-3.
030314       20 DRKI0719-VORPROLTAZKAPABFBKZ   PIC  X(0001).
020614       20 DRKI0719-NI-VERMOEGTOPF        PIC S9(4) COMP.
>>           20 DRKI0719-VERMOEGTOPF           PIC S9(11)V9(2) COMP-3.
             20 DRKI0719-VERMOEGTOPBKZ         PIC  X(0001).
             20 DRKI0719-NI-EINKOMTOPF         PIC S9(4) COMP.
             20 DRKI0719-EINKOMTOPF            PIC S9(11)V9(2) COMP-3.
             20 DRKI0719-EINKOMTOPFBKZ         PIC  X(0001).
             20 DRKI0719-BSPBEGRNTZHLG         PIC X(010).
             20 DRKI0719-NI-SUMTEILRENTEN      PIC S9(4) COMP.
    <<       20 DRKI0719-SUMTEILRENTEN         PIC S9(11)V9(2) COMP-3.
020614       20 DRKI0719-SUMTEILRNTBKZ         PIC  X(0001).
010616*  ----------------------------------------------------------
010616*  Neue Felder fuer Teiltyp 27 (schwere Krankheit)
>>           20 DRKI0719-NI-LRWVSRSK           PIC S9(4) COMP.
             20 DRKI0719-LRWVSRSK              PIC S9(11)V99 COMP-3.
             20 DRKI0719-LRWVSRSKBKZ           PIC X.
             20 DRKI0719-NI-LRWVSRSKUEB        PIC S9(4) COMP.
             20 DRKI0719-LRWVSRSKUEB           PIC S9(11)V99 COMP-3.
             20 DRKI0719-LRWVSRSKUEBBKZ        PIC X.
             20 DRKI0719-NI-LRWRKWSK           PIC S9(4) COMP.
             20 DRKI0719-LRWRKWSK              PIC S9(11)V99 COMP-3.
             20 DRKI0719-LRWRKWSKBKZ           PIC X.
             20 DRKI0719-NI-LRWBTFRSUMSK       PIC S9(4) COMP.
             20 DRKI0719-LRWBTFRSUMSK          PIC S9(11)V99 COMP-3.
             20 DRKI0719-LRWBTFRSUMSKBKZ       PIC X(001).
             20 DRKI0719-LRWFMBTFRTTSK         PIC X(010).
             20 DRKI0719-LRWFMBTFRTTSKBKZ      PIC X(001).
             20 DRKI0719-NI-LRWBTFRISTWERTSK   PIC S9(4) COMP.
             20 DRKI0719-LRWBTFRISTWERTSK      PIC S9(11)V9(2) COMP-3.
    <<       20 DRKI0719-NI-LRWBTFRMINDWERTSK  PIC S9(4) COMP.
010616       20 DRKI0719-LRWBTFRMINDWERTSK     PIC S9(11)V9(2) COMP-3.
130117       20 DRKI0719-LRWKONSORTIAL-KZ      PIC X(001).
200217       20 DRKI0719-NI-LRWGARABLRNTPRL    PIC S9(4) COMP.
             20 DRKI0719-LRWGARABLRNTPRL       PIC S9(11)V9(2) COMP-3.
             20 DRKI0719-LRWGARABRNTPRLBKZ     PIC X(0001).
             20 DRKI0719-NI-LRWAKTVTRWERT      PIC S9(4) COMP.
             20 DRKI0719-LRWAKTVTRWERT         PIC S9(11)V9(2) COMP-3.
200217       20 DRKI0719-LRWAKTVTRWERTBKZ      PIC X(0001).
040718       20 DRKI0719-NI-LRWGBURENTANW      PIC S9(4) COMP.
             20 DRKI0719-LRWGBURENTANW         PIC S9(11)V9(2) COMP-3.
040718       20 DRKI0719-LRWGBURENTANWBKZ      PIC X(0001).
      *  ----------------------------------------------------------
      *  NEUE FELDER AKI01757 ÄNDERUNG AML-RT 03/2020
             20 DRKI0719-PFSTAFFELKZN          PIC  X(001).
             20 DRKI0719-NI-GARPFRENTE         PIC S9(004)    COMP.
             20 DRKI0719-GARPFRENTE1           PIC S9(011)V99 COMP-3.
             20 DRKI0719-GARPFRENTE2           PIC S9(011)V99 COMP-3.
             20 DRKI0719-GARPFRENTE3           PIC S9(011)V99 COMP-3.
             20 DRKI0719-GARPFRENTE4           PIC S9(011)V99 COMP-3.
             20 DRKI0719-GARPFRENTE5           PIC S9(011)V99 COMP-3.
             20 DRKI0719-GARPFRENTEBKZ         PIC  X(001).
             20 DRKI0719-NI-GESPFRENTE         PIC S9(004)    COMP.
             20 DRKI0719-GESPFRENTE1           PIC S9(011)V99 COMP-3.
             20 DRKI0719-GESPFRENTE2           PIC S9(011)V99 COMP-3.
             20 DRKI0719-GESPFRENTE3           PIC S9(011)V99 COMP-3.
             20 DRKI0719-GESPFRENTE4           PIC S9(011)V99 COMP-3.
             20 DRKI0719-GESPFRENTE5           PIC S9(011)V99 COMP-3.
             20 DRKI0719-GESPFRENTEBKZ         PIC  X(001).
             20 DRKI0719-NI-GARPFRENTANW       PIC S9(004)    COMP.
             20 DRKI0719-GARPFRENTANW          PIC S9(011)V99 COMP-3.
             20 DRKI0719-GARPFRENTANWBKZ       PIC  X(001).
             20 DRKI0719-NI-GESPFRENTANW       PIC S9(004)    COMP.
             20 DRKI0719-GESPFRENTANW          PIC S9(011)V99 COMP-3.
             20 DRKI0719-GESPFRENTANWBKZ       PIC  X(001).
             20 DRKI0719-NI-LSTPROZPFL         PIC S9(004)    COMP.
             20 DRKI0719-LSTPROZPFL1           PIC S9(003)    COMP-3.
             20 DRKI0719-LSTPROZPFL2           PIC S9(003)    COMP-3.
             20 DRKI0719-LSTPROZPFL3           PIC S9(003)    COMP-3.
             20 DRKI0719-LSTPROZPFL4           PIC S9(003)    COMP-3.
             20 DRKI0719-LSTPROZPFL5           PIC S9(003)    COMP-3.
010616*  ----------------------------------------------------------
070610*      20 FILLER                         PIC X(167).
131210*      20 FILLER                         PIC X(133).
260811*      20 FILLER                         PIC X(083).
030314*      20 FILLER                         PIC X(183).
020614*      20 FILLER                         PIC X(143).
100315*      20 FILLER                         PIC X(133).
010616*      20 FILLER                         PIC X(064).
130117*      20 FILLER                         PIC X(063).
200217*      20 FILLER                         PIC X(043).
030417*      20 FILLER                         PIC X(062).
040718*      20 FILLER                         PIC X(052).
             20 FILLER                         PIC X(161).
      *
               15  DRKI0719-LEBENTEILLEISTUNG REDEFINES
                   DRKI0719-RECHENWERTE.
      *
             20 DRKI0719-LTLLAUFENDENR         PIC S9(4) COMP.
             20 DRKI0719-LTLVERSTARLFDNR       PIC S9(4) COMP.
             20 DRKI0719-LTLAUSZAHLBETRAG      PIC S9(9)V9(2) COMP-3.
030417*      20 DRKI0719-LTLAUSZAHLBETRAGWKZ   PIC X(001).
             20 DRKI0719-N-LTLSZHLBTRG         PIC S9(4) COMP.
             20 DRKI0719-LTLAUSZAHLZEITPKT     PIC X(10).
000606       20 DRKI0719-N-LTLAUSZRENTE        PIC S9(4) COMP.
000606       20 DRKI0719-LTLAUSZAHLRENTE       PIC S9(9)V9(2) COMP-3.
000606       20 DRKI0719-LTLAUSZAHLRENTEWKZ    PIC X(001).
291208       20 DRKI0719-LTLAUSZBETRAGBKZ      PIC X(001).
291208       20 DRKI0719-LTLZAHLZEITPBKZ       PIC X(001).
291208       20 DRKI0719-LTLVERRENTKAPBKZ      PIC X(001).
 
               15  DRKI0719-LEBENFOND REDEFINES
                   DRKI0719-RECHENWERTE.
      *
             20 DRKI0719-LFOLFDNR              PIC S9(4) COMP.
             20 DRKI0719-LFOVERSTARLFDNR       PIC S9(4) COMP.
100408*      20 DRKI0719-LFOANZAHLANTEILE      PIC S9(6)V9(3) COMP-3.
100408       20 DRKI0719-LFOANZAHLANTEILE      PIC S9(8)V9(6) COMP-3.
             20 DRKI0719-N-LFNZHLNTL           PIC S9(4) COMP.
             20 DRKI0719-LFODECKUNGSKAPITAL    PIC S9(11)V9(2) COMP-3.
             20 DRKI0719-LFODECKUNGSKAPITALWKZ PIC X(001).
             20 DRKI0719-N-LFDCKNGSKPTL        PIC S9(4) COMP.
100408       20 DRKI0719-LFOFONDSKUERZEL       PIC X(02).
040505       20 DRKI0719-LFOFONDISIN           PIC X(12).
             20 DRKI0719-LFOPROZSPARBETR       PIC S9(3)V9(2) COMP-3.
             20 DRKI0719-NI-LFOPROZSPARBETR    PIC S9(4) COMP.
100408*      20 DRKI0719-LFORUECKNAHMEBETR     PIC S9(3)V9(2) COMP-3.
100408       20 DRKI0719-LFORUECKNAHMEBETR     PIC S9(11)V9(2) COMP-3.
             20 DRKI0719-LFORUECKNAHMEBETRWKZ  PIC X(001).
             20 DRKI0719-N-LFRCKNHMBTR         PIC S9(4) COMP.
291208       20 DRKI0719-LFOANZAHLANTBKZ       PIC X(001).
291208       20 DRKI0719-LFORUECKNAHMEBKZ      PIC X(001).
291208       20 DRKI0719-LFOPROZSPARBKZ        PIC X(001).
291208       20 DRKI0719-LFODECKKAPITBKZ       PIC X(001).
291208       20 DRKI0719-LFOFONDSKUEBKZ        PIC X(001).
      *
               15  DRKI0719-VERSTARIF REDEFINES
                   DRKI0719-RECHENWERTE.
             20 DRKI0719-VETVERSTARLFDNR       PIC S9(4) COMP.
             20 DRKI0719-VETVERSSUMME          PIC S9(9)V9(2) COMP-3.
             20 DRKI0719-VETVERSSUMMEWKZ       PIC X(001).
             20 DRKI0719-NI-VETVERSSUMME       PIC S9(4) COMP.
291208       20 DRKI0719-VETVERSSUMMEBKZ       PIC X(001).
      *
               15  DRKI0719-VERTRAG REDEFINES
                   DRKI0719-RECHENWERTE.
             20 DRKI0719-VERGESAMTBEITRAG      PIC S9(9)V9(2) COMP-3.
             20 DRKI0719-N-VRGSMTBTRG          PIC S9(4) COMP.
291208       20 DRKI0719-VERGESBEITRBKZ        PIC X(001).
      *
      *     Felder für Tabelle LEBENRWVASP TCKI0317
      *
180417         15  DRKI0719-LEBENRWVASP REDEFINES
>>                 DRKI0719-RECHENWERTE.
      *
             20 DRKI0719-LRVVERSTARLFDNR       PIC S9(4) COMP.
             20 DRKI0719-LRVRKWHV              PIC S9(11)V9(2) COMP-3.
             20 DRKI0719-NI-LRVRKWHV           PIC S9(4) COMP.
             20 DRKI0719-LRVRKWHVBKZ           PIC X(0001).
             20 DRKI0719-LRVRKWBUZV            PIC S9(11)V9(2) COMP-3.
             20 DRKI0719-NI-LRVRKWBUZV         PIC S9(4) COMP.
             20 DRKI0719-LRVRKWBUZVBKZ         PIC X(0001).
             20 DRKI0719-LRVRKWTZV             PIC S9(11)V9(2) COMP-3.
             20 DRKI0719-NI-LRVRKWTZV          PIC S9(4) COMP.
             20 DRKI0719-LRVRKWTZVBKZ          PIC X(0001).
             20 DRKI0719-LRVRKWPZV             PIC S9(11)V9(2) COMP-3.
             20 DRKI0719-NI-LRVRKWPZV          PIC S9(4) COMP.
             20 DRKI0719-LRVRKWPZVBKZ          PIC X(0001).
             20 DRKI0719-LRVRKWEUZV            PIC S9(11)V9(2) COMP-3.
             20 DRKI0719-NI-LRVRKWEUZV         PIC S9(4) COMP.
             20 DRKI0719-LRVRKWEUZVBKZ         PIC X(0001).
             20 DRKI0719-LRVBTFRSUMBUZV        PIC S9(11)V9(2) COMP-3.
             20 DRKI0719-NI-LRVBTFRSUMBUZV     PIC S9(4) COMP.
             20 DRKI0719-LRVBTFRSUMBUZVBKZ     PIC X(0001).
             20 DRKI0719-LRVBTFRSUMTZV         PIC S9(11)V9(2) COMP-3.
             20 DRKI0719-NI-LRVBTFRSUMTZV      PIC S9(4) COMP.
             20 DRKI0719-LRVBTFRSUMTZVBKZ      PIC X(0001).
             20 DRKI0719-LRVBTFRSUMPZV         PIC S9(11)V9(2) COMP-3.
             20 DRKI0719-NI-LRVBTFRSUMPZV      PIC S9(4) COMP.
             20 DRKI0719-LRVBTFRSUMPZVBKZ      PIC X(0001).
             20 DRKI0719-LRVBTFRSUMEUZV        PIC S9(11)V9(2) COMP-3.
             20 DRKI0719-NI-LRVBTFRSUMEUZV     PIC S9(4) COMP.
             20 DRKI0719-LRVBTFRSUMEUZVBKZ     PIC X(0001).
             20 DRKI0719-LRVFMBTFRTTBUZV       PIC X(010).
             20 DRKI0719-NI-LRVFMBTFRTTBUZV    PIC S9(4) COMP.
             20 DRKI0719-LRVFMBTFRTTBUZVBKZ    PIC X(0001).
             20 DRKI0719-LRVFMBTFRTTTZV        PIC X(010).
             20 DRKI0719-NI-LRVFMBTFRTTTZV     PIC S9(4) COMP.
             20 DRKI0719-LRVFMBTFRTTTZVBKZ     PIC X(0001).
             20 DRKI0719-LRVFMBTFRTTPZV        PIC X(010).
             20 DRKI0719-NI-LRVFMBTFRTTPZV     PIC S9(4) COMP.
             20 DRKI0719-LRVFMBTFRTTPZVBKZ     PIC X(0001).
             20 DRKI0719-LRVFMBTFRTTEUZV       PIC X(010).
             20 DRKI0719-NI-LRVFMBTFRTTEUZV    PIC S9(4) COMP.
             20 DRKI0719-LRVFMBTFRTTEUZVBKZ    PIC X(0001).
             20 DRKI0719-LRVBTFRISTWERTBUZV    PIC S9(11)V9(2) COMP-3.
             20 DRKI0719-NI-LRVBTFRISTWERTBUZV PIC S9(4) COMP.
             20 DRKI0719-LRVBTFRISTWERTTZV     PIC S9(11)V9(2) COMP-3.
             20 DRKI0719-NI-LRVBTFRISTWERTTZV  PIC S9(4) COMP.
             20 DRKI0719-LRVBTFRISTWERTPZV     PIC S9(11)V9(2) COMP-3.
             20 DRKI0719-NI-LRVBTFRISTWERTPZV  PIC S9(4) COMP.
             20 DRKI0719-LRVBTFRISTWERTEUZV    PIC S9(11)V9(2) COMP-3.
             20 DRKI0719-NI-LRVBTFRISTWERTEUZV PIC S9(4) COMP.
             20 DRKI0719-LRVBTFRMINDWERTBUZV   PIC S9(11)V9(2) COMP-3.
             20 DRKI0719-N-LRVBTFRMINDWERTBUZV PIC S9(4) COMP.
             20 DRKI0719-LRVBTFRMINDWERTTZV    PIC S9(11)V9(2) COMP-3.
             20 DRKI0719-N-LRVBTFRMINDWERTTZV  PIC S9(4) COMP.
             20 DRKI0719-LRVBTFRMINDWERTPZV    PIC S9(11)V9(2) COMP-3.
             20 DRKI0719-N-LRVBTFRMINDWERTPZV  PIC S9(4) COMP.
             20 DRKI0719-LRVBTFRMINDWERTEUZV   PIC S9(11)V9(2) COMP-3.
             20 DRKI0719-N-LRVBTFRMINDWERTEUZV PIC S9(4) COMP.
             20 DRKI0719-LRVBETBWRRKHV         PIC S9(11)V9(2) COMP-3.
             20 DRKI0719-NI-LRVBETBWRRKHV      PIC S9(4) COMP.
             20 DRKI0719-LRVBETBWRRKHVBKZ      PIC X(0001).
             20 DRKI0719-LRVBETBWRRKBUZV       PIC S9(11)V9(2) COMP-3.
             20 DRKI0719-NI-LRVBETBWRRKBUZV    PIC S9(4) COMP.
             20 DRKI0719-LRVBETBWRRKBUZVBKZ    PIC X(0001).
             20 DRKI0719-LRVBETBWRRKTZV        PIC S9(11)V9(2) COMP-3.
             20 DRKI0719-NI-LRVBETBWRRKTZV     PIC S9(4) COMP.
             20 DRKI0719-LRVBETBWRRKTZVBKZ     PIC X(0001).
             20 DRKI0719-LRVBETBWRRKPZV        PIC S9(11)V9(2) COMP-3.
             20 DRKI0719-NI-LRVBETBWRRKPZV     PIC S9(4) COMP.
             20 DRKI0719-LRVBETBWRRKPZVBKZ     PIC X(0001).
             20 DRKI0719-LRVBETBWRRKEUZV       PIC S9(11)V9(2) COMP-3.
             20 DRKI0719-NI-LRVBETBWRRKEUZV    PIC S9(4) COMP.
             20 DRKI0719-LRVBETBWRRKEUZVBKZ    PIC X(0001).
      *
      ******* ENDE COPY-MEMBER ***************************************
 


