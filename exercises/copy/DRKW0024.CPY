      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.09.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKW0024.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKW0024.CPv  $
      *
      *   Rev 2.3   Apr 04 2017 14:30:28   t13ac20
      *AKI01503: Copies und Programme bereinigen
      *
      *   Rev 2.2   Jan 04 2011 13:53:02   t13ex27
      *AKI00961: Anzeige "Frühester Rentenbeginn" in KI-Vertragsdat
      *en-Aktualisierung
      *
      *   Rev 2.1   Nov 11 2010 11:40:02   t13ex14
      *AKI00966: DVAG Österreich: Leben-DTA-Aktualisierung
      *
      *   Rev 2.0   Sep 01 2010 11:59:40   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *    COPY-MEMBER ZUM AUFBEREITUNGSPROGRAMM PMKW0009              *
      *        - VERTRAGSDATENAENDERUNGEN RECHENWERTE DVAG Wien -      *
      ******  Kopie von DRKW0024, ergänzt um das Feld DRKW0024-GES *****
      *                                                                *
      *    SATZLÄNGE 321                                               *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                          !NAME     !VER-*
      *AUFTRAG !                                        !         !SION*
      *--------!----------------------------------------!--------_!----*
      *01.09.10!NEUERSTELLUNG                           ! WMI     !    *
      *AKI00966!                                        !         !    *
      *--------!----------------------------------------!---------!----*
030417*03.04.17! WÄHRUNGSKENNZEICHEN GELÖSCHT:          ! SWO     !2.3 *
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
      *        ! LRWMOEGLDBWKZ                          !         !    *
      *        ! LRWRKWERTGESWKZ                        !         !    *
      *        ! LRWUEBETODWKZ                          !         !    *
      *        ! LRWVORAUSZDBWKZ                        !         !    *
      *        ! LRWVSRTODWKZ                           !         !    *
      *        ! LRWZAHLBTGWKZ                          !         !    *
      *        ! LTLAUSZAHLBETRAGWKZ                    !         !    *
      *--------!----------------------------------------!---------!----*
      *        !                                        !         !    *
      *        !                                        !         !    *
      *--------!----------------------------------------!---------!----*
      *
       01  DRKW0024.
      *
           10  DRKW0024-SATZ.
      *
               15  DRKW0024-SA              PIC 9.
               15  DRKW0024-VERTRAGSID      PIC X(13).
               15  DRKW0024-VNR             PIC 9(10).
               15  DRKW0024-RECHENWERTE.
      *
             20 DRKW0024-LRWVERSTARLFDNR       PIC S9(4) COMP.
             20 DRKW0024-LRWABLEISTSIM         PIC S9(9)V9(2) COMP-3.
             20 DRKW0024-NI-LRWABLEISTSIM      PIC S9(4) COMP.
030417*      20 DRKW0024-LRWABLEISTSIMWKZ      PIC X(01).
             20 DRKW0024-LRWABLEISTVOR         PIC S9(9)V9(2) COMP-3.
             20 DRKW0024-NI-LRWABLEISTVOR      PIC S9(4) COMP.
             20 DRKW0024-LRWABLEISTVORWKZ      PIC X(01).
             20 DRKW0024-LRWBEITRFREIESUM      PIC S9(9)V9(2) COMP-3.
             20 DRKW0024-N-LRWBTRFRSM          PIC S9(4) COMP.
030417*      20 DRKW0024-LRWBETRFSUMWKZ        PIC X(01).
             20 DRKW0024-LRWBONUSRENTE         PIC S9(9)V9(2) COMP-3.
             20 DRKW0024-NI-LRWBONUSRENTE      PIC S9(4) COMP.
030417*      20 DRKW0024-LRWBONUSRENTEWKZ      PIC X(01).
             20 DRKW0024-LRWDATUMSTAND         PIC X(10).
             20 DRKW0024-LRWERMABL             PIC X(10).
             20 DRKW0024-LRWFORENDITE          PIC S9(3)V9(2) COMP-3.
             20 DRKW0024-NI-LRWFORENDITE       PIC S9(4) COMP.
             20 DRKW0024-LRWGARLSTABL          PIC S9(11)V9(2) COMP-3.
             20 DRKW0024-NI-LRWGARLSTABL       PIC S9(4) COMP.
030417*      20 DRKW0024-LRWGARLSTABLWKZ       PIC X(01).
             20 DRKW0024-LRWGBURENTE           PIC S9(7)V9(2) COMP-3.
             20 DRKW0024-NI-LRWGBURENTE        PIC S9(4) COMP.
030417*      20 DRKW0024-LRWGBURENTEWKZ        PIC X(01).
             20 DRKW0024-LRWGBUZVRENTE         PIC S9(7)V9(2) COMP-3.
             20 DRKW0024-NI-LRWGBUZVRENTE      PIC S9(4) COMP.
030417*      20 DRKW0024-LRWGBUZVRENTEWKZ      PIC X(01).
             20 DRKW0024-LRWGUZVSUMME          PIC S9(11)V9(2) COMP-3.
             20 DRKW0024-NI-LRWGUZVSUMME       PIC S9(4) COMP.
030417*      20 DRKW0024-LRWGUZVSUMMEWKZ       PIC X(01).
             20 DRKW0024-LRWKAPABFGAR          PIC S9(9)V9(2) COMP-3.
             20 DRKW0024-NI-LRWKAPABFGAR       PIC S9(4) COMP.
030417*      20 DRKW0024-LRWKAPABFGARWKZ       PIC X(01).
030417*      20 DRKW0024-LRWKAPABFINKDWKZ      PIC X(01).
             20 DRKW0024-LRWKAPABFINKDYN       PIC S9(9)V9(2) COMP-3.
             20 DRKW0024-NI-LRWKAPABFINKDYN    PIC S9(4) COMP.
             20 DRKW0024-LRWKAPABFVOR          PIC S9(9)V9(2) COMP-3.
             20 DRKW0024-NI-LRWKAPABFVOR       PIC S9(4) COMP.
030417*      20 DRKW0024-LRWKAPABFVORWKZ       PIC X(01).
             20 DRKW0024-LRWMOEGLDARLBETR      PIC S9(9)V9(2) COMP-3.
             20 DRKW0024-N-LRWMGLDRLBTR        PIC S9(4) COMP.
030417*      20 DRKW0024-LRWMOEGLDBWKZ         PIC X(01).
             20 DRKW0024-LRWRKWERTGES          PIC S9(9)V9(2) COMP-3.
             20 DRKW0024-NI-LRWRKWERTGES       PIC S9(4) COMP.
030417*      20 DRKW0024-LRWRKWERTGESWKZ       PIC X(01).
030417*      20 DRKW0024-LRWUEBETODWKZ         PIC X(01).
             20 DRKW0024-LRWUEBETTOD           PIC S9(9)V9(2) COMP-3.
             20 DRKW0024-NI-LRWUEBETTOD        PIC S9(4) COMP.
             20 DRKW0024-LRWVORAUSZDARLBETR    PIC S9(9)V9(2) COMP-3.
             20 DRKW0024-N-LRWVRSZDRLBTR       PIC S9(4) COMP.
030417*      20 DRKW0024-LRWVORAUSZDBWKZ       PIC X(01).
             20 DRKW0024-LRWVSRTOD             PIC S9(9)V9(2) COMP-3.
             20 DRKW0024-NI-LRWVSRTOD          PIC S9(4) COMP.
030417*      20 DRKW0024-LRWVSRTODWKZ          PIC X(01).
             20 DRKW0024-LRWZAHLBTG            PIC S9(11)V9(2) COMP-3.
             20 DRKW0024-NI-LRWZAHLBTG         PIC S9(4) COMP.
030417*      20 DRKW0024-LRWZAHLBTGWKZ         PIC X(01).
             20 DRKW0024-LRWGKIRENTE           PIC S9(9)V9(2) COMP-3.
             20 DRKW0024-NI-LRWGKIRENTE        PIC S9(4) COMP.
030417*      20 DRKW0024-LRWGKIRENTEWKZ        PIC X(01).
             20 DRKW0024-LRWGPFRENTE           PIC S9(9)V9(2) COMP-3.
             20 DRKW0024-NI-LRWGPFRENTE        PIC S9(4) COMP.
030417*      20 DRKW0024-LRWGPFRENTEWKZ        PIC X(01).
             20 DRKW0024-LRWGPFRENTEANW        PIC S9(9)V9(2) COMP-3.
             20 DRKW0024-NI-LRWGPFRENTEANW     PIC S9(4) COMP.
030417*      20 DRKW0024-LRWGPFRENTEANWWKZ     PIC X(01).
             20 DRKW0024-NI-LRWSUMBETBWRRK     PIC S9(4) COMP.
             20 DRKW0024-LRWSUMBETBWRRK        PIC S9(11)V9(2) COMP-3.
             20 DRKW0024-LRWBEGABRUFPHASE      PIC X(010).
             20 DRKW0024-NI-LRWVVGLSTABLSIM    PIC S9(4) COMP.
             20 DRKW0024-LRWVVGLSTABLSIM       PIC S9(11)V9(2) COMP-3.
             20 DRKW0024-NI-LRWVVGKAPABFSIM    PIC S9(4) COMP.
             20 DRKW0024-LRWVVGKAPABFSIM       PIC S9(11)V9(2) COMP-3.
             20 DRKW0024-LRWABLEISTVORBKZ      PIC X(01).
             20 DRKW0024-LRWKAPABFVORBKZ       PIC X(01).
             20 DRKW0024-LRWKAPABFGARBKZ       PIC X(01).
             20 DRKW0024-LRWVSRTODBKZ          PIC X(01).
             20 DRKW0024-LRWUEBETTODBKZ        PIC X(01).
             20 DRKW0024-LRWBETRFSUMBKZ        PIC X(01).
             20 DRKW0024-LRWRKWERTGESBKZ       PIC X(01).
             20 DRKW0024-LRWBONUSRENTEBKZ      PIC X(01).
             20 DRKW0024-LRWKAPABFINKDBKZ      PIC X(01).
             20 DRKW0024-LRWABLEISTSIMBKZ      PIC X(01).
             20 DRKW0024-LRWVORAUSZDBBKZ       PIC X(01).
             20 DRKW0024-LRWMOEGLDBBKZ         PIC X(01).
             20 DRKW0024-LRWGBURENTEBKZ        PIC X(01).
             20 DRKW0024-LRWGBUZVRENTEBKZ      PIC X(01).
             20 DRKW0024-LRWGUZVSUMMEBKZ       PIC X(01).
             20 DRKW0024-LRWERMABLBKZ          PIC X(01).
             20 DRKW0024-LRWGARLSTABLBKZ       PIC X(01).
             20 DRKW0024-LRWFORENDITEBKZ       PIC X(01).
             20 DRKW0024-LRWZAHLBEITRBKZ       PIC X(01).
             20 DRKW0024-LRWGKIRENTEBKZ        PIC X(01).
             20 DRKW0024-LRWGPFRENTEBKZ        PIC X(01).
             20 DRKW0024-LRWGPFRENTEANBKZ      PIC X(01).
             20 DRKW0024-LRWSUMBETBWRKBKZ      PIC X(01).
             20 DRKW0024-LRWVVGLSTASIBKZ       PIC X(01).
             20 DRKW0024-LRWVVKAPABFSIBKZ      PIC X(01).
             20 DRKW0024-NI-LRWSUMDK           PIC S9(4) COMP.
             20 DRKW0024-LRWSUMDK              PIC S9(11)V9(2) COMP-3.
             20 DRKW0024-LRWSUMDKBKZ           PIC X(01).
030417*  ----------------------------------------------------------
030417       20 FILLER                         PIC X(019).
      *
               15  DRKW0024-LEBENTEILLEISTUNG REDEFINES
                   DRKW0024-RECHENWERTE.
      *
             20 DRKW0024-LTLLAUFENDENR         PIC S9(4) COMP.
             20 DRKW0024-LTLVERSTARLFDNR       PIC S9(4) COMP.
             20 DRKW0024-LTLAUSZAHLBETRAG      PIC S9(9)V9(2) COMP-3.
030417*      20 DRKW0024-LTLAUSZAHLBETRAGWKZ   PIC X(001).
             20 DRKW0024-N-LTLSZHLBTRG         PIC S9(4) COMP.
             20 DRKW0024-LTLAUSZAHLZEITPKT     PIC X(10).
             20 DRKW0024-N-LTLAUSZRENTE        PIC S9(4) COMP.
             20 DRKW0024-LTLAUSZAHLRENTE       PIC S9(9)V9(2) COMP-3.
             20 DRKW0024-LTLAUSZAHLRENTEWKZ    PIC X(001).
             20 DRKW0024-LTLAUSZBETRAGBKZ      PIC X(001).
             20 DRKW0024-LTLZAHLZEITPBKZ       PIC X(001).
             20 DRKW0024-LTLVERRENTKAPBKZ      PIC X(001).
 
               15  DRKW0024-LEBENFOND REDEFINES
                   DRKW0024-RECHENWERTE.
      *
             20 DRKW0024-LFOLFDNR              PIC S9(4) COMP.
             20 DRKW0024-LFOVERSTARLFDNR       PIC S9(4) COMP.
      *      20 DRKW0024-LFOANZAHLANTEILE      PIC S9(6)V9(3) COMP-3.
             20 DRKW0024-LFOANZAHLANTEILE      PIC S9(8)V9(6) COMP-3.
             20 DRKW0024-N-LFNZHLNTL           PIC S9(4) COMP.
             20 DRKW0024-LFODECKUNGSKAPITAL    PIC S9(11)V9(2) COMP-3.
             20 DRKW0024-LFODECKUNGSKAPITALWKZ PIC X(001).
             20 DRKW0024-N-LFDCKNGSKPTL        PIC S9(4) COMP.
             20 DRKW0024-LFOFONDSKUERZEL       PIC X(02).
             20 DRKW0024-LFOFONDISIN           PIC X(12).
             20 DRKW0024-LFOPROZSPARBETR       PIC S9(3)V9(2) COMP-3.
             20 DRKW0024-NI-LFOPROZSPARBETR    PIC S9(4) COMP.
      *      20 DRKW0024-LFORUECKNAHMEBETR     PIC S9(3)V9(2) COMP-3.
             20 DRKW0024-LFORUECKNAHMEBETR     PIC S9(11)V9(2) COMP-3.
             20 DRKW0024-LFORUECKNAHMEBETRWKZ  PIC X(001).
             20 DRKW0024-N-LFRCKNHMBTR         PIC S9(4) COMP.
             20 DRKW0024-LFOANZAHLANTBKZ       PIC X(001).
             20 DRKW0024-LFORUECKNAHMEBKZ      PIC X(001).
             20 DRKW0024-LFOPROZSPARBKZ        PIC X(001).
             20 DRKW0024-LFODECKKAPITBKZ       PIC X(001).
             20 DRKW0024-LFOFONDSKUEBKZ        PIC X(001).
      *
               15  DRKW0024-VERSTARIF REDEFINES
                   DRKW0024-RECHENWERTE.
             20 DRKW0024-VETVERSTARLFDNR       PIC S9(4) COMP.
             20 DRKW0024-VETVERSSUMME          PIC S9(9)V9(2) COMP-3.
             20 DRKW0024-VETVERSSUMMEWKZ       PIC X(001).
             20 DRKW0024-NI-VETVERSSUMME       PIC S9(4) COMP.
             20 DRKW0024-VETVERSSUMMEBKZ       PIC X(001).
      *
               15  DRKW0024-VERTRAG REDEFINES
                   DRKW0024-RECHENWERTE.
             20 DRKW0024-VERGESAMTBEITRAG      PIC S9(9)V9(2) COMP-3.
             20 DRKW0024-N-VRGSMTBTRG          PIC S9(4) COMP.
             20 DRKW0024-VERGESBEITRBKZ        PIC X(001).
      *
      *    folgende Felder werden zur Ermittlung der Vertragsid
      *    im PMKW0019 benötigt:
           10 DRKW0024-GES                     PIC 99.
           10 DRKW0024-VNR-15                  PIC X(15).
      ******* ENDE COPY-MEMBER ***************************************

