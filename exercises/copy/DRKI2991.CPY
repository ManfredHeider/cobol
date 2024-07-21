      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  09.03.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2991.CPY  $
      *    $Revision:   2.17  $
      *
      *    $Log:   L:/mf/CPY/DRKI2991.CPv  $
      *
      *   Rev 2.17   Nov 24 2020 08:47:52   u130685
      *AKI01805: Bankprodukt OZP/Copy
      *
      *   Rev 2.16   Apr 17 2020 13:37:52   t13ex39
      *AKI01767: Deutsche Bank - PK Smart - Erweiterung der Datenli
      *eferung um Laufzeit und Restkreditversicherungen
      *
      *   Rev 2.15   Jan 22 2020 10:46:00   t13ex39
      *AKI01754: Investment D: Neues Datenfeld und Fixselektion - U
      *ltimosparen
      *
      *   Rev 2.14   Jun 24 2019 14:05:46   t13ex39
      *AKI01687: Deutsche Bank: Anzeige gebührenbefreites Konto im
      *Bestandsvertrag inkl. Erweiterung der Selektionen
      *
      *   Rev 2.13   Mar 18 2019 17:49:44   t13ae52
      *AKI01658: G48 - Kontokorrent Sourcen/COPYs
      *
      *   Rev 2.12   Nov 29 2018 09:51:56   t13ae52
      *AKI01645: G48  -  Sourcen/COPYs
      *
      *   Rev 2.11   Jun 26 2017 10:36:04   t13ex39
      *AKI01506: Deuba KK aus GHP und DBP nach BANKPRODUKT überführ
      *en
      *
      *   Rev 2.10   Jul 20 2016 12:44:10   t13ex15
      *AKI01449: Deutsche Bank Konto: Datum des Saldos BKP OZP und
      *PMKI1197
      *
      *   Rev 2.9   Nov 06 2015 12:31:02   t13ex73
      *AKI01385: Korrektur der Darstellung des Eigenvermitteltkennz
      *eichens OZP ZTKI0465
      *
      *   Rev 2.8   Oct 19 2015 13:34:24   t13ex39
      *AKI01361: DeuBa Sparverträge von DBP in BKP
      *W H O
      *
      *   Rev 2.7   Aug 01 2015 11:19:52   t13ex39
      *AKI01361: DeuBa Sparverträge von DBP in BKP         P M K I
      *0 6 1 2
      *
      *   Rev 2.6   Aug 01 2015 11:05:38   t13ex39
      *AKI01361: DeuBa Sparverträge von DBP in BKP         P M K I
      *0 6 1 2
      *
      *   Rev 2.5   Jul 23 2015 13:46:24   t13ex78
      *FKI01496: Wartungsauftrag - Ticketverarbeitung - Anpassung K
      *eyfelder
      *
      *   Rev 2.4   Jul 22 2015 16:36:56   t13ex78
      *FKI01496: Wartungsauftrag - Ticketverarbeitung - Anpassung K
      *eyfelder
      *
      *   Rev 2.3   Jun 17 2015 11:24:44   t13ex78
      *AKI01361: DeuBa Sparverträge von DBP in BKP
      *
      *   Rev 2.2   Apr 01 2015 09:43:58   t13ex15
      *AKI01337: Änderung KLU Sonderleistungen 2015 Anpassung VBI S
      *ST und AS-Club
      *
      *   Rev 2.1   Mar 12 2015 13:32:56   t13ex15
      *AKI01337: Änderung KLU Sonderleistungen 2015 Anpassung VBI S
      *ST und AS-Club
      *
      *   Rev 2.0   Mar 09 2015 14:00:22   t13ex15
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01361!21.05.15!    ! Anpassung der Struktur    ! tfl.     *
      *         !        !    ! an den ZTKI0465           !          *
      * -------------------------------------------------------------*
      * FKI01496!23.07.15!    ! Anpassung PO-Bereich      ! tfl.     *
      * -------------------------------------------------------------*
      * AKI01361!01.08.15!    ! NULL-Indikatoren          ! WHO.     *
      * -------------------------------------------------------------*
061115* AKI01385!06.11.15!    ! Provisions KZ einbauen    ! DDY      *
      * -------------------------------------------------------------*
200716* AKI01449!20.07.16!    ! 3 neue Felder             ! SWZ      *
      * -------------------------------------------------------------*
010417* AKI01506!01.04.17! DRKI2991: PI-Attribute in PO-  ! WHO      *
      *         !        ! umbenannt                      !          *
      * -------------------------------------------------------------*
      * AKI01645!26.11.18!    ! NEUE FELDER               !SCHUMACHER*
      *         !        !    ! - BKPDATVOLLAUSZ          !          *
      *         !        !    ! - BKPRESTLAUFZEIT         !          *
      *         !        !    ! - BKPSICHPAKET            !          *
      *         !        !    ! - BKPPIN                  !          *
      *         !        !    !   (VERGESSEN IN AKI01556) !          *
      *--------------------------------------------------------------*
      * AKI01658!01.02.19!    ! NEUE FELDER               !SCHUMACHER*
      *         !        !    ! - BKPKFWERSTR             !          *
      *         !        !    ! - BKPKFWFOLGER            !          *
      *         !        !    ! - BKPKFWKREDITB           !          *
      *         !        !    ! - BKPKFWSOLLZ             !          *
      *         !        !    ! - BKPKFWDATFEST           !          *
      *         !        !    ! - BKPKFWTILGUNG           !          *
      *--------------------------------------------------------------*
      * AKI01687!16.06.19!    ! NEUES ATTRIBUT            ! T13EX39
      *         !        !    ! "KOSTENSCHLÜSSE ZKP"      !
      *---------!--------!--------------------------------!----------*
      * AKI01754!04.01.20! NEUES ATTRIBUT "ULTIMOSPAREN"  ! T13EX39
      *         !        !                                !
      *---------!--------!--------------------------------!----------*
      * AKI01767!04.03.20! NEUES ATTRIBUTE                ! T13EX39
      *         !        ! GESAMTLAUFZEIT                 !
      *         !        ! VERSICHERUNGSUMFANG1           !
      *         !        ! VERSICHERUNGSUMFANG2           !
      *---------!--------!----!---------------------------!----------*
      * AKI01805!09.11.20!    ! NEUES ATTRIBUT ESAFE      ! ATAR     *
      *--------------------------------------------------------------*
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01 PI-DRKI2991.
           05  PI-DRKI2991-PROGRAMM            PIC  X(008).
           05 PI-DRKI2991-UFKT                 PIC  9(002).
              88 PI-DRKI2991-UFKT-FIRST                 VALUE 1.
              88 PI-DRKI2991-UFKT-NEXT                  VALUE 2.
              88 PI-DRKI2991-UFKT-QUIT                  VALUE 3.


      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT
           05 PI-DRKI2991-DATEN.
             10 PI-DRKI2991-BKPVERTRAGSID   PIC  X(13).
      ***
       01 PO-DRKI2991.
           05 PO-DRKI2991-RC                    PIC  X(003).
             88 PO-DRKI2991-RC-OK                        VALUE 'OK '.
             88 PO-DRKI2991-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI2991-RC-EOF                       VALUE 'EOF'.

210515     05 PO-DRKI2991-KEY.
210515       10 PO-DRKI2991-BKPLFDNR           PIC S9(04) COMP.
230715       10 PO-DRKI2991-BKPVERTRAGSID      PIC  X(13).
           05 PO-DRKI2991-DATEN.
ANF   *        %M_PO_DATEN%
010417*      10 PI-DRKI2991-BKPABRECHKONTO   PIC  X(0010).
010417       10 PO-DRKI2991-BKPABRECHKONTO   PIC  X(0010).
             10 PO-DRKI2991-BKPABSCHLUSSVBNR   PIC S9(9) COMP.
             10 PO-DRKI2991-BKPANNUITAET   PIC S9(12)V9(2) COMP-3.
010417*      10 PI-DRKI2991-BKPANTEILRATE   PIC S9(12)V9(2) COMP-3.
010417       10 PO-DRKI2991-BKPANTEILRATE   PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI2991-BKPBIC   PIC  X(0011).
             10 PO-DRKI2991-BKPBILANZSCHLKNR   PIC S9(4) COMP.
010815       10 PO-DRKI2991-N-BKPBLNZSCHLKNR   PIC S9(4) COMP.
             10 PO-DRKI2991-BKPBILANZSCHLKTY   PIC S9(4) COMP.
010815       10 PO-DRKI2991-N-BKPBLNZSCHLKTY   PIC S9(4) COMP.
             10 PO-DRKI2991-BKPBLZKUNDEN   PIC  X(0008).
             10 PO-DRKI2991-BKPDARLBETRAG   PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI2991-BKPDARLBETRAKT   PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI2991-BKPDARLEHENSART   PIC  X(0001).
             10 PO-DRKI2991-BKPDATBEGENDE   PIC  X(010).
             10 PO-DRKI2991-BKPDATEROFFNUNG   PIC  X(010).
             10 PO-DRKI2991-BKPDATFAELLIG   PIC  X(010).
             10 PO-DRKI2991-BKPDATKREDITAUSZ   PIC  X(010).
             10 PO-DRKI2991-BKPDATKREDITZUS   PIC  X(010).
             10 PO-DRKI2991-BKPDATSCHLIESS   PIC  X(010).
010417*      10 PI-DRKI2991-BKPDATUMSALDO   PIC  X(010).
010417       10 PO-DRKI2991-BKPDATUMSALDO   PIC  X(010).
             10 PO-DRKI2991-BKPDATVOLLAUSZ   PIC  X(010).
             10 PO-DRKI2991-BKPDETAILKNR   PIC S9(15)V COMP-3.
010815       10 PO-DRKI2991-NI-BKPDETAILKNR   PIC S9(4) COMP.
             10 PO-DRKI2991-BKPDETAILKTY   PIC S9(4) COMP.
010815       10 PO-DRKI2991-NI-BKPDETAILKTY   PIC S9(4) COMP.
             10 PO-DRKI2991-BKPEIGENVERMKZ   PIC  X(0001).
             10 PO-DRKI2991-BKPENDEZINSFEST   PIC  X(010).
             10 PO-DRKI2991-BKPEZRFREIJAHRE   PIC  X(010).
             10 PO-DRKI2991-BKPHABENZINS   PIC S9(2)V9(4) COMP-3.
             10 PO-DRKI2991-BKPIBAN   PIC  X(0034).
             10 PO-DRKI2991-BKPKFWDATFEST   PIC  X(010).
             10 PO-DRKI2991-BKPKFWERSTR   PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI2991-BKPKFWFOLGER   PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI2991-BKPKFWKREDITB   PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI2991-BKPKFWSOLLZ   PIC S9(2)V9(4) COMP-3.
             10 PO-DRKI2991-BKPKFWTILGUNG   PIC S9(2)V9(4) COMP-3.
             10 PO-DRKI2991-BKPKONDSCHL   PIC  X(0002).
             10 PO-DRKI2991-BKPKONTOART   PIC  X(0003).
             10 PO-DRKI2991-BKPKREDITART   PIC  X(0006).
210515***    10 PO-DRKI2991-BKPLFDNR        PIC S9(004) COMP.
             10 PO-DRKI2991-BKPLIMITEXT   PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI2991-BKPONLREVERSKZ   PIC  X(0001).
             10 PO-DRKI2991-BKPPIN   PIC S9(9) COMP.
             10 PO-DRKI2991-BKPPRODUKTID   PIC S9(4) COMP.
061115       10 PO-DRKI2991-BKPPROVISIONKZ   PIC  X(0001).
             10 PO-DRKI2991-BKPRATENHOEHE   PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI2991-BKPRESTLAUFZEIT   PIC S9(4) COMP.
             10 PO-DRKI2991-BKPSALDOBETR   PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI2991-BKPSICHPAKET   PIC  X(0001).
             10 PO-DRKI2991-BKPSOLLZINS   PIC S9(2)V9(4) COMP-3.
             10 PO-DRKI2991-BKPTELREVERSKZ   PIC  X(0001).
             10 PO-DRKI2991-BKPTILGUNGANF   PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI2991-BKPTILGUNGPROZ   PIC S9(2)V9(4) COMP-3.
             10 PO-DRKI2991-BKPTILGUNGSART   PIC  X(0001).
             10 PO-DRKI2991-BKPUEBERZZINS   PIC S9(2)V9(4) COMP-3.
             10 PO-DRKI2991-BKPVERTRAGSENDE   PIC  X(010).
             10 PO-DRKI2991-BKPWAEHRSCHL   PIC S9(5)V COMP-3.
             10 PO-DRKI2991-BKPZAHLWEISEKNR   PIC S9(4) COMP.
             10 PO-DRKI2991-BKPZAHLWEISEKTY   PIC S9(4) COMP.
             10 PO-DRKI2991-BKPZINSANTEIL   PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI2991-LETZTEAENDERUNG   PIC  X(026).
             10 PO-DRKI2991-BKPKOSTSCHLZKP  PIC  X(0001).
             10 PO-DRKI2991-BKPULTOSPARKZ   PIC  X(0001).
END   *
             10 PO-DRKI2991-BKPGESAMTLAUFZEIT  PIC S9(4) COMP.
             10 PO-DRKI2991-BKPVERSUMFANG1     PIC S9(4) COMP.
             10 PO-DRKI2991-BKPVERSUMFANG2     PIC S9(4) COMP.

             10 PO-DRKI2991-BKPESAFE           PIC X(0001).



