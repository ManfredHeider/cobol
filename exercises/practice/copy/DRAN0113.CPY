      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  10.07.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0113.CPY  $
      *    $Revision:   2.15  $
      *
      *    $Log:   L:/mf/CPY/DRAN0113.CPv  $
      *
      *   Rev 2.15   Sep 12 2018 09:01:50   t13ae34
      *AAN01379: AMLeben: Überarbeitung KIZ-Produkt
      *
      *   Rev 2.14   May 15 2018 08:25:16   t13ae34
      *AAN01368: AM-Leben: Neue ungezillmerte bAV-Tarife
      *
      *   Rev 2.13   Jun 03 2016 14:13:18   t13ae34
      *AAN01263: Produktpaket Arbeitskraftabsicherung
      *
      *   Rev 2.12   May 28 2015 09:52:58   t13ae34
      *AAN01211: Elektronischer Antrag - Änderungen eingereichtes G
      *eschäft - Einpflegen in Vorschlagsdaten
      *
      *   Rev 2.11   Apr 23 2015 13:24:54   t13ae34
      *AAN01211: Elektronischer Antrag - Änderungen eingereichtes G
      *eschäft - Einpflegen in Vorschlagsdaten
      *
      *   Rev 2.10   Jun 26 2014 08:15:08   t13ae34
      *AAN01168: VERMÖGENS- & EINKOMMENSPLAN - VEP
      *
      *   Rev 2.9   Dec 09 2013 14:40:42   t13ae34
      *AAN01139: neuer Pensionskassentarif PRGV
      *
      *   Rev 2.8   Mar 15 2012 11:30:20   t13ae34
      *AAN01054: VASP-Umstellungstarife
      *
      *   Rev 2.7   Sep 30 2011 09:35:50   t13ae34
      *AAN01025: AMLeben - Einführung Impulsprodukt - VASP mit TZV
      *(PS 124)
      *
      *   Rev 2.6   Sep 13 2011 13:10:50   t13ae23
      *AAN01025: AMLeben - Einführung Impulsprodukt - VASP mit TZV
      *(PS 124)
      *
      *   Rev 2.5   Aug 25 2011 10:53:40   t13ae34
      *AAN01025: AMLeben - Einführung Impulsprodukt - VASP mit TZV
      *(PS 124)
      *
      *   Rev 2.4   Dec 10 2010 13:00:44   t13ae34
      *AAN00977: Anpassung der Schnittstellen und Ressourcen
      *
      *   Rev 2.3   Sep 03 2010 16:24:28   t13ae34
      *AAN00971: PDP Ergänzung Ablaufdatum u. Ablaufsumme
      *
      *   Rev 2.2   27 Dec 2007 11:10:58   t13ae23
      *AAN00730: VRA - Kennzeichen Vertriebs-Ges in elektr. Daten G
      *es. 20 AM-Leben
      *
      *   Rev 2.1   08 Sep 2006 09:40:56   T13AE23
      *AAN00685: elektr. Anträge AM-Leben: Umstellung auf "Header/B
      *lackbox"
      *
      *   Rev 2.0   10 Jul 2006 15:05:12   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    TEILMASCHINELLE DATEN MIT DFÜ
      *    DIESE WERDEN UNVERÄNDERT VOM PC-DRUCKPROGRAMM ÜBER DVAG-HOST
      *    ZUM AML-HOST ÜBERTRAGEN
      *    DIESES COPY VERWENDET DAS HEADER-BLACK-BOX VERFAHREN UND
      *    ERSETZT DAS DRAN0043
      ****************************************************************
      *    COPY-MEMBER: DRAN0113                                     *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: RAHNER                                          *
      *    DATUM:    10.07.2006                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00685!10.07.06!2.0 !NEU                        !RAHNER    *
      * AAN00730!28.12.07!2.1 !ALLFINANZ                  !RAHNER    *
      * AAN00971!31.08.10!2.2 !TARIF PDP                  !HEIDER    *
      * AAN00977!08.12.10!2.4 !TARIF EU + XGDV AUF 4000   !HEIDER    *
      * AAN01025!25.07.11!2.5 !VASP IMPULSPRODUKT         !HEIDER    *
      * AAN01054!12.03.12!2.8 !VASP UMSTELLUNGSTARIFE     !HEIDER    *
      * AAN01139!06.12.13!2.9 !XGDV VERGRÖSSERN AUF 5000  !HEIDER    *
      * AAN01168!24.04.14!2.10!VEP, XGDV -> 20.000        !HEIDER    *
      * AAN01211!10.03.15!2.11!ESIGN                      !HEIDER    *
      * AAN01263!30.05.16!2.13!ARBEITSKRAFTABSICHERUNG    !HEIDER    *
      * AAN01379!10.09.18!2.15! SKN SKNE (NEUE KIZ)       ! HEIDER   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
           05  PZM-PC-DATEN.
      *
      *        DATEN ZUR HAUPTVERSICHERUNG
               10  PZM-PC-HVANTNUM     PIC X(09).
               10  PZM-PC-HVTARIF      PIC X(15).
               10  PZM-PC-HVVBEGINN    PIC X(10).
               10  PZM-PC-HVN          PIC 9(03)         COMP-3.
310810         10  PZM-PC-HVENDN       PIC X(10).
               10  PZM-PC-HVNS         PIC 9(03)         COMP-3.
               10  PZM-PC-HVENDNS      PIC X(10).
               10  PZM-PC-HV8ENDALT    PIC 9(03)         COMP-3.
               10  PZM-PC-EZAHLW       PIC 9(02).
               10  PZM-PC-HVBEITRAG    PIC S9(09)V9(02)  COMP-3.
               10  PZM-PC-HVVSUMME     PIC S9(09)V9(02)  COMP-3.
310810         10  PZM-PC-HVENDSUMME   PIC S9(09)V9(02)  COMP-3.
               10  PZM-PC-HVBEGREN     PIC X(10).
               10  PZM-PC-ZBE          PIC X(01).
      *
      *        VERTRIEBSDATEN / INTERNE DATEN
      *            ABSCHLUSSVERMITTLER
               10  PZM-PC-VDABVERM     PIC 9(07)         COMP-3.
      *            PRÜFZIFFER AVERM
               10  PZM-PC-VDABVERMPZ   PIC X(01).
      *            VM
               10  PZM-PC-VDVMNR       PIC 9(07)         COMP-3.
      *            PRÜFZIFFER VM
               10  PZM-PC-VDVMNRPZ     PIC X(01).
      *            VOR- UND ZUNAME VM
               10  PZM-PC-VDVMVORNA    PIC X(16).
               10  PZM-PC-VDVMZUNA     PIC X(21).
      *            DVAG-PRODUKTSCHLÜSSEL
               10  PZM-PC-VDPRODKEY    PIC 9(04)         COMP-3.
      *            DVAG-ABSCHLUSSEINHEITEN
               10  PZM-PC-VDABLEIN     PIC S9(6)V9(01)   COMP-3.
      *            DVAG-DIREKTION
               10  PZM-PC-VDDIREKT     PIC 9(03).
281207*            VERTRIEBSGESELLSCHAFT
281207         10  PZM-PC-VB-GES       PIC 9(03).
100315*            ESIGN
100315         10  PZM-PC-ESIGN        PIC X(01).
      *
      *        VERSIONSDATEN
               10  PZM-PC-VERSION      PIC X(05).
      *
      *        VERSICHERUNGSNEHMER/ANTRAGSTELLER
               10  PZM-PC-PDVNANRED    PIC X(02).
               10  PZM-PC-PDVNVORNA    PIC X(16).
               10  PZM-PC-PDVNZUNA     PIC X(33).
               10  PZM-PC-PDVNLKZ      PIC X(03).
               10  PZM-PC-PDVNLAND     PIC X(35).
               10  PZM-PC-PDVNPLZ      PIC X(06).
               10  PZM-PC-PDVNORT      PIC X(30).
               10  PZM-PC-PDVNGBDAT    PIC X(10).
      *
      *        ERSTE VERSICHERTE PERSON
               10  PZM-PC-PDV1VORNA    PIC X(16).
               10  PZM-PC-PDV1ZUNA     PIC X(33).
               10  PZM-PC-PDV1GBDAT    PIC X(10).
      *
      *        ZWEITE VERSICHERTE PERSON
               10  PZM-PC-PDV2GBDAT    PIC X(10).
      *
      *        STATISTIKDATEN DVAG
               10 PZM-PC-DIVE          PIC X.
               10 PZM-PC-VDPRODKEYBUZI PIC S9(4) COMP.
               10 PZM-PC-VDABLEINBUZI  PIC S9(6)V9(1) COMP-3.
               10 PZM-PC-BEITRAGBUZI   PIC S9(9)V9(2) COMP-3.
               10 PZM-PC-VDPRODKEYBUZR PIC S9(4) COMP.
               10 PZM-PC-VDABLEINBUZR  PIC S9(6)V9(1) COMP-3.
               10 PZM-PC-BEITRAGBUZR   PIC S9(9)V9(2) COMP-3.
               10 PZM-PC-VDPRODKEYKIZ  PIC S9(4) COMP.
               10 PZM-PC-VDABLEINKIZ   PIC S9(6)V9(1) COMP-3.
               10 PZM-PC-BEITRAGKIZ    PIC S9(9)V9(2) COMP-3.
               10 PZM-PC-VDPRODKEYTZV  PIC S9(4) COMP.
               10 PZM-PC-VDABLEINTZV   PIC S9(6)V9(1) COMP-3.
               10 PZM-PC-BEITRAGTZV    PIC S9(9)V9(2) COMP-3.
               10 PZM-PC-VDPRODKEYHZV  PIC S9(4) COMP.
               10 PZM-PC-VDABLEINHZV   PIC S9(6)V9(1) COMP-3.
               10 PZM-PC-BEITRAGHZV    PIC S9(9)V9(2) COMP-3.
               10 PZM-PC-VDPRODKEYP    PIC S9(4) COMP.
               10 PZM-PC-VDABLEINP     PIC S9(6)V9(1) COMP-3.
               10 PZM-PC-BEITRAGP      PIC S9(9)V9(2) COMP-3.
               10 PZM-PC-VDPRODKEYPO   PIC S9(4) COMP.
               10 PZM-PC-VDABLEINPO    PIC S9(6)V9(1) COMP-3.
               10 PZM-PC-BEITRAGPO     PIC S9(9)V9(2) COMP-3.
281207         10 PZM-PC-BEITRABUZI    PIC S9(9)V9(2) COMP-3.
281207         10 PZM-PC-BEITRABUZR    PIC S9(9)V9(2) COMP-3.
281207         10 PZM-PC-BEITRAKIZ     PIC S9(9)V9(2) COMP-3.
281207         10 PZM-PC-BEITRATZV     PIC S9(9)V9(2) COMP-3.
281207         10 PZM-PC-BEITRAHZV     PIC S9(9)V9(2) COMP-3.
281207         10 PZM-PC-BEITRAB       PIC S9(9)V9(2) COMP-3.
281207         10 PZM-PC-BEITRAPO      PIC S9(9)V9(2) COMP-3.
281207         10 PZM-PC-BEITRAHV      PIC S9(9)V9(2) COMP-3.
081210*>>>>>
               10 PZM-PC-VDPRODKEYEUZ  PIC S9(4) COMP.
               10 PZM-PC-VDABLEINEUZ   PIC S9(6)V9(1) COMP-3.
               10 PZM-PC-BEITRAGEUZ    PIC S9(9)V9(2) COMP-3.
               10 PZM-PC-VDPRODKEYEUR  PIC S9(4) COMP.
               10 PZM-PC-VDABLEINEUR   PIC S9(6)V9(1) COMP-3.
               10 PZM-PC-BEITRAGEUR    PIC S9(9)V9(2) COMP-3.
               10 PZM-PC-BEITRAEUZ     PIC S9(9)V9(2) COMP-3.
               10 PZM-PC-BEITRAEUR     PIC S9(9)V9(2) COMP-3.
081210*<<<<<
250711*>>>>>
               10 PZM-PC-VDPRODKEYPA   PIC S9(4)      COMP.
               10 PZM-PC-VDABLEINPA    PIC S9(6)V9(1) COMP-3.
               10 PZM-PC-BEITRAPA      PIC S9(9)V9(2) COMP-3.
               10 PZM-PC-BEITRAGPA     PIC S9(9)V9(2) COMP-3.
240414         10 PZM-PC-VDPRODKEYVEPBP   PIC S9(4)      COMP.
240414         10 PZM-PC-VDABLEINVEPBP    PIC S9(6)V9(1) COMP-3.
250711*<<<<<
300516*>>>>>
               10 PZM-PC-VDPRODKEYSK   PIC S9(4)      COMP.
               10 PZM-PC-VDABLEINSK    PIC S9(6)V9(1) COMP-3.
               10 PZM-PC-BEITRASK      PIC S9(9)V9(2) COMP-3.
               10 PZM-PC-BEITRAGSK     PIC S9(9)V9(2) COMP-3.
300516*<<<<<
090518         10 PZM-PC-VDPSBUZI-VEPBP PIC 9(4)       COMP.
090518         10 PZM-PC-VDEHBUZI-VEPBP PIC S9(6)V9(1) COMP-3.
090518         10 PZM-PC-VDPSBUZR-VEPBP PIC 9(4)       COMP.
090518         10 PZM-PC-VDEHBUZR-VEPBP PIC S9(6)V9(1) COMP-3.
100918*>>>>>
               10 PZM-PC-VDPRODKEYBO   PIC S9(4)      COMP.
               10 PZM-PC-VDABLEINBO    PIC S9(6)V9(1) COMP-3.
               10 PZM-PC-BEITRAGBO     PIC S9(9)V9(2) COMP-3.
               10 PZM-PC-VDPRODKEYVTZV PIC S9(4)      COMP.
               10 PZM-PC-VDABLEINVTZV  PIC S9(6)V9(1) COMP-3.
               10 PZM-PC-BEITRAGVTZV   PIC S9(9)V9(2) COMP-3.
               10 PZM-PC-VDPRODKEYVBUZ PIC S9(4)      COMP.
               10 PZM-PC-VDABLEINVBUZ  PIC S9(6)V9(1) COMP-3.
               10 PZM-PC-BEITRAGVBUZ   PIC S9(9)V9(2) COMP-3.
100918*<<<<<
      *        SONSTIGE DATEN
               10  PZM-PC-DEPOT        PIC X(01).
120312*        10  PZM-PC-ASAMNR       PIC 9(01).
120312         10  PZM-PC-ASAMNR       PIC 9(02).
               10  PZM-PC-EUROKZ       PIC 9(01).
               10  PZM-PC-KD-BEST-ID   PIC S9(18)V    COMP-3.
      *
      *        BLACK-BOX (XGTV-DATEN)
               10 PZM-PC-XGDV.
                  15 PZM-PC-XGDV-LEN   PIC S9(4) COMP.
240414*           15 PZM-PC-XGDV-TEXT  PIC X(5000).
240414            15 PZM-PC-XGDV-TEXT  PIC X(20000).
      *
      ****** ENDE COPY-MEMBER ****************************************

