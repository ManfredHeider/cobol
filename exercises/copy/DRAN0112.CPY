      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  10.07.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0112.CPY  $
      *    $Revision:   2.15  $
      *
      *    $Log:   L:/mf/CPY/DRAN0112.CPv  $
      *
      *   Rev 2.15   Sep 12 2018 09:01:38   t13ae34
      *AAN01379: AMLeben: Überarbeitung KIZ-Produkt
      *
      *   Rev 2.14   May 15 2018 08:25:08   t13ae34
      *AAN01368: AM-Leben: Neue ungezillmerte bAV-Tarife
      *
      *   Rev 2.13   Jun 03 2016 14:13:06   t13ae34
      *AAN01263: Produktpaket Arbeitskraftabsicherung
      *
      *   Rev 2.12   May 28 2015 09:52:52   t13ae34
      *AAN01211: Elektronischer Antrag - Änderungen eingereichtes G
      *eschäft - Einpflegen in Vorschlagsdaten
      *
      *   Rev 2.11   Apr 23 2015 13:24:46   t13ae34
      *AAN01211: Elektronischer Antrag - Änderungen eingereichtes G
      *eschäft - Einpflegen in Vorschlagsdaten
      *
      *   Rev 2.10   Jun 26 2014 08:14:58   t13ae34
      *AAN01168: VERMÖGENS- & EINKOMMENSPLAN - VEP
      *
      *   Rev 2.9   Dec 09 2013 14:40:38   t13ae34
      *AAN01139: neuer Pensionskassentarif PRGV
      *
      *   Rev 2.8   Mar 15 2012 11:30:12   t13ae34
      *AAN01054: VASP-Umstellungstarife
      *
      *   Rev 2.7   Sep 30 2011 09:35:34   t13ae34
      *AAN01025: AMLeben - Einführung Impulsprodukt - VASP mit TZV
      *(PS 124)
      *
      *   Rev 2.6   Sep 13 2011 13:10:42   t13ae23
      *AAN01025: AMLeben - Einführung Impulsprodukt - VASP mit TZV
      *(PS 124)
      *
      *   Rev 2.5   Aug 25 2011 10:53:32   t13ae34
      *AAN01025: AMLeben - Einführung Impulsprodukt - VASP mit TZV
      *(PS 124)
      *
      *   Rev 2.4   Dec 10 2010 13:00:34   t13ae34
      *AAN00977: Anpassung der Schnittstellen und Ressourcen
      *
      *   Rev 2.3   Sep 03 2010 16:24:22   t13ae34
      *AAN00971: PDP Ergänzung Ablaufdatum u. Ablaufsumme
      *
      *   Rev 2.2   27 Dec 2007 11:10:48   t13ae23
      *AAN00730: VRA - Kennzeichen Vertriebs-Ges in elektr. Daten G
      *es. 20 AM-Leben
      *
      *   Rev 2.1   08 Sep 2006 09:40:44   T13AE23
      *AAN00685: elektr. Anträge AM-Leben: Umstellung auf "Header/B
      *lackbox"
      *
      *   Rev 2.0   10 Jul 2006 15:04:52   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0112  FUNKTION INSERT STANDARD-OZP
      *    NEU: UMSTELLUNG IM RAHMEN EINFÜHRUNG BLACK-BOX VERFAHREN
      *         BEI AM-LEBEN. ERSETZT DRAN0035
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00685!08.09.06!2.1 ! NEUERSTELLUNG             ! RAHNER   *
      * AAN00730!28.12.07!2.2 ! ALLFINANZ                 ! RAHNER   *
      * AAN00971!31.08.10!2.3 ! TARIF PDP                 ! HEIDER   *
      * AAN00977!08.12.10!2.4 ! TARIF EU + XGDV AUF 4000  ! HEIDER   *
      * AAN01025!25.07.11!2.5 ! VASP IMPULSPRODUKT        ! HEIDER   *
      * AAN01025!13.09.11!2.6 ! VASP IMPULSPRODUKT        ! RAHNER   *
      * AAN01054!12.03.12!2.8 ! VASP UMSTELLUNGSTARIFE    ! HEIDER   *
      * AAN01139!06.12.13!2.9 ! XGDV VERGRÖSSERN AUF 5000 ! HEIDER   *
      * AAN01168!24.04.14!2.10! VEP, XGDV -> 20.000       ! HEIDER   *
      * AAN01211!10.03.15!2.11! ESIGN                     ! HEIDER   *
      * AAN01263!30.05.16!2.13! ARBEITSKRAFTABSICHERUNG   ! HEIDER   *
      * AAN01368!09.05.18!2.14! BRGU                      ! HEIDER   *
      * AAN01379!10.09.18!2.15! SKN SKNE (NEUE KIZ)       ! HEIDER   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0112                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT  <<<       TABELLE PZMA TBAN0007              *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0112.
           05 PI-DRAN0112-PZMA-KEY.
              10 PI-DRAN0112-PGEB-GES          PIC S9(4)      COMP.
              10 PI-DRAN0112-PZM-HVANTNUM      PIC X(9).
           05 PI-DRAN0112-PZMA-DATEN.
      *        DATEN ZUR HAUPTVERSICHERUNG
              10  PI-DRAN0112-PZM-PIN          PIC S9(9)      COMP.
              10  PI-DRAN0112-PZM-HVTARIF      PIC X(15).
              10  PI-DRAN0112-PZM-HVVBEGINN    PIC X(10).
              10  PI-DRAN0112-PZM-HVN          PIC 9(03)      COMP-3.
310810        10  PI-DRAN0112-PZM-HVENDN       PIC X(10).
              10  PI-DRAN0112-PZM-HVNS         PIC 9(03)      COMP-3.
              10  PI-DRAN0112-PZM-HVENDNS      PIC X(10).
              10  PI-DRAN0112-PZM-HV8ENDALT    PIC 9(03)      COMP-3.
              10  PI-DRAN0112-PZM-EZAHLW       PIC 9(02).
              10  PI-DRAN0112-PZM-HVBEITRAG    PIC S9(09)V9(02) COMP-3.
              10  PI-DRAN0112-PZM-HVVSUMME     PIC S9(09)V9(02) COMP-3.
310810        10  PI-DRAN0112-PZM-HVENDSUMME   PIC S9(09)V9(02) COMP-3.
              10  PI-DRAN0112-PZM-HVBEGREN     PIC X(10).
              10  PI-DRAN0112-PZM-ZBE          PIC X(01).
      *
      *        VERTRIEBSDATEN / INTERNE DATEN
      *            ABSCHLUSSVERMITTLER
             10  PI-DRAN0112-PZM-VDABVERM     PIC 9(07)       COMP-3.
      *          PRÜFZIFFER AVERM
             10  PI-DRAN0112-PZM-VDABVERMPZ   PIC X(01).
      *          VM
             10  PI-DRAN0112-PZM-VDVMNR       PIC 9(07)       COMP-3.
      *          PRÜFZIFFER VM
             10  PI-DRAN0112-PZM-VDVMNRPZ     PIC X(01).
      *          VOR- UND ZUNAME VM
             10  PI-DRAN0112-PZM-VDVMVORNA    PIC X(16).
             10  PI-DRAN0112-PZM-VDVMZUNA     PIC X(21).
      *          DVAG-PRODUKTSCHLÜSSEL
             10  PI-DRAN0112-PZM-VDPRODKEY    PIC 9(04)       COMP-3.
      *          DVAG-ABSCHLUSSEINHEITEN
             10  PI-DRAN0112-PZM-VDABLEIN     PIC S9(6)V9(01) COMP-3.
      *          DVAG-DIREKTION
             10  PI-DRAN0112-PZM-VDDIREKT     PIC 9(03).
281207*          VERTRIEBS-GESELLSCHAFT DES VB
281207       10  PI-DRAN0112-PZM-VB-GES       PIC 9(03).
100315*          PRÜFZIFFER AVERM
100315       10  PI-DRAN0112-PZM-ESIGN        PIC X(01).
      *
      *      VERSIONSDATEN
             10  PI-DRAN0112-PZM-VERSION      PIC X(05).
      *
      *      VERSICHERUNGSNEHMER/ANTRAGSTELLER
             10  PI-DRAN0112-PZM-PDVNANRED    PIC X(02).
             10  PI-DRAN0112-PZM-PDVNVORNA    PIC X(16).
             10  PI-DRAN0112-PZM-PDVNZUNA     PIC X(33).
             10  PI-DRAN0112-PZM-PDVNLKZ      PIC X(03).
             10  PI-DRAN0112-PZM-PDVNLAND     PIC X(35).
             10  PI-DRAN0112-PZM-PDVNPLZ      PIC X(06).
             10  PI-DRAN0112-PZM-PDVNORT      PIC X(30).
             10  PI-DRAN0112-PZM-PDVNGBDAT    PIC X(10).
      *
      *      ERSTE VERSICHERTE PERSON
             10  PI-DRAN0112-PZM-PDV1VORNA    PIC X(16).
             10  PI-DRAN0112-PZM-PDV1ZUNA     PIC X(33).
             10  PI-DRAN0112-PZM-PDV1GBDAT    PIC X(10).
      *
      *      ZWEITE VERSICHERTE PERSON
             10  PI-DRAN0112-PZM-PDV2GBDAT    PIC X(10).
      *
      *      STATISTIKDATEN DVAG
             10 PI-DRAN0112-PZM-DIVE          PIC X.
             10 PI-DRAN0112-PZM-VDPRODKEYBUZI PIC S9(4)      COMP.
             10 PI-DRAN0112-PZM-VDABLEINBUZI  PIC S9(6)V9(1) COMP-3.
             10 PI-DRAN0112-PZM-BEITRAGBUZI   PIC S9(9)V9(2) COMP-3.
             10 PI-DRAN0112-PZM-VDPRODKEYBUZR PIC S9(4)      COMP.
             10 PI-DRAN0112-PZM-VDABLEINBUZR  PIC S9(6)V9(1) COMP-3.
             10 PI-DRAN0112-PZM-BEITRAGBUZR   PIC S9(9)V9(2) COMP-3.
             10 PI-DRAN0112-PZM-VDPRODKEYKIZ  PIC S9(4)      COMP.
             10 PI-DRAN0112-PZM-VDABLEINKIZ   PIC S9(6)V9(1) COMP-3.
             10 PI-DRAN0112-PZM-BEITRAGKIZ    PIC S9(9)V9(2) COMP-3.
             10 PI-DRAN0112-PZM-VDPRODKEYTZV  PIC S9(4)      COMP.
             10 PI-DRAN0112-PZM-VDABLEINTZV   PIC S9(6)V9(1) COMP-3.
             10 PI-DRAN0112-PZM-BEITRAGTZV    PIC S9(9)V9(2) COMP-3.
             10 PI-DRAN0112-PZM-VDPRODKEYHZV  PIC S9(4)      COMP.
             10 PI-DRAN0112-PZM-VDABLEINHZV   PIC S9(6)V9(1) COMP-3.
             10 PI-DRAN0112-PZM-BEITRAGHZV    PIC S9(9)V9(2) COMP-3.
             10 PI-DRAN0112-PZM-VDPRODKEYP    PIC S9(4)      COMP.
             10 PI-DRAN0112-PZM-VDABLEINP     PIC S9(6)V9(1) COMP-3.
             10 PI-DRAN0112-PZM-BEITRAGP      PIC S9(9)V9(2) COMP-3.
             10 PI-DRAN0112-PZM-VDPRODKEYPO   PIC S9(4)      COMP.
             10 PI-DRAN0112-PZM-VDABLEINPO    PIC S9(6)V9(1) COMP-3.
             10 PI-DRAN0112-PZM-BEITRAGPO     PIC S9(9)V9(2) COMP-3.
281207       10 PI-DRAN0112-PZM-BEITRABUZI    PIC S9(9)V9(2) COMP-3.
281207       10 PI-DRAN0112-PZM-BEITRABUZR    PIC S9(9)V9(2) COMP-3.
281207       10 PI-DRAN0112-PZM-BEITRAKIZ     PIC S9(9)V9(2) COMP-3.
281207       10 PI-DRAN0112-PZM-BEITRATZV     PIC S9(9)V9(2) COMP-3.
281207       10 PI-DRAN0112-PZM-BEITRAHZV     PIC S9(9)V9(2) COMP-3.
281207       10 PI-DRAN0112-PZM-BEITRAB       PIC S9(9)V9(2) COMP-3.
281207       10 PI-DRAN0112-PZM-BEITRAPO      PIC S9(9)V9(2) COMP-3.
281207       10 PI-DRAN0112-PZM-BEITRAHV      PIC S9(9)V9(2) COMP-3.
081210*>>>>>
             10 PI-DRAN0112-PZM-VDPRODKEYEUZ  PIC S9(4)      COMP.
             10 PI-DRAN0112-PZM-VDABLEINEUZ   PIC S9(6)V9(1) COMP-3.
             10 PI-DRAN0112-PZM-BEITRAGEUZ    PIC S9(9)V9(2) COMP-3.
             10 PI-DRAN0112-PZM-VDPRODKEYEUR  PIC S9(4)      COMP.
             10 PI-DRAN0112-PZM-VDABLEINEUR   PIC S9(6)V9(1) COMP-3.
             10 PI-DRAN0112-PZM-BEITRAGEUR    PIC S9(9)V9(2) COMP-3.
             10 PI-DRAN0112-PZM-BEITRAEUZ     PIC S9(9)V9(2) COMP-3.
             10 PI-DRAN0112-PZM-BEITRAEUR     PIC S9(9)V9(2) COMP-3.
081210*<<<<<
250711*>>>>>
130911*      10 PI-DRAN0112-PZM-VDPRODKEYPA   PIC S9(4)      COMP.
130911       10 PI-DRAN0112-PZM-VDPRODKEYPA   PIC 9(4)       COMP-3.
             10 PI-DRAN0112-PZM-VDABLEINPA    PIC S9(6)V9(1) COMP-3.
             10 PI-DRAN0112-PZM-BEITRAPA      PIC S9(9)V9(2) COMP-3.
             10 PI-DRAN0112-PZM-BEITRAGPA     PIC S9(9)V9(2) COMP-3.
240414       10 PI-DRAN0112-PZM-VDPRODKEYVEPBP   PIC S9(4)      COMP.
240414       10 PI-DRAN0112-PZM-VDABLEINVEPBP    PIC S9(6)V9(1) COMP-3.
250711*<<<<<
300516*>>>>>
             10 PI-DRAN0112-PZM-VDPRODKEYSK   PIC 9(4)       COMP-3.
             10 PI-DRAN0112-PZM-VDABLEINSK    PIC S9(6)V9(1) COMP-3.
             10 PI-DRAN0112-PZM-BEITRASK      PIC S9(9)V9(2) COMP-3.
             10 PI-DRAN0112-PZM-BEITRAGSK     PIC S9(9)V9(2) COMP-3.
300516*<<<<<
090518       10 PI-DRAN0112-VDPSBUZI-VEPBP    PIC 9(4)       COMP.
090518       10 PI-DRAN0112-VDEHBUZI-VEPBP    PIC S9(6)V9(1) COMP-3.
090518       10 PI-DRAN0112-VDPSBUZR-VEPBP    PIC 9(4)       COMP.
090518       10 PI-DRAN0112-VDEHBUZR-VEPBP    PIC S9(6)V9(1) COMP-3.
100918*>>>>>
             10 PI-DRAN0112-PZM-VDPRODKEYBO   PIC 9(4)       COMP-3.
             10 PI-DRAN0112-PZM-VDABLEINBO    PIC S9(6)V9(1) COMP-3.
             10 PI-DRAN0112-PZM-BEITRAGBO     PIC S9(9)V9(2) COMP-3.
             10 PI-DRAN0112-PZM-VDPRODKEYVTZV PIC 9(4)       COMP-3.
             10 PI-DRAN0112-PZM-VDABLEINVTZV  PIC S9(6)V9(1) COMP-3.
             10 PI-DRAN0112-PZM-BEITRAGVTZV   PIC S9(9)V9(2) COMP-3.
             10 PI-DRAN0112-PZM-VDPRODKEYVBUZ PIC 9(4)       COMP-3.
             10 PI-DRAN0112-PZM-VDABLEINVBUZ  PIC S9(6)V9(1) COMP-3.
             10 PI-DRAN0112-PZM-BEITRAGVBUZ   PIC S9(9)V9(2) COMP-3.
100918*<<<<<
      *      SONSTIGE DATEN
             10 PI-DRAN0112-PZM-DEPOT         PIC X(01).
120312*      10 PI-DRAN0112-PZM-ASAMNR        PIC 9(01).
120312       10 PI-DRAN0112-PZM-ASAMNR        PIC 9(02).
             10 PI-DRAN0112-PZM-EUROKZ        PIC 9(01).
             10 PI-DRAN0112-PZM-KD-BEST-ID    PIC S9(18)V    COMP-3.
      *
      *      BLACK-BOX (XGTV-DATEN)
             10 PI-DRAN0112-PZM-XGDV.
                15 PI-DRAN0112-PZM-XGDV-LEN   PIC S9(4)      COMP.
240414*         15 PI-DRAN0112-PZM-XGDV-TEXT  PIC X(5000).
240414          15 PI-DRAN0112-PZM-XGDV-TEXT  PIC X(20000).
      *
       01  PO-DRAN0112.
           05 PO-DRAN0112-RC                  PIC  X(003).
             88 PO-DRAN0112-RC-OK                         VALUE 'OK '.
             88 PO-DRAN0112-RC-DUPLICATE-VALUE            VALUE 'DVA'.
      *
      ****** ENDE COPY-MEMBER ****************************************

