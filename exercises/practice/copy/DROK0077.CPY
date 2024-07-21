      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  23.02.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DROK0077.CPY  $
      *    $Revision:   2.18  $
      *
      *    $Log:   L:/mf/CPY/DROK0077.CPv  $
      *
      *   Rev 2.18   Mar 02 2020 17:50:34   t13ae34
      *AAN01457: VDL-Leben: Einführung neuer Pflegetarife
      *
      *   Rev 2.17   Sep 12 2018 09:02:02   t13ae34
      *AAN01379: AMLeben: Überarbeitung KIZ-Produkt
      *
      *   Rev 2.16   May 15 2018 08:25:24   t13ae34
      *AAN01368: AM-Leben: Neue ungezillmerte bAV-Tarife
      *
      *   Rev 2.15   Jun 20 2016 12:39:08   t13ae34
      *AAN01263: Produktpaket Arbeitskraftabsicherung
      *
      *   Rev 2.14   Jun 03 2016 14:13:26   t13ae34
      *AAN01263: Produktpaket Arbeitskraftabsicherung
      *
      *   Rev 2.13   Jun 26 2014 08:15:28   t13ae34
      *AAN01168: VERMÖGENS- & EINKOMMENSPLAN - VEP
      *
      *   Rev 2.12   Dec 09 2013 14:40:52   t13ae34
      *AAN01139: neuer Pensionskassentarif PRGV
      *
      *   Rev 2.11   Mar 15 2012 11:30:34   t13ae34
      *AAN01054: VASP-Umstellungstarife
      *
      *   Rev 2.10   Sep 30 2011 09:35:58   t13ae34
      *AAN01025: AMLeben - Einführung Impulsprodukt - VASP mit TZV
      *(PS 124)
      *
      *   Rev 2.9   Sep 13 2011 13:10:58   t13ae23
      *AAN01025: AMLeben - Einführung Impulsprodukt - VASP mit TZV
      *(PS 124)
      *
      *   Rev 2.8   Aug 25 2011 10:53:48   t13ae34
      *AAN01025: AMLeben - Einführung Impulsprodukt - VASP mit TZV
      *(PS 124)
      *
      *   Rev 2.7   Dec 10 2010 13:00:56   t13ae34
      *AAN00977: Anpassung der Schnittstellen und Ressourcen
      *
      *   Rev 2.6   Sep 03 2010 16:24:36   t13ae34
      *AAN00971: PDP Ergänzung Ablaufdatum u. Ablaufsumme
      *
      *   Rev 2.5   Aug 23 2010 14:06:10   t13ae23
      *AAN00971: PDP Ergänzung Ablaufdatum u. Ablaufsumme
      *
      *   Rev 2.4   25 Apr 2008 11:46:28   T13EX32
      *AKI00556: PGMe - Elektronische Anträge
      *
      *   Rev 2.3   22 Apr 2008 13:09:22   T13EX32
      *AKI00556: PGMe - Elektronische Anträge
      *
      *   Rev 2.2   07 Apr 2008 14:48:40   T13EX32
      *AKI00556: PGMe - Elektronische Anträge
      *
      *   Rev 2.1   12 Jul 2007 16:07:24   t13ex43
      *AKI00556: PGMe - Elektronische Anträge
      *
      *   Rev 2.0   23 Feb 2007 09:04:26   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DROK0077                                      *
      *    --------------------------------------------------------   *
      *    ELEKTRONISCHE ANTRÄGE AML, GESELLSCHAFT 20                 *
      *    AUFBAU DER BLACK-BOX-DATEN IN ELEKTR_ANTRAEGE (TCOK0016)   *
      *    --------------------------------------------------------   *
      *    ACHTUNG: DER SATZAUFBAU IST NICHT IDENTISCH MIT TBAN0007   *
      *             SONDERN BERUHT AUF DER AUSGABESCHNITTSTELLE       *
      *             PC-DRUCKPROGRAMM DER AML (HR.SOUMAGNE/FR. RIECKE) *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    23.02.2007                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * AKI00556!23.02.07!2.1 ! NEU                       ! KLEEMEIER *
      * --------+--------+----+---------------------------+---------- *
      * AKI00556!07.04.08!2.2 ! Mandantenmerkmal eingefügt! Bößhar    *
      *         !        !    ! PZM-VB-GES                !           *
      * --------+--------+----+---------------------------+---------- *
      * AKI00556!25.04.08!2.3 ! PZM-VDPRODKEY auf 5 Byte  ! Bößhar    *
      *         !        !    ! verlängert                !           *
      * --------+--------+----+---------------------------+---------- *
      * AAN00971!23.08.10!2.4 ! NEUE FELDER FÜR TARIF PDP ! RAHNER    *
      * --------+--------+----+---------------------------+---------- *
      * AAN00977!08.12.10!2.7 ! NEUE FELDER FÜR TARIF EU  ! HEIDER    *
      *         !        !    ! + XGDV AUF 4000 (3000)    ! HEIDER    *
      * --------+--------+----+---------------------------+---------- *
      * AAN01025!25.07.11!2.5 ! VASP IMPULSPRODUKT        ! HEIDER    *
      * AAN01025!13.09.11!2.6 !                           ! RAHNER    *
      * --------+--------+----+---------------------------+---------- *
      * AAN01054!12.03.12!2.11! VASP UMSTELLUNGSTARIFE    ! HEIDER    *
      * --------+--------+----+---------------------------+---------- *
      * AAN01139!06.12.13!2.12! XGDV VERGRÖSSERN AUF 5000 ! HEIDER    *
      * --------+--------+----+---------------------------+---------- *
      * AAN01168!24.04.14!2.13! VEP, XGDV -> 20.000       !HEIDER     *
      * --------+--------+----+---------------------------+---------- *
      * AAN01263!30.05.16!2.14! ARBEITSKRAFTABSICHERUNG   !HEIDER     *
      * AAN01263!20.06.16!2.15! KORR NUR HOST (AN1263)    !RAHNER     *
      * AAN01368!09.05.18!2.16! BRGU                      !HEIDER     *
      * AAN01379!10.09.18!2.17! SKN SKNE (NEUE KIZ)       !HEIDER     *
      * AAN01457!26.02.20!2.18! NEUE PFLEGETARIFE         !HEIDER     *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *
       01 EA-DATEN-AML.

         03 PZM-DATEN.
           05 PZM-HVANTNUM PIC X(09).
           05 PZM-VB-GES PIC 9(02).
           05 PZM-VDABVERM PIC 9(07).
           05 PZM-VDABVERMPZ PIC X(01).
           05 PZM-VDVMNR PIC 9(07).
           05 PZM-VDVMNRPZ PIC X(01).
           05 PZM-VDVMVORNA PIC X(16).
           05 PZM-VDVMZUNA PIC X(21).
           05 PZM-VDDIREKT PIC 9(03).
           05 PZM-KD-BEST-ID PIC 9(18).
           05 PZM-PDVNANRED PIC X(02).
           05 PZM-PDVNVORNA PIC X(16).
           05 PZM-PDVNZUNA PIC X(33).
           05 PZM-PDVNGBDAT PIC X(10).
           05 PZM-PDVNPLZ PIC X(06).
           05 PZM-PDVNORT PIC X(30).
           05 PZM-PDVNLKZ PIC X(03).
          05 PZM-PDVNLAND PIC X(35).
          05 PZM-PDV1VORNA PIC X(16).
          05 PZM-PDV1ZUNA PIC X(21).
          05 PZM-PDV1GBDAT PIC X(10).
          05 PZM-PDV2GBDAT PIC X(10).
          05 PZM-HVTARIF PIC X(15).
          05 PZM-HVVBEGINN PIC X(10).
          05 PZM-HVN PIC 9(03).
          05 PZM-HVENDN PIC X(10).
          05 PZM-HVNS PIC 9(03).
          05 PZM-HVENDNS PIC X(10).
          05 PZM-HV8ENDALT PIC 9(03).
          05 PZM-HVBEGREN PIC X(10).
          05 PZM-HVVSUMME PIC 9(09)V99.
          05 PZM-HVENDSUMME PIC 9(09)V99.
          05 PZM-HVBEITRAG PIC 9(09)V99.
          05 PZM-EZAHLW PIC 9(02).
          05 PZM-EUROKZ PIC 9(01).
          05 PZM-ZBE PIC X(01).
          05 PZM-DEPOT PIC X(01).
          05 PZM-DIVE PIC X(01).
          05 FILLER PIC X(01).
          05 PZM-VDPRODKEY PIC 9(05).
          05 PZM-VDABLEIN PIC 9(06)V9.
 
          05 PZM-VDPRODKEYBUZI PIC 9(04).
          05 PZM-VDABLEINBUZI PIC 9(06)V9.
          05 PZM-BEITRAGBUZI PIC 9(09)V99.
 
          05 PZM-VDPRODKEYBUZR PIC 9(04).
          05 PZM-VDABLEINBUZR PIC 9(06)V9.
          05 PZM-BEITRAGBUZR PIC 9(09)V99.
 
          05 PZM-VDPRODKEYKIZ PIC 9(04).
          05 PZM-VDABLEINKIZ PIC 9(06)V9.
          05 PZM-BEITRAGKIZ PIC 9(09)V99.
 
          05 PZM-VDPRODKEYTZV PIC 9(04).
          05 PZM-VDABLEINTZV PIC 9(06)V9.
          05 PZM-BEITRAGTZV PIC 9(09)V99.
 
          05 PZM-VDPRODKEYVEPBP PIC 9(05).
          05 PZM-VDABLEINVEPBP PIC 9(06)V9.
 
          05 FILLER PIC X(08).
 
          05 PZM-ASAMNR PIC 9(02).
 
260220    05 PZM-VDPRODKEYPZV   PIC 9(04).
260220    05 PZM-VDABLEINPZV    PIC 9(06)V9.
260220    05 PZM-BEITRAGPZV     PIC 9(09)V99.
 
          05 PZM-VDPRODKEYPO PIC 9(04).
          05 PZM-VDABLEINPO PIC 9(06)V9.
          05 PZM-BEITRAGPO PIC 9(09)V99.
 
          05 PZM-VERSION PIC X(05).
          05 PZM-BEITRAHV PIC 9(9)V99.
          05 PZM-BEITRABUZI PIC 9(9)V99.
          05 PZM-BEITRABUZR PIC 9(9)V99.
          05 PZM-BEITRAKIZ PIC 9(9)V99.
          05 PZM-BEITRATZV PIC 9(9)V99.
260220    05 FILLER        PIC X(11).
260220    05 PZM-BEITRAPZV PIC 9(9)V99.
          05 PZM-BEITRAPO PIC 9(9)V99.
          05 PZM-VDPRODKEYBUZI-VEPBP PIC 9(4).
          05 PZM-VDABLEINBUZI-VEPBP PIC 9(6)V9(1).
          05 FILLER PIC X(11).
          05 PZM-VDPRODKEYBUZR-VEPBP PIC 9(4).
          05 PZM-VDABLEINBUZR-VEPBP PIC 9(6)V9(1).
          05 PZM-VDPRODKEYBO PIC 9(04).
          05 PZM-VDABLEINBO PIC 9(06)V9.
          05 PZM-BEITRAGBO PIC 9(09)V99.
          05 FILLER PIC X(11).
 
260220    05 PZM-VDPRODKEYPZVB PIC 9(5).
260220    05 PZM-VDABLEINPZVB PIC 9(6)V9(1).
260220    05 PZM-BEITRAPZVB PIC 9(9)V9(2).
260220    05 PZM-BEITRAGPZVB PIC 9(9)V9(2).
          05 PZM-VDPRODKEYSK PIC 9(5).
          05 PZM-VDABLEINSK PIC 9(6)V9(1).
          05 PZM-BEITRASK PIC 9(9)V9(2).
          05 PZM-BEITRAGSK PIC 9(9)V9(2).
          05 PZM-VDPRODKEYVTZV PIC 9(04).
          05 PZM-VDABLEINVTZV PIC 9(06)V9.
          05 PZM-BEITRAGVTZV PIC 9(09)V99.
          05 PZM-VDPRODKEYVBUZ PIC 9(04).
          05 PZM-VDABLEINVBUZ PIC 9(06)V9.
          05 PZM-BEITRAGVBUZ PIC 9(09)V99.
260220    05 PZM-PROZ2PR     PIC 9(03).
260220    05 PZM-PROZ3PR     PIC 9(03).
260220    05 PZM-PROZ4PR     PIC 9(03).
260220    05 PZM-LSTERHPR    PIC 9(02)V9(01).
260220    05 PZM-VABLPR      PIC X(10).
262020*   05 FILLER PIC X(182).
262020    05 FILLER PIC X(160).
          05 PZM-XGDV PIC X(20000).
     ****
     **** ENDE COPY DROK0077 *****************************************
 
 
