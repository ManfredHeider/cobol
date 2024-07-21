      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  09.06.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DROK0142.CPY  $
      *    $Revision:   2.8  $
      *
      *    $Log:   L:/mf/CPY/DROK0142.CPv  $
      *
      *   Rev 2.8   Jul 24 2020 10:07:16   t13ae23
      *AAN01474: Teilmaschinelle Anträge Deutsche Bank - PIN aus Vo
      *rschlagsdaten an Hostdialog übergeben
      *
      *   Rev 2.7   Jul 04 2016 10:06:12   t13ae34
      *AAN01269: Anträge Fondsdepotbank - Berücksichtigung Fremdfonds
      *
      *   Rev 2.6   Feb 25 2016 10:14:08   t13ae23
      *AAN01240: Neue Feld "Fimenname (für DV-Antrag)" +
      *Elektra
      *- AGI, CKV - Deutsche Bank -
      *
      *   Rev 2.5   Feb 19 2016 08:10:44   t13ae34
      *AAN01211: Elektra - AGI, CKV, Deutsche Bank
      *
      *   Rev 2.4   Feb 12 2016 07:58:46   t13ae34
      *AAN01211: Elektra - AGI, CKV, Deutsche Bank
      *
      *   Rev 2.3   Feb 11 2016 16:05:26   t13ae34
      *AAN01211: Elektra - AGI, CKV, Deutsche Bank
      *
      *   Rev 2.2   Oct 23 2015 07:40:56   t13ae34
      *AAN01211: Elektra - AGI, CKV, Deutsche Bank
      *
      *   Rev 2.1   Aug 26 2015 12:06:52   t13ae34
      *AAN01211: Elektra - AGI, CKV, Deutsche Bank
      *
      *   Rev 2.0   Jun 09 2015 15:41:44   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DROK0142                                      *
      *    --------------------------------------------------------   *
      *    ELEKTRONISCHE ANTRÄGE FÜR PRODUKTGEBER DB (60)             *
      *                                       UND DWS (48)            *
      *                                                               *
      *    AUFBAU DER BLACK-BOX-DATEN IN ELEKTR_ANTRAEGE (TCOK0016)   *
      *    --------------------------------------------------------   *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * AAN01211!02.07.15!2.1 ! NEU                       ! HEIDER    *
      * AAN01211!28.08.15!2.1 ! NEU                       ! HEIDER    *
      * AAN01211!08.02.16!2.3 ! ZUSAMMENLEGUNG DWS+DB     ! HEIDER    *
      * AAN01211!15.02.16!2.4 ! STELLENNR. 9-ST. GELIEFERT! HEIDER    *
      * AAN01269!01.07.16!2.6 ! FREMDFONDS                ! HEIDER    *
      * AAN01474!24.07.20!2.7 ! MEHR ALS 9 PRODUKTE       ! RAHNER    *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *
       01  EA-DATEN-DB.
 
           03  DB-DATEN.
               05  DB-PGEB-GES             PIC 9(04).
               05  DB-HVANTNUM             PIC X(09).
               05  DB-PIN                  PIC 9(09).
               05  DB-PROD                 PIC 9(04).
               05  DB-BETRAG-PROD          PIC 9(09)V9(2).
               05  DB-ZAHLWEISE            PIC 9(04).
               05  DB-LAUFZEIT             PIC 9(04).
               05  DB-EINH-PROD            PIC 9(06)V9.
 
               05  DB-VB-GES               PIC 9(04).
               05  DB-VDABVERM             PIC 9(09).
               05  DB-VDABVERMPZ           PIC X(01).
               05  DB-AFO-34C-ZEICHN       PIC 9(09).
               05  DB-VDVMNR               PIC 9(09).
               05  DB-VDVMNRPZ             PIC X(01).
               05  DB-VDVMZUNA             PIC X(26).
               05  DB-VDVMVORNA            PIC X(26).
               05  DB-VDDIREKT             PIC 9(04).
               05  DB-PDVNZUNA             PIC X(26).
               05  DB-PDVNVORNA            PIC X(26).
               05  DB-PDVNPLZ              PIC X(06).
               05  DB-PDVNORT              PIC X(30).
               05  DB-PDVNLKZ              PIC X(03).
 
010716*        05  DB-PGEB-GES-NP          PIC 9(04).
               05  DB-PIN-NP               PIC 9(09).
 
               05  DB-NEBENPRODUKTE.
                   10 DB-NP-TABELLE OCCURS 9.
010716                15 DB-NP-PGEB-GES    PIC 9(04).
                      15 DB-NP-PROD        PIC 9(04).
                      15 DB-NP-BETRAG-PROD PIC 9(09)V9(2).
                      15 DB-NP-ZAHLWEISE   PIC 9(04).
                      15 DB-NP-LAUFZEIT    PIC 9(04).
                      15 DB-NP-EINH-PROD   PIC 9(06)V9.
 
               05  DB-EINH-GESAMT          PIC 9(06)V9.
               05  DB-DYNAMIK              PIC 9(03)V9(3).
               05  DB-ANTRAG-DATUM         PIC X(10).
               05  DB-KUNDENBESTANDSID     PIC 9(18).
               05  DB-VERSION              PIC 9(04).
 
240720         05  DB-WEITERE-H-PRODUKTE.
240720             10 DB-W-HP-TABELLE OCCURS 25.
240720                15 DB-W-HP-PGEB-GES    PIC 9(04).
240720                15 DB-W-HP-PROD        PIC 9(04).
240720                15 DB-W-HP-BETRAG-PROD PIC 9(09)V9(2).
240720                15 DB-W-HP-ZAHLWEISE   PIC 9(04).
240720                15 DB-W-HP-LAUFZEIT    PIC 9(04).
240720                15 DB-W-HP-EINH-PROD   PIC 9(06)V9.
240720
240720         05  DB-WEITERE-N-PRODUKTE.
240720             10 DB-W-NP-TABELLE OCCURS 25.
240720                15 DB-W-NP-PGEB-GES    PIC 9(04).
240720                15 DB-W-NP-PROD        PIC 9(04).
240720                15 DB-W-NP-BETRAG-PROD PIC 9(09)V9(2).
240720                15 DB-W-NP-ZAHLWEISE   PIC 9(04).
240720                15 DB-W-NP-LAUFZEIT    PIC 9(04).
240720                15 DB-W-NP-EINH-PROD   PIC 9(06)V9.
 
      **** BLACK-BOX-DATEN DES PRODUKTGEBERS
240720*        05  DB-BLACK-BOX-DATEN      PIC X(32000).
240720         05  DB-BLACK-BOX-DATEN      PIC X(29708).
 
      ****
      **** ENDE COPY DROK0142
      *****************************************************************

