      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  09.06.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DROK0141.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DROK0141.CPv  $
      *
      *   Rev 2.4   Jul 04 2016 10:06:04   t13ae34
      *AAN01269: Anträge Fondsdepotbank - Berücksichtigung Fremdfonds
      *
      *   Rev 2.3   Feb 25 2016 10:13:58   t13ae23
      *AAN01240: Neue Feld "Fimenname (für DV-Antrag)" +
      *Elektra
      *- AGI, CKV - Deutsche Bank -
      *
      *   Rev 2.2   Feb 12 2016 07:58:38   t13ae34
      *AAN01211: Elektra - AGI, CKV, Deutsche Bank
      *
      *   Rev 2.1   Aug 26 2015 12:06:44   t13ae34
      *AAN01211: Elektra - AGI, CKV, Deutsche Bank
      *
      *   Rev 2.0   Jun 09 2015 15:41:36   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DROK0141                                      *
      *    --------------------------------------------------------   *
      *    ELEKTRONISCHE ANTRÄGE FÜR PRODUKTGEBER AGI (40)            *
      *                                                               *
      *                                                               *
      *    AUFBAU DER BLACK-BOX-DATEN IN ELEKTR_ANTRAEGE (TCOK0016)   *
      *    --------------------------------------------------------   *
      *    ACHTUNG: DER SATZAUFBAU IST NICHT IDENTISCH MIT TBAN0021   *
      *    --------------------------------------------------------   *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * AAN01211!02.07.15!2.1 ! NEU                       ! HEIDER    *
      * AAN01269!01.07.16!2.4 ! FREMDFONDS                ! HEIDER    *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *
       01  EA-DATEN-AGI.
 
           03  AGI-DATEN.
010716*        05  AGI-PGEB-GES             PIC 9(04).
               05  AGI-HVANTNUM             PIC X(09).
               05  AGI-PIN                  PIC 9(09).
               05  AGI-VB-GES               PIC 9(04).
               05  AGI-VDABVERM             PIC 9(09).
               05  AGI-VDABVERMPZ           PIC X(01).
               05  AGI-AFO-34C-ZEICHN       PIC 9(09).
               05  AGI-VDVMNR               PIC 9(09).
               05  AGI-VDVMNRPZ             PIC X(01).
               05  AGI-VDVMZUNA             PIC X(26).
               05  AGI-VDVMVORNA            PIC X(26).
               05  AGI-VDDIREKT             PIC 9(04).
               05  AGI-PDVNZUNA             PIC X(26).
               05  AGI-PDVNVORNA            PIC X(26).
               05  AGI-PDVNPLZ              PIC X(06).
               05  AGI-PDVNORT              PIC X(30).
               05  AGI-PDVNLKZ              PIC X(03).
010716*>>>>>
               05  AGI-NEBENPRODUKTE.
                   10 AGI-NP-TABELLE OCCURS 9.
                      15 AGI-NP-PGEB-GES    PIC 9(04).
                      15 AGI-NP-PROD        PIC 9(04).
                      15 AGI-NP-BETRAG-PROD PIC 9(09)V9(2).
                      15 AGI-NP-ZAHLWEISE   PIC 9(04).
                      15 AGI-NP-LAUFZEIT    PIC 9(04).
                      15 AGI-NP-EINH-PROD   PIC 9(06)V9.
      *
      *        05  A G I - P G E B - G E S 1            PIC 9(04).
      *        .......
010716*<<<<<
               05  AGI-EINH-GESAMT          PIC 9(06)V9.
               05  AGI-DYNAMIK              PIC 9(03)V9(3).
               05  AGI-ANTRAG-DATUM         PIC X(10).
               05  AGI-KUNDENBESTANDSID     PIC 9(18).
               05  AGI-VERSION              PIC 9(04).
 
      **** BLACK-BOX-DATEN DES PRODUKTGEBERS
               05  AGI-XGDV                 PIC X(32000).
      ****
      **** ENDE COPY DROK0141
      *****************************************************************
 



