      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  09.06.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DROK0140.CPY  $
      *    $Revision:   2.9  $
      *
      *    $Log:   L:/mf/CPY/DROK0140.CPv  $
      *
      *   Rev 2.9   Feb 25 2016 10:13:48   t13ae23
      *AAN01240: Neue Feld "Fimenname (für DV-Antrag)" + 
      *Elektra 
      *- AGI, CKV - Deutsche Bank -
      *
      *   Rev 2.8   Feb 12 2016 07:58:30   t13ae34
      *AAN01211: Elektra - AGI, CKV, Deutsche Bank
      *
      *   Rev 2.7   Feb 11 2016 16:05:18   t13ae34
      *AAN01211: Elektra - AGI, CKV, Deutsche Bank
      *
      *   Rev 2.6   Jan 22 2016 09:18:20   t13ae34
      *AAN01230: Archivierung der Generali Verbraucherinformation +
      * CKV
      *
      *   Rev 2.5   Jan 22 2016 09:11:10   t13ae34
      *AAN01238: Elektra-Anforderungen R. 7.28 - CKV
      *
      *   Rev 2.4   Nov 16 2015 11:48:52   t13ae34
      *AAN01211: Elektra - AGI, CKV, Deutsche Bank
      *
      *   Rev 2.3   Nov 16 2015 11:41:46   t13ae34
      *AAN01211: Elektra - AGI, CKV, Deutsche Bank
      *
      *   Rev 2.2   Oct 23 2015 09:26:04   t13ae34
      *AAN01211: Elektra - AGI, CKV, Deutsche Bank
      *
      *   Rev 2.1   Aug 26 2015 12:06:36   t13ae34
      *AAN01211: Elektra - AGI, CKV, Deutsche Bank
      *
      *   Rev 2.0   Jun 09 2015 15:41:26   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DROK0140                                      *
      *    --------------------------------------------------------   *
      *    ELEKTRONISCHE ANTRÄGE FÜR PRODUKTGEBER CKV (50)            *
      *                                                               *
      *                                                               *
      *    AUFBAU DER BLACK-BOX-DATEN IN ELEKTR_ANTRAEGE (TCOK0016)   *
      *    --------------------------------------------------------   *
      *    ACHTUNG: DER SATZAUFBAU IST NICHT IDENTISCH MIT TBAN0023   *
      *    --------------------------------------------------------   *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * AAN01211!02.07.15!2.1 ! NEU                       ! HEIDER    *
      * AAN01211!23.10.15!2.2 ! NEU                       ! HEIDER    *
      * AAN01211!16.11.15!2.3 ! VB,VM,ZEICHNER 7-STELLIG  ! HEIDER    *
      * AAN01211!16.11.15!2.4 ! PIN 7-STELLIG             ! HEIDER    *
      * AAN01238!05.01.16!2.5 ! RELEASE 7.26              ! HEIDER    *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *
       01  EA-DATEN-CKV.
 
           03  CKV-DATEN.
               05  CKV-PGEB-GES             PIC 9(04).
               05  CKV-HVANTNUM             PIC X(09).
               05  CKV-PIN                  PIC 9(07).
               05  CKV-ANTRAG-DATUM         PIC X(10).
               05  CKV-VDABVERM             PIC 9(07).
               05  CKV-VDVMNR               PIC 9(07).
               05  CKV-VDVMZUNA             PIC X(26).
               05  CKV-VDVMVORNA            PIC X(26).
               05  CKV-VDDIREKT             PIC 9(04).
               05  CKV-PDVNZUNA             PIC X(26).
               05  CKV-PDVNVORNA            PIC X(26).
               05  CKV-PDVNPLZ              PIC X(06).
               05  CKV-PDVNORT              PIC X(30).
               05  CKV-PDVNLKZ              PIC X(03).
               05  CKV-BEITRAG-GESAMT       PIC 9(09)V9(2).
               05  CKV-VERSION              PIC 9(04).
 
               05  CKV-PROD1                PIC 9(04).
               05  CKV-TARIF1               PIC 9(09).
               05  CKV-BEITRAG-PROD1        PIC 9(09)V9(2).
               05  CKV-EH-PROD1             PIC 9(06)V9.
               05  CKV-PERSON-PROD1         PIC 9(04).
 
               05  CKV-PROD2                PIC 9(04).
               05  CKV-TARIF2               PIC 9(09).
               05  CKV-BEITRAG-PROD2        PIC 9(09)V9(2).
               05  CKV-EH-PROD2             PIC 9(06)V9.
               05  CKV-PERSON-PROD2         PIC 9(04).
 
               05  CKV-PROD3                PIC 9(04).
               05  CKV-TARIF3               PIC 9(09).
               05  CKV-BEITRAG-PROD3        PIC 9(09)V9(2).
               05  CKV-EH-PROD3             PIC 9(06)V9.
               05  CKV-PERSON-PROD3         PIC 9(04).
 
               05  CKV-PROD4                PIC 9(04).
               05  CKV-TARIF4               PIC 9(09).
               05  CKV-BEITRAG-PROD4        PIC 9(09)V9(2).
               05  CKV-EH-PROD4             PIC 9(06)V9.
               05  CKV-PERSON-PROD4         PIC 9(04).
 
               05  CKV-PROD5                PIC 9(04).
               05  CKV-TARIF5               PIC 9(09).
               05  CKV-BEITRAG-PROD5        PIC 9(09)V9(2).
               05  CKV-EH-PROD5             PIC 9(06)V9.
               05  CKV-PERSON-PROD5         PIC 9(04).
 
               05  CKV-PROD6                PIC 9(04).
               05  CKV-TARIF6               PIC 9(09).
               05  CKV-BEITRAG-PROD6        PIC 9(09)V9(2).
               05  CKV-EH-PROD6             PIC 9(06)V9.
               05  CKV-PERSON-PROD6         PIC 9(04).
 
               05  CKV-PROD7                PIC 9(04).
               05  CKV-TARIF7               PIC 9(09).
               05  CKV-BEITRAG-PROD7        PIC 9(09)V9(2).
               05  CKV-EH-PROD7             PIC 9(06)V9.
               05  CKV-PERSON-PROD7         PIC 9(04).
 
               05  CKV-PROD8                PIC 9(04).
               05  CKV-TARIF8               PIC 9(09).
               05  CKV-BEITRAG-PROD8        PIC 9(09)V9(2).
               05  CKV-EH-PROD8             PIC 9(06)V9.
               05  CKV-PERSON-PROD8         PIC 9(04).
 
               05  CKV-PROD9                PIC 9(04).
               05  CKV-TARIF9               PIC 9(09).
               05  CKV-BEITRAG-PROD9        PIC 9(09)V9(2).
               05  CKV-EH-PROD9             PIC 9(06)V9.
               05  CKV-PERSON-PROD9         PIC 9(04).
 
      **** BLACK-BOX-DATEN DES PRODUKTGEBERS
               05  CKV-BLACK-BOX            PIC X(32000).
      ****
      **** ENDE COPY DROK0140
      *****************************************************************
 
 
 


