      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  10.06.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0294.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRAN0294.CPv  $
      *
      *   Rev 2.6   Feb 25 2016 10:13:38   t13ae23
      *AAN01240: Neue Feld "Fimenname (für DV-Antrag)" + 
      *Elektra 
      *- AGI, CKV - Deutsche Bank -
      *
      *   Rev 2.5   Feb 12 2016 07:58:22   t13ae34
      *AAN01211: Elektra - AGI, CKV, Deutsche Bank
      *
      *   Rev 2.4   Feb 11 2016 16:05:10   t13ae34
      *AAN01211: Elektra - AGI, CKV, Deutsche Bank
      *
      *   Rev 2.3   Jan 22 2016 09:18:14   t13ae34
      *AAN01230: Archivierung der Generali Verbraucherinformation +
      * CKV
      *
      *   Rev 2.2   Jan 22 2016 09:11:02   t13ae34
      *AAN01238: Elektra-Anforderungen R. 7.28 - CKV
      *
      *   Rev 2.1   Aug 26 2015 12:06:28   t13ae34
      *AAN01211: Elektra - AGI, CKV, Deutsche Bank
      *
      *   Rev 2.0   Jun 10 2015 12:59:38   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    PC-DATEN BEI ELEKTRONISCHEN VORSCHLAGSDATEN CKV (50)
      ****************************************************************
      *    COPY-MEMBER: DRAN0294                                     *
      *    SCHNITTSTELLE                                             *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN01238!05.01.16!2.2 ! NEU                       ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
           05 CKV-PC-DATEN.
 
              10 CKV-PC-HVANTNUM         PIC X(9).
              10 CKV-PC-PIN              PIC S9(9) COMP.
              10 CKV-PC-PGEB-GES         PIC S9(4) COMP.
              10 CKV-PC-VDABVERM         PIC S9(9) COMP.
              10 CKV-PC-VDABVERMPZ       PIC X(1).
              10 CKV-PC-VDVMNR           PIC S9(9) COMP.
              10 CKV-PC-VDVMNRPZ         PIC X(1).
              10 CKV-PC-VDVMZUNA         PIC X(26).
              10 CKV-PC-VDVMVORNA        PIC X(26).
              10 CKV-PC-PDVNZUNA         PIC X(26).
              10 CKV-PC-PDVNVORNA        PIC X(26).
              10 CKV-PC-PDVNPLZ          PIC X(6).
              10 CKV-PC-PDVNORT          PIC X(30).
              10 CKV-PC-PDVNLKZ          PIC X(3).
              10 CKV-PC-PROD1            PIC S9(4) COMP.
              10 CKV-PC-BEITRAG-PROD1    PIC S9(9)V9(2) COMP-3.
              10 CKV-PC-EH-PROD1         PIC S9(6)V9(1) COMP-3.
              10 CKV-PC-PROD2            PIC S9(4) COMP.
              10 CKV-PC-BEITRAG-PROD2    PIC S9(9)V9(2) COMP-3.
              10 CKV-PC-EH-PROD2         PIC S9(6)V9(1) COMP-3.
              10 CKV-PC-PROD3            PIC S9(4) COMP.
              10 CKV-PC-BEITRAG-PROD3    PIC S9(9)V9(2) COMP-3.
              10 CKV-PC-EH-PROD3         PIC S9(6)V9(1) COMP-3.
              10 CKV-PC-PROD4            PIC S9(4) COMP.
              10 CKV-PC-BEITRAG-PROD4    PIC S9(9)V9(2) COMP-3.
              10 CKV-PC-EH-PROD4         PIC S9(6)V9(1) COMP-3.
              10 CKV-PC-PROD5            PIC S9(4) COMP.
              10 CKV-PC-BEITRAG-PROD5    PIC S9(9)V9(2) COMP-3.
              10 CKV-PC-EH-PROD5         PIC S9(6)V9(1) COMP-3.
              10 CKV-PC-PROD6            PIC S9(4) COMP.
              10 CKV-PC-BEITRAG-PROD6    PIC S9(9)V9(2) COMP-3.
              10 CKV-PC-EH-PROD6         PIC S9(6)V9(1) COMP-3.
              10 CKV-PC-PROD7            PIC S9(4) COMP.
              10 CKV-PC-BEITRAG-PROD7    PIC S9(9)V9(2) COMP-3.
              10 CKV-PC-EH-PROD7         PIC S9(6)V9(1) COMP-3.
              10 CKV-PC-PROD8            PIC S9(4) COMP.
              10 CKV-PC-BEITRAG-PROD8    PIC S9(9)V9(2) COMP-3.
              10 CKV-PC-EH-PROD8         PIC S9(6)V9(1) COMP-3.
              10 CKV-PC-PROD9            PIC S9(4) COMP.
              10 CKV-PC-BEITRAG-PROD9    PIC S9(9)V9(2) COMP-3.
              10 CKV-PC-EH-PROD9         PIC S9(6)V9(1) COMP-3.
              10 CKV-PC-ANTRAG-DATUM     PIC X(10).
              10 CKV-PC-KD-NUMMER        PIC S9(18)V COMP-3.
              10 CKV-PC-ESIGN            PIC X(1).
              10 CKV-PC-VERSION          PIC S9(4) COMP.
              10 CKV-PC-ERSTELLUNG       PIC X(26).
              10 CKV-PC-KENNUNG-ERST     PIC X(8).
              10 CKV-PC-LAST-UPDATE      PIC X(26).
              10 CKV-PC-KENNUNG-AEND     PIC X(8).
              10 CKV-PC-BLACK-BOX.
              49 CKV-PC-BLACK-BOX-LEN
                 PIC S9(4) COMP.
              49 CKV-PC-BLACK-BOX-TEXT
                 PIC X(30000).
      *
      ****** ENDE COPY-MEMBER ****************************************
 
 
 


