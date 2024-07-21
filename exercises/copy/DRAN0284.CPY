      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.06.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0284.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRAN0284.CPv  $
      *
      *   Rev 2.3   Feb 25 2016 10:11:58   t13ae23
      *AAN01240: Neue Feld "Fimenname (für DV-Antrag)" + 
      *Elektra 
      *- AGI, CKV - Deutsche Bank -
      *
      *   Rev 2.2   Feb 12 2016 07:57:04   t13ae34
      *AAN01211: Elektra - AGI, CKV, Deutsche Bank
      *
      *   Rev 2.1   Aug 26 2015 12:05:14   t13ae34
      *AAN01211: Elektra - AGI, CKV, Deutsche Bank
      *
      *   Rev 2.0   Jun 08 2015 11:16:22   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    PC-DATEN BEI ELEKTRONISCHEN VORSCHLAGSDATEN AGI (40)
      ****************************************************************
      *    COPY-MEMBER: DRAN0284                                     *
      *    SCHNITTSTELLE                                             *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN01211!09.06.15!2.1 ! NEU                       ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
           05  AGI-PC-DATEN.
 
                10 AGI-PC-HVANTNUM         PIC X(9).
                10 AGI-PC-PIN              PIC S9(9)  COMP.
                10 AGI-PC-PGEB-GES         PIC S9(4)  COMP.
                10 AGI-PC-VB-GES           PIC S9(4)  COMP.
                10 AGI-PC-VDABVERM         PIC S9(9)  COMP.
                10 AGI-PC-VDABVERMPZ       PIC X(1).
                10 AGI-PC-AFO-34C-ZEICHN   PIC S9(9)  COMP.
                10 AGI-PC-VDVMNR           PIC S9(9)  COMP.
                10 AGI-PC-VDVMNRPZ         PIC X(1).
                10 AGI-PC-VDVMZUNA         PIC X(26).
                10 AGI-PC-VDVMVORNA        PIC X(26).
                10 AGI-PC-VDDIREKT         PIC S9(4)  COMP.
                10 AGI-PC-PDVNZUNA         PIC X(26).
                10 AGI-PC-PDVNVORNA        PIC X(26).
                10 AGI-PC-PDVNPLZ          PIC X(6).
                10 AGI-PC-PDVNORT          PIC X(30).
                10 AGI-PC-PDVNLKZ          PIC X(3).
                10 AGI-PC-PROD1            PIC S9(4)  COMP.
                10 AGI-PC-BETRAG-PROD1     PIC S9(9)V9(2)  COMP-3.
                10 AGI-PC-ZAHLWEISE1       PIC S9(4)  COMP.
                10 AGI-PC-LAUFZEIT1        PIC S9(4)  COMP.
                10 AGI-PC-EINH-PROD1       PIC S9(6)V9(1)  COMP-3.
                10 AGI-PC-PROD2            PIC S9(4)  COMP.
                10 AGI-PC-BETRAG-PROD2     PIC S9(9)V9(2)  COMP-3.
                10 AGI-PC-ZAHLWEISE2       PIC S9(4)  COMP.
                10 AGI-PC-LAUFZEIT2        PIC S9(4)  COMP.
                10 AGI-PC-EINH-PROD2       PIC S9(6)V9(1)  COMP-3.
                10 AGI-PC-PROD3            PIC S9(4)  COMP.
                10 AGI-PC-BETRAG-PROD3     PIC S9(9)V9(2)  COMP-3.
                10 AGI-PC-ZAHLWEISE3       PIC S9(4)  COMP.
                10 AGI-PC-LAUFZEIT3        PIC S9(4)  COMP.
                10 AGI-PC-EINH-PROD3       PIC S9(6)V9(1)  COMP-3.
                10 AGI-PC-PROD4            PIC S9(4)  COMP.
                10 AGI-PC-BETRAG-PROD4     PIC S9(9)V9(2)  COMP-3.
                10 AGI-PC-ZAHLWEISE4       PIC S9(4)  COMP.
                10 AGI-PC-LAUFZEIT4        PIC S9(4)  COMP.
                10 AGI-PC-EINH-PROD4       PIC S9(6)V9(1)  COMP-3.
                10 AGI-PC-PROD5            PIC S9(4)  COMP.
                10 AGI-PC-BETRAG-PROD5     PIC S9(9)V9(2)  COMP-3.
                10 AGI-PC-ZAHLWEISE5       PIC S9(4)  COMP.
                10 AGI-PC-LAUFZEIT5        PIC S9(4)  COMP.
                10 AGI-PC-EINH-PROD5       PIC S9(6)V9(1)  COMP-3.
                10 AGI-PC-PROD6            PIC S9(4)  COMP.
                10 AGI-PC-BETRAG-PROD6     PIC S9(9)V9(2)  COMP-3.
                10 AGI-PC-ZAHLWEISE6       PIC S9(4)  COMP.
                10 AGI-PC-LAUFZEIT6        PIC S9(4)  COMP.
                10 AGI-PC-EINH-PROD6       PIC S9(6)V9(1)  COMP-3.
                10 AGI-PC-PROD7            PIC S9(4)  COMP.
                10 AGI-PC-BETRAG-PROD7     PIC S9(9)V9(2)  COMP-3.
                10 AGI-PC-ZAHLWEISE7       PIC S9(4)  COMP.
                10 AGI-PC-LAUFZEIT7        PIC S9(4)  COMP.
                10 AGI-PC-EINH-PROD7       PIC S9(6)V9(1)  COMP-3.
                10 AGI-PC-PROD8            PIC S9(4)  COMP.
                10 AGI-PC-BETRAG-PROD8     PIC S9(9)V9(2)  COMP-3.
                10 AGI-PC-ZAHLWEISE8       PIC S9(4)  COMP.
                10 AGI-PC-LAUFZEIT8        PIC S9(4)  COMP.
                10 AGI-PC-EINH-PROD8       PIC S9(6)V9(1)  COMP-3.
                10 AGI-PC-PROD9            PIC S9(4)  COMP.
                10 AGI-PC-BETRAG-PROD9     PIC S9(9)V9(2)  COMP-3.
                10 AGI-PC-ZAHLWEISE9       PIC S9(4)  COMP.
                10 AGI-PC-LAUFZEIT9        PIC S9(4)  COMP.
                10 AGI-PC-EINH-PROD9       PIC S9(6)V9(1)  COMP-3.
                10 AGI-PC-VDABLEIN         PIC S9(6)V9(1)  COMP-3.
                10 AGI-PC-DYNAMIK          PIC S9(3)V9(3)  COMP-3.
                10 AGI-PC-HAND             PIC X(1).
                10 AGI-PC-ANTRAG-DATUM     PIC X(10).
                10 AGI-PC-KD-BEST-ID       PIC S9(18)V  COMP-3.
                10 AGI-PC-ESIGN            PIC X(1).
                10 AGI-PC-VERSION          PIC S9(4)  COMP.
                10 AGI-PC-ERSTELLUNG       PIC X(26).
                10 AGI-PC-KENNUNG-ERST     PIC X(8).
                10 AGI-PC-LAST-UPDATE      PIC X(26).
                10 AGI-PC-KENNUNG-AEND     PIC X(8).
                10 AGI-PC-XGDV.
                   49 AGI-PC-XGDV-LEN      PIC S9(4)  COMP.
                   49 AGI-PC-XGDV-TEXT     PIC X(20000).
 
      *
      ****** ENDE COPY-MEMBER ****************************************
 


