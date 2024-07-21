      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0063.CPY  $
      *    $Revision:   2.8  $
      *
      *    $Log:   L:/mf/CPY/DRAN0063.CPv  $
      *
      *   Rev 2.8   Nov 19 2015 08:04:32   t13ae34
      *AAN01211: Badenia - Erweiterung Blackbox
      *
      *   Rev 2.7   Nov 04 2015 13:31:54   t13ae34
      *AAN01211: Badenia - Erweiterung Blackbox
      *
      *   Rev 2.6   May 28 2015 09:52:06   t13ae34
      *AAN01211: Elektronischer Antrag - Änderungen eingereichtes G
      *eschäft - Einpflegen in Vorschlagsdaten
      *
      *   Rev 2.5   Apr 23 2015 13:24:06   t13ae34
      *AAN01211: Elektronischer Antrag - Änderungen eingereichtes G
      *eschäft - Einpflegen in Vorschlagsdaten
      *
      *   Rev 2.4   27 Dec 2007 11:39:38   t13ae23
      *AAN00730: VRA - Kennzeichen Vertriebs-Ges in elektr. Daten G
      *es. 70 BADENIA
      *
      *   Rev 2.3   01 Sep 2006 09:45:30   T13AE23
      *AAN00684: elektronische Anträge aus Online-KI
      *
      *   Rev 2.2   31 Aug 2006 09:43:02   T13AE23
      *AAN00684: elektronische Anträge aus Online-KI
      *
      *   Rev 2.1   Mar 06 2003 16:54:18   t13ae23
      *AAN00409 -  VIA BADENIA Elektron. Antrag -
      *
      *   Rev 2.0   Mar 18 2002 09:14:52   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    TEILMASCHINELLE DATEN MIT DFÜ
      *    ZUM AMV-HOST
      ****************************************************************
      *    COPY-MEMBER: DRAN0063                                     *
      *    SCHNITTSTELLE                                             *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00409!07.03.03!2.1 ! NEU                       ! RAHNER   *
      * AAN00684!31.08.06!2.2 ! KUNDENBESTANDS-ID FÜR AVS ! RAHNER   *
      * AAN00730!28.12.07!2.3 ! ALLFINANZ                 ! RAHNER   *
      * AAN01211!06.03.15!2.5 ! ESIGN                     ! HEIDER   *
      * AAN01211!21.10.15!2.7 ! XGDV AUF 30000            ! HEIDER   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
           05  VIA-PC-DATEN.
 
      *        GLEICHHEITSPRÜFUNG
                10 VIA-PC-PGEB-GES          PIC S9(4) COMP.
                10 VIA-PC-PIN               PIC S9(9) COMP.
                10 VIA-PC-PIN-2             PIC S9(9) COMP.
                10 VIA-PC-HVANTNUM          PIC S9(17) COMP-3.
                10 VIA-PC-VDABVERM          PIC S9(9) COMP.
                10 VIA-PC-VDABVERMPZ        PIC X.
                10 VIA-PC-VDVMNR            PIC S9(9) COMP.
                10 VIA-PC-VDVMNRPZ          PIC X.
                10 VIA-PC-VDVMZUNA          PIC X(26).
                10 VIA-PC-VDVMVORNA         PIC X(26).
                10 VIA-PC-VDDIREKT          PIC S9(4) COMP.
                10 VIA-PC-PDVNZUNA          PIC X(26).
                10 VIA-PC-PDVNVORNA         PIC X(26).
                10 VIA-PC-PDVNPLZ           PIC X(6).
                10 VIA-PC-PDVNORT           PIC X(30).
                10 VIA-PC-PDVNLKZ           PIC X(3).
                10 VIA-PC-VDPRODKEY         PIC S9(4) COMP.
                10 VIA-PC-DVAG-PRODKEY      PIC S9(4) COMP.
                10 VIA-PC-VDABLEIN          PIC S9(6)V9(1) COMP-3.
                10 VIA-PC-SUMME             PIC S9(9)V9(2) COMP-3.
                10 VIA-PC-ANTRAGS-DATUM     PIC X(10).
                10 VIA-PC-VERSION           PIC S9(4) COMP.
                10 VIA-PC-ERSTELLUNG        PIC X(26).
                10 VIA-PC-KENNUNG-ERST      PIC X(8).
                10 VIA-PC-KENNUNG-AEND      PIC X(8).
                10 VIA-PC-LAST-UPDATE       PIC X(26).
                10 VIA-PC-KENNZ-BSV         PIC X.
                10 VIA-PC-KENNZ-TARIF       PIC X.
                10 VIA-PC-KENNZ-FRST        PIC X.
                10 VIA-PC-KENNZ-LEG         PIC X.
                10 VIA-PC-KENNZ-VL          PIC X.
                10 VIA-PC-KENNZ-VL2         PIC X.
                10 VIA-PC-KENNZ-ERHOEH      PIC X.
                10 VIA-PC-HAND-BSV          PIC X.
                10 VIA-PC-HAND-TARIF        PIC X.
                10 VIA-PC-HAND-FRST         PIC X.
                10 VIA-PC-HAND-LEG          PIC X.
                10 VIA-PC-HAND-VL           PIC X.
                10 VIA-PC-HAND-VL2          PIC X.
                10 VIA-PC-XGDV.
                   15 VIA-PC-XGDV-LEN       PIC S9(4) COMP.
                   15 VIA-PC-XGDV-TEXT      PIC X(30000).
                10 VIA-PC-KD-BEST-ID        PIC S9(18)V COMP-3.
                10 VIA-PC-VB-GES            PIC S9(4) COMP.
                10 VIA-PC-ESIGN             PIC X.
      *
      ****** ENDE COPY-MEMBER ****************************************
 
 

