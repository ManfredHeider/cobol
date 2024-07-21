      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0060.CPY  $
      *    $Revision:   2.8  $
      *
      *    $Log:   L:/mf/CPY/DRAN0060.CPv  $
      *
      *   Rev 2.8   Nov 19 2015 08:04:26   t13ae34
      *AAN01211: Badenia - Erweiterung Blackbox
      *
      *   Rev 2.7   Nov 04 2015 13:31:46   t13ae34
      *AAN01211: Badenia - Erweiterung Blackbox
      *
      *   Rev 2.6   May 28 2015 09:51:58   t13ae34
      *AAN01211: Elektronischer Antrag - Änderungen eingereichtes G
      *eschäft - Einpflegen in Vorschlagsdaten
      *
      *   Rev 2.5   Apr 23 2015 13:23:58   t13ae34
      *AAN01211: Elektronischer Antrag - Änderungen eingereichtes G
      *eschäft - Einpflegen in Vorschlagsdaten
      *
      *   Rev 2.4   27 Dec 2007 11:38:52   t13ae23
      *AAN00730: VRA - Kennzeichen Vertriebs-Ges in elektr. Daten G
      *es. 70 BADENIA
      *
      *   Rev 2.3   01 Sep 2006 09:45:08   T13AE23
      *AAN00684: elektronische Anträge aus Online-KI
      *
      *   Rev 2.2   31 Aug 2006 09:42:44   T13AE23
      *AAN00684: elektronische Anträge aus Online-KI
      *
      *   Rev 2.1   Mar 06 2003 16:54:14   t13ae23
      *AAN00409 -  VIA BADENIA Elektron. Antrag -
      *
      *   Rev 2.0   Mar 18 2002 09:14:40   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0060  FUNKTION INSERT STANDARD-OZP       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00409!07.03.03!2.1 ! NEUERSTELLUNG             ! RAHNER   *
      * AAN00684!31.08.06!2.2 ! NEU KUNDENBESTANDSID FÜR  !          *
      *         !        !    ! AVS                       ! RAHNER   *
      * AAN00730!28.12.07!2.3 ! ALLFINANZ                 ! RAHNER   *
      * AAN01211!06.03.15!2.5 ! ESIGN                     ! HEIDER   *
      * AAN01211!21.10.15!2.7 ! XGDV AUF 30000            ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0060                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT  <<<       TABELLE VIA-BADENIA TBAN0004       *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0060.
           05 PI-DRAN0060-VIA-KEY.
               10 PI-DRAN0060-PGEB-GES            PIC S9(4) COMP.
               10 PI-DRAN0060-VIA-HVANTNUM        PIC S9(17) COMP-3.
           05 PI-DRAN0060-VIA-DATEN.
               10 PI-DRAN0060-VIA-PIN             PIC S9(9) COMP.
               10 PI-DRAN0060-VIA-PIN-2           PIC S9(9) COMP.
               10 PI-DRAN0060-VIA-VDABVERM        PIC S9(9) COMP.
               10 PI-DRAN0060-VIA-VDABVERMPZ      PIC X.
               10 PI-DRAN0060-VIA-VDVMNR          PIC S9(9) COMP.
               10 PI-DRAN0060-VIA-VDVMNRPZ        PIC X.
               10 PI-DRAN0060-VIA-VDVMZUNA        PIC X(26).
               10 PI-DRAN0060-VIA-VDVMVORNA       PIC X(26).
               10 PI-DRAN0060-VIA-VDDIREKT        PIC S9(4) COMP.
               10 PI-DRAN0060-VIA-PDVNZUNA        PIC X(26).
               10 PI-DRAN0060-VIA-PDVNVORNA       PIC X(26).
               10 PI-DRAN0060-VIA-PDVNPLZ         PIC X(06).
               10 PI-DRAN0060-VIA-PDVNORT         PIC X(30).
               10 PI-DRAN0060-VIA-PDVNLKZ         PIC X(3).
               10 PI-DRAN0060-VIA-VDPRODKEY       PIC S9(4) COMP.
               10 PI-DRAN0060-VIA-DVAG-PRODKEY    PIC S9(4) COMP.
               10 PI-DRAN0060-VIA-VDABLEIN        PIC S9(6)V9(1) COMP-3.
               10 PI-DRAN0060-VIA-SUMME           PIC S9(9)V9(2) COMP-3.
               10 PI-DRAN0060-VIA-ANTRAGS-DATUM   PIC X(10).
               10 PI-DRAN0060-VIA-VERSION         PIC S9(4) COMP.
               10 PI-DRAN0060-VIA-KENNZ-BSV       PIC X.
               10 PI-DRAN0060-VIA-KENNZ-TARIF     PIC X.
               10 PI-DRAN0060-VIA-KENNZ-FRST      PIC X.
               10 PI-DRAN0060-VIA-KENNZ-LEG       PIC X.
               10 PI-DRAN0060-VIA-KENNZ-VL        PIC X.
               10 PI-DRAN0060-VIA-KENNZ-VL2       PIC X.
               10 PI-DRAN0060-VIA-KENNZ-ERHOEH    PIC X.
               10 PI-DRAN0060-VIA-HAND-BSV        PIC X.
               10 PI-DRAN0060-VIA-HAND-TARIF      PIC X.
               10 PI-DRAN0060-VIA-HAND-FRST       PIC X.
               10 PI-DRAN0060-VIA-HAND-LEG        PIC X.
               10 PI-DRAN0060-VIA-HAND-VL         PIC X.
               10 PI-DRAN0060-VIA-HAND-VL2        PIC X.
               10 PI-DRAN0060-VIA-XGDV            PIC X(30000).
               10 PI-DRAN0060-VIA-KD-BEST-ID      PIC S9(18)V COMP-3.
               10 PI-DRAN0060-VIA-VB-GES          PIC S9(4) COMP.
               10 PI-DRAN0060-VIA-ESIGN           PIC X(1).
      *
       01  PO-DRAN0060.
           05 PO-DRAN0060-RC                   PIC  X(003).
             88 PO-DRAN0060-RC-OK                         VALUE 'OK '.
             88 PO-DRAN0060-RC-DUPLICATE-VALUE            VALUE 'DVA'.
      *
      ****** ENDE COPY-MEMBER ****************************************
 
 

