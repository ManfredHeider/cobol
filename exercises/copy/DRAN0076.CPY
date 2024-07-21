      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0076.CPY  $
      *    $Revision:   2.14  $
      *
      *    $Log:   L:/mf/CPY/DRAN0076.CPv  $
      *
      *   Rev 2.14   Dec 18 2015 08:57:04   t13ae23
      *AAN01223: Änderung Beginn/Ablauf AdvoCard
      *
      *   Rev 2.13   Oct 21 2015 14:38:58   t13ae23
      *AAN01223: Änderung Beginn/Ablauf AdvoCard
      *
      *   Rev 2.12   May 28 2015 09:52:14   t13ae34
      *AAN01211: Elektronischer Antrag - Änderungen eingereichtes G
      *eschäft - Einpflegen in Vorschlagsdaten
      *
      *   Rev 2.11   Apr 23 2015 13:24:14   t13ae34
      *AAN01211: Elektronischer Antrag - Änderungen eingereichtes G
      *eschäft - Einpflegen in Vorschlagsdaten
      *
      *   Rev 2.10   Oct 24 2013 11:00:44   t13ae23
      *AAN01073: Projekt SEPA - Realisierung AN Deutschland
      *AMV +
      *ADVC (Dialog)
      *
      *   Rev 2.9   07 Apr 2008 14:03:48   T13EX32
      *AKI00556: PGMe - Elektronische Anträge
      *
      *   Rev 2.8   23 Nov 2007 11:01:10   t13ae23
      *AAN00730: VRA - Kennzeichen Vertriebs-Ges in elektr. Daten G
      *es. 81 Advocard
      *
      *   Rev 2.7   12 Jul 2007 16:05:46   t13ex43
      *AKI00556: PGMe - Elektronische Anträge
      *
      *   Rev 2.6   01 Sep 2006 09:46:48   T13AE23
      *AAN00684: elektronische Anträge aus Online-KI
      *
      *   Rev 2.5   31 Aug 2006 09:43:18   T13AE23
      *AAN00684: elektronische Anträge aus Online-KI
      *
      *   Rev 2.4   Dec 30 2003 08:52:16   t13ae23
      *AAN00422 - Aktivieren Änderungen 31.10.2003 -
      *
      *   Rev 2.3   Dec 12 2003 12:05:12   t13ae34
      *AAN00422 - Vergrößern Blackbox + Feld Version
      *
      *   Rev 2.2   Oct 31 2003 10:50:42   t13ae23
      *AAN00422 - Vergrößern BlackBox Advocard -
      *
      *   Rev 2.1   Jul 25 2003 09:05:30   t13ae23
      *AAN00422 - Elektron. Antrag ADVOCARD -
      *
      *   Rev 2.0   Mar 18 2002 09:15:40   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0076  FUNKTION INSERT STANDARD-OZP       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00422!25.07.03!2.1 ! NEUERSTELLUNG             ! RAHNER   *
      * AAN00422!31.10.03!2.2 ! VERGRÖßERN BLACKBOX       ! RAHNER   *
      * AAN00422!30.12.03!2.3 !AKTIVIEREN ÄNDERUNGEN VOM  !          *
      *         !        !    !31.10.2003                 ! RAHNER   *
      * AAN00684!31.08.06!2.4 ! KUNDENBESTANDS-ID FÜR AVS ! RAHNER   *
      * AKI00556!26.02.07!2.7 ! NEUE FELDER HINZU         ! KLEEMEIER*
      * AAN00730!30.11.07!2.8 ! ALLFINANZ                 ! RAHNER   *
      * AAN01073!24.10.13!2.9 ! SEPA                      ! RAHNER   *
      * AAN01211!09.03.15!2.11! ESIGN                     ! HEIDER   *
      * AAN01223!08.12.15!2.14! BEGINN/ABLAUF ÄNDERBAR    ! RAHNER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0076                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT  <<<       TABELLE ADVC-BADENIA TBAN0005      *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0076.
           05 PI-DRAN0076-ADVC-KEY.
               10 PI-DRAN0076-PGEB-GES            PIC S9(4) COMP.
               10 PI-DRAN0076-ADVC-HVANTNUM       PIC X(9).
           05 PI-DRAN0076-ADVC-DATEN.
               10 PI-DRAN0076-ADVC-PIN            PIC S9(9) COMP.
               10 PI-DRAN0076-ADVC-VDABVERM       PIC S9(9) COMP.
               10 PI-DRAN0076-ADVC-VDABVERMPZ     PIC X.
               10 PI-DRAN0076-ADVC-VDVMNR         PIC S9(9) COMP.
               10 PI-DRAN0076-ADVC-VDVMNRPZ       PIC X.
               10 PI-DRAN0076-ADVC-VDVMZUNA       PIC X(26).
               10 PI-DRAN0076-ADVC-VDVMVORNA      PIC X(26).
               10 PI-DRAN0076-ADVC-VDDIREKT       PIC S9(4) COMP.
               10 PI-DRAN0076-ADVC-PDVNZUNA       PIC X(26).
               10 PI-DRAN0076-ADVC-PDVNVORNA      PIC X(26).
               10 PI-DRAN0076-ADVC-PDVNPLZ        PIC X(06).
               10 PI-DRAN0076-ADVC-PDVNORT        PIC X(30).
               10 PI-DRAN0076-ADVC-PDVNLKZ        PIC X(3).
               10 PI-DRAN0076-ADVC-VDPRODKEY      PIC S9(4) COMP.
               10 PI-DRAN0076-ADVC-VBEGINN        PIC X(10).
               10 PI-DRAN0076-ADVC-VABLAUF        PIC X(10).
               10 PI-DRAN0076-ADVC-VDABLEIN       PIC S9(6)V9(1) COMP-3.
               10 PI-DRAN0076-ADVC-BEITRAG-BR     PIC S9(9)V9(2) COMP-3.
               10 PI-DRAN0076-ADVC-BEITRAG-NE     PIC S9(9)V9(2) COMP-3.
               10 PI-DRAN0076-ADVC-KNR-ZAHLW      PIC S9(4) COMP.
               10 PI-DRAN0076-ADVC-KTY-ZAHLW      PIC S9(4) COMP.
               10 PI-DRAN0076-ADVC-ANTRAGS-DATUM  PIC X(10).
               10 PI-DRAN0076-ADVC-HAND           PIC X.
      *        10 PI-DRAN0076-ADVC-XGDV           PIC X(9999).
               10 PI-DRAN0076-ADVC-XGDV           PIC X(32460).
               10 PI-DRAN0076-ADVC-KD-BEST-ID     PIC S9(18)V COMP-3.
               10 PI-DRAN0076-ADVC-VERSION        PIC S9(4)   COMP.
               10 PI-DRAN0076-ADVC-KONTONR        PIC S9(12)V COMP-3.
               10 PI-DRAN0076-ADVC-KONTONR-E      PIC S9(12)V COMP-3.
               10 PI-DRAN0076-ADVC-BLZ            PIC S9(9)   COMP.
               10 PI-DRAN0076-ADVC-BLZ-E          PIC S9(9)   COMP.
               10 PI-DRAN0076-ADVC-AMTLKZ         PIC X(12).
               10 PI-DRAN0076-ADVC-AMTLKZ-E       PIC X(12).
               10 PI-DRAN0076-ADVC-KNR-ZAHLW-E    PIC S9(4) COMP.
               10 PI-DRAN0076-ADVC-KTY-ZAHLW-E    PIC S9(4) COMP.
      *        10 PI-DRAN0076-ADVC-V-PLUS         PIC X.
      *        10 PI-DRAN0076-ADVC-V-PLUS-E       PIC X.
               10 PI-DRAN0076-ADVC-VB-GES         PIC S9(4) COMP.
               10 PI-DRAN0076-ADVC-IBAN           PIC X(34).
               10 PI-DRAN0076-ADVC-IBAN-E         PIC X(34).
               10 PI-DRAN0076-ADVC-BIC            PIC X(11).
               10 PI-DRAN0076-ADVC-BIC-E          PIC X(11).
               10 PI-DRAN0076-ADVC-ESIGN          PIC X(1).
               10 PI-DRAN0076-ADVC-VBEGINN-E      PIC X(10).
               10 PI-DRAN0076-ADVC-VABLAUF-E      PIC X(10).
      *
       01  PO-DRAN0076.
           05 PO-DRAN0076-RC                      PIC  X(003).
             88 PO-DRAN0076-RC-OK                   VALUE 'OK '.
             88 PO-DRAN0076-RC-DUPLICATE-VALUE      VALUE 'DVA'.
      *
      ****** ENDE COPY-MEMBER ****************************************
 



