      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0090.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRAN0090.CPv  $
      *
      *   Rev 2.7   Jul 04 2016 10:05:40   t13ae34
      *AAN01269: Anträge Fondsdepotbank - Berücksichtigung Fremdfonds
      *
      *   Rev 2.6   May 28 2015 09:52:28   t13ae34
      *AAN01211: Elektronischer Antrag - Änderungen eingereichtes G
      *eschäft - Einpflegen in Vorschlagsdaten
      *
      *   Rev 2.5   Apr 23 2015 13:24:30   t13ae34
      *AAN01211: Elektronischer Antrag - Änderungen eingereichtes G
      *eschäft - Einpflegen in Vorschlagsdaten
      *
      *   Rev 2.4   23 Nov 2007 10:37:24   t13ae23
      *AAN00730: VRA - Kennzeichen Vertriebs-Ges in elektr. Daten G
      *es. 48 DWS
      *
      *   Rev 2.3   01 Sep 2006 09:48:18   T13AE23
      *AAN00684: elektronische Anträge aus Online-KI
      *
      *   Rev 2.2   31 Aug 2006 09:43:44   T13AE23
      *AAN00684: elektronische Anträge aus Online-KI
      *
      *   Rev 2.1   Aug 06 2004 08:35:48   t13ae23
      *AAN00455: Elektronischer Antrag DWS - Programme -
      *
      *   Rev 2.0   Mar 18 2002 09:16:34   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0090  FUNKTION INSERT STANDARD-OZP       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00455!06.08.04!2.1 ! NEUERSTELLUNG             ! RAHNER   *
      * AAN00684!31.08.06!2.2 ! KUNDENBESTANDSID FÜR AVS  ! RAHNER   *
      * AAN00730!30.11.07!2.2  !ALLFINANZ                 ! RAHNER   *
      * AAN01211!09.03.15!2.5 ! ESIGN, ZW, LAUFZEIT,XGDV  ! HEIDER   *
      * AAN01269!01.07.16!2.7 ! PRODUKTE ALS TABELLE      ! HEIDER   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0090                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT  <<<       TABELLE DWS          TBAN0006      *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0090.
           05 PI-DRAN0090-DWS-KEY.
               10 PI-DRAN0090-PGEB-GES           PIC S9(4) COMP.
               10 PI-DRAN0090-DWS-HVANTNUM       PIC X(9).
           05 PI-DRAN0090-DWS-DATEN.
               10 PI-DRAN0090-DWS-PIN            PIC S9(9) COMP.
               10 PI-DRAN0090-DWS-VDABVERM       PIC S9(9) COMP.
               10 PI-DRAN0090-DWS-VDABVERMPZ     PIC X.
               10 PI-DRAN0090-DWS-VDVMNR         PIC S9(9) COMP.
               10 PI-DRAN0090-DWS-VDVMNRPZ       PIC X.
               10 PI-DRAN0090-DWS-VDVMZUNA       PIC X(26).
               10 PI-DRAN0090-DWS-VDVMVORNA      PIC X(26).
               10 PI-DRAN0090-DWS-VDDIREKT       PIC S9(4) COMP.
               10 PI-DRAN0090-DWS-PDVNZUNA       PIC X(26).
               10 PI-DRAN0090-DWS-PDVNVORNA      PIC X(26).
               10 PI-DRAN0090-DWS-PDVNPLZ        PIC X(06).
               10 PI-DRAN0090-DWS-PDVNORT        PIC X(30).
               10 PI-DRAN0090-DWS-PDVNLKZ        PIC X(3).
      *>>>>>
               10 PI-DRAN0090-PRODUKTE OCCURS 9.
                  15 PI-DRAN0090-DWS-PROD       PIC S9(4) COMP.
                  15 PI-DRAN0090-DWS-SUMME-PROD PIC S9(9) COMP-3.
                  15 PI-DRAN0090-DWS-EINH-PROD  PIC S9(6)V9(1) COMP-3.
                  15 PI-DRAN0090-DWS-ZAHLWEISE  PIC S9(4) COMP.
                  15 PI-DRAN0090-DWS-LAUFZEIT   PIC S9(4) COMP.
      *<<<<<
               10 PI-DRAN0090-DWS-VDABLEIN       PIC S9(6)V9(1) COMP-3.
               10 PI-DRAN0090-DWS-AFO-34C-ZEICHN PIC S9(9) COMP.
               10 PI-DRAN0090-DWS-ANTRAGS-DATUM  PIC X(10).
               10 PI-DRAN0090-DWS-HAND           PIC X.
               10 PI-DRAN0090-DWS-VERSION        PIC S9(4) COMP.
               10 PI-DRAN0090-DWS-KD-BEST-ID     PIC S9(18)V COMP-3.
               10 PI-DRAN0090-DWS-VB-GES         PIC S9(4) COMP.
               10 PI-DRAN0090-DWS-ESIGN          PIC X.
 
               10 PI-DRAN0090-DWS-XGDV.
                  15 PI-DRAN0090-DWS-XGDV-LEN   PIC S9(4)      COMP.
                  15 PI-DRAN0090-DWS-XGDV-TEXT  PIC X(20000).
      *
       01  PO-DRAN0090.
           05 PO-DRAN0090-RC                      PIC  X(003).
             88 PO-DRAN0090-RC-OK                   VALUE 'OK '.
             88 PO-DRAN0090-RC-DUPLICATE-VALUE      VALUE 'DVA'.
      *
      ****** ENDE COPY-MEMBER ****************************************
 


