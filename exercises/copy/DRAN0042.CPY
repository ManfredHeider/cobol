      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0042.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRAN0042.CPv  $
      *
      *   Rev 2.5   Oct 24 2013 10:59:40   t13ae23
      *AAN01073: Projekt SEPA - Realisierung AN Deutschland
      *AMV + 
      *ADVC (Dialog)
      *
      *   Rev 2.4   Nov 26 2009 14:33:18   t13ae34
      *AAN00896: OptimA Stufe 1 - SST OKI -> Antragsbestand
      *
      *   Rev 2.3   May 28 2008 08:49:52   t13ae34
      *AAN00801: POA-Antrag
      *
      *   Rev 2.2   Nov 26 2004 15:29:22   t13ae23
      *AAN00529: POA AMVers. Optimierung Quote
      *
      *   Rev 2.1   Apr 23 2002 16:59:50   t13ae34
      *AAN00357-POA-AMV
      *
      *   Rev 2.0   Feb 27 1999 11:05:38   P13SP20
      *NT-Umstellung, Einführung MFE
      *
      *   Rev 1.0   30 Jul 1997 17:37:46   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0042  FUNKTION UPDATE STANDARD-OZP       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00357!18.03.02!2.1 ! NEUERSTELLUNG             ! HEIDER   *
      * AAN00529!26.11.04!2.2 ! OPTIMIERUNG POA-QUOTE     ! RAHNER   *
      * AAN00801!26.05.08!2.3 ! KENNZEICHEN WIDERSPRUCH   ! HEIDER   *
      * AAN00896!16.11.09!2.4 ! PIN PRÜFZIFFER            ! HEIDER   *
      * AAN01073!24.10.13!2.5 ! SEPA                      ! RAHNER   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0042                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE  <<<       TABELLE POAA TBAN0003              *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0042.
           05 PI-DRAN0042-POAA-KEY.
               10 PI-DRAN0042-PGEB-GES         PIC S9(4) COMP.
               10 PI-DRAN0042-POA-HVANTNUM     PIC X(09).
               10 PI-DRAN0042-POA-HVANTNUM-LFD PIC X(01).
               10 PI-DRAN0042-POA-VDABVERM     PIC  S9(9) COMP.
           05 PI-DRAN0042-POAA-DATEN.
               10 PI-DRAN0042-POA-PIN            PIC S9(9) COMP.
161109         10 PI-DRAN0042-POA-PIN-PZ         PIC  X(2).
               10 PI-DRAN0042-POA-HAND           PIC  X(1).
261104         10 PI-DRAN0042-POA-VBEGINN-E      PIC X(10).
261104         10 PI-DRAN0042-POA-VABLAUF-E      PIC X(10).
261104         10 PI-DRAN0042-POA-KNR-ZAHLW-E    PIC S9(4) COMP.
261104         10 PI-DRAN0042-POA-KTY-ZAHLW-E    PIC S9(4) COMP.
261104         10 PI-DRAN0042-POA-PDVNBLZ-E      PIC S9(8) COMP.
261104         10 PI-DRAN0042-POA-PDVNKONTONR-E  PIC S9(10) COMP.
261104         10 PI-DRAN0042-POA-PDVNBEGINKM-E  PIC S9(6) COMP.
261104         10 PI-DRAN0042-POA-PDVNFGESTNR-E  PIC X(17).
261104         10 PI-DRAN0042-POA-PDVNAMTLKZ-E   PIC X(11).
261104         10 PI-DRAN0042-POA-PDVNJAHRKM-E   PIC S9(6) COMP.
260508         10 PI-DRAN0042-POA-WIDERSPR-E     PIC X(1).
241013         10 PI-DRAN0042-POA-PDVN-IBAN-E    PIC X(34).
241013         10 PI-DRAN0042-POA-PDVN-BIC-E     PIC X(11).
      *
       01  PO-DRAN0042.
           05 PO-DRAN0042-RC                   PIC  X(003).
             88 PO-DRAN0042-RC-OK                   VALUE 'OK '.
             88 PO-DRAN0042-RC-CHANGED              VALUE 'CHA'.
             88 PO-DRAN0042-RC-NOT-FOUND            VALUE 'NFD'.
      *
      ****** ENDE COPY-MEMBER ****************************************
