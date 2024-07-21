      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0055.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRAN0055.CPv  $
      *
      *   Rev 2.5   Oct 24 2013 11:00:18   t13ae23
      *AAN01073: Projekt SEPA - Realisierung AN Deutschland
      *AMV + 
      *ADVC (Dialog)
      *
      *   Rev 2.4   Nov 26 2009 14:33:32   t13ae34
      *AAN00896: OptimA Stufe 1 - SST OKI -> Antragsbestand
      *
      *   Rev 2.3   May 28 2008 08:50:06   t13ae34
      *AAN00801: POA-Antrag
      *
      *   Rev 2.2   Nov 26 2004 15:29:30   t13ae23
      *AAN00529: POA AMVers. Optimierung Quote
      *
      *   Rev 2.1   Apr 23 2002 17:00:02   t13ae34
      *AAN00357-POA-AMV
      *
      *   Rev 2.0   Mar 18 2002 09:14:20   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0055                                     *
      *    FUNKTION   : UPDATE PIN IN POA-BESTAND                    *
      *                                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00357!19.03.02!2.1 !NEU                        !HEIDER    *
      * AAN00529!26.11.04!2.2 !OPTIMIERUNG POA-QUOTE      !RAHNER    *
      * AAN00801!27.05.08!2.3 !KENNZ. WIDERSPRUCH AMBGSM  !HEIDER    *
      * AAN00896!16.11.09!2.4 !PIN PRÜFZIFFER             !HEIDER    *
      * AAN01073!24.10.13!2.5 !SEPA                       !RAHNER    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0055                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0055.
           05 PI-DRAN0055-POAA-KEY.
               10 PI-DRAN0055-PGEB-GES         PIC  X(2).
               10 PI-DRAN0055-POA-HVANTNUM     PIC  X(9).
               10 PI-DRAN0055-POA-HVANTNUM-LFD PIC  X(1).
               10 PI-DRAN0055-POA-VDABVERM     PIC  S9(9) COMP.
          05 PI-DRAN0055-DATEN.
               10 PI-DRAN0055-POA-PIN            PIC S9(9) COMP.
               10 PI-DRAN0055-POA-PIN-PZ         PIC X(2).
               10 PI-DRAN0055-POA-HAND           PIC X(1).
               10 PI-DRAN0055-POA-VBEGINN-E      PIC X(10).
               10 PI-DRAN0055-POA-VABLAUF-E      PIC X(10).
               10 PI-DRAN0055-POA-KNR-ZAHLW-E    PIC S9(4) COMP.
               10 PI-DRAN0055-POA-KTY-ZAHLW-E    PIC S9(4) COMP.
               10 PI-DRAN0055-POA-PDVNBLZ-E      PIC S9(8) COMP.
               10 PI-DRAN0055-POA-PDVNKONTONR-E  PIC S9(10) COMP.
               10 PI-DRAN0055-POA-PDVNBEGINKM-E  PIC S9(6) COMP.
               10 PI-DRAN0055-POA-PDVNFGESTNR-E  PIC X(17).
               10 PI-DRAN0055-POA-PDVNAMTLKZ-E   PIC X(11).
               10 PI-DRAN0055-POA-PDVNJAHRKM-E   PIC S9(6) COMP.
               10 PI-DRAN0055-POA-WIDERSPR-E     PIC  X(1).
               10 PI-DRAN0055-POA-PDVN-IBAN-E    PIC X(34).
               10 PI-DRAN0055-POA-PDVN-BIC-E     PIC X(11).
      *
       01 PO-DRAN0055.
          05 PO-DRAN0055-RC                    PIC  X(003).
             88 PO-DRAN0055-RC-OK                   VALUE 'OK '.
             88 PO-DRAN0055-RC-NOK                  VALUE 'NOK'.
      ******** ENDE COPY-MEMBER **************************************
