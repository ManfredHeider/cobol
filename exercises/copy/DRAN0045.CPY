      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0045.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRAN0045.CPv  $
      *
      *   Rev 2.7   Oct 24 2013 10:59:46   t13ae23
      *AAN01073: Projekt SEPA - Realisierung AN Deutschland
      *AMV + 
      *ADVC (Dialog)
      *
      *   Rev 2.6   Mar 24 2009 10:56:02   t13ae23
      *AAN00843: Umstellung XGDV-Daten (Antragsdatensätze)
      *
      *   Rev 2.5   Mar 11 2009 16:11:54   t13ae23
      *AAN00843: Umstellung XGDV-Daten (Antragsdatensätze)
      *
      *   Rev 2.4   23 Nov 2007 10:17:24   t13ae23
      *AAN00730: VRA - Kennzeichen Vertriebs-Ges in elektr. Daten G
      *es. 10 AM-Vers -
      *
      *   Rev 2.3   01 Sep 2006 09:43:34   T13AE23
      *AAN00684: elektronische Anträge aus Online-KI
      *
      *   Rev 2.2   31 Aug 2006 09:42:28   T13AE23
      *AAN00684: elektronische Anträge aus Online-KI
      *
      *   Rev 2.1   Apr 23 2002 16:59:52   t13ae34
      *AAN00357-POA-AMV
      *
      *   Rev 2.0   Feb 27 1999 11:05:38   P13SP20
      *NT-Umstellung, Einführung MFE
      *
      *   Rev 1.0   30 Jul 1997 17:38:14   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0045  FUNKTION SELECT STANDARD-OZP       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00268!23.11.00!2.1 ! NEUERSTELLUNG             ! HEIDER   *
      * AAN00684!31.08.06!2.2 ! KUNDENBESTANDS-ID FÜR AVS ! RAHNER   *
      * AAN00730!30.11.07!2.4 ! ALLFINANZ                 ! RAHNER   *
      * AAN00843!11.03.09!2.6 ! VERGRÖßERN XGDV-FELD POA  ! RAHNER   *
      * AAN01073!24.10.13!2.7 ! SEPA                      ! RAHNER   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0045                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT  <<<       TABELLE POAA TBAN0003              *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0045.
           05 PI-DRAN0045-POAA-KEY.
               10 PI-DRAN0045-PGEB-GES        PIC S9(4) COMP.
               10 PI-DRAN0045-POA-PIN         PIC S9(9) COMP.
      *
       01  PO-DRAN0045.
           05 PO-DRAN0045-RC                   PIC  X(003).
             88 PO-DRAN0045-RC-OK                   VALUE 'OK '.
             88 PO-DRAN0045-RC-NOT-FOUND            VALUE 'NFD'.
             88 PO-DRAN0045-RC-KEY-NOT-UNIQUE       VALUE 'KNU'.
      *
           05 PO-DRAN0045-POAA-DATEN.
               10 PO-DRAN0045-POA-LAST-UPDATE PIC X(26).
               10 PO-DRAN0045-POA-KENNUNG     PIC X(8).

      *
      *    POA-DATEN AUS PC-DATEN
           COPY DRAN0056.
      ****** ENDE COPY-MEMBER ****************************************
