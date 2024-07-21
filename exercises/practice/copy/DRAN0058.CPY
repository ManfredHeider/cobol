      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0058.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRAN0058.CPv  $
      *
      *   Rev 2.6   Oct 24 2013 11:00:32   t13ae23
      *AAN01073: Projekt SEPA - Realisierung AN Deutschland
      *AMV + 
      *ADVC (Dialog)
      *
      *   Rev 2.5   Mar 24 2009 10:56:50   t13ae23
      *AAN00843: Umstellung XGDV-Daten (Antragsdatensätze)
      *
      *   Rev 2.4   Mar 11 2009 16:12:40   t13ae23
      *AAN00843: Umstellung XGDV-Daten (Antragsdatensätze)
      *
      *   Rev 2.3   23 Nov 2007 10:17:14   t13ae23
      *AAN00730: VRA - Kennzeichen Vertriebs-Ges in elektr. Daten G
      *es. 10 AM-Vers -
      *
      *   Rev 2.2   01 Sep 2006 09:44:58   T13AE23
      *AAN00684: elektronische Anträge aus Online-KI
      *
      *   Rev 2.1   Apr 23 2002 17:00:08   t13ae34
      *AAN00357-POA-AMV
      *
      *   Rev 2.0   Mar 18 2002 09:14:32   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0058  FUNKTION SELECT ÜBER GES, PIN, AVERM
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00357!19.04.02!2.1 ! NEUERSTELLUNG             ! HEIDER   *
      * AAN00730!30.11.07!2.3 ! ALLFINANZ                 ! RAHNER   *
      * AAN00843!11.03.09!2.5 ! VERGRÖßERN XGDV-FELD POA  ! RAHNER   *
      * AAN01073!24.10.13!2.6 ! SEPA                      ! RAHNER   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0058                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT  <<<       TABELLE POAA TBAN0002              *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0058.
           05 PI-DRAN0058-POAA-KEY.
               10 PI-DRAN0058-PGEB-GES        PIC S9(4) COMP.
               10 PI-DRAN0058-POA-PIN         PIC S9(9) COMP.
               10 PI-DRAN0058-POA-VDABVERM    PIC S9(9) COMP.
      *
       01  PO-DRAN0058.
           05 PO-DRAN0058-RC                   PIC  X(003).
             88 PO-DRAN0058-RC-OK                   VALUE 'OK '.
             88 PO-DRAN0058-RC-NOT-FOUND            VALUE 'NFD'.
             88 PO-DRAN0058-RC-KEY-NOT-UNIQUE       VALUE 'KNU'.
      *
           05 PO-DRAN0058-POAA-DATEN.
               10 PO-DRAN0058-POA-LAST-UPDATE PIC X(26).
               10 PO-DRAN0058-POA-KENNUNG     PIC X(8).

      *    POA-DATEN AUS PC-DATEN
           COPY DRAN0056.

      ****** ENDE COPY-MEMBER ****************************************
