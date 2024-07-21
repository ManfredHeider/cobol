      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  23.11.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1033.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB1033.CPv  $
      *
      *   Rev 2.2   May 31 2016 09:25:30   t13ae23
      *AAN01259: Prüfung eingereichtes Geschäft bezüglich §34i
      *
      *   Rev 2.1   Jan 30 2012 16:30:42   t13ae23
      *AAN01042: fehlende Anlageberatung eingereichtes Investement-
      *Geschäft
      *
      *   Rev 2.0   Nov 23 2011 11:28:12   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ***************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN01042!23.11.11!2.1 ! NEU                       ! RAHNER   *
      * AAN01258!30.05.16!2.2 ! WEITERE INFOS ZU VB ERM.  ! RAHNER   *
      * AAN01259!31.05.16!2.2 ! WEITERE INFOS ZU VB ERM.  ! RAHNER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB1033                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB1033.
          05 PI-DRVB1033-DATEN.
             10 PI-DRVB1033-MA-ST-NR         PIC S9(009) COMP.
      *
       01 PO-DRVB1033.
          05 PO-DRVB1033-RC                  PIC  X(003).
             88 PO-DRVB1033-RC-OK            VALUE 'OK '.
             88 PO-DRVB1033-RC-NFD           VALUE 'NFD'.
          05 PO-DRVB1033-DATEN.
             10 PO-DRVB1033-MA-ANLAGE-BER    PIC X(001).
300516       10 PO-DRVB1033-MA-34F-REG-NR    PIC X(015).
310516       10 PO-DRVB1033-MA-34I-REG-NR    PIC X(015).
310516       10 PO-DRVB1033-MA-34I-DATUM     PIC X(010).

