      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1419.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/MF/CPY/DRPA1419.CPv  $
      *
      *   Rev 2.3   Feb 03 2003 10:09:36   t13ex21
      *FPA00524 -  Erweiterung Gesellschaftsbezeichnung
      *
      *   Rev 2.2   23 May 2001 13:47:46   t13ex21
      *APA00096 - Erweiterung um 2. Last-Update
      *
      *   Rev 2.1   07 Sep 2000 14:56:52   T13EX23
      *APA00096 - Neuerstellung
      *
      *   Rev 2.0   12 Jul 2000 12:32:46   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER DRPA1419: DATENFELDER FÜR DIALOG PA02         *
      *    ( GESELLSCHAFTSABHÄNGIGE DATEN FÜR DAISY INFOCENTER)      *
      *    BENUTZT ALS PO-DRPA1419-ERGEBNISZEILE                     *
      *    -------------------------------------------------------   *
      *    ERSTELLT: GRAY                                            *
      *    DATUM:    08.08.2000                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00096!08.08.00! 2.1! NEU                       ! GRAY     *
      *         !21.05.01! 2.2! 2. LAST-UPDATE EINGEFÜGT  ! STORCH   *
      * FPA00524!31.01.03! 2.3! GESELLSCHAFTSBEZEICHNUNG  ! STORCH   *
      *         !        !    ! EINGEFÜGT                 !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE DATENFELDER FÜR DIALOG PA02       *
      *    ( DAISY INFOCENTER)                                       *
      *    BENUTZT ALS PO-DRPA1418-ERGEBNISZEILE                     *
      *                                                              *
      ****************************************************************
      * DIESE ERGEBNISZEILE HAT EINE LAENGE VON 56 BYTES.            *
      * BEI ANPASSUNGEN, BITTE LAENGE AUCH AKTUALISIEREN             *
      ****************************************************************

      ***   DATENFELDER FÜR LISTDIALOG PA02

             35 ALLG-STAT-PEG          PIC  X.
             35 ALLG-STAT-PVW          PIC  X.
             35 ALLG-NOTIZ-KZ          PIC  X.
             35 DAT-NAECHST-VB         PIC  X(10).
             35 DAT-NAECHST-MA         PIC  X(10).
             35 PGEB-GES               PIC  S9(004) BINARY.
             35 GES-NOTIZ              PIC  X.
310103       35 GES-BEZ10              PIC  X(010).
310103*      35 GES-ZUSTAND            PIC  X(020).
310103       35 GES-ZUSTAND            PIC  X(010).
             35 ANZ-DTAS               PIC  S9(004) BINARY.
             35 ANZ-DTAS-UNR           PIC  S9(004) BINARY.
             35 ANZ-M-DTAS             PIC  S9(004) BINARY.
             35 ANZ-M-DTAS-UNR         PIC  S9(004) BINARY.
             35 SUMME-PRV-BETR         PIC  S9(013)V9(002) COMP-3.
             35 ANZAHL-UMBUCH          PIC  S9(009) BINARY.
             35 DIA-STAT-PEG           PIC  X.
             35 DIA-STAT-PVW           PIC  X.
             35 PGKF-LAST-UPDATE-1     PIC  X(26).
             35 PGKF-LAST-UPDATE-2     PIC  X(26).
      *
      ******* ENDE COPY-MEMBER **************************************
