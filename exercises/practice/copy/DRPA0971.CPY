      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0971.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA0971.CPv  $
      *
      *   Rev 2.1   Nov 03 2006 10:13:24   t13ex44
      *APA00452: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   May 26 1999 16:25:14   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   01 Apr 1998 14:19:32   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER DRPA0971: DATENFELDER FüR DIALOG PO12         *
      *    ( PROVISIONSBESTAND - AUSKUNFT VERTRAGSPRODUKTE )         *
      *    BENUTZT ALS PO-DRPA0967-ERGEBNISZEILE                     *
      *                PO-DRPA0875-ERGEBNISZEILE                     *
      *    -------------------------------------------------------   *
      *    ERSTELLT: KUEKENHOEHNER                                   *
      *    DATUM:    12.06.1998                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!12.06.98! 1.1!NEU                        !KUEKEN    *
      * APA00452!01.11.06! 2.1! DREIST. PRODUKTSCHLUESSEL ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHäLT DIE DATENFELDER FüR DIALOG PO12       *
      *    ( PROVISIONSBESTAND - AUSKUNFT VERTRAGSPRODUKTE )         *
      *    BENUTZT ALS PO-DRPA0967-ERGEBNISZEILE                     *
      *                PO-DRPA0875-ERGEBNISZEILE                     *
      *                                                              *
      ****************************************************************

      ***   DATENFELDER FüR LISTDIALOG PO12

          30 PO-DRPA0971-ERGEBNISZEILE.
011106*      35 PO-DRPA0971-PROD-SCHL            PIC S9(004)  BINARY.
011106       35 PO-DRPA0971-PROD-PROD-SCHL       PIC S9(004)  BINARY.
             35 PO-DRPA0971-PROD-BEZ-KURZ        PIC  X(010).
             35 PO-DRPA0971-PVPT-F-GUELTIG-AB    PIC  X(010).
             35 PO-DRPA0971-PVPT-LAUFZEIT        PIC S9(004)  BINARY.
             35 PO-DRPA0971-PVPT-PTEIL-ABLAUF    PIC  X(010).
             35 PO-DRPA0971-JAHRES-NT-PRAEMIE    PIC S9(9)V9(2)
                                                 PACKED-DECIMAL.
             35 PO-DRPA0971-PVPT-WAEHRUNG-BK     PIC  X(004).
