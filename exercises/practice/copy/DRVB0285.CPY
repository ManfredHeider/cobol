      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0285.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/MF/CPY/DRVB0285.CPv  $
      *
      *   Rev 2.2   May 06 2002 10:11:46   t13ae23
      *AVB00600 - Anzeigen Ortsnamen -
      *
      *   Rev 2.1   16 Jun 2000 11:31:44   T13AE34
      *AVB00483-Dialog VB51 
      *
      *   Rev 2.0   Feb 27 1999 15:58:42   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   23 Jul 1997 12:26:30   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER DRVB0285: DATENFELDER FÜR DIALOG VB51         *
      *    ( SUCHE VB NACH PLZ )                                     *
      *    BENUTZT ALS PO-DRVB0459-ERGEBNISZEILE                     *
      *    -------------------------------------------------------   *
      *    ERSTELLT: RAHNER                                          *
      *    DATUM:    22.03.2000                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00483!22.03.00! 2.1!NEU                        !RAHNER    *
      * AVB00600!06.05.02! 2.2!ANZEIGE ORTSNAME           !RAHNER    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE DATENFELDER FÜR DIALOG VB51       *
      *    ( SUCHE VB NACH PLZ                             )         *
      *    BENUTZT ALS PO-DRPA0459-ERGEBNISZEILE                     *
      *                                                              *
      ****************************************************************

      ***   DATENFELDER FÜR LISTDIALOG VB51

          30 PO-DRVB0285-ERGEBNISZEILE.
             35 PO-DRVB0285-PLZ                  PIC  X(005).
060502       35 PO-DRVB0285-ORTSNAME             PIC  X(015).
             35 PO-DRVB0285-DIRNR                PIC  9(003).
             35 PO-DRVB0285-VBNR                 PIC  9(007).
060502*      35 PO-DRVB0285-NAME                 PIC  X(031).
060502       35 PO-DRVB0285-NAME                 PIC  X(015).
             35 PO-DRVB0285-STUFE-TEXT           PIC  X(010).

