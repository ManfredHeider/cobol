      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0987.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0987.CPv  $
      *
      *   Rev 2.1   21 Sep 2000 16:06:04   T13EX21
      *APA00106 - Erweiterungen
      *
      *   Rev 2.0   May 26 1999 13:27:14   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   01 Apr 1998 14:20:56   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER DRPA0987: DATENFELDER FÜR DIALOG PO14         *
      *    ( PROVISIONSBESTAND - AUSKUNFT VERTRAGSHISTORIE )         *
      *    BENUTZT ALS PO-DRPA0874-ERGEBNISZEILE                     *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    02.07.1998                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!02.07.98! 1.1! NEU                       ! STORCH   *
      * APA00098!11.09.00! 2.1! ERWEITERUNG UM LAST-UPDATE! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE DATENFELDER FÜR DIALOG PO14       *
      *    ( PROVISIONSBESTAND - AUSKUNFT VERTRAGSPRODUKTE )         *
      *    BENUTZT ALS PO-DRPA0874-ERGEBNISZEILE                     *
      *                                                              *
      ****************************************************************

      ***   DATENFELDER FüR LISTDIALOG PO14

          30 PO-DRPA0987-ERGEBNISZEILE.
             35 PO-DRPA0987-PVER-VNR             PIC  X(015).
             35 PO-DRPA0987-PVER-VERSION         PIC S9(004)    COMP.
             35 PO-DRPA0987-AVERM-KDBETR         PIC S9(007)    COMP.
             35 PO-DRPA0987-PVER-F-GUELTIG-AB    PIC  X(010).
             35 PO-DRPA0987-PVER-F-GUELTIG-BIS   PIC  X(010).
             35 PO-DRPA0987-PVER-KENNUNG         PIC  X(008).
110900       35 PO-DRPA0987-PVER-LAST-UPDATE     PIC  X(026).
