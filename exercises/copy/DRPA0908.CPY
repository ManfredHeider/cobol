      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0908.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0908.CPv  $
      *
      *   Rev 2.0   May 26 1999 15:29:22   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   01 Apr 1998 14:15:10   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0908 DATENFELDER FüR DIALOG PO23         *
      *                          'pm je produktteil'                 *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    07.05.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!07.05.98! 01 !NEU                        !BOHLINGER *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE DATENFELDER FüR LISTDIALOG PO23   *
      *                                                              *
      ****************************************************************

             30 DATEN-PO23.
                35 PROD-PROD-SCHL      PIC S9(004) BINARY.
                35 PVPT-VT-PRIO        PIC S9(004) BINARY.
                35 PMPT-FAELLIG-VON    PIC  X(010).
                35 PMPT-FAELLIG-BIS    PIC  X(010).
                35 PDPT-BUCH-RFOLGE    PIC S9(004) BINARY.
                35 PMPT-VSTAT          PIC  X(004).
                35 PDPT-MA-ABR-DAT     PIC  X(010).
                35 PDPT-DGV            PIC S9(004) BINARY.
                35 PMPT-GSMT-PROV      PIC S9(009)V99    PACKED-DECIMAL.
                35 PMPT-EINH           PIC S9(006)V99    PACKED-DECIMAL.

