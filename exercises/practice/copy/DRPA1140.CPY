      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1140.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1140.CPv  $
      *
      *   Rev 2.2   Jun 09 2020 10:05:38   t13ex67
      *APA02908: Änderung der Abrechnungsweise beim Ersatzversicher
      *ungsgeschäft mit Umstellung von Ges. 11 auf Ges. 10 (betriff
      *finanz 2)
      *
      *   Rev 2.1   May 25 1999 14:08:40   T13AE18
      *apa00012
      *
      *   Rev 2.0   Mar 25 1999 14:10:08   T13AE18
      *APA00012 Einführung EVS-Umbuchungs-
      *dialog
      *
      *   Rev 1.0   21 Oct 1998 13:59:58   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1140 DATENFELDER FüR DIALOG PO15         *
      *                          'VERTRAGSPRODUKTDATEN' FüR EVS      *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    24.11.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!24.11.98! 01 !NEU                        !BOHLINGER *
      * APA02908!27.02.20!2.01!ERWEITERUNG UM GES/VVNR    ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE DATENFELDER FüR DIE LISTDATEN     *
      *    DES DIALOGMODULS FÜR DEN DIALOG PO15                      *
      *                                                              *
      ****************************************************************

      ***    LISTFELDER
             35 PVPT-ID                          PIC S9(015)
                                                 PACKED-DECIMAL.
             35 PVPT-VERSION                     PIC S9(004) BINARY.
             35 PROD-PROD-SCHL                   PIC S9(004) BINARY.
             35 PVPT-VT-PRIO                     PIC S9(004) BINARY.
             35 KNR-PVPT-ART                     PIC S9(004) BINARY.
             35 KNR-PVPT-ART-BK                  PIC  X(004) .
270220       35 GES-VVNR                         PIC  9(002).
             35 VVNR                             PIC  X(015) .
             35 PVPT-PPFL-BEGINN                 PIC  X(010).
             35 KDBETR                           PIC S9(009) BINARY.
             35 PVPT-BEWERT-BETRAG               PIC S9(009)V9(2)
                                                 PACKED-DECIMAL.
             35 PVPT-KNR-WAEHRUNG-BK             PIC  X(004).
             35 EE-ENTSCHEIDUNG                  PIC  X(002).
             35 KNR-EVS-GRUND                    PIC S9(004) BINARY.
             35 PE-STRUKTUR-KZ                   PIC  X(001).
             35 PEST-ID                          PIC S9(015)
                                                 PACKED-DECIMAL.
             35 PDPT-ID                          PIC S9(015)
                                                 PACKED-DECIMAL.

