      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0955.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA0955.CPv  $
      *
      *   Rev 2.2   18 Jul 2006 09:37:14   t13ae46
      *APA00458: SF44 Textanzeige für PGV
      *
      *   Rev 2.1   Jan 18 2000 14:37:54   T13EX22
      *APA00071: SF44 mit TBPA0026-Zugriff
      *
      *   Rev 2.0   Feb 27 1999 14:29:54   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   01 Apr 1998 14:18:10   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0159 DATENFELDER FÜR DIALOG SF44         *
      *                          'ZUORDNUNG PGV/ DAISY-PGV'          *
      *    -------------------------------------------------------   *
      *    ERSTELLT: MUND                                            *
      *    DATUM:    22.11.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00071!22.11.99! 01 !NEU                        ! MUND     *
      * APA00458!16.06.06! 2.2!PGVZ-PGV-BEG 41-STELLIGE   ! WANG     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE DATENFELDER FÜR DIE LISTDATEN     *
      *    DES DIALOGMODULS FÜR DEN DIALOG SF44                      *
      *                                                              *
      ****************************************************************

      ***   DATENFELDER FÜR LISTDIALOG SF44
            35 PGEB-GES              PIC S9(004)  BINARY.
            35 PGVZ-PGV              PIC  X(008).
160606*     35 PGVZ-PGV-BEZ          PIC  X(040).
            35 PGVZ-VZ-POSITIV       PIC  X(001).
            35 PGVZ-GUELTIG-AB       PIC  X(010).
            35 PGVZ-GUELTIG-BIS      PIC  X(010).
            35 KNR-PGV               PIC S9(004)  BINARY.
            35 PGVZ-LAST-UPDATE      PIC  X(026).
            35 PGVZ-KENNUNG          PIC  X(010).
160606      35 PGVZ-PGV-TEXT         PIC  X(042).

