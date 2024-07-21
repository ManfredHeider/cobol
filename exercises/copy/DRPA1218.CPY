      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1218.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1218.CPv  $
      *
      *   Rev 2.1   Apr 21 1999 09:25:30   T13AE18
      *APA00012 Neu
      *
      *   Rev 2.0   Mar 30 1999 08:44:04   P13SP99
      *Neuerstellung
      *
      *   Rev 2.0   Mar 05 1999 09:20:26   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1218  FUNKTION UPDATE PGFAELLIG-BIS      *
      *                                       für Standard-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Bohlinger                                       *
      *    DATUM:    16.04.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!16.05.99! 01 ! NEUERSTELLUNG             !Bohlinger *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHäLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FüR DIE FUNKTION DRPA1218                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE  PGFAELLIG-BIS <<<   TABELLE PMBE TBPA0113    *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA1218.
          05 PI-DRPA1218-PMBE-LAST-UPDATE   PIC  X(026).
          05 PI-DRPA1218-PMBE-KEY.
           10 PI-DRPA1218-PMBE-ID           PIC S9(015)V PACKED-DECIMAL.
           10 PI-DRPA1218-PMBE-VERSION      PIC S9(004)  BINARY.
          05 PI-DRPA1218-PMBE-DATEN.
           10 PI-DRPA1218-PMBE-PGFAELLIG-BIS PIC X(010).
      *
       01 PO-DRPA1218.
          05 PO-DRPA1218-RC                 PIC  X(003).
             88 PO-DRPA1218-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1218-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRPA1218-RC-NOT-FOUND                  VALUE 'NFD'.
      *
      ****** Ende Copy-Member ****************************************
