      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1219.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1219.CPv  $
      *
      *   Rev 2.1   Apr 21 1999 09:25:32   T13AE18
      *APA00012 Neu
      *
      *   Rev 2.0   Mar 30 1999 08:44:02   P13SP99
      *Neuerstellung
      *
      *   Rev 2.0   Mar 05 1999 09:20:24   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1219  Funktion Update faellig-bis        *
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
      *    FüR DIE FUNKTION DRPA1219                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE  FAELLIG-BIS <<<     TABELLE PMPT TBPA0117    *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA1219.
          05 PI-DRPA1219-PMPT-LAST-UPDATE   PIC  X(026).
          05 PI-DRPA1219-PMPT-KEY.
           10 PI-DRPA1219-PMPT-ID           PIC S9(015)V PACKED-DECIMAL.
           10 PI-DRPA1219-PMPT-VERSION      PIC S9(004)  BINARY.
          05 PI-DRPA1219-PMPT-DATEN.
           10 PI-DRPA1219-PMPT-FAELLIG-BIS  PIC X(010).
      *
       01 PO-DRPA1219.
          05 PO-DRPA1219-RC                 PIC  X(003).
             88 PO-DRPA1219-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1219-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRPA1219-RC-NOT-FOUND                  VALUE 'NFD'.
      *
      ****** Ende Copy-Member ****************************************
