      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0641.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0641.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:29:24   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   06 Aug 1998 15:12:32   T01JM2S
      *APAA0032 - PEW-RELEASE 2.0
      *
      *   Rev 1.0   11 Nov 1997 16:57:04   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0641                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<CHECKPOINT SETZEN   >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    24.11.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!24.11.97! 01 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0641                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0641.
          05 PI-DRPA0641-DATEN.
             10 FILLER                              PIC  X(001).
      *
       01 PO-DRPA0641.
          05 PO-DRPA0641-RC                         PIC  X(003).
             88 PO-DRPA0641-RC-OK                         VALUE 'OK '.
          05 PO-DRPA0641-DATEN.
             10 PO-DRPA0641-CHECKPOINT              PIC S9(004) BINARY.
      *
      ******* ENDE COPY-MEMBER **************************************
