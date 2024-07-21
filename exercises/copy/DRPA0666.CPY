      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0666.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0666.CPv  $
      *
      *   Rev 2.0   Mar 10 1999 12:11:28   T13EX22
      *MFE-Umstellung
      *
      *   Rev 1.0   11 Nov 1997 16:58:46   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0666  Funktion DELETE Standard-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: MUND                                            *
      *    DATUM:    05.12.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !05.12.97! 01 ! NEUERSTELLUNG             ! MUND     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0666                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  DELETE  <<<       TABELLE PEST TBPA0119              *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA0666.
          05 PI-DRPA0666-LAST-UPDATE         PIC  X(026).
          05 PI-DRPA0666-PEST-KEY.
             10 PI-DRPA0666-PEST-ID          PIC S9(15)V USAGE COMP-3.
      *
       01 PO-DRPA0666.
          05 PO-DRPA0666-RC                   PIC  X(003).
             88 PO-DRPA0666-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0666-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRPA0666-RC-NOT-FOUND                  VALUE 'NFD'.
      *
