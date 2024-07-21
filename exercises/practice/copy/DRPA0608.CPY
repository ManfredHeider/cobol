      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0608.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA0608.CPv  $
      *
      *   Rev 2.1   16 May 2007 06:44:22   t13ex44
      *APA00604: Anpassungen in Daisy zur Ablösung PEW
      *
      *   Rev 2.0   Mar 10 1999 12:11:16   T13EX22
      *MFE-Umstellung
      *
      *   Rev 1.0   11 Nov 1997 16:54:30   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0608  FUNKTION KNR-PEWA-ZUSTAND SETZEN   *
      *                           (KTY 148)                          *
      *    -------------------------------------------------------   *
      *    ERSTELLT: MUND                                            *
      *    DATUM:    10.12.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!10.12.97! 01 ! NEUERSTELLUNG             ! MUND     *
      * APA00604!12.04.07!2.1 ! ABSCHLATUNG PEW           ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0608                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>> KNR-PEWA-ZUSTAND SETZEN <<<  PEWA TBPA0011            *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA0608.
          05 PI-DRPA0608-DATEN.
120407*      10 PI-DRPA0608-PEWA-ID          PIC S9(15)V USAGE COMP-3.
120407*      10 PI-DRPA0608-KNR-PEWA-ZUSTAND PIC S9(04) BINARY.
120407       10 PI-DRPA0608-PGEB-GES         PIC S9(004) BINARY.
120407       10 PI-DRPA0608-VNR              PIC  X(015).
120407       10 PI-DRPA0608-PIN              PIC S9(009) BINARY.
120407       10 PI-DRPA0608-KNR-ANTR-STATUS  PIC S9(004) BINARY.
      *
       01 PO-DRPA0608.
          05 PO-DRPA0608-RC                  PIC  X(003).
             88 PO-DRPA0608-RC-OK            VALUE 'OK '.
             88 PO-DRPA0608-RC-NOT-FOUND     VALUE 'NFD'.
             88 PO-DRPA0608-RC-ABGER         VALUE 'ABG'.
      *
      ****** Ende Copy-Member ****************************************
