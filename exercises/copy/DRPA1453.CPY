      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1453.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1453.CPv  $
      *
      *   Rev 2.1   05 Feb 2001 10:21:42   T13EX22
      *APA00118: Anpassungen für PEG-BFG
      *
      *   Rev 2.0   04 Jan 2001 11:48:14   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *    COPY-MEMBER: DRPA1453  FUNKTION Prüfung des nPGVs für den PG
      *    -------------------------------------------------------
      *    ERSTELLT: MUND
      *    DATUM:    06.01.01
      ******************************************************************
      *    VERSION   01
      ******************************************************************
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME
      * -------------------------------------------------------------
      * APA00118!06.01.01! 01 ! NEUERSTELLUNG             ! MUND
      *         !        !    !                           !
      *         !        !    !                           !
      *         !        !    !                           !
      *
      * ENDE-LOGBUCH
      ******************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA1453
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  Prüfung des nPGVs für den PG  <<<
      *
      ******************************************************************
      *
       01 PI-DRPA1453.
          05 PI-DRPA1453-KEY.
             10 PI-DRPA1453-PGEB-GES              PIC S9(004)  BINARY.
             10 PI-DRPA1453-PGV                   PIC S9(004)  BINARY.
             10 PI-DRPA1453-VZ-POSITIV            PIC  X(001).
             10 PI-DRPA1453-STICHTAG              PIC  X(010).
      *
       01 PO-DRPA1453.
          05 PO-DRPA1453-RC                       PIC  X(003).
             88 PO-DRPA1453-RC-OK                      VALUE 'OK '.
             88 PO-DRPA1453-RC-UNZ                     VALUE 'UNZ'.
      *
      ****** ENDE COPY-MEMBER ******************************************
