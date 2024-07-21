      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0330.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRAT0330.CPv  $
      *
      *   Rev 2.1   07 Jun 2000 14:10:40   T13AE34
      *AAT00019-SF14 Kompetenzgesteuert
      *
      *   Rev 2.0   Mar 16 1999 09:57:28   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0330  FUNKTION DELETE STANDARD-OZP       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAT00019!29.05.00!2.1 ! NEUERSTELLUNG             !HEIDER    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAT0330                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  DELETE <<<        BPKN VIAT0031                      *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRAT0330.
          05 PI-DRAT0330-BPKN-KEY.
             10 PI-DRAT0330-BPKN-USER         PIC  X(007).
             10 PI-DRAT0330-KTY-NR            PIC S9(004)  BINARY.
      *
       01 PO-DRAT0330.
          05 PO-DRAT0330-RC                   PIC  X(003).
             88 PO-DRAT0330-RC-OK                         VALUE 'OK '.
             88 PO-DRAT0330-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRAT0330-RC-NOT-FOUND                  VALUE 'NFD'.
      *
      ****** ENDE COPY-MEMBER ****************************************
