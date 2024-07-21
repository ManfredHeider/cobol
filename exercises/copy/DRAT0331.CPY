      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0331.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRAT0331.CPv  $
      *
      *   Rev 2.1   07 Jun 2000 14:10:42   T13AE34
      *AAT00019-SF14 Kompetenzgesteuert
      *
      *   Rev 2.0   Mar 16 1999 09:57:28   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0331  FUNKTION SELECT ALL STANDARD-OZP   *
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
      *    FÜR DIE FUNKTION DRAT0331                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SEL ALL<<<        BPKN VIAT0031                      *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRAT0331.
          05 PI-DRAT0331-BPKN-KEY.
             10 PI-DRAT0331-BPKN-USER         PIC  X(007).
             10 PI-DRAT0331-KTY-NR            PIC S9(004)  BINARY.
      *
       01 PO-DRAT0331.
          05 PO-DRAT0331-BPKN-DATEN.
             10 PO-DRAT0331-BPKN-KS-PROFIL    PIC  X(001).
             10 PO-DRAT0331-BPKN-KENNUNG      PIC  X(008).
          05 PO-DRAT0331-RC                   PIC  X(003).
             88 PO-DRAT0331-RC-OK                         VALUE 'OK '.
             88 PO-DRAT0331-RC-NOT-FOUND                  VALUE 'NFD'.
      *
      ****** ENDE COPY-MEMBER ****************************************
