      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.04.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DROK0087.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DROK0087.CPv  $
      *
      *   Rev 2.1   24 Apr 2007 09:53:16   t13ex43
      *AKI00641: PGM7 - Anpassung für HOST_KUNDENUEBER
      *
      *   Rev 2.0   16 Apr 2007 14:19:20   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DROK0087                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LIST-OZP ZTOK0017                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    16.04.2007                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * AKI00641!16.04.07!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DROK0087 - INSERT HOST_KUNDENUEBER                *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DROK0087.
           05  PI-DROK0087-HKUID                 PIC  X(013).
           05  PI-DROK0087-HKUTYP                PIC S9(004) BINARY.
           05  PI-DROK0087-HKUERSTELLTVON        PIC  X(008).
           05  PI-DROK0087-HKUSTEUERART          PIC  X(001).
           05  PI-DROK0087-HKUKUNDENID           PIC  X(013).
           05  PI-DROK0087-HKUVERTRAGID          PIC  X(013).
           05  PI-DROK0087-HKUVBNRALT            PIC S9(009) BINARY.
           05  PI-DROK0087-HKUVBNRNEU            PIC S9(009) BINARY.
      *
       01  PO-DROK0087.
           05  PO-DROK0087-RC                    PIC  X(003).
               88  PO-DROK0087-RC-OK                  VALUE 'OK '.
               88  PO-DROK0087-RC-DUPLICATE-VALUE     VALUE 'DVA'.
      ****
      **** ENDE COPY DROK0087 ****************************************
      ****
