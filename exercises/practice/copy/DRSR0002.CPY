      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.10.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRSR0002.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRSR0002.CPv  $
      *
      *   Rev 2.2   Oct 28 2004 13:56:10   t13ex22
      *ASR00001: PGM - CTV-Parameterbeschaffung Online
      *
      *   Rev 2.1   Oct 26 2004 08:24:20   t13ex43
      *ASR00001: PGM - CTV-Parameterbeschaffung
      *
      *   Rev 2.0   Oct 19 2004 12:56:22   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRSR0002                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO UPSR0003                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    19.10.2004                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASR00001!19.10.04!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER          *
      *    FÜR DIE FUNKTION DRSR0002                                  *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01 PI-DRSR0002.
          05 PI-DRSR0002-MA-ST-NR           PIC S9(009) BINARY.
      *
       01 PO-DRSR0002.
          05 PO-DRSR0002-RC                 PIC  X(003).
             88 PO-DRSR0002-RC-OK                VALUE 'OK '.
             88 PO-DRSR0002-RC-NOT-FOUND         VALUE 'NFD'.
          05 PO-DRSR0002-DATEN.
             10 PO-DRSR0002-MA-ST-NR        PIC S9(009) BINARY.
             10 PO-DRSR0002-PZ              PIC  X(001).
             10 PO-DRSR0002-LKZ             PIC S9(009) BINARY.
             10 PO-DRSR0002-SKZ             PIC S9(009) BINARY.
             10 PO-DRSR0002-VORNAME         PIC  X(020).
             10 PO-DRSR0002-NACHNAME        PIC  X(040).
      *
      ***** ENDE COPY-MEMBER DRSR0002 *****************************
      *
