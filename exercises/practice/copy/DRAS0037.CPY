      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  07.05.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAS0037.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAS0037.CPv  $
      *
      *   Rev 2.1   May 16 2012 09:49:40   t13ae36
      *AAS00997: VBI-Bericht zu Bestandsübertragungen auf Gruppeneb
      *ene
      *
      *   Rev 2.0   May 07 2012 10:30:18   t13ae36
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAS0037                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAS0037                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: FRANKE                                           *
      *    DATUM:    10.05.2012                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AAS00997!10.05.12!2.1 ! NEU                       ! FRANKE    *
      * ENDE-LOGB                                                     *
      *****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRAS0037
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRAS0037.
           05  PI-DRAS0037-VBNR        PIC  9(07)     VALUE  ZERO.
           05  PI-DRAS0037-DAT-VON     PIC  X(10)     VALUE SPACE.
           05  PI-DRAS0037-DAT-BIS     PIC  X(10)     VALUE SPACE.
      *
       01  PO-DRAS0037.
           05  PO-DRAS0037-RC                 PIC  X(003).
               88 PO-DRAS0037-RC-OK                VALUE 'OK '.
               88 PO-DRAS0037-RC-NOK               VALUE 'NOK'.
               88 PO-DRAS0037-RC-NOT-FOUND         VALUE 'NFD'.
               88 PO-DRAS0037-RC-ERR               VALUE 'ERR'.
      *
           05  PO-DRAS0037-MA-ST-NR           PIC S9(09)       BINARY.
           05  PO-DRAS0037-ANZ-ISRT           PIC S9(09)       BINARY.
           05  PO-DRAS0037-ANZ-NO-ISRT        PIC S9(09)       BINARY.
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRAS0037   --------------------------*
      *---------------------------------------------------------------*


