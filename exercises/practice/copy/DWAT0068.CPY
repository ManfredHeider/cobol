      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.01.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DWAT0068.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DWAT0068.CPv  $
      *
      *   Rev 2.1   Jan 13 2017 15:30:02   t13ac20
      *AAT00056: Ermittlung Datum, Uhrzeit
      *
      *   Rev 2.0   Jan 13 2017 11:33:26   t13ac20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DWAT0068                                      *
      *    --------------------------------------------------------   *
      *    BATCHRAHMEN: DATENFELDER ZU LAUFDATUM UND -UHRZEIT,        *
      *                 SOWIE GENERIERUNGSDATUM UND -UHRZEIT          *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SWO                                              *
      *    DATUM:    13.01.2017                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AAT00056!13.01.17!2.1 ! NEU                       ! SWO       *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
       01  BR-Z-LAUFDATUM-ZEIT.
      *    LAUFDATUM IM FORMAT TT.MM.JJJJ
           05 BR-Z-LAUFDATUM-EUR.
              10 BR-Z-LD-TAG-EUR    PIC 99.
              10                    PIC X             VALUE '.'.
              10 BR-Z-LD-MON-EUR    PIC 99.
              10                    PIC X             VALUE '.'.
              10 BR-Z-LD-JAHR-EUR   PIC 9(4).
      *    LAUFDATUM IM FORMAT JJJJMMTT
           05 BR-Z-LAUFDATUM-ISO1   PIC 9(8).
           05                       REDEFINES BR-Z-LAUFDATUM-ISO1.
              10 BR-Z-LD-JAHR-ISO1  PIC 9(4).
              10 BR-Z-LD-MON-ISO1   PIC 99.
              10 BR-Z-LD-TAG-ISO1   PIC 99.
      *    LAUFDATUM IM FORMAT JJJJ-MM-TT
           05 BR-Z-LAUFDATUM-ISO2.
              10 BR-Z-LD-JAHR-ISO2  PIC 9(4).
              10                    PIC X             VALUE '-'.
              10 BR-Z-LD-MON-ISO2   PIC 99.
              10                    PIC X             VALUE '-'.
              10 BR-Z-LD-TAG-ISO2   PIC 99.
      *    LAUFDATUM IM FORMAT JJJJ-MM-TT-HH.MM.SS.HS0000
      *                        (DB2-TIMESTAMP)
           05 BR-Z-LAUFDATUM-TS.
              10 BR-Z-LD-JAHR-TS    PIC 9(4).
              10                    PIC X             VALUE '-'.
              10 BR-Z-LD-MON-TS     PIC 99.
              10                    PIC X             VALUE '-'.
              10 BR-Z-LD-TAG-TS     PIC 99.
              10                    PIC X             VALUE '-'.
              10 BR-Z-LD-STD-TS     PIC 99.
              10                    PIC X             VALUE '.'.
              10 BR-Z-LD-MIN-TS     PIC 99.
              10                    PIC X             VALUE '.'.
              10 BR-Z-LD-SEK-TS     PIC 99.
              10                    PIC X             VALUE '.'.
              10 BR-Z-LD-HSEK-TS    PIC 99.
              10                    PIC 9999          VALUE ZERO.
      *    LAUFZEIT IM FORMAT HH:MM:SS:HS
           05 BR-Z-LAUFZEIT-A.
              10 BR-Z-LZ-STD-A      PIC 99.
              10                    PIC X             VALUE ':'.
              10 BR-Z-LZ-MIN-A      PIC 99.
              10                    PIC X             VALUE ':'.
              10 BR-Z-LZ-SEK-A      PIC 99.
              10                    PIC X             VALUE ':'.
              10 BR-Z-LZ-HSEK-A     PIC 99.
      *    LAUFZEIT IM FORMAT HHMMSSHS
           05 BR-Z-LAUFZEIT-B.
              10 BR-Z-LZ-STD-B      PIC 99.
              10 BR-Z-LZ-MIN-B      PIC 99.
              10 BR-Z-LZ-SEK-B      PIC 99.
              10 BR-Z-LZ-HSEK-B     PIC 99.
      *    GENERIERUNGSDATUM/-ZEIT IM FORMAT TT.MM.JJJJ-HH:MM:SS
           05 BR-Z-GENDATUM-A.
              10 BR-Z-GD-TAG-A      PIC 99.
              10                    PIC X             VALUE '.'.
              10 BR-Z-GD-MON-A      PIC 99.
              10                    PIC X             VALUE '.'.
              10 BR-Z-GD-JAHR-A     PIC 9(4).
           05 BR-Z-GENZEIT-A.
              10 BR-Z-GZ-STD-A      PIC 99.
              10                    PIC X             VALUE ':'.
              10 BR-Z-GZ-MIN-A      PIC 99.
              10                    PIC X             VALUE ':'.
              10 BR-Z-GZ-SEK-A      PIC 99.
      *    GENERIERUNGSDATUM/-ZEIT IM FORMAT MM.TT.JJ.HH MM SS
           05 BR-Z-GENDATUM-B.
              10 BR-Z-GD-MON-B      PIC 99.
              10                    PIC X             VALUE SPACE.
              10 BR-Z-GD-TAG-B      PIC 99.
              10                    PIC X             VALUE SPACE.
              10 BR-Z-GD-JHR-B      PIC 99.
              10 BR-Z-GD-STD-B      PIC 99.
              10                    PIC X             VALUE SPACE.
              10 BR-Z-GD-MIN-B      PIC 99.
              10                    PIC X             VALUE SPACE.
              10 BR-Z-GD-SEK-B      PIC 99.

