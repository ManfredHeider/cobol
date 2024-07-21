      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.04.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1135.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1135.CPv  $
      *
      *   Rev 2.1   May 04 2016 13:37:54   t13ex71
      *AVB01400: Generierung Lese-OZP, Schreibmodul, etc. für Tabel
      *le TBVB0505
      *
      *   Rev 2.0   Apr 21 2016 15:05:28   t13ex71
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1135                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0505                    *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION ÜBERGEBEN WERDEN    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: B.PRETSCH                                        *
      *    DATUM:    21.04.2016                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB01400!21.04.16!2.1 ! NEU                       ! B.PRETSCH *
120522* VLA-4763!12.05.22!2.2 ! Felddefinitionen COMP-3   ! U. Heim  *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL          *
      *    PRO FUNKTION ÜBERGEBEN WERDEN --> PI/PO-BEREICH            *
      *                                                               *
      *    FUNKTION DRVB1135 - ERMITTELN SATZ AUS TABELLE USTSATZ     *
      *                                                               *
      *****************************************************************
 
       01 PI-DRVB1135.
          05 PI-DRVB1135-DATEN.
             10 PI-DRVB1135-KNR-GESELL-TYP     PIC S9(04)       BINARY.
             10 PI-DRVB1135-KNR-UST-TYP        PIC S9(04)       BINARY.
             10 PI-DRVB1135-UST-DAT-VON        PIC  X(10).
 
       01 PO-DRVB1135.
         03 PO-DRVB1135-RC                     PIC  X(03).
             88 PO-DRVB1135-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1135-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB1135-DATEN.
          05 PO-DRVB1135-NI-VIVB0505.
             10 NI-DRVB1135-KNR-GESELL-TYP     PIC S9(04)       BINARY.
             10 NI-DRVB1135-KNR-UST-TYP        PIC S9(04)       BINARY.
             10 NI-DRVB1135-UST-SATZ           PIC S9(04)       BINARY.
             10 NI-DRVB1135-UST-SCHLUESSEL     PIC S9(04)       BINARY.
             10 NI-DRVB1135-UST-DAT-VON        PIC S9(04)       BINARY.
             10 NI-DRVB1135-UST-DAT-BIS        PIC S9(04)       BINARY.
             10 NI-DRVB1135-KTY-GESELL-TYP     PIC S9(04)       BINARY.
             10 NI-DRVB1135-KTY-UST-TYP        PIC S9(04)       BINARY.
             10 NI-DRVB1135-UST-LAST-UPDATE    PIC S9(04)       BINARY.
          05 PO-DRVB1135-DCL-VIVB0505.
             10 PO-DRVB1135-KNR-GESELL-TYP     PIC S9(04)       BINARY.
             10 PO-DRVB1135-KNR-UST-TYP        PIC S9(04)       BINARY.
120522       10 PO-DRVB1135-UST-SATZ           PIC S9(04)V9(03) COMP-3.
             10 PO-DRVB1135-UST-SCHLUESSEL     PIC S9(04)       BINARY.
             10 PO-DRVB1135-UST-DAT-VON        PIC  X(10).
             10 PO-DRVB1135-UST-DAT-BIS        PIC  X(10).
             10 PO-DRVB1135-KTY-GESELL-TYP     PIC S9(04)       BINARY.
             10 PO-DRVB1135-KTY-UST-TYP        PIC S9(04)       BINARY.
             10 PO-DRVB1135-UST-LAST-UPDATE    PIC  X(26).
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1135   --------------------------*
      *---------------------------------------------------------------*

