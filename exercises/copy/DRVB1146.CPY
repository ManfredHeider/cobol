      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  07.06.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1146.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1146.CPv  $
      *
      *   Rev 2.1   Jun 30 2016 11:54:12   t13ex71
      *AVB01400: Generierung Lese-OZP, Schreibmodul, etc. für Tabel
      *le TBVB0116 (BPR)
      *
      *   Rev 2.0   Jun 07 2016 15:01:48   t13ex71
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1146                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0116                    *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION ÜBERGEBEN WERDEN    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: B.PRETSCH                                        *
      *    DATUM:    07.06.2016                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB01400!07.06.16!2.1 ! NEU                       ! B.PRETSCH *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL          *
      *    PRO FUNKTION ÜBERGEBEN WERDEN --> PI/PO-BEREICH            *
      *                                                               *
      *    FUNKTION DRVB1146 - ERMITTELN SATZ AUS TABELLE ALIAS    *
      *                                                               *
      *****************************************************************
 
       01 PI-DRVB1146.
          05 PI-DRVB1146-DATEN.
             10 PI-DRVB1146-ST-NR              PIC S9(09)       BINARY.
             10 PI-DRVB1146-RSTAT-DAT-VON      PIC  X(26).
 
       01 PO-DRVB1146.
         03 PO-DRVB1146-RC                     PIC  X(03).
             88 PO-DRVB1146-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1146-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB1146-DATEN.
          05 PO-DRVB1146-NI-VIVB0116.
             10 NI-DRVB1146-ST-NR              PIC S9(04)       BINARY.
             10 NI-DRVB1146-RSTAT-DAT-VON      PIC S9(04)       BINARY.
             10 NI-DRVB1146-KNR-RSTAT-FIBU     PIC S9(04)       BINARY.
             10 NI-DRVB1146-KTY-RSTAT-FIBU     PIC S9(04)       BINARY.
             10 NI-DRVB1146-KNR-RSTAT-DAISY    PIC S9(04)       BINARY.
             10 NI-DRVB1146-KTY-RSTAT-DAISY    PIC S9(04)       BINARY.
             10 NI-DRVB1146-RSTAT-DAT-BIS      PIC S9(04)       BINARY.
             10 NI-DRVB1146-RSTAT-LAST-UPDATE  PIC S9(04)       BINARY.
          05 PO-DRVB1146-DCL-VIVB0116.
             10 PO-DRVB1146-ST-NR              PIC S9(09)       BINARY.
             10 PO-DRVB1146-RSTAT-DAT-VON      PIC  X(26).
             10 PO-DRVB1146-KNR-RSTAT-FIBU     PIC S9(04)       BINARY.
             10 PO-DRVB1146-KTY-RSTAT-FIBU     PIC S9(04)       BINARY.
             10 PO-DRVB1146-KNR-RSTAT-DAISY    PIC S9(04)       BINARY.
             10 PO-DRVB1146-KTY-RSTAT-DAISY    PIC S9(04)       BINARY.
             10 PO-DRVB1146-RSTAT-DAT-BIS      PIC  X(10).
             10 PO-DRVB1146-RSTAT-LAST-UPDATE  PIC  X(26).
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1146   --------------------------*
      *---------------------------------------------------------------*

