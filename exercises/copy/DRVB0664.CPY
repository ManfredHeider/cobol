      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2003
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0664.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0664.CPv  $
      *
      *   Rev 2.1   Jul 17 2003 10:43:28   t13ex22
      *AVB00688: Neuerstellung
      *
      *   Rev 2.0   Feb 17 2003 10:58:30   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0664                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS FUNKTIONS-OZP UPVB0131               *
      *    --------------------------------------------------------   *
      *    ERSTELLT: MUND                                             *
      *    DATUM:    17.07.2003                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00688!17.07.03! 02 ! NEU                       ! MUND     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      ****************************************************************
      *
       01  PI-DRVB0664.
           05  PI-DRVB0664-DATEN.
               10 PI-DRVB0664-KL-ST-NR         PIC S9(009) COMP.
               10 PI-DRVB0664-MONAT-VON        PIC  X(010).
               10 PI-DRVB0664-MONAT-BIS        PIC  X(010).
               10 PI-DRVB0664-FUNKTION         PIC  9(002).
                  88 PI-DRVB0664-NETTO-NEU-EIGEN    VALUE 1.
                  88 PI-DRVB0664-NETTO-NEU-GRUPPE   VALUE 2.
                  88 PI-DRVB0664-NETTO-NEU-GESAMT   VALUE 3.
                  88 PI-DRVB0664-NETTO-EIGEN        VALUE 4.
                  88 PI-DRVB0664-NETTO-GRUPPE       VALUE 5.
                  88 PI-DRVB0664-NETTO-GESAMT       VALUE 6.
      *
       01  PO-DRVB0664.
           05  PO-DRVB0664-RC                  PIC  X(003).
               88 PO-DRVB0664-RC-OK                 VALUE 'OK '.
               88 PO-DRVB0664-RC-NOT-FOUND          VALUE 'NFD'.
           05  PO-DRVB0664-DATEN.
               10  PO-DRVB0664-EH              PIC S9(9)V9 COMP-3.
      *
