      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2003
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0648.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB0648.CPv  $
      *
      *   Rev 2.2   Jun 05 2003 10:42:24   t13ex22
      *AVB00666: AL-Zusätze anzeigen
      *
      *   Rev 2.1   Mar 26 2003 09:41:50   t13ex22
      *FVB00523: Neue Funktion DRVB0648
      *
      *   Rev 2.0   Feb 17 2003 10:56:58   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0648                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS FUNKTIONS-OZP UPVB0128               *
      *    --------------------------------------------------------   *
      *    ERSTELLT: MUND                                             *
      *    DATUM:    14.02.2003                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00648!14.02.03! 01 ! NEU                       ! MUND     *
      * AVB00666!26.05.03! 2.1! PO-BEREICH ERWEITERT      ! MUND     *
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
       01  PI-DRVB0648.
           05  PI-DRVB0648-DATEN.
               10 PI-DRVB0648-MA-ST-NR         PIC S9(009) COMP.
               10 PI-DRVB0648-MS-NR            PIC S9(004) COMP.

      *
       01  PO-DRVB0648.
           05  PO-DRVB0648-RC                  PIC  X(003).
               88 PO-DRVB0648-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0648-RC-NOT-FOUND                 VALUE 'NFD'.
           05  PO-DRVB0648-MA                  PIC  X(003).
               88 PO-DRVB0648-MA-ST-NR-NFD                 VALUE 'NFD'.
               88 PO-DRVB0648-MA-KEIN-VB-AL                VALUE 'KVB'.
               88 PO-DRVB0648-MA-IST-VBA                   VALUE 'VBA'.
260503         88 PO-DRVB0648-MA-IST-VBA2                  VALUE 'VB2'.
260503         88 PO-DRVB0648-MA-IST-VBA3                  VALUE 'VB3'.
               88 PO-DRVB0648-MA-IST-VBA-K                 VALUE 'VBK'.
               88 PO-DRVB0648-MA-IST-AL                    VALUE 'AL '.
               88 PO-DRVB0648-MA-IST-ALD                   VALUE 'ALD'.
260503         88 PO-DRVB0648-MA-IST-AL-K                  VALUE 'ALK'.
260503         88 PO-DRVB0648-MA-IST-AL-P                  VALUE 'ALP'.
260503         88 PO-DRVB0648-MA-IST-AL-D                  VALUE 'ALA'.
      *    NUR GEFÜLLT, WENN VBA ODER VBA-KARRIERE BZW. AL ODER ALD
           05  PO-DRVB0648-MS-BEZ-KURZ         PIC  X(004).
           05  PO-DRVB0648-MS-BEZ              PIC  X(040).

