      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0639.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRVB0639.CPv  $
      *
      *   Rev 2.3   Jun 05 2003 10:42:06   t13ex22
      *AVB00666: AL-Zusätze anzeigen
      *
      *   Rev 2.2   Mar 11 2003 14:33:30   t13ex22
      *AVB00648: Zusätzlich Prüfung auf ALD
      *
      *   Rev 2.1   Feb 20 2003 15:33:16   t13ex22
      *AVB00648: Unterscheidung VBA/ VBA-Karriere
      *
      *   Rev 2.0   Dec 04 2002 12:22:06   t13ex15
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0639                                      *
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
       01  PI-DRVB0639.
           05  PI-DRVB0639-DATEN.
               10 PI-DRVB0639-MA-ST-NR         PIC S9(009) COMP.
               10 PI-DRVB0639-ZEITPUNKT        PIC  X(010).
      *
       01  PO-DRVB0639.
           05  PO-DRVB0639-RC                  PIC  X(003).
               88 PO-DRVB0639-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0639-RC-NOT-FOUND                 VALUE 'NFD'.
           05  PO-DRVB0639-MA                  PIC  X(003).
               88 PO-DRVB0639-MA-ST-NR-NFD                 VALUE 'NFD'.
               88 PO-DRVB0639-MA-KEIN-VB-AL                VALUE 'KVB'.
               88 PO-DRVB0639-MA-IST-VBA                   VALUE 'VBA'.
260503         88 PO-DRVB0639-MA-IST-VBA2                  VALUE 'VB2'.
260503         88 PO-DRVB0639-MA-IST-VBA3                  VALUE 'VB3'.
               88 PO-DRVB0639-MA-IST-VBA-K                 VALUE 'VBK'.
               88 PO-DRVB0639-MA-IST-AL                    VALUE 'AL '.
               88 PO-DRVB0639-MA-IST-ALD                   VALUE 'ALD'.
260503         88 PO-DRVB0639-MA-IST-AL-K                  VALUE 'ALK'.
260503         88 PO-DRVB0639-MA-IST-AL-P                  VALUE 'ALP'.
260503         88 PO-DRVB0639-MA-IST-AL-D                  VALUE 'ALA'.
      *    NUR GEFÜLLT, WENN VBA ODER VBA-KARRIERE BZW. AL ODER ALD
           05  PO-DRVB0639-MS-BEZ-KURZ         PIC  X(004).
           05  PO-DRVB0639-MS-BEZ              PIC  X(040).
      *
