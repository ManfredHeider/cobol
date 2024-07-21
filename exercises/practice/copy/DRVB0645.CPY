      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2003
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0645.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB0645.CPv  $
      *
      *   Rev 2.2   Jun 05 2003 10:42:16   t13ex22
      *AVB00666: AL-Zusätze anzeigen
      *
      *   Rev 2.1   Mar 19 2003 08:46:12   t13ex22
      *AVB00648: Neuerstellung
      *
      *   Rev 2.0   Feb 17 2003 10:56:40   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0645                                      *
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
       01  PI-DRVB0645.
           05  PI-DRVB0645-DATEN.
               10 PI-DRVB0645-MA-ST-NR         PIC S9(009) COMP.
               10 PI-DRVB0645-ZEITPUNKT        PIC  X(010).
               10 PI-DRVB0645-MS-NR            PIC S9(004) COMP.
      *
       01  PO-DRVB0645.
           05  PO-DRVB0645-RC                  PIC  X(003).
               88 PO-DRVB0645-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0645-RC-NOT-FOUND                 VALUE 'NFD'.
           05  PO-DRVB0645-MA                  PIC  X(003).
               88 PO-DRVB0645-MA-ST-NR-NFD                 VALUE 'NFD'.
               88 PO-DRVB0645-MA-KEIN-VB-AL                VALUE 'KVB'.
               88 PO-DRVB0645-MA-IST-VBA                   VALUE 'VBA'.
260503         88 PO-DRVB0645-MA-IST-VBA2                  VALUE 'VB2'.
260503         88 PO-DRVB0645-MA-IST-VBA3                  VALUE 'VB3'.
               88 PO-DRVB0645-MA-IST-VBA-K                 VALUE 'VBK'.
               88 PO-DRVB0645-MA-IST-AL                    VALUE 'AL '.
               88 PO-DRVB0645-MA-IST-ALD                   VALUE 'ALD'.
260503         88 PO-DRVB0645-MA-IST-AL-K                  VALUE 'ALK'.
260503         88 PO-DRVB0645-MA-IST-AL-P                  VALUE 'ALP'.
260503         88 PO-DRVB0645-MA-IST-AL-D                  VALUE 'ALA'.
      *    NUR GEFÜLLT, WENN VBA ODER VBA-KARRIERE BZW. AL ODER ALD
           05  PO-DRVB0645-MS-BEZ-KURZ         PIC  X(004).
           05  PO-DRVB0645-MS-BEZ              PIC  X(040).
      *
