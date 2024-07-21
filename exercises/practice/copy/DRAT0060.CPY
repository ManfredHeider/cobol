      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0060.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/DRAT0060.CPv  $
      *
      *   Rev 2.1   Oct 25 2002 09:26:42   t13ae34
      *AAT00023-KS10 - Doku und Vergabe Host-Kompetenzen
      *verbessern
      *
      *   Rev 2.0   Feb 27 1999 11:57:14   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:45:54   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DRAT0060  SATZDEFINITION BENUTZERDATEI-1      * 00020000
      *    --------------------------------------------------------   * 00030000
      *    VERSION   3    11.07.95 16:37:26
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: WIECHERS                                         * 00060000
      *    DATUM:    26.08.1994                                       * 00070000
      *    AUFTRAG   AFRZ0013 - UMSTELLUNG KUS / RACF                 * 00080000
      *                                                               * 00080100
      *    --------------------------------------------------------   * 00050000
      *    GEAENDERT: WIECHERS                                        * 00060000
      *    DATUM:     11.07.1995                                      * 00070000
      *    AUFTRAG    AFRZ0013 - UMSTELLUNG KUS / RACF RESTARBEITEN   * 00080000
      *    VERSION:   03                                              * 00080100
      ***************************************************************** 00084000
      *****************************************************************
      *    VERSION   5    02.10.95 15:14:29
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAT00023!28.08.02!2.1 ! K21-KNR-BENUTZERGRUPPE    ! HEIDER   *
      *         !        !    !                           !          *
      *                                                                 00085000
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  K21-AT0103S.
           02 K21-KEY.
              03 K21-USERID           PIC X(7).
           02 K21-STATUS              PIC X.
950711     02 K21-FILLER              PIC XXXX.
950711*    02 K21-TERMINAL            PIC XXXX.
           02 K21-ABT                 PIC X(5).
           02 K21-VBNR                PIC 9(7).
           02 K21-PERSNR              PIC 9(7).
950711     02 K21-TERMINAL            PIC X(8).
280802*       KTY=206 K+S BENUTZERGRUPPE
280802     02 K21-KNR-BENUTZERGRUPPE-X  PIC X(03).
280802        88 K21-GUELTIGE-BENUTZERGRUPPE   VALUE '001' THRU '999'.
280802        88 K21-KEINE-BENUTZERGRUPPE      VALUE '000'.
280802        88 K21-UNGUELTIGE-BENUTZERGRUPPE VALUE X'000000'
                                                   , X'404040'.
280802     02 K21-KNR-BENUTZERGRUPPE-9 REDEFINES
280802        K21-KNR-BENUTZERGRUPPE-X  PIC 9(3).
280802     02 K21-RESERVE             PIC X(08).
280802*    02 K21-RESERVE          PIC X(11).
950711*    02 K21-RESERVE          PIC X(19).
