      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0049.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0049.CPv  $
      *
      *   Rev 2.1   30 Jan 2008 09:57:20   t13ex43
      *ASL00285: PGM2 - Auszahlung von Geldleistungen
      *
      *   Rev 2.0   Feb 27 1999 11:57:10   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:43:32   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0049    KNR-KONSTANTEN FUER SPERRE        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KUHN                                             *
      *    DATUM:    03.05.1993                                       *
      *****************************************************************
      *    VERSION   02   16.09.93                                    *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * ------------------------------------------------------------- *
      *         !TT.MM.JJ! 01 ! NEU                       ! KUHN      *
      *         !29.07.93! 02 ! KNR-SPERRE = 5 HINZUGEF.  ! NAUMANN   *
250108* ASL00285!25.01.08! 2.1! KNR-SPERRE = 7 HINZUGEF.  ! KLEEMEIER *
      *         !        !    !                           !           *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *
      *   SCHLÜSSELWERTE FÜR SPERRE, KTY = 40
      *
       01  FILLER.
           05  K-KNR-KEINE-SPERRE        PIC  S9(004) COMP VALUE 00.
           05  K-KNR-SPERRE-AUSZAHLUNG   PIC  S9(004) COMP VALUE 01.
           05  K-KNR-SPERRE-VERSTORBEN   PIC  S9(004) COMP VALUE 02.
           05  K-KNR-SPERRE-PFAENDUNG    PIC  S9(004) COMP VALUE 03.
           05  K-KNR-SPERRE-BV-UNG       PIC  S9(004) COMP VALUE 04.
           05  K-KNR-KEINE-SPERRE-BV-UNG PIC  S9(004) COMP VALUE 05.
250108     05  K-KNR-SPERRE-VORL-ZAHLVB  PIC  S9(004) COMP VALUE 07.
      *
      ******** ENDE COPY-MEMBER DRAT0049  KNR-KONSTANTEN SPERRE  ******
      *
