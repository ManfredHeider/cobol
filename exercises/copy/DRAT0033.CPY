      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0033.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0033.CPv  $
      *
      *   Rev 2.0   Jun 23 1999 15:46:24   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   Nov 04 1995 08:40:08   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0033     MENÜ-DIALOG: TSQ-BLÄTTERN PARAMS *
      *    --------------------------------------------------------   *
      *    VERSION   001                                              *
      *    --------------------------------------------------------   *
      *    ERSTELLT: W. SCHÖLLHAMMER (INTEGRATA AG)                   *
      *    DATUM:    31.03.1993                                       *
      *    STAND:    05.04.1993                                       *
      *****************************************************************
      *
      *    MENÜ-DIALOG:
      *    JEDES ITEM DER TSQ-BLÄTTERN ENTHÄLT ALLE INFORMATIONEN
      *    ZUR ANZEIGE UND AUSWAHL EINER ANWENDUNG (1 MENÜ-ZEILE)
      *
      * -------------------------------------------------------------
       01  Z-DRAT0033-ANWENDUNG.
           05  Z-DRAT0033-TRANS               PIC  X(004).
           05  Z-DRAT0033-TEXT                PIC  X(045).
           05  Z-DRAT0033-STATUS              PIC  9(001).
               88  STATUS-GESPERRT-JA                      VALUE 1.
               88  STATUS-GESPERRT-NEIN                    VALUE 2.
      *
      ******* ENDE COPY-MEMBER DRAT0033 (MENÜ: TSQBL PARAMS)  *********
CCC009*    EJECT
