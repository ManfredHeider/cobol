      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU705S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UU705S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:15:28   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:04:40   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***** VERSION   1    03.08.93 10:51:09
      ****************************************************************
      *                                                              *
      *   COPYMEMBER: STAMMSATZ HOST ==> PC  (SUSIS)                 *
      *   ==========================================                 *
      *                                                              *
      *   SATZLÄNGE :  225  FEST                                     *
      *   STAND :      31.03.1993                                    *
      *                                                              *
      *   SATZAUFBAU FÜR DEN FD-EINTRAG  UU705S                      *
      ****************************************************************
      *
      *
       01  UU705S.
           05  UU705S-MA-KTO             PIC X(010).
           05  UU705S-DIRNR              PIC X(004).
           05  UU705S-STUFE              PIC X(004).
           05  UU705S-EINTRITT-DAT.
               10  UU705S-EIN-JJ         PIC 9(004).
               10  UU705S-EIN-MM         PIC 9(002).
               10  UU705S-EIN-TT         PIC 9(002).
           05  UU705S-AUSTRITT-DAT.
               10  UU705S-AUS-JJ         PIC 9(004).
               10  UU705S-AUS-MM         PIC 9(002).
               10  UU705S-AUS-TT         PIC 9(002).
           05  UU705S-KTO-DAT.
               10  UU705S-KTO-JJ         PIC 9(004).
               10  UU705S-KTO-MM         PIC 9(002).
               10  UU705S-KTO-TT         PIC 9(002).
           05  UU705S-KTOSTAENDE.
               10  UU705S-DISKONT        PIC -(09)9V99.
               10  UU705S-RUECKST        PIC -(09)9V99.
               10  UU705S-VORSCHUSS      PIC -(09)9V99.
               10  UU705S-ABSCHREIBUNG   PIC -(09)9V99.
           05  UU705S-HAFTUNGSVOLUMEN    PIC -(09)9V99.
           05  UU705S-ANSCHRIFT.
               10  UU705S-ANREDE         PIC 9(002).
               10  UU705S-NNAME          PIC X(021).
               10  UU705S-VNAME          PIC X(016).
               10  UU705S-STR            PIC X(025).
               10  UU705S-LKZ            PIC X(003).
               10  UU705S-PLZ            PIC X(005).
               10  UU705S-ORT            PIC X(025).
               10  UU705S-GEB-DAT        PIC 9(008).
               10  UU705S-KNR-GRND       PIC 9(002).
           05  UU705S-KENNZEICHEN.
               10  UU705S-AKT-LFD-GJ     PIC 9(006).
               10  UU705S-KZ-BEWG        PIC X(001).
               10  UU705S-KZ-LOESCH      PIC X(001).
               10  UU705S-KZ-SELEKT      PIC X(001).
           05  UU705S-FILLER             PIC X(007).
      *
