      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU099S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UU099S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:15:18   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   17 Dec 1998 08:39:24   T13EX36
      *AOP00161 - Jahr2000-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:54:50   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FUER NEUE-DIREKTIONEN-DATEI UU0099              *
      *    ----------------------------------------------              *
      *                                                                *
      *    LAENGE = 14 BYTES         ERSTELLT AM 04.10.91.             *
      *                              MUELLER (VD-178)                  *
      *----------------------------------------------------------------*
CCC009*    SKIP1
       01  UU099S.
           10 XDIR-NEU              PIC 9(3).
           10 XAIX.
              20 XDIR-ALT           PIC 9(3).
*Y2K***       20 XDATUM             PIC 9(6) COMP-3.
              20 XDATUM             PIC 9(7) COMP-3.
              20 XMANR              PIC 9(7) COMP-3.
