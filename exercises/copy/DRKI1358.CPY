      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1358.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/DRKI1358.CPv  $
      *
      *   Rev 2.1   May 22 2002 15:28:54   t13ae44
      *AKI00266 - Auswertung VB-Magazin-Bestellungen
      *
      *   Rev 2.0   Mar 12 2002 12:34:36   p13sp16
      *Archiverstellung
      *
      *****************************************************************
      *                                                               *
      *    **** DRKI1358 *****                                        *
      *                                                               *
      *    COPY-MEMBER fuer Auswertung VB-Magazin-Bestellungen        *
      *                                                               *
      *    SATZLÄNGE = 78 Byte                                        *
      *                                                               *
      *---------------------------------------------------------------*
      *    LOGBUCH                                                    *
      *                                                               *
      *AUFTRAG !Datum   !Text                              !Name      *
      *--------!-------------------------------------------!----------*
      *AKI00266!13.03.02! Neuerstellung                    !R. Biehl  *
      *        !        !                                  !          *
      *--------!-------------------------------------------!----------*
       01  DRKI1358.
           05 VB-M-REGION                 PIC X(10).
           05 VB-M-DIR                    PIC 9(3).
           05 VB-M-STUFE                  PIC 9(4).
           05 VB-M-VBNR                   PIC X(7).
           05 VB-M-VBID                   PIC X(26).
           05 VB-M-JAHR                   PIC 9(4).
           05 VB-M-HEFTNUMMER             PIC 9(4).
      *  Anzahl bestellte Hefte :
           05 VB-M-BESTELLUNGEN.
              10 VB-M-BEST-EIGEN              PIC 9(7) COMP-3.
              10 VB-M-BEST-KUNDE              PIC 9(7) COMP-3.
              10 VB-M-BEST-ECHTKUNDE          PIC 9(7) COMP-3.
              10 VB-M-BEST-INTERESSENT        PIC 9(7) COMP-3.
              10 VB-M-BEST-ALTKUNDE           PIC 9(7) COMP-3.
      *---------------------------------------------------------------*
