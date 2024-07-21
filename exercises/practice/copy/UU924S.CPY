      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU924S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UU924S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:15:32   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:08:10   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION
      ******************************************************
      *                                                    *
      *       COPY-MEMBER TEXT-TABELLE VB-BEARBEITUNG      *
      *                                                    *
      ******************************************************
       01  UU924S.
           02  FTAB.
               05  FILLER          PIC X(79)       VALUE
                   'FEHLER:  ANREDE FEHLT ODER UNGUELTIG'.
               05  FILLER          PIC X(79)       VALUE
                   'FEHLER:  ZUNAME FEHLT ODER UNGUELTIG'.
               05  FILLER          PIC X(79)       VALUE
                   'FEHLER:  VORNAME FEHLT ODER UNGUELTIG'.
               05  FILLER          PIC X(79)       VALUE
                   'FEHLER:  STRASSE FEHLT ODER UNGUELTIG'.
               05  FILLER          PIC X(79)       VALUE
                   'FEHLER:  POSTLEITZAHL FEHLT ODER UNGUELTIG'.
               05  FILLER          PIC X(79)       VALUE
                   'FEHLER:  WOHNORT FEHLT ODER UNGUELTIG'.
               05  FILLER          PIC X(79)       VALUE
                   'FEHLER:  GELDINSTITUT FEHLT ODER UNGUELTIG'.
               05  FILLER          PIC X(79)       VALUE
                   'FEHLER:  BANKLEITZAHL FEHLT ODER UNGUELTIG'.
               05  FILLER          PIC X(79)       VALUE
                   'FEHLER:  KONTO-NR. FEHLT ODER UNGUELTIG'.
               05  FILLER          PIC X(79)       VALUE
                   'FEHLER:  EINTRITTS-DATUM FEHLT ODER NICHT NUMERISCH
      -            '    '.
               05  FILLER          PIC X(79)       VALUE
                   'FEHLER:  EINTRITTS-DATUM ENTHAELT EIN UNGUELTIGES DA
      -            'TUM'.
               05  FILLER          PIC X(79)       VALUE
                   'FEHLER:  EINTRITTS-DATUM KLEINER TAGESDATUM'.
               05  FILLER          PIC X(79)       VALUE
                   'FEHLER:  FUNKTION UNGUELTIG'.
               05  FILLER          PIC X(79)       VALUE
                   'FEHLER:  PROVRUECKSTELLUNG FEHLT ODER UNGUELTIG'.
               05  FILLER          PIC X(79)       VALUE
                   'FEHLER:  STRUKTUR-STUFE FEHLT ODER UNGUELTIG'.
               05  FILLER          PIC X(79)       VALUE
                   'FEHLER:  BETREUER-VBNR FEHLT ODER UNGUELTIG'.
               05  FILLER          PIC X(79)       VALUE
                   'FEHLER:  BETREUER-VBNR 1.STELLE NICHT = 8'.
               05  FILLER          PIC X(79)       VALUE
                   'FEHLER:  BETREUER-VBNR 7.STELLE NICHT = 0'.
               05  FILLER          PIC X(79)       VALUE
                   'FEHLER:  PRUEFZIFFER BETREUER VBNR NICHT KORREKT'.
               05  FILLER          PIC X(79)       VALUE
                   'FEHLER:  BETREUER-VBNR NICHT IN DER VB-DATEI VORHAND
      -            'EN'.
               05  FILLER          PIC X(79)       VALUE
                   'FEHLER:  STRUKTURSTUFE BETREUER NICHT GROESSER STRUK
      -            'TURSTUFE DES NEUEN VB'.
               05  FILLER          PIC X(79)       VALUE
                    'FEHLER:  BETREUER NICHT MEHR TAETIG'.
               05  FILLER          PIC X(79)       VALUE
                   'FEHLER:  AUSNAHME-PROVISION UNGUELTIG'.
               05  FILLER          PIC X(79)       VALUE
                   'FEHLER:  AUSNAHME-PROVISION-PROZENTSATZ NICHT KLEINE
      -            'R PROVISIONSSATZ'.
               05  FILLER          PIC X(79)       VALUE
                   'FEHLER:  AUSNAHME-PROVISIONS-KZ NICHT = 1 - 9'.
               05  FILLER          PIC X(79)       VALUE
                   'FEHLER:  STRUKTURSTUFE BETREUER IST NICHT IN DER PRO
      -            'V.-TABELLE'.
               05  FILLER          PIC X(79)       VALUE
                   'FEHLER:  EINGABE UNGUELTIG. ES IST NUR DIE PF-TASTE
      -            '9 ERLAUBT'.
               05  FILLER          PIC X(79)       VALUE
                   'FEHLER:  VBNR WURDE INZWISCHEN IN DIE VB-DATEI AUFGE
      -            'NOMMEN'.
               05  FILLER          PIC X(79)       VALUE
                   'FEHLER:  EINGEGEBENE STRUKTURSTUFE NICHT IN DER PROV
      -            '.-TABELLE'.
               05  FILLER          PIC X(79)       VALUE
                   'FEHLER:  KONTOINHABER FALSCH'.
               05  FILLER          PIC X(79)       VALUE
                   'FEHLER:  DIREKTION DES BETREUERS ENTSPRICHT NICHT DE
      -            'R EINGEGEBENEN DIREKTION'.
               05  FILLER          PIC X(79)       VALUE
                   'FEHLER:  BEDIENER DARF FELD NICHT EINGEBEN'.
               05  FILLER          PIC X(79)       VALUE
                   'FEHLER:  AVBG DARF KEINE AUSNAHME-PROVISION ERHALTEN
      -            '   '.
               05  FILLER          PIC X(79)       VALUE
                   'FEHLER:  AP-SATZ-% UNGUELTIG'.
               05  FILLER          PIC X(79)       VALUE
                   'FEHLER:  AP-SATZ-% DARF ERST AB AL EINGEGEBEN WERDEN
      -            ' '.
           02  FTAB1   REDEFINES   FTAB.
               05  TEXT4           PIC X(79)       OCCURS 35.

