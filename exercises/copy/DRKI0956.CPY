      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0956.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:\MF\CPY\DRKI0956.CPv  $
      *
      *   Rev 2.3   04 Sep 2000 11:00:48   T13EX49
      *AKI00071 - endgültige Einführung
      *
      *   Rev 2.2   09 May 2000 15:43:46   T13EX49
      *AKI00071 - Neues Feld B-ID
      *
      *   Rev 2.1   Apr 14 2000 08:46:00   T13EX49
      *AKI00071 - erstmalige Übergabe
      *
      *   Rev 2.0   Nov 29 1999 13:55:06   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
       01  DRKI0956.
           05 B-VB-NR                      PIC 9(07).
           05 B-DIR-NR                     PIC 9(07).
           05 B-MS-NR                      PIC 9(04).
           05 B-ST-TYP                     PIC 9(04).
      *           ED   = 01                                             00040000
      *           MA   = 02                                             00050000
      *           GK   = 03                                             00060000
      *           RFT  = 04                                             00070000
      *           KL   = 05                                             00080000
      *           JPI  = 06                                             00090000
      *           SDNR = 07                                             00100000

      *    Änderungen in Adresse, Telefon, Geburtsdatum etc.
      *    Inhalte: X = Neu, J = Ja geändert, N = nicht geändert
           05 B-PERS-VB-DATEN              PIC X(01).

      *    Änderungen in DIR, Betreuung, Arbeitsstation etc.
      *    Inhalte: X = Neu, J = Ja geändert, N = nicht geändert
           05 B-ALLG-VB-DATEN              PIC X(01).

      *    Änderung in der Direktion
      *    N = neue Direktion
      *    B = Betreuer in neuer Direktion
      *    X = Neuanlage (neuer VB)
      *    K = keine Änderung
           05 B-DIR-GEAENDERT              PIC X(01).
           05 B-DIREKTION-NEU              PIC 9(07).
           05 B-DIREKTION-ALT              PIC 9(07).

      *    Änderung im Betreuungsverhälntiss
      *    X = Neuanlage (neuer VB)
      *    A = Änderung
      *    K = keine Änderung
           05 B-BETREUER-GEAENDERT         PIC X(01).

      *    Änderung in der PC-NR
      *    V = Vergabe
      *    R = Rückgabe
      *    W = Wechsel
      *    K = keine Änderung
      *    L = Löschdatum entfernen, da wieder vergeben
           05 B-PC-NR-GEAENDERT            PIC X(01).

      *    Neue PC-Nummern aus VB-System
           05 B-DESKTOP-PCZ-NR-NEU         PIC 9(07).
           05 B-NOTEBOOK-PCZ-NR-NEU        PIC 9(07).
           05 B-VERWALTUNGS-PCZ-NR-NEU     PIC 9(07).
      *    Alte PC-Nummern aus KI-System
           05 B-DESKTOP-PCZ-NR-ALT         PIC 9(07).
           05 B-NOTEBOOK-PCZ-NR-ALT        PIC 9(07).
           05 B-VERWALTUNGS-PCZ-NR-ALT     PIC 9(07).

      *    Umstufung
      *    0 = keine Umstufung
      *    1 = VM -> VB
      *    2 = VB -> VM
           05 B-UMSTUFUNG                  PIC X(01).

      *    Bestandtrennung
      *    J = Ja, N = Nein
           05 B-Bestandstrennung           PIC X(01).

      *    ID für neuen VB
           05 B-ID                         PIC X(13).

      *    MITARBVORGESID noch einfügen
      *    'J' = ID fehlt noch; sonst ' '
           05 B-MITARBVORGESID             PIC X(01).


