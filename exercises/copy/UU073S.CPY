      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU073S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UU073S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:15:14   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:52:42   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION   2    26.08.94 16:35:48
      *    -------------------------------------------------------------
      *    COPY-MEMBER UU073S
      *    ------------------
      *
      *    T D S - S A T Z    AENDERN KUNDEN-DATEI
      *    ---------------------------------------
      *    SATZLAENGE: 70 BYTE
      *                                    ERSTELLT: H.SCHORN (AGS)
      *                                       DATUM: 23.11.82
      *
      *    GEAENDERT AM 03.06.85 / BIEHL:  KD-AVMA, KD-AVMN, KD-GUELTN
      *                                    HINZUGEFUEGT
      *
      *    GEAENDERT AM 26.08.94 / WIECHERS / AUFTRAG AFRZ0013
      *                                       KUS-SYSTEM / RACF
      *
      *    -------------------------------------------------------------
CCC009*    SKIP1
       01  UU073S.
           10  KD-MODUS                PIC 99.
           10  KD-DATUM                PIC 9(7)     COMP-3.
           10  KD-UHRZEIT              PIC 9(9)     COMP-3.
           10  KD-TERM                 PIC X(4).
260894*    10  KD-PERSNR               PIC 9(7).
260894     10  KD-USERID               PIC X(7).
           10  KD-SA                   PIC 99.
           10  KD-GES                  PIC 99.
           10  KD-VNR                  PIC 9(15)    COMP-3.
           10  KD-IDNRA                PIC 9(7)     COMP-3.
           10  KD-INFOA                PIC 9(7)     COMP-3.
           10  KD-VBNRA                PIC 9(7)     COMP-3.
           10  KD-AVMA                 PIC 9(7)     COMP-3.
           10  KD-IDNRN                PIC 9(7)     COMP-3.
           10  KD-INFON                PIC 9(7)     COMP-3.
           10  KD-VBNRN                PIC 9(7)     COMP-3.
           10  KD-GUELTN               PIC 9(7)     COMP-3.
           10  KD-AVMN                 PIC 9(7)     COMP-3.
CCC009*    SKIP1
      ****************** ENDE COPY-MEMBER  UU073S **********************

