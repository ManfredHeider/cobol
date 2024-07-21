      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU910S.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\UU910S.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:15:28   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:05:18   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION   4    26.08.94 16:37:19
      *    *************************************************************
      *    COPY-MEMBER: TERMINAL-STORAGE (TCTUA)
      *    *************************************
      *    LAENGE = 255 BYTES                ERSTELLT: H.SCHORN (AGS)
      *                                         DATUM: 10.01.80
      *    --------------- AENDERUNGEN ---------------------------------
      *    DATUM     NAME          INHALT
      *    -------------------------------------------------------------
      *    19.04.93  SCHÖLLHAMMER  NEU: TS-OB (ORDNUNGSBEGRIFF)
      *                                 TS-SYSTEM-KZ (P=PROD, T=TEST)
      *
      *    26.08.94  WIECHERS
      *              - NEU:  TS-USERID = RACF-USERID
      *              - AEND: TS-PERSNR ENTFAELLT
      *                      TS-CODEWORT ENTFAELLT
      *                      TS-VP-DIR2 - TS-VP-DIR20 ENTFALLEN
      *                      TS-BILDNR VERGROESSERT AUF 7 STELLEN
      *                      TS-VP-DIR1 VERGROESSERT AUF 3 STELLEN
      *                         (UNGEPACKT, ALPHA-NUMERISCH)
      *                      TS-VP-VBNR VERGROESSERT AUF 7 STELLEN
      *                         (UNGEPACKT)
      *    *************************************************************
CCC009*    SKIP1
       01  UU910S.
           02  TS-USERID                 PIC X(7).
           02  TS-TRANS                  PIC X(4).
           02  TS-VARPRUEFD.
               03  TS-VP-DIR1            PIC 999.
               03  TS-VP-VBNR            PIC 9(7).
               03  TS-VP-RES.
                   04  TS-VP-KZ          PIC X.
                   04  TS-BILD1          PIC X.
                   04  TS-BILD2          PIC X.
                   04  TS-BILD3          PIC X.
                   04  TS-BILD4          PIC X.
                   04  TS-VP-RES1        PIC X(21).
           02  TS-KZ                     PIC X.
           02  TS-BILDNR                 PIC X(7).
           02  TS-TEXTNR                 PIC X(3).
           02  TS-OB                     PIC X(32).
           02  TS-SYSTEM-KZ              PIC X.
           02  TS-FILLER.
               03  FILLER                PIC X(164).
CCC009*    SKIP1
      ******** ENDE COPY-MEMBER TERMINAL-STORAGE (UU910S) **************


