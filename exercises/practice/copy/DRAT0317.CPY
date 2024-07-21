      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0317.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRAT0317.CPv  $
      *
      *   Rev 2.1   Feb 21 2000 15:45:46   T13EX48
      *FKI00329
      *Änderung der Eingabe-Datei-Struktur
      *
      *   Rev 2.0   Mar 16 1999 09:57:24   P13SP20
      *Archiverstellung
      *
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FÜR Vorschubsteuerzeichen
      *    Unveränderte Kopie von LKTR360
      *    ----------------------------------------------------------  *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                          !NAME     !VER-*
      *AUFTRAG !                                        !         !SION*
      *--------!----------------------------------------!---------!----*
      *18.02.00!NEUERSTELLUNG                           !H.Sebbach! 01 *
      *KI-NEU  !Benoetigt fuer COMPILE PMPC0010         !         !    *
      *--------!----------------------------------------!---------!----*
      *        !                                        !         !    *
      *--------!----------------------------------------!---------!----*
      *
      *****  VERSION
       01  STEUERKZ360 COMPUTATIONAL SYNC RIGHT.
           02 FILLER PICTURE S9(9) VALUE +265.
           02 FILLER PICTURE S9(9) VALUE +4377.
           02 FILLER PICTURE S9(9) VALUE +35217.
           02 FILLER PICTURE S9(9) VALUE +39329.
           02 FILLER PICTURE S9(9) VALUE +43441.
           02 FILLER PICTURE S9(9) VALUE +47553.
           02 FILLER PICTURE S9(9) VALUE +51665.
           02 FILLER PICTURE S9(9) VALUE +55777.
           02 FILLER PICTURE S9(9) VALUE +2835.
           02 FILLER PICTURE S9(9) VALUE +7051.
           02 FILLER PICTURE S9(9) VALUE +37787.
           02 FILLER PICTURE S9(9) VALUE +41899.
           02 FILLER PICTURE S9(9) VALUE +46011.
           02 FILLER PICTURE S9(9) VALUE +50123.
           02 FILLER PICTURE S9(9) VALUE +54235.
           02 FILLER PICTURE S9(9) VALUE +58115.
       01  STEUERKENNZEICHEN360 REDEFINES STEUERKZ360.
           02 FILLER PICTURE XX.
           02 KTR1 PICTURE X.
      *       X'01' = WRITE OHNE VORSCHUB
           02 KTR2 PICTURE X.
      *       X'09' = WRITE UND ZT1
           02 FILLER PICTURE XX.
           02 KTR3 PICTURE X.
      *       X'11' = WRITE UND ZT2
           02 KTR4 PICTURE X.
      *       X'19' = WRITE UND ZT3
           02 FILLER PICTURE XX.
           02 KTR5 PICTURE X.
      *       X'89' = WRITE UND VORSCHUB NACH KANAL 1
           02 KTR6 PICTURE X.
      *       X'91' = WRITE UND VORSCHUB NACH KANAL 2
           02 FILLER PICTURE XX.
           02 KTR7 PICTURE X.
      *       X'99' = WRITE UND VORSCHUB NACH KANAL 3
           02 KTR8 PICTURE X.
      *       X'A1' = WRITE UND VORSCHUB NACH KANAL 4
           02 FILLER PICTURE XX.
           02 KTR9 PICTURE X.
      *       X'A9' = WRITE UND VORSCHUB NACH KANAL 5
           02 KTR10 PICTURE X.
      *       X'B1' = WRITE UND VORSCHUB NACH KANAL 6
           02 FILLER PICTURE XX.
           02 KTR11 PICTURE X.
      *       X'B9' = WRITE UND VORSCHUB NACH KANAL 7
           02 KTR12 PICTURE X.
      *       X'C1' = WRITE UND VORSCHUB NACH KANAL 8
           02 FILLER PICTURE XX.
           02 KTR13 PICTURE X.
      *       X'C9' = WRITE UND VORSCHUB NACH KANAL 9
           02 KTR14 PICTURE X.
      *       X'D1' = WRITE UND VORSCHUB NACH KANAL 10
           02 FILLER PICTURE XX.
           02 KTR15 PICTURE X.
      *       X'D9' = WRITE UND VORSCHUB NACH KANAL 11
           02 KTR16 PICTURE X.
      *       X'E1' = WRITE UND VORSCHUB NACH KANAL 12
           02 FILLER PICTURE XX.
           02 KTR17 PICTURE X.
      *       X'0B' = ZT1
           02 KTR18 PICTURE X.
      *       X'13' = ZT2
           02 FILLER PICTURE XX.
           02 KTR19 PICTURE X.
      *       X'1B' = ZT3
           02 KTR20 PICTURE X.
      *       X'8B' = VORSCHUB NACH KANAL 1
           02 FILLER PICTURE XX.
           02 KTR21 PICTURE X.
      *       X'93' = VORSCHUB NACH KANAL 2
           02 KTR22 PICTURE X.
      *       X'9B' = VORSCHUB NACH KANAL 3
           02 FILLER PICTURE XX.
           02 KTR23 PICTURE X.
      *       X'A3' = VORSCHUB NACH KANAL 4
           02 KTR24 PICTURE X.
      *       X'AB' = VORSCHUB NACH KANAL 5
           02 FILLER PICTURE XX.
           02 KTR25 PICTURE X.
      *       X'B3' = VORSCHUB NACH KANAL 6
           02 KTR26 PICTURE X.
      *       X'BB' = VORSCHUB NACH KANAL 7
           02 FILLER PICTURE XX.
           02 KTR27 PICTURE X.
      *       X'C3' = VORSCHUB NACH KANAL 8
           02 KTR28 PICTURE X.
      *       X'CB' = VORSCHUB NACH KANAL 9
           02 FILLER PICTURE XX.
           02 KTR29 PICTURE X.
      *       X'D3' = VORSCHUB NACH KANAL 10
           02 KTR30 PICTURE X.
      *       X'DB' = VORSCHUB NACH KANAL 11
           02 FILLER PICTURE XX.
           02 KTR31 PICTURE X.
      *       X'E3' = VORSCHUB NACH KANAL 12
           02 KTR32 PICTURE X.
      *       X'03' = NO OPERATION

      *---------------------------------------------------------------*
